"""
ETH Forecast Reader - Trading Algorithm Interface

This module provides a clean interface for trading algorithms to read
forecast data from the forecast generation system. Implements caching,
validation, and performance monitoring for forecast consumption.

Features:
- Simple forecast retrieval API
- Automatic forecast validation
- Performance caching
- Multiple timeframe support
- Error handling and fallbacks
"""

import pandas as pd
import numpy as np
import json
import sys
import os
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, Any, List, Optional, Tuple, Union
import warnings
from dataclasses import dataclass
from enum import Enum

warnings.filterwarnings('ignore', category=RuntimeWarning)

class ForecastQuality(Enum):
    """Forecast quality levels."""
    HIGH = "high"
    MEDIUM = "medium" 
    LOW = "low"
    STALE = "stale"
    UNAVAILABLE = "unavailable"

@dataclass
class ForecastData:
    """Simplified forecast data for trading algorithms."""
    asset: str
    timeframe: str
    current_price_estimate: float
    predicted_prices: List[float]  # Next N periods
    confidence_score: float
    quality: ForecastQuality
    forecast_age_minutes: float
    model_type: str
    horizon_periods: int
    
    def get_next_price(self) -> float:
        """Get the next period price prediction."""
        return self.predicted_prices[0] if self.predicted_prices else self.current_price_estimate
    
    def get_price_change_pct(self) -> float:
        """Get expected price change percentage for next period."""
        if not self.predicted_prices:
            return 0.0
        return (self.predicted_prices[0] - self.current_price_estimate) / self.current_price_estimate
    
    def get_trend_direction(self) -> str:
        """Get overall trend direction across forecast horizon."""
        if len(self.predicted_prices) < 2:
            return "neutral"
        
        first_half = self.predicted_prices[:len(self.predicted_prices)//2]
        second_half = self.predicted_prices[len(self.predicted_prices)//2:]
        
        avg_first = np.mean(first_half)
        avg_second = np.mean(second_half)
        
        if avg_second > avg_first * 1.01:  # More than 1% increase
            return "bullish"
        elif avg_second < avg_first * 0.99:  # More than 1% decrease
            return "bearish"
        else:
            return "neutral"
    
    def get_volatility_estimate(self) -> float:
        """Estimate volatility from forecast spread."""
        if len(self.predicted_prices) < 2:
            return 0.0
        
        returns = np.diff(self.predicted_prices) / self.predicted_prices[:-1]
        return np.std(returns) if len(returns) > 0 else 0.0


class ETHForecastReader:
    """
    Reader interface for ETH forecasts optimized for trading algorithm consumption.
    """
    
    def __init__(self, asset: str = "ETH", cache_duration_minutes: int = 5):
        self.asset = asset
        self.cache_duration_minutes = cache_duration_minutes
        self.forecasts_dir = Path(__file__).parent / "forecasts"
        
        # Cache for forecast data
        self.forecast_cache = {}
        self.cache_timestamps = {}
        
        # Quality thresholds
        self.quality_thresholds = {
            'confidence_high': 0.8,
            'confidence_medium': 0.6,
            'age_fresh_minutes': 60,    # 1 hour
            'age_stale_minutes': 360,   # 6 hours
        }
        
        # Timeframe specifications
        self.timeframe_specs = {
            '1min': {
                'expected_update_interval': 1,  # Every minute
                'max_age_tolerance': 5,         # 5 minutes max age
                'forecast_horizon': 60
            },
            '1hour': {
                'expected_update_interval': 60,  # Every hour
                'max_age_tolerance': 180,        # 3 hours max age
                'forecast_horizon': 24
            },
            '1day': {
                'expected_update_interval': 1440,  # Every day (1440 minutes)
                'max_age_tolerance': 2880,          # 2 days max age
                'forecast_horizon': 30
            }
        }
    
    def get_forecast(self, timeframe: str, use_cache: bool = True) -> ForecastData:
        """
        Get the latest forecast for specified timeframe.
        
        Args:
            timeframe: '1min', '1hour', or '1day'
            use_cache: Whether to use cached data if available
            
        Returns:
            ForecastData object with predictions and metadata
        """
        # Check cache first
        if use_cache and self._is_cache_valid(timeframe):
            return self.forecast_cache[timeframe]
        
        # Load latest forecast from disk
        forecast_record = self._load_latest_forecast(timeframe)
        
        if forecast_record is None:
            # Return unavailable forecast
            return self._create_unavailable_forecast(timeframe)
        
        # Convert to ForecastData
        forecast_data = self._convert_to_forecast_data(forecast_record, timeframe)
        
        # Cache the result
        self.forecast_cache[timeframe] = forecast_data
        self.cache_timestamps[timeframe] = datetime.now()
        
        return forecast_data
    
    def get_multi_timeframe_forecasts(self, timeframes: List[str] = None) -> Dict[str, ForecastData]:
        """Get forecasts for multiple timeframes."""
        if timeframes is None:
            timeframes = ['1min', '1hour', '1day']
        
        forecasts = {}
        for timeframe in timeframes:
            forecasts[timeframe] = self.get_forecast(timeframe)
        
        return forecasts
    
    def get_forecast_signals(self, timeframe: str, threshold: float = 0.02) -> Dict[str, Any]:
        """
        Get trading signals based on forecast.
        
        Args:
            timeframe: Forecast timeframe
            threshold: Minimum price change % to generate signal
            
        Returns:
            Dictionary with signal information
        """
        forecast = self.get_forecast(timeframe)
        
        if forecast.quality == ForecastQuality.UNAVAILABLE:
            return {
                'signal': 'hold',
                'strength': 0.0,
                'reason': 'no_forecast_available'
            }
        
        price_change_pct = forecast.get_price_change_pct()
        trend_direction = forecast.get_trend_direction()
        
        # Generate signal based on forecast
        if abs(price_change_pct) < threshold:
            signal = 'hold'
            strength = 0.0
        elif price_change_pct > threshold:
            signal = 'buy'
            strength = min(abs(price_change_pct) / threshold, 1.0)
        else:
            signal = 'sell'
            strength = min(abs(price_change_pct) / threshold, 1.0)
        
        # Adjust strength based on forecast quality and confidence
        quality_multiplier = {
            ForecastQuality.HIGH: 1.0,
            ForecastQuality.MEDIUM: 0.7,
            ForecastQuality.LOW: 0.4,
            ForecastQuality.STALE: 0.2,
            ForecastQuality.UNAVAILABLE: 0.0
        }
        
        strength *= quality_multiplier[forecast.quality]
        strength *= forecast.confidence_score
        
        return {
            'signal': signal,
            'strength': strength,
            'price_change_pct': price_change_pct,
            'trend_direction': trend_direction,
            'forecast_quality': forecast.quality.value,
            'confidence': forecast.confidence_score,
            'model_type': forecast.model_type,
            'forecast_age_minutes': forecast.forecast_age_minutes
        }
    
    def _is_cache_valid(self, timeframe: str) -> bool:
        """Check if cached forecast is still valid."""
        if timeframe not in self.cache_timestamps:
            return False
        
        age_minutes = (datetime.now() - self.cache_timestamps[timeframe]).total_seconds() / 60
        return age_minutes < self.cache_duration_minutes
    
    def _load_latest_forecast(self, timeframe: str) -> Optional[Dict[str, Any]]:
        """Load the latest forecast file for timeframe."""
        forecast_dir = self.forecasts_dir / timeframe
        
        if not forecast_dir.exists():
            return None
        
        # Find the most recent forecast file
        forecast_files = list(forecast_dir.glob(f"{self.asset}_{timeframe}_*.json"))
        
        if not forecast_files:
            return None
        
        # Sort by filename (which includes timestamp)
        latest_file = sorted(forecast_files)[-1]
        
        try:
            with open(latest_file, 'r') as f:
                return json.load(f)
        except Exception as e:
            print(f"⚠️  Error loading forecast from {latest_file}: {e}")
            return None
    
    def _convert_to_forecast_data(self, forecast_record: Dict[str, Any], timeframe: str) -> ForecastData:
        """Convert forecast record to ForecastData object."""
        metadata = forecast_record['metadata']
        predictions = forecast_record['predictions']
        
        # Extract metadata
        model_type = metadata['model_type']
        confidence_score = metadata['confidence_score']
        forecast_timestamp = pd.to_datetime(metadata['forecast_timestamp'])
        
        # Calculate forecast age
        age_minutes = (datetime.now() - forecast_timestamp).total_seconds() / 60
        
        # Extract predictions
        predicted_prices = []
        prediction_times = sorted(predictions.keys())
        
        for pred_time in prediction_times:
            pred_data = predictions[pred_time]
            predicted_prices.append(pred_data['predicted_price'])
        
        # Estimate current price (use last prediction as baseline)
        current_price_estimate = predicted_prices[0] if predicted_prices else 3000.0
        
        # Determine forecast quality
        quality = self._assess_forecast_quality(confidence_score, age_minutes, timeframe)
        
        return ForecastData(
            asset=self.asset,
            timeframe=timeframe,
            current_price_estimate=current_price_estimate,
            predicted_prices=predicted_prices,
            confidence_score=confidence_score,
            quality=quality,
            forecast_age_minutes=age_minutes,
            model_type=model_type,
            horizon_periods=len(predicted_prices)
        )
    
    def _assess_forecast_quality(self, confidence: float, age_minutes: float, timeframe: str) -> ForecastQuality:
        """Assess the quality of a forecast based on confidence and age."""
        spec = self.timeframe_specs[timeframe]
        
        # Check if forecast is too old
        if age_minutes > spec['max_age_tolerance']:
            return ForecastQuality.STALE
        
        # Check if forecast is stale but not too old
        if age_minutes > self.quality_thresholds['age_stale_minutes']:
            return ForecastQuality.LOW
        
        # Quality based on confidence score
        if confidence >= self.quality_thresholds['confidence_high']:
            if age_minutes <= self.quality_thresholds['age_fresh_minutes']:
                return ForecastQuality.HIGH
            else:
                return ForecastQuality.MEDIUM
        elif confidence >= self.quality_thresholds['confidence_medium']:
            return ForecastQuality.MEDIUM
        else:
            return ForecastQuality.LOW
    
    def _create_unavailable_forecast(self, timeframe: str) -> ForecastData:
        """Create a forecast data object for when no forecast is available."""
        return ForecastData(
            asset=self.asset,
            timeframe=timeframe,
            current_price_estimate=0.0,
            predicted_prices=[],
            confidence_score=0.0,
            quality=ForecastQuality.UNAVAILABLE,
            forecast_age_minutes=float('inf'),
            model_type="none",
            horizon_periods=0
        )
    
    def get_forecast_status(self) -> Dict[str, Dict[str, Any]]:
        """Get status of all forecast timeframes."""
        status = {}
        
        for timeframe in self.timeframe_specs.keys():
            forecast = self.get_forecast(timeframe)
            
            status[timeframe] = {
                'available': forecast.quality != ForecastQuality.UNAVAILABLE,
                'quality': forecast.quality.value,
                'confidence': forecast.confidence_score,
                'age_minutes': forecast.forecast_age_minutes,
                'model_type': forecast.model_type,
                'last_update': datetime.now() - timedelta(minutes=forecast.forecast_age_minutes),
                'predictions_count': len(forecast.predicted_prices)
            }
        
        return status
    
    def clear_cache(self):
        """Clear the forecast cache."""
        self.forecast_cache.clear()
        self.cache_timestamps.clear()
    
    def is_forecast_fresh(self, timeframe: str, max_age_minutes: int = None) -> bool:
        """Check if forecast is fresh enough for trading decisions."""
        if max_age_minutes is None:
            max_age_minutes = self.timeframe_specs[timeframe]['expected_update_interval'] * 2
        
        forecast = self.get_forecast(timeframe)
        return (forecast.quality != ForecastQuality.UNAVAILABLE and 
                forecast.forecast_age_minutes <= max_age_minutes)


# Convenience functions for trading algorithms
def get_eth_forecast_signal(timeframe: str, threshold: float = 0.02) -> Dict[str, Any]:
    """
    Quick function to get ETH forecast signal for trading algorithms.
    
    Args:
        timeframe: '1min', '1hour', or '1day'
        threshold: Minimum price change % to generate signal
        
    Returns:
        Signal dictionary with trading recommendation
    """
    reader = ETHForecastReader()
    return reader.get_forecast_signals(timeframe, threshold)

def get_eth_price_prediction(timeframe: str, periods_ahead: int = 1) -> float:
    """
    Get ETH price prediction for specific periods ahead.
    
    Args:
        timeframe: '1min', '1hour', or '1day'
        periods_ahead: How many periods ahead to predict (1-based)
        
    Returns:
        Predicted price or 0.0 if unavailable
    """
    reader = ETHForecastReader()
    forecast = reader.get_forecast(timeframe)
    
    if (forecast.quality == ForecastQuality.UNAVAILABLE or 
        periods_ahead > len(forecast.predicted_prices)):
        return 0.0
    
    return forecast.predicted_prices[periods_ahead - 1]

def get_eth_trend_direction(timeframe: str) -> str:
    """
    Get ETH trend direction from forecast.
    
    Args:
        timeframe: '1min', '1hour', or '1day'
        
    Returns:
        'bullish', 'bearish', or 'neutral'
    """
    reader = ETHForecastReader()
    forecast = reader.get_forecast(timeframe)
    
    if forecast.quality == ForecastQuality.UNAVAILABLE:
        return "neutral"
    
    return forecast.get_trend_direction()


# Usage example
if __name__ == "__main__":
    # Example usage for trading algorithms
    reader = ETHForecastReader()
    
    print("📊 ETH Forecast Reader Demo")
    print("=" * 30)
    
    # Get forecasts for all timeframes
    forecasts = reader.get_multi_timeframe_forecasts()
    
    for timeframe, forecast in forecasts.items():
        print(f"\n{timeframe.upper()} Forecast:")
        print(f"  Quality: {forecast.quality.value}")
        print(f"  Confidence: {forecast.confidence_score:.3f}")
        print(f"  Age: {forecast.forecast_age_minutes:.1f} minutes")
        print(f"  Model: {forecast.model_type}")
        
        if forecast.quality != ForecastQuality.UNAVAILABLE:
            print(f"  Next Price: ${forecast.get_next_price():.2f}")
            print(f"  Price Change: {forecast.get_price_change_pct():.2%}")
            print(f"  Trend: {forecast.get_trend_direction()}")
            
            # Get trading signal
            signal = reader.get_forecast_signals(timeframe)
            print(f"  Signal: {signal['signal']} (strength: {signal['strength']:.3f})")
    
    # Show system status
    print(f"\n📋 Forecast Status:")
    status = reader.get_forecast_status()
    for timeframe, info in status.items():
        availability = "✅" if info['available'] else "❌"
        print(f"  {timeframe}: {availability} {info['quality']} (age: {info['age_minutes']:.1f}m)")
    
    # Demonstrate convenience functions
    print(f"\n🚀 Quick Functions:")
    print(f"  1min signal: {get_eth_forecast_signal('1min')['signal']}")
    print(f"  1hour price: ${get_eth_price_prediction('1hour'):.2f}")
    print(f"  1day trend: {get_eth_trend_direction('1day')}")
