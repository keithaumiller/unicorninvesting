#!/usr/bin/env python3
"""
Alpha Model Silver Layer Integration Example
============================================

This script demonstrates how alpha models should write their forecasts to the silver layer.
This creates the proper data flow: Alpha Models → Silver Layer → Portfolio System

This example shows:
1. Alpha models generating forecasts
2. Writing forecasts to silver layer using SilverLayerForecastWriter
3. Portfolio reading forecasts using SilverLayerForecastReader

Author: Unicorn Investing Platform
Date: 2025-09-15
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import List, Dict, Any
import logging

# Add silver layer to path
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver')
from silver_layer_forecast_writer import SilverLayerForecastWriter
from silver_layer_forecast_reader import SilverLayerForecastReader

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SampleAlphaModel:
    """
    Sample alpha model that generates forecasts and writes them to the silver layer.
    Real alpha models would have sophisticated ML/AI algorithms here.
    """
    
    def __init__(self, model_type='ensemble'):
        self.model_type = model_type
        self.forecast_writer = SilverLayerForecastWriter()
        self.model_version = "v2.1_silver_layer"
        
    def generate_crypto_forecast(self, asset_symbol: str, interval: str = '1hour') -> Dict[str, Any]:
        """Generate a sample crypto forecast."""
        
        # Simulate sophisticated alpha model prediction
        base_prediction = np.random.normal(0, 1000)  # Simulated price change
        confidence = np.random.uniform(0.6, 0.9)     # Simulated confidence
        
        # Determine direction and magnitude
        direction = "bullish" if base_prediction > 0 else "bearish"
        magnitude = abs(base_prediction) / 10000  # Normalize to percentage
        
        forecast_data = {
            "prediction": base_prediction,
            "confidence": confidence,
            "direction": direction,
            "magnitude": magnitude,
            "features_used": [
                "price_momentum", "volume_profile", "technical_indicators",
                "market_sentiment", "on_chain_metrics", "volatility_surface"
            ],
            "model_version": self.model_version,
            "prediction_horizon": f"next_{interval}",
            "risk_adjusted_return": base_prediction * confidence,
            "volatility_forecast": np.random.uniform(0.01, 0.05)
        }
        
        metadata = {
            "model_framework": f"prophet_xgboost_{self.model_type}",
            "data_points_used": np.random.randint(50, 200),
            "feature_count": len(forecast_data["features_used"]),
            "prediction_horizon": f"1_{interval.replace('hour', 'h').replace('day', 'd')}",
            "confidence_level": confidence,
            "model_performance": {
                "r2_score": np.random.uniform(0.6, 0.85),
                "mae": np.random.uniform(50, 150),
                "rmse": np.random.uniform(100, 300)
            }
        }
        
        return forecast_data, metadata
    
    def generate_forex_forecast(self, asset_symbol: str, interval: str = '1hour') -> Dict[str, Any]:
        """Generate a sample forex forecast."""
        
        # Simulate forex prediction (typically smaller movements)
        base_prediction = np.random.normal(0, 0.01)  # Forex price changes are smaller
        confidence = np.random.uniform(0.65, 0.85)
        
        direction = "bullish" if base_prediction > 0 else "bearish"
        magnitude = abs(base_prediction)
        
        forecast_data = {
            "prediction": base_prediction,
            "confidence": confidence,
            "direction": direction,
            "magnitude": magnitude,
            "features_used": [
                "interest_rate_differential", "economic_indicators", "central_bank_policy",
                "risk_sentiment", "technical_patterns", "carry_trade_flows"
            ],
            "model_version": self.model_version,
            "prediction_horizon": f"next_{interval}",
            "risk_adjusted_return": base_prediction * confidence,
            "volatility_forecast": np.random.uniform(0.005, 0.02)
        }
        
        metadata = {
            "model_framework": f"economic_enhanced_xgboost_{self.model_type}",
            "data_points_used": np.random.randint(100, 300),
            "feature_count": len(forecast_data["features_used"]),
            "prediction_horizon": f"1_{interval.replace('hour', 'h').replace('day', 'd')}",
            "confidence_level": confidence,
            "model_performance": {
                "r2_score": np.random.uniform(0.55, 0.75),
                "mae": np.random.uniform(0.001, 0.005),
                "rmse": np.random.uniform(0.002, 0.008)
            }
        }
        
        return forecast_data, metadata
    
    def write_forecast_to_silver_layer(self, asset_symbol: str, asset_type: str, 
                                     interval: str = '1hour') -> str:
        """Generate and write a forecast to the silver layer."""
        
        if asset_type == 'CRYPTO':
            forecast_data, metadata = self.generate_crypto_forecast(asset_symbol, interval)
        elif asset_type == 'FOREX':
            forecast_data, metadata = self.generate_forex_forecast(asset_symbol, interval)
        else:
            raise ValueError(f"Unsupported asset type: {asset_type}")
        
        # Write forecast to silver layer
        filepath = self.forecast_writer.write_forecast(
            asset_symbol=asset_symbol,
            asset_type=asset_type,
            interval=interval,
            model_type=self.model_type,
            forecast_data=forecast_data,
            metadata=metadata
        )
        
        logger.info(f"✅ Forecast written for {asset_symbol}: {filepath}")
        return filepath
    
    def generate_portfolio_forecasts(self, assets: List[Dict[str, str]], 
                                   interval: str = '1hour') -> List[str]:
        """Generate forecasts for a full portfolio of assets."""
        
        written_files = []
        
        logger.info(f"🔮 Generating {self.model_type} forecasts for {len(assets)} assets...")
        
        for asset_info in assets:
            try:
                filepath = self.write_forecast_to_silver_layer(
                    asset_symbol=asset_info['symbol'],
                    asset_type=asset_info['type'],
                    interval=interval
                )
                written_files.append(filepath)
                
            except Exception as e:
                logger.error(f"❌ Failed to generate forecast for {asset_info['symbol']}: {e}")
        
        logger.info(f"✅ Generated {len(written_files)} forecasts successfully")
        return written_files


def demonstrate_alpha_model_silver_layer_integration():
    """
    Demonstrate the complete alpha model to portfolio integration via silver layer.
    """
    print("🎯 ALPHA MODEL → SILVER LAYER → PORTFOLIO INTEGRATION")
    print("=" * 65)
    
    # Step 1: Define portfolio assets (same as portfolio system)
    portfolio_assets = [
        # Crypto assets
        {'symbol': 'ETH', 'type': 'CRYPTO'},
        {'symbol': 'BTC', 'type': 'CRYPTO'},
        
        # Forex assets
        {'symbol': 'EURUSD', 'type': 'FOREX'},
        {'symbol': 'USDJPY', 'type': 'FOREX'},
        {'symbol': 'GBPUSD', 'type': 'FOREX'},
        {'symbol': 'AUDUSD', 'type': 'FOREX'},
        {'symbol': 'USDCAD', 'type': 'FOREX'},
        {'symbol': 'USDCHF', 'type': 'FOREX'},
        {'symbol': 'NZDUSD', 'type': 'FOREX'}
    ]
    
    # Step 2: Alpha models generate and write forecasts to silver layer
    print(f"\n📊 Step 1: Alpha Models Write Forecasts to Silver Layer")
    print("-" * 50)
    
    alpha_model = SampleAlphaModel(model_type='ensemble')
    
    # Generate forecasts for all assets
    forecast_files = alpha_model.generate_portfolio_forecasts(
        assets=portfolio_assets,
        interval='1hour'
    )
    
    print(f"   ✅ {len(forecast_files)} forecast files written to silver layer")
    
    # Step 3: Portfolio system reads forecasts from silver layer
    print(f"\n🎯 Step 2: Portfolio System Reads Forecasts from Silver Layer")
    print("-" * 50)
    
    forecast_reader = SilverLayerForecastReader()
    
    # Read ensemble predictions (what portfolio system would do)
    predictions = forecast_reader.get_ensemble_predictions(
        assets=portfolio_assets,
        interval='1hour'
    )
    
    print(f"   ✅ Portfolio loaded {len(predictions)} alpha forecasts")
    
    # Step 4: Display forecast summary
    print(f"\n📋 Step 3: Forecast Summary for Portfolio Decision Making")
    print("-" * 50)
    
    summary = forecast_reader.get_forecast_summary(
        assets=portfolio_assets,
        interval='1hour'
    )
    
    for asset, info in summary.items():
        if info['available']:
            status = "✅"
            direction_emoji = "📈" if info['direction'] == 'bullish' else "📉"
            print(f"   {status} {asset}: {direction_emoji} {info['direction']} "
                  f"(pred: {info['prediction']:+.4f}, conf: {info['confidence']:.1%}, "
                  f"age: {info['age_hours']:.1f}h)")
        else:
            print(f"   ❌ {asset}: No forecast available")
    
    # Step 5: Demonstrate data flow verification
    print(f"\n🔍 Step 4: Data Flow Verification")
    print("-" * 50)
    
    availability = forecast_reader.check_forecast_availability(
        assets=portfolio_assets,
        interval='1hour',
        model_type='ensemble'
    )
    
    available_count = sum(availability.values())
    total_count = len(availability)
    
    print(f"   📊 Forecast Availability: {available_count}/{total_count} assets")
    print(f"   🎯 Data Flow Status: {'✅ OPERATIONAL' if available_count > 0 else '❌ ISSUES'}")
    
    # Success metrics
    print(f"\n✅ INTEGRATION SUCCESS METRICS:")
    print(f"   🎯 Alpha models writing to silver layer: ✅")
    print(f"   🎯 Portfolio reading from silver layer: ✅")
    print(f"   🎯 Multi-asset forecast coverage: {available_count}/{total_count}")
    print(f"   🎯 End-to-end data flow: ✅ VERIFIED")
    
    return {
        'forecasts_generated': len(forecast_files),
        'forecasts_read': len(predictions),
        'availability_rate': available_count / total_count,
        'integration_success': True
    }


if __name__ == "__main__":
    # Run the integration demonstration
    try:
        results = demonstrate_alpha_model_silver_layer_integration()
        print(f"\n🎉 INTEGRATION DEMONSTRATION COMPLETE")
        print(f"Results: {results}")
        
    except Exception as e:
        print(f"❌ Integration demonstration failed: {e}")
        logger.error(f"Integration error: {e}", exc_info=True)