"""
Silver Layer Forecast Reader
============================

Unified interface for portfolio systems to read forecasts from the silver layer.
This ensures consistent data flow: Alpha Models → Silver Layer → Portfolio System

Architecture:
- Layer 2 (Alpha Models) writes forecasts via SilverLayerForecastWriter
- Layer 3 (Silver Layer) stores all forecasts in standardized format
- Layer 4 (Portfolio) reads forecasts via this reader

Author: Unicorn Investing Platform
Date: 2025-09-15
"""

import json
import os
import pandas as pd
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Dict, Any, List, Optional, Union, Tuple
import logging
import glob

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SilverLayerForecastReader:
    """
    Unified forecast reader for portfolio systems to read from silver layer.
    
    Features:
    - Read latest forecasts by asset/interval/model type
    - Batch forecast loading for portfolio optimization
    - Historical forecast analysis and comparison
    - Forecast quality and confidence filtering
    - Real-time forecast availability checking
    """
    
    def __init__(self, base_path: str = None):
        """
        Initialize the silver layer forecast reader.
        
        Args:
            base_path: Base path for silver layer forecasts
        """
        if base_path is None:
            self.base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/forecasts"
        else:
            self.base_path = base_path
            
        self.base_path = Path(self.base_path)
        
        # Supported configurations
        self.supported_asset_types = ['CRYPTO', 'FOREX', 'EQUITIES', 'COMMODITIES']
        self.supported_intervals = ['1min', '1hour', '1day']
        self.supported_model_types = ['ensemble', 'prophet', 'xgboost']
        
        logger.info(f"SilverLayerForecastReader initialized with base path: {self.base_path}")
    
    def get_latest_forecast(self, 
                           asset_symbol: str,
                           asset_type: str,
                           interval: str,
                           model_type: str = 'ensemble') -> Optional[Dict[str, Any]]:
        """
        Get the latest forecast for a specific asset/interval/model combination.
        
        Args:
            asset_symbol: Asset symbol (e.g., 'ETH', 'EURUSD')
            asset_type: Asset type ('CRYPTO', 'FOREX', etc.)
            interval: Time interval ('1min', '1hour', '1day')
            model_type: Model type ('ensemble', 'prophet', 'xgboost')
            
        Returns:
            Dict containing forecast data or None if not found
        """
        forecast_dir = self.base_path / asset_type / asset_symbol / interval / model_type
        
        if not forecast_dir.exists():
            logger.warning(f"Forecast directory not found: {forecast_dir}")
            return None
        
        # Get all forecast files and find the latest
        forecast_files = list(forecast_dir.glob(f"{asset_symbol}_{interval}_{model_type}_*.json"))
        
        if not forecast_files:
            logger.warning(f"No forecast files found for {asset_symbol} {interval} {model_type}")
            return None
        
        # Sort by modification time and get the latest
        latest_file = max(forecast_files, key=os.path.getmtime)
        
        try:
            with open(latest_file, 'r') as f:
                forecast_data = json.load(f)
            
            logger.info(f"Loaded latest forecast: {latest_file}")
            return forecast_data
            
        except Exception as e:
            logger.error(f"Failed to load forecast from {latest_file}: {e}")
            return None
    
    def get_portfolio_forecasts(self, 
                               assets: List[Dict[str, str]],
                               interval: str = '1hour',
                               model_type: str = 'ensemble',
                               max_age_hours: int = 6) -> Dict[str, Dict[str, Any]]:
        """
        Get latest forecasts for a portfolio of assets.
        
        Args:
            assets: List of asset dicts with 'symbol' and 'type' keys
            interval: Time interval for forecasts
            model_type: Model type to use
            max_age_hours: Maximum age of forecasts in hours
            
        Returns:
            Dict mapping asset symbols to their forecast data
        """
        portfolio_forecasts = {}
        cutoff_time = datetime.now(timezone.utc) - timedelta(hours=max_age_hours)
        
        for asset_info in assets:
            asset_symbol = asset_info['symbol']
            asset_type = asset_info['type']
            
            forecast = self.get_latest_forecast(asset_symbol, asset_type, interval, model_type)
            
            if forecast is None:
                logger.warning(f"No forecast found for {asset_symbol}")
                continue
            
            # Check forecast age
            forecast_time_str = forecast['forecast_metadata']['created_at']
            forecast_time = datetime.fromisoformat(forecast_time_str.replace('Z', '+00:00'))
            
            if forecast_time < cutoff_time:
                logger.warning(f"Forecast for {asset_symbol} is too old: {forecast_time}")
                continue
            
            portfolio_forecasts[asset_symbol] = forecast
        
        logger.info(f"Loaded {len(portfolio_forecasts)} portfolio forecasts")
        return portfolio_forecasts
    
    def get_ensemble_predictions(self, 
                                assets: List[Dict[str, str]],
                                interval: str = '1hour') -> Dict[str, float]:
        """
        Get ensemble prediction values for quick portfolio decisions.
        
        Args:
            assets: List of asset dicts with 'symbol' and 'type' keys
            interval: Time interval for forecasts
            
        Returns:
            Dict mapping asset symbols to prediction values
        """
        predictions = {}
        
        forecasts = self.get_portfolio_forecasts(assets, interval, 'ensemble')
        
        for asset_symbol, forecast_data in forecasts.items():
            try:
                prediction = forecast_data['forecast_data']['prediction']
                confidence = forecast_data['forecast_data'].get('confidence', 0.5)
                
                # Apply confidence weighting
                weighted_prediction = prediction * confidence
                predictions[asset_symbol] = weighted_prediction
                
            except KeyError as e:
                logger.warning(f"Missing prediction data for {asset_symbol}: {e}")
                continue
        
        return predictions
    
    def get_forecast_history(self, 
                            asset_symbol: str,
                            asset_type: str,
                            interval: str,
                            model_type: str,
                            limit: int = 10) -> List[Dict[str, Any]]:
        """
        Get historical forecasts for analysis and backtesting.
        
        Args:
            asset_symbol: Asset symbol
            asset_type: Asset type
            interval: Time interval
            model_type: Model type
            limit: Maximum number of forecasts to return
            
        Returns:
            List of historical forecast data
        """
        forecast_dir = self.base_path / asset_type / asset_symbol / interval / model_type
        
        if not forecast_dir.exists():
            return []
        
        # Get all forecast files
        forecast_files = list(forecast_dir.glob(f"{asset_symbol}_{interval}_{model_type}_*.json"))
        
        # Sort by modification time (newest first)
        forecast_files.sort(key=os.path.getmtime, reverse=True)
        
        # Limit results
        forecast_files = forecast_files[:limit]
        
        historical_forecasts = []
        
        for file_path in forecast_files:
            try:
                with open(file_path, 'r') as f:
                    forecast_data = json.load(f)
                
                historical_forecasts.append(forecast_data)
                
            except Exception as e:
                logger.warning(f"Failed to load historical forecast {file_path}: {e}")
                continue
        
        logger.info(f"Loaded {len(historical_forecasts)} historical forecasts for {asset_symbol}")
        return historical_forecasts
    
    def check_forecast_availability(self, 
                                   assets: List[Dict[str, str]],
                                   interval: str = '1hour',
                                   model_type: str = 'ensemble') -> Dict[str, bool]:
        """
        Check which assets have recent forecasts available.
        
        Args:
            assets: List of asset dicts
            interval: Time interval
            model_type: Model type
            
        Returns:
            Dict mapping asset symbols to availability status
        """
        availability = {}
        
        for asset_info in assets:
            asset_symbol = asset_info['symbol']
            asset_type = asset_info['type']
            
            forecast = self.get_latest_forecast(asset_symbol, asset_type, interval, model_type)
            availability[asset_symbol] = forecast is not None
        
        return availability
    
    def get_forecast_summary(self, 
                            assets: List[Dict[str, str]],
                            interval: str = '1hour') -> Dict[str, Dict[str, Any]]:
        """
        Get a summary of forecast data for portfolio dashboard.
        
        Args:
            assets: List of asset dicts
            interval: Time interval
            
        Returns:
            Dict with forecast summary for each asset
        """
        summary = {}
        
        for asset_info in assets:
            asset_symbol = asset_info['symbol']
            asset_type = asset_info['type']
            
            ensemble_forecast = self.get_latest_forecast(asset_symbol, asset_type, interval, 'ensemble')
            
            if ensemble_forecast is None:
                summary[asset_symbol] = {
                    'available': False,
                    'prediction': None,
                    'confidence': None,
                    'direction': None,
                    'last_updated': None
                }
                continue
            
            forecast_data = ensemble_forecast['forecast_data']
            metadata = ensemble_forecast['forecast_metadata']
            
            summary[asset_symbol] = {
                'available': True,
                'prediction': forecast_data.get('prediction'),
                'confidence': forecast_data.get('confidence'),
                'direction': forecast_data.get('direction'),
                'magnitude': forecast_data.get('magnitude'),
                'last_updated': metadata['created_at'],
                'model_framework': metadata.get('model_framework'),
                'age_hours': self._calculate_forecast_age(metadata['created_at'])
            }
        
        return summary
    
    def _calculate_forecast_age(self, created_at_str: str) -> float:
        """Calculate forecast age in hours."""
        try:
            forecast_time = datetime.fromisoformat(created_at_str.replace('Z', '+00:00'))
            age = datetime.now(timezone.utc) - forecast_time
            return age.total_seconds() / 3600
        except Exception:
            return 999.0  # Return large number if calculation fails
    
    def list_available_assets(self) -> Dict[str, List[str]]:
        """List all assets with available forecasts by type."""
        available_assets = {}
        
        for asset_type in self.supported_asset_types:
            type_path = self.base_path / asset_type
            
            if type_path.exists():
                assets = [d.name for d in type_path.iterdir() if d.is_dir()]
                available_assets[asset_type] = assets
            else:
                available_assets[asset_type] = []
        
        return available_assets


def main():
    """Demo usage of the SilverLayerForecastReader."""
    print("📊 Silver Layer Forecast Reader - Demo")
    print("=" * 50)
    
    reader = SilverLayerForecastReader()
    
    # Check available assets
    available = reader.list_available_assets()
    print(f"\n🔍 Available Assets:")
    for asset_type, assets in available.items():
        if assets:
            print(f"   {asset_type}: {', '.join(assets)}")
    
    # Demo portfolio forecasts
    portfolio_assets = [
        {'symbol': 'ETH', 'type': 'CRYPTO'},
        {'symbol': 'BTC', 'type': 'CRYPTO'},
        {'symbol': 'EURUSD', 'type': 'FOREX'}
    ]
    
    # Get forecast summary
    summary = reader.get_forecast_summary(portfolio_assets)
    print(f"\n📋 Forecast Summary:")
    for asset, info in summary.items():
        status = "✅" if info['available'] else "❌"
        print(f"   {status} {asset}: {info}")
    
    # Get ensemble predictions
    predictions = reader.get_ensemble_predictions(portfolio_assets)
    print(f"\n🔮 Ensemble Predictions:")
    for asset, prediction in predictions.items():
        print(f"   {asset}: {prediction:+.4f}")


if __name__ == "__main__":
    main()