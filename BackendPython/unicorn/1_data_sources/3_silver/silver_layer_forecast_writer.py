"""
Silver Layer Forecast Writer
===========================

Unified interface for all alpha models to write forecasts to the silver layer.
This ensures consistent data flow: Alpha Models → Silver Layer → Portfolio System

Architecture:
- Layer 2 (Alpha Models) writes forecasts via this connector
- Layer 3 (Silver Layer) stores all forecasts in standardized format
- Layer 4 (Portfolio) reads forecasts via SilverLayerForecastReader

Author: Unicorn Investing Platform
Date: 2025-09-15
"""

import json
import os
import pandas as pd
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SilverLayerForecastWriter:
    """
    Unified forecast writer for all alpha models to write to silver layer.
    
    Features:
    - Standardized forecast format across all assets
    - Automatic directory creation and management
    - Timestamp-based versioning
    - Metadata tracking and validation
    - Support for ensemble, prophet, and xgboost forecasts
    """
    
    def __init__(self, base_path: str = None):
        """
        Initialize the silver layer forecast writer.
        
        Args:
            base_path: Base path for silver layer forecasts
        """
        if base_path is None:
            self.base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/forecasts"
        else:
            self.base_path = base_path
            
        self.base_path = Path(self.base_path)
        self.timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
        
        # Supported asset types and intervals
        self.supported_asset_types = ['CRYPTO', 'FOREX', 'EQUITIES', 'COMMODITIES']
        self.supported_intervals = ['1min', '1hour', '1day']
        self.supported_model_types = ['ensemble', 'prophet', 'xgboost']
        
        logger.info(f"SilverLayerForecastWriter initialized with base path: {self.base_path}")
    
    def write_forecast(self, 
                      asset_symbol: str,
                      asset_type: str,
                      interval: str,
                      model_type: str,
                      forecast_data: Dict[str, Any],
                      metadata: Dict[str, Any] = None) -> str:
        """
        Write a forecast to the silver layer in standardized format.
        
        Args:
            asset_symbol: Asset symbol (e.g., 'ETH', 'EURUSD', 'AAPL')
            asset_type: Asset type ('CRYPTO', 'FOREX', 'EQUITIES', etc.)
            interval: Time interval ('1min', '1hour', '1day')
            model_type: Model type ('ensemble', 'prophet', 'xgboost')
            forecast_data: Forecast predictions and associated data
            metadata: Additional metadata about the forecast
            
        Returns:
            str: Path to the written forecast file
        """
        # Validate inputs
        self._validate_inputs(asset_symbol, asset_type, interval, model_type)
        
        # Create directory structure
        forecast_dir = self._create_forecast_directory(asset_symbol, asset_type, interval, model_type)
        
        # Prepare forecast payload
        forecast_payload = self._prepare_forecast_payload(
            asset_symbol, asset_type, interval, model_type, forecast_data, metadata
        )
        
        # Generate filename
        filename = f"{asset_symbol}_{interval}_{model_type}_{self.timestamp}.json"
        filepath = forecast_dir / filename
        
        # Write forecast to file
        try:
            with open(filepath, 'w') as f:
                json.dump(forecast_payload, f, indent=2, default=str)
            
            logger.info(f"Forecast written successfully: {filepath}")
            return str(filepath)
            
        except Exception as e:
            logger.error(f"Failed to write forecast to {filepath}: {e}")
            raise
    
    def write_batch_forecasts(self, forecasts: List[Dict[str, Any]]) -> List[str]:
        """
        Write multiple forecasts in batch.
        
        Args:
            forecasts: List of forecast dictionaries with required fields
            
        Returns:
            List[str]: List of written file paths
        """
        written_files = []
        
        for forecast in forecasts:
            try:
                filepath = self.write_forecast(
                    asset_symbol=forecast['asset_symbol'],
                    asset_type=forecast['asset_type'],
                    interval=forecast['interval'],
                    model_type=forecast['model_type'],
                    forecast_data=forecast['forecast_data'],
                    metadata=forecast.get('metadata', {})
                )
                written_files.append(filepath)
                
            except Exception as e:
                logger.error(f"Failed to write forecast for {forecast.get('asset_symbol', 'unknown')}: {e}")
                continue
        
        logger.info(f"Batch write completed: {len(written_files)}/{len(forecasts)} forecasts written successfully")
        return written_files
    
    def _validate_inputs(self, asset_symbol: str, asset_type: str, interval: str, model_type: str):
        """Validate input parameters."""
        if not asset_symbol or not isinstance(asset_symbol, str):
            raise ValueError(f"Invalid asset_symbol: {asset_symbol}")
        
        if asset_type not in self.supported_asset_types:
            raise ValueError(f"Unsupported asset_type: {asset_type}. Supported: {self.supported_asset_types}")
        
        if interval not in self.supported_intervals:
            raise ValueError(f"Unsupported interval: {interval}. Supported: {self.supported_intervals}")
        
        if model_type not in self.supported_model_types:
            raise ValueError(f"Unsupported model_type: {model_type}. Supported: {self.supported_model_types}")
    
    def _create_forecast_directory(self, asset_symbol: str, asset_type: str, interval: str, model_type: str) -> Path:
        """Create the directory structure for the forecast."""
        forecast_dir = self.base_path / asset_type / asset_symbol / interval / model_type
        forecast_dir.mkdir(parents=True, exist_ok=True)
        return forecast_dir
    
    def _prepare_forecast_payload(self, 
                                 asset_symbol: str,
                                 asset_type: str, 
                                 interval: str,
                                 model_type: str,
                                 forecast_data: Dict[str, Any],
                                 metadata: Dict[str, Any] = None) -> Dict[str, Any]:
        """Prepare the standardized forecast payload."""
        
        if metadata is None:
            metadata = {}
        
        payload = {
            "forecast_metadata": {
                "asset_symbol": asset_symbol,
                "asset_type": asset_type,
                "interval": interval,
                "model_type": model_type,
                "timestamp": self.timestamp,
                "created_at": datetime.now(timezone.utc).isoformat(),
                "silver_layer_version": "1.0",
                "data_source": "silver_layer",
                "model_framework": metadata.get('model_framework', 'ensemble'),
                "prediction_horizon": metadata.get('prediction_horizon', '1_period'),
                "confidence_level": metadata.get('confidence_level', 0.75)
            },
            "forecast_data": forecast_data,
            "metadata": metadata,
            "data_quality": {
                "validation_status": "passed",
                "data_points_used": metadata.get('data_points_used', 0),
                "feature_count": metadata.get('feature_count', 0),
                "model_performance": metadata.get('model_performance', {})
            }
        }
        
        return payload
    
    def get_forecast_path(self, asset_symbol: str, asset_type: str, interval: str, model_type: str) -> Path:
        """Get the directory path where forecasts for this asset/interval/model are stored."""
        return self.base_path / asset_type / asset_symbol / interval / model_type
    
    def list_asset_forecasts(self, asset_symbol: str, asset_type: str) -> Dict[str, List[str]]:
        """List all available forecasts for a specific asset."""
        asset_path = self.base_path / asset_type / asset_symbol
        
        if not asset_path.exists():
            return {}
        
        forecasts = {}
        
        for interval in self.supported_intervals:
            interval_path = asset_path / interval
            if interval_path.exists():
                forecasts[interval] = {}
                
                for model_type in self.supported_model_types:
                    model_path = interval_path / model_type
                    if model_path.exists():
                        forecast_files = list(model_path.glob("*.json"))
                        forecasts[interval][model_type] = [str(f) for f in forecast_files]
        
        return forecasts
    
    def cleanup_old_forecasts(self, asset_symbol: str, asset_type: str, keep_latest: int = 10):
        """Clean up old forecast files, keeping only the most recent ones."""
        forecasts = self.list_asset_forecasts(asset_symbol, asset_type)
        
        for interval, model_types in forecasts.items():
            for model_type, files in model_types.items():
                if len(files) > keep_latest:
                    # Sort by modification time and remove oldest
                    files_with_time = [(f, os.path.getmtime(f)) for f in files]
                    files_with_time.sort(key=lambda x: x[1], reverse=True)
                    
                    files_to_remove = files_with_time[keep_latest:]
                    
                    for file_path, _ in files_to_remove:
                        try:
                            os.remove(file_path)
                            logger.info(f"Removed old forecast: {file_path}")
                        except Exception as e:
                            logger.warning(f"Failed to remove {file_path}: {e}")


def main():
    """Demo usage of the SilverLayerForecastWriter."""
    print("🔮 Silver Layer Forecast Writer - Demo")
    print("=" * 50)
    
    writer = SilverLayerForecastWriter()
    
    # Demo forecast data
    sample_forecast = {
        "prediction": 2500.75,
        "confidence": 0.78,
        "direction": "bullish",
        "magnitude": 0.045,
        "features_used": ["price", "volume", "momentum"],
        "model_version": "v2.1"
    }
    
    sample_metadata = {
        "model_framework": "prophet_xgboost_ensemble",
        "data_points_used": 168,
        "feature_count": 15,
        "prediction_horizon": "1_hour",
        "confidence_level": 0.78
    }
    
    # Write sample forecasts
    try:
        # ETH crypto forecast
        eth_path = writer.write_forecast(
            asset_symbol="ETH",
            asset_type="CRYPTO",
            interval="1hour",
            model_type="ensemble",
            forecast_data=sample_forecast,
            metadata=sample_metadata
        )
        print(f"✅ ETH forecast written: {eth_path}")
        
        # EURUSD forex forecast
        eur_path = writer.write_forecast(
            asset_symbol="EURUSD",
            asset_type="FOREX",
            interval="1hour", 
            model_type="ensemble",
            forecast_data={**sample_forecast, "prediction": 1.0875},
            metadata=sample_metadata
        )
        print(f"✅ EURUSD forecast written: {eur_path}")
        
    except Exception as e:
        print(f"❌ Error: {e}")


if __name__ == "__main__":
    main()