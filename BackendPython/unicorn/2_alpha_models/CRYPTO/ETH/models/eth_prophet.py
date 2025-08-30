"""
ETH Prophet Model for CRYPTO

Time series forecasting model using Facebook Prophet optimized for Ethereum.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))

from shared.model_framework import ProphetModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class ETHProphetModel(ProphetModel):
    """
    Prophet-based forecasting model for ETH.
    
    Uses Facebook Prophet for time series forecasting with ETH-specific optimizations.
    ETH typically shows higher volatility and different seasonality patterns than BTC.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        # ETH-specific Prophet configuration
        default_config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.1,  # Higher than BTC due to more volatility
            'seasonality_prior_scale': 15.0,  # Higher seasonality variation
            'interval_width': 0.80,  # Wider prediction intervals for ETH
            'growth': 'linear',
            'mcmc_samples': 0,  # No MCMC for speed
            'uncertainty_samples': 1000
        }
        
        if config:
            default_config.update(config)
            
        super().__init__('ETH', default_config)
        self.performance_tracker = ModelPerformanceTracker()
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train model with validation split.
        
        Args:
            data: Historical price data with OHLCV columns
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Split data
        split_idx = int(len(data) * (1 - validation_split))
        train_data = data.iloc[:split_idx]
        val_data = data.iloc[split_idx:]
        
        # Train model
        train_result = self.train(train_data)
        
        if not train_result['success']:
            return train_result
        
        # Validate on holdout data
        val_predictions = self.predict(train_data, periods=len(val_data))
        
        # Track performance
        model_id = f"{self.asset_name}_prophet_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Track validation performance
        self.performance_tracker.track_validation_performance(
            model_id=model_id,
            predictions=val_predictions['yhat'],
            actuals=val_data['Close']
        )
        
        return {
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'validation_predictions': val_predictions,
            'validation_actuals': val_data['Close'],
            'model_config': self.config
        }
        
    def add_eth_specific_regressors(self, model, data: pd.DataFrame):
        """
        Add ETH-specific external regressors to the Prophet model.
        
        Args:
            model: Prophet model instance
            data: Training data with potential regressors
        """
        # Add volume as regressor (ETH volume patterns differ from BTC)
        if 'Volume' in data.columns:
            model.add_regressor('volume_normalized', prior_scale=10.0)
            
        # Add volatility regressor (ETH is more volatile)
        if 'Close' in data.columns:
            data['volatility'] = data['Close'].rolling(window=7).std()
            model.add_regressor('volatility', prior_scale=5.0)
            
        return model
        
    def prepare_prophet_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Prepare data in Prophet format with ETH-specific features.
        
        Args:
            data: Input OHLCV data
            
        Returns:
            DataFrame in Prophet format (ds, y, regressors)
        """
        prophet_data = super().prepare_prophet_data(data)
        
        # Add ETH-specific regressors
        if 'Volume' in data.columns:
            # Normalize volume for better model performance
            volume_norm = (data['Volume'] - data['Volume'].mean()) / data['Volume'].std()
            prophet_data['volume_normalized'] = volume_norm
            
        # Add volatility feature
        if 'Close' in data.columns:
            volatility = data['Close'].rolling(window=7).std().fillna(method='bfill')
            volatility_norm = (volatility - volatility.mean()) / volatility.std()
            prophet_data['volatility'] = volatility_norm
            
        return prophet_data

def create_and_train_eth_prophet(data: pd.DataFrame) -> ETHProphetModel:
    """
    Convenience function to create and train ETH Prophet model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained Prophet model
    """
    model = ETHProphetModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ ETH Prophet model trained successfully")
        print(f"   Model ID: {result['model_id']}")
        if 'performance_metrics' in result:
            print(f"   MAPE: {result['performance_metrics'].get('mape', 'N/A'):.2f}%")
    else:
        print(f"❌ ETH Prophet model training failed: {result.get('error', 'Unknown error')}")
    
    return model

if __name__ == "__main__":
    # Example usage
    print("ETH Prophet Model - Example Usage")
    print("=" * 40)
    
    # This would be replaced with real data loading
    dates = pd.date_range(start='2022-01-01', periods=365, freq='D')
    sample_data = pd.DataFrame({
        'Open': np.random.normal(3000, 150, 365),
        'High': np.random.normal(3100, 150, 365),
        'Low': np.random.normal(2900, 150, 365),
        'Close': np.random.normal(3000, 150, 365),
        'Volume': np.random.normal(1000000, 200000, 365)
    }, index=dates)
    
    model = create_and_train_eth_prophet(sample_data)
    print(f"Model trained: {model.is_trained}")
