"""
BTC Prophet Model for CRYPTO

Time series forecasting model using Facebook Prophet.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.model_framework import ProphetModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class BTCProphetModel(ProphetModel):
    """
    Prophet-based forecasting model for BTC.
    
    Uses Facebook Prophet for time series forecasting with BTC-specific optimizations.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        # Asset-specific Prophet configuration
        default_config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.05 if 'BTC' == 'ETH' else 0.1,
            'seasonality_prior_scale': 10.0 if 'BTC' == 'ETH' else 15.0
        }
        
        if config:
            default_config.update(config)
            
        super().__init__('BTC', default_config)
        self.performance_tracker = ModelPerformanceTracker()
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train model with validation split.
        
        Args:
            data: Historical price data
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
            'validation_samples': len(val_data),
            'train_samples': len(train_data)
        }

def create_and_train_btc_prophet(data: pd.DataFrame) -> BTCProphetModel:
    """
    Convenience function to create and train BTC Prophet model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained Prophet model
    """
    model = BTCProphetModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ BTC Prophet model trained successfully")
        print(f"Model ID: {result['model_id']}")
    else:
        print(f"❌ BTC Prophet model training failed: {result.get('error', 'Unknown error')}")
    
    return model

if __name__ == "__main__":
    print("✅ BTC Prophet Model Template Ready")
