"""
BTC XGBoost Model for CRYPTO

Gradient boosting model for BTC price prediction.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.model_framework import XGBoostModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class BTCXGBoostModel(XGBoostModel):
    """
    XGBoost-based prediction model for BTC.
    
    Uses gradient boosting with BTC-specific feature engineering.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        # Asset-specific XGBoost configuration
        default_config = {
            'n_estimators': 100 if 'BTC' == 'ETH' else 150,
            'max_depth': 6 if 'BTC' == 'ETH' else 8,
            'learning_rate': 0.1 if 'BTC' == 'ETH' else 0.08,
            'subsample': 0.8 if 'BTC' == 'ETH' else 0.9,
            'colsample_bytree': 0.8 if 'BTC' == 'ETH' else 0.9,
            'feature_windows': [5, 10, 20, 50] if 'BTC' == 'ETH' else [5, 10, 20, 50, 100]
        }
        
        if config:
            default_config.update(config)
            
        super().__init__('BTC', default_config)
        self.performance_tracker = ModelPerformanceTracker()
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train model with validation split and performance tracking.
        
        Args:
            data: Historical price data
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Train model
        train_result = self.train(data)
        
        if not train_result['success']:
            return train_result
        
        # Track performance
        model_id = f"{self.asset_name}_xgboost_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Get predictions for performance tracking
        X, y = self.prepare_data(data)
        train_size = int(len(X) * 0.8)
        
        X_train = X.iloc[:train_size]
        y_train = y.iloc[:train_size]
        X_val = X.iloc[train_size:]
        y_val = y.iloc[train_size:]
        
        # Scale and predict
        X_train_scaled = self.scaler.transform(X_train)
        X_val_scaled = self.scaler.transform(X_val)
        
        train_pred = pd.Series(self.model.predict(X_train_scaled), index=y_train.index)
        val_pred = pd.Series(self.model.predict(X_val_scaled), index=y_val.index)
        
        # Track validation performance  
        self.performance_tracker.track_validation_performance(
            model_id=model_id,
            predictions=val_pred,
            actuals=y_val
        )
        
        return {
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'validation_samples': len(X_val),
            'train_samples': len(X_train),
            'feature_importance': dict(zip(self.feature_columns, self.model.feature_importances_))
        }

def create_and_train_btc_xgboost(data: pd.DataFrame) -> BTCXGBoostModel:
    """
    Convenience function to create and train BTC XGBoost model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained XGBoost model
    """
    model = BTCXGBoostModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ BTC XGBoost model trained successfully")
        print(f"Model ID: {result['model_id']}")
    else:
        print(f"❌ BTC XGBoost model training failed: {result.get('error', 'Unknown error')}")
    
    return model

if __name__ == "__main__":
    print("✅ BTC XGBoost Model Template Ready")
