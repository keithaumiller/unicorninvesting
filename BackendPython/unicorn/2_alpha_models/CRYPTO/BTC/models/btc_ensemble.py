"""
BTC Ensemble Model for CRYPTO

Ensemble model combining Prophet and XGBoost for BTC.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.model_framework import EnsembleModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage
from .btc_prophet import BTCProphetModel
from .btc_xgboost import BTCXGBoostModel

class BTCEnsembleModel(EnsembleModel):
    """
    Ensemble model for BTC combining Prophet and XGBoost.
    
    Uses weighted combination of time series forecasting and feature-based prediction.
    """
    
    def __init__(self, prophet_weight: float = 0.6, xgboost_weight: float = 0.4):
        super().__init__('BTC', prophet_weight, xgboost_weight)
        
        # Replace base models with asset-specific models
        self.prophet_model = BTCProphetModel()
        self.xgboost_model = BTCXGBoostModel()
        
        self.performance_tracker = ModelPerformanceTracker()
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train ensemble model with validation split and performance tracking.
        
        Args:
            data: Historical price data
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Train ensemble
        train_result = self.train(data)
        
        if not train_result['success']:
            return train_result
        
        # Track performance
        model_id = f"{self.asset_name}_ensemble_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        return {
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'prophet_weight': self.prophet_weight,
            'xgboost_weight': self.xgboost_weight
        }

def create_and_train_btc_ensemble(data: pd.DataFrame) -> BTCEnsembleModel:
    """
    Convenience function to create and train BTC Ensemble model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained Ensemble model
    """
    model = BTCEnsembleModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ BTC Ensemble model trained successfully")
        print(f"Model ID: {result['model_id']}")
    else:
        print(f"❌ BTC Ensemble model training failed: {result.get('error', 'Unknown error')}")
    
    return model

if __name__ == "__main__":
    print("✅ BTC Ensemble Model Template Ready")
