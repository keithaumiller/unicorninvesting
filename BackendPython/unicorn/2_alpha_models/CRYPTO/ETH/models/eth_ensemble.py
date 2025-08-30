"""
ETH Ensemble Model for CRYPTO

Ensemble model combining Prophet and XGBoost for ETH with optimized weighting.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))

from shared.model_framework import EnsembleModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage
from eth_prophet import ETHProphetModel
from eth_xgboost import ETHXGBoostModel

class ETHEnsembleModel(EnsembleModel):
    """
    Ensemble model for ETH combining Prophet and XGBoost.
    
    Uses weighted combination of time series forecasting and feature-based prediction
    with ETH-specific optimizations. Given ETH's higher volatility, this ensemble
    favors the XGBoost component slightly more than the BTC ensemble.
    """
    
    def __init__(self, prophet_weight: float = 0.55, xgboost_weight: float = 0.45):
        """
        Initialize ETH ensemble model.
        
        Args:
            prophet_weight: Weight for Prophet predictions (default 0.55)
            xgboost_weight: Weight for XGBoost predictions (default 0.45)
            
        Note: ETH ensemble favors XGBoost slightly more than BTC ensemble
        due to ETH's higher volatility and feature-rich patterns.
        """
        super().__init__('ETH', prophet_weight, xgboost_weight)
        
        # Replace base models with ETH-specific models
        self.prophet_model = ETHProphetModel()
        self.xgboost_model = ETHXGBoostModel()
        
        self.performance_tracker = ModelPerformanceTracker()
        
    def dynamic_weight_adjustment(self, prophet_pred: pd.Series, xgboost_pred: pd.Series, 
                                actuals: pd.Series = None) -> tuple:
        """
        Dynamically adjust weights based on recent performance.
        
        Args:
            prophet_pred: Prophet predictions
            xgboost_pred: XGBoost predictions  
            actuals: Actual values (if available)
            
        Returns:
            Adjusted (prophet_weight, xgboost_weight)
        """
        if actuals is None:
            return self.prophet_weight, self.xgboost_weight
            
        # Calculate recent performance (last 30 predictions)
        recent_period = min(30, len(actuals))
        recent_actuals = actuals.iloc[-recent_period:]
        recent_prophet = prophet_pred.iloc[-recent_period:]
        recent_xgboost = xgboost_pred.iloc[-recent_period:]
        
        # Calculate MAE for each model
        prophet_mae = np.mean(np.abs(recent_actuals - recent_prophet))
        xgboost_mae = np.mean(np.abs(recent_actuals - recent_xgboost))
        
        # Adjust weights based on performance (inverse MAE weighting)
        total_inverse_mae = 1/prophet_mae + 1/xgboost_mae
        adjusted_prophet_weight = (1/prophet_mae) / total_inverse_mae
        adjusted_xgboost_weight = (1/xgboost_mae) / total_inverse_mae
        
        # Smoothing factor to prevent dramatic weight changes
        smoothing = 0.3
        final_prophet_weight = (smoothing * adjusted_prophet_weight + 
                              (1-smoothing) * self.prophet_weight)
        final_xgboost_weight = (smoothing * adjusted_xgboost_weight + 
                              (1-smoothing) * self.xgboost_weight)
        
        return final_prophet_weight, final_xgboost_weight
        
    def ensemble_predict(self, data: pd.DataFrame, periods: int = 30, 
                        dynamic_weights: bool = True) -> Dict[str, Any]:
        """
        Generate ensemble predictions with optional dynamic weighting.
        
        Args:
            data: Historical data for prediction
            periods: Number of periods to predict
            dynamic_weights: Whether to use dynamic weight adjustment
            
        Returns:
            Ensemble predictions with metadata
        """
        # Get predictions from both models
        prophet_pred = self.prophet_model.predict(data, periods)
        
        # For XGBoost, we need to prepare the most recent data point
        xgboost_pred = self.xgboost_model.predict(data.tail(1), periods)
        
        # Use dynamic weights if enabled and we have historical performance
        if dynamic_weights and len(data) >= 30:
            # Use the close prices as pseudo-actuals for weight adjustment
            weights = self.dynamic_weight_adjustment(
                prophet_pred['yhat'], 
                xgboost_pred,
                data['Close'].tail(30)
            )
            prophet_weight, xgboost_weight = weights
        else:
            prophet_weight, xgboost_weight = self.prophet_weight, self.xgboost_weight
        
        # Combine predictions
        ensemble_prediction = (prophet_weight * prophet_pred['yhat'] + 
                             xgboost_weight * xgboost_pred)
        
        # Calculate prediction intervals by combining individual model uncertainties
        if 'yhat_lower' in prophet_pred and 'yhat_upper' in prophet_pred:
            # Weighted combination of prediction intervals
            lower_bound = (prophet_weight * prophet_pred['yhat_lower'] + 
                          xgboost_weight * (xgboost_pred * 0.95))  # Assume 5% lower for XGBoost
            upper_bound = (prophet_weight * prophet_pred['yhat_upper'] + 
                          xgboost_weight * (xgboost_pred * 1.05))  # Assume 5% upper for XGBoost
        else:
            # Fallback: use simple percentage bands
            volatility = data['Close'].rolling(window=30).std().iloc[-1]
            lower_bound = ensemble_prediction - 1.96 * volatility
            upper_bound = ensemble_prediction + 1.96 * volatility
        
        return {
            'ensemble_prediction': ensemble_prediction,
            'prophet_prediction': prophet_pred['yhat'],
            'xgboost_prediction': xgboost_pred,
            'lower_bound': lower_bound,
            'upper_bound': upper_bound,
            'prophet_weight_used': prophet_weight,
            'xgboost_weight_used': xgboost_weight,
            'prediction_metadata': {
                'model_type': 'ETH_Ensemble',
                'dynamic_weights': dynamic_weights,
                'prediction_timestamp': datetime.now().isoformat()
            }
        }
        
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
        
        # Validate ensemble predictions
        split_idx = int(len(data) * (1 - validation_split))
        train_data = data.iloc[:split_idx]
        val_data = data.iloc[split_idx:]
        
        # Generate ensemble predictions for validation period
        ensemble_pred = self.ensemble_predict(train_data, periods=len(val_data))
        
        # Track performance
        model_id = f"{self.asset_name}_ensemble_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        self.performance_tracker.track_validation_performance(
            model_id=model_id,
            predictions=ensemble_pred['ensemble_prediction'],
            actuals=val_data['Close']
        )
        
        return {
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'validation_predictions': ensemble_pred,
            'validation_actuals': val_data['Close'],
            'prophet_weight': self.prophet_weight,
            'xgboost_weight': self.xgboost_weight,
            'ensemble_metadata': ensemble_pred['prediction_metadata']
        }

def create_and_train_eth_ensemble(data: pd.DataFrame) -> ETHEnsembleModel:
    """
    Convenience function to create and train ETH Ensemble model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained Ensemble model
    """
    model = ETHEnsembleModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ ETH Ensemble model trained successfully")
        print(f"   Model ID: {result['model_id']}")
        print(f"   Prophet Weight: {result['prophet_weight']:.3f}")
        print(f"   XGBoost Weight: {result['xgboost_weight']:.3f}")
    else:
        print(f"❌ ETH Ensemble model training failed: {result.get('error', 'Unknown error')}")
    
    return model

if __name__ == "__main__":
    # Example usage
    print("ETH Ensemble Model - Example Usage")
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
    
    model = create_and_train_eth_ensemble(sample_data)
    print(f"Model trained: {model.is_trained}")
    
    # Test ensemble prediction
    if model.is_trained:
        prediction = model.ensemble_predict(sample_data, periods=7)
        print(f"7-day prediction generated: {len(prediction['ensemble_prediction'])} points")
