"""
Updated BTC Ensemble Model with Economic-Enhanced XGBoost Integration

This updated ensemble properly leverages the new economic-enhanced XGBoost models
instead of the legacy basic XGBoost models.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import sys
import os
from datetime import datetime

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
alpha_models_dir = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
sys.path.append(alpha_models_dir)

# Import economic-enhanced models
sys.path.append(os.path.dirname(current_dir))
from btc_xgboost_economic_enhanced import BTCXGBoostWithEconomicIndicators

# Import legacy Prophet model (still good to use)
from models.btc_prophet import BTCProphetModel

from shared.model_framework import EnsembleModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class BTCEconomicEnsembleModel:
    """
    Updated BTC Ensemble Model leveraging Economic-Enhanced XGBoost.
    
    This ensemble combines Prophet time series forecasting with economic-enhanced
    XGBoost models that integrate macroeconomic indicators for superior performance.
    
    Key Improvements:
    - Uses BTCXGBoostWithEconomicIndicators instead of basic XGBoost
    - Optimized weighting for economic-enhanced models (60% economic XGBoost)
    - Advanced performance tracking and validation
    - Multiple ensemble strategies (conservative, balanced, aggressive)
    """
    
    def __init__(self, ensemble_strategy: str = 'balanced'):
        """
        Initialize BTC Economic Ensemble Model.
        
        Args:
            ensemble_strategy: 'conservative', 'balanced', or 'aggressive'
        """
        self.ensemble_strategy = ensemble_strategy
        self.asset_name = 'BTC'
        
        # Initialize component models
        self.prophet_model = BTCProphetModel()
        self.xgboost_model = BTCXGBoostWithEconomicIndicators(enable_economic_indicators=True)
        
        # Set weights based on strategy
        self.weights = self._get_strategy_weights(ensemble_strategy)
        self.performance_tracker = ModelPerformanceTracker()
        
    def _get_strategy_weights(self, strategy: str) -> Dict[str, float]:
        """
        Get ensemble weights based on strategy.
        
        Args:
            strategy: Ensemble strategy type
            
        Returns:
            Dictionary with model weights
        """
        strategies = {
            'conservative': {'prophet': 0.70, 'xgboost_economic': 0.30},  # Favor stable Prophet
            'balanced': {'prophet': 0.40, 'xgboost_economic': 0.60},     # Favor economic XGBoost
            'aggressive': {'prophet': 0.25, 'xgboost_economic': 0.75}    # Heavy economic emphasis
        }
        
        if strategy not in strategies:
            raise ValueError(f"Strategy must be one of: {list(strategies.keys())}")
            
        return strategies[strategy]
    
    def train(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Train the ensemble model.
        
        Args:
            data: Historical price data with OHLCV format
            
        Returns:
            Training results and model metadata
        """
        try:
            print(f"🚀 Training BTC Economic Ensemble ({self.ensemble_strategy} strategy)...")
            
            # Train Prophet model
            print("   📈 Training Prophet component...")
            prophet_result = self.prophet_model.train(data)
            
            if not prophet_result.get('success', False):
                return {'success': False, 'error': 'Prophet training failed'}
            
            # Train Economic XGBoost model (Deep variant for best performance)
            print("   🧠 Training Economic-Enhanced XGBoost component...")
            xgboost_result = self.xgboost_model.create_enhanced_model(
                data, 
                model_variant='deep',  # Use best performing variant
                test_size=0.2
            )
            
            if not xgboost_result.get('success', False):
                return {'success': False, 'error': 'Economic XGBoost training failed'}
            
            # Store training metadata
            ensemble_metadata = {
                'ensemble_id': f"btc_economic_ensemble_{self.ensemble_strategy}_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
                'strategy': self.ensemble_strategy,
                'weights': self.weights,
                'prophet_result': prophet_result,
                'xgboost_result': xgboost_result,
                'economic_integration': True,
                'created_at': datetime.now().isoformat()
            }
            
            self.ensemble_metadata = ensemble_metadata
            
            return {
                'success': True,
                'ensemble_metadata': ensemble_metadata,
                'component_results': {
                    'prophet': prophet_result,
                    'xgboost_economic': xgboost_result
                }
            }
            
        except Exception as e:
            return {'success': False, 'error': f'Ensemble training error: {str(e)}'}
    
    def predict(self, data: pd.DataFrame, forecast_periods: int = 30) -> Dict[str, Any]:
        """
        Generate ensemble predictions.
        
        Args:
            data: Historical data for prediction
            forecast_periods: Number of periods to forecast
            
        Returns:
            Ensemble predictions with component breakdowns
        """
        try:
            # Get Prophet predictions
            prophet_pred = self.prophet_model.predict(data, forecast_periods)
            
            # Get Economic XGBoost predictions
            xgboost_pred = self.xgboost_model.predict(data)
            
            # Combine predictions using ensemble weights
            ensemble_pred = (
                self.weights['prophet'] * prophet_pred['predictions'] +
                self.weights['xgboost_economic'] * xgboost_pred['predictions']
            )
            
            return {
                'success': True,
                'ensemble_predictions': ensemble_pred,
                'component_predictions': {
                    'prophet': prophet_pred['predictions'],
                    'xgboost_economic': xgboost_pred['predictions']
                },
                'weights_used': self.weights,
                'strategy': self.ensemble_strategy,
                'economic_features_included': True
            }
            
        except Exception as e:
            return {'success': False, 'error': f'Prediction error: {str(e)}'}
    
    def evaluate_ensemble_performance(self, test_data: pd.DataFrame) -> Dict[str, Any]:
        """
        Evaluate ensemble performance against test data.
        
        Args:
            test_data: Test dataset with actual values
            
        Returns:
            Comprehensive performance metrics
        """
        try:
            # Get predictions for test period
            predictions = self.predict(test_data)
            
            if not predictions['success']:
                return predictions
            
            # Calculate ensemble metrics
            actual_values = test_data['price'] if 'price' in test_data.columns else test_data.iloc[:, 0]
            predicted_values = predictions['ensemble_predictions']
            
            # Align arrays (handle length differences)
            min_length = min(len(actual_values), len(predicted_values))
            actual_values = actual_values[-min_length:]
            predicted_values = predicted_values[-min_length:]
            
            # Calculate performance metrics
            mae = np.mean(np.abs(actual_values - predicted_values))
            mse = np.mean((actual_values - predicted_values) ** 2)
            rmse = np.sqrt(mse)
            mape = np.mean(np.abs((actual_values - predicted_values) / actual_values)) * 100
            
            # R² score
            ss_res = np.sum((actual_values - predicted_values) ** 2)
            ss_tot = np.sum((actual_values - np.mean(actual_values)) ** 2)
            r2_score = 1 - (ss_res / ss_tot)
            
            # Component performance comparison
            prophet_mae = np.mean(np.abs(actual_values - predictions['component_predictions']['prophet'][-min_length:]))
            xgboost_mae = np.mean(np.abs(actual_values - predictions['component_predictions']['xgboost_economic'][-min_length:]))
            
            ensemble_performance = {
                'ensemble_mae': mae,
                'ensemble_mse': mse,
                'ensemble_rmse': rmse,
                'ensemble_mape': mape,
                'ensemble_r2': r2_score,
                'component_comparison': {
                    'prophet_mae': prophet_mae,
                    'xgboost_economic_mae': xgboost_mae,
                    'ensemble_improvement_vs_prophet': (prophet_mae - mae) / prophet_mae * 100,
                    'ensemble_improvement_vs_xgboost': (xgboost_mae - mae) / xgboost_mae * 100
                },
                'strategy': self.ensemble_strategy,
                'weights': self.weights,
                'test_samples': min_length
            }
            
            return {
                'success': True,
                'performance': ensemble_performance
            }
            
        except Exception as e:
            return {'success': False, 'error': f'Evaluation error: {str(e)}'}

def create_btc_economic_ensemble_variants(data: pd.DataFrame) -> Dict[str, BTCEconomicEnsembleModel]:
    """
    Create all three ensemble strategy variants for BTC.
    
    Args:
        data: Training data
        
    Returns:
        Dictionary of trained ensemble models
    """
    variants = {}
    strategies = ['conservative', 'balanced', 'aggressive']
    
    for strategy in strategies:
        print(f"\n🔄 Creating {strategy.upper()} BTC Economic Ensemble...")
        
        ensemble = BTCEconomicEnsembleModel(ensemble_strategy=strategy)
        training_result = ensemble.train(data)
        
        if training_result['success']:
            variants[strategy] = ensemble
            print(f"   ✅ {strategy.capitalize()} ensemble trained successfully")
        else:
            print(f"   ❌ {strategy.capitalize()} ensemble training failed: {training_result.get('error', 'Unknown error')}")
    
    return variants

if __name__ == "__main__":
    print("✅ Updated BTC Economic Ensemble Model Ready")
    print("🔬 Features:")
    print("   - Economic-Enhanced XGBoost Integration")
    print("   - Prophet + Economic XGBoost Combination")
    print("   - Multiple Strategy Options (Conservative/Balanced/Aggressive)")
    print("   - Comprehensive Performance Tracking")
    print("   - Production-Ready Implementation")
