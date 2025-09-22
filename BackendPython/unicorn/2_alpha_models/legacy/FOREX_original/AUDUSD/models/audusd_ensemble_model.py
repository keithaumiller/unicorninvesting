#!/usr/bin/env python3
"""
AUDUSD Ensemble Model - Multi-Interval Implementation
Combines Prophet and XGBoost predictions with optimized weighting
Intervals: 1m, 1h, 1d with dynamic weight adjustment
"""

import pandas as pd
import numpy as np
from datetime import datetime
from pathlib import Path
import sys
import logging
import pickle
import json

# Add project root to path
project_root = Path(__file__).parent.parent.parent.parent
sys.path.append(str(project_root))

# Import individual models
from audusd_prophet_model import AUDUSDProphetModel
from audusd_xgboost_model import AUDUSDXGBoostModel

from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.linear_model import LinearRegression

class AUDUSDEnsembleModel:
    """Ensemble model combining Prophet and XGBoost for AUDUSD"""
    
    def __init__(self, interval: str = "1d"):
        self.asset = "AUDUSD"
        self.category = "forex"
        self.interval = interval
        
        # Initialize component models
        self.prophet_model = AUDUSDProphetModel(interval=interval)
        self.xgboost_model = AUDUSDXGBoostModel(interval=interval)
        
        # Ensemble configuration
        self.weights = {'prophet': 0.5, 'xgboost': 0.5}
        self.weight_optimizer = LinearRegression()
        self.ensemble_metrics = {}
        
        # Ensemble methods
        self.ensemble_methods = {
            "simple_average": self._simple_average,
            "weighted_average": self._weighted_average,
            "optimized_weights": self._optimized_weights,
            "dynamic_weights": self._dynamic_weights
        }
        
    def train_component_models(self) -> dict:
        """Train both Prophet and XGBoost models"""
        results = {}
        
        try:
            # Train Prophet model
            logging.info(f"Training Prophet model for AUDUSD {self.interval}")
            prophet_result = self.prophet_model.train_model()
            results['prophet'] = prophet_result
            
            if prophet_result['status'] != 'success':
                return {'status': 'failed', 'error': 'Prophet training failed'}
            
            # Train XGBoost model
            logging.info(f"Training XGBoost model for AUDUSD {self.interval}")
            xgboost_result = self.xgboost_model.train_model()
            results['xgboost'] = xgboost_result
            
            if xgboost_result['status'] != 'success':
                return {'status': 'failed', 'error': 'XGBoost training failed'}
            
            return {'status': 'success', 'component_results': results}
            
        except Exception as e:
            logging.error(f"Component model training failed: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def _simple_average(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray) -> np.ndarray:
        """Simple average ensemble"""
        return (prophet_pred + xgboost_pred) / 2
    
    def _weighted_average(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray) -> np.ndarray:
        """Weighted average based on individual model performance"""
        return (self.weights['prophet'] * prophet_pred + 
                self.weights['xgboost'] * xgboost_pred)
    
    def _optimized_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray, 
                          actual: np.ndarray) -> np.ndarray:
        """Optimize weights using linear regression"""
        # Stack predictions
        X = np.column_stack([prophet_pred, xgboost_pred])
        
        # Fit weight optimizer
        self.weight_optimizer.fit(X, actual)
        
        # Get optimized prediction
        return self.weight_optimizer.predict(X)
    
    def _dynamic_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray,
                        actual: np.ndarray = None) -> np.ndarray:
        """Dynamic weights based on recent performance"""
        if actual is None:
            return self._weighted_average(prophet_pred, xgboost_pred)
        
        # Calculate recent performance (last 20% of data)
        split_idx = int(len(actual) * 0.8)
        
        prophet_recent_mae = mean_absolute_error(actual[split_idx:], prophet_pred[split_idx:])
        xgboost_recent_mae = mean_absolute_error(actual[split_idx:], xgboost_pred[split_idx:])
        
        # Inverse MAE weighting (lower MAE = higher weight)
        total_inv_mae = (1/prophet_recent_mae) + (1/xgboost_recent_mae)
        prophet_weight = (1/prophet_recent_mae) / total_inv_mae
        xgboost_weight = (1/xgboost_recent_mae) / total_inv_mae
        
        return prophet_weight * prophet_pred + xgboost_weight * xgboost_pred
    
    def create_ensemble_predictions(self, data: pd.DataFrame, method: str = "weighted_average") -> dict:
        """Create ensemble predictions using specified method"""
        try:
            # Get Prophet predictions
            prophet_forecast = self.prophet_model.forecast
            prophet_pred = prophet_forecast['yhat'].values
            
            # Get XGBoost predictions  
            xgboost_pred = self.xgboost_model.predict(data)
            
            # Align predictions (take minimum length)
            min_len = min(len(prophet_pred), len(xgboost_pred))
            prophet_pred = prophet_pred[-min_len:]
            xgboost_pred = xgboost_pred[-min_len:]
            
            # Get actual values for comparison
            actual = data['close'].iloc[-min_len:].values
            
            # Apply ensemble method
            ensemble_method = self.ensemble_methods.get(method, self._weighted_average)
            
            if method == "optimized_weights" or method == "dynamic_weights":
                ensemble_pred = ensemble_method(prophet_pred, xgboost_pred, actual)
            else:
                ensemble_pred = ensemble_method(prophet_pred, xgboost_pred)
            
            # Calculate ensemble metrics
            ensemble_metrics = {
                'mae': mean_absolute_error(actual, ensemble_pred),
                'mse': mean_squared_error(actual, ensemble_pred),
                'rmse': np.sqrt(mean_squared_error(actual, ensemble_pred)),
                'r2': r2_score(actual, ensemble_pred)
            }
            
            # Compare with individual models
            prophet_metrics = {
                'mae': mean_absolute_error(actual, prophet_pred),
                'r2': r2_score(actual, prophet_pred)
            }
            
            xgboost_metrics = {
                'mae': mean_absolute_error(actual, xgboost_pred),
                'r2': r2_score(actual, xgboost_pred)
            }
            
            return {
                'status': 'success',
                'ensemble_predictions': ensemble_pred,
                'prophet_predictions': prophet_pred,
                'xgboost_predictions': xgboost_pred,
                'actual_values': actual,
                'ensemble_metrics': ensemble_metrics,
                'prophet_metrics': prophet_metrics,
                'xgboost_metrics': xgboost_metrics,
                'method': method,
                'weights': self.weights
            }
            
        except Exception as e:
            logging.error(f"Ensemble prediction failed: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def train_ensemble(self, method: str = "weighted_average") -> dict:
        """Train complete ensemble model"""
        try:
            # Train component models
            component_result = self.train_component_models()
            if component_result['status'] != 'success':
                return component_result
            
            # Load data for ensemble creation
            data = self.prophet_model.load_data()
            
            # Create ensemble predictions
            ensemble_result = self.create_ensemble_predictions(data, method)
            if ensemble_result['status'] != 'success':
                return ensemble_result
            
            # Store results
            self.ensemble_metrics = ensemble_result['ensemble_metrics']
            
            # Complete result
            final_result = {
                'status': 'success',
                'component_results': component_result['component_results'],
                'ensemble_result': ensemble_result,
                'metadata': {
                    'asset': self.asset,
                    'category': self.category,
                    'interval': self.interval,
                    'ensemble_method': method,
                    'trained_at': datetime.now().isoformat()
                }
            }
            
            return final_result
            
        except Exception as e:
            logging.error(f"Ensemble training failed: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def save_ensemble(self, filepath: str = None):
        """Save complete ensemble model"""
        if not filepath:
            filepath = f"AUDUSD_ensemble_{self.interval}_model.pkl"
            
        ensemble_data = {
            'prophet_model': self.prophet_model,
            'xgboost_model': self.xgboost_model,
            'weights': self.weights,
            'ensemble_metrics': self.ensemble_metrics,
            'metadata': {
                'asset': self.asset,
                'category': self.category,
                'interval': self.interval,
                'saved_at': datetime.now().isoformat()
            }
        }
        
        with open(filepath, 'wb') as f:
            pickle.dump(ensemble_data, f)
            
        logging.info(f"Ensemble model saved to {filepath}")

def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description='AUDUSD Ensemble Model')
    parser.add_argument('--interval', default='1d', choices=['1m', '1h', '1d'],
                       help='Time interval for modeling')
    parser.add_argument('--method', default='weighted_average',
                       choices=['simple_average', 'weighted_average', 'optimized_weights', 'dynamic_weights'],
                       help='Ensemble method')
    parser.add_argument('--save', action='store_true',
                       help='Save trained ensemble')
    
    args = parser.parse_args()
    
    # Train ensemble
    ensemble = AUDUSDEnsembleModel(interval=args.interval)
    result = ensemble.train_ensemble(method=args.method)
    
    if result['status'] == 'success':
        print(f"✅ AUDUSD Ensemble {args.interval} model training successful!")
        
        # Display metrics comparison
        ensemble_metrics = result['ensemble_result']['ensemble_metrics']
        prophet_metrics = result['ensemble_result']['prophet_metrics']
        xgboost_metrics = result['ensemble_result']['xgboost_metrics']
        
        print(f"📊 Ensemble Metrics: R² = {ensemble_metrics['r2']:.4f}, MAE = {ensemble_metrics['mae']:.4f}")
        print(f"📈 Prophet Metrics:  R² = {prophet_metrics['r2']:.4f}, MAE = {prophet_metrics['mae']:.4f}")
        print(f"🚀 XGBoost Metrics:  R² = {xgboost_metrics['r2']:.4f}, MAE = {xgboost_metrics['mae']:.4f}")
        
        if args.save:
            ensemble.save_ensemble()
    else:
        print(f"❌ Training failed: {result['error']}")

if __name__ == "__main__":
    main()
