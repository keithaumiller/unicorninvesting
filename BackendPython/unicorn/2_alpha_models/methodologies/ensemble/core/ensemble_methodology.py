"""
Ensemble Methodology Implementation

Complete ensemble methodology combining Prophet and XGBoost for 6-month ETH forecasting.
Migrated from legacy implementations with enhanced dynamic weighting.

Features:
- Prophet + XGBoost combination with dynamic weighting
- Performance-based weight adjustment
- Multiple ensemble strategies (simple average, weighted, optimized)
- Cross-validation for ensemble validation
- 6-month daily forecast generation
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional, Union, Tuple
import warnings
warnings.filterwarnings('ignore')

from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import logging

logger = logging.getLogger(__name__)

class EnsembleMethodology:
    """
    Ensemble Methodology for ETH 6-Month Daily Forecasting
    
    This class combines Prophet and XGBoost methodologies using dynamic weighting
    strategies to improve forecast accuracy. Includes multiple combination methods
    and performance-based weight optimization.
    """
    
    def __init__(self, asset: str = "ETH", forecast_horizon: int = 180):
        """
        Initialize ensemble methodology
        
        Args:
            asset: Asset symbol (ETH, BTC, etc.)
            forecast_horizon: Number of days to forecast (default: 180 for 6 months)
        """
        self.asset = asset
        self.forecast_horizon = forecast_horizon
        self.is_trained = False
        
        # Component models (will be injected)
        self.prophet_model = None
        self.xgboost_model = None
        
        # Ensemble weights
        self.prophet_weight = 0.5
        self.xgboost_weight = 0.5
        
        # Performance history for dynamic weighting
        self.performance_history = {
            'prophet': [],
            'xgboost': [],
            'ensemble': []
        }
        
        # Ensemble strategies
        self.ensemble_methods = {
            'simple_average': self._simple_average,
            'weighted_average': self._weighted_average,
            'dynamic_weights': self._dynamic_weights,
            'optimized_weights': self._optimized_weights
        }
        
        logger.info(f"Initialized Ensemble methodology for {asset} with {forecast_horizon}-day horizon")
    
    def set_component_models(self, prophet_model, xgboost_model):
        """
        Set the component models for ensemble
        
        Args:
            prophet_model: Trained Prophet model instance
            xgboost_model: Trained XGBoost model instance
        """
        self.prophet_model = prophet_model
        self.xgboost_model = xgboost_model
        
        if hasattr(prophet_model, 'is_trained') and hasattr(xgboost_model, 'is_trained'):
            self.is_trained = prophet_model.is_trained and xgboost_model.is_trained
        
        logger.info("Component models set for ensemble")
    
    def train_ensemble(self, data: pd.DataFrame, validation_data: pd.DataFrame = None) -> Dict[str, Any]:
        """
        Train ensemble by optimizing weights on validation data
        
        Args:
            data: Training data
            validation_data: Validation data for weight optimization
            
        Returns:
            Training results and optimized weights
        """
        try:
            if self.prophet_model is None or self.xgboost_model is None:
                raise ValueError("Component models must be set before training ensemble")
            
            logger.info("Training ensemble methodology...")
            
            # If no validation data provided, use last 20% of training data
            if validation_data is None:
                split_idx = int(len(data) * 0.8)
                train_data = data[:split_idx]
                validation_data = data[split_idx:]
            else:
                train_data = data
            
            # Get predictions from component models on validation data
            prophet_pred = self._get_prophet_predictions(validation_data)
            xgboost_pred = self._get_xgboost_predictions(validation_data)
            
            # Get actual values
            actual_values = validation_data['close'].values
            
            if len(prophet_pred) == 0 or len(xgboost_pred) == 0:
                logger.warning("Could not get predictions from component models")
                return {'status': 'failed', 'error': 'Component model predictions failed'}
            
            # Align predictions and actuals
            min_len = min(len(prophet_pred), len(xgboost_pred), len(actual_values))
            prophet_pred = prophet_pred[-min_len:]
            xgboost_pred = xgboost_pred[-min_len:]
            actual_values = actual_values[-min_len:]
            
            # Calculate individual model performance
            prophet_metrics = self._calculate_metrics(actual_values, prophet_pred)
            xgboost_metrics = self._calculate_metrics(actual_values, xgboost_pred)
            
            # Optimize ensemble weights
            optimized_weights = self._optimize_ensemble_weights(
                prophet_pred, xgboost_pred, actual_values
            )
            
            self.prophet_weight = optimized_weights['prophet']
            self.xgboost_weight = optimized_weights['xgboost']
            
            # Generate ensemble predictions with optimized weights
            ensemble_pred = (self.prophet_weight * prophet_pred + 
                           self.xgboost_weight * xgboost_pred)
            ensemble_metrics = self._calculate_metrics(actual_values, ensemble_pred)
            
            # Store performance history
            self.performance_history['prophet'].append(prophet_metrics)
            self.performance_history['xgboost'].append(xgboost_metrics)
            self.performance_history['ensemble'].append(ensemble_metrics)
            
            self.is_trained = True
            
            logger.info(f"Ensemble training completed")
            logger.info(f"Optimized weights - Prophet: {self.prophet_weight:.3f}, XGBoost: {self.xgboost_weight:.3f}")
            logger.info(f"Ensemble MAPE: {ensemble_metrics['mape']:.2f}%")
            
            return {
                'status': 'success',
                'prophet_weight': self.prophet_weight,
                'xgboost_weight': self.xgboost_weight,
                'prophet_metrics': prophet_metrics,
                'xgboost_metrics': xgboost_metrics,
                'ensemble_metrics': ensemble_metrics,
                'improvement_over_prophet': ensemble_metrics['mape'] - prophet_metrics['mape'],
                'improvement_over_xgboost': ensemble_metrics['mape'] - xgboost_metrics['mape']
            }
            
        except Exception as e:
            logger.error(f"Error training ensemble: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def _get_prophet_predictions(self, data: pd.DataFrame) -> np.ndarray:
        """Get predictions from Prophet model"""
        try:
            if hasattr(self.prophet_model, 'predict_on_data'):
                return self.prophet_model.predict_on_data(data)
            elif hasattr(self.prophet_model, 'predict'):
                # Create future dataframe for Prophet
                future = self.prophet_model.make_future_dataframe(periods=0, freq='D')
                future = future.tail(len(data))
                forecast = self.prophet_model.predict(future)
                return forecast['yhat'].values
            else:
                logger.error("Prophet model does not have predict method")
                return np.array([])
                
        except Exception as e:
            logger.error(f"Error getting Prophet predictions: {e}")
            return np.array([])
    
    def _get_xgboost_predictions(self, data: pd.DataFrame) -> np.ndarray:
        """Get predictions from XGBoost model"""
        try:
            if hasattr(self.xgboost_model, 'predict') and hasattr(self.xgboost_model, 'feature_names'):
                # Extract features for XGBoost
                if hasattr(self.xgboost_model, 'create_crypto_features'):
                    featured_data = self.xgboost_model.create_crypto_features(data)
                    X = featured_data[self.xgboost_model.feature_names].values
                    return self.xgboost_model.predict(X)
                else:
                    # Use available features
                    available_features = [col for col in self.xgboost_model.feature_names if col in data.columns]
                    if available_features:
                        X = data[available_features].values
                        return self.xgboost_model.predict(X)
            
            logger.error("XGBoost model not properly configured for prediction")
            return np.array([])
            
        except Exception as e:
            logger.error(f"Error getting XGBoost predictions: {e}")
            return np.array([])
    
    def _calculate_metrics(self, y_true: np.ndarray, y_pred: np.ndarray) -> Dict[str, float]:
        """Calculate comprehensive performance metrics"""
        try:
            mse = mean_squared_error(y_true, y_pred)
            rmse = np.sqrt(mse)
            mae = mean_absolute_error(y_true, y_pred)
            r2 = r2_score(y_true, y_pred)
            mape = np.mean(np.abs((y_true - y_pred) / y_true)) * 100
            
            return {
                'mse': mse,
                'rmse': rmse,
                'mae': mae,
                'r2': r2,
                'mape': mape
            }
        except Exception as e:
            logger.error(f"Error calculating metrics: {e}")
            return {}
    
    def _optimize_ensemble_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray, 
                                 actual: np.ndarray) -> Dict[str, float]:
        """
        Optimize ensemble weights using linear regression
        
        Args:
            prophet_pred: Prophet predictions
            xgboost_pred: XGBoost predictions
            actual: Actual values
            
        Returns:
            Optimized weights dictionary
        """
        try:
            # Stack predictions as features
            X = np.column_stack([prophet_pred, xgboost_pred])
            
            # Fit linear regression without intercept (weights should sum to ~1)
            lr = LinearRegression(fit_intercept=False)
            lr.fit(X, actual)
            
            # Get weights
            weights = lr.coef_
            
            # Normalize weights to sum to 1
            weight_sum = np.sum(np.abs(weights))
            if weight_sum > 0:
                weights = weights / weight_sum
            else:
                weights = np.array([0.5, 0.5])
            
            # Ensure weights are positive and sum to 1
            weights = np.abs(weights)
            weights = weights / np.sum(weights)
            
            return {
                'prophet': float(weights[0]),
                'xgboost': float(weights[1])
            }
            
        except Exception as e:
            logger.error(f"Error optimizing weights: {e}")
            # Return equal weights as fallback
            return {'prophet': 0.5, 'xgboost': 0.5}
    
    def _simple_average(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray) -> np.ndarray:
        """Simple average ensemble"""
        return (prophet_pred + xgboost_pred) / 2
    
    def _weighted_average(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray) -> np.ndarray:
        """Weighted average using stored weights"""
        return (self.prophet_weight * prophet_pred + self.xgboost_weight * xgboost_pred)
    
    def _dynamic_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray, 
                        actual: np.ndarray = None) -> np.ndarray:
        """
        Dynamic weighting based on recent performance
        
        Args:
            prophet_pred: Prophet predictions
            xgboost_pred: XGBoost predictions
            actual: Actual values (for weight adjustment)
            
        Returns:
            Ensemble predictions with dynamic weights
        """
        try:
            if actual is not None and len(actual) > 0:
                # Calculate recent performance (last 30 points or available)
                recent_period = min(30, len(actual))
                recent_actual = actual[-recent_period:]
                recent_prophet = prophet_pred[-recent_period:]
                recent_xgboost = xgboost_pred[-recent_period:]
                
                # Calculate recent MAPE for each model
                prophet_mape = np.mean(np.abs((recent_actual - recent_prophet) / recent_actual))
                xgboost_mape = np.mean(np.abs((recent_actual - recent_xgboost) / recent_actual))
                
                # Inverse MAPE weighting (lower MAPE gets higher weight)
                if prophet_mape + xgboost_mape > 0:
                    prophet_weight = xgboost_mape / (prophet_mape + xgboost_mape)
                    xgboost_weight = prophet_mape / (prophet_mape + xgboost_mape)
                else:
                    prophet_weight = 0.5
                    xgboost_weight = 0.5
                
                # Apply smoothing with existing weights
                smoothing = 0.3
                final_prophet_weight = (smoothing * prophet_weight + 
                                      (1 - smoothing) * self.prophet_weight)
                final_xgboost_weight = (smoothing * xgboost_weight + 
                                      (1 - smoothing) * self.xgboost_weight)
                
                # Update weights
                self.prophet_weight = final_prophet_weight
                self.xgboost_weight = final_xgboost_weight
            
            return self._weighted_average(prophet_pred, xgboost_pred)
            
        except Exception as e:
            logger.error(f"Error in dynamic weighting: {e}")
            return self._simple_average(prophet_pred, xgboost_pred)
    
    def _optimized_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray, 
                          actual: np.ndarray = None) -> np.ndarray:
        """Optimized weights using linear regression on the fly"""
        try:
            if actual is not None and len(actual) >= len(prophet_pred):
                # Re-optimize weights
                optimized = self._optimize_ensemble_weights(prophet_pred, xgboost_pred, actual)
                temp_prophet_weight = optimized['prophet']
                temp_xgboost_weight = optimized['xgboost']
                
                return (temp_prophet_weight * prophet_pred + temp_xgboost_weight * xgboost_pred)
            else:
                return self._weighted_average(prophet_pred, xgboost_pred)
                
        except Exception as e:
            logger.error(f"Error in optimized weighting: {e}")
            return self._weighted_average(prophet_pred, xgboost_pred)
    
    def ensemble_predict(self, data: pd.DataFrame, method: str = "weighted_average") -> Dict[str, Any]:
        """
        Generate ensemble predictions
        
        Args:
            data: Historical data for prediction context
            method: Ensemble method ('simple_average', 'weighted_average', 'dynamic_weights', 'optimized_weights')
            
        Returns:
            Ensemble prediction results
        """
        try:
            if not self.is_trained:
                raise ValueError("Ensemble must be trained before making predictions")
            
            logger.info(f"Generating ensemble predictions using {method}")
            
            # Get predictions from component models
            prophet_pred = self._get_prophet_predictions(data)
            xgboost_pred = self._get_xgboost_predictions(data)
            
            if len(prophet_pred) == 0 or len(xgboost_pred) == 0:
                return {'status': 'failed', 'error': 'Component model predictions failed'}
            
            # Align predictions
            min_len = min(len(prophet_pred), len(xgboost_pred))
            prophet_pred = prophet_pred[-min_len:]
            xgboost_pred = xgboost_pred[-min_len:]
            
            # Apply ensemble method
            ensemble_method = self.ensemble_methods.get(method, self._weighted_average)
            
            if method in ['dynamic_weights', 'optimized_weights'] and 'close' in data.columns:
                actual_values = data['close'].values[-min_len:]
                ensemble_pred = ensemble_method(prophet_pred, xgboost_pred, actual_values)
            else:
                ensemble_pred = ensemble_method(prophet_pred, xgboost_pred)
            
            return {
                'status': 'success',
                'method': method,
                'prophet_predictions': prophet_pred,
                'xgboost_predictions': xgboost_pred,
                'ensemble_predictions': ensemble_pred,
                'prophet_weight': self.prophet_weight,
                'xgboost_weight': self.xgboost_weight,
                'prediction_length': len(ensemble_pred)
            }
            
        except Exception as e:
            logger.error(f"Error in ensemble prediction: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def forecast_6_months(self, data: pd.DataFrame, method: str = "dynamic_weights") -> Dict[str, Any]:
        """
        Generate 6-month ensemble forecast
        
        Args:
            data: Historical data
            method: Ensemble method to use
            
        Returns:
            6-month forecast results
        """
        try:
            logger.info(f"Generating {self.forecast_horizon}-day ensemble forecast...")
            
            if not self.is_trained:
                raise ValueError("Ensemble must be trained before forecasting")
            
            # Get forecasts from component models
            prophet_forecast = self._get_component_forecast(self.prophet_model, data)
            xgboost_forecast = self._get_component_forecast(self.xgboost_model, data)
            
            if prophet_forecast is None or xgboost_forecast is None:
                return {'status': 'failed', 'error': 'Component forecasts failed'}
            
            # Apply ensemble method to forecasts
            ensemble_method = self.ensemble_methods.get(method, self._weighted_average)
            
            if method in ['dynamic_weights', 'optimized_weights']:
                # Use recent actual data for weight optimization
                recent_actual = data['close'].tail(30).values
                recent_prophet = prophet_forecast['predictions'][:len(recent_actual)]
                recent_xgboost = xgboost_forecast['predictions'][:len(recent_actual)]
                
                # Optimize weights on recent data
                if len(recent_actual) > 0:
                    ensemble_method(recent_prophet, recent_xgboost, recent_actual)
            
            # Generate ensemble forecast
            ensemble_predictions = ensemble_method(
                prophet_forecast['predictions'], 
                xgboost_forecast['predictions']
            )
            
            # Calculate forecast statistics
            current_price = data['close'].iloc[-1]
            final_price = ensemble_predictions[-1]
            price_change_pct = ((final_price - current_price) / current_price) * 100
            
            # Create forecast DataFrame
            forecast_dates = pd.date_range(
                start=data.index[-1] + timedelta(days=1),
                periods=self.forecast_horizon,
                freq='D'
            )
            
            forecast_df = pd.DataFrame({
                'date': forecast_dates,
                'ensemble_prediction': ensemble_predictions,
                'prophet_prediction': prophet_forecast['predictions'],
                'xgboost_prediction': xgboost_forecast['predictions']
            })
            
            logger.info(f"6-month ensemble forecast completed")
            logger.info(f"Current ETH Price: ${current_price:.2f}")
            logger.info(f"6-month forecast: ${final_price:.2f}")
            logger.info(f"Expected change: {price_change_pct:+.2f}%")
            logger.info(f"Ensemble weights - Prophet: {self.prophet_weight:.3f}, XGBoost: {self.xgboost_weight:.3f}")
            
            return {
                'status': 'success',
                'forecast_horizon': self.forecast_horizon,
                'current_price': current_price,
                'forecast_final': final_price,
                'price_change_pct': price_change_pct,
                'forecast_data': forecast_df,
                'methodology': 'Ensemble',
                'ensemble_method': method,
                'prophet_weight': self.prophet_weight,
                'xgboost_weight': self.xgboost_weight,
                'component_forecasts': {
                    'prophet': prophet_forecast,
                    'xgboost': xgboost_forecast
                },
                'timestamp': datetime.now()
            }
            
        except Exception as e:
            logger.error(f"Error generating ensemble forecast: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def _get_component_forecast(self, model, data: pd.DataFrame) -> Optional[Dict[str, Any]]:
        """Get forecast from a component model"""
        try:
            if hasattr(model, 'forecast_6_months'):
                result = model.forecast_6_months(data)
                if result.get('status') == 'success':
                    forecast_data = result['forecast_data']
                    if isinstance(forecast_data, pd.DataFrame):
                        if 'predicted_price' in forecast_data.columns:
                            return {'predictions': forecast_data['predicted_price'].values}
                        elif 'yhat' in forecast_data.columns:
                            return {'predictions': forecast_data['yhat'].values}
            
            # Try alternative forecast methods
            if hasattr(model, 'predict'):
                # For Prophet-like models
                if hasattr(model, 'make_future_dataframe'):
                    future = model.make_future_dataframe(periods=self.forecast_horizon, freq='D')
                    forecast = model.predict(future)
                    return {'predictions': forecast['yhat'].tail(self.forecast_horizon).values}
                
                # For XGBoost-like models
                elif hasattr(model, 'create_crypto_features'):
                    # Generate predictions iteratively
                    predictions = []
                    current_data = data.tail(1).copy()
                    
                    for i in range(self.forecast_horizon):
                        featured_data = model.create_crypto_features(current_data)
                        if hasattr(model, 'feature_names'):
                            X = featured_data[model.feature_names].values.reshape(1, -1)
                            pred = model.predict(X)[0]
                            predictions.append(pred)
                            
                            # Update for next iteration (simplified)
                            current_data['close'].iloc[-1] = pred
                    
                    return {'predictions': np.array(predictions)}
            
            logger.error(f"Could not generate forecast from component model")
            return None
            
        except Exception as e:
            logger.error(f"Error getting component forecast: {e}")
            return None
    
    def get_ensemble_performance(self) -> Dict[str, Any]:
        """Get ensemble performance summary"""
        try:
            if not self.performance_history['ensemble']:
                return {'status': 'no_data', 'message': 'No performance history available'}
            
            latest_performance = self.performance_history['ensemble'][-1]
            
            # Calculate improvements
            improvements = {}
            if self.performance_history['prophet']:
                prophet_mape = self.performance_history['prophet'][-1]['mape']
                improvements['vs_prophet'] = prophet_mape - latest_performance['mape']
            
            if self.performance_history['xgboost']:
                xgboost_mape = self.performance_history['xgboost'][-1]['mape']
                improvements['vs_xgboost'] = xgboost_mape - latest_performance['mape']
            
            return {
                'status': 'success',
                'latest_metrics': latest_performance,
                'improvements': improvements,
                'current_weights': {
                    'prophet': self.prophet_weight,
                    'xgboost': self.xgboost_weight
                },
                'performance_history_length': len(self.performance_history['ensemble'])
            }
            
        except Exception as e:
            logger.error(f"Error getting ensemble performance: {e}")
            return {'status': 'failed', 'error': str(e)}
