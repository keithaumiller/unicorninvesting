"""
Model Development Framework

Comprehensive framework for building Prophet, XGBoost, and Ensemble models for any asset.
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Any, Optional, Tuple, Union
from datetime import datetime, timedelta
import logging
import pickle
import json
from abc import ABC, abstractmethod
import os
from pathlib import Path

# Import ML libraries with fallback handling
try:
    from prophet import Prophet
    PROPHET_AVAILABLE = True
except ImportError:
    PROPHET_AVAILABLE = False
    Prophet = None

try:
    import xgboost as xgb
    from sklearn.model_selection import TimeSeriesSplit, GridSearchCV
    from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
    from sklearn.preprocessing import StandardScaler
    XGBOOST_AVAILABLE = True
except ImportError:
    XGBOOST_AVAILABLE = False
    xgb = None

logger = logging.getLogger(__name__)

class ModelConfig:
    """Configuration management for models."""
    
    def __init__(self, asset_name: str, model_type: str):
        self.asset_name = asset_name
        self.model_type = model_type
        self.config = self._get_default_config()
    
    def _get_default_config(self) -> Dict[str, Any]:
        """Get default configuration based on asset and model type."""
        
        # Asset-specific base configurations
        asset_configs = {
            'ETH': {
                'prophet': {
                    'seasonality_mode': 'multiplicative',
                    'yearly_seasonality': True,
                    'weekly_seasonality': True,
                    'daily_seasonality': False,
                    'changepoint_prior_scale': 0.05,
                    'seasonality_prior_scale': 10.0
                },
                'xgboost': {
                    'n_estimators': 100,
                    'max_depth': 6,
                    'learning_rate': 0.1,
                    'subsample': 0.8,
                    'colsample_bytree': 0.8,
                    'feature_windows': [5, 10, 20, 50]
                }
            },
            'BTC': {
                'prophet': {
                    'seasonality_mode': 'multiplicative',
                    'yearly_seasonality': True,
                    'weekly_seasonality': True,
                    'daily_seasonality': False,
                    'changepoint_prior_scale': 0.1,
                    'seasonality_prior_scale': 15.0
                },
                'xgboost': {
                    'n_estimators': 150,
                    'max_depth': 8,
                    'learning_rate': 0.08,
                    'subsample': 0.9,
                    'colsample_bytree': 0.9,
                    'feature_windows': [5, 10, 20, 50, 100]
                }
            }
        }
        
        # Get asset-specific config or use ETH as default
        asset_config = asset_configs.get(self.asset_name, asset_configs['ETH'])
        return asset_config.get(self.model_type, {})
    
    def update_config(self, updates: Dict[str, Any]):
        """Update configuration with new values."""
        self.config.update(updates)
    
    def get_config(self) -> Dict[str, Any]:
        """Get current configuration."""
        return self.config.copy()

class BaseModel(ABC):
    """Abstract base class for all forecasting models."""
    
    def __init__(self, asset_name: str, model_name: str):
        self.asset_name = asset_name
        self.model_name = model_name
        self.model = None
        self.is_trained = False
        self.training_data = None
        self.scaler = None
        self.feature_columns = []
        self.performance_metrics = {}
        
    @abstractmethod
    def prepare_data(self, data: pd.DataFrame) -> Tuple[pd.DataFrame, pd.Series]:
        """Prepare data for training/prediction."""
        pass
    
    @abstractmethod
    def train(self, data: pd.DataFrame, target_column: str = 'Close') -> Dict[str, Any]:
        """Train the model."""
        pass
    
    @abstractmethod
    def predict(self, data: pd.DataFrame, periods: int = 1) -> pd.DataFrame:
        """Make predictions."""
        pass
    
    def save_model(self, filepath: str):
        """Save trained model to file."""
        if not self.is_trained:
            raise ValueError("Model must be trained before saving")
        
        model_data = {
            'model': self.model,
            'asset_name': self.asset_name,
            'model_name': self.model_name,
            'is_trained': self.is_trained,
            'scaler': self.scaler,
            'feature_columns': self.feature_columns,
            'performance_metrics': self.performance_metrics,
            'training_timestamp': datetime.now().isoformat()
        }
        
        with open(filepath, 'wb') as f:
            pickle.dump(model_data, f)
    
    def load_model(self, filepath: str):
        """Load trained model from file."""
        with open(filepath, 'rb') as f:
            model_data = pickle.load(f)
        
        self.model = model_data['model']
        self.asset_name = model_data['asset_name']
        self.model_name = model_data['model_name']
        self.is_trained = model_data['is_trained']
        self.scaler = model_data.get('scaler')
        self.feature_columns = model_data.get('feature_columns', [])
        self.performance_metrics = model_data.get('performance_metrics', {})

class ProphetModel(BaseModel):
    """Prophet time series forecasting model."""
    
    def __init__(self, asset_name: str, config: Optional[Dict[str, Any]] = None):
        super().__init__(asset_name, "Prophet")
        self.config = ModelConfig(asset_name, "prophet")
        if config:
            self.config.update_config(config)
    
    def prepare_data(self, data: pd.DataFrame) -> Tuple[pd.DataFrame, pd.Series]:
        """Prepare data for Prophet (requires 'ds' and 'y' columns)."""
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet not available. Install with: pip install prophet")
        
        # Convert to Prophet format
        prophet_data = pd.DataFrame({
            'ds': data.index,
            'y': data['Close']
        })
        
        return prophet_data, data['Close']
    
    def train(self, data: pd.DataFrame, target_column: str = 'Close') -> Dict[str, Any]:
        """Train Prophet model."""
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet not available. Install with: pip install prophet")
        
        try:
            # Prepare data
            prophet_data, target = self.prepare_data(data)
            
            # Initialize Prophet with configuration
            self.model = Prophet(**self.config.get_config())
            
            # Fit model
            self.model.fit(prophet_data)
            
            # Generate in-sample predictions for evaluation
            future = self.model.make_future_dataframe(periods=0)
            forecast = self.model.predict(future)
            
            # Calculate performance metrics
            y_true = prophet_data['y'].values
            y_pred = forecast['yhat'].values
            
            self.performance_metrics = {
                'mse': float(mean_squared_error(y_true, y_pred)),
                'mae': float(mean_absolute_error(y_true, y_pred)),
                'r2': float(r2_score(y_true, y_pred)),
                'training_samples': len(prophet_data)
            }
            
            self.is_trained = True
            self.training_data = prophet_data
            
            return {
                'success': True,
                'metrics': self.performance_metrics,
                'message': f"Prophet model trained successfully for {self.asset_name}"
            }
            
        except Exception as e:
            logger.error(f"Prophet training failed: {e}")
            return {
                'success': False,
                'error': str(e),
                'metrics': {}
            }
    
    def predict(self, data: pd.DataFrame, periods: int = 1) -> pd.DataFrame:
        """Make Prophet predictions."""
        if not self.is_trained:
            raise ValueError("Model must be trained before making predictions")
        
        # Make future dataframe
        future = self.model.make_future_dataframe(periods=periods)
        
        # Generate forecast
        forecast = self.model.predict(future)
        
        # Return relevant columns
        return forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].tail(periods)

class XGBoostModel(BaseModel):
    """XGBoost regression model for price prediction."""
    
    def __init__(self, asset_name: str, config: Optional[Dict[str, Any]] = None):
        super().__init__(asset_name, "XGBoost")
        self.config = ModelConfig(asset_name, "xgboost")
        if config:
            self.config.update_config(config)
        self.scaler = StandardScaler() if XGBOOST_AVAILABLE else None
    
    def create_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """Create features for XGBoost model."""
        features = pd.DataFrame(index=data.index)
        
        # Price-based features
        features['close'] = data['Close']
        features['open'] = data['Open']
        features['high'] = data['High']
        features['low'] = data['Low']
        features['volume'] = data['Volume']
        
        # Returns
        features['returns'] = data['Close'].pct_change()
        features['log_returns'] = np.log(data['Close'] / data['Close'].shift(1))
        
        # Moving averages
        for window in self.config.get_config().get('feature_windows', [5, 10, 20]):
            features[f'sma_{window}'] = data['Close'].rolling(window).mean()
            features[f'ema_{window}'] = data['Close'].ewm(span=window).mean()
            features[f'std_{window}'] = data['Close'].rolling(window).std()
        
        # Technical indicators
        features['rsi'] = self._calculate_rsi(data['Close'])
        features['macd'], features['macd_signal'] = self._calculate_macd(data['Close'])
        features['bb_upper'], features['bb_lower'] = self._calculate_bollinger_bands(data['Close'])
        
        # Lag features
        for lag in [1, 2, 3, 5]:
            features[f'close_lag_{lag}'] = data['Close'].shift(lag)
            features[f'returns_lag_{lag}'] = features['returns'].shift(lag)
        
        # Volume features
        features['volume_sma'] = data['Volume'].rolling(20).mean()
        features['volume_ratio'] = data['Volume'] / features['volume_sma']
        
        # Price position features
        features['high_low_ratio'] = data['High'] / data['Low']
        features['close_open_ratio'] = data['Close'] / data['Open']
        
        return features
    
    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate RSI."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
    
    def _calculate_macd(self, prices: pd.Series) -> Tuple[pd.Series, pd.Series]:
        """Calculate MACD."""
        ema12 = prices.ewm(span=12).mean()
        ema26 = prices.ewm(span=26).mean()
        macd = ema12 - ema26
        signal = macd.ewm(span=9).mean()
        return macd, signal
    
    def _calculate_bollinger_bands(self, prices: pd.Series, window: int = 20) -> Tuple[pd.Series, pd.Series]:
        """Calculate Bollinger Bands."""
        sma = prices.rolling(window).mean()
        std = prices.rolling(window).std()
        upper = sma + (std * 2)
        lower = sma - (std * 2)
        return upper, lower
    
    def prepare_data(self, data: pd.DataFrame) -> Tuple[pd.DataFrame, pd.Series]:
        """Prepare features and target for XGBoost."""
        # Create features
        features = self.create_features(data)
        
        # Target: next period's close price
        target = data['Close'].shift(-1)
        
        # Remove NaN values
        valid_idx = features.dropna().index.intersection(target.dropna().index)
        features = features.loc[valid_idx]
        target = target.loc[valid_idx]
        
        self.feature_columns = features.columns.tolist()
        
        return features, target
    
    def train(self, data: pd.DataFrame, target_column: str = 'Close') -> Dict[str, Any]:
        """Train XGBoost model."""
        if not XGBOOST_AVAILABLE:
            raise ImportError("XGBoost not available. Install with: pip install xgboost scikit-learn")
        
        try:
            # Prepare data
            X, y = self.prepare_data(data)
            
            if len(X) < 50:
                return {
                    'success': False,
                    'error': 'Insufficient data for training (minimum 50 samples required)',
                    'metrics': {}
                }
            
            # Scale features
            X_scaled = self.scaler.fit_transform(X)
            
            # Split data for time series validation
            train_size = int(len(X) * 0.8)
            X_train, X_test = X_scaled[:train_size], X_scaled[train_size:]
            y_train, y_test = y.iloc[:train_size], y.iloc[train_size:]
            
            # Initialize model with configuration
            config = self.config.get_config()
            self.model = xgb.XGBRegressor(**{k: v for k, v in config.items() if k != 'feature_windows'})
            
            # Train model
            self.model.fit(X_train, y_train)
            
            # Evaluate on test set
            y_pred_train = self.model.predict(X_train)
            y_pred_test = self.model.predict(X_test)
            
            # Calculate metrics
            self.performance_metrics = {
                'train_mse': float(mean_squared_error(y_train, y_pred_train)),
                'test_mse': float(mean_squared_error(y_test, y_pred_test)),
                'train_mae': float(mean_absolute_error(y_train, y_pred_train)),
                'test_mae': float(mean_absolute_error(y_test, y_pred_test)),
                'train_r2': float(r2_score(y_train, y_pred_train)),
                'test_r2': float(r2_score(y_test, y_pred_test)),
                'feature_importance': dict(zip(self.feature_columns, self.model.feature_importances_)),
                'training_samples': len(X_train),
                'test_samples': len(X_test)
            }
            
            self.is_trained = True
            
            return {
                'success': True,
                'metrics': self.performance_metrics,
                'message': f"XGBoost model trained successfully for {self.asset_name}"
            }
            
        except Exception as e:
            logger.error(f"XGBoost training failed: {e}")
            return {
                'success': False,
                'error': str(e),
                'metrics': {}
            }
    
    def predict(self, data: pd.DataFrame, periods: int = 1) -> pd.DataFrame:
        """Make XGBoost predictions."""
        if not self.is_trained:
            raise ValueError("Model must be trained before making predictions")
        
        # Prepare features for the last available data point
        features = self.create_features(data)
        features = features[self.feature_columns].iloc[-1:].dropna()
        
        if len(features) == 0:
            raise ValueError("No valid features available for prediction")
        
        # Scale features
        X_scaled = self.scaler.transform(features)
        
        # Make predictions
        predictions = []
        for _ in range(periods):
            pred = self.model.predict(X_scaled[-1:])
            predictions.append(pred[0])
        
        # Create result dataframe
        future_dates = pd.date_range(
            start=data.index[-1] + pd.Timedelta(days=1),
            periods=periods,
            freq='D'
        )
        
        return pd.DataFrame({
            'ds': future_dates,
            'yhat': predictions
        })

class EnsembleModel(BaseModel):
    """Ensemble model combining Prophet and XGBoost."""
    
    def __init__(self, asset_name: str, prophet_weight: float = 0.6, xgboost_weight: float = 0.4):
        super().__init__(asset_name, "Ensemble")
        self.prophet_model = ProphetModel(asset_name)
        self.xgboost_model = XGBoostModel(asset_name)
        self.prophet_weight = prophet_weight
        self.xgboost_weight = xgboost_weight
        
        # Ensure weights sum to 1
        total_weight = self.prophet_weight + self.xgboost_weight
        self.prophet_weight /= total_weight
        self.xgboost_weight /= total_weight
    
    def prepare_data(self, data: pd.DataFrame) -> Tuple[pd.DataFrame, pd.Series]:
        """Prepare data for ensemble (use XGBoost format)."""
        return self.xgboost_model.prepare_data(data)
    
    def train(self, data: pd.DataFrame, target_column: str = 'Close') -> Dict[str, Any]:
        """Train both Prophet and XGBoost models."""
        results = {}
        
        # Train Prophet
        prophet_result = self.prophet_model.train(data, target_column)
        results['prophet'] = prophet_result
        
        # Train XGBoost
        xgboost_result = self.xgboost_model.train(data, target_column)
        results['xgboost'] = xgboost_result
        
        # Check if both models trained successfully
        both_successful = prophet_result['success'] and xgboost_result['success']
        
        if both_successful:
            # Calculate ensemble metrics (simplified)
            self.performance_metrics = {
                'prophet_weight': self.prophet_weight,
                'xgboost_weight': self.xgboost_weight,
                'prophet_metrics': prophet_result['metrics'],
                'xgboost_metrics': xgboost_result['metrics'],
                'ensemble_r2': (
                    prophet_result['metrics'].get('r2', 0) * self.prophet_weight +
                    xgboost_result['metrics'].get('test_r2', 0) * self.xgboost_weight
                )
            }
            
            self.is_trained = True
            
            return {
                'success': True,
                'metrics': self.performance_metrics,
                'individual_results': results,
                'message': f"Ensemble model trained successfully for {self.asset_name}"
            }
        else:
            return {
                'success': False,
                'error': "One or more individual models failed to train",
                'individual_results': results,
                'metrics': {}
            }
    
    def predict(self, data: pd.DataFrame, periods: int = 1) -> pd.DataFrame:
        """Make ensemble predictions."""
        if not self.is_trained:
            raise ValueError("Ensemble model must be trained before making predictions")
        
        # Get predictions from both models
        prophet_pred = self.prophet_model.predict(data, periods)
        xgboost_pred = self.xgboost_model.predict(data, periods)
        
        # Combine predictions using weights
        ensemble_pred = (
            prophet_pred['yhat'].values * self.prophet_weight +
            xgboost_pred['yhat'].values * self.xgboost_weight
        )
        
        return pd.DataFrame({
            'ds': prophet_pred['ds'],
            'yhat': ensemble_pred,
            'prophet_pred': prophet_pred['yhat'],
            'xgboost_pred': xgboost_pred['yhat']
        })

class ModelFactory:
    """Factory for creating and managing models."""
    
    @staticmethod
    def create_model(model_type: str, asset_name: str, config: Optional[Dict[str, Any]] = None) -> BaseModel:
        """Create a model of specified type."""
        model_type = model_type.lower()
        
        if model_type == 'prophet':
            return ProphetModel(asset_name, config)
        elif model_type == 'xgboost':
            return XGBoostModel(asset_name, config)
        elif model_type == 'ensemble':
            return EnsembleModel(asset_name)
        else:
            raise ValueError(f"Unknown model type: {model_type}")
    
    @staticmethod
    def create_all_models(asset_name: str) -> Dict[str, BaseModel]:
        """Create all model types for an asset."""
        models = {}
        
        try:
            models['prophet'] = ProphetModel(asset_name)
        except ImportError:
            logger.warning("Prophet not available, skipping Prophet model")
        
        try:
            models['xgboost'] = XGBoostModel(asset_name)
        except ImportError:
            logger.warning("XGBoost not available, skipping XGBoost model")
        
        if 'prophet' in models and 'xgboost' in models:
            models['ensemble'] = EnsembleModel(asset_name)
        
        return models

# Example usage
if __name__ == "__main__":
    print("✅ Model Development Framework Loaded Successfully")
    print(f"Prophet Available: {PROPHET_AVAILABLE}")
    print(f"XGBoost Available: {XGBOOST_AVAILABLE}")
    print("Available Models:")
    print("- ProphetModel")
    print("- XGBoostModel") 
    print("- EnsembleModel")
    print("- ModelFactory")
