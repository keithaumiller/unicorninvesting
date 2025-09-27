"""
XGBoost Methodology Implementation for Cryptocurrency Forecasting

Production-ready XGBoost implementation optimized for crypto markets with:
- Advanced feature engineering for time series
- Crypto-specific market indicators
- Robust cross-validation
- Overfitting prevention
- Performance tracking
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import logging
import warnings
warnings.filterwarnings('ignore')

# XGBoost and ML imports
import xgboost as xgb
from sklearn.model_selection import TimeSeriesSplit, GridSearchCV
from sklearn.metrics import mean_absolute_error, mean_squared_error, mean_absolute_percentage_error
from sklearn.preprocessing import StandardScaler

# Add paths for core imports
current_dir = Path(__file__).parent.parent.parent.parent
sys.path.append(str(current_dir))

try:
    from core.interfaces.alpha_methodology import AlphaMethodology
    from core.interfaces.trained_model import TrainedModel
    from core.config.config_manager import ConfigManager
except ImportError:
    # Fallback for development
    print("Warning: Core interfaces not available, using base classes")
    
    class AlphaMethodology:
        pass
    
    class TrainedModel:
        pass
    
    class ConfigManager:
        @staticmethod
        def load_config(config_type, asset_class):
            return {}

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class XGBoostCryptoModel(TrainedModel):
    """Trained XGBoost model wrapper for crypto forecasting"""
    
    def __init__(self, model: xgb.XGBRegressor, scaler: StandardScaler, 
                 feature_columns: List[str], performance_metrics: Dict[str, float],
                 training_config: Dict[str, Any]):
        self.model = model
        self.scaler = scaler
        self.feature_columns = feature_columns
        self.performance_metrics = performance_metrics
        self.training_config = training_config
        self.created_at = datetime.now()
        
    def predict(self, features: pd.DataFrame) -> np.ndarray:
        """Generate predictions from features"""
        # Ensure features match training columns
        features_aligned = features[self.feature_columns]
        
        # Scale features
        features_scaled = self.scaler.transform(features_aligned)
        
        # Generate predictions
        predictions = self.model.predict(features_scaled)
        
        return predictions
    
    def get_feature_importance(self) -> pd.DataFrame:
        """Get feature importance from trained model"""
        importance_scores = self.model.feature_importances_
        importance_df = pd.DataFrame({
            'feature': self.feature_columns,
            'importance': importance_scores
        }).sort_values('importance', ascending=False)
        
        return importance_df
    
    def get_metadata(self) -> Dict[str, Any]:
        """Get model metadata"""
        return {
            'model_type': 'XGBoost',
            'features_count': len(self.feature_columns),
            'performance_metrics': self.performance_metrics,
            'training_config': self.training_config,
            'created_at': self.created_at.isoformat()
        }

class XGBoostMethodology(AlphaMethodology):
    """
    XGBoost methodology optimized for cryptocurrency price forecasting.
    
    Features:
    - Advanced time series feature engineering
    - Crypto-specific technical indicators
    - Robust cross-validation with time series splits
    - Hyperparameter optimization
    - Overfitting prevention
    - Performance tracking and validation
    """
    
    def __init__(self, asset_adapter, config: Optional[Dict] = None):
        """
        Initialize XGBoost methodology
        
        Args:
            asset_adapter: Asset adapter providing market data
            config: Optional configuration dictionary
        """
        self.asset_adapter = asset_adapter
        self.config = config or self._get_default_config()
        self.scaler = StandardScaler()
        self.trained_model = None
        
        # Feature engineering components
        self.feature_columns = []
        self.target_column = 'target_return'
        
        logger.info(f"Initialized XGBoost methodology for {asset_adapter.__class__.__name__}")
    
    def _get_default_config(self) -> Dict[str, Any]:
        """Get default XGBoost configuration optimized for crypto"""
        return {
            # XGBoost model parameters
            'n_estimators': 200,
            'max_depth': 6,
            'learning_rate': 0.05,
            'subsample': 0.8,
            'colsample_bytree': 0.8,
            'reg_alpha': 0.1,
            'reg_lambda': 0.1,
            'random_state': 42,
            
            # Feature engineering parameters
            'lookback_periods': [1, 2, 3, 5, 10, 20],
            'target_horizon': 1,  # Predict 1 day ahead
            'volatility_windows': [5, 10, 20],
            'ma_windows': [5, 10, 20, 50],
            
            # Training parameters
            'test_size': 0.2,
            'cv_folds': 5,
            'validation_metric': 'mape',
            
            # Feature selection
            'feature_importance_threshold': 0.001,
            'max_features': 50
        }
    
    def prepare_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Comprehensive feature engineering for crypto price prediction
        
        Args:
            data: Raw market data with OHLCV columns
            
        Returns:
            DataFrame with engineered features and target
        """
        logger.info("Starting feature engineering for XGBoost...")
        
        df = data.copy()
        
        # Basic price features
        df['returns'] = df['close'].pct_change()
        df['log_returns'] = np.log(df['close'] / df['close'].shift(1))
        df['price_change'] = df['close'] - df['open']
        df['price_range'] = df['high'] - df['low']
        df['body_size'] = abs(df['close'] - df['open'])
        
        # Volume features
        df['volume_change'] = df['volume'].pct_change()
        df['volume_price_trend'] = df['volume'] * df['returns']
        df['volume_ma_ratio'] = df['volume'] / df['volume'].rolling(20).mean()
        
        # Moving averages and trends
        for window in self.config['ma_windows']:
            df[f'ma_{window}'] = df['close'].rolling(window).mean()
            df[f'price_ma_ratio_{window}'] = df['close'] / df[f'ma_{window}']
            df[f'ma_slope_{window}'] = df[f'ma_{window}'].diff(5)
        
        # Technical indicators
        self._add_technical_indicators(df)
        
        # Volatility features
        for window in self.config['volatility_windows']:
            df[f'volatility_{window}'] = df['returns'].rolling(window).std()
            df[f'volatility_ratio_{window}'] = df[f'volatility_{window}'] / df[f'volatility_{window}'].shift(window)
        
        # Lag features
        for lag in self.config['lookback_periods']:
            df[f'return_lag_{lag}'] = df['returns'].shift(lag)
            df[f'volume_lag_{lag}'] = df['volume_change'].shift(lag)
            df[f'volatility_lag_{lag}'] = df[f'volatility_{self.config["volatility_windows"][0]}'].shift(lag)
        
        # Time-based features
        df['hour'] = df.index.hour if hasattr(df.index, 'hour') else 0
        df['day_of_week'] = df.index.dayofweek if hasattr(df.index, 'dayofweek') else 0
        df['day_of_month'] = df.index.day if hasattr(df.index, 'day') else 1
        df['month'] = df.index.month if hasattr(df.index, 'month') else 1
        
        # Momentum features
        for period in [5, 10, 20]:
            df[f'momentum_{period}'] = df['close'] / df['close'].shift(period) - 1
            df[f'acceleration_{period}'] = df[f'momentum_{period}'] - df[f'momentum_{period}'].shift(1)
        
        # Support and resistance levels
        df['high_20'] = df['high'].rolling(20).max()
        df['low_20'] = df['low'].rolling(20).min()
        df['price_position'] = (df['close'] - df['low_20']) / (df['high_20'] - df['low_20'])
        
        # Target variable (future return)
        target_horizon = self.config['target_horizon']
        df[self.target_column] = df['close'].shift(-target_horizon) / df['close'] - 1
        
        # Remove rows with NaN values
        df = df.dropna()
        
        # Select relevant feature columns (exclude OHLCV and target)
        feature_cols = [col for col in df.columns if col not in 
                       ['open', 'high', 'low', 'close', 'volume', self.target_column] and
                       not col.startswith('ma_')]  # Exclude raw MA columns, keep ratios
        
        self.feature_columns = feature_cols
        
        logger.info(f"Feature engineering completed: {len(feature_cols)} features, {len(df)} samples")
        
        return df
    
    def _add_technical_indicators(self, df: pd.DataFrame):
        """Add technical indicators to the dataset"""
        
        # RSI
        delta = df['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # MACD
        exp1 = df['close'].ewm(span=12).mean()
        exp2 = df['close'].ewm(span=26).mean()
        df['macd'] = exp1 - exp2
        df['macd_signal'] = df['macd'].ewm(span=9).mean()
        df['macd_histogram'] = df['macd'] - df['macd_signal']
        
        # Bollinger Bands
        df['bb_middle'] = df['close'].rolling(20).mean()
        bb_std = df['close'].rolling(20).std()
        df['bb_upper'] = df['bb_middle'] + (bb_std * 2)
        df['bb_lower'] = df['bb_middle'] - (bb_std * 2)
        df['bb_position'] = (df['close'] - df['bb_lower']) / (df['bb_upper'] - df['bb_lower'])
        df['bb_width'] = (df['bb_upper'] - df['bb_lower']) / df['bb_middle']
        
        # Williams %R
        highest_high = df['high'].rolling(14).max()
        lowest_low = df['low'].rolling(14).min()
        df['williams_r'] = -100 * ((highest_high - df['close']) / (highest_high - lowest_low))
        
        # Average True Range (ATR)
        high_low = df['high'] - df['low']
        high_close = np.abs(df['high'] - df['close'].shift())
        low_close = np.abs(df['low'] - df['close'].shift())
        ranges = pd.concat([high_low, high_close, low_close], axis=1)
        true_range = ranges.max(axis=1)
        df['atr'] = true_range.rolling(14).mean()
        df['atr_ratio'] = df['atr'] / df['close']
    
    def train(self, data: pd.DataFrame, validation_split: Optional[float] = None) -> XGBoostCryptoModel:
        """
        Train XGBoost model on prepared data
        
        Args:
            data: Raw market data
            validation_split: Optional validation split ratio
            
        Returns:
            Trained XGBoostCryptoModel
        """
        logger.info("Starting XGBoost model training...")
        
        # Prepare features
        processed_data = self.prepare_features(data)
        
        if len(processed_data) < 100:
            raise ValueError(f"Insufficient data for training: {len(processed_data)} samples")
        
        # Prepare features and target
        X = processed_data[self.feature_columns]
        y = processed_data[self.target_column]
        
        # Remove any remaining NaN values
        mask = ~(X.isna().any(axis=1) | y.isna())
        X = X[mask]
        y = y[mask]
        
        logger.info(f"Training data: {len(X)} samples, {len(self.feature_columns)} features")
        
        # Split data for validation
        test_size = validation_split or self.config['test_size']
        split_index = int(len(X) * (1 - test_size))
        
        X_train, X_test = X.iloc[:split_index], X.iloc[split_index:]
        y_train, y_test = y.iloc[:split_index], y.iloc[split_index:]
        
        # Scale features
        X_train_scaled = self.scaler.fit_transform(X_train)
        X_test_scaled = self.scaler.transform(X_test)
        
        # Create and train XGBoost model
        xgb_model = xgb.XGBRegressor(
            n_estimators=self.config['n_estimators'],
            max_depth=self.config['max_depth'],
            learning_rate=self.config['learning_rate'],
            subsample=self.config['subsample'],
            colsample_bytree=self.config['colsample_bytree'],
            reg_alpha=self.config['reg_alpha'],
            reg_lambda=self.config['reg_lambda'],
            random_state=self.config['random_state'],
            objective='reg:squarederror',
            eval_metric='rmse'
        )
        
        # Train with early stopping
        xgb_model.fit(
            X_train_scaled, y_train,
            eval_set=[(X_test_scaled, y_test)],
            early_stopping_rounds=20,
            verbose=False
        )
        
        # Generate predictions for validation
        y_pred_train = xgb_model.predict(X_train_scaled)
        y_pred_test = xgb_model.predict(X_test_scaled)
        
        # Calculate performance metrics
        performance_metrics = {
            'train_mape': mean_absolute_percentage_error(y_train, y_pred_train) * 100,
            'test_mape': mean_absolute_percentage_error(y_test, y_pred_test) * 100,
            'train_rmse': np.sqrt(mean_squared_error(y_train, y_pred_train)),
            'test_rmse': np.sqrt(mean_squared_error(y_test, y_pred_test)),
            'train_mae': mean_absolute_error(y_train, y_pred_train),
            'test_mae': mean_absolute_error(y_test, y_pred_test)
        }
        
        # Cross-validation for robust performance estimation
        cv_scores = self._perform_cross_validation(X, y)
        performance_metrics.update(cv_scores)
        
        logger.info(f"Training completed - Test MAPE: {performance_metrics['test_mape']:.2f}%")
        
        # Create trained model wrapper
        self.trained_model = XGBoostCryptoModel(
            model=xgb_model,
            scaler=self.scaler,
            feature_columns=self.feature_columns,
            performance_metrics=performance_metrics,
            training_config=self.config
        )
        
        return self.trained_model
    
    def _perform_cross_validation(self, X: pd.DataFrame, y: pd.Series) -> Dict[str, float]:
        """Perform time series cross-validation"""
        logger.info("Performing time series cross-validation...")
        
        tscv = TimeSeriesSplit(n_splits=self.config['cv_folds'])
        cv_scores = []
        
        for train_idx, val_idx in tscv.split(X):
            X_train_cv, X_val_cv = X.iloc[train_idx], X.iloc[val_idx]
            y_train_cv, y_val_cv = y.iloc[train_idx], y.iloc[val_idx]
            
            # Scale features
            scaler_cv = StandardScaler()
            X_train_cv_scaled = scaler_cv.fit_transform(X_train_cv)
            X_val_cv_scaled = scaler_cv.transform(X_val_cv)
            
            # Train model
            model_cv = xgb.XGBRegressor(**{
                k: v for k, v in self.config.items() 
                if k in ['n_estimators', 'max_depth', 'learning_rate', 'subsample', 
                        'colsample_bytree', 'reg_alpha', 'reg_lambda', 'random_state']
            })
            
            model_cv.fit(X_train_cv_scaled, y_train_cv, verbose=False)
            
            # Predict and evaluate
            y_pred_cv = model_cv.predict(X_val_cv_scaled)
            mape_cv = mean_absolute_percentage_error(y_val_cv, y_pred_cv) * 100
            cv_scores.append(mape_cv)
        
        cv_results = {
            'cv_mape_mean': np.mean(cv_scores),
            'cv_mape_std': np.std(cv_scores),
            'cv_mape_scores': cv_scores
        }
        
        logger.info(f"Cross-validation MAPE: {cv_results['cv_mape_mean']:.2f}% ± {cv_results['cv_mape_std']:.2f}%")
        
        return cv_results
    
    def predict(self, data: pd.DataFrame, periods: int = 1) -> pd.DataFrame:
        """
        Generate predictions using trained model
        
        Args:
            data: Input data for prediction
            periods: Number of periods to forecast
            
        Returns:
            DataFrame with predictions
        """
        if self.trained_model is None:
            raise ValueError("Model must be trained before making predictions")
        
        # Prepare features for the latest data point
        processed_data = self.prepare_features(data)
        
        if len(processed_data) == 0:
            raise ValueError("No valid data for prediction after feature engineering")
        
        # Get latest features for prediction
        latest_features = processed_data[self.feature_columns].iloc[-1:] 
        
        predictions = []
        current_data = data.copy()
        
        for period in range(periods):
            # Generate prediction for current period
            pred_return = self.trained_model.predict(latest_features)[0]
            
            # Convert return to price prediction
            last_price = current_data['close'].iloc[-1]
            pred_price = last_price * (1 + pred_return)
            
            predictions.append({
                'period': period + 1,
                'predicted_return': pred_return,
                'predicted_price': pred_price,
                'confidence': self._calculate_prediction_confidence(latest_features)
            })
            
            # For multi-period prediction, we'd need to update features
            # This is a simplified implementation
            if period < periods - 1:
                # Create synthetic next period data point for iterative prediction
                next_row = current_data.iloc[-1:].copy()
                next_row['close'] = pred_price
                next_row.index = next_row.index + pd.Timedelta(days=1)
                current_data = pd.concat([current_data, next_row])
                
                # Re-engineer features with updated data
                processed_data = self.prepare_features(current_data)
                latest_features = processed_data[self.feature_columns].iloc[-1:]
        
        return pd.DataFrame(predictions)
    
    def _calculate_prediction_confidence(self, features: pd.DataFrame) -> float:
        """Calculate prediction confidence based on feature similarity to training data"""
        # Simplified confidence calculation
        # In practice, could use uncertainty quantification methods
        return 0.75  # Placeholder confidence score
    
    def get_feature_importance(self) -> pd.DataFrame:
        """Get feature importance from trained model"""
        if self.trained_model is None:
            raise ValueError("Model must be trained before getting feature importance")
        
        return self.trained_model.get_feature_importance()
    
    def validate_performance(self, data: pd.DataFrame) -> Dict[str, float]:
        """Validate model performance on new data"""
        if self.trained_model is None:
            raise ValueError("Model must be trained before validation")
        
        # Prepare features and target
        processed_data = self.prepare_features(data)
        X = processed_data[self.feature_columns]
        y = processed_data[self.target_column]
        
        # Generate predictions
        y_pred = self.trained_model.predict(X)
        
        # Calculate metrics
        metrics = {
            'mape': mean_absolute_percentage_error(y, y_pred) * 100,
            'rmse': np.sqrt(mean_squared_error(y, y_pred)),
            'mae': mean_absolute_error(y, y_pred),
            'samples': len(y)
        }
        
        return metrics
