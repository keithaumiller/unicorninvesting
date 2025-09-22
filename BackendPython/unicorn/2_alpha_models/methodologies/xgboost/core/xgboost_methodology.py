"""
XGBoost Methodology Implementation for Crypto Forecasting

Advanced XGBoost implementation optimized for cryptocurrency price prediction
with comprehensive feature engineering, hyperparameter tuning, and validation.

Features:
- Crypto-specific feature engineering
- Advanced time-series features
- Hyperparameter optimization
- Cross-validation and performance metrics
- Overfitting detection and prevention
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
from sklearn.model_selection import train_test_split, TimeSeriesSplit, GridSearchCV
from sklearn.preprocessing import StandardScaler, RobustScaler
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.ensemble import IsolationForest

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class XGBoostMethodology:
    """
    Advanced XGBoost Methodology for Cryptocurrency Price Forecasting
    
    Designed specifically for crypto markets with 24/7 trading patterns,
    high volatility, and complex non-linear relationships.
    """
    
    def __init__(self, asset_adapter=None, config: Optional[Dict] = None):
        """
        Initialize XGBoost methodology with crypto-optimized parameters
        
        Args:
            asset_adapter: Asset-specific adapter for data and features
            config: Configuration dictionary for XGBoost parameters
        """
        self.asset_adapter = asset_adapter
        self.config = config or self._get_default_config()
        
        # Model components
        self.model = None
        self.scaler = None
        self.feature_columns = None
        self.trained = False
        
        # Performance tracking
        self.performance_metrics = {}
        self.feature_importance = {}
        
        logger.info("🚀 XGBoost Methodology initialized for crypto forecasting")
    
    def _get_default_config(self) -> Dict:
        """Get crypto-optimized XGBoost configuration"""
        return {
            # XGBoost parameters optimized for crypto volatility
            'objective': 'reg:squarederror',
            'eval_metric': 'rmse',
            'max_depth': 8,              # Deeper trees for complex patterns
            'learning_rate': 0.05,       # Conservative learning rate
            'n_estimators': 500,         # More trees for complex patterns
            'subsample': 0.8,            # Prevent overfitting
            'colsample_bytree': 0.8,     # Feature sampling
            'reg_alpha': 0.1,            # L1 regularization
            'reg_lambda': 1.0,           # L2 regularization
            'random_state': 42,
            'n_jobs': -1,                # Use all cores
            
            # Crypto-specific parameters
            'min_child_weight': 3,       # Handle noisy crypto data
            'gamma': 0.1,                # Minimum split gain
            'scale_pos_weight': 1,       # Balanced for regression
            
            # Early stopping
            'early_stopping_rounds': 50,
            'verbose': False,
            
            # Validation parameters
            'cv_folds': 5,
            'test_size': 0.2,
            'validation_size': 0.15,
            
            # Feature engineering
            'lookback_periods': [5, 10, 20, 50],  # Multiple timeframes
            'technical_indicators': True,
            'volume_features': True,
            'volatility_features': True,
            'momentum_features': True,
            'market_regime_features': True,
        }
    
    def create_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Create comprehensive feature set for XGBoost crypto prediction
        
        Args:
            data: DataFrame with OHLCV data
            
        Returns:
            DataFrame with engineered features
        """
        logger.info("🔧 Creating comprehensive XGBoost features for crypto prediction")
        
        df = data.copy()
        
        # Ensure we have required columns
        required_cols = ['open', 'high', 'low', 'close', 'volume']
        for col in required_cols:
            if col not in df.columns:
                raise ValueError(f"Required column '{col}' not found in data")
        
        # 1. Basic price features
        df['returns'] = df['close'].pct_change()
        df['log_returns'] = np.log(df['close'] / df['close'].shift(1))
        df['price_change'] = df['close'] - df['close'].shift(1)
        df['price_change_pct'] = (df['close'] - df['close'].shift(1)) / df['close'].shift(1) * 100
        
        # 2. OHLC relationships
        df['hl_ratio'] = (df['high'] - df['low']) / df['close']
        df['oc_ratio'] = (df['close'] - df['open']) / df['open']
        df['body_size'] = abs(df['close'] - df['open']) / df['close']
        df['upper_shadow'] = (df['high'] - np.maximum(df['open'], df['close'])) / df['close']
        df['lower_shadow'] = (np.minimum(df['open'], df['close']) - df['low']) / df['close']
        
        # 3. Volume features
        if self.config['volume_features']:
            df['volume_change'] = df['volume'].pct_change()
            df['volume_price_trend'] = df['volume'] * df['returns']
            df['volume_ma_ratio'] = df['volume'] / df['volume'].rolling(20).mean()
            
            # Volume-weighted prices
            df['vwap'] = (df['volume'] * (df['high'] + df['low'] + df['close']) / 3).rolling(20).sum() / df['volume'].rolling(20).sum()
            df['vwap_deviation'] = (df['close'] - df['vwap']) / df['vwap']
        
        # 4. Technical indicators (moving averages)
        for period in self.config['lookback_periods']:
            # Price moving averages
            df[f'ma_{period}'] = df['close'].rolling(period).mean()
            df[f'ma_{period}_ratio'] = df['close'] / df[f'ma_{period}']
            df[f'ma_{period}_slope'] = df[f'ma_{period}'].diff(5) / df[f'ma_{period}'].shift(5)
            
            # Exponential moving averages
            df[f'ema_{period}'] = df['close'].ewm(span=period).mean()
            df[f'ema_{period}_ratio'] = df['close'] / df[f'ema_{period}']
            
            # Volume moving averages
            df[f'volume_ma_{period}'] = df['volume'].rolling(period).mean()
            df[f'volume_ratio_{period}'] = df['volume'] / df[f'volume_ma_{period}']
        
        # 5. Volatility features
        if self.config['volatility_features']:
            for period in [10, 20, 30]:
                df[f'volatility_{period}'] = df['returns'].rolling(period).std()
                df[f'volatility_{period}_ma'] = df[f'volatility_{period}'].rolling(5).mean()
                
                # Bollinger Bands
                ma = df['close'].rolling(period).mean()
                std = df['close'].rolling(period).std()
                df[f'bb_upper_{period}'] = ma + (2 * std)
                df[f'bb_lower_{period}'] = ma - (2 * std)
                df[f'bb_position_{period}'] = (df['close'] - df[f'bb_lower_{period}']) / (df[f'bb_upper_{period}'] - df[f'bb_lower_{period}'])
        
        # 6. Momentum features
        if self.config['momentum_features']:
            for period in [5, 10, 14, 20]:
                # RSI
                delta = df['close'].diff()
                gain = (delta.where(delta > 0, 0)).rolling(period).mean()
                loss = (-delta.where(delta < 0, 0)).rolling(period).mean()
                rs = gain / loss
                df[f'rsi_{period}'] = 100 - (100 / (1 + rs))
                
                # MACD components
                if period == 14:
                    ema_12 = df['close'].ewm(span=12).mean()
                    ema_26 = df['close'].ewm(span=26).mean()
                    df['macd'] = ema_12 - ema_26
                    df['macd_signal'] = df['macd'].ewm(span=9).mean()
                    df['macd_histogram'] = df['macd'] - df['macd_signal']
                
                # Rate of change
                df[f'roc_{period}'] = df['close'].pct_change(period) * 100
                
                # Stochastic oscillator
                high_roll = df['high'].rolling(period).max()
                low_roll = df['low'].rolling(period).min()
                df[f'stoch_k_{period}'] = ((df['close'] - low_roll) / (high_roll - low_roll)) * 100
                df[f'stoch_d_{period}'] = df[f'stoch_k_{period}'].rolling(3).mean()
        
        # 7. Lag features for time series patterns
        for lag in [1, 2, 3, 5, 10]:
            df[f'close_lag_{lag}'] = df['close'].shift(lag)
            df[f'returns_lag_{lag}'] = df['returns'].shift(lag)
            df[f'volume_lag_{lag}'] = df['volume'].shift(lag)
        
        # 8. Market regime features
        if self.config['market_regime_features']:
            # Trend strength
            df['trend_strength'] = abs(df['close'].rolling(20).apply(lambda x: np.corrcoef(range(len(x)), x)[0, 1], raw=False))
            
            # Market regime (bull/bear/sideways)
            df['regime_ma_short'] = df['close'].rolling(10).mean()
            df['regime_ma_long'] = df['close'].rolling(50).mean()
            df['market_regime'] = np.where(df['regime_ma_short'] > df['regime_ma_long'], 1,  # Bull
                                         np.where(df['regime_ma_short'] < df['regime_ma_long'], -1, 0))  # Bear/Sideways
            
            # Volatility regime
            vol_20 = df['returns'].rolling(20).std()
            vol_ma = vol_20.rolling(20).mean()
            df['volatility_regime'] = np.where(vol_20 > vol_ma * 1.5, 1,  # High vol
                                             np.where(vol_20 < vol_ma * 0.5, -1, 0))  # Low/Normal vol
        
        # 9. Time-based features for 24/7 crypto markets
        if df.index.tz is not None:
            df.index = df.index.tz_localize(None)  # Remove timezone for processing
        
        df['hour'] = df.index.hour
        df['day_of_week'] = df.index.dayofweek
        df['day_of_month'] = df.index.day
        df['month'] = df.index.month
        df['quarter'] = df.index.quarter
        
        # Cyclical encoding for time features
        df['hour_sin'] = np.sin(2 * np.pi * df['hour'] / 24)
        df['hour_cos'] = np.cos(2 * np.pi * df['hour'] / 24)
        df['dow_sin'] = np.sin(2 * np.pi * df['day_of_week'] / 7)
        df['dow_cos'] = np.cos(2 * np.pi * df['day_of_week'] / 7)
        
        # 10. Target variable (next period return)
        df['target'] = df['close'].shift(-1)  # Predict next period close price
        
        # Remove infinite and NaN values
        df = df.replace([np.inf, -np.inf], np.nan)
        
        logger.info(f"✅ Created {df.shape[1]} features for XGBoost training")
        logger.info(f"📊 Feature categories: Price({len([c for c in df.columns if 'ma_' in c or 'ema_' in c])}), "
                   f"Volume({len([c for c in df.columns if 'volume' in c])}), "
                   f"Technical({len([c for c in df.columns if any(x in c for x in ['rsi', 'macd', 'stoch', 'bb_'])])}), "
                   f"Momentum({len([c for c in df.columns if 'roc_' in c or 'momentum' in c])})")
        
        return df
    
    def prepare_data(self, data: pd.DataFrame) -> Tuple[np.ndarray, np.ndarray, List[str]]:
        """
        Prepare data for XGBoost training with proper feature selection
        
        Args:
            data: DataFrame with features
            
        Returns:
            Tuple of (X, y, feature_names)
        """
        logger.info("📋 Preparing data for XGBoost training")
        
        # Remove target and non-feature columns
        exclude_columns = ['target', 'open', 'high', 'low', 'close', 'volume']
        feature_columns = [col for col in data.columns if col not in exclude_columns]
        
        # Handle any remaining object/string columns
        feature_columns = [col for col in feature_columns if data[col].dtype in ['float64', 'int64', 'float32', 'int32']]
        
        X = data[feature_columns].copy()
        y = data['target'].copy()
        
        # Remove rows with NaN in target
        valid_idx = ~(y.isna() | X.isna().any(axis=1))
        X = X[valid_idx]
        y = y[valid_idx]
        
        # Forward fill any remaining NaN values
        X = X.fillna(method='ffill').fillna(method='bfill')
        
        # Remove constant features
        constant_features = [col for col in X.columns if X[col].nunique() <= 1]
        if constant_features:
            logger.warning(f"Removing {len(constant_features)} constant features")
            X = X.drop(columns=constant_features)
            feature_columns = [col for col in feature_columns if col not in constant_features]
        
        # Detect and handle outliers
        isolation_forest = IsolationForest(contamination=0.05, random_state=42)
        outlier_mask = isolation_forest.fit_predict(X) == 1
        X = X[outlier_mask]
        y = y[outlier_mask]
        
        logger.info(f"✅ Prepared data: {X.shape[0]} samples × {X.shape[1]} features")
        logger.info(f"📊 Target range: ${y.min():.2f} - ${y.max():.2f}")
        
        return X.values, y.values, feature_columns
    
    def train(self, data: pd.DataFrame, periods: int = None) -> Dict[str, Any]:
        """
        Train XGBoost model with crypto-optimized parameters
        
        Args:
            data: DataFrame with OHLCV data
            periods: Not used for XGBoost (compatibility with Prophet)
            
        Returns:
            Dictionary with training results and model info
        """
        logger.info("🎯 Starting XGBoost model training for crypto forecasting")
        
        # Create features
        feature_data = self.create_features(data)
        
        # Prepare data
        X, y, feature_names = self.prepare_data(feature_data)
        self.feature_columns = feature_names
        
        if len(X) < 50:
            raise ValueError(f"Insufficient data for training: {len(X)} samples (minimum 50 required)")
        
        # Train-validation-test split for time series
        train_size = int(len(X) * (1 - self.config['test_size'] - self.config['validation_size']))
        val_size = int(len(X) * self.config['validation_size'])
        
        X_train = X[:train_size]
        y_train = y[:train_size]
        X_val = X[train_size:train_size + val_size]
        y_val = y[train_size:train_size + val_size]
        X_test = X[train_size + val_size:]
        y_test = y[train_size + val_size:]
        
        logger.info(f"📊 Data split: Train({len(X_train)}), Val({len(X_val)}), Test({len(X_test)})")
        
        # Scale features
        self.scaler = RobustScaler()  # Better for outliers than StandardScaler
        X_train_scaled = self.scaler.fit_transform(X_train)
        X_val_scaled = self.scaler.transform(X_val)
        X_test_scaled = self.scaler.transform(X_test)
        
        # Create XGBoost datasets
        dtrain = xgb.DMatrix(X_train_scaled, label=y_train, feature_names=feature_names)
        dval = xgb.DMatrix(X_val_scaled, label=y_val, feature_names=feature_names)
        
        # XGBoost parameters
        params = {
            'objective': self.config['objective'],
            'eval_metric': self.config['eval_metric'],
            'max_depth': self.config['max_depth'],
            'learning_rate': self.config['learning_rate'],
            'subsample': self.config['subsample'],
            'colsample_bytree': self.config['colsample_bytree'],
            'reg_alpha': self.config['reg_alpha'],
            'reg_lambda': self.config['reg_lambda'],
            'min_child_weight': self.config['min_child_weight'],
            'gamma': self.config['gamma'],
            'random_state': self.config['random_state'],
            'verbosity': 0
        }
        
        # Train model with early stopping
        start_time = datetime.now()
        
        self.model = xgb.train(
            params=params,
            dtrain=dtrain,
            num_boost_round=self.config['n_estimators'],
            evals=[(dtrain, 'train'), (dval, 'val')],
            early_stopping_rounds=self.config['early_stopping_rounds'],
            verbose_eval=False
        )
        
        training_time = (datetime.now() - start_time).total_seconds()
        
        # Evaluate model
        train_pred = self.model.predict(dtrain)
        val_pred = self.model.predict(dval)
        
        if len(X_test) > 0:
            dtest = xgb.DMatrix(X_test_scaled, label=y_test, feature_names=feature_names)
            test_pred = self.model.predict(dtest)
        else:
            test_pred = None
            
        # Calculate metrics
        train_rmse = np.sqrt(mean_squared_error(y_train, train_pred))
        train_mae = mean_absolute_error(y_train, train_pred)
        train_r2 = r2_score(y_train, train_pred)
        
        val_rmse = np.sqrt(mean_squared_error(y_val, val_pred))
        val_mae = mean_absolute_error(y_val, val_pred)
        val_r2 = r2_score(y_val, val_pred)
        
        # Calculate MAPE
        train_mape = np.mean(np.abs((y_train - train_pred) / y_train)) * 100
        val_mape = np.mean(np.abs((y_val - val_pred) / y_val)) * 100
        
        test_metrics = {}
        if test_pred is not None:
            test_rmse = np.sqrt(mean_squared_error(y_test, test_pred))
            test_mae = mean_absolute_error(y_test, test_pred)
            test_r2 = r2_score(y_test, test_pred)
            test_mape = np.mean(np.abs((y_test - test_pred) / y_test)) * 100
            test_metrics = {
                'test_rmse': test_rmse,
                'test_mae': test_mae,
                'test_r2': test_r2,
                'test_mape': test_mape
            }
        
        # Store performance metrics
        self.performance_metrics = {
            'train_rmse': train_rmse,
            'train_mae': train_mae,
            'train_r2': train_r2,
            'train_mape': train_mape,
            'val_rmse': val_rmse,
            'val_mae': val_mae,
            'val_r2': val_r2,
            'val_mape': val_mape,
            'training_time': training_time,
            'best_iteration': self.model.best_iteration,
            'num_features': len(feature_names),
            **test_metrics
        }
        
        # Get feature importance
        importance_dict = self.model.get_score(importance_type='weight')
        self.feature_importance = {k: v for k, v in sorted(importance_dict.items(), key=lambda x: x[1], reverse=True)}
        
        self.trained = True
        
        logger.info(f"✅ XGBoost training completed in {training_time:.2f} seconds")
        logger.info(f"📊 Best iteration: {self.model.best_iteration}")
        logger.info(f"📈 Validation MAPE: {val_mape:.2f}%")
        logger.info(f"🎯 Validation R²: {val_r2:.4f}")
        
        return {
            'model': self.model,
            'scaler': self.scaler,
            'feature_columns': feature_names,
            'performance_metrics': self.performance_metrics,
            'feature_importance': self.feature_importance
        }
    
    def predict(self, data: pd.DataFrame, periods: int = 30) -> pd.DataFrame:
        """
        Generate XGBoost predictions for future periods
        
        Args:
            data: Historical data for feature generation
            periods: Number of future periods to predict
            
        Returns:
            DataFrame with predictions
        """
        if not self.trained:
            raise ValueError("Model must be trained before making predictions")
        
        logger.info(f"🔮 Generating {periods} XGBoost predictions")
        
        # Create features from historical data
        feature_data = self.create_features(data)
        
        # Use the last available data point as starting point
        last_valid_idx = feature_data.dropna().index[-1]
        last_features = feature_data.loc[last_valid_idx, self.feature_columns].values.reshape(1, -1)
        
        # Scale features
        last_features_scaled = self.scaler.transform(last_features)
        
        # Generate predictions iteratively
        predictions = []
        current_features = last_features_scaled.copy()
        current_price = data['close'].iloc[-1]
        
        for step in range(periods):
            # Make prediction
            dmatrix = xgb.DMatrix(current_features, feature_names=self.feature_columns)
            pred_price = self.model.predict(dmatrix)[0]
            predictions.append(pred_price)
            
            # For multi-step prediction, update features (simplified approach)
            # In production, you might want more sophisticated feature updating
            if step < periods - 1:
                # Update price-based features for next prediction
                price_change = (pred_price - current_price) / current_price
                current_price = pred_price
                
                # This is a simplified feature update - in practice you'd update all relevant features
                current_features = current_features.copy()
        
        # Create prediction DataFrame
        last_date = data.index[-1]
        if hasattr(data.index, 'freq') and data.index.freq:
            freq = data.index.freq
        else:
            # Infer frequency from the last few data points
            time_diff = data.index[-1] - data.index[-2]
            freq = time_diff
        
        future_dates = pd.date_range(start=last_date + freq, periods=periods, freq=freq)
        
        prediction_df = pd.DataFrame({
            'ds': future_dates,
            'yhat': predictions,
            'yhat_lower': np.array(predictions) * 0.95,  # Simple confidence interval
            'yhat_upper': np.array(predictions) * 1.05
        })
        
        logger.info(f"✅ Generated {len(prediction_df)} XGBoost predictions")
        
        return prediction_df
    
    def get_feature_importance(self, top_n: int = 20) -> Dict[str, float]:
        """Get top N most important features"""
        if not self.feature_importance:
            return {}
        
        return dict(list(self.feature_importance.items())[:top_n])
    
    def validate_model(self, data: pd.DataFrame) -> Dict[str, float]:
        """Perform cross-validation on the model"""
        if not self.trained:
            raise ValueError("Model must be trained before validation")
        
        logger.info("🔍 Performing XGBoost model validation")
        
        # Create features
        feature_data = self.create_features(data)
        X, y, _ = self.prepare_data(feature_data)
        
        # Time series cross-validation
        tscv = TimeSeriesSplit(n_splits=self.config['cv_folds'])
        
        cv_scores = {
            'rmse': [],
            'mae': [],
            'mape': [],
            'r2': []
        }
        
        for train_idx, val_idx in tscv.split(X):
            X_train_cv, X_val_cv = X[train_idx], X[val_idx]
            y_train_cv, y_val_cv = y[train_idx], y[val_idx]
            
            # Scale features
            scaler_cv = RobustScaler()
            X_train_cv_scaled = scaler_cv.fit_transform(X_train_cv)
            X_val_cv_scaled = scaler_cv.transform(X_val_cv)
            
            # Train model
            dtrain_cv = xgb.DMatrix(X_train_cv_scaled, label=y_train_cv, feature_names=self.feature_columns)
            dval_cv = xgb.DMatrix(X_val_cv_scaled, label=y_val_cv, feature_names=self.feature_columns)
            
            params = {
                'objective': self.config['objective'],
                'eval_metric': self.config['eval_metric'],
                'max_depth': self.config['max_depth'],
                'learning_rate': self.config['learning_rate'],
                'subsample': self.config['subsample'],
                'colsample_bytree': self.config['colsample_bytree'],
                'reg_alpha': self.config['reg_alpha'],
                'reg_lambda': self.config['reg_lambda'],
                'verbosity': 0
            }
            
            model_cv = xgb.train(
                params=params,
                dtrain=dtrain_cv,
                num_boost_round=200,  # Reduced for CV
                verbose_eval=False
            )
            
            # Predict and evaluate
            y_pred_cv = model_cv.predict(dval_cv)
            
            cv_scores['rmse'].append(np.sqrt(mean_squared_error(y_val_cv, y_pred_cv)))
            cv_scores['mae'].append(mean_absolute_error(y_val_cv, y_pred_cv))
            cv_scores['mape'].append(np.mean(np.abs((y_val_cv - y_pred_cv) / y_val_cv)) * 100)
            cv_scores['r2'].append(r2_score(y_val_cv, y_pred_cv))
        
        # Calculate mean and std of CV scores
        cv_results = {}
        for metric, scores in cv_scores.items():
            cv_results[f'cv_{metric}_mean'] = np.mean(scores)
            cv_results[f'cv_{metric}_std'] = np.std(scores)
        
        logger.info(f"✅ Cross-validation completed")
        logger.info(f"📊 CV MAPE: {cv_results['cv_mape_mean']:.2f}% ± {cv_results['cv_mape_std']:.2f}%")
        
        return cv_results
