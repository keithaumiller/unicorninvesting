"""
XGBoost Methodology Implementation

Complete XGBoost methodology implementation for 6-month ETH forecasting.
Migrated from legacy implementations with crypto-specific optimizations.

Features:
- Crypto-specific feature engineering (price patterns, volatility, volume)
- Technical indicators integration (RSI, MACD, Bollinger Bands)
- Lag features for time series prediction
- Cross-validation and hyperparameter tuning
- Daily predictions for 6-month horizon
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional, Union, Tuple
import warnings
warnings.filterwarnings('ignore')

try:
    import xgboost as xgb
    XGBOOST_AVAILABLE = True
except ImportError:
    XGBOOST_AVAILABLE = False
    print("XGBoost not available - using mock implementation")

from sklearn.model_selection import TimeSeriesSplit, cross_val_score
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import logging

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class XGBoostMethodology:
    """
    XGBoost Methodology for ETH 6-Month Daily Forecasting
    
    Complete implementation with crypto-specific optimizations:
    - Feature engineering for cryptocurrency markets
    - Technical indicators and lag features
    - Cross-validation for time series
    - 6-month daily predictions
    """
    
    def __init__(self, asset: str = "ETH", forecast_horizon: int = 180):
        """
        Initialize XGBoost methodology
        
        Args:
            asset: Asset symbol (ETH, BTC, etc.)
            forecast_horizon: Number of days to forecast (default: 180 for 6 months)
        """
        self.asset = asset
        self.forecast_horizon = forecast_horizon
        self.is_trained = False
        self.model = None
        self.scaler = StandardScaler()
        self.feature_names = []
        self.training_history = {}
        
        # Crypto-specific parameters
        self.crypto_params = {
            'objective': 'reg:squarederror',
            'eval_metric': 'rmse',
            'n_estimators': 1000,
            'max_depth': 6,
            'learning_rate': 0.1,
            'subsample': 0.8,
            'colsample_bytree': 0.8,
            'random_state': 42,
            'early_stopping_rounds': 50,
            'verbosity': 0
        }
        
        logger.info(f"Initialized XGBoost methodology for {asset} with {forecast_horizon}-day horizon")
    
    def create_crypto_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Create comprehensive crypto-specific features
        
        Args:
            data: OHLCV DataFrame with datetime index
            
        Returns:
            DataFrame with engineered features
        """
        df = data.copy()
        
        try:
            logger.info("Creating crypto-specific features...")
            
            # Price-based features
            df['price_change'] = df['close'].pct_change()
            df['price_change_abs'] = df['price_change'].abs()
            df['high_low_spread'] = (df['high'] - df['low']) / df['close']
            df['open_close_spread'] = (df['close'] - df['open']) / df['open']
            
            # Volatility features (multiple windows)
            for window in [7, 14, 30]:
                df[f'volatility_{window}'] = df['price_change'].rolling(window).std()
                df[f'price_range_{window}'] = (df['high'].rolling(window).max() - 
                                             df['low'].rolling(window).min()) / df['close']
            
            # Volume features
            df['volume_change'] = df['volume'].pct_change()
            df['price_volume_trend'] = df['price_change'] * df['volume_change']
            df['volume_weighted_price'] = (df['volume'] * df['close']) / df['volume'].rolling(20).sum()
            
            # Moving averages and trends
            for ma_period in [7, 14, 30, 50]:
                df[f'ma_{ma_period}'] = df['close'].rolling(ma_period).mean()
                df[f'price_ma_ratio_{ma_period}'] = df['close'] / df[f'ma_{ma_period}']
                df[f'ma_trend_{ma_period}'] = df[f'ma_{ma_period}'].pct_change()
            
            # Technical indicators
            df = self._add_technical_indicators(df)
            
            # Lag features for time series
            df = self._add_lag_features(df)
            
            # Time-based features (crypto trades 24/7)
            df = self._add_time_features(df)
            
            # Clean and validate features
            df = self._clean_features(df)
            
            logger.info(f"Created {len(df.columns)} features for XGBoost training")
            
            return df
            
        except Exception as e:
            logger.error(f"Error creating features: {e}")
            return data
    
    def _add_technical_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add technical indicators optimized for crypto"""
        try:
            # RSI (Relative Strength Index)
            delta = df['close'].diff()
            gain = delta.where(delta > 0, 0).rolling(14).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(14).mean()
            rs = gain / loss
            df['rsi'] = 100 - (100 / (1 + rs))
            
            # MACD
            ema_12 = df['close'].ewm(span=12).mean()
            ema_26 = df['close'].ewm(span=26).mean()
            df['macd'] = ema_12 - ema_26
            df['macd_signal'] = df['macd'].ewm(span=9).mean()
            df['macd_histogram'] = df['macd'] - df['macd_signal']
            
            # Bollinger Bands
            for period in [20, 50]:
                bb_middle = df['close'].rolling(period).mean()
                bb_std = df['close'].rolling(period).std()
                df[f'bb_upper_{period}'] = bb_middle + (bb_std * 2)
                df[f'bb_lower_{period}'] = bb_middle - (bb_std * 2)
                df[f'bb_position_{period}'] = (df['close'] - df[f'bb_lower_{period}']) / (
                    df[f'bb_upper_{period}'] - df[f'bb_lower_{period}'])
                df[f'bb_width_{period}'] = (df[f'bb_upper_{period}'] - df[f'bb_lower_{period}']) / bb_middle
            
            # Momentum indicators
            for period in [5, 10, 20]:
                df[f'momentum_{period}'] = df['close'] / df['close'].shift(period) - 1
                df[f'roc_{period}'] = df['close'].pct_change(period)
            
            # Volume indicators
            df['obv'] = (df['volume'] * np.sign(df['close'].diff())).cumsum()
            df['obv_change'] = df['obv'].pct_change()
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding technical indicators: {e}")
            return df
    
    def _add_lag_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add lag features for time series prediction"""
        try:
            # Price lags
            for lag in [1, 2, 3, 5, 7, 14]:
                df[f'close_lag_{lag}'] = df['close'].shift(lag)
                df[f'volume_lag_{lag}'] = df['volume'].shift(lag)
                df[f'volatility_lag_{lag}'] = df['volatility_14'].shift(lag)
            
            # Price change lags
            for lag in [1, 2, 3, 5]:
                df[f'price_change_lag_{lag}'] = df['price_change'].shift(lag)
            
            # Moving average lags
            for lag in [1, 3, 7]:
                df[f'ma_14_lag_{lag}'] = df['ma_14'].shift(lag)
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding lag features: {e}")
            return df
    
    def _add_time_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add time-based features for crypto (24/7 markets)"""
        try:
            # Day of week (crypto trades 24/7 but may have patterns)
            df['day_of_week'] = df.index.dayofweek
            df['is_weekend'] = (df['day_of_week'] >= 5).astype(int)
            
            # Hour of day (if datetime has hour info)
            if hasattr(df.index, 'hour'):
                df['hour'] = df.index.hour
                df['is_us_trading_hours'] = ((df['hour'] >= 14) & (df['hour'] <= 21)).astype(int)
                df['is_asia_trading_hours'] = ((df['hour'] >= 0) & (df['hour'] <= 8)).astype(int)
            
            # Day of month (potential month-end effects)
            df['day_of_month'] = df.index.day
            df['is_month_end'] = (df.index.day >= 28).astype(int)
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding time features: {e}")
            return df
    
    def _clean_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Clean and validate features"""
        try:
            # Remove infinite values
            df = df.replace([np.inf, -np.inf], np.nan)
            
            # Forward fill then backward fill missing values
            df = df.fillna(method='ffill').fillna(method='bfill')
            
            # Remove any remaining NaN rows
            initial_rows = len(df)
            df = df.dropna()
            dropped_rows = initial_rows - len(df)
            
            if dropped_rows > 0:
                logger.warning(f"Dropped {dropped_rows} rows with missing values")
            
            return df
            
        except Exception as e:
            logger.error(f"Error cleaning features: {e}")
            return df
    
    def prepare_features_for_training(self, df: pd.DataFrame, target_col: str = 'close') -> Tuple[np.ndarray, np.ndarray, List[str]]:
        """
        Prepare features and target for XGBoost training
        
        Args:
            df: DataFrame with engineered features
            target_col: Target column name
            
        Returns:
            Tuple of (features, target, feature_names)
        """
        try:
            # Define feature columns (exclude target and identifier columns)
            exclude_cols = [target_col, 'open', 'high', 'low', 'volume'] if target_col == 'close' else [target_col]
            feature_cols = [col for col in df.columns if col not in exclude_cols]
            
            # Create target variable (next day's close price)
            target = df[target_col].shift(-1).dropna()
            
            # Align features with target
            features_df = df[feature_cols].iloc[:-1]  # Remove last row to align with target
            
            # Store feature names
            self.feature_names = feature_cols
            
            # Convert to numpy arrays
            X = features_df.values
            y = target.values
            
            logger.info(f"Prepared {X.shape[0]} samples with {X.shape[1]} features for training")
            
            return X, y, feature_cols
            
        except Exception as e:
            logger.error(f"Error preparing features: {e}")
            return np.array([]), np.array([]), []
    
    def train_model(self, X: np.ndarray, y: np.ndarray, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train XGBoost model with cross-validation
        
        Args:
            X: Feature matrix
            y: Target vector
            validation_split: Fraction for validation
            
        Returns:
            Training results and metrics
        """
        try:
            if not XGBOOST_AVAILABLE:
                logger.error("XGBoost not available - cannot train model")
                return {'status': 'failed', 'error': 'XGBoost not installed'}
            
            logger.info("Training XGBoost model...")
            
            # Time series split for validation
            split_idx = int(len(X) * (1 - validation_split))
            X_train, X_val = X[:split_idx], X[split_idx:]
            y_train, y_val = y[:split_idx], y[split_idx:]
            
            # Scale features
            X_train_scaled = self.scaler.fit_transform(X_train)
            X_val_scaled = self.scaler.transform(X_val)
            
            # Create XGBoost datasets
            dtrain = xgb.DMatrix(X_train_scaled, label=y_train, feature_names=self.feature_names)
            dval = xgb.DMatrix(X_val_scaled, label=y_val, feature_names=self.feature_names)
            
            # Train model
            start_time = datetime.now()
            self.model = xgb.train(
                self.crypto_params,
                dtrain,
                evals=[(dtrain, 'train'), (dval, 'val')],
                verbose_eval=False
            )
            training_time = (datetime.now() - start_time).total_seconds()
            
            # Make predictions for evaluation
            train_pred = self.model.predict(dtrain)
            val_pred = self.model.predict(dval)
            
            # Calculate metrics
            train_metrics = self._calculate_metrics(y_train, train_pred)
            val_metrics = self._calculate_metrics(y_val, val_pred)
            
            # Store training history
            self.training_history = {
                'training_time': training_time,
                'train_metrics': train_metrics,
                'val_metrics': val_metrics,
                'n_features': X.shape[1],
                'n_samples': X.shape[0]
            }
            
            self.is_trained = True
            
            logger.info(f"Model training completed in {training_time:.2f} seconds")
            logger.info(f"Validation RMSE: {val_metrics['rmse']:.2f}")
            logger.info(f"Validation MAPE: {val_metrics['mape']:.2f}%")
            
            return {
                'status': 'success',
                'training_time': training_time,
                'train_metrics': train_metrics,
                'val_metrics': val_metrics,
                'feature_importance': dict(zip(self.feature_names, self.model.get_score().values())) if hasattr(self.model, 'get_score') else {}
            }
            
        except Exception as e:
            logger.error(f"Error training model: {e}")
            return {'status': 'failed', 'error': str(e)}
    
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
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        """
        Make predictions using trained model
        
        Args:
            X: Feature matrix
            
        Returns:
            Predictions array
        """
        try:
            if not self.is_trained or self.model is None:
                raise ValueError("Model must be trained before making predictions")
            
            # Scale features
            X_scaled = self.scaler.transform(X)
            
            # Create DMatrix and predict
            dtest = xgb.DMatrix(X_scaled, feature_names=self.feature_names)
            predictions = self.model.predict(dtest)
            
            return predictions
            
        except Exception as e:
            logger.error(f"Error making predictions: {e}")
            return np.array([])
    
    def forecast_6_months(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Generate 6-month daily ETH forecast
        
        Args:
            data: Historical ETH data with features
            
        Returns:
            Forecast results with predictions and metadata
        """
        try:
            logger.info(f"Generating {self.forecast_horizon}-day forecast...")
            
            # Use the most recent data for forecasting
            latest_data = data.tail(1).copy()
            current_price = latest_data['close'].iloc[0]
            
            # Generate iterative predictions
            forecast_dates = []
            forecast_prices = []
            
            # Start from the day after the last data point
            start_date = data.index[-1] + timedelta(days=1)
            
            for i in range(self.forecast_horizon):
                forecast_date = start_date + timedelta(days=i)
                forecast_dates.append(forecast_date)
                
                # Prepare features for prediction
                features = latest_data[self.feature_names].values.reshape(1, -1)
                
                # Make prediction
                prediction = self.predict(features)[0]
                forecast_prices.append(prediction)
                
                # Update latest_data for next iteration (simple approach)
                # In a more sophisticated approach, we would update all features
                latest_data['close'] = prediction
            
            # Calculate forecast statistics
            price_change_pct = ((forecast_prices[-1] - current_price) / current_price) * 100
            
            # Create forecast DataFrame
            forecast_df = pd.DataFrame({
                'date': forecast_dates,
                'predicted_price': forecast_prices
            })
            
            logger.info(f"6-month forecast completed")
            logger.info(f"Current ETH Price: ${current_price:.2f}")
            logger.info(f"6-month forecast: ${forecast_prices[-1]:.2f}")
            logger.info(f"Expected change: {price_change_pct:+.2f}%")
            
            return {
                'status': 'success',
                'forecast_horizon': self.forecast_horizon,
                'current_price': current_price,
                'forecast_final': forecast_prices[-1],
                'price_change_pct': price_change_pct,
                'forecast_data': forecast_df,
                'methodology': 'XGBoost',
                'model_metrics': self.training_history.get('val_metrics', {}),
                'timestamp': datetime.now()
            }
            
        except Exception as e:
            logger.error(f"Error generating forecast: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def get_feature_importance(self) -> Dict[str, float]:
        """Get feature importance from trained model"""
        try:
            if not self.is_trained or self.model is None:
                return {}
            
            if hasattr(self.model, 'get_score'):
                importance = self.model.get_score(importance_type='weight')
                return dict(sorted(importance.items(), key=lambda x: x[1], reverse=True))
            else:
                return {}
                
        except Exception as e:
            logger.error(f"Error getting feature importance: {e}")
            return {}
    
    def cross_validate(self, X: np.ndarray, y: np.ndarray, cv_folds: int = 5) -> Dict[str, Any]:
        """
        Perform time series cross-validation
        
        Args:
            X: Feature matrix
            y: Target vector
            cv_folds: Number of CV folds
            
        Returns:
            Cross-validation results
        """
        try:
            if not XGBOOST_AVAILABLE:
                return {'status': 'failed', 'error': 'XGBoost not available'}
            
            logger.info(f"Performing {cv_folds}-fold time series cross-validation...")
            
            # Use TimeSeriesSplit for time series data
            tscv = TimeSeriesSplit(n_splits=cv_folds)
            
            cv_scores = []
            fold_results = []
            
            for fold, (train_idx, val_idx) in enumerate(tscv.split(X)):
                X_train_fold, X_val_fold = X[train_idx], X[val_idx]
                y_train_fold, y_val_fold = y[train_idx], y[val_idx]
                
                # Scale features
                scaler_fold = StandardScaler()
                X_train_scaled = scaler_fold.fit_transform(X_train_fold)
                X_val_scaled = scaler_fold.transform(X_val_fold)
                
                # Train model
                dtrain_fold = xgb.DMatrix(X_train_scaled, label=y_train_fold)
                model_fold = xgb.train(self.crypto_params, dtrain_fold, verbose_eval=False)
                
                # Predict and evaluate
                dval_fold = xgb.DMatrix(X_val_scaled)
                val_pred = model_fold.predict(dval_fold)
                fold_metrics = self._calculate_metrics(y_val_fold, val_pred)
                
                cv_scores.append(fold_metrics['mape'])
                fold_results.append(fold_metrics)
                
                logger.info(f"Fold {fold + 1}: MAPE = {fold_metrics['mape']:.2f}%")
            
            cv_mean = np.mean(cv_scores)
            cv_std = np.std(cv_scores)
            
            logger.info(f"Cross-validation MAPE: {cv_mean:.2f} ± {cv_std:.2f}%")
            
            return {
                'status': 'success',
                'cv_mean_mape': cv_mean,
                'cv_std_mape': cv_std,
                'cv_scores': cv_scores,
                'fold_results': fold_results
            }
            
        except Exception as e:
            logger.error(f"Error in cross-validation: {e}")
            return {'status': 'failed', 'error': str(e)}
