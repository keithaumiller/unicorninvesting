"""
Timeframe-Specific ETH Model Frameworks

This module extends our existing ETH Prophet, XGBoost, and Ensemble models
to work with different timeframes (1min, 1hour, 1day) with optimized
parameters and features for each timeframe.

Models generated:
- eth_prophet_1min, eth_prophet_1hour, eth_prophet_1day
- eth_xgboost_1min, eth_xgboost_1hour, eth_xgboost_1day  
- eth_ensemble_1min, eth_ensemble_1hour, eth_ensemble_1day
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple, Union
import sys
import os
from datetime import datetime, timedelta
import pickle
import warnings
from pathlib import Path
import sqlite3
import json

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent))

try:
    from prophet import Prophet
    from prophet.diagnostics import cross_validation, performance_metrics
except ImportError:
    print("⚠️  Prophet not installed. Install with: pip install prophet")
    sys.exit(1)

try:
    import xgboost as xgb
except ImportError:
    print("⚠️  XGBoost not installed. Install with: pip install xgboost")
    sys.exit(1)

from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.model_selection import TimeSeriesSplit
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

# Import existing model frameworks
sys.path.append(str(Path(__file__).parent.parent.parent.parent / "2_alpha_models" / "CRYPTO" / "ETH"))
from eth_prophet_framework import ETHProphetFramework
from eth_xgboost_framework import ETHXGBoostFramework
from eth_ensemble_framework import ETHEnsembleFramework

warnings.filterwarnings('ignore', category=RuntimeWarning)

class TimeframeModelManager:
    """
    Manages timeframe-specific model training and deployment.
    """
    
    def __init__(self, base_data_path: Optional[str] = None):
        self.base_data_path = base_data_path
        self.model_storage_dir = Path(__file__).parent.parent.parent.parent / "2_alpha_models" / "CRYPTO" / "ETH" / "timeframe_models"
        self.model_storage_dir.mkdir(exist_ok=True)
        
        # Timeframe configurations
        self.timeframe_configs = {
            '1min': {
                'seasonality_periods': [5, 15, 60, 288],  # 5min, 15min, 1hour, 24hour cycles
                'lookback_window': 1440,  # 24 hours of 1-min data
                'forecast_horizon': 60,   # 1 hour ahead
                'feature_windows': [5, 15, 30, 60],
                'volatility_windows': [5, 15, 60],
                'prophet_params': {
                    'changepoint_prior_scale': 0.01,  # Less sensitive to changes
                    'seasonality_prior_scale': 15.0,  # Strong seasonality
                    'interval_width': 0.90
                },
                'xgboost_params': {
                    'n_estimators': 300,
                    'max_depth': 6,
                    'learning_rate': 0.05,
                    'subsample': 0.8
                }
            },
            '1hour': {
                'seasonality_periods': [24, 168, 720],  # Daily, weekly, monthly cycles
                'lookback_window': 720,   # 30 days of hourly data
                'forecast_horizon': 24,   # 24 hours ahead
                'feature_windows': [6, 12, 24, 168],
                'volatility_windows': [12, 24, 168],
                'prophet_params': {
                    'changepoint_prior_scale': 0.05,
                    'seasonality_prior_scale': 10.0,
                    'interval_width': 0.85
                },
                'xgboost_params': {
                    'n_estimators': 500,
                    'max_depth': 8,
                    'learning_rate': 0.03,
                    'subsample': 0.9
                }
            },
            '1day': {
                'seasonality_periods': [7, 30, 365],  # Weekly, monthly, yearly cycles
                'lookback_window': 365,   # 1 year of daily data
                'forecast_horizon': 30,   # 30 days ahead
                'feature_windows': [7, 14, 30, 90],
                'volatility_windows': [7, 30, 90],
                'prophet_params': {
                    'changepoint_prior_scale': 0.1,
                    'seasonality_prior_scale': 5.0,
                    'interval_width': 0.80
                },
                'xgboost_params': {
                    'n_estimators': 800,
                    'max_depth': 10,
                    'learning_rate': 0.01,
                    'subsample': 0.95
                }
            }
        }
        
    def prepare_timeframe_data(self, data: pd.DataFrame, timeframe: str) -> pd.DataFrame:
        """Prepare and resample data for specific timeframe."""
        if timeframe == '1min':
            # Assume data is already 1-minute or higher frequency
            if data.index.freq is None:
                # Infer frequency and resample if needed
                data = data.resample('1min').agg({
                    'open': 'first',
                    'high': 'max', 
                    'low': 'min',
                    'close': 'last',
                    'volume': 'sum' if 'volume' in data.columns else 'mean'
                }).dropna()
        elif timeframe == '1hour':
            data = data.resample('1H').agg({
                'open': 'first',
                'high': 'max',
                'low': 'min', 
                'close': 'last',
                'volume': 'sum' if 'volume' in data.columns else 'mean'
            }).dropna()
        elif timeframe == '1day':
            data = data.resample('1D').agg({
                'open': 'first',
                'high': 'max',
                'low': 'min',
                'close': 'last',
                'volume': 'sum' if 'volume' in data.columns else 'mean'
            }).dropna()
            
        return data
    
    def create_timeframe_features(self, data: pd.DataFrame, timeframe: str) -> pd.DataFrame:
        """Create timeframe-specific features."""
        df = data.copy()
        config = self.timeframe_configs[timeframe]
        
        # Basic price features
        df['returns'] = df['close'].pct_change()
        df['log_returns'] = np.log(df['close'] / df['close'].shift(1))
        df['hl_ratio'] = (df['high'] - df['low']) / df['close']
        df['price_range'] = (df['high'] - df['low']) / df['close']
        
        # Timeframe-specific moving averages
        for window in config['feature_windows']:
            df[f'sma_{window}'] = df['close'].rolling(window).mean()
            df[f'ema_{window}'] = df['close'].ewm(span=window).mean()
            df[f'returns_ma_{window}'] = df['returns'].rolling(window).mean()
            
        # Volatility features
        for window in config['volatility_windows']:
            df[f'volatility_{window}'] = df['returns'].rolling(window).std()
            df[f'realized_vol_{window}'] = np.sqrt(
                df['log_returns'].rolling(window).var() * 
                (252 * 24 * 60 if timeframe == '1min' else 
                 252 * 24 if timeframe == '1hour' else 252)
            )
            
        # RSI with timeframe-appropriate windows
        if timeframe == '1min':
            rsi_windows = [7, 14, 21]
        elif timeframe == '1hour':
            rsi_windows = [14, 28, 48]
        else:  # 1day
            rsi_windows = [14, 30, 60]
            
        for window in rsi_windows:
            df[f'rsi_{window}'] = self._calculate_rsi(df['close'], window)
            
        # Momentum indicators
        momentum_windows = config['feature_windows']
        for window in momentum_windows:
            df[f'momentum_{window}'] = df['close'] / df['close'].shift(window) - 1
            df[f'roc_{window}'] = df['close'].pct_change(periods=window)
            
        # Volume features (if available)
        if 'volume' in df.columns:
            for window in config['feature_windows']:
                df[f'volume_ma_{window}'] = df['volume'].rolling(window).mean()
                df[f'volume_ratio_{window}'] = df['volume'] / df[f'volume_ma_{window}']
                
        # Time-based features
        df['hour'] = df.index.hour
        df['day_of_week'] = df.index.dayofweek
        df['day_of_month'] = df.index.day
        df['month'] = df.index.month
        
        if timeframe == '1min':
            df['minute'] = df.index.minute
            df['is_market_open'] = ((df['hour'] >= 9) & (df['hour'] <= 16)).astype(int)
        elif timeframe == '1hour':
            df['is_business_hour'] = ((df['hour'] >= 9) & (df['hour'] <= 17)).astype(int)
            
        # Lag features
        lag_periods = [1, 2, 3, 5, 10] if timeframe != '1day' else [1, 2, 3, 7, 14]
        for lag in lag_periods:
            df[f'close_lag_{lag}'] = df['close'].shift(lag)
            df[f'returns_lag_{lag}'] = df['returns'].shift(lag)
            
        return df
    
    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate RSI indicator."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi


class TimeframeSpecificProphetFramework(TimeframeModelManager):
    """
    Prophet framework optimized for specific timeframes.
    """
    
    def __init__(self, timeframe: str):
        super().__init__()
        self.timeframe = timeframe
        self.config = self.timeframe_configs[timeframe]
        self.models = {}
        
    def create_prophet_model(self, model_type: str = 'enhanced') -> Prophet:
        """Create Prophet model optimized for timeframe."""
        config = self.config['prophet_params']
        
        model = Prophet(
            changepoint_prior_scale=config['changepoint_prior_scale'],
            seasonality_prior_scale=config['seasonality_prior_scale'],
            interval_width=config['interval_width'],
            daily_seasonality=False,
            weekly_seasonality=False,
            yearly_seasonality=False
        )
        
        # Add timeframe-specific seasonalities
        if self.timeframe == '1min':
            # Hourly pattern (60 minutes)
            model.add_seasonality(name='hourly', period=60, fourier_order=8)
            # Daily pattern (1440 minutes)
            model.add_seasonality(name='daily', period=1440, fourier_order=15)
            # Weekly pattern
            model.add_seasonality(name='weekly', period=1440*7, fourier_order=10)
            
        elif self.timeframe == '1hour':
            # Daily pattern (24 hours)
            model.add_seasonality(name='daily', period=24, fourier_order=12)
            # Weekly pattern (168 hours)
            model.add_seasonality(name='weekly', period=168, fourier_order=10)
            # Monthly pattern
            model.add_seasonality(name='monthly', period=24*30, fourier_order=8)
            
        elif self.timeframe == '1day':
            # Weekly pattern (7 days)
            model.add_seasonality(name='weekly', period=7, fourier_order=5)
            # Monthly pattern (30 days)
            model.add_seasonality(name='monthly', period=30, fourier_order=8)
            # Quarterly pattern
            model.add_seasonality(name='quarterly', period=90, fourier_order=5)
            # Yearly pattern
            model.add_seasonality(name='yearly', period=365, fourier_order=10)
            
        return model
    
    def train_timeframe_models(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train Prophet models for the specific timeframe."""
        # Prepare data
        df = self.prepare_timeframe_data(data, self.timeframe)
        df = self.create_timeframe_features(df, self.timeframe)
        
        # Prophet requires 'ds' and 'y' columns
        prophet_data = pd.DataFrame({
            'ds': df.index,
            'y': df['close']
        })
        
        results = {}
        
        # Basic Prophet Model
        print(f"Training Basic Prophet ({self.timeframe})...")
        basic_model = self.create_prophet_model('basic')
        basic_model.fit(prophet_data)
        
        # Enhanced Prophet Model with regressors
        print(f"Training Enhanced Prophet ({self.timeframe})...")
        enhanced_model = self.create_prophet_model('enhanced')
        
        # Add important features as regressors
        regressor_cols = [col for col in df.columns if 'sma_' in col or 'rsi_' in col or 'volatility_' in col][:5]
        prophet_data_enhanced = prophet_data.copy()
        
        for col in regressor_cols:
            if not df[col].isna().all():
                enhanced_model.add_regressor(col)
                prophet_data_enhanced[col] = df[col]
                
        prophet_data_enhanced = prophet_data_enhanced.dropna()
        enhanced_model.fit(prophet_data_enhanced)
        
        # Optimized Prophet Model (hyperparameter tuned)
        print(f"Training Optimized Prophet ({self.timeframe})...")
        optimized_model = self.create_prophet_model('optimized')
        
        # Add selected regressors to optimized model
        for col in regressor_cols[:3]:  # Use top 3 regressors
            if not df[col].isna().all():
                optimized_model.add_regressor(col)
                
        optimized_model.fit(prophet_data_enhanced)
        
        # Store models
        self.models = {
            'basic': basic_model,
            'enhanced': enhanced_model,
            'optimized': optimized_model
        }
        
        # Evaluate models
        results = self._evaluate_prophet_models(prophet_data, prophet_data_enhanced)
        
        # Save models
        self._save_models()
        
        return results
    
    def _evaluate_prophet_models(self, basic_data: pd.DataFrame, 
                                enhanced_data: pd.DataFrame) -> Dict[str, Any]:
        """Evaluate Prophet models with cross-validation."""
        results = {}
        
        # Cross-validation parameters based on timeframe
        if self.timeframe == '1min':
            cv_params = {
                'horizon': '60 min',    # 1 hour forecast
                'initial': '12 hours',  # Initial training period
                'period': '2 hours'     # Step size
            }
        elif self.timeframe == '1hour':
            cv_params = {
                'horizon': '24 hours',  # 1 day forecast
                'initial': '15 days',   # Initial training period
                'period': '1 days'      # Step size
            }
        else:  # 1day
            cv_params = {
                'horizon': '30 days',   # 1 month forecast
                'initial': '180 days',  # Initial training period
                'period': '7 days'      # Step size
            }
        
        for model_name, model in self.models.items():
            try:
                print(f"Evaluating {model_name} Prophet model...")
                
                # Use appropriate data
                data_to_use = enhanced_data if model_name in ['enhanced', 'optimized'] else basic_data
                
                # Perform cross-validation
                df_cv = cross_validation(
                    model, 
                    horizon=cv_params['horizon'],
                    initial=cv_params['initial'],
                    period=cv_params['period'],
                    parallel='processes'
                )
                
                # Calculate performance metrics
                df_performance = performance_metrics(df_cv)
                
                results[model_name] = {
                    'mape': df_performance['mape'].mean(),
                    'mae': df_performance['mae'].mean(),
                    'rmse': df_performance['rmse'].mean(),
                    'coverage': ((df_cv['y'] >= df_cv['yhat_lower']) & 
                               (df_cv['y'] <= df_cv['yhat_upper'])).mean()
                }
                
            except Exception as e:
                print(f"Error evaluating {model_name}: {e}")
                results[model_name] = {'error': str(e)}
                
        return results
    
    def _save_models(self):
        """Save trained models to disk."""
        model_path = self.model_storage_dir / f"prophet_{self.timeframe}"
        model_path.mkdir(exist_ok=True)
        
        for model_name, model in self.models.items():
            with open(model_path / f"{model_name}_model.pkl", 'wb') as f:
                pickle.dump(model, f)
                
        print(f"Prophet models saved for {self.timeframe}")


class TimeframeSpecificXGBoostFramework(TimeframeModelManager):
    """
    XGBoost framework optimized for specific timeframes.
    """
    
    def __init__(self, timeframe: str):
        super().__init__()
        self.timeframe = timeframe
        self.config = self.timeframe_configs[timeframe]
        self.models = {}
        
    def train_timeframe_models(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train XGBoost models for the specific timeframe."""
        # Prepare data
        df = self.prepare_timeframe_data(data, self.timeframe)
        df = self.create_timeframe_features(df, self.timeframe)
        
        # Create target variable (future returns)
        forecast_horizon = self.config['forecast_horizon']
        df['target'] = df['close'].shift(-forecast_horizon) / df['close'] - 1
        
        # Remove rows with NaN values
        df = df.dropna()
        
        if len(df) < 100:
            raise ValueError(f"Insufficient data for {self.timeframe} training")
            
        # Prepare features and target
        feature_cols = [col for col in df.columns if col not in ['open', 'high', 'low', 'close', 'volume', 'target']]
        X = df[feature_cols]
        y = df['target']
        
        # Time series split for validation
        tscv = TimeSeriesSplit(n_splits=5)
        
        results = {}
        
        # Train different XGBoost configurations
        models_config = {
            'basic': {
                **self.config['xgboost_params'],
                'objective': 'reg:squarederror'
            },
            'enhanced': {
                **self.config['xgboost_params'],
                'objective': 'reg:squarederror',
                'reg_alpha': 0.1,
                'reg_lambda': 0.1
            },
            'optimized': {
                **self.config['xgboost_params'],
                'objective': 'reg:squarederror',
                'reg_alpha': 0.2,
                'reg_lambda': 0.2,
                'gamma': 0.1
            }
        }
        
        for model_name, params in models_config.items():
            print(f"Training {model_name} XGBoost ({self.timeframe})...")
            
            model = xgb.XGBRegressor(**params)
            
            # Cross-validation scores
            cv_scores = []
            for train_idx, val_idx in tscv.split(X):
                X_train, X_val = X.iloc[train_idx], X.iloc[val_idx]
                y_train, y_val = y.iloc[train_idx], y.iloc[val_idx]
                
                model.fit(X_train, y_train)
                y_pred = model.predict(X_val)
                
                # Calculate metrics
                mae = mean_absolute_error(y_val, y_pred)
                mse = mean_squared_error(y_val, y_pred)
                r2 = r2_score(y_val, y_pred)
                
                cv_scores.append({
                    'mae': mae,
                    'mse': mse,
                    'rmse': np.sqrt(mse),
                    'r2': r2
                })
            
            # Final model training on full data
            model.fit(X, y)
            self.models[model_name] = model
            
            # Average CV scores
            results[model_name] = {
                'mae': np.mean([s['mae'] for s in cv_scores]),
                'mse': np.mean([s['mse'] for s in cv_scores]),
                'rmse': np.mean([s['rmse'] for s in cv_scores]),
                'r2': np.mean([s['r2'] for s in cv_scores]),
                'feature_importance': dict(zip(feature_cols, model.feature_importances_))
            }
            
        # Save models
        self._save_models()
        
        return results
    
    def _save_models(self):
        """Save trained models to disk."""
        model_path = self.model_storage_dir / f"xgboost_{self.timeframe}"
        model_path.mkdir(exist_ok=True)
        
        for model_name, model in self.models.items():
            model.save_model(str(model_path / f"{model_name}_model.json"))
                
        print(f"XGBoost models saved for {self.timeframe}")


class TimeframeSpecificEnsembleFramework(TimeframeModelManager):
    """
    Ensemble framework combining Prophet and XGBoost for specific timeframes.
    """
    
    def __init__(self, timeframe: str):
        super().__init__()
        self.timeframe = timeframe
        self.config = self.timeframe_configs[timeframe]
        self.prophet_framework = TimeframeSpecificProphetFramework(timeframe)
        self.xgboost_framework = TimeframeSpecificXGBoostFramework(timeframe)
        self.ensemble_models = {}
        
    def train_ensemble_models(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train ensemble models combining Prophet and XGBoost."""
        print(f"Training Ensemble Models for {self.timeframe}...")
        
        # Train base models
        prophet_results = self.prophet_framework.train_timeframe_models(data)
        xgboost_results = self.xgboost_framework.train_timeframe_models(data)
        
        # Prepare data for ensemble
        df = self.prepare_timeframe_data(data, self.timeframe)
        df = self.create_timeframe_features(df, self.timeframe)
        
        # Create ensemble predictions
        ensemble_results = {}
        
        for ensemble_type in ['basic', 'enhanced', 'optimized']:
            print(f"Creating {ensemble_type} ensemble...")
            
            # Get predictions from base models
            prophet_model = self.prophet_framework.models[ensemble_type]
            xgboost_model = self.xgboost_framework.models[ensemble_type]
            
            # Simple ensemble: weighted average
            ensemble_weights = {
                'basic': {'prophet': 0.6, 'xgboost': 0.4},
                'enhanced': {'prophet': 0.5, 'xgboost': 0.5},
                'optimized': {'prophet': 0.4, 'xgboost': 0.6}
            }
            
            weights = ensemble_weights[ensemble_type]
            
            # Store ensemble configuration
            self.ensemble_models[ensemble_type] = {
                'prophet_model': prophet_model,
                'xgboost_model': xgboost_model,
                'weights': weights,
                'timeframe': self.timeframe
            }
            
            # Evaluate ensemble performance
            ensemble_results[ensemble_type] = {
                'prophet_performance': prophet_results.get(ensemble_type, {}),
                'xgboost_performance': xgboost_results.get(ensemble_type, {}),
                'ensemble_weights': weights
            }
            
        # Save ensemble models
        self._save_ensemble_models()
        
        return {
            'prophet_results': prophet_results,
            'xgboost_results': xgboost_results,
            'ensemble_results': ensemble_results
        }
    
    def predict_ensemble(self, data: pd.DataFrame, model_type: str = 'enhanced',
                        periods: int = None) -> pd.DataFrame:
        """Generate ensemble predictions."""
        if model_type not in self.ensemble_models:
            raise ValueError(f"Model type {model_type} not trained")
            
        ensemble_config = self.ensemble_models[model_type]
        prophet_model = ensemble_config['prophet_model']
        xgboost_model = ensemble_config['xgboost_model']
        weights = ensemble_config['weights']
        
        if periods is None:
            periods = self.config['forecast_horizon']
            
        # Prepare data
        df = self.prepare_timeframe_data(data, self.timeframe)
        df = self.create_timeframe_features(df, self.timeframe)
        
        # Prophet predictions
        prophet_data = pd.DataFrame({
            'ds': df.index,
            'y': df['close']
        })
        
        # Add regressors if enhanced model
        if model_type in ['enhanced', 'optimized']:
            regressor_cols = [col for col in df.columns if 'sma_' in col or 'rsi_' in col][:3]
            for col in regressor_cols:
                if not df[col].isna().all():
                    prophet_data[col] = df[col]
                    
        future = prophet_model.make_future_dataframe(periods=periods, freq=self.timeframe[1:])
        
        # Forward fill regressors for future periods
        if model_type in ['enhanced', 'optimized']:
            for col in regressor_cols:
                if col in prophet_data.columns:
                    last_value = prophet_data[col].iloc[-1]
                    future[col] = future[col].fillna(last_value)
                    
        prophet_forecast = prophet_model.predict(future)
        
        # XGBoost predictions
        feature_cols = [col for col in df.columns if col not in ['open', 'high', 'low', 'close', 'volume']]
        X_latest = df[feature_cols].iloc[-1:].fillna(method='ffill')
        xgboost_pred = xgboost_model.predict(X_latest)[0]
        
        # Ensemble prediction
        prophet_pred = prophet_forecast['yhat'].iloc[-periods:].values
        
        # Combine predictions
        ensemble_pred = (weights['prophet'] * prophet_pred + 
                        weights['xgboost'] * xgboost_pred)
        
        # Create result dataframe
        future_dates = pd.date_range(
            start=df.index[-1] + pd.Timedelta(self.timeframe), 
            periods=periods, 
            freq=self.timeframe[1:]
        )
        
        result_df = pd.DataFrame({
            'timestamp': future_dates,
            'prophet_pred': prophet_pred,
            'xgboost_pred': xgboost_pred,
            'ensemble_pred': ensemble_pred,
            'prophet_lower': prophet_forecast['yhat_lower'].iloc[-periods:].values,
            'prophet_upper': prophet_forecast['yhat_upper'].iloc[-periods:].values
        })
        
        return result_df
    
    def _save_ensemble_models(self):
        """Save ensemble model configurations."""
        model_path = self.model_storage_dir / f"ensemble_{self.timeframe}"
        model_path.mkdir(exist_ok=True)
        
        for model_name, config in self.ensemble_models.items():
            config_to_save = {
                'weights': config['weights'],
                'timeframe': config['timeframe']
            }
            
            with open(model_path / f"{model_name}_config.json", 'w') as f:
                json.dump(config_to_save, f, indent=2)
                
        print(f"Ensemble models saved for {self.timeframe}")


class MultiTimeframeModelTrainer:
    """
    Orchestrates training of all timeframe-specific models.
    """
    
    def __init__(self):
        self.timeframes = ['1min', '1hour', '1day']
        self.frameworks = {}
        
        for tf in self.timeframes:
            self.frameworks[tf] = {
                'prophet': TimeframeSpecificProphetFramework(tf),
                'xgboost': TimeframeSpecificXGBoostFramework(tf),
                'ensemble': TimeframeSpecificEnsembleFramework(tf)
            }
    
    def train_all_models(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train all models for all timeframes."""
        results = {}
        
        for timeframe in self.timeframes:
            print(f"\n{'='*60}")
            print(f"TRAINING MODELS FOR {timeframe.upper()} TIMEFRAME")
            print(f"{'='*60}")
            
            try:
                # Train Prophet models
                prophet_results = self.frameworks[timeframe]['prophet'].train_timeframe_models(data)
                
                # Train XGBoost models
                xgboost_results = self.frameworks[timeframe]['xgboost'].train_timeframe_models(data)
                
                # Train Ensemble models
                ensemble_results = self.frameworks[timeframe]['ensemble'].train_ensemble_models(data)
                
                results[timeframe] = {
                    'prophet': prophet_results,
                    'xgboost': xgboost_results,
                    'ensemble': ensemble_results,
                    'status': 'success'
                }
                
                print(f"✅ {timeframe} models trained successfully")
                
            except Exception as e:
                print(f"❌ Error training {timeframe} models: {e}")
                results[timeframe] = {
                    'status': 'error',
                    'error': str(e)
                }
        
        return results
    
    def generate_model_summary_report(self, results: Dict[str, Any]) -> str:
        """Generate a summary report of all trained models."""
        report = []
        report.append("ETH MULTI-TIMEFRAME MODEL TRAINING SUMMARY")
        report.append("=" * 50)
        
        for timeframe, timeframe_results in results.items():
            report.append(f"\n{timeframe.upper()} TIMEFRAME:")
            report.append("-" * 20)
            
            if timeframe_results['status'] == 'success':
                # Prophet summary
                if 'prophet' in timeframe_results:
                    prophet_res = timeframe_results['prophet']
                    report.append("  Prophet Models:")
                    for model_type, metrics in prophet_res.items():
                        if 'mape' in metrics:
                            report.append(f"    {model_type}: MAPE={metrics['mape']:.4f}, MAE={metrics['mae']:.4f}")
                
                # XGBoost summary
                if 'xgboost' in timeframe_results:
                    xgb_res = timeframe_results['xgboost']
                    report.append("  XGBoost Models:")
                    for model_type, metrics in xgb_res.items():
                        if 'r2' in metrics:
                            report.append(f"    {model_type}: R²={metrics['r2']:.4f}, RMSE={metrics['rmse']:.4f}")
                
                # Ensemble summary
                report.append("  Ensemble Models: Prophet + XGBoost combinations")
                
            else:
                report.append(f"  ❌ Training failed: {timeframe_results['error']}")
        
        return "\n".join(report)


# Usage example
if __name__ == "__main__":
    # Example usage
    trainer = MultiTimeframeModelTrainer()
    
    # Generate sample data
    dates = pd.date_range(start='2024-01-01', end='2025-09-02', freq='1min')
    np.random.seed(42)
    
    sample_data = pd.DataFrame({
        'open': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'high': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) + np.abs(np.random.randn(len(dates))),
        'low': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) - np.abs(np.random.randn(len(dates))),
        'close': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'volume': np.random.randint(1000, 10000, len(dates))
    }, index=dates)
    
    print("Starting multi-timeframe model training...")
    results = trainer.train_all_models(sample_data)
    
    print("\n" + trainer.generate_model_summary_report(results))
