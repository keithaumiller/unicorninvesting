#!/usr/bin/env python3
"""
Enhanced ETH Production Model Manager - IBKR Live Data Integration
Retrains models every interval with production performance tracking and lifecycle management.
Now with IBKR live data integration for real-time model training and validation.
"""

import os
import json
import sqlite3
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Union, Tuple
from dataclasses import dataclass, asdict
from enum import Enum
import warnings
import logging
import argparse
import hashlib

# IBKR Live Data Integration
try:
    from ibkr_data_integration import IBKRLiveDataIntegration, IBKRETHDataPoint
    ibkr_available = True
except ImportError:
    ibkr_available = False
    print("⚠️  IBKR data integration not available - using simulated data")

# Basic ML imports
try:
    from prophet import Prophet
    prophet_available = True
except ImportError:
    prophet_available = False
    print("⚠️  Prophet not available")

try:
    import xgboost as xgb
    from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
    from sklearn.ensemble import RandomForestRegressor
    sklearn_available = True
except ImportError:
    sklearn_available = False
    print("⚠️  XGBoost/sklearn not available")

warnings.filterwarnings('ignore')

class ModelStatus(Enum):
    TRAINING = "training"
    EVALUATING = "evaluating"
    PRODUCTION = "production"
    RETIRED = "retired"
    FAILED = "failed"

@dataclass
class ModelMetadata:
    """Comprehensive model metadata."""
    model_id: str
    asset: str
    timeframe: str
    method: str
    created_timestamp: str
    training_data_points: int
    training_mape: float
    training_mae: float
    training_r2: float
    status: ModelStatus
    production_start: Optional[str] = None
    production_predictions: int = 0
    production_mape: float = 0.0
    production_mae: float = 0.0
    production_r2: float = 0.0
    production_score: float = 0.0  # Composite score for ranking

@dataclass
class ProductionPrediction:
    """Production prediction record for performance tracking."""
    prediction_id: str
    model_id: str
    timestamp: str
    predicted_price: float
    actual_price: Optional[float] = None
    error: Optional[float] = None
    percentage_error: Optional[float] = None

class ProductionModelManager:
    """
    Enhanced model manager with production performance tracking and lifecycle management.
    """
    
    def __init__(self, asset: str = "ETH"):
        self.asset = asset
        self.base_dir = Path(__file__).parent
        self.forecasts_dir = self.base_dir / "forecasts"
        self.models_dir = self.base_dir / "production_models"
        self.logs_dir = self.base_dir / "logs"
        
        # Create directories
        for directory in [self.forecasts_dir, self.models_dir, self.logs_dir]:
            directory.mkdir(exist_ok=True)
        
        # Create timeframe and method directories
        self.timeframes = ['1min', '1hour', '1day']
        self.methods = ['prophet', 'xgboost', 'ensemble']
        
        for timeframe in self.timeframes:
            (self.forecasts_dir / timeframe).mkdir(exist_ok=True)
            (self.models_dir / timeframe).mkdir(exist_ok=True)
            for method in self.methods:
                (self.forecasts_dir / timeframe / method).mkdir(exist_ok=True)
                (self.models_dir / timeframe / method).mkdir(exist_ok=True)
        
        # Setup logging
        self.setup_logging()
        
        # Initialize IBKR data integration
        self.ibkr_integration = None
        if ibkr_available:
            try:
                self.ibkr_integration = IBKRLiveDataIntegration()
                self.logger.info("✅ IBKR data integration initialized")
                
                # Test connection
                if self.ibkr_integration.authenticate():
                    self.logger.info("✅ IBKR Gateway authenticated - using live data")
                else:
                    self.logger.warning("⚠️ IBKR Gateway not authenticated - using simulated data")
            except Exception as e:
                self.logger.warning(f"⚠️ IBKR integration failed: {e} - using simulated data")
                self.ibkr_integration = None
        else:
            self.logger.warning("⚠️ IBKR integration not available - using simulated data")
        
        # Enhanced timeframe configurations - RETRAIN EVERY INTERVAL
        self.timeframe_configs = {
            '1min': {
                'retrain_interval': 1,  # Retrain every 1 minute
                'forecast_horizon': 60,
                'max_history_hours': 48,
                'performance_window': 1000,
                'data_frequency': '1min',
                'min_data_points': 100,
                'max_models_kept': 10,
                'evaluation_period_hours': 1
            },
            '1hour': {
                'retrain_interval': 1,  # Retrain every 1 hour
                'forecast_horizon': 24,
                'max_history_days': 14,
                'performance_window': 240,
                'data_frequency': '1H',
                'min_data_points': 100,
                'max_models_kept': 10,
                'evaluation_period_hours': 1
            },
            '1day': {
                'retrain_interval': 1,  # Retrain every 1 day
                'forecast_horizon': 30,
                'max_history_days': 90,
                'performance_window': 60,
                'data_frequency': '1D',
                'min_data_points': 60,
                'max_models_kept': 10,
                'evaluation_period_hours': 1
            }
        }
        
        # Initialize databases
        self.init_databases()
        
    def setup_logging(self):
        """Setup logging configuration."""
        log_file = self.logs_dir / f"production_model_manager_{datetime.now().strftime('%Y%m%d')}.log"
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
        
    def init_databases(self):
        """Initialize SQLite databases for comprehensive tracking."""
        self.performance_db = self.base_dir / "production_performance.db"
        
        with sqlite3.connect(self.performance_db) as conn:
            # Model metadata table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_metadata (
                    model_id TEXT PRIMARY KEY,
                    asset TEXT,
                    timeframe TEXT,
                    method TEXT,
                    created_timestamp TEXT,
                    training_data_points INTEGER,
                    training_mape REAL,
                    training_mae REAL,
                    training_r2 REAL,
                    status TEXT,
                    production_start TEXT,
                    production_predictions INTEGER DEFAULT 0,
                    production_mape REAL DEFAULT 0.0,
                    production_mae REAL DEFAULT 0.0,
                    production_r2 REAL DEFAULT 0.0,
                    production_score REAL DEFAULT 0.0
                )
            """)
            
            # Production predictions table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS production_predictions (
                    prediction_id TEXT PRIMARY KEY,
                    model_id TEXT,
                    timestamp TEXT,
                    predicted_price REAL,
                    actual_price REAL,
                    error REAL,
                    percentage_error REAL,
                    FOREIGN KEY (model_id) REFERENCES model_metadata (model_id)
                )
            """)
            
            # Model lifecycle events
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_lifecycle (
                    id INTEGER PRIMARY KEY,
                    model_id TEXT,
                    event_type TEXT,
                    timestamp TEXT,
                    details TEXT,
                    FOREIGN KEY (model_id) REFERENCES model_metadata (model_id)
                )
            """)

    def generate_model_id(self, timeframe: str, method: str) -> str:
        """Generate unique model ID."""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S_%f')
        unique_string = f"{self.asset}_{timeframe}_{method}_{timestamp}"
        return hashlib.md5(unique_string.encode()).hexdigest()[:12]

    def generate_enhanced_data(self, timeframe: str, config: Dict) -> pd.DataFrame:
        """
        Generate enhanced data using IBKR live data integration when available,
        fallback to simulated data for testing.
        """
        # Try to use IBKR live data first
        if self.ibkr_integration and self.ibkr_integration.authenticated:
            try:
                self.logger.info(f"📊 Fetching live IBKR data for {timeframe}")
                
                if timeframe == '1min':
                    # Get 1-minute bars from IBKR
                    lookback_hours = config.get('max_history_hours', 24)
                    data_points = self.ibkr_integration.get_historical_minute_bars(lookback_hours)
                    
                elif timeframe == '1hour':
                    # Get 1-hour bars from IBKR
                    lookback_days = config.get('max_history_days', 14)
                    data_points = self.ibkr_integration.get_historical_hourly_bars(lookback_days)
                    
                else:  # '1day'
                    # Get daily bars from IBKR
                    lookback_days = config.get('max_history_days', 90)
                    data_points = self.ibkr_integration.get_historical_daily_bars(lookback_days)
                
                if data_points and len(data_points) > config.get('min_data_points', 50):
                    # Convert to DataFrame
                    df = self.ibkr_integration.convert_to_dataframe(data_points)
                    self.logger.info(f"✅ Retrieved {len(df)} live data points for {timeframe}")
                    return df
                else:
                    self.logger.warning(f"⚠️ Insufficient live data ({len(data_points) if data_points else 0} points), using simulated data")
                    
            except Exception as e:
                self.logger.error(f"❌ IBKR data fetch failed for {timeframe}: {e}, using simulated data")
        
        # Fallback to simulated data
        self.logger.info(f"📊 Generating simulated data for {timeframe}")
        return self._generate_simulated_data(timeframe, config)
    
    def _generate_simulated_data(self, timeframe: str, config: Dict) -> pd.DataFrame:
        """Generate simulated data when live data is not available."""
        if timeframe == '1min':
            periods = config['max_history_hours'] * 60
            freq = '1min'
            base_volatility = 0.005  # 0.5% per minute
        elif timeframe == '1hour':
            periods = config['max_history_days'] * 24
            freq = '1H'
            base_volatility = 0.02  # 2% per hour
        else:  # 1day
            periods = config['max_history_days']
            freq = '1D'
            base_volatility = 0.05  # 5% per day
        
        # Generate dates
        end_date = datetime.now()
        start_date = end_date - timedelta(hours=config.get('max_history_hours', config.get('max_history_days', 30) * 24))
        dates = pd.date_range(start=start_date, end=end_date, freq=freq)
        
        # Generate realistic ETH price with trends and patterns
        base_price = 3000
        prices = [base_price]
        volumes = []
        
        # Add trend component
        trend_strength = np.random.uniform(-0.0001, 0.0001)  # Much smaller trend
        
        for i in range(len(dates) - 1):
            # Trend component (very small to avoid exponential growth)
            trend = trend_strength
            
            # Seasonal component
            if timeframe == '1min':
                seasonal = 0.001 * np.sin(2 * np.pi * i / (24 * 60))  # Daily pattern
            elif timeframe == '1hour':
                seasonal = 0.005 * np.sin(2 * np.pi * i / 24)  # Daily pattern
            else:  # 1day
                seasonal = 0.01 * np.sin(2 * np.pi * i / 7)  # Weekly pattern
            
            # Random walk component (much smaller)
            random_component = np.random.normal(0, base_volatility * 0.1)
            
            # Combine components with mean reversion
            total_return = trend + seasonal + random_component
            
            # Mean reversion towards base price
            current_price = prices[-1]
            mean_reversion = -0.0001 * (current_price - base_price) / base_price
            total_return += mean_reversion
            
            new_price = current_price * (1 + total_return)
            new_price = max(new_price, 100)  # Price floor
            new_price = min(new_price, 10000)  # Price ceiling to prevent explosion
            
            prices.append(new_price)
            
            # Generate volume
            volume_base = 5000 if timeframe == '1min' else 10000 if timeframe == '1hour' else 50000
            volume_volatility = abs(total_return) * 10000
            volume = max(100, np.random.normal(volume_base + volume_volatility, volume_base * 0.3))
            volumes.append(volume)
        
        # Final volume for the last price
        volumes.append(volumes[-1] if volumes else 5000)
        
        return pd.DataFrame({
            'timestamp': dates,
            'price': prices[:len(dates)],
            'volume': volumes[:len(dates)]
        }).set_index('timestamp')

    def prepare_enhanced_features(self, data: pd.DataFrame, timeframe: str) -> pd.DataFrame:
        """Prepare enhanced features for ML models."""
        features = data.copy()
        
        # Price-based features
        for window in [5, 10, 20]:
            features[f'price_ma_{window}'] = features['price'].rolling(window).mean()
            features[f'price_std_{window}'] = features['price'].rolling(window).std()
        
        # Price change features
        for period in [1, 5, 10]:
            features[f'price_change_{period}'] = features['price'].pct_change(period)
        
        # Volume features
        features['volume_ma_5'] = features['volume'].rolling(5).mean()
        features['volume_ratio'] = features['volume'] / features['volume_ma_5']
        
        # Technical indicators
        features['rsi'] = self.calculate_rsi(features['price'], 14)
        features['bollinger_upper'], features['bollinger_lower'] = self.calculate_bollinger_bands(features['price'], 20)
        features['macd'], features['macd_signal'] = self.calculate_macd(features['price'])
        
        # Lag features
        for lag in [1, 2, 3, 5]:
            features[f'price_lag_{lag}'] = features['price'].shift(lag)
            features[f'volume_lag_{lag}'] = features['volume'].shift(lag)
        
        # Time features
        features['hour'] = features.index.hour if timeframe != '1day' else 0
        features['day_of_week'] = features.index.dayofweek
        features['day_of_month'] = features.index.day
        features['month'] = features.index.month
        
        return features.dropna()

    def calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
    
    def calculate_bollinger_bands(self, prices: pd.Series, window: int = 20, num_std: float = 2) -> Tuple[pd.Series, pd.Series]:
        """Calculate Bollinger Bands."""
        ma = prices.rolling(window).mean()
        std = prices.rolling(window).std()
        upper = ma + (std * num_std)
        lower = ma - (std * num_std)
        return upper, lower
    
    def calculate_macd(self, prices: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9) -> Tuple[pd.Series, pd.Series]:
        """Calculate MACD."""
        ema_fast = prices.ewm(span=fast).mean()
        ema_slow = prices.ewm(span=slow).mean()
        macd = ema_fast - ema_slow
        macd_signal = macd.ewm(span=signal).mean()
        return macd, macd_signal

    def train_new_model(self, timeframe: str, method: str) -> Optional[ModelMetadata]:
        """Train a new model and return its metadata."""
        self.logger.info(f"Training new {method} model for {timeframe}")
        
        try:
            config = self.timeframe_configs[timeframe]
            market_data = self.generate_enhanced_data(timeframe, config)
            
            model_id = self.generate_model_id(timeframe, method)
            
            if method == 'prophet':
                model, metrics = self.train_prophet_model(market_data, timeframe)
            elif method == 'xgboost':
                model, metrics = self.train_xgboost_model(market_data, timeframe)
            else:  # ensemble
                # For ensemble, we need the production models from other methods
                prophet_metadata = self.get_production_model(timeframe, 'prophet')
                xgb_metadata = self.get_production_model(timeframe, 'xgboost')
                
                if not prophet_metadata or not xgb_metadata:
                    self.logger.warning(f"Cannot create ensemble model for {timeframe} - missing production models")
                    return None
                
                model, metrics = self.create_ensemble_from_production(timeframe, prophet_metadata, xgb_metadata)
            
            if not model or 'error' in metrics:
                self.logger.error(f"Failed to train {method} model for {timeframe}: {metrics.get('error', 'Unknown error')}")
                return None
            
            # Create model metadata
            model_metadata = ModelMetadata(
                model_id=model_id,
                asset=self.asset,
                timeframe=timeframe,
                method=method,
                created_timestamp=datetime.now().isoformat(),
                training_data_points=len(market_data),
                training_mape=metrics.get('mape', 0),
                training_mae=metrics.get('mae', 0),
                training_r2=metrics.get('r2_score', 0),
                status=ModelStatus.EVALUATING
            )
            
            # Save model to disk
            self.save_model_to_disk(model, model_metadata, metrics)
            
            # Store metadata in database
            self.store_model_metadata(model_metadata)
            
            # For 1hour and 1day models, use testing metrics as initial production metrics
            if timeframe in ['1hour', '1day']:
                self.use_testing_metrics_as_production(model_id, timeframe)
            
            # Log lifecycle event
            self.log_lifecycle_event(model_id, "created", f"Training metrics: MAPE={metrics.get('mape', 0):.2f}%, R²={metrics.get('r2_score', 0):.4f}")
            
            self.logger.info(f"Successfully trained {method} model {model_id} for {timeframe}")
            return model_metadata
            
        except Exception as e:
            self.logger.error(f"Model training failed for {method} {timeframe}: {e}")
            return None

    def train_prophet_model(self, data: pd.DataFrame, timeframe: str) -> Tuple[object, Dict]:
        """Train a Prophet model with timeframe-specific configurations."""
        if not prophet_available:
            return None, {"error": "Prophet not available"}
        
        try:
            prophet_data = pd.DataFrame({
                'ds': data.index,
                'y': data['price']
            })
            
            # Clean data for Prophet
            prophet_data = prophet_data.dropna()
            prophet_data = prophet_data[np.isfinite(prophet_data['y'])]
            
            if len(prophet_data) == 0:
                raise Exception("No valid data for Prophet after cleaning")
            
            # Configure Prophet based on timeframe
            if timeframe == '1min':
                model = Prophet(
                    daily_seasonality=True,
                    weekly_seasonality=False,
                    yearly_seasonality=False,
                    seasonality_mode='multiplicative',
                    changepoint_prior_scale=0.05
                )
            elif timeframe == '1hour':
                model = Prophet(
                    daily_seasonality=True,
                    weekly_seasonality=True,
                    yearly_seasonality=False,
                    seasonality_mode='multiplicative',
                    changepoint_prior_scale=0.05
                )
            else:  # 1day
                model = Prophet(
                    daily_seasonality=False,
                    weekly_seasonality=True,
                    yearly_seasonality=True,
                    seasonality_mode='multiplicative',
                    changepoint_prior_scale=0.1
                )
            
            model.fit(prophet_data)
            
            # Validation
            train_predictions = model.predict(prophet_data)
            y_true = prophet_data['y'].values
            y_pred = train_predictions['yhat'].values
            
            mape = np.mean(np.abs((y_true - y_pred) / y_true)) * 100
            mae = mean_absolute_error(y_true, y_pred) if sklearn_available else 0
            r2 = r2_score(y_true, y_pred) if sklearn_available else 0
            
            metrics = {
                'mape': mape,
                'mae': mae,
                'r2_score': r2,
                'model_type': 'prophet'
            }
            
            return model, metrics
            
        except Exception as e:
            return None, {"error": str(e)}

    def train_xgboost_model(self, data: pd.DataFrame, timeframe: str) -> Tuple[object, Dict]:
        """Train an enhanced XGBoost model."""
        if not sklearn_available:
            return None, {"error": "XGBoost/sklearn not available"}
        
        try:
            features = self.prepare_enhanced_features(data, timeframe)
            
            # Prepare target variable
            features = self.prepare_enhanced_features(data, timeframe)
            
            # Create target as next period price
            features_with_target = features.copy()
            features_with_target['target'] = features_with_target['price'].shift(-1)
            
            # Remove rows with NaN target
            features_with_target = features_with_target.dropna()
            
            if len(features_with_target) == 0:
                raise Exception("No valid data after cleaning")
            
            # Split back into features and target
            target_col = features_with_target['target']
            feature_data = features_with_target.drop('target', axis=1)
            
            # Select numeric features
            feature_cols = [col for col in feature_data.columns if feature_data[col].dtype in ['float64', 'int64']]
            X = feature_data[feature_cols].fillna(method='ffill').fillna(0)
            y = target_col.values
            
            # Final validation - ensure all data is finite
            self.logger.debug(f"Target stats before validation: min={y.min()}, max={y.max()}, has_nan={np.isnan(y).any()}, has_inf={np.isinf(y).any()}")
            
            if not np.all(np.isfinite(y)):
                finite_mask = np.isfinite(y)
                self.logger.debug(f"Removing {(~finite_mask).sum()} non-finite target values")
                X = X.iloc[finite_mask]
                y = y[finite_mask]
            
            self.logger.debug(f"Final target stats: min={y.min()}, max={y.max()}, length={len(y)}")
            
            if len(y) == 0:
                raise Exception("No valid training data after cleaning")
            
            # Ensure all features are finite
            X = X.replace([np.inf, -np.inf], np.nan).fillna(0)
            
            # Configure XGBoost based on timeframe
            if timeframe == '1min':
                model = xgb.XGBRegressor(
                    n_estimators=200,
                    max_depth=8,
                    learning_rate=0.05,
                    subsample=0.8,
                    colsample_bytree=0.8,
                    random_state=42
                )
            elif timeframe == '1hour':
                model = xgb.XGBRegressor(
                    n_estimators=300,
                    max_depth=10,
                    learning_rate=0.03,
                    subsample=0.8,
                    colsample_bytree=0.8,
                    random_state=42
                )
            else:  # 1day
                model = xgb.XGBRegressor(
                    n_estimators=400,
                    max_depth=12,
                    learning_rate=0.02,
                    subsample=0.8,
                    colsample_bytree=0.8,
                    random_state=42
                )
            
            model.fit(X, y)
            
            # Calculate metrics
            y_pred = model.predict(X)
            
            mape = np.mean(np.abs((y - y_pred) / y)) * 100
            mae = mean_absolute_error(y, y_pred)
            r2 = r2_score(y, y_pred)
            
            metrics = {
                'mape': mape,
                'mae': mae,
                'r2_score': r2,
                'model_type': 'xgboost',
                'feature_cols': feature_cols
            }
            
            return model, metrics
            
        except Exception as e:
            return None, {"error": str(e)}

    def create_ensemble_from_production(self, timeframe: str, prophet_metadata: ModelMetadata, xgb_metadata: ModelMetadata) -> Tuple[object, Dict]:
        """Create ensemble using production forecasts from other methods."""
        try:
            # Weight based on production performance scores
            prophet_score = prophet_metadata.production_score or prophet_metadata.training_r2
            xgb_score = xgb_metadata.production_score or xgb_metadata.training_r2
            
            total_score = prophet_score + xgb_score
            if total_score > 0:
                prophet_weight = prophet_score / total_score
                xgb_weight = xgb_score / total_score
            else:
                prophet_weight = 0.5
                xgb_weight = 0.5
            
            # Ensemble "model" (just the weights and metadata)
            ensemble_model = {
                'prophet_model_id': prophet_metadata.model_id,
                'xgb_model_id': xgb_metadata.model_id,
                'prophet_weight': prophet_weight,
                'xgb_weight': xgb_weight,
                'method': 'ensemble'
            }
            
            # Estimate ensemble performance based on component performance
            estimated_r2 = prophet_weight * prophet_score + xgb_weight * xgb_score
            estimated_mape = prophet_weight * prophet_metadata.production_mape + xgb_weight * xgb_metadata.production_mape
            
            if estimated_mape == 0:  # Fallback to training metrics
                estimated_mape = prophet_weight * prophet_metadata.training_mape + xgb_weight * xgb_metadata.training_mape
            
            metrics = {
                'mape': estimated_mape,
                'mae': 0,  # Will be calculated in production
                'r2_score': estimated_r2,
                'model_type': 'ensemble',
                'prophet_weight': prophet_weight,
                'xgb_weight': xgb_weight
            }
            
            return ensemble_model, metrics
            
        except Exception as e:
            return None, {"error": str(e)}

    def save_model_to_disk(self, model, metadata: ModelMetadata, metrics: Dict):
        """Save model to disk with comprehensive metadata."""
        model_dir = self.models_dir / metadata.timeframe / metadata.method
        model_file = model_dir / f"{metadata.model_id}.json"
        
        # Convert ModelMetadata to dict with enum handling
        metadata_dict = asdict(metadata)
        metadata_dict['status'] = metadata.status.value  # Convert enum to string
        
        # Create model package
        model_package = {
            'metadata': metadata_dict,
            'training_metrics': metrics,
            'model_data': None  # Placeholder - in production would serialize actual model
        }
        
        # For ensemble models, save the configuration
        if metadata.method == 'ensemble' and isinstance(model, dict):
            model_package['ensemble_config'] = model
        
        with open(model_file, 'w') as f:
            json.dump(model_package, f, indent=2)
        
        self.logger.info(f"Model {metadata.model_id} saved to {model_file}")

    def store_model_metadata(self, metadata: ModelMetadata):
        """Store model metadata in database."""
        with sqlite3.connect(self.performance_db) as conn:
            conn.execute("""
                INSERT INTO model_metadata 
                (model_id, asset, timeframe, method, created_timestamp, training_data_points,
                 training_mape, training_mae, training_r2, status)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                metadata.model_id, metadata.asset, metadata.timeframe, metadata.method,
                metadata.created_timestamp, metadata.training_data_points,
                metadata.training_mape, metadata.training_mae, metadata.training_r2,
                metadata.status.value
            ))

    def log_lifecycle_event(self, model_id: str, event_type: str, details: str):
        """Log model lifecycle event."""
        with sqlite3.connect(self.performance_db) as conn:
            conn.execute("""
                INSERT INTO model_lifecycle (model_id, event_type, timestamp, details)
                VALUES (?, ?, ?, ?)
            """, (model_id, event_type, datetime.now().isoformat(), details))

    def get_production_model(self, timeframe: str, method: str) -> Optional[ModelMetadata]:
        """Get current production model for timeframe/method."""
        with sqlite3.connect(self.performance_db) as conn:
            result = conn.execute("""
                SELECT * FROM model_metadata 
                WHERE asset = ? AND timeframe = ? AND method = ? AND status = 'production'
                ORDER BY production_score DESC LIMIT 1
            """, (self.asset, timeframe, method)).fetchone()
            
            if result:
                return ModelMetadata(
                    model_id=result[0],
                    asset=result[1],
                    timeframe=result[2],
                    method=result[3],
                    created_timestamp=result[4],
                    training_data_points=result[5],
                    training_mape=result[6],
                    training_mae=result[7],
                    training_r2=result[8],
                    status=ModelStatus(result[9]),
                    production_start=result[10],
                    production_predictions=result[11],
                    production_mape=result[12],
                    production_mae=result[13],
                    production_r2=result[14],
                    production_score=result[15]
                )
        return None

    def update_production_performance(self, model_id: str, actual_price: float, predicted_price: float):
        """Update production performance metrics for a model."""
        error = abs(actual_price - predicted_price)
        percentage_error = abs((actual_price - predicted_price) / actual_price) * 100
        
        # Store individual prediction
        prediction_id = f"{model_id}_{datetime.now().strftime('%Y%m%d_%H%M%S_%f')}"
        
        with sqlite3.connect(self.performance_db) as conn:
            # Store prediction
            conn.execute("""
                INSERT INTO production_predictions 
                (prediction_id, model_id, timestamp, predicted_price, actual_price, error, percentage_error)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            """, (prediction_id, model_id, datetime.now().isoformat(), 
                 predicted_price, actual_price, error, percentage_error))
            
            # Update model's aggregated performance
            conn.execute("""
                UPDATE model_metadata SET 
                    production_predictions = production_predictions + 1,
                    production_mape = (
                        SELECT AVG(percentage_error) 
                        FROM production_predictions 
                        WHERE model_id = ?
                    ),
                    production_mae = (
                        SELECT AVG(error) 
                        FROM production_predictions 
                        WHERE model_id = ?
                    )
                WHERE model_id = ?
            """, (model_id, model_id, model_id))
    
    def use_testing_metrics_as_production(self, model_id: str, timeframe: str):
        """
        For 1hour and 1day models, use testing evaluation metrics as production metrics
        until sufficient live data is available.
        """
        if timeframe == '1min':
            # 1min models always use live production tracking
            return
            
        with sqlite3.connect(self.performance_db) as conn:
            # Get the model's training metrics
            model_data = conn.execute("""
                SELECT training_mape, training_mae, training_r2
                FROM model_metadata WHERE model_id = ?
            """, (model_id,)).fetchone()
            
            if model_data:
                training_mape, training_mae, training_r2 = model_data
                
                # Use training metrics as initial production metrics
                conn.execute("""
                    UPDATE model_metadata SET 
                        production_mape = ?,
                        production_mae = ?,
                        production_r2 = ?,
                        production_predictions = 1
                    WHERE model_id = ?
                """, (training_mape, training_mae, training_r2, model_id))
                
                # Log the action
                conn.execute("""
                    INSERT INTO model_lifecycle
                    (model_id, event_type, timestamp, details)
                    VALUES (?, ?, ?, ?)
                """, (model_id, "testing_metrics_as_production", datetime.now().isoformat(),
                     f"Using testing metrics as production for {timeframe} model: MAPE={training_mape:.3f}, MAE={training_mae:.3f}"))
                
                self.logger.info(f"📊 Using testing metrics as production for {timeframe} model {model_id}")
            
            # Calculate R² score if we have enough predictions
            predictions_count = conn.execute("""
                SELECT COUNT(*) FROM production_predictions WHERE model_id = ?
            """, (model_id,)).fetchone()[0]
            
            if predictions_count >= 10:
                # Calculate R² for production predictions
                results = conn.execute("""
                    SELECT actual_price, predicted_price 
                    FROM production_predictions 
                    WHERE model_id = ?
                """, (model_id,)).fetchall()
                
                if results and sklearn_available:
                    actual_values = [r[0] for r in results]
                    predicted_values = [r[1] for r in results]
                    r2 = r2_score(actual_values, predicted_values)
                    
                    # Calculate composite production score
                    mape = conn.execute("""
                        SELECT production_mape FROM model_metadata WHERE model_id = ?
                    """, (model_id,)).fetchone()[0]
                    
                    # Production score: weighted combination of R² and inverse MAPE
                    production_score = (r2 * 0.7) + ((100 - min(mape, 100)) / 100 * 0.3)
                    
                    conn.execute("""
                        UPDATE model_metadata SET 
                            production_r2 = ?,
                            production_score = ?
                        WHERE model_id = ?
                    """, (r2, production_score, model_id))

    def select_best_production_model(self, timeframe: str, method: str) -> Optional[ModelMetadata]:
        """Select the best model for production based on production performance."""
        with sqlite3.connect(self.performance_db) as conn:
            # Get all models that have been evaluating for more than the evaluation period
            evaluation_hours = self.timeframe_configs[timeframe]['evaluation_period_hours']
            cutoff_time = (datetime.now() - timedelta(hours=evaluation_hours)).isoformat()
            
            results = conn.execute("""
                SELECT * FROM model_metadata 
                WHERE asset = ? AND timeframe = ? AND method = ? 
                AND status IN ('evaluating', 'production')
                AND (created_timestamp < ? OR production_predictions >= 10)
                ORDER BY 
                    CASE 
                        WHEN production_predictions >= 10 THEN production_score
                        ELSE training_r2 
                    END DESC
                LIMIT 1
            """, (self.asset, timeframe, method, cutoff_time)).fetchone()
            
            if results:
                return ModelMetadata(
                    model_id=results[0],
                    asset=results[1],
                    timeframe=results[2],
                    method=results[3],
                    created_timestamp=results[4],
                    training_data_points=results[5],
                    training_mape=results[6],
                    training_mae=results[7],
                    training_r2=results[8],
                    status=ModelStatus(results[9]),
                    production_start=results[10],
                    production_predictions=results[11],
                    production_mape=results[12],
                    production_mae=results[13],
                    production_r2=results[14],
                    production_score=results[15]
                )
        return None

    def promote_to_production(self, model_id: str):
        """Promote a model to production status."""
        with sqlite3.connect(self.performance_db) as conn:
            # Get model info
            model_info = conn.execute("""
                SELECT timeframe, method FROM model_metadata WHERE model_id = ?
            """, (model_id,)).fetchone()
            
            if not model_info:
                return False
            
            timeframe, method = model_info
            
            # Demote current production model
            conn.execute("""
                UPDATE model_metadata SET status = 'retired'
                WHERE asset = ? AND timeframe = ? AND method = ? AND status = 'production'
            """, (self.asset, timeframe, method))
            
            # Promote new model
            conn.execute("""
                UPDATE model_metadata SET 
                    status = 'production',
                    production_start = ?
                WHERE model_id = ?
            """, (datetime.now().isoformat(), model_id))
            
            self.log_lifecycle_event(model_id, "promoted_to_production", f"Promoted to production for {timeframe} {method}")
            self.logger.info(f"Model {model_id} promoted to production for {timeframe} {method}")
            return True

    def cleanup_old_models(self, timeframe: str, method: str):
        """Remove models that are not in top 10 performing and older than 1 hour."""
        config = self.timeframe_configs[timeframe]
        max_models = config['max_models_kept']
        cutoff_time = (datetime.now() - timedelta(hours=1)).isoformat()
        
        with sqlite3.connect(self.performance_db) as conn:
            # Get models to keep (top performers + recent models + production model)
            models_to_keep = conn.execute("""
                SELECT model_id FROM (
                    SELECT model_id, 
                           ROW_NUMBER() OVER (ORDER BY 
                               CASE 
                                   WHEN production_predictions >= 10 THEN production_score
                                   ELSE training_r2 
                               END DESC) as rank
                    FROM model_metadata 
                    WHERE asset = ? AND timeframe = ? AND method = ?
                    AND (created_timestamp >= ? OR status = 'production')
                ) ranked 
                WHERE rank <= ? OR EXISTS (
                    SELECT 1 FROM model_metadata m2 
                    WHERE m2.model_id = ranked.model_id AND m2.status = 'production'
                )
            """, (self.asset, timeframe, method, cutoff_time, max_models)).fetchall()
            
            keep_ids = [row[0] for row in models_to_keep]
            
            if keep_ids:
                placeholders = ','.join(['?' for _ in keep_ids])
                models_to_remove = conn.execute(f"""
                    SELECT model_id FROM model_metadata 
                    WHERE asset = ? AND timeframe = ? AND method = ?
                    AND status != 'production'
                    AND model_id NOT IN ({placeholders})
                """, [self.asset, timeframe, method] + keep_ids).fetchall()
                
                for (model_id,) in models_to_remove:
                    # Mark as retired
                    conn.execute("""
                        UPDATE model_metadata SET status = 'retired'
                        WHERE model_id = ?
                    """, (model_id,))
                    
                    # Remove model file
                    model_file = self.models_dir / timeframe / method / f"{model_id}.json"
                    if model_file.exists():
                        model_file.unlink()
                    
                    self.log_lifecycle_event(model_id, "retired", "Removed due to poor performance")
                    self.logger.info(f"Retired model {model_id} for {timeframe} {method}")

    def run_interval_cycle(self, timeframe: str) -> Dict:
        """Run a complete interval cycle: train, evaluate, promote, cleanup."""
        self.logger.info(f"Running interval cycle for {timeframe}")
        
        results = {}
        
        # Train new models for prophet and xgboost
        for method in ['prophet', 'xgboost']:
            new_model = self.train_new_model(timeframe, method)
            if new_model:
                results[f"{method}_trained"] = new_model.model_id
                
                # Check if this should be promoted to production
                best_model = self.select_best_production_model(timeframe, method)
                if best_model and best_model.model_id == new_model.model_id:
                    self.promote_to_production(new_model.model_id)
                    results[f"{method}_promoted"] = True
                
                # Cleanup old models
                self.cleanup_old_models(timeframe, method)
        
        # Train ensemble model (after prophet and xgboost)
        ensemble_model = self.train_new_model(timeframe, 'ensemble')
        if ensemble_model:
            results["ensemble_trained"] = ensemble_model.model_id
            
            # Check if this should be promoted to production
            best_ensemble = self.select_best_production_model(timeframe, 'ensemble')
            if best_ensemble and best_ensemble.model_id == ensemble_model.model_id:
                self.promote_to_production(ensemble_model.model_id)
                results["ensemble_promoted"] = True
            
            # Cleanup old ensemble models
            self.cleanup_old_models(timeframe, 'ensemble')
        
        return results

    def get_comprehensive_status(self) -> Dict:
        """Get comprehensive status of all models and performance."""
        with sqlite3.connect(self.performance_db) as conn:
            status = {}
            
            for timeframe in self.timeframes:
                status[timeframe] = {}
                
                for method in self.methods:
                    # Current production model
                    prod_model = conn.execute("""
                        SELECT model_id, production_predictions, production_mape, production_r2, production_score
                        FROM model_metadata 
                        WHERE asset = ? AND timeframe = ? AND method = ? AND status = 'production'
                    """, (self.asset, timeframe, method)).fetchone()
                    
                    # Count of models in each status
                    status_counts = conn.execute("""
                        SELECT status, COUNT(*) 
                        FROM model_metadata 
                        WHERE asset = ? AND timeframe = ? AND method = ?
                        GROUP BY status
                    """, (self.asset, timeframe, method)).fetchall()
                    
                    status[timeframe][method] = {
                        'production_model': prod_model[0] if prod_model else None,
                        'production_predictions': prod_model[1] if prod_model else 0,
                        'production_mape': prod_model[2] if prod_model else 0,
                        'production_r2': prod_model[3] if prod_model else 0,
                        'production_score': prod_model[4] if prod_model else 0,
                        'model_counts': dict(status_counts)
                    }
            
            return status

def main():
    """Main function for CLI usage."""
    parser = argparse.ArgumentParser(description='Production Model Manager')
    parser.add_argument('--action', choices=['cycle', 'status', 'train'], default='status',
                       help='Action to perform')
    parser.add_argument('--timeframe', choices=['1min', '1hour', '1day', 'all'], default='all',
                       help='Timeframe for operations')
    parser.add_argument('--asset', default='ETH',
                       help='Asset symbol')
    
    args = parser.parse_args()
    
    manager = ProductionModelManager(args.asset)
    
    if args.action == 'status':
        status = manager.get_comprehensive_status()
        print(f"\n📊 Production Model Status for {args.asset}")
        print("=" * 80)
        
        for timeframe, methods in status.items():
            print(f"\n{timeframe.upper()}:")
            for method, stats in methods.items():
                prod_model = stats['production_model']
                if prod_model:
                    print(f"  {method:10} | PROD: {prod_model[:8]} | "
                         f"Preds: {stats['production_predictions']:3d} | "
                         f"MAPE: {stats['production_mape']:5.2f}% | "
                         f"R²: {stats['production_r2']:6.4f} | "
                         f"Score: {stats['production_score']:5.3f}")
                else:
                    print(f"  {method:10} | No production model")
                
                # Show model counts
                counts = stats['model_counts']
                count_str = " | ".join([f"{status}: {count}" for status, count in counts.items()])
                print(f"               Models: {count_str}")
    
    elif args.action == 'cycle':
        if args.timeframe == 'all':
            timeframes = ['1min', '1hour', '1day']
        else:
            timeframes = [args.timeframe]
        
        for timeframe in timeframes:
            print(f"\n🔄 Running cycle for {timeframe}...")
            results = manager.run_interval_cycle(timeframe)
            
            for key, value in results.items():
                if 'trained' in key:
                    print(f"  ✅ Trained: {key.replace('_trained', '')} model {value}")
                elif 'promoted' in key:
                    print(f"  🏆 Promoted: {key.replace('_promoted', '')} to production")
    
    elif args.action == 'train':
        if args.timeframe == 'all':
            timeframes = ['1min', '1hour', '1day']
        else:
            timeframes = [args.timeframe]
        
        for timeframe in timeframes:
            print(f"\n🏗️ Training models for {timeframe}...")
            for method in ['prophet', 'xgboost', 'ensemble']:
                model = manager.train_new_model(timeframe, method)
                if model:
                    print(f"  ✅ Trained {method} model: {model.model_id}")
                else:
                    print(f"  ❌ Failed to train {method} model")

if __name__ == "__main__":
    main()
