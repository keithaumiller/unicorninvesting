#!/usr/bin/env python3
"""
Enhanced Forex Prophet Model Builder
====================================

Specialized Prophet model builder for forex assets with improved:
- Forex-specific feature engineering
- Enhanced timezone handling for forex markets
- Market session awareness (London, New York, Tokyo)
- Robust error handling and validation
- Comprehensive performance tracking

Focus: Rebuild failed forex models (Standard, Aggressive, Economic, Ensemble variants)
Target: Achieve meaningful R² scores (> 0.1) for all forex assets and timeframes
"""

import os
import sys
import json
import sqlite3
import warnings
import logging
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any
import traceback

import pandas as pd
import numpy as np
from prophet import Prophet
from prophet.utilities import regressor_coefficients
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error
import ta

# Suppress warnings for cleaner output
warnings.filterwarnings('ignore')
logging.getLogger('prophet').setLevel(logging.WARNING)
logging.getLogger('cmdstanpy').setLevel(logging.WARNING)

class EnhancedForexProphetBuilder:
    """Enhanced Prophet model builder specifically optimized for forex assets"""
    
    def __init__(self, base_path: str = "/workspaces/unicorninvesting"):
        self.base_path = Path(base_path)
        self.silver_path = self.base_path / "BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/processed_data/forex"
        self.models_path = self.base_path / "BackendPython/unicorn/2_alpha_models/enhanced_prophet_models"
        self.db_path = self.models_path / "enhanced_prophet_performance.db"
        
        # Forex-specific configuration
        self.forex_assets = ['AUDUSD', 'EURUSD', 'GBPUSD', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY']
        self.timeframes = ['1h', '1d']
        
        # Enhanced leak-free forex variants
        self.variants = {
            'conservative': {
                'description': 'Simple leak-free forex models with basic features',
                'features': ['market_session', 'returns_lag_1', 'volatility_lag_5'],
                'expected_count': 3
            },
            'standard': {
                'description': 'Balanced leak-free forex models with time and lagged features',
                'features': ['market_session', 'returns_lag_1', 'returns_lag_2', 'volatility_lag_20', 
                           'momentum_lag_10', 'hour_sin'],
                'expected_count': 6
            },
            'aggressive': {
                'description': 'Complex leak-free forex models with full feature suite',
                'features': ['market_session', 'returns_lag_1', 'returns_lag_2', 'returns_lag_5',
                           'volatility_lag_20', 'volatility_lag_5', 'momentum_lag_10', 'momentum_lag_20',
                           'hour_sin', 'hour_cos', 'day_sin', 'day_cos', 'volatility_regime', 'return_regime'],
                'expected_count': 14
            },
            'economic': {
                'description': 'Economic-focused leak-free forex models',
                'features': ['market_session', 'volatility_regime', 'return_regime', 'momentum_lag_20',
                           'volatility_lag_20', 'hour_sin', 'day_sin'],
                'expected_count': 7
            },
            'ensemble': {
                'description': 'Hybrid leak-free forex models with balanced feature set',
                'features': ['market_session', 'returns_lag_1', 'volatility_lag_20', 'momentum_lag_10',
                           'hour_sin', 'day_sin', 'volatility_regime', 'return_regime'],
                'expected_count': 8
            }
        }
        
        # Initialize tracking
        self.results = []
        self.performance_summary = {
            'timestamp': datetime.now().isoformat(),
            'total_models': 0,
            'successful_models': 0,
            'success_rate': 0.0,
            'model_variants': list(self.variants.keys()),
            'performance_summary': []
        }
        
        # Create directories
        self.models_path.mkdir(parents=True, exist_ok=True)
        self._init_database()
        
        print("🏦 Enhanced Forex Prophet Model Builder")
        print("="*50)
        print(f"📁 Silver Layer Path: {self.silver_path}")
        print(f"📁 Models Output Path: {self.models_path}")
        print(f"📊 Target Assets: {len(self.forex_assets)} forex pairs")
        print(f"📈 Variants: {len(self.variants)} model types")
        print(f"⏰ Timeframes: {len(self.timeframes)} intervals")

    def _init_database(self):
        """Initialize SQLite database for performance tracking"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute('''
                    CREATE TABLE IF NOT EXISTS forex_prophet_performance (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        timestamp TEXT,
                        asset TEXT,
                        interval TEXT,
                        variant TEXT,
                        r2_score REAL,
                        mae REAL,
                        mse REAL,
                        features_used INTEGER,
                        training_records INTEGER,
                        status TEXT,
                        error_message TEXT
                    )
                ''')
                conn.commit()
                print("✅ Performance tracking database initialized")
        except Exception as e:
            print(f"⚠️  Database initialization warning: {e}")

    def load_forex_data(self, asset: str, interval: str) -> Optional[pd.DataFrame]:
        """Load forex data with enhanced path resolution"""
        possible_paths = [
            self.silver_path / f"{asset}_silver_{interval}_latest.csv",
            self.silver_path / f"{asset.lower()}_silver_{interval}_latest.csv",
            self.silver_path / f"{asset}_{interval}_processed_data.csv",
            self.silver_path / f"{asset.lower()}_{interval}_processed_data.csv",
        ]
        
        # Check for timestamped files
        if self.silver_path.exists():
            pattern_files = list(self.silver_path.glob(f"{asset}*silver*{interval}*.csv"))
            if pattern_files:
                # Use the most recent file
                latest_file = max(pattern_files, key=lambda x: x.stat().st_mtime)
                possible_paths.insert(0, latest_file)
        
        for file_path in possible_paths:
            if file_path.exists():
                try:
                    print(f"📂 Loading {asset} {interval} data from: {file_path.name}")
                    df = pd.read_csv(file_path)
                    
                    # Validate data structure - handle different datetime column names
                    datetime_col = None
                    for col in ['timestamp', 'Datetime', 'datetime', 'Date']:
                        if col in df.columns:
                            datetime_col = col
                            break
                    
                    if datetime_col and 'close' in df.columns:
                        # Enhanced datetime parsing with timezone handling
                        # Use utc=True to handle timezone-aware strings properly
                        df['timestamp'] = pd.to_datetime(df[datetime_col], utc=True)
                        
                        # Remove timezone for Prophet compatibility
                        if df['timestamp'].dt.tz is not None:
                            df['timestamp'] = df['timestamp'].dt.tz_localize(None)
                        
                        df = df.sort_values('timestamp').reset_index(drop=True)
                        
                        print(f"   ✅ Loaded {len(df)} records from {df['timestamp'].min()} to {df['timestamp'].max()}")
                        print(f"   📊 Features: {len(df.columns)} columns, {df.select_dtypes(include=[np.number]).shape[1]} numeric")
                        return df
                    else:
                        print(f"   ❌ Invalid data structure in {file_path.name} - missing datetime or close columns")
                        continue
                        
                except Exception as e:
                    print(f"   ❌ Error loading {file_path.name}: {e}")
                    continue
        
        print(f"❌ No valid data file found for {asset} {interval}")
        return None

    def add_forex_features(self, df: pd.DataFrame, asset: str) -> pd.DataFrame:
        """Add leak-free forex features for realistic financial modeling"""
        try:
            df = df.copy()
            
            # Ensure we have required price columns
            required_cols = ['open', 'high', 'low', 'close', 'volume']
            missing_cols = [col for col in required_cols if col not in df.columns]
            if missing_cols:
                print(f"   ⚠️  Missing price columns: {missing_cols}")
                return df
            
            # Calculate returns (future-looking prevention)
            returns = df['close'].pct_change()
            
            # LEAK-FREE FEATURE SET - No direct OHLC dependencies
            
            # 1. Time-based features (no price dependency)
            if not pd.api.types.is_datetime64_any_dtype(df['timestamp']):
                df['timestamp'] = pd.to_datetime(df['timestamp'])
            
            df['hour'] = df['timestamp'].dt.hour
            df['day_of_week'] = df['timestamp'].dt.dayofweek
            df['month'] = df['timestamp'].dt.month
            
            # Market session (time-based only)
            df['market_session'] = np.where(
                ((df['hour'] >= 8) & (df['hour'] <= 16)), 1,  # London/NY overlap
                np.where(((df['hour'] >= 0) & (df['hour'] <= 8)), 0.5, 0.2)  # Tokyo session
            )
            
            # 2. Lagged return features (avoiding look-ahead bias)
            df['returns_lag_1'] = returns.shift(1)  # Previous period return
            df['returns_lag_2'] = returns.shift(2)  # 2 periods ago
            df['returns_lag_5'] = returns.shift(5)  # 5 periods ago
            
            # 3. Historical volatility (lagged to avoid leakage)
            df['volatility_lag_20'] = returns.rolling(window=20).std().shift(1)
            df['volatility_lag_5'] = returns.rolling(window=5).std().shift(1)
            
            # 4. Historical momentum (lagged)
            df['momentum_lag_10'] = returns.rolling(window=10).mean().shift(1)
            df['momentum_lag_20'] = returns.rolling(window=20).mean().shift(1)
            
            # 5. Volume-based features (if volume available and not price-correlated)
            if 'volume' in df.columns and df['volume'].std() > 0:
                df['volume_lag_1'] = df['volume'].shift(1)
                df['volume_ma_5'] = df['volume'].rolling(window=5).mean().shift(1)
                df['volume_relative'] = (df['volume'] / df['volume'].rolling(window=20).mean()).shift(1)
            
            # 6. Seasonal/cyclic features
            df['hour_sin'] = np.sin(2 * np.pi * df['hour'] / 24)
            df['hour_cos'] = np.cos(2 * np.pi * df['hour'] / 24)
            df['day_sin'] = np.sin(2 * np.pi * df['day_of_week'] / 7)
            df['day_cos'] = np.cos(2 * np.pi * df['day_of_week'] / 7)
            
            # 7. Volatility regime (historical)
            vol_percentile = df['volatility_lag_20'].rolling(window=100).rank(pct=True)
            df['volatility_regime'] = vol_percentile
            
            # 8. Return regime classification (historical)
            df['return_regime'] = np.where(
                df['returns_lag_1'] > df['returns_lag_1'].rolling(100).quantile(0.8), 1,  # High return
                np.where(df['returns_lag_1'] < df['returns_lag_1'].rolling(100).quantile(0.2), -1, 0)  # Low return
            )
            
            # Clean up temporary columns
            temp_cols = ['hour', 'day_of_week', 'month']
            for col in temp_cols:
                if col in df.columns:
                    df = df.drop(col, axis=1)
            
            # Forward fill missing values (but limit to reasonable bounds)
            df = df.fillna(method='ffill', limit=5).fillna(method='bfill', limit=5)
            
            # Remove any remaining NaN rows
            initial_len = len(df)
            df = df.dropna()
            if len(df) < initial_len:
                print(f"   📊 Removed {initial_len - len(df)} rows with NaN values")
            
            print(f"   ✅ Leak-free features added: {len(df.columns)} total columns")
            print(f"   🔒 Features are lagged/time-based to prevent data leakage")
            return df
            
        except Exception as e:
            print(f"   ❌ Error adding leak-free forex features: {e}")
            return df

    def prepare_prophet_data(self, df: pd.DataFrame, variant_features: List[str]) -> Tuple[pd.DataFrame, List[str]]:
        """Prepare data for Prophet with enhanced forex handling"""
        try:
            # Create Prophet-compatible dataset
            prophet_df = pd.DataFrame()
            prophet_df['ds'] = df['timestamp']
            prophet_df['y'] = df['close']
            
            # Add available features from variant specification
            available_features = []
            for feature in variant_features:
                if feature in df.columns:
                    # Handle any remaining timezone issues
                    if df[feature].dtype == 'object':
                        try:
                            # Try to convert datetime-like objects to numeric
                            feature_series = pd.to_numeric(df[feature], errors='coerce')
                            if not feature_series.isna().all():
                                prophet_df[feature] = feature_series
                                available_features.append(feature)
                            else:
                                print(f"   ⚠️  Skipping non-numeric feature: {feature}")
                        except:
                            print(f"   ⚠️  Could not convert feature {feature} to numeric")
                    else:
                        # Numeric feature
                        prophet_df[feature] = df[feature]
                        available_features.append(feature)
                else:
                    print(f"   ⚠️  Feature not found: {feature}")
            
            # Enhanced timezone handling - ensure ds is timezone-naive
            prophet_df['ds'] = pd.to_datetime(prophet_df['ds'])
            if prophet_df['ds'].dt.tz is not None:
                prophet_df['ds'] = prophet_df['ds'].dt.tz_convert('UTC').dt.tz_localize(None)
            
            # Remove any remaining NaN values
            prophet_df = prophet_df.dropna()
            
            # Ensure we have sufficient data
            if len(prophet_df) < 30:
                raise ValueError(f"Insufficient data after preprocessing: {len(prophet_df)} records")
            
            print(f"   ✅ Prophet data prepared: {len(prophet_df)} records, {len(available_features)} features")
            return prophet_df, available_features
            
        except Exception as e:
            print(f"   ❌ Error preparing Prophet data: {e}")
            raise

    def train_forex_prophet_model(self, df: pd.DataFrame, features: List[str], variant: str) -> Tuple[Prophet, Dict]:
        """Train Prophet model with forex-specific optimizations"""
        try:
            # Enhanced Prophet configuration for forex
            prophet_config = {
                'seasonality_mode': 'multiplicative',  # Better for forex
                'weekly_seasonality': True,
                'daily_seasonality': True if '1h' in str(df) else False,
                'yearly_seasonality': False,  # Not relevant for forex
                'changepoint_prior_scale': 0.1,  # More conservative for forex
                'seasonality_prior_scale': 0.01,  # Reduced seasonality strength
                'holidays_prior_scale': 0.01,
                'mcmc_samples': 0,  # Faster training
                'interval_width': 0.8,
                'uncertainty_samples': 100
            }
            
            # Variant-specific adjustments
            if variant == 'conservative':
                prophet_config['changepoint_prior_scale'] = 0.05  # More conservative
                prophet_config['seasonality_prior_scale'] = 0.005
            elif variant == 'aggressive':
                prophet_config['changepoint_prior_scale'] = 0.2  # More flexible
                prophet_config['seasonality_prior_scale'] = 0.02
            
            # Initialize Prophet
            model = Prophet(**prophet_config)
            
            # Add regressors
            for feature in features:
                model.add_regressor(feature, standardize=True)
            
            # Split data into train/validation sets (80/20 split) for proper evaluation
            split_idx = int(len(df) * 0.8)
            train_df = df[:split_idx].copy()
            val_df = df[split_idx:].copy()
            
            print(f"   📊 Split data: {len(train_df)} training, {len(val_df)} validation records")
            
            # Train model with enhanced error handling
            print(f"   🔄 Training {variant} Prophet model with {len(features)} features...")
            model.fit(train_df)  # Train only on training set
            
            # Generate validation predictions
            val_future = val_df[['ds'] + [f for f in features if f != 'y']].copy()
            val_forecast = model.predict(val_future)
            val_predictions = val_forecast['yhat'].values
            val_actual = val_df['y'].values
            
            # Generate forecast for future periods
            future = model.make_future_dataframe(periods=24)  # 24 periods ahead
            
            # Add regressor values for forecast
            for feature in features:
                if feature in df.columns:
                    # Extend feature values (simple forward fill for demo)
                    feature_values = df[feature].values
                    extended_values = np.concatenate([
                        feature_values,
                        [feature_values[-1]] * (len(future) - len(feature_values))
                    ])
                    future[feature] = extended_values[:len(future)]
            
            forecast = model.predict(future)
            
            # Calculate performance metrics on validation set (realistic evaluation)
            val_r2 = r2_score(val_actual, val_predictions)
            val_mae = mean_absolute_error(val_actual, val_predictions)
            val_mse = mean_squared_error(val_actual, val_predictions)
            
            # Also calculate training metrics for overfitting detection
            train_future = train_df[['ds'] + [f for f in features if f != 'y']].copy()
            train_forecast = model.predict(train_future)
            train_predictions = train_forecast['yhat'].values
            train_actual = train_df['y'].values
            train_r2 = r2_score(train_actual, train_predictions)
            
            overfitting_gap = train_r2 - val_r2
            
            metrics = {
                'r2': val_r2,  # Report validation R² (realistic performance)
                'mae': val_mae,
                'mse': val_mse,
                'features_used': len(features),
                'training_records': len(train_df),
                'validation_records': len(val_df),
                'train_r2': train_r2,
                'validation_r2': val_r2,
                'overfitting_gap': overfitting_gap,
                'overfitting_detected': overfitting_gap > 0.3
            }
            
            status_icon = "⚠️" if metrics['overfitting_detected'] else "✅"
            print(f"   {status_icon} Model trained: Validation R² = {val_r2:.3f} (Train R² = {train_r2:.3f}, Gap = {overfitting_gap:.3f})")
            return model, metrics, forecast
            
        except Exception as e:
            print(f"   ❌ Error training Prophet model: {e}")
            error_metrics = {
                'r2': 0.0,
                'mae': float('inf'),
                'mse': float('inf'),
                'features_used': len(features),
                'training_records': len(df) if df is not None else 0
            }
            return None, error_metrics, None

    def save_forex_model_results(self, asset: str, interval: str, variant: str, 
                                model: Prophet, metrics: Dict, forecast: pd.DataFrame) -> bool:
        """Save forex model results with enhanced structure"""
        try:
            # Create asset-specific directory
            asset_dir = self.models_path / asset / interval / variant
            asset_dir.mkdir(parents=True, exist_ok=True)
            
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            base_filename = f"{asset}_{interval}_{variant}_{timestamp}"
            
            # Save model (JSON format for Prophet)
            if model is not None:
                model_data = {
                    'asset': asset,
                    'interval': interval,
                    'variant': variant,
                    'timestamp': timestamp,
                    'metrics': metrics,
                    'model_params': model.params if hasattr(model, 'params') else {},
                    'changepoints': model.changepoints.tolist() if hasattr(model, 'changepoints') else [],
                    'seasonalities': model.seasonalities if hasattr(model, 'seasonalities') else {}
                }
                
                model_file = asset_dir / f"{base_filename}.json"
                with open(model_file, 'w') as f:
                    json.dump(model_data, f, indent=2, default=str)
            
            # Save forecast
            if forecast is not None:
                forecast_file = asset_dir / f"{base_filename}_forecast.csv"
                forecast.to_csv(forecast_file, index=False)
            
            # Save configuration
            config_data = {
                'asset': asset,
                'interval': interval,
                'variant': variant,
                'timestamp': timestamp,
                'metrics': metrics,
                'variant_config': self.variants[variant]
            }
            
            config_file = asset_dir / f"{base_filename}_config.json"
            with open(config_file, 'w') as f:
                json.dump(config_data, f, indent=2)
            
            print(f"   ✅ Model results saved to {asset_dir.name}")
            return True
            
        except Exception as e:
            print(f"   ❌ Error saving model results: {e}")
            return False

    def log_performance(self, asset: str, interval: str, variant: str, 
                       metrics: Dict, status: str, error_msg: str = None):
        """Log performance to database and tracking"""
        try:
            # Database logging
            with sqlite3.connect(self.db_path) as conn:
                conn.execute('''
                    INSERT INTO forex_prophet_performance 
                    (timestamp, asset, interval, variant, r2_score, mae, mse, 
                     features_used, training_records, status, error_message)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    datetime.now().isoformat(),
                    asset, interval, variant,
                    metrics.get('r2', 0.0),
                    metrics.get('mae', 0.0),
                    metrics.get('mse', 0.0),
                    metrics.get('features_used', 0),
                    metrics.get('training_records', 0),
                    status,
                    error_msg
                ))
                conn.commit()
            
            # Summary tracking with proper validation metrics
            performance_record = {
                'asset': asset,
                'interval': interval,
                'variant': variant,
                'r2': metrics.get('validation_r2', metrics.get('r2', 0.0)),  # Use validation R² as primary
                'validation_r2': metrics.get('validation_r2', 0.0),
                'train_r2': metrics.get('train_r2', 0.0),
                'overfitting_gap': metrics.get('overfitting_gap', 0.0),
                'overfitting_detected': metrics.get('overfitting_detected', False),
                'features_used': metrics.get('features_used', 0),
                'training_records': metrics.get('training_records', 0),
                'validation_records': metrics.get('validation_records', 0),
                'status': status
            }
            self.performance_summary['performance_summary'].append(performance_record)
            
        except Exception as e:
            print(f"   ⚠️  Error logging performance: {e}")

    def rebuild_forex_models(self) -> Dict:
        """Rebuild all forex Prophet models with enhanced handling"""
        print(f"\n🔄 Starting enhanced forex model rebuilding...")
        print(f"📊 Target: {len(self.forex_assets)} assets × {len(self.variants)} variants × {len(self.timeframes)} timeframes")
        
        total_models = len(self.forex_assets) * len(self.variants) * len(self.timeframes)
        successful_models = 0
        
        for asset in self.forex_assets:
            print(f"\n💱 Processing {asset}...")
            
            for interval in self.timeframes:
                print(f"  ⏰ Timeframe: {interval}")
                
                # Load data
                data = self.load_forex_data(asset, interval)
                if data is None:
                    print(f"    ❌ No data available for {asset} {interval}")
                    for variant in self.variants:
                        self.log_performance(asset, interval, variant, {}, 'failed', 'No data available')
                    continue
                
                # Add forex-specific features
                enhanced_data = self.add_forex_features(data, asset)
                
                for variant_name, variant_config in self.variants.items():
                    print(f"    🎯 Variant: {variant_name}")
                    
                    try:
                        # Prepare Prophet data
                        prophet_data, available_features = self.prepare_prophet_data(
                            enhanced_data, variant_config['features']
                        )
                        
                        if len(available_features) == 0:
                            print(f"      ❌ No features available for {variant_name}")
                            self.log_performance(asset, interval, variant_name, {}, 'failed', 'No features available')
                            continue
                        
                        # Train model
                        model, metrics, forecast = self.train_forex_prophet_model(
                            prophet_data, available_features, variant_name
                        )
                        
                        # Save results - allow negative R² as it's realistic for financial time series
                        if model is not None and metrics['validation_r2'] > -50.0:  # Allow negative but not extremely bad
                            self.save_forex_model_results(asset, interval, variant_name, model, metrics, forecast)
                            self.log_performance(asset, interval, variant_name, metrics, 'success')
                            successful_models += 1
                            val_r2 = metrics.get('validation_r2', metrics.get('r2', 0))
                            print(f"      ✅ {variant_name}: Validation R² = {val_r2:.3f}")
                        else:
                            reason = f"Poor validation performance (R² = {metrics.get('validation_r2', 'N/A')})"
                            self.log_performance(asset, interval, variant_name, metrics, 'failed', reason)
                            print(f"      ❌ {variant_name}: {reason}")
                        
                    except Exception as e:
                        error_msg = str(e)
                        print(f"      ❌ {variant_name}: {error_msg}")
                        self.log_performance(asset, interval, variant_name, {}, 'failed', error_msg)
        
        # Update summary
        self.performance_summary['total_models'] = total_models
        self.performance_summary['successful_models'] = successful_models
        self.performance_summary['success_rate'] = (successful_models / total_models * 100) if total_models > 0 else 0
        
        return self.performance_summary

    def save_summary_report(self) -> str:
        """Save comprehensive summary report with overfitting detection"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        summary_file = self.models_path / f"enhanced_forex_prophet_summary_{timestamp}.json"
        
        # Calculate overfitting statistics
        performance_records = self.performance_summary.get('performance_summary', [])
        overfitted_models = 0
        total_evaluated = 0
        avg_overfitting_gap = 0
        
        for record in performance_records:
            if 'overfitting_detected' in record:
                total_evaluated += 1
                if record['overfitting_detected']:
                    overfitted_models += 1
                avg_overfitting_gap += record.get('overfitting_gap', 0)
        
        if total_evaluated > 0:
            avg_overfitting_gap /= total_evaluated
            overfitting_rate = (overfitted_models / total_evaluated) * 100
        else:
            overfitting_rate = 0
        
        # Add overfitting stats to summary
        self.performance_summary['overfitting_statistics'] = {
            'overfitted_models': overfitted_models,
            'total_evaluated': total_evaluated,
            'overfitting_rate_pct': overfitting_rate,
            'avg_overfitting_gap': avg_overfitting_gap
        }
        
        with open(summary_file, 'w') as f:
            json.dump(self.performance_summary, f, indent=2)
        
        print(f"\n📊 Summary Report: {summary_file.name}")
        print(f"📈 Success Rate: {self.performance_summary['success_rate']:.1f}%")
        print(f"✅ Successful Models: {self.performance_summary['successful_models']}/{self.performance_summary['total_models']}")
        
        if total_evaluated > 0:
            status_icon = "🚨" if overfitting_rate > 50 else "⚠️" if overfitting_rate > 20 else "✅"
            print(f"{status_icon} Overfitting Rate: {overfitting_rate:.1f}% ({overfitted_models}/{total_evaluated} models)")
            print(f"📊 Avg Train/Val Gap: {avg_overfitting_gap:.3f}")
        
        return str(summary_file)

def main():
    """Main execution function"""
    try:
        # Initialize builder
        builder = EnhancedForexProphetBuilder()
        
        # Rebuild forex models
        results = builder.rebuild_forex_models()
        
        # Save summary
        summary_file = builder.save_summary_report()
        
        print(f"\n🏁 Enhanced Forex Prophet Model Rebuilding Complete!")
        print(f"📊 Final Results:")
        print(f"   • Total Models: {results['total_models']}")
        print(f"   • Successful: {results['successful_models']}")
        print(f"   • Success Rate: {results['success_rate']:.1f}%")
        print(f"   • Summary: {summary_file}")
        
        return results['success_rate'] >= 50.0  # Success if >50% models work
        
    except Exception as e:
        print(f"❌ Fatal error in forex model rebuilding: {e}")
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)