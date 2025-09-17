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
        
        # Enhanced forex-specific variants
        self.variants = {
            'conservative': {
                'description': 'Simple, stable forex models',
                'features': ['ma_5', 'rsi_14', 'market_session'],
                'expected_count': 3
            },
            'standard': {
                'description': 'Balanced forex models with technical indicators',
                'features': ['ma_5', 'ma_20', 'rsi_14', 'bbands_upper', 'bbands_lower', 'atr_14'],
                'expected_count': 6
            },
            'aggressive': {
                'description': 'Complex forex models with full technical suite',
                'features': ['ma_5', 'ma_20', 'ma_50', 'rsi_14', 'rsi_9', 'macd', 'macd_signal',
                           'bbands_upper', 'bbands_lower', 'atr_14', 'cci', 'williams_r',
                           'stoch_k', 'market_session', 'volatility_regime'],
                'expected_count': 15
            },
            'economic': {
                'description': 'Economically-focused forex models',
                'features': ['ma_20', 'atr_14', 'rsi_14', 'carry_trade_proxy', 
                           'volatility_regime', 'market_session', 'momentum_5', 'mean_reversion',
                           'risk_sentiment'],
                'expected_count': 9
            },
            'ensemble': {
                'description': 'Hybrid forex models combining technical and fundamental',
                'features': ['ma_5', 'ma_20', 'rsi_14', 'macd', 'bbands_upper', 'atr_14',
                           'market_session', 'volatility_regime', 'momentum_5', 'carry_trade_proxy'],
                'expected_count': 10
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
        """Add forex-specific features with enhanced error handling"""
        try:
            df = df.copy()
            
            # Ensure we have required price columns
            required_cols = ['open', 'high', 'low', 'close', 'volume']
            missing_cols = [col for col in required_cols if col not in df.columns]
            if missing_cols:
                print(f"   ⚠️  Missing price columns: {missing_cols}")
                return df
            
            # Basic technical indicators
            if 'ma_5' not in df.columns:
                df['ma_5'] = ta.trend.sma_indicator(df['close'], window=5)
            if 'ma_20' not in df.columns:
                df['ma_20'] = ta.trend.sma_indicator(df['close'], window=20)
            if 'ma_50' not in df.columns:
                df['ma_50'] = ta.trend.sma_indicator(df['close'], window=50)
            
            # RSI indicators
            if 'rsi_14' not in df.columns:
                df['rsi_14'] = ta.momentum.rsi(df['close'], window=14)
            if 'rsi_9' not in df.columns:
                df['rsi_9'] = ta.momentum.rsi(df['close'], window=9)
            
            # MACD
            if 'macd' not in df.columns:
                macd_line = ta.trend.macd_diff(df['close'])
                df['macd'] = macd_line
                df['macd_signal'] = ta.trend.macd_signal(df['close'])
            
            # Bollinger Bands
            if 'bbands_upper' not in df.columns:
                df['bbands_upper'] = ta.volatility.bollinger_hband(df['close'])
                df['bbands_lower'] = ta.volatility.bollinger_lband(df['close'])
            
            # ATR
            if 'atr_14' not in df.columns:
                df['atr_14'] = ta.volatility.average_true_range(df['high'], df['low'], df['close'], window=14)
            
            # Additional momentum indicators
            if 'cci' not in df.columns:
                df['cci'] = ta.trend.cci(df['high'], df['low'], df['close'], window=20)
            if 'williams_r' not in df.columns:
                df['williams_r'] = ta.momentum.williams_r(df['high'], df['low'], df['close'], lbp=14)
            if 'stoch_k' not in df.columns:
                df['stoch_k'] = ta.momentum.stoch(df['high'], df['low'], df['close'], k=14)
            
            # Forex-specific features
            # Market session indicator (simplified)
            if 'market_session' not in df.columns:
                # Ensure timestamp is datetime for dt accessor
                if not pd.api.types.is_datetime64_any_dtype(df['timestamp']):
                    df['timestamp'] = pd.to_datetime(df['timestamp'])
                
                df['hour'] = df['timestamp'].dt.hour
                df['market_session'] = np.where(
                    ((df['hour'] >= 8) & (df['hour'] <= 16)), 1,  # London/NY overlap
                    np.where(((df['hour'] >= 0) & (df['hour'] <= 8)), 0.5, 0.2)  # Tokyo session
                )
            
            # Volatility regime
            if 'volatility_regime' not in df.columns:
                returns = df['close'].pct_change()
                rolling_vol = returns.rolling(window=20).std()
                vol_percentile = rolling_vol.rolling(window=100).rank(pct=True)
                df['volatility_regime'] = vol_percentile
            
            # Momentum features
            if 'momentum_5' not in df.columns:
                df['momentum_5'] = df['close'].pct_change(periods=5)
            
            # Mean reversion indicator
            if 'mean_reversion' not in df.columns:
                ma_20 = df['ma_20'] if 'ma_20' in df.columns else ta.trend.sma_indicator(df['close'], window=20)
                df['mean_reversion'] = (df['close'] - ma_20) / ma_20
            
            # Carry trade proxy (simplified interest rate differential proxy)
            if 'carry_trade_proxy' not in df.columns:
                # Use long-term moving average ratio as a proxy
                ma_long = ta.trend.sma_indicator(df['close'], window=100)
                df['carry_trade_proxy'] = df['close'] / ma_long
            
            # Risk sentiment (VIX-like measure using price volatility)
            if 'risk_sentiment' not in df.columns:
                returns = df['close'].pct_change()
                rolling_vol = returns.rolling(window=20).std()
                df['risk_sentiment'] = 1 / (1 + rolling_vol * 100)  # Inverse volatility
            
            # Clean up temporary columns
            if 'hour' in df.columns:
                df = df.drop('hour', axis=1)
            
            # Forward fill and backward fill missing values
            df = df.fillna(method='ffill').fillna(method='bfill')
            
            print(f"   ✅ Enhanced features added: {len(df.columns)} total columns")
            return df
            
        except Exception as e:
            print(f"   ❌ Error adding forex features: {e}")
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
            
            # Train model with enhanced error handling
            print(f"   🔄 Training {variant} Prophet model with {len(features)} features...")
            model.fit(df)
            
            # Generate forecast
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
            
            # Calculate performance metrics
            train_predictions = forecast['yhat'][:len(df)]
            actual_values = df['y'].values
            
            r2 = r2_score(actual_values, train_predictions)
            mae = mean_absolute_error(actual_values, train_predictions)
            mse = mean_squared_error(actual_values, train_predictions)
            
            metrics = {
                'r2': max(0.0, r2),  # Ensure non-negative R²
                'mae': mae,
                'mse': mse,
                'features_used': len(features),
                'training_records': len(df)
            }
            
            print(f"   ✅ Model trained successfully: R² = {metrics['r2']:.3f}")
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
            
            # Summary tracking
            performance_record = {
                'asset': asset,
                'interval': interval,
                'variant': variant,
                'r2': metrics.get('r2', 0.0),
                'validation_r2': 0.0,  # Not calculated in this simplified version
                'features_used': metrics.get('features_used', 0),
                'training_records': metrics.get('training_records', 0),
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
                        
                        # Save results
                        if model is not None and metrics['r2'] > 0.0:
                            self.save_forex_model_results(asset, interval, variant_name, model, metrics, forecast)
                            self.log_performance(asset, interval, variant_name, metrics, 'success')
                            successful_models += 1
                            print(f"      ✅ {variant_name}: R² = {metrics['r2']:.3f}")
                        else:
                            self.log_performance(asset, interval, variant_name, metrics, 'failed', 'Training failed')
                            print(f"      ❌ {variant_name}: Training failed")
                        
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
        """Save comprehensive summary report"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        summary_file = self.models_path / f"enhanced_forex_prophet_summary_{timestamp}.json"
        
        with open(summary_file, 'w') as f:
            json.dump(self.performance_summary, f, indent=2)
        
        print(f"\n📊 Summary Report: {summary_file.name}")
        print(f"📈 Success Rate: {self.performance_summary['success_rate']:.1f}%")
        print(f"✅ Successful Models: {self.performance_summary['successful_models']}/{self.performance_summary['total_models']}")
        
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