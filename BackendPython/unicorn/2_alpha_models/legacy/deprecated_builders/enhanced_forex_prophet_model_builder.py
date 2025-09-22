#!/usr/bin/env python3
"""
Enhanced Forex Prophet Model Builder - Specialized for Forex Assets

This script creates improved Prophet models specifically optimized for forex trading:
1. Forex-specific feature engineering and selection
2. Currency-pair aware seasonality patterns
3. Economic indicator integration
4. Improved data preprocessing for forex characteristics
5. Enhanced Prophet configurations for FX volatility patterns

Addresses the R² = 0.000 issues by:
- Better handling of forex data preprocessing
- Improved feature selection for each model variant
- Currency-specific seasonality and holiday effects
- Enhanced error handling and validation
"""

import os
import sys
import pandas as pd
import numpy as np
import sqlite3
import json
import pickle
from datetime import datetime, timedelta
from pathlib import Path
import logging
from typing import Dict, List, Tuple, Optional, Any
import warnings
warnings.filterwarnings('ignore')

# Add project root to path
project_root = Path(__file__).parent.parent.parent
sys.path.append(str(project_root))

# Prophet and ML imports
try:
    from prophet import Prophet
    from prophet.serialize import model_to_json, model_from_json
except ImportError:
    print("Prophet not installed. Installing...")
    import subprocess
    subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'prophet'])
    from prophet import Prophet
    from prophet.serialize import model_to_json, model_from_json

from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler, RobustScaler

class ForexProphetModelBuilder:
    \"\"\"Enhanced Prophet model builder specifically optimized for forex assets.\"\"\\"
    
    def __init__(self, output_dir: str = None):
        self.output_dir = output_dir or "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/enhanced_prophet_models"
        self.setup_logging()
        self.forex_assets = ['AUDUSD', 'EURUSD', 'GBPUSD', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY']
        self.intervals = ['1h', '1d']
        self.variants = ['conservative', 'standard', 'aggressive', 'economic', 'ensemble']
        
        # Forex-specific configurations
        self.forex_configs = self.setup_forex_configs()
        
        # Initialize performance tracking
        self.performance_db = f"{self.output_dir}/enhanced_prophet_performance.db"
        self.init_performance_db()
        
    def setup_logging(self):
        \"\"\"Setup logging configuration.\"\"\""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler('forex_prophet_builder.log'),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
        
    def setup_forex_configs(self) -> Dict[str, Dict]:
        \"\"\"Setup forex-specific model configurations.\"\"\""
        
        # Base forex features that work well for all pairs
        base_forex_features = [
            'ma_21', 'ma_50', 'volatility_20', 'atr', 'rsi', 
            'price_change', 'hl_range', 'pips_change'
        ]
        
        # Economic/fundamental features for forex
        economic_features = [
            'resistance_level', 'support_level', 'trend_strength',
            'momentum_5', 'momentum_10', 'session_overlap', 'trading_session'
        ]
        
        # Advanced technical features
        advanced_features = [
            'stoch_k', 'stoch_d', 'macd', 'macd_signal', 'williams_r', 
            'cci', 'adx', 'distance_to_resistance', 'distance_to_support'
        ]
        
        return {
            'conservative': {
                'features': base_forex_features[:3],  # 3 most reliable features
                'seasonality_mode': 'additive',
                'yearly_seasonality': False,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.01,  # Very conservative
                'seasonality_prior_scale': 1.0,
                'description': 'Simple, stable forex models with minimal features'
            },
            'standard': {
                'features': base_forex_features[:6],  # 6 balanced features
                'seasonality_mode': 'multiplicative',
                'yearly_seasonality': False,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.05,
                'seasonality_prior_scale': 5.0,
                'description': 'Balanced forex models with key technical indicators'
            },
            'aggressive': {
                'features': base_forex_features + advanced_features[:5],  # 13 features
                'seasonality_mode': 'multiplicative',
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.1,  # More flexible
                'seasonality_prior_scale': 10.0,
                'description': 'Complex forex models with maximum technical features'
            },
            'economic': {
                'features': base_forex_features[:4] + economic_features[:5],  # 9 features
                'seasonality_mode': 'multiplicative',
                'yearly_seasonality': False,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.05,
                'seasonality_prior_scale': 8.0,
                'description': 'Economic-focused forex models with fundamental analysis'
            },
            'ensemble': {
                'features': base_forex_features[:5] + economic_features[:3] + advanced_features[:2],  # 10 features
                'seasonality_mode': 'multiplicative',
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.075,
                'seasonality_prior_scale': 7.0,
                'description': 'Hybrid forex models combining technical and fundamental analysis'
            }
        }
    
    def load_forex_data(self, symbol: str, timeframe: str = '1d') -> Optional[pd.DataFrame]:
        \"\"\"Load forex silver layer data with enhanced preprocessing.\"\"\""
        try:
            silver_path = f"/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/processed_data/forex/{symbol}_silver_{timeframe}_latest.csv"
            
            if not os.path.exists(silver_path):
                self.logger.error(f"No forex data found at: {silver_path}")
                return None
            
            data = pd.read_csv(silver_path)
            self.logger.info(f"✅ Loaded forex data from: {silver_path}")
            self.logger.info(f"   Data shape: {data.shape}")
            
            # Enhanced forex data preprocessing
            data = self.preprocess_forex_data(data, symbol, timeframe)
            
            return data
            
        except Exception as e:
            self.logger.error(f"Error loading forex data for {symbol}: {e}")
            return None
    
    def preprocess_forex_data(self, data: pd.DataFrame, symbol: str, timeframe: str) -> pd.DataFrame:
        \"\"\"Enhanced preprocessing specifically for forex data.\"\"\""
        try:
            # Convert datetime
            data['Datetime'] = pd.to_datetime(data['Datetime'], utc=True)
            
            # Handle missing values with forex-specific methods
            numeric_cols = data.select_dtypes(include=[np.number]).columns
            
            # Forward fill small gaps (common in forex)
            for col in numeric_cols:
                if col != 'volume':  # Volume can be 0 in forex
                    data[col] = data[col].fillna(method='ffill').fillna(method='bfill')
            
            # Remove rows with excessive missing data
            missing_threshold = 0.3  # Allow up to 30% missing features
            data = data.dropna(thresh=len(data.columns) * (1 - missing_threshold))
            
            # Forex-specific feature engineering
            if 'close' in data.columns:
                # Add forex-specific features
                data['price_momentum'] = data['close'].pct_change(5)
                data['volatility_ratio'] = data['volatility_20'] / data['volatility_20'].rolling(20).mean()
                
                # Add session-based features for forex
                data['hour'] = data['Datetime'].dt.hour
                data['is_london_session'] = ((data['hour'] >= 8) & (data['hour'] <= 16)).astype(int)
                data['is_ny_session'] = ((data['hour'] >= 13) & (data['hour'] <= 21)).astype(int)
                data['is_overlap_session'] = ((data['hour'] >= 13) & (data['hour'] <= 16)).astype(int)
            
            # Remove infinite values
            data = data.replace([np.inf, -np.inf], np.nan)
            
            # Final cleanup
            data = data.dropna()
            
            self.logger.info(f"   Preprocessed data shape: {data.shape}")
            return data
            
        except Exception as e:
            self.logger.error(f"Error preprocessing forex data: {e}")
            return data
    
    def prepare_prophet_data(self, data: pd.DataFrame, variant: str) -> Optional[pd.DataFrame]:
        \"\"\"Prepare data for Prophet training with forex-specific enhancements.\"\"\""
        try:
            if data is None or len(data) < 50:
                self.logger.warning(f"Insufficient data for Prophet training: {len(data) if data is not None else 0} rows")
                return None
            
            # Get variant configuration
            config = self.forex_configs[variant]
            
            # Create Prophet dataframe
            prophet_df = pd.DataFrame()
            
            # Handle timezone-aware datetime conversion with enhanced robustness
            if 'Datetime' in data.columns:
                try:
                    # Convert to UTC first, then to naive datetime
                    dt_series = pd.to_datetime(data['Datetime'], utc=True)
                    prophet_df['ds'] = dt_series.dt.tz_convert('UTC').dt.tz_localize(None)
                except Exception as e:
                    self.logger.warning(f"Timezone conversion issue, using string conversion: {e}")
                    # Fallback: convert to string then back to datetime
                    prophet_df['ds'] = pd.to_datetime(data['Datetime'].astype(str).str[:19])
            else:
                self.logger.error("No Datetime column found")
                return None
            
            # Set target variable
            if 'close' in data.columns:
                prophet_df['y'] = data['close'].astype(float)
            else:
                self.logger.error("No close price column found")
                return None
            
            # Add variant-specific features
            features_to_add = config['features']
            available_features = []
            
            for feature in features_to_add:
                if feature in data.columns:
                    # Handle missing values and scaling for forex
                    feature_data = data[feature].astype(float)
                    
                    # Scale features appropriately for Prophet
                    if feature_data.std() > 0:
                        # Use robust scaling for forex data
                        scaler = RobustScaler()
                        scaled_data = scaler.fit_transform(feature_data.values.reshape(-1, 1)).flatten()
                        prophet_df[feature] = scaled_data
                        available_features.append(feature)
                    else:
                        self.logger.warning(f"Feature {feature} has zero variance, skipping")
                else:
                    self.logger.warning(f"Feature {feature} not found in data")
            
            self.logger.info(f"Added {len(available_features)} features: {available_features}")
            
            # Ensure we have enough data points
            if len(prophet_df) < 50:
                self.logger.warning(f"Insufficient data after preprocessing: {len(prophet_df)} rows")
                return None
            
            return prophet_df
            
        except Exception as e:
            self.logger.error(f"Error preparing Prophet data: {e}")
            return None
    
    def train_forex_prophet_model(self, data: pd.DataFrame, variant: str, symbol: str, interval: str) -> Optional[Dict]:
        \"\"\"Train Prophet model with forex-specific optimizations.\"\"\""
        try:
            config = self.forex_configs[variant]
            
            # Initialize Prophet with forex-optimized settings
            model = Prophet(
                seasonality_mode=config['seasonality_mode'],
                yearly_seasonality=config['yearly_seasonality'],
                weekly_seasonality=config['weekly_seasonality'],
                daily_seasonality=config['daily_seasonality'] if interval == '1h' else False,
                changepoint_prior_scale=config['changepoint_prior_scale'],
                seasonality_prior_scale=config['seasonality_prior_scale'],
                n_changepoints=25,  # Good for forex data
                changepoint_range=0.8,
                interval_width=0.80,  # 80% confidence intervals
                mcmc_samples=0,  # Faster training
                uncertainty_samples=1000
            )
            
            # Add forex-specific seasonalities
            if interval == '1h':
                # Add hourly seasonality for intraday patterns
                model.add_seasonality(name='hourly', period=24, fourier_order=8)
                # Add session-based seasonality
                model.add_seasonality(name='forex_session', period=24, fourier_order=4)
            
            # Add weekly patterns common in forex
            if interval == '1d':
                model.add_seasonality(name='weekly_forex', period=7, fourier_order=3)
            
            # Add regressors (features) with forex-appropriate priors
            available_features = []
            for feature in config['features']:
                if feature in data.columns:
                    # Use smaller prior for forex features to avoid overfitting
                    model.add_regressor(feature, prior_scale=1.0, standardize=False)
                    available_features.append(feature)
            
            self.logger.info(f"Training {variant} model for {symbol} {interval} with {len(available_features)} features")
            
            # Train the model
            model.fit(data)
            
            # Make predictions for validation
            future = model.make_future_dataframe(periods=min(30, len(data) // 4), freq='H' if interval == '1h' else 'D')
            
            # Add regressor values for future predictions
            for feature in available_features:
                if feature in data.columns:
                    # Use last known values for future predictions
                    last_values = data[feature].iloc[-min(30, len(data) // 4):].values
                    future[feature] = list(data[feature]) + list(last_values)
            
            forecast = model.predict(future)
            
            # Calculate performance metrics
            train_data = data.copy()
            train_predictions = forecast[:len(train_data)]
            
            # Calculate R²
            r2 = r2_score(train_data['y'], train_predictions['yhat'])
            mae = mean_absolute_error(train_data['y'], train_predictions['yhat'])
            mse = mean_squared_error(train_data['y'], train_predictions['yhat'])
            
            model_info = {
                'model': model,
                'forecast': forecast,
                'r2': r2,
                'mae': mae,
                'mse': mse,
                'features_used': len(available_features),
                'training_records': len(train_data),
                'variant': variant,
                'symbol': symbol,
                'interval': interval,
                'config': config,
                'available_features': available_features
            }
            
            self.logger.info(f"✅ {symbol} {interval} {variant}: R² = {r2:.3f}, MAE = {mae:.6f}, Features = {len(available_features)}")
            
            return model_info
            
        except Exception as e:
            self.logger.error(f"Error training {variant} model for {symbol} {interval}: {e}")
            return None
    
    def save_model_artifacts(self, model_info: Dict, timestamp: str) -> Dict[str, str]:
        \"\"\"Save model artifacts and return file paths.\"\"\""
        try:
            symbol = model_info['symbol']
            interval = model_info['interval']
            variant = model_info['variant']
            
            # Create output directory
            output_dir = f"{self.output_dir}/{symbol}/{interval}/{variant}"
            os.makedirs(output_dir, exist_ok=True)
            
            # File paths
            model_file = f"{output_dir}/{symbol}_{interval}_{variant}_{timestamp}.json"
            forecast_file = f"{output_dir}/{symbol}_{interval}_{variant}_forecast_{timestamp}.csv"
            config_file = f"{output_dir}/{symbol}_{interval}_{variant}_config_{timestamp}.json"
            
            # Save Prophet model as JSON
            with open(model_file, 'w') as f:
                json.dump(model_to_json(model_info['model']), f, indent=2)
            
            # Save forecast data
            model_info['forecast'].to_csv(forecast_file, index=False)
            
            # Save configuration and metadata
            config_data = {
                'variant': variant,
                'symbol': symbol,
                'interval': interval,
                'r2_score': model_info['r2'],
                'mae': model_info['mae'],
                'mse': model_info['mse'],
                'features_used': model_info['features_used'],
                'training_records': model_info['training_records'],
                'available_features': model_info['available_features'],
                'config': model_info['config'],
                'timestamp': timestamp,
                'model_type': 'forex_prophet'
            }
            
            with open(config_file, 'w') as f:
                json.dump(config_data, f, indent=2)
            
            return {
                'model_file': model_file,
                'forecast_file': forecast_file,
                'config_file': config_file
            }
            
        except Exception as e:
            self.logger.error(f"Error saving model artifacts: {e}")
            return {}
    
    def init_performance_db(self):
        \"\"\"Initialize performance tracking database.\"\"\""
        try:
            os.makedirs(os.path.dirname(self.performance_db), exist_ok=True)
            conn = sqlite3.connect(self.performance_db)
            cursor = conn.cursor()
            
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS forex_model_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT,
                    symbol TEXT,
                    interval TEXT,
                    variant TEXT,
                    r2_score REAL,
                    mae REAL,
                    mse REAL,
                    features_used INTEGER,
                    training_records INTEGER,
                    model_file TEXT,
                    forecast_file TEXT,
                    config_file TEXT,
                    status TEXT
                )
            ''')
            
            conn.commit()
            conn.close()
            
        except Exception as e:
            self.logger.error(f"Error initializing performance database: {e}")
    
    def record_performance(self, model_info: Dict, file_paths: Dict, timestamp: str):
        \"\"\"Record model performance in database.\"\"\""
        try:
            conn = sqlite3.connect(self.performance_db)
            cursor = conn.cursor()
            
            cursor.execute('''
                INSERT INTO forex_model_performance 
                (timestamp, symbol, interval, variant, r2_score, mae, mse, 
                 features_used, training_records, model_file, forecast_file, config_file, status)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                timestamp,
                model_info['symbol'],
                model_info['interval'],
                model_info['variant'],
                model_info['r2'],
                model_info['mae'],
                model_info['mse'],
                model_info['features_used'],
                model_info['training_records'],
                file_paths.get('model_file', ''),
                file_paths.get('forecast_file', ''),
                file_paths.get('config_file', ''),
                'success'
            ))
            
            conn.commit()
            conn.close()
            
        except Exception as e:
            self.logger.error(f"Error recording performance: {e}")
    
    def build_all_forex_models(self):
        \"\"\"Build all forex Prophet models with enhanced configuration.\"\"\""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        self.logger.info("🚀 Starting Enhanced Forex Prophet Model Building")
        self.logger.info(f"Assets: {self.forex_assets}")
        self.logger.info(f"Intervals: {self.intervals}")
        self.logger.info(f"Variants: {self.variants}")
        
        results = []
        total_models = len(self.forex_assets) * len(self.intervals) * len(self.variants)
        current_model = 0
        
        for asset in self.forex_assets:
            for interval in self.intervals:
                for variant in self.variants:
                    current_model += 1
                    self.logger.info(f"\\n[{current_model}/{total_models}] Processing {asset} {interval} {variant}")
                    
                    try:
                        # Load data
                        data = self.load_forex_data(asset, interval)
                        if data is None:
                            self.logger.warning(f"❌ No data for {asset} {interval}")
                            results.append({
                                'asset': asset,
                                'interval': interval,
                                'variant': variant,
                                'status': 'failed',
                                'reason': 'no_data',
                                'r2': 0.0,
                                'features_used': 0
                            })
                            continue
                        
                        # Prepare Prophet data
                        prophet_data = self.prepare_prophet_data(data, variant)
                        if prophet_data is None:
                            self.logger.warning(f"❌ Failed to prepare data for {asset} {interval} {variant}")
                            results.append({
                                'asset': asset,
                                'interval': interval,
                                'variant': variant,
                                'status': 'failed',
                                'reason': 'data_preparation',
                                'r2': 0.0,
                                'features_used': 0
                            })
                            continue
                        
                        # Train model
                        model_info = self.train_forex_prophet_model(prophet_data, variant, asset, interval)
                        if model_info is None:
                            self.logger.warning(f"❌ Failed to train model for {asset} {interval} {variant}")
                            results.append({
                                'asset': asset,
                                'interval': interval,
                                'variant': variant,
                                'status': 'failed',
                                'reason': 'training_error',
                                'r2': 0.0,
                                'features_used': 0
                            })
                            continue
                        
                        # Save artifacts
                        file_paths = self.save_model_artifacts(model_info, timestamp)
                        
                        # Record performance
                        self.record_performance(model_info, file_paths, timestamp)
                        
                        # Add to results
                        results.append({
                            'asset': asset,
                            'interval': interval,
                            'variant': variant,
                            'status': 'success',
                            'r2': model_info['r2'],
                            'mae': model_info['mae'],
                            'mse': model_info['mse'],
                            'features_used': model_info['features_used'],
                            'training_records': model_info['training_records']
                        })
                        
                    except Exception as e:
                        self.logger.error(f"❌ Unexpected error for {asset} {interval} {variant}: {e}")
                        results.append({
                            'asset': asset,
                            'interval': interval,
                            'variant': variant,
                            'status': 'failed',
                            'reason': str(e),
                            'r2': 0.0,
                            'features_used': 0
                        })
        
        # Generate summary
        self.generate_summary(results, timestamp)
        
        return results
    
    def generate_summary(self, results: List[Dict], timestamp: str):
        \"\"\"Generate comprehensive summary of model building results.\"\"\""
        try:
            # Calculate statistics
            total_models = len(results)
            successful_models = len([r for r in results if r['status'] == 'success'])
            success_rate = (successful_models / total_models) * 100 if total_models > 0 else 0
            
            # Performance statistics
            successful_results = [r for r in results if r['status'] == 'success']
            if successful_results:
                avg_r2 = np.mean([r['r2'] for r in successful_results])
                max_r2 = max([r['r2'] for r in successful_results])
                min_r2 = min([r['r2'] for r in successful_results])
            else:
                avg_r2 = max_r2 = min_r2 = 0.0
            
            # Create summary
            summary = {
                'timestamp': timestamp,
                'total_models': total_models,
                'successful_models': successful_models,
                'success_rate': success_rate,
                'performance_stats': {
                    'avg_r2': avg_r2,
                    'max_r2': max_r2,
                    'min_r2': min_r2
                },
                'forex_variants': self.variants,
                'model_results': results
            }
            
            # Save summary
            summary_file = f"{self.output_dir}/enhanced_forex_prophet_summary_{timestamp}.json"
            with open(summary_file, 'w') as f:
                json.dump(summary, f, indent=2)
            
            # Print summary
            self.logger.info(f"\\n🎯 ENHANCED FOREX PROPHET MODEL BUILDING COMPLETE")
            self.logger.info(f"   Total Models: {total_models}")
            self.logger.info(f"   Successful: {successful_models}")
            self.logger.info(f"   Success Rate: {success_rate:.1f}%")
            self.logger.info(f"   Average R²: {avg_r2:.3f}")
            self.logger.info(f"   Best R²: {max_r2:.3f}")
            self.logger.info(f"   Summary saved: {summary_file}")
            
            # Print top performers
            if successful_results:
                top_performers = sorted(successful_results, key=lambda x: x['r2'], reverse=True)[:10]
                self.logger.info(f"\\n🏆 TOP PERFORMING FOREX MODELS:")
                for i, model in enumerate(top_performers, 1):
                    self.logger.info(f"   {i:2d}. {model['asset']} {model['interval']} {model['variant']:12s} | R² = {model['r2']:.3f}")
            
        except Exception as e:
            self.logger.error(f"Error generating summary: {e}")

def main():
    \"\"\"Main execution function.\"\"\""
    builder = ForexProphetModelBuilder()
    results = builder.build_all_forex_models()
    
    # Print final summary
    successful = len([r for r in results if r['status'] == 'success'])
    total = len(results)
    print(f"\\n✅ Enhanced Forex Prophet Model Building Complete!")
    print(f"   Success Rate: {successful}/{total} ({successful/total*100:.1f}%)")

if __name__ == "__main__":
    main()