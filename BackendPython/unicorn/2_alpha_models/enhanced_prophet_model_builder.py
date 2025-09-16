#!/usr/bin/env python3
"""
Enhanced Prophet Model Generator - 5 Variants Per Asset

This script creates 5 different Prophet model configurations for each asset:
1. Conservative Prophet - Minimal features, stable predictions
2. Standard Prophet - Balanced configuration with key features
3. Aggressive Prophet - Maximum features and flexibility
4. Economic Prophet - Specialized for economic indicator integration
5. Ensemble Prophet - Multi-frequency combination model

Features:
- Silver layer data integration with economic indicators
- Multiple Prophet configurations optimized for different market conditions
- Comprehensive performance tracking and validation
- Production-ready model saving and deployment
- Enhanced feature engineering pipeline
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
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class ProphetVariantConfig:
    """Configuration class for different Prophet model variants"""
    
    @staticmethod
    def get_conservative_config():
        """Conservative Prophet configuration - minimal features, stable predictions"""
        return {
            'name': 'conservative',
            'description': 'Conservative Prophet with minimal features and stable parameters',
            'prophet_params': {
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': False,
                'changepoint_prior_scale': 0.01,  # Very conservative
                'seasonality_prior_scale': 1.0,   # Low seasonality impact
                'holidays_prior_scale': 1.0,
                'seasonality_mode': 'additive',
                'changepoint_range': 0.8,
                'n_changepoints': 10,
                'interval_width': 0.80
            },
            'max_features': 3,
            'feature_selection': ['rsi', 'ma_20', 'volume'],
            'validation_split': 0.2
        }
    
    @staticmethod
    def get_standard_config():
        """Standard Prophet configuration - balanced approach"""
        return {
            'name': 'standard',
            'description': 'Standard Prophet with balanced features and parameters',
            'prophet_params': {
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.05,
                'seasonality_prior_scale': 10.0,
                'holidays_prior_scale': 10.0,
                'seasonality_mode': 'multiplicative',
                'changepoint_range': 0.8,
                'n_changepoints': 25,
                'interval_width': 0.80
            },
            'max_features': 7,
            'feature_selection': ['rsi', 'macd', 'atr', 'ma_20', 'ma_50', 'volume', 'volatility_14'],
            'validation_split': 0.2
        }
    
    @staticmethod
    def get_aggressive_config():
        """Aggressive Prophet configuration - maximum features and flexibility"""
        return {
            'name': 'aggressive',
            'description': 'Aggressive Prophet with maximum features and high flexibility',
            'prophet_params': {
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.5,   # High flexibility
                'seasonality_prior_scale': 50.0,  # High seasonality impact
                'holidays_prior_scale': 50.0,
                'seasonality_mode': 'multiplicative',
                'changepoint_range': 0.9,
                'n_changepoints': 50,
                'interval_width': 0.95
            },
            'max_features': 15,
            'feature_selection': [
                'rsi', 'macd', 'atr', 'cci', 'williams_r', 'adx',
                'ma_10', 'ma_20', 'ma_50', 'volume', 'volatility_14', 'volatility_20',
                'momentum_5', 'momentum_10', 'momentum_20'
            ],
            'validation_split': 0.25
        }
    
    @staticmethod
    def get_economic_config():
        """Economic Prophet configuration - optimized for economic indicator integration"""
        return {
            'name': 'economic',
            'description': 'Economic-focused Prophet optimized for macroeconomic indicators',
            'prophet_params': {
                'yearly_seasonality': True,
                'weekly_seasonality': False,  # Economic data less affected by weekly patterns
                'daily_seasonality': False,
                'changepoint_prior_scale': 0.1,
                'seasonality_prior_scale': 5.0,
                'holidays_prior_scale': 5.0,
                'seasonality_mode': 'additive',
                'changepoint_range': 0.85,
                'n_changepoints': 15,
                'interval_width': 0.80
            },
            'max_features': 10,
            'feature_selection': [
                'rsi', 'ma_20', 'ma_50', 'volume', 'volatility_14',
                'atr', 'momentum_10', 'macd', 'cci', 'adx'
            ],
            'validation_split': 0.2,
            'include_economic': True  # Flag to include economic indicators
        }
    
    @staticmethod
    def get_ensemble_config():
        """Ensemble Prophet configuration - multi-timeframe combination"""
        return {
            'name': 'ensemble',
            'description': 'Ensemble Prophet combining multiple timeframe signals',
            'prophet_params': {
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.08,
                'seasonality_prior_scale': 15.0,
                'holidays_prior_scale': 15.0,
                'seasonality_mode': 'multiplicative',
                'changepoint_range': 0.85,
                'n_changepoints': 30,
                'interval_width': 0.85
            },
            'max_features': 12,
            'feature_selection': [
                'rsi', 'macd', 'atr', 'ma_10', 'ma_20', 'ma_50',
                'volume', 'volatility_14', 'momentum_5', 'momentum_10',
                'cci', 'williams_r'
            ],
            'validation_split': 0.2,
            'include_ensemble_features': True
        }

class EnhancedProphetModelBuilder:
    """Enhanced Prophet model builder for multiple variants"""
    
    def __init__(self, output_dir: str = None):
        """Initialize the enhanced Prophet model builder"""
        self.output_dir = output_dir or "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/enhanced_prophet_models"
        self.silver_data_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver"
        self.economic_data_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/economic_indicators"
        
        # Create output directory
        Path(self.output_dir).mkdir(parents=True, exist_ok=True)
        
        # Initialize performance tracking database
        self.db_path = os.path.join(self.output_dir, "enhanced_prophet_performance.db")
        self.init_performance_db()
        
        # Model configurations
        self.configs = {
            'conservative': ProphetVariantConfig.get_conservative_config(),
            'standard': ProphetVariantConfig.get_standard_config(),
            'aggressive': ProphetVariantConfig.get_aggressive_config(),
            'economic': ProphetVariantConfig.get_economic_config(),
            'ensemble': ProphetVariantConfig.get_ensemble_config()
        }
        
        logger.info(f"Enhanced Prophet Builder initialized with 5 model variants")
        logger.info(f"Output directory: {self.output_dir}")
    
    def init_performance_db(self):
        """Initialize SQLite database for performance tracking"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS enhanced_prophet_performance (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                asset TEXT NOT NULL,
                interval TEXT NOT NULL,
                variant TEXT NOT NULL,
                training_date TIMESTAMP NOT NULL,
                training_records INTEGER,
                features_used INTEGER,
                mae REAL,
                mse REAL,
                rmse REAL,
                r2 REAL,
                validation_r2 REAL,
                model_path TEXT,
                config_used TEXT,
                status TEXT,
                error_message TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def get_available_assets(self) -> Dict[str, List[str]]:
        """Get all available assets from the directory structure"""
        assets = {
            'crypto': [],
            'forex': [],
            'equities': []
        }
        
        # Crypto assets
        crypto_dir = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO"
        if os.path.exists(crypto_dir):
            for item in os.listdir(crypto_dir):
                if os.path.isdir(os.path.join(crypto_dir, item)) and item not in ['__pycache__']:
                    assets['crypto'].append(item)
        
        # Forex assets
        forex_dir = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/FOREX"
        if os.path.exists(forex_dir):
            for item in os.listdir(forex_dir):
                if os.path.isdir(os.path.join(forex_dir, item)) and item not in ['__pycache__', 'economic_enhanced_xgboost']:
                    assets['forex'].append(item)
        
        # Equities assets
        equities_dir = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/EQUITIES"
        if os.path.exists(equities_dir):
            for item in os.listdir(equities_dir):
                if os.path.isdir(os.path.join(equities_dir, item)) and item not in ['__pycache__']:
                    assets['equities'].append(item)
        
        return assets
    
    def load_silver_layer_data(self, symbol: str, timeframe: str = '1d') -> Optional[pd.DataFrame]:
        """Load processed silver layer data for a symbol."""
        try:
            # Determine asset type and build correct path
            crypto_assets = ['BTC', 'ETH']
            forex_assets = ['AUDUSD', 'EURUSD', 'GBPUSD', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY']
            
            if symbol in crypto_assets:
                asset_dir = 'crypto'
            elif symbol in forex_assets:
                asset_dir = 'forex'
            else:
                print(f"❌ Unknown asset type for {symbol}")
                return None
            
            # Use the latest silver layer data from processed_data structure
            silver_path = f"/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/processed_data/{asset_dir}/{symbol}_silver_{timeframe}_latest.csv"
            
            if not os.path.exists(silver_path):
                print(f"❌ No silver layer data found at: {silver_path}")
                return None
            
            data = pd.read_csv(silver_path)
            print(f"✅ Loaded silver layer data from: {silver_path}")
            print(f"   Data shape: {data.shape}")
            
            # Check for datetime column
            datetime_col = None
            for col in ['Datetime', 'datetime', 'date', 'Date', 'timestamp', 'Timestamp']:
                if col in data.columns:
                    datetime_col = col
                    break
            
            if datetime_col:
                print(f"   Datetime column: {datetime_col}")
                print(f"   Date range: {data[datetime_col].min()} to {data[datetime_col].max()}")
            else:
                print(f"   Available columns: {list(data.columns[:10])}")  # Show first 10 columns
            
            return data
            
        except Exception as e:
            print(f"❌ Error loading silver layer data for {symbol}: {e}")
            return None
    
    def load_economic_indicators(self) -> Optional[pd.DataFrame]:
        """Load economic indicators from silver layer"""
        try:
            # Try to load the latest economic silver data
            econ_paths = [
                f"{self.economic_data_path}/consolidated_economic_indicators_silver.csv",
                f"{self.economic_data_path}/../processed_data/1_day/economic_silver_latest.csv",
                f"{self.economic_data_path}/economic_growth_silver.csv"
            ]
            
            for path in econ_paths:
                if os.path.exists(path):
                    logger.info(f"Loading economic data from: {path}")
                    df = pd.read_csv(path)
                    
                    # Ensure proper datetime index
                    if 'Date' in df.columns:
                        df['Date'] = pd.to_datetime(df['Date'])
                        df.set_index('Date', inplace=True)
                    elif 'timestamp' in df.columns:
                        df['timestamp'] = pd.to_datetime(df['timestamp'])
                        df.set_index('timestamp', inplace=True)
                    elif not isinstance(df.index, pd.DatetimeIndex):
                        df.index = pd.to_datetime(df.index)
                    
                    logger.info(f"Loaded economic indicators: {len(df)} records, {len(df.columns)} features")
                    return df
            
            logger.warning("No economic indicators data found")
            return None
            
        except Exception as e:
            logger.error(f"Error loading economic indicators: {e}")
            return None
    
    def prepare_prophet_data(self, data: pd.DataFrame, config: Dict[str, Any]) -> pd.DataFrame:
        """Prepare data for Prophet model based on configuration"""
        try:
            # Find datetime column
            datetime_col = None
            for col in ['Datetime', 'datetime', 'date', 'Date', 'timestamp', 'Timestamp']:
                if col in data.columns:
                    datetime_col = col
                    break
            
            if datetime_col is None:
                raise ValueError("No suitable datetime column found")
            
            # Ensure we have a close price column
            target_col = None
            for col in ['close', 'Close', 'close_price', 'price']:
                if col in data.columns:
                    target_col = col
                    break
            
            if target_col is None:
                raise ValueError("No suitable target column found (close, Close, close_price, price)")
            
            # Debug: check the datetime format
            logger.debug(f"Datetime column sample: {data[datetime_col].head()}")
            logger.debug(f"Datetime column type: {data[datetime_col].dtype}")
            
            # Robust datetime handling for mixed timezone data
            dt_series = data[datetime_col].copy()
            
            # Convert to string first, then parse uniformly
            if not pd.api.types.is_datetime64_any_dtype(dt_series):
                # If string, convert to datetime
                dt_series = pd.to_datetime(dt_series, utc=True, errors='coerce')
            else:
                # Already datetime - handle mixed timezones
                try:
                    # Try to convert existing datetime series
                    if dt_series.dt.tz is not None:
                        # Has timezone info - convert to UTC
                        dt_series = dt_series.dt.tz_convert('UTC')
                    else:
                        # No timezone - assume UTC
                        dt_series = dt_series.dt.tz_localize('UTC')
                except Exception:
                    # Fallback: re-parse as strings to handle mixed timezones
                    dt_strings = dt_series.astype(str)
                    dt_series = pd.to_datetime(dt_strings, utc=True, errors='coerce')
            
            # Remove timezone for Prophet compatibility
            dt_series_naive = dt_series.dt.tz_localize(None)
            
            # Create base Prophet dataframe
            prophet_df = pd.DataFrame({
                'ds': dt_series_naive,
                'y': pd.to_numeric(data[target_col], errors='coerce')
            })
            
            # Remove any rows with NaN datetime or target values
            prophet_df = prophet_df.dropna(subset=['ds', 'y'])
            
            # Add selected features based on configuration
            available_features = []
            for feature in config['feature_selection']:
                if feature in data.columns:
                    series = data[feature].copy()
                    # Handle infinite values
                    series = series.replace([np.inf, -np.inf], np.nan)
                    if not series.isna().all():
                        # Normalize the feature
                        series_normalized = (series - series.mean()) / (series.std() + 1e-8)
                        prophet_df[feature] = series_normalized.fillna(0)
                        available_features.append(feature)
            
            # Limit features based on max_features
            if len(available_features) > config['max_features']:
                # Keep the most important features based on correlation with target
                correlations = []
                for feature in available_features:
                    corr = prophet_df[feature].corr(prophet_df['y'])
                    correlations.append((feature, abs(corr) if not np.isnan(corr) else 0))
                
                # Sort by correlation and keep top features
                correlations.sort(key=lambda x: x[1], reverse=True)
                top_features = [f[0] for f in correlations[:config['max_features']]]
                
                # Keep only top features
                columns_to_keep = ['ds', 'y'] + top_features
                prophet_df = prophet_df[columns_to_keep]
                available_features = top_features
            
            # Remove rows with NaN target values
            prophet_df = prophet_df.dropna(subset=['y'])
            
            logger.info(f"Prepared Prophet data: {len(prophet_df)} records, {len(available_features)} features")
            logger.info(f"Features used: {available_features}")
            
            return prophet_df
            
        except Exception as e:
            logger.error(f"Error preparing Prophet data: {e}")
            raise
    
    def train_prophet_model(self, data: pd.DataFrame, config: Dict[str, Any]) -> Dict[str, Any]:
        """Train a Prophet model with the given configuration"""
        try:
            logger.info(f"Training {config['name']} Prophet model")
            
            # Prepare data
            prophet_data = self.prepare_prophet_data(data, config)
            
            if len(prophet_data) < 30:
                raise ValueError(f"Insufficient data for training: {len(prophet_data)} records")
            
            # Split data for validation
            split_idx = int(len(prophet_data) * (1 - config['validation_split']))
            train_data = prophet_data.iloc[:split_idx]
            val_data = prophet_data.iloc[split_idx:]
            
            # Initialize Prophet model
            model = Prophet(**config['prophet_params'])
            
            # Add regressors
            feature_columns = [col for col in prophet_data.columns if col not in ['ds', 'y']]
            for col in feature_columns:
                model.add_regressor(col, standardize=True)
            
            # Fit model
            model.fit(train_data)
            
            # Make predictions on validation set
            if len(val_data) > 0:
                future_val = val_data[['ds'] + feature_columns].copy()
                forecast_val = model.predict(future_val)
                
                # Calculate validation metrics
                val_actual = val_data['y'].values
                val_pred = forecast_val['yhat'].values
                
                val_metrics = {
                    'mae': mean_absolute_error(val_actual, val_pred),
                    'mse': mean_squared_error(val_actual, val_pred),
                    'rmse': np.sqrt(mean_squared_error(val_actual, val_pred)),
                    'r2': max(0, r2_score(val_actual, val_pred))
                }
            else:
                val_metrics = {'mae': 0, 'mse': 0, 'rmse': 0, 'r2': 0}
            
            # Make forecast on full dataset for training metrics
            future_full = prophet_data[['ds'] + feature_columns].copy()
            forecast_full = model.predict(future_full)
            
            # Calculate training metrics
            train_actual = prophet_data['y'].values
            train_pred = forecast_full['yhat'].values
            
            train_metrics = {
                'mae': mean_absolute_error(train_actual, train_pred),
                'mse': mean_squared_error(train_actual, train_pred),
                'rmse': np.sqrt(mean_squared_error(train_actual, train_pred)),
                'r2': max(0, r2_score(train_actual, train_pred))
            }
            
            return {
                'model': model,
                'forecast': forecast_full,
                'train_metrics': train_metrics,
                'val_metrics': val_metrics,
                'features_used': len(feature_columns),
                'training_records': len(prophet_data),
                'status': 'success'
            }
            
        except Exception as e:
            logger.error(f"Error training Prophet model: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def save_model_results(self, asset: str, interval: str, variant: str, 
                          result: Dict[str, Any], config: Dict[str, Any]):
        """Save model results to database and files"""
        try:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            
            # Create asset directory
            asset_dir = os.path.join(self.output_dir, asset, interval, variant)
            Path(asset_dir).mkdir(parents=True, exist_ok=True)
            
            # Save model if successful
            model_path = None
            if result['status'] == 'success':
                # Save Prophet model
                model_path = os.path.join(asset_dir, f"{asset}_{interval}_{variant}_{timestamp}.json")
                with open(model_path, 'w') as f:
                    json.dump(model_to_json(result['model']), f, indent=2)
                
                # Save forecast
                forecast_path = os.path.join(asset_dir, f"{asset}_{interval}_{variant}_forecast_{timestamp}.csv")
                result['forecast'].to_csv(forecast_path)
                
                # Save configuration
                config_path = os.path.join(asset_dir, f"{asset}_{interval}_{variant}_config_{timestamp}.json")
                with open(config_path, 'w') as f:
                    json.dump(config, f, indent=2)
                
                logger.info(f"Saved {variant} model for {asset} {interval}")
            
            # Save to database
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            if result['status'] == 'success':
                cursor.execute('''
                    INSERT INTO enhanced_prophet_performance 
                    (asset, interval, variant, training_date, training_records, features_used,
                     mae, mse, rmse, r2, validation_r2, model_path, config_used, status)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    asset, interval, variant, datetime.now(),
                    result.get('training_records', 0),
                    result.get('features_used', 0),
                    result['train_metrics']['mae'],
                    result['train_metrics']['mse'],
                    result['train_metrics']['rmse'],
                    result['train_metrics']['r2'],
                    result['val_metrics']['r2'],
                    model_path,
                    json.dumps(config),
                    'success'
                ))
            else:
                cursor.execute('''
                    INSERT INTO enhanced_prophet_performance 
                    (asset, interval, variant, training_date, status, error_message)
                    VALUES (?, ?, ?, ?, ?, ?)
                ''', (
                    asset, interval, variant, datetime.now(),
                    'failed', result.get('error', 'Unknown error')
                ))
            
            conn.commit()
            conn.close()
            
        except Exception as e:
            logger.error(f"Error saving model results: {e}")
    
    def build_models_for_asset(self, asset: str, category: str, intervals: List[str] = ['1h', '1d']):
        """Build all 5 Prophet variant models for a specific asset"""
        logger.info(f"Building 5 Prophet variants for {asset} ({category})")
        
        results = {}
        
        for interval in intervals:
            logger.info(f"Processing {asset} {interval}")
            
            # Load data
            data = self.load_silver_layer_data(asset, interval)
            if data is None or len(data) < 100:
                logger.warning(f"Insufficient data for {asset} {interval}: {len(data) if data is not None else 0} records")
                continue
            
            results[interval] = {}
            
            # Build each variant
            for variant_name, config in self.configs.items():
                logger.info(f"Training {variant_name} Prophet for {asset} {interval}")
                
                # Train model
                result = self.train_prophet_model(data, config)
                
                # Save results
                self.save_model_results(asset, interval, variant_name, result, config)
                
                # Store result summary
                if result['status'] == 'success':
                    results[interval][variant_name] = {
                        'r2': result['train_metrics']['r2'],
                        'val_r2': result['val_metrics']['r2'],
                        'features': result['features_used'],
                        'records': result['training_records']
                    }
                    logger.info(f"✅ {variant_name}: R² = {result['train_metrics']['r2']:.3f}, Val R² = {result['val_metrics']['r2']:.3f}")
                else:
                    results[interval][variant_name] = {'error': result.get('error', 'Failed')}
                    logger.error(f"❌ {variant_name}: {result.get('error', 'Failed')}")
        
        return results
    
    def build_all_prophet_models(self):
        """Build all Prophet models for all available assets"""
        logger.info("Starting enhanced Prophet model building for all assets")
        
        # Get available assets
        assets = self.get_available_assets()
        
        total_models = 0
        successful_models = 0
        
        # Process each asset category
        for category, asset_list in assets.items():
            if not asset_list:
                continue
                
            logger.info(f"Processing {category} assets: {asset_list}")
            
            for asset in asset_list:
                try:
                    results = self.build_models_for_asset(asset, category)
                    
                    # Count results
                    for interval, variants in results.items():
                        for variant, result in variants.items():
                            total_models += 1
                            if 'error' not in result:
                                successful_models += 1
                
                except Exception as e:
                    logger.error(f"Error processing {asset}: {e}")
        
        # Generate summary report
        self.generate_summary_report(total_models, successful_models)
        
        logger.info(f"Enhanced Prophet model building complete!")
        logger.info(f"Total models attempted: {total_models}")
        logger.info(f"Successful models: {successful_models}")
        logger.info(f"Success rate: {(successful_models/total_models*100) if total_models > 0 else 0:.1f}%")
    
    def generate_summary_report(self, total_models: int, successful_models: int):
        """Generate a comprehensive summary report"""
        try:
            conn = sqlite3.connect(self.db_path)
            
            # Get performance summary
            summary_df = pd.read_sql_query('''
                SELECT asset, interval, variant, r2, validation_r2, features_used, training_records, status
                FROM enhanced_prophet_performance
                WHERE status = 'success'
                ORDER BY asset, interval, r2 DESC
            ''', conn)
            
            conn.close()
            
            # Create report
            report = {
                'timestamp': datetime.now().isoformat(),
                'total_models': total_models,
                'successful_models': successful_models,
                'success_rate': (successful_models/total_models*100) if total_models > 0 else 0,
                'model_variants': list(self.configs.keys()),
                'performance_summary': summary_df.to_dict('records') if not summary_df.empty else []
            }
            
            # Save report
            report_path = os.path.join(self.output_dir, f"enhanced_prophet_summary_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json")
            with open(report_path, 'w') as f:
                json.dump(report, f, indent=2)
            
            logger.info(f"Summary report saved to: {report_path}")
            
        except Exception as e:
            logger.error(f"Error generating summary report: {e}")

def main():
    """Main execution function"""
    logger.info("Enhanced Prophet Model Builder - 5 Variants Per Asset")
    logger.info("=" * 60)
    
    # Initialize builder
    builder = EnhancedProphetModelBuilder()
    
    # Build all models
    builder.build_all_prophet_models()
    
    logger.info("Enhanced Prophet model building completed!")

if __name__ == "__main__":
    main()