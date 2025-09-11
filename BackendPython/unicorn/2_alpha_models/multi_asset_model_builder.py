#!/usr/bin/env python3
"""
Comprehensive Multi-Asset Model Builder
Creates Prophet, XGBoost, and Ensemble models for all    def prepare_prophet_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for Prophet model"""
        # Prophet requires 'ds' (date) and 'y' (target) columns
        
        # Handle datetime index - remove timezone if present
        date_series = df.index
        if hasattr(date_series, 'tz') and date_series.tz is not None:
            date_series = date_series.tz_localize(None)
        
        prophet_df = pd.DataFrame({
            'ds': date_series,
            'y': df['close'] if 'close' in df.columns else df['Close']
        })
        
        # Ensure ds column is timezone-naive
        if hasattr(prophet_df['ds'].dtype, 'tz') and prophet_df['ds'].dtype.tz is not None:
            prophet_df['ds'] = prophet_df['ds'].dt.tz_localize(None)
        
        # Add regressors from silver layer features
        feature_cols = [col for col in df.columns if col in [
            'volume', 'rsi', 'williams_r', 'cci', 'adx', 'volatility_14',
            'ma_10', 'ma_20', 'ma_50', 'momentum_5', 'momentum_10'
        ]]
        
        for col in feature_cols:
            if col in df.columns and not df[col].isna().all():
                # Clean data and handle infinities
                series = df[col].copy()
                series = series.replace([np.inf, -np.inf], np.nan)
                series = series.fillna(series.mean())
                prophet_df[col] = series
                
        return prophet_dfiple intervals
Integrates silver layer features for enhanced prediction capabilities

Features:
- Multiple intervals: 1m, 1h, 1d
- Multiple methodologies: Prophet, XGBoost, Ensemble
- Silver layer feature integration
- Comprehensive model validation
- Performance tracking and storage
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

# Imports for modeling
try:
    from prophet import Prophet
    from prophet.serialize import model_to_json, model_from_json
except ImportError:
    print("Prophet not installed. Installing...")
    os.system("pip install prophet")
    from prophet import Prophet
    from prophet.serialize import model_to_json, model_from_json

try:
    import xgboost as xgb
except ImportError:
    print("XGBoost not installed. Installing...")
    os.system("pip install xgboost")
    import xgboost as xgb

from sklearn.model_selection import train_test_split, TimeSeriesSplit
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import talib as ta

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class SilverLayerDataLoader:
    """Load and prepare silver layer data for model training"""
    
    def __init__(self, silver_path: str):
        self.silver_path = Path(silver_path)
        self.crypto_path = self.silver_path / "yahoo_finance_assets/processed_data/crypto"
        self.forex_path = self.silver_path / "yahoo_finance_assets/processed_data/forex"
        self.economic_path = self.silver_path / "economic_indicators"
        
    def get_available_assets(self) -> Dict[str, List[str]]:
        """Get list of available assets and intervals"""
        assets = {"crypto": [], "forex": []}
        
        # Crypto assets
        if self.crypto_path.exists():
            for file in self.crypto_path.glob("*_latest.csv"):
                parts = file.stem.split("_")
                if len(parts) >= 3:
                    asset = parts[0]
                    interval = parts[2]
                    if asset not in [a.split("_")[0] for a in assets["crypto"]]:
                        assets["crypto"].append(f"{asset}")
        
        # Forex assets  
        if self.forex_path.exists():
            for file in self.forex_path.glob("*_latest.csv"):
                parts = file.stem.split("_")
                if len(parts) >= 3:
                    asset = parts[0]
                    interval = parts[2]
                    if asset not in [a.split("_")[0] for a in assets["forex"]]:
                        assets["forex"].append(f"{asset}")
                        
        return assets
    
    def load_asset_data(self, asset: str, interval: str, category: str = "crypto") -> Optional[pd.DataFrame]:
        """Load silver layer data for specific asset and interval"""
        try:
            if category == "crypto":
                filepath = self.crypto_path / f"{asset}_silver_{interval}_latest.csv"
            else:
                filepath = self.forex_path / f"{asset}_silver_{interval}_latest.csv"
                
            if not filepath.exists():
                logger.warning(f"Data file not found: {filepath}")
                return None
                
            df = pd.read_csv(filepath, index_col=0, parse_dates=True)
            logger.info(f"Loaded {asset} {interval} data: {df.shape[0]} records × {df.shape[1]} features")
            return df
            
        except Exception as e:
            logger.error(f"Error loading {asset} {interval} data: {e}")
            return None

class ProphetModelBuilder:
    """Prophet model builder with silver layer integration"""
    
    def __init__(self):
        self.model = None
        self.scaler = StandardScaler()
        
    def prepare_prophet_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for Prophet model"""
        # Prophet requires 'ds' (date) and 'y' (target) columns
        prophet_df = pd.DataFrame({
            'ds': df.index,
            'y': df['close'] if 'close' in df.columns else df['Close']
        })
        
        # Add regressors from silver layer features
        feature_cols = [col for col in df.columns if col in [
            'volume', 'rsi', 'williams_r', 'cci', 'adx', 'volatility_14',
            'ma_10', 'ma_20', 'ma_50', 'momentum_5', 'momentum_10'
        ]]
        
        for col in feature_cols:
            if col in df.columns and not df[col].isna().all():
                prophet_df[col] = df[col].fillna(df[col].mean())
                
        return prophet_df
    
    def train_model(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train Prophet model"""
        try:
            prophet_data = self.prepare_prophet_data(data)
            
            # Initialize Prophet model
            self.model = Prophet(
                yearly_seasonality=True,
                weekly_seasonality=True,
                daily_seasonality=True,
                changepoint_prior_scale=0.05,
                seasonality_prior_scale=10.0,
                holidays_prior_scale=10.0,
                seasonality_mode='multiplicative'
            )
            
            # Add regressors
            for col in prophet_data.columns:
                if col not in ['ds', 'y']:
                    self.model.add_regressor(col)
            
            # Fit model
            self.model.fit(prophet_data)
            
            # Generate forecasts for validation
            future = self.model.make_future_dataframe(periods=30, freq='H')
            for col in prophet_data.columns:
                if col not in ['ds', 'y']:
                    future[col] = prophet_data[col].fillna(prophet_data[col].mean()).iloc[-1]
            
            forecast = self.model.predict(future)
            
            # Calculate metrics
            train_pred = forecast['yhat'].iloc[:-30]
            train_actual = prophet_data['y']
            
            metrics = {
                'mae': mean_absolute_error(train_actual, train_pred),
                'mse': mean_squared_error(train_actual, train_pred),
                'rmse': np.sqrt(mean_squared_error(train_actual, train_pred)),
                'r2': r2_score(train_actual, train_pred)
            }
            
            return {
                'model': self.model,
                'forecast': forecast,
                'metrics': metrics,
                'status': 'success'
            }
            
        except Exception as e:
            logger.error(f"Prophet model training failed: {e}")
            return {'status': 'failed', 'error': str(e)}

class XGBoostModelBuilder:
    """XGBoost model builder with silver layer features"""
    
    def __init__(self):
        self.model = None
        self.scaler = StandardScaler()
        self.feature_columns = []
        
    def prepare_features(self, df: pd.DataFrame) -> Tuple[np.ndarray, np.ndarray]:
        """Prepare features and target from silver layer data"""
        # Select relevant features
        feature_cols = [col for col in df.columns if col in [
            'open', 'high', 'low', 'volume', 'rsi', 'williams_r', 'cci', 'adx',
            'volatility_14', 'ma_10', 'ma_20', 'ma_50', 'momentum_5', 'momentum_10',
            'momentum_20', 'momentum_50', 'price_change', 'volume_change',
            'hour', 'day_of_week', 'month', 'trend_strength'
        ]]
        
        # Filter available columns
        available_cols = [col for col in feature_cols if col in df.columns]
        self.feature_columns = available_cols
        
        # Prepare features
        X = df[available_cols].fillna(df[available_cols].mean())
        
        # Target variable (next period close price)
        y = df['close'].shift(-1).fillna(method='ffill')
        
        # Remove last row (no target)
        X = X.iloc[:-1]
        y = y.iloc[:-1]
        
        return X.values, y.values
    
    def train_model(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train XGBoost model"""
        try:
            X, y = self.prepare_features(data)
            
            # Split data
            X_train, X_test, y_train, y_test = train_test_split(
                X, y, test_size=0.2, random_state=42, shuffle=False
            )
            
            # Scale features
            X_train_scaled = self.scaler.fit_transform(X_train)
            X_test_scaled = self.scaler.transform(X_test)
            
            # Train XGBoost model
            self.model = xgb.XGBRegressor(
                n_estimators=100,
                max_depth=6,
                learning_rate=0.1,
                random_state=42,
                n_jobs=-1
            )
            
            self.model.fit(X_train_scaled, y_train)
            
            # Predictions
            y_pred_train = self.model.predict(X_train_scaled)
            y_pred_test = self.model.predict(X_test_scaled)
            
            # Calculate metrics
            train_metrics = {
                'mae': mean_absolute_error(y_train, y_pred_train),
                'mse': mean_squared_error(y_train, y_pred_train),
                'rmse': np.sqrt(mean_squared_error(y_train, y_pred_train)),
                'r2': r2_score(y_train, y_pred_train)
            }
            
            test_metrics = {
                'mae': mean_absolute_error(y_test, y_pred_test),
                'mse': mean_squared_error(y_test, y_pred_test),
                'rmse': np.sqrt(mean_squared_error(y_test, y_pred_test)),
                'r2': r2_score(y_test, y_pred_test)
            }
            
            return {
                'model': self.model,
                'scaler': self.scaler,
                'feature_columns': self.feature_columns,
                'train_metrics': train_metrics,
                'test_metrics': test_metrics,
                'feature_importance': dict(zip(self.feature_columns, self.model.feature_importances_)),
                'status': 'success'
            }
            
        except Exception as e:
            logger.error(f"XGBoost model training failed: {e}")
            return {'status': 'failed', 'error': str(e)}

class EnsembleModelBuilder:
    """Ensemble model combining Prophet and XGBoost"""
    
    def __init__(self):
        self.prophet_builder = ProphetModelBuilder()
        self.xgboost_builder = XGBoostModelBuilder()
        self.weights = {'prophet': 0.5, 'xgboost': 0.5}
        
    def train_model(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train ensemble model"""
        try:
            # Train Prophet model
            prophet_result = self.prophet_builder.train_model(data)
            if prophet_result['status'] != 'success':
                return prophet_result
                
            # Train XGBoost model
            xgboost_result = self.xgboost_builder.train_model(data)
            if xgboost_result['status'] != 'success':
                return xgboost_result
            
            # Combine predictions (simple weighted average)
            prophet_pred = prophet_result['forecast']['yhat'].iloc[:-30]
            xgboost_X, xgboost_y = self.xgboost_builder.prepare_features(data)
            xgboost_pred = self.xgboost_builder.model.predict(
                self.xgboost_builder.scaler.transform(xgboost_X)
            )
            
            # Align predictions
            min_len = min(len(prophet_pred), len(xgboost_pred))
            prophet_pred = prophet_pred[-min_len:]
            xgboost_pred = xgboost_pred[-min_len:]
            actual = data['close'].iloc[-min_len-1:-1]  # Adjust for target shift
            
            # Ensemble prediction
            ensemble_pred = (self.weights['prophet'] * prophet_pred + 
                           self.weights['xgboost'] * xgboost_pred)
            
            # Calculate ensemble metrics
            ensemble_metrics = {
                'mae': mean_absolute_error(actual, ensemble_pred),
                'mse': mean_squared_error(actual, ensemble_pred),
                'rmse': np.sqrt(mean_squared_error(actual, ensemble_pred)),
                'r2': r2_score(actual, ensemble_pred)
            }
            
            return {
                'prophet_model': prophet_result,
                'xgboost_model': xgboost_result,
                'ensemble_metrics': ensemble_metrics,
                'weights': self.weights,
                'status': 'success'
            }
            
        except Exception as e:
            logger.error(f"Ensemble model training failed: {e}")
            return {'status': 'failed', 'error': str(e)}

class MultiAssetModelBuilder:
    """Main class for building models across all assets and intervals"""
    
    def __init__(self, silver_path: str, output_path: str):
        self.silver_path = Path(silver_path)
        self.output_path = Path(output_path)
        self.data_loader = SilverLayerDataLoader(silver_path)
        
        # Create output directories
        self.output_path.mkdir(parents=True, exist_ok=True)
        
        # Model storage
        self.models_db_path = self.output_path / "multi_asset_models.db"
        self.init_database()
        
    def init_database(self):
        """Initialize SQLite database for model storage"""
        conn = sqlite3.connect(self.models_db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS models (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                asset TEXT NOT NULL,
                category TEXT NOT NULL,
                interval TEXT NOT NULL,
                methodology TEXT NOT NULL,
                model_data BLOB,
                metadata TEXT,
                performance_metrics TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS performance_summary (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                asset TEXT NOT NULL,
                category TEXT NOT NULL,
                interval TEXT NOT NULL,
                methodology TEXT NOT NULL,
                mae REAL,
                mse REAL,
                rmse REAL,
                r2 REAL,
                feature_count INTEGER,
                training_records INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        conn.commit()
        conn.close()
        
    def save_model(self, asset: str, category: str, interval: str, methodology: str, 
                   model_result: Dict[str, Any]):
        """Save model to database"""
        try:
            conn = sqlite3.connect(self.models_db_path)
            cursor = conn.cursor()
            
            # Serialize model
            if methodology == 'prophet':
                model_data = pickle.dumps(model_result.get('model'))
                metrics = model_result.get('metrics', {})
            elif methodology == 'xgboost':
                model_data = pickle.dumps({
                    'model': model_result.get('model'),
                    'scaler': model_result.get('scaler'),
                    'feature_columns': model_result.get('feature_columns')
                })
                metrics = model_result.get('test_metrics', {})
            elif methodology == 'ensemble':
                model_data = pickle.dumps(model_result)
                metrics = model_result.get('ensemble_metrics', {})
                
            # Insert model
            cursor.execute('''
                INSERT OR REPLACE INTO models 
                (asset, category, interval, methodology, model_data, metadata, performance_metrics)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', (
                asset, category, interval, methodology,
                model_data,
                json.dumps(model_result.get('metadata', {})),
                json.dumps(metrics)
            ))
            
            # Insert performance summary
            cursor.execute('''
                INSERT OR REPLACE INTO performance_summary
                (asset, category, interval, methodology, mae, mse, rmse, r2, feature_count, training_records)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                asset, category, interval, methodology,
                metrics.get('mae', 0),
                metrics.get('mse', 0), 
                metrics.get('rmse', 0),
                metrics.get('r2', 0),
                len(model_result.get('feature_columns', [])),
                model_result.get('training_records', 0)
            ))
            
            conn.commit()
            conn.close()
            
            logger.info(f"Saved {methodology} model for {asset} {interval}")
            
        except Exception as e:
            logger.error(f"Error saving model: {e}")
    
    def build_models_for_asset(self, asset: str, category: str, intervals: List[str] = ['1d', '1h']):
        """Build all models for a specific asset"""
        logger.info(f"Building models for {asset} ({category})")
        
        results = {}
        
        for interval in intervals:
            logger.info(f"Processing {asset} {interval}")
            
            # Load data
            data = self.data_loader.load_asset_data(asset, interval, category)
            if data is None or len(data) < 100:
                logger.warning(f"Insufficient data for {asset} {interval}")
                continue
                
            results[interval] = {}
            
            # Build Prophet model
            logger.info(f"Training Prophet model for {asset} {interval}")
            prophet_builder = ProphetModelBuilder()
            prophet_result = prophet_builder.train_model(data)
            if prophet_result['status'] == 'success':
                prophet_result['training_records'] = len(data)
                self.save_model(asset, category, interval, 'prophet', prophet_result)
                results[interval]['prophet'] = prophet_result
            
            # Build XGBoost model
            logger.info(f"Training XGBoost model for {asset} {interval}")
            xgboost_builder = XGBoostModelBuilder()
            xgboost_result = xgboost_builder.train_model(data)
            if xgboost_result['status'] == 'success':
                xgboost_result['training_records'] = len(data)
                self.save_model(asset, category, interval, 'xgboost', xgboost_result)
                results[interval]['xgboost'] = xgboost_result
            
            # Build Ensemble model
            logger.info(f"Training Ensemble model for {asset} {interval}")
            ensemble_builder = EnsembleModelBuilder()
            ensemble_result = ensemble_builder.train_model(data)
            if ensemble_result['status'] == 'success':
                ensemble_result['training_records'] = len(data)
                self.save_model(asset, category, interval, 'ensemble', ensemble_result)
                results[interval]['ensemble'] = ensemble_result
                
        return results
    
    def build_all_models(self):
        """Build models for all available assets"""
        logger.info("Starting comprehensive model building process")
        
        # Get available assets
        assets = self.data_loader.get_available_assets()
        
        total_models = 0
        successful_models = 0
        
        # Process crypto assets
        for asset in assets['crypto']:
            try:
                results = self.build_models_for_asset(asset, 'crypto')
                for interval in results:
                    for methodology in results[interval]:
                        total_models += 1
                        if results[interval][methodology]['status'] == 'success':
                            successful_models += 1
            except Exception as e:
                logger.error(f"Error processing crypto asset {asset}: {e}")
        
        # Process forex assets
        for asset in assets['forex']:
            try:
                results = self.build_models_for_asset(asset, 'forex')
                for interval in results:
                    for methodology in results[interval]:
                        total_models += 1
                        if results[interval][methodology]['status'] == 'success':
                            successful_models += 1
            except Exception as e:
                logger.error(f"Error processing forex asset {asset}: {e}")
        
        # Generate summary report
        self.generate_summary_report(total_models, successful_models)
        
        logger.info(f"Model building complete: {successful_models}/{total_models} models successful")
    
    def generate_summary_report(self, total_models: int, successful_models: int):
        """Generate comprehensive summary report"""
        try:
            conn = sqlite3.connect(self.models_db_path)
            
            # Get performance summary
            performance_df = pd.read_sql_query(
                "SELECT * FROM performance_summary ORDER BY asset, interval, methodology",
                conn
            )
            
            # Create summary report
            report = {
                'timestamp': datetime.now().isoformat(),
                'total_models_attempted': total_models,
                'successful_models': successful_models,
                'success_rate': successful_models / total_models if total_models > 0 else 0,
                'assets_processed': performance_df['asset'].nunique(),
                'methodologies': ['prophet', 'xgboost', 'ensemble'],
                'intervals': ['1d', '1h'],
                'performance_summary': performance_df.to_dict('records'),
                'best_performers': {
                    'by_r2': performance_df.nlargest(5, 'r2')[['asset', 'interval', 'methodology', 'r2']].to_dict('records'),
                    'by_mae': performance_df.nsmallest(5, 'mae')[['asset', 'interval', 'methodology', 'mae']].to_dict('records')
                }
            }
            
            # Save report
            report_path = self.output_path / f"model_building_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
            with open(report_path, 'w') as f:
                json.dump(report, f, indent=2)
                
            logger.info(f"Summary report saved to: {report_path}")
            
            conn.close()
            
        except Exception as e:
            logger.error(f"Error generating summary report: {e}")

def main():
    """Main execution function"""
    # Configuration
    silver_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver"
    output_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/multi_asset_models"
    
    # Build models
    builder = MultiAssetModelBuilder(silver_path, output_path)
    builder.build_all_models()

if __name__ == "__main__":
    main()
