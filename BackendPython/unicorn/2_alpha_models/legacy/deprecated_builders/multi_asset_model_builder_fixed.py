#!/usr/bin/env python3
"""
Multi-Asset Model Builder with Fixed Timezone and Infinity Handling
Enhanced Prophet, XGBoost, and Ensemble models for all assets and intervals
"""

import pandas as pd
import numpy as np
from pathlib import Path
import logging
import json
from datetime import datetime
from typing import Dict, List, Any, Optional
from dataclasses import dataclass

# Model building imports
from prophet import Prophet
import xgboost as xgb
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split, TimeSeriesSplit
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import sqlite3
import joblib

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class ModelConfig:
    """Configuration for model building"""
    test_size: float = 0.2
    random_state: int = 42
    prophet_params: Dict = None
    xgboost_params: Dict = None
    
    def __post_init__(self):
        if self.prophet_params is None:
            self.prophet_params = {
                'yearly_seasonality': True,
                'weekly_seasonality': True,
                'daily_seasonality': True,
                'changepoint_prior_scale': 0.05,
                'seasonality_prior_scale': 10.0,
                'holidays_prior_scale': 10.0,
                'seasonality_mode': 'multiplicative'
            }
        
        if self.xgboost_params is None:
            self.xgboost_params = {
                'objective': 'reg:squarederror',
                'n_estimators': 100,
                'max_depth': 6,
                'learning_rate': 0.1,
                'subsample': 0.8,
                'colsample_bytree': 0.8,
                'random_state': 42
            }

class SilverLayerDataLoader:
    """Load and prepare silver layer data for model training"""
    
    def __init__(self):
        self.base_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/processed_data")
        self.crypto_path = self.base_path / "crypto"
        self.forex_path = self.base_path / "forex"
        
    def get_available_assets(self) -> Dict[str, List[str]]:
        """Get all available assets and intervals"""
        assets = {"crypto": [], "forex": []}
        
        # Check crypto assets
        if self.crypto_path.exists():
            for file in self.crypto_path.glob("*_silver_*_latest.csv"):
                parts = file.stem.split("_")
                if len(parts) >= 3:
                    asset = parts[0]
                    interval = parts[2]
                    if asset not in [a.split("_")[0] for a in assets["crypto"]]:
                        assets["crypto"].append(f"{asset}")
        
        # Check forex assets
        if self.forex_path.exists():
            for file in self.forex_path.glob("*_silver_*_latest.csv"):
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
    """Prophet model builder with fixed timezone handling"""
    
    def __init__(self):
        self.model = None
        
    def prepare_prophet_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for Prophet model with timezone handling"""
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
                
        return prophet_df
    
    def train_model(self, data: pd.DataFrame, config: ModelConfig) -> Dict[str, Any]:
        """Train Prophet model with enhanced error handling"""
        try:
            prophet_data = self.prepare_prophet_data(data)
            
            # Initialize Prophet model
            self.model = Prophet(**config.prophet_params)
            
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
            
            # Ensure same length for metrics calculation
            min_len = min(len(train_pred), len(train_actual))
            train_pred = train_pred.iloc[:min_len]
            train_actual = train_actual.iloc[:min_len]
            
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
    """XGBoost model builder with infinity handling"""
    
    def __init__(self):
        self.model = None
        self.scaler = StandardScaler()
        
    def prepare_features(self, df: pd.DataFrame) -> tuple:
        """Prepare features for XGBoost with infinity handling"""
        # Select relevant features from silver layer
        feature_cols = [col for col in df.columns if col not in ['Close', 'close']]
        
        # Clean features - handle infinities and NaN
        X = df[feature_cols].copy()
        
        # Replace infinite values with NaN
        X = X.replace([np.inf, -np.inf], np.nan)
        
        # Fill NaN with column means
        for col in X.columns:
            if X[col].isna().any():
                X[col] = X[col].fillna(X[col].mean())
        
        # Additional check for any remaining problematic values
        X = X.fillna(0)
        
        # Target variable
        y = df['close'] if 'close' in df.columns else df['Close']
        
        return X, y
    
    def train_model(self, data: pd.DataFrame, config: ModelConfig) -> Dict[str, Any]:
        """Train XGBoost model"""
        try:
            X, y = self.prepare_features(data)
            
            # Split data
            X_train, X_test, y_train, y_test = train_test_split(
                X, y, test_size=config.test_size, random_state=config.random_state, shuffle=False
            )
            
            # Scale features
            X_train_scaled = self.scaler.fit_transform(X_train)
            X_test_scaled = self.scaler.transform(X_test)
            
            # Train model
            self.model = xgb.XGBRegressor(**config.xgboost_params)
            self.model.fit(X_train_scaled, y_train)
            
            # Predictions
            train_pred = self.model.predict(X_train_scaled)
            test_pred = self.model.predict(X_test_scaled)
            
            # Metrics
            metrics = {
                'train_mae': mean_absolute_error(y_train, train_pred),
                'train_mse': mean_squared_error(y_train, train_pred),
                'train_rmse': np.sqrt(mean_squared_error(y_train, train_pred)),
                'train_r2': r2_score(y_train, train_pred),
                'test_mae': mean_absolute_error(y_test, test_pred),
                'test_mse': mean_squared_error(y_test, test_pred),
                'test_rmse': np.sqrt(mean_squared_error(y_test, test_pred)),
                'test_r2': r2_score(y_test, test_pred)
            }
            
            return {
                'model': self.model,
                'scaler': self.scaler,
                'metrics': metrics,
                'feature_importance': dict(zip(X.columns, self.model.feature_importances_)),
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
        self.weights = None
        
    def train_model(self, data: pd.DataFrame, config: ModelConfig) -> Dict[str, Any]:
        """Train ensemble model"""
        try:
            # Train individual models
            prophet_result = self.prophet_builder.train_model(data, config)
            xgboost_result = self.xgboost_builder.train_model(data, config)
            
            # Check if both models trained successfully
            prophet_success = prophet_result.get('status') == 'success'
            xgboost_success = xgboost_result.get('status') == 'success'
            
            if not prophet_success and not xgboost_success:
                return {'status': 'failed', 'error': 'Both Prophet and XGBoost failed'}
            
            # Calculate ensemble weights based on performance
            if prophet_success and xgboost_success:
                prophet_r2 = prophet_result['metrics'].get('r2', 0)
                xgboost_r2 = xgboost_result['metrics'].get('test_r2', 0)
                
                total_r2 = prophet_r2 + xgboost_r2
                if total_r2 > 0:
                    self.weights = {
                        'prophet': prophet_r2 / total_r2,
                        'xgboost': xgboost_r2 / total_r2
                    }
                else:
                    self.weights = {'prophet': 0.5, 'xgboost': 0.5}
            elif prophet_success:
                self.weights = {'prophet': 1.0, 'xgboost': 0.0}
            else:
                self.weights = {'prophet': 0.0, 'xgboost': 1.0}
            
            # Combine metrics
            combined_metrics = {
                'ensemble_weights': self.weights,
                'prophet_metrics': prophet_result.get('metrics', {}),
                'xgboost_metrics': xgboost_result.get('metrics', {}),
                'prophet_success': prophet_success,
                'xgboost_success': xgboost_success
            }
            
            return {
                'prophet_model': prophet_result if prophet_success else None,
                'xgboost_model': xgboost_result if xgboost_success else None,
                'weights': self.weights,
                'metrics': combined_metrics,
                'status': 'success'
            }
            
        except Exception as e:
            logger.error(f"Ensemble model training failed: {e}")
            return {'status': 'failed', 'error': str(e)}

class MultiAssetModelBuilder:
    """Main class for building models across all assets"""
    
    def __init__(self):
        self.data_loader = SilverLayerDataLoader()
        self.config = ModelConfig()
        self.results = []
        
        # Create output directories
        self.output_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/multi_asset_models")
        self.output_dir.mkdir(exist_ok=True)
        
        # Database for storing results
        self.db_path = self.output_dir / "model_performance.db"
        self._init_database()
    
    def _init_database(self):
        """Initialize SQLite database for model performance tracking"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT,
                    asset TEXT,
                    interval TEXT,
                    category TEXT,
                    model_type TEXT,
                    status TEXT,
                    mae REAL,
                    mse REAL,
                    rmse REAL,
                    r2 REAL,
                    additional_metrics TEXT
                )
            """)
    
    def save_model_results(self, asset: str, interval: str, category: str, 
                          model_type: str, result: Dict[str, Any]):
        """Save model results to database"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                metrics = result.get('metrics', {})
                
                # Extract primary metrics
                mae = metrics.get('mae') or metrics.get('test_mae', 0)
                mse = metrics.get('mse') or metrics.get('test_mse', 0)
                rmse = metrics.get('rmse') or metrics.get('test_rmse', 0)
                r2 = metrics.get('r2') or metrics.get('test_r2', 0)
                
                conn.execute("""
                    INSERT INTO model_performance 
                    (timestamp, asset, interval, category, model_type, status, mae, mse, rmse, r2, additional_metrics)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    datetime.now().isoformat(),
                    asset, interval, category, model_type,
                    result.get('status', 'unknown'),
                    mae, mse, rmse, r2,
                    json.dumps(metrics)
                ))
                
                # Save model files
                if result.get('status') == 'success' and 'model' in result:
                    model_dir = self.output_dir / f"{asset}_{interval}"
                    model_dir.mkdir(exist_ok=True)
                    
                    model_file = model_dir / f"{model_type}_model.joblib"
                    joblib.dump(result, model_file)
                    logger.info(f"Saved {model_type} model for {asset} {interval}")
                    
        except Exception as e:
            logger.error(f"Error saving model results: {e}")
    
    def build_all_models(self):
        """Build models for all available assets and intervals"""
        logger.info("Starting comprehensive model building process")
        
        # Get available assets
        assets = self.data_loader.get_available_assets()
        
        # Target intervals
        intervals = ['1d', '1h']  # Skip 1m for now due to size
        
        for category, asset_list in assets.items():
            for asset in asset_list:
                logger.info(f"Building models for {asset} ({category})")
                
                for interval in intervals:
                    logger.info(f"Processing {asset} {interval}")
                    
                    # Load data
                    data = self.data_loader.load_asset_data(asset, interval, category)
                    if data is None or data.empty:
                        continue
                    
                    # Build Prophet model
                    logger.info(f"Training Prophet model for {asset} {interval}")
                    prophet_builder = ProphetModelBuilder()
                    prophet_result = prophet_builder.train_model(data, self.config)
                    self.save_model_results(asset, interval, category, 'prophet', prophet_result)
                    
                    # Build XGBoost model
                    logger.info(f"Training XGBoost model for {asset} {interval}")
                    xgboost_builder = XGBoostModelBuilder()
                    xgboost_result = xgboost_builder.train_model(data, self.config)
                    self.save_model_results(asset, interval, category, 'xgboost', xgboost_result)
                    
                    # Build Ensemble model
                    logger.info(f"Training Ensemble model for {asset} {interval}")
                    ensemble_builder = EnsembleModelBuilder()
                    ensemble_result = ensemble_builder.train_model(data, self.config)
                    self.save_model_results(asset, interval, category, 'ensemble', ensemble_result)
        
        # Generate summary report
        self.generate_summary_report()
    
    def generate_summary_report(self):
        """Generate comprehensive summary report"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                df = pd.read_sql_query("SELECT * FROM model_performance", conn)
            
            # Summary statistics
            summary = {
                'total_models': len(df),
                'successful_models': len(df[df['status'] == 'success']),
                'failed_models': len(df[df['status'] == 'failed']),
                'success_rate': len(df[df['status'] == 'success']) / len(df) if len(df) > 0 else 0,
                'timestamp': datetime.now().isoformat(),
                'by_model_type': df.groupby('model_type')['status'].value_counts().to_dict(),
                'by_asset': df.groupby('asset')['status'].value_counts().to_dict(),
                'performance_summary': df[df['status'] == 'success'].groupby('model_type').agg({
                    'mae': 'mean',
                    'mse': 'mean', 
                    'rmse': 'mean',
                    'r2': 'mean'
                }).to_dict()
            }
            
            # Save report
            report_file = self.output_dir / f"model_building_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
            with open(report_file, 'w') as f:
                json.dump(summary, f, indent=2)
                
            logger.info(f"Summary report saved to: {report_file}")
            logger.info(f"Model building complete: {summary['successful_models']}/{summary['total_models']} models successful")
            
        except Exception as e:
            logger.error(f"Error generating summary report: {e}")

def main():
    """Main execution function"""
    builder = MultiAssetModelBuilder()
    builder.build_all_models()

if __name__ == "__main__":
    main()
