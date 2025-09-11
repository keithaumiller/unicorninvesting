#!/usr/bin/env python3
"""
USDCHF Prophet Model - Multi-Interval Implementation
Time series forecasting using Facebook Prophet with silver layer features
Intervals: 1m, 1h, 1d
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import sys
import logging
import pickle
import json

# Add project root to path
project_root = Path(__file__).parent.parent.parent.parent
sys.path.append(str(project_root))

try:
    from prophet import Prophet
    from prophet.serialize import model_to_json, model_from_json
except ImportError:
    print("Installing Prophet...")
    import subprocess
    subprocess.run([sys.executable, "-m", "pip", "install", "prophet"])
    from prophet import Prophet
    from prophet.serialize import model_to_json, model_from_json

from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

class USDCHFProphetModel:
    """Prophet forecasting model for USDCHF"""
    
    def __init__(self, interval: str = "1d"):
        self.asset = "USDCHF"
        self.category = "forex"
        self.interval = interval
        self.model = None
        self.forecast = None
        self.metrics = {}
        
        # Silver layer data path
        self.data_path = Path(__file__).parent.parent.parent.parent / "1_data_sources/3_silver"
        
        # Configure Prophet parameters by interval
        self.prophet_configs = {
            "1m": {
                "changepoint_prior_scale": 0.01,
                "seasonality_prior_scale": 10.0,
                "holidays_prior_scale": 10.0,
                "seasonality_mode": "additive",
                "yearly_seasonality": False,
                "weekly_seasonality": True,
                "daily_seasonality": True
            },
            "1h": {
                "changepoint_prior_scale": 0.05,
                "seasonality_prior_scale": 10.0,
                "holidays_prior_scale": 10.0,
                "seasonality_mode": "multiplicative",
                "yearly_seasonality": True,
                "weekly_seasonality": True,
                "daily_seasonality": True
            },
            "1d": {
                "changepoint_prior_scale": 0.1,
                "seasonality_prior_scale": 10.0,
                "holidays_prior_scale": 10.0,
                "seasonality_mode": "multiplicative",
                "yearly_seasonality": True,
                "weekly_seasonality": True,
                "daily_seasonality": False
            }
        }
        
    def load_data(self) -> pd.DataFrame:
        """Load silver layer data"""
        try:
            if self.category == "crypto":
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/crypto/USDCHF_silver_{self.interval}_latest.csv"
            else:
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/forex/USDCHF_silver_{self.interval}_latest.csv"
                
            if not filepath.exists():
                raise FileNotFoundError(f"Data file not found: {filepath}")
                
            df = pd.read_csv(filepath, index_col=0, parse_dates=True)
            logging.info(f"Loaded USDCHF {self.interval} data: {df.shape[0]} records")
            return df
            
        except Exception as e:
            logging.error(f"Error loading data: {e}")
            raise
    
    def prepare_prophet_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for Prophet"""
        # Basic Prophet format
        prophet_df = pd.DataFrame({
            'ds': df.index,
            'y': df['close']
        })
        
        # Add silver layer regressors
        regressors = [
            'volume', 'rsi', 'williams_r', 'cci', 'adx', 'volatility_14',
            'ma_10', 'ma_20', 'ma_50', 'momentum_5', 'momentum_10', 'trend_strength'
        ]
        
        for regressor in regressors:
            if regressor in df.columns and not df[regressor].isna().all():
                prophet_df[regressor] = df[regressor].fillna(df[regressor].mean())
                
        return prophet_df
    
    def train_model(self, periods: int = 30) -> dict:
        """Train Prophet model"""
        try:
            # Load and prepare data
            data = self.load_data()
            prophet_data = self.prepare_prophet_data(data)
            
            # Initialize Prophet with interval-specific config
            config = self.prophet_configs[self.interval]
            self.model = Prophet(**config)
            
            # Add regressors
            for col in prophet_data.columns:
                if col not in ['ds', 'y']:
                    self.model.add_regressor(col)
            
            # Fit model
            self.model.fit(prophet_data)
            
            # Generate forecast
            future = self.model.make_future_dataframe(periods=periods, freq='H' if self.interval == '1h' else 'D')
            
            # Fill regressor values for future periods
            for col in prophet_data.columns:
                if col not in ['ds', 'y']:
                    future[col] = prophet_data[col].fillna(prophet_data[col].mean()).iloc[-1]
            
            self.forecast = self.model.predict(future)
            
            # Calculate metrics
            train_pred = self.forecast['yhat'].iloc[:-periods]
            train_actual = prophet_data['y']
            
            self.metrics = {
                'mae': mean_absolute_error(train_actual, train_pred),
                'mse': mean_squared_error(train_actual, train_pred),
                'rmse': np.sqrt(mean_squared_error(train_actual, train_pred)),
                'r2': r2_score(train_actual, train_pred),
                'training_records': len(train_actual),
                'forecast_periods': periods,
                'interval': self.interval
            }
            
            return {
                'status': 'success',
                'metrics': self.metrics,
                'forecast': self.forecast,
                'model': self.model
            }
            
        except Exception as e:
            logging.error(f"Training failed: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def save_model(self, filepath: str = None):
        """Save trained model"""
        if not filepath:
            filepath = f"USDCHF_prophet_{self.interval}_model.pkl"
            
        model_data = {
            'model_json': model_to_json(self.model),
            'forecast': self.forecast,
            'metrics': self.metrics,
            'metadata': {
                'asset': self.asset,
                'category': self.category,
                'interval': self.interval,
                'trained_at': datetime.now().isoformat()
            }
        }
        
        with open(filepath, 'wb') as f:
            pickle.dump(model_data, f)
            
        logging.info(f"Model saved to {filepath}")
    
    def load_model(self, filepath: str):
        """Load saved model"""
        with open(filepath, 'rb') as f:
            model_data = pickle.load(f)
            
        self.model = model_from_json(model_data['model_json'])
        self.forecast = model_data['forecast']
        self.metrics = model_data['metrics']
        
        logging.info(f"Model loaded from {filepath}")

def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description='USDCHF Prophet Model')
    parser.add_argument('--interval', default='1d', choices=['1m', '1h', '1d'],
                       help='Time interval for modeling')
    parser.add_argument('--periods', type=int, default=30,
                       help='Number of periods to forecast')
    parser.add_argument('--save', action='store_true',
                       help='Save trained model')
    
    args = parser.parse_args()
    
    # Train model
    model = USDCHFProphetModel(interval=args.interval)
    result = model.train_model(periods=args.periods)
    
    if result['status'] == 'success':
        print(f"✅ USDCHF Prophet {args.interval} model training successful!")
        print(f"📊 Metrics: R² = {result['metrics']['r2']:.4f}, MAE = {result['metrics']['mae']:.4f}")
        
        if args.save:
            model.save_model()
    else:
        print(f"❌ Training failed: {result['error']}")

if __name__ == "__main__":
    main()
