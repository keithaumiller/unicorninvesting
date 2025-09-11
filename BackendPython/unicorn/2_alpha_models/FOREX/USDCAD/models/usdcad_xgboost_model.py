#!/usr/bin/env python3
"""
USDCAD XGBoost Model - Multi-Interval Implementation  
Gradient boosting prediction using silver layer features
Intervals: 1m, 1h, 1d with comprehensive feature engineering
"""

import pandas as pd
import numpy as np
from datetime import datetime
from pathlib import Path
import sys
import logging
import pickle
import json

# Add project root to path
project_root = Path(__file__).parent.parent.parent.parent
sys.path.append(str(project_root))

try:
    import xgboost as xgb
except ImportError:
    print("Installing XGBoost...")
    import subprocess
    subprocess.run([sys.executable, "-m", "pip", "install", "xgboost"])
    import xgboost as xgb

from sklearn.model_selection import train_test_split, TimeSeriesSplit
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

class USDCADXGBoostModel:
    """XGBoost prediction model for USDCAD"""
    
    def __init__(self, interval: str = "1d"):
        self.asset = "USDCAD"
        self.category = "forex"
        self.interval = interval
        self.model = None
        self.scaler = StandardScaler()
        self.feature_columns = []
        self.metrics = {}
        
        # Silver layer data path
        self.data_path = Path(__file__).parent.parent.parent.parent / "1_data_sources/3_silver"
        
        # XGBoost parameters by interval
        self.xgb_params = {
            "1m": {
                "n_estimators": 200,
                "max_depth": 4,
                "learning_rate": 0.05,
                "subsample": 0.8,
                "colsample_bytree": 0.8,
                "random_state": 42
            },
            "1h": {
                "n_estimators": 150,
                "max_depth": 6,
                "learning_rate": 0.1,
                "subsample": 0.9,
                "colsample_bytree": 0.9,
                "random_state": 42
            },
            "1d": {
                "n_estimators": 100,
                "max_depth": 8,
                "learning_rate": 0.15,
                "subsample": 1.0,
                "colsample_bytree": 1.0,
                "random_state": 42
            }
        }
        
    def load_data(self) -> pd.DataFrame:
        """Load silver layer data"""
        try:
            if self.category == "crypto":
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/crypto/USDCAD_silver_{self.interval}_latest.csv"
            else:
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/forex/USDCAD_silver_{self.interval}_latest.csv"
                
            if not filepath.exists():
                raise FileNotFoundError(f"Data file not found: {filepath}")
                
            df = pd.read_csv(filepath, index_col=0, parse_dates=True)
            logging.info(f"Loaded USDCAD {self.interval} data: {df.shape[0]} records × {df.shape[1]} features")
            return df
            
        except Exception as e:
            logging.error(f"Error loading data: {e}")
            raise
    
    def prepare_features(self, df: pd.DataFrame) -> tuple:
        """Prepare features and target from silver layer data"""
        # Core price features
        price_features = ['open', 'high', 'low', 'volume', 'price_change', 'hl_range', 'oc_range']
        
        # Technical indicators
        technical_features = [
            'rsi', 'williams_r', 'cci', 'adx', 'volatility_14', 'volatility_annualized',
            'ma_10', 'ma_20', 'ma_50', 'momentum_5', 'momentum_10', 'momentum_20', 'momentum_50'
        ]
        
        # Volume analysis  
        volume_features = [
            'volume_change', 'volume_ma_20', 'volume_ratio', 'volume_roc', 
            'volume_ma_50', 'volume_ratio_50', 'volume_trend'
        ]
        
        # Support/Resistance
        support_resistance = [
            'high_20', 'low_20', 'price_position', 'resistance_level', 
            'support_level', 'price_position_enhanced'
        ]
        
        # Temporal features
        temporal_features = ['hour', 'day_of_week', 'day_of_month', 'month']
        
        # Market regime
        regime_features = ['trend_strength', 'volatility_regime']
        
        # Additional forex-specific features
        forex_features = []
        if self.category == "forex":
            forex_features = [
                'pips_change', 'pips_range', 'atr', 'stoch_k', 'stoch_d',
                'macd', 'macd_signal', 'macd_histogram', 'spread_proxy',
                'spread_ma', 'spread_normalized', 'session_overlap'
            ]
        
        # Combine all features
        all_features = (price_features + technical_features + volume_features + 
                       support_resistance + temporal_features + regime_features + forex_features)
        
        # Filter available features
        available_features = [f for f in all_features if f in df.columns]
        self.feature_columns = available_features
        
        # Prepare feature matrix
        X = df[available_features].fillna(df[available_features].mean())
        
        # Target: next period close price (shifted)
        y = df['close'].shift(-1).fillna(method='ffill')
        
        # Remove last row (no target available)
        X = X.iloc[:-1]
        y = y.iloc[:-1]
        
        return X.values, y.values
    
    def train_model(self, test_size: float = 0.2) -> dict:
        """Train XGBoost model"""
        try:
            # Load and prepare data
            data = self.load_data()
            X, y = self.prepare_features(data)
            
            # Split data (time series split)
            split_idx = int(len(X) * (1 - test_size))
            X_train, X_test = X[:split_idx], X[split_idx:]
            y_train, y_test = y[:split_idx], y[split_idx:]
            
            # Scale features
            X_train_scaled = self.scaler.fit_transform(X_train)
            X_test_scaled = self.scaler.transform(X_test)
            
            # Train model with interval-specific parameters
            params = self.xgb_params[self.interval]
            self.model = xgb.XGBRegressor(**params, n_jobs=-1)
            
            self.model.fit(X_train_scaled, y_train)
            
            # Predictions
            y_train_pred = self.model.predict(X_train_scaled)
            y_test_pred = self.model.predict(X_test_scaled)
            
            # Calculate metrics
            train_metrics = {
                'mae': mean_absolute_error(y_train, y_train_pred),
                'mse': mean_squared_error(y_train, y_train_pred),
                'rmse': np.sqrt(mean_squared_error(y_train, y_train_pred)),
                'r2': r2_score(y_train, y_train_pred)
            }
            
            test_metrics = {
                'mae': mean_absolute_error(y_test, y_test_pred),
                'mse': mean_squared_error(y_test, y_test_pred),
                'rmse': np.sqrt(mean_squared_error(y_test, y_test_pred)),
                'r2': r2_score(y_test, y_test_pred)
            }
            
            self.metrics = {
                'train_metrics': train_metrics,
                'test_metrics': test_metrics,
                'feature_importance': dict(zip(self.feature_columns, self.model.feature_importances_)),
                'training_records': len(X_train),
                'test_records': len(X_test),
                'feature_count': len(self.feature_columns),
                'interval': self.interval
            }
            
            return {
                'status': 'success',
                'metrics': self.metrics,
                'model': self.model,
                'scaler': self.scaler,
                'feature_columns': self.feature_columns
            }
            
        except Exception as e:
            logging.error(f"Training failed: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def predict(self, data: pd.DataFrame) -> np.ndarray:
        """Make predictions on new data"""
        X, _ = self.prepare_features(data)
        X_scaled = self.scaler.transform(X)
        return self.model.predict(X_scaled)
    
    def save_model(self, filepath: str = None):
        """Save trained model"""
        if not filepath:
            filepath = f"USDCAD_xgboost_{self.interval}_model.pkl"
            
        model_data = {
            'model': self.model,
            'scaler': self.scaler,
            'feature_columns': self.feature_columns,
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
            
        self.model = model_data['model']
        self.scaler = model_data['scaler']
        self.feature_columns = model_data['feature_columns']
        self.metrics = model_data['metrics']
        
        logging.info(f"Model loaded from {filepath}")

def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description='USDCAD XGBoost Model')
    parser.add_argument('--interval', default='1d', choices=['1m', '1h', '1d'],
                       help='Time interval for modeling')
    parser.add_argument('--test-size', type=float, default=0.2,
                       help='Test set size (0.0-1.0)')
    parser.add_argument('--save', action='store_true',
                       help='Save trained model')
    
    args = parser.parse_args()
    
    # Train model
    model = USDCADXGBoostModel(interval=args.interval)
    result = model.train_model(test_size=args.test_size)
    
    if result['status'] == 'success':
        print(f"✅ USDCAD XGBoost {args.interval} model training successful!")
        print(f"📊 Test Metrics: R² = {result['metrics']['test_metrics']['r2']:.4f}, MAE = {result['metrics']['test_metrics']['mae']:.4f}")
        print(f"🔧 Features: {result['metrics']['feature_count']} features used")
        
        if args.save:
            model.save_model()
    else:
        print(f"❌ Training failed: {result['error']}")

if __name__ == "__main__":
    main()
