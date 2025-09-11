#!/usr/bin/env python3
"""
Individual Asset Model Generator
Creates dedicated model files for each asset with interval-specific implementations
Supports: ETH, BTC, EURUSD, USDJPY, GBPUSD, AUDUSD, USDCAD, USDCHF, NZDUSD
"""

import os
import sys
from pathlib import Path
from datetime import datetime
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AssetModelGenerator:
    """Generate individual model files for each asset"""
    
    def __init__(self, base_path: str):
        self.base_path = Path(base_path)
        self.crypto_assets = ['ETH', 'BTC']
        self.forex_assets = ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
        self.intervals = ['1m', '1h', '1d']
        self.methodologies = ['prophet', 'xgboost', 'ensemble']
        
    def create_prophet_model_template(self, asset: str, category: str) -> str:
        """Generate Prophet model template"""
        return f'''#!/usr/bin/env python3
"""
{asset} Prophet Model - Multi-Interval Implementation
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

class {asset}ProphetModel:
    """Prophet forecasting model for {asset}"""
    
    def __init__(self, interval: str = "1d"):
        self.asset = "{asset}"
        self.category = "{category}"
        self.interval = interval
        self.model = None
        self.forecast = None
        self.metrics = {{}}
        
        # Silver layer data path
        self.data_path = Path(__file__).parent.parent.parent.parent / "1_data_sources/3_silver"
        
        # Configure Prophet parameters by interval
        self.prophet_configs = {{
            "1m": {{
                "changepoint_prior_scale": 0.01,
                "seasonality_prior_scale": 10.0,
                "holidays_prior_scale": 10.0,
                "seasonality_mode": "additive",
                "yearly_seasonality": False,
                "weekly_seasonality": True,
                "daily_seasonality": True
            }},
            "1h": {{
                "changepoint_prior_scale": 0.05,
                "seasonality_prior_scale": 10.0,
                "holidays_prior_scale": 10.0,
                "seasonality_mode": "multiplicative",
                "yearly_seasonality": True,
                "weekly_seasonality": True,
                "daily_seasonality": True
            }},
            "1d": {{
                "changepoint_prior_scale": 0.1,
                "seasonality_prior_scale": 10.0,
                "holidays_prior_scale": 10.0,
                "seasonality_mode": "multiplicative",
                "yearly_seasonality": True,
                "weekly_seasonality": True,
                "daily_seasonality": False
            }}
        }}
        
    def load_data(self) -> pd.DataFrame:
        """Load silver layer data"""
        try:
            if self.category == "crypto":
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/crypto/{asset}_silver_{{self.interval}}_latest.csv"
            else:
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/forex/{asset}_silver_{{self.interval}}_latest.csv"
                
            if not filepath.exists():
                raise FileNotFoundError(f"Data file not found: {{filepath}}")
                
            df = pd.read_csv(filepath, index_col=0, parse_dates=True)
            logging.info(f"Loaded {asset} {{self.interval}} data: {{df.shape[0]}} records")
            return df
            
        except Exception as e:
            logging.error(f"Error loading data: {{e}}")
            raise
    
    def prepare_prophet_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for Prophet"""
        # Basic Prophet format
        prophet_df = pd.DataFrame({{
            'ds': df.index,
            'y': df['close']
        }})
        
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
            
            self.metrics = {{
                'mae': mean_absolute_error(train_actual, train_pred),
                'mse': mean_squared_error(train_actual, train_pred),
                'rmse': np.sqrt(mean_squared_error(train_actual, train_pred)),
                'r2': r2_score(train_actual, train_pred),
                'training_records': len(train_actual),
                'forecast_periods': periods,
                'interval': self.interval
            }}
            
            return {{
                'status': 'success',
                'metrics': self.metrics,
                'forecast': self.forecast,
                'model': self.model
            }}
            
        except Exception as e:
            logging.error(f"Training failed: {{e}}")
            return {{'status': 'failed', 'error': str(e)}}
    
    def save_model(self, filepath: str = None):
        """Save trained model"""
        if not filepath:
            filepath = f"{asset}_prophet_{{self.interval}}_model.pkl"
            
        model_data = {{
            'model_json': model_to_json(self.model),
            'forecast': self.forecast,
            'metrics': self.metrics,
            'metadata': {{
                'asset': self.asset,
                'category': self.category,
                'interval': self.interval,
                'trained_at': datetime.now().isoformat()
            }}
        }}
        
        with open(filepath, 'wb') as f:
            pickle.dump(model_data, f)
            
        logging.info(f"Model saved to {{filepath}}")
    
    def load_model(self, filepath: str):
        """Load saved model"""
        with open(filepath, 'rb') as f:
            model_data = pickle.load(f)
            
        self.model = model_from_json(model_data['model_json'])
        self.forecast = model_data['forecast']
        self.metrics = model_data['metrics']
        
        logging.info(f"Model loaded from {{filepath}}")

def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description='{asset} Prophet Model')
    parser.add_argument('--interval', default='1d', choices=['1m', '1h', '1d'],
                       help='Time interval for modeling')
    parser.add_argument('--periods', type=int, default=30,
                       help='Number of periods to forecast')
    parser.add_argument('--save', action='store_true',
                       help='Save trained model')
    
    args = parser.parse_args()
    
    # Train model
    model = {asset}ProphetModel(interval=args.interval)
    result = model.train_model(periods=args.periods)
    
    if result['status'] == 'success':
        print(f"✅ {asset} Prophet {{args.interval}} model training successful!")
        print(f"📊 Metrics: R² = {{result['metrics']['r2']:.4f}}, MAE = {{result['metrics']['mae']:.4f}}")
        
        if args.save:
            model.save_model()
    else:
        print(f"❌ Training failed: {{result['error']}}")

if __name__ == "__main__":
    main()
'''
    
    def create_xgboost_model_template(self, asset: str, category: str) -> str:
        """Generate XGBoost model template"""
        return f'''#!/usr/bin/env python3
"""
{asset} XGBoost Model - Multi-Interval Implementation  
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

class {asset}XGBoostModel:
    """XGBoost prediction model for {asset}"""
    
    def __init__(self, interval: str = "1d"):
        self.asset = "{asset}"
        self.category = "{category}"
        self.interval = interval
        self.model = None
        self.scaler = StandardScaler()
        self.feature_columns = []
        self.metrics = {{}}
        
        # Silver layer data path
        self.data_path = Path(__file__).parent.parent.parent.parent / "1_data_sources/3_silver"
        
        # XGBoost parameters by interval
        self.xgb_params = {{
            "1m": {{
                "n_estimators": 200,
                "max_depth": 4,
                "learning_rate": 0.05,
                "subsample": 0.8,
                "colsample_bytree": 0.8,
                "random_state": 42
            }},
            "1h": {{
                "n_estimators": 150,
                "max_depth": 6,
                "learning_rate": 0.1,
                "subsample": 0.9,
                "colsample_bytree": 0.9,
                "random_state": 42
            }},
            "1d": {{
                "n_estimators": 100,
                "max_depth": 8,
                "learning_rate": 0.15,
                "subsample": 1.0,
                "colsample_bytree": 1.0,
                "random_state": 42
            }}
        }}
        
    def load_data(self) -> pd.DataFrame:
        """Load silver layer data"""
        try:
            if self.category == "crypto":
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/crypto/{asset}_silver_{{self.interval}}_latest.csv"
            else:
                filepath = self.data_path / f"yahoo_finance_assets/processed_data/forex/{asset}_silver_{{self.interval}}_latest.csv"
                
            if not filepath.exists():
                raise FileNotFoundError(f"Data file not found: {{filepath}}")
                
            df = pd.read_csv(filepath, index_col=0, parse_dates=True)
            logging.info(f"Loaded {asset} {{self.interval}} data: {{df.shape[0]}} records × {{df.shape[1]}} features")
            return df
            
        except Exception as e:
            logging.error(f"Error loading data: {{e}}")
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
            train_metrics = {{
                'mae': mean_absolute_error(y_train, y_train_pred),
                'mse': mean_squared_error(y_train, y_train_pred),
                'rmse': np.sqrt(mean_squared_error(y_train, y_train_pred)),
                'r2': r2_score(y_train, y_train_pred)
            }}
            
            test_metrics = {{
                'mae': mean_absolute_error(y_test, y_test_pred),
                'mse': mean_squared_error(y_test, y_test_pred),
                'rmse': np.sqrt(mean_squared_error(y_test, y_test_pred)),
                'r2': r2_score(y_test, y_test_pred)
            }}
            
            self.metrics = {{
                'train_metrics': train_metrics,
                'test_metrics': test_metrics,
                'feature_importance': dict(zip(self.feature_columns, self.model.feature_importances_)),
                'training_records': len(X_train),
                'test_records': len(X_test),
                'feature_count': len(self.feature_columns),
                'interval': self.interval
            }}
            
            return {{
                'status': 'success',
                'metrics': self.metrics,
                'model': self.model,
                'scaler': self.scaler,
                'feature_columns': self.feature_columns
            }}
            
        except Exception as e:
            logging.error(f"Training failed: {{e}}")
            return {{'status': 'failed', 'error': str(e)}}
    
    def predict(self, data: pd.DataFrame) -> np.ndarray:
        """Make predictions on new data"""
        X, _ = self.prepare_features(data)
        X_scaled = self.scaler.transform(X)
        return self.model.predict(X_scaled)
    
    def save_model(self, filepath: str = None):
        """Save trained model"""
        if not filepath:
            filepath = f"{asset}_xgboost_{{self.interval}}_model.pkl"
            
        model_data = {{
            'model': self.model,
            'scaler': self.scaler,
            'feature_columns': self.feature_columns,
            'metrics': self.metrics,
            'metadata': {{
                'asset': self.asset,
                'category': self.category,
                'interval': self.interval,
                'trained_at': datetime.now().isoformat()
            }}
        }}
        
        with open(filepath, 'wb') as f:
            pickle.dump(model_data, f)
            
        logging.info(f"Model saved to {{filepath}}")
    
    def load_model(self, filepath: str):
        """Load saved model"""
        with open(filepath, 'rb') as f:
            model_data = pickle.load(f)
            
        self.model = model_data['model']
        self.scaler = model_data['scaler']
        self.feature_columns = model_data['feature_columns']
        self.metrics = model_data['metrics']
        
        logging.info(f"Model loaded from {{filepath}}")

def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description='{asset} XGBoost Model')
    parser.add_argument('--interval', default='1d', choices=['1m', '1h', '1d'],
                       help='Time interval for modeling')
    parser.add_argument('--test-size', type=float, default=0.2,
                       help='Test set size (0.0-1.0)')
    parser.add_argument('--save', action='store_true',
                       help='Save trained model')
    
    args = parser.parse_args()
    
    # Train model
    model = {asset}XGBoostModel(interval=args.interval)
    result = model.train_model(test_size=args.test_size)
    
    if result['status'] == 'success':
        print(f"✅ {asset} XGBoost {{args.interval}} model training successful!")
        print(f"📊 Test Metrics: R² = {{result['metrics']['test_metrics']['r2']:.4f}}, MAE = {{result['metrics']['test_metrics']['mae']:.4f}}")
        print(f"🔧 Features: {{result['metrics']['feature_count']}} features used")
        
        if args.save:
            model.save_model()
    else:
        print(f"❌ Training failed: {{result['error']}}")

if __name__ == "__main__":
    main()
'''
    
    def create_ensemble_model_template(self, asset: str, category: str) -> str:
        """Generate Ensemble model template"""
        return f'''#!/usr/bin/env python3
"""
{asset} Ensemble Model - Multi-Interval Implementation
Combines Prophet and XGBoost predictions with optimized weighting
Intervals: 1m, 1h, 1d with dynamic weight adjustment
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

# Import individual models
from {asset.lower()}_prophet_model import {asset}ProphetModel
from {asset.lower()}_xgboost_model import {asset}XGBoostModel

from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.linear_model import LinearRegression

class {asset}EnsembleModel:
    """Ensemble model combining Prophet and XGBoost for {asset}"""
    
    def __init__(self, interval: str = "1d"):
        self.asset = "{asset}"
        self.category = "{category}"
        self.interval = interval
        
        # Initialize component models
        self.prophet_model = {asset}ProphetModel(interval=interval)
        self.xgboost_model = {asset}XGBoostModel(interval=interval)
        
        # Ensemble configuration
        self.weights = {{'prophet': 0.5, 'xgboost': 0.5}}
        self.weight_optimizer = LinearRegression()
        self.ensemble_metrics = {{}}
        
        # Ensemble methods
        self.ensemble_methods = {{
            "simple_average": self._simple_average,
            "weighted_average": self._weighted_average,
            "optimized_weights": self._optimized_weights,
            "dynamic_weights": self._dynamic_weights
        }}
        
    def train_component_models(self) -> dict:
        """Train both Prophet and XGBoost models"""
        results = {{}}
        
        try:
            # Train Prophet model
            logging.info(f"Training Prophet model for {asset} {{self.interval}}")
            prophet_result = self.prophet_model.train_model()
            results['prophet'] = prophet_result
            
            if prophet_result['status'] != 'success':
                return {{'status': 'failed', 'error': 'Prophet training failed'}}
            
            # Train XGBoost model
            logging.info(f"Training XGBoost model for {asset} {{self.interval}}")
            xgboost_result = self.xgboost_model.train_model()
            results['xgboost'] = xgboost_result
            
            if xgboost_result['status'] != 'success':
                return {{'status': 'failed', 'error': 'XGBoost training failed'}}
            
            return {{'status': 'success', 'component_results': results}}
            
        except Exception as e:
            logging.error(f"Component model training failed: {{e}}")
            return {{'status': 'failed', 'error': str(e)}}
    
    def _simple_average(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray) -> np.ndarray:
        """Simple average ensemble"""
        return (prophet_pred + xgboost_pred) / 2
    
    def _weighted_average(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray) -> np.ndarray:
        """Weighted average based on individual model performance"""
        return (self.weights['prophet'] * prophet_pred + 
                self.weights['xgboost'] * xgboost_pred)
    
    def _optimized_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray, 
                          actual: np.ndarray) -> np.ndarray:
        """Optimize weights using linear regression"""
        # Stack predictions
        X = np.column_stack([prophet_pred, xgboost_pred])
        
        # Fit weight optimizer
        self.weight_optimizer.fit(X, actual)
        
        # Get optimized prediction
        return self.weight_optimizer.predict(X)
    
    def _dynamic_weights(self, prophet_pred: np.ndarray, xgboost_pred: np.ndarray,
                        actual: np.ndarray = None) -> np.ndarray:
        """Dynamic weights based on recent performance"""
        if actual is None:
            return self._weighted_average(prophet_pred, xgboost_pred)
        
        # Calculate recent performance (last 20% of data)
        split_idx = int(len(actual) * 0.8)
        
        prophet_recent_mae = mean_absolute_error(actual[split_idx:], prophet_pred[split_idx:])
        xgboost_recent_mae = mean_absolute_error(actual[split_idx:], xgboost_pred[split_idx:])
        
        # Inverse MAE weighting (lower MAE = higher weight)
        total_inv_mae = (1/prophet_recent_mae) + (1/xgboost_recent_mae)
        prophet_weight = (1/prophet_recent_mae) / total_inv_mae
        xgboost_weight = (1/xgboost_recent_mae) / total_inv_mae
        
        return prophet_weight * prophet_pred + xgboost_weight * xgboost_pred
    
    def create_ensemble_predictions(self, data: pd.DataFrame, method: str = "weighted_average") -> dict:
        """Create ensemble predictions using specified method"""
        try:
            # Get Prophet predictions
            prophet_forecast = self.prophet_model.forecast
            prophet_pred = prophet_forecast['yhat'].values
            
            # Get XGBoost predictions  
            xgboost_pred = self.xgboost_model.predict(data)
            
            # Align predictions (take minimum length)
            min_len = min(len(prophet_pred), len(xgboost_pred))
            prophet_pred = prophet_pred[-min_len:]
            xgboost_pred = xgboost_pred[-min_len:]
            
            # Get actual values for comparison
            actual = data['close'].iloc[-min_len:].values
            
            # Apply ensemble method
            ensemble_method = self.ensemble_methods.get(method, self._weighted_average)
            
            if method == "optimized_weights" or method == "dynamic_weights":
                ensemble_pred = ensemble_method(prophet_pred, xgboost_pred, actual)
            else:
                ensemble_pred = ensemble_method(prophet_pred, xgboost_pred)
            
            # Calculate ensemble metrics
            ensemble_metrics = {{
                'mae': mean_absolute_error(actual, ensemble_pred),
                'mse': mean_squared_error(actual, ensemble_pred),
                'rmse': np.sqrt(mean_squared_error(actual, ensemble_pred)),
                'r2': r2_score(actual, ensemble_pred)
            }}
            
            # Compare with individual models
            prophet_metrics = {{
                'mae': mean_absolute_error(actual, prophet_pred),
                'r2': r2_score(actual, prophet_pred)
            }}
            
            xgboost_metrics = {{
                'mae': mean_absolute_error(actual, xgboost_pred),
                'r2': r2_score(actual, xgboost_pred)
            }}
            
            return {{
                'status': 'success',
                'ensemble_predictions': ensemble_pred,
                'prophet_predictions': prophet_pred,
                'xgboost_predictions': xgboost_pred,
                'actual_values': actual,
                'ensemble_metrics': ensemble_metrics,
                'prophet_metrics': prophet_metrics,
                'xgboost_metrics': xgboost_metrics,
                'method': method,
                'weights': self.weights
            }}
            
        except Exception as e:
            logging.error(f"Ensemble prediction failed: {{e}}")
            return {{'status': 'failed', 'error': str(e)}}
    
    def train_ensemble(self, method: str = "weighted_average") -> dict:
        """Train complete ensemble model"""
        try:
            # Train component models
            component_result = self.train_component_models()
            if component_result['status'] != 'success':
                return component_result
            
            # Load data for ensemble creation
            data = self.prophet_model.load_data()
            
            # Create ensemble predictions
            ensemble_result = self.create_ensemble_predictions(data, method)
            if ensemble_result['status'] != 'success':
                return ensemble_result
            
            # Store results
            self.ensemble_metrics = ensemble_result['ensemble_metrics']
            
            # Complete result
            final_result = {{
                'status': 'success',
                'component_results': component_result['component_results'],
                'ensemble_result': ensemble_result,
                'metadata': {{
                    'asset': self.asset,
                    'category': self.category,
                    'interval': self.interval,
                    'ensemble_method': method,
                    'trained_at': datetime.now().isoformat()
                }}
            }}
            
            return final_result
            
        except Exception as e:
            logging.error(f"Ensemble training failed: {{e}}")
            return {{'status': 'failed', 'error': str(e)}}
    
    def save_ensemble(self, filepath: str = None):
        """Save complete ensemble model"""
        if not filepath:
            filepath = f"{asset}_ensemble_{{self.interval}}_model.pkl"
            
        ensemble_data = {{
            'prophet_model': self.prophet_model,
            'xgboost_model': self.xgboost_model,
            'weights': self.weights,
            'ensemble_metrics': self.ensemble_metrics,
            'metadata': {{
                'asset': self.asset,
                'category': self.category,
                'interval': self.interval,
                'saved_at': datetime.now().isoformat()
            }}
        }}
        
        with open(filepath, 'wb') as f:
            pickle.dump(ensemble_data, f)
            
        logging.info(f"Ensemble model saved to {{filepath}}")

def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description='{asset} Ensemble Model')
    parser.add_argument('--interval', default='1d', choices=['1m', '1h', '1d'],
                       help='Time interval for modeling')
    parser.add_argument('--method', default='weighted_average',
                       choices=['simple_average', 'weighted_average', 'optimized_weights', 'dynamic_weights'],
                       help='Ensemble method')
    parser.add_argument('--save', action='store_true',
                       help='Save trained ensemble')
    
    args = parser.parse_args()
    
    # Train ensemble
    ensemble = {asset}EnsembleModel(interval=args.interval)
    result = ensemble.train_ensemble(method=args.method)
    
    if result['status'] == 'success':
        print(f"✅ {asset} Ensemble {{args.interval}} model training successful!")
        
        # Display metrics comparison
        ensemble_metrics = result['ensemble_result']['ensemble_metrics']
        prophet_metrics = result['ensemble_result']['prophet_metrics']
        xgboost_metrics = result['ensemble_result']['xgboost_metrics']
        
        print(f"📊 Ensemble Metrics: R² = {{ensemble_metrics['r2']:.4f}}, MAE = {{ensemble_metrics['mae']:.4f}}")
        print(f"📈 Prophet Metrics:  R² = {{prophet_metrics['r2']:.4f}}, MAE = {{prophet_metrics['mae']:.4f}}")
        print(f"🚀 XGBoost Metrics:  R² = {{xgboost_metrics['r2']:.4f}}, MAE = {{xgboost_metrics['mae']:.4f}}")
        
        if args.save:
            ensemble.save_ensemble()
    else:
        print(f"❌ Training failed: {{result['error']}}")

if __name__ == "__main__":
    main()
'''
    
    def generate_asset_models(self, asset: str, category: str):
        """Generate all model files for a specific asset"""
        # Create asset directory
        if category == "crypto":
            asset_dir = self.base_path / "CRYPTO" / asset
        else:
            asset_dir = self.base_path / "FOREX" / asset
            
        asset_dir.mkdir(parents=True, exist_ok=True)
        
        # Create models subdirectory
        models_dir = asset_dir / "models"
        models_dir.mkdir(exist_ok=True)
        
        # Generate Prophet model
        prophet_content = self.create_prophet_model_template(asset, category)
        prophet_file = models_dir / f"{asset.lower()}_prophet_model.py"
        with open(prophet_file, 'w') as f:
            f.write(prophet_content)
        logger.info(f"Created Prophet model: {prophet_file}")
        
        # Generate XGBoost model
        xgboost_content = self.create_xgboost_model_template(asset, category)
        xgboost_file = models_dir / f"{asset.lower()}_xgboost_model.py"
        with open(xgboost_file, 'w') as f:
            f.write(xgboost_content)
        logger.info(f"Created XGBoost model: {xgboost_file}")
        
        # Generate Ensemble model
        ensemble_content = self.create_ensemble_model_template(asset, category)
        ensemble_file = models_dir / f"{asset.lower()}_ensemble_model.py"
        with open(ensemble_file, 'w') as f:
            f.write(ensemble_content)
        logger.info(f"Created Ensemble model: {ensemble_file}")
        
        # Create __init__.py
        init_file = models_dir / "__init__.py"
        with open(init_file, 'w') as f:
            f.write(f'"""Models for {asset}"""\n')
        
        # Make files executable
        os.chmod(prophet_file, 0o755)
        os.chmod(xgboost_file, 0o755)
        os.chmod(ensemble_file, 0o755)
    
    def generate_all_models(self):
        """Generate models for all assets"""
        logger.info("Generating individual asset models...")
        
        # Generate crypto asset models
        for asset in self.crypto_assets:
            logger.info(f"Generating models for {asset} (crypto)")
            self.generate_asset_models(asset, "crypto")
        
        # Generate forex asset models
        for asset in self.forex_assets:
            logger.info(f"Generating models for {asset} (forex)")
            self.generate_asset_models(asset, "forex")
        
        logger.info("✅ All individual asset models generated successfully!")

def main():
    """Main execution"""
    base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models"
    generator = AssetModelGenerator(base_path)
    generator.generate_all_models()

if __name__ == "__main__":
    main()
