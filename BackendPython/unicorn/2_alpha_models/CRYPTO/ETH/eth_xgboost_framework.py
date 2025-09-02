"""
ETH XGBoost Model Framework with Organized Storage

Enhanced XGBoost framework with scalable model storage, version control, and easy retrieval.
Integrates with ModelStorageManager for clean organization following the same pattern as Prophet.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import warnings
import sqlite3
import json
from datetime import datetime, timedelta
from pathlib import Path
from models.model_management.model_storage_manager import ModelStorageManager

try:
    import xgboost as xgb
    from sklearn.model_selection import train_test_split, TimeSeriesSplit
    from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
    from sklearn.preprocessing import StandardScaler
    XGBOOST_AVAILABLE = True
except ImportError:
    print("Warning: XGBoost or sklearn not available. Install with: pip install xgboost scikit-learn")
    XGBOOST_AVAILABLE = False

warnings.filterwarnings('ignore')

class ETHXGBoostFrameworkWithStorage:
    """
    Enhanced ETH XGBoost framework with organized model storage.
    
    Features:
    - Three distinct XGBoost model variants (standard, tuned, ensemble)
    - Organized model storage with version control
    - Comprehensive performance tracking
    - Easy model retrieval and comparison
    - Feature engineering for time series
    """
    
    def __init__(self):
        self.storage_manager = ModelStorageManager()
        self.db_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_xgboost_comparison.db"
        self._init_performance_db()
        
    def _init_performance_db(self):
        """Initialize performance tracking database."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    model_id TEXT NOT NULL,
                    model_variant TEXT NOT NULL,
                    methodology TEXT DEFAULT 'xgboost',
                    asset TEXT DEFAULT 'ETH',
                    mae REAL,
                    mse REAL,
                    rmse REAL,
                    mape REAL,
                    r2_score REAL,
                    training_samples INTEGER,
                    test_samples INTEGER,
                    feature_count INTEGER,
                    training_time REAL,
                    created_at TEXT NOT NULL,
                    model_config TEXT,
                    feature_importance TEXT
                )
            """)

    def _create_features(self, data: pd.DataFrame, target_col: str = 'price') -> pd.DataFrame:
        """
        Create comprehensive features for XGBoost time series prediction.
        
        Args:
            data: Input dataframe with datetime index and price column
            target_col: Name of the target column
            
        Returns:
            DataFrame with engineered features
        """
        df = data.copy()
        
        # Ensure datetime index
        if not isinstance(df.index, pd.DatetimeIndex):
            if 'date' in df.columns:
                df['date'] = pd.to_datetime(df['date'])
                df = df.set_index('date')
            else:
                df.index = pd.to_datetime(df.index)
        
        # Price-based features
        df['price_lag_1'] = df[target_col].shift(1)
        df['price_lag_3'] = df[target_col].shift(3)
        df['price_lag_7'] = df[target_col].shift(7)
        df['price_lag_14'] = df[target_col].shift(14)
        
        # Moving averages
        df['ma_7'] = df[target_col].rolling(window=7).mean()
        df['ma_14'] = df[target_col].rolling(window=14).mean()
        df['ma_30'] = df[target_col].rolling(window=30).mean()
        
        # Technical indicators
        df['rsi'] = self._calculate_rsi(df[target_col], window=14)
        df['price_change'] = df[target_col].pct_change()
        df['volatility'] = df['price_change'].rolling(window=7).std()
        
        # Time-based features
        df['hour'] = df.index.hour
        df['day_of_week'] = df.index.dayofweek
        df['day_of_month'] = df.index.day
        df['month'] = df.index.month
        df['quarter'] = df.index.quarter
        
        # Trend features
        df['price_trend_7'] = df[target_col] / df['ma_7'] - 1
        df['price_trend_14'] = df[target_col] / df['ma_14'] - 1
        df['ma_trend'] = df['ma_7'] / df['ma_14'] - 1
        
        # Volume-based features (if available)
        if 'volume' in df.columns:
            df['volume_lag_1'] = df['volume'].shift(1)
            df['volume_ma_7'] = df['volume'].rolling(window=7).mean()
            df['price_volume_ratio'] = df[target_col] / (df['volume'] + 1e-8)
        
        return df

    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi

    def _prepare_data_for_xgboost(self, df: pd.DataFrame, target_col: str = 'price') -> Tuple[pd.DataFrame, pd.Series]:
        """
        Prepare data for XGBoost training.
        
        Args:
            df: DataFrame with features
            target_col: Target column name
            
        Returns:
            Tuple of (features_df, target_series)
        """
        # Create features
        df_features = self._create_features(df, target_col)
        
        # Select feature columns (exclude target and any non-numeric columns)
        feature_cols = [col for col in df_features.columns 
                       if col != target_col and df_features[col].dtype in ['int64', 'float64']]
        
        # Remove rows with NaN values
        df_clean = df_features.dropna()
        
        X = df_clean[feature_cols]
        y = df_clean[target_col]
        
        return X, y

    def create_standard_xgboost_model(self, df: pd.DataFrame, target_col: str = 'price') -> Dict[str, Any]:
        """
        Create standard XGBoost model with basic parameters.
        
        Args:
            df: Training data
            target_col: Target column name
            
        Returns:
            Dictionary containing model and metadata
        """
        print("Creating Standard XGBoost Model...")
        
        X, y = self._prepare_data_for_xgboost(df, target_col)
        
        # Split data chronologically
        split_idx = int(len(X) * 0.8)
        X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
        y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
        
        # Standard XGBoost parameters
        model_params = {
            'objective': 'reg:squarederror',
            'max_depth': 6,
            'learning_rate': 0.1,
            'n_estimators': 100,
            'subsample': 0.8,
            'colsample_bytree': 0.8,
            'random_state': 42
        }
        
        # Train model
        start_time = datetime.now()
        model = xgb.XGBRegressor(**model_params)
        model.fit(X_train, y_train)
        training_time = (datetime.now() - start_time).total_seconds()
        
        # Make predictions
        y_pred = model.predict(X_test)
        
        # Calculate metrics
        mae = mean_absolute_error(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        rmse = np.sqrt(mse)
        mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
        r2 = r2_score(y_test, y_pred)
        
        # Feature importance
        feature_importance = dict(zip(X.columns, model.feature_importances_))
        # Convert numpy types to Python types for JSON serialization
        feature_importance = {k: float(v) for k, v in feature_importance.items()}
        
        return {
            'model': model,
            'variant': 'standard',
            'config': model_params,
            'performance': {
                'mae': float(mae),
                'mse': float(mse),
                'rmse': float(rmse),
                'mape': float(mape),
                'r2_score': float(r2),
                'training_samples': len(X_train),
                'test_samples': len(X_test),
                'feature_count': len(X.columns),
                'training_time': training_time
            },
            'feature_importance': feature_importance,
            'features': list(X.columns)
        }

    def create_tuned_xgboost_model(self, df: pd.DataFrame, target_col: str = 'price') -> Dict[str, Any]:
        """
        Create tuned XGBoost model with optimized hyperparameters.
        
        Args:
            df: Training data
            target_col: Target column name
            
        Returns:
            Dictionary containing model and metadata
        """
        print("Creating Tuned XGBoost Model...")
        
        X, y = self._prepare_data_for_xgboost(df, target_col)
        
        # Split data chronologically
        split_idx = int(len(X) * 0.8)
        X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
        y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
        
        # Tuned XGBoost parameters (optimized for time series)
        model_params = {
            'objective': 'reg:squarederror',
            'max_depth': 8,
            'learning_rate': 0.05,
            'n_estimators': 200,
            'subsample': 0.9,
            'colsample_bytree': 0.9,
            'min_child_weight': 3,
            'gamma': 0.1,
            'reg_alpha': 0.1,
            'reg_lambda': 1.0,
            'random_state': 42
        }
        
        # Train model
        start_time = datetime.now()
        model = xgb.XGBRegressor(**model_params)
        model.fit(X_train, y_train, 
                 eval_set=[(X_test, y_test)], 
                 verbose=False)
        training_time = (datetime.now() - start_time).total_seconds()
        
        # Make predictions
        y_pred = model.predict(X_test)
        
        # Calculate metrics
        mae = mean_absolute_error(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        rmse = np.sqrt(mse)
        mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
        r2 = r2_score(y_test, y_pred)
        
        # Feature importance
        feature_importance = dict(zip(X.columns, model.feature_importances_))
        # Convert numpy types to Python types for JSON serialization
        feature_importance = {k: float(v) for k, v in feature_importance.items()}
        
        return {
            'model': model,
            'variant': 'tuned',
            'config': model_params,
            'performance': {
                'mae': float(mae),
                'mse': float(mse),
                'rmse': float(rmse),
                'mape': float(mape),
                'r2_score': float(r2),
                'training_samples': len(X_train),
                'test_samples': len(X_test),
                'feature_count': len(X.columns),
                'training_time': training_time
            },
            'feature_importance': feature_importance,
            'features': list(X.columns)
        }

    def create_ensemble_xgboost_model(self, df: pd.DataFrame, target_col: str = 'price') -> Dict[str, Any]:
        """
        Create ensemble XGBoost model with multiple estimators.
        
        Args:
            df: Training data
            target_col: Target column name
            
        Returns:
            Dictionary containing model and metadata
        """
        print("Creating Ensemble XGBoost Model...")
        
        X, y = self._prepare_data_for_xgboost(df, target_col)
        
        # Split data chronologically
        split_idx = int(len(X) * 0.8)
        X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
        y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
        
        # Ensemble parameters - multiple models with different configurations
        ensemble_configs = [
            {
                'max_depth': 6,
                'learning_rate': 0.1,
                'n_estimators': 150,
                'subsample': 0.8,
                'colsample_bytree': 0.8,
                'gamma': 0.0
            },
            {
                'max_depth': 8,
                'learning_rate': 0.05,
                'n_estimators': 200,
                'subsample': 0.9,
                'colsample_bytree': 0.9,
                'gamma': 0.1
            },
            {
                'max_depth': 4,
                'learning_rate': 0.15,
                'n_estimators': 100,
                'subsample': 0.85,
                'colsample_bytree': 0.85,
                'gamma': 0.05
            }
        ]
        
        # Train ensemble models
        start_time = datetime.now()
        models = []
        
        for i, config in enumerate(ensemble_configs):
            base_params = {
                'objective': 'reg:squarederror',
                'random_state': 42 + i,
                **config
            }
            model = xgb.XGBRegressor(**base_params)
            model.fit(X_train, y_train)
            models.append(model)
        
        training_time = (datetime.now() - start_time).total_seconds()
        
        # Make ensemble predictions (average)
        predictions = []
        for model in models:
            pred = model.predict(X_test)
            predictions.append(pred)
        
        y_pred = np.mean(predictions, axis=0)
        
        # Calculate metrics
        mae = mean_absolute_error(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        rmse = np.sqrt(mse)
        mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
        r2 = r2_score(y_test, y_pred)
        
        # Average feature importance across ensemble
        feature_importance = {}
        for col in X.columns:
            importance_values = [model.feature_importances_[list(X.columns).index(col)] for model in models]
            feature_importance[col] = float(np.mean(importance_values))
        
        # Create ensemble wrapper
        ensemble_model = {
            'models': models,
            'ensemble_configs': ensemble_configs,
            'prediction_method': 'average'
        }
        
        return {
            'model': ensemble_model,
            'variant': 'ensemble',
            'config': {
                'ensemble_size': len(models),
                'prediction_method': 'average',
                'individual_configs': ensemble_configs
            },
            'performance': {
                'mae': float(mae),
                'mse': float(mse),
                'rmse': float(rmse),
                'mape': float(mape),
                'r2_score': float(r2),
                'training_samples': len(X_train),
                'test_samples': len(X_test),
                'feature_count': len(X.columns),
                'training_time': training_time
            },
            'feature_importance': feature_importance,
            'features': list(X.columns)
        }

    def train_and_store_model(self, 
                             df: pd.DataFrame, 
                             variant: str = 'standard',
                             target_col: str = 'price',
                             description: str = "",
                             tags: List[str] = None) -> str:
        """
        Train and store XGBoost model with specified variant.
        
        Args:
            df: Training data
            variant: Model variant ('standard', 'tuned', 'ensemble')
            target_col: Target column name
            description: Model description
            tags: List of tags
            
        Returns:
            Model ID of stored model
        """
        if not XGBOOST_AVAILABLE:
            raise ImportError("XGBoost not available. Please install: pip install xgboost scikit-learn")
        
        if tags is None:
            tags = ['xgboost', 'eth', 'time_series', variant]
        
        # Train model based on variant
        if variant == 'standard':
            result = self.create_standard_xgboost_model(df, target_col)
        elif variant == 'tuned':
            result = self.create_tuned_xgboost_model(df, target_col)
        elif variant == 'ensemble':
            result = self.create_ensemble_xgboost_model(df, target_col)
        else:
            raise ValueError(f"Unknown variant: {variant}. Use 'standard', 'tuned', or 'ensemble'")
        
        # Store model
        model_id = self.storage_manager.store_model(
            model=result['model'],
            methodology='xgboost',
            asset='ETH',
            model_config=result['config'],
            performance_metrics=result['performance'],
            description=description or f"ETH XGBoost {variant} model",
            variant=variant,
            tags=tags
        )
        
        # Store performance in database
        self._store_performance_metrics(model_id, result, variant)
        
        print(f"✅ XGBoost {variant} model stored with ID: {model_id}")
        print(f"   MAPE: {result['performance']['mape']:.2f}%")
        print(f"   R²: {result['performance']['r2_score']:.4f}")
        print(f"   Features: {result['performance']['feature_count']}")
        
        return model_id

    def _store_performance_metrics(self, model_id: str, result: Dict[str, Any], variant: str):
        """Store performance metrics in database."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                INSERT INTO model_performance (
                    model_id, model_variant, methodology, asset,
                    mae, mse, rmse, mape, r2_score,
                    training_samples, test_samples, feature_count, training_time,
                    created_at, model_config, feature_importance
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                model_id, variant, 'xgboost', 'ETH',
                result['performance']['mae'],
                result['performance']['mse'],
                result['performance']['rmse'],
                result['performance']['mape'],
                result['performance']['r2_score'],
                result['performance']['training_samples'],
                result['performance']['test_samples'],
                result['performance']['feature_count'],
                result['performance']['training_time'],
                datetime.now().isoformat(),
                json.dumps(result['config']),
                json.dumps(result['feature_importance'])
            ))

    def get_model_performance_comparison(self) -> pd.DataFrame:
        """Get performance comparison of all XGBoost models."""
        with sqlite3.connect(self.db_path) as conn:
            df = pd.read_sql_query("""
                SELECT model_id, model_variant, mae, mse, rmse, mape, r2_score,
                       training_samples, test_samples, feature_count, training_time,
                       created_at
                FROM model_performance
                ORDER BY mape ASC
            """, conn)
        return df

    def generate_sample_data(self, days: int = 100) -> pd.DataFrame:
        """
        Generate sample ETH price data for testing.
        
        Args:
            days: Number of days of data to generate
            
        Returns:
            DataFrame with sample ETH price data
        """
        dates = pd.date_range(start=datetime.now() - timedelta(days=days), 
                             end=datetime.now(), freq='H')
        
        # Generate realistic ETH price movement
        np.random.seed(42)
        base_price = 2000
        
        # Generate price with more controlled volatility
        prices = []
        current_price = base_price
        
        for i in range(len(dates)):
            # Generate small random changes (0.5% max change per hour)
            change_pct = np.random.normal(0, 0.005)  # 0.5% volatility
            # Add slight upward bias
            if np.random.random() < 0.51:
                change_pct += 0.0001  # Small upward bias
            
            # Apply price change with bounds
            new_price = current_price * (1 + change_pct)
            # Keep price within reasonable bounds
            new_price = max(500, min(10000, new_price))
            current_price = new_price
            prices.append(current_price)
        
        # Generate volume data with reasonable scale
        volumes = np.random.lognormal(10, 0.3, len(dates))  # More reasonable volume
        
        df = pd.DataFrame({
            'date': dates,
            'price': prices,
            'volume': volumes
        })
        
        df = df.set_index('date')
        
        # Ensure no NaN or infinity values
        df = df.replace([np.inf, -np.inf], np.nan).ffill().bfill()
        
        return df

# Demo function
def demo_xgboost_framework():
    """Demonstrate XGBoost framework capabilities."""
    print("🚀 ETH XGBoost Framework Demo")
    print("=" * 50)
    
    if not XGBOOST_AVAILABLE:
        print("❌ XGBoost not available. Please install: pip install xgboost scikit-learn")
        return
    
    # Initialize framework
    framework = ETHXGBoostFrameworkWithStorage()
    
    # Generate sample data
    print("📊 Generating sample ETH data...")
    data = framework.generate_sample_data(days=60)
    print(f"   Generated {len(data)} hours of data")
    print(f"   Price range: ${data['price'].min():.2f} - ${data['price'].max():.2f}")
    
    # Train different model variants
    variants = ['standard', 'tuned', 'ensemble']
    model_ids = []
    
    for variant in variants:
        print(f"\n🔧 Training {variant} XGBoost model...")
        model_id = framework.train_and_store_model(
            df=data,
            variant=variant,
            description=f"Demo ETH XGBoost {variant} model with synthetic data",
            tags=['demo', 'synthetic', 'xgboost', variant]
        )
        model_ids.append(model_id)
    
    # Show performance comparison
    print("\n📈 Performance Comparison:")
    performance_df = framework.get_model_performance_comparison()
    print(performance_df[['model_variant', 'mape', 'r2_score', 'feature_count', 'training_time']].to_string(index=False))
    
    # Show storage organization
    print(f"\n📁 Storage Organization:")
    storage_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/model_storage/xgboost")
    if storage_path.exists():
        xgboost_files = list(storage_path.glob("*.pkl"))
        for file in sorted(xgboost_files):
            size_mb = file.stat().st_size / (1024 * 1024)
            print(f"   📄 {file.name} ({size_mb:.2f} MB)")
    
    print("\n✅ XGBoost framework demo completed!")
    print(f"   Models stored: {len(model_ids)}")
    print(f"   Storage path: {storage_path}")
    
    return model_ids

if __name__ == "__main__":
    demo_xgboost_framework()
