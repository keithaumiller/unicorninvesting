"""
Enhanced ETH XGBoost Model with Economic Indicators Integration

This enhanced model integrates bronze layer economic indicators into ETH price prediction,
combining traditional technical analysis with macroeconomic factors.

Features:
- Economic indicators integration (growth, consumer, trade, monetary policy)
- Advanced feature engineering combining crypto and macro data
- Multi-timeframe economic factor analysis
- Enhanced prediction accuracy through fundamental analysis
"""

import os
import sys
import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import warnings
import sqlite3
import json
from datetime import datetime, timedelta
from pathlib import Path

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
alpha_models_dir = os.path.dirname(os.path.dirname(current_dir))
sys.path.append(alpha_models_dir)

# Import our economic indicators integration module
sys.path.append(os.path.join(alpha_models_dir, 'shared'))
from economic_indicators_integration import integrate_economic_indicators_into_eth_model, EconomicIndicatorsIntegrator

from models.model_management.model_storage_manager import ModelStorageManager

try:
    import xgboost as xgb
    from sklearn.model_selection import train_test_split, TimeSeriesSplit
    from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
    from sklearn.preprocessing import StandardScaler
    from sklearn.feature_selection import SelectKBest, f_regression
    XGBOOST_AVAILABLE = True
except ImportError:
    print("Warning: XGBoost or sklearn not available. Install with: pip install xgboost scikit-learn")
    XGBOOST_AVAILABLE = False

warnings.filterwarnings('ignore')

class ETHXGBoostWithEconomicIndicators:
    """
    Enhanced ETH XGBoost framework with economic indicators integration.
    
    This model combines traditional crypto technical analysis with macroeconomic indicators
    to improve prediction accuracy by incorporating fundamental economic factors.
    
    Features:
    - Bronze layer economic indicators integration
    - Advanced feature engineering combining technical and fundamental analysis
    - Dynamic feature selection based on economic importance
    - Multi-model variants (technical-only, economic-enhanced, ensemble)
    - Comprehensive performance tracking and comparison
    """
    
    def __init__(self, enable_economic_indicators: bool = True):
        """
        Initialize the enhanced ETH XGBoost model.
        
        Args:
            enable_economic_indicators: Whether to integrate economic indicators
        """
        self.storage_manager = ModelStorageManager()
        self.economic_integrator = EconomicIndicatorsIntegrator() if enable_economic_indicators else None
        self.enable_economic_indicators = enable_economic_indicators
        self.db_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_xgboost_economic_enhanced.db"
        self._init_performance_db()
        
    def _init_performance_db(self):
        """Initialize enhanced performance tracking database."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS enhanced_model_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    model_id TEXT NOT NULL,
                    model_variant TEXT NOT NULL,
                    methodology TEXT DEFAULT 'xgboost_economic_enhanced',
                    asset TEXT DEFAULT 'ETH',
                    mae REAL,
                    mse REAL,
                    rmse REAL,
                    mape REAL,
                    r2_score REAL,
                    training_samples INTEGER,
                    test_samples INTEGER,
                    technical_features INTEGER,
                    economic_features INTEGER,
                    total_features INTEGER,
                    economic_feature_importance REAL,
                    training_time REAL,
                    created_at TEXT NOT NULL,
                    model_config TEXT,
                    feature_importance TEXT,
                    economic_summary TEXT
                )
            """)

    def _create_technical_features(self, data: pd.DataFrame, target_col: str = 'price') -> pd.DataFrame:
        """
        Create traditional technical analysis features for crypto data.
        
        Args:
            data: Input dataframe with datetime index and price column
            target_col: Name of the target column
            
        Returns:
            DataFrame with technical features
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
        df['price_lag_30'] = df[target_col].shift(30)
        
        # Moving averages
        df['ma_7'] = df[target_col].rolling(window=7).mean()
        df['ma_14'] = df[target_col].rolling(window=14).mean()
        df['ma_30'] = df[target_col].rolling(window=30).mean()
        df['ma_60'] = df[target_col].rolling(window=60, min_periods=30).mean()
        
        # Technical indicators
        df['rsi_14'] = self._calculate_rsi(df[target_col], window=14)
        df['rsi_7'] = self._calculate_rsi(df[target_col], window=7)
        df['price_change_1d'] = df[target_col].pct_change(1)
        df['price_change_7d'] = df[target_col].pct_change(7)
        df['price_change_30d'] = df[target_col].pct_change(30)
        
        # Volatility measures
        df['volatility_7d'] = df['price_change_1d'].rolling(window=7).std()
        df['volatility_30d'] = df['price_change_1d'].rolling(window=30, min_periods=7).std()
        df['volatility_ratio'] = df['volatility_7d'] / (df['volatility_30d'] + 1e-8)
        
        # Time-based features
        df['hour'] = df.index.hour
        df['day_of_week'] = df.index.dayofweek
        df['day_of_month'] = df.index.day
        df['month'] = df.index.month
        df['quarter'] = df.index.quarter
        df['is_weekend'] = df.index.dayofweek.isin([5, 6]).astype(int)
        
        # Trend features
        df['price_trend_7d'] = df[target_col] / df['ma_7'] - 1
        df['price_trend_30d'] = df[target_col] / df['ma_30'] - 1
        df['ma_trend'] = df['ma_7'] / df['ma_30'] - 1
        df['momentum_14d'] = df[target_col] / df[target_col].shift(14) - 1
        
        # Bollinger Bands
        df['bb_upper'], df['bb_lower'] = self._calculate_bollinger_bands(df[target_col])
        df['bb_position'] = (df[target_col] - df['bb_lower']) / (df['bb_upper'] - df['bb_lower'])
        df['bb_squeeze'] = (df['bb_upper'] - df['bb_lower']) / df['ma_14']
        
        # Volume-based features (if available)
        if 'volume' in df.columns:
            df['volume_lag_1'] = df['volume'].shift(1)
            df['volume_ma_7'] = df['volume'].rolling(window=7).mean()
            df['volume_ratio'] = df['volume'] / df['volume_ma_7']
            df['price_volume_ratio'] = df[target_col] / (df['volume'] + 1e-8)
            df['volume_trend'] = df['volume'].pct_change(7)
        
        return df

    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / (loss + 1e-8)
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def _calculate_bollinger_bands(self, prices: pd.Series, window: int = 14, num_std: int = 2) -> Tuple[pd.Series, pd.Series]:
        """Calculate Bollinger Bands."""
        rolling_mean = prices.rolling(window=window).mean()
        rolling_std = prices.rolling(window=window).std()
        upper_band = rolling_mean + (rolling_std * num_std)
        lower_band = rolling_mean - (rolling_std * num_std)
        return upper_band, lower_band

    def create_enhanced_features(self, df: pd.DataFrame, target_col: str = 'price', 
                               n_economic_features: int = 25) -> Tuple[pd.DataFrame, Dict[str, Any]]:
        """
        Create enhanced features combining technical analysis and economic indicators.
        
        Args:
            df: Input cryptocurrency data
            target_col: Target column name
            n_economic_features: Number of economic features per category to include
            
        Returns:
            Tuple of (enhanced_dataframe, integration_summary)
        """
        print("🚀 Creating enhanced features with economic indicators...")
        
        # Create technical features
        df_technical = self._create_technical_features(df, target_col)
        
        if not self.enable_economic_indicators:
            return df_technical, {"economic_indicators": "disabled"}
        
        # Integrate economic indicators
        try:
            enhanced_df, economic_summary = integrate_economic_indicators_into_eth_model(
                df_technical, n_features_per_category=n_economic_features
            )
            
            print(f"✅ Enhanced features created: {enhanced_df.shape[1]} total features")
            print(f"   📊 Technical features: {df_technical.shape[1]}")
            print(f"   🏦 Economic features: {economic_summary.get('total_economic_features', 0)}")
            
            return enhanced_df, economic_summary
            
        except Exception as e:
            print(f"⚠️  Warning: Could not integrate economic indicators: {e}")
            return df_technical, {"error": str(e)}

    def _prepare_data_for_training(self, df: pd.DataFrame, target_col: str = 'price') -> Tuple[pd.DataFrame, pd.Series, Dict[str, Any]]:
        """
        Prepare enhanced data for XGBoost training.
        
        Args:
            df: Enhanced DataFrame with technical and economic features
            target_col: Target column name
            
        Returns:
            Tuple of (features_df, target_series, feature_metadata)
        """
        # Identify feature types
        technical_features = []
        economic_features = []
        
        for col in df.columns:
            if col == target_col:
                continue
            elif any(category in col for category in ['economic_growth', 'consumer_business', 'international_trade', 'monetary_policy']):
                economic_features.append(col)
            elif df[col].dtype in ['int64', 'float64']:
                technical_features.append(col)
        
        all_features = technical_features + economic_features
        
        # Remove rows with NaN values
        df_clean = df.dropna()
        
        if len(df_clean) == 0:
            raise ValueError("No data remaining after removing NaN values")
        
        X = df_clean[all_features]
        y = df_clean[target_col]
        
        feature_metadata = {
            'technical_features': technical_features,
            'economic_features': economic_features,
            'technical_count': len(technical_features),
            'economic_count': len(economic_features),
            'total_features': len(all_features),
            'samples': len(df_clean)
        }
        
        return X, y, feature_metadata

    def create_economic_enhanced_model(self, df: pd.DataFrame, target_col: str = 'price',
                                     n_economic_features: int = 20) -> Dict[str, Any]:
        """
        Create XGBoost model enhanced with economic indicators.
        
        Args:
            df: Training data
            target_col: Target column name
            n_economic_features: Number of economic features per category
            
        Returns:
            Dictionary containing model and metadata
        """
        print("🏦 Creating Economic-Enhanced XGBoost Model...")
        
        start_time = datetime.now()
        
        # Create enhanced features
        enhanced_df, economic_summary = self.create_enhanced_features(df, target_col, n_economic_features)
        
        # Prepare data
        X, y, feature_metadata = self._prepare_data_for_training(enhanced_df, target_col)
        
        # Split data chronologically (preserve time series nature)
        split_idx = int(len(X) * 0.8)
        X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
        y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
        
        # Enhanced XGBoost parameters optimized for economic indicators
        model_params = {
            'objective': 'reg:squarederror',
            'max_depth': 8,  # Increased for complex economic relationships
            'learning_rate': 0.08,  # Slightly reduced for stability
            'n_estimators': 200,  # Increased for better economic pattern learning
            'subsample': 0.85,
            'colsample_bytree': 0.85,  # Higher to utilize economic features
            'reg_alpha': 0.01,  # L1 regularization for feature selection
            'reg_lambda': 0.1,  # L2 regularization
            'random_state': 42,
            'early_stopping_rounds': 20,
            'eval_metric': 'rmse'
        }
        
        # Train model with validation
        model = xgb.XGBRegressor(**model_params)
        
        eval_set = [(X_train, y_train), (X_test, y_test)]
        model.fit(X_train, y_train, eval_set=eval_set, verbose=False)
        
        # Make predictions
        y_pred_train = model.predict(X_train)
        y_pred_test = model.predict(X_test)
        
        # Calculate performance metrics
        train_mae = mean_absolute_error(y_train, y_pred_train)
        test_mae = mean_absolute_error(y_test, y_pred_test)
        train_mse = mean_squared_error(y_train, y_pred_train)
        test_mse = mean_squared_error(y_test, y_pred_test)
        train_r2 = r2_score(y_train, y_pred_train)
        test_r2 = r2_score(y_test, y_pred_test)
        
        # Calculate MAPE
        train_mape = np.mean(np.abs((y_train - y_pred_train) / y_train)) * 100
        test_mape = np.mean(np.abs((y_test - y_pred_test) / y_test)) * 100
        
        # Feature importance analysis
        feature_importance = dict(zip(X.columns, model.feature_importances_))
        
        # Calculate economic vs technical importance
        economic_importance = sum(importance for feature, importance in feature_importance.items() 
                                if any(cat in feature for cat in ['economic_growth', 'consumer_business', 'international_trade', 'monetary_policy']))
        technical_importance = sum(importance for feature, importance in feature_importance.items() 
                                 if not any(cat in feature for cat in ['economic_growth', 'consumer_business', 'international_trade', 'monetary_policy']))
        
        economic_importance_ratio = economic_importance / (economic_importance + technical_importance + 1e-8)
        
        training_time = (datetime.now() - start_time).total_seconds()
        
        # Generate model ID
        model_id = f"eth_xgb_economic_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Store model
        model_metadata = {
            'model_id': model_id,
            'model_variant': 'economic_enhanced',
            'methodology': 'xgboost_economic_enhanced',
            'asset': 'ETH',
            'parameters': model_params,
            'feature_metadata': feature_metadata,
            'economic_summary': economic_summary,
            'feature_importance': feature_importance,
            'economic_importance_ratio': economic_importance_ratio,
            'created_at': datetime.now().isoformat(),
            'training_time': training_time
        }
        
        self.storage_manager.store_xgboost_model(model, model_metadata)
        
        # Store performance metrics
        performance_data = {
            'model_id': model_id,
            'model_variant': 'economic_enhanced',
            'methodology': 'xgboost_economic_enhanced',
            'asset': 'ETH',
            'mae': test_mae,
            'mse': test_mse,
            'rmse': np.sqrt(test_mse),
            'mape': test_mape,
            'r2_score': test_r2,
            'training_samples': len(X_train),
            'test_samples': len(X_test),
            'technical_features': feature_metadata['technical_count'],
            'economic_features': feature_metadata['economic_count'],
            'total_features': feature_metadata['total_features'],
            'economic_feature_importance': economic_importance_ratio,
            'training_time': training_time,
            'created_at': datetime.now().isoformat(),
            'model_config': json.dumps(model_params),
            'feature_importance': json.dumps(feature_importance),
            'economic_summary': json.dumps(economic_summary)
        }
        
        self._store_performance_metrics(performance_data)
        
        print(f"✅ Economic-Enhanced Model Training Complete!")
        print(f"   🎯 Test R² Score: {test_r2:.4f}")
        print(f"   📊 Test MAE: {test_mae:.2f}")
        print(f"   🏦 Economic Features Importance: {economic_importance_ratio:.1%}")
        print(f"   ⏱️  Training Time: {training_time:.1f}s")
        
        return {
            'model': model,
            'model_id': model_id,
            'performance': {
                'train_r2': train_r2,
                'test_r2': test_r2,
                'train_mae': train_mae,
                'test_mae': test_mae,
                'train_mse': train_mse,
                'test_mse': test_mse,
                'train_mape': train_mape,
                'test_mape': test_mape,
                'economic_importance_ratio': economic_importance_ratio
            },
            'metadata': model_metadata,
            'feature_importance': feature_importance,
            'economic_summary': economic_summary
        }

    def _store_performance_metrics(self, performance_data: Dict[str, Any]):
        """Store performance metrics in database."""
        with sqlite3.connect(self.db_path) as conn:
            placeholders = ', '.join(['?' for _ in performance_data])
            columns = ', '.join(performance_data.keys())
            sql = f"INSERT INTO enhanced_model_performance ({columns}) VALUES ({placeholders})"
            conn.execute(sql, list(performance_data.values()))

    def compare_models(self) -> pd.DataFrame:
        """
        Compare performance of different model variants.
        
        Returns:
            DataFrame with model comparison results
        """
        with sqlite3.connect(self.db_path) as conn:
            df = pd.read_sql_query("""
                SELECT model_variant, methodology, r2_score, mae, mse, rmse, mape,
                       economic_feature_importance, total_features, economic_features,
                       technical_features, created_at
                FROM enhanced_model_performance
                ORDER BY r2_score DESC
            """, conn)
        
        return df

    def get_top_economic_features(self, model_id: str = None, top_n: int = 20) -> pd.DataFrame:
        """
        Get the most important economic features from a trained model.
        
        Args:
            model_id: Specific model ID, or None for latest
            top_n: Number of top features to return
            
        Returns:
            DataFrame with top economic features and their importance
        """
        with sqlite3.connect(self.db_path) as conn:
            if model_id:
                query = "SELECT feature_importance FROM enhanced_model_performance WHERE model_id = ?"
                result = conn.execute(query, (model_id,)).fetchone()
            else:
                query = "SELECT feature_importance FROM enhanced_model_performance ORDER BY created_at DESC LIMIT 1"
                result = conn.execute(query).fetchone()
            
            if not result:
                return pd.DataFrame()
            
            feature_importance = json.loads(result[0])
            
            # Filter economic features
            economic_features = {k: v for k, v in feature_importance.items() 
                               if any(cat in k for cat in ['economic_growth', 'consumer_business', 'international_trade', 'monetary_policy'])}
            
            # Convert to DataFrame and sort
            df = pd.DataFrame(list(economic_features.items()), columns=['Feature', 'Importance'])
            df = df.sort_values('Importance', ascending=False).head(top_n)
            df['Importance_Percentage'] = (df['Importance'] / df['Importance'].sum() * 100).round(2)
            
            return df
