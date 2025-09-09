"""
Enhanced BTC XGBoost Model with Economic Indicators Integration

This enhanced model integrates bronze layer economic indicators into BTC price prediction,
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
from economic_indicators_integration import EconomicIndicatorsIntegrator

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

class BTCXGBoostWithEconomicIndicators:
    """
    Enhanced BTC XGBoost framework with economic indicators integration.
    
    This model combines traditional crypto technical analysis with macroeconomic indicators
    to improve BTC prediction accuracy by incorporating fundamental economic factors.
    
    Features:
    - Bronze layer economic indicators integration
    - Advanced feature engineering combining technical and fundamental analysis
    - Dynamic feature selection based on economic importance
    - Multi-model variants (technical-only, economic-enhanced, ensemble)
    - Comprehensive performance tracking and comparison
    """
    
    def __init__(self, enable_economic_indicators: bool = True):
        """
        Initialize the enhanced BTC XGBoost model.
        
        Args:
            enable_economic_indicators: Whether to integrate economic indicators
        """
        self.economic_integrator = EconomicIndicatorsIntegrator() if enable_economic_indicators else None
        self.enable_economic_indicators = enable_economic_indicators
        self.db_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/BTC/btc_xgboost_economic_enhanced.db"
        self._init_performance_db()
        
    def _init_performance_db(self):
        """Initialize enhanced performance tracking database."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS btc_enhanced_model_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    model_id TEXT NOT NULL,
                    model_variant TEXT NOT NULL,
                    methodology TEXT DEFAULT 'xgboost_economic_enhanced',
                    asset TEXT DEFAULT 'BTC',
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
        Create BTC-specific technical analysis features.
        
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
        
        # Price-based features (BTC-specific parameters)
        df['price_lag_1'] = df[target_col].shift(1)
        df['price_lag_3'] = df[target_col].shift(3)
        df['price_lag_7'] = df[target_col].shift(7)
        df['price_lag_14'] = df[target_col].shift(14)
        df['price_lag_30'] = df[target_col].shift(30)
        df['price_lag_90'] = df[target_col].shift(90)  # BTC longer cycles
        
        # Moving averages (BTC-tuned timeframes)
        df['ma_7'] = df[target_col].rolling(window=7).mean()
        df['ma_14'] = df[target_col].rolling(window=14).mean()
        df['ma_30'] = df[target_col].rolling(window=30).mean()
        df['ma_60'] = df[target_col].rolling(window=60, min_periods=30).mean()
        df['ma_120'] = df[target_col].rolling(window=120, min_periods=60).mean()  # BTC longer trends
        df['ma_200'] = df[target_col].rolling(window=200, min_periods=100).mean()  # BTC key resistance
        
        # Technical indicators
        df['rsi_14'] = self._calculate_rsi(df[target_col], window=14)
        df['rsi_30'] = self._calculate_rsi(df[target_col], window=30)  # BTC longer RSI
        df['price_change_1d'] = df[target_col].pct_change(1)
        df['price_change_7d'] = df[target_col].pct_change(7)
        df['price_change_30d'] = df[target_col].pct_change(30)
        df['price_change_90d'] = df[target_col].pct_change(90)  # BTC quarterly cycles
        
        # Volatility measures (BTC-specific)
        df['volatility_7d'] = df['price_change_1d'].rolling(window=7).std()
        df['volatility_30d'] = df['price_change_1d'].rolling(window=30, min_periods=7).std()
        df['volatility_90d'] = df['price_change_1d'].rolling(window=90, min_periods=30).std()
        df['volatility_ratio'] = df['volatility_7d'] / (df['volatility_30d'] + 1e-8)
        df['volatility_trend'] = df['volatility_30d'] / (df['volatility_90d'] + 1e-8)
        
        # Time-based features
        df['hour'] = df.index.hour
        df['day_of_week'] = df.index.dayofweek
        df['day_of_month'] = df.index.day
        df['month'] = df.index.month
        df['quarter'] = df.index.quarter
        df['is_weekend'] = df.index.dayofweek.isin([5, 6]).astype(int)
        df['is_month_end'] = (df.index.day > 25).astype(int)  # BTC month-end patterns
        
        # Trend features (BTC-specific)
        df['price_trend_7d'] = df[target_col] / df['ma_7'] - 1
        df['price_trend_30d'] = df[target_col] / df['ma_30'] - 1
        df['price_trend_90d'] = df[target_col] / df['ma_120'] - 1
        df['ma_trend_short'] = df['ma_7'] / df['ma_30'] - 1
        df['ma_trend_long'] = df['ma_30'] / df['ma_120'] - 1
        df['momentum_14d'] = df[target_col] / df[target_col].shift(14) - 1
        df['momentum_30d'] = df[target_col] / df[target_col].shift(30) - 1
        
        # Bollinger Bands (BTC-tuned)
        df['bb_upper_20'], df['bb_lower_20'] = self._calculate_bollinger_bands(df[target_col], window=20)
        df['bb_upper_50'], df['bb_lower_50'] = self._calculate_bollinger_bands(df[target_col], window=50)
        df['bb_position_20'] = (df[target_col] - df['bb_lower_20']) / (df['bb_upper_20'] - df['bb_lower_20'])
        df['bb_position_50'] = (df[target_col] - df['bb_lower_50']) / (df['bb_upper_50'] - df['bb_lower_50'])
        df['bb_squeeze_20'] = (df['bb_upper_20'] - df['bb_lower_20']) / df['ma_20'] if 'ma_20' in df.columns else (df['bb_upper_20'] - df['bb_lower_20']) / df['ma_14']
        
        # BTC-specific cycle indicators
        df['halving_cycle'] = ((df.index - pd.Timestamp('2020-05-11')).days / 365.25 / 4) % 1  # BTC halving cycle
        df['year_progress'] = df.index.dayofyear / 365.25  # Seasonal patterns
        
        # Volume-based features (if available)
        if 'volume' in df.columns:
            df['volume_lag_1'] = df['volume'].shift(1)
            df['volume_ma_7'] = df['volume'].rolling(window=7).mean()
            df['volume_ma_30'] = df['volume'].rolling(window=30).mean()
            df['volume_ratio_7'] = df['volume'] / df['volume_ma_7']
            df['volume_ratio_30'] = df['volume'] / df['volume_ma_30']
            df['price_volume_ratio'] = df[target_col] / (df['volume'] + 1e-8)
            df['volume_trend_7d'] = df['volume'].pct_change(7)
            df['volume_trend_30d'] = df['volume'].pct_change(30)
        
        return df

    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / (loss + 1e-8)
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def _calculate_bollinger_bands(self, prices: pd.Series, window: int = 20, num_std: int = 2) -> Tuple[pd.Series, pd.Series]:
        """Calculate Bollinger Bands."""
        rolling_mean = prices.rolling(window=window).mean()
        rolling_std = prices.rolling(window=window).std()
        upper_band = rolling_mean + (rolling_std * num_std)
        lower_band = rolling_mean - (rolling_std * num_std)
        return upper_band, lower_band

    def create_enhanced_features(self, df: pd.DataFrame, target_col: str = 'price', 
                               n_economic_features: int = 25) -> Tuple[pd.DataFrame, Dict[str, Any]]:
        """
        Create enhanced features combining BTC technical analysis and economic indicators.
        
        Args:
            df: Input Bitcoin data
            target_col: Target column name
            n_economic_features: Number of economic features per category to include
            
        Returns:
            Tuple of (enhanced_dataframe, integration_summary)
        """
        print("🚀 Creating BTC enhanced features with economic indicators...")
        
        # Create BTC technical features
        df_technical = self._create_technical_features(df, target_col)
        
        if not self.enable_economic_indicators:
            return df_technical, {"economic_indicators": "disabled"}
        
        # Integrate economic indicators using our existing integrator
        try:
            integrator = self.economic_integrator
            
            # Load indicators
            indicators = integrator.load_latest_indicators('1_day')
            
            # Select key features
            selected_features = integrator.select_key_features(indicators, n_economic_features)
            
            # Align with BTC data
            enhanced_df = integrator.align_with_crypto_data(df_technical, indicators, selected_features)
            
            # Create summary
            economic_summary = integrator.create_economic_features_summary(enhanced_df)
            
            print(f"✅ BTC enhanced features created: {enhanced_df.shape[1]} total features")
            print(f"   📊 Technical features: {df_technical.shape[1]}")
            print(f"   🏦 Economic features: {economic_summary.get('total_economic_features', 0)}")
            
            return enhanced_df, economic_summary
            
        except Exception as e:
            print(f"⚠️  Warning: Could not integrate economic indicators: {e}")
            return df_technical, {"error": str(e)}

    def create_economic_enhanced_model(self, df: pd.DataFrame, target_col: str = 'price',
                                     n_economic_features: int = 20, model_variant: str = 'standard') -> Dict[str, Any]:
        """
        Create BTC XGBoost model enhanced with economic indicators.
        
        Args:
            df: Training data
            target_col: Target column name
            n_economic_features: Number of economic features per category
            model_variant: Model variant name for tracking
            
        Returns:
            Dictionary containing model and metadata
        """
        print(f"🟠 Creating BTC Economic-Enhanced XGBoost Model ({model_variant})...")
        
        start_time = datetime.now()
        
        # Create enhanced features
        enhanced_df, economic_summary = self.create_enhanced_features(df, target_col, n_economic_features)
        
        # Prepare data
        X, y, feature_metadata = self._prepare_data_for_training(enhanced_df, target_col)
        
        # Split data chronologically
        split_idx = int(len(X) * 0.8)
        X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
        y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
        
        # Get BTC-optimized parameters based on variant
        model_params = self._get_model_parameters(model_variant)
        
        # Train model
        model = xgb.XGBRegressor(**model_params)
        
        eval_set = [(X_train, y_train), (X_test, y_test)]
        model.fit(X_train, y_train, eval_set=eval_set, verbose=False)
        
        # Calculate metrics
        y_pred_test = model.predict(X_test)
        test_mae = mean_absolute_error(y_test, y_pred_test)
        test_mse = mean_squared_error(y_test, y_pred_test)
        test_r2 = r2_score(y_test, y_pred_test)
        test_mape = np.mean(np.abs((y_test - y_pred_test) / y_test)) * 100
        
        # Feature importance analysis
        feature_importance = dict(zip(X.columns, model.feature_importances_))
        
        # Calculate economic importance
        economic_importance = sum(importance for feature, importance in feature_importance.items() 
                                if any(cat in feature for cat in ['economic_growth', 'consumer_business', 'international_trade', 'monetary_policy']))
        total_importance = sum(feature_importance.values())
        economic_importance_ratio = economic_importance / (total_importance + 1e-8)
        
        training_time = (datetime.now() - start_time).total_seconds()
        model_id = f"btc_xgb_economic_{model_variant}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Store performance
        performance_data = {
            'model_id': model_id,
            'model_variant': model_variant,
            'methodology': 'xgboost_economic_enhanced',
            'asset': 'BTC',
            'mae': test_mae,
            'mse': test_mse,
            'rmse': np.sqrt(test_mse),
            'mape': test_mape,
            'r2_score': test_r2,
            'training_samples': len(X_train),
            'test_samples': len(X_test),
            'technical_features': feature_metadata.get('technical_count', 0),
            'economic_features': feature_metadata.get('economic_count', 0),
            'total_features': feature_metadata.get('total_features', 0),
            'economic_feature_importance': economic_importance_ratio,
            'training_time': training_time,
            'created_at': datetime.now().isoformat(),
            'model_config': json.dumps(model_params),
            'feature_importance': json.dumps(feature_importance),
            'economic_summary': json.dumps(economic_summary)
        }
        
        self._store_performance_metrics(performance_data)
        
        print(f"✅ BTC Economic-Enhanced Model ({model_variant}) Training Complete!")
        print(f"   🎯 Test R² Score: {test_r2:.4f}")
        print(f"   📊 Test MAE: ${test_mae:.2f}")
        print(f"   🏦 Economic Features Importance: {economic_importance_ratio:.1%}")
        print(f"   ⏱️  Training Time: {training_time:.1f}s")
        
        return {
            'model': model,
            'model_id': model_id,
            'performance': {
                'test_r2': test_r2,
                'test_mae': test_mae,
                'test_mse': test_mse,
                'test_mape': test_mape,
                'economic_importance_ratio': economic_importance_ratio
            },
            'feature_importance': feature_importance,
            'economic_summary': economic_summary
        }
    
    def _get_model_parameters(self, variant: str) -> Dict[str, Any]:
        """Get BTC-optimized XGBoost parameters for different variants."""
        base_params = {
            'objective': 'reg:squarederror',
            'random_state': 42,
            'early_stopping_rounds': 20,
            'eval_metric': 'rmse'
        }
        
        variant_params = {
            'conservative': {
                'max_depth': 6,
                'learning_rate': 0.05,
                'n_estimators': 150,
                'subsample': 0.8,
                'colsample_bytree': 0.8,
                'reg_alpha': 0.1,
                'reg_lambda': 0.1
            },
            'standard': {
                'max_depth': 8,
                'learning_rate': 0.08,
                'n_estimators': 200,
                'subsample': 0.85,
                'colsample_bytree': 0.85,
                'reg_alpha': 0.01,
                'reg_lambda': 0.1
            },
            'aggressive': {
                'max_depth': 10,
                'learning_rate': 0.1,
                'n_estimators': 300,
                'subsample': 0.9,
                'colsample_bytree': 0.9,
                'reg_alpha': 0.005,
                'reg_lambda': 0.05
            },
            'deep': {
                'max_depth': 12,
                'learning_rate': 0.06,
                'n_estimators': 400,
                'subsample': 0.85,
                'colsample_bytree': 0.8,
                'reg_alpha': 0.01,
                'reg_lambda': 0.15
            },
            'ensemble_ready': {
                'max_depth': 9,
                'learning_rate': 0.07,
                'n_estimators': 250,
                'subsample': 0.88,
                'colsample_bytree': 0.88,
                'reg_alpha': 0.008,
                'reg_lambda': 0.08
            }
        }
        
        return {**base_params, **variant_params.get(variant, variant_params['standard'])}
    
    def _prepare_data_for_training(self, df: pd.DataFrame, target_col: str = 'price') -> Tuple[pd.DataFrame, pd.Series, Dict[str, Any]]:
        """Prepare enhanced data for training."""
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
        
        # Handle NaN values more robustly
        df_clean = df.fillna(method='ffill').fillna(method='bfill').fillna(0)
        
        if len(df_clean) == 0:
            raise ValueError("No data remaining after cleaning")
        
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

    def _store_performance_metrics(self, performance_data: Dict[str, Any]):
        """Store performance metrics in database."""
        with sqlite3.connect(self.db_path) as conn:
            placeholders = ', '.join(['?' for _ in performance_data])
            columns = ', '.join(performance_data.keys())
            sql = f"INSERT INTO btc_enhanced_model_performance ({columns}) VALUES ({placeholders})"
            conn.execute(sql, list(performance_data.values()))

    def compare_models(self) -> pd.DataFrame:
        """Compare BTC model performance."""
        with sqlite3.connect(self.db_path) as conn:
            df = pd.read_sql_query("""
                SELECT model_variant, methodology, r2_score, mae, mse, rmse, mape,
                       economic_feature_importance, total_features, economic_features,
                       technical_features, created_at
                FROM btc_enhanced_model_performance
                ORDER BY r2_score DESC
            """, conn)
        return df
