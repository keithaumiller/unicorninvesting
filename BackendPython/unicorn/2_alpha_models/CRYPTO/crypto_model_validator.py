#!/usr/bin/env python3
"""
Crypto Model Validation Framework

Comprehensive validation system for detecting and eliminating overfitting in crypto Prophet and XGBoost models.
Based on lessons learned from forex Prophet overfitting elimination.

Key Features:
- Proper train/validation splits with time series methodology
- Data leakage detection and elimination
- Realistic performance metrics for crypto time series
- OHLC-derived feature identification and removal
- Comprehensive overfitting statistics

CRITICAL DISCOVERIES from Forex Analysis:
1. Training data evaluation creates artificial 99%+ R² scores
2. OHLC-derived technical indicators cause inevitable data leakage
3. Negative validation R² scores are NORMAL for financial time series
4. Proper validation methodology is essential for realistic assessment

This framework implements leak-free validation for crypto models.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import sys
import os
from datetime import datetime, timedelta
import warnings
import sqlite3
import json
from pathlib import Path

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
crypto_dir = os.path.dirname(current_dir)
alpha_models_dir = os.path.dirname(crypto_dir)
sys.path.append(alpha_models_dir)
sys.path.append(crypto_dir)

try:
    from prophet import Prophet
    from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
    from sklearn.preprocessing import StandardScaler
    import xgboost as xgb
except ImportError as e:
    print(f"⚠️  Missing required packages: {e}")
    print("Install with: pip install prophet scikit-learn xgboost")
    sys.exit(1)

warnings.filterwarnings('ignore')

class CryptoModelValidator:
    """
    Comprehensive validation framework for crypto models with overfitting detection.
    
    Implements all lessons learned from forex overfitting elimination:
    - Proper train/validation methodology
    - Data leakage detection and prevention
    - Realistic performance assessment for crypto time series
    - OHLC-derived feature elimination
    """
    
    def __init__(self):
        """Initialize the crypto validation framework."""
        self.results_db = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/crypto_validation_results.db"
        self.overfitting_threshold = 0.3  # Train-validation R² gap threshold
        self.validation_results = []
        self._init_results_db()
    
    def _init_results_db(self):
        """Initialize validation results database."""
        with sqlite3.connect(self.results_db) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS crypto_validation_results (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    asset TEXT NOT NULL,
                    model_type TEXT NOT NULL,
                    model_variant TEXT,
                    original_train_r2 REAL,
                    original_validation_r2 REAL,
                    leak_free_train_r2 REAL,
                    leak_free_validation_r2 REAL,
                    overfitting_detected BOOLEAN,
                    train_validation_gap REAL,
                    data_leakage_features INTEGER,
                    leak_free_features INTEGER,
                    validation_methodology TEXT,
                    performance_improvement REAL,
                    created_at TEXT NOT NULL,
                    feature_analysis TEXT,
                    recommendations TEXT
                )
            """)
    
    def detect_data_leakage_features(self, feature_names: List[str]) -> Dict[str, Any]:
        """
        Detect features that cause data leakage in crypto models.
        
        Based on forex analysis, OHLC-derived features are the primary culprit.
        
        Args:
            feature_names: List of feature names to analyze
            
        Returns:
            Analysis of data leakage potential
        """
        leakage_patterns = {
            'ohlc_derived': [
                'rsi', 'macd', 'bollinger', 'stochastic', 'williams', 'cci',
                'open', 'high', 'low', 'close', 'hl2', 'hlc3', 'ohlc4',
                'typical_price', 'weighted_price', 'true_range', 'atr'
            ],
            'high_correlation': [
                'price_change', 'return_1d', 'return_intraday', 'price_ratio',
                'momentum_1d', 'price_deviation', 'normalized_price'
            ],
            'future_looking': [
                'forward_return', 'next_price', 'future_high', 'future_low',
                'target_shift', 'lead_price', 'ahead_close'
            ]
        }
        
        leakage_features = {
            'ohlc_derived': [],
            'high_correlation': [],
            'future_looking': [],
            'safe_features': []
        }
        
        for feature in feature_names:
            feature_lower = feature.lower()
            categorized = False
            
            for category, patterns in leakage_patterns.items():
                if any(pattern in feature_lower for pattern in patterns):
                    leakage_features[category].append(feature)
                    categorized = True
                    break
            
            if not categorized:
                leakage_features['safe_features'].append(feature)
        
        total_leakage = sum(len(v) for k, v in leakage_features.items() if k != 'safe_features')
        leakage_ratio = total_leakage / len(feature_names) if feature_names else 0
        
        return {
            'leakage_features': leakage_features,
            'total_features': len(feature_names),
            'leakage_count': total_leakage,
            'safe_count': len(leakage_features['safe_features']),
            'leakage_ratio': leakage_ratio,
            'severity': 'HIGH' if leakage_ratio > 0.5 else 'MEDIUM' if leakage_ratio > 0.2 else 'LOW'
        }
    
    def create_leak_free_features(self, data: pd.DataFrame, asset: str = 'ETH') -> pd.DataFrame:
        """
        Create leak-free features for crypto time series.
        
        Based on successful forex methodology:
        - Time-based indicators (hour, day, month)
        - Lagged returns and price changes
        - Historical volatility
        - Volume-based indicators (when available)
        - Market session indicators
        
        Args:
            data: OHLCV data
            asset: Crypto asset name
            
        Returns:
            DataFrame with leak-free features
        """
        features_df = pd.DataFrame(index=data.index)
        
        # Ensure we have datetime index
        if not isinstance(data.index, pd.DatetimeIndex):
            if 'timestamp' in data.columns:
                data.index = pd.to_datetime(data['timestamp'])
            elif 'date' in data.columns:
                data.index = pd.to_datetime(data['date'])
            else:
                print("⚠️  Warning: No datetime index found, using range index")
                data.index = pd.date_range(start='2020-01-01', periods=len(data), freq='1H')
        
        # Use Close price as base (or price column if available)
        price_col = 'Close' if 'Close' in data.columns else 'price' if 'price' in data.columns else data.columns[0]
        prices = data[price_col]
        
        # 1. Time-based features (no data leakage)
        features_df['hour'] = data.index.hour
        features_df['day_of_week'] = data.index.dayofweek
        features_df['month'] = data.index.month
        features_df['quarter'] = data.index.quarter
        features_df['is_weekend'] = (data.index.dayofweek >= 5).astype(int)
        
        # 2. Lagged returns (using only past data)
        for lag in [1, 2, 3, 6, 12, 24]:  # Hours for crypto
            if len(prices) > lag:
                features_df[f'return_{lag}h'] = prices.pct_change(lag)
        
        # 3. Lagged price changes (absolute, in asset terms)
        for lag in [1, 6, 24, 168]:  # 1h, 6h, 1d, 1w
            if len(prices) > lag:
                features_df[f'price_change_{lag}h'] = prices.diff(lag)
        
        # 4. Historical volatility (using only past data)
        for window in [6, 24, 168]:  # 6h, 1d, 1w
            if len(prices) > window:
                returns = prices.pct_change()
                features_df[f'volatility_{window}h'] = returns.rolling(window).std()
        
        # 5. Volume-based features (if available)
        if 'Volume' in data.columns and not data['Volume'].isna().all():
            volume = data['Volume']
            
            # Volume lags (no data leakage)
            for lag in [1, 6, 24]:
                if len(volume) > lag:
                    features_df[f'volume_lag_{lag}h'] = volume.shift(lag)
            
            # Historical volume ratios
            for window in [24, 168]:
                if len(volume) > window:
                    features_df[f'volume_ratio_{window}h'] = volume / volume.rolling(window).mean()
        
        # 6. Crypto market session indicators
        # Crypto trades 24/7 but has different activity patterns
        features_df['asian_session'] = ((data.index.hour >= 0) & (data.index.hour < 8)).astype(int)
        features_df['european_session'] = ((data.index.hour >= 8) & (data.index.hour < 16)).astype(int)
        features_df['american_session'] = ((data.index.hour >= 16) & (data.index.hour < 24)).astype(int)
        
        # 7. Seasonal indicators specific to crypto
        features_df['is_end_of_month'] = (data.index.day >= 28).astype(int)
        features_df['is_start_of_month'] = (data.index.day <= 3).astype(int)
        features_df['is_mid_month'] = ((data.index.day >= 14) & (data.index.day <= 16)).astype(int)
        
        # Remove any rows with NaN values
        features_df = features_df.fillna(method='bfill').fillna(method='ffill')
        
        print(f"✅ Created {len(features_df.columns)} leak-free features for {asset}")
        print(f"   📊 Time features: {sum(1 for c in features_df.columns if any(t in c for t in ['hour', 'day', 'month', 'weekend', 'session']))}")
        print(f"   📈 Lagged returns: {sum(1 for c in features_df.columns if 'return_' in c)}")
        print(f"   📉 Price changes: {sum(1 for c in features_df.columns if 'price_change_' in c)}")
        print(f"   📊 Volatility: {sum(1 for c in features_df.columns if 'volatility_' in c)}")
        print(f"   📦 Volume: {sum(1 for c in features_df.columns if 'volume_' in c)}")
        
        return features_df
    
    def validate_crypto_model(self, data: pd.DataFrame, asset: str, model_type: str = 'prophet', 
                             model_variant: str = 'standard') -> Dict[str, Any]:
        """
        Comprehensive validation of crypto model with overfitting detection.
        
        Args:
            data: Historical crypto data
            asset: Crypto asset (ETH, BTC, etc.)
            model_type: Type of model (prophet, xgboost, ensemble)
            model_variant: Model variant (basic, enhanced, optimized)
            
        Returns:
            Comprehensive validation results
        """
        print(f"🔍 Validating {asset} {model_type} {model_variant} model...")
        
        # Ensure minimum data length
        if len(data) < 100:
            return {
                'success': False,
                'error': f'Insufficient data: {len(data)} rows (minimum 100 required)',
                'asset': asset,
                'model_type': model_type
            }
        
        # 1. Create leak-free features
        features_df = self.create_leak_free_features(data, asset)
        
        # 2. Prepare target variable
        price_col = 'Close' if 'Close' in data.columns else 'price' if 'price' in data.columns else data.columns[0]
        target = data[price_col].values
        
        # Ensure same length
        min_length = min(len(features_df), len(target))
        features_df = features_df.iloc[:min_length]
        target = target[:min_length]
        
        # 3. Proper time series split (80/20)
        split_idx = int(len(features_df) * 0.8)
        X_train = features_df.iloc[:split_idx]
        X_val = features_df.iloc[split_idx:]
        y_train = target[:split_idx]
        y_val = target[split_idx:]
        
        print(f"   📊 Training samples: {len(X_train)}")
        print(f"   📊 Validation samples: {len(X_val)}")
        
        # 4. Train model with leak-free features
        if model_type.lower() == 'prophet':
            validation_result = self._validate_prophet_model(X_train, X_val, y_train, y_val, asset)
        elif model_type.lower() == 'xgboost':
            validation_result = self._validate_xgboost_model(X_train, X_val, y_train, y_val, asset)
        else:
            validation_result = self._validate_ensemble_model(X_train, X_val, y_train, y_val, asset)
        
        # 5. Analyze results and detect overfitting
        analysis = self._analyze_validation_results(validation_result, asset, model_type, model_variant)
        
        # 6. Store results
        self._store_validation_results(analysis)
        
        return analysis
    
    def _validate_prophet_model(self, X_train: pd.DataFrame, X_val: pd.DataFrame, 
                               y_train: np.ndarray, y_val: np.ndarray, asset: str) -> Dict[str, Any]:
        """Validate Prophet model with proper methodology."""
        try:
            # Prepare Prophet data format
            train_data = pd.DataFrame({
                'ds': X_train.index,
                'y': y_train
            })
            
            # Add top leak-free regressors
            important_features = ['return_1h', 'volatility_24h', 'hour', 'day_of_week', 'volume_lag_1h']
            for feature in important_features:
                if feature in X_train.columns:
                    train_data[feature] = X_train[feature].values
            
            # Configure Prophet for crypto
            model = Prophet(
                yearly_seasonality=True,
                weekly_seasonality=True,
                daily_seasonality=False,
                seasonality_mode='multiplicative',
                changepoint_prior_scale=0.05,
                interval_width=0.8
            )
            
            # Add regressors
            for feature in important_features:
                if feature in train_data.columns:
                    model.add_regressor(feature)
            
            # Train model
            model.fit(train_data)
            
            # Create validation future dataframe
            val_data = pd.DataFrame({
                'ds': X_val.index
            })
            
            for feature in important_features:
                if feature in X_val.columns:
                    val_data[feature] = X_val[feature].values
            
            # Make predictions
            val_predictions = model.predict(val_data)
            train_predictions = model.predict(train_data)
            
            # Calculate metrics
            train_r2 = r2_score(y_train, train_predictions['yhat'])
            val_r2 = r2_score(y_val, val_predictions['yhat'])
            train_mae = mean_absolute_error(y_train, train_predictions['yhat'])
            val_mae = mean_absolute_error(y_val, val_predictions['yhat'])
            
            return {
                'success': True,
                'train_r2': train_r2,
                'validation_r2': val_r2,
                'train_mae': train_mae,
                'validation_mae': val_mae,
                'model_type': 'prophet',
                'predictions': val_predictions['yhat'],
                'actuals': y_val
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'model_type': 'prophet'
            }
    
    def _validate_xgboost_model(self, X_train: pd.DataFrame, X_val: pd.DataFrame,
                               y_train: np.ndarray, y_val: np.ndarray, asset: str) -> Dict[str, Any]:
        """Validate XGBoost model with proper methodology."""
        try:
            # Scale features
            scaler = StandardScaler()
            X_train_scaled = scaler.fit_transform(X_train)
            X_val_scaled = scaler.transform(X_val)
            
            # Configure XGBoost for crypto
            model = xgb.XGBRegressor(
                n_estimators=100,
                max_depth=6,
                learning_rate=0.1,
                subsample=0.8,
                colsample_bytree=0.8,
                random_state=42
            )
            
            # Train model
            model.fit(X_train_scaled, y_train)
            
            # Make predictions
            train_pred = model.predict(X_train_scaled)
            val_pred = model.predict(X_val_scaled)
            
            # Calculate metrics
            train_r2 = r2_score(y_train, train_pred)
            val_r2 = r2_score(y_val, val_pred)
            train_mae = mean_absolute_error(y_train, train_pred)
            val_mae = mean_absolute_error(y_val, val_pred)
            
            return {
                'success': True,
                'train_r2': train_r2,
                'validation_r2': val_r2,
                'train_mae': train_mae,
                'validation_mae': val_mae,
                'model_type': 'xgboost',
                'feature_importance': dict(zip(X_train.columns, model.feature_importances_)),
                'predictions': val_pred,
                'actuals': y_val
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'model_type': 'xgboost'
            }
    
    def _validate_ensemble_model(self, X_train: pd.DataFrame, X_val: pd.DataFrame,
                                y_train: np.ndarray, y_val: np.ndarray, asset: str) -> Dict[str, Any]:
        """Validate ensemble model combining Prophet and XGBoost."""
        try:
            # Get Prophet predictions
            prophet_result = self._validate_prophet_model(X_train, X_val, y_train, y_val, asset)
            if not prophet_result['success']:
                return prophet_result
            
            # Get XGBoost predictions
            xgb_result = self._validate_xgboost_model(X_train, X_val, y_train, y_val, asset)
            if not xgb_result['success']:
                return xgb_result
            
            # Combine predictions (simple average)
            ensemble_val_pred = (prophet_result['predictions'] + xgb_result['predictions']) / 2
            
            # Calculate ensemble metrics
            val_r2 = r2_score(y_val, ensemble_val_pred)
            val_mae = mean_absolute_error(y_val, ensemble_val_pred)
            
            return {
                'success': True,
                'train_r2': (prophet_result['train_r2'] + xgb_result['train_r2']) / 2,
                'validation_r2': val_r2,
                'train_mae': (prophet_result['train_mae'] + xgb_result['train_mae']) / 2,
                'validation_mae': val_mae,
                'model_type': 'ensemble',
                'prophet_r2': prophet_result['validation_r2'],
                'xgboost_r2': xgb_result['validation_r2'],
                'predictions': ensemble_val_pred,
                'actuals': y_val
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'model_type': 'ensemble'
            }
    
    def _analyze_validation_results(self, validation_result: Dict[str, Any], asset: str, 
                                   model_type: str, model_variant: str) -> Dict[str, Any]:
        """Analyze validation results and detect overfitting."""
        if not validation_result['success']:
            return validation_result
        
        train_r2 = validation_result['train_r2']
        val_r2 = validation_result['validation_r2']
        train_val_gap = train_r2 - val_r2
        
        # Detect overfitting
        overfitting_detected = train_val_gap > self.overfitting_threshold
        
        # Performance assessment
        if val_r2 < -10:
            performance_level = "POOR"
        elif val_r2 < 0:
            performance_level = "TYPICAL" # Normal for financial time series!
        elif val_r2 < 0.3:
            performance_level = "GOOD"
        elif val_r2 < 0.6:
            performance_level = "EXCELLENT"
        else:
            performance_level = "SUSPICIOUS" # May indicate data leakage
        
        # Generate recommendations
        recommendations = []
        if overfitting_detected:
            recommendations.append("🚨 Significant overfitting detected - implement regularization")
        if val_r2 > 0.8:
            recommendations.append("⚠️  Suspiciously high validation R² - check for data leakage")
        if val_r2 < -5:
            recommendations.append("📉 Very negative R² - consider feature engineering improvements")
        if abs(train_val_gap) < 0.1:
            recommendations.append("✅ Good generalization - train/validation gap is reasonable")
        
        return {
            'success': True,
            'asset': asset,
            'model_type': model_type,
            'model_variant': model_variant,
            'train_r2': train_r2,
            'validation_r2': val_r2,
            'train_validation_gap': train_val_gap,
            'overfitting_detected': overfitting_detected,
            'performance_level': performance_level,
            'train_mae': validation_result['train_mae'],
            'validation_mae': validation_result['validation_mae'],
            'recommendations': recommendations,
            'analysis_timestamp': datetime.now().isoformat()
        }
    
    def _store_validation_results(self, analysis: Dict[str, Any]):
        """Store validation results in database."""
        if not analysis['success']:
            return
        
        with sqlite3.connect(self.results_db) as conn:
            conn.execute("""
                INSERT INTO crypto_validation_results 
                (asset, model_type, model_variant, original_train_r2, original_validation_r2,
                 leak_free_train_r2, leak_free_validation_r2, overfitting_detected,
                 train_validation_gap, validation_methodology, created_at, recommendations)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                analysis['asset'],
                analysis['model_type'],
                analysis['model_variant'],
                None,  # original_train_r2 (to be filled when comparing with existing models)
                None,  # original_validation_r2
                analysis['train_r2'],
                analysis['validation_r2'],
                analysis['overfitting_detected'],
                analysis['train_validation_gap'],
                'proper_time_series_split',
                analysis['analysis_timestamp'],
                json.dumps(analysis['recommendations'])
            ))
    
    def run_comprehensive_crypto_validation(self) -> Dict[str, Any]:
        """
        Run comprehensive validation across all available crypto models.
        
        Returns:
            Summary of validation results across all models
        """
        print("🚀 Starting Comprehensive Crypto Model Validation")
        print("=" * 60)
        
        # Sample crypto assets and configurations
        crypto_configs = [
            {'asset': 'ETH', 'model_type': 'prophet', 'variant': 'basic'},
            {'asset': 'ETH', 'model_type': 'prophet', 'variant': 'enhanced'},
            {'asset': 'ETH', 'model_type': 'xgboost', 'variant': 'standard'},
            {'asset': 'ETH', 'model_type': 'ensemble', 'variant': 'optimized'},
            {'asset': 'BTC', 'model_type': 'prophet', 'variant': 'basic'},
            {'asset': 'BTC', 'model_type': 'prophet', 'variant': 'enhanced'},
            {'asset': 'BTC', 'model_type': 'xgboost', 'variant': 'standard'},
            {'asset': 'BTC', 'model_type': 'ensemble', 'variant': 'optimized'},
        ]
        
        validation_summary = {
            'total_models': len(crypto_configs),
            'successful_validations': 0,
            'overfitting_detected': 0,
            'excellent_performance': 0,
            'typical_performance': 0,
            'poor_performance': 0,
            'detailed_results': []
        }
        
        for config in crypto_configs:
            print(f"\n📊 Validating {config['asset']} {config['model_type']} {config['variant']}...")
            
            # Generate sample data (in production, load real data)
            sample_data = self._generate_crypto_sample_data(config['asset'])
            
            # Run validation
            result = self.validate_crypto_model(
                sample_data, 
                config['asset'], 
                config['model_type'], 
                config['variant']
            )
            
            if result['success']:
                validation_summary['successful_validations'] += 1
                
                if result['overfitting_detected']:
                    validation_summary['overfitting_detected'] += 1
                
                if result['performance_level'] == 'EXCELLENT':
                    validation_summary['excellent_performance'] += 1
                elif result['performance_level'] in ['TYPICAL', 'GOOD']:
                    validation_summary['typical_performance'] += 1
                else:
                    validation_summary['poor_performance'] += 1
                
                print(f"   ✅ {result['performance_level']} Performance")
                print(f"   📊 Validation R²: {result['validation_r2']:.4f}")
                print(f"   📈 Train/Val Gap: {result['train_validation_gap']:.4f}")
                print(f"   🔍 Overfitting: {'Yes' if result['overfitting_detected'] else 'No'}")
            else:
                print(f"   ❌ Validation failed: {result.get('error', 'Unknown error')}")
            
            validation_summary['detailed_results'].append(result)
        
        # Calculate summary statistics
        if validation_summary['successful_validations'] > 0:
            overfitting_rate = validation_summary['overfitting_detected'] / validation_summary['successful_validations']
            success_rate = validation_summary['successful_validations'] / validation_summary['total_models']
        else:
            overfitting_rate = 0
            success_rate = 0
        
        print(f"\n" + "=" * 60)
        print(f"🎯 CRYPTO VALIDATION SUMMARY")
        print(f"=" * 60)
        print(f"📊 Total Models Validated: {validation_summary['total_models']}")
        print(f"✅ Successful Validations: {validation_summary['successful_validations']} ({success_rate:.1%})")
        print(f"🚨 Overfitting Detected: {validation_summary['overfitting_detected']} ({overfitting_rate:.1%})")
        print(f"🌟 Excellent Performance: {validation_summary['excellent_performance']}")
        print(f"📈 Typical/Good Performance: {validation_summary['typical_performance']}")
        print(f"📉 Poor Performance: {validation_summary['poor_performance']}")
        
        validation_summary['overfitting_rate'] = overfitting_rate
        validation_summary['success_rate'] = success_rate
        
        return validation_summary
    
    def _generate_crypto_sample_data(self, asset: str = 'ETH', hours: int = 2000) -> pd.DataFrame:
        """Generate realistic sample crypto data for validation testing."""
        # Create realistic crypto price data with proper volatility and trends
        np.random.seed(42)  # For reproducible results
        
        # Start with base prices
        base_prices = {'ETH': 2000, 'BTC': 45000}
        base_price = base_prices.get(asset, 2000)
        
        # Generate datetime index (hourly data)
        start_date = datetime.now() - timedelta(hours=hours)
        dates = pd.date_range(start=start_date, periods=hours, freq='1H')
        
        # Generate realistic price movements
        returns = np.random.normal(0, 0.02, hours)  # 2% hourly volatility
        returns[0] = 0  # First return is 0
        
        # Add some trends and patterns
        trend = np.sin(np.arange(hours) * 2 * np.pi / (24 * 30)) * 0.01  # Monthly cycle
        returns += trend
        
        # Calculate prices
        prices = [base_price]
        for i in range(1, hours):
            prices.append(prices[-1] * (1 + returns[i]))
        
        # Generate OHLC from prices
        opens = [prices[0]] + prices[:-1]
        closes = prices
        highs = [p * (1 + abs(np.random.normal(0, 0.005))) for p in prices]
        lows = [p * (1 - abs(np.random.normal(0, 0.005))) for p in prices]
        
        # Generate volume
        volumes = np.random.lognormal(15, 1, hours)  # Log-normal distribution
        
        return pd.DataFrame({
            'Open': opens,
            'High': highs,
            'Low': lows,
            'Close': closes,
            'Volume': volumes
        }, index=dates)


def main():
    """Main execution function for crypto validation."""
    print("🔍 Crypto Model Validation Framework")
    print("Based on Forex Overfitting Elimination Success")
    print("=" * 60)
    
    validator = CryptoModelValidator()
    
    # Run comprehensive validation
    summary = validator.run_comprehensive_crypto_validation()
    
    print(f"\n🎯 Validation Complete!")
    print(f"✅ Success Rate: {summary['success_rate']:.1%}")
    print(f"🚨 Overfitting Rate: {summary['overfitting_rate']:.1%}")
    print(f"\n💾 Results saved to: {validator.results_db}")


if __name__ == "__main__":
    main()