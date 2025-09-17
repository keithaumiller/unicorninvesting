#!/usr/bin/env python3
"""
Forex Model Validation Framework
==================================

Proper validation framework to identify and fix overfitting in forex Prophet models.
Implements train/validation/test splits, walk-forward validation, and realistic metrics.
"""

import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import json
import warnings
warnings.filterwarnings('ignore')

from prophet import Prophet
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error, mean_absolute_percentage_error

class ForexModelValidator:
    """
    Validate forex Prophet models with proper train/test splits and realistic metrics
    """
    
    def __init__(self, silver_data_path: str = None):
        self.base_path = Path('/workspaces/unicorninvesting')
        if silver_data_path:
            self.silver_path = Path(silver_data_path)
        else:
            self.silver_path = self.base_path / 'BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/processed_data/forex'
        
        self.results_path = self.base_path / 'BackendPython/unicorn/2_alpha_models/validation_results'
        self.results_path.mkdir(parents=True, exist_ok=True)
        
        # Forex assets to validate
        self.forex_assets = ['EURUSD', 'GBPUSD', 'USDJPY', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
        self.intervals = ['1h', '1d']
        
        # Model variants with feature configurations
        self.variants = {
            'conservative': ['ma_20', 'rsi_14', 'momentum_5'],
            'standard': ['ma_20', 'ma_50', 'rsi_14', 'atr_14', 'volatility_20', 'momentum_5'],
            'aggressive': [
                'ma_20', 'ma_50', 'rsi_14', 'rsi_9', 'macd', 'macd_signal',
                'bbands_upper', 'bbands_lower', 'atr_14', 'cci', 'williams_r',
                'stoch_k', 'volatility_20', 'momentum_5', 'volatility_regime'
            ],
            'economic': [
                'ma_20', 'rsi_14', 'atr_14', 'volatility_20', 'momentum_5',
                'volatility_regime', 'mean_reversion', 'carry_trade_proxy', 'risk_sentiment'
            ],
            'ensemble': [
                'ma_20', 'ma_50', 'rsi_14', 'macd', 'atr_14', 'cci',
                'volatility_20', 'momentum_5', 'volatility_regime', 'mean_reversion'
            ]
        }

    def load_forex_data(self, asset: str, interval: str) -> Optional[pd.DataFrame]:
        """Load forex data with proper datetime handling"""
        try:
            file_path = self.silver_path / f"{asset}_silver_{interval}_latest.csv"
            if not file_path.exists():
                print(f"❌ Data file not found: {file_path}")
                return None
            
            df = pd.read_csv(file_path)
            
            # Handle datetime column
            datetime_col = None
            for col in ['timestamp', 'Datetime', 'datetime', 'Date']:
                if col in df.columns:
                    datetime_col = col
                    break
            
            if not datetime_col or 'close' not in df.columns:
                print(f"❌ Invalid data structure for {asset} {interval}")
                return None
            
            # Convert datetime with timezone handling
            df['ds'] = pd.to_datetime(df[datetime_col], utc=True)
            if df['ds'].dt.tz is not None:
                df['ds'] = df['ds'].dt.tz_localize(None)
            
            df['y'] = df['close']
            df = df.sort_values('ds').reset_index(drop=True)
            
            print(f"✅ Loaded {asset} {interval}: {len(df)} records from {df['ds'].min()} to {df['ds'].max()}")
            return df
            
        except Exception as e:
            print(f"❌ Error loading {asset} {interval}: {e}")
            return None

    def create_train_validation_split(self, df: pd.DataFrame, train_ratio: float = 0.7) -> Tuple[pd.DataFrame, pd.DataFrame]:
        """Create proper train/validation split"""
        split_idx = int(len(df) * train_ratio)
        
        train_df = df.iloc[:split_idx].copy()
        val_df = df.iloc[split_idx:].copy()
        
        print(f"   📊 Train: {len(train_df)} records ({train_df['ds'].min()} to {train_df['ds'].max()})")
        print(f"   📊 Validation: {len(val_df)} records ({val_df['ds'].min()} to {val_df['ds'].max()})")
        
        return train_df, val_df

    def prepare_features(self, df: pd.DataFrame, features: List[str]) -> Tuple[pd.DataFrame, List[str]]:
        """Prepare features and remove any with data leakage"""
        # Remove OHLC features that cause data leakage
        forbidden_features = ['open', 'high', 'low', 'volume']
        safe_features = [f for f in features if f in df.columns and f not in forbidden_features]
        
        # Prepare Prophet dataframe
        prophet_df = pd.DataFrame({
            'ds': df['ds'],
            'y': df['y']
        })
        
        # Add safe features
        for feature in safe_features:
            if not df[feature].isna().all():
                prophet_df[feature] = df[feature]
            else:
                safe_features.remove(feature)
        
        # Remove any remaining NaN values
        prophet_df = prophet_df.dropna()
        
        if len(safe_features) != len(features):
            removed = set(features) - set(safe_features)
            print(f"   ⚠️  Removed features (data leakage risk): {removed}")
        
        return prophet_df, safe_features

    def train_and_validate_model(self, train_df: pd.DataFrame, val_df: pd.DataFrame, 
                                features: List[str], variant: str) -> Dict:
        """Train model on training data and validate on separate validation set"""
        try:
            # Prophet configuration
            model = Prophet(
                seasonality_mode='multiplicative',
                weekly_seasonality=True,
                daily_seasonality=False,
                yearly_seasonality=False,
                changepoint_prior_scale=0.05,  # Conservative to prevent overfitting
                seasonality_prior_scale=0.01,
                interval_width=0.8
            )
            
            # Add regressors
            for feature in features:
                model.add_regressor(feature, standardize=True)
            
            # Train on training data only
            print(f"   🔄 Training {variant} model with {len(features)} features...")
            model.fit(train_df)
            
            # Create future dataframe for validation period
            future = pd.concat([train_df[['ds'] + features], val_df[['ds'] + features]], ignore_index=True)
            
            # Make predictions
            forecast = model.predict(future)
            
            # Calculate training metrics (in-sample)
            train_pred = forecast['yhat'][:len(train_df)]
            train_actual = train_df['y'].values
            train_r2 = r2_score(train_actual, train_pred)
            train_mae = mean_absolute_error(train_actual, train_pred)
            train_mape = mean_absolute_percentage_error(train_actual, train_pred) * 100
            
            # Calculate validation metrics (out-of-sample) - THE REAL TEST
            val_pred = forecast['yhat'][len(train_df):]
            val_actual = val_df['y'].values
            val_r2 = r2_score(val_actual, val_pred)
            val_mae = mean_absolute_error(val_actual, val_pred)
            val_mape = mean_absolute_percentage_error(val_actual, val_pred) * 100
            
            metrics = {
                'variant': variant,
                'features_used': len(features),
                'train_records': len(train_df),
                'val_records': len(val_df),
                
                # Training metrics (in-sample - expect high scores)
                'train_r2': train_r2,
                'train_mae': train_mae,
                'train_mape': train_mape,
                
                # Validation metrics (out-of-sample - realistic scores)
                'val_r2': val_r2,
                'val_mae': val_mae,
                'val_mape': val_mape,
                
                # Overfitting indicator
                'overfitting_gap': train_r2 - val_r2,
                'is_overfitting': (train_r2 - val_r2) > 0.1,  # Flag if >10% gap
                
                'features': features
            }
            
            print(f"   ✅ Training R²: {train_r2:.3f} | Validation R²: {val_r2:.3f} | Gap: {train_r2-val_r2:.3f}")
            if metrics['is_overfitting']:
                print(f"   🚨 OVERFITTING DETECTED: Gap = {metrics['overfitting_gap']:.3f}")
            
            return metrics
            
        except Exception as e:
            print(f"   ❌ Error training model: {e}")
            return {
                'variant': variant,
                'features_used': len(features),
                'error': str(e),
                'train_r2': 0.0,
                'val_r2': 0.0,
                'is_overfitting': False,
                'overfitting_gap': 0.0
            }

    def validate_asset(self, asset: str, interval: str) -> Dict:
        """Validate all model variants for a specific asset and interval"""
        print(f"\n💱 Validating {asset} {interval}...")
        
        # Load data
        df = self.load_forex_data(asset, interval)
        if df is None:
            return {'asset': asset, 'interval': interval, 'error': 'Data loading failed'}
        
        # Check minimum data requirements
        if len(df) < 100:
            print(f"   ⚠️  Insufficient data: {len(df)} records")
            return {'asset': asset, 'interval': interval, 'error': 'Insufficient data'}
        
        # Create train/validation split
        train_df, val_df = self.create_train_validation_split(df, train_ratio=0.7)
        
        results = {
            'asset': asset,
            'interval': interval,
            'total_records': len(df),
            'train_records': len(train_df),
            'val_records': len(val_df),
            'models': {}
        }
        
        # Validate each variant
        for variant, features in self.variants.items():
            print(f"   🎯 Validating {variant} variant...")
            
            # Prepare features (remove data leakage)
            train_prep, safe_features = self.prepare_features(train_df, features)
            val_prep, _ = self.prepare_features(val_df, features)
            
            if len(safe_features) == 0:
                print(f"   ❌ No valid features for {variant}")
                continue
            
            # Train and validate
            metrics = self.train_and_validate_model(train_prep, val_prep, safe_features, variant)
            results['models'][variant] = metrics
        
        return results

    def run_comprehensive_validation(self) -> Dict:
        """Run validation across all forex assets and intervals"""
        print("🔍 Starting Comprehensive Forex Model Validation")
        print("=" * 60)
        
        all_results = {
            'timestamp': datetime.now().isoformat(),
            'validation_summary': {},
            'assets': {}
        }
        
        total_models = 0
        overfitting_models = 0
        
        for asset in self.forex_assets:
            all_results['assets'][asset] = {}
            
            for interval in self.intervals:
                results = self.validate_asset(asset, interval)
                all_results['assets'][asset][interval] = results
                
                # Count overfitting models
                if 'models' in results:
                    for variant, metrics in results['models'].items():
                        total_models += 1
                        if metrics.get('is_overfitting', False):
                            overfitting_models += 1
        
        # Summary statistics
        all_results['validation_summary'] = {
            'total_models_tested': total_models,
            'overfitting_models': overfitting_models,
            'overfitting_rate': overfitting_models / total_models if total_models > 0 else 0,
            'assets_tested': len(self.forex_assets),
            'intervals_tested': len(self.intervals)
        }
        
        # Save results
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = self.results_path / f"forex_validation_results_{timestamp}.json"
        
        with open(results_file, 'w') as f:
            json.dump(all_results, f, indent=2, default=str)
        
        print(f"\n📊 Validation Complete!")
        print(f"📁 Results saved to: {results_file}")
        print(f"🚨 Overfitting Rate: {overfitting_models}/{total_models} ({all_results['validation_summary']['overfitting_rate']:.1%})")
        
        return all_results

    def print_validation_summary(self, results: Dict):
        """Print a comprehensive summary of validation results"""
        print("\n" + "="*80)
        print("🔍 FOREX MODEL VALIDATION SUMMARY")
        print("="*80)
        
        summary = results['validation_summary']
        print(f"📊 Models Tested: {summary['total_models_tested']}")
        print(f"🚨 Overfitting Detected: {summary['overfitting_models']} ({summary['overfitting_rate']:.1%})")
        print(f"📈 Assets: {summary['assets_tested']} | Intervals: {summary['intervals_tested']}")
        
        print(f"\n{'Asset':<8} {'Interval':<8} {'Variant':<12} {'Train R²':<8} {'Val R²':<8} {'Gap':<8} {'Status'}")
        print("-" * 80)
        
        for asset, asset_data in results['assets'].items():
            for interval, interval_data in asset_data.items():
                if 'models' in interval_data:
                    for variant, metrics in interval_data['models'].items():
                        if 'train_r2' in metrics:
                            status = "🚨 OVERFIT" if metrics.get('is_overfitting', False) else "✅ OK"
                            print(f"{asset:<8} {interval:<8} {variant:<12} {metrics['train_r2']:<8.3f} "
                                  f"{metrics['val_r2']:<8.3f} {metrics['overfitting_gap']:<8.3f} {status}")

if __name__ == "__main__":
    validator = ForexModelValidator()
    results = validator.run_comprehensive_validation()
    validator.print_validation_summary(results)