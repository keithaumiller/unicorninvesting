"""
ETH Ensemble Forecast Model Framework

Advanced ensemble framework that combines the best performing models from different methodologies.
Integrates Prophet and XGBoost models with sophisticated weighting strategies.
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
import pickle

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
alpha_models_dir = os.path.dirname(os.path.dirname(current_dir))
sys.path.append(alpha_models_dir)

from models.model_management.model_storage_manager import ModelStorageManager

try:
    from prophet import Prophet
    PROPHET_AVAILABLE = True
except ImportError:
    PROPHET_AVAILABLE = False

try:
    import xgboost as xgb
    from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
    XGBOOST_AVAILABLE = True
except ImportError:
    XGBOOST_AVAILABLE = False

warnings.filterwarnings('ignore')

class ETHEnsembleForecastFramework:
    """
    Advanced ETH ensemble forecast framework combining best models from multiple methodologies.
    
    Features:
    - Dynamic model loading from storage
    - Multiple ensemble strategies (weighted average, performance-based, adaptive)
    - Comprehensive performance tracking
    - Model confidence scoring
    - Automatic best model selection
    """
    
    def __init__(self):
        self.storage_manager = ModelStorageManager()
        self.db_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_ensemble_comparison.db"
        self.loaded_models = {}
        self._init_performance_db()
        
    def _init_performance_db(self):
        """Initialize ensemble performance tracking database."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS ensemble_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    ensemble_id TEXT NOT NULL,
                    strategy TEXT NOT NULL,
                    component_models TEXT NOT NULL,
                    weights TEXT NOT NULL,
                    mae REAL,
                    mse REAL,
                    rmse REAL,
                    mape REAL,
                    r2_score REAL,
                    confidence_score REAL,
                    training_samples INTEGER,
                    test_samples INTEGER,
                    created_at TEXT NOT NULL,
                    model_config TEXT
                )
            """)

    def load_best_models(self) -> Dict[str, Any]:
        """
        Load the best performing models from each methodology.
        
        Returns:
            Dictionary containing loaded models and their metadata
        """
        print("🔍 Loading best models from each methodology...")
        
        models = self.storage_manager.list_models()
        best_models = {}
        
        # Find best Prophet model
        prophet_models = [m for m in models if m.methodology == 'prophet']
        if prophet_models:
            best_prophet_meta = min(prophet_models, key=lambda x: x.performance_metrics.get('mape', float('inf')))
            try:
                prophet_model, prophet_metadata = self.storage_manager.load_model(best_prophet_meta.model_id)
                best_models['prophet'] = {
                    'model': prophet_model,
                    'metadata': best_prophet_meta,
                    'performance': best_prophet_meta.performance_metrics
                }
                print(f"   ✅ Prophet: {best_prophet_meta.model_id} (MAPE: {best_prophet_meta.performance_metrics.get('mape', 'N/A'):.2f}%)")
            except Exception as e:
                print(f"   ❌ Failed to load Prophet model: {e}")
        
        # Find best XGBoost model
        xgb_models = [m for m in models if m.methodology == 'xgboost']
        if xgb_models:
            best_xgb_meta = min(xgb_models, key=lambda x: x.performance_metrics.get('mape', float('inf')))
            try:
                xgb_model, xgb_metadata = self.storage_manager.load_model(best_xgb_meta.model_id)
                best_models['xgboost'] = {
                    'model': xgb_model,
                    'metadata': best_xgb_meta,
                    'performance': best_xgb_meta.performance_metrics
                }
                print(f"   ✅ XGBoost: {best_xgb_meta.model_id} (MAPE: {best_xgb_meta.performance_metrics.get('mape', 'N/A'):.2f}%)")
            except Exception as e:
                print(f"   ❌ Failed to load XGBoost model: {e}")
        
        self.loaded_models = best_models
        return best_models

    def _prepare_prophet_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for Prophet prediction."""
        prophet_data = data.copy()
        if not isinstance(prophet_data.index, pd.DatetimeIndex):
            prophet_data.index = pd.to_datetime(prophet_data.index)
        
        # Prophet expects 'ds' and 'y' columns
        prophet_df = pd.DataFrame({
            'ds': prophet_data.index,
            'y': prophet_data['price'] if 'price' in prophet_data.columns else prophet_data.iloc[:, 0]
        })
        return prophet_df

    def _prepare_xgboost_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Prepare data for XGBoost prediction with feature engineering."""
        df = data.copy()
        
        # Ensure datetime index
        if not isinstance(df.index, pd.DatetimeIndex):
            df.index = pd.to_datetime(df.index)
        
        target_col = 'price' if 'price' in df.columns else df.columns[0]
        
        # Create features (same as in XGBoost framework)
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
        
        # Volume-based features (create dummy features if volume not available)
        if 'volume' in df.columns:
            df['volume_lag_1'] = df['volume'].shift(1)
            df['volume_ma_7'] = df['volume'].rolling(window=7).mean()
            df['price_volume_ratio'] = df[target_col] / (df['volume'] + 1e-8)
        else:
            # Create dummy volume features to match XGBoost model expectations
            df['volume_lag_1'] = 0
            df['volume_ma_7'] = 0
            df['price_volume_ratio'] = 1
        
        return df

    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi

    def predict_with_prophet(self, data: pd.DataFrame, periods: int = 24) -> pd.DataFrame:
        """Generate predictions using the best Prophet model."""
        if 'prophet' not in self.loaded_models:
            raise ValueError("Prophet model not loaded. Call load_best_models() first.")
        
        prophet_model = self.loaded_models['prophet']['model']
        prophet_data = self._prepare_prophet_data(data)
        
        # Create future dataframe
        future = prophet_model.make_future_dataframe(periods=periods, freq='H')
        
        # Make predictions
        forecast = prophet_model.predict(future)
        
        # Return only future predictions
        future_forecast = forecast.tail(periods)[['ds', 'yhat', 'yhat_lower', 'yhat_upper']]
        future_forecast = future_forecast.set_index('ds')
        
        return future_forecast

    def predict_with_xgboost(self, data: pd.DataFrame, periods: int = 24) -> pd.DataFrame:
        """Generate predictions using the best XGBoost model."""
        if 'xgboost' not in self.loaded_models:
            raise ValueError("XGBoost model not loaded. Call load_best_models() first.")
        
        xgb_model = self.loaded_models['xgboost']['model']
        
        # Handle ensemble model structure
        if isinstance(xgb_model, dict) and 'models' in xgb_model:
            # This is an ensemble model
            models = xgb_model['models']
            prediction_method = xgb_model.get('prediction_method', 'average')
        else:
            # Single model
            models = [xgb_model]
            prediction_method = 'single'
        
        # Prepare data with features
        df_with_features = self._prepare_xgboost_data(data)
        
        # Get feature columns (excluding target)
        target_col = 'price' if 'price' in df_with_features.columns else df_with_features.columns[0]
        feature_cols = [col for col in df_with_features.columns 
                       if col != target_col and df_with_features[col].dtype in ['int64', 'float64']]
        
        # Generate rolling predictions
        predictions = []
        current_data = df_with_features.copy()
        
        for step in range(periods):
            # Get latest features (drop NaN rows)
            latest_features = current_data[feature_cols].dropna().tail(1)
            
            if latest_features.empty:
                # If no valid features, use last known values
                latest_features = current_data[feature_cols].fillna(method='ffill').tail(1)
            
            # Make prediction with all models
            step_predictions = []
            for model in models:
                pred = model.predict(latest_features)[0]
                step_predictions.append(pred)
            
            # Combine predictions
            if prediction_method == 'average':
                final_pred = np.mean(step_predictions)
            else:
                final_pred = step_predictions[0]
            
            predictions.append(final_pred)
            
            # Update data for next prediction
            next_time = current_data.index[-1] + timedelta(hours=1)
            
            # Create new row with predicted value
            new_row = current_data.iloc[-1:].copy()
            new_row.index = [next_time]
            new_row[target_col] = final_pred
            
            # Update features for new row
            new_row = self._update_features_for_prediction(new_row, current_data, target_col)
            
            # Append to current data
            current_data = pd.concat([current_data, new_row])
        
        # Create prediction dataframe
        future_dates = pd.date_range(start=data.index[-1] + timedelta(hours=1), periods=periods, freq='H')
        prediction_df = pd.DataFrame({
            'prediction': predictions
        }, index=future_dates)
        
        return prediction_df

    def _update_features_for_prediction(self, new_row: pd.DataFrame, historical_data: pd.DataFrame, target_col: str) -> pd.DataFrame:
        """Update features for a new prediction row."""
        # This is a simplified feature update - in production you'd want more sophisticated feature engineering
        combined_data = pd.concat([historical_data, new_row])
        updated_data = self._prepare_xgboost_data(combined_data)
        return updated_data.tail(1)

    def create_weighted_ensemble(self, 
                                data: pd.DataFrame, 
                                periods: int = 24,
                                strategy: str = 'performance_weighted') -> Dict[str, Any]:
        """
        Create ensemble predictions using different weighting strategies.
        
        Args:
            data: Historical data for prediction
            periods: Number of periods to forecast
            strategy: Weighting strategy ('equal', 'performance_weighted', 'inverse_error')
            
        Returns:
            Dictionary containing ensemble predictions and metadata
        """
        print(f"🎯 Creating ensemble forecast with {strategy} strategy...")
        
        # Load models if not already loaded
        if not self.loaded_models:
            self.load_best_models()
        
        predictions = {}
        weights = {}
        
        # Get predictions from each model
        if 'prophet' in self.loaded_models:
            try:
                prophet_pred = self.predict_with_prophet(data, periods)
                predictions['prophet'] = prophet_pred['yhat'].values
                prophet_mape = self.loaded_models['prophet']['performance'].get('mape', 10.0)
                print(f"   📊 Prophet predictions generated (MAPE: {prophet_mape:.2f}%)")
            except Exception as e:
                print(f"   ❌ Prophet prediction failed: {e}")
        
        if 'xgboost' in self.loaded_models:
            try:
                xgb_pred = self.predict_with_xgboost(data, periods)
                predictions['xgboost'] = xgb_pred['prediction'].values
                xgb_mape = self.loaded_models['xgboost']['performance'].get('mape', 1.0)
                print(f"   🎯 XGBoost predictions generated (MAPE: {xgb_mape:.2f}%)")
            except Exception as e:
                print(f"   ❌ XGBoost prediction failed: {e}")
        
        if not predictions:
            raise ValueError("No valid predictions generated from component models")
        
        # Calculate weights based on strategy
        if strategy == 'equal':
            # Equal weights
            total_models = len(predictions)
            weights = {model: 1/total_models for model in predictions.keys()}
            
        elif strategy == 'performance_weighted':
            # Weight based on inverse MAPE (better performance = higher weight)
            mapes = {}
            for model in predictions.keys():
                mapes[model] = self.loaded_models[model]['performance'].get('mape', 10.0)
            
            # Calculate inverse weights (lower MAPE = higher weight)
            inverse_mapes = {model: 1/max(mape, 0.01) for model, mape in mapes.items()}
            total_inverse = sum(inverse_mapes.values())
            weights = {model: weight/total_inverse for model, weight in inverse_mapes.items()}
            
        elif strategy == 'inverse_error':
            # Weight based on inverse of error rates
            for model in predictions.keys():
                error_rate = self.loaded_models[model]['performance'].get('mape', 10.0) / 100
                weights[model] = 1 / max(error_rate, 0.001)
            
            # Normalize weights
            total_weight = sum(weights.values())
            weights = {model: weight/total_weight for model, weight in weights.items()}
        
        # Create ensemble prediction
        ensemble_pred = np.zeros(periods)
        for model, pred_values in predictions.items():
            ensemble_pred += weights[model] * pred_values
        
        # Calculate confidence score based on agreement between models
        confidence_score = self._calculate_ensemble_confidence(predictions, weights)
        
        # Create future dates
        future_dates = pd.date_range(start=data.index[-1] + timedelta(hours=1), periods=periods, freq='H')
        
        # Create ensemble result
        ensemble_result = {
            'predictions': pd.DataFrame({
                'ensemble_prediction': ensemble_pred,
                'confidence_score': confidence_score
            }, index=future_dates),
            'component_predictions': predictions,
            'weights': weights,
            'strategy': strategy,
            'methodology': 'ensemble',
            'component_models': list(predictions.keys()),
            'metadata': {
                'created_at': datetime.now().isoformat(),
                'forecast_horizon': periods,
                'strategy': strategy,
                'component_count': len(predictions)
            }
        }
        
        print(f"   ✅ Ensemble forecast created with {len(predictions)} models")
        print(f"   📊 Weights: {', '.join([f'{k}: {v:.2f}' for k, v in weights.items()])}")
        print(f"   🎯 Average confidence: {np.mean(confidence_score):.2f}")
        
        return ensemble_result

    def _calculate_ensemble_confidence(self, predictions: Dict[str, np.ndarray], weights: Dict[str, float]) -> np.ndarray:
        """
        Calculate confidence score based on agreement between models.
        
        Higher confidence when models agree, lower when they diverge.
        """
        if len(predictions) < 2:
            return np.ones(len(list(predictions.values())[0]))
        
        pred_values = list(predictions.values())
        periods = len(pred_values[0])
        confidence_scores = []
        
        for i in range(periods):
            period_preds = [pred[i] for pred in pred_values]
            
            # Calculate coefficient of variation (std/mean) as measure of disagreement
            mean_pred = np.mean(period_preds)
            std_pred = np.std(period_preds)
            
            if mean_pred == 0:
                cv = 0
            else:
                cv = std_pred / abs(mean_pred)
            
            # Convert to confidence score (lower CV = higher confidence)
            confidence = max(0, min(1, 1 - cv))
            confidence_scores.append(confidence)
        
        return np.array(confidence_scores)

    def store_ensemble_model(self, ensemble_result: Dict[str, Any], description: str = "") -> str:
        """
        Store ensemble model and its predictions.
        
        Args:
            ensemble_result: Result from create_weighted_ensemble
            description: Model description
            
        Returns:
            Model ID of stored ensemble
        """
        # Create ensemble model object
        ensemble_model = {
            'type': 'ensemble',
            'strategy': ensemble_result['strategy'],
            'weights': ensemble_result['weights'],
            'component_models': ensemble_result['component_models'],
            'metadata': ensemble_result['metadata']
        }
        
        # Calculate performance metrics (if validation data available)
        performance_metrics = {
            'strategy': ensemble_result['strategy'],
            'component_count': len(ensemble_result['component_models']),
            'average_confidence': float(np.mean(ensemble_result['predictions']['confidence_score'])),
            'forecast_horizon': ensemble_result['metadata']['forecast_horizon']
        }
        
        # Store using storage manager
        model_id = self.storage_manager.store_model(
            model=ensemble_model,
            methodology='ensemble',
            asset='ETH',
            model_config={
                'strategy': ensemble_result['strategy'],
                'component_models': ensemble_result['component_models'],
                'weights': ensemble_result['weights']
            },
            performance_metrics=performance_metrics,
            description=description or f"ETH Ensemble model with {ensemble_result['strategy']} strategy",
            variant=ensemble_result['strategy'],
            tags=['ensemble', 'eth', 'forecast', ensemble_result['strategy']]
        )
        
        print(f"✅ Ensemble model stored with ID: {model_id}")
        return model_id

    def generate_sample_data(self, days: int = 30) -> pd.DataFrame:
        """Generate sample data for ensemble testing."""
        dates = pd.date_range(start=datetime.now() - timedelta(days=days), 
                             end=datetime.now(), freq='H')
        
        np.random.seed(42)
        base_price = 2000
        
        prices = []
        current_price = base_price
        
        for i in range(len(dates)):
            change_pct = np.random.normal(0, 0.005)
            if np.random.random() < 0.51:
                change_pct += 0.0001
            
            new_price = current_price * (1 + change_pct)
            new_price = max(500, min(10000, new_price))
            current_price = new_price
            prices.append(current_price)
        
        volumes = np.random.lognormal(10, 0.3, len(dates))
        
        df = pd.DataFrame({
            'date': dates,
            'price': prices,
            'volume': volumes
        })
        
        df = df.set_index('date')
        df = df.replace([np.inf, -np.inf], np.nan).ffill().bfill()
        
        return df

# Demo function
def demo_ensemble_framework():
    """Demonstrate ensemble forecast framework capabilities."""
    print("🚀 ETH Ensemble Forecast Framework Demo")
    print("=" * 60)
    
    # Initialize framework
    framework = ETHEnsembleForecastFramework()
    
    # Load best models
    best_models = framework.load_best_models()
    
    if not best_models:
        print("❌ No models available for ensemble. Please train Prophet and XGBoost models first.")
        return
    
    print(f"\n📊 Loaded {len(best_models)} best models for ensemble")
    
    # Generate sample data
    print("\n📈 Generating sample data for forecasting...")
    data = framework.generate_sample_data(days=30)
    print(f"   Generated {len(data)} hours of historical data")
    print(f"   Price range: ${data['price'].min():.2f} - ${data['price'].max():.2f}")
    
    # Test different ensemble strategies
    strategies = ['equal', 'performance_weighted', 'inverse_error']
    ensemble_results = {}
    
    for strategy in strategies:
        print(f"\n🎯 Testing {strategy} ensemble strategy...")
        try:
            result = framework.create_weighted_ensemble(
                data=data,
                periods=24,  # 24 hour forecast
                strategy=strategy
            )
            ensemble_results[strategy] = result
            
            # Store the ensemble model
            model_id = framework.store_ensemble_model(
                result,
                description=f"ETH ensemble forecast using {strategy} strategy with best Prophet and XGBoost models"
            )
            
            # Show sample predictions
            predictions = result['predictions']
            print(f"   📊 Sample forecasts (next 6 hours):")
            for i in range(min(6, len(predictions))):
                pred_time = predictions.index[i]
                pred_value = predictions.iloc[i]['ensemble_prediction']
                confidence = predictions.iloc[i]['confidence_score']
                print(f"      {pred_time.strftime('%Y-%m-%d %H:%M')}: ${pred_value:.2f} (confidence: {confidence:.2f})")
                
        except Exception as e:
            print(f"   ❌ {strategy} strategy failed: {e}")
    
    # Compare ensemble strategies
    if ensemble_results:
        print(f"\n📊 Ensemble Strategy Comparison:")
        print(f"   {'Strategy':<20} {'Avg Confidence':<15} {'Price Range':<20} {'Components'}")
        print(f"   {'-'*75}")
        
        for strategy, result in ensemble_results.items():
            avg_conf = np.mean(result['predictions']['confidence_score'])
            pred_min = result['predictions']['ensemble_prediction'].min()
            pred_max = result['predictions']['ensemble_prediction'].max()
            components = len(result['component_models'])
            
            print(f"   {strategy:<20} {avg_conf:<15.3f} ${pred_min:.2f} - ${pred_max:.2f}    {components}")
    
    # Show storage summary
    print(f"\n📁 Updated Storage Summary:")
    all_models = framework.storage_manager.list_models()
    ensemble_models = [m for m in all_models if m.methodology == 'ensemble']
    
    print(f"   Total Models: {len(all_models)}")
    print(f"   Ensemble Models: {len(ensemble_models)}")
    print(f"   Methodologies: {len(set(m.methodology for m in all_models))}")
    
    print(f"\n✅ Ensemble framework demo completed!")
    print(f"   Strategies tested: {len(ensemble_results)}")
    print(f"   Ensemble models created: {len(ensemble_models)}")

if __name__ == "__main__":
    demo_ensemble_framework()
