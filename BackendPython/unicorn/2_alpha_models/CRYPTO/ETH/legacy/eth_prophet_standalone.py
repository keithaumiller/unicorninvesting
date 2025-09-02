"""
ETH Prophet Framework - Standalone Implementation

A self-contained ETH Prophet forecasting framework with three model variants.
This version includes all necessary components without external dependencies.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import sys
import os
from datetime import datetime, timedelta
import sqlite3
import json
import warnings
from pathlib import Path

# Prophet import with error handling
try:
    from prophet import Prophet
    from prophet.diagnostics import cross_validation, performance_metrics
    PROPHET_AVAILABLE = True
except ImportError:
    print("⚠️  Prophet not installed. Install with: pip install prophet")
    PROPHET_AVAILABLE = False

warnings.filterwarnings('ignore', category=RuntimeWarning)

class ETHProphetModel:
    """
    Prophet-based forecasting model for ETH.
    Self-contained implementation without external dependencies.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None, model_variant: str = "basic"):
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet is required but not installed")
            
        self.asset_name = "ETH"
        self.model_variant = model_variant
        self.config = config or self._get_default_config()
        self.model = None
        self.is_trained = False
        self.training_data = None
        
    def _get_default_config(self) -> Dict[str, Any]:
        """Get default Prophet configuration."""
        return {
            'seasonality_mode': 'additive',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.05,
            'seasonality_prior_scale': 10.0,
            'interval_width': 0.80,
            'growth': 'linear'
        }
    
    def prepare_prophet_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Prepare data in Prophet format.
        
        Args:
            data: Input OHLCV data
            
        Returns:
            DataFrame in Prophet format (ds, y, regressors)
        """
        if 'Close' not in data.columns:
            raise ValueError("Data must contain 'Close' column")
            
        prophet_data = pd.DataFrame({
            'ds': data.index,
            'y': data['Close']
        })
        
        # Add regressors based on model variant
        if self.model_variant in ['enhanced', 'optimized']:
            # Volume regressor
            if 'Volume' in data.columns:
                volume_norm = (data['Volume'] - data['Volume'].mean()) / data['Volume'].std()
                prophet_data['volume_normalized'] = volume_norm.fillna(0)
            
            # Volatility regressor
            volatility = data['Close'].rolling(window=7).std().bfill()
            volatility_norm = (volatility - volatility.mean()) / volatility.std()
            prophet_data['volatility'] = volatility_norm.fillna(0)
            
            # Momentum regressor
            momentum_7d = data['Close'].pct_change(7).fillna(0)
            prophet_data['momentum_7d'] = momentum_7d
        
        return prophet_data
    
    def train(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Train the Prophet model.
        
        Args:
            data: Historical OHLCV data
            
        Returns:
            Training results
        """
        try:
            # Prepare data
            prophet_data = self.prepare_prophet_data(data)
            
            # Initialize Prophet model
            self.model = Prophet(**self.config)
            
            # Add regressors for enhanced/optimized variants
            if self.model_variant in ['enhanced', 'optimized']:
                if 'volume_normalized' in prophet_data.columns:
                    self.model.add_regressor('volume_normalized', prior_scale=10.0)
                if 'volatility' in prophet_data.columns:
                    self.model.add_regressor('volatility', prior_scale=5.0)
                if 'momentum_7d' in prophet_data.columns:
                    self.model.add_regressor('momentum_7d', prior_scale=8.0)
            
            # Add custom seasonalities for optimized variant
            if self.model_variant == 'optimized':
                self.model.add_seasonality(
                    name='crypto_weekly',
                    period=7,
                    fourier_order=3,
                    prior_scale=15.0
                )
                self.model.add_seasonality(
                    name='crypto_monthly',
                    period=30.5,
                    fourier_order=5,
                    prior_scale=10.0
                )
            
            # Fit the model
            self.model.fit(prophet_data)
            self.is_trained = True
            self.training_data = prophet_data
            
            return {
                'success': True,
                'model_variant': self.model_variant,
                'training_samples': len(prophet_data),
                'training_period': f"{prophet_data['ds'].min()} to {prophet_data['ds'].max()}"
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def predict(self, periods: int = 30, include_history: bool = True) -> pd.DataFrame:
        """
        Make predictions.
        
        Args:
            periods: Number of periods to forecast
            include_history: Whether to include historical predictions
            
        Returns:
            DataFrame with predictions
        """
        if not self.is_trained:
            raise ValueError("Model must be trained before making predictions")
        
        # Create future dataframe
        future = self.model.make_future_dataframe(periods=periods)
        
        # Add regressor values for future dates
        if self.model_variant in ['enhanced', 'optimized']:
            # For simplicity, use last known values for regressors
            if len(self.training_data) > 0:
                last_values = self.training_data.iloc[-1]
                
                for col in ['volume_normalized', 'volatility', 'momentum_7d']:
                    if col in self.training_data.columns:
                        # Fill missing values with last known value
                        future[col] = future[col].fillna(last_values[col])
                        # Fill any remaining NaN with 0
                        future[col] = future[col].fillna(0)
        
        # Make predictions
        forecast = self.model.predict(future)
        
        if not include_history:
            forecast = forecast.tail(periods)
        
        return forecast

class ETHProphetFramework:
    """
    Comprehensive framework for developing and comparing three ETH Prophet models.
    """
    
    def __init__(self):
        self.models = {}
        self.results = {}
        
        # Database for storing model comparisons
        self.db_path = Path(__file__).parent / "eth_prophet_comparison.db"
        self._initialize_comparison_db()
        
    def _initialize_comparison_db(self):
        """Initialize the comparison database for model results."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            # Create comparison table
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS model_comparisons (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    experiment_id TEXT NOT NULL,
                    model_variant TEXT NOT NULL,
                    training_date TIMESTAMP NOT NULL,
                    validation_mape REAL,
                    validation_mae REAL,
                    validation_rmse REAL,
                    validation_r2 REAL,
                    directional_accuracy REAL,
                    config TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Create performance summary table
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS performance_summary (
                    experiment_id TEXT PRIMARY KEY,
                    best_model TEXT,
                    best_mape REAL,
                    total_models INTEGER,
                    training_period TEXT,
                    validation_period TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            conn.commit()
    
    def create_basic_model(self) -> ETHProphetModel:
        """Create Basic Prophet Model."""
        config = {
            'seasonality_mode': 'additive',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.05,
            'seasonality_prior_scale': 10.0,
            'interval_width': 0.80,
            'growth': 'linear'
        }
        return ETHProphetModel(config, model_variant="basic")
    
    def create_enhanced_model(self) -> ETHProphetModel:
        """Create Enhanced Prophet Model."""
        config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.1,
            'seasonality_prior_scale': 15.0,
            'interval_width': 0.80,
            'growth': 'linear',
            'uncertainty_samples': 1000
        }
        return ETHProphetModel(config, model_variant="enhanced")
    
    def create_optimized_model(self) -> ETHProphetModel:
        """Create Optimized Prophet Model."""
        config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': True,
            'changepoint_prior_scale': 0.15,
            'seasonality_prior_scale': 20.0,
            'holidays_prior_scale': 12.0,
            'interval_width': 0.85,
            'growth': 'linear',
            'uncertainty_samples': 1500,
            'mcmc_samples': 0,
            'changepoint_range': 0.9
        }
        return ETHProphetModel(config, model_variant="optimized")
    
    def calculate_metrics(self, predictions: pd.Series, actuals: pd.Series) -> Dict[str, float]:
        """
        Calculate comprehensive performance metrics.
        
        Args:
            predictions: Model predictions
            actuals: Actual values
            
        Returns:
            Dictionary of performance metrics
        """
        # Align data
        aligned_data = pd.DataFrame({
            'pred': predictions,
            'actual': actuals
        }).dropna()
        
        if len(aligned_data) == 0:
            return {}
        
        pred = aligned_data['pred']
        actual = aligned_data['actual']
        
        # Basic accuracy metrics
        mae = np.mean(np.abs(actual - pred))
        mse = np.mean((actual - pred) ** 2)
        rmse = np.sqrt(mse)
        mape = np.mean(np.abs((actual - pred) / actual)) * 100
        
        # R-squared
        ss_res = np.sum((actual - pred) ** 2)
        ss_tot = np.sum((actual - np.mean(actual)) ** 2)
        r2 = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0
        
        # Directional accuracy
        actual_direction = np.sign(actual.diff().dropna())
        pred_direction = np.sign(pred.diff().dropna())
        
        if len(actual_direction) > 0:
            directional_accuracy = np.mean(actual_direction == pred_direction) * 100
        else:
            directional_accuracy = 0
        
        return {
            'mae': mae,
            'mse': mse,
            'rmse': rmse,
            'mape': mape,
            'r2': r2,
            'directional_accuracy': directional_accuracy
        }
    
    def train_all_models(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train all three Prophet models and compare performance.
        
        Args:
            data: Historical OHLCV data
            validation_split: Fraction of data for validation
            
        Returns:
            Comprehensive results for all models
        """
        experiment_id = f"eth_prophet_exp_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        print(f"🚀 Starting ETH Prophet Framework Experiment: {experiment_id}")
        print("=" * 70)
        
        # Split data
        split_idx = int(len(data) * (1 - validation_split))
        train_data = data.iloc[:split_idx]
        val_data = data.iloc[split_idx:]
        
        results = {
            'experiment_id': experiment_id,
            'models': {},
            'best_model': None
        }
        
        # Model configurations
        model_creators = {
            'basic': self.create_basic_model,
            'enhanced': self.create_enhanced_model,
            'optimized': self.create_optimized_model
        }
        
        best_mape = float('inf')
        best_model_name = None
        
        for model_name, model_creator in model_creators.items():
            print(f"\n📊 Training {model_name.title()} Prophet Model...")
            print("-" * 50)
            
            try:
                # Create and train model
                model = model_creator()
                training_result = model.train(train_data)
                
                if training_result['success']:
                    # Make predictions on validation data
                    forecast = model.predict(periods=len(val_data), include_history=False)
                    
                    # Calculate metrics
                    metrics = self.calculate_metrics(
                        forecast['yhat'], 
                        val_data['Close']
                    )
                    
                    # Store results
                    results['models'][model_name] = {
                        'model': model,
                        'training_result': training_result,
                        'forecast': forecast,
                        'metrics': metrics,
                        'config': model.config
                    }
                    
                    # Track best model
                    if metrics.get('mape', float('inf')) < best_mape:
                        best_mape = metrics['mape']
                        best_model_name = model_name
                    
                    # Save to comparison database
                    self._save_model_comparison(
                        experiment_id, model_name, metrics, model.config
                    )
                    
                    # Print results
                    print(f"✅ {model_name.title()} Model Results:")
                    
                    # Safe printing with type checking
                    mape = metrics.get('mape', 'N/A')
                    mae = metrics.get('mae', 'N/A')
                    rmse = metrics.get('rmse', 'N/A')
                    r2 = metrics.get('r2', 'N/A')
                    dir_acc = metrics.get('directional_accuracy', 'N/A')
                    
                    if isinstance(mape, (int, float)) and not pd.isna(mape):
                        print(f"   MAPE: {mape:.2f}%")
                    else:
                        print(f"   MAPE: N/A")
                        
                    if isinstance(mae, (int, float)) and not pd.isna(mae):
                        print(f"   MAE: {mae:.2f}")
                    else:
                        print(f"   MAE: N/A")
                        
                    if isinstance(rmse, (int, float)) and not pd.isna(rmse):
                        print(f"   RMSE: {rmse:.2f}")
                    else:
                        print(f"   RMSE: N/A")
                        
                    if isinstance(r2, (int, float)) and not pd.isna(r2):
                        print(f"   R²: {r2:.4f}")
                    else:
                        print(f"   R²: N/A")
                        
                    if isinstance(dir_acc, (int, float)) and not pd.isna(dir_acc):
                        print(f"   Directional Accuracy: {dir_acc:.1f}%")
                    else:
                        print(f"   Directional Accuracy: N/A")
                    
                else:
                    print(f"❌ {model_name.title()} Model training failed: {training_result.get('error', 'Unknown error')}")
                    
            except Exception as e:
                print(f"❌ Error training {model_name} model: {str(e)}")
        
        # Save performance summary
        if best_model_name:
            self._save_performance_summary(
                experiment_id, best_model_name, best_mape, len(model_creators),
                f"{train_data.index[0]} to {train_data.index[-1]}",
                f"{val_data.index[0]} to {val_data.index[-1]}"
            )
        
        results['best_model'] = best_model_name
        self.results = results
        return results
    
    def _save_model_comparison(self, experiment_id: str, model_name: str, 
                             metrics: Dict[str, float], config: Dict[str, Any]):
        """Save model comparison results to database."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                INSERT INTO model_comparisons (
                    experiment_id, model_variant, training_date,
                    validation_mape, validation_mae, validation_rmse, validation_r2,
                    directional_accuracy, config
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                experiment_id, model_name, datetime.now(),
                metrics.get('mape'), metrics.get('mae'), metrics.get('rmse'), 
                metrics.get('r2'), metrics.get('directional_accuracy'),
                json.dumps(config)
            ))
            
            conn.commit()
    
    def _save_performance_summary(self, experiment_id: str, best_model: str, 
                                best_mape: float, total_models: int,
                                training_period: str, validation_period: str):
        """Save performance summary to database."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                INSERT OR REPLACE INTO performance_summary (
                    experiment_id, best_model, best_mape, total_models,
                    training_period, validation_period
                ) VALUES (?, ?, ?, ?, ?, ?)
            """, (
                experiment_id, best_model, best_mape, total_models,
                training_period, validation_period
            ))
            
            conn.commit()
    
    def generate_comparison_report(self) -> str:
        """
        Generate a comprehensive comparison report.
        
        Returns:
            Formatted comparison report
        """
        if not self.results:
            return "No results available. Please run train_all_models() first."
        
        report = []
        report.append("ETH Prophet Models Comparison Report")
        report.append("=" * 50)
        report.append(f"Experiment ID: {self.results['experiment_id']}")
        report.append(f"Best Model: {self.results['best_model']}")
        report.append("")
        
        # Model comparison table
        report.append("Model Performance Comparison:")
        report.append("-" * 30)
        
        for model_name, model_data in self.results['models'].items():
            metrics = model_data['metrics']
            report.append(f"\n{model_name.upper()} MODEL:")
            
            # Safe formatting with proper type checking
            mape = metrics.get('mape', 'N/A')
            mae = metrics.get('mae', 'N/A')
            rmse = metrics.get('rmse', 'N/A')
            r2 = metrics.get('r2', 'N/A')
            dir_acc = metrics.get('directional_accuracy', 'N/A')
            
            if isinstance(mape, (int, float)) and not pd.isna(mape):
                report.append(f"  MAPE: {mape:.2f}%")
            else:
                report.append(f"  MAPE: N/A")
                
            if isinstance(mae, (int, float)) and not pd.isna(mae):
                report.append(f"  MAE: {mae:.2f}")
            else:
                report.append(f"  MAE: N/A")
                
            if isinstance(rmse, (int, float)) and not pd.isna(rmse):
                report.append(f"  RMSE: {rmse:.2f}")
            else:
                report.append(f"  RMSE: N/A")
                
            if isinstance(r2, (int, float)) and not pd.isna(r2):
                report.append(f"  R²: {r2:.4f}")
            else:
                report.append(f"  R²: N/A")
                
            if isinstance(dir_acc, (int, float)) and not pd.isna(dir_acc):
                report.append(f"  Directional Accuracy: {dir_acc:.1f}%")
            else:
                report.append(f"  Directional Accuracy: N/A")
        
        return "\n".join(report)
    
    def get_historical_experiments(self) -> pd.DataFrame:
        """Get historical experiment results from database."""
        with sqlite3.connect(self.db_path) as conn:
            query = """
                SELECT * FROM model_comparisons 
                ORDER BY training_date DESC
            """
            return pd.read_sql_query(query, conn)

def create_sample_eth_data(days: int = 365) -> pd.DataFrame:
    """
    Create sample ETH data for testing.
    
    Args:
        days: Number of days to generate
        
    Returns:
        Sample ETH OHLCV data
    """
    np.random.seed(42)
    dates = pd.date_range(start='2023-01-01', periods=days, freq='D')
    
    # Generate realistic ETH price data with trend and volatility
    base_price = 2000
    trend = np.linspace(0, 500, days)  # Upward trend
    noise = np.random.normal(0, 100, days)
    volatility = np.random.normal(0, 50, days)
    
    close_prices = base_price + trend + noise.cumsum() * 0.1 + volatility
    
    # Generate OHLCV from close prices
    data = pd.DataFrame(index=dates)
    data['Close'] = close_prices
    data['Open'] = data['Close'].shift(1) + np.random.normal(0, 10, days)
    data['High'] = np.maximum(data['Open'], data['Close']) + np.abs(np.random.normal(0, 20, days))
    data['Low'] = np.minimum(data['Open'], data['Close']) - np.abs(np.random.normal(0, 20, days))
    data['Volume'] = np.random.lognormal(15, 0.5, days)
    
    # Ensure no negative prices
    for col in ['Open', 'High', 'Low', 'Close']:
        data[col] = np.maximum(data[col], 100)
    
    return data.dropna()

if __name__ == "__main__":
    print("ETH Prophet Framework - Standalone Implementation")
    print("=" * 60)
    
    # Check Prophet availability
    if not PROPHET_AVAILABLE:
        print("❌ Prophet not available. Please install with: pip install prophet")
        sys.exit(1)
    
    # Create sample data for demonstration
    sample_data = create_sample_eth_data(500)
    print(f"📊 Created sample data: {len(sample_data)} days")
    
    # Initialize framework
    framework = ETHProphetFramework()
    print("🔧 Framework initialized")
    
    # Train all models and compare
    results = framework.train_all_models(sample_data, validation_split=0.2)
    
    # Generate comparison report
    if results['models']:
        print("\n" + framework.generate_comparison_report())
        
        print(f"\n💾 Best model ({results['best_model']}) ready for deployment")
        print("🚀 Framework successfully tested!")
    else:
        print("❌ No models trained successfully")
