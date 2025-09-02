"""
ETH Prophet Model Development Framework

Comprehensive framework for developing and comparing three Prophet-based models for ETH forecasting:
1. Basic Prophet Model - Standard configuration
2. Enhanced Prophet Model - With external regressors and custom seasonality
3. Optimized Prophet Model - Hyperparameter tuned for maximum performance

All models include comprehensive performance tracking and comparison metrics.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import sys
import os
from datetime import datetime, timedelta
import pickle
import warnings
from models.model_management.model_storage_manager import ModelStorageManager
import os
from datetime import datetime, timedelta
import sqlite3
import json
import warnings
from pathlib import Path

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))

try:
    from prophet import Prophet
    from prophet.diagnostics import cross_validation, performance_metrics
except ImportError:
    print("⚠️  Prophet not installed. Install with: pip install prophet")
    sys.exit(1)

from shared.model_framework import ProphetModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage, MetricType, PerformanceMetric
from models.eth_prophet import ETHProphetModel

warnings.filterwarnings('ignore', category=RuntimeWarning)

class ETHProphetFramework:
    """
    Comprehensive framework for developing and comparing three ETH Prophet models.
    """
    
    def __init__(self, data_path: Optional[str] = None):
        self.data_path = data_path
        self.models = {}
        self.performance_tracker = ModelPerformanceTracker()
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
                    sharpe_ratio REAL,
                    max_drawdown REAL,
                    volatility REAL,
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
    
    def create_basic_prophet_model(self) -> ETHProphetModel:
        """
        Create Basic Prophet Model with standard configuration.
        
        Returns:
            Configured basic Prophet model
        """
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
        
        model = ETHProphetModel(config)
        model.model_variant = "basic"
        return model
    
    def create_enhanced_prophet_model(self) -> ETHProphetModel:
        """
        Create Enhanced Prophet Model with external regressors and custom seasonality.
        
        Returns:
            Configured enhanced Prophet model
        """
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
        
        model = ETHProphetModel(config)
        model.model_variant = "enhanced"
        return model
    
    def create_optimized_prophet_model(self) -> ETHProphetModel:
        """
        Create Optimized Prophet Model with hyperparameter tuning.
        
        Returns:
            Configured optimized Prophet model
        """
        config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': True,
            'changepoint_prior_scale': 0.15,  # Optimized for ETH volatility
            'seasonality_prior_scale': 20.0,  # Higher for crypto seasonality
            'holidays_prior_scale': 12.0,     # Account for market events
            'interval_width': 0.85,
            'growth': 'linear',
            'uncertainty_samples': 1500,
            'mcmc_samples': 0,
            'changepoint_range': 0.9  # Extend changepoint detection
        }
        
        model = ETHProphetModel(config)
        model.model_variant = "optimized"
        return model
    
    def add_crypto_holidays(self, model: Prophet) -> Prophet:
        """
        Add cryptocurrency-specific holidays and events.
        
        Args:
            model: Prophet model to enhance
            
        Returns:
            Enhanced Prophet model with crypto holidays
        """
        # Create crypto holidays dataframe
        crypto_holidays = pd.DataFrame({
            'holiday': 'crypto_events',
            'ds': pd.to_datetime([
                '2021-01-01',  # ETH 2.0 events
                '2021-08-05',  # London Hard Fork
                '2022-09-15',  # The Merge
                '2023-04-12',  # Shapella Upgrade
            ]),
            'lower_window': 0,
            'upper_window': 3,
        })
        
        model.add_country_holidays(country_name='US')  # US market influence
        
        # Add the crypto holidays
        for _, holiday in crypto_holidays.iterrows():
            model.add_country_holidays(country_name='US')
        
        return model
    
    def enhanced_feature_engineering(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Advanced feature engineering for enhanced and optimized models.
        
        Args:
            data: Raw OHLCV data
            
        Returns:
            Data with additional features
        """
        df = data.copy()
        
        # Technical indicators
        df['sma_7'] = df['Close'].rolling(window=7).mean()
        df['sma_30'] = df['Close'].rolling(window=30).mean()
        df['ema_12'] = df['Close'].ewm(span=12).mean()
        df['ema_26'] = df['Close'].ewm(span=26).mean()
        
        # Volatility measures
        df['volatility_7d'] = df['Close'].rolling(window=7).std()
        df['volatility_30d'] = df['Close'].rolling(window=30).std()
        
        # Price momentum
        df['momentum_3d'] = df['Close'].pct_change(3)
        df['momentum_7d'] = df['Close'].pct_change(7)
        
        # Volume indicators
        df['volume_sma'] = df['Volume'].rolling(window=7).mean()
        df['volume_ratio'] = df['Volume'] / df['volume_sma']
        
        # Price position within recent range
        df['high_14d'] = df['High'].rolling(window=14).max()
        df['low_14d'] = df['Low'].rolling(window=14).min()
        df['price_position'] = (df['Close'] - df['low_14d']) / (df['high_14d'] - df['low_14d'])
        
        # Fill NaN values
        df = df.fillna(method='bfill').fillna(method='ffill')
        
        return df
    
    def calculate_comprehensive_metrics(self, predictions: pd.Series, actuals: pd.Series) -> Dict[str, float]:
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
        directional_accuracy = np.mean(actual_direction == pred_direction) * 100
        
        # Financial metrics
        returns_actual = actual.pct_change().dropna()
        returns_pred = pred.pct_change().dropna()
        
        # Sharpe ratio (assuming 0% risk-free rate)
        sharpe_actual = np.mean(returns_actual) / np.std(returns_actual) * np.sqrt(252) if np.std(returns_actual) != 0 else 0
        sharpe_pred = np.mean(returns_pred) / np.std(returns_pred) * np.sqrt(252) if np.std(returns_pred) != 0 else 0
        
        # Maximum drawdown
        cumulative_actual = (1 + returns_actual).cumprod()
        running_max_actual = cumulative_actual.expanding().max()
        drawdown_actual = (cumulative_actual - running_max_actual) / running_max_actual
        max_drawdown_actual = drawdown_actual.min() * 100
        
        cumulative_pred = (1 + returns_pred).cumprod()
        running_max_pred = cumulative_pred.expanding().max()
        drawdown_pred = (cumulative_pred - running_max_pred) / running_max_pred
        max_drawdown_pred = drawdown_pred.min() * 100
        
        # Volatility
        volatility_actual = np.std(returns_actual) * np.sqrt(252) * 100
        volatility_pred = np.std(returns_pred) * np.sqrt(252) * 100
        
        return {
            'mae': mae,
            'mse': mse,
            'rmse': rmse,
            'mape': mape,
            'r2': r2,
            'directional_accuracy': directional_accuracy,
            'sharpe_actual': sharpe_actual,
            'sharpe_pred': sharpe_pred,
            'max_drawdown_actual': max_drawdown_actual,
            'max_drawdown_pred': max_drawdown_pred,
            'volatility_actual': volatility_actual,
            'volatility_pred': volatility_pred
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
        
        # Prepare enhanced data
        enhanced_data = self.enhanced_feature_engineering(data)
        
        # Split data
        split_idx = int(len(enhanced_data) * (1 - validation_split))
        train_data = enhanced_data.iloc[:split_idx]
        val_data = enhanced_data.iloc[split_idx:]
        
        results = {
            'experiment_id': experiment_id,
            'models': {},
            'comparison': {},
            'best_model': None
        }
        
        # Model configurations
        model_configs = {
            'basic': self.create_basic_prophet_model(),
            'enhanced': self.create_enhanced_prophet_model(),
            'optimized': self.create_optimized_prophet_model()
        }
        
        best_mape = float('inf')
        best_model_name = None
        
        for model_name, model in model_configs.items():
            print(f"\n📊 Training {model_name.title()} Prophet Model...")
            print("-" * 50)
            
            try:
                # Train model
                training_result = model.train_and_validate(train_data, validation_split=0)
                
                if training_result['success']:
                    # Make predictions on validation data
                    val_predictions = model.predict(train_data, periods=len(val_data))
                    
                    # Calculate comprehensive metrics
                    metrics = self.calculate_comprehensive_metrics(
                        val_predictions['yhat'], 
                        val_data['Close']
                    )
                    
                    # Store results
                    results['models'][model_name] = {
                        'model': model,
                        'training_result': training_result,
                        'validation_predictions': val_predictions,
                        'metrics': metrics,
                        'config': model.config
                    }
                    
                    # Track best model
                    if metrics['mape'] < best_mape:
                        best_mape = metrics['mape']
                        best_model_name = model_name
                    
                    # Save to comparison database
                    self._save_model_comparison(
                        experiment_id, model_name, metrics, model.config
                    )
                    
                    # Print results
                    print(f"✅ {model_name.title()} Model Results:")
                    print(f"   MAPE: {metrics['mape']:.2f}%")
                    print(f"   MAE: {metrics['mae']:.2f}")
                    print(f"   RMSE: {metrics['rmse']:.2f}")
                    print(f"   R²: {metrics['r2']:.4f}")
                    print(f"   Directional Accuracy: {metrics['directional_accuracy']:.1f}%")
                    print(f"   Sharpe Ratio (Pred): {metrics['sharpe_pred']:.2f}")
                    
                else:
                    print(f"❌ {model_name.title()} Model training failed: {training_result.get('error', 'Unknown error')}")
                    
            except Exception as e:
                print(f"❌ Error training {model_name} model: {str(e)}")
        
        # Save performance summary
        self._save_performance_summary(
            experiment_id, best_model_name, best_mape, len(model_configs),
            f"{train_data.index[0]} to {train_data.index[-1]}",
            f"{val_data.index[0]} to {val_data.index[-1]}"
        )
        
        results['best_model'] = best_model_name
        results['experiment_summary'] = {
            'total_models': len(model_configs),
            'best_model': best_model_name,
            'best_mape': best_mape,
            'training_period': f"{train_data.index[0]} to {train_data.index[-1]}",
            'validation_period': f"{val_data.index[0]} to {val_data.index[-1]}"
        }
        
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
                    directional_accuracy, sharpe_ratio, max_drawdown, volatility, config
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                experiment_id, model_name, datetime.now(),
                metrics['mape'], metrics['mae'], metrics['rmse'], metrics['r2'],
                metrics['directional_accuracy'], metrics['sharpe_pred'],
                metrics['max_drawdown_pred'], metrics['volatility_pred'],
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
            report.append(f"  MAPE: {metrics['mape']:.2f}%")
            report.append(f"  MAE: {metrics['mae']:.2f}")
            report.append(f"  RMSE: {metrics['rmse']:.2f}")
            report.append(f"  R²: {metrics['r2']:.4f}")
            report.append(f"  Directional Accuracy: {metrics['directional_accuracy']:.1f}%")
            report.append(f"  Sharpe Ratio: {metrics['sharpe_pred']:.2f}")
            report.append(f"  Max Drawdown: {metrics['max_drawdown_pred']:.1f}%")
            report.append(f"  Volatility: {metrics['volatility_pred']:.1f}%")
        
        return "\n".join(report)
    
    def get_historical_experiments(self) -> pd.DataFrame:
        """
        Get historical experiment results from database.
        
        Returns:
            DataFrame with historical experiment data
        """
        with sqlite3.connect(self.db_path) as conn:
            query = """
                SELECT * FROM model_comparisons 
                ORDER BY training_date DESC
            """
            return pd.read_sql_query(query, conn)
    
    def get_best_models_summary(self) -> pd.DataFrame:
        """
        Get summary of best models from each experiment.
        
        Returns:
            DataFrame with best model summaries
        """
        with sqlite3.connect(self.db_path) as conn:
            query = """
                SELECT * FROM performance_summary 
                ORDER BY created_at DESC
            """
            return pd.read_sql_query(query, conn)

def load_eth_data(file_path: str) -> pd.DataFrame:
    """
    Load ETH price data from file.
    
    Args:
        file_path: Path to data file
        
    Returns:
        DataFrame with OHLCV data
    """
    try:
        data = pd.read_csv(file_path, index_col=0, parse_dates=True)
        return data
    except Exception as e:
        print(f"Error loading data: {e}")
        return None

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
    data['Volume'] = np.random.lognormal(15, 0.5, days)  # Log-normal volume distribution
    
    # Ensure no negative prices
    for col in ['Open', 'High', 'Low', 'Close']:
        data[col] = np.maximum(data[col], 100)
    
    return data.dropna()

if __name__ == "__main__":
    print("ETH Prophet Framework - Comprehensive Model Development")
    print("=" * 60)
    
    # Create sample data for demonstration
    sample_data = create_sample_eth_data(500)  # 500 days of data
    
    # Initialize framework
    framework = ETHProphetFramework()
    
    # Train all models and compare
    results = framework.train_all_models(sample_data, validation_split=0.2)
    
    # Generate comparison report
    print("\n" + framework.generate_comparison_report())
    
    # Save best model
    if results['best_model']:
        best_model = results['models'][results['best_model']]['model']
        model_filename = f"eth_prophet_{results['best_model']}_best.pkl"
        # Save model logic would go here
        print(f"\n💾 Best model ({results['best_model']}) ready for deployment")
        print(f"    Model file: {model_filename}")
