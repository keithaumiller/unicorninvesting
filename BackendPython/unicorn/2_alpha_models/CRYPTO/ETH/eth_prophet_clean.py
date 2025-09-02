"""
ETH Prophet Model Framework with Organized Storage

Enhanced framework with scalable model storage, version control, and easy retrieval.
Integrates with ModelStorageManager for clean organization.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import warnings
import sqlite3
import json
from datetime import datetime, timedelta
from pathlib import Path
from model_storage_manager import ModelStorageManager

try:
    from prophet import Prophet
    PROPHET_AVAILABLE = True
except ImportError:
    print("Warning: Prophet not available. Install with: pip install prophet")
    PROPHET_AVAILABLE = False

warnings.filterwarnings('ignore')

class ETHProphetFrameworkWithStorage:
    """
    Enhanced ETH Prophet framework with organized model storage.
    
    Features:
    - Three distinct Prophet model variants
    - Organized model storage with version control
    - Comprehensive performance tracking
    - Easy model retrieval and comparison
    """
    
    def __init__(self):
        self.storage_manager = ModelStorageManager()
        self.db_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_prophet_comparison.db"
        self._init_performance_db()
        
    def _init_performance_db(self):
        """Initialize performance tracking database."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_performance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    model_id TEXT NOT NULL,
                    model_variant TEXT NOT NULL,
                    metric_name TEXT NOT NULL,
                    metric_value REAL NOT NULL,
                    created_at TEXT NOT NULL,
                    data_period TEXT NOT NULL
                )
            """)
            
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_predictions (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    model_id TEXT NOT NULL,
                    prediction_date TEXT NOT NULL,
                    actual_price REAL,
                    predicted_price REAL NOT NULL,
                    prediction_interval_lower REAL,
                    prediction_interval_upper REAL,
                    created_at TEXT NOT NULL
                )
            """)
    
    def create_basic_prophet_model(self, config_override: Dict = None):
        """Create basic Prophet model with ETH-optimized settings."""
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet not available")
        
        config = {
            'daily_seasonality': False,
            'weekly_seasonality': True,
            'yearly_seasonality': True,
            'seasonality_mode': 'multiplicative',
            'changepoint_prior_scale': 0.05,
            'seasonality_prior_scale': 10.0,
            'interval_width': 0.8
        }
        
        if config_override:
            config.update(config_override)
        
        model = Prophet(**config)
        return model
    
    def create_enhanced_prophet_model(self, config_override: Dict = None):
        """Create enhanced Prophet model with external regressors."""
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet not available")
        
        config = {
            'daily_seasonality': False,
            'weekly_seasonality': True,
            'yearly_seasonality': True,
            'seasonality_mode': 'multiplicative',
            'changepoint_prior_scale': 0.08,
            'seasonality_prior_scale': 15.0,
            'interval_width': 0.8
        }
        
        if config_override:
            config.update(config_override)
        
        model = Prophet(**config)
        
        # Add custom seasonality
        model.add_seasonality(name='monthly', period=30.5, fourier_order=5)
        model.add_seasonality(name='quarterly', period=91.25, fourier_order=3)
        
        return model
    
    def create_optimized_prophet_model(self, config_override: Dict = None):
        """Create optimized Prophet model with hyperparameter tuning."""
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet not available")
        
        config = {
            'daily_seasonality': False,
            'weekly_seasonality': True,
            'yearly_seasonality': True,
            'seasonality_mode': 'multiplicative',
            'changepoint_prior_scale': 0.12,
            'seasonality_prior_scale': 20.0,
            'holidays_prior_scale': 15.0,
            'interval_width': 0.8,
            'n_changepoints': 30
        }
        
        if config_override:
            config.update(config_override)
        
        model = Prophet(**config)
        
        # Add multiple seasonalities
        model.add_seasonality(name='monthly', period=30.5, fourier_order=8)
        model.add_seasonality(name='quarterly', period=91.25, fourier_order=5)
        model.add_seasonality(name='crypto_cycle', period=1460, fourier_order=3)  # 4-year cycle
        
        return model
    
    def prepare_prophet_data(self, data: pd.DataFrame, include_regressors: bool = False) -> pd.DataFrame:
        """Prepare data for Prophet training."""
        # Basic Prophet format
        prophet_data = pd.DataFrame({
            'ds': data.index,
            'y': data['Close'].values
        })
        
        if include_regressors:
            # Add volume regressor (normalized)
            if 'Volume' in data.columns:
                volume_norm = (data['Volume'] - data['Volume'].mean()) / data['Volume'].std()
                prophet_data['volume'] = volume_norm.fillna(0)
            
            # Add volatility regressor
            volatility = data['Close'].rolling(window=7).std()
            vol_norm = (volatility - volatility.mean()) / volatility.std()
            prophet_data['volatility'] = vol_norm.fillna(0)
            
            # Add price momentum
            momentum = data['Close'].pct_change(periods=5)
            momentum_norm = (momentum - momentum.mean()) / momentum.std()
            prophet_data['momentum'] = momentum_norm.fillna(0)
        
        # Remove any remaining NaN values
        prophet_data = prophet_data.fillna(0)
        
        return prophet_data
    
    def train_and_store_model(self, 
                             data: pd.DataFrame, 
                             variant: str, 
                             validation_split: float = 0.2) -> str:
        """
        Train a Prophet model variant and store it using the storage manager.
        
        Args:
            data: Historical ETH price data
            variant: Model variant ('basic', 'enhanced', 'optimized')
            validation_split: Fraction for validation
            
        Returns:
            model_id: Identifier of stored model
        """
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet not available")
        
        # Split data
        split_idx = int(len(data) * (1 - validation_split))
        train_data = data.iloc[:split_idx]
        val_data = data.iloc[split_idx:]
        
        # Create model based on variant
        if variant == 'basic':
            model = self.create_basic_prophet_model()
            include_regressors = False
        elif variant == 'enhanced':
            model = self.create_enhanced_prophet_model()
            include_regressors = True
        elif variant == 'optimized':
            model = self.create_optimized_prophet_model()
            include_regressors = True
        else:
            raise ValueError(f"Unknown variant: {variant}")
        
        # Prepare training data
        prophet_train = self.prepare_prophet_data(train_data, include_regressors)
        
        # Add regressors to model if needed
        if include_regressors and variant != 'basic':
            if 'volume' in prophet_train.columns:
                model.add_regressor('volume', prior_scale=10.0)
            if 'volatility' in prophet_train.columns:
                model.add_regressor('volatility', prior_scale=5.0)
            if 'momentum' in prophet_train.columns:
                model.add_regressor('momentum', prior_scale=8.0)
        
        # Train model
        print(f"Training {variant} Prophet model...")
        model.fit(prophet_train)
        
        # Validate model
        val_periods = len(val_data)
        if include_regressors and variant != 'basic':
            # Create future dataframe with regressors for validation
            prophet_val = self.prepare_prophet_data(
                pd.concat([train_data, val_data]), include_regressors
            )
            future = prophet_val.tail(val_periods)[['ds'] + [col for col in prophet_val.columns if col not in ['ds', 'y']]]
        else:
            future = model.make_future_dataframe(periods=val_periods, freq='D')
        
        # Generate predictions
        forecast = model.predict(future.tail(val_periods))
        
        # Calculate performance metrics
        actual_prices = val_data['Close'].values
        predicted_prices = forecast['yhat'].values
        
        mape = np.mean(np.abs((actual_prices - predicted_prices) / actual_prices)) * 100
        rmse = np.sqrt(np.mean((actual_prices - predicted_prices) ** 2))
        mae = np.mean(np.abs(actual_prices - predicted_prices))
        
        # Calculate directional accuracy
        actual_direction = np.sign(np.diff(actual_prices))
        predicted_direction = np.sign(np.diff(predicted_prices))
        directional_accuracy = np.mean(actual_direction == predicted_direction) * 100
        
        performance_metrics = {
            'mape': mape,
            'rmse': rmse,
            'mae': mae,
            'directional_accuracy': directional_accuracy,
            'validation_periods': val_periods,
            'training_periods': len(train_data)
        }
        
        # Create model configuration
        model_config = {
            'variant': variant,
            'include_regressors': include_regressors,
            'validation_split': validation_split,
            'prophet_params': {
                'daily_seasonality': model.daily_seasonality,
                'weekly_seasonality': model.weekly_seasonality,
                'yearly_seasonality': model.yearly_seasonality,
                'seasonality_mode': model.seasonality_mode,
                'changepoint_prior_scale': model.changepoint_prior_scale,
                'seasonality_prior_scale': model.seasonality_prior_scale,
                'interval_width': model.interval_width
            }
        }
        
        # Store model using storage manager
        model_id = self.storage_manager.store_model(
            model=model,
            methodology='prophet',
            asset='ETH',
            model_config=model_config,
            performance_metrics=performance_metrics,
            description=f'ETH Prophet {variant} model with {len(train_data)} training periods',
            variant=variant,
            tags=['eth', 'prophet', variant, 'time_series']
        )
        
        # Store performance in comparison database
        self._store_performance_metrics(model_id, variant, performance_metrics)
        
        # Store predictions for analysis
        self._store_predictions(model_id, val_data.index, actual_prices, predicted_prices, 
                              forecast['yhat_lower'].values, forecast['yhat_upper'].values)
        
        print(f"✅ {variant.title()} Prophet model trained and stored successfully!")
        print(f"   Model ID: {model_id}")
        print(f"   MAPE: {mape:.2f}%")
        print(f"   RMSE: {rmse:.2f}")
        print(f"   Directional Accuracy: {directional_accuracy:.1f}%")
        
        return model_id
    
    def _store_performance_metrics(self, model_id: str, variant: str, metrics: Dict[str, float]):
        """Store performance metrics in comparison database."""
        with sqlite3.connect(self.db_path) as conn:
            timestamp = datetime.now().isoformat()
            for metric_name, metric_value in metrics.items():
                conn.execute("""
                    INSERT INTO model_performance 
                    (model_id, model_variant, metric_name, metric_value, created_at, data_period)
                    VALUES (?, ?, ?, ?, ?, ?)
                """, (model_id, variant, metric_name, metric_value, timestamp, "validation"))
    
    def _store_predictions(self, model_id: str, dates, actual_prices, predicted_prices, 
                          lower_bounds, upper_bounds):
        """Store prediction results for analysis."""
        with sqlite3.connect(self.db_path) as conn:
            timestamp = datetime.now().isoformat()
            for i, date in enumerate(dates):
                conn.execute("""
                    INSERT INTO model_predictions
                    (model_id, prediction_date, actual_price, predicted_price, 
                     prediction_interval_lower, prediction_interval_upper, created_at)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                """, (model_id, date.strftime('%Y-%m-%d'), actual_prices[i], 
                     predicted_prices[i], lower_bounds[i], upper_bounds[i], timestamp))
    
    def train_all_variants(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, str]:
        """Train all three Prophet model variants."""
        variants = ['basic', 'enhanced', 'optimized']
        model_ids = {}
        
        print("Training all Prophet model variants...")
        print("=" * 50)
        
        for variant in variants:
            try:
                model_id = self.train_and_store_model(data, variant, validation_split)
                model_ids[variant] = model_id
                print()
            except Exception as e:
                print(f"❌ Failed to train {variant} model: {e}")
                model_ids[variant] = None
        
        return model_ids
    
    def compare_model_performance(self, model_ids: List[str] = None) -> pd.DataFrame:
        """Compare performance of different model variants."""
        if model_ids is None:
            # Get latest models of each variant
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.execute("""
                    SELECT DISTINCT model_id, model_variant 
                    FROM model_performance 
                    ORDER BY created_at DESC
                """)
                model_data = cursor.fetchall()
                model_ids = [row[0] for row in model_data]
        
        # Get performance metrics
        with sqlite3.connect(self.db_path) as conn:
            query = """
                SELECT model_id, model_variant, metric_name, metric_value
                FROM model_performance 
                WHERE model_id IN ({})
            """.format(','.join(['?' for _ in model_ids]))
            
            df = pd.read_sql_query(query, conn, params=model_ids)
        
        # Pivot to get metrics as columns
        comparison_df = df.pivot_table(
            index=['model_id', 'model_variant'], 
            columns='metric_name', 
            values='metric_value'
        ).reset_index()
        
        return comparison_df
    
    def load_model_by_variant(self, variant: str) -> Tuple[Any, Any]:
        """Load the latest model of a specific variant."""
        try:
            return self.storage_manager.load_latest_model('prophet', 'ETH')
        except ValueError:
            print(f"No {variant} Prophet models found for ETH")
            return None, None
    
    def generate_sample_data(self, periods: int = 365, start_price: float = 3000) -> pd.DataFrame:
        """Generate sample ETH price data for testing."""
        dates = pd.date_range(start='2022-01-01', periods=periods, freq='D')
        
        # Generate realistic price movements
        returns = np.random.normal(0.001, 0.04, periods)  # Daily returns
        prices = [start_price]
        
        for ret in returns[1:]:
            prices.append(prices[-1] * (1 + ret))
        
        # Add some volatility clustering
        volatility = np.random.gamma(2, 0.02, periods)
        prices = np.array(prices) * (1 + np.random.normal(0, volatility))
        
        # Ensure positive prices
        prices = np.maximum(prices, 100)
        
        # Create OHLCV data
        data = pd.DataFrame({
            'Open': prices * np.random.uniform(0.995, 1.005, periods),
            'High': prices * np.random.uniform(1.001, 1.02, periods),
            'Low': prices * np.random.uniform(0.98, 0.999, periods),
            'Close': prices,
            'Volume': np.random.lognormal(15, 0.5, periods)
        }, index=dates)
        
        # Ensure OHLC relationships
        data['High'] = np.maximum.reduce([data['Open'], data['High'], data['Close']])
        data['Low'] = np.minimum.reduce([data['Open'], data['Low'], data['Close']])
        
        return data


def demo_storage_framework():
    """Demonstrate the enhanced framework with storage."""
    print("ETH Prophet Framework with Storage Management")
    print("=" * 60)
    
    # Initialize framework
    framework = ETHProphetFrameworkWithStorage()
    
    # Generate sample data
    print("\n1. Generating sample ETH data...")
    sample_data = framework.generate_sample_data(periods=500)
    print(f"   Generated {len(sample_data)} days of sample data")
    print(f"   Price range: ${sample_data['Close'].min():.0f} - ${sample_data['Close'].max():.0f}")
    
    # Train all model variants
    print("\n2. Training all Prophet model variants...")
    model_ids = framework.train_all_variants(sample_data)
    
    # Compare performance
    print("\n3. Model Performance Comparison:")
    comparison = framework.compare_model_performance()
    if not comparison.empty:
        print(comparison.to_string(index=False))
    
    # Show storage summary
    print("\n4. Storage Summary:")
    framework.storage_manager.print_storage_summary()
    
    # List stored models
    print("\n5. Stored Prophet Models:")
    prophet_models = framework.storage_manager.list_models(methodology='prophet', asset='ETH')
    
    for model in prophet_models:
        print(f"   {model.model_id}: {model.description}")
        print(f"      Version: v{model.version:03d}, Size: {model.file_size/1024:.1f}KB")
        if model.performance_metrics:
            mape = model.performance_metrics.get('mape', 'N/A')
            print(f"      MAPE: {mape}")
        print()
    
    return framework, model_ids


if __name__ == "__main__":
    # Run demonstration
    framework, model_ids = demo_storage_framework()
    
    print("\n✅ ETH Prophet Framework with Storage completed successfully!")
    print("\nNext steps:")
    print("- Load models: framework.storage_manager.load_model(model_id)")
    print("- Compare performance: framework.compare_model_performance()")
    print("- Train new variants with real data")
