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
    
    def create_basic_prophet_model(self, config_override: Dict = None) -> Prophet:
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
    
    def create_enhanced_prophet_model(self, config_override: Dict = None) -> Prophet:
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
    
    def create_optimized_prophet_model(self, config_override: Dict = None) -> Prophet:
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
            'prophet_params': model.__dict__.copy()
        }\n        \n        # Clean prophet_params (remove non-serializable objects)\n        model_config['prophet_params'] = {\n            k: v for k, v in model_config['prophet_params'].items() \n            if isinstance(v, (str, int, float, bool, list, dict, type(None)))\n        }\n        \n        # Store model using storage manager\n        model_id = self.storage_manager.store_model(\n            model=model,\n            methodology='prophet',\n            asset='ETH',\n            model_config=model_config,\n            performance_metrics=performance_metrics,\n            description=f'ETH Prophet {variant} model with {len(train_data)} training periods',\n            variant=variant,\n            tags=['eth', 'prophet', variant, 'time_series']\n        )\n        \n        # Store performance in comparison database\n        self._store_performance_metrics(model_id, variant, performance_metrics)\n        \n        # Store predictions for analysis\n        self._store_predictions(model_id, val_data.index, actual_prices, predicted_prices, \n                              forecast['yhat_lower'].values, forecast['yhat_upper'].values)\n        \n        print(f\"✅ {variant.title()} Prophet model trained and stored successfully!\")\n        print(f\"   Model ID: {model_id}\")\n        print(f\"   MAPE: {mape:.2f}%\")\n        print(f\"   RMSE: {rmse:.2f}\")\n        print(f\"   Directional Accuracy: {directional_accuracy:.1f}%\")\n        \n        return model_id\n    \n    def _store_performance_metrics(self, model_id: str, variant: str, metrics: Dict[str, float]):\n        \"\"\"Store performance metrics in comparison database.\"\"\"\n        with sqlite3.connect(self.db_path) as conn:\n            timestamp = datetime.now().isoformat()\n            for metric_name, metric_value in metrics.items():\n                conn.execute(\"\"\"\n                    INSERT INTO model_performance \n                    (model_id, model_variant, metric_name, metric_value, created_at, data_period)\n                    VALUES (?, ?, ?, ?, ?, ?)\n                \"\"\", (model_id, variant, metric_name, metric_value, timestamp, \"validation\"))\n    \n    def _store_predictions(self, model_id: str, dates, actual_prices, predicted_prices, \n                          lower_bounds, upper_bounds):\n        \"\"\"Store prediction results for analysis.\"\"\"\n        with sqlite3.connect(self.db_path) as conn:\n            timestamp = datetime.now().isoformat()\n            for i, date in enumerate(dates):\n                conn.execute(\"\"\"\n                    INSERT INTO model_predictions\n                    (model_id, prediction_date, actual_price, predicted_price, \n                     prediction_interval_lower, prediction_interval_upper, created_at)\n                    VALUES (?, ?, ?, ?, ?, ?, ?)\n                \"\"\", (model_id, date.strftime('%Y-%m-%d'), actual_prices[i], \n                     predicted_prices[i], lower_bounds[i], upper_bounds[i], timestamp))\n    \n    def train_all_variants(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, str]:\n        \"\"\"Train all three Prophet model variants.\"\"\"\n        variants = ['basic', 'enhanced', 'optimized']\n        model_ids = {}\n        \n        print(\"Training all Prophet model variants...\")\n        print(\"=\" * 50)\n        \n        for variant in variants:\n            try:\n                model_id = self.train_and_store_model(data, variant, validation_split)\n                model_ids[variant] = model_id\n                print()\n            except Exception as e:\n                print(f\"❌ Failed to train {variant} model: {e}\")\n                model_ids[variant] = None\n        \n        return model_ids\n    \n    def compare_model_performance(self, model_ids: List[str] = None) -> pd.DataFrame:\n        \"\"\"Compare performance of different model variants.\"\"\"\n        if model_ids is None:\n            # Get latest models of each variant\n            with sqlite3.connect(self.db_path) as conn:\n                cursor = conn.execute(\"\"\"\n                    SELECT DISTINCT model_id, model_variant \n                    FROM model_performance \n                    ORDER BY created_at DESC\n                \"\"\")\n                model_data = cursor.fetchall()\n                model_ids = [row[0] for row in model_data]\n        \n        # Get performance metrics\n        with sqlite3.connect(self.db_path) as conn:\n            query = \"\"\"\n                SELECT model_id, model_variant, metric_name, metric_value\n                FROM model_performance \n                WHERE model_id IN ({})\n            \"\"\".format(','.join(['?' for _ in model_ids]))\n            \n            df = pd.read_sql_query(query, conn, params=model_ids)\n        \n        # Pivot to get metrics as columns\n        comparison_df = df.pivot_table(\n            index=['model_id', 'model_variant'], \n            columns='metric_name', \n            values='metric_value'\n        ).reset_index()\n        \n        return comparison_df\n    \n    def load_model_by_variant(self, variant: str) -> Tuple[Any, Any]:\n        \"\"\"Load the latest model of a specific variant.\"\"\"\n        try:\n            return self.storage_manager.load_latest_model('prophet', 'ETH')\n        except ValueError:\n            print(f\"No {variant} Prophet models found for ETH\")\n            return None, None\n    \n    def generate_sample_data(self, periods: int = 365, start_price: float = 3000) -> pd.DataFrame:\n        \"\"\"Generate sample ETH price data for testing.\"\"\"\n        dates = pd.date_range(start='2022-01-01', periods=periods, freq='D')\n        \n        # Generate realistic price movements\n        returns = np.random.normal(0.001, 0.04, periods)  # Daily returns\n        prices = [start_price]\n        \n        for ret in returns[1:]:\n            prices.append(prices[-1] * (1 + ret))\n        \n        # Add some volatility clustering\n        volatility = np.random.gamma(2, 0.02, periods)\n        prices = np.array(prices) * (1 + np.random.normal(0, volatility))\n        \n        # Ensure positive prices\n        prices = np.maximum(prices, 100)\n        \n        # Create OHLCV data\n        data = pd.DataFrame({\n            'Open': prices * np.random.uniform(0.995, 1.005, periods),\n            'High': prices * np.random.uniform(1.001, 1.02, periods),\n            'Low': prices * np.random.uniform(0.98, 0.999, periods),\n            'Close': prices,\n            'Volume': np.random.lognormal(15, 0.5, periods)\n        }, index=dates)\n        \n        # Ensure OHLC relationships\n        data['High'] = np.maximum.reduce([data['Open'], data['High'], data['Close']])\n        data['Low'] = np.minimum.reduce([data['Open'], data['Low'], data['Close']])\n        \n        return data\n\n\ndef demo_storage_framework():\n    \"\"\"Demonstrate the enhanced framework with storage.\"\"\"\n    print(\"ETH Prophet Framework with Storage Management\")\n    print(\"=\" * 60)\n    \n    # Initialize framework\n    framework = ETHProphetFrameworkWithStorage()\n    \n    # Generate sample data\n    print(\"\\n1. Generating sample ETH data...\")\n    sample_data = framework.generate_sample_data(periods=500)\n    print(f\"   Generated {len(sample_data)} days of sample data\")\n    print(f\"   Price range: ${sample_data['Close'].min():.0f} - ${sample_data['Close'].max():.0f}\")\n    \n    # Train all model variants\n    print(\"\\n2. Training all Prophet model variants...\")\n    model_ids = framework.train_all_variants(sample_data)\n    \n    # Compare performance\n    print(\"\\n3. Model Performance Comparison:\")\n    comparison = framework.compare_model_performance()\n    if not comparison.empty:\n        print(comparison.to_string(index=False))\n    \n    # Show storage summary\n    print(\"\\n4. Storage Summary:\")\n    framework.storage_manager.print_storage_summary()\n    \n    # List stored models\n    print(\"\\n5. Stored Prophet Models:\")\n    prophet_models = framework.storage_manager.list_models(methodology='prophet', asset='ETH')\n    \n    for model in prophet_models:\n        print(f\"   {model.model_id}: {model.description}\")\n        print(f\"      Version: v{model.version:03d}, Size: {model.file_size/1024:.1f}KB\")\n        if model.performance_metrics:\n            mape = model.performance_metrics.get('mape', 'N/A')\n            print(f\"      MAPE: {mape}\")\n        print()\n    \n    return framework, model_ids\n\n\nif __name__ == \"__main__\":\n    # Run demonstration\n    framework, model_ids = demo_storage_framework()\n    \n    print(\"\\n✅ ETH Prophet Framework with Storage completed successfully!\")\n    print(\"\\nNext steps:\")\n    print(\"- Load models: framework.storage_manager.load_model(model_id)\")\n    print(\"- Compare performance: framework.compare_model_performance()\")\n    print(\"- Train new variants with real data\")
