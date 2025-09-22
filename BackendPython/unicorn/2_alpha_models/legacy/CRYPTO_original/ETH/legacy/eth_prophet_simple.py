"""
ETH Prophet Framework - Simple Working Implementation

A simplified but fully functional ETH Prophet framework with three model variants.
This version focuses on core functionality without complex features.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime
import sqlite3
import json
import warnings
from pathlib import Path

# Prophet import with error handling
try:
    from prophet import Prophet
    PROPHET_AVAILABLE = True
    print("✅ Prophet available")
except ImportError:
    print("❌ Prophet not installed. Install with: pip install prophet")
    PROPHET_AVAILABLE = False

warnings.filterwarnings('ignore')

class ETHProphetModel:
    """Simple Prophet-based forecasting model for ETH."""
    
    def __init__(self, config: Dict[str, Any], model_variant: str = "basic"):
        if not PROPHET_AVAILABLE:
            raise ImportError("Prophet is required but not installed")
            
        self.asset_name = "ETH"
        self.model_variant = model_variant
        self.config = config
        self.model = None
        self.is_trained = False
        
    def prepare_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Prepare data in Prophet format."""
        return pd.DataFrame({
            'ds': data.index,
            'y': data['Close']
        })
    
    def train(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Train the Prophet model."""
        try:
            # Prepare data
            prophet_data = self.prepare_data(data)
            
            # Initialize and fit Prophet model
            self.model = Prophet(**self.config)
            self.model.fit(prophet_data)
            self.is_trained = True
            
            return {
                'success': True,
                'model_variant': self.model_variant,
                'training_samples': len(prophet_data)
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def predict(self, periods: int = 30) -> pd.DataFrame:
        """Make predictions."""
        if not self.is_trained:
            raise ValueError("Model must be trained before making predictions")
        
        future = self.model.make_future_dataframe(periods=periods)
        forecast = self.model.predict(future)
        return forecast

class ETHProphetFramework:
    """Framework for comparing three ETH Prophet models."""
    
    def __init__(self):
        self.results = {}
        
        # Database setup
        self.db_path = Path(__file__).parent / "eth_prophet_simple.db"
        self._setup_database()
        
    def _setup_database(self):
        """Setup SQLite database for results."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS experiment_results (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    experiment_id TEXT NOT NULL,
                    model_variant TEXT NOT NULL,
                    mape REAL,
                    mae REAL,
                    rmse REAL,
                    r2 REAL,
                    directional_accuracy REAL,
                    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            conn.commit()
    
    def create_models(self) -> Dict[str, ETHProphetModel]:
        """Create three model variants."""
        models = {}
        
        # Basic model - conservative settings
        basic_config = {
            'seasonality_mode': 'additive',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.05,
            'seasonality_prior_scale': 10.0
        }
        models['basic'] = ETHProphetModel(basic_config, 'basic')
        
        # Enhanced model - more flexible
        enhanced_config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.1,
            'seasonality_prior_scale': 15.0
        }
        models['enhanced'] = ETHProphetModel(enhanced_config, 'enhanced')
        
        # Optimized model - aggressive settings
        optimized_config = {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': True,
            'changepoint_prior_scale': 0.15,
            'seasonality_prior_scale': 20.0,
            'changepoint_range': 0.9
        }
        models['optimized'] = ETHProphetModel(optimized_config, 'optimized')
        
        return models
    
    def calculate_metrics(self, predictions: np.array, actuals: np.array) -> Dict[str, float]:
        """Calculate performance metrics."""
        # Remove NaN values
        mask = ~(np.isnan(predictions) | np.isnan(actuals))
        pred_clean = predictions[mask]
        actual_clean = actuals[mask]
        
        if len(pred_clean) == 0:
            return {'error': 'No valid data points'}
        
        # Calculate metrics
        mae = np.mean(np.abs(actual_clean - pred_clean))
        mse = np.mean((actual_clean - pred_clean) ** 2)
        rmse = np.sqrt(mse)
        
        # MAPE with protection against division by zero
        mape_values = np.abs((actual_clean - pred_clean) / np.where(actual_clean != 0, actual_clean, 1))
        mape = np.mean(mape_values) * 100
        
        # R-squared
        ss_res = np.sum((actual_clean - pred_clean) ** 2)
        ss_tot = np.sum((actual_clean - np.mean(actual_clean)) ** 2)
        r2 = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0
        
        # Directional accuracy
        actual_diff = np.diff(actual_clean)
        pred_diff = np.diff(pred_clean)
        directional_accuracy = np.mean(np.sign(actual_diff) == np.sign(pred_diff)) * 100
        
        return {
            'mae': float(mae),
            'mse': float(mse),
            'rmse': float(rmse),
            'mape': float(mape),
            'r2': float(r2),
            'directional_accuracy': float(directional_accuracy)
        }
    
    def train_and_compare(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """Train all models and compare performance."""
        
        experiment_id = f"eth_exp_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        print(f"🚀 Starting Experiment: {experiment_id}")
        print("=" * 60)
        
        # Split data
        split_idx = int(len(data) * (1 - validation_split))
        train_data = data.iloc[:split_idx]
        val_data = data.iloc[split_idx:]
        
        print(f"📊 Training data: {len(train_data)} days")
        print(f"📊 Validation data: {len(val_data)} days")
        
        # Create models
        models = self.create_models()
        results = {'experiment_id': experiment_id, 'models': {}}
        
        best_mape = float('inf')
        best_model = None
        
        for model_name, model in models.items():
            print(f"\n🔧 Training {model_name.title()} Model...")
            print("-" * 40)
            
            try:
                # Train model
                train_result = model.train(train_data)
                
                if train_result['success']:
                    # Make predictions
                    forecast = model.predict(periods=len(val_data))
                    
                    # Extract predictions for validation period
                    val_predictions = forecast['yhat'].tail(len(val_data)).values
                    val_actuals = val_data['Close'].values
                    
                    # Calculate metrics
                    metrics = self.calculate_metrics(val_predictions, val_actuals)
                    
                    if 'error' not in metrics:
                        # Store results
                        results['models'][model_name] = {
                            'model': model,
                            'metrics': metrics,
                            'config': model.config
                        }
                        
                        # Track best model
                        if metrics['mape'] < best_mape:
                            best_mape = metrics['mape']
                            best_model = model_name
                        
                        # Save to database
                        self._save_results(experiment_id, model_name, metrics)
                        
                        # Print results
                        print(f"✅ Results:")
                        print(f"   MAPE: {metrics['mape']:.2f}%")
                        print(f"   MAE: {metrics['mae']:.2f}")
                        print(f"   RMSE: {metrics['rmse']:.2f}")
                        print(f"   R²: {metrics['r2']:.4f}")
                        print(f"   Directional Accuracy: {metrics['directional_accuracy']:.1f}%")
                    else:
                        print(f"❌ Metrics calculation failed: {metrics['error']}")
                else:
                    print(f"❌ Training failed: {train_result.get('error', 'Unknown error')}")
                    
            except Exception as e:
                print(f"❌ Error with {model_name}: {str(e)}")
        
        results['best_model'] = best_model
        self.results = results
        
        print(f"\n🏆 Best Model: {best_model}")
        print(f"🎯 Best MAPE: {best_mape:.2f}%")
        
        return results
    
    def _save_results(self, experiment_id: str, model_name: str, metrics: Dict[str, float]):
        """Save results to database."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute("""
                INSERT INTO experiment_results 
                (experiment_id, model_variant, mape, mae, rmse, r2, directional_accuracy)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            """, (
                experiment_id, model_name,
                metrics['mape'], metrics['mae'], metrics['rmse'],
                metrics['r2'], metrics['directional_accuracy']
            ))
            conn.commit()
    
    def generate_report(self) -> str:
        """Generate comparison report."""
        if not self.results:
            return "No results available. Run train_and_compare() first."
        
        report = []
        report.append("ETH PROPHET MODELS COMPARISON REPORT")
        report.append("=" * 50)
        report.append(f"Experiment: {self.results['experiment_id']}")
        report.append(f"Best Model: {self.results['best_model']}")
        report.append("")
        
        for model_name, model_data in self.results['models'].items():
            metrics = model_data['metrics']
            report.append(f"{model_name.upper()} MODEL:")
            report.append(f"  MAPE: {metrics['mape']:.2f}%")
            report.append(f"  MAE: {metrics['mae']:.2f}")
            report.append(f"  RMSE: {metrics['rmse']:.2f}")
            report.append(f"  R²: {metrics['r2']:.4f}")
            report.append(f"  Directional Accuracy: {metrics['directional_accuracy']:.1f}%")
            report.append("")
        
        return "\n".join(report)
    
    def get_history(self) -> pd.DataFrame:
        """Get historical results from database."""
        with sqlite3.connect(self.db_path) as conn:
            return pd.read_sql_query(
                "SELECT * FROM experiment_results ORDER BY timestamp DESC", 
                conn
            )

def create_sample_data(days: int = 500) -> pd.DataFrame:
    """Create sample ETH price data."""
    np.random.seed(42)
    dates = pd.date_range(start='2023-01-01', periods=days, freq='D')
    
    # Generate realistic price series
    base_price = 2000
    trend = np.linspace(0, 500, days)
    noise = np.random.normal(0, 50, days).cumsum() * 0.1
    
    prices = base_price + trend + noise
    
    # Create OHLCV data
    data = pd.DataFrame(index=dates)
    data['Close'] = prices
    data['Open'] = data['Close'].shift(1) + np.random.normal(0, 5, days)
    data['High'] = np.maximum(data['Open'], data['Close']) + np.abs(np.random.normal(0, 10, days))
    data['Low'] = np.minimum(data['Open'], data['Close']) - np.abs(np.random.normal(0, 10, days))
    data['Volume'] = np.random.lognormal(15, 0.3, days)
    
    # Ensure positive prices
    for col in ['Open', 'High', 'Low', 'Close']:
        data[col] = np.maximum(data[col], 100)
    
    return data.dropna()

def main():
    """Main function to run the framework."""
    print("ETH Prophet Framework - Simple Implementation")
    print("=" * 55)
    
    if not PROPHET_AVAILABLE:
        print("❌ Prophet not available. Exiting.")
        return
    
    # Create sample data
    print("📊 Creating sample ETH data...")
    data = create_sample_data(500)
    print(f"   Generated {len(data)} days of data")
    print(f"   Price range: ${data['Close'].min():.2f} - ${data['Close'].max():.2f}")
    
    # Initialize framework
    framework = ETHProphetFramework()
    print("🔧 Framework initialized")
    
    # Train and compare models
    results = framework.train_and_compare(data, validation_split=0.2)
    
    # Generate report
    if results['models']:
        print("\n" + "=" * 60)
        print(framework.generate_report())
        
        # Show database contents
        history = framework.get_history()
        print(f"📊 Database contains {len(history)} experiment records")
        
        print("\n🚀 Framework test completed successfully!")
        
        # Save best model info
        best_model = results['best_model']
        if best_model:
            best_metrics = results['models'][best_model]['metrics']
            print(f"\n💾 Best Model Summary:")
            print(f"   Model: {best_model}")
            print(f"   MAPE: {best_metrics['mape']:.2f}%")
            print(f"   R²: {best_metrics['r2']:.4f}")
            print(f"   Ready for deployment!")
    else:
        print("\n❌ No models trained successfully")

if __name__ == "__main__":
    main()
