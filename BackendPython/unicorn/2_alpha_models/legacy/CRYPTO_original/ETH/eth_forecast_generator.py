"""
ETH Forecast Generator - Production System

This module generates forecasts for ETH trading using existing model frameworks
with production-grade forecast storage and retrieval. Implements periodic model
retraining (every 10 iterations) and best model selection for production use.

Features:
- Asset-specific forecast directories
- Periodic model retraining schedule
- Best model selection and production designation
- JSON forecast storage with metadata
- Performance tracking and model comparison
- Scalable multi-timeframe architecture
"""

import pandas as pd
import numpy as np
import json
import sys
import os
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, Any, List, Optional, Tuple
import sqlite3
import pickle
import warnings
from dataclasses import dataclass, asdict
from enum import Enum

# Add parent directories to path
sys.path.append(str(Path(__file__).parent))
sys.path.append(str(Path(__file__).parent.parent.parent.parent))

# Import existing frameworks
from eth_prophet_framework import ETHProphetFramework
from eth_xgboost_framework import ETHXGBoostFramework
from eth_ensemble_framework import ETHEnsembleFramework

warnings.filterwarnings('ignore', category=RuntimeWarning)

class ModelStatus(Enum):
    TRAINING = "training"
    PRODUCTION = "production"
    DEPRECATED = "deprecated"
    FAILED = "failed"

@dataclass
class ForecastMetadata:
    """Metadata for forecast records."""
    asset: str
    timeframe: str
    model_type: str
    model_version: str
    forecast_timestamp: datetime
    forecast_horizon: int
    confidence_score: float
    model_performance: Dict[str, float]
    data_quality_score: float
    is_production: bool = False

@dataclass
class ForecastRecord:
    """Complete forecast record with predictions and metadata."""
    metadata: ForecastMetadata
    predictions: Dict[str, Any]  # timestamp -> prediction data
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            'metadata': asdict(self.metadata),
            'predictions': self.predictions
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'ForecastRecord':
        """Create from dictionary loaded from JSON."""
        metadata_dict = data['metadata']
        metadata_dict['forecast_timestamp'] = pd.to_datetime(metadata_dict['forecast_timestamp'])
        metadata = ForecastMetadata(**metadata_dict)
        return cls(metadata=metadata, predictions=data['predictions'])


class ETHForecastGenerator:
    """
    Production ETH forecast generator with model management and periodic retraining.
    """
    
    def __init__(self, asset: str = "ETH"):
        self.asset = asset
        self.base_path = Path(__file__).parent
        self.forecasts_dir = self.base_path / "forecasts"
        self.models_dir = self.base_path / "models"
        self.performance_db = self.base_path / "forecast_performance.db"
        
        # Create directories
        self.forecasts_dir.mkdir(exist_ok=True)
        self.models_dir.mkdir(exist_ok=True)
        
        # Timeframe configurations
        self.timeframe_configs = {
            '1min': {
                'retrain_interval': 10,  # Every 10 minutes
                'forecast_horizon': 60,  # 1 hour ahead
                'max_history_hours': 48,
                'performance_window': 1000,
                'model_frameworks': ['prophet', 'xgboost', 'ensemble']
            },
            '1hour': {
                'retrain_interval': 10,  # Every 10 hours  
                'forecast_horizon': 24,  # 24 hours ahead
                'max_history_days': 14,
                'performance_window': 240,
                'model_frameworks': ['prophet', 'xgboost', 'ensemble']
            },
            '1day': {
                'retrain_interval': 10,  # Every 10 days
                'forecast_horizon': 30,  # 30 days ahead
                'max_history_days': 90,
                'performance_window': 60,
                'model_frameworks': ['prophet', 'xgboost', 'ensemble']
            }
        }
        
        # Initialize model frameworks
        self.model_frameworks = {
            '1min': {
                'prophet': ETHProphetFramework(),
                'xgboost': ETHXGBoostFramework(),
                'ensemble': ETHEnsembleFramework()
            },
            '1hour': {
                'prophet': ETHProphetFramework(),
                'xgboost': ETHXGBoostFramework(), 
                'ensemble': ETHEnsembleFramework()
            },
            '1day': {
                'prophet': ETHProphetFramework(),
                'xgboost': ETHXGBoostFramework(),
                'ensemble': ETHEnsembleFramework()
            }
        }
        
        # Initialize performance tracking
        self._init_performance_db()
        
        # Load existing model states
        self.model_states = self._load_model_states()
        
    def _init_performance_db(self):
        """Initialize SQLite database for performance tracking."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS model_performance (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                asset TEXT NOT NULL,
                timeframe TEXT NOT NULL,
                model_type TEXT NOT NULL,
                model_version TEXT NOT NULL,
                timestamp DATETIME NOT NULL,
                mae REAL,
                mse REAL,
                mape REAL,
                r2_score REAL,
                directional_accuracy REAL,
                confidence_score REAL,
                is_production BOOLEAN DEFAULT FALSE,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS forecast_requests (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                asset TEXT NOT NULL,
                timeframe TEXT NOT NULL,
                request_timestamp DATETIME NOT NULL,
                iteration_count INTEGER NOT NULL,
                retrain_triggered BOOLEAN DEFAULT FALSE,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        conn.commit()
        conn.close()
        
    def _load_model_states(self) -> Dict[str, Dict[str, Any]]:
        """Load existing model states and iteration counts."""
        states_file = self.models_dir / "model_states.json"
        
        if states_file.exists():
            with open(states_file, 'r') as f:
                return json.load(f)
        else:
            # Initialize default states
            states = {}
            for timeframe in self.timeframe_configs.keys():
                states[timeframe] = {
                    'iteration_count': 0,
                    'last_retrain': None,
                    'production_model': None,
                    'models': {
                        'prophet': {'status': 'training', 'version': '1.0.0', 'performance': {}},
                        'xgboost': {'status': 'training', 'version': '1.0.0', 'performance': {}},
                        'ensemble': {'status': 'training', 'version': '1.0.0', 'performance': {}}
                    }
                }
            return states
    
    def _save_model_states(self):
        """Save model states to disk."""
        states_file = self.models_dir / "model_states.json"
        with open(states_file, 'w') as f:
            json.dump(self.model_states, f, indent=2, default=str)
    
    def should_retrain_models(self, timeframe: str) -> bool:
        """Check if models should be retrained based on iteration count."""
        state = self.model_states[timeframe]
        retrain_interval = self.timeframe_configs[timeframe]['retrain_interval']
        
        return state['iteration_count'] % retrain_interval == 0
    
    def get_production_model(self, timeframe: str) -> Optional[str]:
        """Get the current production model for a timeframe."""
        return self.model_states[timeframe].get('production_model')
    
    def set_production_model(self, timeframe: str, model_type: str):
        """Set the production model based on performance."""
        self.model_states[timeframe]['production_model'] = model_type
        
        # Update model statuses
        for model in self.model_states[timeframe]['models']:
            if model == model_type:
                self.model_states[timeframe]['models'][model]['status'] = 'production'
            else:
                self.model_states[timeframe]['models'][model]['status'] = 'deprecated'
                
        self._save_model_states()
    
    def generate_forecast(self, timeframe: str, market_data: pd.DataFrame,
                         force_retrain: bool = False) -> ForecastRecord:
        """
        Generate forecast for specified timeframe with automatic model management.
        """
        print(f"🔮 Generating {timeframe} forecast for {self.asset}...")
        
        # Update iteration count
        self.model_states[timeframe]['iteration_count'] += 1
        
        # Check if retraining is needed
        should_retrain = force_retrain or self.should_retrain_models(timeframe)
        
        if should_retrain:
            print(f"🔄 Retraining models for {timeframe} (iteration {self.model_states[timeframe]['iteration_count']})")
            self._retrain_models(timeframe, market_data)
        
        # Get production model or best available model
        production_model = self.get_production_model(timeframe)
        if not production_model:
            production_model = self._select_best_model(timeframe)
            self.set_production_model(timeframe, production_model)
        
        print(f"📊 Using production model: {production_model}")
        
        # Generate forecast using production model
        forecast_record = self._generate_model_forecast(
            timeframe, production_model, market_data
        )
        
        # Save forecast to disk
        self._save_forecast(forecast_record)
        
        # Update performance tracking
        self._track_forecast_request(timeframe, should_retrain)
        
        # Save updated states
        self._save_model_states()
        
        return forecast_record
    
    def _retrain_models(self, timeframe: str, market_data: pd.DataFrame):
        """Retrain all models for the timeframe."""
        config = self.timeframe_configs[timeframe]
        
        # Prepare data based on timeframe
        if timeframe == '1min':
            # Use last 48 hours for 1-minute models
            cutoff_time = market_data.index[-1] - timedelta(hours=config['max_history_hours'])
            training_data = market_data[market_data.index >= cutoff_time]
        else:
            # Use configured history for other timeframes
            max_days = config.get('max_history_days', 90)
            cutoff_time = market_data.index[-1] - timedelta(days=max_days)
            training_data = market_data[market_data.index >= cutoff_time]
        
        print(f"📈 Training data: {len(training_data)} records from {training_data.index[0]} to {training_data.index[-1]}")
        
        # Train each model framework
        for model_type in config['model_frameworks']:
            try:
                print(f"🧠 Training {model_type} model...")
                
                framework = self.model_frameworks[timeframe][model_type]
                
                # Train model using framework
                if model_type == 'prophet':
                    results = framework.train_enhanced_model(training_data)
                elif model_type == 'xgboost':
                    results = framework.train_optimized_model(training_data)
                elif model_type == 'ensemble':
                    results = framework.train_ensemble_model(training_data)
                
                # Update model version and performance
                current_version = self.model_states[timeframe]['models'][model_type]['version']
                new_version = self._increment_version(current_version)
                
                self.model_states[timeframe]['models'][model_type].update({
                    'version': new_version,
                    'status': 'training',
                    'performance': results.get('performance_metrics', {}),
                    'last_trained': datetime.now().isoformat()
                })
                
                # Save model to disk
                model_path = self.models_dir / f"{model_type}_{timeframe}_{new_version}.pkl"
                framework.save_model(str(model_path))
                
                print(f"✅ {model_type} model trained successfully (v{new_version})")
                
            except Exception as e:
                print(f"❌ Error training {model_type}: {e}")
                self.model_states[timeframe]['models'][model_type]['status'] = 'failed'
        
        # Update last retrain timestamp
        self.model_states[timeframe]['last_retrain'] = datetime.now().isoformat()
    
    def _select_best_model(self, timeframe: str) -> str:
        """Select the best performing model for production."""
        models = self.model_states[timeframe]['models']
        
        best_model = None
        best_score = float('-inf')
        
        for model_type, model_info in models.items():
            if model_info['status'] == 'failed':
                continue
                
            # Calculate composite performance score
            perf = model_info.get('performance', {})
            
            # Use R2 score for primary ranking, or MAPE if available
            if 'r2_score' in perf:
                score = perf['r2_score']
            elif 'mape' in perf:
                score = -perf['mape']  # Lower MAPE is better
            elif 'mae' in perf:
                score = -perf['mae']   # Lower MAE is better
            else:
                score = 0  # Default for untested models
            
            if score > best_score:
                best_score = score
                best_model = model_type
        
        if not best_model:
            # Fallback to ensemble if no clear winner
            best_model = 'ensemble' if 'ensemble' in models else list(models.keys())[0]
        
        print(f"🏆 Best model selected: {best_model} (score: {best_score:.4f})")
        return best_model
    
    def _generate_model_forecast(self, timeframe: str, model_type: str,
                               market_data: pd.DataFrame) -> ForecastRecord:
        """Generate forecast using specified model."""
        config = self.timeframe_configs[timeframe]
        framework = self.model_frameworks[timeframe][model_type]
        
        # Load the latest model version
        model_info = self.model_states[timeframe]['models'][model_type]
        model_version = model_info['version']
        
        model_path = self.models_dir / f"{model_type}_{timeframe}_{model_version}.pkl"
        if model_path.exists():
            framework.load_model(str(model_path))
        
        # Generate predictions
        horizon = config['forecast_horizon']
        predictions = framework.generate_forecast(market_data, periods=horizon)
        
        # Calculate confidence score
        if hasattr(predictions, 'confidence_intervals'):
            # Use prediction interval width as confidence indicator
            interval_width = np.mean(predictions['upper'] - predictions['lower']) / np.mean(predictions['yhat'])
            confidence_score = max(0, min(1, 1 - interval_width))
        else:
            confidence_score = 0.8  # Default confidence for models without intervals
        
        # Create forecast metadata
        metadata = ForecastMetadata(
            asset=self.asset,
            timeframe=timeframe,
            model_type=model_type,
            model_version=model_version,
            forecast_timestamp=datetime.now(),
            forecast_horizon=horizon,
            confidence_score=confidence_score,
            model_performance=model_info.get('performance', {}),
            data_quality_score=self._calculate_data_quality_score(market_data),
            is_production=True
        )
        
        # Format predictions
        forecast_data = {}
        if isinstance(predictions, pd.DataFrame):
            for i, (timestamp, row) in enumerate(predictions.iterrows()):
                forecast_data[timestamp.isoformat()] = {
                    'predicted_price': float(row.get('yhat', row.get('prediction', 0))),
                    'confidence_lower': float(row.get('yhat_lower', 0)),
                    'confidence_upper': float(row.get('yhat_upper', 0)),
                    'step_ahead': i + 1
                }
        else:
            # Handle single prediction case
            current_time = market_data.index[-1]
            for i in range(horizon):
                if timeframe == '1min':
                    future_time = current_time + pd.Timedelta(minutes=1)
                elif timeframe == '1hour':
                    future_time = current_time + pd.Timedelta(hours=1)
                else:  # 1day
                    future_time = current_time + pd.Timedelta(days=1)
                    
                forecast_data[future_time.isoformat()] = {
                    'predicted_price': float(predictions) if isinstance(predictions, (int, float)) else 0,
                    'confidence_lower': 0,
                    'confidence_upper': 0,
                    'step_ahead': i + 1
                }
        
        return ForecastRecord(metadata=metadata, predictions=forecast_data)
    
    def _calculate_data_quality_score(self, data: pd.DataFrame) -> float:
        """Calculate data quality score."""
        if len(data) == 0:
            return 0.0
        
        # Check for missing values
        missing_pct = data.isnull().sum().sum() / (len(data) * len(data.columns))
        
        # Check for price anomalies
        returns = data['close'].pct_change().dropna()
        extreme_returns = (np.abs(returns) > 0.5).sum() / len(returns)
        
        # Check data recency
        time_since_last = (datetime.now() - data.index[-1]).total_seconds() / 3600  # hours
        recency_score = max(0, 1 - time_since_last / 24)  # Penalty after 24 hours
        
        # Composite score
        score = (
            (1 - missing_pct) * 0.4 +       # 40% weight on completeness
            (1 - extreme_returns) * 0.3 +    # 30% weight on stability
            recency_score * 0.3               # 30% weight on recency
        )
        
        return max(0, min(1, score))
    
    def _save_forecast(self, forecast_record: ForecastRecord):
        """Save forecast to timeframe-specific directory."""
        timeframe = forecast_record.metadata.timeframe
        forecast_dir = self.forecasts_dir / timeframe
        forecast_dir.mkdir(exist_ok=True)
        
        # Generate filename with timestamp
        timestamp = forecast_record.metadata.forecast_timestamp.strftime('%Y%m%d_%H%M%S')
        filename = f"{self.asset}_{timeframe}_{timestamp}.json"
        filepath = forecast_dir / filename
        
        # Save forecast
        with open(filepath, 'w') as f:
            json.dump(forecast_record.to_dict(), f, indent=2, default=str)
        
        # Keep only recent forecasts (cleanup old files)
        self._cleanup_old_forecasts(timeframe)
        
        print(f"💾 Forecast saved: {filepath}")
    
    def _cleanup_old_forecasts(self, timeframe: str, max_files: int = 100):
        """Remove old forecast files to manage disk space."""
        forecast_dir = self.forecasts_dir / timeframe
        
        # Get all forecast files
        forecast_files = list(forecast_dir.glob(f"{self.asset}_{timeframe}_*.json"))
        
        if len(forecast_files) > max_files:
            # Sort by modification time and remove oldest
            forecast_files.sort(key=lambda x: x.stat().st_mtime)
            files_to_remove = forecast_files[:-max_files]
            
            for file_path in files_to_remove:
                file_path.unlink()
                
            print(f"🧹 Cleaned up {len(files_to_remove)} old forecast files for {timeframe}")
    
    def _track_forecast_request(self, timeframe: str, retrain_triggered: bool):
        """Track forecast request in database."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT INTO forecast_requests 
            (asset, timeframe, request_timestamp, iteration_count, retrain_triggered)
            VALUES (?, ?, ?, ?, ?)
        ''', (
            self.asset,
            timeframe, 
            datetime.now(),
            self.model_states[timeframe]['iteration_count'],
            retrain_triggered
        ))
        
        conn.commit()
        conn.close()
    
    def _increment_version(self, version: str) -> str:
        """Increment semantic version number."""
        try:
            parts = version.split('.')
            major, minor, patch = int(parts[0]), int(parts[1]), int(parts[2])
            return f"{major}.{minor}.{patch + 1}"
        except:
            return "1.0.1"
    
    def get_latest_forecast(self, timeframe: str) -> Optional[ForecastRecord]:
        """Retrieve the latest forecast for a timeframe."""
        forecast_dir = self.forecasts_dir / timeframe
        
        if not forecast_dir.exists():
            return None
        
        # Find the most recent forecast file
        forecast_files = list(forecast_dir.glob(f"{self.asset}_{timeframe}_*.json"))
        
        if not forecast_files:
            return None
        
        # Sort by filename (which includes timestamp)
        latest_file = sorted(forecast_files)[-1]
        
        try:
            with open(latest_file, 'r') as f:
                data = json.load(f)
            return ForecastRecord.from_dict(data)
        except Exception as e:
            print(f"❌ Error loading forecast from {latest_file}: {e}")
            return None
    
    def get_forecast_summary(self) -> Dict[str, Any]:
        """Get summary of forecast system status."""
        summary = {
            'asset': self.asset,
            'timeframes': {},
            'system_status': 'operational'
        }
        
        for timeframe in self.timeframe_configs.keys():
            state = self.model_states[timeframe]
            latest_forecast = self.get_latest_forecast(timeframe)
            
            summary['timeframes'][timeframe] = {
                'iteration_count': state['iteration_count'],
                'production_model': state.get('production_model'),
                'last_retrain': state.get('last_retrain'),
                'latest_forecast': latest_forecast.metadata.forecast_timestamp.isoformat() if latest_forecast else None,
                'models_status': {model: info['status'] for model, info in state['models'].items()}
            }
        
        return summary


# Usage example and CLI interface
if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate ETH forecasts')
    parser.add_argument('--timeframe', choices=['1min', '1hour', '1day'], 
                       required=True, help='Timeframe to generate forecast for')
    parser.add_argument('--force-retrain', action='store_true',
                       help='Force model retraining regardless of iteration count')
    parser.add_argument('--asset', default='ETH', help='Asset symbol')
    
    args = parser.parse_args()
    
    # Initialize generator
    generator = ETHForecastGenerator(args.asset)
    
    # Generate sample data (replace with real data source)
    print(f"📊 Generating sample data for {args.timeframe} forecast...")
    
    if args.timeframe == '1min':
        freq = '1min'
        periods = 2880  # 48 hours
    elif args.timeframe == '1hour':
        freq = '1H'
        periods = 720   # 30 days
    else:  # 1day
        freq = '1D'
        periods = 365   # 1 year
    
    dates = pd.date_range(end=pd.Timestamp.now(), periods=periods, freq=freq)
    np.random.seed(42)
    
    sample_data = pd.DataFrame({
        'open': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'high': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) + np.abs(np.random.randn(len(dates))),
        'low': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) - np.abs(np.random.randn(len(dates))),
        'close': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'volume': np.random.randint(1000, 10000, len(dates))
    }, index=dates)
    
    # Generate forecast
    try:
        forecast = generator.generate_forecast(
            args.timeframe, 
            sample_data, 
            force_retrain=args.force_retrain
        )
        
        print(f"\n✅ Forecast generated successfully!")
        print(f"Model: {forecast.metadata.model_type} v{forecast.metadata.model_version}")
        print(f"Confidence: {forecast.metadata.confidence_score:.3f}")
        print(f"Horizon: {forecast.metadata.forecast_horizon} periods")
        print(f"Predictions: {len(forecast.predictions)} time steps")
        
        # Show summary
        summary = generator.get_forecast_summary()
        print(f"\n📋 System Summary:")
        for tf, info in summary['timeframes'].items():
            print(f"  {tf}: Production={info['production_model']}, Iteration={info['iteration_count']}")
        
    except Exception as e:
        print(f"❌ Forecast generation failed: {e}")
        import traceback
        traceback.print_exc()
