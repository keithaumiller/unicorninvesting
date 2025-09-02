"""
Multi-Timeframe Model Integration for Myportolio

This module integrates timeframe-specific ETH models with the multi-timeframe
trading strategies, providing seamless model-to-strategy communication and
unified prediction capabilities.

Integration Features:
- Automatic model loading and caching
- Real-time prediction generation
- Signal confidence scoring
- Performance monitoring
- Model health checks
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple, Union
import sys
import os
from datetime import datetime, timedelta
import pickle
import warnings
from pathlib import Path
import sqlite3
import json
import logging
from dataclasses import dataclass
from enum import Enum

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent))

# Import our frameworks
from .timeframe_model_frameworks import (
    TimeframeSpecificProphetFramework,
    TimeframeSpecificXGBoostFramework, 
    TimeframeSpecificEnsembleFramework,
    MultiTimeframeModelTrainer
)

from .multi_timeframe_strategies import (
    ScalpStrategy,
    SwingStrategy,
    PositionStrategy,
    InvestmentStrategy,
    MultiTimeframePortfolioManager
)

warnings.filterwarnings('ignore', category=RuntimeWarning)

class ModelType(Enum):
    """Model type enumeration."""
    PROPHET = "prophet"
    XGBOOST = "xgboost"
    ENSEMBLE = "ensemble"

class PredictionConfidence(Enum):
    """Prediction confidence levels."""
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"

@dataclass
class ModelPrediction:
    """Structured model prediction result."""
    timeframe: str
    model_type: str
    predicted_price: float
    predicted_return: float
    confidence: PredictionConfidence
    upper_bound: Optional[float] = None
    lower_bound: Optional[float] = None
    features_used: Optional[List[str]] = None
    prediction_timestamp: datetime = None
    
    def __post_init__(self):
        if self.prediction_timestamp is None:
            self.prediction_timestamp = datetime.now()

@dataclass
class StrategySignal:
    """Enhanced strategy signal with model integration."""
    timeframe: str
    strategy_name: str
    signal_type: str  # 'buy', 'sell', 'hold'
    signal_strength: float  # 0.0 to 1.0
    model_prediction: ModelPrediction
    suggested_position_size: float
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
    risk_score: Optional[float] = None
    timestamp: datetime = None
    
    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = datetime.now()


class ModelIntegrationManager:
    """
    Manages the integration between timeframe-specific models and trading strategies.
    """
    
    def __init__(self, portfolio_name: str = "Myportolio"):
        self.portfolio_name = portfolio_name
        self.base_path = Path(__file__).parent.parent.parent.parent
        self.model_storage_dir = self.base_path / "2_alpha_models" / "CRYPTO" / "ETH" / "timeframe_models"
        
        # Initialize components
        self.model_frameworks = {}
        self.trading_strategies = {}
        self.loaded_models = {}
        self.performance_cache = {}
        
        # Setup logging
        self.logger = self._setup_logging()
        
        # Initialize timeframe configurations
        self.timeframe_configs = {
            '1min': {
                'strategy_class': ScalpStrategy,
                'model_update_frequency': timedelta(minutes=5),
                'prediction_horizon': 60,  # 1 hour ahead
                'confidence_threshold': 0.6
            },
            '1hour': {
                'strategy_class': SwingStrategy,
                'model_update_frequency': timedelta(hours=1),
                'prediction_horizon': 24,  # 24 hours ahead
                'confidence_threshold': 0.7
            },
            '1day': {
                'strategy_class': PositionStrategy,
                'model_update_frequency': timedelta(days=1),
                'prediction_horizon': 30,  # 30 days ahead
                'confidence_threshold': 0.75
            }
        }
        
        self._initialize_components()
        
    def _setup_logging(self) -> logging.Logger:
        """Setup logging for the integration manager."""
        logger = logging.getLogger(f"ModelIntegration_{self.portfolio_name}")
        logger.setLevel(logging.INFO)
        
        # Create handler if it doesn't exist
        if not logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
            handler.setFormatter(formatter)
            logger.addHandler(handler)
            
        return logger
    
    def _initialize_components(self):
        """Initialize model frameworks and trading strategies."""
        self.logger.info("Initializing model integration components...")
        
        # Initialize model frameworks for each timeframe
        for timeframe in self.timeframe_configs.keys():
            self.model_frameworks[timeframe] = {
                'prophet': TimeframeSpecificProphetFramework(timeframe),
                'xgboost': TimeframeSpecificXGBoostFramework(timeframe),
                'ensemble': TimeframeSpecificEnsembleFramework(timeframe)
            }
            
            # Initialize trading strategies
            strategy_class = self.timeframe_configs[timeframe]['strategy_class']
            self.trading_strategies[timeframe] = strategy_class()
            
        self.logger.info("Components initialized successfully")
    
    def load_models(self, force_reload: bool = False) -> Dict[str, bool]:
        """Load all trained models from disk."""
        if self.loaded_models and not force_reload:
            return {tf: True for tf in self.timeframe_configs.keys()}
            
        load_results = {}
        
        for timeframe in self.timeframe_configs.keys():
            self.logger.info(f"Loading models for {timeframe}...")
            timeframe_success = True
            
            try:
                # Load Prophet models
                prophet_path = self.model_storage_dir / f"prophet_{timeframe}"
                if prophet_path.exists():
                    prophet_models = {}
                    for model_file in prophet_path.glob("*_model.pkl"):
                        model_name = model_file.stem.replace('_model', '')
                        with open(model_file, 'rb') as f:
                            prophet_models[model_name] = pickle.load(f)
                    self.loaded_models.setdefault(timeframe, {})['prophet'] = prophet_models
                else:
                    self.logger.warning(f"Prophet models not found for {timeframe}")
                    timeframe_success = False
                
                # Load XGBoost models
                xgb_path = self.model_storage_dir / f"xgboost_{timeframe}"
                if xgb_path.exists():
                    import xgboost as xgb
                    xgb_models = {}
                    for model_file in xgb_path.glob("*_model.json"):
                        model_name = model_file.stem.replace('_model', '')
                        model = xgb.XGBRegressor()
                        model.load_model(str(model_file))
                        xgb_models[model_name] = model
                    self.loaded_models.setdefault(timeframe, {})['xgboost'] = xgb_models
                else:
                    self.logger.warning(f"XGBoost models not found for {timeframe}")
                    timeframe_success = False
                
                # Load Ensemble configurations
                ensemble_path = self.model_storage_dir / f"ensemble_{timeframe}"
                if ensemble_path.exists():
                    ensemble_configs = {}
                    for config_file in ensemble_path.glob("*_config.json"):
                        model_name = config_file.stem.replace('_config', '')
                        with open(config_file, 'r') as f:
                            ensemble_configs[model_name] = json.load(f)
                    self.loaded_models.setdefault(timeframe, {})['ensemble'] = ensemble_configs
                else:
                    self.logger.warning(f"Ensemble configs not found for {timeframe}")
                    timeframe_success = False
                    
            except Exception as e:
                self.logger.error(f"Error loading models for {timeframe}: {e}")
                timeframe_success = False
                
            load_results[timeframe] = timeframe_success
            
        self.logger.info(f"Model loading results: {load_results}")
        return load_results
    
    def generate_predictions(self, market_data: pd.DataFrame, 
                           timeframes: Optional[List[str]] = None,
                           model_types: Optional[List[str]] = None) -> Dict[str, Dict[str, ModelPrediction]]:
        """Generate predictions for specified timeframes and model types."""
        if timeframes is None:
            timeframes = list(self.timeframe_configs.keys())
        if model_types is None:
            model_types = ['prophet', 'xgboost', 'ensemble']
            
        predictions = {}
        
        for timeframe in timeframes:
            if timeframe not in self.loaded_models:
                self.logger.warning(f"Models not loaded for {timeframe}")
                continue
                
            predictions[timeframe] = {}
            
            for model_type in model_types:
                try:
                    prediction = self._generate_single_prediction(
                        market_data, timeframe, model_type
                    )
                    predictions[timeframe][model_type] = prediction
                    
                except Exception as e:
                    self.logger.error(f"Error generating {model_type} prediction for {timeframe}: {e}")
                    
        return predictions
    
    def _generate_single_prediction(self, market_data: pd.DataFrame, 
                                   timeframe: str, model_type: str) -> ModelPrediction:
        """Generate a single prediction for specific timeframe and model type."""
        framework = self.model_frameworks[timeframe][model_type]
        
        if model_type == 'prophet':
            return self._generate_prophet_prediction(market_data, timeframe, framework)
        elif model_type == 'xgboost':
            return self._generate_xgboost_prediction(market_data, timeframe, framework)
        elif model_type == 'ensemble':
            return self._generate_ensemble_prediction(market_data, timeframe, framework)
        else:
            raise ValueError(f"Unknown model type: {model_type}")
    
    def _generate_prophet_prediction(self, market_data: pd.DataFrame,
                                   timeframe: str, framework) -> ModelPrediction:
        """Generate Prophet prediction."""
        # Use enhanced model by default
        model = self.loaded_models[timeframe]['prophet']['enhanced']
        
        # Prepare data
        df = framework.prepare_timeframe_data(market_data, timeframe)
        df = framework.create_timeframe_features(df, timeframe)
        
        # Create future dataframe
        periods = self.timeframe_configs[timeframe]['prediction_horizon']
        future = model.make_future_dataframe(periods=periods, freq=timeframe[1:])
        
        # Add regressors
        regressor_cols = [col for col in df.columns if 'sma_' in col or 'rsi_' in col][:3]
        for col in regressor_cols:
            if col in df.columns and not df[col].isna().all():
                last_value = df[col].iloc[-1]
                future[col] = future[col].fillna(last_value)
        
        # Generate forecast
        forecast = model.predict(future)
        
        # Extract prediction for the next period
        next_prediction = forecast['yhat'].iloc[-periods]
        current_price = df['close'].iloc[-1]
        predicted_return = (next_prediction - current_price) / current_price
        
        # Calculate confidence based on prediction interval width
        upper_bound = forecast['yhat_upper'].iloc[-periods]
        lower_bound = forecast['yhat_lower'].iloc[-periods]
        interval_width = (upper_bound - lower_bound) / current_price
        
        if interval_width < 0.05:  # Less than 5% width
            confidence = PredictionConfidence.HIGH
        elif interval_width < 0.15:  # Less than 15% width
            confidence = PredictionConfidence.MEDIUM
        else:
            confidence = PredictionConfidence.LOW
        
        return ModelPrediction(
            timeframe=timeframe,
            model_type='prophet',
            predicted_price=next_prediction,
            predicted_return=predicted_return,
            confidence=confidence,
            upper_bound=upper_bound,
            lower_bound=lower_bound,
            features_used=regressor_cols
        )
    
    def _generate_xgboost_prediction(self, market_data: pd.DataFrame,
                                   timeframe: str, framework) -> ModelPrediction:
        """Generate XGBoost prediction."""
        # Use enhanced model by default
        model = self.loaded_models[timeframe]['xgboost']['enhanced']
        
        # Prepare data
        df = framework.prepare_timeframe_data(market_data, timeframe)
        df = framework.create_timeframe_features(df, timeframe)
        
        # Get features (exclude target and price columns)
        feature_cols = [col for col in df.columns if col not in ['open', 'high', 'low', 'close', 'volume', 'target']]
        X_latest = df[feature_cols].iloc[-1:].fillna(method='ffill')
        
        # Generate prediction (returns predicted return)
        predicted_return = model.predict(X_latest)[0]
        current_price = df['close'].iloc[-1]
        predicted_price = current_price * (1 + predicted_return)
        
        # Calculate confidence based on feature importance and prediction magnitude
        feature_importance = model.feature_importances_
        avg_importance = np.mean(feature_importance)
        
        if abs(predicted_return) > 0.02 and avg_importance > 0.1:  # Strong signal
            confidence = PredictionConfidence.HIGH
        elif abs(predicted_return) > 0.01 and avg_importance > 0.05:  # Medium signal
            confidence = PredictionConfidence.MEDIUM
        else:
            confidence = PredictionConfidence.LOW
        
        return ModelPrediction(
            timeframe=timeframe,
            model_type='xgboost',
            predicted_price=predicted_price,
            predicted_return=predicted_return,
            confidence=confidence,
            features_used=feature_cols
        )
    
    def _generate_ensemble_prediction(self, market_data: pd.DataFrame,
                                    timeframe: str, framework) -> ModelPrediction:
        """Generate Ensemble prediction."""
        # Get individual predictions
        prophet_pred = self._generate_prophet_prediction(market_data, timeframe, 
                                                        self.model_frameworks[timeframe]['prophet'])
        xgboost_pred = self._generate_xgboost_prediction(market_data, timeframe,
                                                        self.model_frameworks[timeframe]['xgboost'])
        
        # Get ensemble weights
        ensemble_config = self.loaded_models[timeframe]['ensemble']['enhanced']
        weights = ensemble_config['weights']
        
        # Combine predictions
        ensemble_return = (weights['prophet'] * prophet_pred.predicted_return + 
                          weights['xgboost'] * xgboost_pred.predicted_return)
        
        current_price = market_data['close'].iloc[-1]
        ensemble_price = current_price * (1 + ensemble_return)
        
        # Ensemble confidence is weighted average of individual confidences
        confidence_scores = {
            PredictionConfidence.HIGH: 3,
            PredictionConfidence.MEDIUM: 2,
            PredictionConfidence.LOW: 1
        }
        
        prophet_score = confidence_scores[prophet_pred.confidence]
        xgboost_score = confidence_scores[xgboost_pred.confidence]
        ensemble_score = weights['prophet'] * prophet_score + weights['xgboost'] * xgboost_score
        
        if ensemble_score >= 2.5:
            confidence = PredictionConfidence.HIGH
        elif ensemble_score >= 1.5:
            confidence = PredictionConfidence.MEDIUM
        else:
            confidence = PredictionConfidence.LOW
        
        return ModelPrediction(
            timeframe=timeframe,
            model_type='ensemble',
            predicted_price=ensemble_price,
            predicted_return=ensemble_return,
            confidence=confidence,
            upper_bound=prophet_pred.upper_bound,
            lower_bound=prophet_pred.lower_bound,
            features_used=list(set(prophet_pred.features_used + xgboost_pred.features_used))
        )
    
    def generate_integrated_signals(self, market_data: pd.DataFrame) -> Dict[str, StrategySignal]:
        """Generate trading signals by integrating model predictions with strategies."""
        # Generate predictions for all timeframes
        predictions = self.generate_predictions(market_data)
        
        integrated_signals = {}
        
        for timeframe, timeframe_predictions in predictions.items():
            if not timeframe_predictions:
                continue
                
            # Use ensemble prediction if available, otherwise enhanced model
            if 'ensemble' in timeframe_predictions:
                model_prediction = timeframe_predictions['ensemble']
            elif 'prophet' in timeframe_predictions:
                model_prediction = timeframe_predictions['prophet']
            else:
                continue
                
            # Get strategy for this timeframe
            strategy = self.trading_strategies[timeframe]
            
            # Generate strategy signal based on model prediction
            strategy_signal = self._create_strategy_signal(
                timeframe, strategy, model_prediction, market_data
            )
            
            integrated_signals[timeframe] = strategy_signal
            
        return integrated_signals
    
    def _create_strategy_signal(self, timeframe: str, strategy, 
                              model_prediction: ModelPrediction,
                              market_data: pd.DataFrame) -> StrategySignal:
        """Create integrated strategy signal."""
        # Determine signal type based on prediction
        predicted_return = model_prediction.predicted_return
        confidence_threshold = self.timeframe_configs[timeframe]['confidence_threshold']
        
        if (predicted_return > 0.01 and 
            model_prediction.confidence in [PredictionConfidence.HIGH, PredictionConfidence.MEDIUM]):
            signal_type = 'buy'
            signal_strength = min(abs(predicted_return) * 10, 1.0)  # Scale to 0-1
        elif (predicted_return < -0.01 and 
              model_prediction.confidence in [PredictionConfidence.HIGH, PredictionConfidence.MEDIUM]):
            signal_type = 'sell'
            signal_strength = min(abs(predicted_return) * 10, 1.0)
        else:
            signal_type = 'hold'
            signal_strength = 0.0
        
        # Apply confidence adjustment
        if model_prediction.confidence == PredictionConfidence.HIGH:
            signal_strength *= 1.0
        elif model_prediction.confidence == PredictionConfidence.MEDIUM:
            signal_strength *= 0.7
        else:
            signal_strength *= 0.3
        
        # Get strategy-specific parameters
        current_price = market_data['close'].iloc[-1]
        
        # Calculate position size using Kelly Criterion from strategy
        if hasattr(strategy, 'calculate_kelly_position_size'):
            suggested_position_size = strategy.calculate_kelly_position_size(
                predicted_return, signal_strength
            )
        else:
            # Fallback position sizing
            base_position = strategy.max_position_size * 0.5
            suggested_position_size = base_position * signal_strength
        
        # Set stop loss and take profit
        if signal_type == 'buy':
            stop_loss = current_price * (1 - strategy.stop_loss_pct)
            take_profit = current_price * (1 + strategy.take_profit_pct)
        elif signal_type == 'sell':
            stop_loss = current_price * (1 + strategy.stop_loss_pct)
            take_profit = current_price * (1 - strategy.take_profit_pct)
        else:
            stop_loss = None
            take_profit = None
        
        # Calculate risk score
        volatility = market_data['close'].pct_change().rolling(20).std().iloc[-1]
        risk_score = min(volatility * signal_strength * 100, 1.0)
        
        return StrategySignal(
            timeframe=timeframe,
            strategy_name=strategy.__class__.__name__,
            signal_type=signal_type,
            signal_strength=signal_strength,
            model_prediction=model_prediction,
            suggested_position_size=suggested_position_size,
            stop_loss=stop_loss,
            take_profit=take_profit,
            risk_score=risk_score
        )
    
    def get_model_health_status(self) -> Dict[str, Dict[str, str]]:
        """Check health status of all loaded models."""
        health_status = {}
        
        for timeframe in self.timeframe_configs.keys():
            health_status[timeframe] = {}
            
            if timeframe not in self.loaded_models:
                health_status[timeframe]['overall'] = 'NOT_LOADED'
                continue
                
            timeframe_models = self.loaded_models[timeframe]
            healthy_models = 0
            total_models = 0
            
            for model_type in ['prophet', 'xgboost', 'ensemble']:
                if model_type in timeframe_models:
                    if timeframe_models[model_type]:
                        health_status[timeframe][model_type] = 'HEALTHY'
                        healthy_models += 1
                    else:
                        health_status[timeframe][model_type] = 'ERROR'
                    total_models += 1
                else:
                    health_status[timeframe][model_type] = 'NOT_LOADED'
                    
            # Overall health for timeframe
            if healthy_models == total_models and total_models > 0:
                health_status[timeframe]['overall'] = 'HEALTHY'
            elif healthy_models > 0:
                health_status[timeframe]['overall'] = 'PARTIAL'
            else:
                health_status[timeframe]['overall'] = 'UNHEALTHY'
                
        return health_status
    
    def update_performance_cache(self, timeframe: str, signal: StrategySignal,
                               actual_return: float):
        """Update performance tracking for models and strategies."""
        if timeframe not in self.performance_cache:
            self.performance_cache[timeframe] = {
                'predictions': [],
                'actual_returns': [],
                'signals': [],
                'timestamps': []
            }
            
        cache = self.performance_cache[timeframe]
        cache['predictions'].append(signal.model_prediction.predicted_return)
        cache['actual_returns'].append(actual_return)
        cache['signals'].append(signal.signal_type)
        cache['timestamps'].append(datetime.now())
        
        # Keep only last 1000 entries
        if len(cache['predictions']) > 1000:
            for key in cache:
                cache[key] = cache[key][-1000:]
                
    def get_performance_summary(self) -> Dict[str, Dict[str, float]]:
        """Get performance summary for all timeframes."""
        summary = {}
        
        for timeframe, cache in self.performance_cache.items():
            if len(cache['predictions']) < 10:  # Need minimum data
                summary[timeframe] = {'status': 'insufficient_data'}
                continue
                
            predictions = np.array(cache['predictions'])
            actual_returns = np.array(cache['actual_returns'])
            
            # Calculate metrics
            mae = np.mean(np.abs(predictions - actual_returns))
            mse = np.mean((predictions - actual_returns) ** 2)
            correlation = np.corrcoef(predictions, actual_returns)[0, 1] if len(predictions) > 1 else 0
            
            # Signal accuracy
            signals = cache['signals']
            signal_accuracy = sum(1 for i, sig in enumerate(signals) 
                                if (sig == 'buy' and actual_returns[i] > 0) or 
                                   (sig == 'sell' and actual_returns[i] < 0) or
                                   (sig == 'hold' and abs(actual_returns[i]) < 0.01)) / len(signals)
            
            summary[timeframe] = {
                'mae': mae,
                'mse': mse,
                'rmse': np.sqrt(mse),
                'correlation': correlation,
                'signal_accuracy': signal_accuracy,
                'sample_size': len(predictions)
            }
            
        return summary


# Integration usage example
if __name__ == "__main__":
    # Example usage
    integration_manager = ModelIntegrationManager()
    
    # Load models
    load_results = integration_manager.load_models()
    print("Model loading results:", load_results)
    
    # Check health
    health_status = integration_manager.get_model_health_status()
    print("Model health status:", health_status)
    
    # Generate sample market data
    dates = pd.date_range(start='2024-12-01', end='2024-12-02', freq='1min')
    np.random.seed(42)
    
    market_data = pd.DataFrame({
        'open': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'high': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) + np.abs(np.random.randn(len(dates))),
        'low': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) - np.abs(np.random.randn(len(dates))),
        'close': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'volume': np.random.randint(1000, 10000, len(dates))
    }, index=dates)
    
    # Generate integrated signals
    try:
        signals = integration_manager.generate_integrated_signals(market_data)
        print("\nGenerated signals:")
        for timeframe, signal in signals.items():
            print(f"{timeframe}: {signal.signal_type} (strength: {signal.signal_strength:.3f})")
    except Exception as e:
        print(f"Error generating signals: {e}")
        print("This is expected if models haven't been trained yet.")
