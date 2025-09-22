"""
Base Alpha Model Class

Provides common functionality for all alpha models across asset classes.
"""

from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
import pandas as pd
import numpy as np
from datetime import datetime

class BaseAlphaModel(ABC):
    """
    Abstract base class for all alpha models.
    
    Provides common interface and utilities for:
    - Signal generation
    - Confidence scoring
    - Risk management
    - Performance tracking
    """
    
    def __init__(self, name: str, asset_class: str):
        self.name = name
        self.asset_class = asset_class
        self.signals_generated = 0
        self.last_signal_time = None
        self.performance_metrics = {}
        
    @abstractmethod
    def generate_signal(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Generate trading signal from market data.
        
        Args:
            data: Market data DataFrame
            
        Returns:
            Dictionary containing:
            - signal: Direction (1 for buy, -1 for sell, 0 for hold)
            - confidence: Signal confidence (0.0 to 1.0)
            - metadata: Additional signal information
        """
        pass
        
    @abstractmethod
    def get_required_columns(self) -> List[str]:
        """Return list of required data columns."""
        pass
        
    def validate_data(self, data: pd.DataFrame) -> bool:
        """Validate input data has required columns."""
        required = self.get_required_columns()
        missing = [col for col in required if col not in data.columns]
        if missing:
            raise ValueError(f"Missing required columns: {missing}")
        return True
        
    def update_performance(self, signal: Dict[str, Any], actual_return: float):
        """Update performance metrics with realized returns."""
        if 'performance_history' not in self.performance_metrics:
            self.performance_metrics['performance_history'] = []
            
        self.performance_metrics['performance_history'].append({
            'timestamp': datetime.now(),
            'signal': signal,
            'actual_return': actual_return
        })
        
    def get_performance_summary(self) -> Dict[str, Any]:
        """Get performance summary statistics."""
        if 'performance_history' not in self.performance_metrics:
            return {'signals_generated': self.signals_generated}
            
        history = self.performance_metrics['performance_history']
        returns = [h['actual_return'] for h in history]
        
        return {
            'signals_generated': self.signals_generated,
            'total_signals': len(history),
            'mean_return': np.mean(returns) if returns else 0,
            'std_return': np.std(returns) if returns else 0,
            'sharpe_ratio': np.mean(returns) / np.std(returns) if len(returns) > 1 and np.std(returns) > 0 else 0,
            'win_rate': sum(1 for r in returns if r > 0) / len(returns) if returns else 0
        }

class TechnicalAlphaModel(BaseAlphaModel):
    """
    Base class for technical analysis based alpha models.
    
    Provides common technical analysis utilities.
    """
    
    def __init__(self, name: str, asset_class: str, lookback_window: int = 100):
        super().__init__(name, asset_class)
        self.lookback_window = lookback_window
        
    def calculate_sma(self, data: pd.Series, window: int) -> pd.Series:
        """Calculate Simple Moving Average."""
        return data.rolling(window=window).mean()
        
    def calculate_ema(self, data: pd.Series, window: int) -> pd.Series:
        """Calculate Exponential Moving Average."""
        return data.ewm(span=window).mean()
        
    def calculate_rsi(self, data: pd.Series, window: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = data.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
        
    def calculate_bollinger_bands(self, data: pd.Series, window: int = 20, num_std: float = 2.0) -> Dict[str, pd.Series]:
        """Calculate Bollinger Bands."""
        sma = self.calculate_sma(data, window)
        std = data.rolling(window=window).std()
        return {
            'upper': sma + (std * num_std),
            'middle': sma,
            'lower': sma - (std * num_std)
        }

class MachineLearningAlphaModel(BaseAlphaModel):
    """
    Base class for machine learning based alpha models.
    
    Provides common ML utilities and patterns.
    """
    
    def __init__(self, name: str, asset_class: str, model_type: str = "sklearn"):
        super().__init__(name, asset_class)
        self.model_type = model_type
        self.model = None
        self.is_trained = False
        self.feature_importance = {}
        
    @abstractmethod
    def prepare_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """Prepare features for ML model."""
        pass
        
    @abstractmethod
    def train_model(self, data: pd.DataFrame, target: pd.Series):
        """Train the ML model."""
        pass
        
    def get_feature_importance(self) -> Dict[str, float]:
        """Get feature importance from trained model."""
        return self.feature_importance
        
    def validate_model(self, test_data: pd.DataFrame, test_target: pd.Series) -> Dict[str, float]:
        """Validate model performance on test data."""
        if not self.is_trained:
            raise ValueError("Model must be trained before validation")
            
        features = self.prepare_features(test_data)
        predictions = self.model.predict(features)
        
        # Calculate validation metrics
        from sklearn.metrics import accuracy_score, precision_score, recall_score
        
        return {
            'accuracy': accuracy_score(test_target, predictions),
            'precision': precision_score(test_target, predictions, average='weighted'),
            'recall': recall_score(test_target, predictions, average='weighted')
        }
