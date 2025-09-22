"""
Interface for trained models in the alpha models framework.

Defines the standard interface for all trained models regardless of methodology.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List, Union
from datetime import datetime
import pickle
import joblib
from pathlib import Path

class TrainedModel(ABC):
    """
    Abstract interface for all trained models.
    
    This interface ensures consistent behavior across all methodologies
    for model serialization, metadata management, and prediction.
    """
    
    def __init__(self, methodology: str, asset_class: str, symbol: str):
        """
        Initialize the trained model.
        
        Args:
            methodology: Name of the methodology used
            asset_class: Asset class (crypto, forex, etc.)
            symbol: Specific asset symbol
        """
        self.methodology = methodology
        self.asset_class = asset_class
        self.symbol = symbol
        self.model_id = None
        self.training_date = datetime.now()
        self.version = "1.0.0"
        self.metadata = {}
        self.performance_metrics = {}
        self.is_trained = False
        self.model_artifacts = {}
    
    @abstractmethod
    def predict(self, input_data: Any) -> Any:
        """
        Generate prediction using the trained model.
        
        Args:
            input_data: Input data for prediction
            
        Returns:
            Prediction result
        """
        pass
    
    @abstractmethod
    def predict_proba(self, input_data: Any) -> Any:
        """
        Generate prediction probabilities (if applicable).
        
        Args:
            input_data: Input data for prediction
            
        Returns:
            Prediction probabilities
        """
        pass
    
    @abstractmethod
    def get_model_size(self) -> int:
        """
        Get model size in bytes.
        
        Returns:
            Model size in bytes
        """
        pass
    
    def save_model(self, filepath: Union[str, Path], 
                   format_type: str = 'joblib') -> bool:
        """
        Save the trained model to disk.
        
        Args:
            filepath: Path to save the model
            format_type: Serialization format ('joblib', 'pickle')
            
        Returns:
            True if save successful
        """
        try:
            filepath = Path(filepath)
            filepath.parent.mkdir(parents=True, exist_ok=True)
            
            # Prepare model data for serialization
            model_data = {
                'model_artifacts': self.model_artifacts,
                'metadata': self.get_metadata(),
                'performance_metrics': self.performance_metrics,
                'methodology': self.methodology,
                'asset_class': self.asset_class,
                'symbol': self.symbol,
                'version': self.version,
                'training_date': self.training_date,
                'is_trained': self.is_trained
            }
            
            if format_type == 'joblib':
                joblib.dump(model_data, filepath)
            elif format_type == 'pickle':
                with open(filepath, 'wb') as f:
                    pickle.dump(model_data, f)
            else:
                raise ValueError(f"Unsupported format: {format_type}")
            
            return True
            
        except Exception as e:
            print(f"Error saving model: {e}")
            return False
    
    def load_model(self, filepath: Union[str, Path], 
                   format_type: str = 'joblib') -> bool:
        """
        Load a trained model from disk.
        
        Args:
            filepath: Path to load the model from
            format_type: Serialization format ('joblib', 'pickle')
            
        Returns:
            True if load successful
        """
        try:
            filepath = Path(filepath)
            
            if not filepath.exists():
                raise FileNotFoundError(f"Model file not found: {filepath}")
            
            if format_type == 'joblib':
                model_data = joblib.load(filepath)
            elif format_type == 'pickle':
                with open(filepath, 'rb') as f:
                    model_data = pickle.load(f)
            else:
                raise ValueError(f"Unsupported format: {format_type}")
            
            # Restore model state
            self.model_artifacts = model_data.get('model_artifacts', {})
            self.metadata = model_data.get('metadata', {})
            self.performance_metrics = model_data.get('performance_metrics', {})
            self.methodology = model_data.get('methodology', self.methodology)
            self.asset_class = model_data.get('asset_class', self.asset_class)
            self.symbol = model_data.get('symbol', self.symbol)
            self.version = model_data.get('version', self.version)
            self.training_date = model_data.get('training_date', self.training_date)
            self.is_trained = model_data.get('is_trained', False)
            
            return True
            
        except Exception as e:
            print(f"Error loading model: {e}")
            return False
    
    def get_metadata(self) -> Dict[str, Any]:
        """
        Get comprehensive model metadata.
        
        Returns:
            Dictionary with model metadata
        """
        return {
            'model_id': self.model_id,
            'methodology': self.methodology,
            'asset_class': self.asset_class,
            'symbol': self.symbol,
            'version': self.version,
            'training_date': self.training_date.isoformat(),
            'is_trained': self.is_trained,
            'model_size_bytes': self.get_model_size(),
            'performance_metrics': self.performance_metrics,
            'custom_metadata': self.metadata
        }
    
    def set_performance_metrics(self, metrics: Dict[str, float]):
        """
        Set performance metrics for the model.
        
        Args:
            metrics: Dictionary of performance metrics
        """
        self.performance_metrics = metrics
    
    def get_performance_metrics(self) -> Dict[str, float]:
        """
        Get performance metrics for the model.
        
        Returns:
            Dictionary of performance metrics
        """
        return self.performance_metrics.copy()
    
    def add_metadata(self, key: str, value: Any):
        """
        Add custom metadata to the model.
        
        Args:
            key: Metadata key
            value: Metadata value
        """
        self.metadata[key] = value
    
    def get_feature_names(self) -> Optional[List[str]]:
        """
        Get list of feature names used by this model.
        
        Returns:
            List of feature names or None if not applicable
        """
        return self.metadata.get('feature_names')
    
    def set_feature_names(self, feature_names: List[str]):
        """
        Set feature names for this model.
        
        Args:
            feature_names: List of feature names
        """
        self.metadata['feature_names'] = feature_names
    
    def is_compatible_with(self, asset_class: str, symbol: str) -> bool:
        """
        Check if model is compatible with given asset.
        
        Args:
            asset_class: Target asset class
            symbol: Target symbol
            
        Returns:
            True if compatible
        """
        return (self.asset_class == asset_class and 
                self.symbol == symbol and 
                self.is_trained)
    
    def get_training_summary(self) -> Dict[str, Any]:
        """
        Get summary of training process and results.
        
        Returns:
            Dictionary with training summary
        """
        return {
            'methodology': self.methodology,
            'asset': f"{self.asset_class}_{self.symbol}",
            'training_date': self.training_date.isoformat(),
            'is_trained': self.is_trained,
            'performance_metrics': self.performance_metrics,
            'model_size_mb': self.get_model_size() / (1024 * 1024),
            'version': self.version
        }
    
    def __str__(self) -> str:
        """String representation of trained model"""
        return f"{self.methodology}_{self.asset_class}_{self.symbol}_model"
    
    def __repr__(self) -> str:
        """Detailed representation of trained model"""
        return (f"TrainedModel(methodology='{self.methodology}', "
                f"asset='{self.asset_class}_{self.symbol}', "
                f"trained={self.is_trained}, "
                f"date='{self.training_date.strftime('%Y-%m-%d')}')")