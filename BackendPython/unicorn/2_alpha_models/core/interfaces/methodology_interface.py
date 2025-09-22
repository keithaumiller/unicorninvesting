"""
Abstract base class for all alpha methodologies.

This interface defines the standard contract that all alpha methodologies
must implement in the methodology-first architecture.

ENHANCED TO SUPPORT: Legacy Prophet functionality including:
- Leak-free feature engineering
- Overfitting detection and elimination
- Realistic performance criteria
- Comprehensive model variants
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List, Union, TYPE_CHECKING
import pandas as pd
from .data_interfaces import AssetData, FeatureSet, ForecastResult, PerformanceMetrics

if TYPE_CHECKING:
    from .asset_adapter_interface import AssetAdapter
    from ..configuration.methodology_config import MethodologyConfig
    from .model_interface import TrainedModel

class AlphaMethodology(ABC):
    """
    Abstract base class for all alpha methodologies.
    
    This class defines the standard interface that all methodologies
    (Prophet, XGBoost, Ensemble, LSTM, etc.) must implement.
    
    ENHANCED TO SUPPORT: Legacy Prophet capabilities including
    overfitting elimination and realistic crypto modeling.
    """
    
    def __init__(self, methodology_name: str, version: str = "1.0.0"):
        """
        Initialize the methodology.
        
        Args:
            methodology_name: Name of the methodology (e.g., 'prophet', 'xgboost')
            version: Version of the methodology implementation
        """
        self.methodology_name = methodology_name
        self.version = version
        self.is_trained = False
        self.metadata = {}
    
    @abstractmethod
    def prepare_features(self, asset_data: AssetData, asset_adapter: 'AssetAdapter') -> FeatureSet:
        """
        Prepare methodology-specific features using asset adapter.
        
        Args:
            asset_data: Standardized asset data
            asset_adapter: Asset-specific adapter for feature engineering
            
        Returns:
            FeatureSet: Prepared features for this methodology
        """
        pass
    
    @abstractmethod
    def train_model(self, features: FeatureSet, config: 'MethodologyConfig') -> 'TrainedModel':
        """
        Train the methodology model using prepared features.
        
        Args:
            features: Prepared feature set
            config: Methodology configuration with parameters
            
        Returns:
            TrainedModel: Trained model ready for forecasting
        """
        pass
    
    @abstractmethod
    def generate_forecast(self, model: 'TrainedModel', current_data: AssetData) -> ForecastResult:
        """
        Generate forecast using trained model.
        
        Args:
            model: Trained model
            current_data: Current asset data for forecasting
            
        Returns:
            ForecastResult: Forecast with prediction and confidence
        """
        pass
    
    @abstractmethod
    def validate_performance(self, model: 'TrainedModel', test_data: AssetData) -> PerformanceMetrics:
        """
        Validate model performance on test data.
        
        Args:
            model: Trained model to validate
            test_data: Test dataset for validation
            
        Returns:
            PerformanceMetrics: Performance metrics for the model
        """
        pass
    
    @abstractmethod
    def get_feature_importance(self, model: 'TrainedModel') -> Dict[str, float]:
        """
        Get feature importance for this methodology.
        
        Args:
            model: Trained model
            
        Returns:
            Dict mapping feature names to importance scores
        """
        pass
    
    # ENHANCED METHODS TO SUPPORT LEGACY PROPHET FUNCTIONALITY
    
    def add_leak_free_features(self, data: pd.DataFrame, asset: str) -> pd.DataFrame:
        """
        Add leak-free features optimized for specific asset and methodology.
        
        SUPPORTS: Legacy Prophet feature engineering capabilities
        
        Args:
            data: OHLCV asset data
            asset: Asset symbol (ETH, BTC, EURUSD, etc.)
            
        Returns:
            DataFrame with leak-free features suitable for methodology
        """
        # Default implementation - methodologies should override for specific needs
        return data
    
    def detect_overfitting(self, train_metrics: Dict[str, float], 
                          val_metrics: Dict[str, float]) -> Dict[str, Any]:
        """
        Detect overfitting using train/validation metric comparison.
        
        SUPPORTS: Legacy overfitting detection from enhanced_crypto_prophet_builder.py
        
        Args:
            train_metrics: Training performance metrics
            val_metrics: Validation performance metrics
            
        Returns:
            Dictionary with overfitting detection results
        """
        # Default implementation
        train_score = train_metrics.get('r2', 0)
        val_score = val_metrics.get('r2', 0)
        gap = train_score - val_score
        
        return {
            'overfitting_detected': gap > 0.3,  # Default threshold
            'train_val_gap': gap,
            'overfitting_severity': 'low' if gap < 0.3 else 'medium' if gap < 0.6 else 'high'
        }
    
    def classify_performance_level(self, validation_score: float, 
                                 asset_class: str = 'crypto') -> str:
        """
        Classify model performance level based on validation score.
        
        SUPPORTS: Legacy realistic performance criteria from Prophet builder
        
        Args:
            validation_score: Validation R² or similar metric
            asset_class: Asset class for appropriate thresholds
            
        Returns:
            Performance level classification
        """
        # Default crypto thresholds (from legacy)
        if asset_class == 'crypto':
            if validation_score < -10:
                return "POOR"
            elif validation_score < -2:
                return "WEAK"
            elif validation_score < 0:
                return "TYPICAL"  # NORMAL for crypto!
            elif validation_score < 0.3:
                return "GOOD"
            elif validation_score < 0.6:
                return "EXCELLENT"
            else:
                return "SUSPICIOUS"  # May indicate data leakage
        
        # Default thresholds for other assets
        if validation_score < 0:
            return "POOR"
        elif validation_score < 0.3:
            return "WEAK"
        elif validation_score < 0.6:
            return "GOOD"
        elif validation_score < 0.8:
            return "EXCELLENT"
        else:
            return "SUSPICIOUS"
    
    def get_model_variant_config(self, variant: str, asset: str) -> Dict[str, Any]:
        """
        Get model configuration for specific variant and asset.
        
        SUPPORTS: Legacy Prophet variant configurations (basic, standard, enhanced, aggressive)
        
        Args:
            variant: Model variant name
            asset: Asset symbol
            
        Returns:
            Configuration dictionary for model variant
        """
        # Default implementation - methodologies should override
        return {}
    
    def build_comprehensive_models(self, assets: List[str], 
                                 variants: List[str]) -> Dict[str, Any]:
        """
        Build comprehensive models across multiple assets and variants.
        
        SUPPORTS: Legacy comprehensive model building from Prophet builder
        
        Args:
            assets: List of asset symbols
            variants: List of model variants
            
        Returns:
            Summary of all model building results
        """
        # Default implementation - methodologies should override
        return {
            'total_models': 0,
            'successful_models': 0,
            'overfitting_detected': 0,
            'status': 'NOT_IMPLEMENTED'
        }
    
    def get_supported_assets(self) -> List[str]:
        """
        Get list of supported asset classes for this methodology.
        
        Returns:
            List of supported asset class names
        """
        # Default: support all asset classes
        return ['crypto', 'forex', 'equities', 'commodities']
    
    def get_required_data_points(self) -> int:
        """
        Get minimum number of data points required for training.
        
        Returns:
            Minimum data points needed
        """
        # Default: 100 data points
        return 100
    
    def supports_online_learning(self) -> bool:
        """
        Check if methodology supports online/incremental learning.
        
        Returns:
            True if online learning is supported
        """
        # Default: no online learning
        return False
    
    def get_hyperparameter_space(self) -> Dict[str, Any]:
        """
        Get hyperparameter search space for optimization.
        
        Returns:
            Dictionary defining hyperparameter ranges and types
        """
        # Default: empty hyperparameter space
        return {}
    
    def preprocess_data(self, asset_data: AssetData) -> AssetData:
        """
        Methodology-specific data preprocessing.
        
        Args:
            asset_data: Input asset data
            
        Returns:
            Preprocessed asset data
        """
        # Default: no preprocessing
        return asset_data
    
    def postprocess_forecast(self, forecast: ForecastResult, 
                           asset_adapter: 'AssetAdapter') -> ForecastResult:
        """
        Methodology-specific forecast postprocessing.
        
        Args:
            forecast: Raw forecast result
            asset_adapter: Asset adapter for postprocessing
            
        Returns:
            Postprocessed forecast result
        """
        # Default: no postprocessing
        return forecast
    
    def __str__(self) -> str:
        """String representation of methodology"""
        return f"{self.methodology_name} v{self.version}"
    
    def __repr__(self) -> str:
        """Detailed representation of methodology"""
        return f"AlphaMethodology(name='{self.methodology_name}', version='{self.version}', trained={self.is_trained})"