"""
Core interfaces package for the alpha models framework.

This package defines the abstract base classes and interfaces that form
the foundation of the methodology-first architecture.
"""

__version__ = "1.0.0"
__author__ = "Unicorn Investing Team"

from .methodology_interface import AlphaMethodology
from .asset_adapter_interface import AssetAdapter
from .model_interface import TrainedModel
from .data_interfaces import (
    AssetData, 
    RawAssetData, 
    StandardizedData, 
    FeatureSet, 
    ForecastResult, 
    PerformanceMetrics,
    MarketCharacteristics,
    ConstrainedForecast
)

__all__ = [
    'AlphaMethodology',
    'AssetAdapter', 
    'TrainedModel',
    'AssetData',
    'RawAssetData',
    'StandardizedData',
    'FeatureSet',
    'ForecastResult',
    'PerformanceMetrics',
    'MarketCharacteristics',
    'ConstrainedForecast'
]