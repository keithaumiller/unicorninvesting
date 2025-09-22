"""
Data pipeline package for the alpha models framework.

Provides unified data loading, normalization, and feature engineering.
"""

__version__ = "1.0.0"
__author__ = "Unicorn Investing Team"

from .data_loader import DataLoader
from .data_normalizer import DataNormalizer
from .feature_pipeline import FeaturePipeline

__all__ = [
    'DataLoader',
    'DataNormalizer',
    'FeaturePipeline'
]