"""
Configuration management package for the alpha models framework.

Provides hierarchical configuration loading with asset-specific overrides.
"""

__version__ = "1.0.0"
__author__ = "Unicorn Investing Team"

from .methodology_config import MethodologyConfig
from .config_manager import ConfigManager
from .asset_config import AssetConfig

__all__ = [
    'MethodologyConfig',
    'ConfigManager', 
    'AssetConfig'
]