"""
Central configuration manager for the alpha models framework.

Provides centralized access to all configuration types and validation.
"""

from typing import Dict, Any, Optional, List, Union
from pathlib import Path
import json
import logging

from .methodology_config import MethodologyConfig

logger = logging.getLogger(__name__)

class ConfigManager:
    """
    Central configuration manager for the alpha models framework.
    
    Manages all configuration types and provides validation and caching.
    """
    
    def __init__(self, config_root: Optional[Union[str, Path]] = None):
        """
        Initialize configuration manager.
        
        Args:
            config_root: Root directory for all configuration files
        """
        if config_root is None:
            current_dir = Path(__file__).parent.parent.parent
            self.config_root = current_dir
        else:
            self.config_root = Path(config_root)
        
        self._config_cache = {}
        self._global_config = self._load_global_config()
        
        logger.info(f"ConfigManager initialized with root: {self.config_root}")
    
    def _load_global_config(self) -> Dict[str, Any]:
        """Load global framework configuration"""
        global_config_path = self.config_root / "global_config.json"
        
        if global_config_path.exists():
            try:
                with open(global_config_path, 'r') as f:
                    config = json.load(f)
                logger.info(f"Loaded global config from {global_config_path}")
                return config
            except Exception as e:
                logger.warning(f"Failed to load global config: {e}")
        
        # Default global configuration
        return {
            "framework": {
                "version": "1.0.0",
                "name": "Unicorn Alpha Models Framework",
                "architecture": "methodology_first"
            },
            "data": {
                "silver_layer_path": "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver",
                "cache_enabled": True,
                "cache_ttl_hours": 24
            },
            "storage": {
                "models_path": "storage/artifacts",
                "metadata_db": "storage/metadata/model_metadata.db",
                "performance_db": "storage/performance/performance.db"
            },
            "validation": {
                "min_training_periods": 100,
                "max_training_periods": 10000,
                "required_metrics": ["r2", "mape", "mae"],
                "performance_thresholds": {
                    "min_r2": -0.5,
                    "max_mape": 100.0,
                    "max_mae": 1000.0
                }
            },
            "logging": {
                "level": "INFO",
                "format": "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
            }
        }
    
    def get_methodology_config(self, methodology: str, asset_class: str, 
                             symbol: str) -> MethodologyConfig:
        """
        Get methodology configuration with caching.
        
        Args:
            methodology: Methodology name
            asset_class: Asset class
            symbol: Asset symbol
            
        Returns:
            MethodologyConfig instance
        """
        cache_key = f"{methodology}_{asset_class}_{symbol}"
        
        if cache_key not in self._config_cache:
            config_root = self.config_root / "methodologies" / methodology / "configs"
            self._config_cache[cache_key] = MethodologyConfig(
                methodology, asset_class, symbol, config_root
            )
        
        return self._config_cache[cache_key]
    
    def get_global_config(self, key: Optional[str] = None, default: Any = None) -> Any:
        """
        Get global configuration value.
        
        Args:
            key: Configuration key (supports dot notation)
            default: Default value if key not found
            
        Returns:
            Configuration value
        """
        if key is None:
            return self._global_config.copy()
        
        keys = key.split('.')
        value = self._global_config
        
        try:
            for k in keys:
                value = value[k]
            return value
        except (KeyError, TypeError):
            return default
    
    def get_supported_methodologies(self) -> List[str]:
        """
        Get list of supported methodologies.
        
        Returns:
            List of methodology names
        """
        methodologies_dir = self.config_root / "methodologies"
        
        if methodologies_dir.exists():
            return [d.name for d in methodologies_dir.iterdir() 
                   if d.is_dir() and not d.name.startswith('.')]
        
        # Default supported methodologies
        return ['prophet', 'xgboost', 'ensemble']
    
    def get_supported_assets(self) -> Dict[str, List[str]]:
        """
        Get supported assets by asset class.
        
        Returns:
            Dictionary mapping asset classes to symbol lists
        """
        assets_config_path = self.config_root / "assets" / "supported_assets.json"
        
        if assets_config_path.exists():
            try:
                with open(assets_config_path, 'r') as f:
                    return json.load(f)
            except Exception as e:
                logger.warning(f"Failed to load assets config: {e}")
        
        # Default supported assets
        return {
            "crypto": ["ETH", "BTC", "ADA", "DOT", "LINK"],
            "forex": ["EURUSD", "USDJPY", "GBPUSD", "AUDUSD", "USDCAD", "USDCHF", "NZDUSD"],
            "equities": ["AAPL", "MSFT", "GOOGL", "AMZN", "TSLA"],
            "commodities": ["GOLD", "SILVER", "OIL", "GAS"]
        }
    
    def validate_configuration(self, methodology: str, asset_class: str, 
                             symbol: str) -> Dict[str, Any]:
        """
        Validate configuration for given parameters.
        
        Args:
            methodology: Methodology name
            asset_class: Asset class
            symbol: Asset symbol
            
        Returns:
            Validation results dictionary
        """
        validation_results = {
            "valid": True,
            "errors": [],
            "warnings": []
        }
        
        # Check if methodology is supported
        supported_methodologies = self.get_supported_methodologies()
        if methodology not in supported_methodologies:
            validation_results["valid"] = False
            validation_results["errors"].append(
                f"Unsupported methodology: {methodology}. "
                f"Supported: {supported_methodologies}"
            )
        
        # Check if asset is supported
        supported_assets = self.get_supported_assets()
        if asset_class not in supported_assets:
            validation_results["valid"] = False
            validation_results["errors"].append(
                f"Unsupported asset class: {asset_class}. "
                f"Supported: {list(supported_assets.keys())}"
            )
        elif symbol not in supported_assets[asset_class]:
            validation_results["warnings"].append(
                f"Asset {symbol} not in default supported list for {asset_class}. "
                f"Proceeding with custom configuration."
            )
        
        # Validate methodology configuration
        try:
            config = self.get_methodology_config(methodology, asset_class, symbol)
            
            # Check required parameters
            required_sections = ['methodology_params', 'training_params', 'validation_params']
            for section in required_sections:
                if not config.get(section):
                    validation_results["warnings"].append(
                        f"Missing configuration section: {section}"
                    )
            
            # Validate training parameters
            training_params = config.get_training_params()
            min_periods = training_params.get('min_training_periods', 0)
            max_periods = training_params.get('max_training_periods', float('inf'))
            
            if min_periods < self.get_global_config('validation.min_training_periods', 100):
                validation_results["warnings"].append(
                    f"min_training_periods ({min_periods}) below recommended minimum"
                )
            
            if max_periods > self.get_global_config('validation.max_training_periods', 10000):
                validation_results["warnings"].append(
                    f"max_training_periods ({max_periods}) above recommended maximum"
                )
                
        except Exception as e:
            validation_results["valid"] = False
            validation_results["errors"].append(f"Configuration validation error: {e}")
        
        return validation_results
    
    def create_default_configs(self, methodology: str, force: bool = False):
        """
        Create default configuration files for a methodology.
        
        Args:
            methodology: Methodology name
            force: Whether to overwrite existing configurations
        """
        config_dir = self.config_root / "methodologies" / methodology / "configs"
        config_dir.mkdir(parents=True, exist_ok=True)
        
        # Create default config
        default_config_path = config_dir / "default_config.json"
        if not default_config_path.exists() or force:
            temp_config = MethodologyConfig(methodology, "crypto", "ETH")
            base_config = temp_config._get_default_config()
            
            with open(default_config_path, 'w') as f:
                json.dump(base_config, f, indent=2)
            
            logger.info(f"Created default config for {methodology}")
        
        # Create asset class override templates
        for asset_class in ['crypto', 'forex', 'equities']:
            override_path = config_dir / f"{asset_class}_overrides.json"
            if not override_path.exists() or force:
                override_config = {
                    f"{asset_class}_specific": {
                        "note": f"Asset class specific overrides for {asset_class}",
                        "methodology_params": {},
                        "training_params": {},
                        "validation_params": {}
                    }
                }
                
                with open(override_path, 'w') as f:
                    json.dump(override_config, f, indent=2)
                
                logger.info(f"Created {asset_class} overrides template for {methodology}")
    
    def clear_cache(self):
        """Clear configuration cache"""
        self._config_cache.clear()
        logger.info("Configuration cache cleared")
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """
        Get configuration cache statistics.
        
        Returns:
            Cache statistics dictionary
        """
        return {
            "cached_configs": len(self._config_cache),
            "cache_keys": list(self._config_cache.keys()),
            "memory_usage_mb": sum(
                len(str(config)) for config in self._config_cache.values()
            ) / (1024 * 1024)
        }
    
    def __str__(self) -> str:
        """String representation of config manager"""
        return f"ConfigManager(root={self.config_root})"
    
    def __repr__(self) -> str:
        """Detailed representation of config manager"""
        return (f"ConfigManager(config_root='{self.config_root}', "
                f"cached_configs={len(self._config_cache)})")