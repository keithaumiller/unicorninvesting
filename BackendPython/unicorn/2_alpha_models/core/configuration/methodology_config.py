"""
Methodology configuration management with hierarchical parameter loading.

Provides unified configuration for methodologies with asset-specific overrides.
"""

from typing import Dict, Any, Optional, Union
import json
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

class MethodologyConfig:
    """
    Unified configuration for methodologies with asset-specific overrides.
    
    Configuration loading priority:
    1. Base methodology defaults
    2. Asset class overrides  
    3. Symbol-specific overrides
    4. Runtime overrides
    """
    
    def __init__(self, methodology: str, asset_class: str, symbol: str, 
                 config_root: Optional[Union[str, Path]] = None):
        """
        Initialize methodology configuration.
        
        Args:
            methodology: Name of the methodology (e.g., 'prophet', 'xgboost')
            asset_class: Asset class (e.g., 'crypto', 'forex')
            symbol: Asset symbol (e.g., 'ETH', 'EURUSD')
            config_root: Root directory for configuration files
        """
        self.methodology = methodology
        self.asset_class = asset_class
        self.symbol = symbol
        
        # Set config root directory
        if config_root is None:
            # Default to methodologies/{methodology}/configs/
            current_dir = Path(__file__).parent.parent.parent
            self.config_root = current_dir / "methodologies" / methodology / "configs"
        else:
            self.config_root = Path(config_root)
        
        # Load configurations in priority order
        self.base_config = self._load_base_config()
        self.asset_class_overrides = self._load_asset_class_overrides()
        self.symbol_overrides = self._load_symbol_overrides()
        self.runtime_overrides = {}
        
        # Merge configurations
        self.merged_config = self._merge_configurations()
        
        logger.info(f"Loaded configuration for {methodology}_{asset_class}_{symbol}")
    
    def _load_base_config(self) -> Dict[str, Any]:
        """Load base methodology configuration"""
        config_path = self.config_root / "default_config.json"
        
        if config_path.exists():
            try:
                with open(config_path, 'r') as f:
                    config = json.load(f)
                logger.debug(f"Loaded base config from {config_path}")
                return config
            except Exception as e:
                logger.warning(f"Failed to load base config from {config_path}: {e}")
        
        # Return default configurations for known methodologies
        return self._get_default_config()
    
    def _load_asset_class_overrides(self) -> Dict[str, Any]:
        """Load asset class specific overrides"""
        config_path = self.config_root / f"{self.asset_class.lower()}_overrides.json"
        
        if config_path.exists():
            try:
                with open(config_path, 'r') as f:
                    config = json.load(f)
                logger.debug(f"Loaded asset class overrides from {config_path}")
                return config
            except Exception as e:
                logger.warning(f"Failed to load asset class overrides from {config_path}: {e}")
        
        return {}
    
    def _load_symbol_overrides(self) -> Dict[str, Any]:
        """Load symbol-specific overrides"""
        config_path = self.config_root / f"{self.asset_class.lower()}_{self.symbol.lower()}_overrides.json"
        
        if config_path.exists():
            try:
                with open(config_path, 'r') as f:
                    config = json.load(f)
                logger.debug(f"Loaded symbol overrides from {config_path}")
                return config
            except Exception as e:
                logger.warning(f"Failed to load symbol overrides from {config_path}: {e}")
        
        return {}
    
    def _get_default_config(self) -> Dict[str, Any]:
        """Get default configuration for methodology"""
        defaults = {
            "prophet": {
                "methodology_params": {
                    "seasonality_mode": "multiplicative",
                    "yearly_seasonality": True,
                    "weekly_seasonality": True, 
                    "daily_seasonality": False,
                    "changepoint_prior_scale": 0.05,
                    "seasonality_prior_scale": 10.0,
                    "holidays_prior_scale": 10.0,
                    "uncertainty_samples": 1000,
                    "mcmc_samples": 0
                },
                "training_params": {
                    "train_size": 0.8,
                    "validation_size": 0.1,
                    "test_size": 0.1,
                    "min_training_periods": 100,
                    "max_training_periods": 2000
                },
                "validation_params": {
                    "cv_folds": 5,
                    "horizon_periods": 24,
                    "metrics": ["mape", "mae", "rmse", "r2"]
                }
            },
            "xgboost": {
                "methodology_params": {
                    "n_estimators": 200,
                    "max_depth": 6,
                    "learning_rate": 0.1,
                    "subsample": 0.8,
                    "colsample_bytree": 0.8,
                    "gamma": 0,
                    "min_child_weight": 1,
                    "reg_alpha": 0,
                    "reg_lambda": 1,
                    "random_state": 42
                },
                "training_params": {
                    "train_size": 0.8,
                    "validation_size": 0.1, 
                    "test_size": 0.1,
                    "min_training_periods": 200,
                    "max_training_periods": 5000,
                    "early_stopping_rounds": 50,
                    "eval_metric": "rmse"
                },
                "validation_params": {
                    "cv_folds": 5,
                    "horizon_periods": 24,
                    "metrics": ["mape", "mae", "rmse", "r2"],
                    "feature_importance_threshold": 0.01
                }
            },
            "ensemble": {
                "methodology_params": {
                    "prophet_weight": 0.4,
                    "xgboost_weight": 0.6,
                    "confidence_threshold": 0.7,
                    "consensus_required": True,
                    "weight_optimization": "performance_based",
                    "rebalance_frequency": "monthly"
                },
                "training_params": {
                    "require_all_methodologies": False,
                    "min_methodologies": 2,
                    "validation_window": 500,
                    "optimization_metric": "r2"
                },
                "validation_params": {
                    "cv_folds": 3,
                    "horizon_periods": 24,
                    "metrics": ["mape", "mae", "rmse", "r2", "sharpe_ratio"]
                }
            }
        }
        
        return defaults.get(self.methodology, {})
    
    def _merge_configurations(self) -> Dict[str, Any]:
        """Merge configurations with priority: runtime > symbol > asset_class > base"""
        merged = self._deep_copy_dict(self.base_config)
        
        # Apply asset class overrides
        merged = self._deep_merge_dicts(merged, self.asset_class_overrides)
        
        # Apply symbol overrides
        merged = self._deep_merge_dicts(merged, self.symbol_overrides)
        
        # Apply runtime overrides
        merged = self._deep_merge_dicts(merged, self.runtime_overrides)
        
        return merged
    
    def _deep_copy_dict(self, d: Dict[str, Any]) -> Dict[str, Any]:
        """Deep copy dictionary"""
        import copy
        return copy.deepcopy(d)
    
    def _deep_merge_dicts(self, base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
        """Deep merge two dictionaries"""
        result = base.copy()
        
        for key, value in override.items():
            if (key in result and 
                isinstance(result[key], dict) and 
                isinstance(value, dict)):
                result[key] = self._deep_merge_dicts(result[key], value)
            else:
                result[key] = value
        
        return result
    
    def get(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value.
        
        Args:
            key: Configuration key (supports dot notation)
            default: Default value if key not found
            
        Returns:
            Configuration value
        """
        keys = key.split('.')
        value = self.merged_config
        
        try:
            for k in keys:
                value = value[k]
            return value
        except (KeyError, TypeError):
            return default
    
    def set_runtime_override(self, key: str, value: Any):
        """
        Set runtime configuration override.
        
        Args:
            key: Configuration key (supports dot notation)
            value: Value to set
        """
        keys = key.split('.')
        current = self.runtime_overrides
        
        for k in keys[:-1]:
            if k not in current:
                current[k] = {}
            current = current[k]
        
        current[keys[-1]] = value
        
        # Re-merge configurations
        self.merged_config = self._merge_configurations()
        
        logger.debug(f"Set runtime override: {key} = {value}")
    
    def get_methodology_params(self) -> Dict[str, Any]:
        """Get methodology-specific parameters"""
        return self.get('methodology_params', {})
    
    def get_training_params(self) -> Dict[str, Any]:
        """Get training-specific parameters"""
        return self.get('training_params', {})
    
    def get_validation_params(self) -> Dict[str, Any]:
        """Get validation-specific parameters"""
        return self.get('validation_params', {})
    
    def get_feature_params(self) -> Dict[str, Any]:
        """Get feature engineering parameters"""
        return self.get('feature_params', {})
    
    def save_config(self, filepath: Union[str, Path]):
        """
        Save current merged configuration to file.
        
        Args:
            filepath: Path to save configuration
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        try:
            with open(filepath, 'w') as f:
                json.dump(self.merged_config, f, indent=2, default=str)
            logger.info(f"Saved configuration to {filepath}")
        except Exception as e:
            logger.error(f"Failed to save configuration to {filepath}: {e}")
    
    def to_dict(self) -> Dict[str, Any]:
        """
        Convert configuration to dictionary.
        
        Returns:
            Complete merged configuration as dictionary
        """
        return self._deep_copy_dict(self.merged_config)
    
    def __str__(self) -> str:
        """String representation of configuration"""
        return f"MethodologyConfig({self.methodology}_{self.asset_class}_{self.symbol})"
    
    def __repr__(self) -> str:
        """Detailed representation of configuration"""
        return (f"MethodologyConfig(methodology='{self.methodology}', "
                f"asset_class='{self.asset_class}', symbol='{self.symbol}', "
                f"config_keys={list(self.merged_config.keys())})")