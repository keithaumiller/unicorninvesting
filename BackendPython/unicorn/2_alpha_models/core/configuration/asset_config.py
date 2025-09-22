"""
Asset-specific configuration management.

Provides configuration templates and validation for different asset classes.
"""

from typing import Dict, Any, List, Optional, Union
from pathlib import Path
import json
import logging

logger = logging.getLogger(__name__)

class AssetConfig:
    """
    Asset-specific configuration management.
    
    Manages asset class characteristics, trading constraints, and 
    methodology-specific optimizations.
    """
    
    def __init__(self, asset_class: str, symbol: str):
        """
        Initialize asset configuration.
        
        Args:
            asset_class: Asset class name
            symbol: Asset symbol
        """
        self.asset_class = asset_class
        self.symbol = symbol
        self.characteristics = self._get_asset_characteristics()
        self.trading_constraints = self._get_trading_constraints()
        self.methodology_optimizations = self._get_methodology_optimizations()
    
    def _get_asset_characteristics(self) -> Dict[str, Any]:
        """Get asset class characteristics"""
        characteristics = {
            "crypto": {
                "market_hours": "24/7",
                "volatility_regime": "high",
                "liquidity_level": "medium_to_high",
                "tick_size": 0.00001,
                "minimum_trade_size": 0.001,
                "trading_costs": {
                    "spread_bps": 5,
                    "commission_pct": 0.1,
                    "slippage_bps": 10
                },
                "market_impact_factor": 0.1,
                "seasonal_patterns": ["weekend_effect", "end_of_month"],
                "correlation_factors": ["btc_correlation", "risk_sentiment"],
                "price_precision": 5,
                "volume_precision": 8
            },
            "forex": {
                "market_hours": "business_hours",
                "volatility_regime": "medium",
                "liquidity_level": "high",
                "tick_size": 0.0001,
                "minimum_trade_size": 1000,
                "trading_costs": {
                    "spread_bps": 1,
                    "commission_pct": 0.0,
                    "slippage_bps": 2
                },
                "market_impact_factor": 0.05,
                "seasonal_patterns": ["london_session", "ny_session", "end_of_month"],
                "correlation_factors": ["interest_rates", "economic_data"],
                "price_precision": 5,
                "volume_precision": 0
            },
            "equities": {
                "market_hours": "exchange_hours",
                "volatility_regime": "medium",
                "liquidity_level": "high",
                "tick_size": 0.01,
                "minimum_trade_size": 1,
                "trading_costs": {
                    "spread_bps": 2,
                    "commission_pct": 0.05,
                    "slippage_bps": 5
                },
                "market_impact_factor": 0.08,
                "seasonal_patterns": ["earnings_season", "options_expiry"],
                "correlation_factors": ["sector_rotation", "market_sentiment"],
                "price_precision": 2,
                "volume_precision": 0
            },
            "commodities": {
                "market_hours": "exchange_hours",
                "volatility_regime": "medium_to_high",
                "liquidity_level": "medium",
                "tick_size": 0.01,
                "minimum_trade_size": 1,
                "trading_costs": {
                    "spread_bps": 5,
                    "commission_pct": 0.1,
                    "slippage_bps": 15
                },
                "market_impact_factor": 0.15,
                "seasonal_patterns": ["harvest_cycles", "storage_costs"],
                "correlation_factors": ["usd_strength", "inflation_expectations"],
                "price_precision": 3,
                "volume_precision": 0
            }
        }
        
        base_char = characteristics.get(self.asset_class, characteristics["crypto"])
        
        # Apply symbol-specific customizations
        symbol_customizations = self._get_symbol_customizations()
        if symbol_customizations:
            for key, value in symbol_customizations.items():
                if isinstance(value, dict) and key in base_char:
                    base_char[key].update(value)
                else:
                    base_char[key] = value
        
        return base_char
    
    def _get_symbol_customizations(self) -> Dict[str, Any]:
        """Get symbol-specific customizations"""
        customizations = {
            # Crypto customizations
            "ETH": {
                "tick_size": 0.01,
                "minimum_trade_size": 0.01,
                "trading_costs": {"spread_bps": 3}
            },
            "BTC": {
                "tick_size": 0.01,
                "minimum_trade_size": 0.0001,
                "trading_costs": {"spread_bps": 2}
            },
            
            # Forex customizations
            "EURUSD": {
                "tick_size": 0.00001,
                "trading_costs": {"spread_bps": 0.5}
            },
            "USDJPY": {
                "tick_size": 0.001,
                "price_precision": 3,
                "trading_costs": {"spread_bps": 0.7}
            },
            "GBPUSD": {
                "tick_size": 0.00001,
                "trading_costs": {"spread_bps": 1.2}
            },
            
            # Equity customizations
            "AAPL": {
                "liquidity_level": "very_high",
                "trading_costs": {"spread_bps": 1}
            },
            "TSLA": {
                "volatility_regime": "high",
                "trading_costs": {"spread_bps": 5}
            }
        }
        
        return customizations.get(self.symbol, {})
    
    def _get_trading_constraints(self) -> Dict[str, Any]:
        """Get trading constraints for asset"""
        base_constraints = {
            "max_position_size_pct": 0.1,
            "max_daily_trades": 10,
            "min_hold_time_minutes": 5,
            "max_leverage": 1.0,
            "stop_loss_pct": 0.05,
            "take_profit_pct": 0.10,
            "risk_per_trade_pct": 0.02
        }
        
        # Asset class specific constraints
        asset_constraints = {
            "crypto": {
                "max_position_size_pct": 0.15,
                "max_daily_trades": 20,
                "max_leverage": 3.0,
                "stop_loss_pct": 0.08,
                "take_profit_pct": 0.15
            },
            "forex": {
                "max_position_size_pct": 0.2,
                "max_daily_trades": 50,
                "max_leverage": 10.0,
                "stop_loss_pct": 0.02,
                "take_profit_pct": 0.04
            },
            "equities": {
                "max_position_size_pct": 0.05,
                "max_daily_trades": 5,
                "max_leverage": 2.0,
                "stop_loss_pct": 0.03,
                "take_profit_pct": 0.06
            }
        }
        
        constraints = base_constraints.copy()
        constraints.update(asset_constraints.get(self.asset_class, {}))
        
        return constraints
    
    def _get_methodology_optimizations(self) -> Dict[str, Dict[str, Any]]:
        """Get methodology-specific optimizations for this asset"""
        optimizations = {
            "prophet": {
                "crypto": {
                    "changepoint_prior_scale": 0.1,
                    "seasonality_prior_scale": 15.0,
                    "daily_seasonality": True,
                    "weekly_seasonality": True,
                    "yearly_seasonality": False
                },
                "forex": {
                    "changepoint_prior_scale": 0.03,
                    "seasonality_prior_scale": 20.0,
                    "daily_seasonality": True,
                    "weekly_seasonality": True,
                    "yearly_seasonality": True
                },
                "equities": {
                    "changepoint_prior_scale": 0.05,
                    "seasonality_prior_scale": 10.0,
                    "daily_seasonality": False,
                    "weekly_seasonality": True,
                    "yearly_seasonality": True
                }
            },
            "xgboost": {
                "crypto": {
                    "learning_rate": 0.05,
                    "max_depth": 8,
                    "n_estimators": 300,
                    "subsample": 0.7,
                    "reg_alpha": 0.1
                },
                "forex": {
                    "learning_rate": 0.1,
                    "max_depth": 6,
                    "n_estimators": 200,
                    "subsample": 0.8,
                    "reg_alpha": 0.0
                },
                "equities": {
                    "learning_rate": 0.08,
                    "max_depth": 7,
                    "n_estimators": 250,
                    "subsample": 0.9,
                    "reg_alpha": 0.05
                }
            },
            "ensemble": {
                "crypto": {
                    "prophet_weight": 0.3,
                    "xgboost_weight": 0.7,
                    "confidence_threshold": 0.6
                },
                "forex": {
                    "prophet_weight": 0.5,
                    "xgboost_weight": 0.5,
                    "confidence_threshold": 0.8
                },
                "equities": {
                    "prophet_weight": 0.4,
                    "xgboost_weight": 0.6,
                    "confidence_threshold": 0.7
                }
            }
        }
        
        result = {}
        for methodology, asset_opts in optimizations.items():
            result[methodology] = asset_opts.get(self.asset_class, {})
        
        return result
    
    def get_feature_engineering_config(self, methodology: str) -> Dict[str, Any]:
        """
        Get feature engineering configuration for methodology.
        
        Args:
            methodology: Methodology name
            
        Returns:
            Feature engineering configuration
        """
        base_config = {
            "technical_indicators": {
                "sma_periods": [5, 10, 20, 50],
                "ema_periods": [12, 26],
                "rsi_period": 14,
                "macd_fast": 12,
                "macd_slow": 26,
                "macd_signal": 9,
                "bollinger_period": 20,
                "bollinger_std": 2
            },
            "price_features": {
                "returns": True,
                "log_returns": True,
                "high_low_ratio": True,
                "open_close_ratio": True,
                "price_ranges": [1, 5, 10, 20]
            },
            "volume_features": {
                "volume_sma": [10, 20],
                "volume_ratio": True,
                "price_volume": True
            },
            "time_features": {
                "hour_of_day": True,
                "day_of_week": True,
                "month_of_year": True,
                "quarter": True
            }
        }
        
        # Methodology-specific adjustments
        methodology_adjustments = {
            "prophet": {
                "time_features": {
                    "hour_of_day": True,
                    "day_of_week": True,
                    "month_of_year": True,
                    "quarter": True,
                    "is_weekend": True,
                    "is_month_end": True
                }
            },
            "xgboost": {
                "technical_indicators": {
                    "sma_periods": [5, 10, 20, 50, 100],
                    "ema_periods": [12, 26, 50],
                    "additional_oscillators": True
                },
                "lag_features": {
                    "price_lags": [1, 2, 3, 5, 10],
                    "return_lags": [1, 2, 3, 5],
                    "volume_lags": [1, 2, 3]
                }
            }
        }
        
        # Apply methodology adjustments
        if methodology in methodology_adjustments:
            for section, updates in methodology_adjustments[methodology].items():
                if section in base_config:
                    base_config[section].update(updates)
                else:
                    base_config[section] = updates
        
        return base_config
    
    def get_validation_config(self) -> Dict[str, Any]:
        """Get validation configuration for this asset"""
        base_validation = {
            "cv_folds": 5,
            "test_size": 0.2,
            "validation_size": 0.1,
            "metrics": ["r2", "mape", "mae", "rmse"],
            "performance_thresholds": {
                "min_r2": -0.3,
                "max_mape": 50.0,
                "max_mae": 100.0
            }
        }
        
        # Asset-specific validation adjustments
        asset_adjustments = {
            "crypto": {
                "performance_thresholds": {
                    "min_r2": -0.5,
                    "max_mape": 100.0,
                    "max_mae": 200.0
                }
            },
            "forex": {
                "cv_folds": 3,
                "performance_thresholds": {
                    "min_r2": -0.2,
                    "max_mape": 20.0,
                    "max_mae": 50.0
                }
            }
        }
        
        validation_config = base_validation.copy()
        validation_config.update(asset_adjustments.get(self.asset_class, {}))
        
        return validation_config
    
    def to_dict(self) -> Dict[str, Any]:
        """
        Convert asset configuration to dictionary.
        
        Returns:
            Complete asset configuration as dictionary
        """
        return {
            "asset_class": self.asset_class,
            "symbol": self.symbol,
            "characteristics": self.characteristics,
            "trading_constraints": self.trading_constraints,
            "methodology_optimizations": self.methodology_optimizations
        }
    
    def save_config(self, filepath: Union[str, Path]):
        """
        Save asset configuration to file.
        
        Args:
            filepath: Path to save configuration
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        try:
            with open(filepath, 'w') as f:
                json.dump(self.to_dict(), f, indent=2, default=str)
            logger.info(f"Saved asset config to {filepath}")
        except Exception as e:
            logger.error(f"Failed to save asset config to {filepath}: {e}")
    
    @classmethod
    def load_config(cls, filepath: Union[str, Path]) -> 'AssetConfig':
        """
        Load asset configuration from file.
        
        Args:
            filepath: Path to load configuration from
            
        Returns:
            AssetConfig instance
        """
        filepath = Path(filepath)
        
        with open(filepath, 'r') as f:
            config_data = json.load(f)
        
        asset_config = cls(
            config_data["asset_class"],
            config_data["symbol"]
        )
        
        # Override with loaded data
        asset_config.characteristics = config_data.get("characteristics", asset_config.characteristics)
        asset_config.trading_constraints = config_data.get("trading_constraints", asset_config.trading_constraints)
        asset_config.methodology_optimizations = config_data.get("methodology_optimizations", asset_config.methodology_optimizations)
        
        return asset_config
    
    def __str__(self) -> str:
        """String representation of asset config"""
        return f"AssetConfig({self.asset_class}_{self.symbol})"
    
    def __repr__(self) -> str:
        """Detailed representation of asset config"""
        return (f"AssetConfig(asset_class='{self.asset_class}', "
                f"symbol='{self.symbol}', "
                f"volatility='{self.characteristics.get('volatility_regime')}', "
                f"liquidity='{self.characteristics.get('liquidity_level')}')")