"""
ETH Prophet Framework Configuration

Configuration file for the ETH Prophet model framework.
Contains all model parameters and system settings.
"""

import os
from typing import Dict, Any, List
from pathlib import Path

# Base configuration
BASE_CONFIG = {
    'data_path': '/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/CRYPTO/ETH/',
    'models_path': '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/models/',
    'reports_path': '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/reports/',
    'database_path': '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_prophet_comparison.db',
    'validation_split': 0.2,
    'forecast_horizon': 30,  # days
    'min_training_days': 365,
    'retrain_frequency_days': 7,
}

# Model-specific configurations
MODEL_CONFIGS = {
    'basic': {
        'description': 'Basic Prophet model with standard configuration',
        'prophet_params': {
            'seasonality_mode': 'additive',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.05,
            'seasonality_prior_scale': 10.0,
            'interval_width': 0.80,
            'growth': 'linear'
        },
        'use_external_regressors': False,
        'use_holidays': False,
        'feature_engineering': False
    },
    
    'enhanced': {
        'description': 'Enhanced Prophet model with external regressors and custom features',
        'prophet_params': {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.1,
            'seasonality_prior_scale': 15.0,
            'interval_width': 0.80,
            'growth': 'linear',
            'uncertainty_samples': 1000
        },
        'use_external_regressors': True,
        'use_holidays': True,
        'feature_engineering': True,
        'regressors': {
            'volume_normalized': {'prior_scale': 10.0},
            'volatility': {'prior_scale': 5.0},
            'momentum_7d': {'prior_scale': 8.0},
            'price_position': {'prior_scale': 6.0}
        }
    },
    
    'optimized': {
        'description': 'Optimized Prophet model with hyperparameter tuning',
        'prophet_params': {
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': True,
            'changepoint_prior_scale': 0.15,
            'seasonality_prior_scale': 20.0,
            'holidays_prior_scale': 12.0,
            'interval_width': 0.85,
            'growth': 'linear',
            'uncertainty_samples': 1500,
            'mcmc_samples': 0,
            'changepoint_range': 0.9
        },
        'use_external_regressors': True,
        'use_holidays': True,
        'feature_engineering': True,
        'custom_seasonalities': [
            {
                'name': 'crypto_weekly',
                'period': 7,
                'fourier_order': 3,
                'prior_scale': 15.0
            },
            {
                'name': 'crypto_monthly',
                'period': 30.5,
                'fourier_order': 5,
                'prior_scale': 10.0
            }
        ],
        'regressors': {
            'volume_normalized': {'prior_scale': 12.0},
            'volatility': {'prior_scale': 8.0},
            'momentum_3d': {'prior_scale': 6.0},
            'momentum_7d': {'prior_scale': 10.0},
            'price_position': {'prior_scale': 8.0},
            'volume_ratio': {'prior_scale': 7.0},
            'sma_ratio': {'prior_scale': 5.0}
        }
    }
}

# Performance evaluation configuration
EVALUATION_CONFIG = {
    'metrics': [
        'mape',           # Mean Absolute Percentage Error
        'mae',            # Mean Absolute Error
        'rmse',           # Root Mean Square Error
        'r2',             # R-squared
        'directional_accuracy',  # Direction prediction accuracy
        'sharpe_ratio',   # Sharpe ratio
        'max_drawdown',   # Maximum drawdown
        'volatility',     # Volatility
        'information_ratio',  # Information ratio
        'calmar_ratio'    # Calmar ratio
    ],
    'benchmark_models': ['naive', 'sma_7', 'sma_30', 'ema_12'],
    'cross_validation': {
        'initial': '365 days',
        'period': '30 days',
        'horizon': '90 days'
    },
    'confidence_levels': [0.80, 0.90, 0.95]
}

# Crypto-specific events and holidays
CRYPTO_EVENTS = [
    # Ethereum major events
    {'date': '2020-12-01', 'event': 'ETH 2.0 Beacon Chain Launch', 'impact': 'high'},
    {'date': '2021-08-05', 'event': 'London Hard Fork (EIP-1559)', 'impact': 'high'},
    {'date': '2022-09-15', 'event': 'The Merge (ETH 2.0)', 'impact': 'very_high'},
    {'date': '2023-04-12', 'event': 'Shapella Upgrade', 'impact': 'high'},
    
    # Bitcoin events that affect ETH
    {'date': '2020-05-11', 'event': 'Bitcoin Halving', 'impact': 'medium'},
    {'date': '2024-04-20', 'event': 'Bitcoin Halving (projected)', 'impact': 'medium'},
    
    # Market events
    {'date': '2021-01-01', 'event': 'Institutional Adoption Wave', 'impact': 'high'},
    {'date': '2022-03-01', 'event': 'Ukraine Crisis Impact', 'impact': 'high'},
    {'date': '2022-11-08', 'event': 'FTX Collapse', 'impact': 'very_high'},
]

# Feature engineering configuration
FEATURE_CONFIG = {
    'technical_indicators': {
        'sma_periods': [7, 14, 30, 50],
        'ema_periods': [12, 26, 50],
        'rsi_period': 14,
        'bollinger_period': 20,
        'macd_fast': 12,
        'macd_slow': 26,
        'macd_signal': 9
    },
    'volatility_measures': {
        'rolling_periods': [7, 14, 30],
        'garch_model': True,
        'realized_volatility': True
    },
    'momentum_indicators': {
        'periods': [1, 3, 7, 14, 30],
        'rate_of_change': True,
        'price_momentum': True
    },
    'volume_indicators': {
        'volume_sma_periods': [7, 14, 30],
        'volume_ratio': True,
        'obv': True,  # On-Balance Volume
        'volume_price_trend': True
    }
}

# Data requirements
DATA_REQUIREMENTS = {
    'required_columns': ['Open', 'High', 'Low', 'Close', 'Volume'],
    'optional_columns': ['Adj Close'],
    'min_data_points': 365,
    'data_frequency': 'daily',
    'data_sources': ['yahoo', 'binance', 'coinbase', 'local'],
    'data_validation': {
        'check_gaps': True,
        'max_gap_days': 3,
        'check_outliers': True,
        'outlier_threshold': 3.0  # standard deviations
    }
}

# Production deployment configuration
PRODUCTION_CONFIG = {
    'model_selection_criteria': {
        'primary_metric': 'mape',
        'secondary_metrics': ['directional_accuracy', 'sharpe_ratio'],
        'minimum_r2': 0.3,
        'maximum_mape': 15.0,
        'minimum_directional_accuracy': 55.0
    },
    'monitoring': {
        'performance_degradation_threshold': 0.1,  # 10% increase in MAPE
        'retraining_trigger_days': 30,
        'prediction_confidence_threshold': 0.6,
        'alert_thresholds': {
            'mape_increase': 20.0,  # %
            'directional_accuracy_drop': 10.0,  # %
            'prediction_interval_width': 50.0  # %
        }
    },
    'deployment': {
        'model_versioning': True,
        'a_b_testing': True,
        'rollback_capability': True,
        'performance_logging': True
    }
}

# Risk management configuration
RISK_CONFIG = {
    'position_sizing': {
        'max_position_size': 0.05,  # 5% of portfolio
        'volatility_scaling': True,
        'kelly_criterion': False
    },
    'risk_limits': {
        'max_daily_loss': 0.02,  # 2%
        'max_weekly_loss': 0.05,  # 5%
        'max_monthly_loss': 0.10,  # 10%
        'var_confidence': 0.95,
        'expected_shortfall': True
    },
    'stop_loss': {
        'enabled': True,
        'method': 'volatility_based',  # 'fixed', 'volatility_based', 'atr_based'
        'multiplier': 2.0,
        'trailing': True
    }
}

def get_config(config_type: str = 'base') -> Dict[str, Any]:
    """
    Get configuration by type.
    
    Args:
        config_type: Type of configuration to retrieve
        
    Returns:
        Configuration dictionary
    """
    configs = {
        'base': BASE_CONFIG,
        'models': MODEL_CONFIGS,
        'evaluation': EVALUATION_CONFIG,
        'features': FEATURE_CONFIG,
        'data': DATA_REQUIREMENTS,
        'production': PRODUCTION_CONFIG,
        'risk': RISK_CONFIG
    }
    
    return configs.get(config_type, {})

def get_model_config(model_name: str) -> Dict[str, Any]:
    """
    Get specific model configuration.
    
    Args:
        model_name: Name of the model ('basic', 'enhanced', 'optimized')
        
    Returns:
        Model configuration dictionary
    """
    return MODEL_CONFIGS.get(model_name, {})

def validate_config() -> bool:
    """
    Validate all configurations.
    
    Returns:
        True if all configurations are valid
    """
    try:
        # Check required paths
        required_paths = [
            BASE_CONFIG['data_path'],
            BASE_CONFIG['models_path'],
            BASE_CONFIG['reports_path']
        ]
        
        for path in required_paths:
            Path(path).mkdir(parents=True, exist_ok=True)
        
        # Validate model configurations
        for model_name, config in MODEL_CONFIGS.items():
            required_keys = ['description', 'prophet_params']
            for key in required_keys:
                if key not in config:
                    raise ValueError(f"Missing {key} in {model_name} config")
        
        return True
        
    except Exception as e:
        print(f"Configuration validation failed: {e}")
        return False

def create_directories():
    """Create all required directories."""
    directories = [
        BASE_CONFIG['data_path'],
        BASE_CONFIG['models_path'],
        BASE_CONFIG['reports_path'],
        Path(BASE_CONFIG['database_path']).parent
    ]
    
    for directory in directories:
        Path(directory).mkdir(parents=True, exist_ok=True)
        print(f"✅ Created directory: {directory}")

if __name__ == "__main__":
    print("ETH Prophet Framework Configuration")
    print("=" * 40)
    
    # Validate configuration
    if validate_config():
        print("✅ Configuration validation passed")
    else:
        print("❌ Configuration validation failed")
    
    # Create directories
    create_directories()
    
    # Display configuration summary
    print("\n📋 Configuration Summary:")
    print(f"  Models: {list(MODEL_CONFIGS.keys())}")
    print(f"  Metrics: {len(EVALUATION_CONFIG['metrics'])}")
    print(f"  Features: {len(FEATURE_CONFIG)}")
    print(f"  Crypto Events: {len(CRYPTO_EVENTS)}")
    print("  ✅ Framework ready for deployment")
