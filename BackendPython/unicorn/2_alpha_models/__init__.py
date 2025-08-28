"""
Alpha Models - LEAN Framework Component 1
========================================

Signal Generation & Forecasting Components

Alpha Models are responsible for:
- Analyzing market data
- Generating trading insights/signals
- Forecasting price movements
- Determining signal confidence levels

Alpha Models should ONLY generate forecasts, not make trading decisions.

Organization:
- Framework Alpha Models: Clean LEAN framework implementations
- Legacy Algorithms: Original forecasting algorithms (for reference)
- Demos & Examples: Educational and testing implementations
- Specialized Models: Predictive analytics and recommendation systems
"""

# Framework-ready Alpha Models
from .AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha
from .EthFocusedAlpha import EthFocusedAlpha

# Legacy forecasting algorithms (for reference and migration)
# These should eventually be converted to proper Alpha Models

__all__ = [
    # Framework Alpha Models
    'AdvancedForexForecastingAlpha',
    'EthFocusedAlpha',
    
    # Legacy algorithms available for reference
    # Note: Consider converting these to proper AlphaModel classes
]

# Directory contents:
# - AdvancedForexForecastingAlpha.py: Multi-model ensemble Alpha Model
# - EthFocusedAlpha.py: ETH technical analysis Alpha Model  
# - advanced_forex_forecasting_algorithm.py: Legacy complete algorithm
# - prophet_forex_algorithm.py: Prophet-focused forecasting algorithm
# - xgboost_forex_algorithm.py: XGBoost ML forecasting algorithm
# - PureForecastingExample.py: Educational framework example
# - prophet_forex_demo.py: Prophet demonstration
# - simple_forex_forecasting_demo.py: Basic forecasting demo
# - standalone_forex_demo.py: Standalone forecasting example
# - quick_forecasting_comparison.py: Model comparison utility
# - forecasting_performance_analysis.py: Performance analysis tools
# - predictiveanalytics/: Advanced ML forecasting models
# - recomendationsystems/: Recommendation engine components
