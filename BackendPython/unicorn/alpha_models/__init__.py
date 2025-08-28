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
"""

from .AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha
from .EthFocusedAlpha import EthFocusedAlpha

__all__ = [
    'AdvancedForexForecastingAlpha',
    'EthFocusedAlpha'
]
