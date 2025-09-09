#!/usr/bin/env python3
"""
Processors Package Initialization

This package contains specialized processors for different categories of economic indicators:
- Economic Growth (GDP, employment, industrial production)
- Monetary Policy (Fed rates, money supply, yield curves) 
- Consumer & Business Activity (spending, investment, retail)
- International Trade (trade balance, current account, exchange rates)

All processors inherit from BaseEconomicProcessor and produce standardized output
compatible with XGBoost alpha models at 1-minute, 1-hour, and 1-day intervals.
"""

from .base_processor import BaseEconomicProcessor

__all__ = [
    'BaseEconomicProcessor'
]
