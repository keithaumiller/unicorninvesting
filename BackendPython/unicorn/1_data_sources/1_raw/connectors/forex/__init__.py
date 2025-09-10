"""
Forex Data Connectors Package

Multi-Currency Forex Alpha Models & Forecasting System
Issue #36 Implementation

This package provides forex-specific extensions to existing data connectors:
- Yahoo Finance forex data (extends existing yahoo_finance connector)
- Interactive Brokers forex market data (extends existing interactive_brokers connector)
- Economic data integration for currency fundamentals

Supported Currency Pairs:
- Major: EUR/USD, USD/JPY, GBP/USD, AUD/USD, USD/CAD, USD/CHF, NZD/USD
- Cross: EUR/JPY, EUR/GBP, GBP/JPY, EUR/AUD
- Emerging: USD/CNY (future)
"""

# Import from existing connectors
import sys
import os

# Add parent connectors to path
parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from .forex_data_collector import ForexDataCollector
from .forex_symbols import FOREX_SYMBOLS
from .ibkr_forex_connector import IBKRForexConnector

__all__ = [
    'ForexDataCollector',
    'FOREX_SYMBOLS',
    'IBKRForexConnector'
]
