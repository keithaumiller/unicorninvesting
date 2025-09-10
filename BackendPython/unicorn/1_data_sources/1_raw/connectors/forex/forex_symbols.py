"""
Forex Symbol Definitions for Multi-Currency Trading System

Issue #36: Multi-Currency Forex Alpha Models & Forecasting System

Defines all supported forex pairs with Yahoo Finance symbols and metadata.
"""

from typing import Dict, List, Tuple
from dataclasses import dataclass

@dataclass
class ForexPair:
    """Forex pair configuration"""
    yahoo_symbol: str
    name: str
    base_currency: str
    quote_currency: str
    category: str
    description: str
    is_commodity_linked: bool = False
    is_safe_haven: bool = False

# Major Currency Pairs (High Priority)
MAJOR_PAIRS = {
    'EURUSD': ForexPair(
        yahoo_symbol='EURUSD=X',
        name='EUR/USD',
        base_currency='EUR',
        quote_currency='USD',
        category='major',
        description='Euro/US Dollar - Most traded globally',
        is_commodity_linked=False,
        is_safe_haven=False
    ),
    'USDJPY': ForexPair(
        yahoo_symbol='USDJPY=X',
        name='USD/JPY',
        base_currency='USD',
        quote_currency='JPY',
        category='major',
        description='US Dollar/Japanese Yen - High liquidity, safe haven dynamics',
        is_commodity_linked=False,
        is_safe_haven=True
    ),
    'GBPUSD': ForexPair(
        yahoo_symbol='GBPUSD=X',
        name='GBP/USD',
        base_currency='GBP',
        quote_currency='USD',
        category='major',
        description='British Pound/US Dollar - "The Cable" high volume',
        is_commodity_linked=False,
        is_safe_haven=False
    ),
    'AUDUSD': ForexPair(
        yahoo_symbol='AUDUSD=X',
        name='AUD/USD',
        base_currency='AUD',
        quote_currency='USD',
        category='major',
        description='Australian Dollar/US Dollar - Commodity-linked',
        is_commodity_linked=True,
        is_safe_haven=False
    ),
    'USDCAD': ForexPair(
        yahoo_symbol='USDCAD=X',
        name='USD/CAD',
        base_currency='USD',
        quote_currency='CAD',
        category='major',
        description='US Dollar/Canadian Dollar - "The Loonie" oil-linked',
        is_commodity_linked=True,
        is_safe_haven=False
    ),
    'USDCHF': ForexPair(
        yahoo_symbol='USDCHF=X',
        name='USD/CHF',
        base_currency='USD',
        quote_currency='CHF',
        category='major',
        description='US Dollar/Swiss Franc - Safe haven pair',
        is_commodity_linked=False,
        is_safe_haven=True
    ),
    'NZDUSD': ForexPair(
        yahoo_symbol='NZDUSD=X',
        name='NZD/USD',
        base_currency='NZD',
        quote_currency='USD',
        category='major',
        description='New Zealand Dollar/US Dollar - Agricultural commodity-linked',
        is_commodity_linked=True,
        is_safe_haven=False
    )
}

# Cross Currency Pairs (Medium Priority)
CROSS_PAIRS = {
    'EURJPY': ForexPair(
        yahoo_symbol='EURJPY=X',
        name='EUR/JPY',
        base_currency='EUR',
        quote_currency='JPY',
        category='cross',
        description='Euro/Japanese Yen - European/Asian cross',
        is_commodity_linked=False,
        is_safe_haven=False
    ),
    'EURGBP': ForexPair(
        yahoo_symbol='EURGBP=X',
        name='EUR/GBP',
        base_currency='EUR',
        quote_currency='GBP',
        category='cross',
        description='Euro/British Pound - European cross',
        is_commodity_linked=False,
        is_safe_haven=False
    ),
    'GBPJPY': ForexPair(
        yahoo_symbol='GBPJPY=X',
        name='GBP/JPY',
        base_currency='GBP',
        quote_currency='JPY',
        category='cross',
        description='British Pound/Japanese Yen - High volatility cross',
        is_commodity_linked=False,
        is_safe_haven=False
    ),
    'EURAUD': ForexPair(
        yahoo_symbol='EURAUD=X',
        name='EUR/AUD',
        base_currency='EUR',
        quote_currency='AUD',
        category='cross',
        description='Euro/Australian Dollar - European/Oceanic cross',
        is_commodity_linked=False,
        is_safe_haven=False
    )
}

# Emerging Market Pairs (Future Expansion)
EMERGING_PAIRS = {
    'USDCNY': ForexPair(
        yahoo_symbol='USDCNY=X',
        name='USD/CNY',
        base_currency='USD',
        quote_currency='CNY',
        category='emerging',
        description='US Dollar/Chinese Renminbi - Major emerging market',
        is_commodity_linked=False,
        is_safe_haven=False
    )
}

# Combined symbols dictionary
FOREX_SYMBOLS = {**MAJOR_PAIRS, **CROSS_PAIRS, **EMERGING_PAIRS}

# Helper functions
def get_major_pairs() -> Dict[str, ForexPair]:
    """Get major currency pairs (highest liquidity)"""
    return MAJOR_PAIRS

def get_cross_pairs() -> Dict[str, ForexPair]:
    """Get cross currency pairs (medium liquidity)"""
    return CROSS_PAIRS

def get_emerging_pairs() -> Dict[str, ForexPair]:
    """Get emerging market pairs (lower liquidity)"""
    return EMERGING_PAIRS

def get_commodity_linked_pairs() -> Dict[str, ForexPair]:
    """Get commodity-linked currency pairs"""
    return {k: v for k, v in FOREX_SYMBOLS.items() if v.is_commodity_linked}

def get_safe_haven_pairs() -> Dict[str, ForexPair]:
    """Get safe haven currency pairs"""
    return {k: v for k, v in FOREX_SYMBOLS.items() if v.is_safe_haven}

def get_yahoo_symbols() -> List[str]:
    """Get all Yahoo Finance forex symbols"""
    return [pair.yahoo_symbol for pair in FOREX_SYMBOLS.values()]

def get_symbol_mapping() -> Dict[str, str]:
    """Get mapping from pair name to Yahoo Finance symbol"""
    return {k: v.yahoo_symbol for k, v in FOREX_SYMBOLS.items()}

# Priority implementation order
IMPLEMENTATION_PRIORITY = [
    # Phase 1: Major Pairs (Highest Priority)
    'EURUSD', 'USDJPY', 'GBPUSD',
    # Phase 2: Commodity & Safe Haven
    'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD',
    # Phase 3: Cross Pairs
    'EURJPY', 'EURGBP', 'GBPJPY', 'EURAUD',
    # Phase 4: Emerging Markets
    'USDCNY'
]

# Economic data mapping for currency fundamentals
ECONOMIC_DATA_MAPPING = {
    'USD': ['FEDFUNDS', 'GDP', 'CPIAUCSL', 'UNRATE', 'PAYEMS'],
    'EUR': ['ECBDFR', 'GDPQS_EUR', 'CP0000EZ19M086NEST'],
    'JPY': ['JPNIR', 'JPNGDP', 'JPNCPI'],
    'GBP': ['GBRIR', 'GDPQS_GBR', 'GBRCPI'],
    'AUD': ['AUDIR', 'GDPQS_AUS', 'CPIAU'],
    'CAD': ['CADIR', 'GDPQS_CAN', 'CANCPI'],
    'CHF': ['CHFIR', 'GDPQS_CHE', 'CHECPI'],
    'NZD': ['NZDIR', 'GDPQS_NZL', 'NZCPI'],
    'CNY': ['CNYIN', 'CHNGDP', 'CHNCPI']
}
