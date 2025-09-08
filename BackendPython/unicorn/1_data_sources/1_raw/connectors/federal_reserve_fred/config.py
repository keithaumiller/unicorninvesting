"""
FRED Connector Configuration

Series definitions and configuration settings for the Federal Reserve
Economic Data (FRED) connector using the fredapi library.
"""

from typing import Dict, List
from dataclasses import dataclass


@dataclass
class FredConfig:
    """Configuration settings for FRED API integration."""
    
    # Default data collection settings
    DEFAULT_LOOKBACK_YEARS: int = 5
    DEFAULT_LOOKBACK_WINDOWS: List[int] = None
    
    # Feature engineering settings
    ALPHA_FEATURE_WINDOWS: List[int] = None
    
    # Output settings
    DATA_OUTPUT_DIR: str = 'data'
    PROCESSED_OUTPUT_DIR: str = '../../../processed/economic_indicators'
    
    def __post_init__(self):
        if self.DEFAULT_LOOKBACK_WINDOWS is None:
            self.DEFAULT_LOOKBACK_WINDOWS = [5, 10, 20, 60, 252]  # Trading days
        
        if self.ALPHA_FEATURE_WINDOWS is None:
            self.ALPHA_FEATURE_WINDOWS = [5, 10, 20, 60, 252]  # 1w, 2w, 1m, 3m, 1y


# Economic series organized by category and priority for crypto alpha models
ECONOMIC_SERIES_CATALOG = {
    'monetary_policy': {
        'description': 'Federal Reserve monetary policy indicators',
        'crypto_relevance': 'High - Affects liquidity and risk appetite',
        'series': {
            'FEDFUNDS': {
                'name': 'Federal Funds Rate',
                'description': 'Effective Federal Funds Rate',
                'frequency': 'Monthly',
                'units': 'Percent',
                'priority': 1,
                'crypto_impact': 'Direct - Higher rates reduce crypto appetite'
            },
            'DFF': {
                'name': 'Daily Federal Funds Rate',
                'description': 'Effective Federal Funds Rate (Daily)',
                'frequency': 'Daily',
                'units': 'Percent',
                'priority': 1,
                'crypto_impact': 'Direct - Real-time policy impact'
            },
            'M2SL': {
                'name': 'M2 Money Supply',
                'description': 'M2 Money Stock',
                'frequency': 'Monthly',
                'units': 'Billions of Dollars',
                'priority': 1,
                'crypto_impact': 'High - Liquidity expansion drives crypto demand'
            },
            'WALCL': {
                'name': 'Fed Balance Sheet',
                'description': 'Federal Reserve Total Assets',
                'frequency': 'Weekly',
                'units': 'Millions of Dollars',
                'priority': 2,
                'crypto_impact': 'High - QE/QT directly affects crypto flows'
            }
        }
    },
    
    'inflation': {
        'description': 'Inflation and price level indicators',
        'crypto_relevance': 'High - Crypto as inflation hedge',
        'series': {
            'CPIAUCSL': {
                'name': 'Consumer Price Index',
                'description': 'Consumer Price Index: All Items',
                'frequency': 'Monthly',
                'units': 'Index 1982-1984=100',
                'priority': 1,
                'crypto_impact': 'High - Inflation drives alternative asset demand'
            },
            'CPILFESL': {
                'name': 'Core CPI',
                'description': 'Core CPI: All Items Less Food & Energy',
                'frequency': 'Monthly', 
                'units': 'Index 1982-1984=100',
                'priority': 1,
                'crypto_impact': 'High - Fed policy response indicator'
            },
            'PCEPI': {
                'name': 'PCE Price Index',
                'description': 'Personal Consumption Expenditures Price Index',
                'frequency': 'Monthly',
                'units': 'Index 2012=100',
                'priority': 2,
                'crypto_impact': 'Medium - Fed preferred inflation measure'
            }
        }
    },
    
    'interest_rates': {
        'description': 'Treasury yields and risk-free rates',
        'crypto_relevance': 'High - Risk-free rate benchmarks',
        'series': {
            'DGS10': {
                'name': '10-Year Treasury',
                'description': '10-Year Treasury Constant Maturity Rate',
                'frequency': 'Daily',
                'units': 'Percent',
                'priority': 1,
                'crypto_impact': 'High - Long-term risk-free rate benchmark'
            },
            'DGS2': {
                'name': '2-Year Treasury',
                'description': '2-Year Treasury Constant Maturity Rate',
                'frequency': 'Daily',
                'units': 'Percent',
                'priority': 1,
                'crypto_impact': 'High - Policy rate expectations'
            },
            'DGS5': {
                'name': '5-Year Treasury',
                'description': '5-Year Treasury Constant Maturity Rate',
                'frequency': 'Daily',
                'units': 'Percent',
                'priority': 1,
                'crypto_impact': 'Medium - Medium-term rate expectations'
            },
            'TB3MS': {
                'name': '3-Month Treasury',
                'description': '3-Month Treasury Bill Rate',
                'frequency': 'Monthly',
                'units': 'Percent',
                'priority': 2,
                'crypto_impact': 'Medium - Short-term funding costs'
            }
        }
    },
    
    'employment': {
        'description': 'Employment and labor market indicators',
        'crypto_relevance': 'Medium - Economic strength indicators',
        'series': {
            'UNRATE': {
                'name': 'Unemployment Rate',
                'description': 'Unemployment Rate',
                'frequency': 'Monthly',
                'units': 'Percent',
                'priority': 2,
                'crypto_impact': 'Medium - Economic health indicator'
            },
            'PAYEMS': {
                'name': 'Nonfarm Payrolls',
                'description': 'All Employees, Nonfarm Payrolls',
                'frequency': 'Monthly',
                'units': 'Thousands of Persons',
                'priority': 2,
                'crypto_impact': 'Medium - Employment strength'
            },
            'CIVPART': {
                'name': 'Labor Force Participation',
                'description': 'Labor Force Participation Rate',
                'frequency': 'Monthly',
                'units': 'Percent',
                'priority': 3,
                'crypto_impact': 'Low - Structural employment indicator'
            }
        }
    },
    
    'economic_growth': {
        'description': 'GDP and economic growth indicators',
        'crypto_relevance': 'Medium - Overall economic health',
        'series': {
            'GDP': {
                'name': 'Nominal GDP',
                'description': 'Gross Domestic Product',
                'frequency': 'Quarterly',
                'units': 'Billions of Dollars',
                'priority': 2,
                'crypto_impact': 'Medium - Economic growth strength'
            },
            'GDPC1': {
                'name': 'Real GDP',
                'description': 'Real Gross Domestic Product',
                'frequency': 'Quarterly',
                'units': 'Billions of Chained 2017 Dollars',
                'priority': 2,
                'crypto_impact': 'Medium - Inflation-adjusted growth'
            },
            'INDPRO': {
                'name': 'Industrial Production',
                'description': 'Industrial Production Index',
                'frequency': 'Monthly',
                'units': 'Index 2017=100',
                'priority': 3,
                'crypto_impact': 'Low - Manufacturing activity'
            }
        }
    },
    
    'market_indicators': {
        'description': 'Financial market stress and volatility',
        'crypto_relevance': 'High - Risk sentiment indicators',
        'series': {
            'VIXCLS': {
                'name': 'VIX',
                'description': 'CBOE Volatility Index (VIX)',
                'frequency': 'Daily',
                'units': 'Index',
                'priority': 2,
                'crypto_impact': 'High - Market fear/greed indicator'
            },
            'NFCI': {
                'name': 'Financial Conditions Index',
                'description': 'Chicago Fed National Financial Conditions Index',
                'frequency': 'Weekly',
                'units': 'Index',
                'priority': 2,
                'crypto_impact': 'High - Financial stress indicator'
            },
            'GSCPI': {
                'name': 'Goldman Sachs Commodity Index',
                'description': 'Goldman Sachs Commodity Index',
                'frequency': 'Daily',
                'units': 'Index',
                'priority': 3,
                'crypto_impact': 'Medium - Alternative asset demand'
            }
        }
    },
    
    'currency': {
        'description': 'USD exchange rates and dollar strength',
        'crypto_relevance': 'High - USD weakness supports crypto',
        'series': {
            'DEXUSEU': {
                'name': 'USD/EUR Exchange Rate',
                'description': 'U.S. / Euro Foreign Exchange Rate',
                'frequency': 'Daily',
                'units': 'U.S. Dollars to One Euro',
                'priority': 2,
                'crypto_impact': 'High - USD weakness supports crypto'
            },
            'TWEXBMTH': {
                'name': 'Trade Weighted USD Index',
                'description': 'Trade Weighted U.S. Dollar Index: Broad',
                'frequency': 'Monthly',
                'units': 'Index Jan 1997=100',
                'priority': 2,
                'crypto_impact': 'High - Broad USD strength measure'
            },
            'DEXCHUS': {
                'name': 'China/US Exchange Rate',
                'description': 'China / U.S. Foreign Exchange Rate',
                'frequency': 'Daily',
                'units': 'Chinese Yuan to One U.S. Dollar',
                'priority': 3,
                'crypto_impact': 'Medium - China trade impact'
            }
        }
    }
}


def get_series_by_priority(priority: int) -> List[str]:
    """Get all series IDs for a given priority level."""
    series_list = []
    for category, data in ECONOMIC_SERIES_CATALOG.items():
        for series_id, info in data['series'].items():
            if info['priority'] == priority:
                series_list.append(series_id)
    return series_list


def get_critical_series() -> List[str]:
    """Get all Priority 1 (critical) series for crypto alpha models."""
    return get_series_by_priority(1)


def get_important_series() -> List[str]:
    """Get all Priority 2 (important) series for crypto alpha models."""
    return get_series_by_priority(2)


def get_supplementary_series() -> List[str]:
    """Get all Priority 3 (supplementary) series for crypto alpha models."""
    return get_series_by_priority(3)


def get_all_series() -> List[str]:
    """Get all available series IDs."""
    series_list = []
    for category, data in ECONOMIC_SERIES_CATALOG.items():
        series_list.extend(data['series'].keys())
    return series_list


def get_series_by_category(category: str) -> List[str]:
    """Get all series IDs for a specific category."""
    if category not in ECONOMIC_SERIES_CATALOG:
        return []
    return list(ECONOMIC_SERIES_CATALOG[category]['series'].keys())


def get_crypto_relevant_series() -> List[str]:
    """Get series most relevant for crypto alpha models (Priority 1 & 2)."""
    return get_series_by_priority(1) + get_series_by_priority(2)


# Economic regime detection thresholds
REGIME_THRESHOLDS = {
    'high_inflation': 5.0,          # CPI YoY > 5%
    'low_rates': 2.0,               # Fed Funds < 2%
    'inverted_curve': 0.0,          # 2Y > 10Y yield
    'high_volatility': 30.0,        # VIX > 30
    'tight_financial_conditions': 0.5,  # NFCI > 0.5
    'strong_dollar': 110.0,         # Trade-weighted USD > 110
    'high_unemployment': 5.5        # Unemployment > 5.5%
}


# Feature engineering parameters
FEATURE_CONFIG = {
    'rate_of_change_windows': [5, 10, 20, 60, 252],
    'moving_average_windows': [5, 10, 20, 60, 252],
    'volatility_windows': [20, 60, 252],
    'z_score_windows': [252],
    'momentum_windows': [20, 60]
}


# Alpha model integration settings
ALPHA_MODEL_CONFIG = {
    'update_frequency': 'daily',
    'min_observations': 252,        # Minimum 1 year of data
    'max_missing_pct': 0.1,         # Max 10% missing data
    'outlier_threshold': 3.0,       # Z-score threshold for outliers
    'feature_lag': 1,               # Lag economic features by 1 period
    'standardize_features': True    # Standardize features for ML models
}
