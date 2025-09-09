"""
Bureau of Economic Analysis (BEA) API Configuration

This module defines the key economic datasets available from the BEA API
that are most relevant for crypto trading alpha models and risk management.

BEA provides comprehensive GDP, national accounts, and economic activity data
that complements FRED monetary policy data for complete macroeconomic context.
"""

from dataclasses import dataclass
from typing import Dict, List, Optional
import datetime

@dataclass
class BEADatasetInfo:
    """Information about a BEA dataset."""
    dataset_name: str
    table_name: str
    description: str
    frequency: str  # A=Annual, Q=Quarterly, M=Monthly
    line_codes: List[str]  # Specific data series within the table
    crypto_relevance: int  # 1=Critical, 2=Important, 3=Context
    priority: int  # 1=High (delta updates), 2=Medium (daily), 3=Low (weekly)
    category: str
    
    def __post_init__(self):
        """Validate configuration after initialization."""
        if self.crypto_relevance not in [1, 2, 3]:
            raise ValueError("crypto_relevance must be 1, 2, or 3")
        if self.priority not in [1, 2, 3]:
            raise ValueError("priority must be 1, 2, or 3")


# BEA Economic Dataset Catalog
# Organized by economic category with crypto trading relevance
BEA_ECONOMIC_DATASETS = {
    'gross_domestic_product': {
        'description': 'GDP and economic growth indicators',
        'datasets': {
            'GDP_QUARTERLY': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T10101',  # Gross Domestic Product
                description='Real GDP - Quarterly growth rate',
                frequency='Q',
                line_codes=['1'],  # Real GDP
                crypto_relevance=1,
                priority=1,
                category='economic_growth'
            ),
            'GDP_ANNUAL': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T10101',
                description='Real GDP - Annual data', 
                frequency='A',
                line_codes=['1'],
                crypto_relevance=2,
                priority=2,
                category='economic_growth'
            ),
            'PERSONAL_INCOME': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T20100',  # Personal Income and Outlays
                description='Personal Income and Consumption',
                frequency='M',
                line_codes=['1', '2'],  # Personal Income, Personal Consumption
                crypto_relevance=2,
                priority=2,
                category='consumer_spending'
            )
        }
    },
    
    'consumer_spending': {
        'description': 'Personal consumption and spending patterns',
        'datasets': {
            'PCE_MONTHLY': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T20804',  # Personal Consumption Expenditures by Major Type
                description='Personal Consumption Expenditures - Monthly',
                frequency='M', 
                line_codes=['1', '2', '3'],  # Total PCE, Goods, Services
                crypto_relevance=2,
                priority=2,
                category='consumer_spending'
            ),
            'PERSONAL_SAVING': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T20100',
                description='Personal Saving Rate',
                frequency='M',
                line_codes=['12'],  # Personal Saving Rate
                crypto_relevance=1,
                priority=1, 
                category='consumer_spending'
            )
        }
    },
    
    'business_investment': {
        'description': 'Business capital investment and equipment spending',
        'datasets': {
            'BUSINESS_INVESTMENT': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T50100',  # Private Fixed Investment
                description='Private Fixed Investment by Type',
                frequency='Q',
                line_codes=['1', '3', '7'],  # Total, Equipment, Structures
                crypto_relevance=2,
                priority=2,
                category='business_investment'
            ),
            'CAPEX_SURVEY': BEADatasetInfo(
                dataset_name='CapitalExpenditureSurvey',
                table_name='T1',  # Annual Capital Expenditures Survey
                description='Business Capital Expenditures Survey',
                frequency='A',
                line_codes=['1'],  # Total Capital Expenditures
                crypto_relevance=3,
                priority=3,
                category='business_investment'
            )
        }
    },
    
    'international_trade': {
        'description': 'Trade balance and international economic activity', 
        'datasets': {
            'TRADE_BALANCE': BEADatasetInfo(
                dataset_name='IntlServTrade',
                table_name='T1',  # International Trade in Services
                description='Trade in Services Balance',
                frequency='M',
                line_codes=['1', '2', '3'],  # Exports, Imports, Balance
                crypto_relevance=2,
                priority=2,
                category='international_trade'
            ),
            'CURRENT_ACCOUNT': BEADatasetInfo(
                dataset_name='IntlServTrade',
                table_name='T2',
                description='Current Account Balance', 
                frequency='Q',
                line_codes=['1'],  # Current Account Balance
                crypto_relevance=2,
                priority=2,
                category='international_trade'
            )
        }
    },
    
    'regional_data': {
        'description': 'State and regional economic indicators',
        'datasets': {
            'STATE_GDP': BEADatasetInfo(
                dataset_name='RegionalData',
                table_name='SAGDP',  # State Annual GDP
                description='State GDP by Industry',
                frequency='A',
                line_codes=['1'],  # All Industries GDP
                crypto_relevance=3,
                priority=3,
                category='regional_data'
            )
        }
    },
    
    'industry_data': {
        'description': 'Industry-specific economic performance',
        'datasets': {
            'INDUSTRY_GDP': BEADatasetInfo(
                dataset_name='NIPA',
                table_name='T60100',  # Value Added by Industry
                description='GDP by Industry',
                frequency='Q',
                line_codes=['1', '11', '21'],  # Total, Tech, Finance
                crypto_relevance=2,
                priority=2,
                category='industry_data'
            )
        }
    }
}

# Priority-based dataset collections for automated updates
CRITICAL_DATASETS = [
    info for category in BEA_ECONOMIC_DATASETS.values()
    for info in category['datasets'].values() 
    if info.priority == 1
]

IMPORTANT_DATASETS = [
    info for category in BEA_ECONOMIC_DATASETS.values() 
    for info in category['datasets'].values()
    if info.priority == 2
]

ALL_DATASETS = [
    info for category in BEA_ECONOMIC_DATASETS.values()
    for info in category['datasets'].values()
]

# Crypto trading relevance mapping
CRYPTO_RELEVANCE_MAP = {
    1: "Critical for crypto models - Direct impact on risk appetite and capital flows",
    2: "Important context - Influences market sentiment and economic cycles", 
    3: "Background context - Useful for regime detection and long-term analysis"
}

# Feature engineering settings
FEATURE_CONFIG = {
    'rate_of_change_windows': [1, 2, 4, 8],  # Quarters for quarterly data
    'moving_average_windows': [2, 4, 8, 12], # Quarters/months
    'volatility_windows': [4, 8, 12],        # Quarters/months
    'regime_detection_windows': [8, 12, 16]  # Quarters for cycle detection
}

# Economic regime thresholds (based on historical analysis)
REGIME_THRESHOLDS = {
    'recession_gdp_threshold': -0.5,      # Negative GDP growth for 2+ quarters
    'high_growth_threshold': 4.0,        # GDP growth > 4% annually
    'low_savings_threshold': 5.0,        # Personal saving rate < 5%
    'investment_boom_threshold': 15.0,   # Business investment growth > 15%
    'trade_deficit_threshold': -50.0,    # Trade balance < -$50B monthly
}

# Data quality and validation settings
DATA_QUALITY_CONFIG = {
    'max_missing_values_pct': 10,        # Max 10% missing values
    'outlier_detection_method': 'iqr',   # IQR-based outlier detection
    'outlier_threshold': 3.0,            # 3 IQR threshold
    'min_data_points': 20,               # Minimum data points for analysis
    'data_staleness_days': 90,           # Data considered stale after 90 days
}

def get_datasets_by_priority(priority: int) -> List[BEADatasetInfo]:
    """Get all datasets matching a specific priority level."""
    return [
        info for category in BEA_ECONOMIC_DATASETS.values()
        for info in category['datasets'].values()
        if info.priority == priority
    ]

def get_datasets_by_category(category: str) -> List[BEADatasetInfo]:
    """Get all datasets in a specific category."""
    if category not in BEA_ECONOMIC_DATASETS:
        raise ValueError(f"Category '{category}' not found")
    return list(BEA_ECONOMIC_DATASETS[category]['datasets'].values())

def get_dataset_info(dataset_key: str) -> Optional[BEADatasetInfo]:
    """Get information about a specific dataset."""
    for category in BEA_ECONOMIC_DATASETS.values():
        if dataset_key in category['datasets']:
            return category['datasets'][dataset_key]
    return None

# BEA API configuration
BEA_API_CONFIG = {
    'base_url': 'https://apps.bea.gov/api/data',
    'user_agent': 'UnicornInvesting-Crypto-Alpha-Models/1.0',
    'timeout': 30,
    'retry_attempts': 3,
    'retry_delay': 1.0,
    'rate_limit_delay': 0.5,  # 0.5 seconds between requests
}

# Logging configuration  
LOGGING_CONFIG = {
    'level': 'INFO',
    'format': '%(asctime)s - %(levelname)s - %(message)s',
    'file_rotation': True,
    'max_file_size': '10MB',
    'backup_count': 5
}
