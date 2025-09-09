# Data Source Connectors

This directory contains connectors for various economic and financial data sources used in the Unicorn Investing platform. Each connector provides standardized access to external APIs and data feeds for integration with our alpha models and risk management systems.

## Architecture Overview

All connectors follow a standardized interface pattern:
- **Authentication**: API key management and secure credential handling
- **Data Retrieval**: Standardized methods for fetching historical and real-time data
- **Error Handling**: Robust error handling and retry mechanisms
- **Rate Limiting**: Compliance with API rate limits and throttling
- **Data Validation**: Input validation and data quality checks
- **Caching**: Intelligent caching to minimize API calls and costs

## Financial Market Data Connectors

### Interactive Brokers (`interactive_brokers/`)
- **Status**: ✅ **ACTIVE** - Production Ready
- **Purpose**: Real-time market data, portfolio management, and trade execution
- **Features**: 
  - Live portfolio data integration
  - Account information and positions
  - Real-time price feeds
  - Historical market data
- **API**: IBKR Gateway REST API
- **Authentication**: Session-based authentication with gateway
- **Rate Limits**: Real-time streaming, historical data limits apply
- **Dependencies**: IBKR Gateway service running

### Alpha Vantage (`alpha_vantage/`, `AlphaVantageMinuteData.py`)
- **Status**: ❌ **PLACEHOLDER** - Not Implemented
- **Purpose**: Financial market data, technical indicators, and economic data
- **Features**: 
  - Intraday and daily stock prices
  - Technical indicators (SMA, EMA, RSI, MACD)
  - Economic indicators integration
  - Cryptocurrency data
- **API**: Alpha Vantage REST API
- **Authentication**: API key required
- **Rate Limits**: 5 API calls per minute, 500 calls per day (free tier)
- **Implementation**: Empty file requires development

### Yahoo Finance (`yahoo_finance/`, `YahooFinanceMinuteData.py`)
- **Status**: 🔄 **PARTIAL** - Basic Implementation
- **Purpose**: Financial market data and historical prices
- **Features**:
  - Historical stock prices
  - Real-time quotes
  - Financial fundamentals
  - Market indices
- **API**: Yahoo Finance API (unofficial)
- **Authentication**: None required
- **Rate Limits**: Implicit rate limiting, no official limits
- **Implementation**: Basic functionality exists

## U.S. Government Economic Data Sources

### Bureau of Labor Statistics (`bureau_of_labor_statistics/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Employment, wages, productivity, and price data
- **Key Data Series**:
  - Consumer Price Index (CPI)
  - Employment statistics
  - Unemployment rates
  - Producer Price Index (PPI)
  - Employment Cost Index (ECI)
  - Productivity and costs
- **API**: BLS Public Data API v2
- **Authentication**: API key recommended for higher limits
- **Rate Limits**: 25 queries per day (no key), 500 queries per day (with key)
- **Data Frequency**: Monthly, quarterly, annual updates

### Bureau of Economic Analysis (`bureau_of_economic_analysis/`)
- **Status**: ✅ **PRODUCTION** - Complete API Implementation Deployed
- **Purpose**: Macroeconomic data including GDP, consumption, and business investment for crypto alpha models
- **Implementation**: Complete 900+ line production connector with command-line interface and shell script automation
- **Automation**:
  - Delta updates every 6 hours (critical indicators, 30-60s execution)
  - Daily updates at 6 AM (comprehensive indicators, 2-3 min execution)
  - Historical collection available (20+ years, 5-10 min execution)
- **Key Data Series**:
  - GDP data (`Real GDP`, `GDP Growth Rate`, `GDP by Industry`) - Quarterly releases
  - Consumer spending (`Personal Consumption Expenditures`, `Personal Saving Rate`) - Monthly data
  - Business investment (`Gross Private Investment`, `Fixed Assets`, `Equipment Investment`) - Quarterly data
  - International trade (`Current Account Balance`, `Trade in Services`) - Quarterly/annual data
  - Economic indicators (`Business Investment`, `Consumer Confidence Proxies`) - Various frequencies
- **Alpha Integration**: 
  - 50+ engineered features for crypto alpha models
  - Economic regime classification (recession, growth, transition indicators)
  - Growth acceleration metrics and economic strength index
  - Integration ready for ETH model enhancement
- **API**: BEA Data API (beaapi library)
- **Authentication**: API key required (free at https://apps.bea.gov/API/signup/)
- **Rate Limits**: Managed with 0.5s-2s adaptive throttling
- **Data Coverage**: 15+ datasets across 6 economic categories (2000-present)
- **Features**: Economic regime detection, GDP growth analysis, consumer spending trends
- **Priority**: ⭐ **HIGH** - Critical for macroeconomic context in crypto trading

### U.S. Census Bureau (`us_census_bureau/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Comprehensive economic and business statistics
- **Key Data Series**:
  - Economic Census data
  - County Business Patterns
  - American Community Survey
  - Monthly retail trade data
  - Manufacturing statistics
  - Construction spending
- **API**: Census Bureau API
- **Authentication**: API key required
- **Rate Limits**: 500 queries per IP per day
- **Data Frequency**: Various (monthly, quarterly, annual, decennial)

### Federal Reserve Economic Data (`federal_reserve_fred/`)
- **Status**: ✅ **PRODUCTION** - Fully Automated Pipeline Deployed
- **Purpose**: Comprehensive Federal Reserve economic time series data with automated collection
- **Implementation**: Complete 650+ line production connector with command-line interface
- **Automation**: 
  - Delta updates every 15 minutes (8 critical indicators, 10-15s execution)
  - Daily updates at 9 PM (16 series, 30-60s execution) 
  - Historical collection available (25+ years, 3-5 min execution)
- **Key Data Series**:
  - Federal funds rate (`FEDFUNDS`, `DFF`) - Current: 4.33%
  - Money supply (`M2SL`) - Current: $22.1T
  - Treasury rates (`DGS10`, `DGS2`, `DGS5`) - 10Y: 4.10%
  - Inflation indicators (`CPIAUCSL`, `CPILFESL`)
  - Employment data (`UNRATE`, `PAYEMS`)
  - GDP growth (`GDP`, `GDPC1`)
  - Market stress (`VIXCLS`, `NFCI`)
  - Currency strength (`DEXUSEU`, `TWEXBMTH`)
- **API**: FRED API (fredapi library)
- **Authentication**: API key configured (e4de78babaac7891e9896f8fa390e675)
- **Rate Limits**: 120 requests per minute (handled automatically)
- **Data Coverage**: 26,426+ observations (1919-2025)
- **Success Rate**: 95.7% (22/23 series operational)
- **Alpha Integration**: Economic features ready for ETH model enhancement
- **Priority**: ⭐ **HIGH** - Critical for monetary policy indicators

## International Organization Data Sources

### International Monetary Fund (`imf/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Global economic and financial statistics
- **Key Data Series**:
  - World Economic Outlook database
  - International Financial Statistics
  - Government Finance Statistics
  - Balance of payments data
  - Exchange rates
  - Global financial stability indicators
- **API**: IMF Data API (JSON RESTful)
- **Authentication**: None required for public data
- **Rate Limits**: Reasonable use policy
- **Data Frequency**: Annual, quarterly, monthly

### World Bank (`world_bank/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Development and economic data from around the world
- **Key Data Series**:
  - World Development Indicators
  - Country economic profiles
  - Poverty and inequality data
  - Climate change data
  - Financial sector data
- **API**: World Bank Data API
- **Authentication**: None required
- **Rate Limits**: No explicit limits
- **Data Frequency**: Annual, some quarterly and monthly

### OECD (`oecd/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Economic data for OECD member countries
- **Key Data Series**:
  - Main Economic Indicators
  - National accounts
  - Labour force statistics
  - Economic outlook
  - Productivity statistics
- **API**: OECD Data API (SDMX-JSON)
- **Authentication**: None required
- **Rate Limits**: Fair use policy
- **Data Frequency**: Monthly, quarterly, annual

## Private Economic Data Sources

### Conference Board (`conference_board/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Leading economic indicators and business confidence data
- **Key Data Series**:
  - Consumer Confidence Index
  - Leading Economic Indicators (LEI)
  - Help Wanted OnLine (HWOL) data
  - CEO Confidence Survey
- **API**: Custom API (subscription required)
- **Authentication**: Subscription and API credentials
- **Rate Limits**: Subscription-dependent
- **Data Frequency**: Monthly releases
- **Cost**: ⚠️ **PAID** - Subscription required

### National Bureau of Economic Research (`nber/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Economic research data and business cycle dating
- **Key Data Series**:
  - Historical macroeconomic data
  - Business cycle chronology
  - Research datasets
  - Industry productivity data
- **API**: Limited; primarily file downloads
- **Authentication**: None for public data
- **Rate Limits**: Reasonable use
- **Data Frequency**: Various, historical focus

### St. Louis Fed FRASER (`st_louis_fed_fraser/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Historical U.S. economic and banking data archive
- **Key Data Series**:
  - Historical Federal Reserve data
  - Banking statistics
  - Economic policy documents
  - Historical economic indicators
- **API**: Limited API access
- **Authentication**: None required
- **Rate Limits**: Reasonable use policy
- **Data Frequency**: Historical archives

### Statista (`statista/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Aggregated statistics from various sources
- **Key Data Series**:
  - Market research data
  - Industry statistics
  - Consumer data
  - Economic indicators compilation
- **API**: Statista API
- **Authentication**: Subscription required
- **Rate Limits**: Subscription-dependent
- **Data Frequency**: Various
- **Cost**: ⚠️ **PAID** - Subscription required

### DBnomics (`dbnomics/`)
- **Status**: 📋 **PLANNED** - Directory Created
- **Purpose**: Global economic database aggregating multiple providers
- **Key Data Series**:
  - Aggregated data from 80+ providers
  - International economic indicators
  - Central bank data
  - Statistical office data
- **API**: DBnomics API (RESTful)
- **Authentication**: None required for basic access
- **Rate Limits**: Generous limits
- **Data Frequency**: Varies by source
- **Priority**: ⭐ **HIGH** - Single API for multiple sources

## Implementation Priority

### Phase 1 - Core Economic Indicators (✅ COMPLETE)
1. ✅ **Federal Reserve Economic Data (FRED)** - Monetary policy indicators (PRODUCTION)
2. ✅ **Bureau of Economic Analysis (BEA)** - GDP and economic growth (PRODUCTION)
3. **Bureau of Labor Statistics (BLS)** - Employment and inflation data (PLANNED)

### Phase 2 - Market Integration (Short-term)
4. **Alpha Vantage** - Complete existing placeholder implementation
5. **DBnomics** - Multi-source aggregator for international data
6. **World Bank** - Global economic context

### Phase 3 - Advanced Sources (Medium-term)
7. **IMF** - International monetary data
8. **OECD** - Developed economy indicators
9. **U.S. Census Bureau** - Detailed economic surveys

### Phase 4 - Premium Sources (Long-term)
10. **Conference Board** - Confidence indicators (requires subscription)
11. **Statista** - Market research data (requires subscription)
12. **NBER** - Academic research data
13. **St. Louis Fed FRASER** - Historical data archive

## API Key Management

All API keys and credentials are managed through:
- Environment variables for local development
- Secure configuration files (not in version control)
- Azure Key Vault for production deployments
- Rotation policies for security

## Data Integration Framework

### Standard Output Format
All connectors output data in standardized formats:
```python
{
    "source": "connector_name",
    "timestamp": "ISO_8601_datetime",
    "data_series": "series_identifier", 
    "frequency": "daily|weekly|monthly|quarterly|annual",
    "values": [
        {
            "date": "YYYY-MM-DD",
            "value": numeric_value,
            "metadata": {...}
        }
    ]
}
```

### Error Handling
- Connection failures with exponential backoff
- Rate limit compliance with automatic throttling
- Data validation and quality checks
- Comprehensive logging for debugging

### Caching Strategy
- Local SQLite cache for development
- Redis cache for production
- TTL-based expiration policies
- Smart invalidation on data updates

## Testing Framework

Each connector includes:
- Unit tests for API functionality
- Integration tests with live APIs
- Mock data for development testing
- Performance benchmarking
- Error scenario testing

## Monitoring and Alerting

Production monitoring includes:
- API availability and response times
- Data freshness and quality checks
- Rate limit utilization
- Cost tracking for paid services
- Alert notifications for failures

## Contributing

When implementing new connectors:
1. Follow the standard interface pattern
2. Include comprehensive error handling
3. Implement proper rate limiting
4. Add unit and integration tests
5. Update this README with current status
6. Document API limits and costs

## Status Legend

- ✅ **ACTIVE** - Production ready and operational
- 🔄 **PARTIAL** - Partially implemented, basic functionality
- ❌ **PLACEHOLDER** - File exists but not implemented
- 📋 **PLANNED** - Directory created, implementation pending
- ⚠️ **PAID** - Requires paid subscription or API access
- ⭐ **HIGH** - High priority for implementation

## Dependencies

Core Python packages required:
```
requests>=2.28.0
pandas>=1.5.0
numpy>=1.23.0
python-dotenv>=0.19.0
redis>=4.3.0
sqlalchemy>=1.4.0
pytest>=7.0.0
```

API-specific packages will be documented in each connector's directory.
