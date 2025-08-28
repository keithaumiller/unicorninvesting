# Raw Data Storage

This directory contains raw market data organized by data provider and asset.

## Directory Structure

```
data/
├── yahoo_finance/
│   └── ETH/                 # Ethereum data from Yahoo Finance
├── alpha_vantage/
│   └── ETH/                 # Ethereum data from Alpha Vantage  
└── interactive_brokers/
    └── ETH/                 # Ethereum data from Interactive Brokers
```

## Data Organization

### By Provider
- **yahoo_finance/**: Free real-time and historical cryptocurrency data
- **alpha_vantage/**: API-based financial data (requires API key)
- **interactive_brokers/**: Professional trading platform data

### By Asset
- **ETH/**: Ethereum cryptocurrency data
- Future assets will have their own directories (BTC/, AAPL/, etc.)

## File Naming Convention

Files should follow this pattern:
- `ETH_YYYYMMDD_HHMMSS.csv` - Timestamped data files
- `ETH_minute_data_YYYYMMDD.csv` - Daily minute-level data
- `ETH_historical_YYYY.csv` - Annual historical data

## Data Format

All CSV files should include these standard columns:
- `timestamp` - ISO 8601 timestamp
- `open` - Opening price
- `high` - High price  
- `low` - Low price
- `close` - Closing price
- `volume` - Trading volume
- `source` - Data provider identifier

## Usage

```python
# Example: Load ETH data from Yahoo Finance
import pandas as pd

eth_data = pd.read_csv('data/yahoo_finance/ETH/ETH_20250828_120000.csv')
```

## Data Sources

### Yahoo Finance
- **Advantages**: Free, real-time, no API key required
- **Coverage**: Cryptocurrencies, stocks, indices
- **Update Frequency**: 1-minute intervals
- **Connector**: `connectors/yahoo_finance/YahooFinanceMinuteData.py`

### Alpha Vantage
- **Advantages**: Professional API, extensive historical data
- **Coverage**: Stocks, forex, crypto, technical indicators
- **Rate Limits**: 5 calls/minute (free tier)
- **Connector**: `connectors/alpha_vantage/AlphaVantageMinuteData.py`

### Interactive Brokers
- **Advantages**: Real-time professional data, trading integration
- **Coverage**: Global markets, all asset classes
- **Requirements**: IBKR account, Client Portal Gateway
- **Connector**: `connectors/interactive_brokers/IBKRClientPortalConnector.py`

## Maintenance

- Data files should be compressed/archived after 30 days
- Monitor disk usage as crypto data can accumulate quickly
- Implement data retention policies based on analysis needs
