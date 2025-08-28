# Yahoo Finance Connector

This directory contains the Yahoo Finance integration for the Unicorn Investing platform.

## Overview

The Yahoo Finance connector provides free access to historical and real-time market data for stocks, cryptocurrencies, indices, and other financial instruments.

## Features

- ✅ **No Authentication Required**: Free access to market data
- ✅ **Comprehensive Coverage**: Stocks, crypto, indices, forex
- ✅ **Multiple Timeframes**: Minute, hourly, daily data
- ✅ **Real-time Data**: Live market prices and volume
- ✅ **Historical Data**: Years of historical OHLCV data

## Files

- `eth_data_collector.py` - ETH cryptocurrency data collection
- `eth_data_reader.py` - Analysis and reading of collected ETH data

## Supported Assets

### Cryptocurrencies
- ETH-USD (Ethereum)
- BTC-USD (Bitcoin)
- ADA-USD (Cardano)
- And many more crypto pairs

### Stocks
- All major US exchanges (NYSE, NASDAQ)
- International stocks with appropriate suffixes
- ETFs and mutual funds

### Indices
- S&P 500 (^GSPC)
- NASDAQ (^IXIC)
- Dow Jones (^DJI)

## Usage Examples

### Collect ETH Data
```bash
# Activate Python environment
source /workspaces/unicorninvesting/.venv/bin/activate

# Run ETH data collection
python eth_data_collector.py
```

### Analyze Collected Data
```bash
python eth_data_reader.py
```

## Data Storage

Collected data is stored in CSV format:
```
/data/yahoo_finance/{SYMBOL}/
└── {SYMBOL}_YYYYMMDD_HHMMSS.csv
```

### Data Format
```csv
Datetime,open,high,low,close,volume,dividends,stock splits,source,symbol,provider
2025-08-28 00:00:00+00:00,4506.87,4506.87,4506.87,4506.87,0,0.0,0.0,yahoo_finance,ETH-USD,Yahoo Finance
```

## Configuration

No API key or authentication required. The yfinance library handles all API interactions.

### Library Dependencies
```python
import yfinance as yf
import pandas as pd
```

## Advantages

1. **Free Access**: No API key required
2. **Simple Integration**: Easy-to-use Python library
3. **Rich Data**: OHLCV + dividends, splits, etc.
4. **Wide Coverage**: Global markets and instruments
5. **Active Maintenance**: Well-maintained open-source library

## Limitations

1. **Rate Limits**: Implicit rate limiting (avoid excessive requests)
2. **Data Quality**: Occasional gaps or delays in data
3. **No Real-time Guarantee**: Data may have slight delays
4. **No Official Support**: Unofficial API access

## Health Check Integration

The Yahoo Finance connector is validated by the main health check script:

```bash
# Check Yahoo Finance integration status
/workspaces/unicorninvesting/scripts/unicorn_environment.sh --check-only
```

Validates:
- ✅ yfinance library availability
- ✅ Data collection capabilities
- ✅ File system access for data storage

## Troubleshooting

### Common Issues

1. **Import Errors**
   ```bash
   # Install yfinance library
   pip install yfinance
   ```

2. **Network Issues**
   - Check internet connectivity
   - Verify Yahoo Finance website accessibility
   - Wait for temporary outages to resolve

3. **Data Collection Fails**
   - Verify symbol format (e.g., "ETH-USD" not "ETH")
   - Check if market is open for real-time data
   - Reduce request frequency to avoid rate limits

4. **Empty Data Results**
   - Verify symbol exists on Yahoo Finance
   - Check date ranges for historical requests
   - Some symbols may have limited data availability

## Best Practices

1. **Rate Limiting**: Add delays between requests for bulk operations
2. **Error Handling**: Implement retry logic for network issues
3. **Data Validation**: Check for missing or invalid data points
4. **Caching**: Store frequently accessed data locally
5. **Symbol Verification**: Validate symbols before data requests

## Production Considerations

- **Reliability**: Implement fallback data sources for critical operations
- **Monitoring**: Track data quality and availability
- **Storage**: Consider database storage for large datasets
- **Performance**: Use efficient data structures for analysis
- **Compliance**: Respect Yahoo's terms of service
