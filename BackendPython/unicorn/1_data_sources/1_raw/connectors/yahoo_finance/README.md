# Yahoo Finance Connector

This directory contains the Yahoo Finance integration for the Unicorn Investing platform with unified multi-asset data collection.

## Overview

The Yahoo Finance connector provides free access to historical and real-time market data for stocks, cryptocurrencies, forex, and other financial instruments. **Updated September 2025** with unified asset collection system supporting multiple intervals and organized data storage.

## Features

- ✅ **No Authentication Required**: Free access to market data
- ✅ **Comprehensive Coverage**: ETH, BTC, 7 major forex pairs, stocks, indices
- ✅ **Multiple Intervals**: 1-minute, 1-hour, 1-day data collection
- ✅ **Real-time Data**: Live market prices and volume
- ✅ **Historical Data**: Years of historical OHLCV data
- ✅ **Organized Storage**: Asset/category/interval directory structure
- ✅ **Pipeline Integration**: Integrated with cron-based data pipeline

## Files

### Core Scripts
- `unified_asset_collector.py` - **NEW**: Unified collector for all assets (ETH, Forex, BTC)
- `YahooFinanceMinuteData.py` - LEAN framework integration
- `eth_data_collector.py` - Legacy ETH-specific data collection
- `eth_data_reader.py` - Analysis and reading of collected ETH data

### Testing & Examples
**Testing scripts have been moved to the centralized testing directory:**
- Testing location: `/tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/`
- Available tests: `test_eth_connector.py`, `enhanced_eth_test.py`, `comprehensive_eth_test.py`
- Run tests: `cd /workspaces/unicorninvesting && python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/`

## Supported Assets (9 Total)

### Cryptocurrencies (2)
- **ETH-USD** (Ethereum) - Primary focus
- **BTC-USD** (Bitcoin) - Secondary crypto

### Major Forex Pairs (7)
- **EURUSD** (EUR/USD) - Most traded globally
- **USDJPY** (USD/JPY) - High liquidity, safe haven
- **GBPUSD** (GBP/USD) - "The Cable"
- **AUDUSD** (AUD/USD) - Commodity-linked
- **USDCAD** (USD/CAD) - Oil-linked
- **USDCHF** (USD/CHF) - Safe haven
- **NZDUSD** (NZD/USD) - Agricultural commodity-linked

## Data Collection Intervals

| Interval | Period Coverage | Use Case | Pipeline |
|----------|----------------|----------|----------|
| **1m** | 5 days | High-frequency trading | Delta (every 30min) |
| **1h** | 1 month | Intraday analysis | Hourly + Daily |
| **1d** | 1 year | Long-term trends | Daily |

## Directory Structure

```
yahoo_finance/
├── crypto/
│   ├── ETH/
│   │   ├── 1m/ (latest.csv + timestamped files)
│   │   ├── 1h/ (latest.csv + timestamped files)
│   │   └── 1d/ (latest.csv + timestamped files)
│   └── BTC/
│       ├── 1m/, 1h/, 1d/
└── forex/
    ├── EURUSD/
    │   ├── 1m/, 1h/, 1d/
    ├── USDJPY/
    │   ├── 1m/, 1h/, 1d/
    └── [other pairs]/
        ├── 1m/, 1h/, 1d/
```

## Usage Examples

### Unified Asset Collector (Recommended)

```bash
# Show all available assets and intervals
python unified_asset_collector.py --summary

# Collect specific asset and interval
python unified_asset_collector.py --asset EURUSD --interval 1h

# Collect all intervals for specific asset
python unified_asset_collector.py --asset ETH --all-intervals

# Collect all assets for specific interval
python unified_asset_collector.py --all-assets --interval 1d
```

### Pipeline Integration

The unified collector is integrated with the main data pipeline:

```bash
# Daily pipeline: Collects 1d and 1h data for all assets
./scripts/data_pipeline.sh daily

# Delta pipeline: Collects 1m data for all assets
./scripts/data_pipeline.sh delta

# Hourly pipeline: Collects 1h data for all assets
./scripts/data_pipeline.sh hourly
```

### Legacy ETH-Specific Collection

```bash
# Legacy ETH data collection (still supported)
python eth_data_collector.py

```

## Data Storage

Data is now organized by asset category and interval:
```
/data/yahoo_finance/
├── crypto/
│   ├── ETH/
│   │   ├── 1m/
│   │   │   ├── latest.csv
│   │   │   └── ETH_1m_20250910_202133.csv
│   │   ├── 1h/ (similar structure)
│   │   └── 1d/ (similar structure)
│   └── BTC/ (similar structure)
└── forex/
    ├── EURUSD/
    │   ├── 1m/, 1h/, 1d/ (each with latest.csv + timestamped files)
    └── [other pairs]/ (similar structure)
```

### Data Format (Enhanced with Metadata)
```csv
Datetime,open,high,low,close,volume,dividends,stock_splits,symbol,assetcode,name,category,interval,source
2025-09-10 20:00:00+00:00,4506.87,4506.87,4506.87,4506.87,0,0.0,0.0,ETH-USD,ETH,Ethereum,crypto,1h,yahoo_finance
2025-09-10 20:00:00+01:00,1.1651,1.1653,1.1648,1.1651,0,0.0,0.0,EURUSD=X,EURUSD,EUR/USD,forex,1h,yahoo_finance
```

## Configuration

No API key or authentication required. The yfinance library handles all API interactions.

### Library Dependencies
```python
import yfinance as yf
import pandas as pd
import logging
from typing import Dict, List, Optional
```

## Cron Integration

Asset collection is automated via cron jobs:

```bash
# Setup cron jobs (includes Yahoo Finance asset collection)
./scripts/setup_data_cron.sh

# Verify cron schedule
crontab -l | grep data_pipeline
```

### Automated Schedule
- **Daily (10 PM)**: 1d + 1h data for all 9 assets
- **Delta (every 30min)**: 1m data for all 9 assets  
- **Hourly (every hour)**: 1h data for all 9 assets

## Performance & Reliability

### Collection Statistics
- **Assets Supported**: 9 total (2 crypto, 7 forex)
- **Data Points per Collection**: 500-750 records per asset per interval
- **File Sizes**: ~85KB per asset per interval
- **Success Rate**: 100% (tested September 2025)
- **Collection Time**: ~2 seconds per asset per interval

### Error Handling
- Automatic retry on network issues
- Graceful handling of missing data
- Comprehensive logging for debugging
- Pipeline-level error reporting

## Migration from Legacy Scripts

Existing `eth_data_collector.py` scripts continue to work, but the new `unified_asset_collector.py` is recommended for:
- ✅ Better organization (category/interval structure)
- ✅ Multi-asset support (ETH + Forex + BTC)
- ✅ Pipeline integration
- ✅ Enhanced metadata and logging
- ✅ Consistent data formats

## Advantages

1. **Free Access**: No API key required
2. **Comprehensive**: 9 assets across crypto and forex markets
3. **Multi-Interval**: 1m, 1h, 1d granularity options
4. **Organized Storage**: Clean directory structure by asset/interval
5. **Pipeline Integration**: Automated via cron with monitoring
6. **Rich Metadata**: Enhanced data format with asset classification
7. **Active Maintenance**: Well-maintained open-source yfinance library

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
