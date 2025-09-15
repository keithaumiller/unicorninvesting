# Yahoo Finance Connector Tests

## Overview
Comprehensive testing suite for Yahoo Finance data connectors, focusing on ETH, BTC, and Forex data collection and validation.

## Test Files

### `test_eth_connector.py`
- **Purpose**: Core ETH data collection testing
- **Coverage**: API connectivity, data retrieval, basic validation
- **Usage**: `python -m pytest test_eth_connector.py -v`

### `enhanced_eth_test.py`
- **Purpose**: Enhanced ETH data validation and quality checks
- **Coverage**: Data quality, timestamp validation, price consistency
- **Usage**: `python -m pytest enhanced_eth_test.py -v`

### `comprehensive_eth_test.py`
- **Purpose**: Complete ETH testing suite with edge cases
- **Coverage**: Symbol conversion, URL generation, error handling, performance
- **Usage**: `python -m pytest comprehensive_eth_test.py -v`

## Test Coverage

### **Data Collection**
- ✅ ETH-USD real-time data
- ✅ Historical data retrieval
- ✅ Multiple timeframe support
- ✅ API rate limiting compliance

### **Data Validation**
- ✅ Price data accuracy
- ✅ Volume data consistency
- ✅ Timestamp validation
- ✅ Schema compliance

### **Error Handling**
- ✅ Network connectivity issues
- ✅ API rate limit responses
- ✅ Invalid symbol handling
- ✅ Data corruption detection

## Running Tests

### All Yahoo Finance Tests
```bash
cd /workspaces/unicorninvesting
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/ --verbose
```

### Individual Tests
```bash
# Basic ETH connector test
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/test_eth_connector.py

# Enhanced validation
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/enhanced_eth_test.py

# Comprehensive suite
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/comprehensive_eth_test.py
```

## Test Data

### **Assets Tested**
- **ETH-USD**: Primary cryptocurrency focus
- **BTC-USD**: Secondary crypto validation
- **Forex Pairs**: EURUSD, USDJPY, GBPUSD, etc.

### **Timeframes**
- 1-minute intervals (primary)
- 5-minute, 15-minute, 1-hour
- Daily historical data

## Requirements
- Active internet connection for API calls
- Yahoo Finance API availability
- Python packages: `yfinance`, `pandas`, `pytest`

---

**Source Location**: Originally located in `BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/`
**Moved**: September 2025 for centralized test management