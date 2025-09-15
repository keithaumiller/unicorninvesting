# Forex Data Connector Tests

## Overview
Testing suite for foreign exchange (Forex) data collection through Yahoo Finance and other providers.

## Test Files

### `test_forex_collection.py`
- **Purpose**: Forex data collection validation
- **Coverage**: Currency pair data retrieval, price validation, data consistency
- **Usage**: `python -m pytest test_forex_collection.py -v`

### `test_yahoo_intervals.py`
- **Purpose**: Yahoo Finance interval testing for various timeframes
- **Coverage**: 1m, 5m, 15m, 1h, 4h, 1d intervals validation
- **Usage**: `python -m pytest test_yahoo_intervals.py -v`

## Test Coverage

### **Currency Pairs Tested**
- **EURUSD**: Euro vs US Dollar (most traded pair)
- **USDJPY**: US Dollar vs Japanese Yen
- **GBPUSD**: British Pound vs US Dollar
- **USDCHF**: US Dollar vs Swiss Franc
- **AUDUSD**: Australian Dollar vs US Dollar
- **USDCAD**: US Dollar vs Canadian Dollar
- **NZDUSD**: New Zealand Dollar vs US Dollar

### **Data Validation**
- ✅ Real-time exchange rates
- ✅ Historical price accuracy
- ✅ Bid/Ask spread validation
- ✅ Volume data consistency
- ✅ Timestamp alignment

### **Timeframe Testing**
- ✅ 1-minute intervals (primary focus)
- ✅ 5-minute, 15-minute intervals
- ✅ Hourly and 4-hour intervals
- ✅ Daily historical data
- ✅ Cross-timeframe consistency

## Running Tests

### All Forex Tests
```bash
cd /workspaces/unicorninvesting
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/forex/ --verbose
```

### Individual Tests
```bash
# Forex collection testing
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/forex/test_forex_collection.py

# Yahoo interval testing
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/forex/test_yahoo_intervals.py
```

## Test Data

### **Market Coverage**
- Major currency pairs (USD-based)
- Cross currency pairs (non-USD)
- Exotic currency pairs (emerging markets)
- Commodity currencies (AUD, CAD, NZD)

### **Data Quality Checks**
- Price data within expected ranges
- No missing timestamps during market hours
- Consistent spread calculations
- Volume data availability and accuracy

## Requirements

### **Data Sources**
- Yahoo Finance API access
- Active internet connection
- Real-time market data availability

### **Dependencies**
```bash
pip install yfinance pandas pytest requests
```

### **Market Hours**
- Forex markets: 24/5 (Monday 00:00 UTC - Friday 22:00 UTC)
- Tests account for market closures and holidays
- Weekend data validation for historical tests

## Configuration

### **Test Parameters**
- Default timeframe: 1-minute intervals
- Historical data range: 1-30 days
- Real-time data sampling: 5-second intervals
- Timeout settings: 30 seconds per request

### **Error Handling**
- Network connectivity issues
- API rate limiting
- Market closure periods
- Invalid currency pair handling

---

**Source Location**: Originally located in `BackendPython/unicorn/1_data_sources/1_raw/connectors/forex/`
**Moved**: September 2025 for centralized test management