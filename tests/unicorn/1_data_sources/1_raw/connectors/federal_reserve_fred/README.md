# Federal Reserve FRED Connector Tests

## Overview
Testing suite for Federal Reserve Economic Data (FRED) API connector, covering economic indicators and data processing pipelines.

## Test Files

### `test_fred_connector.py`
- **Purpose**: Comprehensive FRED connector testing
- **Coverage**: API connectivity, data retrieval, economic indicators, feature engineering
- **Test Functions**:
  - `test_api_connection()` - API authentication and connectivity
  - `test_single_series()` - Individual economic series retrieval
  - `test_multiple_series()` - Batch series collection
  - `test_critical_indicators()` - Key economic indicators validation
  - `test_feature_engineering()` - Data transformation pipeline
  - `test_alpha_model_integration()` - Integration with alpha models
  - `test_series_metadata()` - Metadata validation and processing

## Test Coverage

### **API Connectivity**
- ✅ FRED API authentication
- ✅ Rate limit compliance
- ✅ Error handling and retries
- ✅ Response validation

### **Economic Data Series**
- ✅ **GDP**: Gross Domestic Product indicators
- ✅ **Inflation**: CPI, Core CPI, PCE metrics
- ✅ **Employment**: Unemployment rate, payroll data
- ✅ **Interest Rates**: Federal funds rate, Treasury yields
- ✅ **Money Supply**: M1, M2 monetary aggregates

### **Data Processing**
- ✅ Time series alignment
- ✅ Missing data handling
- ✅ Frequency conversion
- ✅ Feature engineering pipeline
- ✅ Alpha model integration

## Running Tests

### Complete FRED Test Suite
```bash
cd /workspaces/unicorninvesting
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/test_fred_connector.py --verbose
```

### Specific Test Categories
```bash
# API connectivity only
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/test_fred_connector.py::test_api_connection

# Economic indicators
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/test_fred_connector.py::test_critical_indicators

# Feature engineering
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/test_fred_connector.py::test_feature_engineering
```

## Test Data

### **Economic Indicators Tested**
- **GDPC1**: Real GDP
- **CPIAUCSL**: Consumer Price Index
- **UNRATE**: Unemployment Rate
- **FEDFUNDS**: Federal Funds Rate
- **DGS10**: 10-Year Treasury Rate
- **M2SL**: M2 Money Supply

### **Data Validation**
- Historical data consistency
- Current data availability
- Metadata accuracy
- Series relationships

## Requirements

### **API Access**
- FRED API key configured in `config/secrets.json`
- Internet connectivity for API calls
- Valid FRED account and permissions

### **Dependencies**
```bash
pip install pandas requests pytest fredapi
```

### **Environment Setup**
```bash
# Verify FRED API key
python -c "import json; print('FRED Key:', 'FRED_API_KEY' in json.load(open('config/secrets.json')))"

# Test basic connectivity
python -c "from fredapi import Fred; f = Fred(); print('FRED API accessible')"
```

## Test Configuration

### **Rate Limiting**
- Tests respect FRED API rate limits
- Automatic retry with exponential backoff
- Mock responses for unit tests to avoid API calls

### **Data Freshness**
- Tests validate recent data availability
- Alerts for stale economic indicators
- Automatic updates for series metadata

---

**Source Location**: Originally located in `BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/`
**Moved**: September 2025 for centralized test management