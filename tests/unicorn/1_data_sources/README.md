# Data Sources Testing Suite

## Overview
Centralized testing and validation for all data source components in the Unicorn Investing platform. This directory contains comprehensive test suites that were moved from the source directories to provide centralized test management.

# Data Sources Testing Suite

## Overview
Centralized testing and validation for all data source components in the Unicorn Investing platform. This directory contains comprehensive test suites that were moved from the source directories to provide centralized test management.

## 🏗️ Comprehensive Testing Script

### `test_data_warehouse.sh`
**Main testing wrapper that validates all layers of the data warehouse**

```bash
# Test all data warehouse layers
./test_data_warehouse.sh

# Test specific layers
./test_data_warehouse.sh --layer=raw
./test_data_warehouse.sh --layer=bronze
./test_data_warehouse.sh --layer=silver
./test_data_warehouse.sh --layer=gold

# Test specific connectors
./test_data_warehouse.sh --connector=yahoo
./test_data_warehouse.sh --connector=fred
./test_data_warehouse.sh --connector=ibkr
./test_data_warehouse.sh --connector=forex

# Additional options
./test_data_warehouse.sh --quick      # Skip integration tests
./test_data_warehouse.sh --verbose    # Detailed output
./test_data_warehouse.sh --help       # Show all options
```

**Features:**
- **Raw Layer (Layer 1)**: Tests all data connectors
- **Bronze Layer (Layer 2)**: Validates cleaned data
- **Silver Layer (Layer 3)**: Tests enriched data
- **Gold Layer (Layer 4)**: Validates aggregated data
- **ETL Pipelines**: Tests automated refresh
- **End-to-End Flow**: Complete pipeline validation
- Detailed reporting with success rates

## Directory Structure

```
tests/unicorn/1_data_sources/
├── test_data_warehouse.sh              # 🆕 Comprehensive testing script
├── test_ibkr_connection.py              # IBKR Gateway integration tests
├── 1_raw/                               # Raw layer testing
│   └── connectors/                      # Connector-specific tests
│       ├── yahoo_finance/               # Yahoo Finance connector tests
│       │   ├── test_eth_connector.py    # ETH data collection tests
│       │   ├── enhanced_eth_test.py     # Enhanced ETH validation
│       │   └── comprehensive_eth_test.py # Complete ETH test suite
│       ├── federal_reserve_fred/        # FRED connector tests
│       │   └── test_fred_connector.py   # FRED API and data tests
│       ├── forex/                       # Forex data tests
│       │   ├── test_forex_collection.py # Forex data collection
│       │   └── test_yahoo_intervals.py  # Yahoo interval testing
│       └── interactive_brokers/         # IBKR connector tests
│           ├── test_connector_demo.py   # IBKR connector demo
│           ├── test_data_quality.py     # Data quality validation
│           ├── test_e2e_pipeline.py     # End-to-end pipeline
│           ├── test_ibkr_integration.py # IBKR integration tests
│           └── test_technical_indicators.py # Technical analysis
├── data/                                # Test data and fixtures
└── database/                            # Database testing
```

## Test Categories

### 🔌 **Connector Tests**
- **Yahoo Finance**: ETH/BTC/Forex data collection and validation
- **FRED**: Federal Reserve economic data API testing
- **Interactive Brokers**: Gateway connection and trading data
- **Forex**: Currency pair data validation

### 📊 **Data Quality Tests**
- Schema validation and data integrity
- API response validation
- Historical data consistency
- Real-time data accuracy

### 🏗️ **Integration Tests**
- End-to-end data pipeline testing
- Cross-connector data consistency
- Database integration validation
- Performance and reliability testing

## Running Tests

### All Data Source Tests
```bash
cd /workspaces/unicorninvesting
python -m pytest tests/unicorn/1_data_sources/ --verbose
```

### Specific Connector Tests
```bash
# Yahoo Finance tests
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/ -v

# FRED connector tests
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/ -v

# IBKR tests
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/ -v
```

### Individual Test Files
```bash
# ETH connector testing
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/test_eth_connector.py

# FRED comprehensive testing
python -m pytest tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/test_fred_connector.py

# IBKR connection testing
python -m pytest tests/unicorn/1_data_sources/test_ibkr_connection.py
```

## Test Configuration

### Prerequisites
- Active Python virtual environment: `source /workspaces/unicorninvesting/.venv/bin/activate`
- API credentials configured in `config/secrets.json`
- Required packages: `pytest`, `pandas`, `yfinance`, `requests`

### Environment Setup
```bash
# Ensure environment is ready
cd /workspaces/unicorninvesting
source .venv/bin/activate

# Install testing dependencies
pip install pytest pytest-cov pytest-mock

# Verify API credentials
python -c "import json; print('Secrets:', list(json.load(open('config/secrets.json')).keys()))"
```

## Test Data Management

### **Test Fixtures**
- Located in `tests/unicorn/1_data_sources/data/`
- Sample data for offline testing
- Mock API responses for unit tests

### **Integration Test Data**
- Real API connections for integration tests
- ETH 1-minute data samples
- FRED economic indicator samples

## Continuous Integration

### **Automated Testing**
- All tests run on code changes
- Nightly comprehensive test runs
- Performance regression detection

### **Test Reporting**
- Coverage reports generated automatically
- Test results logged for trend analysis
- Failed test notifications and alerts

## Contributing to Tests

### **Adding New Tests**
1. Place tests in appropriate connector directory
2. Follow naming convention: `test_*.py` or `*_test.py`
3. Include docstrings and comments
4. Update this README with new test descriptions

### **Test Best Practices**
- Use meaningful test names that describe the scenario
- Include both positive and negative test cases
- Mock external APIs for unit tests
- Use real APIs sparingly for integration tests
- Clean up test data after test completion

---

**Note**: These tests were moved from the source directories (`BackendPython/unicorn/1_data_sources/`) to provide centralized test management and separation of concerns between production code and testing code.