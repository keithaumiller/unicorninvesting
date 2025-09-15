# Data Warehouse Testing Suite

## Overview

The Data Warehouse Testing Suite provides comprehensive validation of Unicorn Investing's multi-layer data architecture. This testing framework validates data flow from raw market data ingestion through bronze, silver, and gold layers of the data warehouse.

## Architecture

### Data Warehouse Layers

```
┌─────────────────────────────────────────────────────────────┐
│                    UNICORN DATA WAREHOUSE                  │
├─────────────────────────────────────────────────────────────┤
│ 🗃️  RAW LAYER (Layer 1)     │ Market data ingestion       │
├─────────────────────────────────────────────────────────────┤
│ 🥉 BRONZE LAYER (Layer 2)   │ Data cleansing & validation │
├─────────────────────────────────────────────────────────────┤
│ 🥈 SILVER LAYER (Layer 3)   │ Structured & enriched data │
├─────────────────────────────────────────────────────────────┤
│ 🥇 GOLD LAYER (Layer 4)     │ Analytics-ready datasets   │
└─────────────────────────────────────────────────────────────┘
```

### Data Sources & Connectors

| Connector | Type | Purpose | Status |
|-----------|------|---------|--------|
| **Yahoo Finance** | Market Data | Stock/ETF/Options pricing | ✅ Active |
| **FRED** | Economic Data | Federal Reserve economic indicators | 🔧 Testing |
| **Interactive Brokers** | Trading Platform | Real-time market data & execution | 🔧 Testing |
| **Forex** | Currency Data | Foreign exchange rates | 🔧 Testing |

## Testing Framework

### Main Test Script

**File:** `test_data_warehouse.sh`

**Purpose:** Comprehensive validation of data warehouse infrastructure

**Usage:**
```bash
# Run all tests
./test_data_warehouse.sh

# Test specific layer
./test_data_warehouse.sh --layer=raw
./test_data_warehouse.sh --layer=bronze
./test_data_warehouse.sh --layer=silver
./test_data_warehouse.sh --layer=gold

# Test specific connector
./test_data_warehouse.sh --connector=yahoo
./test_data_warehouse.sh --connector=fred
./test_data_warehouse.sh --connector=ibkr
./test_data_warehouse.sh --connector=forex

# Quick mode (skip integration tests)
./test_data_warehouse.sh --quick

# Verbose output
./test_data_warehouse.sh --verbose
```

### Test Categories

#### 🗃️ Raw Layer Tests (Layer 1)
- **Yahoo Finance Connector:** Market data ingestion validation
- **FRED Connector:** Economic data API connectivity
- **IBKR Connector:** Interactive Brokers gateway integration
- **Forex Connector:** Currency data pipeline validation
- **IBKR Gateway Connection:** Real-time trading platform connectivity
- **Raw Data Validation:** Data quality and format checks
- **Database Integration:** Raw data storage validation

#### 🥉 Bronze Layer Tests (Layer 2)
- **Directory Structure:** Validation of bronze layer organization
- **Data Cleansing:** Quality control and validation processes
- **Schema Compliance:** Data format standardization

#### 🥈 Silver Layer Tests (Layer 3)
- **Directory Structure:** Validation of silver layer organization
- **Data Connector:** Silver layer integration with portfolio management
- **Data Enrichment:** Feature engineering and data enhancement
- **Performance Metrics:** ETH model performance tracking

#### 🥇 Gold Layer Tests (Layer 4)
- **Directory Structure:** Analytics-ready data organization
- **Portfolio Analytics:** Investment performance calculations
- **Risk Metrics:** Advanced risk assessment capabilities

## Test Results & Reporting

### Output Locations

**Console Output:** Real-time test progress with color-coded results
**JSON Results:** `datawarehousetestingresults/` directory (gitignored)

### Result Files
- `test_results_YYYYMMDD_HHMMSS.json` - Detailed test execution results
- `summary_YYYYMMDD_HHMMSS.json` - Executive summary with success rates
- `latest_results.json` - Symlink to most recent test results

### JSON Schema

```json
{
  "test_run": {
    "timestamp": "2025-09-15T13:16:26Z",
    "layer": "all",
    "connector": "all",
    "mode": "full"
  },
  "summary": {
    "total_tests": 11,
    "passed": 5,
    "failed": 6,
    "skipped": 0,
    "success_rate": 45
  },
  "layers": {
    "raw": {
      "connectors": {
        "yahoo_finance": {"status": "PASSED", "duration": 2.3},
        "fred": {"status": "FAILED", "error": "Connection timeout"},
        "ibkr": {"status": "FAILED", "error": "Gateway not running"},
        "forex": {"status": "FAILED", "error": "API key missing"}
      }
    },
    "bronze": {"status": "PASSED"},
    "silver": {"status": "PASSED"},
    "gold": {"status": "PASSED"}
  }
}
```

## Process Flow

### 1. Environment Validation
```bash
# Activate Python virtual environment
source .venv/bin/activate

# Verify test dependencies
python -m pytest --version
```

### 2. Layer-by-Layer Testing
1. **Raw Layer Validation**
   - Test each connector individually
   - Validate API connectivity
   - Check data ingestion pipelines
   - Verify gateway connections

2. **Bronze Layer Validation**
   - Directory structure checks
   - Data cleansing validation
   - Schema compliance verification

3. **Silver Layer Validation**
   - Enhanced data structure validation
   - Portfolio integration testing
   - Performance tracking verification

4. **Gold Layer Validation**
   - Analytics readiness assessment
   - Final data quality validation

### 3. Results Aggregation
- Collect test results from each layer
- Generate comprehensive summary
- Export results to JSON format
- Create symbolic links for latest results

## Directory Structure

```
tests/unicorn/1_data_sources/
├── test_data_warehouse.sh              # 🆕 Comprehensive testing script
├── datawarehousetestingresults/         # 📊 JSON test results (gitignored)
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

## Dependencies

### Required Python Packages
- `pytest` - Test framework
- `yfinance` - Yahoo Finance connector
- `pandas` - Data manipulation
- `sqlalchemy` - Database integration
- `requests` - HTTP API connectivity

### External Services
- **Yahoo Finance API** - Market data source
- **FRED API** - Economic data (requires API key)
- **Interactive Brokers Gateway** - Trading platform
- **MySQL Database** - Data storage backend

## Configuration

### Environment Variables
```bash
export FRED_API_KEY="your_fred_api_key"
export IBKR_HOST="localhost"
export IBKR_PORT="7497"
export MYSQL_HOST="localhost"
export MYSQL_USER="unicorn"
export MYSQL_PASSWORD="your_password"
```

### Configuration Files
- `config/database.json` - Database connection settings
- `config/secrets.json` - API keys and credentials (gitignored)

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

## Troubleshooting

### Common Issues

#### FRED Connector Failures
```bash
# Check API key configuration
echo $FRED_API_KEY

# Test FRED API connectivity
curl "https://api.stlouisfed.org/fred/series?series_id=GDP&api_key=$FRED_API_KEY&file_type=json"
```

#### IBKR Gateway Issues
```bash
# Check gateway status
./scripts/ibkr_status.sh

# Restart IBKR gateway
sudo systemctl restart ibkr-gateway
```

#### Database Connection Problems
```bash
# Test MySQL connectivity
mysql -h localhost -u unicorn -p

# Check database configuration
cat config/database.json
```

## Continuous Integration

### Automated Testing
- Tests run automatically on code changes
- Results stored in JSON format for CI/CD integration
- Success rate monitoring and alerting

### Performance Benchmarks
- Track test execution times
- Monitor data ingestion performance
- Validate system scalability

## Development Guidelines

### Adding New Tests
1. Create test files in appropriate layer directory
2. Follow pytest naming conventions (`test_*.py`)
3. Include comprehensive docstrings
4. Add connector-specific validation logic

### Test Standards
- **Unit Tests:** Individual component validation
- **Integration Tests:** Cross-component functionality
- **Performance Tests:** Speed and scalability validation
- **Security Tests:** API key and credential protection

## Related Documentation
- `ARCHITECTURE.md` - Overall system architecture
- `scripts/README.md` - Environment setup scripts
- `docs/LEAN_ARCHITECTURE_GUIDE.md` - LEAN framework integration
- `BackendPython/unicorn/README.md` - Backend architecture overview