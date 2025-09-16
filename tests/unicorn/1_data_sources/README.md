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
| **FRED** | Economic Data | Federal Reserve economic indicators | ✅ Active |
| **Interactive Brokers** | Trading Platform | Real-time market data & execution | 🔧 Testing |
| **Forex** | Currency Data | Foreign exchange rates | 🔧 Testing |

## Testing Framework

### Main Test Scripts

#### **Primary Test Script:** `test_data_warehouse.sh`
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

#### **Pipeline Validation Script:** `pipeline_validation.py`
**Purpose:** End-to-end pipeline testing with data lineage tracing

**Features:**
- **Sequential Dependency Testing:** Validates that each layer depends on previous layer success
- **Data Lineage Tracing:** Follows specific data samples from raw → bronze → silver
- **Performance Monitoring:** Measures pipeline throughput and timing
- **Cross-Connector Validation:** Ensures data consistency across all connectors

**Usage:**
```bash
# Run comprehensive pipeline validation
python3 pipeline_validation.py

# Test specific connector pipeline
from pipeline_validation import PipelineValidator
validator = PipelineValidator()
"
```

#### **🆕 Economic Indicators Test Runner:** `run_economic_indicators_tests.py`
**Purpose:** Comprehensive validation of economic indicators bronze and silver layer processing

**Features:**
- **Bronze Layer Validation:** Tests all 4 economic categories processing
- **Silver Layer Validation:** Tests aggregation and feature selection
- **End-to-End Pipeline:** Validates complete bronze-to-silver workflow
- **Performance Benchmarking:** Measures processing speed and data quality

**Usage:**
```bash
# Run all economic indicators tests
python3 run_economic_indicators_tests.py

# Run only bronze layer tests
python3 run_economic_indicators_tests.py --bronze-only

# Run only silver layer tests
python3 run_economic_indicators_tests.py --silver-only

# Run with detailed output
python3 run_economic_indicators_tests.py --verbose
```

### Test Categories

# Validate connector with performance metrics
python3 -c "
from pipeline_validation import PipelineValidator
validator = PipelineValidator()
result = validator.validate_raw_connector('yahoo_finance', 'ETH-USD')
print(f'Status: {result[\"status\"]}, Samples: {len(result[\"data_samples\"])}')
"
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
- **ETL Pipeline Validation:** Bronze layer processing workflows
- **🆕 Economic Indicators Bronze Processing:** Comprehensive validation of 4 economic categories
  - Economic Growth, Consumer Business, International Trade, Monetary Policy
  - Feature engineering validation (lag, diff, momentum, volatility)
  - Data quality metrics and temporal coverage analysis
  - 100% processing success rate validation

#### 🥈 Silver Layer Tests (Layer 3)
- **Directory Structure:** Validation of silver layer organization
- **Data Connector:** Silver layer integration with portfolio management
- **Data Enrichment:** Feature engineering and data enhancement
- **Performance Metrics:** ETH model performance tracking
- **Data Freshness:** Automated refresh system validation
- **🆕 Economic Indicators Silver Processing:** Complete aggregation and normalization testing
  - Temporal alignment across economic categories validation
  - Feature selection quality assessment (50 curated features)
  - Data quality metrics (32,873 daily + 788,929 hourly observations)
  - End-to-end bronze-to-silver pipeline validation

#### 🥇 Gold Layer Tests (Layer 4)
- **Directory Structure:** Analytics-ready data organization
- **Portfolio Analytics:** Investment performance calculations
- **Risk Metrics:** Advanced risk assessment capabilities

#### 🔄 End-to-End Pipeline Tests
- **Data Lineage Tracing:** Follow data samples through all layers
- **Sequential Validation:** Ensure layer dependencies are satisfied
- **Performance Benchmarking:** Pipeline throughput and timing analysis
- **Cross-Connector Consistency:** Data validation across all sources
- **Automated Refresh Integration:** 5-minute refresh cycle validation

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

### 2. Sequential Pipeline Testing
The testing framework follows a strict dependency order:

#### **Stage 1: Raw Layer Validation**
```bash
# Test connectors in parallel (independent)
./test_data_warehouse.sh --layer=raw --connector=yahoo
./test_data_warehouse.sh --layer=raw --connector=fred
./test_data_warehouse.sh --layer=raw --connector=ibkr
./test_data_warehouse.sh --layer=raw --connector=forex
```

**Requirements for Success:**
- At least one connector must successfully retrieve data
- Data must conform to expected schema
- API connectivity must be validated

#### **Stage 2: Bronze Layer Validation** 
```bash
# Depends on Stage 1 success
./test_data_warehouse.sh --layer=bronze
```

**Dependencies:**
- Raw layer data available
- ETL scripts operational
- Data cleansing rules applied

#### **Stage 3: Silver Layer Validation**
```bash
# Depends on Stage 2 success  
./test_data_warehouse.sh --layer=silver
```

**Dependencies:**
- Bronze layer processing complete
- Data enrichment pipeline operational
- Feature engineering applied

#### **Stage 4: Gold Layer Validation**
```bash
# Depends on Stage 3 success
./test_data_warehouse.sh --layer=gold
```

**Dependencies:**
- Silver layer data available
- Analytics processing complete
- Portfolio integration ready

### 3. Data Lineage Tracing

#### **End-to-End Pipeline Test**
```bash
# Trace specific symbols through entire pipeline
python3 pipeline_validation.py
```

**Process:**
1. **Raw Data Retrieval:** Fetch live data from Yahoo Finance for ETH-USD
2. **Bronze Processing:** Validate data cleansing and transformation
3. **Silver Enrichment:** Confirm feature engineering and enhancement
4. **Lineage Verification:** Ensure same data sample exists at each layer
5. **Performance Validation:** Measure pipeline timing and throughput

#### **Symbol-Specific Tracing**
```python
# Trace ETH data through pipeline
validator = PipelineValidator()
lineage = validator.trace_data_lineage('yahoo_finance', 'ETH-USD')

# Validate each stage
assert lineage['stages']['raw']['status'] == 'PASSED'
assert lineage['stages']['bronze']['status'] == 'PASSED'  
assert lineage['stages']['silver']['status'] == 'PASSED'
```

### 4. Performance Monitoring

#### **Pipeline Metrics**
- **Data Freshness:** Silver layer files updated within 10 minutes
- **Processing Speed:** Raw → Silver completion under 5 minutes
- **Throughput:** Multiple symbols processed concurrently
- **Reliability:** 95%+ success rate across all connectors

#### **Automated Refresh Integration**
```bash
# Validate 5-minute refresh cycle
validator.validate_pipeline_performance()

# Expected metrics:
# - silver_data_age_minutes < 10
# - crypto_assets > 0
# - forex_assets > 0
# - automated_refresh_active = true
```

### 3. Results Aggregation
- Collect test results from each layer
- Generate comprehensive summary
- Export results to JSON format
- Create symbolic links for latest results

## Directory Structure

```
tests/unicorn/1_data_sources/
├── test_data_warehouse.sh              # 🆕 Comprehensive testing script
├── pipeline_validation.py              # 🆕 End-to-end pipeline validation
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