# Interactive Brokers Testing Suite
# Unicorn Investing Platform - IBKR Data Collection Tests

This directory contains comprehensive testing for Interactive Brokers (IBKR) data collection and processing.

## Test Structure

### Core Test Files

#### `test_ibkr_integration.py`
**Purpose**: Integration testing for IBKR Gateway connectivity and data collection
**Coverage**:
- Gateway authentication and connection validation
- ETH contract data retrieval 
- Real-time data collection performance
- Error handling and timeout scenarios
- Data format validation

**Key Test Classes**:
- `TestIBKRConnectivity`: Gateway connection and authentication
- `TestETHDataCollection`: ETH-specific data collection
- `TestPerformance`: Performance benchmarks and stress testing

#### `test_technical_indicators.py`
**Purpose**: Validation of technical indicator calculations
**Coverage**:
- Trend indicators (SMA, EMA, MACD, etc.)
- Momentum indicators (RSI, Stochastic, Williams %R)
- Volatility indicators (ATR, Bollinger Bands)
- Volume indicators (VWAP, OBV, etc.)
- Edge cases and error handling

**Key Test Classes**:
- `TestTrendIndicators`: Moving averages and trend analysis
- `TestMomentumIndicators`: Oscillators and momentum metrics
- `TestVolatilityIndicators`: Volatility and range measurements
- `TestVolumeIndicators`: Volume-based analysis

#### `test_data_quality.py`
**Purpose**: Data integrity and quality assurance
**Coverage**:
- OHLC relationship validation
- Price reasonableness checks
- Data completeness analysis
- Gap detection and handling
- Quality scoring algorithms

**Key Test Classes**:
- `TestDataIntegrity`: OHLC validation and price checks
- `TestDataCompleteness`: Gap analysis and freshness
- `TestDataQualityScoring`: Comprehensive quality metrics
- `TestRealDataValidation`: Live data quality assessment

#### `test_e2e_pipeline.py`
**Purpose**: End-to-end pipeline testing and performance analysis
**Coverage**:
- Complete data flow from IBKR → Indicators → Signals
- Error recovery and fault tolerance
- Stress testing under high load
- Latency benchmarks for real-time trading

**Key Test Classes**:
- `TestE2EDataPipeline`: Full pipeline integration
- `TestE2EPerformanceBenchmarks`: Performance and latency testing

### Running Tests

#### Prerequisites
```bash
# Ensure IBKR Gateway is running
# Default: http://localhost:5000
# Authentication required for integration tests

# Install test dependencies
cd /workspaces/unicorninvesting
pip install -r tests/requirements-test.txt
```

#### Basic Test Execution
```bash
# Run all IBKR tests
cd /workspaces/unicorninvesting/tests
pytest unicorn/1_data_sources/1_raw/connectors/interactive_brokers/ -v

# Run specific test categories
pytest -m unit                    # Fast unit tests only
pytest -m integration            # Integration tests (requires IBKR)
pytest -m performance           # Performance benchmarks

# Run specific test files
pytest unicorn/1_data_sources/1_raw/connectors/interactive_brokers/test_ibkr_integration.py -v
pytest unicorn/1_data_sources/1_raw/connectors/interactive_brokers/test_technical_indicators.py -v
```

#### Advanced Test Options
```bash
# Run with detailed output
pytest --verbose --tb=short

# Run only fast tests (skip slow/integration)
pytest -m "not slow and not integration"

# Run performance benchmarks
pytest -m performance --benchmark-only

# Run with coverage report
pytest --cov=unicorn/1_data_sources/1_raw/connectors/interactive_brokers
```

### Test Configuration

#### Markers
- `unit`: Fast unit tests with no external dependencies
- `integration`: Tests requiring live IBKR Gateway connection
- `performance`: Performance benchmarks and stress tests
- `slow`: Long-running tests (>30 seconds)

#### Fixtures
- `sample_eth_data`: Synthetic ETH data for testing
- `ibkr_gateway_available`: Check if IBKR Gateway is accessible
- `real_collector`: Live IBKR collector (requires authentication)

### Performance Benchmarks

#### Data Collection Benchmarks
- **Target**: <5 seconds for data collection
- **Throughput**: >20 data points per second
- **Memory**: <500MB usage during collection

#### Technical Indicators Benchmarks
- **Individual Indicators**: <50ms calculation time
- **Average Calculation**: <20ms per indicator
- **Batch Processing**: >100 points per second

#### End-to-End Pipeline Benchmarks
- **Total Latency**: <2000ms average, <5000ms P95
- **Data Collection**: <1000ms typical
- **Indicator Calculation**: <100ms typical

### Test Data and Validation

#### ETH Contract Details
- **Contract ID**: 541686654
- **Exchange**: ZEROHASH (professional data feed)
- **Data Type**: 1-minute OHLCV bars
- **Expected Range**: $1000-$8000 (reasonable ETH prices)

#### Quality Metrics
- **Completeness**: >95% data points present (minimal gaps)
- **Consistency**: 100% valid OHLC relationships
- **Freshness**: <10 minutes for live trading
- **Reasonableness**: >90% prices within expected range

### Error Scenarios Tested

#### Network and Connectivity
- Gateway offline/unavailable
- Network timeouts and interruptions
- Authentication failures
- SSL/TLS certificate issues

#### Data Quality Issues
- Invalid OHLC relationships (high < low, etc.)
- Negative volumes
- Unreasonable price movements
- Missing or duplicate timestamps

#### Performance Edge Cases
- High-frequency data processing
- Memory constraints under load
- Concurrent operations
- Buffer overflow protection

### Debugging and Troubleshooting

#### Common Test Failures

1. **IBKR Gateway Not Available**
   ```bash
   # Check Gateway status
   curl http://localhost:5000/v1/api/portal/sso/validate
   
   # Start Gateway if needed
   # (Follow IBKR installation instructions)
   ```

2. **Authentication Failures**
   ```bash
   # Verify Gateway is authenticated
   # Check IBKR Trader Workstation is logged in
   ```

3. **Import Errors**
   ```bash
   # Verify source modules are accessible
   python -c "from optimized_eth_collector import OptimizedETHCollector"
   ```

4. **Performance Failures**
   ```bash
   # Check system resources
   top
   free -h
   
   # Verify IBKR Gateway response time
   time curl http://localhost:5000/v1/api/portal/sso/validate
   ```

#### Test Output Analysis
```bash
# Verbose test output shows:
# - Data collection timing
# - Indicator calculation performance  
# - Quality score breakdowns
# - Signal generation statistics
# - Memory usage patterns

# Example output:
# ✅ Collected 1000+ data points in 2.3s
# 📊 Quality Scores: Overall 94%, Consistency 100%, Freshness 87%
# ⚡ Latency: 450ms average, 850ms P95
```

### Integration with CI/CD

#### Automated Testing Strategy
- **Unit Tests**: Run on every commit (fast feedback)
- **Integration Tests**: Run on pull requests (requires IBKR setup)
- **Performance Tests**: Run nightly (trend monitoring)
- **Stress Tests**: Run weekly (capacity planning)

#### Test Environment Requirements
- IBKR Gateway running with authentication
- Stable network connection for data collection
- Sufficient memory (>1GB available)
- Python 3.9+ with required dependencies

### Continuous Improvement

#### Metrics Tracking
- Test execution time trends
- Performance benchmark results
- Data quality score evolution
- Error rate monitoring

#### Adding New Tests
1. Follow existing test structure and naming
2. Include appropriate markers (`@pytest.mark.unit`, etc.)
3. Add comprehensive docstrings
4. Update this README with new test descriptions
5. Ensure tests are deterministic and fast

### Related Documentation
- [IBKR Integration Setup](../../../../../../../docs/IBKR_INTEGRATION_SETUP.md)
- [IBKR Quick Start Guide](../../../../../../../docs/IBKR_QUICK_START.md)
- [ETH Portfolio Framework](../../../../../../README_ETH_PORTFOLIO.md)
- [Technical Analysis Documentation](../../../../../../../docs/technical_analysis.md)
