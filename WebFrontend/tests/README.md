# WebFrontend Testing Framework

## 🧪 Comprehensive Testing Suite for UnicornMetrics Module

This testing framework provides complete validation for the UnicornMetrics Drupal module, including frontend-backend integration, UI testing, and performance validation.

### ✅ **Testing Framework Features**

- **Unit Testing**: Individual component validation using PHPUnit
- **Functional Testing**: Complete workflow testing with BrowserTestBase
- **Integration Testing**: Frontend-backend API connectivity validation
- **Performance Testing**: Load times, memory usage, and database query optimization
- **JavaScript Testing**: Frontend component testing with Jest
- **End-to-End Testing**: Complete user journey validation with Cypress
- **Accessibility Testing**: WCAG compliance validation with pa11y

### 🎯 **Coverage & Quality Metrics**

- **Target Code Coverage**: >95% for all PHP and JavaScript code
- **Performance Benchmarks**: Page loads <3s, Memory usage <256MB
- **Integration Validation**: Real-time IBKR and backend API connectivity
- **Accessibility Compliance**: WCAG 2.1 AA standards

## 📁 **Directory Structure**

```
/WebFrontend/tests/
├── src/
│   ├── Unit/                    # PHPUnit unit tests
│   │   └── Controller/          # Controller unit tests
│   ├── Functional/              # PHPUnit functional tests
│   ├── Integration/             # Frontend-backend integration tests
│   └── Performance/             # Performance and load tests
├── js/
│   ├── unit/                    # JavaScript unit tests (Jest)
│   ├── integration/             # UI integration tests
│   ├── e2e/                     # End-to-end tests (Cypress)
│   └── accessibility/           # WCAG compliance tests
├── fixtures/                    # Test data and fixtures
├── config/                      # Testing configuration
├── phpunit.xml                  # PHPUnit configuration
├── package.json                 # JavaScript testing dependencies
├── bootstrap.php                # Test environment setup
└── run-tests.sh                 # Comprehensive test runner
```

## 🚀 **Quick Start**

### Prerequisites

1. **System Health Check** (Required):
   ```bash
   ./scripts/unicorn_environment.sh --check-only
   ```
   Must achieve >85% system validation before running tests.

2. **Dependencies**:
   - PHP 8.2+ with PHPUnit
   - Node.js 16+ (for JavaScript tests)
   - Drupal 11 installation
   - MySQL database access

### Running All Tests

```bash
cd /WebFrontend/tests
./run-tests.sh
```

### Running Specific Test Suites

```bash
# Unit tests only
./run-tests.sh --unit

# Functional tests only  
./run-tests.sh --functional

# Integration tests only
./run-tests.sh --integration

# Performance tests only
./run-tests.sh --performance

# JavaScript/UI tests only
./run-tests.sh --ui

# End-to-end tests only
./run-tests.sh --e2e

# Accessibility tests only
./run-tests.sh --accessibility
```

## 📊 **Test Categories**

### 1. Unit Tests (`src/Unit/`)

Tests individual module components in isolation:

- **DashboardController**: Portfolio management and data formatting
- **Form Components**: Configuration and settings forms
- **Data Processing**: Portfolio calculations and metrics
- **Utility Functions**: Helper methods and data transformation

**Example**:
```bash
./vendor/bin/phpunit --testsuite=unit
```

### 2. Functional Tests (`src/Functional/`)

Tests complete user workflows and module integration:

- **User Journeys**: Complete navigation through all portfolio pages
- **Permission Testing**: Access control and authorization
- **Form Submission**: Configuration and settings workflows
- **Responsive Design**: Cross-device compatibility

**Example**:
```bash
./vendor/bin/phpunit --testsuite=functional
```

### 3. Integration Tests (`src/Integration/`)

Tests frontend-backend API connectivity and data flow:

- **Backend API**: Portfolio data, performance metrics, algorithm status
- **IBKR Gateway**: Real-time data collection and authentication
- **Data Synchronization**: Frontend-backend consistency validation
- **Error Handling**: API failure scenarios and recovery

**Example**:
```bash
./vendor/bin/phpunit --testsuite=integration
```

### 4. Performance Tests (`src/Performance/`)

Tests system performance under various conditions:

- **Page Load Times**: Dashboard, portfolio, holdings, performance pages
- **Memory Usage**: Peak usage and per-request consumption
- **Database Queries**: Query count and execution time optimization
- **Concurrent Users**: Multi-user load testing
- **Stress Testing**: System stability under high load

**Example**:
```bash
./vendor/bin/phpunit --testsuite=performance
```

### 5. JavaScript Tests (`js/unit/`)

Tests frontend JavaScript functionality:

- **Portfolio Selection**: Dropdown handling and validation
- **Real-time Updates**: Data refresh and status indicators
- **Data Formatting**: Currency, percentage, and number formatting
- **Interactive Elements**: Navigation, hover effects, keyboard support
- **Error Handling**: API failures and fallback behavior

**Example**:
```bash
cd tests && npm test
```

### 6. End-to-End Tests (`js/e2e/`)

Tests complete user journeys in real browsers:

- **Complete Workflows**: Portfolio analysis from start to finish
- **Cross-browser Testing**: Chrome, Firefox, Safari compatibility
- **Responsive Testing**: Mobile, tablet, desktop viewports
- **User Interactions**: Clicks, navigation, form submission
- **Error Scenarios**: Invalid inputs and recovery

**Example**:
```bash
cd tests && npm run test:integration
```

### 7. Accessibility Tests (`js/accessibility/`)

Tests WCAG 2.1 AA compliance:

- **Keyboard Navigation**: Tab order and focus management
- **Screen Reader Support**: ARIA labels and announcements
- **Color Contrast**: Text readability and visual accessibility
- **Semantic HTML**: Proper heading structure and landmarks

**Example**:
```bash
cd tests && npm run test:accessibility
```

## 🔧 **Configuration**

### PHPUnit Configuration (`phpunit.xml`)

Key configuration settings:

```xml
<env name="SIMPLETEST_BASE_URL" value="http://localhost"/>
<env name="SIMPLETEST_DB" value="mysql://root:@localhost/unicorn_test"/>
<env name="BACKEND_API_BASE_URL" value="http://localhost:8000"/>
<env name="IBKR_GATEWAY_URL" value="http://localhost:5000"/>
```

### JavaScript Testing (`package.json`)

Test scripts and dependencies:

```json
{
  "scripts": {
    "test": "jest",
    "test:coverage": "jest --coverage",
    "test:integration": "cypress run",
    "test:accessibility": "pa11y-ci"
  }
}
```

## 📈 **Test Data & Fixtures**

### Portfolio Test Data (`fixtures/PortfolioFixtures.php`)

Standardized test data for consistent testing:

- **Portfolio Configurations**: Forex, equity, and test portfolios
- **Holdings Data**: Securities, prices, and performance metrics
- **Performance Metrics**: Returns, risk measures, and benchmarks
- **Algorithm Data**: Signal accuracy, performance, and backtest results
- **Error Scenarios**: API failures, timeouts, and edge cases

### Mock Data Usage

```php
use Drupal\Tests\unicornmetrics\Fixtures\PortfolioFixtures;

$portfolio_data = PortfolioFixtures::getPortfolioData();
$holdings = PortfolioFixtures::getHoldingsData();
$performance = PortfolioFixtures::getPerformanceData();
```

## 🔍 **Coverage Analysis**

### PHP Coverage

- **Target**: >95% line coverage for custom module code
- **Reports**: HTML and XML coverage reports generated
- **Exclusions**: Third-party code, templates, and test files

### JavaScript Coverage

- **Target**: >80% coverage for frontend JavaScript
- **Tools**: Jest coverage reporting with Istanbul
- **Reports**: LCOV and HTML coverage reports

### Coverage Commands

```bash
# PHP coverage (included in unit tests)
./vendor/bin/phpunit --coverage-html=coverage-html

# JavaScript coverage
npm run test:coverage
```

## ⚡ **Performance Benchmarks**

### Page Load Time Targets

- **Dashboard**: <2.0 seconds
- **Portfolio Overview**: <3.0 seconds  
- **Holdings Table**: <4.0 seconds
- **Performance Metrics**: <2.5 seconds
- **Algorithm Dashboard**: <2.0 seconds

### Memory Usage Targets

- **Peak Memory**: <256 MB per request
- **Memory Increase**: <10 MB per page load
- **Database Queries**: <50 queries per page
- **Slow Query Threshold**: <100ms

### Performance Validation

Performance tests automatically validate these benchmarks and fail if targets are not met.

## 🔗 **Integration with System Health**

### Pre-Test Validation

Before running tests, the framework validates:

- **System Health**: Using existing `unicorn_environment.sh`
- **Backend Connectivity**: API endpoints and IBKR Gateway
- **Database Access**: MySQL connection and permissions
- **Drupal Installation**: Module installation and configuration

### Health Check Integration

```bash
# Validate system before testing
./scripts/unicorn_environment.sh --check-only

# Run tests with health validation
./tests/run-tests.sh
```

## 📊 **Test Reporting**

### Comprehensive Reports

The test runner generates detailed reports including:

- **Test Summary**: Pass/fail counts and execution time
- **Coverage Analysis**: PHP and JavaScript coverage metrics
- **Performance Metrics**: Load times and resource usage
- **Integration Status**: Backend and IBKR Gateway connectivity
- **Error Analysis**: Failed test details and recommendations

### Report Formats

- **Console Output**: Real-time test execution feedback
- **Markdown Reports**: Detailed analysis with timestamps
- **HTML Coverage**: Interactive coverage exploration
- **XML Results**: CI/CD integration format

## 🚨 **Error Handling & Debugging**

### Common Issues

1. **Backend API Unavailable**:
   ```bash
   # Check backend status
   curl http://localhost:8000/health
   ```

2. **IBKR Gateway Not Running**:
   ```bash
   # Start IBKR Gateway
   cd BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/tools
   ./bin/run.sh root/conf-codespace.yaml
   ```

3. **Database Connection Issues**:
   ```bash
   # Verify MySQL service
   sudo systemctl status mysql
   ```

4. **Permission Errors**:
   ```bash
   # Fix Drupal permissions
   sudo chown -R www-data:www-data /path/to/WebFrontend
   ```

### Debug Mode

Enable verbose output for debugging:

```bash
# Verbose PHPUnit output
./vendor/bin/phpunit --verbose --debug

# Detailed test runner output
DEBUG=1 ./run-tests.sh
```

## 🔄 **Continuous Integration**

### CI/CD Integration

The testing framework integrates with CI/CD pipelines:

- **Pre-commit Hooks**: Unit tests before code commits
- **Pull Request Validation**: Full test suite on PR creation
- **Nightly Builds**: Performance and integration testing
- **Release Validation**: Complete test suite before deployment

### Environment Variables

```bash
# Test execution control
export RUN_UNIT_TESTS=true
export RUN_INTEGRATION_TESTS=true
export RUN_PERFORMANCE_TESTS=false

# Performance thresholds
export COVERAGE_THRESHOLD=95
export PERFORMANCE_THRESHOLD=85
```

## 📚 **Best Practices**

### Writing Tests

1. **Use Test Fixtures**: Leverage standardized test data
2. **Mock External Services**: Use mocks for API and database calls
3. **Test Edge Cases**: Include error scenarios and boundary conditions
4. **Maintain Coverage**: Aim for >95% code coverage
5. **Performance Awareness**: Consider memory and execution time

### Test Organization

1. **Clear Naming**: Descriptive test method names
2. **Logical Grouping**: Related tests in same class/file
3. **Documentation**: Comments explaining complex test scenarios
4. **Consistent Structure**: Follow established patterns

### Debugging Tests

1. **Isolated Testing**: Run individual test methods
2. **Debug Output**: Use verbose flags and debug statements
3. **Environment Validation**: Check system state before testing
4. **Incremental Development**: Test frequently during development

## 🎯 **Success Metrics**

### Quality Gates

- ✅ **Unit Tests**: 100% pass rate, >95% coverage
- ✅ **Functional Tests**: All user journeys working
- ✅ **Integration Tests**: Backend connectivity validated
- ✅ **Performance Tests**: All benchmarks met
- ✅ **Accessibility Tests**: WCAG 2.1 AA compliance

### Performance Targets

- ✅ **Page Load Times**: All pages under benchmark targets
- ✅ **Memory Usage**: Under 256MB peak usage
- ✅ **Database Performance**: Under 50 queries per page
- ✅ **API Response Times**: Under 3 seconds for data calls

## 🔧 **Maintenance**

### Regular Updates

1. **Dependency Updates**: Keep testing frameworks current
2. **Benchmark Review**: Adjust performance targets as needed
3. **Test Data Refresh**: Update fixtures with realistic data
4. **Coverage Analysis**: Monitor and improve coverage metrics

### Adding New Tests

1. **Follow Patterns**: Use existing test structure
2. **Update Fixtures**: Add new test data as needed
3. **Document Changes**: Update this README with new features
4. **Validate Integration**: Ensure new tests work with CI/CD

---

## 🏆 **Testing Excellence**

This comprehensive testing framework ensures the UnicornMetrics module meets the highest standards for:

- **Reliability**: Comprehensive validation of all functionality
- **Performance**: Optimized for real-time trading requirements  
- **Integration**: Seamless frontend-backend data flow
- **Accessibility**: Inclusive design for all users
- **Maintainability**: Sustainable code quality practices

**Success Rate Target**: >95% test coverage with <5% failure rate

---

**Last Updated**: January 2025  
**Framework Version**: 1.0.0  
**Drupal Compatibility**: 11.x  
**PHP Requirement**: 8.2+