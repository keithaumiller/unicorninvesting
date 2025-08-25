# Tests

Test suites for backend analytics, machine learning models, and API endpoints.

## Purpose
- Unit tests for data processing functions
- Integration tests for database operations
- Model validation and performance testing
- API endpoint testing and validation

## Testing Strategy

### Python Migration Tests
- **R to Python Equivalence**: Validate that migrated Python functions produce identical results to R originals
- **Performance Benchmarks**: Ensure Python implementations meet or exceed R performance
- **Data Integrity**: Verify data processing maintains accuracy during migration

### Machine Learning Tests
- **Model Training**: Validate neural network training convergence
- **GA Optimization**: Test genetic algorithm feature selection effectiveness
- **Backtesting**: Automated historical performance validation
- **Performance Metrics**: Unit tests for portfolio performance calculations

### Database Tests
- **Data Migration**: Validate R data correctly migrates to MySQL
- **Query Performance**: Test database query optimization
- **Data Integrity**: Foreign key constraints and data validation
- **Connection Pooling**: Database connection management testing

### API Tests
- **Endpoint Validation**: Test all FastAPI route responses
- **Authentication**: User authentication and authorization testing
- **Error Handling**: API error response validation
- **Load Testing**: Performance under concurrent user loads

## Future Test Files
- `test_portfolio_optimization.py` - GA + NN algorithm testing
- `test_data_migration.py` - R to Python data conversion validation
- `test_market_data.py` - Market data collection and processing
- `test_performance_metrics.py` - Portfolio performance calculations
- `test_api_endpoints.py` - FastAPI route testing
- `test_database_operations.py` - SQLAlchemy ORM testing
- `test_ml_models.py` - Neural network and ML model validation

## Testing Tools
- **pytest**: Python unit testing framework
- **pytest-cov**: Code coverage reporting
- **pytest-mock**: Mocking for isolated unit tests
- **requests**: API endpoint testing
- **pandas.testing**: DataFrame comparison utilities
- **numpy.testing**: Numerical accuracy validation

## Test Data
- Historical market data samples for backtesting
- Synthetic datasets for model training validation
- Mock API responses for integration testing
- Database fixtures for consistent test environments
