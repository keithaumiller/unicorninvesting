# Backend Utilities

This directory contains utility functions, helpers, and common functionality shared across the backend codebase.

## Utilities Structure

```
utils/
├── config.py                   # ⏳ Configuration management
├── logger.py                   # ⏳ Logging configuration
├── helpers.py                  # ⏳ General helper functions
├── validators.py               # ⏳ Data validation utilities
├── exceptions.py               # ⏳ Custom exception classes
├── constants.py                # ⏳ Application constants
├── decorators.py               # ⏳ Custom decorators
├── financial_utils.py          # ⏳ Financial calculation utilities
├── data_utils.py               # ⏳ Data processing utilities
└── auth_utils.py               # ⏳ Authentication utilities
```

## Planned Utilities

### config.py
Configuration management and environment handling:
- Environment variable loading
- Configuration validation
- Database connection strings
- API keys and secrets management
- Application settings

### logger.py
Centralized logging configuration:
- Structured logging setup
- Log level management
- File rotation and archiving
- Performance logging
- Error tracking integration

### helpers.py
General-purpose helper functions:
- Date and time utilities
- String manipulation functions
- File I/O helpers
- Data formatting utilities
- Common validation functions

### validators.py
Data validation utilities:
- Input data validation
- Portfolio allocation validation
- Symbol format validation
- Financial metric validation
- Configuration validation

### exceptions.py
Custom exception classes for the application:
- `PortfolioError`: Portfolio-related exceptions
- `DataValidationError`: Data validation failures
- `MarketDataError`: Market data collection errors
- `MLError`: Machine learning operation errors
- `AuthenticationError`: Authentication failures

### constants.py
Application-wide constants:
- Market trading hours
- Financial calculation constants
- API endpoint URLs
- Default configuration values
- Error codes and messages

### decorators.py
Custom decorators for common functionality:
- Performance monitoring decorators
- Retry logic decorators
- Authentication required decorators
- Rate limiting decorators
- Caching decorators

### financial_utils.py
Financial calculation utilities:
- Portfolio performance metrics
- Risk calculations (VaR, Sharpe ratio)
- Return calculations
- Volatility analysis
- Correlation calculations

### data_utils.py
Data processing utilities:
- Data cleaning functions
- Time series processing
- Statistical analysis helpers
- Data transformation utilities
- Missing data handling

### auth_utils.py
Authentication and authorization utilities:
- JWT token handling
- Password hashing and validation
- Session management
- Permission checking
- API key validation

## Design Principles

### Reusability
- **Single Responsibility**: Each utility has one clear purpose
- **Pure Functions**: Stateless functions where possible
- **Type Safety**: Full type hints for all functions
- **Documentation**: Comprehensive docstrings

### Performance
- **Efficient Algorithms**: Optimized implementations
- **Caching**: Memoization for expensive operations
- **Lazy Loading**: Delayed computation when appropriate
- **Memory Management**: Efficient memory usage

### Error Handling
- **Graceful Degradation**: Fallback mechanisms
- **Informative Errors**: Clear error messages
- **Logging Integration**: Proper error logging
- **Exception Hierarchy**: Structured exception handling

## Integration Points

### Service Layer Integration
Utilities are used throughout the service layer:
```python
from backend.utils.financial_utils import calculate_sharpe_ratio
from backend.utils.validators import validate_portfolio_allocations
from backend.utils.logger import get_logger

logger = get_logger(__name__)

def analyze_portfolio(allocations: Dict[str, float], returns: pd.DataFrame):
    # Validate input data
    validate_portfolio_allocations(allocations)
    
    # Calculate metrics
    sharpe_ratio = calculate_sharpe_ratio(returns, allocations)
    
    logger.info(f"Portfolio Sharpe ratio: {sharpe_ratio}")
    return sharpe_ratio
```

### API Layer Integration
- **Request Validation**: Input data validation
- **Response Formatting**: Consistent response structure
- **Error Handling**: Standardized error responses
- **Authentication**: JWT token validation

### ML Integration
- **Data Preprocessing**: Feature engineering utilities
- **Model Validation**: Model performance validation
- **Result Processing**: Prediction post-processing
- **Configuration**: ML model configuration management

## Common Patterns

### Configuration Management
```python
from backend.utils.config import get_config

config = get_config()
database_url = config.database.url
api_key = config.market_data.api_key
```

### Logging
```python
from backend.utils.logger import get_logger

logger = get_logger(__name__)
logger.info("Processing portfolio optimization")
logger.error("Failed to connect to market data provider", exc_info=True)
```

### Validation
```python
from backend.utils.validators import validate_symbol, validate_allocation

def process_portfolio_update(symbol: str, allocation: float):
    validate_symbol(symbol)
    validate_allocation(allocation)
    # Process update...
```

### Exception Handling
```python
from backend.utils.exceptions import PortfolioError, DataValidationError

try:
    result = optimize_portfolio(portfolio_id)
except DataValidationError as e:
    logger.error(f"Invalid portfolio data: {e}")
    raise
except PortfolioError as e:
    logger.error(f"Portfolio optimization failed: {e}")
    raise
```

## Testing Strategy

### Unit Testing
- **Pure Function Testing**: Test individual utility functions
- **Mock Dependencies**: Mock external dependencies
- **Edge Case Testing**: Test boundary conditions
- **Performance Testing**: Validate performance requirements

### Integration Testing
- **Service Integration**: Test utility usage in services
- **Configuration Testing**: Validate configuration loading
- **Error Scenario Testing**: Test error handling paths
- **Logging Testing**: Validate logging output

## Migration Progress

- ⏳ **Planned**: All utilities are planned but not yet implemented
- 🎯 **Priority**: config.py, logger.py, and exceptions.py are highest priority
- 📋 **Dependencies**: Foundation for all other backend components
- 🔗 **Integration**: Will be used throughout the entire backend system
