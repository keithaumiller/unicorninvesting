# Backend API

This directory contains FastAPI route definitions and API infrastructure for the Unicorn Investing platform.

## API Structure

```
api/
├── main.py                     # ⏳ FastAPI application factory
├── routes/                     # ⏳ API route modules
│   ├── auth.py                # ⏳ Authentication endpoints
│   ├── portfolios.py          # ⏳ Portfolio management API
│   ├── market_data.py         # ⏳ Market data endpoints
│   ├── ml.py                  # ⏳ Machine learning API
│   ├── users.py               # ⏳ User management
│   └── analytics.py           # ⏳ Analytics and reporting
├── middleware/                 # ⏳ Custom middleware
│   ├── auth_middleware.py     # ⏳ Authentication middleware
│   ├── cors_middleware.py     # ⏳ CORS configuration
│   └── logging_middleware.py  # ⏳ Request logging
├── schemas/                    # ⏳ Pydantic models
│   ├── user_schemas.py        # ⏳ User data models
│   ├── portfolio_schemas.py   # ⏳ Portfolio data models
│   └── ml_schemas.py          # ⏳ ML request/response models
└── dependencies.py            # ⏳ Dependency injection
```

## Planned API Endpoints

### Authentication API (`routes/auth.py`)
```
POST /auth/login          # User authentication
POST /auth/logout         # Session termination
POST /auth/register       # User registration
GET  /auth/me            # Current user info
POST /auth/refresh       # Token refresh
```

### Portfolio Management API (`routes/portfolios.py`)
```
GET    /portfolios                    # List user portfolios
POST   /portfolios                    # Create new portfolio
GET    /portfolios/{id}               # Get portfolio details
PUT    /portfolios/{id}               # Update portfolio
DELETE /portfolios/{id}               # Delete portfolio
GET    /portfolios/{id}/performance   # Portfolio performance metrics
POST   /portfolios/{id}/rebalance     # Trigger rebalancing
GET    /portfolios/{id}/allocations   # Current allocations
POST   /portfolios/{id}/allocations   # Update allocations
```

### Market Data API (`routes/market_data.py`)
```
GET  /market-data/symbols/{symbol}           # Get symbol data
POST /market-data/download                   # Bulk data download
GET  /market-data/features                   # Available features
GET  /market-data/forex                      # Forex data
GET  /market-data/stocks                     # Stock data
GET  /market-data/etfs                       # ETF data
```

### Machine Learning API (`routes/ml.py`)
```
POST /ml/train                    # Train new model
GET  /ml/models                   # List available models
GET  /ml/models/{id}              # Get model details
POST /ml/predict                  # Make predictions
GET  /ml/features                 # Feature importance
POST /ml/optimize                 # Portfolio optimization
GET  /ml/backtests                # Backtesting results
```

### User Management API (`routes/users.py`)
```
GET    /users/profile             # User profile
PUT    /users/profile             # Update profile
GET    /users/preferences         # User preferences
PUT    /users/preferences         # Update preferences
GET    /users/activity            # User activity log
```

### Analytics API (`routes/analytics.py`)
```
GET  /analytics/performance       # Performance analytics
GET  /analytics/risk              # Risk metrics
GET  /analytics/correlation       # Correlation analysis
GET  /analytics/attribution       # Performance attribution
GET  /analytics/reports           # Generate reports
```

## API Design Principles

### RESTful Architecture
- **Resource-based URLs**: Clear resource identification
- **HTTP Methods**: Proper use of GET, POST, PUT, DELETE
- **Status Codes**: Appropriate HTTP status code usage
- **Stateless**: No server-side session storage

### Data Validation
- **Pydantic Models**: Strong typing for request/response data
- **Input Validation**: Comprehensive input validation
- **Error Handling**: Consistent error response format
- **Documentation**: Auto-generated API documentation

### Security
- **JWT Authentication**: Token-based authentication
- **Rate Limiting**: API usage rate limiting
- **Input Sanitization**: Protection against injection attacks
- **CORS Configuration**: Proper cross-origin resource sharing

### Performance
- **Async/Await**: Asynchronous request handling
- **Connection Pooling**: Database connection optimization
- **Caching**: Response caching for frequently accessed data
- **Pagination**: Large dataset pagination

## Integration Architecture

### Service Layer Integration
The API layer coordinates with backend services:

```python
# Example API endpoint structure
@router.post("/portfolios/{portfolio_id}/optimize")
async def optimize_portfolio(
    portfolio_id: int,
    optimization_params: OptimizationRequest,
    current_user: User = Depends(get_current_user),
    portfolio_service: PortfolioService = Depends(get_portfolio_service),
    ml_service: MLService = Depends(get_ml_service)
):
    # Validate user access to portfolio
    portfolio = await portfolio_service.get_portfolio(portfolio_id, current_user.id)
    
    # Trigger ML optimization
    optimization_result = await ml_service.optimize_portfolio(
        portfolio_id=portfolio_id,
        params=optimization_params
    )
    
    return OptimizationResponse(**optimization_result)
```

### Database Integration
- **SQLAlchemy ORM**: Database operations through models
- **Session Management**: Proper database session handling
- **Transaction Management**: ACID transaction support
- **Connection Pooling**: Efficient database connections

### Frontend Integration
- **OpenAPI/Swagger**: Auto-generated API documentation
- **JSON Responses**: Structured JSON response format
- **Error Handling**: Consistent error response structure
- **CORS Support**: Cross-origin request handling for web frontend

## API Documentation

### Auto-Generated Documentation
FastAPI provides automatic API documentation:
- **Swagger UI**: Interactive API documentation at `/docs`
- **ReDoc**: Alternative documentation at `/redoc`
- **OpenAPI Schema**: Machine-readable API specification at `/openapi.json`

### Authentication Flow
```
1. POST /auth/login with credentials
2. Receive JWT access token
3. Include token in Authorization header: "Bearer <token>"
4. API validates token on each request
5. Refresh token before expiration
```

### Error Response Format
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid input data",
    "details": {
      "field": "portfolio_id",
      "issue": "Must be a positive integer"
    }
  }
}
```

## Development Guidelines

### Code Standards
- **Type Hints**: Full type annotation for all functions
- **Async/Await**: Asynchronous operations for I/O
- **Error Handling**: Comprehensive exception handling
- **Logging**: Detailed request/response logging

### Testing Strategy
- **Unit Tests**: Test individual endpoint logic
- **Integration Tests**: Test service layer integration
- **Load Tests**: Performance testing under load
- **Security Tests**: Authentication and authorization testing

## Migration Progress

- ⏳ **Planned**: All API endpoints are planned but not yet implemented
- 🎯 **Priority**: Authentication and portfolio management endpoints
- 📋 **Dependencies**: Requires completion of service layer components
- 🔗 **Integration**: Will integrate with existing models and services
