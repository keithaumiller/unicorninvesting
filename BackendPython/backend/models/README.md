# Database Models

This directory contains SQLAlchemy ORM models for the Unicorn Investing database schema.

## Models Structure

```
models/
├── database_models.py           # ✅ Complete database schema
├── user_models.py              # ⏳ Extended user functionality
├── portfolio_models.py         # ⏳ Advanced portfolio operations
├── market_data_models.py       # ⏳ Market data caching models
└── ml_models.py                # ⏳ ML model metadata storage
```

## Implemented Models

### database_models.py ✅
**Migrated from**: Direct MySQL schema used by R scripts

Complete SQLAlchemy ORM implementation with:

**Core Entities**:
- `User`: User authentication and profile management
- `Portfolio`: Portfolio definitions and metadata
- `AllocationHistory`: Historical allocation records
- `PerformanceMetrics`: Portfolio performance tracking
- `FeatureList`: Feature sets for machine learning models
- `MLModel`: Machine learning model metadata and storage

**Key Features**:
- **Relationships**: Proper foreign key relationships between entities
- **Timestamps**: Automatic created_at/updated_at tracking
- **Type Safety**: Full type hints for all columns and relationships
- **Constraints**: Unique constraints and data validation
- **Migration Ready**: Designed for database migrations

**Database Schema Overview**:
```sql
-- Core user management
unicorn_users (id, username, email, password_hash, created_at, ...)
unicorn_portfolios (id, userid, portfolioid, symbol, created_at)

-- Allocation and performance tracking
unicorn_allocation_history (id, userid, portfolioid, symbol, allocation, date)
unicorn_performance_metrics (id, userid, portfolioid, return_1d, sharpe_ratio, ...)

-- Machine learning support
unicorn_feature_lists (id, userid, portfolio_name, features, created_at)
unicorn_ml_models (id, model_name, model_type, parameters, performance_metrics)
```

**Relationships**:
- User → Portfolio (one-to-many)
- Portfolio → AllocationHistory (one-to-many)
- Portfolio → PerformanceMetrics (one-to-many)
- User → FeatureList (one-to-many)

## Planned Models

### user_models.py
- Extended user profile functionality
- User preferences and settings
- Authentication token management
- User activity logging

### portfolio_models.py
- Advanced portfolio operations and calculations
- Portfolio composition analysis
- Risk metrics calculation methods
- Performance attribution analysis

### market_data_models.py
- Market data caching and storage
- Real-time price feeds
- Historical data management
- Data quality metrics

### ml_models.py
- Machine learning model versioning
- Training run metadata
- Model performance tracking
- Feature importance storage

## Database Configuration

### Connection Management
The models use SQLAlchemy's declarative base with:
- **Connection Pooling**: Efficient database connection management
- **Session Management**: Proper session lifecycle handling
- **Error Handling**: Comprehensive database error handling
- **Migration Support**: Alembic-ready model definitions

### Production Considerations
- **Indexing**: Proper indexes on frequently queried columns
- **Constraints**: Data integrity through foreign key constraints
- **Validation**: Model-level validation for data quality
- **Performance**: Optimized queries and relationships

## Usage Examples

### Basic Operations
```python
from backend.models.database_models import User, Portfolio, Base
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

# Create engine and session
engine = create_engine('mysql://user:pass@host/db')
Session = sessionmaker(bind=engine)
session = Session()

# Create user
user = User(username='trader1', email='trader@example.com')
session.add(user)
session.commit()

# Create portfolio
portfolio = Portfolio(userid=user.id, portfolioid=1, symbol='AAPL')
session.add(portfolio)
session.commit()
```

### Advanced Queries
```python
# Get user's portfolios with performance metrics
user_portfolios = session.query(User)\
    .filter(User.username == 'trader1')\
    .first()\
    .portfolios

# Get allocation history for analysis
allocations = session.query(AllocationHistory)\
    .filter(AllocationHistory.userid == user_id)\
    .order_by(AllocationHistory.date.desc())\
    .all()
```

## Integration Points

### Service Layer Integration
- **MarketDataCollector**: Uses models for data persistence
- **PortfolioService**: Operates on portfolio and allocation models
- **MLService**: Stores model metadata and feature lists

### API Layer Integration
- **REST Endpoints**: Serialize models to JSON responses
- **Data Validation**: Model validation before database operations
- **Authentication**: User model integration with auth middleware

## Migration Progress

- ✅ **Complete**: database_models.py with full schema
- ⏳ **In Progress**: Core models implemented, extensions planned
- 🎯 **Next Priority**: Extended user and portfolio model functionality
- 📋 **Dependencies**: Integration with services and API layers
