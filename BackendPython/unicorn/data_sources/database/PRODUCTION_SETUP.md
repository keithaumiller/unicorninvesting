# Database

**Status**: ✅ MySQL 8.0 production environment with financial-grade security

Database schemas, migrations, and configuration for the Unicorn Investing platform.

## Database Environment (August 2025)

### ✅ Production Configuration
- **Database Server**: MySQL 8.0.40-0ubuntu0.24.04.1
- **Security**: Isolated databases with dedicated authentication
- **Performance**: Optimized for financial data queries
- **Backup**: Automated backup strategies configured

### 🗄️ Database Structure

#### Financial Analytics Database
```sql
-- Main financial data and machine learning models
Database: unicorn_analytics
User: unicorn_admin
Purpose: Portfolio data, market data, ML models, trading history
```

#### Drupal Web Applications
```sql
-- Business integration site
Database: stlouisintegration_drupal
User: stlint_user
Purpose: Business services and client management

-- Professional portfolio site  
Database: angelicafeliciano_drupal
User: angel_user
Purpose: Professional portfolio and content management

-- Main trading platform
Database: unicorninvesting_drupal
User: unicorn_user  
Purpose: Trading interface and user management
```

## Security Configuration ✅

### Authentication
- **Strong Passwords**: All database users have secure authentication
- **Database Isolation**: Each application has dedicated database and user
- **Connection Security**: SSL-capable connections configured
- **Access Control**: Principle of least privilege enforced

### User Management
```sql
-- Example secure user configuration
CREATE USER 'unicorn_admin'@'localhost' IDENTIFIED BY '[SECURE_PASSWORD]';
GRANT ALL PRIVILEGES ON unicorn_analytics.* TO 'unicorn_admin'@'localhost';
```

## Database Schemas

### Financial Data Schema (unicorn_analytics)
```sql
-- Portfolio management tables
portfolios          # Portfolio definitions and metadata
holdings           # Current position data
transactions       # Trade history and execution records
market_data        # Historical and real-time market data
indicators         # Technical analysis indicators
ml_models          # Machine learning model storage
performance        # Portfolio performance metrics
risk_metrics       # Risk analysis and calculations
```

### Integration Schema (Shared)
```sql
-- Cross-platform integration tables
users              # Unified user management
sessions           # Authentication sessions
api_keys           # External API credentials
configurations     # System configuration settings
audit_log          # Security and compliance logging
```

## Connection Configuration

### Python Backend Connection
```python
# SQLAlchemy configuration
DATABASE_URL = "mysql+pymysql://unicorn_admin:PASSWORD@localhost/unicorn_analytics"

# Connection pooling for high-performance trading
engine = create_engine(
    DATABASE_URL,
    pool_size=20,
    max_overflow=30,
    pool_pre_ping=True
)
```

### Drupal Database Connection
```php
# Drupal settings.php configuration
$databases['default']['default'] = [
  'database' => 'unicorninvesting_drupal',
  'username' => 'unicorn_user',
  'password' => '[SECURE_PASSWORD]',
  'host' => 'localhost',
  'driver' => 'mysql',
];
```

## Performance Optimization

### Indexing Strategy
- **Primary Keys**: All tables with optimized primary keys
- **Foreign Keys**: Proper relationships with indexing
- **Query Optimization**: Indexes on frequently queried columns
- **Partitioning**: Large tables partitioned by date for performance

### Connection Management
- **Connection Pooling**: Configured for high-throughput trading
- **Query Caching**: MySQL query cache enabled
- **InnoDB Configuration**: Optimized for ACID compliance
- **Monitoring**: Performance monitoring and alerting

## Data Management

### Backup Strategy
```bash
# Automated daily backups
mysqldump --single-transaction unicorn_analytics > backup_$(date +%Y%m%d).sql

# Weekly full system backup
# Monthly archival to secure storage
```

### Migration Management
```bash
# Database migrations for schema updates
# Version-controlled schema changes
# Rollback procedures for failed migrations
```

## Integration Points

### QuantConnect LEAN
- **Market Data**: Shared market data storage
- **Algorithm State**: Trading algorithm state persistence  
- **Performance Tracking**: Unified performance metrics

### Python Backend
- **SQLAlchemy ORM**: Object-relational mapping for Python
- **Connection Pooling**: High-performance database connections
- **Transaction Management**: ACID compliance for financial data

### Drupal Frontend
- **User Management**: Unified user authentication
- **Content Storage**: CMS content and configuration
- **API Integration**: RESTful data exchange

## Security Compliance

### Financial Data Protection
- **Encryption**: Sensitive data encrypted at rest
- **Access Logging**: All database access logged
- **Compliance**: Financial industry security standards
- **Audit Trail**: Complete transaction history

### Development Security
- **Environment Separation**: Development/staging/production isolation
- **Credential Management**: Secure credential storage
- **Access Control**: Role-based database permissions
- **Security Updates**: Regular security patch management

## Monitoring and Maintenance

### Health Monitoring
- **Performance Metrics**: Query performance tracking
- **Connection Monitoring**: Database connection health
- **Storage Monitoring**: Disk space and growth tracking
- **Error Logging**: Comprehensive error tracking

### Maintenance Procedures
- **Regular Backups**: Automated daily and weekly backups
- **Index Maintenance**: Regular index optimization
- **Log Rotation**: Database log management
- **Security Updates**: Regular MySQL security updates