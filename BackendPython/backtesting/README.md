# Backtesting

Historical strategy validation and performance testing framework.

## Purpose
- Validate trading strategies against historical market data
- Performance attribution and risk analysis
- Strategy comparison and optimization
- Portfolio analytics and research

## Files

### portfolioanalyticsnotes
**Purpose**: Documentation and research notes on portfolio analytics methodologies
**Content**: Text file containing notes on backtesting approaches, performance metrics, and analytical frameworks

## Subdirectories

### unicorninvesting-uniquant/
**Purpose**: Comprehensive backtesting and portfolio management framework
**Description**: Self-contained R package for quantitative analysis with database integration

**Key Components**:
- User management system with bcrypt password hashing
- Portfolio creation and management
- Holding tracking for stocks and forex
- Trade history and audit trails
- Database schema with proper relationships

**Database Tables**:
- `uniquant_users` - User authentication and profiles
- `uniquant_portfolio` - Portfolio definitions and ownership
- `uniquant_holding` - Individual security holdings
- `uniquant_holding_forex` - Forex-specific holding details
- `uniquant_history` - Historical performance tracking
- `uniquant_trade` - Trade execution records

**Environment Configuration**:
- Configurable database connections (MySQL)
- CRAN package mirror settings
- Cache directory management
- Password encryption salt configuration

**Dependencies**:
- R statistical computing environment
- Python integration for data scraping (BeautifulSoup4, scrapy)
- MySQL database backend
- bcrypt for secure password handling

**Usage**:
- Run `Rscript setup.R` to install R dependencies
- Run `pip install -r requirements.txt` for Python components
- Configure environment variables for database connection
- Execute `example.R` for demonstration workflows

This backtesting framework provides enterprise-grade portfolio management capabilities with proper user authentication, trade tracking, and performance analytics suitable for institutional use.
