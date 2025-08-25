# Backtesting

**Status**: 📦 Legacy R framework preserved, ready for LEAN integration

Historical strategy validation and performance testing framework for the Unicorn Investing platform.

## Current Status (August 2025)

### ✅ Preservation Complete
- **Legacy Framework**: Complete R-based backtesting system preserved
- **Database Schema**: Historical database structures maintained
- **Integration Ready**: Prepared for QuantConnect LEAN backtesting engine
- **Migration Path**: Clear pathway from R to Python/LEAN implementation

### 🔄 Future Integration
- **LEAN Backtesting**: Transition to QuantConnect LEAN backtesting engine
- **Python Migration**: Convert R analytics to Python with pandas/numpy
- **Performance Boost**: Leverage LEAN's optimized backtesting infrastructure
- **Enhanced Features**: Access to LEAN's advanced backtesting capabilities

## Purpose
- Validate trading strategies against historical market data
- Performance attribution and risk analysis using LEAN framework
- Strategy comparison and optimization with advanced metrics
- Portfolio analytics and research with institutional-grade tools
- Integration with genetic algorithms and neural network strategies

## Legacy Components

### portfolioanalyticsnotes
**Status**: 📦 Historical documentation preserved
**Purpose**: Documentation and research notes on portfolio analytics methodologies
**Content**: Text file containing notes on backtesting approaches, performance metrics, and analytical frameworks
**Migration**: Ready for conversion to Python documentation

## Subdirectories

### unicorninvesting-uniquant/
**Status**: ✅ Complete R framework preserved
**Purpose**: Comprehensive backtesting and portfolio management framework
**Description**: Self-contained R package for quantitative analysis with database integration

**Key Components (Legacy R Implementation)**:
- User management system with bcrypt password hashing
- Portfolio creation and management
- Holding tracking for stocks and forex
- Trade history and audit trails
- Database schema with proper relationships

**Database Tables (Historical Schema)**:
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
