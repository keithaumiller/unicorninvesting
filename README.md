# Unicorn Investing Platform

An advanced algorithmic trading platform that combines Genetic Algorithms with Neural Networks for automated portfolio optimization and quantitative trading, now integrated with QuantConnect LEAN for institutional-grade algorithmic trading execution.

## 🚀 Quick Start After Codespace Restart

**IMPORTANT: After any codespace restart or pause, run these commands first:**

```bash
# Set up environment and aliases
source /workspaces/unicorninvesting/scripts/setup_environment.sh

# Start and validate all services (Apache, MySQL, Drupal)
drupal-start
```

This will ensure Apache web server and MySQL database are running, and the Drupal site is accessible at:
- **Homepage**: https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/
- **LEAN Dashboard**: https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/admin/metrics

## Overview

Unicorn Investing is a sophisticated investment analysis and algorithmic trading platform that uses machine learning to optimize portfolio allocations across stocks and forex markets. The platform has been modernized with a clean separation between proprietary algorithms and the QuantConnect LEAN trading framework.

## Core Technology Stack

### Current Architecture (August 2025)
- **Frontend**: Drupal 11 web interface with PHP 8.3
- **Backend**: Python 3.12 with advanced data science libraries
- **Trading Engine**: QuantConnect LEAN framework integration
- **Database**: MySQL 8.0 with financial-grade security
- **Infrastructure**: Ubuntu 24.04 LAMP stack with SSL certificates

### Algorithm Architecture
- **Genetic Algorithm (GA)**: Feature selection and neural network hyperparameter optimization
- **Neural Networks**: Portfolio allocation decisions and risk balancing using TensorFlow
- **Portfolio Optimization**: Daily rebalancing based on ML predictions
- **LEAN Integration**: Algorithmic trading execution through QuantConnect framework
- **Performance Tracking**: Continuous backtesting and performance evaluation

### Key Capabilities
- **Multi-Asset Trading**: Stocks, forex, and cryptocurrency support
- **Real-Time Analytics**: Live market data processing and analysis
- **Machine Learning**: Advanced AI-driven portfolio optimization
- **Risk Management**: Sophisticated risk controls and position sizing
- **Backtesting**: Historical strategy validation with LEAN engine
- **Web Interface**: Modern Drupal 11 interface replacing legacy desktop apps

## Quick Start

### 🚀 Production Installation

For complete production deployment, see the **[Installation Guide](INSTALLATION.md)** which provides:

- Step-by-step setup instructions for all components
- Database configuration and optimization
- Python environment with Prophet forecasting
- LEAN framework integration
- Apache/PHP/Drupal configuration
- SSL certificate setup
- Production deployment scripts
- Health monitoring and backup procedures

### 📋 Deployment Configuration

The **[deploy.yml](deploy.yml)** file contains the complete deployment specification including:

- Infrastructure requirements and system dependencies
- Service configurations and environment variables
- Security settings and performance tuning
- Validation checklists and rollback procedures

### 🧪 Development Environment

For immediate development, the workspace is pre-configured with:
- Python 3.12 virtual environment at `/workspaces/unicorninvesting/.venv/`
- FastAPI backend running at `http://localhost:8000`
- Prophet forecasting capabilities fully installed and tested
- LEAN framework available for algorithm development

## Project Structure

### BackendPython/
**Status**: ✅ Fully configured Python 3.12 environment with 90+ packages
```
BackendPython/
├── Lean/                      # QuantConnect LEAN Framework (git submodule)
│   ├── Algorithm/            # LEAN algorithm framework
│   ├── Algorithm.Python/     # Python algorithm examples
│   ├── Data/                 # Market data handling
│   └── Engine/               # Trading execution engine
└── unicorn/                  # Proprietary Unicorn code
    ├── backend/              # ✅ Python backend services
    │   ├── api/             # ✅ FastAPI REST endpoints
    │   ├── ml/              # Machine learning models
    │   ├── models/          # Database models
    │   └── services/        # Business logic
    ├── algorithms/          # Custom LEAN algorithms
    ├── integrations/        # LEAN integration layer
    ├── data/               # Market data and analysis
    ├── backtesting/        # Strategy validation
    └── legacy/             # R scripts (being migrated)
```

### Infrastructure
**Status**: ✅ Production-ready LAMP stack deployed
- **Web Server**: Apache 2.4.58 with SSL certificates
- **Database**: MySQL 8.0 with isolated databases
- **SSL Security**: Let's Encrypt certificates with auto-renewal
- **Multi-Domain**: Supporting 4 domains with dedicated configurations

### WebFrontend/
**Status**: ✅ Drupal 11 installations ready for configuration
- **Framework**: Drupal 11 with PHP 8.3
- **Domains**: Multiple production sites configured
- **Security**: Database isolation with strong authentication
- **Integration**: Ready for Python backend API integration

## Environment Status

### Python Environment (✅ Configured)
- **Version**: Python 3.12.3 in virtual environment
- **Location**: `/workspaces/unicorninvesting/.venv/`
- **Packages**: 90+ scientific and financial libraries installed
- **Testing**: FastAPI application verified and functional

### Key Dependencies Installed
```bash
# Core Data Science
pandas==2.3.2, numpy==2.3.2, scipy==1.16.1

# Machine Learning
tensorflow==2.20.0, scikit-learn==1.7.1, statsmodels==0.14.5

# Financial Data
yfinance==0.2.65, alpha_vantage==3.0.0

# Web Framework
fastapi==0.116.1, uvicorn==0.35.0, pydantic==2.11.7

# Database
SQLAlchemy==2.0.43, PyMySQL==1.1.2, redis==6.4.0

# Development
jupyter==1.1.1, pytest==8.4.1, structlog==25.4.0
```

### Database Configuration (✅ Operational)
- **MySQL Databases**: 
  - `unicorn_analytics` (main financial data)
  - `stlouisintegration_drupal`, `angelicafeliciano_drupal`, `unicorninvesting_drupal`
- **Security**: Dedicated users with strong authentication
- **Backup**: Automated backup strategies in place

### API Status (✅ Functional)
- **Endpoint**: `http://localhost:8000`
- **Framework**: FastAPI with automatic documentation
- **Features**: Configuration management, health checks, CORS support
- **Documentation**: Available at `/docs` and `/redoc`

## Architecture Transition

### Completed Components ✅
- **LEAN Integration**: QuantConnect framework cloned and configured as git submodule
- **Python Migration**: Virtual environment with all required packages
- **Code Segregation**: Clean separation of proprietary vs third-party code
- **Infrastructure**: Full LAMP stack with SSL certificates
- **API Foundation**: FastAPI application ready for development
- **Database Setup**: MySQL with financial-grade security

### In Progress 🔄
- **R to Python Migration**: Converting legacy R algorithms to Python
- **LEAN Algorithm Development**: Creating custom trading algorithms
- **Frontend Integration**: Connecting Drupal with Python backend
- **ML Model Implementation**: Rebuilding genetic algorithms and neural networks

### Legacy Components (Being Migrated) 📦
- **R Scripts**: Original quantitative models in R
- **WPF Applications**: Legacy Windows desktop apps
- **File-based Storage**: Being replaced with database integration
- FastAPI backend services with REST APIs
- Optimized MySQL schema with proper ORM integration

## 📊 Data Sources & Integrations

### Interactive Brokers (IBKR) - ✅ Operational
- **Status**: Fully integrated with Client Portal Gateway
- **Authentication**: Manual login via web interface required
- **Gateway URL**: https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/
- **Data Types**: Real-time and historical cryptocurrency, stocks, forex
- **Implementation**: Custom Python connector with REST API
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/`
- **Features**: 
  - Live market data streaming
  - Historical data collection (OHLCV bars)
  - Account information access
  - Trading capabilities (when authenticated)

### Yahoo Finance - ✅ Operational  
- **Status**: Active and collecting data
- **Authentication**: No authentication required
- **Data Types**: Historical and real-time market data for stocks, crypto, indices
- **Implementation**: Python yfinance library integration
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/`
- **Features**:
  - Minute-level granular data
  - No rate limits for basic usage
  - Comprehensive symbol coverage

### Alpha Vantage - ⚠️ Configuration Required
- **Status**: Framework ready, API key needed
- **Authentication**: API key required (free tier available)
- **Data Types**: Stocks, forex, cryptocurrencies, economic indicators
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/alpha_vantage/`
- **Setup Required**: Configure API key in connector settings

## Getting Started

### Prerequisites
- R 4.0+ (current legacy system)
- Python 3.9+ (target migration)
- MySQL 8.0+
- PHP 8.2+ and Drupal 11 (web interface)

### Current System (R-based)
```bash
# Install R dependencies
Rscript -e "install.packages(c('quantmod','FCNN4R','GA','forecast','RMySQL'))"

# Run single neural network optimization
Rscript BackendPython/quickstartsingleNN.R

# Run genetic algorithm portfolio optimization  
Rscript BackendPython/quickstartGAportfolio.R
```

### Migration to Python (In Progress)
```bash
# Setup Python environment
pip install pandas numpy scikit-learn tensorflow fastapi sqlalchemy

# Future: Run Python equivalent
python backend/main.py
```

## 📁 Portfolio Management

### Portfolio Structure
**Location**: `BackendPython/unicorn/4_portfolios/portfolios/` (Within LEAN framework)
**Organization**: Each portfolio in its own self-contained directory

### Available Portfolios

#### ✅ ETH_Only Portfolio (Ready for Deployment)
- **Strategy**: 100% Ethereum allocation
- **Risk Profile**: Medium-High volatility
- **Configuration**: Complete with risk parameters and execution settings
- **Status**: Production ready with validated alpha models
- **Location**: `BackendPython/unicorn/4_portfolios/portfolios/ETH_Only/`

#### 📋 BTC_ETH_Mixed Portfolio (Planned)
- **Strategy**: Balanced Bitcoin (60%) + Ethereum (40%) allocation
- **Risk Profile**: Medium volatility with diversification benefits
- **Status**: Configuration complete, awaiting BTC model development
- **Location**: `BackendPython/unicorn/4_portfolios/portfolios/BTC_ETH_Mixed/`

#### 📋 Multi_Asset Portfolio (Planned)
- **Strategy**: Diversified across crypto, forex, and equities
- **Risk Profile**: Lower volatility through cross-asset diversification
- **Status**: Planning phase
- **Location**: `BackendPython/unicorn/4_portfolios/portfolios/Multi_Asset/`

### Portfolio Configuration Standard
Each portfolio contains:
- `config.json` - Asset allocations and strategy parameters
- `risk_parameters.json` - Risk management settings and limits
- `execution_settings.json` - Trading execution configuration
- `README.md` - Portfolio-specific documentation and strategy details

### Integration with LEAN Framework
- **Data Sources**: Real-time feeds from IBKR, Yahoo Finance, Alpha Vantage
- **Alpha Models**: Cryptocurrency, forex, and equity forecasting models
- **Risk Management**: Integrated risk budgeting and portfolio construction
- **Execution**: Automated order placement and position management

## Key Features

### Algorithmic Trading
- **Daily Rebalancing**: Automated portfolio optimization based on ML predictions
- **Risk Management**: Position sizing and risk-adjusted returns
- **Multi-Asset Support**: Stocks, forex, and other financial instruments
- **Backtesting**: Historical performance validation over multiple time periods

### Machine Learning Pipeline
1. **Data Collection**: Real-time market data from multiple sources
2. **Feature Engineering**: Technical indicators and market metrics
3. **GA Optimization**: Feature selection and hyperparameter tuning
4. **Neural Training**: Portfolio allocation model training
5. **Performance Evaluation**: Backtesting and risk assessment
6. **Live Trading**: Daily allocation recommendations

### Performance Metrics
- Portfolio value tracking from $1000 seed money
- Risk-adjusted returns and Sharpe ratio analysis
- Drawdown analysis and risk management
- Comparison against market benchmarks

## License

[Add license information]

## Contributing

[Add contribution guidelines]

---

*This platform represents a comprehensive approach to quantitative trading, combining advanced machine learning techniques with robust portfolio management principles for institutional-grade algorithmic trading.*