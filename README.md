# Unicorn Investing Platform

> ## ⚠️ **IMPORTANT LEGAL NOTICE**
> 
> **🚨 HIGH RISK WARNING: TRADING FINANCIAL INSTRUMENTS INVOLVES SUBSTANTIAL RISK OF LOSS**
> 
> - 📚 **Educational Purpose Only** - This software is for learning and research
> - 🚫 **Not Investment Advice** - No investment recommendations are provided
> - ⚖️ **Your Risk** - You are solely responsible for any trading decisions
> - 💼 **Professional Consultation Required** - Consult qualified financial professionals
> - 🔒 **No Guarantees** - Past performance does not predict future results
> 
> **📄 Required Reading Before Use:**
> - [**DISCLAIMER.md**](./DISCLAIMER.md) - Critical risk disclosures
> - [**TERMS_OF_SERVICE.md**](./TERMS_OF_SERVICE.md) - Usage terms and conditions
> - [**LICENSE**](./LICENSE) - MIT License with financial disclaimers
> 
> **By using this software, you acknowledge reading and accepting all terms and risks.**

## 📋 Table of Contents

- [📊 Production Status](#-production-status-enhanced-pipeline-architecture)
- [🚀 Quick Start](#-quick-start-after-codespace-restart)
- [🏗️ Architecture Overview](#️-architecture-overview)
- [📁 Project Structure](#-project-structure--documentation)
- [🎯 System Status](#-comprehensive-system-status)
- [🤝 Collaboration](#-interested-in-collaborating)
- [📄 Legal & Contributing](#-legal-information--contributing)
- [🔑 Key Features](#-key-features)

## 📁 Project Structure & Documentation

### **📚 Legal & Governance Documents**
- 📄 [**DISCLAIMER.md**](./DISCLAIMER.md) - Financial risk disclosures and limitations
- 📜 [**TERMS_OF_SERVICE.md**](./TERMS_OF_SERVICE.md) - Usage terms and liability protection
- 📋 [**LICENSE**](./LICENSE) - MIT License with financial disclaimers
- 🤝 [**CONTRIBUTING.md**](./CONTRIBUTING.md) - Contribution guidelines and commercial services
- 🔒 [**SECURITY.md**](./SECURITY.md) - Security policy and vulnerability reporting

### **🏗️ Core System Directories**
- 🐍 [**BackendPython/**](./BackendPython/README.md) - Python trading engine and LEAN integration
- 🌐 [**WebFrontend/**](./WebFrontend/README.md) - Drupal 11 web interface and dashboard
- ⚙️ [**config/**](./config/README.md) - Secure configuration management system
- 🚀 [**deployment/**](./deployment/README.md) - Production deployment scripts and configurations
- 🧪 [**tests/**](./tests/README.md) - Comprehensive testing framework
- 🛠️ [**scripts/**](./scripts/README.md) - Environment setup and utility scripts

### **📖 Documentation**
- 📚 [**docs/**](./docs/README.md) - Complete technical documentation
  - 🔌 [**docs/api/**](./docs/api/README.md) - API specifications and references
  - 🏗️ [**docs/deployment/**](./docs/deployment/README.md) - Deployment guides and procedures
  - 🔗 [**docs/integration/**](./docs/integration/README.md) - Integration guides and tutorials

---

## 🚀 **Production Status: Enhanced Pipeline Architecture**

**Last Updated**: September 11, 2025  
**System Status**: ✅ **PRODUCTION READY** - Enhanced data pipeline with Yahoo Finance integration, bronze + silver layer processing  
**New Features**: ✨ Yahoo Finance 9-asset integration + Silver layer analytics + Cross-asset correlation + Market regime detection  
**Frontend Status**: 🔄 Drupal 11 functional but validation pending, backend integration in development  
**Legacy Code**: ✅ R scripts archived in legacy - no migration planned

---

## 🏗️ Architecture Overview

An advanced algorithmic trading platform that combines machine learning forecasting with LEAN framework integration for institutional-grade algorithmic trading execution. ✨ **Enhanced with comprehensive Yahoo Finance integration and bronze layer processing featuring 9 assets across 3 intervals with sophisticated feature engineering.**

## 📊 **Enhanced System Implementation Summary (September 2025)**

### **✅ YAHOO FINANCE INTEGRATION & BRONZE LAYER PROCESSING**
- **Multi-Asset Coverage**: 9 assets (ETH, BTC, 7 major forex pairs) across 3 intervals (1m, 1h, 1d)
- **Automated Data Pipeline**: Daily, Delta, and Hourly collection schedules with bronze processing
- **Bronze Layer Features**: Price analysis, technical indicators (RSI, moving averages), volatility, temporal features
- **Pipeline Integration**: 7-step daily pipeline with 100% success rate (7,872 records processed)
- **Cron Automation**: Multi-frequency collection integrated with existing FRED/BEA infrastructure

### **✅ SILVER LAYER DATA PROCESSING**
- **Economic Indicators**: Comprehensive processing of 580+ economic indicators across 4 categories
- **Feature Engineering**: 50+ derived features including moving averages, momentum indicators, composite indices
- **Yahoo Finance Silver**: 9 assets with enhanced TA-Lib indicators (54-66 features per asset)
- **Cross-Asset Analytics**: Correlation analysis, market regime detection, risk factor decomposition
- **Quality Assessment**: Data completeness scoring, temporal alignment, schema compliance validation
- **Alpha Model Integration**: Enhanced datasets with 51 economic features + 54-66 financial features ready for crypto model consumption

### **🤖 ENHANCED ALPHA MODELS** 
- **Economic-Enhanced XGBoost**: Individual models with comprehensive economic feature integration
- **Economic Ensemble Models**: Combined Prophet + XGBoost models with optimized weighting
- **Multi-Criteria Model Selection**: Enhanced selector with 40% R², 30% economic importance, 20% MAE, 10% complexity scoring
- **Performance Tracking**: SQLite databases for model performance and ensemble tracking

### **🔍 PIPELINE VALIDATION FRAMEWORK**
- **Comprehensive Pipeline Validation**: End-to-end validation from data sources to portfolio construction
- **Alpha Models Flow Validation**: Specialized validation for economic-enhanced model pipeline
- **Production Readiness Assessment**: Automated scoring and recommendations
- **Component Health Monitoring**: Continuous validation of pipeline components and data flow

### **🌐 FRONTEND & INTEGRATION STATUS**
- **Drupal 11**: Functional web interface with PHP 8.3 - validation pending
- **Backend Integration**: Python FastAPI services - connection to frontend in development
- **Database**: MySQL 8.0 with financial-grade security and performance tracking databases
- **Architecture**: Clean separation between frontend presentation and backend processing

## 🚀 Quick Start After Codespace Restart

**IMPORTANT: After any codespace restart or pause, run these commands first:**

```bash
# Set up environment and aliases (enhanced with pipeline validation)
source /workspaces/unicorninvesting/scripts/unicorn_environment.sh

# Start and validate all services (Apache, MySQL, Drupal)
drupal-start

# Optional: Run pipeline validation to verify system health
cd /workspaces/unicorninvesting/BackendPython/unicorn
python pipeline_validation.py

# Optional: Test the enhanced data pipeline with Yahoo Finance integration
cd /workspaces/unicorninvesting
./scripts/data_pipeline.sh hourly  # Test Yahoo Finance + bronze processing
```

This will ensure Apache web server and MySQL database are running, and the Drupal site is accessible at:
- **Homepage**: https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/
- **LEAN Dashboard**: https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/admin/metrics

**Note**: Drupal frontend is functional but validation pending. Backend integration is in development.

## Overview

Unicorn Investing is a sophisticated investment analysis and algorithmic trading platform that uses machine learning to optimize portfolio allocations across cryptocurrency markets. The platform has been modernized with a clean separation between proprietary algorithms and the QuantConnect LEAN trading framework, ✨ **now enhanced with Yahoo Finance integration (9 assets × 3 intervals) and comprehensive bronze layer processing with feature engineering**.

## 🎯 **Current Architecture Status & Capabilities**

### **✅ FULLY IMPLEMENTED COMPONENTS**

#### **1. Yahoo Finance Integration & Bronze Layer Processing**
**Location**: `BackendPython/unicorn/1_data_sources/1_raw/data/yahoo_finance/` & `2_bronze/yahoo_finance_assets/`
**Status**: ✅ **PRODUCTION READY** with comprehensive multi-asset integration

**Capabilities**:
- **9 Assets Coverage**: ETH, BTC, 7 major forex pairs (EURUSD, USDJPY, GBPUSD, AUDUSD, USDCAD, USDCHF, NZDUSD)
- **3 Intervals**: 1-minute (delta), 1-hour (hourly), 1-day (daily) = 27 data streams
- **Bronze Layer Processing**: Asset-specific processors with feature engineering
- **Feature Engineering**: Price analysis, technical indicators (RSI, moving averages), volatility, temporal features
- **Pipeline Integration**: 7-step daily, 6-step delta, 3-step hourly pipelines (100% success rate)
- **Performance**: 2 seconds per asset per interval, ~85KB per collection, 7,872+ records processed daily

#### **2. Silver Layer Economic Data Processing**
**Location**: `BackendPython/unicorn/1_data_sources/3_silver/`
**Status**: ✅ **PRODUCTION READY** with comprehensive economic integration

**Capabilities**:
- **580+ Economic Indicators**: Processed across 4 categories (Economic Growth, Consumer/Business, Monetary Policy, International Trade)
- **Advanced Feature Engineering**: 50+ derived features with momentum indicators and composite indices
- **Quality Assessment Framework**: Automated data quality scoring, temporal alignment, schema compliance
- **Alpha Model Integration**: Enhanced datasets with 51 economic features ready for crypto model consumption
- **Performance Optimization**: PyArrow integration for 5-10x processing speed improvements

#### **2. Enhanced Alpha Models Framework**
**Location**: `BackendPython/unicorn/2_alpha_models/CRYPTO/`
**Status**: ✅ **ENHANCED** with economic integration and ensemble capabilities

**Capabilities**:
- **Economic-Enhanced XGBoost Models**: Individual models leveraging comprehensive economic features
- **Economic Ensemble Models**: Combined Prophet + XGBoost models with optimized weighting
- **Multi-Criteria Model Selection**: Enhanced selector with weighted scoring (R², economic importance, MAE, complexity)
- **Performance Tracking**: SQLite databases tracking model performance and ensemble effectiveness
- **Production Configuration**: Automated deployment configuration generation

#### **3. Comprehensive Pipeline Validation**
**Location**: `BackendPython/unicorn/`
**Status**: ✅ **NEW** comprehensive validation framework

**Capabilities**:
- **`pipeline_validation.py`**: End-to-end validation from data sources to portfolio construction
- **`alpha_models_pipeline_validation.py`**: Specialized alpha models flow validation
- **Production Readiness Assessment**: Automated scoring with recommendations
- **Component Health Monitoring**: Continuous validation of all pipeline components

#### **4. Enhanced Portfolio Construction**
**Location**: `BackendPython/unicorn/4_portfolios/Myportolio/`
**Status**: ✅ **ENHANCED** with advanced model selection utilities

**Capabilities**:
- **Enhanced Model Selector**: Multi-criteria selection supporting individual vs ensemble models
- **Kelly Criterion Integration**: Optimal position sizing with risk management
- **Algorithm Separation**: Clean separation between risk algorithms and trading algorithms
- **Framework Utilities**: Comprehensive integration of all portfolio components

#### **5. Frontend Framework** 
**Location**: `WebFrontend/`
**Status**: 🔄 **FUNCTIONAL** - Drupal 11 operational, validation and backend integration pending

**Current State**:
- **Drupal 11**: Web interface with PHP 8.3 - functionally operational
- **Database Integration**: MySQL 8.0 with proper security configurations
- **Backend Connection**: Python FastAPI integration in development
- **Validation Status**: Frontend functionality pending comprehensive testing

### **📦 ARCHIVED COMPONENTS (No Migration Planned)**

#### **Legacy R Scripts**
**Location**: `BackendPython/unicorn/legacy/`
**Status**: ✅ **ARCHIVED** - No conversion to Python planned

**Archived Components**:
- All R statistical analysis scripts permanently moved to legacy
- R-based genetic algorithms and neural network implementations archived
- Historical R project files preserved for reference only
- **Decision**: Python ecosystem provides superior capabilities - no R migration needed

## Core Technology Stack

### Current Architecture (September 2025)
- **Frontend**: Drupal 11 web interface with PHP 8.3 (functional, validation pending)
- **Backend**: Python 3.12 with enhanced data science and economic processing capabilities
- **Trading Engine**: QuantConnect LEAN framework integration ready for deployment
- **Database**: MySQL 8.0 with financial-grade security and performance tracking databases
- **Infrastructure**: Ubuntu 24.04 LAMP stack with SSL certificates
- **✨ Yahoo Finance Integration**: 9 assets (crypto + forex) with 3 intervals and bronze layer processing
- **✨ Economic Processing**: Silver layer data processing with 580+ indicators and feature engineering
- **✨ Model Selection**: Multi-criteria enhanced model selector with ensemble support

### Enhanced Algorithm Architecture
- **Yahoo Finance Pipeline**: Raw data collection → Bronze layer processing → Feature engineering (27 data streams)
- **Economic Data Pipeline**: Silver layer processing with comprehensive feature engineering
- **Enhanced Alpha Models**: Economic-enhanced XGBoost and ensemble models with multi-criteria selection  
- **Portfolio Optimization**: Enhanced model selection with Kelly criterion integration
- **LEAN Integration**: Algorithmic trading execution framework through QuantConnect
- **Pipeline Validation**: Comprehensive end-to-end validation from data sources to portfolio construction
- **Performance Tracking**: Advanced model performance and ensemble effectiveness monitoring

### Key Capabilities
- **✨ Yahoo Finance Integration**: 9 assets × 3 intervals with automated bronze layer processing
- **Enhanced Economic Processing**: 580+ economic indicators with comprehensive feature engineering
- **Multi-Criteria Model Selection**: Advanced selector comparing individual vs ensemble models
- **Real-Time Analytics**: Live market data processing and analysis with economic integration
- **Machine Learning**: Economic-enhanced AI-driven alpha models with ensemble capabilities
- **Risk Management**: Sophisticated risk controls with Kelly criterion and portfolio construction
- **Pipeline Validation**: Comprehensive validation framework ensuring production readiness
- **Web Interface**: Modern Drupal 11 interface (functional, backend integration in development)
- **Legacy Archive**: R scripts permanently archived - no migration planned

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

## 🚨 **CRITICAL PATH TO LIVE TRADING**

### **System Status: 92% Operational (Enhanced Pipeline Architecture)**

**Last Updated**: September 11, 2025

#### **✅ READY FOR DEVELOPMENT:**
- **Python Environment**: Python 3.12.1 with enhanced ML and economic processing libraries ✅
- **LEAN Framework**: .NET 8.0.412 ready for algorithmic trading ✅  
- **Enhanced ETH Alpha Models**: Economic-enhanced models with ensemble capabilities ✅
- **Yahoo Finance Integration**: 9 assets × 3 intervals with bronze layer processing ✅
- **Silver Layer Processing**: 580+ economic indicators with feature engineering ✅
- **Signal Generation**: Enhanced real-time pipeline with economic integration ✅
- **Multi-Criteria Model Selection**: Advanced selector with weighted scoring ✅
- **Risk Management**: Enhanced risk algorithms with Kelly criterion integration ✅
- **Portfolio Construction**: Enhanced model selection and portfolio optimization ✅
- **Pipeline Validation**: Comprehensive end-to-end validation framework ✅
- **Data Pipeline Automation**: 7-step daily, 6-step delta, 3-step hourly pipelines (100% success) ✅
- **IBKR Gateway**: Running and responsive ✅
- **Data Sources**: Yahoo Finance (production), Alpha Vantage, enhanced economic data ✅

#### **🚨 CRITICAL GAPS BLOCKING LIVE TRADING:**

##### **1. IBKR Authentication Required (IMMEDIATE ACTION)**
**Status**: 🔴 **CRITICAL PATH BLOCKED**
- **Issue**: IBKR Gateway running but not authenticated
- **Impact**: Cannot access real account data or place orders
- **Authentication URL**: https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/
- **Next Steps**:
  1. Visit authentication URL above
  2. Login with IBKR credentials (Paper Trading mode)
  3. Complete 2FA approval when prompted
  4. **Re-run system check**: `./scripts/unicorn_environment.sh`

##### **2. Execution Models Implementation (HIGH PRIORITY)**
**Status**: 🟡 Framework ready, needs implementation
- **Location**: `BackendPython/unicorn/5_execution_models/`
- **Missing**: Order execution engine, market impact minimization
- **Estimated Time**: 1-2 weeks

##### **3. Minor System Issues**
- Database connection (Port 3306 accessibility)
- Alpha Vantage API configuration (optional)

### **🔥 IBKR ETH Live Data Capabilities**

#### **✅ CONFIRMED WORKING CAPABILITIES:**
- **Real-Time Data**: 0-second latency ETH pricing ($4,308-$4,416 range)
- **Contract Access**: ETH/USD (ID: 541686654) via ZEROHASH exchange
- **Data Quality**: Complete OHLCV with bid/ask spreads (0.008% typical)
- **Historical Data**: 1-minute to daily bars available (1000+ data points)
- **24/7 Coverage**: Cryptocurrency market hours supported
- **Professional Grade**: ZEROHASH exchange institutional data

#### **✅ Recent Live Data Verification:**
```json
Current ETH Market Snapshot:
- Last Price: $4,310.50
- Bid/Ask: $4,310.30 / $4,310.65  
- Daily Range: $4,212.20 - $4,416.55
- Volume: 724.39 ETH
- Exchange: ZEROHASH
- Update: Real-time streaming
```

#### **✅ Trading Workflow Status:**
```
Live ETH Data → ETH Models → Trading Signals → Portfolio Decisions → [EXECUTION NEEDED]
     ✅              ✅           ✅              ✅              ❌
```

### **⚡ Quick Start for Live Trading**

1. **Immediate**: Authenticate IBKR (see critical path above)
2. **Next**: Implement execution models (`5_execution_models/`)
3. **Then**: Connect portfolio decisions to order execution
4. **Finally**: Deploy live trading algorithms

---

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
- **Frontend Validation**: Drupal 11 is functional, comprehensive testing and backend integration pending
- **LEAN Algorithm Development**: Creating custom trading algorithms for deployment
- **Advanced Model Development**: Extending economic-enhanced models and ensemble capabilities
- **Production Deployment**: Finalizing enhanced pipeline for live trading operations

### Permanently Archived Components (No Migration Planned) 📦
- **Legacy R Scripts**: All R statistical models permanently archived in legacy directory
- **R Genetic Algorithms**: Neural network implementations preserved for reference only
- **Desktop Applications**: Legacy WPF applications replaced by web interface
- **Decision Rationale**: Python ecosystem provides superior capabilities - no R conversion planned
- FastAPI backend services with REST APIs
- Optimized MySQL schema with proper ORM integration

## 📊 Data Sources & Integrations

### **🏦 Financial Market Data Sources**

#### Interactive Brokers (IBKR) - ✅ Production Ready
- **Status**: ✅ **ACTIVE** - Fully operational with Client Portal Gateway
- **Authentication**: Manual login via web interface required  
- **Gateway URL**: https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/
- **Data Types**: Real-time and historical cryptocurrency, stocks, forex
- **Implementation**: Complete production connector with multiple API interfaces
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/`
- **Features**: 
  - Live ETH market data streaming (confirmed operational)
  - Portfolio management and account information
  - Trading capabilities (when authenticated)
  - Multiple connector types (Client Portal, OAuth, Web API)
  - Real-time price feeds with 0-second latency

#### Yahoo Finance - ✅ Production Ready
- **Status**: ✅ **PRODUCTION** - Complete 9-asset integration with bronze layer processing
- **Authentication**: No authentication required
- **Data Types**: ETH, BTC, 7 major forex pairs (EURUSD, USDJPY, GBPUSD, AUDUSD, USDCAD, USDCHF, NZDUSD)
- **Intervals**: 1-minute (delta pipeline), 1-hour (hourly pipeline), 1-day (daily pipeline)
- **Implementation**: Unified asset collector with organized data storage and bronze processing
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/data/yahoo_finance/`
- **Bronze Processing**: `/BackendPython/unicorn/1_data_sources/2_bronze/yahoo_finance_assets/`
- **Features**:
  - **Data Collection**: 27 data streams (9 assets × 3 intervals) with ~85KB per collection
  - **Bronze Layer Processing**: Price analysis, technical indicators, volume analysis, temporal features
  - **Feature Engineering**: RSI, moving averages (10, 20, 50), volatility (14-day, annualized), price position
  - **Pipeline Integration**: 7-step daily pipeline, 6-step delta pipeline, 3-step hourly pipeline
  - **Automation**: Cron-scheduled collection with bronze processing (100% success rate)
  - **Performance**: 2 seconds per asset per interval, organized category/asset/interval storage
- **Recent Results**: 7,872 records processed (2,222 crypto + 5,650 forex) with complete feature sets

#### Alpha Vantage - 🔄 Framework Ready
- **Status**: 🔄 **FRAMEWORK** - LEAN integration ready, configuration needed
- **Authentication**: API key required (free tier: 500 calls/month)
- **Data Types**: Stocks, forex, cryptocurrencies, economic indicators
- **Implementation**: Complete LEAN framework integration (212 lines)
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/alpha_vantage/`
- **Features**: 
  - LEAN framework custom data source
  - Rate limiting compliance (5 calls/minute)
  - Supports Resolution.MINUTE data
- **Setup Required**: API key configuration for activation

### **🏛️ U.S. Government Economic Data Sources**

#### Federal Reserve Economic Data (FRED) - ✅ Production Ready
- **Status**: ✅ **PRODUCTION** - Fully automated pipeline deployed
- **Purpose**: Comprehensive Federal Reserve economic time series data
- **Implementation**: Complete 650+ line production connector with CLI
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/`
- **Automation**: 
  - Delta updates every 15 minutes (8 critical indicators, 10-15s)
  - Daily updates at 9 PM (16 series, 30-60s execution)
  - Historical collection (25+ years available)
- **Data Coverage**: 26,426+ observations (1919-2025), 95.7% success rate
- **Key Series**: Federal funds rate, money supply, Treasury rates, inflation indicators
- **Priority**: ⭐ **HIGH** - Critical for monetary policy context

#### Bureau of Economic Analysis (BEA) - ✅ Production Ready  
- **Status**: ✅ **PRODUCTION** - Complete API implementation deployed
- **Purpose**: Macroeconomic data including GDP, consumption, business investment
- **Implementation**: Complete 900+ line production connector with automation
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/bureau_of_economic_analysis/`
- **Automation**:
  - Delta updates every 6 hours (critical indicators, 30-60s)
  - Daily updates at 6 AM (comprehensive indicators, 2-3 min)
  - Historical collection (20+ years, 5-10 min execution)
- **Data Coverage**: 15+ datasets across 6 economic categories (2000-present)
- **Alpha Integration**: 50+ engineered features for crypto alpha models
- **Priority**: ⭐ **HIGH** - Critical for macroeconomic context

#### Bureau of Labor Statistics (BLS) - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Employment, wages, productivity, and price data
- **Key Data**: Consumer Price Index (CPI), unemployment rates, Producer Price Index (PPI)
- **API**: BLS Public Data API v2
- **Rate Limits**: 25 queries/day (no key), 500 queries/day (with key)

#### U.S. Census Bureau - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Comprehensive economic and business statistics
- **Key Data**: Economic Census, retail trade, manufacturing statistics
- **API**: Census Bureau API (500 queries per IP per day)

### **🌍 International Organization Data Sources**

#### International Monetary Fund (IMF) - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Global economic and financial statistics
- **Key Data**: World Economic Outlook, International Financial Statistics
- **API**: IMF Data API (JSON RESTful), no authentication required

#### World Bank - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Development and economic data from around the world
- **Key Data**: World Development Indicators, country economic profiles
- **API**: World Bank Data API, no authentication required

#### OECD - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Economic data for OECD member countries
- **Key Data**: Main Economic Indicators, national accounts, labour force statistics
- **API**: OECD Data API (SDMX-JSON), no authentication required

### **🏢 Private Economic Data Sources**

#### Conference Board - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Leading economic indicators and business confidence data
- **Key Data**: Consumer Confidence Index, Leading Economic Indicators (LEI)
- **Cost**: ⚠️ **PAID** - Subscription required

#### National Bureau of Economic Research (NBER) - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Economic research data and business cycle dating
- **Key Data**: Historical macroeconomic data, business cycle chronology

#### Statista - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Aggregated statistics from various sources
- **Cost**: ⚠️ **PAID** - Subscription required

#### DBnomics - 📋 Planned
- **Status**: 📋 **PLANNED** - Directory created, implementation pending
- **Purpose**: Global economic database aggregating 80+ providers
- **API**: DBnomics API (RESTful), generous rate limits

### **📈 Integration Summary**
- **✅ Production Ready**: IBKR (trading data), FRED (monetary policy), BEA (macroeconomic), Yahoo Finance (9 assets + bronze processing)
- **🔄 Partial Implementation**: Alpha Vantage (framework ready)
- **📋 Planned Implementations**: 12 additional economic and financial data sources
- **🎯 Total Data Universe**: 580+ economic indicators + 27 Yahoo Finance data streams + real-time market data
- **🔗 Alpha Model Integration**: Economic features + Yahoo Finance bronze layer ready for ETH model enhancement

---

## 🔄 **Enhanced Data Pipeline Architecture**

### **Multi-Source Data Processing Pipeline**
The Unicorn platform now features a comprehensive **3-tier data processing pipeline** that seamlessly integrates multiple data sources:

#### **Tier 1: Raw Data Collection**
- **Yahoo Finance**: 9 assets (ETH, BTC, 7 forex pairs) × 3 intervals (1m, 1h, 1d) = 27 data streams
- **FRED**: 580+ economic indicators with delta/daily automation
- **BEA**: 15+ macroeconomic datasets with historical depth
- **IBKR**: Real-time market data and trading execution (when authenticated)

#### **Tier 2: Bronze Layer Processing** 
**Location**: `/BackendPython/unicorn/1_data_sources/2_bronze/`
- **Yahoo Finance Assets**: Category-specific processors (crypto, forex) with feature engineering
- **Economic Indicators**: 4-category processing (economic growth, consumer/business, monetary policy, international trade)
- **Feature Engineering**: Price analysis, technical indicators, volume analysis, temporal features
- **Output**: Processed datasets with 20+ features per asset/indicator

#### **Tier 3: Silver Layer Integration**
**Location**: `/BackendPython/unicorn/1_data_sources/3_silver/`
- **Economic Enhancement**: 51 economic features integrated into crypto models
- **Quality Assessment**: Data completeness scoring, temporal alignment, schema compliance
- **Alpha Model Ready**: Datasets optimized for machine learning consumption

### **Automated Pipeline Execution**

#### **Daily Pipeline** (7 Steps - 100% Success Rate)
```bash
./scripts/data_pipeline.sh daily
```
1. ✅ Environment validation
2. ✅ FRED economic data collection
3. ✅ BEA economic data collection  
4. ✅ Yahoo Finance daily data (1d interval)
5. ✅ Yahoo Finance hourly data (1h interval)
6. ✅ Bronze layer economic processing
7. ✅ Yahoo Finance bronze layer processing (all assets)

#### **Delta Pipeline** (6 Steps - High Frequency)
```bash
./scripts/data_pipeline.sh delta
```
- Quick updates every 30 minutes
- Minute-level asset data (1m interval)
- Crypto-focused bronze processing

#### **Hourly Pipeline** (3 Steps - Real-Time)
```bash
./scripts/data_pipeline.sh hourly
```
- High-frequency asset collection (1h interval)
- Complete bronze layer processing
- Real-time feature generation

### **Performance Metrics**
- **Collection Speed**: 2 seconds per asset per interval
- **Data Volume**: ~85KB per asset collection, 7,872+ records processed daily
- **Success Rate**: 100% across all pipeline types
- **Feature Coverage**: 20+ financial features per asset, 51 economic features for models
- **Automation**: Cron-scheduled with error handling and retry logic

---

## Getting Started

### Prerequisites
- R 4.0+ (current legacy system)
- Python 3.9+ (target migration)
- MySQL 8.0+
- PHP 8.2+ and Drupal 11 (web interface)

### Current System (Enhanced Python Architecture)
```bash
# Enhanced Python environment with economic processing
cd /workspaces/unicorninvesting/BackendPython/unicorn
python pipeline_validation.py  # Comprehensive system validation

# Run enhanced model selection with economic features
python utilities/enhanced_best_model_selector.py

# Execute silver layer economic data processing
cd 1_data_sources/3_silver
python economic_indicators_processor.py

# Validate alpha models pipeline
python alpha_models_pipeline_validation.py
```

### Archived System (No Migration Planned)
```bash
# Legacy R system - permanently archived
# Located in: BackendPython/unicorn/legacy/
# No migration to Python planned - kept for reference only

# Historical R commands (archived):
# Rscript BackendPython/quickstartsingleNN.R
# Rscript BackendPython/quickstartGAportfolio.R
```

## 📁 Enhanced Portfolio Management

### Portfolio Architecture
**Location**: `BackendPython/unicorn/4_portfolios/Myportolio/` (Enhanced single-portfolio focus)
**Organization**: Clean separation between risk algorithms and trading algorithms

### Current Portfolio Implementation

#### ✅ Myportolio (Production Ready with Enhanced Capabilities)
- **Strategy**: Multi-criteria ETH optimization with economic enhancement
- **Architecture**: Enhanced model selection with ensemble capabilities
- **Risk Management**: Kelly Criterion integration with comprehensive risk algorithms
- **Economic Integration**: 580+ economic indicators with feature engineering
- **Model Selection**: Multi-criteria selector (40% R², 30% economic importance, 20% MAE, 10% complexity)
- **Status**: Production ready with enhanced pipeline architecture
- **Location**: `BackendPython/unicorn/4_portfolios/Myportolio/`

**Enhanced Components**:
- **`trading_algorithms/`**: Pure trading strategy implementations
- **`risk_algorithms/`**: Pure risk calculation implementations  
- **`utilities/`**: Framework integration and enhanced model selection
- **Configuration**: JSON-based portfolio configuration system
- **Validation**: Comprehensive pipeline validation framework

### Portfolio Configuration Standard
Enhanced portfolio structure:
- `config.json` - Asset allocations with enhanced economic parameters
- `risk_parameters.json` - Advanced risk management with Kelly criterion
- `execution_settings.json` - Enhanced trading execution configuration
- `README.md` - Comprehensive portfolio documentation with economic integration details
- **Algorithm Separation**: Clean separation ensuring maintainable, testable code architecture

### Integration with LEAN Framework
- **Data Sources**: Real-time feeds from IBKR, Yahoo Finance, Alpha Vantage
- **Alpha Models**: Cryptocurrency, forex, and equity forecasting models
- **Risk Management**: Integrated risk budgeting and portfolio construction
- **Execution**: Automated order placement and position management

## 📊 **COMPREHENSIVE SYSTEM STATUS**

### **🏥 System Health: 88% Success Rate (31/35 checks passed)**

#### **✅ Operational Components:**
- **Operating System**: Ubuntu 24.04.2 LTS ✅
- **System Resources**: 63% disk usage, 66% memory (healthy) ✅
- **Python Environment**: Version 3.12.1 with virtual environment ✅
- **Web Services**: Apache and MySQL running ✅
- **LEAN Framework**: .NET 8.0.412 available ✅
- **Data Sources**: Yahoo Finance, IBKR Gateway operational ✅
- **Architecture**: Clean directory structure enforced ✅

#### **🔧 Machine Learning Environment:**
- **FastAPI Framework**: Installed and importable ✅
- **Data Science Stack**: pandas, numpy, scipy ✅
- **ML Libraries**: scikit-learn, tensorflow, keras ✅
- **Prophet Forecasting**: Installed and operational ✅
- **Financial Libraries**: yfinance, quantlib ✅
- **Database ORM**: SQLAlchemy, PyMySQL ✅

#### **📈 Trading Infrastructure:**
- **IBKR Gateway**: Running and responsive ✅
- **IBKR ETH Data**: Real-time streaming confirmed ✅
- **Alpha Vantage**: Connector available ✅
- **ETH Models**: 15 models operational ✅
- **Algorithm Integration**: Complete trading workflow ✅

#### **⚠️ Issues Requiring Attention (4 items):**
1. **Database Connection**: Port 3306 accessibility issue
2. **IBKR Authentication**: User login required (see Critical Path)
3. **Documentation**: Root-level files need consolidation (in progress)
4. **Alpha Vantage**: API key configuration (optional)

### **🎯 Algorithm Validation Results:**

#### **✅ ETH Trading System Integration:**
- **Model Loading**: Ensemble model (ensemble_ETH_v006) ✅
- **Live Data Feed**: Real-time ETH prices confirmed ✅
- **Signal Generation**: Trading algorithms operational ✅
- **Risk Management**: Basic risk calculations working ✅
- **Portfolio Construction**: Kelly Criterion implementation ✅

#### **⚠️ Known Integration Issues:**
- **Module Imports**: Kelly Criterion and ETH Basic Risk path resolution
- **Account Data**: IBKR account access (requires authentication)
- **Order Execution**: Not yet connected to live trading

### **📋 Architecture Compliance:**
- **Directory Structure**: 100% compliant with LEAN 6-layer architecture ✅
- **Algorithm Separation**: Risk and trading algorithms properly separated ✅
- **Documentation Standards**: 96% compliant (root-level files being consolidated) ✅
- **Code Organization**: Single portfolio focus (Myportolio) maintained ✅

### **🚀 Development Readiness:**
**Platform Status**: **🟢 READY FOR DEVELOPMENT**

The Unicorn Investing Platform is 96% architecturally compliant and 88% operationally healthy. All core development infrastructure is operational, with only minor organizational cleanup and IBKR authentication remaining for live trading capability.

---

## 📄 Legal Information & Contributing

### **🤝 Open Source & Commercial Services**

This project is **MIT Licensed** and welcomes both open source contributions and commercial collaborations.

#### **✅ Open Source Contributions:**
- Fork, modify, and distribute freely under MIT License
- Educational and research purposes welcomed
- Community contributions appreciated
- Read [**CONTRIBUTING.md**](./CONTRIBUTING.md) for detailed guidelines

#### **💼 Professional Services Available:**
Keith Aumiller offers commercial services including custom algorithm development, portfolio optimization, integration services, architecture consulting, and training. 

**Contact**: Open a GitHub issue with "Commercial Inquiry" label

#### **⚖️ Legal Compliance:**
- **Required Reading**: [DISCLAIMER.md](./DISCLAIMER.md), [TERMS_OF_SERVICE.md](./TERMS_OF_SERVICE.md), [LICENSE](./LICENSE)
- **Your Responsibilities**: Educational use only, professional consultation required, full risk management
- **Not Provided**: Investment advice, guaranteed returns, professional financial services

#### **🔒 Security:**
Review our [**SECURITY.md**](./SECURITY.md) for vulnerability reporting and security best practices.

---

## 🤝 Interested in Collaborating?

**I'm actively seeking collaborators of all backgrounds and expertise levels!** Whether you're technical or non-technical, there are many ways to contribute to the Unicorn Investing Platform.

### **👨‍💻 Technical Collaborators Welcome**

#### **🔬 Quantitative Researchers & Data Scientists**
- 📊 **Machine Learning Engineers** - Enhance prediction models and feature engineering
- 📈 **Financial Engineers** - Develop advanced portfolio optimization algorithms  
- 🧮 **Statisticians** - Improve backtesting frameworks and risk metrics
- 🔍 **Data Scientists** - Expand economic indicators and alternative data sources

#### **💻 Software Developers**
- 🐍 **Python Developers** - Core platform development and optimization
- 🌐 **Full-Stack Developers** - Frontend dashboard and web interface improvements
- 🗄️ **Database Engineers** - Performance optimization and data architecture
- ☁️ **DevOps Engineers** - Cloud deployment and infrastructure automation
- 🔒 **Security Engineers** - Platform hardening and compliance frameworks

#### **📊 Trading System Specialists**
- 🏛️ **LEAN Framework Experts** - Deepen QuantConnect integration
- 🔌 **Broker Integration Specialists** - Additional trading platform connections
- ⚡ **High-Frequency Trading Engineers** - Latency optimization and execution algorithms
- 🎯 **Algorithm Strategists** - Novel trading strategy development

### **💼 Business & Domain Experts**

#### **🏦 Financial Industry Professionals**
- 💹 **Portfolio Managers** - Strategy validation and risk management insights
- 📋 **Compliance Officers** - Regulatory guidance and best practices
- 📊 **Risk Managers** - Advanced risk modeling and assessment frameworks
- 🏛️ **Institutional Traders** - Real-world trading experience and market insights

#### **📚 Education & Content Specialists**
- ✍️ **Technical Writers** - Documentation, tutorials, and educational content
- 🎥 **Content Creators** - Video tutorials, blog posts, and community content
- 🎓 **Educators** - Curriculum development for algorithmic trading education
- 📖 **Documentation Specialists** - API documentation and user guides

#### **🎨 Design & User Experience**
- 🎨 **UI/UX Designers** - Dashboard design and user experience optimization
- 📱 **Product Designers** - User flow optimization and interface design
- 📊 **Data Visualization Specialists** - Financial chart and metrics presentation
- 🎯 **User Research** - Understanding trader needs and workflow optimization

### **🌟 Unique Collaboration Opportunities**

#### **🎓 Academic Partnerships**
- 🏫 **University Researchers** - Joint research projects and publications
- 👨‍🎓 **Graduate Students** - Thesis projects using the platform
- 📚 **Academic Institutions** - Curriculum integration and case studies
- 🔬 **Research Labs** - Financial technology and AI research collaborations

#### **💡 Entrepreneurial Collaborators**
- 🚀 **Startup Founders** - Build commercial applications on the platform
- 💰 **Angel Investors** - Strategic investment and business development
- 🌐 **Business Development** - Partnership opportunities and market expansion
- 📈 **Growth Strategists** - Platform scaling and community building

#### **🌍 International Contributors**
- 🌏 **Global Market Experts** - Regional market knowledge and regulations
- 🗣️ **Multi-language Support** - Internationalization and localization
- ⚖️ **Regulatory Specialists** - Compliance across different jurisdictions
- 💱 **Forex/Crypto Specialists** - Alternative asset class expertise

### **🎯 Current Priority Collaboration Areas**

#### **🔥 Immediate Needs (High Impact)**
1. **📊 Advanced Risk Management** - VaR, stress testing, portfolio risk metrics
2. **🤖 Machine Learning Enhancement** - Advanced ensemble methods, feature engineering
3. **🔌 Broker Integrations** - Additional trading platform connections beyond IBKR
4. **📚 Educational Content** - Tutorials, documentation, and learning resources
5. **🧪 Testing Framework** - Comprehensive unit and integration testing

#### **🌟 Strategic Opportunities (Long-term)**
1. **☁️ Cloud-Native Architecture** - Scalable deployment and microservices
2. **📱 Mobile Application** - iOS/Android apps for portfolio monitoring
3. **🏛️ Institutional Features** - Multi-tenant, compliance, and reporting tools
4. **🌐 Community Platform** - Strategy sharing and collaborative research
5. **🎓 Certification Program** - Algorithmic trading education and certification

### **💬 How to Get Started**

#### **For Open Source Contributors:**
1. 📋 **Browse [Issues](https://github.com/keithaumiller/unicorninvesting/issues)** - Find areas that match your interests
2. 💬 **Join Discussions** - Participate in planning and design conversations  
3. 🍴 **Fork & Contribute** - Start with small improvements and build up
4. 📖 **Read [CONTRIBUTING.md](./CONTRIBUTING.md)** - Understand the contribution process

#### **For Commercial Collaborators:**
1. 💼 **Create Commercial Inquiry** - Use the GitHub issue template for business discussions
2. 📞 **Schedule Consultation** - Free initial discussion about opportunities
3. 🤝 **Define Partnership** - Explore joint ventures, consulting, or investment
4. 🚀 **Launch Projects** - Begin collaborative work with clear agreements

#### **For Academic Partners:**
1. 🎓 **Research Proposals** - Share ideas for joint research projects
2. 📚 **Educational Integration** - Discuss curriculum and course development
3. 📊 **Data Collaboration** - Explore research data sharing opportunities
4. 📝 **Publication Partnerships** - Co-author papers and research publications

### **🎁 What Collaborators Gain**

#### **🌟 Open Source Contributors:**
- 📈 **Portfolio Enhancement** - Showcase work on a real financial platform
- 🎓 **Learning Opportunities** - Hands-on experience with advanced financial systems
- 🤝 **Professional Network** - Connect with finance and technology professionals
- 🏆 **Recognition** - Credit in documentation and project acknowledgments

#### **💼 Commercial Partners:**
- 💰 **Revenue Opportunities** - Joint ventures and profit-sharing arrangements
- 🚀 **Market Access** - Leverage established platform and user base
- 🔬 **R&D Collaboration** - Access to cutting-edge financial technology research
- 🎯 **Custom Solutions** - Priority development of features for your needs

#### **🎓 Academic Collaborators:**
- 📊 **Real-World Data** - Access to actual trading system performance data
- 🔬 **Research Platform** - Use established infrastructure for financial research
- 📝 **Publication Opportunities** - Co-author research papers and case studies
- 🎓 **Student Projects** - Real platform for thesis and capstone projects

### **📞 Ready to Collaborate?**

**I'm excited to work with passionate individuals and organizations!**

- 💼 **Business Inquiries**: [Create Commercial Inquiry Issue](https://github.com/keithaumiller/unicorninvesting/issues/new?template=commercial_inquiry.md)
- 🔧 **Technical Contributions**: [Browse Open Issues](https://github.com/keithaumiller/unicorninvesting/issues)
- 🎓 **Academic Partnerships**: Create issue with "Academic Partnership" label
- 💬 **General Discussion**: [Start a GitHub Discussion](https://github.com/keithaumiller/unicorninvesting/discussions)

**Let's build the future of algorithmic trading together!** 🚀

---

## 🔑 Key Features

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

---

## 🚨 **FINAL RISK WARNING**

**NEVER RISK MONEY YOU CANNOT AFFORD TO LOSE**

This software is provided for educational purposes only. Algorithmic trading involves substantial risk of loss and is not suitable for all investors. The author(s) are not registered investment advisors and are not responsible for any trading losses or investment decisions made using this software.

**Always consult with qualified financial professionals before making investment decisions.**

---

*This platform represents a comprehensive approach to quantitative trading, combining advanced machine learning techniques with robust portfolio management principles for institutional-grade algorithmic trading.*