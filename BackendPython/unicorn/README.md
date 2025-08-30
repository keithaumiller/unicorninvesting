# Unicorn Investing - LEAN Algorithm Framework

## 🏗️ Architecture Overview

This directory implements the **LEAN Algorithm Framework** with clean separation of concerns across six core architectural layers. Each layer has a specific responsibility and operates independently within the framework.

## 📁 Directory Structure

```
unicorn/
├── 1_data_sources/         # 📡 Data Ingestion & Source Management
│   ├── 1_raw/              # Raw data connectors (IBKR, Yahoo Finance, Alpha Vantage)
│   ├── 2_bronze/           # Initial data validation and basic transformations
│   ├── 3_silver/           # Cleaned and enriched datasets
│   ├── 4_gold/             # Analytics-ready data marts
│   ├── 5_data_marts/       # Business logic and aggregated views
│   └── 6_etl_pipelines/    # Extract, Transform, Load processes
├── 2_alpha_models/         # 📊 Signal Generation & Forecasting
├── 3_risk_management/      # 🛡️ Risk Controls & Management
├── 4_portfolio_construction/ # 🎯 Position Sizing & Allocation  
├── 5_execution_models/     # ⚡ Order Placement & Execution
├── 6_algorithms/           # 🚀 Complete Algorithm Implementations
├── config/                 # ⚙️ Configuration files and settings
├── backend/                # 🔧 Backend API services and utilities
├── framework/              # 🏗️ Core framework components
├── legacy/                 # 📦 Legacy R & WPF Code (Archive)
├── README.md              # 📋 This Architecture Guide
└── ARCHITECTURE.md        # 🏗️ Technical Implementation Details
```

## 🎯 LEAN Framework - Six Core Layers

### 1. 📡 Data Sources (`1_data_sources/`)
**Purpose**: Comprehensive data management and ingestion pipeline
- **Raw Layer**: Direct integrations with IBKR, Yahoo Finance, Alpha Vantage
- **Bronze Layer**: Initial data validation and basic transformations
- **Silver Layer**: Cleaned and enriched datasets
- **Gold Layer**: Analytics-ready data marts
- **Data Marts**: Business logic and aggregated views
- **ETL Pipelines**: Automated data processing workflows

**Current Integrations**:
- **Interactive Brokers (IBKR)** - Live trading and market data ✅ **OPTIMIZED & TESTED**
  - Contract ID: 541686654 (ETH)
  - 1000+ minute bars per request, 0-second latency
  - Professional-grade ZEROHASH exchange data
  - ✅ Comprehensive testing framework (100+ tests)
- Yahoo Finance - Free historical and real-time data
- Alpha Vantage - Financial data API integration

**🧪 Testing Infrastructure**: 
- ✅ **Integration Tests**: IBKR connectivity and data collection validation
- ✅ **Technical Indicators**: 30+ indicators with 80+ validation tests
- ✅ **Data Quality**: Comprehensive scoring (95%+ completeness target)
- ✅ **Performance**: <2000ms E2E latency, >20 points/sec throughput
- ✅ **Stress Testing**: 1000+ data points under load

### 2. 📊 Alpha Models (`2_alpha_models/`)
**Purpose**: Pure forecasting and signal generation
- **Input**: Market data, economic indicators, alternative data
- **Output**: Insights (buy/sell signals with confidence and time horizon)
- **Responsibility**: Generate trading signals WITHOUT making trading decisions

**Current Implementations**:
- `AdvancedForexForecastingAlpha.py` - Multi-model ensemble (ARIMA + Neural Networks + Prophet + XGBoost)
- `EthFocusedAlpha.py` - Technical analysis for ETH (SMA + RSI + Bollinger Bands)
- `predictiveanalytics/` - Advanced ML forecasting models
- `recomendationsystems/` - Recommendation engine components

### 3. 🛡️ Risk Management (`3_risk_management/`)
**Purpose**: Monitor and control portfolio risk
- **Input**: Current portfolio state and market conditions
- **Output**: Risk management targets (stop losses, position limits)
- **Responsibility**: Protect capital and enforce risk limits

**Current Implementations**:
- `UnicornRiskManagement.py` - Core risk management models
- `blotterscripts/` - Trade blotter and risk monitoring scripts

### 4. 🎯 Portfolio Construction (`4_portfolio_construction/`)
**Purpose**: Convert insights into target portfolio weights
- **Input**: Insights from Alpha Models
- **Output**: PortfolioTarget objects (how much of each asset to hold)
- **Responsibility**: Position sizing and portfolio allocation decisions

**Current Implementations**:
- `UnicornRiskIntegratedPortfolioConstruction.py` - Integrated risk-portfolio framework
- Risk budgeting as foundation for portfolio construction

**📁 Portfolio Management**: Individual portfolios are managed in the `portfolios/` subdirectory:
- **Structure**: Each portfolio in its own directory with configuration files
- **Location**: `BackendPython/unicorn/4_portfolios/portfolios/`
- **Available Portfolios**:
  - ✅ `ETH_Only/` - 100% Ethereum allocation (Ready for deployment)
  - 📋 `BTC_ETH_Mixed/` - Balanced BTC/ETH portfolio (Planned)
  - 📋 `Multi_Asset/` - Diversified portfolio (Planned)
- **Configuration**: Each portfolio contains:
  - `config.json` - Asset allocations and strategy parameters
  - `risk_parameters.json` - Risk management settings
  - `execution_settings.json` - Trading execution configuration
  - `README.md` - Portfolio-specific documentation

### 5. ⚡ Execution Models (`5_execution_models/`)
**Purpose**: Convert portfolio targets into actual orders
- **Input**: PortfolioTarget objects from Portfolio Construction
**Purpose**: Convert portfolio targets into actual orders
- **Input**: PortfolioTarget objects from Portfolio Construction
- **Output**: Market orders, limit orders, stop orders
- **Responsibility**: Order placement and execution timing

**Current Implementations**:
- `integrations/` - Broker and exchange integrations
- `deployment/` - Live trading deployment configurations
- **Future**: Custom execution for slippage optimization, VWAP strategies

### 6.  Complete Algorithms (`6_algorithms/`)

Pre-built algorithms that combine all framework components:

- `AdvancedForexFrameworkAlgorithm.py` - Multi-currency ensemble forecasting
- `YahooFinanceForexFrameworkAlgorithm.py` - Free Yahoo Finance forex data
- `EthFrameworkAlgorithm.py` - ETH-focused trading algorithms
- `MinuteLevelDataIntegrationAlgorithm.py` - High-frequency data processing
- Algorithm validation and testing scripts

## ⚙️ Configuration (`config/`)

System configuration and settings:
- `eth_portfolio_config.json` - ETH portfolio configuration
- `ibkr/` - Interactive Brokers configuration and credentials
- Environment and API configuration files

## 🔧 Backend Services (`backend/`)

API services and backend utilities:
- `api/` - REST API endpoints
- `ml/` - Machine learning service components
- `models/` - Data models and schemas
- `services/` - Business logic services
- `utils/` - Shared utilities and helpers

## 🏗️ Framework (`framework/`)

Core framework components and abstractions:
- `UnicornFrameworkAlgorithms.py` - Base framework algorithms
- `alphas/` - Alpha model implementations
- `portfolio/` - Portfolio construction components
- `risk/` - Risk management components

## 📦 Legacy Code (`legacy/`)

Archived code from previous implementations:
- `wpf-app/` - Legacy WPF desktop application
- `*.R` files - Legacy R statistical analysis scripts
- Historical documentation and project files

## 🎯 Framework Benefits

1. **Modularity**: Swap components independently
2. **Testability**: Test each component in isolation
3. **Reusability**: Use same Alpha with different Portfolio models
4. **Maintainability**: Clear separation makes debugging easier
5. **Scalability**: Add new strategies without affecting existing ones

## 🔧 Development Guidelines

### 1. **Separation of Concerns**
- **Alpha Models**: ONLY generate forecasts/insights
- **Portfolio Models**: ONLY determine position sizes
- **Execution Models**: ONLY place and manage orders
- **Risk Models**: ONLY manage risk and protection

### 2. **Component Independence**
- Each component should work independently
- No direct dependencies between components
- Communication only through LEAN framework interfaces

### 3. **Adding New Components**

**New Alpha Model**:
```python
from AlgorithmImports import *

class MyNewAlpha(AlphaModel):
    def update(self, algorithm, data):
        # Generate insights only
        return [Insight.price(symbol, timedelta(hours=1), InsightDirection.UP)]
```

**New Portfolio Model**:
```python
from AlgorithmImports import *

class MyNewPortfolio(PortfolioConstructionModel):
    def create_targets(self, algorithm, insights):
        # Convert insights to portfolio targets
        return [PortfolioTarget(symbol, weight)]
```

### 4. **Testing Strategy**
```python
# Test forecasting accuracy separately
alpha_model.test_forecast_accuracy(historical_data)

# Test position sizing separately  
portfolio_model.test_allocation_strategy(mock_insights)

# Test risk controls separately
risk_model.test_stop_loss_trigger(portfolio_state)
```

## 🚀 Quick Start

### 1. **Run a Complete Algorithm**
```python
# Use pre-built framework algorithm
algorithm = AdvancedForexFrameworkAlgorithm()
algorithm.run_backtest(start_date, end_date)
```

### 2. **Test Individual Components**
```python
# Test Alpha Model forecasting
alpha = AdvancedForexForecastingAlpha()
insights = alpha.update(algorithm, data)

# Test Portfolio Construction
portfolio = UnicornConfidenceWeightedPortfolioConstruction()
targets = portfolio.create_targets(algorithm, insights)
```

### 3. **Use Free Data Sources**
```python
# Yahoo Finance (no API key required)
from data_sources.YahooFinanceMinuteData import YahooFinanceForexData
self.add_data(YahooFinanceForexData, "EURUSD", Resolution.MINUTE)
```

## 🚨 Important Notes

- **No Trading Logic in Alpha Models**: They only generate forecasts
- **No Forecasting in Portfolio Models**: They only determine position sizes
- **Framework Orchestrates Everything**: LEAN handles the flow between components
- **Risk Management is Always Active**: Risk models run continuously

## 📚 Documentation

- **ARCHITECTURE.md**: Technical implementation details
- **Component READMEs**: Detailed documentation in each component directory
- **Algorithm Examples**: Working examples in `algorithms/` directory

## 🦄 About Unicorn Investing

This platform provides institutional-grade algorithmic trading strategies with:
- **Advanced ML Forecasting**: ARIMA + Neural Networks + Prophet + XGBoost
- **Risk-Managed Portfolio Construction**: Kelly criterion and confidence weighting
- **Multi-Asset Support**: Forex, crypto, stocks, ETFs
- **Free Data Integration**: Yahoo Finance, Alpha Vantage, and more
- **Clean Architecture**: LEAN framework best practices

---

*This architecture follows institutional trading best practices and ensures clean, maintainable, and testable algorithmic trading strategies.*
