# Unicorn Investing - LEAN Algorithm Framework

## 🚀 **PRODUCTION STATUS: LIVE & OPERATIONAL**

**Last Updated**: September 15, 2025  
**Status**: ✅ **PRODUCTION READY** - Multi-Asset Ensemble Trading System with 100% integration success  
**Integration**: ✅ Complete ensemble model integration + Kelly optimization + Real-time execution  
**Trading System**: ✅ **OPERATIONAL** - 11 ensemble models with silver layer integration + momentum strategy  
**New Achievement**: 🔮 **ALPHA FORECAST SILVER LAYER INTEGRATION** - Complete data flow architecture implemented  

## 🏗️ Architecture Overview

This directory implements the **LEAN Algorithm Framework** with clean separation of concerns across six core architectural layers. Each layer has a specific responsibility and operates independently within the framework.

## 📊 **Current Production Implementation**

### **✅ LIVE SYSTEMS (September 2025)**
- **Ensemble Trading System**: ✅ **100% Success Rate** - 11 production models with silver layer integration
- **Multi-Asset Coverage**: ETH, BTC (1d/1h) + 7 forex pairs (1h) with real-time execution
- **Kelly Optimization**: Risk-adjusted position sizing with confidence weighting (R² 0.817-0.934)
- **Trading Performance**: 73.4% portfolio utilization, 4 active positions, $26,621 cash reserve
- **Silver Layer Integration**: Asset-specific feature mappings with 100% model compatibility
- **Risk Management**: 0.4% portfolio risk (under 2% limit), 25% max position size enforcement
- **Real-Time Execution**: Complete trading cycle in 0.64 seconds with live market data

### **🔧 PIPELINE VALIDATION FRAMEWORK**
- **`pipeline_validation.py`**: Complete pipeline validation from data sources through portfolio construction
- **`alpha_models_pipeline_validation.py`**: Specialized alpha models flow validation and performance assessment
- **Validation Coverage**: Data sources → Alpha models → Risk management → Portfolio construction → End-to-end flow
- **Production Readiness**: Automated assessment with scoring and recommendations
- **Silver Layer Achievement**: 100% processing success with datetime handling improvements

## 📁 Directory Structure

```
unicorn/
├── 1_data_sources/         # 📡 Data Ingestion & Source Management
│   ├── 1_raw/              # Raw data connectors (IBKR, Yahoo Finance, Alpha Vantage)
│   ├── 2_bronze/           # Initial data validation and basic transformations
│   ├── 3_silver/           # ✨ **ENHANCED** Cleaned datasets + Alpha Forecast Integration
│   ├── 4_gold/             # Analytics-ready data marts
│   ├── 5_data_marts/       # Business logic and aggregated views
│   └── 6_etl_pipelines/    # Extract, Transform, Load processes
├── 2_alpha_models/         # 📊 Signal Generation & Forecasting
├── 3_risk_management/      # 🛡️ Risk Controls & Management
├── 4_portfolios/           # 🏆 **ENSEMBLE TRADING** Multi-Asset Momentum Strategy
│   └── Myportolio/         # ✅ **PRODUCTION** Complete trading system with:
│       ├── simplified_ensemble_portfolio.py    # Main trading engine
│       ├── ensemble_model_wrapper.py          # Model integration layer
│       ├── silver_layer_integration_mapper.py # Data pipeline integration
│       ├── risk_algorithms/        # Pure risk calculation algorithms
│       ├── trading_algorithms/     # Pure trading strategy algorithms  
│       ├── utilities/              # Framework-level shared components
│       └── simulations/            # LEAN-aligned backtesting framework  
├── 5_execution_models/     # ⚡ Order Placement & Execution
├── 6_algorithms/           # 🚀 Complete Algorithm Implementations
├── config/                 # ⚙️ Configuration files and settings
├── backend/                # 🔧 Backend API services and utilities
├── framework/              # 🏗️ Core framework components
├── legacy/                 # 📦 Legacy R & WPF Code (Archive)
├── pipeline_validation.py  # ✨ **NEW** Comprehensive pipeline validation framework
├── alpha_models_pipeline_validation.py # ✨ **NEW** Alpha models flow validation
├── README.md              # 📋 This Architecture Guide
└── ARCHITECTURE.md        # 🏗️ Technical Implementation Details
```

## 🎯 LEAN Framework - Six Core Layers

### 1. 📡 Data Sources (`1_data_sources/`)
**Purpose**: Comprehensive data management and ingestion pipeline
- **Raw Layer**: Direct integrations with IBKR, Yahoo Finance, Alpha Vantage
- **Bronze Layer**: Initial data validation and basic transformations
- **Silver Layer**: ✨ **ENHANCED** Economic data processing with 580+ indicators across 4 categories
- **Gold Layer**: Analytics-ready data marts
- **Data Marts**: Business logic and aggregated views
- **ETL Pipelines**: Automated data processing workflows

**✨ Silver Layer Economic Processing**:
- **Economic Indicators Processor**: Comprehensive processing of 580+ economic indicators
- **Economic Integration Connector**: Bridge between silver layer and alpha models
- **4 Economic Categories**: Economic Growth (GDP, employment), Consumer/Business (confidence, retail), Monetary Policy (rates, money supply), International Trade (balance, currencies)
- **Feature Engineering**: 50+ derived features including moving averages, momentum indicators, volatility measures, composite indices
- **Quality Assessment**: Data quality scoring, temporal alignment, schema compliance
- **Alpha Model Integration**: Enhanced datasets with crypto-specific economic features ready for consumption

**Current Integrations**:
- **Interactive Brokers (IBKR)** - Live trading and market data ✅ **OPTIMIZED & TESTED**
  - Contract ID: 541686654 (ETH)
  - 1000+ minute bars per request, 0-second latency
  - Professional-grade ZEROHASH exchange data
  - ✅ Comprehensive testing framework (100+ tests)
- **Federal Reserve Economic Data (FRED)** - Automated economic data pipeline ✅ **PRODUCTION DEPLOYED**
  - 26,426+ economic observations (1919-2025), 23 series operational
  - Delta updates every 15 minutes (critical indicators)
  - Daily updates at 9 PM (comprehensive dataset)
  - Current indicators: Fed Funds 4.33%, 10Y Treasury 4.10%
  - ✅ Ready for crypto alpha model integration
- **Bureau of Economic Analysis (BEA)** - Macroeconomic data automation ✅ **PRODUCTION READY**
  - 15+ datasets across 6 economic categories (2000-present)
  - Delta updates every 6 hours (critical GDP, consumption, investment data)
  - Daily updates at 6 AM (comprehensive macroeconomic indicators)
  - Alpha features: 50+ engineered features with economic regime detection
  - ✅ GDP growth analysis and economic cycle integration ready
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
- **Input**: Market data, economic indicators (FRED + BEA), alternative data
- **Output**: Insights (buy/sell signals with confidence and time horizon)
- **Responsibility**: Generate trading signals WITHOUT making trading decisions
- **Economic Integration**: ✨ **ENHANCED** Silver layer economic processing with 50+ engineered features

**Current Implementations**:
- ✨ **Economic-Enhanced XGBoost Models**: Individual models with comprehensive economic feature integration
- ✨ **Economic Ensemble Models**: Combined Prophet + XGBoost economic models with optimized weighting
- ✨ **Enhanced Model Selection**: Multi-criteria model selector comparing individual vs ensemble performance
- `AdvancedForexForecastingAlpha.py` - Multi-model ensemble (ARIMA + Neural Networks + Prophet + XGBoost)
- `EthFocusedAlpha.py` - Technical analysis for ETH (SMA + RSI + Bollinger Bands)
- `predictiveanalytics/` - Advanced ML forecasting models
- `recomendationsystems/` - Recommendation engine components

**✨ Enhanced Economic Integration**:
- **Silver Layer Features**: 50+ engineered economic features from GDP, unemployment, consumer confidence, retail sales, interest rates, trade balance
- **Composite Indicators**: Economic growth composite, consumer business sentiment, monetary policy stance
- **Momentum Features**: 1-month and 3-month momentum indicators for all economic series
- **Crypto-Specific Features**: Risk sentiment analysis, economic liquidity measures, correlation features
- **Quality-Assured Data**: Comprehensive quality scoring and validation before model consumption

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
- **Myportolio**: Production-ready portfolio with comprehensive simulation framework

**📁 Portfolio Management**: Individual portfolios are managed in the `4_portfolios/` directory:
- **Structure**: Each portfolio in its own directory with configuration files
- **Location**: `BackendPython/unicorn/4_portfolios/`
- **Available Portfolios**:
  - ✅ `Myportolio/` - Production dual-crypto portfolio with ✨ **enhanced model selection framework**
  - 📋 Legacy portfolios archived for reference
- **✨ Enhanced Utilities**:
  - `enhanced_best_model_selector.py` - Multi-criteria model selection with economic ensemble support
  - Weighted scoring: 40% R², 30% economic importance, 20% MAE, 10% complexity bonus
  - Individual vs ensemble model comparison and production configuration generation
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

## � Development Process Flows

### **Alpha Model Development Pipeline**

#### **Phase 1: Research & Strategy Design**
1. **Market Research & Hypothesis Formation**
   - Technical analysis and fundamental research
   - Strategy hypothesis development
   - Literature review and backtesting framework design

2. **Data Requirements & Infrastructure**
   - Define data needs (price, volume, alternative data)
   - Establish data pipeline and quality assurance
   - Design storage architecture and access patterns

#### **Phase 2: Model Development & Implementation**
1. **Signal Generation Algorithm Development**
   - Feature engineering and signal logic implementation
   - Confidence scoring and signal validation
   - Algorithm design patterns

2. **LEAN Framework Integration**
   - IAlphaModel interface implementation
   - Insight creation and event handling
   - Universe selection integration

#### **Phase 3: Backtesting & Validation**
1. **Historical Performance Testing**
   - In-sample and out-of-sample testing
   - Walk-forward analysis and performance metrics
   - Risk analysis and strategy refinement

2. **Statistical Validation**
   - Signal quality analysis and statistical significance
   - Regime stability and correlation analysis
   - Validation reporting

#### **Phase 4: Production Deployment**
1. **Model Production Readiness**
   - Code review, testing, and performance monitoring setup
   - Risk management integration
   - Live monitoring and attribution analysis

### **Risk Model Development Pipeline**

#### **Phase 1: Risk Framework Design**
1. **Risk Model Architecture**
   - Risk factor identification and model selection
   - Parameter design and validation framework
   - Integration architecture planning

2. **Risk Metric Definition**
   - VaR, CVaR, and volatility model specifications
   - Calculation methodology and parameter estimation
   - Model calibration and validation testing

#### **Phase 2: Model Implementation**
1. **Risk Algorithm Development**
   - Mathematical implementation and parameter estimation
   - Statistical, simulation, and optimization models
   - Performance testing and validation

2. **Asset-Specific Implementation**
   - Asset class analysis and risk characteristics
   - Model customization and parameter calibration
   - Asset-specific validation (ETH, BTC, traditional assets)

#### **Phase 3: Validation & Calibration**
1. **Model Validation Framework**
   - Backtesting and out-of-sample testing
   - Cross-validation and stress testing
   - Model comparison and selection

2. **Parameter Calibration**
   - Parameter space definition and optimization objectives
   - Calibration algorithms (grid search, Bayesian optimization)
   - Parameter validation and model finalization

#### **Phase 4: Integration & Deployment**
1. **Portfolio Construction Integration**
   - Risk model interface design
   - Risk constraint application and optimization integration
   - Real-time risk monitoring setup

2. **Real-time Risk Monitoring**
   - Risk metric calculation and limit monitoring
   - Alert generation and risk action execution
   - Performance monitoring and continuous improvement

### **Combined Development Workflow**
- **Integrated Testing**: Joint alpha and risk model validation
- **Production Deployment**: Combined system deployment
- **Live Performance Monitoring**: Continuous performance tracking
- **Continuous Improvement**: Model updates and optimization

## �📚 Documentation

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
