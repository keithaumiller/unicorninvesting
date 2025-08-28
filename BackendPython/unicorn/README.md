# Unicorn Investing - LEAN Algorithm Framework

## 🏗️ Architecture Overview

This directory implements the **LEAN Algorithm Framework** with clean separation of concerns across four core architectural components. Each component has a specific responsibility and operates independently within the framework.

## 📁 Directory Structure

```
unicorn/
├── alpha_models/           # 📊 Signal Generation & Forecasting
├── portfolio_construction/ # 🎯 Position Sizing & Allocation  
├── execution_models/       # ⚡ Order Placement & Execution
├── risk_management/        # 🛡️ Risk Controls & Management
├── algorithms/             # 🚀 Complete Algorithm Implementations
├── data_sources/           # 📡 Custom Data Providers & Sources
├── utils/                  # 🔧 Shared Utilities & Configuration
├── legacy/                 # 📦 Legacy R & WPF Code (Archive)
├── README.md              # 📋 This Architecture Guide
└── ARCHITECTURE.md        # 🏗️ Technical Implementation Details
```

## 🎯 LEAN Framework - Four Core Components

### 1. 📊 Alpha Models (`alpha_models/`)
**Purpose**: Pure forecasting and signal generation
- **Input**: Market data, economic indicators, alternative data
- **Output**: Insights (buy/sell signals with confidence and time horizon)
- **Responsibility**: Generate trading signals WITHOUT making trading decisions

**Current Implementations**:
- `AdvancedForexForecastingAlpha.py` - Multi-model ensemble (ARIMA + Neural Networks + Prophet + XGBoost)
- `EthFocusedAlpha.py` - Technical analysis for ETH (SMA + RSI + Bollinger Bands)
- `predictiveanalytics/` - Advanced ML forecasting models
- `recomendationsystems/` - Recommendation engine components

### 2. 🎯 Portfolio Construction (`portfolio_construction/`)
**Purpose**: Convert insights into target portfolio weights
- **Input**: Insights from Alpha Models
- **Output**: PortfolioTarget objects (how much of each asset to hold)
- **Responsibility**: Position sizing and portfolio allocation decisions

**Current Implementations**:
- `UnicornPortfolioConstruction.py` - Core portfolio construction models
- `batchjobs/` - Batch portfolio optimization processes

### 3. ⚡ Execution Models (`execution_models/`)
**Purpose**: Convert portfolio targets into actual orders
- **Input**: PortfolioTarget objects from Portfolio Construction
- **Output**: Market orders, limit orders, stop orders
- **Responsibility**: Order placement and execution timing

**Current Implementations**:
- `integrations/` - Broker and exchange integrations
- `deployment/` - Live trading deployment configurations
- **Future**: Custom execution for slippage optimization, VWAP strategies

### 4. 🛡️ Risk Management (`risk_management/`)
**Purpose**: Monitor and control portfolio risk
- **Input**: Current portfolio state and market conditions
- **Output**: Risk management targets (stop losses, position limits)
- **Responsibility**: Protect capital and enforce risk limits

**Current Implementations**:
- `UnicornRiskManagement.py` - Core risk management models
- `blotterscripts/` - Trade blotter and risk monitoring scripts

## 🚀 Complete Algorithms (`algorithms/`)

Pre-built algorithms that combine all four components:

- `AdvancedForexFrameworkAlgorithm.py` - Multi-currency ensemble forecasting
- `YahooFinanceForexFrameworkAlgorithm.py` - Free Yahoo Finance forex data
- `test_forex_algorithm.py` - Testing and validation scripts
- Legacy algorithms for reference and migration

## 📡 Data Sources (`data_sources/`)

Custom data providers and integrations:
- `YahooFinanceMinuteData.py` - Free Yahoo Finance integration (no API key)
- `AlphaVantageMinuteData.py` - Alpha Vantage integration
- `data/` - Historical and reference data
- `database/` - Database connections and schemas
- `datagathering/` - Data collection processes
- `datasetcreation/` - Dataset preparation pipelines

## 🔧 Utilities & Support (`utils/`)

Shared utilities and configuration:
- `backend/` - Backend service components
- `config/` - Configuration files and settings
- `scripts/` - Utility and deployment scripts
- `tests/` - Unit and integration tests
- `backtesting/` - Backtesting frameworks and results
- `results/` - Analysis results and reports
- `docs/` - Additional documentation

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
