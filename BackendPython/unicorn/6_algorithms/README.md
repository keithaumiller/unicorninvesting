# Complete Algorithms - LEAN Framework Implementations

## 🚀 Purpose

This directory contains **complete trading algorithms** that use all four LEAN Algorithm Framework components together. These are production-ready algorithms that can be run directly in LEAN.

## 🏗️ Framework Architecture

Each algorithm in this directory follows the LEAN Algorithm Framework pattern:

1. **📊 Alpha Model** - Signal generation (references `alpha_models/`)
2. **🎯 Portfolio Construction** - Position sizing (references `portfolio_construction/`)
3. **⚡ Execution Model** - Order placement (uses LEAN defaults or `execution_models/`)
4. **🛡️ Risk Management** - Risk controls (references `risk_management/`)

## 📁 Current Algorithms

### 🌐 **Yahoo Finance Algorithms** (Free Data Sources)
- `YahooFinanceForexFrameworkAlgorithm.py` - Forex trading with free Yahoo Finance data
- `YahooFinanceMinuteAlgorithm.py` - Multi-asset minute-level trading

### 💱 **Forex Algorithms**
- `AdvancedForexFrameworkAlgorithm.py` - Multi-model ensemble forex trading
- `EnhancedEnsembleForexAlgorithm.py` - Enhanced ensemble with 4 forecasting methods

### 🪙 **Crypto Algorithms**
- `EthFrameworkAlgorithm.py` - ETH-focused framework algorithm
- `EthOnlyPortfolio.py` - Simple ETH-only portfolio ($1,000 capital)

### 📊 **Data Integration Algorithms**
- `MinuteLevelDataIntegrationAlgorithm.py` - Demonstrates minute-level data integration

## 🎯 Algorithm Template

All algorithms in this directory should follow this structure:

```python
from AlgorithmImports import *

# Import Alpha Models
from alpha_models.AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha

# Import Portfolio Construction  
from portfolio_construction.UnicornPortfolioConstruction import UnicornConfidenceWeightedPortfolioConstruction

# Import Risk Management
from risk_management.UnicornRiskManagement import UnicornForexRiskManagement

class MyFrameworkAlgorithm(QCAlgorithm):
    def initialize(self):
        # Basic setup
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(100000)
        
        # 1. Universe Selection
        symbols = [Symbol.create("EURUSD", SecurityType.FOREX, Market.OANDA)]
        self.set_universe_selection(ManualUniverseSelectionModel(symbols))
        
        # 2. Alpha Model (Forecasting)
        self.set_alpha(AdvancedForexForecastingAlpha())
        
        # 3. Portfolio Construction (Position Sizing)
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction())
        
        # 4. Execution Model (Order Placement)
        self.set_execution(ImmediateExecutionModel())
        
        # 5. Risk Management (Risk Controls)
        self.set_risk_management(UnicornForexRiskManagement())
```

## ✅ What Belongs Here

### **Complete Algorithms**
- Files that combine all 4 framework components
- Files that inherit from `QCAlgorithm`
- Files that can be run directly in LEAN
- Files that implement complete trading strategies

### **Framework Demonstrations**
- Examples showing how to use the framework
- Integration examples with different data sources
- Performance comparison algorithms

## ❌ What Doesn't Belong Here

### **Pure Forecasting** → Move to `alpha_models/`
- Files focused only on signal generation
- Files that don't use the framework
- Files that are just forecasting models

### **Utilities** → Move to `utils/`
- Test files (`test_*.py`, `test_*.sh`)
- Configuration files (`*.json`)
- Performance analysis scripts
- Documentation files (`*.md`)

### **Partial Components** → Move to appropriate component directory
- Pure Alpha Models → `alpha_models/`
- Pure Portfolio Models → `portfolio_construction/`
- Pure Risk Models → `risk_management/`
- Pure Execution Models → `execution_models/`

## 🧪 Testing Algorithms

### **Backtest Validation**
```python
# Test complete algorithm performance
def test_algorithm_backtest():
    algorithm = AdvancedForexFrameworkAlgorithm()
    algorithm.run_backtest(start_date, end_date)
    
    assert algorithm.portfolio.total_profit > 0
    assert algorithm.portfolio.max_drawdown < 0.15
```

### **Framework Integration**
```python
# Test all components work together
def test_framework_integration():
    algorithm = MyFrameworkAlgorithm()
    
    # Verify all components are set
    assert algorithm.alpha_model is not None
    assert algorithm.portfolio_construction_model is not None
    assert algorithm.risk_management_model is not None
    assert algorithm.execution_model is not None
```

## 🎯 Development Workflow

### **1. Create New Algorithm**
```bash
# Start with template
cp algorithms/template_algorithm.py algorithms/MyNewAlgorithm.py

# Edit components
# - Choose Alpha Model from alpha_models/
# - Choose Portfolio Model from portfolio_construction/
# - Choose Risk Model from risk_management/
# - Choose Execution Model (usually ImmediateExecutionModel)
```

### **2. Test Locally**
```bash
# Run backtests
cd algorithms/
python MyNewAlgorithm.py

# Run unit tests
python -m pytest ../utils/tests/test_algorithms.py
```

### **3. Deploy to LEAN**
```bash
# Upload to LEAN platform or local LEAN instance
lean cloud deploy MyNewAlgorithm.py
```

## 📊 Performance Monitoring

Track these metrics for complete algorithms:

- **Total Return**: Overall profitability
- **Sharpe Ratio**: Risk-adjusted returns
- **Maximum Drawdown**: Worst peak-to-trough loss
- **Win Rate**: Percentage of profitable trades
- **Volatility**: Standard deviation of returns
- **Alpha Generation**: Insights generated per day
- **Execution Quality**: Slippage and timing

## 🌐 Data Source Integration

### **Free Data Sources**
- Yahoo Finance (no API key required)
- IEX Cloud (free tier)
- Alpha Vantage (500 calls/month)

### **Premium Data Sources**
- QuantConnect data feeds
- Interactive Brokers
- Custom data providers

## 🦄 Unicorn Platform Standards

All algorithms follow Unicorn platform conventions:

- Emoji-based logging for clarity
- Comprehensive error handling
- Performance monitoring integration
- Risk-first design philosophy
- Documentation requirements
- Testing standards

## 🎯 Algorithm Categories

### **Production Algorithms**
- Tested and validated strategies
- Real money deployment ready
- Comprehensive risk management
- Performance monitoring

### **Development Algorithms**
- Experimental strategies
- Testing new components
- Framework demonstrations
- Educational examples

### **Reference Algorithms**
- Benchmark implementations
- Strategy comparisons
- Performance baselines
- Framework examples

---

*Complete algorithms demonstrate the power of the LEAN Algorithm Framework by combining specialized components into profitable trading strategies!*
