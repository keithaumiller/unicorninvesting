# Portfolio Construction & Management

## 🏗️ Directory Overview

This directory contains the portfolio construction framework and individual portfolio configurations for the Unicorn Investing platform.

**Location**: `BackendPython/unicorn/4_portfolios/`

## 📁 Directory Structure

```
4_portfolios/
├── README.md                                    # This file - Portfolio framework overview
├── UnicornRiskIntegratedPortfolioConstruction.py # Main portfolio construction framework
├── UnicornPortfolioConstruction.py             # Legacy portfolio construction (being phased out)
├── README_ETH_PORTFOLIO.md                     # Legacy ETH portfolio documentation
├── batchjobs/                                   # Batch portfolio optimization processes
│   ├── Actiontime.r                            # Legacy R scripts
│   ├── Batchscriptmaster.R                     # Legacy R batch processing
│   └── README.md                               # Batch jobs documentation
├── utilities/                                   # 🔧 Global utilities and shared components
│   ├── README.md                               # Utilities documentation
│   ├── portfolio_factory.py                   # Portfolio creation utilities
│   ├── data_connectors.py                     # Data source connectors
│   ├── database_utils.py                      # Database operations
│   └── validation_framework.py                # Portfolio validation tools
└── Myportolio/                                # 🎯 Main Portfolio Implementation
    ├── README.md                               # Portfolio overview and architecture
    ├── config.json                            # Portfolio configuration
    ├── risk_parameters.json                   # Risk management settings
    ├── execution_settings.json                # Trading execution parameters
    ├── risk_algorithms/                       # Pure risk calculation algorithms
    │   ├── README.md                          # Risk algorithms documentation
    │   ├── var_calculator.py                 # Value at Risk calculations
    │   ├── correlation_analyzer.py           # Asset correlation analysis
    │   └── risk_budgeting.py                 # Risk budget allocation
    ├── trading_algorithms/                    # Pure trading strategy algorithms
    │   ├── README.md                          # Trading algorithms documentation
    │   ├── alpha_models.py                   # Signal generation models
    │   ├── portfolio_optimizer.py            # Portfolio optimization algorithms
    │   └── rebalancer.py                     # Rebalancing strategies
    └── shared_utilities/                      # Portfolio-specific shared utilities
        ├── README.md                          # Shared utilities documentation
        ├── data_utils.py                     # Data processing utilities
        ├── config_loader.py                 # Configuration management
        └── performance_tracker.py           # Performance measurement
```

## 🎯 Framework Components

### Portfolio Construction Engine
- **`UnicornRiskIntegratedPortfolioConstruction.py`**: Main framework implementing risk-integrated portfolio construction
  - Risk budgeting as foundation for portfolio allocation
  - Integrated optimization with continuous risk monitoring
  - Dynamic position sizing and allocation decisions
  - Real-time risk assessment and adjustment

### Portfolio Configuration Management
- **Location**: `portfolios/` subdirectory
- **Structure**: Each portfolio is self-contained with complete configuration
- **Standards**: Standardized JSON configuration files for consistency
- **Templates**: Reusable templates for creating new portfolios

## 🔄 Integration with LEAN Framework

### LEAN Layer 4: Portfolio Construction
This directory implements **Layer 4** of the 6-layer LEAN architecture:

1. **Data Sources** (`../1_data_sources/`) → Raw market data
2. **Alpha Models** (`../2_alpha_models/`) → Trading signals and insights
3. **Risk Management** (`../3_risk_management/`) → Risk controls and limits
4. **Portfolio Construction** (`../4_portfolios/`) → **THIS LAYER** - Position sizing and allocation
5. **Execution Models** (`../5_execution_models/`) → Order placement and execution
6. **Algorithms** (`../6_algorithms/`) → Complete trading algorithms

### Framework Flow
```
Alpha Insights → Risk Assessment → Portfolio Construction → Position Targets → Execution Orders
```

## 🚀 Portfolio Management Workflow

### 1. Portfolio Creation
```bash
# Create new portfolio from template
cd BackendPython/unicorn/4_portfolios/portfolios/
cp -r templates New_Portfolio_Name
cd New_Portfolio_Name
# Edit configuration files
```

### Portfolio Validation
```python
# Validate portfolio configuration
from BackendPython.unicorn.4_portfolios import PortfolioValidator
validator = PortfolioValidator("BackendPython/unicorn/4_portfolios/Myportolio")
validation_result = validator.validate_all()
```

### Portfolio Deployment
```python
# Deploy portfolio for live trading
from BackendPython.unicorn.4_portfolios import UnicornRiskIntegratedPortfolioConstruction
portfolio = UnicornRiskIntegratedPortfolioConstruction("BackendPython/unicorn/4_portfolios/Myportolio")
portfolio.deploy()
```

## 📊 Available Portfolios

### ✅ Myportolio (Consolidated Implementation)
- **Strategy**: Configurable portfolio with separated algorithm architecture
- **Structure**: Clear separation between risk algorithms and trading algorithms
- **Status**: Active development with modular framework
- **Configuration**: Complete with risk parameters and execution settings
- **Innovation**: First implementation of separated algorithm architecture

## 🏗️ Algorithm Separation Architecture

### Design Principle
**Myportolio** implements a clean separation between:
- **Risk Algorithms**: Pure risk calculations (VaR, volatility, correlations)
- **Trading Algorithms**: Pure trading strategies (signals, optimization, rebalancing)
- **Shared Utilities**: Common functions used by both algorithm types

### Benefits
- **Maintainability**: Clear boundaries between risk and trading logic
- **Testability**: Each algorithm type can be tested independently
- **Reusability**: Risk algorithms can be used with different trading strategies
- **Scalability**: Easy to add new algorithms without affecting existing ones

## 🔧 Configuration Standards

### Portfolio Configuration Files
Each portfolio contains standardized configuration files:

1. **`config.json`**: Asset allocations, strategy parameters, metadata
2. **`risk_parameters.json`**: Risk limits, VaR targets, stop-loss settings
3. **`execution_settings.json`**: Order types, slippage tolerance, broker settings
4. **`README.md`**: Strategy documentation and implementation guide

### JSON Schema Compliance
All configuration files follow standardized JSON schemas for:
- Consistency across portfolios
- Validation and error checking
- Easy integration with portfolio construction engine
- Template-based portfolio creation

## 📚 Documentation

### Portfolio-Specific Documentation
- Each portfolio directory contains comprehensive README.md
- Strategy explanations, risk considerations, implementation guides
- Performance expectations and monitoring instructions

### Framework Documentation
- [Portfolio Architecture Guide](portfolios/README.md)
- [Structure Validation Summary](portfolios/STRUCTURE_VALIDATION.md)
- [Risk Management Integration](../../../docs/RISK_MANAGEMENT_EXECUTIVE_SUMMARY.md)

## 🔄 Migration Notes

### Legacy Components
- **`UnicornPortfolioConstruction.py`**: Legacy portfolio construction (being phased out)
- **`README_ETH_PORTFOLIO.md`**: Legacy ETH portfolio documentation
- **`batchjobs/`**: Legacy R scripts for batch processing

### Current Framework
- **`UnicornRiskIntegratedPortfolioConstruction.py`**: Modern risk-integrated framework
- **`portfolios/`**: Standardized portfolio configuration management
- **Template-driven**: Consistent portfolio creation and management

---

**Last Updated**: August 30, 2025  
**Version**: 2.0  
**Status**: Production Ready  
**Location**: `BackendPython/unicorn/4_portfolios/`
