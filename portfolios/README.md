# Portfolios - Unicorn Investing Platform

## 🏗️ Portfolio Architecture Overview

This directory contains all portfolio configurations and management for the Unicorn Investing platform. Each portfolio is self-contained with its own configuration, risk parameters, and execution settings.

## 📁 Directory Structure

```
portfolios/
├── README.md                    # This file - Portfolio architecture guide
├── ETH_Only/                    # 100% Ethereum portfolio
│   ├── config.json             # Portfolio configuration and allocations
│   ├── README.md               # ETH portfolio documentation
│   ├── risk_parameters.json    # Risk management settings
│   └── execution_settings.json # Trading execution parameters
├── BTC_ETH_Mixed/              # Mixed Bitcoin + Ethereum portfolio
│   ├── config.json             # Portfolio configuration and allocations
│   ├── README.md               # Mixed portfolio documentation
│   ├── risk_parameters.json    # Risk management settings
│   └── execution_settings.json # Trading execution parameters
├── Multi_Asset/                # Diversified multi-asset portfolio
│   ├── config.json             # Portfolio configuration and allocations
│   ├── README.md               # Multi-asset portfolio documentation
│   ├── risk_parameters.json    # Risk management settings
│   └── execution_settings.json # Trading execution parameters
└── templates/                  # Portfolio templates for new portfolios
    ├── config_template.json
    ├── risk_parameters_template.json
    └── execution_settings_template.json
```

## 🎯 Portfolio Configuration Standard

### Portfolio Config JSON Structure
```json
{
  "portfolio_name": "Portfolio Name",
  "description": "Portfolio description and strategy",
  "assets": {
    "SYMBOL": {
      "allocation_percent": 0.0,
      "asset_type": "cryptocurrency|equity|forex|commodity",
      "data_source": "ibkr|yahoo|alpha_vantage",
      "model_type": "prophet|xgboost|technical|ensemble"
    }
  },
  "total_allocation": 100,
  "currency": "USD",
  "rebalancing_frequency": "daily|weekly|monthly",
  "minimum_trade_size": 100,
  "created_date": "YYYY-MM-DD",
  "last_updated": "YYYY-MM-DD"
}
```

### Risk Parameters JSON Structure
```json
{
  "max_portfolio_volatility": 0.20,
  "max_single_asset_weight": 0.60,
  "var_limit": 0.05,
  "max_drawdown": 0.15,
  "sharpe_ratio_target": 1.0,
  "risk_budget_allocation": {
    "SYMBOL": 0.0
  },
  "correlation_limits": {
    "max_correlation": 0.8
  }
}
```

### Execution Settings JSON Structure
```json
{
  "execution_method": "market|limit|twap|vwap",
  "slippage_tolerance": 0.001,
  "order_size_limit": 10000,
  "trading_session": {
    "start_time": "09:30",
    "end_time": "16:00",
    "timezone": "US/Eastern"
  },
  "broker_settings": {
    "primary_broker": "ibkr",
    "backup_broker": "none",
    "account_id": "DU123456"
  }
}
```

## 🔄 Portfolio Lifecycle Workflow

### 1. Portfolio Creation
```bash
# Create new portfolio from template
cp -r portfolios/templates portfolios/New_Portfolio_Name
cd portfolios/New_Portfolio_Name
# Edit config.json, risk_parameters.json, execution_settings.json
```

### 2. Portfolio Validation
```python
# Validate portfolio configuration
from BackendPython.unicorn.4_portfolio_construction import PortfolioValidator
validator = PortfolioValidator("portfolios/ETH_Only")
validation_result = validator.validate_all()
```

### 3. Portfolio Deployment
```python
# Deploy portfolio for live trading
from BackendPython.unicorn.4_portfolio_construction import PortfolioManager
manager = PortfolioManager("portfolios/ETH_Only")
manager.deploy()
```

### 4. Portfolio Monitoring
```python
# Monitor portfolio performance
manager.get_performance_metrics()
manager.get_risk_metrics()
manager.get_current_positions()
```

## 📊 Available Portfolios

### ✅ ETH_Only Portfolio
- **Strategy**: 100% Ethereum allocation
- **Risk Profile**: Medium-High volatility
- **Rebalancing**: Daily
- **Status**: Ready for deployment
- **Models**: Enhanced Technical Alpha, Prophet forecasting

### 🔧 BTC_ETH_Mixed Portfolio (Planned)
- **Strategy**: Mixed Bitcoin + Ethereum allocation
- **Risk Profile**: Medium volatility with diversification
- **Rebalancing**: Weekly
- **Status**: Configuration pending
- **Models**: Multi-asset ensemble models

### 📋 Multi_Asset Portfolio (Planned)
- **Strategy**: Diversified across crypto, forex, equities
- **Risk Profile**: Lower volatility through diversification
- **Rebalancing**: Monthly
- **Status**: Planning phase
- **Models**: Cross-asset correlation models

## 🎯 Integration with LEAN Framework

Each portfolio integrates with the 6-layer LEAN architecture:

1. **Data Sources** (`BackendPython/unicorn/1_data_sources/`)
2. **Alpha Models** (`BackendPython/unicorn/2_alpha_models/`)
3. **Risk Management** (`BackendPython/unicorn/3_risk_management/`)
4. **Portfolio Construction** (`BackendPython/unicorn/4_portfolio_construction/`)
5. **Execution Models** (`BackendPython/unicorn/5_execution_models/`)
6. **Algorithms** (`BackendPython/unicorn/6_algorithms/`)

## 🚀 Getting Started

### Create a New Portfolio
1. Copy the template directory
2. Configure `config.json` with asset allocations
3. Set risk parameters in `risk_parameters.json`
4. Configure execution in `execution_settings.json`
5. Validate configuration
6. Deploy for live trading

### Modify Existing Portfolio
1. Update configuration files
2. Run validation tests
3. Redeploy with new settings
4. Monitor performance impact

## 📚 Documentation

- Each portfolio directory contains detailed README.md
- Configuration files are self-documenting with comments
- Integration guides in `BackendPython/unicorn/README.md`
- Risk management docs in `docs/RISK_MANAGEMENT_EXECUTIVE_SUMMARY.md`

---

**Last Updated**: August 30, 2025
**Version**: 1.0
**Status**: Production Ready
