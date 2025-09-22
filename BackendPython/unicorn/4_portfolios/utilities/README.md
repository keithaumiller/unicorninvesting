# Portfolio Framework Utilities

## 🔧 Framework Components

This directory contains framework-level utilities that support portfolio construction and management across the entire system.

## 📁 Component Overview

### Enhanced Portfolio Orchestration Engine
- **`EnhancedPortfolioOrchestrator.py`**: Complete workflow coordination engine (783 lines)
  - Orchestrates entire Data → Alpha → Risk → Portfolio → Execution pipeline
  - Coordinates multi-timeframe analysis and algorithm execution
  - Provides async/sync execution, performance monitoring, and LEAN integration
  - Template system for algorithm integration with comprehensive coordination

### Portfolio Management Framework
- **`EnhancedPortfolioManager.py`**: Main portfolio management engine
  - Integrates portfolio configuration with risk-integrated construction
  - Coordinates between risk algorithms and trading algorithms
  - Provides portfolio lifecycle management

- **`PortfolioConfigManager.py`**: Configuration management system
  - Loads and manages portfolio configurations from JSON files
  - Handles risk parameters and execution settings
  - Provides configuration validation and defaults

- **`UnicornRiskIntegratedPortfolioConstruction.py`**: Core portfolio construction framework
  - Risk-budgeting based portfolio allocation
  - Integrated risk assessment and position sizing
  - Real-time portfolio optimization with risk constraints

## 🎯 Usage Examples

### Complete Workflow Orchestration
```python
from utilities.EnhancedPortfolioOrchestrator import EnhancedPortfolioOrchestrator

# Initialize orchestrator with portfolio configuration
orchestrator = EnhancedPortfolioOrchestrator("Myportolio")

# Execute complete workflow coordination
workflow_results = await orchestrator.execute_complete_workflow()

# Multi-timeframe coordination
results = await orchestrator.execute_multi_timeframe_analysis(
    timeframes=['1m', '5m', '15m', '1h', '4h', '1d']
)

# Sync execution for traditional integration
sync_results = orchestrator.execute_sync_workflow()
```

### Portfolio Configuration Management
```python
from utilities.PortfolioConfigManager import PortfolioConfigManager

# Load portfolio configuration
config_manager = PortfolioConfigManager("../Myportolio/config.json")
portfolio_config = config_manager.load_portfolio_config()
risk_params = config_manager.load_risk_parameters()
execution_settings = config_manager.load_execution_settings()
```

### Enhanced Portfolio Management
```python
from utilities.EnhancedPortfolioManager import EnhancedPortfolioManager

# Initialize portfolio manager
portfolio_manager = EnhancedPortfolioManager(config_manager)

# Construct portfolio with integrated risk management
portfolio_targets = portfolio_manager.construct_portfolio(
    trading_signals=trading_algorithm.generate_signals(),
    risk_constraints=risk_algorithm.calculate_constraints()
)
```

### Risk-Integrated Portfolio Construction
```python
from utilities.UnicornRiskIntegratedPortfolioConstruction import (
    UnicornRiskIntegratedPortfolioConstruction,
    RiskBudget,
    PortfolioTarget
)

# Initialize framework
constructor = UnicornRiskIntegratedPortfolioConstruction()

# Set risk budget
risk_budget = RiskBudget(
    total_risk_budget=0.15,  # 15% max portfolio volatility
    asset_class_limit=0.60,  # 60% risk to any asset class
    concentration_limit=0.25  # 25% risk to any single position
)

# Construct portfolio with risk integration
portfolio_targets = constructor.construct_portfolio(
    alpha_insights=trading_signals,
    risk_budget=risk_budget,
    current_portfolio=current_positions
)
```

## 🔄 Integration with Portfolio

These utilities are designed to work with the separated algorithm architecture:

### Complete Orchestration Pipeline
1. **Data Sources** → Market data collection and validation
2. **Alpha Models** → Trading signal generation algorithms
3. **Risk Management** → Risk constraint calculation and validation
4. **Portfolio Construction** → Integrated portfolio optimization
5. **Execution Models** → Order generation and execution coordination
6. **Algorithm Templates** → Complete strategy coordination

### Traditional Framework Integration
1. **Risk Algorithms** → Generate risk constraints and metrics
2. **Trading Algorithms** → Generate trading signals and targets
3. **Framework Utilities** → Integrate both into portfolio construction
4. **Portfolio Configuration** → Provide parameters and settings

## 📚 Framework Architecture

```
Enhanced Portfolio Orchestrator (783 lines)
    ↓ Coordinates Complete Workflow
Data → Alpha → Risk → Portfolio → Execution
    ↓ Integrated with
Trading Algorithms + Risk Algorithms
    ↓ Through
Framework Utilities (Portfolio Manager, Config Manager)
    ↓ Resulting in
Portfolio Construction & Execution Targets
```

### Orchestration Features
- **Multi-Stage Workflow**: Complete 6-stage trading pipeline coordination
- **Multi-Timeframe Analysis**: Coordinate analysis across timeframes
- **Async/Sync Execution**: Support both execution modes
- **Performance Monitoring**: Built-in execution time tracking
- **Algorithm Integration**: Ready for multiple backtesting platforms
- **Template System**: Algorithm template registry for standardized integration

---

**Component Status**: ✅ Framework utilities organized and ready  
**Integration Status**: 🚧 Ready for algorithm development  
**Usage**: Framework-level components for portfolio management
