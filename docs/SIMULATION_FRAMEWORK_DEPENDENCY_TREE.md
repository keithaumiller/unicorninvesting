# Unicorn Investing - Consolidated Simulation Framework Documentation

## Executive Summary

This document provides a comprehensive dependency tree and process flow for the Unicorn Investing simulation framework, identifying all working scripts, their dependencies, and the consolidated process flow that is operational as of September 16, 2025.

## Overview of Working Systems

### 1. Main Entry Points

#### A. Primary Simulation System
- **Main Entry**: `/BackendPython/unicorn/4_portfolios/Myportolio/simulations/myportolio_simulator.py`
- **Status**: ✅ WORKING (fixed on 2025-09-16)
- **Purpose**: Single authoritative entry point for all simulations with mandatory enhanced logging
- **Usage**: 
  ```bash
  python3 myportolio_simulator.py backtest --start 2024-03-01 --end 2024-09-01 --strategy best_models
  ```

#### B. Legacy Simulation Runner  
- **Main Entry**: `/BackendPython/unicorn/4_portfolios/Myportolio/simulation_runner.py`
- **Status**: ⚠️ PARTIALLY WORKING (some referenced files archived)
- **Purpose**: Manages multiple simulation types via organized core systems
- **Usage**: `python simulation_runner.py [1-9|ALL-CORE|ALL-BACKTESTING]`

### 2. Core Simulation Framework

#### A. Simulation Engine Components
```
myportolio_simulator.py                 # Master simulation coordinator
├── python_simulation_engine.py         # Core simulation execution engine
├── python_result_handler.py           # Results processing and storage
├── performance_logger.py              # Enhanced performance logging
└── simulation_cli.py                  # Command-line interface
```

#### B. Algorithm Templates
```
simulations/algorithms/
├── MyportolioEconomicEnhanced.py      # Economic-enhanced trading algorithm
└── MyportolioEnsembleMultiAsset.py    # Multi-asset ensemble algorithm
```

## Detailed Dependency Tree

### 1. Core Simulation Dependencies

#### A. Internal Myportolio Dependencies

**Core Systems (`core/`)**:
- `simplified_ensemble_portfolio.py` ✅ (Primary ensemble trading system)
- `live_eth_kelly_portfolio.py` ✅ (Kelly Criterion portfolio optimization)
- `dual_crypto_portfolio_manager.py` ✅ (BTC/ETH dual crypto management)
- `integrated_six_position_system.py` ✅ (Multi-position trading framework)
- `live_market_data_feed.py` ✅ (Real-time Coinbase API data feed)
- `lean_backtesting_integration.py` ✅ (QuantConnect LEAN integration)

**Trading Algorithms (`trading_algorithms/`)**:
- `eth_momentum_strategy.py` ✅ (ETH momentum strategy)
- `multi_timeframe_strategies.py` ✅ (Multi-timeframe trading strategies)
- `advanced_multi_asset_strategy.py` ✅ (Advanced multi-asset strategy)

**Risk Management (`risk_algorithms/`)**:
- `eth_basic_risk.py` ✅ (ETH basic risk management)
- `comprehensive_risk_manager.py` ✅ (Comprehensive risk controls)
- `six_position_risk_manager.py` ✅ (Six-position risk management)
- `emergency_stop.py` ✅ (Emergency stop mechanisms)

**Utilities (`utilities/`)**:
- `statuscheck.py` ✅ (System readiness validation)
- `kelly_criterion.py` ✅ (Kelly Criterion calculations)
- `best_model_selector.py` ✅ (Model selection system)
- `enhanced_best_model_selector.py` ✅ (Enhanced model selection)

#### B. External Framework Dependencies

**Alpha Models Framework**:
```
/BackendPython/unicorn/2_alpha_models/
├── CRYPTO/                           # 288 crypto models (ETH, BTC)
├── FOREX/                           # 0 forex models  
├── EQUITIES/                        # 0 equity models
├── fixed_multi_asset_models/        # 34 fixed multi-asset models
└── multi_asset_models/              # 14 multi-asset models
Total: 336 trained models across all asset classes
```

**Silver Layer Data Sources**:
```
/BackendPython/unicorn/1_data_sources/3_silver/
├── yahoo_finance_assets/processed_data/
│   ├── crypto/                      # ETH, BTC processed data
│   └── forex/                       # Forex processed data  
├── economic_indicators/             # Economic data integration
└── silver_layer_forecast_reader.py # Forecast reading components
```

**Configuration Files**:
```
config/
├── secrets.json                    # API keys and credentials (DO NOT TOUCH)
├── database.json                   # Database configuration
├── config.json                     # Portfolio configuration
├── risk_parameters.json            # Risk management parameters
└── execution_settings.json         # Execution settings
```

### 2. Archived/Legacy Components

**Archived Backtesting Tools** (`simulations/archived/backtesting_tools/`):
- `comprehensive_backtesting_suite.py` (Referenced by simulation_runner.py but archived)
- `robust_backtesting_suite.py` (Referenced by simulation_runner.py but archived)  
- `parameter_optimization_backtester.py` (Referenced by simulation_runner.py but archived)

## Process Flow Documentation

### 1. Primary Simulation Process (myportolio_simulator.py)

```
1. Initialize Simulation Components
   ├── PythonSimulationEngine (simulation execution)
   ├── PythonResultHandler (results processing)
   └── PerformanceLogger (enhanced logging)

2. Load Strategy Templates
   ├── best_models_template (Economic-enhanced models)
   ├── backtest_template (ETH momentum)
   ├── dual_crypto_template (BTC/ETH dual strategy)
   └── paper_trading_template (Paper trading)

3. Execute Simulation
   ├── Create enhanced algorithms (trading + risk)
   ├── Generate LEAN algorithm file
   ├── Run Python-based simulation
   ├── Process results with enhanced analysis
   └── Store results in database

4. Output Generation
   ├── Performance logs (JSON format)
   ├── Portfolio state tracking
   ├── Comprehensive result files
   └── Database storage for analysis
```

### 2. Algorithm Integration Process

```
Trading Strategy Creation:
├── Load best models from alpha models database
├── Initialize ETH Momentum Strategy (5/20 MA crossover)
├── Initialize ETH Risk Algorithm (max_dd=0.15, max_pos=0.8)
└── Apply Kelly Criterion for position sizing

Risk Management Integration:
├── Comprehensive risk manager (multiple algorithms)
├── Emergency stop mechanisms  
├── Six-position risk management
└── Real-time risk monitoring

Portfolio Construction:
├── Enhanced portfolio manager integration
├── Multi-timeframe strategy coordination
├── Asset allocation optimization
└── Performance tracking and logging
```

### 3. Data Flow Architecture

```
External Data Sources → Silver Layer → Alpha Models → Simulation Engine
     ↓                      ↓              ↓              ↓
- Market data          - Processed      - 336 trained   - Strategy execution
- Economic data        - data files     - models         - Risk management  
- Real-time feeds      - Forecasts      - Predictions    - Performance tracking
```

## Working Script References

### 1. Operational Scripts (✅ Working)

**Simulation Framework**:
- `simulations/myportolio_simulator.py` - Master simulation system
- `simulations/python_simulation_engine.py` - Core execution engine  
- `simulations/python_result_handler.py` - Results processing
- `simulations/performance_logger.py` - Enhanced logging

**Core Trading Systems**:
- `core/simplified_ensemble_portfolio.py` - Primary ensemble system
- `core/live_eth_kelly_portfolio.py` - Kelly portfolio optimization
- `core/dual_crypto_portfolio_manager.py` - Dual crypto management
- `core/integrated_six_position_system.py` - Multi-position framework

**Algorithm Components**:
- `trading_algorithms/eth_momentum_strategy.py` - ETH momentum
- `risk_algorithms/eth_basic_risk.py` - ETH risk management
- `utilities/kelly_criterion.py` - Kelly calculations
- `utilities/statuscheck.py` - System validation

### 2. Referenced but Archived Scripts (⚠️ Moved)

**Backtesting Tools** (moved to `simulations/archived/backtesting_tools/`):
- `comprehensive_backtesting_suite.py`
- `robust_backtesting_suite.py`
- `parameter_optimization_backtester.py`

### 3. External Dependencies (🔗 Framework)

**Alpha Models**: `/BackendPython/unicorn/2_alpha_models/` (336 models)
**Data Sources**: `/BackendPython/unicorn/1_data_sources/3_silver/`
**Configuration**: `config/` directory (secrets, database, risk parameters)

## Usage Examples

### 1. Run 6-Month Simulation (Best Models)
```bash
cd /workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations
python3 myportolio_simulator.py backtest --start 2024-03-01 --end 2024-09-01 --strategy best_models
```

### 2. Run ETH Momentum Strategy
```bash
python3 myportolio_simulator.py backtest --start 2024-08-01 --end 2024-08-15 --strategy momentum
```

### 3. System Validation
```bash
cd /workspaces/unicorninvesting
python3 BackendPython/unicorn/4_portfolios/Myportolio/utilities/statuscheck.py --detailed
```

### 4. Legacy Simulation Runner
```bash
cd /workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio
python simulation_runner.py 1  # Core Ensemble Portfolio
python simulation_runner.py 2  # Live ETH Kelly Portfolio
python simulation_runner.py ALL-CORE  # All core simulations
```

## Key Fixes Applied (2025-09-16)

1. **Fixed missing method**: `_process_enhanced_backtest_results` → `_process_backtest_results`
2. **Fixed missing method**: `_generate_enhanced_lean_algorithm` → `_prepare_algorithm_file`
3. **Added missing field**: `SimulationRequest.metadata` field added
4. **Updated validation**: `statuscheck.py` now correctly counts 336 models vs old 174 count
5. **Path corrections**: Fixed alpha_models_dir path to point to correct directory structure

## System Status Summary

- ✅ **Primary Simulation System**: Fully operational
- ✅ **6-Month Backtesting**: Working with enhanced logging  
- ✅ **Model Integration**: 336 trained models available across 5 asset classes
- ✅ **Risk Management**: Comprehensive risk algorithms operational
- ✅ **Performance Logging**: Enhanced logging with JSON output
- ⚠️ **Legacy Components**: Some referenced scripts archived but core functionality intact
- 🔴 **Configuration**: Some config files missing but defaults used

## Recommended Usage

**For Production Trading Simulations**: Use `myportolio_simulator.py` (primary system)
**For Development/Testing**: Use `simulation_runner.py` for specific component testing
**For System Validation**: Use `statuscheck.py` for comprehensive system health checks

---

*Documentation updated: September 16, 2025*
*System Status: Operational with enhanced logging and comprehensive model integration*