# Myportolio Utilities

This directory contains utility modules for the Myportolio portfolio construction framework, providing essential components for model selection, risk management, and trading strategy integration.

## Overview

The utilities directory serves as the framework layer of the Myportolio portfolio implementation, containing shared components that integrate trading algorithms, risk algorithms, and model selection systems.

## Components

### Model Selection

#### `best_model_selector.py`
- **Purpose**: Basic model selector for choosing optimal trading models based on performance metrics
- **Key Features**:
  - R² score-based model ranking
  - Performance metric analysis
  - Model confidence assessment
  - Asset-specific model selection
- **Usage**: Used by portfolio construction to select best-performing models for each asset

#### `enhanced_best_model_selector.py` ✨ **NEW**
- **Purpose**: Enhanced model selector with economic ensemble integration
- **Key Features**:
  - Individual vs ensemble model comparison
  - Economic feature importance weighting
  - Multi-criteria decision making (R², economic importance, MAE, complexity)
  - Production deployment configuration
- **Integration**: Supports both individual economic-enhanced models and economic ensemble models
- **Scoring Algorithm**: Weighted scoring with 40% R², 30% economic importance, 20% MAE, 10% complexity bonus

### Risk Management

#### `kelly_criterion.py`
- **Purpose**: Kelly Criterion implementation for optimal position sizing
- **Key Features**:
  - Optimal portfolio allocation calculation
  - Risk-adjusted position sizing
  - Drawdown protection
  - Multi-asset portfolio optimization
- **Integration**: Used by both risk algorithms and trading algorithms for position sizing

### Trading Strategy Integration

#### `model_strategy_integration.py`
- **Purpose**: Integration framework for combining models with trading strategies
- **Key Features**:
  - Model-strategy binding
  - Performance optimization
  - Multi-timeframe coordination
- **Architecture**: Bridges between alpha models (Layer 2) and portfolio construction (Layer 4)

#### `multi_timeframe_strategies.py`
- **Purpose**: Multi-timeframe trading strategy coordination
- **Key Features**:
  - Timeframe synchronization
  - Strategy hierarchy management
  - Signal aggregation across timeframes
- **Usage**: Enables sophisticated multi-timeframe trading approaches

#### `timeframe_model_frameworks.py`
- **Purpose**: Framework for managing models across different timeframes
- **Key Features**:
  - Timeframe-specific model management
  - Cross-timeframe signal coordination
  - Performance tracking by timeframe

### System Utilities

#### `statuscheck.py`
- **Purpose**: System status and health monitoring
- **Key Features**:
  - Component health checks
  - Performance monitoring
  - System diagnostics
- **Integration**: Used for monitoring portfolio construction health

## Architecture Integration

### LEAN Framework (6-Layer Architecture)
- **Layer 1**: Data Sources → Raw market data collection
- **Layer 2**: Alpha Models → Economic-enhanced XGBoost and ensemble models
- **Layer 3**: Risk Management → Risk controls and limits
- **Layer 4**: **Portfolio Construction** → **Utilities integrate all components here**
- **Layer 5**: Execution Models → Order placement and execution  
- **Layer 6**: Algorithms → Complete trading algorithms

### Algorithm Separation Pattern
- **Risk Algorithms**: Pure risk calculations with NO trading decisions
- **Trading Algorithms**: Pure trading strategies with NO risk calculations
- **Framework Utilities**: **Shared components that integrate both algorithm types**

## Usage Examples

### Enhanced Model Selection
```python
from enhanced_best_model_selector import EnhancedBestModelSelector

selector = EnhancedBestModelSelector()
optimal_models = selector.get_best_models_with_ensembles()
production_config = selector.generate_production_config(optimal_models)
```

### Kelly Criterion Position Sizing
```python
from kelly_criterion import KellyCriterion

kelly = KellyCriterion()
optimal_allocation = kelly.calculate_portfolio_allocation(returns, risk_free_rate)
```

### Model-Strategy Integration
```python
from model_strategy_integration import ModelStrategyIntegrator

integrator = ModelStrategyIntegrator()
integrated_strategy = integrator.combine_model_with_strategy(model, strategy)
```

## Recent Updates

### Enhanced Model Selector (September 9, 2025)
- Added comprehensive economic ensemble integration
- Implemented multi-criteria scoring system
- Enhanced production configuration generation
- Added fallback model configurations
- Improved confidence level determination

### Performance Improvements
- Optimized model comparison algorithms
- Enhanced scoring methodology
- Improved production readiness assessment
- Added comprehensive logging and monitoring

## Dependencies

- **Core**: pandas, numpy, sqlite3, json
- **Model Integration**: scikit-learn integration support
- **Database**: SQLite for model performance tracking
- **Configuration**: JSON-based configuration management

## Integration Points

- **Alpha Models**: Consumes model performance data from `2_alpha_models/CRYPTO/`
- **Risk Algorithms**: Integrates with `../risk_algorithms/` for risk calculations
- **Trading Algorithms**: Integrates with `../trading_algorithms/` for strategy execution
- **Simulations**: Provides model selection for `../simulations/` backtesting framework

## Status

✅ **Production Ready**: Enhanced model selector with economic ensemble support  
✅ **Testing Complete**: Comprehensive validation across all utility components  
✅ **Documentation**: Complete API documentation and usage examples  

---

*This README was last updated on September 9, 2025, following the implementation of enhanced model selection with economic ensemble integration.*
