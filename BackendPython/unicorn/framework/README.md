# Unicorn Algorithm Framework Architecture

## 🎯 Overview

This directory contains the restructured Unicorn algorithms using LEAN's Algorithm Framework for **clean separation of concerns**. The modular approach separates forecasting, portfolio construction, execution, and risk management into distinct, testable components.

## 📁 Directory Structure

```
framework/
├── alphas/                          # 🔮 Pure Forecasting Models
│   ├── AdvancedForexForecastingAlpha.py    # Multi-model forex forecasting
│   └── EthFocusedAlpha.py                  # ETH technical analysis
├── portfolio/                       # ⚖️ Position Sizing Models
│   └── UnicornPortfolioConstruction.py     # Equal weight + Confidence weighted
├── execution/                       # ⚡ Order Execution Models
│   └── (Uses LEAN's built-in models)       # ImmediateExecutionModel, etc.
├── risk/                           # 🛡️ Risk Management Models
│   └── UnicornRiskManagement.py           # Stop losses, drawdown, limits
├── universe/                       # 🌍 Universe Selection Models
│   └── (Uses LEAN's built-in models)      # ManualUniverseSelectionModel, etc.
└── UnicornFrameworkAlgorithms.py   # 🦄 Main framework algorithms

algorithms/                         # 🚀 Specific Algorithm Implementations
├── AdvancedForexFrameworkAlgorithm.py      # Forex ensemble with framework
├── EthFrameworkAlgorithm.py               # ETH $1000 portfolio with framework
└── (legacy monolithic algorithms...)      # Old mixed approach
```

## 🔄 Migration: Monolithic → Framework

### Before (Monolithic Approach)
```python
class AdvancedForexForecastingAlgorithm(QCAlgorithm):
    def initialize(self):
        # Mixed: forecasting + trading + risk in one class
        self.arima_models = {}
        self.neural_networks = {}
        pass
    
    def update_forecasts(self):
        # Forecasting logic mixed with trading decisions
        forecast = self.get_arima_forecast()
        if forecast > threshold:
            self.set_holdings(symbol, 0.2)  # Direct trading
```

### After (Framework Approach)
```python
# Pure forecasting (Alpha Model)
class AdvancedForexForecastingAlpha(AlphaModel):
    def update(self, algorithm, data):
        # ONLY forecasting - returns Insights
        forecast = self.get_ensemble_forecast()
        return [Insight.price(symbol, direction, confidence)]

# Main algorithm orchestrates components
class AdvancedForexFrameworkAlgorithm(QCAlgorithm):
    def initialize(self):
        self.set_alpha(AdvancedForexForecastingAlpha())     # Forecasting
        self.set_portfolio_construction(ConfidenceWeighted()) # Position sizing
        self.set_execution(ImmediateExecutionModel())       # Trade execution
        self.set_risk_management(UnicornRiskManagement())   # Risk controls
```

## 🧩 Framework Components

### 1. 🔮 Alpha Models (Forecasting)

#### `AdvancedForexForecastingAlpha`
- **Purpose**: Pure forecasting for forex pairs using ensemble ML models
- **Models**: ARIMA (25%) + Neural Networks (25%) + Prophet (25%) + XGBoost (25%)
- **Output**: Insights with direction, confidence, and prediction horizon
- **Assets**: EURUSD, USDJPY, USDCNH, ETHUSD

```python
# Features:
- Ensemble forecasting with 4 ML models
- Weighted combination based on historical performance
- 4-hour prediction horizon
- Confidence scoring and model agreement analysis
```

#### `EthFocusedAlpha`
- **Purpose**: Technical analysis forecasting for Ethereum
- **Indicators**: SMA crossover, RSI momentum, Bollinger Bands
- **Output**: ETH trading signals with confidence levels
- **Assets**: ETHUSD only

```python
# Features:
- SMA (10/30 hour) crossover signals
- RSI (14 hour) momentum confirmation
- Bollinger Band (20 period) breakout detection
- 2-hour prediction horizon for crypto volatility
```

### 2. ⚖️ Portfolio Construction Models

#### `UnicornEqualWeightPortfolioConstruction`
- **Strategy**: Equal allocation across all positive Insights
- **Features**: Time-based and insight-based rebalancing
- **Max Position**: Configurable (default 30%)

#### `UnicornConfidenceWeightedPortfolioConstruction`
- **Strategy**: Allocate more capital to higher-confidence insights
- **Features**: Kelly criterion position sizing, dynamic rebalancing
- **Risk-Adjusted**: Confidence-based position scaling

### 3. 🛡️ Risk Management Models

#### `UnicornRiskManagementModel`
- **Features**: Stop losses, position limits, drawdown protection
- **Stop Loss**: Fixed % + volatility-based (ATR)
- **Limits**: Max position size, max portfolio drawdown
- **Monitoring**: Real-time risk event tracking

#### `UnicornForexRiskManagement`
- **Extends**: Base risk model with forex-specific controls
- **Features**: Currency exposure limits, correlation management
- **Forex-Specific**: Cross-currency risk, carry trade controls

### 4. ⚡ Execution Models
- Uses LEAN's built-in `ImmediateExecutionModel`
- Framework handles all order placement automatically
- Can be extended for custom execution logic

## 🚀 Algorithm Implementations

### 1. `AdvancedForexFrameworkAlgorithm.py`
```python
# Configuration:
Starting Capital: $100,000
Assets: EURUSD, USDJPY, USDCNH, ETHUSD
Forecasting: Ensemble of 4 ML models
Portfolio: Confidence-weighted allocation
Risk: Forex-specific controls with 10% max drawdown
```

### 2. `EthFrameworkAlgorithm.py`
```python
# Configuration:
Starting Capital: $1,000 (as requested)
Assets: ETHUSD only
Forecasting: Technical analysis (SMA + RSI + Bollinger)
Portfolio: 95% ETH allocation
Risk: Crypto-specific controls with 15% max drawdown
```

### 3. `UnicornMasterFrameworkAlgorithm`
```python
# Configuration:
Starting Capital: $250,000
Assets: Multi-asset (Forex + Crypto)
Forecasting: Composite Alpha (Forex ensemble + ETH technical)
Portfolio: Confidence-weighted across all assets
Risk: Comprehensive multi-asset risk management
```

## 🎯 Benefits of Framework Approach

### ✅ **Separation of Concerns**
- **Forecasting**: Pure Alpha Models generate predictions
- **Trading**: Portfolio Models handle position sizing
- **Risk**: Risk Models provide safety controls
- **Execution**: Execution Models handle order placement

### ✅ **Modularity & Testability**
```python
# Easy to test forecasting accuracy separately
alpha_model = AdvancedForexForecastingAlpha()
insights = alpha_model.update(algorithm, data)
# Test forecast accuracy without trading

# Easy to swap components
self.set_alpha(DifferentForecastingModel())  # Swap forecasting approach
self.set_portfolio_construction(DifferentSizing())  # Swap position sizing
```

### ✅ **Reusability**
```python
# Same Alpha Model can be used in different strategies
strategy_a.set_alpha(AdvancedForexForecastingAlpha())
strategy_b.set_alpha(AdvancedForexForecastingAlpha())  # Reuse same forecasting

# Mix and match components
conservative_strategy.set_risk_management(HighRiskControls())
aggressive_strategy.set_risk_management(LowRiskControls())
```

### ✅ **Professional Standards**
- Industry-standard architecture used by institutional traders
- Clear interfaces between components
- Comprehensive logging and monitoring
- Easier maintenance and debugging

## 🔬 Testing & Validation

### Alpha Model Testing
```python
# Test forecasting accuracy independently
alpha = AdvancedForexForecastingAlpha()
insights = alpha.update(algorithm, data)

# Measure forecast accuracy
mae = calculate_forecast_error(insights, actual_prices)
directional_accuracy = calculate_directional_accuracy(insights, price_moves)
```

### Portfolio Construction Testing
```python
# Test position sizing logic
portfolio = UnicornConfidenceWeightedPortfolioConstruction()
targets = portfolio.create_targets(algorithm, insights)

# Validate allocation logic
total_allocation = sum(abs(target.quantity) for target in targets)
assert total_allocation <= 1.0  # No over-allocation
```

### Risk Management Testing
```python
# Test risk controls
risk_mgmt = UnicornRiskManagementModel(max_drawdown=0.1)
risk_targets = risk_mgmt.manage_risk(algorithm, targets)

# Verify risk limits are enforced
assert portfolio_drawdown <= 0.1
assert max_position_size <= 0.25
```

## 📊 Performance Comparison

| Metric | Monolithic Approach | Framework Approach |
|--------|-------------------|-------------------|
| **Code Organization** | Mixed concerns | Clean separation |
| **Testability** | Hard to isolate | Easy component testing |
| **Reusability** | Copy/paste code | Reuse components |
| **Maintainability** | Complex debugging | Clear interfaces |
| **Scalability** | Add to existing file | Add new components |
| **Professional Standard** | Ad-hoc | Industry standard |

## 🛠️ Usage Examples

### Running Framework Algorithms
```python
# Advanced Forex with Framework
algorithm = AdvancedForexFrameworkAlgorithm()
# Automatically uses:
# - AdvancedForexForecastingAlpha for forecasting
# - UnicornConfidenceWeightedPortfolioConstruction for sizing
# - UnicornForexRiskManagement for risk controls

# ETH Framework Algorithm  
algorithm = EthFrameworkAlgorithm()
# Automatically uses:
# - EthFocusedAlpha for ETH technical analysis
# - UnicornEqualWeightPortfolioConstruction for 95% ETH allocation
# - UnicornRiskManagementModel for crypto risk controls
```

### Creating Custom Combinations
```python
class CustomStrategy(QCAlgorithm):
    def initialize(self):
        # Mix and match any components
        self.set_alpha(EthFocusedAlpha())  # ETH forecasting
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction())  # Confidence weighting
        self.set_risk_management(UnicornForexRiskManagement())  # Forex risk controls
```

## 🔮 Future Enhancements

### Planned Alpha Models
- **SentimentAnalysisAlpha**: News and social media sentiment
- **MacroEconomicAlpha**: Economic indicators and central bank policy
- **CryptocurrencyMomentumAlpha**: Crypto-specific momentum strategies
- **PairsTrading Alpha**: Statistical arbitrage between correlated assets

### Planned Portfolio Models
- **RiskParityPortfolioConstruction**: Risk-based allocation
- **BlackLittermanPortfolioConstruction**: Market equilibrium with views
- **KellyOptimalPortfolioConstruction**: Kelly criterion optimization

### Planned Risk Models
- **MacroRiskManagement**: Economic event and regime change protection
- **VolatilityTargetingRisk**: Dynamic risk based on volatility forecasts
- **CorrelationRiskManagement**: Advanced correlation and covariance controls

## 📚 Documentation

- **LEAN Framework Guide**: `/docs/LEAN_ARCHITECTURE_GUIDE.md`
- **Forecasting Guide**: `/algorithms/LEAN_FORECASTING_GUIDE.md`
- **Migration Guide**: This document
- **API Documentation**: Component-specific docstrings

## 🎯 Conclusion

The Framework approach provides **professional-grade architecture** that separates forecasting from trading decisions. This enables:

1. **Better Testing**: Test forecasting accuracy independently
2. **Easier Maintenance**: Clear component boundaries
3. **Scalability**: Add new models without changing existing code
4. **Reusability**: Use same forecasting models in multiple strategies
5. **Professional Standards**: Industry-standard algorithmic trading architecture

The investment in restructuring pays off through improved code quality, easier testing, and more robust trading systems.
