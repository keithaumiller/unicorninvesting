# Kelly Criterion Position Sizing

## 🎯 **Kelly Criterion Risk Methodology**

Mathematical approach to optimal position sizing based on the Kelly formula, maximizing long-term growth while controlling risk through fractional Kelly implementation.

## 📐 **Mathematical Foundation**

### **Kelly Formula**
```
f* = (bp - q) / b

Where:
- f* = fraction of capital to allocate
- b = odds ratio (average_win / average_loss)
- p = probability of winning
- q = probability of losing (1 - p)
```

### **Risk-Adjusted Implementation**
```
Adjusted Position = Kelly_Fraction × Risk_Adjustment × Portfolio_Value

Where:
- Risk_Adjustment = 0.5 (fractional Kelly for reduced volatility)
- Kelly_Fraction = min(calculated_kelly, max_kelly_fraction)
```

## 🏗️ **Asset-Specific Implementations**

### ✅ **ETH Implementation** (`ETH/kelly_criterion.py`)

#### **KellyCriterionCalculator Class**
- **Signal History Tracking**: Maintains win/loss records for probability estimation
- **Dynamic Probability Adjustment**: Adjusts win rate based on signal confidence
- **Risk Controls**: Maximum Kelly fraction (25%), minimum win rate (35%)
- **Position Optimization**: Converts Kelly fraction to actual position sizes

#### **ETHKellyPortfolioManager Class**  
- **Portfolio Integration**: Seamless integration with ETH trading strategies
- **Real-time Calculation**: Live position sizing based on current signals
- **Performance Tracking**: Comprehensive Kelly performance analytics

#### **Current Performance**
- **Win Rate**: 62.5% (historical validation)
- **Average Return**: 3.63% per signal
- **Sharpe Ratio**: 0.74
- **Risk Adjustment**: 50% fractional Kelly

### 🚧 **Future Asset Implementations**

#### **BTC Implementation** (Planned)
- **Higher Volatility Adjustment**: Modified risk parameters for BTC characteristics
- **Correlation Considerations**: Multi-crypto correlation in position sizing
- **Market Regime Detection**: BTC-specific market cycle adjustments

#### **Multi-Asset Kelly** (Planned)
- **Portfolio-Level Kelly**: Kelly optimization across multiple assets
- **Cross-Asset Correlation**: Correlation-adjusted position sizing
- **Risk Budgeting**: Kelly-based risk budget allocation

## 🔧 **Configuration Parameters**

### **Risk Control Parameters**
```json
{
  "kelly": {
    "lookback_window": 30,           // Historical signal window
    "max_kelly_fraction": 0.25,      // Maximum position size (25%)
    "min_win_rate": 0.35,            // Minimum win rate threshold
    "risk_adjustment": 0.5,          // Fractional Kelly multiplier
    "confidence_scaling": true,       // Enable signal confidence scaling
    "dynamic_adjustment": true        // Enable dynamic parameter adjustment
  }
}
```

### **Asset-Specific Tuning**
- **ETH**: Higher volatility tolerance, 30-day lookback
- **BTC**: Lower risk adjustment due to higher volatility
- **Stocks**: Longer lookback window for stability

## 📊 **Implementation Interface**

### **Core Methods**
```python
class KellyCriterionCalculator:
    def calculate_kelly_fraction(self, signal_confidence: float, signal_type: str) -> Dict
    def calculate_position_size(self, signal_data: Dict, portfolio_value: float, current_price: float) -> Dict
    def update_signal_history(self, signal_data: Dict, outcome_return: float) -> None
    def get_performance_summary(self) -> Dict
```

### **Integration Pattern**
```python
# Initialize Kelly calculator
kelly_calc = KellyCriterionCalculator(max_kelly_fraction=0.25, risk_adjustment=0.5)

# Process trading signal
signal = {'signal': 'BUY', 'confidence': 0.7}
position_recommendation = kelly_calc.calculate_position_size(
    signal, portfolio_value=100000, current_price=3000
)

# Get optimal position
optimal_position = position_recommendation['position_size']
kelly_fraction = position_recommendation['kelly_fraction']
```

## 🎯 **Risk Controls**

### **Position Limits**
- **Maximum Kelly Fraction**: 25% of portfolio (configurable)
- **Minimum Win Rate**: 35% threshold for position entry
- **Risk Adjustment**: 50% of calculated Kelly (fractional Kelly)

### **Dynamic Adjustments**
- **Signal Confidence Scaling**: Higher confidence → higher win probability
- **Historical Performance**: Win rate based on actual signal outcomes
- **Market Regime Adaptation**: Kelly parameters adjusted for market conditions

### **Safety Mechanisms**
- **Insufficient Data Protection**: Conservative defaults when data limited
- **Extreme Kelly Protection**: Maximum fraction caps prevent over-leverage
- **Continuous Monitoring**: Real-time performance tracking and adjustment

## 📈 **Performance Validation**

### **Backtesting Metrics**
- **Win Rate Accuracy**: Historical win rate vs predicted win rate
- **Return Attribution**: Kelly contribution to portfolio performance
- **Risk Metrics**: Sharpe ratio, maximum drawdown, volatility
- **Position Sizing Effectiveness**: Optimal vs actual position performance

### **Live Trading Validation**
- **Real-time Performance**: Live signal outcome tracking
- **Risk Adherence**: Compliance with Kelly position limits
- **Dynamic Adjustment**: Parameter adaptation based on live results

## 🔬 **Usage Examples**

### **Basic Kelly Calculation**
```python
from kelly_criterion.ETH.kelly_criterion import KellyCriterionCalculator

# Initialize calculator
kelly_calc = KellyCriterionCalculator(
    lookback_window=30,
    max_kelly_fraction=0.25,
    risk_adjustment=0.5
)

# Add historical signal outcomes
kelly_calc.update_signal_history(
    {'signal': 'BUY', 'confidence': 0.7}, 
    outcome_return=0.05  # 5% return
)

# Calculate optimal position
result = kelly_calc.calculate_position_size(
    signal_data={'signal': 'BUY', 'confidence': 0.8},
    portfolio_value=100000,
    current_price=3000
)

print(f"Kelly Position: {result['position_size']:.4f} ETH")
print(f"Kelly Fraction: {result['kelly_fraction']:.1%}")
```

### **Portfolio Integration**
```python
from kelly_criterion.ETH.kelly_criterion import ETHKellyPortfolioManager

# Initialize portfolio manager
kelly_manager = ETHKellyPortfolioManager(
    kelly_config={'max_kelly_fraction': 0.25, 'risk_adjustment': 0.5}
)

# Process trading signal
market_data = {'price': 3000, 'timestamp': datetime.now()}
portfolio_data = {'total_value': 100000, 'cash': 80000}
signal_data = {'signal': 'BUY', 'confidence': 0.7}

recommendation = kelly_manager.process_signal(
    signal_data, market_data, portfolio_data
)

# Execute if approved
if recommendation['kelly_fraction'] > 0:
    kelly_manager.update_position(
        recommendation['position_size'], 
        market_data['price']
    )
```

## 🚀 **Development Roadmap**

### **Phase 1: ETH Optimization** ✅ Complete
- ETH-specific Kelly implementation
- IBKR integration and live trading
- Comprehensive testing and validation

### **Phase 2: Multi-Asset Expansion**
- BTC Kelly implementation
- Cross-asset correlation modeling
- Portfolio-level Kelly optimization

### **Phase 3: Advanced Features**
- Machine learning enhanced probability estimation
- Market regime-aware Kelly parameters
- Multi-timeframe Kelly optimization

### **Phase 4: Institutional Features**
- Kelly-based risk budgeting
- Regulatory compliance integration
- Advanced performance attribution

---

**Status**: ✅ ETH Implementation Complete - Ready for Multi-Asset Expansion
**Performance**: 62.5% win rate, 3.63% average return, 0.74 Sharpe ratio
**Integration**: Live IBKR trading operational with comprehensive risk controls
