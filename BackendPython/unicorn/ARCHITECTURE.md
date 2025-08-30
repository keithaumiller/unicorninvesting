# LEAN Algorithm Framework - Technical Architecture

## 🏗️ System Architecture

The Unicorn Investing platform implements the **QuantConnect LEAN Algorithm Framework**, which provides institutional-grade separation of concerns for algorithmic trading systems.

## 🔄 Data Flow Architecture

```
Market Data → Alpha Models → Portfolio Construction → Risk Management → Execution → Orders
     ↓              ↓                ↓                    ↓             ↓         ↓
  Raw Prices   →  Insights    →  Targets        →   Risk Checks  →  Orders  →  Fills
```

### Detailed Flow:

1. **Data Ingestion**: Market data streams into the algorithm
2. **Signal Generation**: Alpha Models analyze data and generate Insights
3. **Position Sizing**: Portfolio Models convert Insights into PortfolioTargets
4. **Risk Validation**: Risk Models validate and potentially modify targets
5. **Order Execution**: Execution Models place orders to achieve targets
6. **Portfolio Updates**: Framework updates portfolio state with fills

## 🧩 Component Interfaces

### Alpha Model Interface
```python
class AlphaModel:
    def update(self, algorithm, data) -> List[Insight]:
        """Generate trading insights from market data"""
        pass
```

### Portfolio Construction Interface
```python
class PortfolioConstructionModel:
    def create_targets(self, algorithm, insights) -> List[PortfolioTarget]:
        """Convert insights into portfolio targets"""
        pass
```

### Risk Management Interface
```python
class RiskManagementModel:
    def manage_risk(self, algorithm, targets) -> List[PortfolioTarget]:
        """Apply risk controls to portfolio targets"""
        pass
```

### Execution Interface
```python
class ExecutionModel:
    def execute(self, algorithm, targets) -> None:
        """Execute portfolio targets as market orders"""
        pass
```

## 🎯 Component Responsibilities

### 📊 Alpha Models (Forecasting)
**Responsibility**: Generate trading signals based on market analysis

**Input**:
- Historical price data
- Economic indicators  
- Alternative data sources
- Technical indicators

**Output**:
- `Insight` objects with:
  - Symbol
  - Direction (Up/Down/Flat)
  - Confidence (0-1)
  - Time horizon
  - Expected return

**Key Principle**: NO trading decisions, only forecasts

**Current Implementations**:
- `AdvancedForexForecastingAlpha.py`: ARIMA + Neural Networks + Prophet + XGBoost ensemble
- `EthFocusedAlpha.py`: SMA + RSI + Bollinger Bands technical analysis
- `predictiveanalytics/`: Advanced ML forecasting models
- `recomendationsystems/`: Recommendation engine components

### 🎯 Portfolio Construction (Position Sizing)
**Responsibility**: Convert insights into position sizes **WITH INTEGRATED RISK MANAGEMENT**

**Input**:
- Insights from Alpha Models
- Current portfolio state
- Available capital
- **Risk budget allocation**
- **Risk metrics and constraints**

**Output**:
- `PortfolioTarget` objects with:
  - Symbol
  - Target weight/quantity
  - **Risk contribution**
  - **Risk-adjusted reasoning**

**Strategies**:
- **Risk budgeting** (foundation)
- **Risk-adjusted Kelly criterion**
- **Risk parity with alpha tilt**
- **Volatility-weighted allocation**

**Current Implementations**:
- `UnicornRiskIntegratedPortfolioConstruction.py`: **INTEGRATED** risk-aware portfolio construction
- `batchjobs/`: Batch portfolio optimization processes

**Key Principle**: Risk management IS portfolio construction - not a separate layer

### 🛡️ Risk Management (INTEGRATED with Portfolio Construction)
**Responsibility**: **FOUNDATIONAL COMPONENT** of portfolio construction, not separate validation

**Architecture**: 
- **Risk Budgeting Framework**: Allocates risk before allocating capital
- **Risk Assessment Engine**: Calculates risk metrics for optimization input
- **Dynamic Risk Monitoring**: Continuous risk budget utilization management

**Integration Points**:
- **Position sizing driven by risk budget allocation**
- **Risk constraints as optimization inputs (not filters)**
- **Continuous risk-return optimization**
- **Dynamic adjustment based on risk utilization**

**Risk Metrics Used IN Portfolio Construction**:
- VaR 95% and Expected Shortfall
- Concentration and correlation risk
- Liquidity and execution risk
- Maximum drawdown protection

**Current Implementations**:
- `UnicornRiskIntegratedPortfolioConstruction.py`: Unified risk-portfolio framework
- `RiskBudgetingFramework`: Foundation risk allocation system
- `RiskAssessmentEngine`: Real-time risk metrics calculation

**⚠️ DEPRECATED APPROACH**: Separate risk management as post-construction validation

### ⚡ Execution Models (Order Placement)
**Responsibility**: Convert targets into actual orders

**Input**:
- Validated portfolio targets
- Current market conditions
- Liquidity considerations

**Output**:
- Market/limit orders
- Order scheduling
- Execution tracking

**Strategies**:
- Immediate execution
- VWAP (Volume Weighted Average Price)
- TWAP (Time Weighted Average Price)
- Implementation shortfall

**Current Implementations**:
- `integrations/`: Broker and exchange integrations
- `deployment/`: Live trading deployment configurations
- **Future**: Custom execution for slippage optimization, VWAP strategies

## 🔧 Technical Implementation

### Component Registration
```python
class MyAlgorithm(QCAlgorithm):
    def initialize(self):
        # Register all framework components
        self.set_alpha(AdvancedForexForecastingAlpha())
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction())
        self.set_risk_management(UnicornRiskManagementModel())
        self.set_execution(ImmediateExecutionModel())
```

### Framework Orchestration
LEAN automatically orchestrates the flow:
1. Calls `Alpha.update()` on new data
2. Calls `Portfolio.create_targets()` with insights
3. Calls `Risk.manage_risk()` with targets
4. Calls `Execution.execute()` with validated targets

### State Management
- Each component maintains its own state
- Framework provides portfolio and data context
- Components communicate only through defined interfaces

## 🧪 Testing Architecture

### Unit Testing
```python
# Test Alpha Model forecasting accuracy
def test_alpha_accuracy():
    alpha = AdvancedForexForecastingAlpha()
    insights = alpha.update(mock_algorithm, historical_data)
    accuracy = evaluate_insights(insights, future_returns)
    assert accuracy > 0.55  # Better than random

# Test Portfolio Model allocation
def test_portfolio_allocation():
    portfolio = UnicornEqualWeightPortfolioConstruction()
    targets = portfolio.create_targets(mock_algorithm, mock_insights)
    assert sum(target.quantity for target in targets) <= available_capital
```

### Integration Testing
```python
# Test full framework pipeline
def test_framework_pipeline():
    algorithm = create_test_algorithm()
    algorithm.run_backtest(start_date, end_date)
    assert algorithm.portfolio.total_profit > 0
```

## 🚀 Performance Considerations

### Memory Management
- Components should be stateless where possible
- Use generators for large datasets
- Implement proper cleanup in `on_securities_changed()`

### Computational Efficiency
- Cache expensive calculations
- Use vectorized operations (numpy/pandas)
- Minimize data copying between components

### Scalability
- Components should scale linearly with universe size
- Use parallel processing for independent calculations
- Implement proper data structures for fast lookups

## 🔒 Security & Risk

### Code Isolation
- Components cannot directly access each other
- Framework controls all communication
- Prevents accidental coupling

### Risk Controls
- Multiple validation layers
- Real-time monitoring
- Automatic circuit breakers

### Audit Trail
- All component decisions are logged
- Framework tracks all state changes
- Complete trade attribution

## 📈 Monitoring & Observability

### Performance Metrics
- Component execution time
- Memory usage
- Forecast accuracy
- Risk-adjusted returns

### Logging Strategy
```python
# Each component logs its decisions
self.log(f"Alpha: Generated {len(insights)} insights")
self.log(f"Portfolio: Allocated to {len(targets)} symbols")
self.log(f"Risk: Applied {len(adjustments)} risk adjustments")
```

### Real-time Monitoring
- Portfolio metrics dashboard
- Risk limit monitoring
- Performance attribution
- Component health checks

## 📊 Data Sources Integration

### Free Data Sources
- **Yahoo Finance**: No API key required, major forex pairs and crypto
- **IEX Cloud**: Free tier available, US stocks and ETFs
- **Alpha Vantage**: 500 API calls/month free

### Custom Data Integration
```python
class MyCustomData(PythonData):
    def get_source(self, config, date, is_live):
        return SubscriptionDataSource(url, SubscriptionTransportMedium.REST)
    
    def reader(self, config, line, date, is_live):
        # Parse custom data format
        return custom_data_object
```

### Data Quality Controls
- Real-time data validation
- Missing data handling
- Outlier detection
- Data source failover

## 🔄 Deployment Architecture

### Development Environment
- Local backtesting with historical data
- Component unit testing
- Integration testing

### Staging Environment
- Paper trading with live data
- Performance validation
- Risk system testing

### Production Environment
- Live trading with real money
- Real-time monitoring
- Automated alerts and circuit breakers

### Infrastructure
```python
# Environment configuration
if algorithm.live_mode:
    # Production settings
    risk_tolerance = 0.02
    position_size = 0.1
else:
    # Backtest settings
    risk_tolerance = 0.05
    position_size = 0.2
```

## 🦄 Unicorn-Specific Enhancements

### Advanced ML Forecasting
- Multi-model ensemble approach
- Real-time model retraining
- Dynamic model weighting based on performance

### Risk-First Design
- Conservative position sizing
- Multiple layers of risk controls
- Drawdown protection

### Free Data Focus
- Yahoo Finance as primary data source
- Fallback data sources
- Cost-effective trading strategies

### Professional Architecture
- Institutional-grade separation of concerns
- Comprehensive testing framework
- Production-ready deployment

This architecture ensures maintainable, testable, and scalable algorithmic trading strategies that follow institutional best practices while remaining accessible through free data sources.
