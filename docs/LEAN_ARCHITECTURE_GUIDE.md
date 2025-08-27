# LEAN Engine Architecture & Portfolio Management Guide

## 🏗️ LEAN Storage Architecture & Data Separation

### **Core Entities & Their Relationships**

```
LEAN Engine
├── Algorithms (Your Trading Logic)
├── Portfolios (Account State)
├── Securities (Instruments/Symbols)
├── Data Feeds (Market Data)
├── Orders (Transaction Management)
├── Results (Performance Tracking)
└── Storage (File-based & Database)
```

---

## 📊 **Key Entities Explained**

### **1. Algorithm**
Your trading strategy - the brain of the operation
```python
class MyAlgorithm(QCAlgorithm):
    def initialize(self):
        # Setup: date range, cash, securities
        self.set_start_date(2023, 1, 1)
        self.set_cash(100000)
        self.add_equity("SPY", Resolution.MINUTE)
    
    def on_data(self, data):
        # Trading logic executed on each data point
        if not self.portfolio.invested:
            self.set_holdings("SPY", 0.5)
```

### **2. Portfolio**
Represents your account state and holdings
```python
# Portfolio contains:
- self.portfolio.cash                 # Available cash
- self.portfolio.total_portfolio_value # Total account value
- self.portfolio["SPY"].quantity      # Shares of SPY held
- self.portfolio["SPY"].market_value  # Current value of SPY position
- self.portfolio["SPY"].unrealized_profit # P&L on position
```

### **3. Securities**
Individual instruments (stocks, options, futures, etc.)
```python
# Securities contain:
- Security symbol and market
- Price and volume data
- Corporate actions (splits, dividends)
- Trading hours and settlement
- Margin requirements
```

### **4. Orders**
Transaction management system
```python
# Order types:
- Market orders (immediate execution)
- Limit orders (specific price)
- Stop orders (risk management)
- Combo orders (multi-leg strategies)
```

---

## 🔒 **Development vs Live Trading Separation**

### **Environment-Based Isolation**

LEAN uses **environment configurations** to completely separate development and live trading:

#### **1. Backtesting Environment**
```json
"environment": "backtesting"
{
  "live-mode": false,
  "data-feed-handler": "FileSystemDataFeed",
  "transaction-handler": "BacktestingTransactionHandler",
  "result-handler": "BacktestingResultHandler"
}
```
- **Data Source**: Historical files (`Data/` directory)
- **Orders**: Simulated - NO real money involved
- **Storage**: Local files for results
- **Speed**: Fast execution (can run years in minutes)

#### **2. Paper Trading Environment**
```json
"environment": "live-paper"
{
  "live-mode": true,
  "live-mode-brokerage": "PaperBrokerage",
  "transaction-handler": "BacktestingTransactionHandler"
}
```
- **Data Source**: Live market data
- **Orders**: Simulated - NO real money involved
- **Storage**: Separate from live accounts
- **Speed**: Real-time execution

#### **3. Live Trading Environment**
```json
"environment": "live-interactive"
{
  "live-mode": true,
  "live-mode-brokerage": "InteractiveBrokersBrokerage",
  "transaction-handler": "BrokerageTransactionHandler"
}
```
- **Data Source**: Live market data
- **Orders**: REAL trades with REAL money
- **Storage**: Connected to actual brokerage account
- **Speed**: Real-time execution

---

## 🗂️ **Data Storage Structure**

### **File-Based Storage**
```
/Data/
├── equity/usa/minute/spy/          # SPY minute data
├── forex/oanda/minute/eurusd/      # EURUSD forex data
├── crypto/coinbase/hour/btcusd/    # Bitcoin hourly data
├── option/usa/minute/spw/          # SPX options data
└── future/cme/daily/es/            # E-mini S&P futures
```

### **Results Storage**
```
/Results/
├── backtests/
│   ├── algorithm_20230827_backtest.json
│   └── performance_charts.html
├── live/
│   ├── algorithm_20230827_live.json
│   └── trade_log.csv
└── paper/
    ├── algorithm_20230827_paper.json
    └── simulation_results.json
```

### **Configuration Isolation**
```
/Config/
├── backtest_config.json     # Historical data settings
├── paper_config.json        # Paper trading settings  
├── live_config.json         # Live trading settings
└── credentials/
    ├── ibkr_paper.json      # Paper account credentials
    └── ibkr_live.json       # Live account credentials
```

---

## 🛡️ **Safety Mechanisms**

### **1. Account Separation**
```python
# Different account configurations prevent crossover
PAPER_ACCOUNT = "DU123456"    # Demo/Paper account
LIVE_ACCOUNT = "U987654"      # Real money account
```

### **2. Environment Variables**
```bash
# Development environment
export LEAN_ENVIRONMENT="paper"
export IBKR_ACCOUNT="DU123456"
export TRADING_MODE="paper"

# Production environment  
export LEAN_ENVIRONMENT="live"
export IBKR_ACCOUNT="U987654"
export TRADING_MODE="live"
```

### **3. Order Size Limits**
```python
class SafetyManager:
    def validate_order(self, order):
        if self.environment == "paper":
            return True  # No limits in paper
        
        if self.environment == "live":
            if order.quantity > MAX_POSITION_SIZE:
                return False  # Prevent large orders
            
            if self.daily_trades > MAX_DAILY_TRADES:
                return False  # Prevent over-trading
```

---

## 🧪 **Algorithm Development Workflow**

### **Phase 1: Research & Backtesting**
```python
# 1. Historical data analysis
self.set_start_date(2020, 1, 1)
self.set_end_date(2023, 1, 1)
self.set_cash(100000)

# 2. Strategy development
def on_data(self, data):
    # Your trading logic here
    pass

# 3. Performance evaluation
# Results saved to: /Results/backtests/
```

### **Phase 2: Paper Trading Validation**
```python
# Switch to paper trading environment
# config.json: "environment": "live-paper"

# Same algorithm code, but with:
# - Live market data
# - Real-time execution
# - No real money risk
# Results saved to: /Results/paper/
```

### **Phase 3: Live Trading Deployment**
```python
# Switch to live trading environment  
# config.json: "environment": "live-interactive"

# Same algorithm code, but with:
# - Live market data
# - Real money execution
# - Full risk management
# Results saved to: /Results/live/
```

---

## 📋 **Portfolio Management Examples**

### **Development Portfolio Setup**
```python
class DevelopmentPortfolio(QCAlgorithm):
    def initialize(self):
        # Test with smaller position sizes
        self.set_cash(10000)  # Start with $10k for testing
        
        # Add multiple securities for diversification testing
        self.spy = self.add_equity("SPY", Resolution.MINUTE)
        self.qqq = self.add_equity("QQQ", Resolution.MINUTE) 
        self.gld = self.add_equity("GLD", Resolution.MINUTE)
        
        # Conservative position sizing for testing
        self.max_position_percent = 0.1  # Max 10% per position
        
    def on_data(self, data):
        # Test your strategy logic
        if self.time.hour == 10 and self.time.minute == 0:
            for symbol in ["SPY", "QQQ", "GLD"]:
                if not self.portfolio[symbol].invested:
                    self.set_holdings(symbol, self.max_position_percent)
```

### **Production Portfolio Setup**
```python
class ProductionPortfolio(QCAlgorithm):
    def initialize(self):
        # Production capital allocation
        self.set_cash(1000000)  # $1M production capital
        
        # Full universe of securities
        self.universe_settings.resolution = Resolution.MINUTE
        self.add_universe(self.select_securities)
        
        # Risk management parameters
        self.max_position_percent = 0.05    # Max 5% per position
        self.max_portfolio_risk = 0.02      # Max 2% daily portfolio risk
        self.stop_loss_percent = 0.03       # 3% stop loss
        
        # Risk manager
        self.risk_manager = ProductionRiskManager()
        
    def on_data(self, data):
        # Production trading logic with full risk controls
        for symbol in self.active_securities.keys:
            if self.risk_manager.validate_trade(symbol, data):
                # Execute trades with full validation
                pass
```

---

## 🔄 **Data Synchronization**

### **Backtest → Paper → Live Pipeline**
```python
# 1. Develop and test in backtest
def backtest_algorithm():
    algorithm = MyStrategy()
    # Run against historical data
    # Validate performance metrics
    return results

# 2. Validate in paper trading
def paper_trading():
    algorithm = MyStrategy()  # Same code!
    # Run against live data, simulated trades
    # Validate real-time behavior
    return results

# 3. Deploy to live trading
def live_trading():
    algorithm = MyStrategy()  # Same code!
    # Run against live data, real trades
    # Monitor with full risk management
    return results
```

### **Portfolio State Synchronization**
```python
class PortfolioSync:
    def sync_from_brokerage(self):
        """Sync current positions from IBKR to LEAN"""
        ibkr_positions = self.ibkr.get_positions()
        
        for position in ibkr_positions:
            self.lean_portfolio.update_position(
                symbol=position.symbol,
                quantity=position.quantity,
                avg_cost=position.avg_cost
            )
    
    def validate_consistency(self):
        """Ensure LEAN and IBKR portfolios match"""
        lean_positions = self.lean_portfolio.get_positions()
        ibkr_positions = self.ibkr.get_positions()
        
        for symbol in lean_positions:
            assert lean_positions[symbol].quantity == ibkr_positions[symbol].quantity
```

---

## 🎯 **Key Takeaways for Safe Development**

### **✅ Best Practices**

1. **Always Start with Backtesting**
   - Use historical data to test your logic
   - No risk to capital
   - Fast iteration and debugging

2. **Validate with Paper Trading**
   - Test with live data but simulated trades
   - Validate real-time behavior
   - Check for timing and execution issues

3. **Deploy Gradually to Live Trading**
   - Start with small position sizes
   - Monitor performance closely
   - Scale up only after validation

4. **Use Separate Accounts**
   - Paper account for testing (DU prefix)
   - Live account for production (U prefix)
   - Never mix the two

5. **Implement Risk Controls**
   - Position size limits
   - Daily loss limits
   - Trade frequency limits
   - Real-time monitoring

### **🚫 Common Pitfalls to Avoid**

- Don't deploy untested algorithms to live trading
- Don't use live accounts for development
- Don't skip paper trading validation
- Don't ignore risk management
- Don't trade without monitoring systems

LEAN's architecture ensures complete separation between development and production environments, giving you confidence to innovate while protecting your capital!
