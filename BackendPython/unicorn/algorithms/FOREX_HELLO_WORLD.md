# Basic Forex Trading Algorithm - Hello World Guide

## What You Minimally Need to Know

### 1. Algorithm Structure
Every LEAN algorithm needs these core components:

```python
from AlgorithmImports import *

class YourAlgorithm(QCAlgorithm):
    def initialize(self):
        # Setup: cash, dates, symbols, indicators
        pass
    
    def on_data(self, data):
        # Trading logic: buy/sell decisions
        pass
```

### 2. Forex Symbol Conventions in LEAN
LEAN uses specific symbol formats for your requested pairs:

| Your Request | LEAN Symbol | Description |
|-------------|-------------|-------------|
| EUR/USD     | "EURUSD"    | Euro to US Dollar |
| USD/EUR     | Not direct* | Use EURUSD and inverse logic |
| JPY/USD     | Not direct* | Use USDJPY and inverse logic |
| USD/JPY     | "USDJPY"    | US Dollar to Japanese Yen |
| USD/CNY     | "USDCNH"    | US Dollar to Chinese Yuan (offshore) |
| CNY/USD     | Not direct* | Use USDCNH and inverse logic |
| ETH         | "ETHUSD"    | Ethereum to US Dollar (crypto) |

*Note: For inverse pairs, you trade the available pair and use inverse logic (1/price)

### 3. Basic Setup Code

```python
def initialize(self):
    # Set starting cash
    self.set_cash(100000)  # $100,000
    
    # Set backtest period
    self.set_start_date(2023, 1, 1)
    self.set_end_date(2023, 12, 31)
    
    # Add forex pairs
    self.eurusd = self.add_forex("EURUSD", Resolution.HOUR).symbol
    self.usdjpy = self.add_forex("USDJPY", Resolution.HOUR).symbol
    self.usdcnh = self.add_forex("USDCNH", Resolution.HOUR).symbol
    
    # Add crypto
    self.ethusd = self.add_crypto("ETHUSD", Resolution.HOUR).symbol
```

### 4. Basic Trading Logic

```python
def on_data(self, data):
    # Check if we have data
    if "EURUSD" not in data:
        return
    
    # Get current price
    price = data["EURUSD"].close
    
    # Simple buy condition
    if not self.portfolio["EURUSD"].invested:
        self.set_holdings("EURUSD", 0.25)  # 25% of portfolio
        self.log(f"Bought EURUSD at {price}")
```

### 5. Key Methods You'll Use

- `self.add_forex(symbol, resolution)` - Add forex pair
- `self.add_crypto(symbol, resolution)` - Add cryptocurrency
- `self.set_holdings(symbol, percentage)` - Set position size
- `self.liquidate(symbol)` - Close position
- `self.portfolio[symbol].invested` - Check if you have position
- `self.log(message)` - Print to algorithm log

### 6. Resolutions Available

- `Resolution.TICK` - Every price tick
- `Resolution.SECOND` - Every second
- `Resolution.MINUTE` - Every minute
- `Resolution.HOUR` - Every hour
- `Resolution.DAILY` - Every day

### 7. Handling Inverse Pairs

For pairs like USD/EUR (inverse of EURUSD):

```python
# If you want to "buy USD/EUR", you actually sell EURUSD
if want_to_buy_usd_eur:
    self.set_holdings("EURUSD", -0.25)  # Negative = short EURUSD
```

## Running Your Algorithm

### Option 1: With LEAN CLI (Recommended)
```bash
cd /workspaces/unicorninvesting/BackendPython/Lean
lean backtest --algorithm-location ../unicorn/algorithms/unicorn_basic_forex_algorithm.py
```

### Option 2: Direct Python Execution
```bash
cd /workspaces/unicorninvesting/BackendPython/Lean
dotnet run --project Launcher -- --algorithm-location ../unicorn/algorithms/unicorn_basic_forex_algorithm.py
```

## What the Hello World Algorithm Does

1. **Setup**: 
   - $100,000 starting capital
   - 1-year backtest period (2023)
   - Adds EURUSD, USDJPY, USDCNH, and ETHUSD

2. **Strategy**:
   - Uses simple moving average crossover
   - 20-hour fast SMA vs 50-hour slow SMA
   - Buys when fast > slow (golden cross)
   - Sells when fast < slow (death cross)

3. **Risk Management**:
   - Maximum 10% position size per currency
   - 4-hour cooldown between trades
   - Automatic position sizing

4. **Logging**:
   - Trade signals with prices
   - Daily portfolio value
   - Final performance summary

## Next Steps

1. **Run the basic algorithm** to see how it works
2. **Modify the strategy** (change SMA periods, position sizes)
3. **Add more pairs** (GBP, AUD, CAD, etc.)
4. **Implement risk management** (stop losses, take profits)
5. **Add more sophisticated indicators** (RSI, MACD, Bollinger Bands)

## Common Gotchas

1. **Symbol Names**: Use exact LEAN conventions ("EURUSD", not "EUR/USD")
2. **Data Availability**: Not all brokers have all currency pairs
3. **Time Zones**: Forex trades 24/5, be aware of market hours
4. **Leverage**: Forex typically has high leverage (be careful!)
5. **Spread Costs**: Consider bid/ask spreads in your strategy

## Files Created

- `unicorn_basic_forex_algorithm.py` - The main trading algorithm
- `config.json` - LEAN configuration for running the algorithm
- This documentation file

The algorithm is ready to run and will give you a foundation for more sophisticated forex trading strategies!
