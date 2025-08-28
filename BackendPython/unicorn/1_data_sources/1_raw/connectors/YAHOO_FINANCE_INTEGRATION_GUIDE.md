# Yahoo Finance Integration Guide for LEAN Framework

## Overview

Yahoo Finance provides free, reliable minute-level trading data without requiring API keys. This makes it an excellent choice for development, backtesting, and small-scale live trading.

## Why Yahoo Finance?

### Advantages
- **No API Key Required** - Start using immediately
- **Free Unlimited Access** - No monthly limits (with reasonable rate limiting)
- **Comprehensive Coverage** - US stocks, ETFs, major forex pairs, cryptocurrencies
- **Real-time Data** - Up-to-date minute-level data
- **Reliable Infrastructure** - Backed by Yahoo's robust systems
- **Easy Integration** - Simple REST API with JSON responses

### Limitations
- **Unofficial API** - Could change without notice (but stable for years)
- **Rate Limiting** - Generous but undocumented limits
- **Recent Data Only** - 1-minute data limited to last 30 days for backtesting
- **US Market Focus** - Limited international market coverage

## Quick Start

### 1. Add Yahoo Finance Data Source

```python
from YahooFinanceMinuteData import YahooFinanceMinuteData

class MyAlgorithm(QCAlgorithm):
    def initialize(self):
        # Add Yahoo Finance minute data
        self.aapl = self.add_data(YahooFinanceMinuteData, "AAPL", Resolution.MINUTE).symbol
        self.spy = self.add_data(YahooFinanceETFData, "SPY", Resolution.MINUTE).symbol
        self.eurusd = self.add_data(YahooFinanceForexData, "EURUSD", Resolution.MINUTE).symbol
```

### 2. Test Connectivity

Use the `YahooFinanceTestAlgorithm` to verify data is flowing correctly:

```python
# Simple test to verify Yahoo Finance is working
class TestYahoo(QCAlgorithm):
    def initialize(self):
        self.set_start_date(2024, 8, 20)
        self.set_end_date(2024, 8, 27)
        
        self.aapl = self.add_data(YahooFinanceMinuteData, "AAPL", Resolution.MINUTE).symbol
        self.data_count = 0
    
    def on_data(self, data):
        if data.contains_key(self.aapl):
            self.data_count += 1
            if self.data_count % 60 == 0:  # Log hourly
                self.debug(f"Yahoo AAPL: ${data[self.aapl].value:.2f}")
```

## Supported Assets

### Stocks (YahooFinanceMinuteData)
```python
# Major US stocks
stocks = ["AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "META", "NVDA"]
for symbol in stocks:
    self.add_data(YahooFinanceMinuteData, symbol, Resolution.MINUTE)
```

### ETFs (YahooFinanceETFData)
```python
# Popular ETFs
etfs = ["SPY", "QQQ", "IWM", "VTI", "VOO", "GLD", "TLT"]
for symbol in etfs:
    self.add_data(YahooFinanceETFData, symbol, Resolution.MINUTE)
```

### Forex (YahooFinanceForexData)
```python
# Major currency pairs (automatically converts to Yahoo format: EURUSD=X)
forex_pairs = ["EURUSD", "GBPUSD", "USDJPY", "USDCHF", "AUDUSD", "USDCAD"]
for pair in forex_pairs:
    self.add_data(YahooFinanceForexData, pair, Resolution.MINUTE)
```

### Cryptocurrencies (YahooFinanceCryptoData)
```python
# Major cryptocurrencies (automatically converts to Yahoo format: BTC-USD)
cryptos = ["BTCUSD", "ETHUSD", "ADAUSD", "DOTUSD"]
for crypto in cryptos:
    self.add_data(YahooFinanceCryptoData, crypto, Resolution.MINUTE)
```

## Best Practices

### 1. Rate Limiting
```python
class YahooFinanceAlgorithm(QCAlgorithm):
    def initialize(self):
        # Limit concurrent requests
        self.max_symbols = 10  # Don't exceed 10-15 symbols
        
        # Space out data requests
        self.symbols = []
        for i, symbol in enumerate(["AAPL", "MSFT", "GOOGL"]):
            # Add small delay between symbol additions
            self.schedule.on(self.date_rules.on(self.start_date + timedelta(seconds=i)), 
                           self.time_rules.at(9, 30), 
                           lambda: self.add_symbol(symbol))
```

### 2. Error Handling
```python
def initialize(self):
    try:
        self.aapl = self.add_data(YahooFinanceMinuteData, "AAPL", Resolution.MINUTE).symbol
        self.yahoo_enabled = True
        self.debug("Yahoo Finance connected successfully")
    except Exception as e:
        # Fallback to regular equity data
        self.aapl = self.add_equity("AAPL", Resolution.MINUTE).symbol
        self.yahoo_enabled = False
        self.debug(f"Yahoo Finance fallback: {e}")

def on_data(self, data):
    if not data.contains_key(self.aapl):
        return  # Skip if no data available
    
    # Process data safely
    price = data[self.aapl].value
    if price > 0:  # Validate price data
        self.process_signal(price)
```

### 3. Data Validation
```python
def on_data(self, data):
    for symbol in self.symbols:
        if data.contains_key(symbol):
            bar = data[symbol]
            
            # Validate OHLCV data
            if (bar.open > 0 and bar.high > 0 and bar.low > 0 and 
                bar.close > 0 and bar.high >= bar.low and
                bar.high >= bar.open and bar.high >= bar.close and
                bar.low <= bar.open and bar.low <= bar.close):
                
                # Data looks valid
                self.process_bar(symbol, bar)
            else:
                self.debug(f"Invalid bar data for {symbol}: OHLC={bar.open}/{bar.high}/{bar.low}/{bar.close}")
```

## Live Trading Configuration

### 1. Market Hours Awareness
```python
def initialize(self):
    # Yahoo Finance provides data during market hours
    self.schedule.on(self.date_rules.every_day("AAPL"), 
                    self.time_rules.before_market_close("AAPL", 5), 
                    self.end_of_day_processing)

def end_of_day_processing(self):
    # Process end-of-day with Yahoo Finance data
    self.debug(f"End of day portfolio value: ${self.portfolio.total_portfolio_value:,.2f}")
```

### 2. Pre-market and After-hours
```python
def initialize(self):
    # Note: Yahoo Finance provides limited pre/post market data
    self.schedule.on(self.date_rules.every_day(), 
                    self.time_rules.at(9, 25),  # 5 minutes before market open
                    self.pre_market_analysis)

def pre_market_analysis(self):
    # Yahoo data may be limited outside market hours
    self.debug("Pre-market analysis with available Yahoo data")
```

## Performance Optimization

### 1. Batch Data Requests
```python
def initialize(self):
    # Group related symbols together
    self.tech_stocks = ["AAPL", "MSFT", "GOOGL"]
    self.etfs = ["SPY", "QQQ"]
    
    # Add in batches to minimize Yahoo API calls
    for symbol in self.tech_stocks:
        self.add_data(YahooFinanceMinuteData, symbol, Resolution.MINUTE)
```

### 2. Caching Strategy
```python
def initialize(self):
    self.price_cache = {}
    self.cache_timeout = timedelta(minutes=1)

def on_data(self, data):
    # Cache recent prices to reduce API dependency
    for symbol in self.symbols:
        if data.contains_key(symbol):
            self.price_cache[symbol] = {
                'price': data[symbol].value,
                'time': self.time
            }
```

## Troubleshooting

### Common Issues

1. **No Data Received**
   ```python
   # Check if data is flowing
   def on_data(self, data):
       if not data.keys():
           self.debug(f"No data received at {self.time}")
           return
   ```

2. **Rate Limiting**
   ```python
   # Reduce symbol count if getting rate limited
   def initialize(self):
       # Start with fewer symbols
       self.symbols = ["AAPL", "SPY"]  # Instead of 20+ symbols
   ```

3. **Data Quality Issues**
   ```python
   # Validate data before using
   def on_data(self, data):
       for symbol in self.symbols:
           if data.contains_key(symbol):
               price = data[symbol].value
               if price <= 0 or price > 10000:  # Sanity check
                   self.debug(f"Suspicious price for {symbol}: ${price}")
                   continue
   ```

### Debug Logging
```python
def initialize(self):
    # Enable detailed logging for Yahoo Finance
    self.debug("Yahoo Finance algorithm starting...")
    self.data_points_received = 0

def on_data(self, data):
    self.data_points_received += len(data.keys())
    
    # Log every 100 data points
    if self.data_points_received % 100 == 0:
        self.debug(f"Received {self.data_points_received} Yahoo Finance data points")

def on_end_of_algorithm(self):
    self.debug(f"Total Yahoo Finance data points: {self.data_points_received}")
```

## Example Implementation

See `YahooFinanceMinuteAlgorithm.py` for a complete working example that includes:
- Multi-asset trading (stocks, ETFs, forex)
- Momentum-based strategy
- Risk management
- Performance tracking
- Error handling

## Next Steps

1. **Test with YahooFinanceTestAlgorithm** - Verify connectivity
2. **Start Small** - Begin with 2-3 symbols
3. **Monitor Performance** - Watch for rate limiting or data issues
4. **Scale Gradually** - Add more symbols as system proves stable
5. **Implement Fallbacks** - Always have backup data sources

Yahoo Finance provides an excellent free foundation for minute-level trading strategies in LEAN. The key is starting simple and scaling up as you validate the data quality and system performance.
