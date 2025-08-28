# LEAN Framework Data Source Integration Guide

## 🎯 Overview

This guide demonstrates how to integrate minute-level trading data from free sources into the LEAN framework, following established patterns and best practices.

## 📊 Recommended Free Data Sources

### 🥇 Top Free Minute-Level Data Providers

| Provider | Free Tier | Assets | API Calls | Best For |
|----------|-----------|--------|-----------|----------|
| **Alpha Vantage** | 500/month, 5/min | Stocks, Forex, Crypto | Rate limited | Comprehensive |
| **Yahoo Finance** | Unlimited* | Stocks, ETFs, Forex | Rate limited | US Markets |
| **IEX Cloud** | 50,000/month | US Stocks | Rate limited | US Equities |
| **Twelve Data** | 800/day | Stocks, Forex, Crypto | Daily limit | International |
| **Polygon.io** | 1,000/month | Stocks, Forex, Crypto | Limited free | Real-time |

*Rate limited but generous

### 🔗 API Endpoints for Minute Data

```bash
# Alpha Vantage - Stocks
https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=AAPL&interval=1min&apikey=YOUR_KEY

# Yahoo Finance - Stocks (unofficial)
https://query1.finance.yahoo.com/v8/finance/chart/AAPL?interval=1m&range=1d

# IEX Cloud - US Stocks
https://cloud.iexapis.com/stable/stock/AAPL/intraday-prices?token=YOUR_TOKEN

# Twelve Data - Multi-asset
https://api.twelvedata.com/time_series?symbol=AAPL&interval=1min&apikey=YOUR_KEY

# Polygon.io - Real-time
https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/minute/2024-01-01/2024-01-02?apikey=YOUR_KEY
```

## 🏗️ LEAN Framework Integration Pattern

### 1. Custom Data Class Structure

```python
class CustomMinuteData(PythonData):
    """
    Standard LEAN custom data pattern:
    1. get_source() - Define data source URL
    2. reader() - Parse data into LEAN format
    """
    
    def get_source(self, config, date, is_live_mode):
        # Return SubscriptionDataSource with API URL
        
    def reader(self, config, line, date, is_live_mode):
        # Parse JSON/CSV response into BaseData format
```

### 2. Data Source Configuration

```python
# In Algorithm.initialize():
self.add_data(AlphaVantageMinuteData, "AAPL", Resolution.MINUTE)
self.add_data(AlphaVantageForexData, "EURUSD", Resolution.MINUTE)  
self.add_data(AlphaVantageCryptoData, "BTCUSD", Resolution.MINUTE)
```

### 3. Data Processing

```python
def on_data(self, data):
    # Process custom minute data
    if "AAPL" in data:
        minute_bar = data["AAPL"]
        # Use minute_bar.open, .high, .low, .close, .volume
```

## 🔧 Implementation Steps

### Step 1: Create Custom Data Class

1. **Choose your data provider** (recommend Alpha Vantage for start)
2. **Implement PythonData interface**:
   - `get_source()`: API URL construction
   - `reader()`: JSON/CSV parsing
3. **Handle rate limiting** and error cases
4. **Test data format** matches LEAN expectations

### Step 2: Configure Algorithm

1. **Add data subscription** in `initialize()`
2. **Set appropriate resolution** (Minute for minute data)
3. **Handle multiple symbols** if needed
4. **Configure error handling** for API failures

### Step 3: Process Data

1. **Implement `on_data()`** to receive minute bars
2. **Extract OHLCV values** from custom data
3. **Implement trading logic** using minute data
4. **Monitor data quality** and gaps

### Step 4: Test and Validate

1. **Backtest with historical data**
2. **Check API rate limits** don't break execution
3. **Validate data accuracy** against known sources
4. **Test error scenarios** (network, API limits)

## 💡 Best Practices

### ✅ Do:
- **Cache API responses** to avoid duplicate calls
- **Implement rate limiting** to respect API limits
- **Handle API errors gracefully** (return None, log errors)
- **Use appropriate data types** (float for prices, int for volume)
- **Test thoroughly** with small datasets first
- **Monitor API usage** to stay within limits

### ❌ Don't:
- **Exceed API rate limits** (causes bans)
- **Ignore error handling** (crashes algorithm)
- **Mix resolutions inappropriately** (hour + minute without care)
- **Skip data validation** (causes calculation errors)
- **Forget timezone handling** (UTC vs market time)

## 🚀 Example Usage

### Quick Start with Alpha Vantage

```python
# 1. Get free API key from https://www.alphavantage.co/support/#api-key
# 2. Add to your algorithm:

class MyMinuteDataAlgorithm(QCAlgorithm):
    def initialize(self):
        # Set up algorithm
        self.set_start_date(2024, 8, 1)
        self.set_cash(10000)
        
        # Add Alpha Vantage minute data
        self.add_data(AlphaVantageMinuteData, "AAPL", Resolution.MINUTE)
        
    def on_data(self, data):
        if "AAPL" in data:
            minute_bar = data["AAPL"]
            
            # Simple strategy: Buy on green minute bars
            if minute_bar.close > minute_bar.open:
                if not self.portfolio.invested:
                    self.set_holdings("AAPL", 0.5)
```

### Advanced Multi-Source Integration

```python
# Combine multiple free sources:
self.add_data(AlphaVantageMinuteData, "AAPL", Resolution.MINUTE)  # Stocks
self.add_data(YahooFinanceData, "SPY", Resolution.MINUTE)        # ETFs  
self.add_data(IEXCloudData, "MSFT", Resolution.MINUTE)          # Alternative stocks
```

## 📈 Performance Considerations

### Rate Limiting Strategy

```python
class RateLimitedDataSource(PythonData):
    _last_call_time = {}
    _min_interval = 12  # seconds (5 calls/minute = 12 seconds)
    
    def get_source(self, config, date, is_live_mode):
        symbol = config.symbol.value
        now = time.time()
        
        # Check rate limiting
        if symbol in self._last_call_time:
            elapsed = now - self._last_call_time[symbol]
            if elapsed < self._min_interval:
                # Return cached source or delay
                time.sleep(self._min_interval - elapsed)
        
        self._last_call_time[symbol] = now
        return SubscriptionDataSource(url, SubscriptionTransportMedium.REST)
```

### Data Caching

```python
class CachedDataSource(PythonData):
    _cache = {}
    _cache_duration = 60  # seconds
    
    def reader(self, config, line, date, is_live_mode):
        cache_key = f"{config.symbol}_{date}"
        
        # Check cache first
        if cache_key in self._cache:
            cached_time, cached_data = self._cache[cache_key]
            if time.time() - cached_time < self._cache_duration:
                return cached_data
        
        # Parse new data and cache
        parsed_data = self.parse_data(line)
        self._cache[cache_key] = (time.time(), parsed_data)
        return parsed_data
```

## 🔍 Troubleshooting

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| No data received | API key invalid | Check API key configuration |
| Rate limit errors | Too many calls | Implement rate limiting |
| JSON parse errors | API response format changed | Update parsing logic |
| Data gaps | API downtime | Implement fallback sources |
| Memory issues | Data not cleaned up | Clear old cached data |

### Debug Tools

```python
# Enable debug logging
def reader(self, config, line, date, is_live_mode):
    self.debug(f"Processing {config.symbol} at {date}")
    self.debug(f"Raw data: {line[:100]}...")  # First 100 chars
    
    try:
        parsed = self.parse_data(line)
        self.debug(f"Parsed: OHLCV = {parsed.open}/{parsed.high}/{parsed.low}/{parsed.close}/{parsed.volume}")
        return parsed
    except Exception as e:
        self.debug(f"Parse error: {e}")
        return None
```

## 📚 Additional Resources

- **LEAN Documentation**: https://www.quantconnect.com/docs/v2
- **Custom Data Tutorial**: https://www.quantconnect.com/docs/v2/writing-algorithms/importing-data/streaming-data/custom-securities
- **Alpha Vantage Docs**: https://www.alphavantage.co/documentation/
- **Yahoo Finance (yfinance)**: https://pypi.org/project/yfinance/
- **IEX Cloud API**: https://iexcloud.io/docs/api/

## 🎯 Next Steps

1. **Choose your preferred free data provider**
2. **Get API key** (Alpha Vantage recommended for start)
3. **Implement custom data class** using provided templates
4. **Test with small dataset** and single symbol
5. **Scale to multiple symbols/assets** once working
6. **Monitor API usage** to stay within limits
7. **Consider paid tiers** when strategy scales

The LEAN framework's flexibility makes it easy to integrate any data source that provides time-series data. Start with Alpha Vantage's generous free tier and expand from there!
