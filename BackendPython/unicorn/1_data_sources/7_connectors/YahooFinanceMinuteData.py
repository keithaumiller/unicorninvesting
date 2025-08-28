"""
Yahoo Finance Minute Data Integration for LEAN Framework
=======================================================

Alternative free data source using Yahoo Finance's public API.
No API key required, but rate limited.

Advantages:
- No API key required
- Good coverage of US stocks and ETFs
- Reliable and fast
- Real-time and historical data

Limitations:
- Unofficial API (could change)
- Rate limiting (generous but not documented)
- Limited to stocks, ETFs, some forex/crypto
- 1-minute data limited to recent periods
"""

import json
import requests
from datetime import datetime, timedelta
from AlgorithmImports import *


class YahooFinanceMinuteData(PythonData):
    """
    Yahoo Finance Minute Data Source
    
    Free alternative to Alpha Vantage for US stocks and ETFs.
    No API key required but rate limited.
    """
    
    BASE_URL = "https://query1.finance.yahoo.com/v8/finance/chart"
    RATE_LIMIT_DELAY = 1  # 1 second between requests (conservative)
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """
        Get Yahoo Finance chart data URL.
        
        Args:
            config: Subscription configuration
            date: Date for data request
            is_live_mode: Whether in live trading mode
            
        Returns:
            SubscriptionDataSource with Yahoo Finance URL
        """
        symbol = config.symbol.value
        
        if is_live_mode:
            # Live mode: Get latest 1 day of minute data
            range_param = "1d"
            interval = "1m"
        else:
            # Backtesting: Get 5 days of minute data
            range_param = "5d"
            interval = "1m"
        
        # Yahoo Finance URL format
        url = f"{self.BASE_URL}/{symbol}?interval={interval}&range={range_param}"
        
        return SubscriptionDataSource(url, SubscriptionTransportMedium.REST, FileFormat.UnfoldingCollection)
    
    def reader(self, config: SubscriptionDataConfig, line: str, date: datetime, is_live_mode: bool) -> BaseData:
        """
        Parse Yahoo Finance JSON response.
        
        Response format:
        {
            "chart": {
                "result": [{
                    "timestamp": [unix_timestamps],
                    "indicators": {
                        "quote": [{
                            "open": [prices],
                            "high": [prices], 
                            "low": [prices],
                            "close": [prices],
                            "volume": [volumes]
                        }]
                    }
                }]
            }
        }
        """
        try:
            # Parse JSON response
            data = json.loads(line)
            
            # Check for errors
            if "chart" not in data:
                self.debug(f"No chart data for {config.symbol}")
                return None
            
            chart = data["chart"]
            if "error" in chart and chart["error"]:
                self.debug(f"Yahoo Finance error: {chart['error']}")
                return None
            
            if not chart.get("result") or len(chart["result"]) == 0:
                self.debug(f"No results for {config.symbol}")
                return None
            
            result = chart["result"][0]
            
            # Extract timestamps and indicators
            timestamps = result.get("timestamp", [])
            indicators = result.get("indicators", {})
            quote_data = indicators.get("quote", [{}])[0] if indicators.get("quote") else {}
            
            if not timestamps or not quote_data:
                self.debug(f"Missing data for {config.symbol}")
                return None
            
            # Get price arrays
            opens = quote_data.get("open", [])
            highs = quote_data.get("high", [])
            lows = quote_data.get("low", [])
            closes = quote_data.get("close", [])
            volumes = quote_data.get("volume", [])
            
            # Find the most recent complete minute bar
            minute_bars = []
            
            for i, timestamp in enumerate(timestamps):
                try:
                    # Skip incomplete data points
                    if (i >= len(opens) or i >= len(highs) or i >= len(lows) or 
                        i >= len(closes) or i >= len(volumes)):
                        continue
                    
                    # Skip null values
                    if (opens[i] is None or highs[i] is None or lows[i] is None or 
                        closes[i] is None or volumes[i] is None):
                        continue
                    
                    # Convert timestamp to datetime
                    bar_time = datetime.fromtimestamp(timestamp)
                    
                    # Create minute bar
                    minute_bar = YahooFinanceMinuteData()
                    minute_bar.symbol = config.symbol
                    minute_bar.time = bar_time
                    minute_bar.end_time = bar_time + timedelta(minutes=1)
                    
                    # Set OHLCV data
                    minute_bar.open = float(opens[i])
                    minute_bar.high = float(highs[i])
                    minute_bar.low = float(lows[i])
                    minute_bar.close = float(closes[i])
                    minute_bar.value = float(closes[i])  # Required by LEAN
                    minute_bar.volume = int(volumes[i] or 0)
                    
                    minute_bars.append(minute_bar)
                    
                except (ValueError, TypeError, IndexError) as e:
                    self.debug(f"Error processing bar {i} for {config.symbol}: {e}")
                    continue
            
            # Return most recent bar
            if minute_bars:
                # Sort by time and return most recent
                minute_bars.sort(key=lambda x: x.time, reverse=True)
                latest_bar = minute_bars[0]
                
                self.debug(f"Yahoo Finance {config.symbol}: "
                          f"Time={latest_bar.time}, "
                          f"OHLCV={latest_bar.open:.2f}/{latest_bar.high:.2f}/"
                          f"{latest_bar.low:.2f}/{latest_bar.close:.2f}/{latest_bar.volume}")
                
                return latest_bar
            
        except json.JSONDecodeError as e:
            self.debug(f"JSON parse error for {config.symbol}: {e}")
            return None
        except Exception as e:
            self.debug(f"Unexpected error processing {config.symbol}: {e}")
            return None
        
        return None
    
    def debug(self, message: str):
        """Helper method for debug logging."""
        print(f"[YahooFinanceMinuteData] {message}")


class YahooFinanceETFData(YahooFinanceMinuteData):
    """
    Yahoo Finance ETF data - same as stock data but specialized for ETFs.
    """
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """ETFs use same API as stocks."""
        return super().get_source(config, date, is_live_mode)


class YahooFinanceForexData(YahooFinanceMinuteData):
    """
    Yahoo Finance Forex data for major pairs.
    
    Supports format like: EURUSD=X, GBPUSD=X, etc.
    """
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """
        Forex symbols in Yahoo Finance need '=X' suffix.
        """
        symbol = config.symbol.value
        
        # Convert LEAN forex symbol (EURUSD) to Yahoo format (EURUSD=X)
        if not symbol.endswith("=X"):
            symbol = f"{symbol}=X"
        
        if is_live_mode:
            range_param = "1d"
            interval = "1m"
        else:
            range_param = "5d"
            interval = "1m"
        
        url = f"{self.BASE_URL}/{symbol}?interval={interval}&range={range_param}"
        return SubscriptionDataSource(url, SubscriptionTransportMedium.REST, FileFormat.UnfoldingCollection)


class YahooFinanceCryptoData(YahooFinanceMinuteData):
    """
    Yahoo Finance Crypto data for major cryptocurrencies.
    
    Supports format like: BTC-USD, ETH-USD, etc.
    """
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """
        Crypto symbols in Yahoo Finance use '-USD' format.
        """
        symbol = config.symbol.value
        
        # Convert LEAN crypto symbol (BTCUSD) to Yahoo format (BTC-USD)
        if symbol.endswith("USD") and not symbol.endswith("-USD"):
            crypto_part = symbol[:-3]  # Remove 'USD'
            symbol = f"{crypto_part}-USD"
        
        if is_live_mode:
            range_param = "1d"
            interval = "1m"
        else:
            range_param = "5d"
            interval = "1m"
        
        url = f"{self.BASE_URL}/{symbol}?interval={interval}&range={range_param}"
        return SubscriptionDataSource(url, SubscriptionTransportMedium.REST, FileFormat.UnfoldingCollection)


# Example usage
if __name__ == "__main__":
    # Test Yahoo Finance data source
    print("Testing Yahoo Finance data sources...")
    
    # This would normally be called by LEAN framework
    config = type('obj', (object,), {
        'symbol': type('obj', (object,), {'value': 'AAPL'})()
    })()
    
    yahoo_data = YahooFinanceMinuteData()
    source = yahoo_data.get_source(config, datetime.now(), True)
    print(f"Yahoo Finance URL: {source.source}")
    
    # Test forex
    forex_config = type('obj', (object,), {
        'symbol': type('obj', (object,), {'value': 'EURUSD'})()
    })()
    
    yahoo_forex = YahooFinanceForexData()
    forex_source = yahoo_forex.get_source(forex_config, datetime.now(), True)
    print(f"Yahoo Forex URL: {forex_source.source}")
    
    # Test crypto
    crypto_config = type('obj', (object,), {
        'symbol': type('obj', (object,), {'value': 'BTCUSD'})()
    })()
    
    yahoo_crypto = YahooFinanceCryptoData()
    crypto_source = yahoo_crypto.get_source(crypto_config, datetime.now(), True)
    print(f"Yahoo Crypto URL: {crypto_source.source}")
