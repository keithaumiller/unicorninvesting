"""
Alpha Vantage Minute Data Integration for LEAN Framework
=======================================================

This module provides a custom data source integration for Alpha Vantage's
free minute-level stock data, following LEAN framework best practices.

Free Tier Limits:
- 500 API calls per month
- 5 API calls per minute
- Supports stocks, forex, crypto

Usage in Algorithm:
    self.add_data(AlphaVantageMinuteData, "AAPL", Resolution.MINUTE)
"""

import json
import requests
from datetime import datetime, timedelta
from AlgorithmImports import *


class AlphaVantageMinuteData(PythonData):
    """
    Custom Data Source: Alpha Vantage Minute-Level Data
    
    Provides minute-level OHLCV data for stocks, ETFs, and some forex pairs.
    Automatically handles API rate limiting and data formatting.
    """
    
    # Class-level API configuration
    API_KEY = "demo"  # Replace with your actual API key
    BASE_URL = "https://www.alphavantage.co/query"
    RATE_LIMIT_DELAY = 12  # Seconds between API calls (5 calls/minute limit)
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """
        Define the data source URL for Alpha Vantage API.
        
        Args:
            config: Subscription configuration
            date: Date for the data request
            is_live_mode: Whether running in live trading mode
            
        Returns:
            SubscriptionDataSource with Alpha Vantage API URL
        """
        symbol = config.symbol.value
        
        if is_live_mode:
            # Live mode: Get latest intraday data
            url = f"{self.BASE_URL}?function=TIME_SERIES_INTRADAY&symbol={symbol}&interval=1min&apikey={self.API_KEY}"
            return SubscriptionDataSource(url, SubscriptionTransportMedium.REST, FileFormat.UnfoldingCollection)
        else:
            # Backtesting mode: Get historical data for specific date
            # Note: Alpha Vantage free tier only provides ~30 days of minute data
            url = f"{self.BASE_URL}?function=TIME_SERIES_INTRADAY&symbol={symbol}&interval=1min&outputsize=full&apikey={self.API_KEY}"
            return SubscriptionDataSource(url, SubscriptionTransportMedium.RemoteFile, FileFormat.UnfoldingCollection)
    
    def reader(self, config: SubscriptionDataConfig, line: str, date: datetime, is_live_mode: bool) -> BaseData:
        """
        Parse Alpha Vantage JSON response into LEAN-compatible data format.
        
        Args:
            config: Subscription configuration
            line: JSON response from Alpha Vantage API
            date: Current date being processed
            is_live_mode: Whether in live trading mode
            
        Returns:
            BaseData object with parsed minute data
        """
        try:
            # Parse JSON response
            data = json.loads(line)
            
            # Check for API errors
            if "Error Message" in data:
                self.debug(f"Alpha Vantage API Error: {data['Error Message']}")
                return None
                
            if "Note" in data:
                self.debug(f"Alpha Vantage Rate Limit: {data['Note']}")
                return None
            
            # Extract time series data
            time_series_key = "Time Series (1min)"
            if time_series_key not in data:
                self.debug(f"No minute data found for {config.symbol}")
                return None
            
            time_series = data[time_series_key]
            
            # Process each minute data point
            minute_data_list = []
            
            for timestamp_str, values in time_series.items():
                try:
                    # Parse timestamp
                    timestamp = datetime.strptime(timestamp_str, "%Y-%m-%d %H:%M:%S")
                    
                    # Create LEAN TradeBar
                    minute_bar = AlphaVantageMinuteData()
                    minute_bar.symbol = config.symbol
                    minute_bar.time = timestamp
                    minute_bar.end_time = timestamp + timedelta(minutes=1)
                    
                    # Parse OHLCV values
                    open_price = float(values["1. open"])
                    high_price = float(values["2. high"])
                    low_price = float(values["3. low"])
                    close_price = float(values["4. close"])
                    volume = int(values["5. volume"])
                    
                    # Set TradeBar properties
                    minute_bar.open = open_price
                    minute_bar.high = high_price
                    minute_bar.low = low_price
                    minute_bar.close = close_price
                    minute_bar.value = close_price  # Required by LEAN
                    minute_bar.volume = volume
                    
                    # Add to collection
                    minute_data_list.append(minute_bar)
                    
                except (ValueError, KeyError) as e:
                    self.debug(f"Error parsing minute data for {timestamp_str}: {e}")
                    continue
            
            # Return collection of minute bars
            if minute_data_list:
                # Sort by timestamp (most recent first for live mode)
                minute_data_list.sort(key=lambda x: x.time, reverse=is_live_mode)
                return minute_data_list[0] if minute_data_list else None
            
        except json.JSONDecodeError as e:
            self.debug(f"JSON parsing error: {e}")
            return None
        except Exception as e:
            self.debug(f"Unexpected error in Alpha Vantage reader: {e}")
            return None
        
        return None
    
    def debug(self, message: str):
        """Helper method for debug logging."""
        print(f"[AlphaVantageMinuteData] {message}")


class AlphaVantageForexData(AlphaVantageMinuteData):
    """
    Extended class for Alpha Vantage Forex minute data.
    
    Supports major forex pairs like EURUSD, GBPUSD, etc.
    """
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """Get forex minute data from Alpha Vantage."""
        symbol = config.symbol.value
        
        # Convert LEAN forex symbol (EURUSD) to Alpha Vantage format (EUR/USD)
        if len(symbol) == 6:
            from_currency = symbol[:3]
            to_currency = symbol[3:]
            forex_symbol = f"{from_currency}/{to_currency}"
        else:
            forex_symbol = symbol
        
        if is_live_mode:
            url = f"{self.BASE_URL}?function=FX_INTRADAY&from_symbol={from_currency}&to_symbol={to_currency}&interval=1min&apikey={self.API_KEY}"
        else:
            url = f"{self.BASE_URL}?function=FX_INTRADAY&from_symbol={from_currency}&to_symbol={to_currency}&interval=1min&outputsize=full&apikey={self.API_KEY}"
        
        return SubscriptionDataSource(url, SubscriptionTransportMedium.RemoteFile, FileFormat.UnfoldingCollection)


class AlphaVantageCryptoData(AlphaVantageMinuteData):
    """
    Extended class for Alpha Vantage Crypto minute data.
    
    Supports major cryptocurrencies like BTC, ETH, etc.
    """
    
    def get_source(self, config: SubscriptionDataConfig, date: datetime, is_live_mode: bool) -> SubscriptionDataSource:
        """Get crypto minute data from Alpha Vantage."""
        symbol = config.symbol.value
        
        # Extract crypto symbol (ETH from ETHUSD)
        if symbol.endswith("USD"):
            crypto_symbol = symbol[:-3]
            market = "USD"
        else:
            crypto_symbol = symbol
            market = "USD"  # Default to USD market
        
        if is_live_mode:
            url = f"{self.BASE_URL}?function=CRYPTO_INTRADAY&symbol={crypto_symbol}&market={market}&interval=1min&apikey={self.API_KEY}"
        else:
            url = f"{self.BASE_URL}?function=CRYPTO_INTRADAY&symbol={crypto_symbol}&market={market}&interval=1min&outputsize=full&apikey={self.API_KEY}"
        
        return SubscriptionDataSource(url, SubscriptionTransportMedium.RemoteFile, FileFormat.UnfoldingCollection)
