#!/usr/bin/env python3
"""
Test Yahoo Finance connector with ETH (Ethereum) data
"""

import sys
import os
import json
import requests
from datetime import datetime, timedelta

# Add the parent directory to path so we can import the module
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

def test_yahoo_finance_eth():
    """Test Yahoo Finance API with ETH-USD directly"""
    
    # Yahoo Finance URL for ETH-USD
    base_url = "https://query1.finance.yahoo.com/v8/finance/chart"
    symbol = "ETH-USD"
    interval = "1m"
    range_param = "1d"
    
    url = f"{base_url}/{symbol}?interval={interval}&range={range_param}"
    
    print(f"Testing Yahoo Finance API for {symbol}")
    print(f"URL: {url}")
    print("-" * 60)
    
    try:
        # Make the request
        response = requests.get(url, timeout=10)
        
        print(f"Status Code: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            
            # Check for chart data
            if "chart" in data and data["chart"]["result"]:
                result = data["chart"]["result"][0]
                
                # Extract basic info
                meta = result.get("meta", {})
                print(f"Symbol: {meta.get('symbol', 'N/A')}")
                print(f"Currency: {meta.get('currency', 'N/A')}")
                print(f"Exchange: {meta.get('exchangeName', 'N/A')}")
                print(f"Current Price: ${meta.get('regularMarketPrice', 'N/A')}")
                print(f"Previous Close: ${meta.get('previousClose', 'N/A')}")
                
                # Extract timestamps and price data
                timestamps = result.get("timestamp", [])
                indicators = result.get("indicators", {})
                quote_data = indicators.get("quote", [{}])[0] if indicators.get("quote") else {}
                
                if timestamps and quote_data:
                    opens = quote_data.get("open", [])
                    highs = quote_data.get("high", [])
                    lows = quote_data.get("low", [])
                    closes = quote_data.get("close", [])
                    volumes = quote_data.get("volume", [])
                    
                    print(f"\nData Points Available: {len(timestamps)}")
                    
                    # Show last 5 data points
                    print("\nLast 5 Minute Bars:")
                    print("Time                 | Open     | High     | Low      | Close    | Volume")
                    print("-" * 80)
                    
                    for i in range(max(0, len(timestamps) - 5), len(timestamps)):
                        if (i < len(opens) and i < len(highs) and i < len(lows) and 
                            i < len(closes) and i < len(volumes)):
                            
                            try:
                                bar_time = datetime.fromtimestamp(timestamps[i])
                                open_price = opens[i] if opens[i] is not None else 0
                                high_price = highs[i] if highs[i] is not None else 0
                                low_price = lows[i] if lows[i] is not None else 0
                                close_price = closes[i] if closes[i] is not None else 0
                                volume = volumes[i] if volumes[i] is not None else 0
                                
                                print(f"{bar_time.strftime('%Y-%m-%d %H:%M:%S')} | "
                                      f"{open_price:8.2f} | {high_price:8.2f} | {low_price:8.2f} | "
                                      f"{close_price:8.2f} | {volume:8.0f}")
                            except Exception as e:
                                print(f"Error processing bar {i}: {e}")
                    
                    return True
                else:
                    print("No price data available")
                    return False
            else:
                print("No chart data in response")
                if "chart" in data:
                    print(f"Chart errors: {data['chart'].get('error', 'None')}")
                return False
        else:
            print(f"HTTP Error: {response.status_code}")
            print(f"Response: {response.text[:500]}")
            return False
            
    except requests.exceptions.RequestException as e:
        print(f"Request error: {e}")
        return False
    except json.JSONDecodeError as e:
        print(f"JSON decode error: {e}")
        return False
    except Exception as e:
        print(f"Unexpected error: {e}")
        return False

def test_connector_classes():
    """Test the actual connector classes"""
    
    print("\n" + "="*60)
    print("Testing Connector Classes")
    print("="*60)
    
    try:
        # Mock config object for testing
        class MockSymbol:
            def __init__(self, value):
                self.value = value
        
        class MockConfig:
            def __init__(self, symbol_value):
                self.symbol = MockSymbol(symbol_value)
        
        # Test crypto data source
        from YahooFinanceMinuteData import YahooFinanceCryptoData
        
        crypto_data = YahooFinanceCryptoData()
        eth_config = MockConfig("ETHUSD")
        
        source = crypto_data.get_source(eth_config, datetime.now(), True)
        
        print(f"Crypto ETH URL: {source.source}")
        
        # Test with direct ETH-USD format
        eth_usd_config = MockConfig("ETH-USD")
        source2 = crypto_data.get_source(eth_usd_config, datetime.now(), True)
        
        print(f"Direct ETH-USD URL: {source2.source}")
        
        return True
        
    except ImportError as e:
        print(f"Import error: {e}")
        print("Note: This is expected when not running in LEAN framework")
        return False
    except Exception as e:
        print(f"Error testing connector: {e}")
        return False

if __name__ == "__main__":
    print("Yahoo Finance ETH Data Test")
    print("="*60)
    
    # Test 1: Direct API call
    success1 = test_yahoo_finance_eth()
    
    # Test 2: Connector classes (may fail outside LEAN framework)
    success2 = test_connector_classes()
    
    print("\n" + "="*60)
    print("Test Summary:")
    print(f"Direct API Test: {'✅ PASSED' if success1 else '❌ FAILED'}")
    print(f"Connector Test: {'✅ PASSED' if success2 else '❌ FAILED (Expected outside LEAN)'}")
    
    if success1:
        print("\n✅ Yahoo Finance API is working correctly for ETH data!")
        print("The connector should work properly within the LEAN framework.")
    else:
        print("\n❌ Yahoo Finance API test failed. Check your internet connection.")
