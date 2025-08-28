"""
ETH Data Collector
Unicorn Investing Platform

Standalone ETH data collection script that works with our organized data directory structure.
Supports multiple data providers and saves data in a consistent format.
"""

import yfinance as yf
import pandas as pd
import os
from datetime import datetime, timedelta
import logging

class ETHDataCollector:
    """
    Collects ETH data from various sources and saves to organized directory structure.
    """
    
    def __init__(self, base_data_dir="data"):
        """
        Initialize the ETH data collector.
        
        Args:
            base_data_dir: Base directory for data storage
        """
        self.base_data_dir = base_data_dir
        self.logger = logging.getLogger(__name__)
        
        # Ensure data directories exist
        self.providers = ['yahoo_finance', 'alpha_vantage', 'interactive_brokers']
        for provider in self.providers:
            provider_dir = os.path.join(base_data_dir, provider, 'ETH')
            os.makedirs(provider_dir, exist_ok=True)
    
    def get_yahoo_finance_data(self, period='1d', interval='1m'):
        """
        Get ETH data from Yahoo Finance.
        
        Args:
            period: Data period (1d, 5d, 1mo, etc.)
            interval: Data interval (1m, 5m, 1h, etc.)
            
        Returns:
            DataFrame with ETH data
        """
        try:
            print(f"📊 Fetching ETH data from Yahoo Finance...")
            print(f"   Period: {period}, Interval: {interval}")
            
            # Create ticker object
            eth_ticker = yf.Ticker("ETH-USD")
            
            # Get historical data
            data = eth_ticker.history(period=period, interval=interval)
            
            if not data.empty:
                # Add metadata columns
                data['source'] = 'yahoo_finance'
                data['symbol'] = 'ETH-USD'
                data['provider'] = 'Yahoo Finance'
                
                # Rename columns to lowercase for consistency
                data.columns = [col.lower() for col in data.columns]
                
                print(f"✅ Retrieved {len(data)} records")
                print(f"📈 Price range: ${data['close'].min():.2f} - ${data['close'].max():.2f}")
                print(f"📅 Time range: {data.index.min()} to {data.index.max()}")
                
                return data
            else:
                print("❌ No data retrieved from Yahoo Finance")
                return pd.DataFrame()
                
        except Exception as e:
            print(f"❌ Error fetching Yahoo Finance data: {e}")
            return pd.DataFrame()
    
    def save_data(self, data, provider, asset='ETH'):
        """
        Save data to the organized directory structure.
        
        Args:
            data: DataFrame with market data
            provider: Data provider name
            asset: Asset symbol (default: ETH)
        """
        if data.empty:
            print("⚠️  No data to save")
            return None
        
        try:
            # Generate filename with timestamp
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            filename = f"{self.base_data_dir}/{provider}/{asset}/{asset}_{timestamp}.csv"
            
            # Save data
            data.to_csv(filename)
            
            # File info
            file_size = os.path.getsize(filename)
            print(f"✅ Data saved to: {filename}")
            print(f"📁 File size: {file_size:,} bytes ({file_size/1024:.1f} KB)")
            
            return filename
            
        except Exception as e:
            print(f"❌ Error saving data: {e}")
            return None
    
    def get_latest_price(self):
        """Get the latest ETH price for quick reference."""
        try:
            eth_ticker = yf.Ticker("ETH-USD")
            info = eth_ticker.info
            
            current_price = info.get('regularMarketPrice', 'N/A')
            prev_close = info.get('previousClose', 'N/A')
            
            if current_price != 'N/A' and prev_close != 'N/A':
                change = current_price - prev_close
                change_pct = (change / prev_close) * 100
                
                print(f"💰 Current ETH Price: ${current_price:.2f}")
                print(f"📊 Change: ${change:+.2f} ({change_pct:+.2f}%)")
            else:
                print(f"💰 Current ETH Price: ${current_price}")
                
        except Exception as e:
            print(f"❌ Error getting latest price: {e}")
    
    def collect_and_save_eth_data(self, period='1d', interval='1m'):
        """
        Complete workflow: collect ETH data and save to directory structure.
        
        Args:
            period: Data period
            interval: Data interval
            
        Returns:
            Filename of saved data or None if failed
        """
        print("🚀 ETH Data Collection & Storage")
        print("================================")
        
        # Get latest price first
        self.get_latest_price()
        print()
        
        # Collect data from Yahoo Finance
        eth_data = self.get_yahoo_finance_data(period=period, interval=interval)
        
        if not eth_data.empty:
            # Save to organized directory
            filename = self.save_data(eth_data, 'yahoo_finance', 'ETH')
            
            # Show sample data
            print("\n📋 Sample data (latest 5 records):")
            sample_data = eth_data[['open', 'high', 'low', 'close', 'volume']].tail()
            print(sample_data.to_string())
            
            return filename
        else:
            print("❌ Data collection failed")
            return None
    
    def list_saved_files(self, provider='yahoo_finance', asset='ETH'):
        """List all saved data files for an asset from a provider."""
        data_dir = os.path.join(self.base_data_dir, provider, asset)
        
        if os.path.exists(data_dir):
            files = [f for f in os.listdir(data_dir) if f.endswith('.csv')]
            files.sort(reverse=True)  # Most recent first
            
            print(f"📁 Saved {asset} files from {provider}:")
            for i, file in enumerate(files[:10]):  # Show latest 10
                file_path = os.path.join(data_dir, file)
                file_size = os.path.getsize(file_path)
                print(f"   {i+1}. {file} ({file_size:,} bytes)")
            
            if len(files) > 10:
                print(f"   ... and {len(files) - 10} more files")
                
            return files
        else:
            print(f"📁 No data directory found: {data_dir}")
            return []

# Example usage and testing
if __name__ == "__main__":
    # Initialize collector
    collector = ETHDataCollector()
    
    # Collect and save ETH data
    filename = collector.collect_and_save_eth_data(period='1d', interval='1m')
    
    print()
    
    # List saved files
    collector.list_saved_files()
    
    print("\n🎯 ETH data collection complete!")
    print("Data is now organized in the provider-specific directories.")
