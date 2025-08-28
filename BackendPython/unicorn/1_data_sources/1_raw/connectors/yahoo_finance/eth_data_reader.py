"""
ETH Data Reader & Analyzer
Unicorn Investing Platform

Simple utility to read and analyze saved ETH data from our organized directory structure.
"""

import pandas as pd
import os
import glob
from datetime import datetime
import matplotlib.pyplot as plt

class ETHDataReader:
    """
    Reads and analyzes ETH data from the organized directory structure.
    """
    
    def __init__(self, base_data_dir="data"):
        """
        Initialize the ETH data reader.
        
        Args:
            base_data_dir: Base directory for data storage
        """
        self.base_data_dir = base_data_dir
    
    def list_available_data(self, provider='yahoo_finance', asset='ETH'):
        """
        List all available data files for an asset from a provider.
        
        Args:
            provider: Data provider name
            asset: Asset symbol
            
        Returns:
            List of available data files
        """
        data_dir = os.path.join(self.base_data_dir, provider, asset)
        pattern = os.path.join(data_dir, f"{asset}_*.csv")
        files = glob.glob(pattern)
        files.sort(reverse=True)  # Most recent first
        
        return files
    
    def load_latest_data(self, provider='yahoo_finance', asset='ETH'):
        """
        Load the most recent data file for an asset.
        
        Args:
            provider: Data provider name
            asset: Asset symbol
            
        Returns:
            DataFrame with the latest data
        """
        files = self.list_available_data(provider, asset)
        
        if not files:
            print(f"❌ No data files found for {asset} from {provider}")
            return pd.DataFrame()
        
        latest_file = files[0]
        print(f"📊 Loading latest {asset} data from {provider}")
        print(f"📁 File: {os.path.basename(latest_file)}")
        
        try:
            # Load data with proper datetime parsing
            data = pd.read_csv(latest_file, index_col='Datetime', parse_dates=True)
            
            print(f"✅ Loaded {len(data)} records")
            print(f"📅 Time range: {data.index.min()} to {data.index.max()}")
            
            return data
            
        except Exception as e:
            print(f"❌ Error loading data: {e}")
            return pd.DataFrame()
    
    def analyze_price_data(self, data):
        """
        Perform basic analysis on ETH price data.
        
        Args:
            data: DataFrame with ETH price data
        """
        if data.empty:
            print("❌ No data to analyze")
            return
        
        print("\n📈 ETH Price Analysis")
        print("====================")
        
        # Basic statistics
        close_prices = data['close']
        
        print(f"💰 Current Price: ${close_prices.iloc[-1]:.2f}")
        print(f"📊 24h High: ${close_prices.max():.2f}")
        print(f"📊 24h Low: ${close_prices.min():.2f}")
        print(f"📊 24h Average: ${close_prices.mean():.2f}")
        
        # Price change analysis
        first_price = close_prices.iloc[0]
        last_price = close_prices.iloc[-1]
        price_change = last_price - first_price
        price_change_pct = (price_change / first_price) * 100
        
        print(f"📈 Price Change: ${price_change:+.2f} ({price_change_pct:+.2f}%)")
        
        # Volatility
        price_std = close_prices.std()
        volatility_pct = (price_std / close_prices.mean()) * 100
        
        print(f"📊 Volatility: ${price_std:.2f} ({volatility_pct:.2f}%)")
        
        # Volume analysis
        if 'volume' in data.columns:
            total_volume = data['volume'].sum()
            avg_volume = data['volume'].mean()
            
            print(f"📊 Total Volume: {total_volume:,.0f}")
            print(f"📊 Average Volume: {avg_volume:,.0f}")
    
    def get_price_summary(self, data, periods=[60, 240]):  # 1h, 4h periods
        """
        Get price summary for different time periods.
        
        Args:
            data: DataFrame with ETH price data
            periods: List of periods in minutes
        """
        if data.empty:
            return
        
        print("\n⏰ Time Period Analysis")
        print("======================")
        
        current_time = data.index[-1]
        current_price = data['close'].iloc[-1]
        
        for period in periods:
            period_start = current_time - pd.Timedelta(minutes=period)
            period_data = data[data.index >= period_start]
            
            if not period_data.empty:
                period_start_price = period_data['close'].iloc[0]
                period_change = current_price - period_start_price
                period_change_pct = (period_change / period_start_price) * 100
                
                period_high = period_data['close'].max()
                period_low = period_data['close'].min()
                
                hours = period / 60
                print(f"🕒 {hours:.0f}h Change: ${period_change:+.2f} ({period_change_pct:+.2f}%)")
                print(f"   High: ${period_high:.2f}, Low: ${period_low:.2f}")
    
    def analyze_eth_data(self, provider='yahoo_finance'):
        """
        Complete ETH data analysis workflow.
        
        Args:
            provider: Data provider to analyze
        """
        print("🔍 ETH Data Analysis")
        print("===================")
        
        # Load latest data
        eth_data = self.load_latest_data(provider, 'ETH')
        
        if not eth_data.empty:
            # Perform analysis
            self.analyze_price_data(eth_data)
            self.get_price_summary(eth_data)
            
            # Show recent price action
            print("\n📋 Recent Price Action (Last 10 minutes):")
            recent_data = eth_data[['open', 'high', 'low', 'close', 'volume']].tail(10)
            print(recent_data.to_string())
            
            return eth_data
        else:
            print("❌ No ETH data available for analysis")
            return pd.DataFrame()

# Example usage and testing
if __name__ == "__main__":
    # Initialize reader
    reader = ETHDataReader()
    
    # Analyze ETH data
    eth_data = reader.analyze_eth_data('yahoo_finance')
    
    print("\n🎯 ETH data analysis complete!")
    
    # List all available files
    print(f"\n📁 Available ETH data files:")
    files = reader.list_available_data('yahoo_finance', 'ETH')
    for i, file in enumerate(files):
        file_size = os.path.getsize(file)
        file_name = os.path.basename(file)
        print(f"   {i+1}. {file_name} ({file_size:,} bytes)")
