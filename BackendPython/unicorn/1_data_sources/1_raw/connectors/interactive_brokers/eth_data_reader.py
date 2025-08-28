"""
IBKR ETH Data Reader and Analyzer
Unicorn Investing Platform

Reads and analyzes ETH data collected from Interactive Brokers.
"""

import os
import json
import pandas as pd
from datetime import datetime
import matplotlib.pyplot as plt
from typing import Dict, List, Optional

class IBKREthDataReader:
    """
    Reader and analyzer for IBKR ETH data.
    """
    
    def __init__(self):
        self.data_dir = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/interactive_brokers/ETH"
        print(f"📁 Reading from: {self.data_dir}")
    
    def list_data_files(self) -> Dict[str, List[str]]:
        """List all available data files by type."""
        files = {
            'realtime': [],
            'historical': [],
            'other': []
        }
        
        if not os.path.exists(self.data_dir):
            print(f"❌ Data directory not found: {self.data_dir}")
            return files
        
        for filename in os.listdir(self.data_dir):
            if filename.endswith('.json'):
                if 'realtime' in filename:
                    files['realtime'].append(filename)
                elif 'historical' in filename:
                    files['historical'].append(filename)
                else:
                    files['other'].append(filename)
        
        return files
    
    def read_data_file(self, filename: str) -> Optional[Dict]:
        """Read a specific data file."""
        filepath = os.path.join(self.data_dir, filename)
        
        try:
            with open(filepath, 'r') as f:
                data = json.load(f)
            return data
        except Exception as e:
            print(f"❌ Error reading {filename}: {e}")
            return None
    
    def extract_historical_dataframe(self, data: Dict) -> Optional[pd.DataFrame]:
        """Extract historical data as a pandas DataFrame."""
        try:
            if 'historical_data' in data and 'data' in data['historical_data']:
                bars = data['historical_data']['data']
                
                if not bars:
                    print("⚠️ No historical bars found")
                    return None
                
                df = pd.DataFrame(bars)
                
                # Convert timestamp to datetime
                if 't' in df.columns:
                    df['timestamp'] = pd.to_datetime(df['t'], unit='ms')
                    df.set_index('timestamp', inplace=True)
                
                # Rename columns to standard format
                column_mapping = {
                    'o': 'open',
                    'h': 'high', 
                    'l': 'low',
                    'c': 'close',
                    'v': 'volume'
                }
                df.rename(columns=column_mapping, inplace=True)
                
                return df
            else:
                print("⚠️ No historical data structure found")
                return None
                
        except Exception as e:
            print(f"❌ Error extracting DataFrame: {e}")
            return None
    
    def analyze_eth_data(self):
        """Comprehensive analysis of ETH data."""
        print("📊 IBKR ETH Data Analysis")
        print("=========================")
        
        # List available files
        files = self.list_data_files()
        
        print(f"📁 Available files:")
        print(f"   Realtime: {len(files['realtime'])} files")
        print(f"   Historical: {len(files['historical'])} files")
        print(f"   Other: {len(files['other'])} files")
        
        if not files['historical']:
            print("❌ No historical data files found")
            return
        
        # Analyze the most recent historical file
        latest_historical = sorted(files['historical'])[-1]
        print(f"\\n📈 Analyzing: {latest_historical}")
        
        data = self.read_data_file(latest_historical)
        if not data:
            return
        
        # Display contract information
        if 'contract' in data:
            contract = data['contract']
            print(f"\\n📊 Contract Information:")
            print(f"   Symbol: {contract.get('symbol', 'Unknown')}")
            print(f"   Name: {contract.get('companyName', 'Unknown')}")
            print(f"   Contract ID: {contract.get('conid', 'Unknown')}")
            
            if 'sections' in contract and contract['sections']:
                section = contract['sections'][0]
                print(f"   Type: {section.get('secType', 'Unknown')}")
                print(f"   Exchange: {section.get('exchange', 'Unknown')}")
        
        # Extract and analyze historical data
        df = self.extract_historical_dataframe(data)
        if df is None:
            return
        
        print(f"\\n📊 Historical Data Analysis:")
        print(f"   Data points: {len(df)}")
        print(f"   Time range: {df.index.min()} to {df.index.max()}")
        print(f"   Columns: {list(df.columns)}")
        
        # Price analysis
        if 'close' in df.columns:
            current_price = df['close'].iloc[-1]
            high_price = df['high'].max()
            low_price = df['low'].min()
            avg_price = df['close'].mean()
            
            print(f"\\n💰 Price Analysis:")
            print(f"   Current: ${current_price:,.2f}")
            print(f"   High: ${high_price:,.2f}")
            print(f"   Low: ${low_price:,.2f}")
            print(f"   Average: ${avg_price:,.2f}")
            print(f"   Range: ${high_price - low_price:,.2f} ({((high_price - low_price) / avg_price * 100):.1f}%)")
        
        # Volume analysis
        if 'volume' in df.columns:
            total_volume = df['volume'].sum()
            avg_volume = df['volume'].mean()
            max_volume = df['volume'].max()
            
            print(f"\\n📊 Volume Analysis:")
            print(f"   Total: {total_volume:,.2f} ETH")
            print(f"   Average: {avg_volume:,.2f} ETH")
            print(f"   Peak: {max_volume:,.2f} ETH")
        
        # Recent price movement
        if len(df) >= 2 and 'close' in df.columns:
            price_change = df['close'].iloc[-1] - df['close'].iloc[-2]
            price_change_pct = (price_change / df['close'].iloc[-2]) * 100
            
            print(f"\\n📈 Recent Movement:")
            print(f"   Last change: ${price_change:,.2f} ({price_change_pct:+.2f}%)")
        
        # Display sample data
        print(f"\\n📋 Sample Data:")
        print(df.head())
        
        return df
    
    def compare_data_sources(self):
        """Compare ETH data from different sources."""
        print("🔄 Data Source Comparison")
        print("========================")
        
        # IBKR data
        ibkr_files = self.list_data_files()
        print(f"📊 IBKR Data: {len(ibkr_files['historical'])} historical files")
        
        # Yahoo Finance data (if available)
        yahoo_dir = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/yahoo_finance/ETH"
        yahoo_files = []
        if os.path.exists(yahoo_dir):
            yahoo_files = [f for f in os.listdir(yahoo_dir) if f.endswith('.csv')]
        
        print(f"📊 Yahoo Finance Data: {len(yahoo_files)} files")
        
        if ibkr_files['historical'] and yahoo_files:
            print("\\n🔍 Data sources available for comparison")
            # Could implement detailed comparison here
        else:
            print("\\n⚠️ Need data from both sources for comparison")

def main():
    """Run ETH data analysis."""
    reader = IBKREthDataReader()
    
    # Analyze ETH data
    df = reader.analyze_eth_data()
    
    # Compare data sources
    reader.compare_data_sources()
    
    print("\\n🎉 ETH data analysis complete!")

if __name__ == "__main__":
    main()
