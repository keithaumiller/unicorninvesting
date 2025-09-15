#!/usr/bin/env python3
"""
Simple Forex Data Collection Test
Test script to collect EUR/USD data in the correct directory structure
"""

import os
import sys
import yfinance as yf
import pandas as pd
from datetime import datetime

def collect_eurusd_sample():
    """Simple EUR/USD data collection test"""
    
    # Set correct data directory path
    project_root = '/workspaces/unicorninvesting'
    base_data_dir = os.path.join(project_root, 'BackendPython', 'unicorn', '1_data_sources', '1_raw', 'data')
    
    print(f"🌍 Simple Forex Data Collection Test")
    print(f"📁 Base data directory: {base_data_dir}")
    print(f"📁 Directory exists: {os.path.exists(base_data_dir)}")
    
    # Create forex directory structure
    forex_dir = os.path.join(base_data_dir, 'yahoo_finance', 'forex')
    eurusd_dir = os.path.join(forex_dir, 'EURUSD')
    
    os.makedirs(eurusd_dir, exist_ok=True)
    print(f"✅ Created directory: {eurusd_dir}")
    
    # Collect EUR/USD data
    print(f"\n💱 Collecting EUR/USD data...")
    ticker = yf.Ticker('EURUSD=X')
    data = ticker.history(period='5d', interval='1d')
    
    if not data.empty:
        print(f"✅ Successfully collected {len(data)} records")
        print(f"📈 Latest EUR/USD price: {data['Close'].iloc[-1]:.5f}")
        print(f"📅 Date range: {data.index[0].strftime('%Y-%m-%d')} to {data.index[-1].strftime('%Y-%m-%d')}")
        
        # Save data
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f"EURUSD_test_{timestamp}.csv"
        file_path = os.path.join(eurusd_dir, filename)
        
        data.to_csv(file_path)
        print(f"💾 Saved data to: {file_path}")
        
        # Also save as latest
        latest_path = os.path.join(eurusd_dir, 'latest.csv')
        data.to_csv(latest_path)
        print(f"💾 Saved latest data to: {latest_path}")
        
        return True
    else:
        print(f"❌ No data collected")
        return False

if __name__ == "__main__":
    collect_eurusd_sample()
