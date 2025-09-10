#!/usr/bin/env python3
"""
Comprehensive Forex Data Collection
Issue #36: Multi-Currency Forex Alpha Models & Forecasting System

Collects all major forex pairs using proper directory structure that matches existing ETH data.
"""

import os
import sys
import yfinance as yf
import pandas as pd
from datetime import datetime
from typing import Dict, List

# Forex pairs configuration
MAJOR_FOREX_PAIRS = {
    'EURUSD': {'symbol': 'EURUSD=X', 'name': 'EUR/USD', 'description': 'Euro/US Dollar - Most traded globally'},
    'USDJPY': {'symbol': 'USDJPY=X', 'name': 'USD/JPY', 'description': 'US Dollar/Japanese Yen - High liquidity, safe haven'},
    'GBPUSD': {'symbol': 'GBPUSD=X', 'name': 'GBP/USD', 'description': 'British Pound/US Dollar - "The Cable"'},
    'AUDUSD': {'symbol': 'AUDUSD=X', 'name': 'AUD/USD', 'description': 'Australian Dollar/US Dollar - Commodity-linked'},
    'USDCAD': {'symbol': 'USDCAD=X', 'name': 'USD/CAD', 'description': 'US Dollar/Canadian Dollar - Oil-linked'},
    'USDCHF': {'symbol': 'USDCHF=X', 'name': 'USD/CHF', 'description': 'US Dollar/Swiss Franc - Safe haven'},
    'NZDUSD': {'symbol': 'NZDUSD=X', 'name': 'NZD/USD', 'description': 'New Zealand Dollar/US Dollar - Agricultural commodity-linked'}
}

def setup_forex_directories(base_data_dir: str) -> Dict[str, str]:
    """Setup forex directory structure matching existing ETH data organization"""
    
    forex_base = os.path.join(base_data_dir, 'yahoo_finance', 'forex')
    pair_directories = {}
    
    for pair_code in MAJOR_FOREX_PAIRS.keys():
        pair_dir = os.path.join(forex_base, pair_code)
        os.makedirs(pair_dir, exist_ok=True)
        pair_directories[pair_code] = pair_dir
    
    print(f"✅ Created forex directories for {len(MAJOR_FOREX_PAIRS)} currency pairs")
    return pair_directories

def collect_forex_data(pair_code: str, pair_info: Dict, pair_dir: str, period: str = '1mo', interval: str = '1h') -> bool:
    """Collect data for a single forex pair"""
    
    yahoo_symbol = pair_info['symbol']
    name = pair_info['name']
    description = pair_info['description']
    
    print(f"\n💱 Collecting {name} ({yahoo_symbol})")
    print(f"   Description: {description}")
    
    try:
        # Fetch data using yfinance
        ticker = yf.Ticker(yahoo_symbol)
        data = ticker.history(period=period, interval=interval)
        
        if data.empty:
            print(f"   ❌ No data returned for {pair_code}")
            return False
        
        # Add metadata
        data['Symbol'] = yahoo_symbol
        data['PairCode'] = pair_code
        data['Name'] = name
        
        # Save timestamped file
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f"{pair_code}_{timestamp}.csv"
        file_path = os.path.join(pair_dir, filename)
        data.to_csv(file_path)
        
        # Save as latest
        latest_path = os.path.join(pair_dir, 'latest.csv')
        data.to_csv(latest_path)
        
        print(f"   ✅ Success: {len(data)} records collected")
        print(f"   📈 Latest Price: {data['Close'].iloc[-1]:.5f}")
        print(f"   📅 Date Range: {data.index[0].strftime('%Y-%m-%d')} to {data.index[-1].strftime('%Y-%m-%d')}")
        print(f"   💾 Saved: {file_path}")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Error collecting {pair_code}: {str(e)}")
        return False

def main():
    """Main forex data collection function"""
    
    print("🌍 Comprehensive Forex Data Collection")
    print("=" * 60)
    print("Issue #36: Multi-Currency Forex Alpha Models & Forecasting System")
    print(f"📅 Collection Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Set up data directory (matching existing ETH data structure)
    project_root = '/workspaces/unicorninvesting'
    base_data_dir = os.path.join(project_root, 'BackendPython', 'unicorn', '1_data_sources', '1_raw', 'data')
    
    print(f"\n📁 Base Data Directory: {base_data_dir}")
    print(f"📁 Directory Exists: {os.path.exists(base_data_dir)}")
    
    # Setup directories
    pair_directories = setup_forex_directories(base_data_dir)
    
    # Collect data for all major pairs
    print(f"\n📊 Collecting data for {len(MAJOR_FOREX_PAIRS)} major currency pairs...")
    
    successful_collections = 0
    failed_collections = 0
    
    for pair_code, pair_info in MAJOR_FOREX_PAIRS.items():
        pair_dir = pair_directories[pair_code]
        
        # Collect both hourly and daily data
        success_hourly = collect_forex_data(pair_code, pair_info, pair_dir, period='1mo', interval='1h')
        success_daily = collect_forex_data(pair_code, pair_info, pair_dir, period='1y', interval='1d')
        
        if success_hourly and success_daily:
            successful_collections += 1
        else:
            failed_collections += 1
    
    # Summary
    print(f"\n🎉 === FOREX DATA COLLECTION SUMMARY ===")
    print(f"✅ Successful: {successful_collections}/{len(MAJOR_FOREX_PAIRS)} currency pairs")
    print(f"❌ Failed: {failed_collections}/{len(MAJOR_FOREX_PAIRS)} currency pairs")
    
    if successful_collections > 0:
        print(f"\n📁 Data Location: {base_data_dir}/yahoo_finance/forex/")
        print(f"📊 Directory Structure:")
        
        # Show directory structure
        forex_dir = os.path.join(base_data_dir, 'yahoo_finance', 'forex')
        for pair_code in MAJOR_FOREX_PAIRS.keys():
            pair_dir = os.path.join(forex_dir, pair_code)
            if os.path.exists(pair_dir):
                files = os.listdir(pair_dir)
                print(f"   {pair_code}/: {len(files)} files")
        
        print(f"\n✅ Forex data collection completed successfully!")
        print(f"🚀 Ready for alpha model development and backtesting!")
    else:
        print(f"\n❌ Forex data collection failed")
        
    return successful_collections > 0

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
