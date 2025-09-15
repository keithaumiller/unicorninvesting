#!/usr/bin/env python3
"""
Test Yahoo Finance Interval Capabilities for Forex Data
=======================================================

Test script to determine the finest granularity available from Yahoo Finance
for forex data and document all supported intervals.
"""

import yfinance as yf
import pandas as pd
from datetime import datetime
import sys
import os

def test_interval(symbol, interval, period, description):
    """Test a specific interval for a forex symbol"""
    print(f"\n🔍 Testing {interval} interval ({description})")
    print(f"   Period: {period}")
    
    try:
        ticker = yf.Ticker(symbol)
        data = ticker.history(period=period, interval=interval)
        
        if data.empty:
            print(f"   ❌ No data returned")
            return False, 0
        
        # Check time difference between first two records to verify interval
        if len(data) >= 2:
            time_diff = data.index[1] - data.index[0]
            print(f"   ✅ Success: {len(data)} records")
            print(f"   📊 Latest Price: {data['Close'].iloc[-1]:.5f}")
            print(f"   ⏱️  Actual Interval: {time_diff}")
            print(f"   📅 Range: {data.index[0]} to {data.index[-1]}")
            return True, len(data)
        else:
            print(f"   ⚠️  Only {len(data)} record(s) returned")
            return True, len(data)
            
    except Exception as e:
        print(f"   ❌ Error: {str(e)}")
        return False, 0

def main():
    """Test all available intervals for forex data"""
    
    print("🧪 Yahoo Finance Forex Interval Testing")
    print("=" * 60)
    print(f"📅 Test Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Test symbol
    test_symbol = "EURUSD=X"
    print(f"🎯 Test Symbol: {test_symbol}")
    
    # Define intervals to test with appropriate periods
    test_cases = [
        # Ultra-short intervals (if available)
        ("1m", "1d", "1-minute (intraday - finest possible)"),
        ("2m", "1d", "2-minute (intraday)"),
        ("5m", "1d", "5-minute (intraday)"),
        ("15m", "5d", "15-minute (short-term)"),
        ("30m", "5d", "30-minute (short-term)"),
        ("60m", "1mo", "1-hour (medium-term)"),
        ("90m", "1mo", "90-minute (extended)"),
        
        # Standard intervals
        ("1h", "1mo", "1-hour (standard)"),
        ("1d", "1y", "1-day (daily)"),
        ("5d", "2y", "5-day (weekly)"),
        ("1wk", "5y", "1-week (weekly)"),
        ("1mo", "max", "1-month (monthly)"),
        ("3mo", "max", "3-month (quarterly)")
    ]
    
    print(f"\n🔬 Testing {len(test_cases)} different intervals...")
    
    successful_intervals = []
    failed_intervals = []
    
    for interval, period, description in test_cases:
        success, records = test_interval(test_symbol, interval, period, description)
        
        if success and records > 0:
            successful_intervals.append((interval, period, description, records))
        else:
            failed_intervals.append((interval, period, description))
    
    # Summary
    print(f"\n🎉 === INTERVAL TESTING SUMMARY ===")
    print(f"✅ Working Intervals: {len(successful_intervals)}")
    print(f"❌ Failed Intervals: {len(failed_intervals)}")
    
    if successful_intervals:
        print(f"\n📊 SUPPORTED INTERVALS (ordered by granularity):")
        print("-" * 70)
        for interval, period, description, records in successful_intervals:
            print(f"   {interval:>6} | Period: {period:>5} | Records: {records:>5} | {description}")
        
        # Find finest granularity
        finest = successful_intervals[0]  # Assuming ordered by fineness
        print(f"\n🎯 FINEST GRANULARITY AVAILABLE:")
        print(f"   Interval: {finest[0]}")
        print(f"   Description: {finest[2]}")
        print(f"   Sample Records: {finest[3]}")
    
    if failed_intervals:
        print(f"\n❌ UNSUPPORTED INTERVALS:")
        print("-" * 50)
        for interval, period, description in failed_intervals:
            print(f"   {interval:>6} | {description}")
    
    # Current data assessment
    print(f"\n📈 CURRENT FOREX DATA ASSESSMENT:")
    print("-" * 50)
    
    # Check our current data
    current_data_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/yahoo_finance/forex/AUDUSD/AUDUSD_20250910_163213.csv"
    
    if os.path.exists(current_data_path):
        try:
            current_df = pd.read_csv(current_data_path, index_col=0, parse_dates=True)
            if len(current_df) >= 2:
                time_diff = current_df.index[1] - current_df.index[0]
                print(f"   Current Data Interval: {time_diff}")
                print(f"   Current Data Records: {len(current_df)}")
                print(f"   Current Data Range: {current_df.index[0]} to {current_df.index[-1]}")
            else:
                print(f"   Current Data: Insufficient records for analysis")
        except Exception as e:
            print(f"   Error analyzing current data: {str(e)}")
    else:
        print(f"   Current Data: File not found")
    
    print(f"\n🎯 RECOMMENDATIONS:")
    if successful_intervals:
        finest_interval = successful_intervals[0][0]
        print(f"   • Use '{finest_interval}' for highest granularity trading algorithms")
        print(f"   • Use '1h' for balanced data size vs. granularity")
        print(f"   • Use '1d' for long-term analysis and reduced storage")
        print(f"   • Current '{time_diff}' interval is appropriate for daily strategies")
    
    return len(successful_intervals) > 0

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
