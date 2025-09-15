#!/usr/bin/env python3
"""
6-Month Simulation Results Analysis Tool
======================================

Analyzes the results from our latest 6-month backtest simulation
with enhanced performance logging and attribution analysis.

Author: Unicorn Investing Platform  
Date: September 15, 2025
"""

import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import sys

def analyze_6_month_simulation():
    """
    Analyze the results of our 6-month simulation (2024-03-15 to 2024-09-15)
    using the best economic-enhanced models.
    """
    print("📊 6-Month Simulation Analysis Report")
    print("=" * 50)
    
    # Simulation details
    simulation_id = "backtest_20250915_184533_b6e6d479"
    period_start = "2024-03-15"
    period_end = "2024-09-15"
    
    print(f"🎯 Simulation ID: {simulation_id}")
    print(f"📅 Period: {period_start} to {period_end} (6 months)")
    print(f"🤖 Strategy: Best Economic-Enhanced Models (BTC + ETH Deep variants)")
    print(f"📈 Models: High confidence >88% R² with economic feature integration")
    print()
    
    # Load simulation results
    results_path = Path(f"backtests/{simulation_id}/myportolio_results.json")
    simulation_data_path = Path(f"backtests/{simulation_id}/simulation_data.json")
    
    if not results_path.exists():
        print(f"❌ Results file not found: {results_path}")
        return
    
    print("📊 SIMULATION RESULTS ANALYSIS")
    print("-" * 30)
    
    # Load results
    with open(results_path, 'r') as f:
        results = json.load(f)
    
    # Extract performance metrics
    performance = results.get('performance', {})
    
    print(f"📈 Total Return: {performance.get('total_return', 0.0):.2%}")
    print(f"📊 Sharpe Ratio: {performance.get('sharpe_ratio', 0.0):.2f}")
    print(f"📉 Max Drawdown: {performance.get('max_drawdown', 0.0):.2%}")
    print(f"🔄 Total Trades: {performance.get('trades_count', 0):,}")
    print(f"🎯 Win Rate: {performance.get('win_rate', 0.0):.1%}")
    print(f"💰 Profit Factor: {performance.get('profit_factor', 0.0):.2f}")
    print()
    
    # Analyze market data and volatility
    if 'lean_results' in results and 'market_data' in results['lean_results']:
        market_data = results['lean_results']['market_data']
        
        print("📊 MARKET DATA ANALYSIS")
        print("-" * 30)
        
        # Convert to DataFrame for analysis
        df = pd.DataFrame(market_data)
        df['timestamp'] = pd.to_datetime(df['timestamp'])
        df['price'] = pd.to_numeric(df['price'])
        
        # Calculate key metrics
        start_price = df['price'].iloc[0]
        end_price = df['price'].iloc[-1]
        market_return = (end_price - start_price) / start_price
        
        # Calculate volatility from returns
        returns = df['returns'].dropna()
        if len(returns) > 0:
            volatility = returns.std() * np.sqrt(365 * 24)  # Annualized hourly volatility
            
            print(f"📅 Period: {df['timestamp'].iloc[0].strftime('%Y-%m-%d')} to {df['timestamp'].iloc[-1].strftime('%Y-%m-%d')}")
            print(f"📊 Starting Price: ${start_price:,.2f}")
            print(f"📊 Ending Price: ${end_price:,.2f}")
            print(f"📈 Market Return: {market_return:.2%}")
            print(f"📊 Volatility (Annualized): {volatility:.1%}")
            print(f"📝 Data Points: {len(df):,} hourly observations")
            print()
    
    # Check for model configuration details
    if 'lean_results' in results and 'btc_model_config' in results['lean_results']:
        btc_config = results['lean_results']['btc_model_config']
        eth_config = results['lean_results'].get('eth_model_config', {})
        
        print("🤖 MODEL CONFIGURATION ANALYSIS")
        print("-" * 30)
        
        # BTC Model Analysis
        btc_perf = btc_config.get('performance_expectations', {})
        print(f"🟡 BTC Model:")
        print(f"   📊 Expected R²: {btc_perf.get('expected_r2', 0.0):.3f}")
        print(f"   📊 Expected MAE: {btc_perf.get('expected_mae', 0.0):.2f}")
        print(f"   🎯 Model Confidence: {btc_perf.get('model_confidence', 'UNKNOWN')}")
        print(f"   ⚖️  Economic Feature Weight: {btc_config.get('parameters', {}).get('economic_feature_weight', 0.0):.3f}")
        
        # ETH Model Analysis  
        if eth_config:
            eth_perf = eth_config.get('performance_expectations', {})
            print(f"🔵 ETH Model:")
            print(f"   📊 Expected R²: {eth_perf.get('expected_r2', 0.0):.3f}")
            print(f"   📊 Expected MAE: {eth_perf.get('expected_mae', 0.0):.2f}")
            print(f"   🎯 Model Confidence: {eth_perf.get('model_confidence', 'UNKNOWN')}")
            print(f"   ⚖️  Economic Feature Weight: {eth_config.get('parameters', {}).get('economic_feature_weight', 0.0):.3f}")
        print()
    
    # Performance Logging Analysis
    print("📊 PERFORMANCE LOGGING ANALYSIS")
    print("-" * 30)
    
    # Check for performance logs
    log_files = list(Path("performance_logs").glob(f"*{simulation_id}*"))
    
    if log_files:
        print(f"✅ Found {len(log_files)} performance log files:")
        for log_file in log_files:
            print(f"   📄 {log_file.name}")
        print()
    else:
        print("⚠️  No performance log files found for this simulation")
        print("   This suggests the enhanced logging may not have been fully activated")
        print()
    
    # Execution Analysis
    execution = results.get('execution', {})
    print("⚡ EXECUTION ANALYSIS")
    print("-" * 30)
    print(f"⏱️  Duration: {execution.get('duration', 'Unknown')}")
    print(f"✅ Status: {execution.get('status', 'Unknown')}")
    print(f"📝 LEAN Output: {execution.get('lean_output', 'No output available')}")
    print()
    
    # Summary and Recommendations
    print("🎯 ANALYSIS SUMMARY & RECOMMENDATIONS")
    print("-" * 30)
    
    if performance.get('trades_count', 0) == 0:
        print("⚠️  CRITICAL: No trades were executed during the 6-month simulation")
        print("   🔍 Possible causes:")
        print("   1. Strategy parameters too conservative")
        print("   2. Confidence threshold (0.65) too high")
        print("   3. Economic feature integration filtering all signals")
        print("   4. Model prediction accuracy below trading threshold")
        print()
        print("🛠️  RECOMMENDED ACTIONS:")
        print("   1. Lower confidence_threshold from 0.65 to 0.50")
        print("   2. Review economic feature weights (BTC: 0.484, ETH: 0.414)")
        print("   3. Test with single-asset strategy to isolate issues")
        print("   4. Enable debug logging to see signal generation")
    else:
        # Analyze performance if trades occurred
        total_return = performance.get('total_return', 0.0)
        sharpe = performance.get('sharpe_ratio', 0.0)
        
        if total_return > 0 and sharpe > 1.0:
            print("✅ EXCELLENT: Strong positive performance with good risk-adjusted returns")
        elif total_return > 0:
            print("✅ POSITIVE: Profitable but check Sharpe ratio for risk efficiency")
        else:
            print("⚠️  UNDERPERFORMING: Negative returns - review strategy parameters")
    
    print()
    print("📈 NEXT STEPS:")
    print("   1. Run diagnostic simulation with lower confidence threshold")
    print("   2. Test individual BTC and ETH strategies separately")  
    print("   3. Enable detailed performance logging for signal analysis")
    print("   4. Compare with previous successful simulations")

if __name__ == "__main__":
    analyze_6_month_simulation()