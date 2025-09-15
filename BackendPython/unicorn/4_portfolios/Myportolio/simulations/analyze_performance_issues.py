#!/usr/bin/env python3
"""
Portfolio Performance Analysis and Diagnosis
===========================================

Analyzes the backtest results to identify specific performance issues
and provide actionable recommendations for improvement.

Author: Unicorn Investing Platform  
Date: September 15, 2025
"""

import json
import pandas as pd
import numpy as np
from datetime import datetime
from pathlib import Path

def analyze_backtest_performance(backtest_id: str):
    """
    Comprehensive analysis of backtest performance issues.
    
    Args:
        backtest_id: The backtest simulation ID to analyze
    """
    
    print(f"🔍 Analyzing Backtest Performance: {backtest_id}")
    print("=" * 60)
    
    # Load backtest results
    results_path = Path(f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/backtests/{backtest_id}/myportolio_results.json")
    
    if not results_path.exists():
        print(f"❌ Results file not found: {results_path}")
        return
    
    with open(results_path, 'r') as f:
        results = json.load(f)
    
    # Extract key performance metrics
    portfolio_results = results.get('portfolio_results', {})
    lean_results = results.get('lean_results', {})
    trades = results.get('trades', [])
    
    print("📊 PERFORMANCE SUMMARY")
    print("-" * 30)
    print(f"Initial Value: ${portfolio_results.get('initial_value', 100000):,.2f}")
    print(f"Final Value:   ${portfolio_results.get('final_value', 0):,.2f}")
    print(f"Total Return:  {portfolio_results.get('total_return', 0):.2%}")
    print(f"Max Drawdown:  {portfolio_results.get('max_drawdown', 0):.2%}")
    print(f"Sharpe Ratio:  {portfolio_results.get('sharpe_ratio', 0):.2f}")
    print(f"Total Trades:  {portfolio_results.get('trades_count', 0)}")
    
    # Calculate trade frequency
    if trades:
        start_date = datetime.fromisoformat(trades[0]['timestamp'].replace('T', ' ').replace('Z', ''))
        end_date = datetime.fromisoformat(trades[-1]['timestamp'].replace('T', ' ').replace('Z', ''))
        days = (end_date - start_date).days + 1
        trades_per_day = len(trades) / days if days > 0 else 0
        
        print(f"Trades/Day:    {trades_per_day:.1f}")
    
    # PROBLEM 1: OVER-TRADING ANALYSIS
    print(f"\n🚨 PROBLEM 1: OVER-TRADING")
    print("-" * 30)
    
    if len(trades) > 100:  # More than ~3 trades per day for a month
        print(f"❌ EXCESSIVE TRADING: {len(trades)} trades detected")
        print(f"   This suggests the strategy is generating too many signals")
        print(f"   Recommended: Add signal filtering or minimum hold periods")
        
        # Analyze trade timing
        buy_trades = [t for t in trades if t['side'] == 'BUY']
        sell_trades = [t for t in trades if t['side'] == 'SELL']
        
        print(f"   • Buy trades: {len(buy_trades)}")
        print(f"   • Sell trades: {len(sell_trades)}")
        
        # Check for rapid buy/sell cycles
        rapid_cycles = 0
        for i in range(len(trades) - 1):
            current_trade = trades[i]
            next_trade = trades[i + 1]
            
            if current_trade['side'] != next_trade['side']:
                current_time = datetime.fromisoformat(current_trade['timestamp'].replace('T', ' ').replace('Z', ''))
                next_time = datetime.fromisoformat(next_trade['timestamp'].replace('T', ' ').replace('Z', ''))
                
                if (next_time - current_time).total_seconds() < 7200:  # Less than 2 hours
                    rapid_cycles += 1
        
        print(f"   • Rapid buy/sell cycles: {rapid_cycles}")
        if rapid_cycles > 50:
            print(f"   ⚠️  Too many rapid reversals - strategy is whipsawing")
    
    # PROBLEM 2: POOR SIGNAL QUALITY
    print(f"\n🚨 PROBLEM 2: SIGNAL QUALITY ANALYSIS")
    print("-" * 30)
    
    if trades:
        # Calculate win rate
        profitable_trades = 0
        total_pnl = 0
        
        for i in range(0, len(trades) - 1, 2):  # Assume buy/sell pairs
            if i + 1 < len(trades):
                buy_trade = trades[i]
                sell_trade = trades[i + 1]
                
                if buy_trade['side'] == 'BUY' and sell_trade['side'] == 'SELL':
                    buy_price = buy_trade['price']
                    sell_price = sell_trade['price']
                    quantity = min(abs(buy_trade['quantity']), abs(sell_trade['quantity']))
                    
                    pnl = (sell_price - buy_price) * quantity
                    total_pnl += pnl
                    
                    if pnl > 0:
                        profitable_trades += 1
        
        total_trade_pairs = len(trades) // 2
        win_rate = (profitable_trades / total_trade_pairs) if total_trade_pairs > 0 else 0
        
        print(f"Win Rate: {win_rate:.1%}")
        print(f"Average P&L per trade: ${total_pnl / total_trade_pairs:.2f}" if total_trade_pairs > 0 else "N/A")
        
        if win_rate < 0.4:  # Less than 40% win rate
            print(f"❌ LOW WIN RATE: {win_rate:.1%} suggests poor signal quality")
            print(f"   Recommended: Improve alpha model accuracy or signal filters")
    
    # PROBLEM 3: MOVING AVERAGE STRATEGY ISSUES
    print(f"\n🚨 PROBLEM 3: STRATEGY CONFIGURATION ISSUES")
    print("-" * 30)
    
    # Analyze market data for strategy suitability
    market_data = lean_results.get('market_data', [])
    if market_data:
        prices = [data['price'] for data in market_data]
        returns = [data['returns'] for data in market_data if data['returns'] is not None]
        
        if len(prices) > 20:
            # Calculate short and long MA for the period
            prices_series = pd.Series(prices)
            short_ma = prices_series.rolling(5).mean()
            long_ma = prices_series.rolling(20).mean()
            
            # Count crossovers
            crossovers = 0
            for i in range(20, len(prices)):
                if ((short_ma.iloc[i-1] <= long_ma.iloc[i-1]) and (short_ma.iloc[i] > long_ma.iloc[i])) or \
                   ((short_ma.iloc[i-1] >= long_ma.iloc[i-1]) and (short_ma.iloc[i] < long_ma.iloc[i])):
                    crossovers += 1
            
            crossovers_per_day = crossovers / 31  # August has 31 days
            
            print(f"MA Crossovers: {crossovers} total ({crossovers_per_day:.1f}/day)")
            
            if crossovers_per_day > 1:
                print(f"❌ TOO MANY CROSSOVERS: Strategy parameters may be too sensitive")
                print(f"   Recommended: Increase MA periods (e.g., 10/50 instead of 5/20)")
                print(f"   Recommended: Add confirmation signals or minimum percentage separation")
            
            # Check market volatility
            if returns:
                volatility = np.std(returns) * np.sqrt(24 * 365)  # Annualized hourly volatility
                print(f"Market Volatility: {volatility:.1%} annualized")
                
                if volatility > 1.0:  # More than 100% annual volatility
                    print(f"❌ HIGH VOLATILITY MARKET: {volatility:.1%} may be unsuitable for MA crossover")
                    print(f"   Recommended: Use volatility-adjusted position sizing")
                    print(f"   Recommended: Consider mean reversion strategies instead")
    
    # PROBLEM 4: RISK MANAGEMENT ANALYSIS
    print(f"\n🚨 PROBLEM 4: RISK MANAGEMENT ISSUES")
    print("-" * 30)
    
    max_drawdown = portfolio_results.get('max_drawdown', 0)
    if max_drawdown > 0.10:  # More than 10% drawdown
        print(f"❌ EXCESSIVE DRAWDOWN: {max_drawdown:.1%} suggests inadequate risk controls")
        print(f"   Current limit: 15% - consider lowering to 10%")
        print(f"   Recommended: Implement more conservative position sizing")
        print(f"   Recommended: Add stop-loss mechanisms")
    
    # Check position sizing
    if trades:
        quantities = [abs(trade['quantity']) for trade in trades]
        max_quantity = max(quantities) if quantities else 0
        avg_quantity = np.mean(quantities) if quantities else 0
        
        print(f"Max position size: {max_quantity:.2f} ETH")
        print(f"Avg position size: {avg_quantity:.2f} ETH")
        
        # Estimate position as % of portfolio (rough calculation)
        if market_data:
            avg_price = np.mean([data['price'] for data in market_data])
            max_position_value = max_quantity * avg_price
            max_position_pct = max_position_value / 100000  # Initial capital
            
            print(f"Max position: ~{max_position_pct:.1%} of portfolio")
            
            if max_position_pct > 0.2:  # More than 20%
                print(f"❌ EXCESSIVE POSITION SIZE: {max_position_pct:.1%} is too risky")
                print(f"   Recommended: Limit to 5-10% per position")
    
    # RECOMMENDATIONS SUMMARY
    print(f"\n💡 RECOMMENDED FIXES")
    print("=" * 30)
    print("1. 🎯 REDUCE OVER-TRADING:")
    print("   • Increase MA periods: 10/50 instead of 5/20")
    print("   • Add minimum hold period: 4-6 hours")
    print("   • Require 2%+ MA separation for signals")
    
    print("\n2. 🎯 IMPROVE SIGNAL QUALITY:")
    print("   • Add RSI or MACD confirmation")
    print("   • Filter signals during high volatility periods")
    print("   • Use volume confirmation for breakouts")
    
    print("\n3. 🎯 ENHANCE RISK MANAGEMENT:")
    print("   • Reduce position size to 5-10% max")
    print("   • Lower max drawdown limit to 10%")
    print("   • Implement 3% stop-loss per trade")
    
    print("\n4. 🎯 STRATEGY ALTERNATIVES:")
    print("   • Consider mean reversion during high volatility")
    print("   • Test longer timeframes (4H or daily)")
    print("   • Implement volatility-based position sizing")
    
    print(f"\n📊 NEXT STEPS:")
    print("1. Test with conservative parameters: MA(10,50), 5% max position, 2% signal threshold")
    print("2. Run optimization to find best MA periods for August 2024 data")
    print("3. Implement paper trading with new parameters before live deployment")
    
    return {
        'trades_count': len(trades),
        'win_rate': win_rate if 'win_rate' in locals() else 0,
        'total_return': portfolio_results.get('total_return', 0),
        'max_drawdown': max_drawdown,
        'crossovers_per_day': crossovers_per_day if 'crossovers_per_day' in locals() else 0
    }

def main():
    """Run performance analysis on the latest backtest."""
    
    # Find the most recent backtest
    backtests_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/backtests")
    
    if not backtests_dir.exists():
        print("❌ No backtests directory found")
        return
    
    backtest_dirs = [d for d in backtests_dir.iterdir() if d.is_dir()]
    
    if not backtest_dirs:
        print("❌ No backtest results found")
        return
    
    # Sort by name (contains timestamp) and get the latest
    backtest_dirs.sort(reverse=True)
    latest_backtest = backtest_dirs[0].name
    
    # Run the analysis
    analysis = analyze_backtest_performance(latest_backtest)
    
    print(f"\n✅ Analysis complete for {latest_backtest}")

if __name__ == "__main__":
    main()