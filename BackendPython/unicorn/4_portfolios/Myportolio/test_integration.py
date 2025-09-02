#!/usr/bin/env python3
"""
Complete ETH Kelly Integration Test
Tests the integrated portfolio with proper configuration
"""

import json
import pandas as pd
import numpy as np
from eth_kelly_integration import ETHKellyIntegratedPortfolio

def main():
    print('=== ETH Kelly Integration Test ===')
    
    # Load configuration
    try:
        with open('config/eth_kelly_config.json', 'r') as f:
            config = json.load(f)
        
        print(f'✅ Config loaded: {config["portfolio_name"]}')
        print(f'   Initial capital: ${config["initial_capital"]:,.2f}')
        print(f'   Symbol: {config["trading"]["symbol"]}')
        print(f'   Max position: {config["trading"]["max_position_size"]:.1%}')
        
    except Exception as e:
        print(f'❌ Error loading config: {e}')
        return
    
    # Initialize integrated portfolio
    try:
        portfolio = ETHKellyIntegratedPortfolio(config)
        print(f'✅ Portfolio initialized successfully')
        
    except Exception as e:
        print(f'❌ Error initializing portfolio: {e}')
        import traceback
        traceback.print_exc()
        return
    
    # Generate realistic test data
    print(f'\n=== Generating Test Data ===')
    periods = 60
    dates = pd.date_range('2024-01-01', periods=periods, freq='D')
    base_price = 3000
    volatility = 0.04
    
    # Generate more realistic price series with momentum
    prices = []
    current_price = base_price
    
    for i in range(periods):
        # Add some trend and volatility
        trend = 0.0005 if i > 30 else -0.0003  # Market regime change
        random_change = np.random.normal(trend, volatility)
        current_price = current_price * (1 + random_change)
        current_price = max(current_price, base_price * 0.7)  # Floor at 30% drop
        prices.append(current_price)
    
    market_data = pd.DataFrame({
        'timestamp': dates,
        'close': prices,  # Changed from 'price' to 'close' to match expected format
        'volume': np.random.uniform(1000, 5000, periods)
    })
    
    print(f'✅ Generated {len(market_data)} days of test data')
    print(f'   Price range: ${market_data["close"].min():.2f} - ${market_data["close"].max():.2f}')
    print(f'   Price change: {((market_data["close"].iloc[-1] / market_data["close"].iloc[0]) - 1):.2%}')
    
    # Test portfolio update
    print(f'\n=== Testing Portfolio Update ===')
    try:
        result = portfolio.process_market_data(market_data)
        
        print(f'✅ Portfolio update successful')
        print(f'\n=== Results Summary ===')
        print(f'Signal: {result.get("signal", "N/A")}')
        print(f'Position size: {result.get("position_size", 0):.4f}')
        print(f'Portfolio value: ${result.get("portfolio_value", 0):,.2f}')
        print(f'Risk status: {result.get("risk_status", "N/A")}')
        
        if 'kelly_metrics' in result:
            kelly = result['kelly_metrics']
            print(f'\n=== Kelly Criterion Metrics ===')
            print(f'Win rate: {kelly.get("win_rate", 0):.2%}')
            print(f'Average win/loss ratio: {kelly.get("avg_win_loss", 0):.2f}')
            print(f'Kelly fraction: {kelly.get("kelly_fraction", 0):.4f}')
            print(f'Adjusted fraction: {kelly.get("adjusted_fraction", 0):.4f}')
        
        if 'risk_metrics' in result:
            risk = result['risk_metrics']
            print(f'\n=== Risk Management Metrics ===')
            print(f'Current drawdown: {risk.get("current_drawdown", 0):.2%}')
            print(f'Portfolio heat: {risk.get("portfolio_heat", 0):.2%}')
            print(f'VaR (5%): {risk.get("var_5", 0):.2%}')
        
        if 'trading_metrics' in result:
            trading = result['trading_metrics']
            print(f'\n=== Trading Strategy Metrics ===')
            print(f'Short MA: {trading.get("short_ma", 0):.2f}')
            print(f'Long MA: {trading.get("long_ma", 0):.2f}')
            print(f'Momentum signal: {trading.get("momentum_signal", 0):.4f}')
        
        print(f'\n✅ ETH Kelly Integration Test: COMPLETE SUCCESS')
        return True
        
    except Exception as e:
        print(f'❌ Error during portfolio update: {e}')
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)
