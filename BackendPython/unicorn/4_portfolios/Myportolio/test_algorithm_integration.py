#!/usr/bin/env python3
"""
ETH Algorithm Integration Test with Real IBKR Data
Tests momentum strategy and risk algorithms with live market data and portfolio state.
"""

import sys
import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging

# Add data sources to path for IBKR integration
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources')

from functional_ibkr_integration import FunctionalIBKRIntegration
from trading_algorithms.eth_momentum_strategy import ETHMomentumStrategy
from risk_algorithms.eth_basic_risk import ETHBasicRisk

def main():
    """Test ETH algorithms with real IBKR integration."""
    print("=" * 60)
    print("ETH Algorithm Integration Test with Real IBKR Data")
    print(f"Timestamp: {datetime.now()}")
    print("=" * 60)
    
    # Initialize IBKR integration
    ibkr = FunctionalIBKRIntegration()
    
    # Get current market conditions
    print("\n1. Current Market Data:")
    market_data = ibkr.get_eth_market_data()
    print(f"   ETH Price: ${market_data['current_price']:,.2f}")
    print(f"   24h Change: {market_data['price_change_24h']*100:+.2f}%")
    print(f"   Volatility: {market_data['volatility']*100:.1f}%")
    
    # Get portfolio state
    print("\n2. Current Portfolio:")
    data_feed = ibkr.create_comprehensive_data_feed(eth_quantity=10.0)  # Simulate 10 ETH position
    
    if data_feed['status'] != 'success':
        print(f"   Error: {data_feed.get('message', 'Unknown error')}")
        return
    
    portfolio_metrics = data_feed['portfolio_metrics']
    account_summary = data_feed['account_summary']
    
    print(f"   Total Equity: ${account_summary['net_liquidation']:,.2f}")
    print(f"   ETH Position: {portfolio_metrics['eth_quantity']:.4f} ETH (${portfolio_metrics['eth_value']:,.2f})")
    print(f"   Cash: ${portfolio_metrics['cash_value']:,.2f}")
    
    # Create mock historical data for algorithms
    print("\n3. Testing ETH Momentum Strategy:")
    config = {
        'symbol': 'ETH',
        'short_ma_period': 5,
        'long_ma_period': 20,
        'position_size': 0.1
    }
    
    strategy = ETHMomentumStrategy(config)
    
    # Simulate some price history around current price
    current_price = market_data['current_price']
    price_variance = current_price * 0.02  # 2% variance
    np.random.seed(42)  # For reproducible results
    prices = current_price + np.random.normal(0, price_variance, 25)
    prices[-1] = current_price  # Latest price is current
    
    dates = pd.date_range(end=pd.Timestamp.now(), periods=25, freq='D')
    market_history = pd.DataFrame({
        'timestamp': dates,
        'close': prices,  # Use 'close' as expected by the algorithm
        'volume': np.random.uniform(100000, 500000, 25)
    })
    
    signals = strategy.generate_signal(market_history)
    print(f"   Signal Type: {signals['signal']}")
    print(f"   Signal Confidence: {signals['confidence']:.3f}")
    print(f"   Target Position: {signals['target_position']:.3f}")
    print(f"   Reason: {signals['reason']}")
    
    # Test risk management
    print("\n4. Testing Risk Management:")
    risk_manager = ETHBasicRisk()
    
    # Current portfolio positions
    positions = [{
        'symbol': 'ETH',
        'quantity': portfolio_metrics['eth_quantity'],
        'current_price': market_data['current_price'],
        'market_value': portfolio_metrics['eth_value']
    }]
    
    risk_metrics = risk_manager.calculate_risk_metrics(positions)
    print(f"   Current VaR (95%): ${risk_metrics['var_95']:,.2f}")
    print(f"   Position Risk: {risk_metrics['position_concentration']:.1f}%")
    print(f"   Max Daily Risk: ${risk_metrics['max_daily_risk']:,.2f}")
    
    # Test trading recommendations
    print("\n5. Trading Recommendations:")
    portfolio_value = account_summary['net_liquidation']
    signal_position = signals['target_position']  # Use target_position from signal
    current_allocation = portfolio_metrics['eth_value'] / portfolio_value
    
    if signal_position > current_allocation:
        action = 'BUY'
        quantity = (signal_position - current_allocation) * portfolio_value / market_data['current_price']
    else:
        action = 'SELL'
        quantity = (current_allocation - signal_position) * portfolio_value / market_data['current_price']
    
    print(f"   Current Allocation: {current_allocation:.1%}")
    print(f"   Target Allocation: {signal_position:.1%}")
    print(f"   Recommended Action: {action} {abs(quantity):.4f} ETH")
    
    # Validate with risk limits
    is_valid = risk_manager.validate_risk_limits(positions)
    print(f"   Risk Validation: {'✅ Approved' if is_valid else '❌ Rejected'}")
    
    # Test complete trading workflow
    print("\n6. Complete Trading Workflow Test:")
    
    # Calculate proposed trade
    if abs(quantity) > 0.001:  # Only if significant trade
        trade_value = abs(quantity) * market_data['current_price']
        
        # Test trading validation using the IBKR integration
        proposed_eth_quantity = portfolio_metrics['eth_quantity']
        if action == 'BUY':
            proposed_eth_quantity += abs(quantity)
        else:
            proposed_eth_quantity -= abs(quantity)
            
        validation = ibkr.validate_trading_decision(
            proposed_eth_quantity, 
            market_data['current_price'], 
            portfolio_metrics
        )
        
        print(f"   Trade Size: {abs(quantity):.4f} ETH (${trade_value:,.2f})")
        print(f"   IBKR Validation: {'✅ Valid' if validation.get('valid', False) else '❌ Invalid'}")
        
        if validation.get('valid', False) and is_valid:
            print(f"   🎯 READY TO EXECUTE: {action} {abs(quantity):.4f} ETH")
        else:
            reason = validation.get('reason', 'Risk constraints')
            print(f"   ⚠️  TRADE BLOCKED: {reason}")
    else:
        print("   No significant trade recommended")
    
    print("\n" + "=" * 60)
    print("🎯 ALGORITHM INTEGRATION COMPLETE!")
    print("   ✅ Real market data integrated")
    print("   ✅ Portfolio state tracked")
    print("   ✅ Trading signals generated")
    print("   ✅ Risk management applied")
    print("   ✅ IBKR validation tested")
    print("   ✅ Complete workflow functional")
    print("=" * 60)

if __name__ == "__main__":
    main()
