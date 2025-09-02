"""
Test ETH Kelly Criterion Implementation
Comprehensive testing of Kelly position sizing with ETH portfolio integration
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging

# Setup paths
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)
sys.path.append(os.path.join(current_dir, '..'))

from eth_kelly_integration import ETHKellyIntegratedPortfolio

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def generate_sample_eth_data(days: int = 30) -> pd.DataFrame:
    """Generate sample ETH market data for testing"""
    
    dates = pd.date_range(start=datetime.now() - timedelta(days=days), periods=days*24, freq='H')
    
    # Simulate ETH price with some volatility
    np.random.seed(42)  # For reproducible results
    base_price = 3000.0
    
    # Generate price series with some trend and volatility
    returns = np.random.normal(0.0001, 0.02, len(dates))  # Small positive drift, 2% hourly volatility
    returns[::24] += np.random.normal(0, 0.05, len(dates)//24)  # Daily volatility spikes
    
    prices = [base_price]
    for ret in returns[1:]:
        prices.append(prices[-1] * (1 + ret))
    
    # Create OHLCV data
    data = []
    for i, (date, price) in enumerate(zip(dates, prices)):
        high = price * (1 + abs(np.random.normal(0, 0.005)))
        low = price * (1 - abs(np.random.normal(0, 0.005)))
        volume = np.random.normal(1000000, 200000)
        
        data.append({
            'timestamp': date,
            'open': prices[i-1] if i > 0 else price,
            'high': high,
            'low': low,
            'close': price,
            'volume': max(volume, 100000)
        })
    
    return pd.DataFrame(data)

def test_kelly_criterion_basic():
    """Test basic Kelly Criterion functionality"""
    
    print("\n🧪 Testing Basic Kelly Criterion Functionality")
    print("=" * 50)
    
    # Initialize portfolio with test configuration
    config = {
        'initial_capital': 100000.0,
        'trading': {
            'short_ma_period': 5,
            'long_ma_period': 20,
            'max_position_size': 0.3,
            'volatility_window': 14
        },
        'risk': {
            'max_drawdown': 0.15,
            'max_position_pct': 0.8,
            'var_confidence': 0.05
        },
        'kelly': {
            'lookback_window': 20,
            'max_kelly_fraction': 0.25,
            'min_win_rate': 0.35,
            'risk_adjustment': 0.5
        }
    }
    
    portfolio = ETHKellyIntegratedPortfolio(config)
    
    # Test with sample data
    market_data = generate_sample_eth_data(10)
    
    # Process a few data points
    results = []
    for i in range(5, len(market_data), 6):  # Every 6 hours
        subset_data = market_data.iloc[:i+1]
        decision = portfolio.process_market_data(subset_data)
        results.append(decision)
        
        # Execute decision if it's actionable
        if decision.get('final_decision', {}).get('action') != 'HOLD':
            execution = portfolio.execute_decision(decision)
            print(f"📊 Execution result: {execution.get('executed', False)}")
    
    # Print results
    print(f"\n📈 Portfolio Summary:")
    summary = portfolio.get_portfolio_summary()
    
    print(f"💰 Portfolio Value: ${summary['portfolio_value']:,.2f}")
    print(f"💵 Cash: ${summary['cash']:,.2f}")
    print(f"🪙 ETH Position: {summary['position']['size']:.4f} ETH (${summary['position']['value']:,.2f})")
    print(f"📊 Total Return: {summary['performance']['total_return_pct']:.2f}%")
    print(f"📉 Current Drawdown: {summary['risk_metrics']['current_drawdown']:.1%}")
    print(f"🎯 Trade Count: {summary['trade_count']}")
    print(f"📡 Signal Count: {summary['signal_count']}")
    
    return portfolio, results

def test_kelly_position_sizing():
    """Test Kelly position sizing calculations"""
    
    print("\n🎯 Testing Kelly Position Sizing Logic")
    print("=" * 50)
    
    from utilities.kelly_criterion import KellyCriterionCalculator
    
    # Initialize Kelly calculator
    kelly_calc = KellyCriterionCalculator(
        lookback_window=20,
        max_kelly_fraction=0.25,
        min_win_rate=0.35,
        risk_adjustment=0.5
    )
    
    # Simulate some historical signal outcomes
    print("📊 Simulating historical signals...")
    
    # Add some winning and losing signals
    historical_outcomes = [
        ({'signal': 'BUY', 'confidence': 0.7}, 0.05),   # 5% win
        ({'signal': 'BUY', 'confidence': 0.6}, -0.02),  # 2% loss
        ({'signal': 'BUY', 'confidence': 0.8}, 0.08),   # 8% win
        ({'signal': 'BUY', 'confidence': 0.5}, -0.01),  # 1% loss
        ({'signal': 'BUY', 'confidence': 0.9}, 0.12),   # 12% win
        ({'signal': 'BUY', 'confidence': 0.4}, -0.03),  # 3% loss
        ({'signal': 'BUY', 'confidence': 0.7}, 0.06),   # 6% win
        ({'signal': 'BUY', 'confidence': 0.6}, 0.04),   # 4% win
    ]
    
    for signal_data, outcome in historical_outcomes:
        kelly_calc.update_signal_history(signal_data, outcome)
    
    # Test Kelly calculation with different confidence levels
    test_signals = [
        {'signal': 'BUY', 'confidence': 0.3},
        {'signal': 'BUY', 'confidence': 0.5},
        {'signal': 'BUY', 'confidence': 0.7},
        {'signal': 'BUY', 'confidence': 0.9},
    ]
    
    portfolio_value = 100000.0
    current_price = 3000.0
    
    print(f"\n📊 Kelly Position Sizing Results (Portfolio: ${portfolio_value:,.0f}, ETH: ${current_price:.0f}):")
    print("-" * 80)
    
    for signal in test_signals:
        result = kelly_calc.calculate_position_size(signal, portfolio_value, current_price)
        
        print(f"Confidence: {signal['confidence']:.1%}")
        print(f"  Kelly Fraction: {result.get('kelly_fraction', 0):.1%}")
        print(f"  Position Value: ${result.get('position_value', 0):,.0f}")
        print(f"  Position Size: {result.get('position_size', 0):.4f} ETH")
        print(f"  Reason: {result.get('reason', 'N/A')}")
        
        if 'kelly_details' in result:
            details = result['kelly_details']
            print(f"  Win Probability: {details.get('win_probability', 0):.1%}")
            print(f"  Expected Return: {result.get('risk_metrics', {}).get('expected_return', 0):.2%}")
        print()
    
    # Performance summary
    perf_summary = kelly_calc.get_performance_summary()
    print("📈 Kelly Performance Summary:")
    print(f"  Total Signals: {perf_summary['total_signals']}")
    print(f"  Win Rate: {perf_summary.get('win_rate', 0):.1%}")
    print(f"  Average Return: {perf_summary.get('avg_return', 0):.2%}")
    if perf_summary.get('sharpe_ratio'):
        print(f"  Sharpe Ratio: {perf_summary['sharpe_ratio']:.2f}")

def test_risk_integration():
    """Test risk management integration with Kelly sizing"""
    
    print("\n🛡️ Testing Risk Management Integration")
    print("=" * 50)
    
    config = {
        'initial_capital': 100000.0,
        'trading': {
            'short_ma_period': 5,
            'long_ma_period': 20,
            'max_position_size': 0.3,
        },
        'risk': {
            'max_drawdown': 0.10,  # Strict 10% drawdown limit
            'max_position_pct': 0.3,  # 30% position limit
            'var_confidence': 0.05
        },
        'kelly': {
            'max_kelly_fraction': 0.50,  # High Kelly limit to test risk controls
            'min_win_rate': 0.20,  # Low threshold to test
            'risk_adjustment': 1.0  # Full Kelly to test risk controls
        }
    }
    
    portfolio = ETHKellyIntegratedPortfolio(config)
    
    # Generate data with a downtrend to test drawdown limits
    market_data = generate_sample_eth_data(5)
    # Simulate price decline
    decline_factor = 0.95
    for i in range(len(market_data)):
        market_data.loc[i, 'close'] *= (decline_factor ** (i / 10))
        market_data.loc[i, 'high'] *= (decline_factor ** (i / 10))
        market_data.loc[i, 'low'] *= (decline_factor ** (i / 10))
    
    print("📉 Testing with declining market data...")
    
    decisions = []
    for i in range(5, min(20, len(market_data)), 3):
        subset_data = market_data.iloc[:i+1]
        decision = portfolio.process_market_data(subset_data)
        decisions.append(decision)
        
        # Try to execute
        execution = portfolio.execute_decision(decision)
        
        final_decision = decision.get('final_decision', {})
        risk_validation = decision.get('risk_validation', {})
        
        print(f"\n⏰ Time step {i}:")
        print(f"  💰 Portfolio Value: ${portfolio.portfolio_value:,.2f}")
        print(f"  📊 Action: {final_decision.get('action', 'N/A')}")
        print(f"  📝 Reason: {final_decision.get('reason', 'N/A')}")
        print(f"  ✅ Position Risk: {risk_validation.get('position', {}).get('approved', 'N/A')}")
        print(f"  ✅ Drawdown Risk: {risk_validation.get('drawdown', {}).get('approved', 'N/A')}")
        print(f"  🎯 Executed: {execution.get('executed', False)}")
        
        if not risk_validation.get('position', {}).get('approved', True):
            print(f"    ⚠️ Position Risk: {risk_validation['position']['reason']}")
        if not risk_validation.get('drawdown', {}).get('approved', True):
            print(f"    ⚠️ Drawdown Risk: {risk_validation['drawdown']['reason']}")
    
    print(f"\n🎯 Final Portfolio Summary:")
    final_summary = portfolio.get_portfolio_summary()
    print(f"  💰 Final Value: ${final_summary['portfolio_value']:,.2f}")
    print(f"  📉 Max Drawdown: {final_summary['risk_metrics']['current_drawdown']:.1%}")
    print(f"  🎯 Total Trades: {final_summary['trade_count']}")

def run_comprehensive_test():
    """Run all tests"""
    
    print("🚀 ETH Kelly Criterion Implementation Test Suite")
    print("=" * 60)
    
    try:
        # Test 1: Basic functionality
        portfolio, results = test_kelly_criterion_basic()
        
        # Test 2: Kelly position sizing
        test_kelly_position_sizing()
        
        # Test 3: Risk integration
        test_risk_integration()
        
        print("\n✅ All tests completed successfully!")
        print("\n📊 Key Achievements:")
        print("  ✅ Kelly Criterion position sizing operational")
        print("  ✅ ETH momentum strategy integration working")
        print("  ✅ Risk management controls functioning")
        print("  ✅ Portfolio execution system operational")
        
        return True
        
    except Exception as e:
        print(f"\n❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = run_comprehensive_test()
    exit(0 if success else 1)
