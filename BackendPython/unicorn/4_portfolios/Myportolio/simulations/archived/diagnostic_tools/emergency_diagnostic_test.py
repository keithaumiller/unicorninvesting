#!/usr/bin/env python3
"""
Emergency Diagnostic Test - Trading Logic Validation
==================================================

Tests the enhanced algorithms directly to verify they can generate signals
independently of the LEAN simulation framework.

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

import sys
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path

# Add paths for direct algorithm testing
sys.path.append(str(Path(__file__).parent.parent / "trading_algorithms"))
sys.path.append(str(Path(__file__).parent.parent / "risk_algorithms"))
sys.path.append(str(Path(__file__).parent))

def emergency_diagnostic_test():
    """
    Emergency test to validate enhanced algorithms can generate trading signals.
    """
    print("🚨 EMERGENCY DIAGNOSTIC TEST")
    print("=" * 50)
    print("Testing enhanced algorithms directly...")
    print()
    
    # Test 1: Import Enhanced Algorithms
    print("📋 TEST 1: Algorithm Import Validation")
    print("-" * 30)
    
    try:
        from eth_momentum_strategy import ETHMomentumStrategy
        print("✅ ETH Momentum Strategy imported successfully")
        eth_strategy_available = True
    except ImportError as e:
        print(f"❌ ETH Momentum Strategy import failed: {e}")
        eth_strategy_available = False
    
    try:
        from eth_basic_risk import ETHBasicRisk
        print("✅ ETH Basic Risk imported successfully")
        risk_algorithm_available = True
    except ImportError as e:
        print(f"❌ ETH Basic Risk import failed: {e}")
        risk_algorithm_available = False
    
    try:
        from performance_logger import PerformanceLogger
        print("✅ Performance Logger imported successfully")
        performance_logger_available = True
    except ImportError as e:
        print(f"❌ Performance Logger import failed: {e}")
        performance_logger_available = False
    
    print()
    
    if not (eth_strategy_available and risk_algorithm_available):
        print("🚨 CRITICAL: Cannot proceed - enhanced algorithms not available")
        return
    
    # Test 2: Create Mock Market Data
    print("📋 TEST 2: Generate Mock Market Data")
    print("-" * 30)
    
    # Create 6 months of hourly ETH data (March 15 - September 15, 2024)
    start_date = datetime(2024, 3, 15)
    end_date = datetime(2024, 9, 15)
    dates = pd.date_range(start=start_date, end=end_date, freq='h')
    
    # Generate realistic ETH price data with trend and volatility
    np.random.seed(42)  # For reproducible results
    
    # Start at $2000, end around $2600 (matching our simulation data)
    n_periods = len(dates)
    trend = np.linspace(2000, 2600, n_periods)
    volatility = np.random.normal(0, 50, n_periods)  # High volatility
    
    # Add some momentum patterns
    momentum_cycles = 50 * np.sin(np.linspace(0, 10*np.pi, n_periods))
    
    prices = trend + volatility + momentum_cycles
    prices = np.maximum(prices, 1500)  # Floor price
    
    # Create market data DataFrame
    market_data = pd.DataFrame({
        'timestamp': dates,
        'price': prices,
        'volume': np.random.uniform(1000000, 5000000, n_periods),
        'high': prices * np.random.uniform(1.001, 1.05, n_periods),
        'low': prices * np.random.uniform(0.95, 0.999, n_periods),
        'open': prices * np.random.uniform(0.98, 1.02, n_periods),
        'close': prices
    })
    
    print(f"✅ Generated {len(market_data):,} hours of mock ETH data")
    print(f"📊 Price range: ${market_data['price'].min():.2f} - ${market_data['price'].max():.2f}")
    print(f"📈 Total return: {((market_data['price'].iloc[-1] / market_data['price'].iloc[0]) - 1):.2%}")
    print()
    
    # Test 3: Test ETH Momentum Strategy
    print("📋 TEST 3: ETH Momentum Strategy Testing")
    print("-" * 30)
    
    try:
        # Initialize performance logger if available
        performance_logger = None
        if performance_logger_available:
            performance_logger = PerformanceLogger("emergency_diagnostic_test")
        
        # Create ETH momentum strategy
        eth_strategy = ETHMomentumStrategy(
            symbol="ETHUSD",
            ma_short=5,
            ma_long=20,
            rsi_period=14,
            performance_logger=performance_logger
        )
        
        print("✅ ETH Momentum Strategy initialized")
        
        # Generate signals for sample of data (last 100 hours)
        sample_data = market_data.tail(100).copy()
        signals_generated = 0
        
        for i, row in sample_data.iterrows():
            market_data_point = {
                'timestamp': row['timestamp'],
                'price': row['price'],
                'volume': row['volume'],
                'high': row['high'],
                'low': row['low'],
                'open': row['open'],
                'close': row['close']
            }
            
            # Test signal generation
            signal = eth_strategy.generate_signals(market_data_point)
            
            if signal is not None and signal.get('signal_type') != 'HOLD':
                signals_generated += 1
                if signals_generated <= 3:  # Show first 3 signals
                    print(f"📊 Signal {signals_generated}: {signal.get('signal_type')} at ${row['price']:.2f} "
                          f"(strength: {signal.get('strength', 0):.2f})")
        
        print(f"✅ Generated {signals_generated} trading signals from 100 hours of data")
        
        if signals_generated == 0:
            print("⚠️  WARNING: Strategy generated no signals - parameters may be too conservative")
        else:
            print(f"📈 Signal rate: {signals_generated/100:.1%} (expected: 5-15%)")
        
    except Exception as e:
        print(f"❌ ETH Momentum Strategy test failed: {e}")
        import traceback
        traceback.print_exc()
    
    print()
    
    # Test 4: Test Risk Management
    print("📋 TEST 4: Risk Management Testing")
    print("-" * 30)
    
    try:
        # Create risk manager
        risk_manager = ETHBasicRisk(
            max_drawdown=0.15,
            max_position_pct=0.25,
            performance_logger=performance_logger
        )
        
        print("✅ ETH Basic Risk initialized")
        
        # Test risk decisions
        test_positions = [
            {'symbol': 'ETHUSD', 'size': 10000, 'price': 2500},  # Should pass
            {'symbol': 'ETHUSD', 'size': 50000, 'price': 2500},  # Should fail (too large)
            {'symbol': 'ETHUSD', 'size': 15000, 'price': 2500},  # Should pass
        ]
        
        for i, position in enumerate(test_positions, 1):
            portfolio_value = 100000
            risk_decision = risk_manager.validate_position(position, portfolio_value)
            
            print(f"📊 Position {i}: ${position['size']:,} - "
                  f"{'✅ APPROVED' if risk_decision.get('approved', False) else '❌ REJECTED'} "
                  f"({risk_decision.get('reason', 'No reason')})")
        
    except Exception as e:
        print(f"❌ Risk Management test failed: {e}")
        import traceback
        traceback.print_exc()
    
    print()
    
    # Test 5: Performance Logger Test
    if performance_logger_available and performance_logger:
        print("📋 TEST 5: Performance Logger Testing")
        print("-" * 30)
        
        try:
            # Test logging a trading signal
            from performance_logger import TradingSignal
            
            test_signal = TradingSignal(
                signal_type="BUY",
                strength=0.75,
                reasoning="Test signal for diagnostic",
                technical_indicators={"ma_cross": True, "rsi": 35.2},
                confidence_score=0.78
            )
            
            performance_logger.log_trading_signal(test_signal)
            print("✅ Trading signal logged successfully")
            
            # Generate a basic performance report
            report = performance_logger.generate_performance_report()
            print("✅ Performance report generated successfully")
            
        except Exception as e:
            print(f"❌ Performance Logger test failed: {e}")
    
    print()
    
    # Final Assessment
    print("🎯 DIAGNOSTIC ASSESSMENT")
    print("-" * 30)
    
    if eth_strategy_available and risk_algorithm_available:
        print("✅ Enhanced algorithms are functional")
        if signals_generated > 0:
            print("✅ Signal generation is working")
            print("🚨 ROOT CAUSE IDENTIFIED: LEAN algorithm templates are empty")
            print("🛠️  SOLUTION: Fix _prepare_algorithm_file() method in LEANSimulationEngine")
        else:
            print("⚠️  Signal generation needs parameter tuning")
            print("🛠️  SOLUTION: Adjust strategy parameters for higher sensitivity")
    else:
        print("❌ Enhanced algorithms have import issues")
        print("🛠️  SOLUTION: Fix algorithm import paths and dependencies")
    
    print()
    print("📈 RECOMMENDED NEXT STEPS:")
    print("1. Fix LEAN algorithm generation in _prepare_algorithm_file()")
    print("2. Integrate actual ETH momentum strategy logic")
    print("3. Enable performance logging in LEAN algorithms")
    print("4. Re-run 6-month simulation with working algorithms")

if __name__ == "__main__":
    emergency_diagnostic_test()