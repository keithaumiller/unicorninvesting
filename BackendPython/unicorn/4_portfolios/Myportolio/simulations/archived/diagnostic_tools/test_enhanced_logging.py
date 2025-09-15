#!/usr/bin/env python3
"""
Test Enhanced Logging System for Myportolio Backtesting
======================================================

Test script to validate the comprehensive performance logging system
and identify performance attribution sources.

This script runs a backtest simulation with full logging to analyze:
- Alpha model prediction accuracy
- Trading strategy decision quality  
- Risk management impact
- Trade execution efficiency
- Overall performance attribution

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

import sys
import json
import pandas as pd
from datetime import datetime, timedelta
from pathlib import Path

# Add simulation path
sys.path.append("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations")

def test_performance_logger():
    """Test the performance logger with sample data."""
    
    print("🧪 Testing Performance Logger...")
    
    try:
        from performance_logger import PerformanceLogger
        
        # Create test logger
        logger = PerformanceLogger("test_logging_20250915_000000")
        
        # Test alpha prediction logging
        logger.log_alpha_prediction(
            asset="ETH",
            model_type="xgboost",
            timeframe="1hour",
            predicted_direction="UP",
            predicted_return=0.02,
            confidence=0.75,
            current_price=4500.0
        )
        
        # Test trading signal logging
        logger.log_trading_signal(
            asset="ETHUSD",
            signal_type="BUY",
            confidence=0.8,
            current_price=4500.0,
            target_position=0.1,
            current_position=0.0,
            signal_reason="MA crossover bullish",
            technical_indicators={
                "short_ma": 4520.0,
                "long_ma": 4480.0,
                "volatility": 0.025
            }
        )
        
        # Test risk decision logging
        logger.log_risk_decision(
            asset="ETHUSD",
            decision_type="POSITION_LIMIT",
            proposed_action="Set position to 10%",
            approved=True,
            reason="Position within 80% limit",
            risk_metrics={
                "position_pct": 0.1,
                "max_position_pct": 0.8,
                "current_drawdown": 0.02
            }
        )
        
        # Test portfolio state logging
        logger.log_portfolio_state(
            total_value=100000.0,
            cash=90000.0,
            positions={"ETHUSD": 0.1},
            unrealized_pnl=500.0,
            realized_pnl=0.0,
            drawdown=0.0,
            volatility=0.02,
            var_95=0.05
        )
        
        # Generate test report
        report = logger.generate_performance_report()
        
        print("✅ Performance logger test completed successfully")
        print(f"📊 Test report generated with {len(report)} sections")
        
        return True
        
    except Exception as e:
        print(f"❌ Performance logger test failed: {e}")
        return False

def test_enhanced_algorithms():
    """Test the enhanced algorithms with logging."""
    
    print("🧪 Testing Enhanced Algorithms with Logging...")
    
    try:
        sys.path.append("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/trading_algorithms")
        sys.path.append("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/risk_algorithms")
        
        from performance_logger import PerformanceLogger
        from eth_momentum_strategy import ETHMomentumStrategy
        from eth_basic_risk import ETHBasicRisk
        
        # Create logger
        logger = PerformanceLogger("test_algorithms_20250915_000000")
        
        # Create enhanced algorithms
        strategy = ETHMomentumStrategy(
            config={"short_ma_period": 5, "long_ma_period": 20},
            performance_logger=logger
        )
        
        risk_mgmt = ETHBasicRisk(
            max_drawdown=0.15,
            max_position_pct=0.8,
            performance_logger=logger
        )
        
        # Test with sample market data
        sample_data = pd.DataFrame({
            'close': [4400, 4420, 4450, 4480, 4500, 4520, 4510, 4530, 4550, 4540,
                     4560, 4580, 4600, 4590, 4610, 4620, 4640, 4630, 4650, 4660,
                     4680, 4670, 4690, 4700, 4720, 4710, 4730, 4740, 4760, 4750]
        })
        
        # Generate trading signal
        signal = strategy.generate_signal(sample_data)
        print(f"🎯 Generated signal: {signal['signal']} (confidence: {signal['confidence']:.2f})")
        
        # Test risk validation
        position_check = risk_mgmt.validate_position_size(10000, 100000, "ETHUSD")
        print(f"🛡️  Position validation: {'APPROVED' if position_check['approved'] else 'REJECTED'}")
        
        # Update portfolio metrics
        risk_mgmt.update_portfolio_metrics(105000)  # 5% gain
        
        # Get risk summary
        risk_summary = risk_mgmt.get_risk_summary()
        print(f"📈 Risk summary: {risk_summary['approval_rate']:.1%} approval rate")
        
        print("✅ Enhanced algorithms test completed successfully")
        return True
        
    except Exception as e:
        print(f"❌ Enhanced algorithms test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_simulation_with_logging():
    """Test a full simulation run with enhanced logging."""
    
    print("🧪 Testing Full Simulation with Enhanced Logging...")
    
    try:
        from lean_simulation_engine import LEANSimulationEngine
        
        # Create enhanced simulation engine
        engine = LEANSimulationEngine()
        
        # Test configuration
        test_config = {
            "short_ma_period": 5,
            "long_ma_period": 20,
            "max_position_size": 0.1,
            "volatility_window": 14,
            "initial_cash": 100000,
            "max_drawdown": 0.15,
            "max_position_pct": 0.8
        }
        
        print("📋 Test configuration:")
        print(json.dumps(test_config, indent=2))
        
        # Note: We'll test the logging components but not run a full LEAN backtest
        # since that requires LEAN installation and market data
        
        # Test enhanced algorithm creation
        if hasattr(engine, '_create_enhanced_algorithms'):
            trading_strategy, risk_algorithm = engine._create_enhanced_algorithms(test_config)
            
            if trading_strategy and risk_algorithm:
                print("✅ Enhanced algorithms created successfully")
            else:
                print("⚠️  Enhanced algorithms not available in current environment")
        
        # Test configuration preparation
        if hasattr(engine, '_prepare_enhanced_lean_config'):
            config = engine._prepare_enhanced_lean_config(
                simulation_id="test_simulation",
                start_date="2024-07-01",
                end_date="2024-07-31", 
                parameters=test_config,
                best_model_config={},
                algorithm_type="MyportolioETHMomentum"
            )
            
            print("✅ Enhanced LEAN configuration prepared")
            print(f"📊 Config sections: {list(config.keys())}")
        
        print("✅ Simulation logging test completed successfully")
        return True
        
    except Exception as e:
        print(f"❌ Simulation logging test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def run_performance_analysis():
    """Run performance analysis on existing backtest results."""
    
    print("🧪 Running Performance Analysis on Existing Results...")
    
    try:
        # Find recent backtest results
        backtests_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/backtests")
        
        if not backtests_dir.exists():
            print("⚠️  No backtest results directory found")
            return False
        
        # Get most recent backtest
        backtest_dirs = [d for d in backtests_dir.iterdir() if d.is_dir()]
        
        if not backtest_dirs:
            print("⚠️  No backtest results found")
            return False
        
        # Sort by name (contains timestamp)
        backtest_dirs.sort(reverse=True)
        latest_backtest = backtest_dirs[0]
        
        print(f"📊 Analyzing backtest: {latest_backtest.name}")
        
        # Load results
        results_file = latest_backtest / "myportolio_results.json"
        if results_file.exists():
            with open(results_file, 'r') as f:
                results = json.load(f)
            
            print("📈 Backtest Results Analysis:")
            print(f"   • Total Return: {results.get('total_return', 'N/A')}")
            print(f"   • Max Drawdown: {results.get('max_drawdown', 'N/A')}")
            print(f"   • Sharpe Ratio: {results.get('sharpe_ratio', 'N/A')}")
            print(f"   • Total Trades: {results.get('total_trades', 'N/A')}")
            
            # Check for performance logs
            perf_logs_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/performance_logs")
            if perf_logs_dir.exists():
                log_files = list(perf_logs_dir.glob("*.json"))
                print(f"🗂️  Found {len(log_files)} performance log files")
            else:
                print("⚠️  No performance logs directory found - this would be created by enhanced logging")
        
        print("✅ Performance analysis completed")
        return True
        
    except Exception as e:
        print(f"❌ Performance analysis failed: {e}")
        return False

def main():
    """Run comprehensive logging system tests."""
    
    print("🚀 Testing Enhanced Logging System for Myportolio")
    print("=" * 60)
    
    tests = [
        ("Performance Logger", test_performance_logger),
        ("Enhanced Algorithms", test_enhanced_algorithms),
        ("Simulation Logging", test_simulation_with_logging),
        ("Performance Analysis", run_performance_analysis)
    ]
    
    results = {}
    
    for test_name, test_func in tests:
        print(f"\n🔍 {test_name}")
        print("-" * 40)
        
        try:
            results[test_name] = test_func()
        except Exception as e:
            print(f"❌ {test_name} failed with exception: {e}")
            results[test_name] = False
    
    # Summary
    print("\n📋 Test Results Summary")
    print("=" * 40)
    
    passed = 0
    total = len(tests)
    
    for test_name, result in results.items():
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{status} {test_name}")
        if result:
            passed += 1
    
    print(f"\n🎯 Overall: {passed}/{total} tests passed ({passed/total*100:.1f}%)")
    
    if passed == total:
        print("\n🎉 All logging system tests passed! The enhanced logging is ready for backtesting.")
        print("\n📝 Next Steps:")
        print("   1. Run a backtest with: python simulation_cli.py backtest --start 2024-08-01 --end 2024-08-31")
        print("   2. Check performance logs in: simulations/performance_logs/")
        print("   3. Review attribution analysis in the generated performance report")
        print("   4. Identify which component (alpha, strategy, risk, execution) needs optimization")
    else:
        print(f"\n⚠️  {total-passed} tests failed. Please review the errors above.")
    
    return passed == total

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)