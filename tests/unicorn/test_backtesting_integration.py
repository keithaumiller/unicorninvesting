#!/usr/bin/env python3
"""
Test Backtesting Integration Framework
Tests the backtesting integration with live data pipeline
"""

import sys
import os
import json
import logging
import unittest
from pathlib import Path

# Add project paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/core')

class TestBacktestingIntegration(unittest.TestCase):
    """Test backtesting integration framework"""
    
    def setUp(self):
        """Set up test environment"""
        from backtesting_integration import BacktestingEngine
        self.backtest_engine = BacktestingEngine(initial_capital=100000.0)
    
    def test_backtesting_integration_with_live_data(self):
        """Test backtesting integration with live data pipeline"""
        
        print("🔬 BACKTESTING INTEGRATION TEST")
        print("=" * 50)
        
        try:
            from live_market_data_feed import LiveMarketDataFeed
            from simplified_ensemble_portfolio import EnsembleMultiAssetPortfolio
            
            # Get live market data
            market_feed = LiveMarketDataFeed()
            eth_price = market_feed.get_crypto_price('ETH')
            
            print(f"📊 Live ETH Price: ${eth_price:,.2f}")
            
            # Generate market data for backtesting
            market_data = market_feed.generate_realistic_market_data('ETH', eth_price, periods=20)
            
            # Initialize trading system
            portfolio = EnsembleMultiAssetPortfolio()
            
            print(f"\n🔄 Processing {len(market_data)} data points through backtesting...")
            
            # Process each data point through the pipeline
            for i, (timestamp, row) in enumerate(market_data.iterrows()):
                # Prepare market data for signal generation
                historical_data = market_data.iloc[:i+1] if i >= 10 else market_data.iloc[:11]
                market_dict = {'ETH': historical_data}
                
                # Generate trading signal
                signals = portfolio._generate_simple_momentum_signals(market_dict)
                eth_signal = signals.get('ETH', 0.0)
                
                # Calculate position size
                positions = portfolio.calculate_optimal_positions(signals, market_dict)
                eth_position = positions.get('ETH', 0.0)
                
                # Create features dictionary
                features = {
                    'sma_5': float(historical_data['close'].rolling(5).mean().iloc[-1]),
                    'sma_10': float(historical_data['close'].rolling(10).mean().iloc[-1]),
                    'volume': float(row['volume']),
                    'returns': float(row['returns'])
                }
                
                # Process through backtesting
                backtest_result = self.backtest_engine.process_data_point(
                    timestamp=timestamp,
                    symbol='ETH',
                    price=row['close'],
                    signal=eth_signal,
                    position_size=eth_position,
                    features=features
                )
                
                # Show progress for key points
                if i % 5 == 0 or i == len(market_data) - 1:
                    print(f"   Point {i+1}/{len(market_data)}: Price=${row['close']:.2f}, Signal={eth_signal:.3f}, Position={eth_position:.2%}")
            
            # Get final performance summary
            performance = self.backtest_engine.get_performance_summary()
            
            print(f"\n📈 BACKTESTING RESULTS:")
            print(f"   Initial Capital: ${performance['initial_capital']:,.2f}")
            print(f"   Final Value: ${performance['final_portfolio_value']:,.2f}")
            print(f"   Total Return: {performance['total_return']:.2%}")
            print(f"   Total Trades: {performance['total_trades']}")
            print(f"   Data Points: {performance['data_points_processed']}")
            print(f"   Performance: {performance['performance_trend']}")
            
            # Export results
            output_file = '/tmp/backtest_results.json'
            if self.backtest_engine.export_standard_format(output_file):
                print(f"\n✅ Backtest results exported to: {output_file}")
                
                # Show sample of exported data
                with open(output_file, 'r') as f:
                    backtest_data = json.load(f)
                
                print(f"\n📋 Export Summary:")
                print(f"   Backtest ID: {backtest_data['BacktestId']}")
                print(f"   Date Range: {backtest_data['StartDate'][:10]} to {backtest_data['EndDate'][:10]}")
                print(f"   Total Return: {backtest_data['TotalReturn']:.2%}")
                print(f"   Trades Executed: {len(backtest_data['Trades'])}")
            
            print(f"\n✅ BACKTESTING INTEGRATION COMPLETE")
            print(f"🔄 Data pipeline successfully integrated with backtesting framework")
            
            # Assertions for test validation
            self.assertIsNotNone(performance)
            self.assertGreater(performance['data_points_processed'], 0)
            self.assertIsInstance(performance['total_return'], float)
            
        except Exception as e:
            print(f"❌ Backtesting integration error: {e}")
            self.fail(f"Backtesting integration test failed: {e}")

def main():
    """Run backtesting integration test"""
    logging.basicConfig(level=logging.INFO)
    unittest.main(verbosity=2)

if __name__ == "__main__":
    main()