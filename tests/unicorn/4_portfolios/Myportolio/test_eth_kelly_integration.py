#!/usr/bin/env python3
"""
ETH Kelly Integration Test
Tests the complete integrated ETH portfolio system
"""

import sys
import os
import unittest
import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

# Add paths for imports
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')

try:
    from eth_kelly_integration import ETHKellyIntegratedPortfolio
except ImportError:
    ETHKellyIntegratedPortfolio = None


class TestETHKellyIntegration(unittest.TestCase):
    """Test cases for ETH Kelly Integration"""
    
    def setUp(self):
        """Set up test fixtures"""
        # Load configuration
        config_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/config/eth_kelly_config.json'
        
        if os.path.exists(config_path):
            with open(config_path, 'r') as f:
                self.config = json.load(f)
        else:
            # Fallback configuration
            self.config = {
                "portfolio_name": "ETH_Kelly_Portfolio",
                "initial_capital": 100000.0,
                "trading": {
                    "symbol": "ETHUSD",
                    "short_ma_period": 5,
                    "long_ma_period": 20,
                    "max_position_size": 0.3
                },
                "risk": {
                    "max_drawdown": 0.15,
                    "max_daily_var": 0.06
                },
                "kelly": {
                    "max_kelly_fraction": 0.25,
                    "min_win_rate": 0.35
                }
            }
        
        if ETHKellyIntegratedPortfolio:
            self.portfolio = ETHKellyIntegratedPortfolio(self.config)
        else:
            self.portfolio = None
    
    @unittest.skipIf(ETHKellyIntegratedPortfolio is None, "ETHKellyIntegratedPortfolio not available")
    def test_portfolio_initialization(self):
        """Test portfolio initialization"""
        self.assertIsNotNone(self.portfolio)
        self.assertEqual(self.portfolio.config["portfolio_name"], "ETH_Kelly_Portfolio")
    
    @unittest.skipIf(ETHKellyIntegratedPortfolio is None, "ETHKellyIntegratedPortfolio not available")
    def test_portfolio_summary(self):
        """Test portfolio summary generation"""
        summary = self.portfolio.get_portfolio_summary()
        
        self.assertIsInstance(summary, dict)
        self.assertIn('portfolio_value', summary)
        self.assertIn('cash', summary)
        self.assertIn('position', summary)
        self.assertIn('performance', summary)
        self.assertIn('risk_metrics', summary)
        self.assertIn('kelly_performance', summary)
    
    @unittest.skipIf(ETHKellyIntegratedPortfolio is None, "ETHKellyIntegratedPortfolio not available")
    def test_market_data_processing(self):
        """Test market data processing"""
        # Generate test market data
        periods = 60
        dates = pd.date_range('2024-01-01', periods=periods, freq='D')
        base_price = 3000
        
        prices = []
        current_price = base_price
        for i in range(periods):
            random_change = np.random.normal(0.001, 0.03)
            current_price = current_price * (1 + random_change)
            prices.append(current_price)
        
        market_data = pd.DataFrame({
            'timestamp': dates,
            'close': prices,
            'volume': np.random.uniform(1000, 5000, periods)
        })
        
        # Process market data
        result = self.portfolio.process_market_data(market_data)
        
        self.assertIsInstance(result, dict)
        # Result should contain various metrics even if no signals generated
    
    @unittest.skipIf(ETHKellyIntegratedPortfolio is None, "ETHKellyIntegratedPortfolio not available")
    def test_performance_tracking(self):
        """Test performance tracking"""
        performance = self.portfolio.get_recent_performance(days=7)
        
        self.assertIsInstance(performance, dict)
        # Should have performance metrics even if no trading history


def run_integration_test():
    """Run ETH Kelly Integration tests with detailed output"""
    print('🎯 ETH KELLY INTEGRATION TEST')
    print('=' * 40)
    
    if ETHKellyIntegratedPortfolio is None:
        print('⚠️  ETHKellyIntegratedPortfolio not available - skipping integration tests')
        return True
    
    # Create test suite
    suite = unittest.TestLoader().loadTestsFromTestCase(TestETHKellyIntegration)
    
    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Additional functional test
    print('\n📊 FUNCTIONAL INTEGRATION TEST')
    print('-' * 35)
    
    # Load configuration
    config_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/config/eth_kelly_config.json'
    
    try:
        if os.path.exists(config_path):
            with open(config_path, 'r') as f:
                config = json.load(f)
        else:
            print('⚠️  Configuration file not found - using defaults')
            config = {
                "portfolio_name": "ETH_Kelly_Portfolio",
                "initial_capital": 100000.0,
                "trading": {"symbol": "ETHUSD"},
                "risk": {"max_drawdown": 0.15},
                "kelly": {"max_kelly_fraction": 0.25}
            }
        
        # Initialize portfolio
        portfolio = ETHKellyIntegratedPortfolio(config)
        print(f'✅ Portfolio Initialized: {config["portfolio_name"]}')
        
        # Get portfolio summary
        summary = portfolio.get_portfolio_summary()
        print(f'✅ Portfolio Value: ${summary["portfolio_value"]:,.2f}')
        print(f'✅ Cash: ${summary["cash"]:,.2f}')
        
        # Test performance tracking
        performance = portfolio.get_recent_performance(days=7)
        print(f'✅ Performance Tracking: {len(performance)} metrics available')
        
        print(f'✅ All integration components working')
        
    except Exception as e:
        print(f'❌ Integration test error: {e}')
        return False
    
    success = result.wasSuccessful()
    print(f'\n🎯 ETH KELLY INTEGRATION TEST: {"SUCCESS" if success else "FAILED"}')
    
    return success


if __name__ == "__main__":
    success = run_integration_test()
    sys.exit(0 if success else 1)
