#!/usr/bin/env python3
"""
ETH Basic Risk Management Algorithm Test
Tests the risk management implementation for ETH trading
"""

import sys
import os
import unittest
import pandas as pd
import numpy as np

# Add paths for imports
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/risk_algorithms')

from eth_basic_risk import ETHBasicRisk


class TestETHBasicRisk(unittest.TestCase):
    """Test cases for ETH Basic Risk Management"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.risk_manager = ETHBasicRisk(
            max_drawdown=0.15,
            max_daily_var=0.06
        )
        
        # Sample portfolio data
        self.sample_portfolio = {
            'current_value': 100000.0,
            'peak_value': 110000.0,
            'daily_returns': np.random.normal(0.001, 0.04, 30),
            'current_position': 0.2
        }
    
    def test_risk_initialization(self):
        """Test risk manager initialization"""
        self.assertIsNotNone(self.risk_manager)
        self.assertEqual(self.risk_manager.max_drawdown, 0.15)
        self.assertEqual(self.risk_manager.max_daily_var, 0.06)
    
    def test_position_validation(self):
        """Test position size validation"""
        # Test valid position
        valid_result = self.risk_manager.validate_position_size(0.15)
        self.assertTrue(valid_result['approved'])
        
        # Test oversized position
        invalid_result = self.risk_manager.validate_position_size(0.9)
        self.assertFalse(invalid_result['approved'])
        
    def test_drawdown_validation(self):
        """Test drawdown validation"""
        # Test acceptable drawdown
        low_drawdown = self.risk_manager.validate_drawdown(
            current_value=95000.0,
            peak_value=100000.0
        )
        self.assertTrue(low_drawdown['approved'])
        
        # Test excessive drawdown
        high_drawdown = self.risk_manager.validate_drawdown(
            current_value=80000.0,  # 20% drawdown
            peak_value=100000.0
        )
        self.assertFalse(high_drawdown['approved'])
    
    def test_var_calculation(self):
        """Test Value at Risk calculation"""
        returns = np.random.normal(0.001, 0.04, 100)
        var_result = self.risk_manager.calculate_var(returns, confidence=0.05)
        
        self.assertIsInstance(var_result, dict)
        self.assertIn('var_5pct', var_result)
        self.assertIn('var_1pct', var_result)
        
        # VaR should be negative (loss)
        self.assertLess(var_result['var_5pct'], 0)
    
    def test_risk_limits(self):
        """Test overall risk limit validation"""
        portfolio_data = {
            'current_value': 100000.0,
            'peak_value': 105000.0,
            'position_size': 0.25,
            'daily_returns': np.random.normal(0.001, 0.03, 30)
        }
        
        risk_assessment = self.risk_manager.assess_portfolio_risk(portfolio_data)
        
        self.assertIsInstance(risk_assessment, dict)
        self.assertIn('overall_risk_status', risk_assessment)
        self.assertIn('risk_factors', risk_assessment)


def run_risk_test():
    """Run ETH Basic Risk tests with detailed output"""
    print('⚠️  ETH BASIC RISK MANAGEMENT TEST')
    print('=' * 40)
    
    # Create test suite
    suite = unittest.TestLoader().loadTestsFromTestCase(TestETHBasicRisk)
    
    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Additional functional test
    print('\n📊 FUNCTIONAL TEST')
    print('-' * 20)
    
    risk_manager = ETHBasicRisk(max_drawdown=0.15, max_daily_var=0.06)
    
    # Test position validation
    position_test = risk_manager.validate_position_size(0.25)
    print(f'✅ Position Validation (25%): {"APPROVED" if position_test["approved"] else "REJECTED"}')
    print(f'   Risk Score: {position_test["risk_score"]:.2f}')
    
    # Test drawdown validation
    drawdown_test = risk_manager.validate_drawdown(
        current_value=85000.0,
        peak_value=100000.0
    )
    print(f'✅ Drawdown Validation (15%): {"APPROVED" if drawdown_test["approved"] else "REJECTED"}')
    print(f'   Current Drawdown: {drawdown_test["current_drawdown"]:.2%}')
    
    # Test VaR calculation
    sample_returns = np.random.normal(0.001, 0.04, 60)
    var_result = risk_manager.calculate_var(sample_returns)
    print(f'✅ VaR Calculation:')
    print(f'   VaR (5%): {var_result["var_5pct"]:.2%}')
    print(f'   VaR (1%): {var_result["var_1pct"]:.2%}')
    
    success = result.wasSuccessful()
    print(f'\n⚠️  ETH BASIC RISK TEST: {"SUCCESS" if success else "FAILED"}')
    
    return success


if __name__ == "__main__":
    success = run_risk_test()
    sys.exit(0 if success else 1)
