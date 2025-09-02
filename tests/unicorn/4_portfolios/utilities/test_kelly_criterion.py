#!/usr/bin/env python3
"""
Kelly Criterion Algorithm Test
Tests the Kelly Criterion position sizing implementation
"""

import sys
import os
import unittest
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

# Add paths for imports
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/utilities')

from kelly_criterion import KellyCriterionCalculator


class TestKellyCriterion(unittest.TestCase):
    """Test cases for Kelly Criterion implementation"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.kelly_calculator = KellyCriterionCalculator()
        
        # Sample ETH trading data
        self.sample_data = {
            'current_price': 3000.0,
            'win_probability': 0.7,
            'average_win': 0.15,
            'average_loss': 0.08,
            'portfolio_value': 100000.0,
            'confidence': 0.7
        }
    
    def test_kelly_initialization(self):
        """Test Kelly calculator initialization"""
        self.assertIsNotNone(self.kelly_calculator)
        self.assertEqual(self.kelly_calculator.max_kelly_fraction, 0.25)
        self.assertEqual(self.kelly_calculator.min_win_rate, 0.35)
    
    def test_position_calculation(self):
        """Test Kelly position size calculation"""
        # Add some historical data first
        for i in range(10):
            signal_data = {
                'signal': 'BUY',
                'confidence': 0.7,
                'timestamp': datetime.now() - timedelta(days=i)
            }
            outcome_return = 0.15 if i % 3 != 0 else -0.08  # 67% win rate
            self.kelly_calculator.update_signal_history(signal_data, outcome_return)
        
        position_size = self.kelly_calculator.calculate_position_size(
            current_price=self.sample_data['current_price'],
            portfolio_value=self.sample_data['portfolio_value'],
            signal_strength=self.sample_data['confidence']
        )
        
        # Should return a reasonable position size
        self.assertGreaterEqual(position_size, 0)
        self.assertLessEqual(position_size, 0.25)  # Should not exceed max Kelly
        
    def test_kelly_fraction_calculation(self):
        """Test Kelly fraction calculation"""
        # Add some historical data first
        for i in range(10):
            signal_data = {
                'signal': 'BUY',
                'confidence': 0.7,
                'timestamp': datetime.now() - timedelta(days=i)
            }
            outcome_return = 0.15 if i % 3 != 0 else -0.08
            self.kelly_calculator.update_signal_history(signal_data, outcome_return)
        
        kelly_fraction = self.kelly_calculator.calculate_kelly_fraction()
        
        # Should be between 0 and 1
        self.assertGreaterEqual(kelly_fraction, 0)
        self.assertLessEqual(kelly_fraction, 1)
    
    def test_signal_history_update(self):
        """Test signal history functionality"""
        signal_data = {
            'signal': 'BUY',
            'confidence': 0.8,
            'timestamp': datetime.now()
        }
        
        initial_count = len(self.kelly_calculator.signal_history)
        self.kelly_calculator.update_signal_history(signal_data, 0.12)
        
        # Should have one more signal
        self.assertEqual(len(self.kelly_calculator.signal_history), initial_count + 1)
    
    def test_performance_summary(self):
        """Test performance summary generation"""
        # Add some test data
        for i in range(5):
            signal_data = {
                'signal': 'BUY',
                'confidence': 0.6,
                'timestamp': datetime.now() - timedelta(days=i)
            }
            outcome_return = 0.1 if i % 2 == 0 else -0.05
            self.kelly_calculator.update_signal_history(signal_data, outcome_return)
        
        summary = self.kelly_calculator.get_performance_summary()
        
        self.assertIsInstance(summary, dict)
        self.assertIn('total_signals', summary)
        self.assertIn('win_rate', summary)


def run_kelly_test():
    """Run Kelly Criterion tests with detailed output"""
    print('🎯 KELLY CRITERION ALGORITHM TEST')
    print('=' * 40)
    
    # Create test suite
    suite = unittest.TestLoader().loadTestsFromTestCase(TestKellyCriterion)
    
    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Additional functional test
    print('\n📊 FUNCTIONAL TEST')
    print('-' * 20)
    
    kelly_calculator = KellyCriterionCalculator()
    
    # Add historical data to enable calculations
    for i in range(15):
        signal_data = {
            'signal': 'BUY',
            'confidence': 0.7,
            'timestamp': datetime.now() - timedelta(days=i)
        }
        outcome_return = 0.15 if i % 3 != 0 else -0.08  # 67% win rate
        kelly_calculator.update_signal_history(signal_data, outcome_return)
    
    # Test position calculation
    position_size = kelly_calculator.calculate_position_size(
        current_price=3000.0,
        portfolio_value=100000.0,
        signal_strength=0.7
    )
    
    # Calculate position details
    position_value = position_size * 100000.0
    eth_quantity = position_value / 3000.0 if position_value > 0 else 0
    
    print(f'✅ Position Size: {position_size:.4f} ({position_size*100:.2f}%)')
    print(f'✅ Position Value: ${position_value:,.2f}')
    print(f'✅ ETH Quantity: {eth_quantity:.4f} ETH')
    
    # Get performance summary
    summary = kelly_calculator.get_performance_summary()
    print(f'✅ Total Signals: {summary.get("total_signals", 0)}')
    print(f'✅ Win Rate: {summary.get("win_rate", 0):.1%}')
    
    success = result.wasSuccessful()
    print(f'\n🎯 KELLY CRITERION TEST: {"SUCCESS" if success else "FAILED"}')
    
    return success


if __name__ == "__main__":
    success = run_kelly_test()
    sys.exit(0 if success else 1)
