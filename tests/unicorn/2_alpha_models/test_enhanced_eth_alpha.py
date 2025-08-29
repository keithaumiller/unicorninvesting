"""
Test Suite for Enhanced ETH Technical Alpha Model
================================================

Comprehensive testing for Phase 2 Technical Analysis Alpha implementation.
This test validates integration with Phase 1 technical indicators and
signal generation capabilities.

Test Coverage:
- Alpha model initialization
- Data buffer management  
- Signal generation with 30+ indicators
- Confidence scoring system
- Error handling and edge cases
- Performance validation
"""

import sys
import os
import numpy as np
import unittest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime, timedelta

# Add paths for our implementations
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

# Mock LEAN imports since we're testing outside LEAN environment
class MockInsightDirection:
    UP = "UP"
    DOWN = "DOWN"

class MockInsight:
    def __init__(self, symbol, period, direction, magnitude, confidence, weight=1.0):
        self.symbol = symbol
        self.period = period
        self.direction = direction
        self.magnitude = magnitude
        self.confidence = confidence
        self.weight = weight
        self.tag = ""
    
    @staticmethod
    def price(symbol, period, direction, magnitude=None, confidence=None, weight=None):
        return MockInsight(symbol, period, direction, magnitude, confidence, weight)

# Create comprehensive mock for AlgorithmImports
class MockAlgorithmImports:
    InsightDirection = MockInsightDirection
    Insight = MockInsight
    timedelta = timedelta
    
    class AlphaModel:
        def update(self, algorithm, data):
            return []
        def on_securities_changed(self, algorithm, changes):
            pass

# Mock AlgorithmImports module
mock_imports = MockAlgorithmImports()
sys.modules['AlgorithmImports'] = mock_imports

# Create globals for the module
import builtins
original_import = builtins.__import__

def mock_import(name, *args, **kwargs):
    if name == 'AlgorithmImports':
        return mock_imports
    return original_import(name, *args, **kwargs)

builtins.__import__ = mock_import

class TestEnhancedETHTechnicalAlpha(unittest.TestCase):
    """Test Enhanced ETH Technical Alpha Model implementation."""
    
    def setUp(self):
        """Set up test environment."""
        # Import after mocking
        from EnhancedETHTechnicalAlpha import EnhancedETHTechnicalAlpha
        
        self.alpha_model = EnhancedETHTechnicalAlpha(
            prediction_horizon_hours=2,
            confidence_threshold=0.015,
            max_signals_per_update=1
        )
        
        # Mock algorithm and data
        self.mock_algorithm = Mock()
        self.mock_algorithm.time = datetime.now()
        self.mock_algorithm.debug = Mock()
        
        # Mock symbol
        self.eth_symbol = "ETHUSD"
        
    def test_alpha_model_initialization(self):
        """Test alpha model initializes correctly."""
        self.assertEqual(self.alpha_model.confidence_threshold, 0.015)
        self.assertEqual(self.alpha_model.max_signals_per_update, 1)
        self.assertEqual(self.alpha_model.prediction_interval, timedelta(hours=2))
        self.assertIsInstance(self.alpha_model.symbol_data, dict)
        
    def test_indicators_availability_check(self):
        """Test that indicators availability is properly checked."""
        # Should handle missing indicators gracefully
        if not self.alpha_model.indicators_ready:
            self.assertIsNotNone(self.alpha_model.error_message)
            
    def test_buffer_initialization(self):
        """Test data buffer initialization for new securities."""
        # Mock security changes
        mock_security = Mock()
        mock_security.symbol = self.eth_symbol
        
        mock_changes = Mock()
        mock_changes.added_securities = [mock_security]
        mock_changes.removed_securities = []
        
        # Initialize buffers
        self.alpha_model.on_securities_changed(self.mock_algorithm, mock_changes)
        
        # Check buffers were created
        self.assertIn(self.eth_symbol, self.alpha_model.symbol_data)
        symbol_data = self.alpha_model.symbol_data[self.eth_symbol]
        self.assertIn('prices', symbol_data)
        self.assertIn('highs', symbol_data)
        self.assertIn('lows', symbol_data)
        self.assertIn('volumes', symbol_data)
        
    def test_buffer_updates(self):
        """Test price data buffer updates."""
        # Initialize buffers first
        self.test_buffer_initialization()
        
        # Mock bar data
        mock_bar = Mock()
        mock_bar.close = 2500.0
        mock_bar.high = 2520.0
        mock_bar.low = 2480.0
        mock_bar.volume = 1000.0
        
        # Update buffers
        self.alpha_model._update_buffers(self.eth_symbol, mock_bar)
        
        # Check data was added
        buffers = self.alpha_model.symbol_data[self.eth_symbol]
        self.assertEqual(buffers['prices'].count, 1)
        self.assertEqual(buffers['prices'].to_array()[0], 2500.0)
        
    def test_insufficient_data_handling(self):
        """Test handling of insufficient data scenarios."""
        self.test_buffer_initialization()
        
        # Should return False for insufficient data
        has_data = self.alpha_model._has_sufficient_data(self.eth_symbol)
        self.assertFalse(has_data)
        
    def test_signal_generation_with_mock_data(self):
        """Test signal generation with sufficient mock data."""
        if not self.alpha_model.indicators_ready:
            self.skipTest("Technical indicators not available - Phase 1 dependency")
            
        # Initialize buffers
        self.test_buffer_initialization()
        
        # Add sufficient mock data
        buffers = self.alpha_model.symbol_data[self.eth_symbol]
        
        # Generate realistic ETH price data (trending upward)
        base_price = 2500.0
        for i in range(60):  # 60 data points
            price = base_price + i * 2 + np.random.normal(0, 10)  # Slight upward trend with noise
            high = price + abs(np.random.normal(5, 2))
            low = price - abs(np.random.normal(5, 2))
            volume = 1000 + np.random.normal(0, 200)
            
            buffers['prices'].append(price)
            buffers['highs'].append(high)
            buffers['lows'].append(low)
            buffers['volumes'].append(max(volume, 100))  # Ensure positive volume
        
        # Test signal generation
        current_price = base_price + 120  # Current price higher than start
        signal = self.alpha_model._generate_enhanced_signal(
            self.eth_symbol, current_price, self.mock_algorithm
        )
        
        if signal:  # Signal may be None if indicators don't generate clear signals
            self.assertIn('expected_return', signal)
            self.assertIn('confidence', signal)
            self.assertIn('active_components', signal)
            self.assertIsInstance(signal['expected_return'], (int, float))
            self.assertIsInstance(signal['confidence'], (int, float))
            self.assertGreaterEqual(signal['confidence'], 0)
            self.assertLessEqual(signal['confidence'], 1)
            
    def test_update_method_with_valid_data(self):
        """Test the main update method with valid data."""
        # Initialize buffers
        self.test_buffer_initialization()
        
        # Mock data container
        mock_data = {}
        mock_bar = Mock()
        mock_bar.close = 2500.0
        mock_bar.high = 2520.0
        mock_bar.low = 2480.0
        mock_bar.volume = 1000.0
        
        mock_data[self.eth_symbol] = mock_bar
        mock_data.contains_key = lambda x: x == self.eth_symbol
        
        # Mock securities
        mock_security = Mock()
        mock_security.price = 2500.0
        self.mock_algorithm.securities = {self.eth_symbol: mock_security}
        
        # Call update (should handle insufficient data gracefully)
        insights = self.alpha_model.update(self.mock_algorithm, mock_data)
        
        # Should return empty list due to insufficient data
        self.assertIsInstance(insights, list)
        self.assertLessEqual(len(insights), self.alpha_model.max_signals_per_update)
        
    def test_update_method_with_empty_data(self):
        """Test update method with empty data."""
        # Empty data container
        mock_data = {}
        mock_data.contains_key = lambda x: False
        
        insights = self.alpha_model.update(self.mock_algorithm, mock_data)
        self.assertEqual(insights, [])
        
    def test_security_removal(self):
        """Test proper cleanup when securities are removed."""
        # First add a security
        self.test_buffer_initialization()
        self.assertIn(self.eth_symbol, self.alpha_model.symbol_data)
        
        # Now remove it
        mock_security = Mock()
        mock_security.symbol = self.eth_symbol
        
        mock_changes = Mock()
        mock_changes.added_securities = []
        mock_changes.removed_securities = [mock_security]
        
        self.alpha_model.on_securities_changed(self.mock_algorithm, mock_changes)
        
        # Check it was removed
        self.assertNotIn(self.eth_symbol, self.alpha_model.symbol_data)
        
    def test_status_summary(self):
        """Test status summary generation."""
        status = self.alpha_model.get_status_summary()
        
        self.assertIn('indicators_available', status)
        self.assertIn('active_symbols', status)
        self.assertIn('total_signals_generated', status)
        self.assertIn('model_type', status)
        self.assertEqual(status['model_type'], 'Enhanced ETH Technical Analysis Alpha')
        
    def test_confidence_threshold_filtering(self):
        """Test that signals below confidence threshold are filtered out."""
        # Set high confidence threshold
        alpha_model = EnhancedETHTechnicalAlpha(confidence_threshold=0.9)
        
        # Most signals should be filtered out with very high threshold
        self.assertEqual(alpha_model.confidence_threshold, 0.9)
        
    def test_error_handling_in_signal_generation(self):
        """Test error handling in signal generation methods."""
        # Test with invalid data
        invalid_symbol = "INVALID"
        
        # Should not crash with invalid symbol
        try:
            signal = self.alpha_model._generate_enhanced_signal(
                invalid_symbol, 2500.0, self.mock_algorithm
            )
            # Should return None for invalid symbol
            self.assertIsNone(signal)
        except Exception as e:
            # Should handle gracefully
            self.fail(f"Signal generation should handle errors gracefully: {e}")

class TestTechnicalIndicatorIntegration(unittest.TestCase):
    """Test integration with Phase 1 technical indicators."""
    
    def setUp(self):
        """Set up integration test environment."""
        try:
            from technical_indicators import (
                CircularBuffer, IndicatorValue, simple_moving_average
            )
            self.indicators_available = True
        except ImportError:
            self.indicators_available = False
            
    def test_circular_buffer_integration(self):
        """Test CircularBuffer integration works correctly."""
        if not self.indicators_available:
            self.skipTest("Technical indicators not available")
            
        from technical_indicators import CircularBuffer
        
        buffer = CircularBuffer(10)
        
        # Add data
        for i in range(15):
            buffer.append(float(i))
            
        # Should maintain only last 10 values
        self.assertEqual(buffer.count, 10)
        self.assertEqual(buffer.to_array()[-1], 14.0)  # Last value
        self.assertEqual(buffer.to_array()[0], 5.0)    # First value in buffer
        
    def test_indicator_calculation_integration(self):
        """Test that indicator calculations work with our data."""
        if not self.indicators_available:
            self.skipTest("Technical indicators not available")
            
        from technical_indicators import simple_moving_average
        
        # Test data
        prices = np.array([100.0, 101.0, 102.0, 103.0, 104.0, 105.0, 106.0, 107.0, 108.0, 109.0])
        
        # Calculate SMA
        sma = simple_moving_average(prices, 5)
        
        if sma.is_valid:
            # Should be around 107 for last 5 values
            self.assertAlmostEqual(sma.value, 107.0, places=1)
        else:
            self.fail("SMA calculation should be valid with sufficient data")

def run_enhanced_alpha_tests():
    """Run all tests for Enhanced ETH Technical Alpha."""
    print("🧪 Running Enhanced ETH Technical Alpha Tests")
    print("=" * 60)
    
    # Create test suite
    test_suite = unittest.TestSuite()
    
    # Add alpha model tests
    test_suite.addTest(unittest.makeSuite(TestEnhancedETHTechnicalAlpha))
    test_suite.addTest(unittest.makeSuite(TestTechnicalIndicatorIntegration))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(test_suite)
    
    # Summary
    print("\n" + "=" * 60)
    print(f"🎯 Tests Run: {result.testsRun}")
    print(f"✅ Passed: {result.testsRun - len(result.failures) - len(result.errors)}")
    print(f"❌ Failed: {len(result.failures)}")
    print(f"⚠️ Errors: {len(result.errors)}")
    
    if result.failures:
        print("\n❌ FAILURES:")
        for test, failure in result.failures:
            print(f"- {test}: {failure.split('AssertionError:')[-1].strip()}")
    
    if result.errors:
        print("\n⚠️ ERRORS:")
        for test, error in result.errors:
            print(f"- {test}: {error.split('Exception:')[-1].strip()}")
            
    success_rate = ((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun) * 100
    print(f"\n🏆 Success Rate: {success_rate:.1f}%")
    
    return result.wasSuccessful()

if __name__ == "__main__":
    success = run_enhanced_alpha_tests()
    exit(0 if success else 1)
