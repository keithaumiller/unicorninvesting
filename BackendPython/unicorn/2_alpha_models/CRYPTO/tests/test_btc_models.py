"""
Comprehensive Tests for BTC Models
"""

import pytest
import pandas as pd
import numpy as np
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.testing_framework import run_model_test_suite
from models.btc_alpha import BTCAlphaModel

class TestBTCModels:
    """Comprehensive test suite for BTC models."""
    
    def setup_method(self):
        """Setup test fixtures."""
        self.alpha_model = BTCAlphaModel()
        
        # Create sample data
        np.random.seed(42)
        dates = pd.date_range('2023-01-01', periods=200, freq='D')
        
        base_price = 50000 if 'BTC' == 'BTC' else 3000 if 'BTC' == 'ETH' else 100
        
        self.sample_data = pd.DataFrame({
            'Open': base_price + np.cumsum(np.random.randn(200) * 0.02),
            'High': np.nan,
            'Low': np.nan, 
            'Close': np.nan,
            'Volume': np.random.randint(1000000, 10000000, 200)
        }, index=dates)
        
        # Generate OHLC
        self.sample_data['Close'] = self.sample_data['Open'] + np.random.randn(200) * 0.01
        self.sample_data['High'] = np.maximum(
            self.sample_data['Open'], 
            self.sample_data['Close']
        ) + np.abs(np.random.randn(200) * 0.005)
        self.sample_data['Low'] = np.minimum(
            self.sample_data['Open'],
            self.sample_data['Close'] 
        ) - np.abs(np.random.randn(200) * 0.005)
        
    def test_alpha_model_comprehensive(self):
        """Run comprehensive test suite for alpha model."""
        results = run_model_test_suite(
            model_class=BTCAlphaModel,
            asset_name='BTC',
            model_type="technical"
        )
        
        assert results['overall_passed'], f"Alpha model tests failed: {results}"
        
    def test_signal_generation(self):
        """Test signal generation functionality."""
        signal_result = self.alpha_model.generate_signal(self.sample_data)
        
        assert isinstance(signal_result, dict)
        assert 'signal' in signal_result
        assert 'confidence' in signal_result
        assert signal_result['signal'] in [-1, 0, 1]
        assert 0.0 <= signal_result['confidence'] <= 1.0

if __name__ == "__main__":
    # Run basic tests
    test_suite = TestBTCModels()
    test_suite.setup_method()
    test_suite.test_signal_generation()
    print("✅ BTC Models basic tests passed")
