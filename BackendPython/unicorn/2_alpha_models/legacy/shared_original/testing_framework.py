"""
Comprehensive Testing Framework for Alpha Models

Provides standardized testing utilities for all alpha models across asset classes.
"""

import pytest
import pandas as pd
import numpy as np
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta
import logging
from abc import ABC, abstractmethod
import time
import warnings

# Suppress common warnings for cleaner test output
warnings.filterwarnings('ignore', category=UserWarning)
warnings.filterwarnings('ignore', category=FutureWarning)

logger = logging.getLogger(__name__)

class BaseModelTester(ABC):
    """
    Abstract base class for model testing.
    
    Provides standardized testing patterns for all alpha models.
    """
    
    def __init__(self, model_class, asset_name: str, test_data_periods: int = 252):
        """
        Initialize model tester.
        
        Args:
            model_class: The alpha model class to test
            asset_name: Name of the asset being tested
            test_data_periods: Number of periods of test data to generate
        """
        self.model_class = model_class
        self.asset_name = asset_name
        self.test_data_periods = test_data_periods
        self.test_results = {}
        
    def generate_test_data(self, periods: int = None, volatility: float = 0.02) -> pd.DataFrame:
        """
        Generate realistic test data for the asset.
        
        Args:
            periods: Number of periods to generate
            volatility: Daily volatility for price simulation
            
        Returns:
            DataFrame with OHLCV data
        """
        if periods is None:
            periods = self.test_data_periods
            
        # Set seed for reproducible tests
        np.random.seed(42)
        
        # Generate dates
        dates = pd.date_range(
            start=datetime.now() - timedelta(days=periods),
            periods=periods,
            freq='D'
        )
        
        # Generate realistic price data using geometric Brownian motion
        price_base = 100.0 if self.asset_name == "ETH" else 50000.0 if self.asset_name == "BTC" else 100.0
        
        # Generate returns
        returns = np.random.normal(0, volatility, periods)
        returns[0] = 0  # First return is 0
        
        # Add some autocorrelation for realism
        for i in range(1, len(returns)):
            returns[i] += 0.1 * returns[i-1]
            
        # Convert to prices
        log_prices = np.log(price_base) + np.cumsum(returns)
        prices = np.exp(log_prices)
        
        # Generate OHLC from close prices
        close_prices = prices
        open_prices = np.roll(close_prices, 1)
        open_prices[0] = close_prices[0]
        
        # Add intraday noise
        intraday_noise = np.random.normal(0, volatility * 0.3, periods)
        high_prices = np.maximum(open_prices, close_prices) + np.abs(intraday_noise)
        low_prices = np.minimum(open_prices, close_prices) - np.abs(intraday_noise)
        
        # Generate volume with some correlation to price moves
        price_changes = np.abs(np.diff(np.append(close_prices[0], close_prices)))
        base_volume = 1000000 if self.asset_name == "ETH" else 100000 if self.asset_name == "BTC" else 1000000
        volume = base_volume * (1 + price_changes * 10) * np.random.lognormal(0, 0.5, periods)
        
        return pd.DataFrame({
            'Open': open_prices,
            'High': high_prices,
            'Low': low_prices,
            'Close': close_prices,
            'Volume': volume
        }, index=dates)
    
    def test_model_initialization(self) -> Dict[str, Any]:
        """Test that model initializes correctly."""
        try:
            model = self.model_class()
            
            results = {
                'passed': True,
                'model_name': getattr(model, 'name', 'Unknown'),
                'asset_class': getattr(model, 'asset_class', 'Unknown'),
                'required_columns': getattr(model, 'get_required_columns', lambda: [])(),
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'model_name': None,
                'asset_class': None,
                'required_columns': None
            }
            
        self.test_results['initialization'] = results
        return results
    
    def test_data_validation(self) -> Dict[str, Any]:
        """Test data validation functionality."""
        try:
            model = self.model_class()
            test_data = self.generate_test_data(100)
            
            # Test valid data
            valid_data_test = model.validate_data(test_data)
            
            # Test invalid data (missing column)
            invalid_data = test_data.drop('Close', axis=1)
            invalid_data_test = False
            try:
                model.validate_data(invalid_data)
            except ValueError:
                invalid_data_test = True  # Should raise ValueError
                
            results = {
                'passed': valid_data_test and invalid_data_test,
                'valid_data_validation': valid_data_test,
                'invalid_data_rejection': invalid_data_test,
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'valid_data_validation': False,
                'invalid_data_rejection': False
            }
            
        self.test_results['data_validation'] = results
        return results
    
    def test_signal_generation(self, num_tests: int = 10) -> Dict[str, Any]:
        """Test signal generation functionality."""
        try:
            model = self.model_class()
            test_data = self.generate_test_data(200)
            
            signals = []
            confidences = []
            generation_times = []
            
            # Test multiple signal generations
            for i in range(num_tests):
                # Use rolling windows for realistic testing
                start_idx = i * 10
                end_idx = start_idx + 150
                data_window = test_data.iloc[start_idx:end_idx]
                
                if len(data_window) >= 100:  # Minimum data requirement
                    start_time = time.time()
                    signal_result = model.generate_signal(data_window)
                    end_time = time.time()
                    
                    generation_times.append(end_time - start_time)
                    signals.append(signal_result['signal'])
                    confidences.append(signal_result['confidence'])
                    
            # Analyze results
            signal_array = np.array(signals)
            confidence_array = np.array(confidences)
            
            results = {
                'passed': len(signals) > 0,
                'total_signals': len(signals),
                'signal_distribution': {
                    'buy': int(np.sum(signal_array == 1)),
                    'sell': int(np.sum(signal_array == -1)),
                    'hold': int(np.sum(signal_array == 0))
                },
                'confidence_stats': {
                    'mean': float(np.mean(confidence_array)) if len(confidence_array) > 0 else 0,
                    'std': float(np.std(confidence_array)) if len(confidence_array) > 0 else 0,
                    'min': float(np.min(confidence_array)) if len(confidence_array) > 0 else 0,
                    'max': float(np.max(confidence_array)) if len(confidence_array) > 0 else 0
                },
                'performance_stats': {
                    'mean_generation_time': float(np.mean(generation_times)) if generation_times else 0,
                    'max_generation_time': float(np.max(generation_times)) if generation_times else 0
                },
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'total_signals': 0,
                'signal_distribution': {'buy': 0, 'sell': 0, 'hold': 0},
                'confidence_stats': {'mean': 0, 'std': 0, 'min': 0, 'max': 0},
                'performance_stats': {'mean_generation_time': 0, 'max_generation_time': 0}
            }
            
        self.test_results['signal_generation'] = results
        return results
    
    def test_insufficient_data_handling(self) -> Dict[str, Any]:
        """Test behavior with insufficient data."""
        try:
            model = self.model_class()
            small_data = self.generate_test_data(10)  # Very small dataset
            
            signal_result = model.generate_signal(small_data)
            
            # Should handle gracefully
            results = {
                'passed': True,
                'signal': signal_result['signal'],
                'confidence': signal_result['confidence'],
                'handles_gracefully': signal_result['signal'] == 0 and signal_result['confidence'] == 0,
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'signal': None,
                'confidence': None,
                'handles_gracefully': False
            }
            
        self.test_results['insufficient_data'] = results
        return results
    
    def test_performance_tracking(self) -> Dict[str, Any]:
        """Test performance tracking functionality."""
        try:
            model = self.model_class()
            test_data = self.generate_test_data(200)
            
            # Generate a signal
            signal_result = model.generate_signal(test_data)
            
            # Update performance with mock return
            mock_return = 0.02
            model.update_performance(signal_result, mock_return)
            
            # Get performance summary
            performance = model.get_performance_summary()
            
            results = {
                'passed': True,
                'signals_generated': performance.get('signals_generated', 0),
                'performance_tracking_works': 'total_signals' in performance,
                'performance_summary': performance,
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'signals_generated': 0,
                'performance_tracking_works': False,
                'performance_summary': {}
            }
            
        self.test_results['performance_tracking'] = results
        return results
    
    def run_comprehensive_tests(self) -> Dict[str, Any]:
        """Run all tests and return comprehensive results."""
        print(f"🧪 Running comprehensive tests for {self.asset_name} model...")
        
        # Run all test components
        init_results = self.test_model_initialization()
        validation_results = self.test_data_validation()
        signal_results = self.test_signal_generation()
        insufficient_data_results = self.test_insufficient_data_handling()
        performance_results = self.test_performance_tracking()
        
        # Calculate overall success
        all_tests_passed = all([
            init_results['passed'],
            validation_results['passed'],
            signal_results['passed'],
            insufficient_data_results['passed'],
            performance_results['passed']
        ])
        
        comprehensive_results = {
            'asset_name': self.asset_name,
            'model_class': self.model_class.__name__,
            'test_timestamp': datetime.now().isoformat(),
            'overall_passed': all_tests_passed,
            'test_results': {
                'initialization': init_results,
                'data_validation': validation_results,
                'signal_generation': signal_results,
                'insufficient_data_handling': insufficient_data_results,
                'performance_tracking': performance_results
            },
            'summary': {
                'total_tests': 5,
                'passed_tests': sum(1 for r in [init_results, validation_results, signal_results, 
                                                insufficient_data_results, performance_results] if r['passed']),
                'failed_tests': sum(1 for r in [init_results, validation_results, signal_results,
                                                insufficient_data_results, performance_results] if not r['passed'])
            }
        }
        
        return comprehensive_results

class TechnicalAlphaModelTester(BaseModelTester):
    """Specialized tester for technical alpha models."""
    
    def test_technical_indicators(self) -> Dict[str, Any]:
        """Test technical indicator calculations."""
        try:
            model = self.model_class()
            test_data = self.generate_test_data(200)
            
            # Test if model has technical indicator methods
            technical_methods = []
            for method_name in ['calculate_sma', 'calculate_ema', 'calculate_rsi', 'calculate_bollinger_bands']:
                if hasattr(model, method_name):
                    technical_methods.append(method_name)
                    
            results = {
                'passed': len(technical_methods) > 0,
                'available_indicators': technical_methods,
                'total_indicators': len(technical_methods),
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'available_indicators': [],
                'total_indicators': 0
            }
            
        return results

class MLAlphaModelTester(BaseModelTester):
    """Specialized tester for machine learning alpha models."""
    
    def test_model_training(self) -> Dict[str, Any]:
        """Test model training functionality."""
        try:
            model = self.model_class()
            test_data = self.generate_test_data(200)
            
            # Check if model has training capability
            has_training = hasattr(model, 'train_model') and hasattr(model, 'prepare_features')
            
            results = {
                'passed': has_training,
                'has_training_method': hasattr(model, 'train_model'),
                'has_feature_preparation': hasattr(model, 'prepare_features'),
                'error': None
            }
            
        except Exception as e:
            results = {
                'passed': False,
                'error': str(e),
                'has_training_method': False,
                'has_feature_preparation': False
            }
            
        return results

def run_model_test_suite(model_class, asset_name: str, model_type: str = "technical") -> Dict[str, Any]:
    """
    Run complete test suite for a model.
    
    Args:
        model_class: The model class to test
        asset_name: Name of the asset
        model_type: Type of model ("technical" or "ml")
        
    Returns:
        Comprehensive test results
    """
    if model_type.lower() == "technical":
        tester = TechnicalAlphaModelTester(model_class, asset_name)
    elif model_type.lower() == "ml":
        tester = MLAlphaModelTester(model_class, asset_name)
    else:
        tester = BaseModelTester(model_class, asset_name)
    
    return tester.run_comprehensive_tests()

def generate_test_report(test_results: Dict[str, Any], output_file: str = None) -> str:
    """
    Generate a formatted test report.
    
    Args:
        test_results: Test results from run_model_test_suite
        output_file: Optional file to save report
        
    Returns:
        Formatted test report string
    """
    report_lines = []
    
    # Header
    report_lines.append("=" * 80)
    report_lines.append(f"ALPHA MODEL TEST REPORT")
    report_lines.append("=" * 80)
    report_lines.append(f"Asset: {test_results['asset_name']}")
    report_lines.append(f"Model: {test_results['model_class']}")
    report_lines.append(f"Test Time: {test_results['test_timestamp']}")
    report_lines.append(f"Overall Status: {'PASSED' if test_results['overall_passed'] else 'FAILED'}")
    report_lines.append("")
    
    # Summary
    summary = test_results['summary']
    report_lines.append("TEST SUMMARY")
    report_lines.append("-" * 40)
    report_lines.append(f"Total Tests: {summary['total_tests']}")
    report_lines.append(f"Passed: {summary['passed_tests']}")
    report_lines.append(f"Failed: {summary['failed_tests']}")
    report_lines.append(f"Success Rate: {summary['passed_tests']/summary['total_tests']*100:.1f}%")
    report_lines.append("")
    
    # Detailed Results
    report_lines.append("DETAILED TEST RESULTS")
    report_lines.append("-" * 40)
    
    for test_name, test_result in test_results['test_results'].items():
        status = "PASS" if test_result['passed'] else "FAIL"
        report_lines.append(f"{test_name.replace('_', ' ').title()}: {status}")
        
        if not test_result['passed'] and test_result.get('error'):
            report_lines.append(f"  Error: {test_result['error']}")
        
        # Add specific metrics for signal generation
        if test_name == 'signal_generation' and test_result['passed']:
            dist = test_result['signal_distribution']
            conf = test_result['confidence_stats']
            perf = test_result['performance_stats']
            
            report_lines.append(f"  Signals: {test_result['total_signals']}")
            report_lines.append(f"  Distribution: Buy={dist['buy']}, Sell={dist['sell']}, Hold={dist['hold']}")
            report_lines.append(f"  Confidence: Mean={conf['mean']:.3f}, Std={conf['std']:.3f}")
            report_lines.append(f"  Performance: Avg Time={perf['mean_generation_time']:.4f}s")
        
        report_lines.append("")
    
    report_text = "\n".join(report_lines)
    
    if output_file:
        with open(output_file, 'w') as f:
            f.write(report_text)
    
    return report_text

# Example usage and validation function
if __name__ == "__main__":
    print("✅ Testing Framework Module Loaded Successfully")
    print("Available Classes:")
    print("- BaseModelTester")
    print("- TechnicalAlphaModelTester") 
    print("- MLAlphaModelTester")
    print("Available Functions:")
    print("- run_model_test_suite()")
    print("- generate_test_report()")
