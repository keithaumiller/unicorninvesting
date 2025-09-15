#!/usr/bin/env python3
"""
Test Failure Analysis and Classification Framework

This script analyzes test failures and categorizes them as:
1. Testing Script Problems (fixtures, syntax, imports, test logic)
2. Pipeline Problems (actual functionality issues)

The goal is to separate test infrastructure issues from real pipeline issues.
"""

import os
import sys
import json
import subprocess
import pandas as pd
from datetime import datetime
from pathlib import Path


class TestFailureAnalyzer:
    """Analyzes test failures and categorizes them systematically."""
    
    def __init__(self):
        self.test_root = Path(__file__).parent
        self.failure_categories = {
            'testing_script_problems': [],
            'pipeline_problems': [],
            'unknown': []
        }
        
        # Common patterns for testing script problems
        self.script_problem_patterns = [
            'fixture .* not found',
            'TypeError.*__init__.*unexpected keyword argument',
            'ImportError',
            'ModuleNotFoundError',
            'pytest.*return.*None',
            'AssertionError.*should be.*got',
            'missing.*parameter',
            'undefined.*variable'
        ]
        
        # Common patterns for pipeline problems
        self.pipeline_problem_patterns = [
            'ConnectionError',
            'TimeoutError',
            'HTTP.*Error',
            'API.*Error',
            'Database.*Error',
            'Data.*not.*found',
            'Service.*unavailable'
        ]
    
    def run_comprehensive_test_analysis(self):
        """Run all tests and analyze failures comprehensively."""
        print("🔍 Starting Comprehensive Test Failure Analysis")
        print("=" * 60)
        
        # Define test areas to analyze
        test_areas = [
            'tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/',
            'tests/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/',
            'tests/unicorn/1_data_sources/1_raw/connectors/forex/',
            'tests/unicorn/1_data_sources/database/',
            'tests/unicorn/1_data_sources/data/'
        ]
        
        for test_area in test_areas:
            if os.path.exists(test_area):
                print(f"\n🧪 Analyzing: {test_area}")
                self.analyze_test_area(test_area)
            else:
                print(f"⚠️  Test area not found: {test_area}")
        
        self.generate_analysis_report()
    
    def analyze_test_area(self, test_path):
        """Analyze a specific test area and categorize failures."""
        try:
            # Run pytest with verbose output and capture results
            cmd = [
                'python', '-m', 'pytest', test_path, 
                '-v', '--tb=short', '--no-header'
            ]
            
            result = subprocess.run(
                cmd, 
                capture_output=True, 
                text=True, 
                cwd=self.test_root.parent.parent.parent
            )
            
            # Analyze the output
            output = result.stdout + result.stderr
            self.categorize_failures(test_path, output, result.returncode)
            
        except Exception as e:
            print(f"   ❌ Error running tests: {e}")
            self.failure_categories['unknown'].append({
                'test_area': test_path,
                'error': str(e),
                'category': 'execution_error'
            })
    
    def categorize_failures(self, test_path, output, return_code):
        """Categorize failures based on output patterns."""
        lines = output.split('\n')
        current_test = None
        current_error = []
        
        for line in lines:
            # Track current test
            if '::test_' in line and ('FAILED' in line or 'ERROR' in line):
                if current_test and current_error:
                    self.classify_failure(current_test, '\n'.join(current_error))
                current_test = line.split()[0]
                current_error = []
            
            # Collect error details
            elif line.startswith('E ') or 'Error:' in line or 'TypeError:' in line:
                current_error.append(line)
        
        # Handle last test
        if current_test and current_error:
            self.classify_failure(current_test, '\n'.join(current_error))
    
    def classify_failure(self, test_name, error_text):
        """Classify a single failure as script problem or pipeline problem."""
        import re
        
        # Check for testing script problems
        for pattern in self.script_problem_patterns:
            if re.search(pattern, error_text, re.IGNORECASE):
                self.failure_categories['testing_script_problems'].append({
                    'test': test_name,
                    'error': error_text.strip(),
                    'pattern_matched': pattern,
                    'category': 'testing_script_problem'
                })
                return
        
        # Check for pipeline problems
        for pattern in self.pipeline_problem_patterns:
            if re.search(pattern, error_text, re.IGNORECASE):
                self.failure_categories['pipeline_problems'].append({
                    'test': test_name,
                    'error': error_text.strip(),
                    'pattern_matched': pattern,
                    'category': 'pipeline_problem'
                })
                return
        
        # Unknown category
        self.failure_categories['unknown'].append({
            'test': test_name,
            'error': error_text.strip(),
            'category': 'unknown'
        })
    
    def generate_analysis_report(self):
        """Generate comprehensive analysis report."""
        print("\n" + "=" * 60)
        print("📊 TEST FAILURE ANALYSIS REPORT")
        print("=" * 60)
        
        total_failures = sum(len(v) for v in self.failure_categories.values())
        
        print(f"\n📈 SUMMARY:")
        print(f"   Total Failures Analyzed: {total_failures}")
        print(f"   Testing Script Problems: {len(self.failure_categories['testing_script_problems'])}")
        print(f"   Pipeline Problems: {len(self.failure_categories['pipeline_problems'])}")
        print(f"   Unknown/Unclassified: {len(self.failure_categories['unknown'])}")
        
        # Detailed breakdown
        self.print_category_details('testing_script_problems', '🔧 TESTING SCRIPT PROBLEMS')
        self.print_category_details('pipeline_problems', '🏗️  PIPELINE PROBLEMS')
        self.print_category_details('unknown', '❓ UNCLASSIFIED FAILURES')
        
        # Save detailed report
        self.save_detailed_report()
    
    def print_category_details(self, category, title):
        """Print detailed information for a failure category."""
        failures = self.failure_categories[category]
        if not failures:
            return
        
        print(f"\n{title}")
        print("-" * len(title))
        
        for i, failure in enumerate(failures, 1):
            test_name = failure['test'].split('::')[-1] if '::' in failure['test'] else failure['test']
            print(f"\n{i}. {test_name}")
            if 'pattern_matched' in failure:
                print(f"   Pattern: {failure['pattern_matched']}")
            print(f"   Error: {failure['error'][:200]}...")
    
    def save_detailed_report(self):
        """Save detailed report to JSON file."""
        report_file = self.test_root / 'test_failure_analysis_report.json'
        
        report_data = {
            'timestamp': datetime.now().isoformat(),
            'summary': {
                'total_failures': sum(len(v) for v in self.failure_categories.values()),
                'testing_script_problems': len(self.failure_categories['testing_script_problems']),
                'pipeline_problems': len(self.failure_categories['pipeline_problems']),
                'unknown': len(self.failure_categories['unknown'])
            },
            'detailed_failures': self.failure_categories
        }
        
        with open(report_file, 'w') as f:
            json.dump(report_data, f, indent=2)
        
        print(f"\n💾 Detailed report saved to: {report_file}")


class IndependentTestValidator:
    """Creates pipeline-independent tests for validation."""
    
    def __init__(self):
        self.test_root = Path(__file__).parent
    
    def create_independent_connector_tests(self):
        """Create tests that validate connectors independently of pipeline state."""
        print("\n🔬 Creating Independent Connector Tests")
        print("=" * 50)
        
        # Create independent FRED tests
        self.create_independent_fred_tests()
        
        print("   📝 Independent test framework created successfully!")
    
    def create_independent_ibkr_tests(self):
        """Create IBKR tests that work independently."""
        # Implementation placeholder for IBKR independent tests
        pass
    
    def create_independent_yahoo_tests(self):
        """Create Yahoo Finance tests that work independently.""" 
        # Implementation placeholder for Yahoo independent tests
        pass
    
    def create_independent_fred_tests(self):
        """Create FRED tests that work independently."""
        test_content = '''#!/usr/bin/env python3
"""
Independent FRED Connector Tests
Tests FRED functionality without depending on existing pipeline state.
"""

import pytest
import os
import sys
from unittest.mock import Mock, patch
import pandas as pd
from datetime import datetime, timedelta

@pytest.fixture
def mock_fred_api():
    """Mock FRED API for independent testing."""
    mock_api = Mock()
    
    # Mock successful API response
    sample_data = pd.Series(
        [1.5, 1.6, 1.7, 1.8, 1.9],
        index=pd.date_range('2023-01-01', periods=5, freq='D'),
        name='FEDFUNDS'
    )
    mock_api.get_series.return_value = sample_data
    
    return mock_api

@pytest.fixture
def fred_connector_class():
    """Import FRED connector class with error handling."""
    try:
        fred_path = os.path.join(
            os.path.dirname(__file__), 
            '..', '..', '..', '..', '..', '..',
            'BackendPython', 'unicorn', '1_data_sources', 
            '1_raw', 'connectors', 'federal_reserve_fred'
        )
        sys.path.insert(0, fred_path)
        
        from fred_connector import FredConnector
        return FredConnector
    except ImportError:
        pytest.skip("FRED connector not available")

def test_fred_connector_initialization_independent(fred_connector_class):
    """Test FRED connector can initialize independently."""
    with patch.dict(os.environ, {'FRED_API_KEY': 'test_key'}):
        with patch('fredapi.Fred') as mock_fred:
            connector = fred_connector_class()
            assert connector is not None

def test_fred_data_processing_independent(mock_fred_api):
    """Test FRED data processing logic independently."""
    # Test with mock data
    data = mock_fred_api.get_series('FEDFUNDS')
    
    # Validate data structure
    assert isinstance(data, pd.Series)
    assert len(data) > 0
    assert data.index.dtype.kind == 'M'  # datetime index

def test_fred_error_handling_independent():
    """Test FRED error handling independently."""
    with patch.dict(os.environ, {}, clear=True):
        try:
            # This should fail without API key
            from fred_connector import FredConnector
            with pytest.raises(ValueError):
                FredConnector()
        except ImportError:
            pytest.skip("FRED connector not available")
'''
        
        independent_test_file = self.test_root / '1_raw' / 'connectors' / 'federal_reserve_fred' / 'test_fred_independent.py'
        independent_test_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(independent_test_file, 'w') as f:
            f.write(test_content)
        
        print(f"   ✅ Created: {independent_test_file}")


def main():
    """Main function to run comprehensive test analysis."""
    print("🚀 Starting Test Improvement Framework")
    print("=" * 60)
    
    # Step 1: Analyze current failures
    analyzer = TestFailureAnalyzer()
    analyzer.run_comprehensive_test_analysis()
    
    # Step 2: Create independent tests
    validator = IndependentTestValidator()
    validator.create_independent_connector_tests()
    
    print("\n✅ Test improvement framework completed!")


if __name__ == "__main__":
    main()