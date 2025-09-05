#!/usr/bin/env python3
"""
WebFrontend Basic Validation Tests

Comprehensive test suite for the Drupal 11 WebFrontend, focusing on:
- Homepage functionality and accessibility
- Dashboard integration with live IBKR data
- Simulation selector and navigation links
- Portfolio management interface
- API connectivity and data flow

This test suite validates the restored dashboard functionality including
live IBKR portfolio integration and original portfolio management features.
"""

import requests
import json
import time
import re
import sys
from urllib.parse import urljoin
from datetime import datetime

# Test configuration
BASE_URL = "http://localhost"
API_TIMEOUT = 10
PERFORMANCE_THRESHOLD = 3.0  # seconds
MIN_CONTENT_LENGTH = 1000    # bytes

class TestBasicWebFrontendValidation:
    """Basic WebFrontend validation tests."""
    
    def test_homepage_accessibility(self):
        """Test that the homepage is accessible and returns valid content."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            
            assert response.status_code == 200, f"Homepage returned {response.status_code}"
            assert len(response.content) > MIN_CONTENT_LENGTH, "Homepage content too small"
            assert 'text/html' in response.headers.get('content-type', ''), "Not HTML content"
            
            # Check for Drupal headers
            generator = response.headers.get('x-generator', '')
            assert 'Drupal' in generator, f"Not a Drupal site: {generator}"
            
            print(f"✅ Homepage: {response.status_code}, {len(response.content)} bytes, {generator}")
            return True
            
        except requests.RequestException as e:
            raise Exception(f"Homepage request failed: {e}")
    
    def test_homepage_performance(self):
        """Test homepage loading performance meets requirements."""
        start_time = time.time()
        
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            load_time = time.time() - start_time
            
            assert response.status_code == 200, "Homepage not accessible"
            assert load_time < PERFORMANCE_THRESHOLD, f"Load time {load_time:.2f}s exceeds {PERFORMANCE_THRESHOLD}s"
            
            print(f"✅ Performance: {load_time:.2f}s (target: <{PERFORMANCE_THRESHOLD}s)")
            return load_time
            
        except requests.RequestException as e:
            raise Exception(f"Performance test failed: {e}")
    
    def test_dashboard_content(self):
        """Test dashboard contains required elements for portfolio management."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Check for restored dashboard elements
            required_elements = [
                'Unicorn Investing Platform',          # Main header
                'Myportolio',                         # Portfolio name
                'Live Portfolio Status',              # IBKR integration section
                'simulation-selector',                # Simulation selector (restored)
                'Account Summary',                    # IBKR account info
                'Platform Features',                  # Feature showcase
                'Last Updated',                       # Data freshness
                'Net Liquidation Value',              # IBKR financial data
            ]
            
            missing_elements = []
            for element in required_elements:
                if element not in content:
                    missing_elements.append(element)
            
            assert len(missing_elements) == 0, f"Missing dashboard elements: {missing_elements}"
            
            print(f"✅ Dashboard content: All {len(required_elements)} required elements found")
            return True
            
        except requests.RequestException as e:
            raise Exception(f"Dashboard content test failed: {e}")
    
    def test_simulation_selector_restored(self):
        """Test that simulation selector functionality is restored."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Check for simulation selector elements
            simulation_elements = [
                'simulation-selector',
                'portfolio-selector',
                'Select Simulation',
                'available_simulations',
            ]
            
            found_elements = []
            for element in simulation_elements:
                if element in content:
                    found_elements.append(element)
            
            assert len(found_elements) > 0, "No simulation selector elements found - functionality not restored"
            
            # Check for dropdown functionality
            assert 'select' in content.lower(), "No select dropdown found"
            
            print(f"✅ Simulation selector: {len(found_elements)}/{len(simulation_elements)} elements found")
            return len(found_elements)
            
        except requests.RequestException as e:
            raise Exception(f"Simulation selector test failed: {e}")
    
    def test_navigation_links_restored(self):
        """Test that navigation links to portfolio sub-pages are restored."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Check for navigation links that should be restored
            expected_links = [
                'Holdings',
                'Performance', 
                'Algorithms',
                'Backtests',
                'lean-holdings',
                'lean-performance', 
                'lean-algorithms',
            ]
            
            found_links = []
            for link in expected_links:
                if link in content:
                    found_links.append(link)
            
            assert len(found_links) >= 4, f"Insufficient navigation links found: {found_links}"
            
            print(f"✅ Navigation links: {len(found_links)}/{len(expected_links)} links found")
            return found_links
            
        except requests.RequestException as e:
            raise Exception(f"Navigation links test failed: {e}")
    
    def test_live_ibkr_integration(self):
        """Test that live IBKR integration is working."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Check for IBKR integration elements
            ibkr_elements = [
                'Live Portfolio Status',
                'Account Summary',
                'Net Liquidation Value',
                'Cash Balance', 
                'Market Value',
                'Unrealized P&L',
                'Data Freshness',
                'Source: IBKR',
            ]
            
            found_ibkr = []
            for element in ibkr_elements:
                if element in content:
                    found_ibkr.append(element)
            
            assert len(found_ibkr) >= 5, f"IBKR integration incomplete: {found_ibkr}"
            
            # Check for timestamp data
            assert 'Last Updated' in content, "No data timestamp found"
            
            print(f"✅ IBKR integration: {len(found_ibkr)}/{len(ibkr_elements)} elements found")
            return found_ibkr
            
        except requests.RequestException as e:
            raise Exception(f"IBKR integration test failed: {e}")
    
    def test_responsive_design(self):
        """Test basic responsive design elements."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Check for responsive design indicators
            responsive_elements = [
                'viewport',
                'responsive',
                'mobile',
                '@media',
                'bootstrap',
            ]
            
            found_responsive = []
            for element in responsive_elements:
                if element.lower() in content.lower():
                    found_responsive.append(element)
            
            # Check for CSS grid/flexbox
            modern_css = ['grid', 'flex', 'display']
            for css in modern_css:
                if css in content.lower():
                    found_responsive.append(css)
            
            assert len(found_responsive) > 0, "No responsive design elements found"
            
            print(f"✅ Responsive design: {len(found_responsive)} indicators found")
            return found_responsive
            
        except requests.RequestException as e:
            raise Exception(f"Responsive design test failed: {e}")
    
    def test_css_styling_applied(self):
        """Test that CSS styling is properly applied."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Check for CSS files and styling
            css_indicators = [
                '.css',
                'stylesheet',
                'dashboard.css',
                'style',
                'background-color',
                'font-family',
            ]
            
            found_css = []
            for indicator in css_indicators:
                if indicator in content:
                    found_css.append(indicator)
            
            assert len(found_css) >= 2, f"CSS styling not properly applied: {found_css}"
            
            print(f"✅ CSS styling: {len(found_css)} indicators found")
            return found_css
            
        except requests.RequestException as e:
            raise Exception(f"CSS styling test failed: {e}")

class TestDashboardFunctionality:
    """Test specific dashboard functionality."""
    
    def test_portfolio_data_display(self):
        """Test that portfolio data is properly displayed."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Look for financial data formatting
            financial_patterns = [
                r'\$[\d,]+\.?\d*',       # Currency formatting
                r'\d+\.\d+%',           # Percentage formatting
                r'[\d,]+\s*(shares?|units?)',  # Quantity formatting
            ]
            
            found_patterns = 0
            for pattern in financial_patterns:
                if re.search(pattern, content, re.IGNORECASE):
                    found_patterns += 1
            
            assert found_patterns > 0, "No financial data formatting found"
            
            print(f"✅ Portfolio data: {found_patterns} formatting patterns found")
            return found_patterns
            
        except requests.RequestException as e:
            raise Exception(f"Portfolio data test failed: {e}")
    
    def test_system_status_display(self):
        """Test that system status is properly displayed."""
        try:
            response = requests.get(BASE_URL, timeout=API_TIMEOUT)
            content = response.text
            
            # Look for system status indicators
            status_indicators = [
                'operational',
                'running',
                'connected',
                'ready',
                'status',
                '✅',
                '⚠️',
                'System Status',
            ]
            
            found_status = []
            for indicator in status_indicators:
                if indicator in content:
                    found_status.append(indicator)
            
            assert len(found_status) >= 2, f"System status not properly displayed: {found_status}"
            
            print(f"✅ System status: {len(found_status)} indicators found")
            return found_status
            
        except requests.RequestException as e:
            raise Exception(f"System status test failed: {e}")

def run_comprehensive_webfrontend_tests():
    """Run all WebFrontend tests and return results."""
    
    print("\n🧪 WebFrontend Comprehensive Test Suite")
    print("=" * 50)
    
    test_results = {
        'timestamp': datetime.now().isoformat(),
        'total_tests': 0,
        'passed_tests': 0,
        'failed_tests': 0,
        'test_details': {},
    }
    
    # Basic validation tests
    basic_tests = TestBasicWebFrontendValidation()
    dashboard_tests = TestDashboardFunctionality()
    
    test_methods = [
        ('homepage_accessibility', basic_tests.test_homepage_accessibility),
        ('homepage_performance', basic_tests.test_homepage_performance),
        ('dashboard_content', basic_tests.test_dashboard_content),
        ('simulation_selector_restored', basic_tests.test_simulation_selector_restored),
        ('navigation_links_restored', basic_tests.test_navigation_links_restored),
        ('live_ibkr_integration', basic_tests.test_live_ibkr_integration),
        ('responsive_design', basic_tests.test_responsive_design),
        ('css_styling_applied', basic_tests.test_css_styling_applied),
        ('portfolio_data_display', dashboard_tests.test_portfolio_data_display),
        ('system_status_display', dashboard_tests.test_system_status_display),
    ]
    
    for test_name, test_method in test_methods:
        test_results['total_tests'] += 1
        
        try:
            print(f"\n🔍 Running {test_name}...")
            result = test_method()
            test_results['passed_tests'] += 1
            test_results['test_details'][test_name] = {
                'status': 'PASSED',
                'result': result,
            }
            
        except Exception as e:
            test_results['failed_tests'] += 1
            test_results['test_details'][test_name] = {
                'status': 'FAILED', 
                'error': str(e),
            }
            print(f"❌ {test_name} failed: {e}")
    
    # Calculate success rate
    success_rate = (test_results['passed_tests'] / test_results['total_tests']) * 100 if test_results['total_tests'] > 0 else 0
    
    print(f"\n📊 WebFrontend Test Results")
    print(f"Total Tests: {test_results['total_tests']}")
    print(f"Passed: {test_results['passed_tests']}")
    print(f"Failed: {test_results['failed_tests']}")
    print(f"Success Rate: {success_rate:.1f}%")
    
    if success_rate >= 80:
        print("🎉 WebFrontend testing SUCCESSFUL!")
    else:
        print("⚠️ WebFrontend testing needs attention")
    
    return test_results

if __name__ == "__main__":
    # Run comprehensive WebFrontend tests
    results = run_comprehensive_webfrontend_tests()
    
    # Save results to file
    import os
    results_dir = "/workspaces/unicorninvesting/tests/WebFrontend"
    os.makedirs(results_dir, exist_ok=True)
    
    results_file = f"{results_dir}/test_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\n💾 Test results saved to: {results_file}")
    
    # Exit with appropriate code
    exit_code = 0 if results['failed_tests'] == 0 else 1
    exit(exit_code)
