#!/usr/bin/env python3
"""
Comprehensive WebFrontend Test Runner

This is the master test wrapper that orchestrates all WebFrontend tests.
It provides comprehensive coverage of all unicornmetrics routes and functionality.

Test Coverage:
- Homepage and Public Dashboard
- Admin Dashboard and All Sub-pages
- Simulation Management (5 pages)
- IBKR Integration
- Navigation and Menu System
- Performance and Accessibility

Usage:
    python run_all_tests.py                    # Run all tests
    python run_all_tests.py --basic-only       # Basic validation only
    python run_all_tests.py --simulation-only  # Simulation tests only
    python run_all_tests.py --verbose         # Detailed output
"""

import sys
import os
import subprocess
import json
import time
from datetime import datetime
import argparse

# Add the current directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

class WebFrontendTestRunner:
    """Master test runner for all WebFrontend tests."""
    
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.test_results = {
            'timestamp': datetime.now().isoformat(),
            'total_tests': 0,
            'passed_tests': 0,
            'failed_tests': 0,
            'test_suites': {},
            'summary': {}
        }
    
    def log(self, message, level='INFO'):
        """Log messages with optional verbosity control."""
        if self.verbose or level in ['ERROR', 'SUCCESS', 'SUMMARY']:
            prefix = {
                'INFO': '🔍',
                'SUCCESS': '✅',
                'ERROR': '❌',
                'WARNING': '⚠️',
                'SUMMARY': '📊'
            }.get(level, 'ℹ️')
            print(f"{prefix} {message}")
    
    def run_test_script(self, script_name, description):
        """Run a test script and capture results."""
        self.log(f"Running {description}...")
        
        start_time = time.time()
        script_path = os.path.join(os.path.dirname(__file__), script_name)
        
        try:
            result = subprocess.run(
                [sys.executable, script_path],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            duration = time.time() - start_time
            
            success = result.returncode == 0
            
            test_result = {
                'script': script_name,
                'description': description,
                'success': success,
                'duration': duration,
                'stdout': result.stdout,
                'stderr': result.stderr,
                'return_code': result.returncode
            }
            
            if success:
                self.log(f"{description} PASSED ({duration:.2f}s)", 'SUCCESS')
                self.test_results['passed_tests'] += 1
            else:
                self.log(f"{description} FAILED ({duration:.2f}s)", 'ERROR')
                self.log(f"Error: {result.stderr[:200]}...", 'ERROR')
                self.test_results['failed_tests'] += 1
            
            self.test_results['test_suites'][script_name] = test_result
            self.test_results['total_tests'] += 1
            
            return success
            
        except subprocess.TimeoutExpired:
            self.log(f"{description} TIMEOUT (>30s)", 'ERROR')
            self.test_results['failed_tests'] += 1
            self.test_results['total_tests'] += 1
            return False
            
        except Exception as e:
            self.log(f"{description} ERROR: {str(e)}", 'ERROR')
            self.test_results['failed_tests'] += 1
            self.test_results['total_tests'] += 1
            return False
    
    def run_basic_tests(self):
        """Run basic validation test suite."""
        self.log("=" * 60)
        self.log("🧪 BASIC VALIDATION TEST SUITE", 'SUMMARY')
        self.log("=" * 60)
        
        return self.run_test_script(
            'test_basic_validation.py',
            'Basic WebFrontend Validation (10 tests)'
        )
    
    def run_homepage_tests(self):
        """Run homepage-specific tests."""
        self.log("=" * 60)
        self.log("🏠 HOMEPAGE TEST SUITE", 'SUMMARY')
        self.log("=" * 60)
        
        return self.run_test_script(
            'simple_homepage_test.py',
            'Simple Homepage Functionality Test'
        )
    
    def run_forecasting_tests(self):
        """Run forecasting dashboard tests."""
        self.log("=" * 60)
        self.log("🔮 FORECASTING DASHBOARD TEST SUITE", 'SUMMARY')
        self.log("=" * 60)
        
        return self.run_test_script(
            'test_forecasting_dashboard.py',
            'Forecasting Dashboard Integration Test'
        )
    
    def run_simulation_tests(self):
        """Run simulation management tests."""
        self.log("=" * 60)
        self.log("🎯 SIMULATION MANAGEMENT TEST SUITE", 'SUMMARY')
        self.log("=" * 60)
        
        return self.run_test_script(
            'test_simulation_management.py',
            'LEAN Simulation Management (5 pages)'
        )
    
    def run_selector_tests(self):
        """Run portfolio and simulation selector tests."""
        self.log("=" * 60)
        self.log("🦄 PORTFOLIO & SIMULATION SELECTOR TEST SUITE", 'SUMMARY')
        self.log("=" * 60)
        
        return self.run_test_script(
            'test_portfolio_simulation_selectors.py',
            'Portfolio & Simulation Selector Functionality'
        )
    
    def run_comprehensive_route_test(self):
        """Test all routes for basic accessibility."""
        self.log("=" * 60)
        self.log("🌐 COMPREHENSIVE ROUTE TEST", 'SUMMARY')
        self.log("=" * 60)
        
        # All routes we need to test
        routes_to_test = [
            ('/', 'Homepage (Public Dashboard)'),
            ('/unicorn', 'Public Unicorn Dashboard'),
            ('/admin/metrics', 'Admin Dashboard'),
            ('/admin/metrics/lean/portfolio', 'Portfolio Management'),
            ('/admin/metrics/lean/holdings', 'Portfolio Holdings'),
            ('/admin/metrics/lean/performance', 'Portfolio Performance'),
            ('/admin/metrics/lean/algorithms', 'Algorithm Management'),
            ('/admin/metrics/lean/algorithms/performance', 'Algorithm Performance'),
            ('/admin/metrics/lean/backtest', 'Backtest Results'),
            ('/admin/metrics/lean/simulations', 'Simulation Management'),
            ('/admin/metrics/lean/simulations/test_sim/holdings', 'Simulation Holdings'),
            ('/admin/metrics/lean/simulations/test_sim/performance', 'Simulation Performance'),
            ('/admin/metrics/lean/simulations/test_sim/algorithms', 'Simulation Algorithms'),
            ('/admin/metrics/lean/simulations/test_sim/backtest', 'Simulation Backtest'),
        ]
        
        import requests
        
        passed_routes = 0
        total_routes = len(routes_to_test)
        
        for route, description in routes_to_test:
            try:
                response = requests.get(f"http://localhost{route}", timeout=10)
                if response.status_code in [200, 403]:  # 403 is OK for admin routes
                    self.log(f"{route} -> {response.status_code} ({description})")
                    passed_routes += 1
                else:
                    self.log(f"{route} -> {response.status_code} FAILED ({description})", 'ERROR')
            except Exception as e:
                self.log(f"{route} -> ERROR: {str(e)} ({description})", 'ERROR')
        
        success_rate = (passed_routes / total_routes) * 100
        
        route_test_result = {
            'total_routes': total_routes,
            'passed_routes': passed_routes,
            'success_rate': success_rate,
            'success': success_rate >= 80
        }
        
        self.test_results['test_suites']['route_accessibility'] = route_test_result
        self.test_results['total_tests'] += 1
        
        if route_test_result['success']:
            self.log(f"Route Test PASSED: {passed_routes}/{total_routes} ({success_rate:.1f}%)", 'SUCCESS')
            self.test_results['passed_tests'] += 1
            return True
        else:
            self.log(f"Route Test FAILED: {passed_routes}/{total_routes} ({success_rate:.1f}%)", 'ERROR')
            self.test_results['failed_tests'] += 1
            return False
    
    def generate_summary_report(self):
        """Generate comprehensive test summary."""
        total = self.test_results['total_tests']
        passed = self.test_results['passed_tests']
        failed = self.test_results['failed_tests']
        success_rate = (passed / total * 100) if total > 0 else 0
        
        self.test_results['summary'] = {
            'overall_success_rate': success_rate,
            'total_test_suites': total,
            'passed_test_suites': passed,
            'failed_test_suites': failed,
            'overall_status': 'PASSED' if success_rate >= 70 else 'FAILED'
        }
        
        self.log("=" * 60, 'SUMMARY')
        self.log("🎯 COMPREHENSIVE TEST RESULTS", 'SUMMARY')
        self.log("=" * 60, 'SUMMARY')
        self.log(f"Total Test Suites: {total}", 'SUMMARY')
        self.log(f"Passed: {passed}", 'SUMMARY')
        self.log(f"Failed: {failed}", 'SUMMARY')
        self.log(f"Success Rate: {success_rate:.1f}%", 'SUMMARY')
        
        status_level = 'SUCCESS' if success_rate >= 70 else 'ERROR'
        status_emoji = '🎉' if success_rate >= 70 else '⚠️'
        self.log(f"{status_emoji} Overall Status: {self.test_results['summary']['overall_status']}", status_level)
        
        # Save detailed results
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        results_file = f"/workspaces/unicorninvesting/tests/WebFrontend/test_results/comprehensive_test_results_{timestamp}.json"
        
        try:
            with open(results_file, 'w') as f:
                json.dump(self.test_results, f, indent=2, default=str)
            self.log(f"Detailed results saved: {results_file}", 'SUMMARY')
        except Exception as e:
            self.log(f"Could not save results: {e}", 'WARNING')
        
        return success_rate >= 70
    
    def run_all_tests(self, basic_only=False, simulation_only=False):
        """Run all test suites."""
        self.log("🚀 Starting Comprehensive WebFrontend Testing...")
        self.log(f"Timestamp: {self.test_results['timestamp']}")
        
        results = []
        
        if simulation_only:
            results.append(self.run_simulation_tests())
        elif basic_only:
            results.append(self.run_basic_tests())
        else:
            # Run all test suites
            results.append(self.run_comprehensive_route_test())
            results.append(self.run_basic_tests())
            results.append(self.run_homepage_tests())
            results.append(self.run_forecasting_tests())
            results.append(self.run_simulation_tests())
            results.append(self.run_selector_tests())
        
        # Generate final summary
        overall_success = self.generate_summary_report()
        
        return 0 if overall_success else 1

def main():
    """Main entry point with command line argument handling."""
    parser = argparse.ArgumentParser(
        description='Comprehensive WebFrontend Test Runner',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_all_tests.py                    # Run all tests
  python run_all_tests.py --basic-only       # Basic validation only
  python run_all_tests.py --simulation-only  # Simulation tests only
  python run_all_tests.py --verbose         # Detailed output
        """
    )
    
    parser.add_argument('--basic-only', action='store_true',
                       help='Run only basic validation tests')
    parser.add_argument('--simulation-only', action='store_true',
                       help='Run only simulation management tests')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Enable verbose output')
    
    args = parser.parse_args()
    
    runner = WebFrontendTestRunner(verbose=args.verbose)
    exit_code = runner.run_all_tests(
        basic_only=args.basic_only,
        simulation_only=args.simulation_only
    )
    
    sys.exit(exit_code)

if __name__ == "__main__":
    main()
