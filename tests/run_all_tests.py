#!/usr/bin/env python3
"""
Comprehensive Test Runner for Unicorn Investing Platform
Executes all tests across all directories without duplicates

Usage:
    python run_all_tests.py [OPTIONS]
    
Options:
    --help, -h          Show this help message
    --quick, -q         Run only essential tests (faster)
    --verbose, -v       Show detailed output during execution
    --clean, -c         Clean test artifacts before running
    --json-only         Save results to JSON without console output
    --timeout SECONDS   Set custom timeout per test suite (default: 300)
"""

import os
import sys
import subprocess
import json
import time
import argparse
import shutil
from datetime import datetime
from pathlib import Path

# Import utility functions
try:
    from system.integration_analysis import generate_integration_summary
except ImportError:
    # If utility modules don't exist, define minimal implementations
    def generate_integration_summary(results_data):
        """Generate basic integration summary from test results"""
        return {
            'integration_status': 'analysis_pending',
            'summary': 'Integration analysis functionality not yet implemented',
            'recommendations': []
        }

try:
    from lean.structure_analysis import analyze_lean_compliance
except ImportError:
    def analyze_lean_compliance():
        """Analyze LEAN framework compliance"""
        return {
            'compliance_status': 'analysis_pending',
            'structure_valid': True,
            'recommendations': ['LEAN analysis tools not yet implemented']
        }

class UnicornTestRunner:
    def __init__(self, verbose=False, timeout=300, json_only=False, include_utilities=True):
        self.test_root = Path(__file__).parent
        self.verbose = verbose
        self.timeout = timeout
        self.json_only = json_only
        self.include_utilities = include_utilities
        self.results = {
            'execution_timestamp': datetime.now().isoformat(),
            'execution_mode': 'comprehensive',
            'test_suites': {},
            'utility_results': {},
            'summary': {
                'total_suites': 0,
                'passed_suites': 0,
                'failed_suites': 0,
                'skipped_suites': 0,
                'total_execution_time': 0,
                'utilities_executed': 0
            }
        }

    def print_status(self, status, message):
        """Print colored status messages (unless json_only mode)"""
        if self.json_only:
            return
            
        # Color codes
        colors = {
            'SUCCESS': '\033[0;32m✅',  # Green
            'FAILED': '\033[0;31m❌',   # Red
            'WARNING': '\033[1;33m⚠️',  # Yellow
            'INFO': '\033[0;34mℹ️',     # Blue
            'RUNNING': '\033[0;36m🔄',  # Cyan
            'CLEANUP': '\033[0;35m🧹',  # Purple
        }
        reset = '\033[0m'
        
        icon = colors.get(status, '📋')
        print(f"{icon} {message}{reset}")
        
    def clean_test_artifacts(self):
        """Clean test artifacts and cache files"""
        if not self.json_only:
            self.print_status('CLEANUP', 'Cleaning test artifacts...')
        
        # Clean temporary files
        temp_files = list(Path('/tmp').glob('test_*.log'))
        for temp_file in temp_files:
            try:
                temp_file.unlink()
            except:
                pass
                
        # Clean Python cache
        for root, dirs, files in os.walk(self.test_root):
            # Remove __pycache__ directories
            if '__pycache__' in dirs:
                shutil.rmtree(Path(root) / '__pycache__', ignore_errors=True)
                dirs.remove('__pycache__')
            
            # Remove .pyc files
            for file in files:
                if file.endswith('.pyc'):
                    try:
                        (Path(root) / file).unlink()
                    except:
                        pass
        
        # Clean pytest cache
        pytest_cache = self.test_root / '.pytest_cache'
        if pytest_cache.exists():
            shutil.rmtree(pytest_cache, ignore_errors=True)
            
        if not self.json_only:
            self.print_status('SUCCESS', 'Test artifacts cleaned')
        
    def run_test_suite(self, suite_name, test_path, description=""):
        """Run a specific test suite and capture results"""
        if not self.json_only:
            print(f"\n{'='*60}")
            self.print_status('RUNNING', f"RUNNING TEST SUITE: {suite_name}")
            print(f"📁 Path: {test_path}")
            if description:
                print(f"📝 Description: {description}")
            print(f"{'='*60}")
        
        start_time = time.time()
        
        try:
            # Prepare command for verbose or normal mode
            cmd_args = [sys.executable, '-m', 'pytest', str(test_path)]
            
            if self.verbose and not self.json_only:
                cmd_args.extend(['-v', '--tb=long'])
            else:
                cmd_args.extend(['-v', '--tb=short'])
                
            # Run pytest
            result = subprocess.run(
                cmd_args,
                cwd=self.test_root,
                capture_output=not self.verbose or self.json_only,
                text=True,
                timeout=self.timeout
            )
            
            execution_time = time.time() - start_time
            
            # Parse results
            success = result.returncode == 0
            
            self.results['test_suites'][suite_name] = {
                'path': str(test_path),
                'description': description,
                'status': 'PASSED' if success else 'FAILED',
                'execution_time': round(execution_time, 2),
                'stdout': result.stdout if result.stdout else '',
                'stderr': result.stderr if result.stderr else '',
                'return_code': result.returncode
            }
            
            # Update summary
            if success:
                self.results['summary']['passed_suites'] += 1
                if not self.json_only:
                    self.print_status('SUCCESS', f"{suite_name} - PASSED ({execution_time:.2f}s)")
            else:
                self.results['summary']['failed_suites'] += 1
                if not self.json_only:
                    self.print_status('FAILED', f"{suite_name} - FAILED ({execution_time:.2f}s)")
                    if result.stderr and not self.verbose:
                        print(f"Error Output: {result.stderr}")
                        
        except subprocess.TimeoutExpired:
            execution_time = time.time() - start_time
            self.results['test_suites'][suite_name] = {
                'path': str(test_path),
                'description': description,
                'status': 'TIMEOUT',
                'execution_time': round(execution_time, 2),
                'stdout': '',
                'stderr': f'Test suite timed out after {self.timeout} seconds',
                'return_code': -1
            }
            self.results['summary']['failed_suites'] += 1
            if not self.json_only:
                self.print_status('WARNING', f"{suite_name} - TIMEOUT ({execution_time:.2f}s)")
            
        except Exception as e:
            execution_time = time.time() - start_time
            self.results['test_suites'][suite_name] = {
                'path': str(test_path),
                'description': description,
                'status': 'ERROR',
                'execution_time': round(execution_time, 2),
                'stdout': '',
                'stderr': str(e),
                'return_code': -1
            }
            self.results['summary']['failed_suites'] += 1
            if not self.json_only:
                self.print_status('FAILED', f"{suite_name} - ERROR ({execution_time:.2f}s): {e}")
            
    def run_utility_script(self, utility_name, script_path, description=""):
        """Run a utility script and capture results"""
        if not self.json_only:
            self.print_status('RUNNING', f"EXECUTING UTILITY: {utility_name}")
        
        start_time = time.time()
        
        try:
            # Run utility script
            result = subprocess.run([
                sys.executable, str(script_path)
            ], 
            cwd=self.test_root,
            capture_output=not self.verbose or self.json_only,
            text=True,
            timeout=self.timeout
            )
            
            execution_time = time.time() - start_time
            success = result.returncode == 0
            
            self.results['utility_results'][utility_name] = {
                'path': str(script_path),
                'description': description,
                'status': 'PASSED' if success else 'FAILED',
                'execution_time': round(execution_time, 2),
                'stdout': result.stdout if result.stdout else '',
                'stderr': result.stderr if result.stderr else '',
                'return_code': result.returncode
            }
            
            self.results['summary']['utilities_executed'] += 1
            
            if not self.json_only:
                if success:
                    self.print_status('SUCCESS', f"{utility_name} - COMPLETED ({execution_time:.2f}s)")
                else:
                    self.print_status('FAILED', f"{utility_name} - FAILED ({execution_time:.2f}s)")
                    
        except subprocess.TimeoutExpired:
            execution_time = time.time() - start_time
            self.results['utility_results'][utility_name] = {
                'path': str(script_path),
                'description': description,
                'status': 'TIMEOUT',
                'execution_time': round(execution_time, 2),
                'stdout': '',
                'stderr': f'Utility script timed out after {self.timeout} seconds',
                'return_code': -1
            }
            self.results['summary']['utilities_executed'] += 1
            if not self.json_only:
                self.print_status('WARNING', f"{utility_name} - TIMEOUT ({execution_time:.2f}s)")
                
        except Exception as e:
            execution_time = time.time() - start_time
            self.results['utility_results'][utility_name] = {
                'path': str(script_path),
                'description': description,
                'status': 'ERROR',
                'execution_time': round(execution_time, 2),
                'stdout': '',
                'stderr': str(e),
                'return_code': -1
            }
            self.results['summary']['utilities_executed'] += 1
            if not self.json_only:
                self.print_status('FAILED', f"{utility_name} - ERROR ({execution_time:.2f}s): {e}")

    def run_built_in_utilities(self):
        """Run built-in utility functions"""
        if not self.include_utilities:
            return
            
        if not self.json_only:
            print(f"\n{'='*60}")
            self.print_status('INFO', 'RUNNING BUILT-IN UTILITY ANALYSIS')
            print(f"{'='*60}")
        
        # LEAN Framework Analysis
        try:
            start_time = time.time()
            lean_analysis = analyze_lean_compliance()
            execution_time = time.time() - start_time
            
            self.results['utility_results']['LEAN_Compliance_Analysis'] = {
                'path': 'built-in',
                'description': 'LEAN framework structure and compliance analysis',
                'status': 'COMPLETED',
                'execution_time': round(execution_time, 3),
                'analysis_result': lean_analysis,
                'return_code': 0
            }
            
            if not self.json_only:
                self.print_status('SUCCESS', f"LEAN Compliance Analysis - COMPLETED ({execution_time:.3f}s)")
                
        except Exception as e:
            self.results['utility_results']['LEAN_Compliance_Analysis'] = {
                'path': 'built-in',
                'description': 'LEAN framework structure and compliance analysis',
                'status': 'ERROR',
                'execution_time': 0,
                'stderr': str(e),
                'return_code': -1
            }
            if not self.json_only:
                self.print_status('FAILED', f"LEAN Compliance Analysis - ERROR: {e}")
        
        # Integration Summary Analysis  
        try:
            start_time = time.time()
            integration_summary = generate_integration_summary(self.results)
            execution_time = time.time() - start_time
            
            self.results['utility_results']['Integration_Summary_Analysis'] = {
                'path': 'built-in',
                'description': 'System integration analysis and summary generation',
                'status': 'COMPLETED', 
                'execution_time': round(execution_time, 3),
                'analysis_result': integration_summary,
                'return_code': 0
            }
            
            if not self.json_only:
                self.print_status('SUCCESS', f"Integration Summary Analysis - COMPLETED ({execution_time:.3f}s)")
                
        except Exception as e:
            self.results['utility_results']['Integration_Summary_Analysis'] = {
                'path': 'built-in',
                'description': 'System integration analysis and summary generation',
                'status': 'ERROR',
                'execution_time': 0,
                'stderr': str(e),
                'return_code': -1
            }
            if not self.json_only:
                self.print_status('FAILED', f"Integration Summary Analysis - ERROR: {e}")
        
        self.results['summary']['utilities_executed'] += 2

    def get_test_suites(self, quick_mode=False):
        """Get test suite configuration based on execution mode"""
        
        if quick_mode:
            # Quick mode - only essential tests
            return [
                {
                    'name': 'Complete_System_Validation',
                    'path': 'system/test_complete_system_validation.py',
                    'description': 'Complete end-to-end system integration validation (Essential)'
                },
                {
                    'name': 'ETH_Kelly_Integration',
                    'path': 'unicorn/4_portfolios/Myportolio/test_eth_kelly_integration.py',
                    'description': 'ETH-Kelly Criterion complete integration validation (Essential)'
                }
            ]
        
        # Full comprehensive test suite
        return [
            # 1. System Architecture Tests (Foundation)
            {
                'name': 'System_Architecture',
                'path': 'system/test_system_architecture.py',
                'description': 'Validates system architectural compliance and structure'
            },
            
            # 2. Core Algorithm Tests  
            {
                'name': 'ETH_Basic_Risk',
                'path': 'unicorn/3_risk_algorithms/test_eth_basic_risk.py',
                'description': 'ETH basic risk management algorithm validation'
            },
            {
                'name': 'Kelly_Criterion',
                'path': 'unicorn/4_portfolios/utilities/test_kelly_criterion.py',
                'description': 'Kelly Criterion position sizing algorithm validation'
            },
            {
                'name': 'Enhanced_ETH_Alpha',
                'path': 'unicorn/2_alpha_models/test_enhanced_eth_alpha.py',
                'description': 'Enhanced ETH alpha model validation'
            },
            
            # 3. Data Source Tests
            {
                'name': 'IBKR_Data_Quality',
                'path': 'unicorn/1_data_sources/1_raw/connectors/interactive_brokers/test_data_quality.py',
                'description': 'Interactive Brokers data quality validation'
            },
            {
                'name': 'IBKR_Integration',
                'path': 'unicorn/1_data_sources/1_raw/connectors/interactive_brokers/test_ibkr_integration.py',
                'description': 'Interactive Brokers API integration validation'
            },
            {
                'name': 'IBKR_Technical_Indicators',
                'path': 'unicorn/1_data_sources/1_raw/connectors/interactive_brokers/test_technical_indicators.py',
                'description': 'Interactive Brokers technical indicators validation'
            },
            {
                'name': 'IBKR_E2E_Pipeline',
                'path': 'unicorn/1_data_sources/1_raw/connectors/interactive_brokers/test_e2e_pipeline.py',
                'description': 'Interactive Brokers end-to-end data pipeline validation'
            },
            
            # 4. Portfolio Integration Tests
            {
                'name': 'ETH_Kelly_Integration',
                'path': 'unicorn/4_portfolios/Myportolio/test_eth_kelly_integration.py',
                'description': 'ETH-Kelly Criterion complete integration validation'
            },
            
            # 5. Frontend Data Validation (Progressive Approaches)
            {
                'name': 'Basic_Data_Validation',
                'path': 'unicorn/4_portfolios/Myportolio/test_basic_data_validation.py',
                'description': 'Basic frontend-backend data validation (Approach 1)'
            },
            {
                'name': 'Production_Data_Validation',
                'path': 'unicorn/4_portfolios/Myportolio/test_production_data_validation.py',
                'description': 'Production-ready data validation (Approach 2)'
            },
            {
                'name': 'Intelligent_Data_Validation',
                'path': 'unicorn/4_portfolios/Myportolio/test_intelligent_data_validation.py',
                'description': 'Intelligent semantic data validation (Approach 3)'
            },
            {
                'name': 'Enhanced_Data_Validation',
                'path': 'unicorn/4_portfolios/Myportolio/test_final_enhanced_validation.py',
                'description': 'Ultimate comprehensive data validation (Approach 4)'
            },
            {
                'name': 'Targeted_Data_Validation',
                'path': 'unicorn/4_portfolios/Myportolio/test_targeted_validation.py',
                'description': 'Precision component data validation (Approach 5)'
            },
            
            # 6. WebFrontend Tests
            {
                'name': 'WebFrontend_Basic_Validation',
                'path': 'WebFrontend/test_basic_validation.py',
                'description': 'WebFrontend basic functionality validation'
            },
            {
                'name': 'WebFrontend_Forecasting_Dashboard',
                'path': 'WebFrontend/test_forecasting_dashboard.py',
                'description': 'WebFrontend forecasting dashboard validation'
            },
            
            # 7. LEAN Framework Tests
            {
                'name': 'LEAN_Insights',
                'path': 'lean/test_lean_insights.py',
                'description': 'LEAN framework insights and integration validation'
            },
            
            # 8. Complete System Validation (Final)
            {
                'name': 'Complete_System_Validation',
                'path': 'system/test_complete_system_validation.py',
                'description': 'Complete end-to-end system integration validation'
            }
        ]

    def run_all_tests(self, quick_mode=False):
        """Execute all test suites in the proper order"""
        
        mode_name = "QUICK" if quick_mode else "COMPREHENSIVE"
        self.results['execution_mode'] = 'quick' if quick_mode else 'comprehensive'
        
        if not self.json_only:
            print(f"🚀 STARTING {mode_name} UNICORN INVESTING TEST SUITE")
            print(f"📍 Test Root: {self.test_root}")
            print(f"🕐 Start Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
            if quick_mode:
                self.print_status('INFO', 'Running in QUICK mode - Essential tests only')
            if self.include_utilities:
                self.print_status('INFO', 'Utilities enabled - Will run analysis tools after tests')
            print()
        
        # Get test suites based on mode
        test_suites = self.get_test_suites(quick_mode)
        
        # Execute each test suite
        for suite in test_suites:
            test_path = self.test_root / suite['path']
            
            # Check if test file exists
            if not test_path.exists():
                if not self.json_only:
                    self.print_status('WARNING', f"SKIPPING {suite['name']} - File not found: {test_path}")
                self.results['test_suites'][suite['name']] = {
                    'path': str(suite['path']),
                    'description': suite['description'],
                    'status': 'SKIPPED',
                    'execution_time': 0,
                    'stdout': '',
                    'stderr': f'Test file not found: {test_path}',
                    'return_code': -1
                }
                self.results['summary']['skipped_suites'] += 1
                self.results['summary']['total_suites'] += 1
                continue
                
            # Run the test suite
            self.run_test_suite(
                suite['name'], 
                suite['path'], 
                suite['description']
            )
            
        # Run standalone utility scripts after tests
        if self.include_utilities:
            self.run_standalone_utilities()
            
        # Run built-in utility analysis
        if self.include_utilities:
            self.run_built_in_utilities()

    def run_standalone_utilities(self):
        """Execute standalone utility scripts"""
        if not self.json_only:
            print(f"\n{'='*60}")
            self.print_status('INFO', 'RUNNING STANDALONE UTILITY SCRIPTS')
            print(f"{'='*60}")
        
        # Define standalone utilities to execute
        standalone_utilities = [
            {
                'name': 'Simple_Homepage_Test',
                'path': 'WebFrontend/simple_homepage_test.py',
                'description': 'Standalone homepage functionality validation'
            }
        ]
        
        # Execute each utility
        for utility in standalone_utilities:
            utility_path = self.test_root / utility['path']
            
            if utility_path.exists():
                self.run_utility_script(
                    utility['name'],
                    utility['path'],
                    utility['description']
                )
            else:
                if not self.json_only:
                    self.print_status('WARNING', f"SKIPPING {utility['name']} - File not found: {utility_path}")
                self.results['utility_results'][utility['name']] = {
                    'path': str(utility['path']),
                    'description': utility['description'],
                    'status': 'SKIPPED',
                    'execution_time': 0,
                    'stderr': f'Utility script not found: {utility_path}',
                    'return_code': -1
                }
                self.results['summary']['utilities_executed'] += 1

    def generate_report(self):
        """Generate comprehensive test execution report"""
        
        # Calculate summary statistics
        total_time = self.results['summary']['total_execution_time']
        success_rate = (self.results['summary']['passed_suites'] / 
                       max(self.results['summary']['total_suites'], 1)) * 100
        
        if not self.json_only:
            print(f"\n{'='*80}")
            self.print_status('INFO', 'COMPREHENSIVE TEST EXECUTION SUMMARY')
            print(f"{'='*80}")
            print(f"🕐 Execution Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
            print(f"⏱️ Total Duration: {total_time:.2f} seconds ({total_time/60:.1f} minutes)")
            print(f"📈 Success Rate: {success_rate:.1f}%")
            print(f"🎯 Execution Mode: {self.results['execution_mode'].upper()}")
            print()
            print(f"📊 Test Suite Results:")
            print(f"  ✅ Passed:  {self.results['summary']['passed_suites']}")
            print(f"  ❌ Failed:  {self.results['summary']['failed_suites']}")
            print(f"  ⚠️ Skipped: {self.results['summary']['skipped_suites']}")
            print(f"  📊 Total:   {self.results['summary']['total_suites']}")
            
            if self.include_utilities:
                print(f"  🛠️ Utilities: {self.results['summary']['utilities_executed']}")
                
            print(f"{'='*80}")
            
            # Detailed results by status
            for status in ['PASSED', 'FAILED', 'TIMEOUT', 'ERROR', 'SKIPPED']:
                suites = [name for name, data in self.results['test_suites'].items() 
                         if data['status'] == status]
                if suites:
                    status_icon = {
                        'PASSED': '✅', 'FAILED': '❌', 'TIMEOUT': '⏱️', 
                        'ERROR': '💥', 'SKIPPED': '⚠️'
                    }
                    print(f"\n{status_icon[status]} {status} TEST SUITES ({len(suites)}):")
                    for suite in suites:
                        duration = self.results['test_suites'][suite]['execution_time']
                        print(f"  • {suite} ({duration:.2f}s)")
            
            # Utility results summary
            if self.include_utilities and self.results['utility_results']:
                print(f"\n🛠️ UTILITY EXECUTION RESULTS:")
                for util_name, util_data in self.results['utility_results'].items():
                    status_icon = {
                        'COMPLETED': '✅', 'PASSED': '✅', 'FAILED': '❌', 
                        'TIMEOUT': '⏱️', 'ERROR': '💥', 'SKIPPED': '⚠️'
                    }
                    icon = status_icon.get(util_data['status'], '📋')
                    duration = util_data['execution_time']
                    print(f"  {icon} {util_name} ({duration:.3f}s)")
            
            # Overall system status
            print(f"\n{'='*80}")
            if success_rate >= 80:
                self.print_status('SUCCESS', 'OVERALL STATUS: SYSTEM READY')
                print("💯 Platform validated and ready for deployment")
            else:
                self.print_status('WARNING', 'OVERALL STATUS: ISSUES DETECTED')
                print("🔧 Platform needs attention before deployment")
        
        # Save detailed results to JSON
        results_dir = self.test_root / 'comprehensive_results'
        results_dir.mkdir(exist_ok=True)
        
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        mode_suffix = '_quick' if self.results['execution_mode'] == 'quick' else '_comprehensive'
        utilities_suffix = '_with_utilities' if self.include_utilities else ''
        results_file = results_dir / f'test_results{mode_suffix}{utilities_suffix}_{timestamp}.json'
        
        with open(results_file, 'w') as f:
            json.dump(self.results, f, indent=2)
            
        if not self.json_only:
            print(f"\n📁 Detailed results saved to: {results_file}")
        
        # Return exit code based on results
        if self.results['summary']['failed_suites'] > 0:
            if not self.json_only:
                self.print_status('FAILED', 'TEST EXECUTION COMPLETED WITH FAILURES')
            return 1
        elif self.results['summary']['skipped_suites'] > 0:
            if not self.json_only:
                self.print_status('WARNING', 'TEST EXECUTION COMPLETED WITH SKIPPED TESTS')
            return 2
        else:
            if not self.json_only:
                self.print_status('SUCCESS', 'ALL TESTS PASSED SUCCESSFULLY')
            return 0

def main():
    """Main execution function with argument parsing"""
    parser = argparse.ArgumentParser(
        description='Comprehensive Test Runner for Unicorn Investing Platform',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_all_tests.py                    # Run all tests with utilities
  python run_all_tests.py --quick            # Run only essential tests (faster)
  python run_all_tests.py --verbose          # Run with detailed output
  python run_all_tests.py --clean            # Clean artifacts before running
  python run_all_tests.py --json-only        # Save results to JSON only
  python run_all_tests.py --no-utilities     # Skip utility scripts and analysis
  python run_all_tests.py --timeout 600      # Set 10-minute timeout per test
        """
    )
    
    parser.add_argument('--quick', '-q', action='store_true',
                       help='Run only essential tests (faster execution)')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Show detailed output during execution')
    parser.add_argument('--clean', '-c', action='store_true',
                       help='Clean test artifacts before running')
    parser.add_argument('--json-only', action='store_true',
                       help='Save results to JSON without console output')
    parser.add_argument('--no-utilities', action='store_true',
                       help='Skip utility scripts and analysis tools')
    parser.add_argument('--timeout', type=int, default=300,
                       help='Set timeout per test suite in seconds (default: 300)')
    
    args = parser.parse_args()
    
    # Create runner with specified options
    runner = UnicornTestRunner(
        verbose=args.verbose,
        timeout=args.timeout,
        json_only=args.json_only,
        include_utilities=not args.no_utilities
    )
    
    try:
        # Clean artifacts if requested
        if args.clean:
            runner.clean_test_artifacts()
            
        # Run tests
        runner.run_all_tests(quick_mode=args.quick)
        exit_code = runner.generate_report()
        sys.exit(exit_code)
        
    except KeyboardInterrupt:
        if not args.json_only:
            print(f"\n\n⚠️ TEST EXECUTION INTERRUPTED BY USER")
        runner.generate_report()
        sys.exit(130)
        
    except Exception as e:
        if not args.json_only:
            print(f"\n\n💥 CRITICAL ERROR DURING TEST EXECUTION: {e}")
        runner.generate_report()
        sys.exit(1)

if __name__ == "__main__":
    main()
