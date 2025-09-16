#!/usr/bin/env python3
"""
Economic Indicators Testing Suite Runner

This script runs both bronze and silver layer tests for the economic indicators pipeline,
providing a comprehensive validation of the entire data processing workflow.

Usage:
    python run_economic_indicators_tests.py [--bronze-only] [--silver-only] [--quick]

Options:
    --bronze-only    Run only bronze layer tests
    --silver-only    Run only silver layer tests  
    --quick         Run abbreviated tests for faster validation
    --verbose       Show detailed test output
"""

import os
import sys
import subprocess
import argparse
import time
from pathlib import Path

def main():
    parser = argparse.ArgumentParser(description='Run economic indicators testing suite')
    parser.add_argument('--bronze-only', action='store_true', help='Run only bronze layer tests')
    parser.add_argument('--silver-only', action='store_true', help='Run only silver layer tests')
    parser.add_argument('--quick', action='store_true', help='Run abbreviated tests')
    parser.add_argument('--verbose', '-v', action='store_true', help='Show detailed output')
    
    args = parser.parse_args()
    
    # Setup paths
    tests_root = Path(__file__).parent
    bronze_test_dir = tests_root / "2_bronze" / "economic_indicators"
    silver_test_dir = tests_root / "3_silver" / "economic_indicators"
    
    print("🧪 Economic Indicators Testing Suite")
    print("=" * 50)
    
    start_time = time.time()
    results = {}
    
    # Run bronze layer tests
    if not args.silver_only:
        print("\n📊 Running Bronze Layer Tests...")
        bronze_test_file = bronze_test_dir / "test_bronze_economic_indicators.py"
        
        if bronze_test_file.exists():
            try:
                cmd = [sys.executable, str(bronze_test_file)]
                if args.verbose:
                    result = subprocess.run(cmd, cwd=bronze_test_dir, capture_output=False)
                else:
                    result = subprocess.run(cmd, cwd=bronze_test_dir, capture_output=True, text=True)
                    
                results['bronze'] = {
                    'status': 'PASSED' if result.returncode == 0 else 'FAILED',
                    'returncode': result.returncode
                }
                
                if not args.verbose:
                    if result.returncode == 0:
                        print("✅ Bronze layer tests: PASSED")
                    else:
                        print("❌ Bronze layer tests: FAILED")
                        print(f"Error output: {result.stderr}")
                        
            except Exception as e:
                results['bronze'] = {'status': 'ERROR', 'error': str(e)}
                print(f"❌ Bronze layer tests: ERROR - {e}")
        else:
            print("⚠️ Bronze layer test file not found")
            results['bronze'] = {'status': 'NOT_FOUND'}
    
    # Run silver layer tests
    if not args.bronze_only:
        print("\n🥈 Running Silver Layer Tests...")
        silver_test_file = silver_test_dir / "test_silver_economic_indicators.py"
        
        if silver_test_file.exists():
            try:
                cmd = [sys.executable, str(silver_test_file)]
                if args.verbose:
                    result = subprocess.run(cmd, cwd=silver_test_dir, capture_output=False)
                else:
                    result = subprocess.run(cmd, cwd=silver_test_dir, capture_output=True, text=True)
                    
                results['silver'] = {
                    'status': 'PASSED' if result.returncode == 0 else 'FAILED',
                    'returncode': result.returncode
                }
                
                if not args.verbose:
                    if result.returncode == 0:
                        print("✅ Silver layer tests: PASSED")
                    else:
                        print("❌ Silver layer tests: FAILED")
                        print(f"Error output: {result.stderr}")
                        
            except Exception as e:
                results['silver'] = {'status': 'ERROR', 'error': str(e)}
                print(f"❌ Silver layer tests: ERROR - {e}")
        else:
            print("⚠️ Silver layer test file not found")
            results['silver'] = {'status': 'NOT_FOUND'}
    
    # Summary
    total_time = time.time() - start_time
    print(f"\n📋 Test Summary (completed in {total_time:.2f}s)")
    print("=" * 50)
    
    for layer, result in results.items():
        status = result.get('status', 'UNKNOWN')
        print(f"{layer.capitalize()} Layer: {status}")
    
    # Overall status
    passed_tests = sum(1 for r in results.values() if r.get('status') == 'PASSED')
    total_tests = len(results)
    
    if passed_tests == total_tests and total_tests > 0:
        print("\n🎉 All economic indicators tests PASSED!")
        return 0
    elif passed_tests > 0:
        print(f"\n⚠️ {passed_tests}/{total_tests} test suites passed")
        return 1
    else:
        print("\n❌ All test suites failed or not found")
        return 2

if __name__ == "__main__":
    sys.exit(main())