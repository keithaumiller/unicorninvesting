#!/usr/bin/env python3
"""
Comprehensive test suite for LEAN Simulation Management pages
Tests all simulation routes and functionality
"""

import requests
import json
import re
from datetime import datetime
import sys

def test_simulation_pages():
    """Test all simulation management pages and functionality."""
    
    base_url = "http://localhost"
    timestamp = datetime.now().isoformat()
    
    print("🧪 LEAN Simulation Management Test Suite")
    print("=" * 50)
    print(f"Timestamp: {timestamp}")
    print(f"Target URL: {base_url}")
    
    test_results = {
        'timestamp': timestamp,
        'base_url': base_url,
        'tests': {},
        'summary': {}
    }
    
    # Test cases for simulation management
    simulation_tests = [
        {
            'name': 'simulation_selector_page',
            'url': '/admin/metrics/lean/simulations',
            'title': 'Simulation Management',
            'required_elements': [
                'Simulation Selector',
                'Available Simulations',
                'simulation-card',
                'ETH_Momentum_2024Q4',
                'BTC_Conservative_2024Q3',
                'Mixed_Portfolio_2024Q2',
                'Total Simulations',
                'Average Return',
                'navigateToSimulation',
                'compareSimulations'
            ]
        },
        {
            'name': 'simulation_holdings_page',
            'url': '/admin/metrics/lean/simulations/ETH_Momentum_2024Q4/holdings',
            'title': 'Simulation Holdings',
            'required_elements': [
                'Holdings Analysis',
                'holdings-table',
                'ETHUSD',
                'BTCUSD',
                'Market Value',
                'Unrealized P&L',
                'Algorithm',
                'Performance Analysis',
                'Algorithm Details',
                'Backtest Results'
            ]
        },
        {
            'name': 'simulation_performance_page',
            'url': '/admin/metrics/lean/simulations/ETH_Momentum_2024Q4/performance',
            'title': 'Simulation Performance',
            'required_elements': [
                'Performance Analysis',
                'Total Return',
                '+24.3%',
                'Sharpe Ratio',
                '1.85',
                'Max Drawdown',
                'Win Rate',
                '73.4%',
                '90-day simulation',
                'Holdings Analysis'
            ]
        },
        {
            'name': 'simulation_algorithms_page',
            'url': '/admin/metrics/lean/simulations/ETH_Momentum_2024Q4/algorithms',
            'title': 'Simulation Algorithms',
            'required_elements': [
                'Algorithm Analysis',
                'ETH Momentum Algorithm v3.2',
                'BTC Conservative Algorithm v2.1',
                'Accuracy: <strong>87.3%</strong>',
                'Return: <strong>+26.1%</strong>',
                'Trades: <strong>23</strong>',
                'Conservative approach',
                'momentum strategy'
            ]
        },
        {
            'name': 'simulation_backtest_page',
            'url': '/admin/metrics/lean/simulations/ETH_Momentum_2024Q4/backtest',
            'title': 'Simulation Backtest',
            'required_elements': [
                'Backtest Results',
                'Simulation Parameters',
                'Start Date: <strong>2024-07-01</strong>',
                'End Date: <strong>2024-09-30</strong>',
                'Initial Capital: <strong>$10,000</strong>',
                'Final Value: <strong>$12,430</strong>',
                'Total Trades: <strong>35</strong>',
                'Winning Trades: <strong>25',
                'Trade Analysis'
            ]
        }
    ]
    
    total_tests = len(simulation_tests)
    passed_tests = 0
    
    # Run all simulation tests
    for test_case in simulation_tests:
        print(f"\n🔍 Running {test_case['name']}...")
        
        try:
            # Test page accessibility
            response = requests.get(f"{base_url}{test_case['url']}", timeout=10)
            
            test_result = {
                'url': test_case['url'],
                'status_code': response.status_code,
                'response_time': response.elapsed.total_seconds(),
                'content_length': len(response.content),
                'elements_found': [],
                'elements_missing': [],
                'success': True,
                'error_message': None
            }
            
            if response.status_code == 200:
                content = response.text.lower()
                
                # Check for required elements
                for element in test_case['required_elements']:
                    if element.lower() in content:
                        test_result['elements_found'].append(element)
                        print(f"✅ Found: {element}")
                    else:
                        test_result['elements_missing'].append(element)
                        print(f"❌ Missing: {element}")
                
                # Calculate success rate
                found_count = len(test_result['elements_found'])
                total_count = len(test_case['required_elements'])
                success_rate = (found_count / total_count) * 100
                
                print(f"📊 Elements found: {found_count}/{total_count} ({success_rate:.1f}%)")
                
                if success_rate >= 70:  # 70% threshold for passing
                    print(f"✅ {test_case['name']} PASSED!")
                    passed_tests += 1
                else:
                    print(f"❌ {test_case['name']} failed: Low element detection rate")
                    test_result['success'] = False
                    test_result['error_message'] = f"Only {success_rate:.1f}% elements found"
                
                test_result['success_rate'] = success_rate
                
            else:
                print(f"❌ {test_case['name']} failed: HTTP {response.status_code}")
                test_result['success'] = False
                test_result['error_message'] = f"HTTP {response.status_code}"
        
        except requests.RequestException as e:
            print(f"❌ {test_case['name']} failed: {str(e)}")
            test_result = {
                'url': test_case['url'],
                'success': False,
                'error_message': str(e)
            }
        
        test_results['tests'][test_case['name']] = test_result
    
    # Test navigation integration
    print(f"\n🧭 Testing Navigation Integration...")
    
    try:
        # Check main admin dashboard for simulation link
        response = requests.get(f"{base_url}/admin/metrics", timeout=10)
        
        navigation_test = {
            'admin_dashboard_accessible': response.status_code == 200 or response.status_code == 403,  # 403 is OK (auth required)
            'simulation_menu_integration': False
        }
        
        if response.status_code == 200:
            content = response.text.lower()
            if 'simulation' in content:
                navigation_test['simulation_menu_integration'] = True
                print("✅ Simulation menu integration found")
            else:
                print("⚠️ Simulation menu integration not detected")
        else:
            print(f"⚠️ Admin dashboard returned {response.status_code} (auth required)")
        
        test_results['navigation_integration'] = navigation_test
        
    except requests.RequestException as e:
        print(f"❌ Navigation test failed: {str(e)}")
        test_results['navigation_integration'] = {'error': str(e)}
    
    # Calculate final results
    success_rate = (passed_tests / total_tests) * 100
    test_results['summary'] = {
        'total_tests': total_tests,
        'passed_tests': passed_tests,
        'failed_tests': total_tests - passed_tests,
        'success_rate': success_rate
    }
    
    print(f"\n📊 LEAN Simulation Test Results")
    print(f"Total Tests: {total_tests}")
    print(f"Passed: {passed_tests}")
    print(f"Failed: {total_tests - passed_tests}")
    print(f"Success Rate: {success_rate:.1f}%")
    
    if success_rate >= 70:
        print("🎉 LEAN Simulation testing SUCCESSFUL!")
        result_code = 0
    else:
        print("⚠️ LEAN Simulation testing needs attention")
        result_code = 1
    
    # Save test results
    results_dir = "/workspaces/unicorninvesting/tests/WebFrontend/test_results"
    import os
    os.makedirs(results_dir, exist_ok=True)
    results_filename = f"{results_dir}/simulation_test_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    
    try:
        with open(results_filename, 'w') as f:
            json.dump(test_results, f, indent=2, default=str)
        print(f"\n💾 Test results saved to: {results_filename}")
    except Exception as e:
        print(f"⚠️ Could not save results: {e}")
    
    return result_code

if __name__ == "__main__":
    exit_code = test_simulation_pages()
    sys.exit(exit_code)
