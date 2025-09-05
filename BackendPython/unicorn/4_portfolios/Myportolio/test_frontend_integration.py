#!/usr/bin/env python3
"""
Test script to verify that our frontend can read the Myportolio backend data correctly.
This validates the integration between the Drupal frontend and Python backend.
"""

import json
import sys
import os
from pathlib import Path
from glob import glob
from datetime import datetime

# Add the utilities path to Python path
sys.path.insert(0, '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/utilities')

def test_backend_data_access():
    """Test accessing all the data sources that the frontend should read."""
    
    portfolio_dir = Path('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
    results = {
        'timestamp': datetime.now().isoformat(),
        'tests': {},
        'summary': {'passed': 0, 'failed': 0, 'warnings': 0}
    }
    
    # Test 1: Portfolio Configuration
    try:
        config_file = portfolio_dir / 'config.json'
        with open(config_file) as f:
            config_data = json.load(f)
        
        results['tests']['portfolio_config'] = {
            'status': 'PASS',
            'data': {
                'portfolio_name': config_data.get('portfolio_name'),
                'assets': list(config_data.get('assets', {}).keys()),
                'asset_count': len(config_data.get('assets', {})),
                'target_volatility': config_data.get('target_volatility'),
                'strategy_type': config_data.get('strategy_type')
            }
        }
        results['summary']['passed'] += 1
    except Exception as e:
        results['tests']['portfolio_config'] = {'status': 'FAIL', 'error': str(e)}
        results['summary']['failed'] += 1
    
    # Test 2: Risk Parameters
    try:
        risk_params_file = portfolio_dir / 'risk_parameters.json'
        with open(risk_params_file) as f:
            risk_data = json.load(f)
        
        results['tests']['risk_parameters'] = {
            'status': 'PASS',
            'data': {
                'risk_profile': risk_data.get('risk_profile'),
                'max_portfolio_volatility': risk_data.get('max_portfolio_volatility'),
                'max_drawdown': risk_data.get('max_drawdown'),
                'sharpe_ratio_target': risk_data.get('sharpe_ratio_target')
            }
        }
        results['summary']['passed'] += 1
    except Exception as e:
        results['tests']['risk_parameters'] = {'status': 'FAIL', 'error': str(e)}
        results['summary']['failed'] += 1
    
    # Test 3: Latest Status Report
    try:
        status_reports = sorted(glob(str(portfolio_dir / 'status_report_*.json')), reverse=True)
        if status_reports:
            with open(status_reports[0]) as f:
                status_data = json.load(f)
            
            results['tests']['latest_status_report'] = {
                'status': 'PASS',
                'data': {
                    'file': os.path.basename(status_reports[0]),
                    'timestamp': status_data.get('timestamp'),
                    'overall_readiness': status_data.get('overall_readiness'),
                    'critical_issues': len(status_data.get('critical_issues', [])),
                    'warnings': len(status_data.get('warnings', [])),
                    'passed_checks': len(status_data.get('passed_checks', []))
                }
            }
            results['summary']['passed'] += 1
        else:
            results['tests']['latest_status_report'] = {'status': 'WARN', 'message': 'No status reports found'}
            results['summary']['warnings'] += 1
    except Exception as e:
        results['tests']['latest_status_report'] = {'status': 'FAIL', 'error': str(e)}
        results['summary']['failed'] += 1
    
    # Test 4: Latest Risk Report
    try:
        risk_reports = sorted(glob(str(portfolio_dir / 'risk_report_*.json')), reverse=True)
        if risk_reports:
            with open(risk_reports[0]) as f:
                risk_report_data = json.load(f)
            
            results['tests']['latest_risk_report'] = {
                'status': 'PASS',
                'data': {
                    'file': os.path.basename(risk_reports[0]),
                    'timestamp': risk_report_data.get('timestamp'),
                    'risk_metrics': risk_report_data.get('risk_metrics', {}),
                    'risk_violations': len(risk_report_data.get('risk_violations', []))
                }
            }
            results['summary']['passed'] += 1
        else:
            results['tests']['latest_risk_report'] = {'status': 'WARN', 'message': 'No risk reports found'}
            results['summary']['warnings'] += 1
    except Exception as e:
        results['tests']['latest_risk_report'] = {'status': 'FAIL', 'error': str(e)}
        results['summary']['failed'] += 1
    
    # Test 5: Algorithm Directories
    try:
        risk_algos_dir = portfolio_dir / 'risk_algorithms'
        trading_algos_dir = portfolio_dir / 'trading_algorithms'
        
        risk_algorithms = [f.stem for f in risk_algos_dir.glob('**/*.py') if f.is_file()]
        trading_algorithms = [f.stem for f in trading_algos_dir.glob('**/*.py') if f.is_file()]
        
        results['tests']['algorithm_files'] = {
            'status': 'PASS',
            'data': {
                'risk_algorithms': {
                    'count': len(risk_algorithms),
                    'algorithms': risk_algorithms
                },
                'trading_algorithms': {
                    'count': len(trading_algorithms),
                    'algorithms': trading_algorithms
                }
            }
        }
        results['summary']['passed'] += 1
    except Exception as e:
        results['tests']['algorithm_files'] = {'status': 'FAIL', 'error': str(e)}
        results['summary']['failed'] += 1
    
    # Test 6: Integration Files
    try:
        integration_files = {
            'eth_kelly_integration': (portfolio_dir / 'eth_kelly_integration.py').exists(),
            'eth_algorithm_integration': (portfolio_dir / 'eth_algorithm_integration.py').exists(),
            'eth_kelly_config': (portfolio_dir / 'config' / 'eth_kelly_config.json').exists()
        }
        
        results['tests']['integration_files'] = {
            'status': 'PASS',
            'data': integration_files
        }
        results['summary']['passed'] += 1
    except Exception as e:
        results['tests']['integration_files'] = {'status': 'FAIL', 'error': str(e)}
        results['summary']['failed'] += 1
    
    return results

if __name__ == '__main__':
    print("🦄 Testing Backend Data Access for Drupal Frontend Integration")
    print("=" * 70)
    
    results = test_backend_data_access()
    
    # Print results
    for test_name, test_result in results['tests'].items():
        status_icon = {'PASS': '✅', 'FAIL': '❌', 'WARN': '⚠️'}.get(test_result['status'], '❓')
        print(f"\n{status_icon} {test_name.replace('_', ' ').title()}: {test_result['status']}")
        
        if 'data' in test_result:
            for key, value in test_result['data'].items():
                if isinstance(value, dict) and 'count' in value:
                    print(f"   📊 {key}: {value['count']} items")
                    if 'algorithms' in value and len(value['algorithms']) <= 5:
                        print(f"      📋 {', '.join(value['algorithms'])}")
                elif isinstance(value, list):
                    print(f"   📋 {key}: {', '.join(map(str, value))}")
                else:
                    print(f"   📊 {key}: {value}")
        
        if 'error' in test_result:
            print(f"   🚨 Error: {test_result['error']}")
        
        if 'message' in test_result:
            print(f"   💬 {test_result['message']}")
    
    print("\n" + "=" * 70)
    summary = results['summary']
    total_tests = summary['passed'] + summary['failed'] + summary['warnings']
    success_rate = (summary['passed'] / total_tests * 100) if total_tests > 0 else 0
    
    print(f"📈 Test Summary: {summary['passed']}/{total_tests} passed ({success_rate:.1f}% success rate)")
    if summary['failed'] > 0:
        print(f"❌ Failed: {summary['failed']}")
    if summary['warnings'] > 0:
        print(f"⚠️  Warnings: {summary['warnings']}")
    
    print(f"✅ Backend data integration is {'READY' if summary['failed'] == 0 else 'NEEDS_ATTENTION'}")
    
    # Save results to file for reference
    portfolio_dir = Path('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
    results_file = portfolio_dir / f'frontend_integration_test_{datetime.now().strftime("%Y%m%d_%H%M%S")}.json'
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"📁 Detailed results saved to: {results_file}")
