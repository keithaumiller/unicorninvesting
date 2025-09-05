#!/usr/bin/env python3
"""
Simple Frontend-Backend Data Validation Test

Test connection and extract basic data from portfolio pages.
"""

import requests
import re
import json
from datetime import datetime

def test_basic_page_access():
    """Test basic access to all portfolio pages"""
    
    base_url = "http://localhost"
    session = requests.Session()
    
    pages = {
        'portfolio_overview': '/admin/metrics/lean/portfolio?portfolio=Myportolio',
        'holdings': '/admin/metrics/lean/holdings?portfolio=Myportolio',
        'performance': '/admin/metrics/lean/performance?portfolio=Myportolio',
        'algorithms': '/admin/metrics/lean/algorithms?portfolio=Myportolio',
        'algorithm_performance': '/admin/metrics/lean/algorithms/performance?portfolio=Myportolio',
        'backtest_results': '/admin/metrics/lean/backtest?portfolio=Myportolio'
    }
    
    print("🔍 BASIC PAGE ACCESS TEST")
    print("=" * 50)
    
    results = {}
    
    for page_name, url in pages.items():
        try:
            print(f"  📊 Testing {page_name}...")
            response = session.get(f"{base_url}{url}", timeout=10)
            
            if response.status_code == 200:
                content = response.text
                
                # Basic data extraction
                data_points = extract_basic_data(content, page_name)
                
                results[page_name] = {
                    'status': 'success',
                    'url': url,
                    'data_points_found': len(data_points),
                    'sample_data': data_points
                }
                
                print(f"    ✅ Status: {response.status_code}")
                print(f"    📊 Data points found: {len(data_points)}")
                
                if data_points:
                    print("    🎯 Sample data points:")
                    for key, value in list(data_points.items())[:3]:  # Show first 3
                        print(f"      • {key}: {value}")
                
            else:
                results[page_name] = {
                    'status': 'error',
                    'url': url,
                    'error': f"HTTP {response.status_code}",
                    'data_points_found': 0
                }
                print(f"    ❌ Error: HTTP {response.status_code}")
                
        except Exception as e:
            results[page_name] = {
                'status': 'error',
                'url': url,
                'error': str(e),
                'data_points_found': 0
            }
            print(f"    ❌ Error: {str(e)}")
    
    return results

def extract_basic_data(content, page_type):
    """Extract basic data points from page content"""
    
    data_points = {}
    
    # Look for common patterns
    
    # Portfolio value patterns
    value_matches = re.findall(r'<div class="metric-value">\$([0-9,]+\.[0-9]{2})</div>', content)
    for i, value in enumerate(value_matches):
        data_points[f'metric_value_{i}'] = f'${value}'
    
    # Percentage patterns
    percent_matches = re.findall(r'([0-9.]+)%', content)
    for i, percent in enumerate(percent_matches[:5]):  # Limit to 5
        data_points[f'percentage_{i}'] = f'{percent}%'
    
    # Asset symbols
    symbol_matches = re.findall(r'<h4>([A-Z]{3,4})</h4>', content)
    for i, symbol in enumerate(symbol_matches):
        data_points[f'asset_symbol_{i}'] = symbol
    
    # Date patterns
    date_matches = re.findall(r'(\d{4}-\d{2}-\d{2})', content)
    for i, date in enumerate(date_matches[:3]):  # Limit to 3
        data_points[f'date_{i}'] = date
    
    # Numeric values (general)
    numeric_matches = re.findall(r'>([0-9,]+\.[0-9]{2})<', content)
    for i, num in enumerate(numeric_matches[:5]):  # Limit to 5
        data_points[f'numeric_{i}'] = num
    
    # Status indicators
    status_matches = re.findall(r'<span class="status[^"]*">([^<]+)</span>', content)
    for i, status in enumerate(status_matches):
        data_points[f'status_{i}'] = status.strip()
    
    # Simulation parameters
    if 'Simulation Parameters' in content:
        sim_params = re.findall(r'<span class="param-label">([^:]+):</span>\s*<span class="param-value">([^<]+)</span>', content)
        for label, value in sim_params:
            clean_label = re.sub(r'[^\w\s]', '', label.strip()).lower().replace(' ', '_')
            data_points[f'sim_{clean_label}'] = value.strip()
    
    return data_points

def test_backend_data_access():
    """Test access to backend data files"""
    
    import os
    import glob
    
    print("\n🗂️  BACKEND DATA ACCESS TEST")
    print("=" * 50)
    
    backend_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
    
    backend_files = {}
    
    # Check for configuration files
    config_file = f"{backend_path}/config.json"
    if os.path.exists(config_file):
        try:
            with open(config_file, 'r') as f:
                config_data = json.load(f)
            backend_files['config'] = {
                'status': 'found',
                'keys': list(config_data.keys()),
                'sample_data': {k: v for k, v in list(config_data.items())[:3]}
            }
            print(f"  ✅ config.json: {len(config_data)} keys")
        except Exception as e:
            backend_files['config'] = {'status': 'error', 'error': str(e)}
            print(f"  ❌ config.json: Error - {str(e)}")
    else:
        backend_files['config'] = {'status': 'not_found'}
        print(f"  ❌ config.json: Not found")
    
    # Check for risk parameters
    risk_file = f"{backend_path}/risk_parameters.json"
    if os.path.exists(risk_file):
        try:
            with open(risk_file, 'r') as f:
                risk_data = json.load(f)
            backend_files['risk_parameters'] = {
                'status': 'found',
                'keys': list(risk_data.keys()),
                'sample_data': {k: v for k, v in list(risk_data.items())[:3]}
            }
            print(f"  ✅ risk_parameters.json: {len(risk_data)} keys")
        except Exception as e:
            backend_files['risk_parameters'] = {'status': 'error', 'error': str(e)}
            print(f"  ❌ risk_parameters.json: Error - {str(e)}")
    else:
        backend_files['risk_parameters'] = {'status': 'not_found'}
        print(f"  ❌ risk_parameters.json: Not found")
    
    # Check for status reports
    status_files = glob.glob(f"{backend_path}/status_report_*.json")
    if status_files:
        latest_status = max(status_files, key=os.path.getctime)
        try:
            with open(latest_status, 'r') as f:
                status_data = json.load(f)
            backend_files['status_report'] = {
                'status': 'found',
                'file': os.path.basename(latest_status),
                'keys': list(status_data.keys()),
                'sample_data': {k: v for k, v in list(status_data.items())[:3]}
            }
            print(f"  ✅ status_report: Latest - {os.path.basename(latest_status)}")
        except Exception as e:
            backend_files['status_report'] = {'status': 'error', 'error': str(e)}
            print(f"  ❌ status_report: Error - {str(e)}")
    else:
        backend_files['status_report'] = {'status': 'not_found'}
        print(f"  ❌ status_report: No files found")
    
    # Check for simulation data
    sim_path = f"{backend_path}/simulations/backtests"
    if os.path.exists(sim_path):
        simulations = [d for d in os.listdir(sim_path) if os.path.isdir(f"{sim_path}/{d}")]
        backend_files['simulations'] = {
            'status': 'found',
            'count': len(simulations),
            'simulations': simulations[:5]  # Show first 5
        }
        print(f"  ✅ simulations: {len(simulations)} found")
    else:
        backend_files['simulations'] = {'status': 'not_found'}
        print(f"  ❌ simulations: Directory not found")
    
    return backend_files

def main():
    """Run basic validation tests"""
    
    print("🚀 SIMPLE FRONTEND-BACKEND VALIDATION TEST")
    print("=" * 70)
    print(f"Test started at: {datetime.now().isoformat()}")
    
    # Test frontend page access
    frontend_results = test_basic_page_access()
    
    # Test backend data access
    backend_results = test_backend_data_access()
    
    # Summary
    print("\n📊 VALIDATION SUMMARY")
    print("=" * 50)
    
    total_pages = len(frontend_results)
    successful_pages = len([r for r in frontend_results.values() if r['status'] == 'success'])
    total_data_points = sum(r.get('data_points_found', 0) for r in frontend_results.values())
    
    print(f"  📄 Pages tested: {total_pages}")
    print(f"  ✅ Pages accessible: {successful_pages}")
    print(f"  📊 Total data points found: {total_data_points}")
    print(f"  📁 Backend files accessible: {len([f for f in backend_results.values() if f.get('status') == 'found'])}")
    
    # Overall status
    success_rate = (successful_pages / total_pages) * 100 if total_pages > 0 else 0
    
    if success_rate >= 80 and total_data_points > 20:
        print(f"\n🎉 BASIC VALIDATION SUCCESSFUL!")
        print(f"   Success Rate: {success_rate:.1f}%")
        print(f"   Data Points: {total_data_points}")
        print(f"   Ready for comprehensive validation!")
    else:
        print(f"\n⚠️  BASIC VALIDATION NEEDS IMPROVEMENT")
        print(f"   Success Rate: {success_rate:.1f}% (target: >80%)")
        print(f"   Data Points: {total_data_points} (target: >20)")
    
    # Save results
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    results_file = f"basic_validation_results_{timestamp}.json"
    
    with open(results_file, 'w') as f:
        json.dump({
            'timestamp': datetime.now().isoformat(),
            'frontend_results': frontend_results,
            'backend_results': backend_results,
            'summary': {
                'total_pages': total_pages,
                'successful_pages': successful_pages,
                'success_rate': success_rate,
                'total_data_points': total_data_points
            }
        }, f, indent=2)
    
    print(f"   💾 Results saved: {results_file}")
    
    return frontend_results, backend_results

if __name__ == "__main__":
    main()
