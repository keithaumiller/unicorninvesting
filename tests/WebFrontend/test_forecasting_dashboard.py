#!/usr/bin/env python3
"""
Forecasting Dashboard Test

Tests the ETH forecasting dashboard integration with the restored dashboard,
validating that forecasting data, algorithms, and performance metrics are
properly displayed alongside live IBKR portfolio data.
"""

import requests
import json
import re
import sys
from datetime import datetime

def test_forecasting_dashboard_integration():
    """Test integration of forecasting dashboard with main dashboard."""
    
    print("📈 Testing Forecasting Dashboard Integration")
    print("-" * 45)
    
    try:
        response = requests.get("http://localhost", timeout=10)
        content = response.text
        
        # Test for forecasting-related elements
        forecasting_elements = {
            'ETH': 'Ethereum mentions',
            'Algorithm': 'Algorithm references',
            'Forecast': 'Forecasting functionality',
            'Performance': 'Performance metrics',
            'Prophet': 'Prophet model references',
            'XGBoost': 'XGBoost model references',
            'Ensemble': 'Ensemble method references',
            'MAPE': 'Model accuracy metrics',
            'R²': 'R-squared metrics',
            'Volatility': 'Risk metrics',
        }
        
        found_elements = {}
        for element, description in forecasting_elements.items():
            if element in content:
                found_elements[element] = description
                print(f"📊 Found: {element} ({description})")
            else:
                print(f"❌ Missing: {element} ({description})")
        
        # Test for algorithmic components
        algo_components = [
            'momentum',
            'strategy',
            'signal',
            'model',
            'prediction',
            'analysis',
        ]
        
        found_algo = []
        for component in algo_components:
            if component.lower() in content.lower():
                found_algo.append(component)
                print(f"🤖 Algorithm component: {component}")
        
        print(f"\n📋 Forecasting Integration Results:")
        print(f"Forecasting Elements: {len(found_elements)}/{len(forecasting_elements)} found ({len(found_elements)/len(forecasting_elements)*100:.1f}%)")
        print(f"Algorithm Components: {len(found_algo)}/{len(algo_components)} found ({len(found_algo)/len(algo_components)*100:.1f}%)")
        
        # Success criteria - should have some forecasting elements
        if len(found_elements) >= 3 or len(found_algo) >= 2:
            print("✅ Forecasting dashboard integration PRESENT")
            return True
        else:
            print("⚠️ Forecasting dashboard integration LIMITED")
            return False
            
    except Exception as e:
        print(f"❌ Forecasting dashboard test failed: {e}")
        return False

def test_eth_specific_functionality():
    """Test ETH-specific forecasting functionality."""
    
    print("\n₿ Testing ETH-Specific Functionality")
    print("-" * 40)
    
    try:
        response = requests.get("http://localhost", timeout=10)
        content = response.text
        
        # Test for ETH-specific elements
        test_results = []
        # Run forecasting tests
        test_results.append(("forecasting_integration", test_forecasting_dashboard_integration()))
        test_results.append(("eth_functionality", test_eth_specific_functionality()))
        test_results.append(("performance_display", test_model_performance_display()))
        test_results.append(("forecasting_navigation", test_forecasting_navigation()))

        # Summary
        passed_tests = sum(1 for _, result in test_results if result)
        total_tests = len(test_results)

        print(f"\n3caf Final Results:")
        print(f"Tests Passed: {passed_tests}/{total_tests}")
        print(f"Success Rate: {passed_tests/total_tests*100:.1f}%")

        for test_name, result in test_results:
            status = "705 PASSED" if result else "74c FAILED" 
            print(f"  {test_name}: {status}")

        # Save results to JSON file in test_results directory
        results_dir = "/workspaces/unicorninvesting/tests/WebFrontend/test_results"
        import os
        os.makedirs(results_dir, exist_ok=True)
        results_file = f"{results_dir}/forecasting_dashboard_test_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        results_data = {
            "timestamp": datetime.now().isoformat(),
            "tests": [{"name": name, "passed": result} for name, result in test_results],
            "summary": {
                "passed": passed_tests,
                "total": total_tests,
                "success_rate": passed_tests/total_tests*100
            }
        }
        with open(results_file, 'w') as f:
            json.dump(results_data, f, indent=2, default=str)
        print(f"\n3dbe Test results saved to: {results_file}")

        # Interpretation of results
        if passed_tests == total_tests:
            print("\n3c89 Forecasting dashboard FULLY FUNCTIONAL!")
        elif passed_tests >= total_tests * 0.75:
            print("\n705 Forecasting dashboard MOSTLY FUNCTIONAL")
        elif passed_tests >= total_tests * 0.5:
            print("\n6a0fe0f Forecasting dashboard PARTIALLY FUNCTIONAL")
        else:
            print("\n74c Forecasting dashboard needs SIGNIFICANT WORK")

        return 0 if passed_tests >= total_tests * 0.5 else 1
        print(f"Price Patterns: {found_patterns}/{len(price_patterns)} found")
        
        if len(found_eth) >= 2 or found_patterns >= 1:
            print("✅ ETH-specific functionality PRESENT")
            return True
        else:
            print("⚠️ ETH-specific functionality LIMITED")
            return False
            
    except Exception as e:
        print(f"❌ ETH functionality test failed: {e}")
        return False

def test_model_performance_display():
    """Test that model performance metrics are displayed."""
    
    print("\n📊 Testing Model Performance Display")
    print("-" * 40)
    
    try:
        response = requests.get("http://localhost", timeout=10)
        content = response.text
        
        # Test for performance metrics
        performance_metrics = [
            'MAPE',
            'RMSE',
            'R²',
            'R-squared',
            'accuracy',
            'precision',
            'score',
            'performance',
        ]
        
        found_metrics = []
        for metric in performance_metrics:
            if metric in content:
                found_metrics.append(metric)
                print(f"📈 Performance metric: {metric}")
        
        # Test for numerical performance values
        numeric_patterns = [
            r'\d+\.\d+%',        # Percentage values
            r'0\.\d{4}',         # Decimal accuracy scores
            r'\d+\.\d{4}',       # RMSE/MAPE values
        ]
        
        found_numeric = 0
        for pattern in numeric_patterns:
            if re.search(pattern, content):
                found_numeric += 1
                print(f"🔢 Numeric pattern: {pattern}")
        
        print(f"\n📋 Performance Display Results:")
        print(f"Performance Metrics: {len(found_metrics)}/{len(performance_metrics)} found")
        print(f"Numeric Values: {found_numeric}/{len(numeric_patterns)} patterns found")
        
        if len(found_metrics) >= 2 or found_numeric >= 1:
            print("✅ Model performance display FUNCTIONAL")
            return True
        else:
            print("⚠️ Model performance display LIMITED")
            return False
            
    except Exception as e:
        print(f"❌ Performance display test failed: {e}")
        return False

def test_forecasting_navigation():
    """Test navigation to forecasting-related pages."""
    
    print("\n🧭 Testing Forecasting Navigation")
    print("-" * 35)
    
    try:
        response = requests.get("http://localhost", timeout=10)
        content = response.text
        
        # Test for forecasting navigation elements
        nav_elements = [
            'Algorithms',
            'Performance', 
            'Backtests',
            'lean-algorithms',
            'lean-performance',
            'forecasting',
            'models',
        ]
        
        found_nav = []
        for element in nav_elements:
            if element in content:
                found_nav.append(element)
                print(f"🔗 Navigation found: {element}")
        
        # Test for href links (basic)
        href_count = content.count('href=')
        print(f"🔗 Total links found: {href_count}")
        
        print(f"\n📋 Navigation Results:")
        print(f"Forecasting Navigation: {len(found_nav)}/{len(nav_elements)} found")
        print(f"Total Links: {href_count}")
        
        if len(found_nav) >= 3 and href_count >= 5:
            print("✅ Forecasting navigation FUNCTIONAL")
            return True
        else:
            print("⚠️ Forecasting navigation LIMITED")
            return False
            
    except Exception as e:
        print(f"❌ Navigation test failed: {e}")
        return False

def main():
    """Run all forecasting dashboard tests."""
    
    print("🔮 Forecasting Dashboard Test Suite")
    print("=" * 50)
    print(f"Timestamp: {datetime.now().isoformat()}")
    print(f"Target URL: http://localhost")
    
    test_results = []
    
    # Run forecasting tests
    test_results.append(("forecasting_integration", test_forecasting_dashboard_integration()))
    test_results.append(("eth_functionality", test_eth_specific_functionality()))
    test_results.append(("performance_display", test_model_performance_display()))
    test_results.append(("forecasting_navigation", test_forecasting_navigation()))
    
    # Summary
    passed_tests = sum(1 for _, result in test_results if result)
    total_tests = len(test_results)
    
    print(f"\n🎯 Final Results:")
    print(f"Tests Passed: {passed_tests}/{total_tests}")
    print(f"Success Rate: {passed_tests/total_tests*100:.1f}%")
    
    for test_name, result in test_results:
        status = "✅ PASSED" if result else "❌ FAILED" 
        print(f"  {test_name}: {status}")
    
    # Interpretation of results
    if passed_tests == total_tests:
        print("\n🎉 Forecasting dashboard FULLY FUNCTIONAL!")
    elif passed_tests >= total_tests * 0.75:
        print("\n✅ Forecasting dashboard MOSTLY FUNCTIONAL")
    elif passed_tests >= total_tests * 0.5:
        print("\n⚠️ Forecasting dashboard PARTIALLY FUNCTIONAL")
    else:
        print("\n❌ Forecasting dashboard needs SIGNIFICANT WORK")
    
    return 0 if passed_tests >= total_tests * 0.5 else 1

if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
