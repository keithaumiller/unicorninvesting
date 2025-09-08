#!/usr/bin/env python3
"""
Simple Homepage Test

Basic homepage functionality test that validates the restored dashboard
with live IBKR integration and original portfolio management features.
"""

import requests
import json
import time
import sys
from datetime import datetime

def test_homepage_basic():
    """Basic homepage accessibility and functionality test."""
    
    print("🏠 Testing Homepage Basic Functionality")
    print("-" * 40)
    
    try:
        # Test homepage accessibility
        start_time = time.time()
        response = requests.get("http://localhost", timeout=10)
        load_time = time.time() - start_time
        
        print(f"Status Code: {response.status_code}")
        print(f"Content Length: {len(response.content)} bytes")
        print(f"Load Time: {load_time:.2f} seconds")
        print(f"Content Type: {response.headers.get('content-type', 'Unknown')}")
        print(f"X-Generator: {response.headers.get('x-generator', 'Unknown')}")
        
        # Basic validation
        assert response.status_code == 200, f"Expected 200, got {response.status_code}"
        assert len(response.content) > 1000, "Content too small"
        assert load_time < 5.0, f"Load time too slow: {load_time:.2f}s"
        
        content = response.text
        
        # Test restored dashboard functionality
        dashboard_elements = {
            'Unicorn Investing Platform': 'Main header',
            'Myportolio': 'Portfolio name', 
            'Live Portfolio Status': 'IBKR integration',
            'Account Summary': 'Account information',
            'Net Liquidation Value': 'Financial data',
            'Last Updated': 'Data freshness',
            'System Status': 'Component status',
        }
        
        found_elements = {}
        for element, description in dashboard_elements.items():
            if element in content:
                found_elements[element] = description
                print(f"✅ Found: {element} ({description})")
            else:
                print(f"❌ Missing: {element} ({description})")
        
        # Test navigation functionality
        navigation_elements = [
            'Holdings',
            'Performance', 
            'Algorithms',
            'simulation-selector',
            'portfolio-selector'
        ]
        
        found_navigation = []
        for nav_element in navigation_elements:
            if nav_element in content:
                found_navigation.append(nav_element)
                print(f"🔗 Navigation found: {nav_element}")
        
        # Results summary
        print(f"\n📊 Test Results:")
        print(f"Dashboard Elements: {len(found_elements)}/{len(dashboard_elements)} found")
        print(f"Navigation Elements: {len(found_navigation)}/{len(navigation_elements)} found")
        
        success_rate = (len(found_elements) + len(found_navigation)) / (len(dashboard_elements) + len(navigation_elements)) * 100
        print(f"Overall Success Rate: {success_rate:.1f}%")
        
        if success_rate >= 70:
            print("🎉 Homepage test PASSED!")
            return True
        else:
            print("⚠️ Homepage test needs attention")
            return False
            
    except requests.RequestException as e:
        print(f"❌ Homepage test FAILED: {e}")
        return False
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        return False

def test_dashboard_restoration():
    """Test that dashboard restoration preserved both IBKR and original features."""
    
    print("\n🔄 Testing Dashboard Restoration")
    print("-" * 40)
    
    try:
        response = requests.get("http://localhost", timeout=10)
        content = response.text
        
        # Test live IBKR integration
        ibkr_features = [
            'Live Portfolio Status',
            'Cash Balance',
            'Market Value', 
            'Unrealized P&L',
            'Data Freshness',
        ]
        
        found_ibkr = 0
        for feature in ibkr_features:
            if feature in content:
                found_ibkr += 1
                print(f"💼 IBKR feature found: {feature}")
        
        # Test original portfolio management features  
        original_features = [
            'simulation-selector',
            'Select Simulation',
            'Available Simulations',
            'Holdings',
            'Performance',
            'Algorithms',
        ]
        
        found_original = 0
        for feature in original_features:
            if feature in content:
                found_original += 1
                print(f"🎛️ Original feature found: {feature}")
        
        print(f"\n📋 Dashboard Restoration Results:")
        print(f"IBKR Integration: {found_ibkr}/{len(ibkr_features)} features ({found_ibkr/len(ibkr_features)*100:.1f}%)")
        print(f"Original Features: {found_original}/{len(original_features)} features ({found_original/len(original_features)*100:.1f}%)")
        
        # Both integrations should be working
        if found_ibkr >= 3 and found_original >= 3:
            print("✅ Dashboard restoration SUCCESSFUL - Both IBKR and original features present!")
            return True
        elif found_ibkr >= 3:
            print("⚠️ IBKR integration working, but original features incomplete")
            return False
        elif found_original >= 3:
            print("⚠️ Original features working, but IBKR integration incomplete")
            return False
        else:
            print("❌ Dashboard restoration FAILED - Both integrations incomplete")
            return False
            
    except Exception as e:
        print(f"❌ Dashboard restoration test failed: {e}")
        return False

def main():
    """Run all simple homepage tests."""
    
    print("🧪 Simple Homepage Test Suite")
    print("=" * 50)
    print(f"Timestamp: {datetime.now().isoformat()}")
    print(f"Target URL: http://localhost")
    
    test_results = []
    
    # Run tests
    test_results.append(("homepage_basic", test_homepage_basic()))
    test_results.append(("dashboard_restoration", test_dashboard_restoration()))
    
    # Summary
    passed_tests = sum(1 for _, result in test_results if result)
    total_tests = len(test_results)
    
    print(f"\n🎯 Final Results:")
    print(f"Tests Passed: {passed_tests}/{total_tests}")
    print(f"Success Rate: {passed_tests/total_tests*100:.1f}%")
    
    for test_name, result in test_results:
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"  {test_name}: {status}")
    
    # Save results to JSON file in test_results directory
    results_dir = "/workspaces/unicorninvesting/tests/WebFrontend/test_results"
    import os
    os.makedirs(results_dir, exist_ok=True)
    results_file = f"{results_dir}/simple_homepage_test_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
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
    print(f"\n💾 Test results saved to: {results_file}")

    if passed_tests == total_tests:
        print("\n🎉 All tests PASSED! Homepage is fully functional.")
        return 0
    else:
        print(f"\n⚠️ {total_tests - passed_tests} test(s) failed. Review results above.")
        return 1

if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
