#!/usr/bin/env python3
"""
Direct URL Test for Simulation Selection Issues

This script tests the exact URLs to understand why backtest simulation selection isn't working.
"""

import requests
import re
import json

def test_url(url, expected_simulation):
    """Test a URL and extract simulation data"""
    print(f"\n🔍 Testing: {url}")
    print(f"Expected simulation: {expected_simulation}")
    
    try:
        response = requests.get(url)
        response.raise_for_status()
        content = response.text
        
        # Extract selected option
        selected_match = re.search(r'<option[^>]*value="([^"]*)"[^>]*selected[^>]*>([^<]+)</option>', content)
        if selected_match:
            selected_value = selected_match.group(1)
            selected_text = selected_match.group(2)
            print(f"✅ Found selected option:")
            print(f"   Value: {selected_value}")
            print(f"   Text: {selected_text}")
            
            # Check if it matches expected
            if expected_simulation == 'Myportolio':
                if selected_value == 'Myportolio':
                    print("✅ CORRECT: Myportolio is selected")
                    return True
                else:
                    print(f"❌ WRONG: Expected Myportolio, got {selected_value}")
                    return False
            else:
                if selected_value == expected_simulation:
                    print("✅ CORRECT: Backtest is selected")
                    return True
                else:
                    print(f"❌ WRONG: Expected {expected_simulation}, got {selected_value}")
                    return False
        else:
            print("❌ NO SELECTED OPTION FOUND")
            return False
            
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        return False

def main():
    print("🚀 DIRECT URL SIMULATION SELECTION TEST")
    print("=" * 60)
    
    base_url = "http://localhost/unicorn"
    
    # Test cases
    test_cases = [
        (f"{base_url}", "Myportolio"),
        (f"{base_url}?simulation=Myportolio", "Myportolio"),
        (f"{base_url}?simulation=backtest_20250903_142955_5618caf5", "backtest_20250903_142955_5618caf5"),
        (f"{base_url}?simulation=backtest_20250903_143119_fa83c2ff", "backtest_20250903_143119_fa83c2ff"),
        (f"{base_url}?simulation=backtest_20250903_145040_bef7f054", "backtest_20250903_145040_bef7f054"),
    ]
    
    results = []
    for url, expected in test_cases:
        result = test_url(url, expected)
        results.append(result)
    
    print(f"\n📊 SUMMARY")
    print("=" * 60)
    successful_tests = sum(results)
    total_tests = len(results)
    print(f"Successful: {successful_tests}/{total_tests}")
    
    if successful_tests == total_tests:
        print("🎉 ALL TESTS PASSED!")
    else:
        print("⚠️ SOME TESTS FAILED - Check individual results above")

if __name__ == "__main__":
    main()
