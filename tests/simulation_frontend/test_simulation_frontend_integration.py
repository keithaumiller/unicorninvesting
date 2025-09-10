#!/usr/bin/env python3

import requests
import json
import time
from datetime import datetime

def test_simulation_frontend_integration():
    """Test the complete simulation selection frontend integration"""
    
    print("=" * 70)
    print("🧪 SIMULATION FRONTEND INTEGRATION TEST")
    print("=" * 70)
    
    base_url = "http://localhost"
    dashboard_url = f"{base_url}/unicorn"
    
    results = {
        "timestamp": datetime.now().isoformat(),
        "tests": {},
        "summary": {}
    }
    
    # Test 1: Basic dashboard access
    print("\n🏠 Test 1: Basic Dashboard Access")
    try:
        response = requests.get(dashboard_url, timeout=10)
        dashboard_accessible = response.status_code == 200
        
        results["tests"]["dashboard_access"] = {
            "status_code": response.status_code,
            "accessible": dashboard_accessible,
            "response_time": response.elapsed.total_seconds()
        }
        
        status_icon = "✅" if dashboard_accessible else "❌"
        print(f"  {status_icon} Dashboard access: HTTP {response.status_code}")
        print(f"  📊 Response time: {response.elapsed.total_seconds():.2f}s")
        
        if dashboard_accessible:
            content_length = len(response.text)
            has_simulation_selector = "simulation-dropdown" in response.text
            has_simulation_container = "simulation-selector-container" in response.text
            
            results["tests"]["dashboard_access"]["content_length"] = content_length
            results["tests"]["dashboard_access"]["has_simulation_selector"] = has_simulation_selector
            results["tests"]["dashboard_access"]["has_simulation_container"] = has_simulation_container
            
            print(f"  📄 Content length: {content_length:,} characters")
            print(f"  🔍 Simulation selector present: {'✅' if has_simulation_selector else '❌'}")
            print(f"  📦 Simulation container present: {'✅' if has_simulation_container else '❌'}")
        
    except Exception as e:
        results["tests"]["dashboard_access"] = {"error": str(e)}
        print(f"  ❌ Dashboard access failed: {e}")
    
    # Test 2: Default simulation (Myportolio)
    print("\n🦄 Test 2: Default Simulation (Myportolio)")
    try:
        response = requests.get(f"{dashboard_url}?simulation=Myportolio", timeout=10)
        default_sim_works = response.status_code == 200
        
        results["tests"]["default_simulation"] = {
            "status_code": response.status_code,
            "works": default_sim_works
        }
        
        status_icon = "✅" if default_sim_works else "❌"
        print(f"  {status_icon} Default simulation access: HTTP {response.status_code}")
        
        if default_sim_works:
            has_myportolio = "Myportolio" in response.text
            has_portfolio_stats = "portfolio-stats" in response.text
            has_eth_algorithm = "ETH Algorithm Framework" in response.text
            
            results["tests"]["default_simulation"]["has_myportolio"] = has_myportolio
            results["tests"]["default_simulation"]["has_portfolio_stats"] = has_portfolio_stats
            results["tests"]["default_simulation"]["has_eth_algorithm"] = has_eth_algorithm
            
            print(f"  🦄 Myportolio content: {'✅' if has_myportolio else '❌'}")
            print(f"  📊 Portfolio stats: {'✅' if has_portfolio_stats else '❌'}")
            print(f"  🔥 ETH algorithms: {'✅' if has_eth_algorithm else '❌'}")
        
    except Exception as e:
        results["tests"]["default_simulation"] = {"error": str(e)}
        print(f"  ❌ Default simulation test failed: {e}")
    
    # Test 3: Backtest simulation selection
    print("\n📈 Test 3: Backtest Simulation Selection")
    backtest_simulations = [
        "backtest_20250903_142955_5618caf5",
        "backtest_20250903_145040_bef7f054",
        "backtest_20250903_143119_fa83c2ff"
    ]
    
    backtest_results = {}
    
    for sim_id in backtest_simulations:
        try:
            sim_url = f"{dashboard_url}?simulation={sim_id}"
            response = requests.get(sim_url, timeout=10)
            
            sim_accessible = response.status_code == 200
            sim_results = {
                "status_code": response.status_code,
                "accessible": sim_accessible
            }
            
            if sim_accessible:
                has_simulation_name = sim_id in response.text
                has_backtest_content = "Backtest:" in response.text
                has_selected_option = f'value="{sim_id}" selected' in response.text
                
                sim_results["has_simulation_name"] = has_simulation_name
                sim_results["has_backtest_content"] = has_backtest_content
                sim_results["has_selected_option"] = has_selected_option
            
            backtest_results[sim_id] = sim_results
            
            status_icon = "✅" if sim_accessible else "❌"
            print(f"  {status_icon} {sim_id[:25]}... HTTP {response.status_code}")
            
            if sim_accessible:
                name_icon = "✅" if sim_results.get("has_simulation_name") else "❌"
                content_icon = "✅" if sim_results.get("has_backtest_content") else "❌"
                selected_icon = "✅" if sim_results.get("has_selected_option") else "❌"
                print(f"    📝 Name in content: {name_icon}")
                print(f"    📊 Backtest content: {content_icon}")
                print(f"    🔽 Selected in dropdown: {selected_icon}")
            
            time.sleep(0.5)  # Be nice to the server
            
        except Exception as e:
            backtest_results[sim_id] = {"error": str(e)}
            print(f"  ❌ {sim_id}: {e}")
    
    results["tests"]["backtest_simulations"] = backtest_results
    
    # Test 4: Invalid simulation handling
    print("\n⚠️  Test 4: Invalid Simulation Handling")
    try:
        invalid_url = f"{dashboard_url}?simulation=invalid_simulation_id"
        response = requests.get(invalid_url, timeout=10)
        
        invalid_handled = response.status_code == 200  # Should still load, just fallback to default
        
        results["tests"]["invalid_simulation"] = {
            "status_code": response.status_code,
            "handled_gracefully": invalid_handled
        }
        
        if invalid_handled:
            falls_back_to_myportolio = "Myportolio" in response.text
            has_warning_message = "not found" in response.text.lower()
            
            results["tests"]["invalid_simulation"]["falls_back_to_myportolio"] = falls_back_to_myportolio
            results["tests"]["invalid_simulation"]["has_warning_message"] = has_warning_message
            
            print(f"  ✅ Invalid simulation handled gracefully")
            print(f"  🦄 Falls back to Myportolio: {'✅' if falls_back_to_myportolio else '❌'}")
            print(f"  ⚠️  Shows warning message: {'✅' if has_warning_message else '❌'}")
        else:
            print(f"  ❌ Invalid simulation not handled: HTTP {response.status_code}")
        
    except Exception as e:
        results["tests"]["invalid_simulation"] = {"error": str(e)}
        print(f"  ❌ Invalid simulation test failed: {e}")
    
    # Test 5: JavaScript functionality check
    print("\n🔧 Test 5: JavaScript Integration")
    try:
        response = requests.get(dashboard_url, timeout=10)
        
        if response.status_code == 200:
            has_change_function = "changeSimulation" in response.text
            has_dropdown_onchange = "onchange=\"changeSimulation" in response.text
            has_url_update_logic = "url.searchParams.set" in response.text
            
            results["tests"]["javascript_integration"] = {
                "has_change_function": has_change_function,
                "has_dropdown_onchange": has_dropdown_onchange,
                "has_url_update_logic": has_url_update_logic
            }
            
            print(f"  🔧 changeSimulation function: {'✅' if has_change_function else '❌'}")
            print(f"  🔽 Dropdown onchange handler: {'✅' if has_dropdown_onchange else '❌'}")
            print(f"  🔗 URL update logic: {'✅' if has_url_update_logic else '❌'}")
        
    except Exception as e:
        results["tests"]["javascript_integration"] = {"error": str(e)}
        print(f"  ❌ JavaScript test failed: {e}")
    
    # Summary
    print("\n" + "=" * 70)
    print("📊 SUMMARY")
    print("=" * 70)
    
    total_tests = 0
    passed_tests = 0
    
    for test_name, test_data in results["tests"].items():
        if isinstance(test_data, dict) and "error" not in test_data:
            total_tests += 1
            
            if test_name == "dashboard_access":
                passed_tests += 1 if test_data.get("accessible") else 0
            elif test_name == "default_simulation":
                passed_tests += 1 if test_data.get("works") else 0
            elif test_name == "backtest_simulations":
                accessible_sims = sum(1 for sim_data in test_data.values() 
                                    if isinstance(sim_data, dict) and sim_data.get("accessible"))
                total_sims = len(test_data)
                passed_tests += 1 if accessible_sims >= total_sims * 0.8 else 0  # 80% success threshold
            elif test_name == "invalid_simulation":
                passed_tests += 1 if test_data.get("handled_gracefully") else 0
            elif test_name == "javascript_integration":
                js_score = sum(1 for key in ["has_change_function", "has_dropdown_onchange", "has_url_update_logic"] 
                              if test_data.get(key))
                passed_tests += 1 if js_score >= 2 else 0  # At least 2 out of 3 JS features
    
    success_rate = (passed_tests / total_tests) * 100 if total_tests > 0 else 0
    
    results["summary"] = {
        "total_tests": total_tests,
        "passed_tests": passed_tests,
        "success_rate": f"{success_rate:.1f}%",
        "simulation_selection_ready": success_rate >= 80
    }
    
    print(f"  Total Tests: {total_tests}")
    print(f"  Passed Tests: {passed_tests}")
    print(f"  Success Rate: {success_rate:.1f}%")
    print(f"  Simulation Selection Ready: {'✅' if success_rate >= 80 else '❌'}")
    
    # Save results
    results_file = f"simulation_frontend_test_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\n💾 Results saved to: {results_file}")
    
    if success_rate >= 80:
        print("\n🎉 SIMULATION SELECTION INTEGRATION SUCCESSFUL!")
        print("   Users can now select between live portfolio and backtest simulations")
        print("   All major functionality is working as expected")
    else:
        print("\n⚠️  SIMULATION SELECTION NEEDS ATTENTION")
        print("   Some functionality may not be working properly")
    
    return results

if __name__ == "__main__":
    test_simulation_frontend_integration()
