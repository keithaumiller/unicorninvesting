#!/usr/bin/env python3
"""
Portfolio and Simulation Selector Tests

Tests that verify:
1. All portfolios in 4_portfolios directory are listed in the portfolio selector
2. All simulations in Myportolio/simulations/backtests are listed when Myportolio is selected
3. Portfolio selector functionality and user interface elements
4. Simulation selector functionality and user interface elements
"""

import requests
import json
import os
import re
import sys
from datetime import datetime
from bs4 import BeautifulSoup

def get_expected_portfolios():
    """Get expected portfolios from filesystem."""
    portfolios_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios"
    expected_portfolios = []
    
    if os.path.exists(portfolios_path):
        for item in os.listdir(portfolios_path):
            item_path = os.path.join(portfolios_path, item)
            if os.path.isdir(item_path) and not item.startswith('.') and item not in ['utilities', '__pycache__']:
                expected_portfolios.append(item)
    
    return sorted(expected_portfolios)

def get_expected_simulations(portfolio_id):
    """Get expected simulations for a specific portfolio."""
    simulations_path = f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/{portfolio_id}/simulations/backtests"
    expected_simulations = []
    
    if os.path.exists(simulations_path):
        for item in os.listdir(simulations_path):
            item_path = os.path.join(simulations_path, item)
            if os.path.isdir(item_path) and not item.startswith('.'):
                expected_simulations.append(item)
    
    return sorted(expected_simulations)

def test_portfolio_selector():
    """Test that all portfolios from 4_portfolios directory are listed in the selector."""
    
    print("🦄 Testing Portfolio Selector")
    print("-" * 40)
    
    try:
        # Get expected portfolios from filesystem
        expected_portfolios = get_expected_portfolios()
        print(f"Expected portfolios from filesystem: {expected_portfolios}")
        
        # Get the simulation management page
        response = requests.get("http://localhost/admin/metrics/lean/simulations", timeout=10)
        assert response.status_code == 200, f"Failed to load page: {response.status_code}"
        
        content = response.text
        
        # Parse HTML content
        soup = BeautifulSoup(content, 'html.parser')
        
        # Find the portfolio selector
        portfolio_select = soup.find('select', {'id': 'portfolio-select'})
        if not portfolio_select:
            print("❌ Portfolio selector not found in page")
            return False
        
        # Get all option values (excluding the default "-- Select Portfolio --")
        options = portfolio_select.find_all('option')
        found_portfolios = []
        for option in options:
            value = option.get('value', '').strip()
            if value and value != "":
                found_portfolios.append(value)
        
        print(f"Found portfolios in selector: {found_portfolios}")
        
        # Verify all expected portfolios are present
        missing_portfolios = []
        for expected in expected_portfolios:
            if expected not in found_portfolios:
                missing_portfolios.append(expected)
        
        # Check for extra portfolios
        extra_portfolios = []
        for found in found_portfolios:
            if found not in expected_portfolios:
                extra_portfolios.append(found)
        
        # Results
        success = len(missing_portfolios) == 0
        
        print(f"✅ Expected portfolios: {len(expected_portfolios)}")
        print(f"✅ Found portfolios: {len(found_portfolios)}")
        
        if missing_portfolios:
            print(f"❌ Missing portfolios: {missing_portfolios}")
        else:
            print("✅ All expected portfolios found")
        
        if extra_portfolios:
            print(f"⚠️  Extra portfolios: {extra_portfolios}")
        
        # Test portfolio info display
        portfolio_info = soup.find('div', {'id': 'portfolio-info'})
        if portfolio_info:
            print("✅ Portfolio info section found")
        else:
            print("❌ Portfolio info section not found")
        
        return success
        
    except Exception as e:
        print(f"❌ Portfolio selector test failed: {e}")
        return False

def test_simulation_selector():
    """Test that all simulations for Myportolio are listed in the selector."""
    
    print("\n🎯 Testing Simulation Selector")
    print("-" * 40)
    
    try:
        # Get expected simulations for Myportolio
        expected_simulations = get_expected_simulations("Myportolio")
        print(f"Expected simulations for Myportolio: {expected_simulations}")
        
        # Get the simulation management page
        response = requests.get("http://localhost/admin/metrics/lean/simulations", timeout=10)
        assert response.status_code == 200, f"Failed to load page: {response.status_code}"
        
        content = response.text
        
        # Parse HTML content
        soup = BeautifulSoup(content, 'html.parser')
        
        # Find simulation cards
        simulation_cards = soup.find_all('div', class_='simulation-card')
        print(f"Found {len(simulation_cards)} simulation cards")
        
        found_simulations = []
        for card in simulation_cards:
            sim_id = card.get('data-simulation', '').strip()
            if sim_id:
                found_simulations.append(sim_id)
        
        print(f"Found simulations in selector: {found_simulations}")
        
        # Verify all expected simulations are present
        missing_simulations = []
        for expected in expected_simulations:
            if expected not in found_simulations:
                missing_simulations.append(expected)
        
        # Check for extra simulations
        extra_simulations = []
        for found in found_simulations:
            if found not in expected_simulations:
                extra_simulations.append(found)
        
        # Results
        success = len(missing_simulations) == 0
        
        print(f"✅ Expected simulations: {len(expected_simulations)}")
        print(f"✅ Found simulations: {len(found_simulations)}")
        
        if missing_simulations:
            print(f"❌ Missing simulations: {missing_simulations}")
        else:
            print("✅ All expected simulations found")
        
        if extra_simulations:
            print(f"⚠️  Extra simulations: {extra_simulations}")
        
        # Test simulation card details
        simulation_details_found = 0
        for card in simulation_cards:
            stats = card.find('div', class_='simulation-stats')
            if stats:
                stat_spans = stats.find_all('span', class_='stat')
                if len(stat_spans) >= 3:  # Status, Created, Portfolio
                    simulation_details_found += 1
        
        print(f"✅ Simulation cards with complete details: {simulation_details_found}/{len(simulation_cards)}")
        
        return success
        
    except Exception as e:
        print(f"❌ Simulation selector test failed: {e}")
        return False

def test_selector_interactivity():
    """Test the interactive elements of the selectors."""
    
    print("\n🔧 Testing Selector Interactivity")
    print("-" * 40)
    
    try:
        # Get the simulation management page
        response = requests.get("http://localhost/admin/metrics/lean/simulations", timeout=10)
        assert response.status_code == 200, f"Failed to load page: {response.status_code}"
        
        content = response.text
        
        # Check for required JavaScript functions
        js_functions = [
            'loadPortfolioSimulations',
            'navigateToSimulation', 
            'compareSimulations'
        ]
        
        found_functions = []
        for func in js_functions:
            if f'function {func}()' in content or f'{func} =' in content:
                found_functions.append(func)
                print(f"✅ JavaScript function found: {func}")
            else:
                print(f"❌ JavaScript function missing: {func}")
        
        # Check for simulation action buttons
        soup = BeautifulSoup(content, 'html.parser')
        
        action_buttons = soup.find_all('button', class_=['btn', 'button'])
        button_texts = [btn.get_text().strip() for btn in action_buttons if btn.get_text().strip()]
        
        expected_buttons = ['Analyze Selected Simulation', 'Compare Simulations']
        found_buttons = []
        for expected in expected_buttons:
            for text in button_texts:
                if expected.lower() in text.lower():
                    found_buttons.append(expected)
                    print(f"✅ Action button found: {expected}")
                    break
            else:
                print(f"❌ Action button missing: {expected}")
        
        # Check for event listeners setup
        event_listeners = [
            'addEventListener("click"',
            '.forEach(card =>'
        ]
        
        found_listeners = []
        for listener in event_listeners:
            if listener in content:
                found_listeners.append(listener)
                print(f"✅ Event listener setup found: {listener}")
            else:
                print(f"❌ Event listener setup missing: {listener}")
        
        success = (
            len(found_functions) == len(js_functions) and
            len(found_buttons) >= 1 and  # At least one action button
            len(found_listeners) >= 1    # At least one event listener
        )
        
        print(f"\n📊 Interactivity Results:")
        print(f"JavaScript Functions: {len(found_functions)}/{len(js_functions)}")
        print(f"Action Buttons: {len(found_buttons)}/{len(expected_buttons)}")
        print(f"Event Listeners: {len(found_listeners)}/{len(event_listeners)}")
        
        return success
        
    except Exception as e:
        print(f"❌ Interactivity test failed: {e}")
        return False

def test_selector_conditional_logic():
    """Test that simulation selector only works when portfolio is selected."""
    
    print("\n🔄 Testing Conditional Logic")
    print("-" * 40)
    
    try:
        # Get the simulation management page
        response = requests.get("http://localhost/admin/metrics/lean/simulations", timeout=10)
        assert response.status_code == 200, f"Failed to load page: {response.status_code}"
        
        content = response.text
        soup = BeautifulSoup(content, 'html.parser')
        
        # Check if portfolio selector exists and has options
        portfolio_select = soup.find('select', {'id': 'portfolio-select'})
        portfolio_options = portfolio_select.find_all('option') if portfolio_select else []
        portfolio_has_data = len([opt for opt in portfolio_options if opt.get('value', '').strip()]) > 0
        
        print(f"✅ Portfolio selector has data: {portfolio_has_data}")
        
        # Check if simulation selector shows appropriate state
        simulation_container = soup.find('div', {'id': 'admin-simulation-selector'})
        has_simulation_cards = len(soup.find_all('div', class_='simulation-card')) > 0
        has_no_simulations_msg = soup.find('div', class_='no-simulations') is not None
        
        print(f"✅ Simulation cards present: {has_simulation_cards}")
        print(f"✅ No-simulations message present: {has_no_simulations_msg}")
        
        # Logic test: If portfolio has data, simulations should be shown OR no-simulations message
        logic_correct = True
        if portfolio_has_data:
            if not (has_simulation_cards or has_no_simulations_msg):
                logic_correct = False
                print("❌ Portfolio has data but no simulation display found")
        
        # Check for overview statistics
        stats_grid = soup.find('div', class_='stats-grid')
        stat_cards = soup.find_all('div', class_='stat-card') if stats_grid else []
        
        expected_stats = ['Total Portfolios', 'Total Simulations', 'Completed Simulations']
        found_stats = []
        for card in stat_cards:
            h3 = card.find('h3')
            if h3:
                stat_title = h3.get_text().strip()
                for expected in expected_stats:
                    if expected in stat_title:
                        found_stats.append(expected)
                        break
        
        print(f"✅ Statistics cards found: {len(found_stats)}/{len(expected_stats)}")
        for stat in found_stats:
            print(f"   • {stat}")
        
        success = logic_correct and len(found_stats) >= 2  # At least 2 stat cards
        
        return success
        
    except Exception as e:
        print(f"❌ Conditional logic test failed: {e}")
        return False

def main():
    """Run all portfolio and simulation selector tests."""
    
    print("🧪 Portfolio & Simulation Selector Test Suite")
    print("=" * 60)
    print(f"Timestamp: {datetime.now().isoformat()}")
    print(f"Target URL: http://localhost/admin/metrics/lean/simulations")
    
    test_results = []
    
    # Run tests
    test_results.append(("portfolio_selector", test_portfolio_selector()))
    test_results.append(("simulation_selector", test_simulation_selector()))
    test_results.append(("selector_interactivity", test_selector_interactivity()))
    test_results.append(("conditional_logic", test_selector_conditional_logic()))
    
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
    os.makedirs(results_dir, exist_ok=True)
    results_file = f"{results_dir}/portfolio_simulation_selector_test_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    results_data = {
        "timestamp": datetime.now().isoformat(),
        "test_type": "portfolio_simulation_selector",
        "target_url": "http://localhost/admin/metrics/lean/simulations",
        "tests": [{"name": name, "passed": result} for name, result in test_results],
        "summary": {
            "passed": passed_tests,
            "total": total_tests,
            "success_rate": passed_tests/total_tests*100
        },
        "filesystem_data": {
            "expected_portfolios": get_expected_portfolios(),
            "expected_simulations_myportolio": get_expected_simulations("Myportolio")
        }
    }
    with open(results_file, 'w') as f:
        json.dump(results_data, f, indent=2, default=str)
    print(f"\n💾 Test results saved to: {results_file}")
    
    if passed_tests == total_tests:
        print("\n🎉 All selector tests PASSED! Portfolio and simulation selectors are fully functional.")
        return 0
    else:
        print(f"\n⚠️ {total_tests - passed_tests} test(s) failed. Review results above.")
        return 1

if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
