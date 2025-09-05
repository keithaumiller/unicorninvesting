#!/usr/bin/env python3

import json
import os
import sys
from datetime import datetime

def test_simulation_detection():
    """Test simulation detection functionality"""
    
    print("=" * 60)
    print("🧪 SIMULATION DETECTION TEST")
    print("=" * 60)
    
    backend_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios"
    myportolio_path = f"{backend_path}/Myportolio"
    simulations_path = f"{myportolio_path}/simulations"
    backtests_path = f"{simulations_path}/backtests"
    
    results = {
        "timestamp": datetime.now().isoformat(),
        "tests": {},
        "summary": {}
    }
    
    # Test 1: Check basic structure
    print("\n📁 Test 1: Basic Structure Check")
    basic_structure_check = {
        "myportolio_exists": os.path.exists(myportolio_path),
        "simulations_exists": os.path.exists(simulations_path),
        "backtests_exists": os.path.exists(backtests_path)
    }
    results["tests"]["basic_structure"] = basic_structure_check
    
    for check, status in basic_structure_check.items():
        status_icon = "✅" if status else "❌"
        print(f"  {status_icon} {check}: {status}")
    
    # Test 2: Detect backtest simulations
    print("\n🔍 Test 2: Backtest Simulation Detection")
    backtest_simulations = []
    
    if os.path.exists(backtests_path):
        for item in os.listdir(backtests_path):
            item_path = os.path.join(backtests_path, item)
            if os.path.isdir(item_path) and item.startswith("backtest_"):
                # Check for required files
                has_config = os.path.exists(f"{item_path}/lean_config.json")
                has_results = os.path.exists(f"{item_path}/myportolio_results.json")
                has_algorithm = any(f.endswith('.py') for f in os.listdir(item_path) if os.path.isfile(os.path.join(item_path, f)))
                
                simulation_data = {
                    "id": item,
                    "path": item_path,
                    "has_config": has_config,
                    "has_results": has_results,
                    "has_algorithm": has_algorithm,
                    "valid": has_config and (has_results or has_algorithm)
                }
                
                backtest_simulations.append(simulation_data)
                
                status_icon = "✅" if simulation_data["valid"] else "⚠️"
                print(f"  {status_icon} {item}")
                print(f"    - Config: {'✅' if has_config else '❌'}")
                print(f"    - Results: {'✅' if has_results else '❌'}")  
                print(f"    - Algorithm: {'✅' if has_algorithm else '❌'}")
    
    results["tests"]["backtest_simulations"] = backtest_simulations
    print(f"\n  📊 Found {len(backtest_simulations)} backtest simulations")
    
    # Test 3: Load simulation data
    print("\n📋 Test 3: Simulation Data Loading")
    simulation_details = []
    
    for sim in backtest_simulations:
        if sim["valid"]:
            try:
                detail = {"id": sim["id"], "loaded_data": {}}
                
                # Load lean config
                lean_config_path = f"{sim['path']}/lean_config.json"
                if os.path.exists(lean_config_path):
                    with open(lean_config_path, 'r') as f:
                        lean_config = json.load(f)
                        detail["loaded_data"]["lean_config"] = lean_config
                        print(f"  ✅ Loaded lean config for {sim['id']}")
                
                # Load results if available
                results_path = f"{sim['path']}/myportolio_results.json"
                if os.path.exists(results_path):
                    with open(results_path, 'r') as f:
                        sim_results = json.load(f)
                        detail["loaded_data"]["results"] = sim_results
                        
                        # Extract key performance metrics
                        if "Statistics" in sim_results:
                            stats = sim_results["Statistics"]
                            performance = {
                                "total_return": stats.get("Total Return", "N/A"),
                                "annual_return": stats.get("Annual Return", "N/A"),
                                "max_drawdown": stats.get("Max Drawdown", "N/A"),
                                "sharpe_ratio": stats.get("Sharpe Ratio", "N/A")
                            }
                            detail["performance"] = performance
                            print(f"  📈 Performance metrics for {sim['id']}:")
                            for metric, value in performance.items():
                                print(f"    - {metric}: {value}")
                
                simulation_details.append(detail)
                
            except Exception as e:
                print(f"  ❌ Error loading {sim['id']}: {e}")
    
    results["tests"]["simulation_details"] = simulation_details
    
    # Test 4: PHP Integration Test
    print("\n🐘 Test 4: PHP Integration Compatibility")
    
    # Create a summary format that PHP can easily parse
    php_compatible_format = {
        "default_portfolio": "Myportolio",
        "available_simulations": {}
    }
    
    # Add default Myportolio
    php_compatible_format["available_simulations"]["Myportolio"] = {
        "id": "Myportolio",
        "name": "Myportolio (Live Portfolio)",
        "description": "Live portfolio with real-time data",
        "type": "live",
        "status": "active",
        "last_updated": int(datetime.now().timestamp()),
        "path": myportolio_path
    }
    
    # Add backtest simulations
    for detail in simulation_details:
        sim_id = detail["id"]
        php_compatible_format["available_simulations"][sim_id] = {
            "id": sim_id,
            "name": f"Backtest: {sim_id}",
            "description": f"Historical backtest simulation from {sim_id.split('_')[1] if len(sim_id.split('_')) > 1 else 'unknown date'}",
            "type": "backtest", 
            "status": "completed",
            "last_updated": int(datetime.now().timestamp()),
            "path": detail.get("loaded_data", {}).get("lean_config", {}).get("data-folder", "unknown")
        }
        
        # Add performance if available
        if "performance" in detail:
            php_compatible_format["available_simulations"][sim_id]["performance"] = detail["performance"]
    
    results["php_integration"] = php_compatible_format
    
    print(f"  ✅ Created PHP-compatible format with {len(php_compatible_format['available_simulations'])} simulations")
    
    # Summary
    total_simulations = len(php_compatible_format["available_simulations"])
    valid_backtests = len([s for s in backtest_simulations if s["valid"]])
    
    results["summary"] = {
        "total_simulations_available": total_simulations,
        "live_portfolios": 1,
        "valid_backtests": valid_backtests,
        "success_rate": f"{(valid_backtests / max(len(backtest_simulations), 1)) * 100:.1f}%" if backtest_simulations else "0%",
        "php_integration_ready": True
    }
    
    print("\n" + "=" * 60)
    print("📊 SUMMARY")
    print("=" * 60)
    for key, value in results["summary"].items():
        print(f"  {key}: {value}")
    
    # Save results
    results_file = f"simulation_detection_test_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\n💾 Results saved to: {results_file}")
    
    return results

if __name__ == "__main__":
    test_simulation_detection()
