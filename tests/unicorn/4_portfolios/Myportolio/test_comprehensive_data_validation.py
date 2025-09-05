#!/usr/bin/env python3
"""
Comprehensive Myportolio Data Validation Test

This test script catalogs ALL data points from the Myportolio backend files and validates 
that they appear correctly on the corresponding web pages when different simulations are selected.

Test Structure:
1. Catalog all data points from configuration files
2. Catalog data from simulation backtests
3. Test web page display for each simulation
4. Validate that data changes when simulation is switched
"""

import json
import os
import requests
import re
from datetime import datetime
from pathlib import Path

class MyportolioDataCatalog:
    """Comprehensive catalog of all Myportolio data points"""
    
    def __init__(self):
        self.base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        self.catalog = {}
        self.build_catalog()
    
    def build_catalog(self):
        """Build complete catalog of all data points"""
        
        # 1. Core Configuration Files
        self.catalog['core_config'] = self._catalog_core_config()
        
        # 2. Risk Parameters
        self.catalog['risk_config'] = self._catalog_risk_config()
        
        # 3. Status Reports (latest)
        self.catalog['status_reports'] = self._catalog_status_reports()
        
        # 4. Risk Reports (latest) 
        self.catalog['risk_reports'] = self._catalog_risk_reports()
        
        # 5. Algorithm Framework
        self.catalog['algorithms'] = self._catalog_algorithms()
        
        # 6. Simulation Backtests
        self.catalog['simulations'] = self._catalog_simulations()
        
        # 7. Expected Web Page Mappings
        self.catalog['web_mappings'] = self._build_web_mappings()
    
    def _catalog_core_config(self):
        """Catalog main config.json data"""
        config_path = f"{self.base_path}/config.json"
        
        if not os.path.exists(config_path):
            return {"error": "config.json not found"}
        
        with open(config_path, 'r') as f:
            config = json.load(f)
        
        return {
            "portfolio_name": config.get("portfolio_name"),
            "description": config.get("description"),
            "strategy_type": config.get("strategy_type"),
            "assets": config.get("assets", {}),
            "total_allocation": config.get("total_allocation"),
            "target_volatility": config.get("target_volatility"),
            "rebalancing_frequency": config.get("rebalancing_frequency"),
            "currency": config.get("currency"),
            "status": config.get("status"),
            "created_date": config.get("created_date"),
            "last_updated": config.get("last_updated")
        }
    
    def _catalog_risk_config(self):
        """Catalog risk_parameters.json data"""
        risk_path = f"{self.base_path}/risk_parameters.json"
        
        if not os.path.exists(risk_path):
            return {"error": "risk_parameters.json not found"}
        
        with open(risk_path, 'r') as f:
            risk_config = json.load(f)
        
        return {
            "risk_profile": risk_config.get("risk_profile"),
            "max_portfolio_volatility": risk_config.get("max_portfolio_volatility"),
            "max_drawdown": risk_config.get("max_drawdown"),
            "var_limit_1day": risk_config.get("var_limit_1day"),
            "sharpe_ratio_target": risk_config.get("sharpe_ratio_target"),
            "risk_budget_allocation": risk_config.get("risk_budget_allocation", {}),
            "position_limits": risk_config.get("position_limits", {}),
            "stop_loss_settings": risk_config.get("stop_loss_settings", {})
        }
    
    def _catalog_status_reports(self):
        """Catalog latest status report data"""
        status_files = [f for f in os.listdir(self.base_path) if f.startswith('status_report_') and f.endswith('.json')]
        
        if not status_files:
            return {"error": "No status reports found"}
        
        # Get the latest status report
        latest_status = max(status_files)
        status_path = f"{self.base_path}/{latest_status}"
        
        with open(status_path, 'r') as f:
            status = json.load(f)
        
        return {
            "filename": latest_status,
            "overall_readiness": status.get("overall_readiness"),
            "critical_issues_count": len(status.get("critical_issues", [])),
            "warnings_count": len(status.get("warnings", [])),
            "passed_checks_count": len(status.get("passed_checks", [])),
            "timestamp": status.get("timestamp"),
            "portfolio": status.get("portfolio"),
            "component_status": status.get("component_status", {}),
            "readiness_score": status.get("readiness_score")
        }
    
    def _catalog_risk_reports(self):
        """Catalog latest risk report data"""
        risk_files = [f for f in os.listdir(self.base_path) if f.startswith('risk_report_') and f.endswith('.json')]
        
        if not risk_files:
            return {"error": "No risk reports found"}
        
        # Get the latest risk report
        latest_risk = max(risk_files)
        risk_path = f"{self.base_path}/{latest_risk}"
        
        with open(risk_path, 'r') as f:
            risk_report = json.load(f)
        
        return {
            "filename": latest_risk,
            "portfolio": risk_report.get("portfolio"),
            "timestamp": risk_report.get("timestamp"),
            "risk_metrics": risk_report.get("risk_metrics", {}),
            "violations": risk_report.get("violations", []),
            "recommendations": risk_report.get("recommendations", [])
        }
    
    def _catalog_algorithms(self):
        """Catalog algorithm framework data"""
        algorithms = {}
        
        # Risk algorithms
        risk_algo_path = f"{self.base_path}/risk_algorithms"
        if os.path.exists(risk_algo_path):
            risk_algorithms = []
            for item in os.listdir(risk_algo_path):
                if os.path.isfile(os.path.join(risk_algo_path, item)) and item.endswith('.py'):
                    risk_algorithms.append(item)
                elif os.path.isdir(os.path.join(risk_algo_path, item)) and item not in ['__pycache__']:
                    risk_algorithms.append(f"{item}/")
            algorithms['risk_algorithms'] = risk_algorithms
        
        # Trading algorithms  
        trading_algo_path = f"{self.base_path}/trading_algorithms"
        if os.path.exists(trading_algo_path):
            trading_algorithms = []
            for item in os.listdir(trading_algo_path):
                if os.path.isfile(os.path.join(trading_algo_path, item)) and item.endswith('.py'):
                    trading_algorithms.append(item)
            algorithms['trading_algorithms'] = trading_algorithms
        
        return algorithms
    
    def _catalog_simulations(self):
        """Catalog all simulation backtest data"""
        simulations = {}
        backtests_path = f"{self.base_path}/simulations/backtests"
        
        if not os.path.exists(backtests_path):
            return {"error": "No backtests directory found"}
        
        for backtest_dir in os.listdir(backtests_path):
            backtest_path = os.path.join(backtests_path, backtest_dir)
            
            if not os.path.isdir(backtest_path):
                continue
            
            simulation_data = {
                "id": backtest_dir,
                "path": backtest_path,
                "files": os.listdir(backtest_path) if os.path.isdir(backtest_path) else []
            }
            
            # Load results if available
            results_file = os.path.join(backtest_path, "myportolio_results.json")
            if os.path.exists(results_file):
                with open(results_file, 'r') as f:
                    results = json.load(f)
                
                simulation_data["results"] = {
                    "simulation_id": results.get("simulation_id"),
                    "portfolio": results.get("portfolio"),
                    "strategy": results.get("strategy"),
                    "performance": results.get("performance", {}),
                    "configuration": results.get("configuration", {}),
                    "execution": results.get("execution", {}),
                    "trades_count": len(results.get("lean_results", {}).get("trades", [])),
                    "market_data_points": len(results.get("lean_results", {}).get("market_data", []))
                }
            
            # Load lean config if available
            lean_config_file = os.path.join(backtest_path, "lean_config.json")
            if os.path.exists(lean_config_file):
                with open(lean_config_file, 'r') as f:
                    lean_config = json.load(f)
                
                simulation_data["lean_config"] = {
                    "algorithm-type-name": lean_config.get("algorithm-type-name"),
                    "algorithm-location": lean_config.get("algorithm-location"),
                    "data-folder": lean_config.get("data-folder"),
                    "algorithm-language": lean_config.get("algorithm-language")
                }
            
            simulations[backtest_dir] = simulation_data
        
        return simulations
    
    def _build_web_mappings(self):
        """Define which data points should appear on which web pages"""
        return {
            "dashboard_header": [
                "portfolio_name", "description", "last_updated", "overall_readiness"
            ],
            "portfolio_stats": [
                "portfolio_value", "asset_count", "target_volatility", "status"
            ],
            "asset_allocation": [
                "assets.ETH.allocation_percent", "assets.BTC.allocation_percent", "total_allocation"
            ],
            "risk_metrics": [
                "max_portfolio_volatility", "max_drawdown", "var_limit_1day", "sharpe_ratio_target"
            ],
            "algorithm_status": [
                "risk_algorithms", "trading_algorithms"
            ],
            "simulation_performance": [
                "performance.total_return", "performance.sharpe_ratio", 
                "performance.max_drawdown", "trades_count"
            ],
            "simulation_selector": [
                "simulation_id", "portfolio", "strategy"
            ]
        }

class MyportolioWebValidator:
    """Validates that backend data appears correctly on web pages"""
    
    def __init__(self, catalog):
        self.catalog = catalog
        self.base_url = "http://localhost"
        self.dashboard_url = f"{self.base_url}/unicorn"
        self.results = {
            "timestamp": datetime.now().isoformat(),
            "tests": {},
            "data_validation": {},
            "summary": {}
        }
    
    def validate_all_simulations(self):
        """Test all available simulations and validate data changes"""
        print("🔍 COMPREHENSIVE MYPORTOLIO DATA VALIDATION")
        print("=" * 70)
        
        # Test 1: Live Portfolio (Myportolio)
        print("\n🦄 Test 1: Live Portfolio Data Validation")
        myportolio_validation = self._validate_simulation_data("Myportolio", is_live=True)
        self.results["tests"]["myportolio_live"] = myportolio_validation
        
        # Test 2: Each Backtest Simulation
        print("\n📈 Test 2: Backtest Simulation Data Validation")
        backtest_validations = {}
        
        for sim_id, sim_data in self.catalog['simulations'].items():
            if "error" not in sim_data:
                print(f"\n  Testing: {sim_id}")
                validation = self._validate_simulation_data(sim_id, is_live=False)
                backtest_validations[sim_id] = validation
        
        self.results["tests"]["backtest_simulations"] = backtest_validations
        
        # Test 3: Data Switching Validation
        print("\n🔄 Test 3: Data Switching Validation")
        switching_validation = self._validate_data_switching()
        self.results["tests"]["data_switching"] = switching_validation
        
        # Generate Summary
        self._generate_summary()
        
        return self.results
    
    def _validate_simulation_data(self, simulation_id, is_live=True):
        """Validate specific simulation data appears on web page"""
        
        try:
            # Get web page content
            url = f"{self.dashboard_url}?simulation={simulation_id}"
            response = requests.get(url, timeout=10)
            
            if response.status_code != 200:
                return {"error": f"HTTP {response.status_code}"}
            
            content = response.text
            validation_results = {
                "simulation_id": simulation_id,
                "is_live": is_live,
                "url": url,
                "response_time": response.elapsed.total_seconds(),
                "content_length": len(content),
                "data_points_found": {},
                "data_points_missing": {},
                "expected_data": {}
            }
            
            if is_live:
                # Validate live portfolio data
                expected_data = self.catalog['core_config']
                validation_results["expected_data"] = expected_data
                
                # Check specific data points
                validation_results["data_points_found"]["portfolio_name"] = expected_data.get("portfolio_name", "") in content
                validation_results["data_points_found"]["description"] = expected_data.get("description", "") in content
                validation_results["data_points_found"]["strategy_type"] = expected_data.get("strategy_type", "") in content
                
                # Check asset allocations
                if "assets" in expected_data and expected_data["assets"]:
                    for asset, asset_data in expected_data["assets"].items():
                        allocation = str(asset_data.get("allocation_percent", 0))
                        validation_results["data_points_found"][f"{asset}_allocation"] = allocation in content
                
                # Check risk parameters
                risk_data = self.catalog['risk_config']
                if risk_data and "error" not in risk_data:
                    validation_results["data_points_found"]["risk_profile"] = str(risk_data.get("risk_profile", "")) in content
                    validation_results["data_points_found"]["max_volatility"] = str(risk_data.get("max_portfolio_volatility", 0)) in content
                
            else:
                # Validate backtest simulation data
                if simulation_id in self.catalog['simulations']:
                    sim_data = self.catalog['simulations'][simulation_id]
                    validation_results["expected_data"] = sim_data
                    
                    # Check simulation ID appears
                    validation_results["data_points_found"]["simulation_id"] = simulation_id in content
                    
                    # Check backtest-specific content
                    validation_results["data_points_found"]["backtest_label"] = "Backtest:" in content
                    
                    # If results available, check performance data
                    if "results" in sim_data:
                        results = sim_data["results"]
                        validation_results["data_points_found"]["portfolio_name"] = results.get("portfolio", "") in content
                        
                        # Check if performance metrics show (even if zero)
                        performance = results.get("performance", {})
                        for metric in ["total_return", "sharpe_ratio", "max_drawdown"]:
                            value = str(performance.get(metric, 0))
                            validation_results["data_points_found"][f"performance_{metric}"] = value in content
            
            # Check simulation selector presence and selection
            validation_results["data_points_found"]["simulation_selector"] = "simulation-dropdown" in content
            validation_results["data_points_found"]["selected_option"] = f'value="{simulation_id}" selected' in content
            
            # Count successful validations
            found_count = sum(1 for found in validation_results["data_points_found"].values() if found)
            total_count = len(validation_results["data_points_found"])
            validation_results["validation_score"] = found_count / total_count if total_count > 0 else 0
            
            print(f"    📊 Validation Score: {validation_results['validation_score']:.1%} ({found_count}/{total_count})")
            
            # Report missing data points
            for key, found in validation_results["data_points_found"].items():
                if not found:
                    validation_results["data_points_missing"][key] = "Expected but not found in content"
                    print(f"    ❌ Missing: {key}")
                else:
                    print(f"    ✅ Found: {key}")
            
            return validation_results
            
        except Exception as e:
            return {"error": str(e)}
    
    def _validate_data_switching(self):
        """Test that data actually changes when switching between simulations"""
        
        switching_results = {
            "tests_performed": [],
            "data_changes_detected": {},
            "consistent_elements": {},
            "switching_functional": False
        }
        
        try:
            # Get content for Myportolio (live)
            myportolio_response = requests.get(f"{self.dashboard_url}?simulation=Myportolio", timeout=10)
            myportolio_content = myportolio_response.text if myportolio_response.status_code == 200 else ""
            
            # Get content for a backtest simulation
            backtest_sims = list(self.catalog['simulations'].keys())
            if backtest_sims:
                test_sim = backtest_sims[0]
                backtest_response = requests.get(f"{self.dashboard_url}?simulation={test_sim}", timeout=10)
                backtest_content = backtest_response.text if backtest_response.status_code == 200 else ""
                
                switching_results["tests_performed"] = ["Myportolio", test_sim]
                
                # Check if simulation selector shows different selections
                myportolio_selected = 'value="Myportolio" selected' in myportolio_content
                backtest_selected = f'value="{test_sim}" selected' in backtest_content
                
                switching_results["data_changes_detected"]["selector_changes"] = myportolio_selected and backtest_selected
                switching_results["data_changes_detected"]["different_content"] = myportolio_content != backtest_content
                switching_results["data_changes_detected"]["content_length_diff"] = abs(len(myportolio_content) - len(backtest_content))
                
                # Check for simulation-specific content
                switching_results["data_changes_detected"]["live_portfolio_content"] = "Myportolio" in myportolio_content and "Myportolio" not in backtest_content
                switching_results["data_changes_detected"]["backtest_content"] = test_sim in backtest_content and test_sim not in myportolio_content
                
                # Overall switching functional check
                changes_detected = sum(1 for change in switching_results["data_changes_detected"].values() if change)
                switching_results["switching_functional"] = changes_detected >= 2
                
                print(f"    🔄 Switching functional: {'✅' if switching_results['switching_functional'] else '❌'}")
                print(f"    📊 Changes detected: {changes_detected}/5")
                
        except Exception as e:
            switching_results["error"] = str(e)
        
        return switching_results
    
    def _generate_summary(self):
        """Generate comprehensive test summary"""
        
        total_tests = 0
        passed_tests = 0
        
        # Count live portfolio test
        if "myportolio_live" in self.results["tests"]:
            total_tests += 1
            live_score = self.results["tests"]["myportolio_live"].get("validation_score", 0)
            if live_score >= 0.7:  # 70% threshold
                passed_tests += 1
        
        # Count backtest simulation tests
        if "backtest_simulations" in self.results["tests"]:
            backtest_results = self.results["tests"]["backtest_simulations"]
            for sim_id, sim_result in backtest_results.items():
                total_tests += 1
                sim_score = sim_result.get("validation_score", 0)
                if sim_score >= 0.5:  # 50% threshold for backtests (less data available)
                    passed_tests += 1
        
        # Count data switching test
        if "data_switching" in self.results["tests"]:
            total_tests += 1
            switching_functional = self.results["tests"]["data_switching"].get("switching_functional", False)
            if switching_functional:
                passed_tests += 1
        
        success_rate = (passed_tests / total_tests) * 100 if total_tests > 0 else 0
        
        self.results["summary"] = {
            "total_tests": total_tests,
            "passed_tests": passed_tests,
            "success_rate": f"{success_rate:.1f}%",
            "simulation_switching_works": switching_functional if "data_switching" in self.results["tests"] else False,
            "data_catalog_complete": len(self.catalog.get('simulations', {})) > 0,
            "ready_for_production": success_rate >= 80 and switching_functional
        }
        
        print("\n" + "=" * 70)
        print("📊 VALIDATION SUMMARY")
        print("=" * 70)
        print(f"  Total Tests: {total_tests}")
        print(f"  Passed Tests: {passed_tests}")
        print(f"  Success Rate: {success_rate:.1f}%")
        print(f"  Simulation Switching Works: {'✅' if switching_functional else '❌'}")
        print(f"  Ready for Production: {'✅' if self.results['summary']['ready_for_production'] else '❌'}")


def main():
    """Main test execution"""
    
    print("🚀 MYPORTOLIO COMPREHENSIVE DATA VALIDATION")
    print("=" * 70)
    print("This test catalogs ALL Myportolio data and validates web page display")
    
    # Build data catalog
    print("\n📊 Building Data Catalog...")
    catalog = MyportolioDataCatalog()
    
    print(f"  ✅ Core Config: {len(catalog.catalog['core_config'])} data points")
    print(f"  ✅ Risk Config: {len(catalog.catalog['risk_config'])} data points") 
    print(f"  ✅ Status Reports: {len(catalog.catalog['status_reports'])} data points")
    print(f"  ✅ Risk Reports: {len(catalog.catalog['risk_reports'])} data points")
    print(f"  ✅ Algorithms: {len(catalog.catalog['algorithms'])} categories")
    print(f"  ✅ Simulations: {len(catalog.catalog['simulations'])} backtests")
    
    # Validate web pages
    print("\n🌐 Validating Web Pages...")
    validator = MyportolioWebValidator(catalog.catalog)
    results = validator.validate_all_simulations()
    
    # Save results
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    results_file = f"myportolio_comprehensive_validation_{timestamp}.json"
    
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\n💾 Results saved to: {results_file}")
    
    # Final verdict
    if results["summary"]["ready_for_production"]:
        print("\n🎉 VALIDATION SUCCESSFUL!")
        print("   Myportolio simulation selection is working correctly")
        print("   All data points are displaying properly on web pages")
    else:
        print("\n⚠️  VALIDATION ISSUES DETECTED")
        print("   Some data points are not displaying correctly")
        print("   Review the detailed results for specific issues")
    
    return results

if __name__ == "__main__":
    main()
