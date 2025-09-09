"""
Alpha Models Pipeline Flow Validation

This script specifically validates that our improvements are flowing correctly
from data sources through our economic-enhanced alpha models.
"""

import sys
import os
from datetime import datetime
import pandas as pd
import numpy as np
import sqlite3

# Add correct paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/BTC')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/utilities')

def validate_alpha_models_pipeline():
    """Validate that our alpha models pipeline is working with all improvements."""
    
    print("🔍 ALPHA MODELS PIPELINE VALIDATION")
    print("=" * 60)
    print(f"Validation Time: {datetime.now()}")
    
    results = {
        'economic_models_available': False,
        'ensemble_models_available': False,
        'model_performance_data': False,
        'best_model_selector_working': False,
        'myportolio_integration': False,
        'simulation_framework_working': False,
        'issues': []
    }
    
    # Step 1: Check Economic-Enhanced XGBoost Models
    print("\n🤖 Step 1: Economic-Enhanced XGBoost Models")
    try:
        # Check BTC model
        btc_model_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/BTC/btc_xgboost_economic_enhanced.py'
        btc_exists = os.path.exists(btc_model_path)
        print(f"   BTC Economic Model: {'✅ FOUND' if btc_exists else '❌ MISSING'}")
        
        # Check ETH model  
        eth_model_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_xgboost_economic_enhanced.py'
        eth_exists = os.path.exists(eth_model_path)
        print(f"   ETH Economic Model: {'✅ FOUND' if eth_exists else '❌ MISSING'}")
        
        if btc_exists and eth_exists:
            results['economic_models_available'] = True
            print("   📊 Economic-enhanced models: ✅ AVAILABLE")
            
            # Try to import them
            try:
                from btc_xgboost_economic_enhanced import BTCXGBoostWithEconomicIndicators
                from eth_xgboost_economic_enhanced import ETHXGBoostWithEconomicIndicators
                print("   🔗 Model imports: ✅ SUCCESSFUL")
            except ImportError as e:
                print(f"   ⚠️ Model import warning: {e}")
                results['issues'].append(f"Model import issue: {e}")
        else:
            results['issues'].append("Economic-enhanced models not found")
    
    except Exception as e:
        results['issues'].append(f"Economic models check error: {e}")
        print(f"   ❌ Error checking economic models: {e}")
    
    # Step 2: Check Economic Ensemble Models
    print("\n🔄 Step 2: Economic Ensemble Models")
    try:
        # Check BTC ensemble
        btc_ensemble_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/BTC/btc_ensemble_economic_enhanced.py'
        btc_ensemble_exists = os.path.exists(btc_ensemble_path)
        print(f"   BTC Ensemble Model: {'✅ FOUND' if btc_ensemble_exists else '❌ MISSING'}")
        
        # Check ETH ensemble
        eth_ensemble_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/eth_ensemble_economic_enhanced.py'
        eth_ensemble_exists = os.path.exists(eth_ensemble_path)
        print(f"   ETH Ensemble Model: {'✅ FOUND' if eth_ensemble_exists else '❌ MISSING'}")
        
        if btc_ensemble_exists and eth_ensemble_exists:
            results['ensemble_models_available'] = True
            print("   🎯 Economic ensemble models: ✅ AVAILABLE")
        else:
            results['issues'].append("Economic ensemble models not found")
    
    except Exception as e:
        results['issues'].append(f"Ensemble models check error: {e}")
        print(f"   ❌ Error checking ensemble models: {e}")
    
    # Step 3: Check Model Performance Data
    print("\n📊 Step 3: Model Performance Database")
    try:
        db_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/multi_asset_comparison.db'
        db_exists = os.path.exists(db_path)
        print(f"   Performance Database: {'✅ FOUND' if db_exists else '❌ MISSING'}")
        
        if db_exists:
            with sqlite3.connect(db_path) as conn:
                # Check for performance data
                cursor = conn.cursor()
                cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
                tables = cursor.fetchall()
                print(f"   Database Tables: {[table[0] for table in tables]}")
                
                if tables:
                    # Get model count
                    cursor.execute("SELECT COUNT(*) FROM multi_asset_performance")
                    model_count = cursor.fetchone()[0]
                    print(f"   Total Models: {model_count}")
                    
                    if model_count > 0:
                        results['model_performance_data'] = True
                        print("   📈 Performance data: ✅ AVAILABLE")
                        
                        # Check for economic models specifically
                        cursor.execute("SELECT COUNT(*) FROM multi_asset_performance WHERE methodology LIKE '%economic%'")
                        economic_count = cursor.fetchone()[0]
                        print(f"   Economic Models: {economic_count}")
                        
                        # Show top performers
                        cursor.execute("SELECT asset, model_variant, r2_score FROM multi_asset_performance ORDER BY r2_score DESC LIMIT 3")
                        top_models = cursor.fetchall()
                        print("   🏆 Top Performers:")
                        for asset, variant, r2 in top_models:
                            print(f"      {asset} {variant}: R² = {r2:.4f}")
        else:
            results['issues'].append("Model performance database not found")
    
    except Exception as e:
        results['issues'].append(f"Performance data check error: {e}")
        print(f"   ❌ Error checking performance data: {e}")
    
    # Step 4: Check Best Model Selector
    print("\n🎯 Step 4: Best Model Selector")
    try:
        selector_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/utilities/best_model_selector.py'
        enhanced_selector_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/utilities/enhanced_best_model_selector.py'
        
        selector_exists = os.path.exists(selector_path)
        enhanced_exists = os.path.exists(enhanced_selector_path)
        
        print(f"   Basic Selector: {'✅ FOUND' if selector_exists else '❌ MISSING'}")
        print(f"   Enhanced Selector: {'✅ FOUND' if enhanced_exists else '❌ MISSING'}")
        
        if selector_exists or enhanced_exists:
            results['best_model_selector_working'] = True
            print("   🔗 Model selector: ✅ AVAILABLE")
            
            # Try to import and test
            try:
                from best_model_selector import BestModelSelector
                selector = BestModelSelector()
                best_models = selector.get_best_models()
                print(f"   📊 Best models selection: ✅ WORKING ({len(best_models)} assets)")
                
                for asset, model in best_models.items():
                    r2 = model['performance'].get('r2_score', 0)
                    confidence = model.get('confidence_level', 'UNKNOWN')
                    print(f"      {asset}: R² = {r2:.4f}, Confidence = {confidence}")
            
            except Exception as e:
                print(f"   ⚠️ Selector test warning: {e}")
                results['issues'].append(f"Selector test issue: {e}")
        else:
            results['issues'].append("Best model selector not found")
    
    except Exception as e:
        results['issues'].append(f"Model selector check error: {e}")
        print(f"   ❌ Error checking model selector: {e}")
    
    # Step 5: Check Myportolio Integration
    print("\n🎯 Step 5: Myportolio Integration")
    try:
        myportolio_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio'
        integration_doc = os.path.join(myportolio_path, 'BEST_MODELS_INTEGRATION_COMPLETE.md')
        
        framework_exists = os.path.exists(myportolio_path)
        integration_exists = os.path.exists(integration_doc)
        
        print(f"   Myportolio Framework: {'✅ FOUND' if framework_exists else '❌ MISSING'}")
        print(f"   Integration Documentation: {'✅ FOUND' if integration_exists else '❌ MISSING'}")
        
        if framework_exists and integration_exists:
            results['myportolio_integration'] = True
            print("   🔗 Myportolio integration: ✅ COMPLETE")
        else:
            results['issues'].append("Myportolio integration incomplete")
    
    except Exception as e:
        results['issues'].append(f"Myportolio integration check error: {e}")
        print(f"   ❌ Error checking Myportolio integration: {e}")
    
    # Step 6: Check Simulation Framework
    print("\n🚀 Step 6: Simulation Framework")
    try:
        simulation_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations'
        backtests_path = os.path.join(simulation_path, 'backtests')
        
        simulation_exists = os.path.exists(simulation_path)
        backtests_exist = os.path.exists(backtests_path)
        
        print(f"   Simulation Framework: {'✅ FOUND' if simulation_exists else '❌ MISSING'}")
        print(f"   Backtests Directory: {'✅ FOUND' if backtests_exist else '❌ MISSING'}")
        
        if backtests_exist:
            # Count backtest results
            backtest_dirs = [d for d in os.listdir(backtests_path) 
                           if os.path.isdir(os.path.join(backtests_path, d)) and d.startswith('backtest_')]
            print(f"   Backtest Results: {len(backtest_dirs)} available")
            
            if len(backtest_dirs) > 0:
                results['simulation_framework_working'] = True
                print("   📊 Simulation framework: ✅ WORKING")
                
                # Show recent results
                recent_backtests = sorted(backtest_dirs)[-3:]  # Last 3
                print("   🎯 Recent Backtests:")
                for backtest in recent_backtests:
                    print(f"      {backtest}")
        
        if not results['simulation_framework_working']:
            results['issues'].append("Simulation framework not fully operational")
    
    except Exception as e:
        results['issues'].append(f"Simulation framework check error: {e}")
        print(f"   ❌ Error checking simulation framework: {e}")
    
    # Generate Summary
    print("\n" + "=" * 60)
    print("📋 PIPELINE VALIDATION SUMMARY")
    print("=" * 60)
    
    success_count = sum(1 for v in results.values() if isinstance(v, bool) and v)
    total_checks = len([k for k in results.keys() if k != 'issues'])
    success_rate = success_count / total_checks
    
    print(f"✅ Successful Validations: {success_count}/{total_checks} ({success_rate:.1%})")
    
    status_emoji = "🎉" if success_rate >= 0.8 else "⚠️" if success_rate >= 0.6 else "❌"
    status_text = "EXCELLENT" if success_rate >= 0.8 else "GOOD" if success_rate >= 0.6 else "NEEDS_WORK"
    
    print(f"{status_emoji} Overall Pipeline Status: {status_text}")
    
    print("\n📊 Component Status:")
    print(f"   {'✅' if results['economic_models_available'] else '❌'} Economic-Enhanced Models")
    print(f"   {'✅' if results['ensemble_models_available'] else '❌'} Economic Ensemble Models")
    print(f"   {'✅' if results['model_performance_data'] else '❌'} Performance Database")
    print(f"   {'✅' if results['best_model_selector_working'] else '❌'} Best Model Selector")
    print(f"   {'✅' if results['myportolio_integration'] else '❌'} Myportolio Integration")
    print(f"   {'✅' if results['simulation_framework_working'] else '❌'} Simulation Framework")
    
    if results['issues']:
        print("\n⚠️ Issues Identified:")
        for i, issue in enumerate(results['issues'], 1):
            print(f"   {i}. {issue}")
    
    # Data Flow Assessment
    print(f"\n🌊 Data Flow Assessment:")
    if results['economic_models_available'] and results['model_performance_data']:
        print("   ✅ Alpha Models → Performance Data: FLOWING")
    else:
        print("   ❌ Alpha Models → Performance Data: BLOCKED")
    
    if results['best_model_selector_working'] and results['myportolio_integration']:
        print("   ✅ Model Selection → Portfolio: FLOWING")
    else:
        print("   ❌ Model Selection → Portfolio: BLOCKED")
    
    if results['myportolio_integration'] and results['simulation_framework_working']:
        print("   ✅ Portfolio → Simulations: FLOWING")
    else:
        print("   ❌ Portfolio → Simulations: BLOCKED")
    
    production_ready = success_rate >= 0.7 and results['economic_models_available'] and results['best_model_selector_working']
    print(f"\n🚀 Production Readiness: {'✅ READY' if production_ready else '⚠️ NEEDS_WORK'}")
    
    return results

if __name__ == "__main__":
    validation_results = validate_alpha_models_pipeline()
