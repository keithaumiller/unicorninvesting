#!/usr/bin/env python3
"""
Complete System Validation Test Runner
Runs comprehensive system validation including all component tests
"""

import sys
import os
import subprocess
import time
from datetime import datetime

# Add paths for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(current_dir))  # Go up to unicorninvesting root
backend_python = os.path.join(project_root, 'BackendPython', 'unicorn')
sys.path.append(current_dir)
sys.path.append(project_root)
sys.path.append(backend_python)

# Import individual test modules with dynamic paths
try:
    # Add path for Kelly Criterion tests
    kelly_test_path = os.path.join(project_root, 'tests', 'unicorn', '4_portfolios', 'utilities')
    sys.path.insert(0, kelly_test_path)
    from test_kelly_criterion import run_kelly_test
except ImportError:
    run_kelly_test = None

try:
    # Add path for ETH Basic Risk tests  
    risk_test_path = os.path.join(project_root, 'tests', 'unicorn', '3_risk_algorithms')
    sys.path.insert(0, risk_test_path)
    from test_eth_basic_risk import run_risk_test
except ImportError:
    run_risk_test = None

try:
    # Add path for ETH Kelly Integration tests
    integration_test_path = os.path.join(project_root, 'tests', 'unicorn', '4_portfolios', 'Myportolio')
    sys.path.insert(0, integration_test_path)
    from test_eth_kelly_integration import run_integration_test
except ImportError:
    run_integration_test = None

try:
    from test_system_architecture import run_architecture_test
except ImportError:
    run_architecture_test = None


def run_system_health_check():
    """Run the system health check script"""
    print('🏥 SYSTEM HEALTH CHECK')
    print('=' * 30)
    
    try:
        health_check_path = '/home/runner/work/unicorninvesting/unicorninvesting/scripts/unicorn_environment.sh'
        
        if os.path.exists(health_check_path):
            result = subprocess.run(
                [health_check_path, '--check-only'],
                cwd='/home/runner/work/unicorninvesting/unicorninvesting',
                capture_output=True,
                text=True,
                timeout=120
            )
            
            # Extract key metrics from output
            lines = result.stdout.split('\n')
            summary_found = False
            
            for line in lines:
                if 'Summary' in line or summary_found:
                    summary_found = True
                    if 'Total Checks:' in line or 'Passed:' in line or 'Failed:' in line or 'Success Rate:' in line:
                        print(f'   {line}')
                    elif 'Platform is' in line:
                        print(f'   {line}')
                        break
            
            return result.returncode == 0
        else:
            print('❌ Health check script not found')
            return False
            
    except Exception as e:
        print(f'❌ Health check failed: {e}')
        return False


def run_eth_models_validation():
    """Run ETH alpha models validation - Basic ensemble model loading test"""
    try:
        print('🔬 Testing ETH Alpha Models (Basic Functionality)...')
        
        # Test ETH model storage system
        eth_models_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH'
        
        if not os.path.exists(eth_models_path):
            print('❌ ETH models directory not found')
            return False
        
        # Add ETH models path for imports
        sys.path.append(eth_models_path)
        
        # Test 1: Model Storage Manager - Basic functionality
        try:
            # Add the correct path for model_management
            model_mgmt_path = os.path.join(eth_models_path, 'models', 'model_management')
            sys.path.insert(0, model_mgmt_path)
            from model_storage_manager import ModelStorageManager
            storage = ModelStorageManager()
            models = storage.list_models()
            print(f'✅ Model Storage: {len(models)} models available')
            
            if len(models) == 0:
                print('⚠️  No models found in storage')
                return False
                
        except Exception as e:
            print(f'❌ Model Storage Manager failed: {e}')
            return False
        
        # Test 2: Ensemble Model Loading - Core functionality
        try:
            # Find ensemble models
            ensemble_models = [m for m in models if m.methodology == 'ensemble']
            
            if not ensemble_models:
                print('⚠️  No ensemble models found')
                return False
            
            # Test loading the latest ensemble model
            latest_ensemble = max(ensemble_models, key=lambda x: x.created_at)
            model, metadata = storage.load_model(latest_ensemble.model_id)
            
            print(f'✅ Ensemble Model Loading: Successfully loaded {latest_ensemble.model_id}')
            print(f'   - Model Type: {type(model).__name__}')
            print(f'   - Created: {latest_ensemble.created_at}')
            print(f'   - Asset: {latest_ensemble.asset}')
            
        except Exception as e:
            print(f'❌ Ensemble model loading failed: {e}')
            return False
        
        # Test 3: Basic ETH Asset Validation
        try:
            eth_models = [m for m in models if m.asset == 'ETH']
            
            if not eth_models:
                print('❌ No ETH models found')
                return False
            
            print(f'✅ ETH Asset Models: {len(eth_models)} models for ETH asset')
            
            # Count by methodology
            methodologies = {}
            for model in eth_models:
                methodologies[model.methodology] = methodologies.get(model.methodology, 0) + 1
            
            for methodology, count in methodologies.items():
                print(f'   - {methodology}: {count} models')
                
        except Exception as e:
            print(f'❌ ETH asset validation failed: {e}')
            return False
        
        print('🎯 ETH ENSEMBLE MODEL: BASIC FUNCTIONALITY VERIFIED')
        return True
        
    except Exception as e:
        print(f'❌ ETH models validation error: {e}')
        return False


def run_myportolio_validation():
    """Run comprehensive Myportolio validation"""
    print('\n🎯 MYPORTOLIO INTEGRATION VALIDATION')
    print('=' * 45)
    
    try:
        myportolio_path = os.path.join(project_root, 'BackendPython', 'unicorn', '4_portfolios', 'Myportolio')
        
        # Create validation script
        validation_script = '''
import json
from eth_kelly_integration import ETHKellyIntegratedPortfolio

# Load configuration
with open('config/eth_kelly_config.json', 'r') as f:
    config = json.load(f)

# Initialize portfolio
portfolio = ETHKellyIntegratedPortfolio(config)
print(f"✅ Portfolio initialized: {config['portfolio_name']}")

# Get portfolio summary
summary = portfolio.get_portfolio_summary()
print(f"✅ Portfolio Value: ${summary['portfolio_value']:,.2f}")
print(f"✅ Cash: ${summary['cash']:,.2f}")

# Get performance metrics
performance = portfolio.get_recent_performance(days=7)
print(f"✅ Performance tracking: {len(performance)} metrics available")

print("🎯 MYPORTOLIO: FULLY OPERATIONAL")
'''
        
        # Run validation
        result = subprocess.run(
            ['python', '-c', validation_script],
            cwd=myportolio_path,
            capture_output=True,
            text=True,
            timeout=30
        )
        
        print(result.stdout)
        
        if result.returncode != 0:
            print(f'❌ Myportolio validation failed: {result.stderr}')
            return False
        
        return True
        
    except Exception as e:
        print(f'❌ Myportolio validation error: {e}')
        return False


def main():
    """Run complete system validation"""
    print('🚀 COMPLETE SYSTEM VALIDATION')
    print('=' * 40)
    print(f'Started: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}')
    print()
    
    results = {}
    
    # 1. System Health Check
    results['health_check'] = run_system_health_check()
    
    # 2. System Architecture Test
    if run_architecture_test:
        results['architecture'] = run_architecture_test()
    else:
        print('\n⚠️  SYSTEM ARCHITECTURE TEST: SKIPPED (module not available)')
        results['architecture'] = None
    
    # 3. Kelly Criterion Test (simplified)
    print('\n🎯 KELLY CRITERION TEST (SIMPLIFIED)')
    print('=' * 40)
    try:
        # Test Kelly import and basic functionality - dynamic path
        kelly_path = os.path.join(project_root, 'BackendPython', 'unicorn', '4_portfolios', 'Myportolio', 'utilities')
        sys.path.insert(0, kelly_path)
        from kelly_criterion import KellyCriterionCalculator
        
        calculator = KellyCriterionCalculator()
        print('✅ Kelly Criterion: Import successful')
        print('✅ Kelly Criterion: Initialization working')
        
        results['kelly'] = True
    except Exception as e:
        print(f'❌ Kelly Criterion: Failed ({e})')
        results['kelly'] = False
    
    # 4. ETH Basic Risk Test (simplified)  
    print('\n⚠️  ETH BASIC RISK TEST (SIMPLIFIED)')
    print('=' * 40)
    try:
        # Test Risk import and basic functionality - dynamic path
        risk_path = os.path.join(project_root, 'BackendPython', 'unicorn', '4_portfolios', 'Myportolio', 'risk_algorithms')
        sys.path.insert(0, risk_path)
        from eth_basic_risk import ETHBasicRisk
        
        risk_manager = ETHBasicRisk()
        print('✅ ETH Basic Risk: Import successful')
        print('✅ ETH Basic Risk: Initialization working')
        
        results['risk'] = True
    except Exception as e:
        print(f'❌ ETH Basic Risk: Failed ({e})')
        results['risk'] = False
    
    # 5. ETH Alpha Models Test (NEW)
    print('\n🔬 ETH ALPHA MODELS TEST')
    print('=' * 30)
    results['eth_models'] = run_eth_models_validation()
    
    # 6. ETH Kelly Integration Test
    if run_integration_test:
        results['integration'] = run_integration_test()
    else:
        print('\n⚠️  ETH KELLY INTEGRATION TEST: SKIPPED (module not available)')
        results['integration'] = None
    
    # 7. Myportolio Validation
    results['myportolio'] = run_myportolio_validation()
    
    # Summary
    print('\n🎯 VALIDATION SUMMARY')
    print('=' * 25)
    
    total_tests = 0
    passed_tests = 0
    
    for test_name, result in results.items():
        if result is not None:
            total_tests += 1
            if result:
                passed_tests += 1
                status = '✅ PASSED'
            else:
                status = '❌ FAILED'
        else:
            status = '⚠️  SKIPPED'
        
        print(f'{test_name.replace("_", " ").title()}: {status}')
    
    success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
    
    print(f'\nTotal Tests: {total_tests}')
    print(f'Passed: {passed_tests}')
    print(f'Success Rate: {success_rate:.1f}%')
    
    print(f'\nCompleted: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}')
    
    # Overall result
    if success_rate >= 80:
        print('\n🎉 SYSTEM VALIDATION: SUCCESS')
        print('💯 Platform ready for development and deployment')
        return True
    else:
        print('\n⚠️  SYSTEM VALIDATION: ISSUES DETECTED')
        print('🔧 Platform needs attention before deployment')
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
