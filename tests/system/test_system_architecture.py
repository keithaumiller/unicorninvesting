#!/usr/bin/env python3
"""
System Architecture Validation Test
Tests the overall system architecture compliance and structure
"""

import sys
import os
import unittest
import subprocess

# Add paths for imports
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn')


class TestSystemArchitecture(unittest.TestCase):
    """Test cases for System Architecture validation"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.root_path = '/workspaces/unicorninvesting'
        self.unicorn_path = '/workspaces/unicorninvesting/BackendPython/unicorn'
    
    def test_directory_structure(self):
        """Test core directory structure exists"""
        required_dirs = [
            'BackendPython/unicorn/1_data_sources',
            'BackendPython/unicorn/2_alpha_models', 
            'BackendPython/unicorn/3_risk_algorithms',
            'BackendPython/unicorn/4_portfolios',
            'BackendPython/unicorn/5_execution_models',
            'BackendPython/unicorn/6_algorithms',
            'BackendPython/unicorn/4_portfolios/Myportolio',
            'BackendPython/unicorn/4_portfolios/utilities',
            'WebFrontend',
            'scripts',
            'tests',
            'docs'
        ]
        
        for dir_path in required_dirs:
            full_path = os.path.join(self.root_path, dir_path)
            self.assertTrue(os.path.exists(full_path), f"Required directory missing: {dir_path}")
    
    def test_myportolio_structure(self):
        """Test Myportolio directory structure"""
        myportolio_path = os.path.join(self.unicorn_path, '4_portfolios/Myportolio')
        
        required_subdirs = [
            'risk_algorithms',
            'trading_algorithms', 
            'utilities',
            'config'
        ]
        
        for subdir in required_subdirs:
            full_path = os.path.join(myportolio_path, subdir)
            self.assertTrue(os.path.exists(full_path), f"Myportolio subdir missing: {subdir}")
    
    def test_algorithm_separation(self):
        """Test that risk and trading algorithms are properly separated"""
        myportolio_path = os.path.join(self.unicorn_path, '4_portfolios/Myportolio')
        
        # Check risk algorithms directory
        risk_dir = os.path.join(myportolio_path, 'risk_algorithms')
        self.assertTrue(os.path.exists(risk_dir))
        
        # Check trading algorithms directory  
        trading_dir = os.path.join(myportolio_path, 'trading_algorithms')
        self.assertTrue(os.path.exists(trading_dir))
        
        # Verify they contain Python files
        risk_files = [f for f in os.listdir(risk_dir) if f.endswith('.py') and not f.startswith('__')]
        trading_files = [f for f in os.listdir(trading_dir) if f.endswith('.py') and not f.startswith('__')]
        
        self.assertGreater(len(risk_files), 0, "No risk algorithm files found")
        self.assertGreater(len(trading_files), 0, "No trading algorithm files found")
    
    def test_configuration_system(self):
        """Test configuration system exists"""
        config_path = os.path.join(self.unicorn_path, '4_portfolios/Myportolio/config')
        self.assertTrue(os.path.exists(config_path))
        
        # Check for ETH Kelly config
        eth_config = os.path.join(config_path, 'eth_kelly_config.json')
        self.assertTrue(os.path.exists(eth_config), "ETH Kelly config file missing")
    
    def test_documentation_standards(self):
        """Test documentation standards compliance"""
        key_dirs = [
            'BackendPython',
            'BackendPython/unicorn',
            'BackendPython/unicorn/4_portfolios/Myportolio',
            'WebFrontend',
            'scripts',
            'tests',
            'docs'
        ]
        
        for dir_path in key_dirs:
            full_path = os.path.join(self.root_path, dir_path)
            readme_path = os.path.join(full_path, 'README.md')
            
            if os.path.exists(full_path):
                self.assertTrue(os.path.exists(readme_path), 
                              f"README.md missing in {dir_path}")
    
    def test_forbidden_directories(self):
        """Test that forbidden directories don't exist"""
        forbidden_dirs = [
            'portfolios',  # Should use BackendPython/unicorn/4_portfolios/
            'BackendPython/unicorn/4_portfolios/BTC_ETH_Mixed',
            'BackendPython/unicorn/4_portfolios/ETH_Only',
            'BackendPython/unicorn/4_portfolios/shared_utilities'
        ]
        
        for forbidden_dir in forbidden_dirs:
            full_path = os.path.join(self.root_path, forbidden_dir)
            self.assertFalse(os.path.exists(full_path), 
                           f"Forbidden directory exists: {forbidden_dir}")


def run_architecture_test():
    """Run System Architecture tests with detailed output"""
    print('🏗️  SYSTEM ARCHITECTURE VALIDATION TEST')
    print('=' * 45)
    
    # Create test suite
    suite = unittest.TestLoader().loadTestsFromTestCase(TestSystemArchitecture)
    
    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Additional validation using architecture validator
    print('\n📊 ARCHITECTURE VALIDATOR CHECK')
    print('-' * 35)
    
    try:
        # Run the architecture validator script
        validator_path = '/workspaces/unicorninvesting/BackendPython/unicorn/scripts/validate_architecture.py'
        
        if os.path.exists(validator_path):
            result_proc = subprocess.run(
                ['python', validator_path],
                cwd='/workspaces/unicorninvesting/BackendPython/unicorn',
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result_proc.returncode == 0:
                print('✅ Architecture Validator: PASSED')
                # Print relevant output lines
                lines = result_proc.stdout.split('\n')
                for line in lines:
                    if 'compliant' in line.lower() or 'success' in line.lower():
                        print(f'   {line}')
            else:
                print('⚠️  Architecture Validator: WARNINGS')
                print(f'   {result_proc.stdout}')
        else:
            print('⚠️  Architecture validator script not found')
            
    except Exception as e:
        print(f'⚠️  Could not run architecture validator: {e}')
    
    # Manual directory check
    print('\n📁 DIRECTORY STRUCTURE CHECK')
    print('-' * 30)
    
    root_path = '/workspaces/unicorninvesting'
    key_paths = [
        'BackendPython/unicorn/4_portfolios/Myportolio',
        'BackendPython/unicorn/4_portfolios/utilities', 
        'scripts',
        'tests'
    ]
    
    for path in key_paths:
        full_path = os.path.join(root_path, path)
        if os.path.exists(full_path):
            file_count = len([f for f in os.listdir(full_path) if os.path.isfile(os.path.join(full_path, f))])
            print(f'✅ {path}: {file_count} files')
        else:
            print(f'❌ {path}: NOT FOUND')
    
    success = result.wasSuccessful()
    print(f'\n🏗️  SYSTEM ARCHITECTURE TEST: {"SUCCESS" if success else "FAILED"}')
    
    return success


if __name__ == "__main__":
    success = run_architecture_test()
    sys.exit(0 if success else 1)
