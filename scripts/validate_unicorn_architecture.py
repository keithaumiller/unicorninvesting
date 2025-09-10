#!/usr/bin/env python3
"""
Unicorn Architecture Validation Script

Validates that the entire unicorn backend follows the established clean architecture.
Run this as part of startup checks to ensure compliance.
"""

import os
import sys
from pathlib import Path
from typing import List, Dict, Set

class UnicornArchitectureValidator:
    """Validates entire unicorn backend architecture compliance."""
    
    def __init__(self, base_path: str = "."):
        self.base_path = Path(base_path).resolve()
        self.unicorn_backend_path = self.base_path / "BackendPython" / "unicorn"
        
        # Updated to support full LEAN 6-layer architecture
        self.required_numbered_dirs = {
            '1_data_sources', '2_alpha_models', '3_risk_management', 
            '4_portfolios', '5_execution_models', '6_algorithms'
        }
        self.required_backend_files = {
            'ARCHITECTURE.md', 'README.md'
        }
        self.allowed_backend_dirs = {
            'config', 'docs', 'legacy', 'scripts', 'tests', 'examples'
        }
        self.allowed_root_dirs = {
            'BackendPython', 'WebFrontend', 'config', 'deployment', 
            'docs', 'scripts', 'tests', '.git', '.github', '.vscode',
            '.devcontainer', 'node_modules', '.venv', '__pycache__'
        }
        self.allowed_root_files = {
            'README.md', 'LICENSE', 'INSTALLATION.md', 'CONTRIBUTING.md',
            'SECURITY.md', 'TERMS_OF_SERVICE.md', 'DISCLAIMER.md',
            '.gitignore', '.gitmodules', 'deploy.yml'
        }
        self.errors = []
        self.warnings = []
        
    def validate_structure(self) -> Dict[str, any]:
        """Run complete architecture validation."""
        print("🏗️ Validating Unicorn Backend Architecture")
        print("=" * 50)
        
        # Check root structure
        self._validate_root_structure()
        
        # Check numbered directories
        self._validate_numbered_directories()
        
        # Check for legacy/redundant directories
        self._validate_no_legacy_dirs()
        
        # Check for misplaced test/debug files
        self._validate_no_misplaced_files()
        
        # Check documentation standards
        self._validate_documentation_standards()
        
        # Generate report
        return self._generate_report()
    
    def _validate_root_structure(self):
        """Validate root directory structure."""
        # Validate main project root structure - directories
        existing_root_dirs = set(item.name for item in self.base_path.iterdir() if item.is_dir())
        existing_root_files = set(item.name for item in self.base_path.iterdir() if item.is_file())
        
        # Check for unexpected root directories
        unexpected_root_dirs = existing_root_dirs - self.allowed_root_dirs
        if unexpected_root_dirs:
            self.errors.append(f"Unexpected root directories found: {unexpected_root_dirs}")
        
        # Check for unexpected root files
        unexpected_root_files = existing_root_files - self.allowed_root_files
        if unexpected_root_files:
            self.errors.append(f"Unexpected root files found: {unexpected_root_files}")
        
        # Check that BackendPython/unicorn exists
        if not self.unicorn_backend_path.exists():
            self.errors.append(f"Missing BackendPython/unicorn directory")
            return
            
        # Validate unicorn backend structure
        existing_backend_items = set(item.name for item in self.unicorn_backend_path.iterdir())
        
        # Check for required numbered directories in backend
        missing_numbered = self.required_numbered_dirs - existing_backend_items
        if missing_numbered:
            self.errors.append(f"Missing required numbered directories in BackendPython/unicorn: {missing_numbered}")
        
        # Check for required backend files
        missing_backend_files = self.required_backend_files - existing_backend_items
        if missing_backend_files:
            self.errors.append(f"Missing required backend files in BackendPython/unicorn: {missing_backend_files}")
        
        # Check for unexpected backend directories
        backend_dirs = set(item.name for item in self.unicorn_backend_path.iterdir() if item.is_dir())
        expected_backend_dirs = self.required_numbered_dirs | self.allowed_backend_dirs
        unexpected_backend_dirs = backend_dirs - expected_backend_dirs
        
        if unexpected_backend_dirs:
            self.warnings.append(f"Unexpected backend directories found: {unexpected_backend_dirs}")
    
    def _validate_numbered_directories(self):
        """Validate numbered directories follow clean architecture."""
        for dir_name in self.required_numbered_dirs:
            dir_path = self.unicorn_backend_path / dir_name
            if dir_path.exists():
                # Check if directory has content
                contents = list(dir_path.iterdir())
                if not contents:
                    self.warnings.append(f"Empty numbered directory: {dir_name}")
                elif dir_name == '2_alpha_models':
                    # Special validation for alpha models
                    self._validate_alpha_models_content(dir_path)
                elif dir_name == '4_portfolios':
                    # Special validation for portfolio structure
                    self._validate_portfolio_structure(dir_path)
                elif dir_name == '3_risk_management':
                    # Special validation for risk management structure
                    self._validate_risk_management_structure(dir_path)
            else:
                self.errors.append(f"Missing numbered directory: {dir_name}")
    
    def _validate_portfolio_structure(self, portfolio_dir: Path):
        """Validate the portfolio directory structure."""
        # Check for Myportolio directory
        myportolio_path = portfolio_dir / 'Myportolio'
        if not myportolio_path.exists():
            self.errors.append("Missing Myportolio directory in 4_portfolios")
            return
        
        # Check for required subdirectories in Myportolio
        required_subdirs = {'risk_algorithms', 'trading_algorithms'}
        existing_subdirs = set(item.name for item in myportolio_path.iterdir() if item.is_dir())
        missing_subdirs = required_subdirs - existing_subdirs
        
        if missing_subdirs:
            self.errors.append(f"Missing algorithm directories in Myportolio: {missing_subdirs}")
        
        # Check for utilities directory
        utilities_path = portfolio_dir / 'utilities'
        if not utilities_path.exists():
            self.warnings.append("Missing utilities directory in 4_portfolios")
        
        # Check for configuration files
        config_files = {'config.json', 'risk_parameters.json', 'execution_settings.json'}
        existing_files = set(item.name for item in myportolio_path.iterdir() if item.is_file())
        missing_configs = config_files - existing_files
        
        if missing_configs:
            self.warnings.append(f"Missing configuration files in Myportolio: {missing_configs}")
    
    def _validate_risk_management_structure(self, risk_management_dir: Path):
        """Validate the 3_risk_management directory structure."""
        # Also check portfolio-specific risk algorithms in Myportolio
        myportolio_risk_dir = self.base_path / '4_portfolios' / 'Myportolio' / 'risk_algorithms'
        
        expected_methodologies = {'kelly_criterion', 'basic_risk', 'var_models', 'monte_carlo'}
        
        # Check in both 3_risk_management and Myportolio/risk_algorithms
        found_methodologies = set()
        
        # Check 3_risk_management
        if risk_management_dir.exists():
            existing_items = set(item.name for item in risk_management_dir.iterdir() if item.is_dir())
            existing_methodologies = existing_items - {'shared', 'utilities', 'legacy'}
            found_methodologies.update(existing_methodologies)
        
        # Check Myportolio risk_algorithms directory
        if myportolio_risk_dir.exists():
            myportolio_items = set(item.name for item in myportolio_risk_dir.iterdir() if item.is_dir())
            myportolio_methodologies = myportolio_items & expected_methodologies
            found_methodologies.update(myportolio_methodologies)
            
            # Check for Python implementation files in Myportolio
            for methodology in myportolio_methodologies:
                methodology_path = myportolio_risk_dir / methodology
                py_files = list(methodology_path.glob('*.py'))
                if not py_files:
                    self.warnings.append(f"No Python implementation files in Myportolio/risk_algorithms/{methodology}")
        
        missing_methodologies = expected_methodologies - found_methodologies
        if missing_methodologies:
            self.warnings.append(f"Missing risk management methodologies: {missing_methodologies}")
    
    def _validate_no_legacy_dirs(self):
        """Check for legacy/redundant directories that should be moved."""
        # Check at root level for legacy directories
        root_legacy_dir_names = {
            'algorithms', 'alpha_models', 'backend', 'backtesting',
            'data', 'data_sources', 'eth_framework', 'framework', 'integrations',
            'portfolios'  # Root-level portfolios directory is not allowed
        }
        
        existing_root_items = set(item.name for item in self.base_path.iterdir() if item.is_dir())
        found_root_legacy = root_legacy_dir_names & existing_root_items
        
        if found_root_legacy:
            self.errors.append(f"Legacy directories found at root (should be moved/deleted): {found_root_legacy}")
        
        # Check within BackendPython/unicorn for legacy directories
        backend_legacy_dir_names = {
            '4_portfolio_construction',  # Should be 4_portfolios
            'portfolio_construction'     # Should be 4_portfolios
        }
        
        if self.unicorn_backend_path.exists():
            existing_backend_items = set(item.name for item in self.unicorn_backend_path.iterdir() if item.is_dir())
            found_backend_legacy = backend_legacy_dir_names & existing_backend_items
            
            if found_backend_legacy:
                self.errors.append(f"Legacy directories found in backend (should be moved/deleted): {found_backend_legacy}")
    
    def _validate_no_misplaced_files(self):
        """Validate that test/debug files are not in the root directory."""
        print("🧪 Checking for misplaced test and debug files...")
        
        # Define patterns for files that should not be in root
        test_file_patterns = {
            'test_', 'debug_', '*_test.py', '*_debug.py',
            '*validation*.json', '*_results*.json', '*_simulation*.php'
        }
        
        # Define specific file patterns that indicate test/debug files
        disallowed_root_patterns = [
            'test_*', 'debug_*', '*_test.*', '*_debug.*',
            '*validation*results*.json', '*simulation*.php',
            '*simulation*.py'
        ]
        
        existing_root_files = [item.name for item in self.base_path.iterdir() if item.is_file()]
        misplaced_files = []
        
        for file_name in existing_root_files:
            # Check if file matches any disallowed pattern
            for pattern in disallowed_root_patterns:
                import fnmatch
                if fnmatch.fnmatch(file_name, pattern):
                    misplaced_files.append(file_name)
                    break
        
        if misplaced_files:
            self.errors.append(f"Test/debug files found in root (should be in tests/): {misplaced_files}")
        
        # Check if test files are properly organized
        tests_dir = self.base_path / 'tests'
        if tests_dir.exists():
            # Check for proper test organization
            test_subdirs = ['simulation_frontend', 'debug', 'validation_results']
            missing_test_dirs = []
            
            for subdir in test_subdirs:
                subdir_path = tests_dir / subdir
                if not subdir_path.exists():
                    missing_test_dirs.append(subdir)
            
            if missing_test_dirs:
                self.warnings.append(f"Missing test organization directories: {missing_test_dirs}")
    
    def _validate_alpha_models_content(self, alpha_models_path: Path):
        """Validate alpha models directory structure."""
        # Check for CRYPTO structure
        crypto_path = alpha_models_path / 'CRYPTO'
        if crypto_path.exists():
            # Check for BTC and ETH
            btc_path = crypto_path / 'BTC'
            eth_path = crypto_path / 'ETH'
            
            if btc_path.exists():
                self._validate_asset_structure(btc_path, 'BTC')
            else:
                self.warnings.append("Missing CRYPTO/BTC directory")
                
            if eth_path.exists():
                self._validate_asset_structure(eth_path, 'ETH')
            else:
                self.warnings.append("Missing CRYPTO/ETH directory")
        else:
            self.warnings.append("Missing 2_alpha_models/CRYPTO directory")
    
    def _validate_asset_structure(self, asset_path: Path, asset_name: str):
        """Validate individual asset directory structure."""
        required_subdirs = {'models', 'algorithms', 'features', 'research'}
        existing_subdirs = set(item.name for item in asset_path.iterdir() if item.is_dir())
        
        missing_subdirs = required_subdirs - existing_subdirs
        if missing_subdirs:
            self.warnings.append(f"{asset_name}: Missing subdirectories: {missing_subdirs}")
        
        # Check for ensemble model files in model_storage/ensemble directory
        ensemble_path = asset_path / 'model_storage' / 'ensemble'
        if ensemble_path.exists():
            model_files = list(ensemble_path.glob('*.pkl'))
            if not model_files:
                self.warnings.append(f"{asset_name}: No trained models (.pkl files) found")
        else:
            # Check legacy models path for backward compatibility
            models_path = asset_path / 'models'
            if models_path.exists():
                model_files = list(models_path.glob('*.pkl'))
                if not model_files:
                    self.warnings.append(f"{asset_name}: No trained models (.pkl files) found")
            else:
                self.warnings.append(f"{asset_name}: No trained models (.pkl files) found")
    
    def _validate_documentation_standards(self):
        """Validate that all .md files follow documentation standards."""
        print("📚 Checking documentation standards...")
        
        # Find all .md files in the directory tree
        md_files = []
        
        # Check the unicorn backend directory
        for root, dirs, files in os.walk(self.unicorn_backend_path):
            # Skip legacy directory
            if 'legacy' in Path(root).parts:
                continue
            
            for file in files:
                if file.endswith('.md'):
                    file_path = Path(root) / file
                    relative_path = file_path.relative_to(self.unicorn_backend_path)
                    md_files.append((file, relative_path, 'backend'))
        
        # Check the repository root for governance files
        for file in self.base_path.iterdir():
            if file.is_file() and file.name.endswith('.md'):
                md_files.append((file.name, file.name, 'root'))
        
        # Check each .md file against our standards
        allowed_md_files = {'README.md', 'ARCHITECTURE.md'}
        
        # Allow specific documentation files in certain contexts
        allowed_specific_files = {
            'LIVE_TRADING_READY.md',  # Deployment documentation
            'STORAGE_IMPLEMENTATION_SUMMARY.md',  # Implementation summaries
            'DIRECTORY_OVERVIEW.md',  # Directory documentation
            'IMPLEMENTATION_SUMMARY.md',  # Implementation summaries
            'INSTALLATION.md'  # Installation guide (too detailed for README.md)
        }
        
        # Standard repository governance files (allowed at root level)
        governance_files = {
            'DISCLAIMER.md',
            'SECURITY.md', 
            'CONTRIBUTING.md',
            'TERMS_OF_SERVICE.md',
            'LICENSE.md',
            'CODE_OF_CONDUCT.md'
        }
        
        # Root-level files that should be consolidated into README.md
        root_level_violations = {
            'CRITICAL_PATH_TO_LIVE_TRADING.md',
            'IBKR_ETH_DATA_CAPABILITIES.md', 
            'SYSTEM_TEST_RESULTS.md'
        }
        
        violations = []
        root_consolidation_needed = []
        
        for filename, relative_path, location in md_files:
            if location == 'root' and filename in root_level_violations:
                root_consolidation_needed.append(str(relative_path))
            elif filename not in allowed_md_files and filename not in allowed_specific_files and filename not in governance_files:
                violations.append(str(relative_path))
        
        if violations:
            self.errors.append(f"Documentation standard violation: Only README.md and ARCHITECTURE.md files are allowed. Found: {violations}")
        
        if root_consolidation_needed:
            self.errors.append(f"Root-level documentation files should be consolidated into README.md: {root_consolidation_needed}")
            self.errors.append("These files contain project status information that belongs in the main README.md")
        
        # Check for missing README.md files in key directories
        key_directories = [
            self.base_path,  # Project root should have README.md
            self.unicorn_backend_path,  # Backend root should have README.md
            self.unicorn_backend_path / '1_data_sources',
            self.unicorn_backend_path / '2_alpha_models',
            self.unicorn_backend_path / '3_risk_management', 
            self.unicorn_backend_path / '4_portfolios',
            self.unicorn_backend_path / '5_execution_models'
        ]
        
        for dir_path in key_directories:
            if dir_path.exists():
                readme_path = dir_path / 'README.md'
                if not readme_path.exists():
                    relative_path = dir_path.relative_to(self.base_path) if dir_path != self.base_path else "root"
                    self.warnings.append(f"Missing README.md in key directory: {relative_path}")
        
        total_violations = len(violations) + len(root_consolidation_needed)
        print(f"   Found {len(md_files)} .md files, {total_violations} violations")
    
    def _generate_report(self) -> Dict[str, any]:
        """Generate validation report."""
        print("\n📋 Validation Results:")
        print("=" * 30)
        
        total_issues = len(self.errors) + len(self.warnings)
        
        if self.errors:
            print("🚨 ERRORS (must fix):")
            for error in self.errors:
                print(f"  - {error}")
        
        if self.warnings:
            print("\n⚠️  WARNINGS (recommended fixes):")
            for warning in self.warnings:
                print(f"  - {warning}")
        
        if not total_issues:
            print("✅ Architecture is fully compliant!")
            compliance_status = "FULLY_COMPLIANT"
        elif self.errors:
            print("\n❌ Architecture violations found!")
            compliance_status = "NON_COMPLIANT"
        else:
            print("\n⚠️  Architecture is mostly compliant with warnings.")
            compliance_status = "COMPLIANT_WITH_WARNINGS"
        
        print("\n💡 RECOMMENDATIONS:")
        print("  1. Follow clean numbered directory structure: 1_data_sources, 2_alpha_models, 3_risk_management, 4_portfolios")
        print("  2. Keep legacy items in legacy/ directory")
        print("  3. Use asset-specific directories in 2_alpha_models/")
        print("  4. Risk management belongs in 3_risk_management/ organized by methodology")
        print("  5. Trading algorithms belong in 4_portfolios/Myportolio/trading_algorithms/")
        print("  6. Documentation: Use only README.md and ARCHITECTURE.md files (with exceptions for deployment docs)")
        print("  7. Consolidate all documentation content into appropriate README.md files")
        print("  8. See ARCHITECTURE.md and component README.md files for complete guidelines")
        
        return {
            'status': compliance_status,
            'errors': self.errors,
            'warnings': self.warnings,
            'total_issues': total_issues
        }

def main():
    """Main entry point."""
    # Determine base path - should be /workspaces/unicorninvesting/BackendPython/unicorn
    if len(sys.argv) > 1:
        base_path = sys.argv[1]
    else:
        # Auto-detect based on current working directory
        current_path = Path.cwd()
        
        # Check if we're in the project root or can find it
        if (current_path / 'BackendPython' / 'unicorn' / 'ARCHITECTURE.md').exists():
            base_path = str(current_path)
        elif current_path.name == 'unicorninvesting' and (current_path / 'BackendPython' / 'unicorn' / 'ARCHITECTURE.md').exists():
            base_path = str(current_path)
        elif (current_path.parent / 'BackendPython' / 'unicorn' / 'ARCHITECTURE.md').exists():
            base_path = str(current_path.parent)
        else:
            print("❌ Could not find project root with BackendPython/unicorn/ARCHITECTURE.md")
            print("Usage: python3 validate_unicorn_architecture.py [path_to_project_root]")
            sys.exit(1)
    
    validator = UnicornArchitectureValidator(base_path)
    
    validator = UnicornArchitectureValidator(base_path)
    result = validator.validate_structure()
    
    # Return appropriate exit code
    if result['status'] == 'FULLY_COMPLIANT':
        sys.exit(0)
    elif result['status'] == 'COMPLIANT_WITH_WARNINGS':
        sys.exit(1)  # Warnings but acceptable
    else:
        sys.exit(2)  # Errors that must be fixed

if __name__ == "__main__":
    main()
