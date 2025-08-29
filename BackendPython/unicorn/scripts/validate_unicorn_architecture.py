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
        self.base_path = Path(base_path)
        self.required_numbered_dirs = {
            '1_data_sources', '2_alpha_models', '3_risk_management',
            '4_portfolio_construction', '5_execution_models', '6_algorithms'
        }
        self.required_root_files = {
            'ARCHITECTURE.md', 'README.md'
        }
        self.allowed_dirs = {
            'config', 'docs', 'legacy', 'scripts', 'tests'
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
        
        # Generate report
        return self._generate_report()
    
    def _validate_root_structure(self):
        """Validate root directory structure."""
        existing_items = set(item.name for item in self.base_path.iterdir())
        
        # Check for required numbered directories
        missing_numbered = self.required_numbered_dirs - existing_items
        if missing_numbered:
            self.errors.append(f"Missing required numbered directories: {missing_numbered}")
        
        # Check for required files
        missing_files = self.required_root_files - existing_items
        if missing_files:
            self.errors.append(f"Missing required root files: {missing_files}")
        
        # Check for scattered Python files (except __init__.py)
        scattered_files = []
        for item in self.base_path.iterdir():
            if (item.is_file() and 
                item.suffix == '.py' and 
                item.name not in {'__init__.py'}):
                scattered_files.append(item.name)
        
        if scattered_files:
            self.errors.append(f"Python files should be in numbered directories: {scattered_files}")
    
    def _validate_numbered_directories(self):
        """Validate numbered directory structure."""
        for dir_name in self.required_numbered_dirs:
            dir_path = self.base_path / dir_name
            if dir_path.exists():
                # Check if directory has content
                contents = list(dir_path.iterdir())
                if not contents:
                    self.warnings.append(f"Empty numbered directory: {dir_name}")
                elif dir_name == '2_alpha_models':
                    # Special validation for alpha models
                    self._validate_alpha_models_content(dir_path)
            else:
                self.errors.append(f"Missing numbered directory: {dir_name}")
    
    def _validate_no_legacy_dirs(self):
        """Check for legacy/redundant directories that should be moved."""
        legacy_dir_names = {
            'algorithms', 'alpha_models', 'backend', 'backtesting',
            'data', 'data_sources', 'eth_framework', 'framework', 'integrations'
        }
        
        existing_items = set(item.name for item in self.base_path.iterdir() if item.is_dir())
        found_legacy = legacy_dir_names & existing_items
        
        if found_legacy:
            self.errors.append(f"Legacy directories found (should be moved/deleted): {found_legacy}")
        
        # Check for directories not in allowed list
        all_allowed = self.required_numbered_dirs | self.allowed_dirs
        unexpected_dirs = existing_items - all_allowed
        
        if unexpected_dirs:
            self.warnings.append(f"Unexpected directories found: {unexpected_dirs}")
    
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
        
        # Check for model files
        models_path = asset_path / 'models'
        if models_path.exists():
            model_files = list(models_path.glob('*.pkl'))
            if not model_files:
                self.warnings.append(f"{asset_name}: No trained models (.pkl files) found")
    
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
        print("  1. Follow numbered directory structure (1-6)")
        print("  2. Keep legacy items in legacy/ directory")
        print("  3. Use asset-specific directories in 2_alpha_models/")
        print("  4. Ensure all Python files are in appropriate directories")
        print("  5. See ARCHITECTURE.md for complete guidelines")
        
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
        # Auto-detect if we're in the right directory
        current_path = Path.cwd()
        if current_path.name == 'unicorn' and (current_path / 'ARCHITECTURE.md').exists():
            base_path = str(current_path)
        elif (current_path / 'BackendPython' / 'unicorn' / 'ARCHITECTURE.md').exists():
            base_path = str(current_path / 'BackendPython' / 'unicorn')
        else:
            print("❌ Could not find unicorn directory with ARCHITECTURE.md")
            print("Usage: python3 validate_unicorn_architecture.py [path_to_unicorn_dir]")
            sys.exit(1)
    
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
