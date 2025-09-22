#!/usr/bin/env python3
"""
Architecture Validation Script

Validates that the alpha_models directory structure follows the established architecture.
Run this before committing to ensure compliance.
"""

import os
import sys
from pathlib import Path
from typing import List, Dict, Set

class ArchitectureValidator:
    """Validates alpha models directory architecture compliance."""
    
    def __init__(self, base_path: str = "."):
        self.base_path = Path(base_path)
        self.required_asset_subdirs = {
            'models', 'algorithms', 'tests', 'scripts', 'features', 'research'
        }
        self.valid_asset_classes = {
            'CRYPTO', 'EQUITIES', 'FOREX', 'COMMODITIES', 'BONDS'
        }
        self.errors = []
        self.warnings = []
        
    def validate_structure(self) -> Dict[str, any]:
        """Run complete architecture validation."""
        print("🏗️ Validating Alpha Models Architecture")
        print("=" * 50)
        
        # Check root structure
        self._validate_root_structure()
        
        # Check asset class directories
        self._validate_asset_classes()
        
        # Check shared components
        self._validate_shared_components()
        
        # Check utils
        self._validate_utils()
        
        # Generate report
        return self._generate_report()
    
    def _validate_root_structure(self):
        """Validate root directory structure."""
        required_root_items = {
            'ARCHITECTURE.md', 'README.md', '__init__.py',
            'shared', 'utils', 'examples', 'legacy'
        }
        
        existing_items = set(item.name for item in self.base_path.iterdir())
        
        # Check for required items
        missing_items = required_root_items - existing_items
        if missing_items:
            self.errors.append(f"Missing required root items: {missing_items}")
        
        # Check for scattered files that should be in asset directories
        scattered_files = []
        for item in self.base_path.iterdir():
            if (item.is_file() and 
                item.suffix == '.py' and 
                item.name not in {'__init__.py'} and
                not item.name.startswith('README')):
                scattered_files.append(item.name)
        
        if scattered_files:
            self.errors.append(f"Python files should be in asset directories: {scattered_files}")
    
    def _validate_asset_classes(self):
        """Validate asset class directories."""
        asset_class_dirs = [
            item for item in self.base_path.iterdir() 
            if item.is_dir() and item.name.isupper() and item.name not in {'__pycache__'}
        ]
        
        for asset_class_dir in asset_class_dirs:
            if asset_class_dir.name not in self.valid_asset_classes:
                self.warnings.append(f"Non-standard asset class: {asset_class_dir.name}")
            
            self._validate_asset_class_content(asset_class_dir)
    
    def _validate_asset_class_content(self, asset_class_dir: Path):
        """Validate content of an asset class directory."""
        # Check for individual asset directories
        asset_dirs = [
            item for item in asset_class_dir.iterdir()
            if item.is_dir() and not item.name.startswith('__')
        ]
        
        if not asset_dirs:
            self.warnings.append(f"Empty asset class directory: {asset_class_dir.name}")
            return
        
        # Validate each asset directory
        for asset_dir in asset_dirs:
            self._validate_asset_directory(asset_dir, asset_class_dir.name)
    
    def _validate_asset_directory(self, asset_dir: Path, asset_class: str):
        """Validate individual asset directory structure."""
        existing_subdirs = set(
            item.name for item in asset_dir.iterdir() 
            if item.is_dir() and not item.name.startswith('__')
        )
        
        # Check required subdirectories
        missing_subdirs = self.required_asset_subdirs - existing_subdirs
        if missing_subdirs:
            self.warnings.append(
                f"{asset_class}/{asset_dir.name} missing subdirectories: {missing_subdirs}"
            )
        
        # Check for model files in the asset directory (should be in models/)
        model_files = [
            item.name for item in asset_dir.iterdir()
            if item.is_file() and item.suffix == '.py' and 'model' in item.name.lower()
        ]
        
        if model_files:
            self.errors.append(
                f"{asset_class}/{asset_dir.name} has model files in root, should be in models/: {model_files}"
            )
        
        # Validate models directory
        models_dir = asset_dir / 'models'
        if models_dir.exists():
            self._validate_models_directory(models_dir, asset_dir.name, asset_class)
    
    def _validate_models_directory(self, models_dir: Path, asset_name: str, asset_class: str):
        """Validate models directory content."""
        model_files = [
            item.name for item in models_dir.iterdir()
            if item.is_file() and item.suffix == '.py'
        ]
        
        # Check for expected model types
        expected_patterns = [
            f"{asset_name.lower()}_alpha.py",
            f"{asset_name.lower()}_prophet.py", 
            f"{asset_name.lower()}_xgboost.py",
            f"{asset_name.lower()}_ensemble.py"
        ]
        
        missing_models = []
        for pattern in expected_patterns:
            if pattern not in model_files:
                missing_models.append(pattern)
        
        if missing_models:
            self.warnings.append(
                f"{asset_class}/{asset_name} missing model types: {missing_models}"
            )
    
    def _validate_shared_components(self):
        """Validate shared components directory."""
        shared_dir = self.base_path / 'shared'
        if not shared_dir.exists():
            self.errors.append("Missing shared/ directory")
            return
        
        required_shared_files = {
            'base_alpha.py', 'testing_framework.py', 
            'model_framework.py', 'performance_tracker.py'
        }
        
        existing_files = set(
            item.name for item in shared_dir.iterdir()
            if item.is_file() and item.suffix == '.py'
        )
        
        missing_files = required_shared_files - existing_files
        if missing_files:
            self.errors.append(f"Missing shared framework files: {missing_files}")
    
    def _validate_utils(self):
        """Validate utils directory."""
        utils_dir = self.base_path / 'utils'
        if not utils_dir.exists():
            self.errors.append("Missing utils/ directory")
            return
        
        # Check for asset generators
        generator_files = [
            item.name for item in utils_dir.iterdir()
            if 'generator' in item.name and item.suffix == '.py'
        ]
        
        if not generator_files:
            self.warnings.append("No asset generator found in utils/")
    
    def _generate_report(self) -> Dict[str, any]:
        """Generate validation report."""
        print("\\n📋 Validation Results:")
        print("=" * 30)
        
        if not self.errors and not self.warnings:
            print("✅ Architecture fully compliant!")
            compliance_status = "COMPLIANT"
        elif self.errors:
            print("❌ Architecture violations found!")
            compliance_status = "NON_COMPLIANT"
        else:
            print("⚠️ Architecture mostly compliant with warnings")
            compliance_status = "COMPLIANT_WITH_WARNINGS"
        
        if self.errors:
            print("\\n🚨 ERRORS (must fix):")
            for error in self.errors:
                print(f"  - {error}")
        
        if self.warnings:
            print("\\n⚠️ WARNINGS (should fix):")
            for warning in self.warnings:
                print(f"  - {warning}")
        
        # Architecture recommendations
        if compliance_status != "COMPLIANT":
            print("\\n💡 RECOMMENDATIONS:")
            print("  1. Use enhanced_asset_generator.py for new assets")
            print("  2. Move scattered files to appropriate asset directories")
            print("  3. Ensure all assets have required subdirectories")
            print("  4. Follow naming conventions for model files")
            print("  5. See ARCHITECTURE.md for complete guidelines")
        
        return {
            'status': compliance_status,
            'errors': self.errors,
            'warnings': self.warnings,
            'error_count': len(self.errors),
            'warning_count': len(self.warnings)
        }

def main():
    """Run architecture validation."""
    if len(sys.argv) > 1:
        base_path = sys.argv[1]
    else:
        base_path = "."
    
    validator = ArchitectureValidator(base_path)
    result = validator.validate_structure()
    
    # Exit code based on compliance
    if result['status'] == "NON_COMPLIANT":
        sys.exit(1)
    elif result['status'] == "COMPLIANT_WITH_WARNINGS":
        sys.exit(2)
    else:
        sys.exit(0)

if __name__ == "__main__":
    main()
