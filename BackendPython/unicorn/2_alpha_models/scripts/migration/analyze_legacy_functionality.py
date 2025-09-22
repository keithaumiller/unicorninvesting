#!/usr/bin/env python3
"""
Legacy Functionality Analysis Script

This script analyzes all legacy files to extract functionality and create 
corresponding implementations in the new methodology-first architecture.
"""

import os
import ast
import re
from pathlib import Path
from typing import Dict, List, Set, Tuple, Any
import json

ALPHA_MODELS_DIR = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models")
LEGACY_DIR = ALPHA_MODELS_DIR / "legacy"

class FunctionExtractor(ast.NodeVisitor):
    """Extract function definitions and their signatures from Python files"""
    
    def __init__(self):
        self.functions = []
        self.classes = []
        self.imports = []
        self.docstrings = []
    
    def visit_FunctionDef(self, node):
        """Extract function definitions"""
        args = [arg.arg for arg in node.args.args]
        docstring = ast.get_docstring(node) or ""
        
        self.functions.append({
            'name': node.name,
            'args': args,
            'docstring': docstring,
            'line_number': node.lineno,
            'is_method': False
        })
        self.generic_visit(node)
    
    def visit_ClassDef(self, node):
        """Extract class definitions and their methods"""
        class_docstring = ast.get_docstring(node) or ""
        methods = []
        
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                args = [arg.arg for arg in item.args.args]
                method_docstring = ast.get_docstring(item) or ""
                methods.append({
                    'name': item.name,
                    'args': args,
                    'docstring': method_docstring,
                    'line_number': item.lineno,
                    'is_method': True
                })
        
        self.classes.append({
            'name': node.name,
            'docstring': class_docstring,
            'methods': methods,
            'line_number': node.lineno
        })
        self.generic_visit(node)
    
    def visit_Import(self, node):
        """Extract import statements"""
        for alias in node.names:
            self.imports.append({
                'module': alias.name,
                'alias': alias.asname,
                'type': 'import'
            })
        self.generic_visit(node)
    
    def visit_ImportFrom(self, node):
        """Extract from import statements"""
        module = node.module or ""
        for alias in node.names:
            self.imports.append({
                'module': module,
                'name': alias.name,
                'alias': alias.asname,
                'type': 'from_import'
            })
        self.generic_visit(node)

def analyze_python_file(file_path: Path) -> Dict[str, Any]:
    """Analyze a Python file and extract its functionality"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Parse AST
        try:
            tree = ast.parse(content)
            extractor = FunctionExtractor()
            extractor.visit(tree)
        except SyntaxError as e:
            print(f"Syntax error in {file_path}: {e}")
            return {'error': str(e)}
        
        # Extract additional patterns
        patterns = {
            'prophet_usage': len(re.findall(r'Prophet|prophet', content, re.IGNORECASE)),
            'xgboost_usage': len(re.findall(r'xgboost|XGBoost', content, re.IGNORECASE)),
            'ensemble_usage': len(re.findall(r'ensemble|Ensemble', content, re.IGNORECASE)),
            'model_training': len(re.findall(r'\.fit\(|\.train\(|training', content, re.IGNORECASE)),
            'model_prediction': len(re.findall(r'\.predict\(|\.forecast\(|prediction', content, re.IGNORECASE)),
            'data_loading': len(re.findall(r'pd\.read_|load_data|get_data', content, re.IGNORECASE)),
            'validation': len(re.findall(r'validate|validation|cross_val', content, re.IGNORECASE)),
            'performance_metrics': len(re.findall(r'mse|mae|rmse|accuracy|precision|recall', content, re.IGNORECASE)),
            'feature_engineering': len(re.findall(r'feature|features|transform', content, re.IGNORECASE)),
            'config_usage': len(re.findall(r'config|Config|configuration', content, re.IGNORECASE))
        }
        
        return {
            'file_path': str(file_path),
            'file_size': len(content),
            'line_count': len(content.splitlines()),
            'functions': extractor.functions,
            'classes': extractor.classes,
            'imports': extractor.imports,
            'patterns': patterns,
            'methodology_hints': {
                'prophet': patterns['prophet_usage'] > 0,
                'xgboost': patterns['xgboost_usage'] > 0,
                'ensemble': patterns['ensemble_usage'] > 0
            }
        }
    
    except Exception as e:
        return {'error': str(e), 'file_path': str(file_path)}

def categorize_file(file_path: Path, analysis: Dict[str, Any]) -> Dict[str, Any]:
    """Categorize a file based on its path and analysis"""
    path_parts = file_path.parts
    
    # Determine asset class
    asset_class = 'unknown'
    if 'CRYPTO' in path_parts or 'crypto' in str(file_path).lower():
        asset_class = 'crypto'
    elif 'FOREX' in path_parts or 'forex' in str(file_path).lower():
        asset_class = 'forex'
    elif 'EQUITIES' in path_parts or 'equit' in str(file_path).lower():
        asset_class = 'equities'
    
    # Determine methodology
    methodology = 'unknown'
    if analysis.get('methodology_hints', {}).get('prophet'):
        methodology = 'prophet'
    elif analysis.get('methodology_hints', {}).get('xgboost'):
        methodology = 'xgboost'
    elif analysis.get('methodology_hints', {}).get('ensemble'):
        methodology = 'ensemble'
    
    # Determine file type
    file_type = 'unknown'
    if 'model' in file_path.name.lower():
        file_type = 'model'
    elif 'builder' in file_path.name.lower():
        file_type = 'builder'
    elif 'validator' in file_path.name.lower() or 'validation' in file_path.name.lower():
        file_type = 'validation'
    elif 'util' in file_path.name.lower() or 'tool' in file_path.name.lower():
        file_type = 'utility'
    elif 'example' in file_path.name.lower() or 'demo' in file_path.name.lower():
        file_type = 'example'
    elif 'test' in file_path.name.lower():
        file_type = 'test'
    elif 'config' in file_path.name.lower():
        file_type = 'config'
    elif 'script' in str(file_path):
        file_type = 'script'
    
    return {
        'asset_class': asset_class,
        'methodology': methodology,
        'file_type': file_type,
        'suggested_new_location': suggest_new_location(asset_class, methodology, file_type, file_path.name)
    }

def suggest_new_location(asset_class: str, methodology: str, file_type: str, filename: str) -> str:
    """Suggest where a file should go in the new architecture"""
    
    if file_type == 'builder':
        return f"legacy/deprecated_builders/{filename}"
    
    elif file_type == 'model':
        if methodology != 'unknown':
            if asset_class != 'unknown':
                return f"methodologies/{methodology}/adapters/{asset_class}_adapter.py"
            else:
                return f"methodologies/{methodology}/core/{methodology}_methodology.py"
        else:
            return f"assets/{asset_class}/{asset_class}_adapter.py"
    
    elif file_type == 'validation':
        if methodology != 'unknown':
            return f"methodologies/{methodology}/core/validation.py"
        else:
            return f"core/validation/performance_metrics.py"
    
    elif file_type == 'utility':
        return f"scripts/utilities/{filename}"
    
    elif file_type == 'script':
        return f"scripts/training/{filename}"
    
    elif file_type == 'example':
        if asset_class != 'unknown':
            return f"examples/asset_examples/{asset_class}_example.py"
        elif methodology != 'unknown':
            return f"examples/methodology_examples/{methodology}_example.py"
        else:
            return f"examples/integration_examples/{filename}"
    
    elif file_type == 'config':
        if methodology != 'unknown':
            return f"methodologies/{methodology}/configs/{asset_class}_overrides.json"
        else:
            return f"core/configuration/{filename}"
    
    elif file_type == 'test':
        if asset_class != 'unknown':
            return f"tests/assets/{asset_class}/test_{asset_class}_adapter.py"
        elif methodology != 'unknown':
            return f"tests/methodologies/{methodology}/test_{methodology}_methodology.py"
        else:
            return f"tests/core/test_{filename}"
    
    else:
        return f"scripts/utilities/{filename}"

def create_migration_plan():
    """Create a comprehensive migration plan for all legacy files"""
    print("🔍 Analyzing legacy files for functionality migration...")
    
    migration_plan = {
        'timestamp': str(Path(__file__).stat().st_mtime),
        'files_analyzed': 0,
        'files_with_errors': 0,
        'migration_targets': {
            'methodologies/prophet/core/': [],
            'methodologies/prophet/adapters/': [],
            'methodologies/xgboost/core/': [],
            'methodologies/xgboost/adapters/': [],
            'methodologies/ensemble/core/': [],
            'methodologies/ensemble/adapters/': [],
            'assets/crypto/': [],
            'assets/forex/': [],
            'assets/equities/': [],
            'core/validation/': [],
            'core/orchestration/': [],
            'scripts/training/': [],
            'scripts/utilities/': [],
            'examples/': []
        },
        'functionality_summary': {},
        'duplicate_functionality': []
    }
    
    # Find all Python files in legacy
    legacy_py_files = list(LEGACY_DIR.rglob("*.py"))
    print(f"Found {len(legacy_py_files)} Python files in legacy directory")
    
    for file_path in legacy_py_files:
        print(f"Analyzing: {file_path.relative_to(LEGACY_DIR)}")
        
        # Analyze file
        analysis = analyze_python_file(file_path)
        migration_plan['files_analyzed'] += 1
        
        if 'error' in analysis:
            migration_plan['files_with_errors'] += 1
            print(f"  ❌ Error: {analysis['error']}")
            continue
        
        # Categorize file
        categorization = categorize_file(file_path, analysis)
        
        # Create migration entry
        migration_entry = {
            'source_file': str(file_path.relative_to(ALPHA_MODELS_DIR)),
            'analysis': analysis,
            'categorization': categorization,
            'priority': determine_priority(analysis, categorization)
        }
        
        # Add to appropriate migration target
        target_location = categorization['suggested_new_location']
        target_dir = '/'.join(target_location.split('/')[:-1]) + '/'
        
        if target_dir in migration_plan['migration_targets']:
            migration_plan['migration_targets'][target_dir].append(migration_entry)
        
        print(f"  📁 Target: {target_location}")
        print(f"  🎯 Priority: {migration_entry['priority']}")
        print(f"  📊 Functions: {len(analysis.get('functions', []))}, Classes: {len(analysis.get('classes', []))}")
    
    return migration_plan

def determine_priority(analysis: Dict[str, Any], categorization: Dict[str, Any]) -> str:
    """Determine migration priority based on analysis"""
    patterns = analysis.get('patterns', {})
    
    # High priority: Core model functionality
    if (patterns.get('model_training', 0) > 0 and 
        patterns.get('model_prediction', 0) > 0 and
        categorization['file_type'] == 'model'):
        return 'HIGH'
    
    # Medium priority: Validation and utilities with significant functionality
    if (patterns.get('validation', 0) > 0 or 
        patterns.get('performance_metrics', 0) > 0 or
        len(analysis.get('functions', [])) > 5):
        return 'MEDIUM'
    
    # Low priority: Examples, configs, simple utilities
    if categorization['file_type'] in ['example', 'config', 'builder']:
        return 'LOW'
    
    return 'MEDIUM'

def generate_stub_implementations(migration_plan: Dict[str, Any]):
    """Generate stub implementations for migrated functionality"""
    print("\n🏗️ Generating stub implementations...")
    
    # Group by target methodology/asset
    implementations_needed = {
        'prophet_crypto': [],
        'prophet_forex': [],
        'xgboost_crypto': [],
        'xgboost_forex': [],
        'ensemble_crypto': [],
        'ensemble_forex': [],
        'validation_utils': [],
        'training_scripts': [],
        'utility_scripts': []
    }
    
    for target_dir, entries in migration_plan['migration_targets'].items():
        for entry in entries:
            if entry['priority'] in ['HIGH', 'MEDIUM']:
                categorization = entry['categorization']
                analysis = entry['analysis']
                
                # Determine implementation group
                key = None
                if 'methodologies/prophet' in target_dir:
                    if categorization['asset_class'] == 'crypto':
                        key = 'prophet_crypto'
                    elif categorization['asset_class'] == 'forex':
                        key = 'prophet_forex'
                elif 'methodologies/xgboost' in target_dir:
                    if categorization['asset_class'] == 'crypto':
                        key = 'xgboost_crypto'
                    elif categorization['asset_class'] == 'forex':
                        key = 'xgboost_forex'
                elif 'methodologies/ensemble' in target_dir:
                    if categorization['asset_class'] == 'crypto':
                        key = 'ensemble_crypto'
                    elif categorization['asset_class'] == 'forex':
                        key = 'ensemble_forex'
                elif 'core/validation' in target_dir:
                    key = 'validation_utils'
                elif 'scripts/training' in target_dir:
                    key = 'training_scripts'
                elif 'scripts/utilities' in target_dir:
                    key = 'utility_scripts'
                
                if key:
                    implementations_needed[key].append(entry)
    
    return implementations_needed

if __name__ == "__main__":
    migration_plan = create_migration_plan()
    
    print(f"\n📊 Migration Analysis Summary:")
    print(f"  Files analyzed: {migration_plan['files_analyzed']}")
    print(f"  Files with errors: {migration_plan['files_with_errors']}")
    
    # Show migration targets
    for target_dir, entries in migration_plan['migration_targets'].items():
        if entries:
            print(f"\n📁 {target_dir}: {len(entries)} files")
            for entry in entries[:3]:  # Show first 3
                source = entry['source_file'].split('/')[-1]
                priority = entry['priority']
                print(f"    {priority}: {source}")
            if len(entries) > 3:
                print(f"    ... and {len(entries) - 3} more")
    
    # Generate implementations
    implementations = generate_stub_implementations(migration_plan)
    
    print(f"\n🏗️ Implementation Groups:")
    for group, entries in implementations.items():
        if entries:
            print(f"  {group}: {len(entries)} files to implement")
    
    # Save migration plan
    migration_file = ALPHA_MODELS_DIR / "LEGACY_MIGRATION_PLAN.json"
    with open(migration_file, 'w') as f:
        json.dump(migration_plan, f, indent=2)
    
    print(f"\n💾 Migration plan saved to: {migration_file}")
    print(f"\n✅ Analysis complete!")