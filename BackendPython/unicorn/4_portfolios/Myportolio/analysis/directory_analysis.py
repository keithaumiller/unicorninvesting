#!/usr/bin/env python3
"""
Myportolio Directory Comprehensive Analysis
Reviews all files for redundancy and completeness assessment
"""

import os
import json
import glob
from datetime import datetime

def analyze_directory_structure():
    """Analyze all files in Myportolio directory"""
    
    base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
    
    print("🔍 COMPREHENSIVE MYPORTOLIO DIRECTORY ANALYSIS")
    print("=" * 70)
    
    # File categorization
    file_categories = {
        'core_production': [],
        'backtesting_suite': [],
        'testing_files': [],
        'documentation': [],
        'configuration': [],
        'deprecated_legacy': [],
        'integration_demos': [],
        'status_reports': []
    }
    
    file_analysis = {}
    
    # Get all files recursively
    all_files = []
    for root, dirs, files in os.walk(base_path):
        for file in files:
            if not file.startswith('.') and '__pycache__' not in root:
                full_path = os.path.join(root, file)
                rel_path = os.path.relpath(full_path, base_path)
                all_files.append(rel_path)
    
    print(f"📁 Total Files Found: {len(all_files)}")
    
    # Analyze each file
    for file_path in sorted(all_files):
        full_path = os.path.join(base_path, file_path)
        
        # Get file stats
        try:
            stat = os.stat(full_path)
            file_size = stat.st_size
            mod_time = datetime.fromtimestamp(stat.st_mtime)
        except:
            file_size = 0
            mod_time = datetime.now()
        
        # Categorize files
        category = categorize_file(file_path, full_path)
        purpose = analyze_file_purpose(file_path, full_path)
        
        file_analysis[file_path] = {
            'category': category,
            'purpose': purpose,
            'size': file_size,
            'modified': mod_time.strftime('%Y-%m-%d %H:%M'),
            'status': assess_file_status(file_path, full_path, purpose)
        }
        
        file_categories[category].append(file_path)
    
    return file_categories, file_analysis

def categorize_file(file_path, full_path):
    """Categorize file by type and purpose"""
    
    filename = os.path.basename(file_path).lower()
    
    # Core production files
    if any(x in filename for x in ['live_market_data_feed', 'simplified_ensemble_portfolio', 
                                   'lean_backtesting_integration', 'ensemble_model_wrapper']):
        return 'core_production'
    
    # Backtesting suite
    if any(x in filename for x in ['comprehensive_backtesting', 'robust_backtesting', 
                                   'parameter_optimization', 'backtesting_analysis']):
        return 'backtesting_suite'
    
    # Testing files
    if filename.startswith('test_') or 'test' in filename:
        return 'testing_files'
    
    # Documentation
    if any(x in filename for x in ['readme', '.md', 'pipeline_trace_complete']):
        return 'documentation'
    
    # Configuration
    if any(x in filename for x in ['.json', 'config', 'settings']):
        return 'configuration'
    
    # Status and integration demos
    if any(x in filename for x in ['integration_success', 'status', 'demonstration', 
                                   'simulation']):
        return 'integration_demos'
    
    # Legacy/deprecated files
    if any(x in filename for x in ['system_comparison', 'silver_layer', 'production_ensemble']):
        return 'deprecated_legacy'
    
    # Pipeline and analysis
    if any(x in filename for x in ['pipeline_trace', 'complete_pipeline']):
        return 'backtesting_suite'
    
    return 'core_production'  # Default

def analyze_file_purpose(file_path, full_path):
    """Analyze file purpose by reading first few lines"""
    
    try:
        with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
            first_lines = f.read(500)
            
        # Extract docstring or comments
        if '"""' in first_lines:
            start = first_lines.find('"""') + 3
            end = first_lines.find('"""', start)
            if end > start:
                return first_lines[start:end].strip()[:100]
        
        elif '/*' in first_lines:
            start = first_lines.find('/*') + 2
            end = first_lines.find('*/', start)
            if end > start:
                return first_lines[start:end].strip()[:100]
        
        elif '#' in first_lines:
            lines = first_lines.split('\n')
            for line in lines[1:5]:  # Skip shebang
                if line.strip().startswith('#'):
                    return line.strip()[1:].strip()[:100]
        
        # Fallback: filename-based purpose
        filename = os.path.basename(file_path).lower()
        if 'backtesting' in filename:
            return "Backtesting functionality"
        elif 'live_market' in filename:
            return "Live market data integration"
        elif 'ensemble' in filename:
            return "Ensemble trading system"
        elif 'test' in filename:
            return "Testing and validation"
        elif 'config' in filename:
            return "Configuration settings"
        
        return "Core system component"
        
    except:
        return "Unknown purpose"

def assess_file_status(file_path, full_path, purpose):
    """Assess if file is active, deprecated, or redundant"""
    
    filename = os.path.basename(file_path).lower()
    
    # Check for deprecated markers
    if any(x in filename for x in ['deprecated', 'legacy', 'old', 'backup']):
        return 'DEPRECATED'
    
    # Check for test files
    if filename.startswith('test_'):
        return 'TESTING'
    
    # Check for demo/example files
    if any(x in filename for x in ['demo', 'example', 'simulation', 'demonstration']):
        return 'DEMO'
    
    # Check for documentation
    if filename.endswith('.md') or 'readme' in filename:
        return 'DOCUMENTATION'
    
    # Check for configuration
    if filename.endswith('.json'):
        return 'CONFIGURATION'
    
    # Core production files
    if any(x in filename for x in ['live_market_data_feed', 'simplified_ensemble_portfolio', 
                                   'lean_backtesting_integration']):
        return 'PRODUCTION'
    
    # Backtesting suite
    if 'backtesting' in filename:
        return 'BACKTESTING'
    
    return 'ACTIVE'

def identify_redundancies(file_categories, file_analysis):
    """Identify redundant or duplicate functionality"""
    
    print(f"\n🔍 REDUNDANCY ANALYSIS")
    print("-" * 50)
    
    redundancies = []
    
    # Check for multiple backtesting implementations
    backtesting_files = file_categories['backtesting_suite']
    if len(backtesting_files) > 3:
        redundancies.append({
            'type': 'Multiple Backtesting Systems',
            'files': backtesting_files,
            'recommendation': 'Consolidate to 1-2 primary backtesting systems'
        })
    
    # Check for multiple test files with similar purposes
    test_files = file_categories['testing_files']
    integration_tests = [f for f in test_files if 'integration' in f.lower()]
    if len(integration_tests) > 2:
        redundancies.append({
            'type': 'Multiple Integration Tests',
            'files': integration_tests,
            'recommendation': 'Consolidate integration tests'
        })
    
    # Check for multiple configuration files
    config_files = file_categories['configuration']
    if len(config_files) > 3:
        redundancies.append({
            'type': 'Multiple Configuration Files',
            'files': config_files,
            'recommendation': 'Consider configuration consolidation'
        })
    
    # Check for demonstration/simulation files
    demo_files = file_categories['integration_demos']
    if len(demo_files) > 3:
        redundancies.append({
            'type': 'Multiple Demo/Simulation Files',
            'files': demo_files,
            'recommendation': 'Archive older demos, keep latest'
        })
    
    return redundancies

def assess_completeness(file_categories, file_analysis):
    """Assess system completeness and missing components"""
    
    print(f"\n✅ COMPLETENESS ASSESSMENT")
    print("-" * 50)
    
    completeness = {
        'core_systems': {},
        'missing_components': [],
        'coverage_score': 0
    }
    
    # Required core components
    required_components = {
        'live_data_feed': False,
        'ensemble_system': False,
        'backtesting_engine': False,
        'risk_management': False,
        'lean_integration': False,
        'configuration_system': False,
        'testing_framework': False,
        'documentation': False
    }
    
    # Check for each component
    all_files = [f for category in file_categories.values() for f in category]
    
    for file_path in all_files:
        filename = file_path.lower()
        
        if 'live_market_data_feed' in filename:
            required_components['live_data_feed'] = True
        elif 'ensemble' in filename and 'portfolio' in filename:
            required_components['ensemble_system'] = True
        elif 'backtesting' in filename:
            required_components['backtesting_engine'] = True
        elif 'risk' in filename:
            required_components['risk_management'] = True
        elif 'lean' in filename:
            required_components['lean_integration'] = True
        elif filename.endswith('.json'):
            required_components['configuration_system'] = True
        elif filename.startswith('test_'):
            required_components['testing_framework'] = True
        elif 'readme' in filename:
            required_components['documentation'] = True
    
    # Calculate coverage
    present_components = sum(required_components.values())
    total_components = len(required_components)
    coverage_score = (present_components / total_components) * 100
    
    completeness['core_systems'] = required_components
    completeness['coverage_score'] = coverage_score
    
    # Identify missing components
    for component, present in required_components.items():
        if not present:
            completeness['missing_components'].append(component)
    
    return completeness

def generate_recommendations(file_categories, file_analysis, redundancies, completeness):
    """Generate recommendations for cleanup and organization"""
    
    print(f"\n💡 RECOMMENDATIONS")
    print("-" * 50)
    
    recommendations = {
        'cleanup_actions': [],
        'consolidation_opportunities': [],
        'organization_improvements': [],
        'priority_actions': []
    }
    
    # Cleanup recommendations
    deprecated_files = [f for f, data in file_analysis.items() 
                       if data['status'] == 'DEPRECATED']
    if deprecated_files:
        recommendations['cleanup_actions'].append({
            'action': 'Remove deprecated files',
            'files': deprecated_files,
            'priority': 'LOW'
        })
    
    # Consolidation opportunities
    if len(file_categories['backtesting_suite']) > 3:
        recommendations['consolidation_opportunities'].append({
            'action': 'Consolidate backtesting systems',
            'target': 'Keep robust_backtesting_suite.py as primary',
            'priority': 'MEDIUM'
        })
    
    if len(file_categories['testing_files']) > 5:
        recommendations['consolidation_opportunities'].append({
            'action': 'Consolidate test files',
            'target': 'Group by functionality',
            'priority': 'LOW'
        })
    
    # Organization improvements
    recommendations['organization_improvements'].append({
        'action': 'Create subdirectories for better organization',
        'structure': {
            'core/': 'Production systems',
            'backtesting/': 'All backtesting components', 
            'testing/': 'Test files',
            'docs/': 'Documentation',
            'config/': 'Configuration files',
            'archive/': 'Deprecated files'
        },
        'priority': 'MEDIUM'
    })
    
    # Priority actions based on analysis
    if completeness['coverage_score'] < 90:
        recommendations['priority_actions'].append({
            'action': 'Address missing components',
            'components': completeness['missing_components'],
            'priority': 'HIGH'
        })
    
    if len(redundancies) > 2:
        recommendations['priority_actions'].append({
            'action': 'Resolve redundancies',
            'count': len(redundancies),
            'priority': 'MEDIUM'
        })
    
    return recommendations

def generate_comprehensive_report(file_categories, file_analysis, redundancies, completeness, recommendations):
    """Generate comprehensive analysis report"""
    
    print(f"\n📊 COMPREHENSIVE ANALYSIS REPORT")
    print("=" * 70)
    
    # Summary statistics
    total_files = sum(len(files) for files in file_categories.values())
    total_size = sum(data['size'] for data in file_analysis.values())
    
    print(f"\n📈 SUMMARY STATISTICS")
    print(f"   📁 Total Files: {total_files}")
    print(f"   💾 Total Size: {total_size / 1024:.1f} KB")
    print(f"   ✅ Completeness Score: {completeness['coverage_score']:.1f}%")
    print(f"   ⚠️ Redundancies Found: {len(redundancies)}")
    
    # File breakdown by category
    print(f"\n📂 FILE BREAKDOWN BY CATEGORY")
    print("-" * 40)
    
    for category, files in file_categories.items():
        if files:
            print(f"   {category.upper().replace('_', ' ')}: {len(files)} files")
            for file in sorted(files)[:3]:  # Show first 3
                status = file_analysis[file]['status']
                print(f"      • {file} ({status})")
            if len(files) > 3:
                print(f"      ... and {len(files) - 3} more")
    
    # Core systems status
    print(f"\n🔧 CORE SYSTEMS STATUS")
    print("-" * 40)
    
    for component, present in completeness['core_systems'].items():
        status = "✅ PRESENT" if present else "❌ MISSING"
        print(f"   {component.replace('_', ' ').title()}: {status}")
    
    # Redundancy details
    if redundancies:
        print(f"\n⚠️ REDUNDANCY DETAILS")
        print("-" * 40)
        
        for i, redundancy in enumerate(redundancies, 1):
            print(f"   {i}. {redundancy['type']}")
            print(f"      Files: {len(redundancy['files'])}")
            print(f"      Recommendation: {redundancy['recommendation']}")
    
    # Priority recommendations
    print(f"\n🎯 PRIORITY RECOMMENDATIONS")
    print("-" * 40)
    
    for rec in recommendations['priority_actions']:
        priority = rec['priority']
        action = rec['action']
        print(f"   {priority}: {action}")
    
    return {
        'summary': {
            'total_files': total_files,
            'total_size_kb': total_size / 1024,
            'completeness_score': completeness['coverage_score'],
            'redundancies_count': len(redundancies)
        },
        'file_categories': file_categories,
        'file_analysis': file_analysis,
        'redundancies': redundancies,
        'completeness': completeness,
        'recommendations': recommendations
    }

def main():
    """Run comprehensive directory analysis"""
    
    # Analyze directory structure
    file_categories, file_analysis = analyze_directory_structure()
    
    # Identify redundancies
    redundancies = identify_redundancies(file_categories, file_analysis)
    
    # Assess completeness
    completeness = assess_completeness(file_categories, file_analysis)
    
    # Generate recommendations
    recommendations = generate_recommendations(
        file_categories, file_analysis, redundancies, completeness
    )
    
    # Generate comprehensive report
    report = generate_comprehensive_report(
        file_categories, file_analysis, redundancies, completeness, recommendations
    )
    
    print(f"\n🎉 ANALYSIS COMPLETE!")
    print("=" * 70)
    print(f"✅ Directory comprehensively analyzed")
    print(f"🔍 Redundancies identified and catalogued")
    print(f"📊 Completeness assessed at {completeness['coverage_score']:.1f}%")
    print(f"💡 Recommendations generated for optimization")
    
    return report

if __name__ == "__main__":
    main()