#!/usr/bin/env python3
"""
Myportolio Comprehensive Directory Review
Complete analysis for redundancy and completeness assessment
"""

import os
import ast
import json
from datetime import datetime
from pathlib import Path

class ComprehensiveDirectoryReview:
    """Complete directory analysis for redundancy and completeness"""
    
    def __init__(self, directory_path):
        self.directory_path = Path(directory_path)
        self.files_analyzed = []
        self.categorization = {
            'core_algorithms': [],
            'backtesting_systems': [],
            'data_management': [],
            'risk_management': [],
            'trading_strategies': [],
            'utilities_analysis': [],
            'configuration': [],
            'documentation': [],
            'redundant_candidates': [],
            'essential_files': []
        }
        
    def analyze_all_files(self):
        """Analyze all files in the directory"""
        print("🔍 COMPREHENSIVE DIRECTORY REVIEW")
        print("=" * 60)
        
        for file_path in self.directory_path.iterdir():
            if file_path.is_file():
                file_info = self._analyze_single_file(file_path)
                self.files_analyzed.append(file_info)
        
        print(f"\n📊 Total Files Analyzed: {len(self.files_analyzed)}")
        
        # Categorize and analyze
        self._categorize_all_files()
        self._identify_redundancy()
        self._mark_essential_files()
        
        return self.files_analyzed
    
    def _analyze_single_file(self, file_path):
        """Analyze a single file"""
        try:
            stat = file_path.stat()
            size_kb = stat.st_size / 1024
            modified = datetime.fromtimestamp(stat.st_mtime)
            
            file_info = {
                'name': file_path.name,
                'path': str(file_path),
                'extension': file_path.suffix,
                'size_kb': round(size_kb, 2),
                'last_modified': modified.isoformat(),
                'purpose': 'unknown',
                'category': 'uncategorized',
                'complexity_score': 0,
                'dependencies': [],
                'key_features': []
            }
            
            if file_path.suffix == '.py':
                file_info.update(self._analyze_python_file(file_path))
            elif file_path.suffix in ['.md', '.txt']:
                file_info['category'] = 'documentation'
                file_info['purpose'] = 'documentation'
            elif file_path.suffix in ['.json', '.yaml', '.yml']:
                file_info['category'] = 'configuration'
                file_info['purpose'] = 'configuration'
            
            return file_info
            
        except Exception as e:
            return {
                'name': file_path.name,
                'path': str(file_path),
                'error': str(e),
                'category': 'error'
            }
    
    def _analyze_python_file(self, file_path):
        """Deep analysis of Python files"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            lines = len(content.split('\n'))
            
            # Parse AST
            try:
                tree = ast.parse(content)
                classes = [node.name for node in ast.walk(tree) if isinstance(node, ast.ClassDef)]
                functions = [node.name for node in ast.walk(tree) if isinstance(node, ast.FunctionDef)]
                imports = self._extract_imports(content)
            except:
                classes, functions, imports = [], [], []
            
            # Determine purpose and complexity
            purpose, category = self._determine_purpose_and_category(content, file_path.name)
            complexity_score = self._calculate_complexity(lines, len(classes), len(functions))
            key_features = self._extract_key_features(content, classes, functions)
            
            return {
                'lines': lines,
                'classes': classes,
                'functions': functions,
                'imports': imports,
                'purpose': purpose,
                'category': category,
                'complexity_score': complexity_score,
                'key_features': key_features,
                'docstring': self._extract_module_docstring(content)
            }
            
        except Exception as e:
            return {'analysis_error': str(e)}
    
    def _extract_imports(self, content):
        """Extract import statements"""
        imports = []
        for line in content.split('\n')[:50]:  # Check first 50 lines
            line = line.strip()
            if line.startswith(('import ', 'from ')):
                imports.append(line)
        return imports
    
    def _determine_purpose_and_category(self, content, filename):
        """Determine file purpose and category"""
        content_lower = content.lower()
        filename_lower = filename.lower()
        
        # Core algorithm identification
        if any(keyword in content_lower for keyword in [
            'ensemblemultiassetportfolio', 'kelly criterion', 'prophet', 'xgboost'
        ]):
            return 'core_algorithm', 'core_algorithms'
        
        # Backtesting systems
        elif any(keyword in content_lower for keyword in [
            'backtest', 'backtesting', 'lean', 'comprehensive_backtesting',
            'parameter_optimization', 'robust_backtesting'
        ]):
            return 'backtesting_system', 'backtesting_systems'
        
        # Data management
        elif any(keyword in content_lower for keyword in [
            'live_market_data', 'market data', 'coinbase', 'data feed'
        ]):
            return 'data_management', 'data_management'
        
        # Risk management
        elif any(keyword in content_lower for keyword in [
            'risk management', 'eth_basic_risk', 'risk algorithm'
        ]):
            return 'risk_management', 'risk_management'
        
        # Trading strategies
        elif any(keyword in content_lower for keyword in [
            'momentum strategy', 'trading algorithm', 'signal generation'
        ]):
            return 'trading_strategy', 'trading_strategies'
        
        # Utilities and analysis
        elif any(keyword in content_lower for keyword in [
            'analysis', 'summary', 'trace', 'pipeline', 'utility'
        ]):
            return 'utility_analysis', 'utilities_analysis'
        
        # Configuration
        elif 'config' in filename_lower or filename_lower.endswith('.json'):
            return 'configuration', 'configuration'
        
        else:
            return 'unknown', 'uncategorized'
    
    def _calculate_complexity(self, lines, num_classes, num_functions):
        """Calculate complexity score"""
        score = 0
        score += min(lines / 100, 10)  # Lines contribution (max 10)
        score += num_classes * 2  # Classes worth 2 points each
        score += num_functions * 1  # Functions worth 1 point each
        return round(score, 1)
    
    def _extract_key_features(self, content, classes, functions):
        """Extract key features from content"""
        features = []
        content_lower = content.lower()
        
        # Algorithm features
        if 'kelly criterion' in content_lower:
            features.append('Kelly Criterion Implementation')
        if 'ensemble' in content_lower:
            features.append('Ensemble Learning')
        if 'prophet' in content_lower:
            features.append('Prophet Time Series')
        if 'xgboost' in content_lower:
            features.append('XGBoost ML Model')
        
        # Trading features
        if 'momentum' in content_lower:
            features.append('Momentum Strategy')
        if 'mean reversion' in content_lower:
            features.append('Mean Reversion Strategy')
        if 'risk management' in content_lower:
            features.append('Risk Management')
        
        # Technical features
        if 'backtesting' in content_lower:
            features.append('Backtesting Framework')
        if 'live data' in content_lower or 'market data' in content_lower:
            features.append('Live Market Data')
        if 'lean' in content_lower:
            features.append('LEAN Integration')
        
        return features
    
    def _extract_module_docstring(self, content):
        """Extract module docstring"""
        try:
            tree = ast.parse(content)
            if (tree.body and isinstance(tree.body[0], ast.Expr) and 
                isinstance(tree.body[0].value, ast.Constant)):
                return tree.body[0].value.value.strip()[:150] + "..."
        except:
            pass
        return None
    
    def _categorize_all_files(self):
        """Categorize all analyzed files"""
        print(f"\n📂 FILE CATEGORIZATION")
        print("-" * 40)
        
        for file_info in self.files_analyzed:
            category = file_info.get('category', 'uncategorized')
            if category in self.categorization:
                self.categorization[category].append(file_info)
        
        # Print categorization summary
        for category, files in self.categorization.items():
            if files and category not in ['redundant_candidates', 'essential_files']:
                print(f"\n   📁 {category.upper().replace('_', ' ')}: {len(files)} files")
                for file_info in sorted(files, key=lambda x: x.get('complexity_score', 0), reverse=True):
                    name = file_info['name']
                    size = file_info.get('size_kb', 0)
                    complexity = file_info.get('complexity_score', 0)
                    print(f"      📄 {name:<35} {size:>6.1f}KB  Complexity: {complexity:>4.1f}")
    
    def _identify_redundancy(self):
        """Identify redundant or overlapping files"""
        print(f"\n🔍 REDUNDANCY ANALYSIS")
        print("-" * 40)
        
        # Group similar files
        similar_groups = {}
        
        for file_info in self.files_analyzed:
            if file_info.get('extension') != '.py':
                continue
                
            name = file_info['name']
            purpose = file_info.get('purpose', 'unknown')
            features = file_info.get('key_features', [])
            
            # Create grouping key
            if 'ensemble' in name.lower():
                group_key = 'ensemble_implementations'
            elif 'backtest' in name.lower() or 'backtesting' in purpose:
                group_key = 'backtesting_implementations'
            elif 'kelly' in name.lower():
                group_key = 'kelly_implementations'
            elif 'portfolio' in name.lower():
                group_key = 'portfolio_implementations'
            elif 'market_data' in name.lower() or 'data' in name.lower():
                group_key = 'data_implementations'
            elif 'analysis' in name.lower() or 'Analysis' in name:
                group_key = 'analysis_implementations'
            else:
                group_key = f"{purpose}_implementations"
            
            if group_key not in similar_groups:
                similar_groups[group_key] = []
            similar_groups[group_key].append(file_info)
        
        # Identify redundancy candidates
        for group_name, group_files in similar_groups.items():
            if len(group_files) > 1:
                print(f"\n   ⚠️ POTENTIAL REDUNDANCY: {group_name.replace('_', ' ').title()}")
                
                # Sort by complexity and modification date
                sorted_files = sorted(group_files, 
                                    key=lambda x: (x.get('complexity_score', 0), x.get('last_modified', '')), 
                                    reverse=True)
                
                primary_file = sorted_files[0]
                print(f"      🎯 PRIMARY: {primary_file['name']} (Complexity: {primary_file.get('complexity_score', 0)})")
                
                for candidate in sorted_files[1:]:
                    print(f"      🔄 REVIEW: {candidate['name']} (Complexity: {candidate.get('complexity_score', 0)})")
                    self.categorization['redundant_candidates'].append({
                        'file': candidate,
                        'group': group_name,
                        'primary_file': primary_file['name'],
                        'reason': f"Similar functionality to {primary_file['name']}"
                    })
    
    def _mark_essential_files(self):
        """Identify essential files for the system"""
        print(f"\n⭐ ESSENTIAL FILES IDENTIFICATION")
        print("-" * 40)
        
        essential_criteria = {
            'high_complexity': 15,  # Complexity score threshold
            'core_functionality': ['core_algorithm', 'backtesting_system', 'data_management'],
            'large_size': 10,  # KB threshold
            'recent_modification': True
        }
        
        for file_info in self.files_analyzed:
            if file_info.get('extension') != '.py':
                continue
            
            is_essential = False
            reasons = []
            
            # Check complexity
            complexity = file_info.get('complexity_score', 0)
            if complexity >= essential_criteria['high_complexity']:
                is_essential = True
                reasons.append(f"High complexity ({complexity})")
            
            # Check core functionality
            purpose = file_info.get('purpose', 'unknown')
            if purpose in essential_criteria['core_functionality']:
                is_essential = True
                reasons.append(f"Core functionality ({purpose})")
            
            # Check size
            size = file_info.get('size_kb', 0)
            if size >= essential_criteria['large_size']:
                is_essential = True
                reasons.append(f"Large implementation ({size}KB)")
            
            # Check key features
            features = file_info.get('key_features', [])
            if any(feature in ['Kelly Criterion Implementation', 'Ensemble Learning', 'Live Market Data'] for feature in features):
                is_essential = True
                reasons.append("Core algorithm implementation")
            
            if is_essential:
                file_info['essential_reasons'] = reasons
                self.categorization['essential_files'].append(file_info)
                print(f"      ⭐ {file_info['name']}: {', '.join(reasons)}")
    
    def generate_cleanup_recommendations(self):
        """Generate recommendations for cleanup"""
        print(f"\n🧹 CLEANUP RECOMMENDATIONS")
        print("-" * 40)
        
        recommendations = {
            'files_to_keep': [],
            'files_to_review': [],
            'files_to_consolidate': [],
            'missing_components': []
        }
        
        # Essential files to keep
        for file_info in self.categorization['essential_files']:
            recommendations['files_to_keep'].append({
                'name': file_info['name'],
                'reason': 'Essential functionality',
                'details': file_info.get('essential_reasons', [])
            })
        
        # Files to review (redundancy candidates)
        for candidate in self.categorization['redundant_candidates']:
            recommendations['files_to_review'].append({
                'name': candidate['file']['name'],
                'reason': candidate['reason'],
                'action': 'Consider consolidating or removing'
            })
        
        # Print recommendations
        print(f"\n   ✅ ESSENTIAL FILES TO KEEP ({len(recommendations['files_to_keep'])}):")
        for item in recommendations['files_to_keep']:
            print(f"      🎯 {item['name']}: {', '.join(item['details'])}")
        
        print(f"\n   🔍 FILES TO REVIEW ({len(recommendations['files_to_review'])}):")
        for item in recommendations['files_to_review']:
            print(f"      ⚠️ {item['name']}: {item['reason']}")
        
        return recommendations
    
    def export_analysis_report(self):
        """Export comprehensive analysis report"""
        report = {
            'analysis_metadata': {
                'timestamp': datetime.now().isoformat(),
                'directory': str(self.directory_path),
                'total_files_analyzed': len(self.files_analyzed)
            },
            'file_categories': {
                category: len(files) for category, files in self.categorization.items()
            },
            'detailed_analysis': self.files_analyzed,
            'categorization': self.categorization,
            'cleanup_recommendations': self.generate_cleanup_recommendations()
        }
        
        output_file = self.directory_path / f"comprehensive_directory_review_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        
        print(f"\n📄 COMPREHENSIVE REPORT EXPORTED")
        print(f"   📁 {output_file}")
        
        return report

def main():
    """Run comprehensive directory review"""
    directory_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
    
    reviewer = ComprehensiveDirectoryReview(directory_path)
    files = reviewer.analyze_all_files()
    report = reviewer.export_analysis_report()
    
    print(f"\n🎉 COMPREHENSIVE REVIEW COMPLETE!")
    print("=" * 60)
    print(f"📊 Files Analyzed: {len(files)}")
    print(f"⭐ Essential Files: {len(reviewer.categorization['essential_files'])}")
    print(f"🔍 Files to Review: {len(reviewer.categorization['redundant_candidates'])}")
    print(f"📂 Categories: {len([k for k, v in reviewer.categorization.items() if v])}")
    
    return report

if __name__ == "__main__":
    main()