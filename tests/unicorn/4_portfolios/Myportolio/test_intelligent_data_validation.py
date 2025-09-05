#!/usr/bin/env python3
"""
Enhanced Data Validation System with Intelligent Mapping

This system provides improved mapping capabilities based on the actual data patterns
discovered in the production validation test.
"""

import json
import os
import requests
import re
from datetime import datetime
from typing import Dict, List, Any, Optional
import time

class IntelligentDataMapper:
    """Enhanced data mapping with pattern recognition and intelligent matching"""
    
    def __init__(self):
        self.base_url = "http://localhost"
        self.session = requests.Session()
        self.backend_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        
        # Enhanced page configuration
        self.pages_config = {
            'performance': {
                'url': '/admin/metrics/lean/performance?portfolio=Myportolio',
                'accessible': True,
                'data_context': 'performance_metrics'
            },
            'algorithms': {
                'url': '/admin/metrics/lean/algorithms?portfolio=Myportolio',
                'accessible': True,
                'data_context': 'algorithm_status'
            },
            'algorithm_performance': {
                'url': '/admin/metrics/lean/algorithms/performance?portfolio=Myportolio',
                'accessible': True,
                'data_context': 'algorithm_metrics'
            },
            'backtest_results': {
                'url': '/admin/metrics/lean/backtest?portfolio=Myportolio',
                'accessible': True,
                'data_context': 'simulation_results'
            }
        }
        
        self.results = {
            'validation_timestamp': datetime.now().isoformat(),
            'mapping_intelligence': {},
            'successful_mappings': {},
            'validation_scores': {},
            'recommendations': []
        }
    
    def load_backend_data(self) -> Dict:
        """Load and structure backend data for intelligent mapping"""
        
        backend_data = {
            'raw_data': {},
            'structured_mappings': {},
            'searchable_values': {}
        }
        
        print("🔍 Loading backend data with intelligent indexing...")
        
        # Load raw configuration files
        raw_files = {
            'config': f"{self.backend_path}/config.json",
            'risk_parameters': f"{self.backend_path}/risk_parameters.json"
        }
        
        for key, filepath in raw_files.items():
            if os.path.exists(filepath):
                with open(filepath, 'r') as f:
                    data = json.load(f)
                    backend_data['raw_data'][key] = data
                    print(f"  ✅ Loaded {key}: {len(data)} keys")
        
        # Load latest status report
        import glob
        status_files = glob.glob(f"{self.backend_path}/status_report_*.json")
        if status_files:
            latest_status = max(status_files, key=os.path.getctime)
            with open(latest_status, 'r') as f:
                backend_data['raw_data']['status_report'] = json.load(f)
                print(f"  ✅ Loaded status_report: Latest from {os.path.basename(latest_status)}")
        
        # Create structured mappings for common patterns
        self.create_structured_mappings(backend_data)
        
        return backend_data
    
    def create_structured_mappings(self, backend_data: Dict):
        """Create structured mappings for easy lookup"""
        
        mappings = backend_data['structured_mappings']
        raw_data = backend_data['raw_data']
        
        # Portfolio identification mappings
        mappings['portfolio_identity'] = {}
        for source_key, source_data in raw_data.items():
            if isinstance(source_data, dict):
                if 'portfolio_name' in source_data:
                    mappings['portfolio_identity'][source_key] = source_data['portfolio_name']
        
        # Risk and configuration mappings
        if 'risk_parameters' in raw_data:
            risk_data = raw_data['risk_parameters']
            mappings['risk_metrics'] = {
                'max_portfolio_volatility': risk_data.get('max_portfolio_volatility'),
                'max_drawdown': risk_data.get('max_drawdown'),
                'var_limit_1day': risk_data.get('var_limit_1day'),
                'risk_profile': risk_data.get('risk_profile')
            }
        
        # Asset allocation mappings
        if 'config' in raw_data and 'assets' in raw_data['config']:
            assets_data = raw_data['config']['assets']
            mappings['asset_allocations'] = {}
            for asset, asset_config in assets_data.items():
                mappings['asset_allocations'][asset.lower()] = {
                    'allocation_percent': asset_config.get('allocation_percent'),
                    'symbol': asset
                }
        
        # Performance mappings from status report
        if 'status_report' in raw_data:
            status_data = raw_data['status_report']
            mappings['performance_data'] = {}
            
            # Extract performance metrics if available
            if 'performance_summary' in status_data:
                perf_summary = status_data['performance_summary']
                for metric_name, metric_value in perf_summary.items():
                    mappings['performance_data'][metric_name.lower()] = metric_value
        
        # Create searchable value index
        self.create_searchable_index(backend_data)
    
    def create_searchable_index(self, backend_data: Dict):
        """Create index of all backend values for pattern matching"""
        
        searchable = backend_data['searchable_values']
        
        def index_values(data, path=[]):
            if isinstance(data, dict):
                for key, value in data.items():
                    current_path = path + [key]
                    
                    # Index the value
                    value_str = str(value)
                    if value_str not in searchable:
                        searchable[value_str] = []
                    
                    searchable[value_str].append({
                        'path': current_path,
                        'source': path[0] if path else 'root',
                        'context': key
                    })
                    
                    # Recurse into nested structures
                    index_values(value, current_path)
            
            elif isinstance(data, list):
                for i, item in enumerate(data):
                    index_values(item, path + [i])
        
        # Index all raw data
        for source_name, source_data in backend_data['raw_data'].items():
            index_values(source_data, [source_name])
    
    def extract_enhanced_page_data(self, page_name: str, content: str) -> Dict:
        """Extract data with enhanced pattern recognition"""
        
        data_points = {}
        
        # Common extraction patterns
        data_points.update(self.extract_common_patterns(content))
        
        # Page-specific patterns based on context
        if page_name == 'performance':
            data_points.update(self.extract_performance_patterns(content))
        elif page_name == 'algorithms':
            data_points.update(self.extract_algorithm_patterns(content))
        elif page_name == 'algorithm_performance':
            data_points.update(self.extract_algo_performance_patterns(content))
        elif page_name == 'backtest_results':
            data_points.update(self.extract_backtest_patterns(content))
        
        return data_points
    
    def extract_common_patterns(self, content: str) -> Dict:
        """Extract common patterns with semantic meaning"""
        
        patterns = {}
        
        # Portfolio identification
        portfolio_matches = re.findall(r'portfolio[=:]([^&\s"\']+)', content, re.IGNORECASE)
        if portfolio_matches:
            patterns['portfolio_identifier'] = portfolio_matches[0]
        
        # Page title with context
        title_match = re.search(r'<title>([^<]+)</title>', content)
        if title_match:
            patterns['page_title'] = title_match.group(1).strip()
        
        # Main heading with context
        h1_match = re.search(r'<h1[^>]*>([^<]+)</h1>', content)
        if h1_match:
            patterns['main_heading'] = h1_match.group(1).strip()
        
        # Extract percentages with context
        percentage_contexts = [
            (r'allocation[^>]*>([^<]*?)([0-9.]+)%', 'allocation_percent'),
            (r'performance[^>]*>([^<]*?)([0-9.]+)%', 'performance_percent'),
            (r'risk[^>]*>([^<]*?)([0-9.]+)%', 'risk_percent'),
            (r'return[^>]*>([^<]*?)([+-]?[0-9.]+)%', 'return_percent'),
            (r'volatility[^>]*>([^<]*?)([0-9.]+)%', 'volatility_percent')
        ]
        
        for pattern, context in percentage_contexts:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, (description, value) in enumerate(matches):
                patterns[f'{context}_{i}'] = f'{value}%'
        
        # Extract currency values with context
        currency_contexts = [
            (r'portfolio[^>]*value[^>]*>\$([0-9,]+\.[0-9]{2})', 'portfolio_value'),
            (r'cash[^>]*>\$([0-9,]+\.[0-9]{2})', 'cash_value'),
            (r'position[^>]*value[^>]*>\$([0-9,]+\.[0-9]{2})', 'position_value')
        ]
        
        for pattern, context in currency_contexts:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, value in enumerate(matches):
                patterns[f'{context}_{i}'] = f'${value}'
        
        # Extract timestamps with context
        timestamp_patterns = [
            (r'last[_\s]updated[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2}[T\s][0-9]{2}:[0-9]{2}:[0-9]{2})', 'last_updated'),
            (r'generated[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2}[T\s][0-9]{2}:[0-9]{2}:[0-9]{2})', 'generated_time'),
            (r'timestamp[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2}[T\s][0-9]{2}:[0-9]{2}:[0-9]{2})', 'timestamp')
        ]
        
        for pattern, context in timestamp_patterns:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, timestamp in enumerate(matches):
                patterns[f'{context}_{i}'] = timestamp
        
        return patterns
    
    def extract_performance_patterns(self, content: str) -> Dict:
        """Extract performance-specific patterns"""
        
        patterns = {}
        
        # Performance metrics patterns
        perf_patterns = [
            (r'total[_\s]return[^>]*>([+-]?[0-9.]+)%', 'total_return'),
            (r'sharpe[_\s]ratio[^>]*>([+-]?[0-9.]+)', 'sharpe_ratio'),
            (r'max[_\s]drawdown[^>]*>([0-9.]+)%', 'max_drawdown'),
            (r'volatility[^>]*>([0-9.]+)%', 'portfolio_volatility'),
            (r'alpha[^>]*>([+-]?[0-9.]+)', 'alpha'),
            (r'beta[^>]*>([+-]?[0-9.]+)', 'beta')
        ]
        
        for pattern, metric_name in perf_patterns:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, value in enumerate(matches):
                suffix = f'_{i}' if i > 0 else ''
                patterns[f'perf_{metric_name}{suffix}'] = value + ('%' if 'return' in metric_name or 'drawdown' in metric_name else '')
        
        return patterns
    
    def extract_algorithm_patterns(self, content: str) -> Dict:
        """Extract algorithm-specific patterns"""
        
        patterns = {}
        
        # Algorithm status patterns
        algo_patterns = [
            (r'algorithm[^>]*name[^>]*>([^<]+)', 'algorithm_name'),
            (r'status[^>]*>([^<]+)', 'algorithm_status'),
            (r'last[_\s]run[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2}[T\s][0-9]{2}:[0-9]{2}:[0-9]{2})', 'last_run_time')
        ]
        
        for pattern, context in algo_patterns:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, value in enumerate(matches):
                patterns[f'{context}_{i}'] = value.strip()
        
        return patterns
    
    def extract_algo_performance_patterns(self, content: str) -> Dict:
        """Extract algorithm performance patterns"""
        
        patterns = {}
        
        # Algorithm performance metrics
        algo_perf_patterns = [
            (r'win[_\s]rate[^>]*>([0-9.]+)%', 'win_rate'),
            (r'profit[_\s]factor[^>]*>([0-9.]+)', 'profit_factor'),
            (r'trades[_\s]count[^>]*>([0-9]+)', 'trades_count')
        ]
        
        for pattern, metric_name in algo_perf_patterns:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, value in enumerate(matches):
                suffix = f'_{i}' if i > 0 else ''
                patterns[f'algo_{metric_name}{suffix}'] = value + ('%' if 'rate' in metric_name else '')
        
        return patterns
    
    def extract_backtest_patterns(self, content: str) -> Dict:
        """Extract backtest-specific patterns"""
        
        patterns = {}
        
        # Simulation parameters
        if 'simulation' in content.lower() or 'backtest' in content.lower():
            sim_patterns = [
                (r'simulation[_\s]id[^>]*>([^<]+)', 'simulation_id'),
                (r'start[_\s]date[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2})', 'start_date'),
                (r'end[_\s]date[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2})', 'end_date'),
                (r'algorithm[_\s]type[^>]*>([^<]+)', 'algorithm_type')
            ]
            
            for pattern, param_name in sim_patterns:
                matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
                for i, value in enumerate(matches):
                    suffix = f'_{i}' if i > 0 else ''
                    patterns[f'sim_{param_name}{suffix}'] = value.strip()
        
        return patterns
    
    def intelligent_mapping(self, frontend_data: Dict, backend_data: Dict, page_name: str) -> Dict:
        """Perform intelligent mapping with enhanced pattern matching"""
        
        mapping_results = {
            'exact_matches': {},
            'semantic_matches': {},
            'partial_matches': {},
            'unmapped': {}
        }
        
        structured_mappings = backend_data['structured_mappings']
        searchable_values = backend_data['searchable_values']
        
        print(f"    🧠 Performing intelligent mapping for {page_name}...")
        
        for data_key, data_value in frontend_data.items():
            mapped = False
            
            # 1. Exact value matching
            if str(data_value) in searchable_values:
                matches = searchable_values[str(data_value)]
                mapping_results['exact_matches'][data_key] = {
                    'frontend_value': data_value,
                    'backend_matches': matches,
                    'match_type': 'exact_value',
                    'confidence': 100
                }
                mapped = True
                print(f"      ✅ Exact match: {data_key} = {data_value}")
            
            # 2. Semantic/contextual matching
            elif not mapped:
                semantic_match = self.find_semantic_match(data_key, data_value, structured_mappings, page_name)
                if semantic_match:
                    mapping_results['semantic_matches'][data_key] = semantic_match
                    mapped = True
                    print(f"      🎯 Semantic match: {data_key} -> {semantic_match['backend_context']}")
            
            # 3. Pattern-based partial matching
            elif not mapped:
                partial_match = self.find_partial_match(data_key, data_value, backend_data)
                if partial_match:
                    mapping_results['partial_matches'][data_key] = partial_match
                    mapped = True
                    print(f"      📍 Partial match: {data_key} -> {partial_match['match_reason']}")
            
            # 4. No mapping found
            if not mapped:
                mapping_results['unmapped'][data_key] = {
                    'frontend_value': data_value,
                    'analysis': self.analyze_unmapped_value(data_key, data_value)
                }
                print(f"      ❓ Unmapped: {data_key} = {data_value}")
        
        return mapping_results
    
    def find_semantic_match(self, data_key: str, data_value: str, structured_mappings: Dict, page_context: str) -> Optional[Dict]:
        """Find semantic matches based on context and meaning"""
        
        # Portfolio identity matching
        if 'portfolio' in data_key.lower() and 'portfolio_identity' in structured_mappings:
            for source, portfolio_name in structured_mappings['portfolio_identity'].items():
                if str(data_value) == str(portfolio_name):
                    return {
                        'frontend_value': data_value,
                        'backend_value': portfolio_name,
                        'backend_source': source,
                        'backend_context': 'portfolio_identity',
                        'match_type': 'semantic',
                        'confidence': 95
                    }
        
        # Risk metrics matching
        if any(risk_term in data_key.lower() for risk_term in ['risk', 'volatility', 'drawdown']) and 'risk_metrics' in structured_mappings:
            risk_metrics = structured_mappings['risk_metrics']
            
            # Try to match percentage values
            if '%' in str(data_value):
                numeric_value = float(re.sub(r'[^\d.]', '', str(data_value)))
                
                for risk_key, risk_value in risk_metrics.items():
                    if risk_value is not None:
                        backend_numeric = float(risk_value) * 100 if risk_value < 1 else float(risk_value)
                        if abs(numeric_value - backend_numeric) < 0.1:
                            return {
                                'frontend_value': data_value,
                                'backend_value': risk_value,
                                'backend_source': 'risk_parameters',
                                'backend_context': risk_key,
                                'match_type': 'semantic_numeric',
                                'confidence': 90
                            }
        
        # Asset allocation matching
        if 'allocation' in data_key.lower() or 'asset' in data_key.lower():
            if 'asset_allocations' in structured_mappings:
                allocations = structured_mappings['asset_allocations']
                
                for asset_key, asset_data in allocations.items():
                    if '%' in str(data_value):
                        frontend_percent = float(re.sub(r'[^\d.]', '', str(data_value)))
                        backend_percent = asset_data.get('allocation_percent', 0)
                        
                        if abs(frontend_percent - backend_percent) < 0.1:
                            return {
                                'frontend_value': data_value,
                                'backend_value': backend_percent,
                                'backend_source': 'config',
                                'backend_context': f'{asset_key}_allocation',
                                'match_type': 'semantic_allocation',
                                'confidence': 85
                            }
        
        return None
    
    def find_partial_match(self, data_key: str, data_value: str, backend_data: Dict) -> Optional[Dict]:
        """Find partial matches using fuzzy matching techniques"""
        
        # Numeric similarity matching
        if re.match(r'^[0-9.]+%?$', str(data_value).strip()):
            numeric_value = float(re.sub(r'[^\d.]', '', str(data_value)))
            
            # Search for similar numeric values in backend
            for source_name, source_data in backend_data['raw_data'].items():
                similar_values = self.find_similar_numeric_values(numeric_value, source_data)
                if similar_values:
                    return {
                        'frontend_value': data_value,
                        'backend_candidates': similar_values,
                        'backend_source': source_name,
                        'match_reason': 'numeric_similarity',
                        'confidence': 60
                    }
        
        return None
    
    def find_similar_numeric_values(self, target_value: float, data: Any, path: List[str] = []) -> List[Dict]:
        """Find numeric values within 10% of target value"""
        
        similar_values = []
        
        if isinstance(data, dict):
            for key, value in data.items():
                similar_values.extend(self.find_similar_numeric_values(target_value, value, path + [key]))
        elif isinstance(data, list):
            for i, item in enumerate(data):
                similar_values.extend(self.find_similar_numeric_values(target_value, item, path + [i]))
        else:
            try:
                numeric_value = float(str(data).replace('%', '').replace(',', ''))
                similarity_threshold = max(target_value * 0.1, 0.01)  # 10% or minimum 0.01
                
                if abs(numeric_value - target_value) <= similarity_threshold:
                    similar_values.append({
                        'path': path,
                        'value': data,
                        'similarity_score': 1 - (abs(numeric_value - target_value) / max(target_value, 1))
                    })
            except (ValueError, TypeError):
                pass
        
        return similar_values
    
    def analyze_unmapped_value(self, data_key: str, data_value: str) -> Dict:
        """Analyze unmapped values to provide insights"""
        
        analysis = {
            'value_type': 'unknown',
            'patterns_detected': [],
            'suggestions': []
        }
        
        # Classify value type
        if re.match(r'^[0-9.]+%$', str(data_value)):
            analysis['value_type'] = 'percentage'
            analysis['suggestions'].append('Look for decimal equivalents in backend (e.g., 10% = 0.10)')
        elif re.match(r'^\$[0-9,]+\.[0-9]{2}$', str(data_value)):
            analysis['value_type'] = 'currency'
            analysis['suggestions'].append('Check for numeric-only equivalents in backend')
        elif re.match(r'^[0-9]{4}-[0-9]{2}-[0-9]{2}', str(data_value)):
            analysis['value_type'] = 'timestamp'
            analysis['suggestions'].append('Look for date/datetime fields in backend data')
        elif len(str(data_value).split()) > 3:
            analysis['value_type'] = 'text_description'
            analysis['suggestions'].append('May be computed/display text, check for source components')
        
        # Detect patterns in key
        if 'percentage' in data_key:
            analysis['patterns_detected'].append('generic_percentage_extraction')
        if 'timestamp' in data_key:
            analysis['patterns_detected'].append('generic_timestamp_extraction')
        
        return analysis
    
    def run_intelligent_validation(self, simulation_id: str = "Myportolio") -> Dict:
        """Run comprehensive intelligent validation"""
        
        print("🧠 INTELLIGENT FRONTEND-BACKEND DATA VALIDATION")
        print("=" * 70)
        print(f"Enhanced validation started at: {datetime.now().isoformat()}")
        
        # Load backend data with intelligence
        backend_data = self.load_backend_data()
        
        total_mappings = 0
        total_data_points = 0
        
        # Process each accessible page
        print(f"\n📊 Processing pages with intelligent mapping...")
        
        for page_name, page_config in self.pages_config.items():
            if not page_config['accessible']:
                continue
                
            try:
                print(f"  📄 Processing {page_name}...")
                
                # Fetch and extract data
                response = self.session.get(f"{self.base_url}{page_config['url']}", timeout=15)
                
                if response.status_code == 200:
                    content = response.text
                    page_data = self.extract_enhanced_page_data(page_name, content)
                    
                    # Perform intelligent mapping
                    mapping_results = self.intelligent_mapping(page_data, backend_data, page_name)
                    
                    # Calculate mapping success
                    mapped_count = len(mapping_results['exact_matches']) + len(mapping_results['semantic_matches']) + len(mapping_results['partial_matches'])
                    total_count = len(page_data)
                    coverage_pct = (mapped_count / total_count) * 100 if total_count > 0 else 0
                    
                    # Store results
                    self.results['successful_mappings'][page_name] = mapping_results
                    self.results['validation_scores'][page_name] = {
                        'total_data_points': total_count,
                        'exact_matches': len(mapping_results['exact_matches']),
                        'semantic_matches': len(mapping_results['semantic_matches']),
                        'partial_matches': len(mapping_results['partial_matches']),
                        'unmapped': len(mapping_results['unmapped']),
                        'coverage_percentage': coverage_pct
                    }
                    
                    total_mappings += mapped_count
                    total_data_points += total_count
                    
                    print(f"    📊 Data points: {total_count}")
                    print(f"    ✅ Exact matches: {len(mapping_results['exact_matches'])}")
                    print(f"    🎯 Semantic matches: {len(mapping_results['semantic_matches'])}")
                    print(f"    📍 Partial matches: {len(mapping_results['partial_matches'])}")
                    print(f"    ❓ Unmapped: {len(mapping_results['unmapped'])}")
                    print(f"    📈 Coverage: {coverage_pct:.1f}%")
                
            except Exception as e:
                print(f"    ❌ Error processing {page_name}: {str(e)}")
        
        # Generate overall assessment
        overall_coverage = (total_mappings / total_data_points) * 100 if total_data_points > 0 else 0
        
        self.results['mapping_intelligence'] = {
            'total_data_points': total_data_points,
            'total_mappings': total_mappings,
            'overall_coverage': overall_coverage,
            'validation_success': overall_coverage >= 70
        }
        
        print(f"\n🎯 INTELLIGENT VALIDATION SUMMARY")
        print("=" * 50)
        print(f"  📊 Total Data Points: {total_data_points}")
        print(f"  🔗 Total Mappings: {total_mappings}")
        print(f"  📈 Overall Coverage: {overall_coverage:.1f}%")
        
        if overall_coverage >= 70:
            print(f"  🎉 INTELLIGENT VALIDATION SUCCESSFUL!")
            print(f"     Achieved {overall_coverage:.1f}% mapping coverage")
        else:
            print(f"  ⚠️  Validation needs improvement")
            print(f"     Coverage: {overall_coverage:.1f}% (target: ≥70%)")
        
        return self.results
    
    def save_results(self, filename_prefix: str = "intelligent_validation") -> str:
        """Save intelligent validation results"""
        
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f"{filename_prefix}_{timestamp}.json"
        
        with open(filename, 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        
        print(f"\n💾 INTELLIGENT RESULTS SAVED")
        print(f"   📄 File: {filename}")
        print(f"   📊 Size: {os.path.getsize(filename):,} bytes")
        
        return filename


def main():
    """Run intelligent data validation"""
    
    validator = IntelligentDataMapper()
    
    # Run intelligent validation
    results = validator.run_intelligent_validation("Myportolio")
    
    # Save results
    filename = validator.save_results("intelligent_validation")
    
    print(f"\n🎯 INTELLIGENT VALIDATION COMPLETE")
    print(f"   Results saved to: {filename}")
    
    return results, filename


if __name__ == "__main__":
    main()
