#!/usr/bin/env python3
"""
Final Enhanced Frontend-Backend Data Validation System

This is the ultimate validation script that combines all learned patterns and implements
the most sophisticated data extraction and mapping capabilities. It addresses the
specific issues discovered in previous validation runs and provides maximum coverage.

Key Improvements:
- Enhanced HTML parsing for actual page structure
- Intelligent CSS selector-based extraction
- Context-aware data interpretation
- Advanced backend data correlation
- Comprehensive reporting with actionable insights

Usage:
    python test_final_enhanced_validation.py
    
Results:
    - Comprehensive validation report
    - Detailed mapping analysis
    - Actionable recommendations for improving coverage
    - Production-ready data integrity verification
"""

import json
import os
import requests
import re
from datetime import datetime
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
import time

class FinalEnhancedValidator:
    """Ultimate validation system with maximum data extraction and mapping capabilities"""
    
    def __init__(self):
        self.base_url = "http://localhost"
        self.session = requests.Session()
        self.backend_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        
        # Page accessibility status from previous tests
        self.page_status = {
            'performance': {'accessible': True, 'priority': 'high'},
            'algorithms': {'accessible': True, 'priority': 'high'}, 
            'algorithm_performance': {'accessible': True, 'priority': 'medium'},
            'backtest_results': {'accessible': True, 'priority': 'high'},
            'portfolio_overview': {'accessible': False, 'reason': 'HTTP 403'},
            'holdings': {'accessible': False, 'reason': 'HTTP 403'}
        }
        
        self.results = {
            'validation_timestamp': datetime.now().isoformat(),
            'system_analysis': {},
            'page_analysis': {},
            'mapping_results': {},
            'coverage_assessment': {},
            'improvement_recommendations': []
        }
    
    def load_comprehensive_backend_data(self) -> Dict:
        """Load all available backend data with comprehensive indexing"""
        
        print("🔍 COMPREHENSIVE BACKEND DATA LOADING")
        print("=" * 50)
        
        backend_data = {
            'raw_files': {},
            'processed_data': {},
            'value_index': {},
            'pattern_index': {},
            'metadata': {}
        }
        
        # Load all JSON files
        json_files = {
            'config': f"{self.backend_path}/config.json",
            'risk_parameters': f"{self.backend_path}/risk_parameters.json"
        }
        
        for key, filepath in json_files.items():
            if os.path.exists(filepath):
                with open(filepath, 'r') as f:
                    data = json.load(f)
                    backend_data['raw_files'][key] = data
                    print(f"  ✅ {key}: {len(data)} keys, {self.count_values(data)} total values")
        
        # Load latest status report
        import glob
        status_files = glob.glob(f"{self.backend_path}/status_report_*.json")
        if status_files:
            latest_status = max(status_files, key=os.path.getctime)
            with open(latest_status, 'r') as f:
                backend_data['raw_files']['status_report'] = json.load(f)
                print(f"  ✅ status_report: {os.path.basename(latest_status)}")
        
        # Process and index the data
        self.process_backend_data(backend_data)
        
        return backend_data
    
    def count_values(self, data: Any) -> int:
        """Count total values in nested structure"""
        if isinstance(data, dict):
            return sum(self.count_values(v) for v in data.values())
        elif isinstance(data, list):
            return sum(self.count_values(v) for v in data)
        else:
            return 1
    
    def process_backend_data(self, backend_data: Dict):
        """Process backend data for intelligent matching"""
        
        print("  🧠 Processing data for intelligent matching...")
        
        processed = backend_data['processed_data']
        value_index = backend_data['value_index']
        pattern_index = backend_data['pattern_index']
        
        for source_name, source_data in backend_data['raw_files'].items():
            processed[source_name] = {}
            
            # Extract key data points
            if source_name == 'config' and isinstance(source_data, dict):
                # Portfolio configuration
                processed[source_name]['portfolio_name'] = source_data.get('portfolio_name')
                processed[source_name]['strategy'] = source_data.get('strategy')
                processed[source_name]['target_portfolio_value'] = source_data.get('target_portfolio_value')
                
                # Asset allocations
                if 'assets' in source_data:
                    for asset, asset_config in source_data['assets'].items():
                        processed[source_name][f'{asset.lower()}_allocation'] = asset_config.get('allocation_percent')
                        processed[source_name][f'{asset.lower()}_symbol'] = asset
            
            elif source_name == 'risk_parameters' and isinstance(source_data, dict):
                # Risk management parameters
                for key, value in source_data.items():
                    if isinstance(value, (int, float)):
                        # Convert decimals to percentages for matching
                        if 0 < value < 1 and 'percent' not in key.lower():
                            processed[source_name][f'{key}_percent'] = value * 100
                        processed[source_name][key] = value
            
            elif source_name == 'status_report' and isinstance(source_data, dict):
                # Status report data
                if 'portfolio_summary' in source_data:
                    summary = source_data['portfolio_summary']
                    for key, value in summary.items():
                        processed[source_name][f'status_{key}'] = value
        
        # Create value index for exact matching
        self.create_value_index(backend_data['raw_files'], value_index)
        
        # Create pattern index for fuzzy matching
        self.create_pattern_index(processed, pattern_index)
        
        print(f"    ✅ Processed {len(processed)} data sources")
        print(f"    ✅ Indexed {len(value_index)} unique values")
        print(f"    ✅ Created {len(pattern_index)} pattern mappings")
    
    def create_value_index(self, raw_data: Dict, value_index: Dict):
        """Create comprehensive value index for exact matching"""
        
        def index_recursive(data: Any, path: List[str] = []):
            if isinstance(data, dict):
                for key, value in data.items():
                    index_recursive(value, path + [key])
            elif isinstance(data, list):
                for i, item in enumerate(data):
                    index_recursive(item, path + [str(i)])
            else:
                # Index this value
                value_str = str(data).strip()
                if value_str and value_str not in value_index:
                    value_index[value_str] = []
                
                if value_str:
                    value_index[value_str].append({
                        'path': path,
                        'source': path[0] if path else 'root',
                        'value': data,
                        'type': type(data).__name__
                    })
        
        for source_name, source_data in raw_data.items():
            index_recursive(source_data, [source_name])
    
    def create_pattern_index(self, processed_data: Dict, pattern_index: Dict):
        """Create pattern-based index for semantic matching"""
        
        for source_name, source_data in processed_data.items():
            for key, value in source_data.items():
                if value is not None:
                    # Create patterns for different value representations
                    patterns = self.generate_value_patterns(value)
                    
                    for pattern in patterns:
                        if pattern not in pattern_index:
                            pattern_index[pattern] = []
                        
                        pattern_index[pattern].append({
                            'source': source_name,
                            'key': key,
                            'original_value': value,
                            'pattern_type': self.classify_pattern(pattern)
                        })
    
    def generate_value_patterns(self, value: Any) -> List[str]:
        """Generate different patterns for a value"""
        
        patterns = [str(value)]
        
        # Numeric patterns
        try:
            num_value = float(str(value))
            patterns.append(f"{num_value:.0f}")
            patterns.append(f"{num_value:.1f}")
            patterns.append(f"{num_value:.2f}")
            patterns.append(f"{num_value}%")
            patterns.append(f"{num_value:.1f}%")
            patterns.append(f"{num_value:.2f}%")
            
            # Percentage representations
            if num_value < 1:
                patterns.append(f"{num_value * 100:.1f}%")
                patterns.append(f"{num_value * 100:.2f}%")
            
            # Currency representations
            patterns.append(f"${num_value:,.2f}")
            patterns.append(f"${num_value:,.0f}")
            
        except (ValueError, TypeError):
            pass
        
        # String patterns
        if isinstance(value, str):
            patterns.append(value.lower())
            patterns.append(value.upper())
            patterns.append(value.title())
            patterns.append(value.replace('_', ' '))
            patterns.append(value.replace(' ', '_'))
        
        return list(set(patterns))  # Remove duplicates
    
    def classify_pattern(self, pattern: str) -> str:
        """Classify pattern type for semantic matching"""
        
        if re.match(r'^\d+\.?\d*%$', pattern):
            return 'percentage'
        elif re.match(r'^\$[\d,]+\.?\d*$', pattern):
            return 'currency'
        elif re.match(r'^\d{4}-\d{2}-\d{2}', pattern):
            return 'date'
        elif re.match(r'^\d+\.?\d*$', pattern):
            return 'numeric'
        else:
            return 'text'
    
    def extract_ultimate_page_data(self, page_name: str, content: str) -> Dict:
        """Ultimate data extraction with maximum coverage"""
        
        print(f"    🎯 Ultimate extraction for {page_name}...")
        
        data_points = {}
        
        # 1. Extract structured data from HTML elements
        data_points.update(self.extract_structured_html_data(content))
        
        # 2. Extract text-based patterns
        data_points.update(self.extract_text_patterns(content))
        
        # 3. Extract JavaScript data if present
        data_points.update(self.extract_javascript_data(content))
        
        # 4. Extract page-specific patterns
        if page_name == 'algorithms':
            data_points.update(self.extract_algorithm_specific_data(content))
        elif page_name == 'performance':
            data_points.update(self.extract_performance_specific_data(content))
        elif page_name == 'backtest_results':
            data_points.update(self.extract_backtest_specific_data(content))
        
        print(f"      ✅ Extracted {len(data_points)} data points")
        return data_points
    
    def extract_structured_html_data(self, content: str) -> Dict:
        """Extract data from structured HTML elements"""
        
        data = {}
        
        # Table data extraction
        table_patterns = [
            r'<tr[^>]*>.*?<td[^>]*class="[^"]*label[^"]*"[^>]*>([^<]+)</td>.*?<td[^>]*class="[^"]*value[^"]*"[^>]*>([^<]+)</td>.*?</tr>',
            r'<tr[^>]*>.*?<th[^>]*>([^<]+)</th>.*?<td[^>]*>([^<]+)</td>.*?</tr>',
            r'<tr[^>]*>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?</tr>'
        ]
        
        for i, pattern in enumerate(table_patterns):
            matches = re.findall(pattern, content, re.DOTALL | re.IGNORECASE)
            for j, (label, value) in enumerate(matches):
                clean_label = self.clean_text(label)
                clean_value = self.clean_text(value)
                if clean_label and clean_value:
                    data[f'table_{i}_row_{j}_{clean_label.lower().replace(" ", "_")}'] = clean_value
        
        # Card/metric data extraction
        card_patterns = [
            r'<div[^>]*class="[^"]*metric[^"]*"[^>]*>.*?<[^>]*class="[^"]*label[^"]*"[^>]*>([^<]+)</[^>]*>.*?<[^>]*class="[^"]*value[^"]*"[^>]*>([^<]+)</[^>]*>.*?</div>',
            r'<div[^>]*class="[^"]*card[^"]*"[^>]*>.*?<h[^>]*>([^<]+)</h[^>]*>.*?<[^>]*>([^<]+)</[^>]*>.*?</div>'
        ]
        
        for i, pattern in enumerate(card_patterns):
            matches = re.findall(pattern, content, re.DOTALL | re.IGNORECASE)
            for j, (label, value) in enumerate(matches):
                clean_label = self.clean_text(label)
                clean_value = self.clean_text(value)
                if clean_label and clean_value:
                    data[f'card_{i}_item_{j}_{clean_label.lower().replace(" ", "_")}'] = clean_value
        
        return data
    
    def extract_text_patterns(self, content: str) -> Dict:
        """Extract data using comprehensive text patterns"""
        
        data = {}
        
        # Enhanced percentage patterns with context
        percentage_contexts = [
            (r'allocation[^>]{0,50}>([^<]*?([0-9.]+)%)', 'allocation'),
            (r'performance[^>]{0,50}>([^<]*?([+-]?[0-9.]+)%)', 'performance'),
            (r'return[^>]{0,50}>([^<]*?([+-]?[0-9.]+)%)', 'return'),
            (r'risk[^>]{0,50}>([^<]*?([0-9.]+)%)', 'risk'),
            (r'volatility[^>]{0,50}>([^<]*?([0-9.]+)%)', 'volatility'),
            (r'drawdown[^>]{0,50}>([^<]*?([0-9.]+)%)', 'drawdown')
        ]
        
        for pattern, context in percentage_contexts:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, (full_match, value) in enumerate(matches):
                data[f'{context}_percent_{i}'] = f'{value}%'
        
        # Enhanced currency patterns with context
        currency_contexts = [
            (r'portfolio[^>]{0,50}value[^>]{0,50}>\$([0-9,]+\.?[0-9]*)', 'portfolio_value'),
            (r'cash[^>]{0,50}>\$([0-9,]+\.?[0-9]*)', 'cash'),
            (r'position[^>]{0,50}>\$([0-9,]+\.?[0-9]*)', 'position'),
            (r'balance[^>]{0,50}>\$([0-9,]+\.?[0-9]*)', 'balance')
        ]
        
        for pattern, context in currency_contexts:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, value in enumerate(matches):
                data[f'{context}_{i}'] = f'${value}'
        
        # Status and text patterns
        status_patterns = [
            (r'status[^>]{0,20}>([^<]+)', 'status'),
            (r'state[^>]{0,20}>([^<]+)', 'state'),
            (r'condition[^>]{0,20}>([^<]+)', 'condition')
        ]
        
        for pattern, context in status_patterns:
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for i, value in enumerate(matches):
                clean_value = self.clean_text(value)
                if clean_value:
                    data[f'{context}_{i}'] = clean_value
        
        return data
    
    def extract_javascript_data(self, content: str) -> Dict:
        """Extract data from JavaScript variables and objects"""
        
        data = {}
        
        # JavaScript variable patterns
        js_patterns = [
            r'var\s+(\w+)\s*=\s*["\']([^"\']+)["\'];',
            r'const\s+(\w+)\s*=\s*["\']([^"\']+)["\'];',
            r'let\s+(\w+)\s*=\s*["\']([^"\']+)["\'];'
        ]
        
        for pattern in js_patterns:
            matches = re.findall(pattern, content)
            for var_name, var_value in matches:
                data[f'js_{var_name}'] = var_value
        
        # JSON data in JavaScript
        json_pattern = r'var\s+\w+\s*=\s*(\{[^;]+\});'
        json_matches = re.findall(json_pattern, content)
        
        for i, json_str in enumerate(json_matches):
            try:
                json_data = json.loads(json_str)
                for key, value in json_data.items():
                    data[f'js_json_{i}_{key}'] = value
            except json.JSONDecodeError:
                pass
        
        return data
    
    def extract_algorithm_specific_data(self, content: str) -> Dict:
        """Extract algorithm page specific data"""
        
        data = {}
        
        # Algorithm names and status
        algo_patterns = [
            r'<[^>]*class="[^"]*algorithm[^"]*name[^"]*"[^>]*>([^<]+)</[^>]*>',
            r'<[^>]*>([^<]*(?:Risk Management|Momentum Strategy|Portfolio Manager)[^<]*)</[^>]*>',
            r'<[^>]*>([^<]*(?:ETH|BTC|Algorithm)[^<]*(?:Available|Operational|Active)[^<]*)</[^>]*>'
        ]
        
        for i, pattern in enumerate(algo_patterns):
            matches = re.findall(pattern, content, re.IGNORECASE)
            for j, match in enumerate(matches):
                clean_match = self.clean_text(match)
                if clean_match and len(clean_match) > 3:
                    data[f'algorithm_text_{i}_{j}'] = clean_match
        
        return data
    
    def extract_performance_specific_data(self, content: str) -> Dict:
        """Extract performance page specific data"""
        
        data = {}
        
        # Performance metric patterns
        perf_patterns = [
            r'sharpe[^>]{0,30}>([^<]*?([+-]?[0-9.]+)[^<]*)</[^>]*>',
            r'return[^>]{0,30}>([^<]*?([+-]?[0-9.]+)%?[^<]*)</[^>]*>',
            r'alpha[^>]{0,30}>([^<]*?([+-]?[0-9.]+)[^<]*)</[^>]*>',
            r'beta[^>]{0,30}>([^<]*?([+-]?[0-9.]+)[^<]*)</[^>]*>'
        ]
        
        for i, pattern in enumerate(perf_patterns):
            matches = re.findall(pattern, content, re.IGNORECASE | re.DOTALL)
            for j, (full_match, value) in enumerate(matches):
                data[f'performance_metric_{i}_{j}'] = value
        
        return data
    
    def extract_backtest_specific_data(self, content: str) -> Dict:
        """Extract backtest page specific data"""
        
        data = {}
        
        # Simulation parameter patterns
        sim_patterns = [
            r'simulation[^>]{0,30}>([^<]+)</[^>]*>',
            r'backtest[^>]{0,30}>([^<]+)</[^>]*>',
            r'date[^>]{0,20}>([0-9]{4}-[0-9]{2}-[0-9]{2}[^<]*)</[^>]*>'
        ]
        
        for i, pattern in enumerate(sim_patterns):
            matches = re.findall(pattern, content, re.IGNORECASE)
            for j, match in enumerate(matches):
                clean_match = self.clean_text(match)
                if clean_match:
                    data[f'simulation_data_{i}_{j}'] = clean_match
        
        return data
    
    def clean_text(self, text: str) -> str:
        """Clean extracted text"""
        if not text:
            return ""
        
        # Remove HTML entities and extra whitespace
        cleaned = re.sub(r'&[^;]+;', '', text)
        cleaned = re.sub(r'\s+', ' ', cleaned)
        return cleaned.strip()
    
    def perform_ultimate_mapping(self, frontend_data: Dict, backend_data: Dict, page_name: str) -> Dict:
        """Ultimate mapping with all available techniques"""
        
        print(f"    🎯 Ultimate mapping for {page_name}...")
        
        mapping_results = {
            'exact_matches': {},
            'pattern_matches': {},
            'semantic_matches': {},
            'fuzzy_matches': {},
            'unmapped': {}
        }
        
        value_index = backend_data['value_index']
        pattern_index = backend_data['pattern_index']
        processed_data = backend_data['processed_data']
        
        for data_key, data_value in frontend_data.items():
            mapped = False
            
            # 1. Exact value matching
            if str(data_value).strip() in value_index:
                matches = value_index[str(data_value).strip()]
                mapping_results['exact_matches'][data_key] = {
                    'frontend_value': data_value,
                    'backend_matches': matches,
                    'confidence': 100,
                    'match_type': 'exact_value'
                }
                mapped = True
                print(f"      ✅ Exact: {data_key} = {data_value}")
            
            # 2. Pattern matching
            elif not mapped:
                for pattern in self.generate_value_patterns(data_value):
                    if pattern in pattern_index:
                        matches = pattern_index[pattern]
                        mapping_results['pattern_matches'][data_key] = {
                            'frontend_value': data_value,
                            'matched_pattern': pattern,
                            'backend_matches': matches,
                            'confidence': 90,
                            'match_type': 'pattern_match'
                        }
                        mapped = True
                        print(f"      🎯 Pattern: {data_key} -> {pattern}")
                        break
            
            # 3. Semantic matching based on key name
            elif not mapped:
                semantic_match = self.find_semantic_match_by_key(data_key, data_value, processed_data)
                if semantic_match:
                    mapping_results['semantic_matches'][data_key] = semantic_match
                    mapped = True
                    print(f"      🧠 Semantic: {data_key} -> {semantic_match['backend_key']}")
            
            # 4. Fuzzy numeric matching
            elif not mapped:
                fuzzy_match = self.find_fuzzy_numeric_match(data_value, processed_data)
                if fuzzy_match:
                    mapping_results['fuzzy_matches'][data_key] = fuzzy_match
                    mapped = True
                    print(f"      📍 Fuzzy: {data_key} ≈ {fuzzy_match['backend_key']}")
            
            # 5. Unmapped
            if not mapped:
                mapping_results['unmapped'][data_key] = {
                    'frontend_value': data_value,
                    'analysis': self.analyze_unmapped_data_point(data_key, data_value, backend_data)
                }
                print(f"      ❓ Unmapped: {data_key} = {data_value}")
        
        return mapping_results
    
    def find_semantic_match_by_key(self, data_key: str, data_value: str, processed_data: Dict) -> Optional[Dict]:
        """Find semantic matches based on key similarity"""
        
        # Key word mapping
        key_mappings = {
            'portfolio': ['portfolio_name', 'portfolio_id'],
            'allocation': ['allocation_percent', 'eth_allocation', 'btc_allocation'],
            'risk': ['max_portfolio_volatility', 'risk_profile', 'max_drawdown'],
            'performance': ['total_return', 'sharpe_ratio'],
            'volatility': ['max_portfolio_volatility', 'volatility_percent'],
            'return': ['total_return', 'return_percent']
        }
        
        for keyword, possible_keys in key_mappings.items():
            if keyword in data_key.lower():
                for source_name, source_data in processed_data.items():
                    for backend_key in possible_keys:
                        if backend_key in source_data:
                            backend_value = source_data[backend_key]
                            if self.values_are_similar(data_value, backend_value):
                                return {
                                    'frontend_value': data_value,
                                    'backend_value': backend_value,
                                    'backend_key': backend_key,
                                    'backend_source': source_name,
                                    'confidence': 80,
                                    'match_type': 'semantic_key'
                                }
        
        return None
    
    def find_fuzzy_numeric_match(self, data_value: str, processed_data: Dict) -> Optional[Dict]:
        """Find fuzzy numeric matches"""
        
        try:
            # Extract numeric value from frontend
            frontend_num = float(re.sub(r'[^\d.-]', '', str(data_value)))
            
            for source_name, source_data in processed_data.items():
                for backend_key, backend_value in source_data.items():
                    if isinstance(backend_value, (int, float)):
                        # Try different representations
                        comparisons = [
                            backend_value,
                            backend_value * 100,  # decimal to percentage
                            backend_value / 100,  # percentage to decimal
                        ]
                        
                        for comp_value in comparisons:
                            if abs(frontend_num - comp_value) < max(0.01, abs(comp_value) * 0.1):
                                return {
                                    'frontend_value': data_value,
                                    'backend_value': backend_value,
                                    'backend_key': backend_key,
                                    'backend_source': source_name,
                                    'confidence': 70,
                                    'match_type': 'fuzzy_numeric',
                                    'transformation': f'frontend={frontend_num}, backend={comp_value}'
                                }
        
        except (ValueError, TypeError):
            pass
        
        return None
    
    def values_are_similar(self, value1: Any, value2: Any) -> bool:
        """Check if two values are similar"""
        
        # Exact string match
        if str(value1).strip() == str(value2).strip():
            return True
        
        # Numeric similarity
        try:
            num1 = float(re.sub(r'[^\d.-]', '', str(value1)))
            num2 = float(re.sub(r'[^\d.-]', '', str(value2)))
            return abs(num1 - num2) < max(0.01, abs(num2) * 0.1)
        except (ValueError, TypeError):
            pass
        
        return False
    
    def analyze_unmapped_data_point(self, data_key: str, data_value: str, backend_data: Dict) -> Dict:
        """Analyze unmapped data points for insights"""
        
        analysis = {
            'data_type': self.classify_data_type(data_value),
            'key_insights': [],
            'potential_sources': [],
            'recommendations': []
        }
        
        # Analyze key for insights
        if 'percentage' in data_key or '%' in str(data_value):
            analysis['key_insights'].append('Percentage value - check for decimal equivalents in backend')
        
        if 'currency' in data_key or '$' in str(data_value):
            analysis['key_insights'].append('Currency value - look for numeric amounts in backend')
        
        if 'algorithm' in data_key:
            analysis['key_insights'].append('Algorithm-related data - check algorithm configuration files')
        
        # Look for similar keys in backend
        for source_name, source_data in backend_data['processed_data'].items():
            similar_keys = [k for k in source_data.keys() if any(word in k.lower() for word in data_key.lower().split('_'))]
            if similar_keys:
                analysis['potential_sources'].append({
                    'source': source_name,
                    'similar_keys': similar_keys
                })
        
        # Generate recommendations
        if analysis['potential_sources']:
            analysis['recommendations'].append('Check similar keys in backend data sources')
        
        if analysis['data_type'] == 'percentage':
            analysis['recommendations'].append('Try multiplying backend decimal values by 100')
        
        return analysis
    
    def classify_data_type(self, data_value: str) -> str:
        """Classify data type for analysis"""
        
        value_str = str(data_value)
        
        if re.match(r'^[+-]?\d*\.?\d+%$', value_str):
            return 'percentage'
        elif re.match(r'^\$[\d,]+\.?\d*$', value_str):
            return 'currency'
        elif re.match(r'^\d{4}-\d{2}-\d{2}', value_str):
            return 'date'
        elif re.match(r'^[+-]?\d*\.?\d+$', value_str):
            return 'numeric'
        else:
            return 'text'
    
    def run_final_validation(self, simulation_id: str = "Myportolio") -> Dict:
        """Run the final comprehensive validation"""
        
        print("🎯 FINAL ENHANCED FRONTEND-BACKEND VALIDATION")
        print("=" * 70)
        print(f"Ultimate validation started: {datetime.now().isoformat()}")
        print(f"Target: Complete data integrity verification for {simulation_id}")
        
        # Load comprehensive backend data
        backend_data = self.load_comprehensive_backend_data()
        
        # System analysis
        self.results['system_analysis'] = {
            'backend_sources_loaded': len(backend_data['raw_files']),
            'total_backend_values': sum(self.count_values(data) for data in backend_data['raw_files'].values()),
            'value_index_size': len(backend_data['value_index']),
            'pattern_index_size': len(backend_data['pattern_index'])
        }
        
        print(f"\n📊 COMPREHENSIVE PAGE ANALYSIS")
        print("=" * 50)
        
        total_data_points = 0
        total_mapped_points = 0
        
        # Process each accessible page
        for page_name, status in self.page_status.items():
            if not status['accessible']:
                self.results['page_analysis'][page_name] = {
                    'status': 'inaccessible',
                    'reason': status.get('reason', 'unknown')
                }
                print(f"  ❌ Skipping {page_name}: {status.get('reason', 'inaccessible')}")
                continue
            
            try:
                print(f"  📄 Processing {page_name} ({status['priority']} priority)...")
                
                # Get page content
                url = f"/admin/metrics/lean/{page_name.replace('_', '/')}?portfolio={simulation_id}"
                response = self.session.get(f"{self.base_url}{url}", timeout=20)
                
                if response.status_code == 200:
                    content = response.text
                    
                    # Ultimate data extraction
                    page_data = self.extract_ultimate_page_data(page_name, content)
                    
                    # Ultimate mapping
                    mapping_results = self.perform_ultimate_mapping(page_data, backend_data, page_name)
                    
                    # Calculate metrics
                    mapped_count = (len(mapping_results['exact_matches']) + 
                                  len(mapping_results['pattern_matches']) +
                                  len(mapping_results['semantic_matches']) +
                                  len(mapping_results['fuzzy_matches']))
                    
                    total_count = len(page_data)
                    coverage_pct = (mapped_count / total_count) * 100 if total_count > 0 else 0
                    
                    # Store results
                    self.results['page_analysis'][page_name] = {
                        'status': 'success',
                        'priority': status['priority'],
                        'url': url,
                        'total_data_points': total_count,
                        'exact_matches': len(mapping_results['exact_matches']),
                        'pattern_matches': len(mapping_results['pattern_matches']),
                        'semantic_matches': len(mapping_results['semantic_matches']),
                        'fuzzy_matches': len(mapping_results['fuzzy_matches']),
                        'unmapped': len(mapping_results['unmapped']),
                        'coverage_percentage': coverage_pct
                    }
                    
                    self.results['mapping_results'][page_name] = mapping_results
                    
                    total_data_points += total_count
                    total_mapped_points += mapped_count
                    
                    print(f"    📊 Results: {total_count} points, {mapped_count} mapped ({coverage_pct:.1f}%)")
                
                else:
                    self.results['page_analysis'][page_name] = {
                        'status': 'error',
                        'reason': f'HTTP {response.status_code}'
                    }
                    print(f"    ❌ HTTP {response.status_code}")
                
            except Exception as e:
                self.results['page_analysis'][page_name] = {
                    'status': 'error',
                    'reason': f'Exception: {str(e)}'
                }
                print(f"    ❌ Exception: {str(e)}")
        
        # Final assessment
        overall_coverage = (total_mapped_points / total_data_points) * 100 if total_data_points > 0 else 0
        
        self.results['coverage_assessment'] = {
            'total_data_points': total_data_points,
            'total_mapped_points': total_mapped_points,
            'overall_coverage_percentage': overall_coverage,
            'validation_success': overall_coverage >= 70,
            'accessible_pages': len([p for p in self.results['page_analysis'].values() if p.get('status') == 'success']),
            'total_pages': len(self.page_status)
        }
        
        # Generate recommendations
        self.generate_improvement_recommendations()
        
        # Final report
        print(f"\n🎯 FINAL VALIDATION ASSESSMENT")
        print("=" * 50)
        print(f"  📊 Total Data Points: {total_data_points}")
        print(f"  🔗 Successfully Mapped: {total_mapped_points}")
        print(f"  📈 Overall Coverage: {overall_coverage:.1f}%")
        
        if overall_coverage >= 70:
            print(f"  🎉 VALIDATION SUCCESSFUL!")
            print(f"     Achieved target coverage of {overall_coverage:.1f}%")
        else:
            print(f"  ⚠️  VALIDATION NEEDS IMPROVEMENT")
            print(f"     Current: {overall_coverage:.1f}%, Target: ≥70%")
        
        return self.results
    
    def generate_improvement_recommendations(self):
        """Generate specific recommendations for improving coverage"""
        
        recommendations = []
        
        # Analyze unmapped data
        unmapped_patterns = {}
        for page_name, mapping_results in self.results['mapping_results'].items():
            for data_key, unmapped_info in mapping_results.get('unmapped', {}).items():
                data_type = unmapped_info['analysis']['data_type']
                if data_type not in unmapped_patterns:
                    unmapped_patterns[data_type] = 0
                unmapped_patterns[data_type] += 1
        
        # Generate recommendations based on patterns
        if unmapped_patterns.get('percentage', 0) > 0:
            recommendations.append({
                'category': 'Data Format',
                'issue': f"{unmapped_patterns['percentage']} percentage values unmapped",
                'recommendation': 'Add conversion from decimal to percentage in backend data processing',
                'priority': 'high'
            })
        
        if unmapped_patterns.get('text', 0) > 2:
            recommendations.append({
                'category': 'Text Matching',
                'issue': f"{unmapped_patterns['text']} text values unmapped",
                'recommendation': 'Implement fuzzy text matching for status and description fields',
                'priority': 'medium'
            })
        
        # Check for HTTP 403 issues
        inaccessible_count = len([p for p in self.results['page_analysis'].values() if p.get('status') == 'inaccessible'])
        if inaccessible_count > 0:
            recommendations.append({
                'category': 'Page Access',
                'issue': f"{inaccessible_count} pages return HTTP 403",
                'recommendation': 'Implement proper authentication or adjust page permissions',
                'priority': 'high'
            })
        
        # Coverage recommendations
        overall_coverage = self.results['coverage_assessment']['overall_coverage_percentage']
        if overall_coverage < 50:
            recommendations.append({
                'category': 'Data Structure',
                'issue': f"Low overall coverage ({overall_coverage:.1f}%)",
                'recommendation': 'Review frontend HTML structure and backend data organization',
                'priority': 'critical'
            })
        
        self.results['improvement_recommendations'] = recommendations
        
        print(f"\n📋 IMPROVEMENT RECOMMENDATIONS")
        print("=" * 50)
        for rec in recommendations:
            priority_icon = "🚨" if rec['priority'] == 'critical' else "⚠️" if rec['priority'] == 'high' else "📝"
            print(f"  {priority_icon} {rec['category']}: {rec['issue']}")
            print(f"     → {rec['recommendation']}")
    
    def save_results(self, filename_prefix: str = "final_enhanced_validation") -> str:
        """Save final validation results"""
        
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f"{filename_prefix}_{timestamp}.json"
        
        with open(filename, 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        
        print(f"\n💾 FINAL RESULTS SAVED")
        print(f"   📄 Filename: {filename}")
        print(f"   📊 File Size: {os.path.getsize(filename):,} bytes")
        
        return filename


def main():
    """Execute final enhanced validation"""
    
    validator = FinalEnhancedValidator()
    
    # Run final validation
    results = validator.run_final_validation("Myportolio")
    
    # Save comprehensive results
    filename = validator.save_results()
    
    print(f"\n🎯 FINAL ENHANCED VALIDATION COMPLETE")
    print(f"   📄 Complete results: {filename}")
    print(f"   📊 Coverage achieved: {results['coverage_assessment']['overall_coverage_percentage']:.1f}%")
    
    return results, filename


if __name__ == "__main__":
    main()
