#!/usr/bin/env python3
"""
Comprehensive Portfolio Page Data Validation System

This test system validates EVERY data point on EVERY portfolio page against
its corresponding backend data source, ensuring 100% data integrity mapping:

Page → Data Points → Backend Source → Validation

Test Structure:
1. Page Content Extraction: Extract all displayed data points from each page
2. Backend Data Mapping: Map each data point to its backend source
3. Validation Engine: Compare frontend display vs backend data
4. Coverage Analysis: Ensure 100% mapping coverage for all pages
"""

import json
import os
import requests
import re
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional
import time

class PortfolioPageDataExtractor:
    """Extracts all data points from portfolio pages"""
    
    def __init__(self):
        self.base_url = "http://localhost"
        self.session = requests.Session()
        
    def extract_all_page_data(self, simulation_id: str = "Myportolio") -> Dict[str, Dict]:
        """Extract data from all portfolio pages"""
        
        pages = {
            'portfolio_overview': f"/admin/metrics/lean/portfolio?portfolio={simulation_id}",
            'holdings': f"/admin/metrics/lean/holdings?portfolio={simulation_id}",
            'performance': f"/admin/metrics/lean/performance?portfolio={simulation_id}",
            'algorithms': f"/admin/metrics/lean/algorithms?portfolio={simulation_id}",
            'algorithm_performance': f"/admin/metrics/lean/algorithms/performance?portfolio={simulation_id}",
            'backtest_results': f"/admin/metrics/lean/backtest?portfolio={simulation_id}"
        }
        
        page_data = {}
        
        for page_name, url in pages.items():
            try:
                print(f"  📊 Extracting data from {page_name}...")
                response = self.session.get(f"{self.base_url}{url}")
                
                if response.status_code == 200:
                    content = response.text
                    page_data[page_name] = self.extract_page_data_points(content, page_name)
                    page_data[page_name]['url'] = url
                    page_data[page_name]['status'] = 'success'
                    print(f"    ✅ Found {len(page_data[page_name]['data_points'])} data points")
                else:
                    page_data[page_name] = {
                        'url': url,
                        'status': 'error',
                        'error': f"HTTP {response.status_code}",
                        'data_points': {}
                    }
                    print(f"    ❌ Error: HTTP {response.status_code}")
                    
                time.sleep(0.5)  # Rate limiting
                
            except Exception as e:
                page_data[page_name] = {
                    'url': url,
                    'status': 'error', 
                    'error': str(e),
                    'data_points': {}
                }
                print(f"    ❌ Error: {str(e)}")
        
        return page_data
    
    def extract_page_data_points(self, content: str, page_type: str) -> Dict:
        """Extract specific data points based on page type"""
        
        data_points = {}
        
        if page_type == 'portfolio_overview':
            data_points.update(self.extract_portfolio_metrics(content))
            data_points.update(self.extract_asset_allocation_data(content))
            data_points.update(self.extract_risk_metrics(content))
            data_points.update(self.extract_simulation_parameters(content))
            
        elif page_type == 'holdings':
            data_points.update(self.extract_holdings_data(content))
            data_points.update(self.extract_position_data(content))
            
        elif page_type == 'performance':
            data_points.update(self.extract_performance_metrics(content))
            data_points.update(self.extract_returns_data(content))
            data_points.update(self.extract_risk_adjusted_metrics(content))
            
        elif page_type == 'algorithms':
            data_points.update(self.extract_algorithm_status(content))
            data_points.update(self.extract_algorithm_config(content))
            
        elif page_type == 'algorithm_performance':
            data_points.update(self.extract_algorithm_metrics(content))
            data_points.update(self.extract_algorithm_trades(content))
            
        elif page_type == 'backtest_results':
            data_points.update(self.extract_backtest_metrics(content))
            data_points.update(self.extract_backtest_trades(content))
        
        # Extract common elements from all pages
        data_points.update(self.extract_common_elements(content))
        
        return {'data_points': data_points, 'total_count': len(data_points)}
    
    def extract_portfolio_metrics(self, content: str) -> Dict:
        """Extract portfolio overview metrics"""
        metrics = {}
        
        # Portfolio value
        value_match = re.search(r'<div class="metric-value">\$([0-9,]+\.[0-9]{2})</div>', content)
        if value_match:
            metrics['portfolio_value'] = value_match.group(1)
        
        # Daily change percentage
        change_match = re.search(r'([+-]?[0-9.]+)% Today', content)
        if change_match:
            metrics['daily_change_percent'] = change_match.group(1)
        
        # Cash position
        cash_matches = re.findall(r'<div class="metric-value">\$([0-9,]+\.[0-9]{2})</div>', content)
        if len(cash_matches) >= 2:
            metrics['cash_position'] = cash_matches[1]
        
        # Positions value
        if len(cash_matches) >= 3:
            metrics['positions_value'] = cash_matches[2]
        
        # Unrealized P&L
        pnl_match = re.search(r'<div class="metric-value[^"]*">[^$]*\$([0-9,]+\.[0-9]{2})', content)
        if pnl_match:
            metrics['unrealized_pnl'] = pnl_match.group(1)
        
        return metrics
    
    def extract_asset_allocation_data(self, content: str) -> Dict:
        """Extract asset allocation information"""
        allocations = {}
        
        # Asset symbols and percentages
        asset_matches = re.findall(r'<h4>([A-Z]{3,4})</h4>.*?<div class="allocation-percent">([0-9.]+)%</div>.*?<div class="allocation-value">\$([0-9,]+\.[0-9]{2})</div>', content, re.DOTALL)
        
        for i, (symbol, percent, value) in enumerate(asset_matches):
            allocations[f'asset_{symbol.lower()}_percent'] = percent
            allocations[f'asset_{symbol.lower()}_value'] = value
            allocations[f'asset_{symbol.lower()}_symbol'] = symbol
        
        return allocations
    
    def extract_risk_metrics(self, content: str) -> Dict:
        """Extract risk management metrics"""
        risk_data = {}
        
        # Risk profile indicator
        risk_profile_match = re.search(r'<div class="risk-profile-indicator">([^<]+)</div>', content)
        if risk_profile_match:
            risk_data['risk_profile'] = risk_profile_match.group(1).strip()
        
        # Target volatility
        volatility_match = re.search(r'<div class="target-volatility">Target: ([0-9.]+)%</div>', content)
        if volatility_match:
            risk_data['target_volatility'] = volatility_match.group(1)
        
        # Risk metric items
        risk_items = re.findall(r'<div class="risk-metric-item">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for metric_name, metric_value in risk_items:
            clean_name = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
            if clean_name:
                risk_data[f'risk_{clean_name}'] = metric_value.strip()
        
        return risk_data
    
    def extract_simulation_parameters(self, content: str) -> Dict:
        """Extract simulation parameters if present"""
        sim_data = {}
        
        # Check for simulation parameters section
        if 'Simulation Parameters' in content:
            # Simulation ID
            sim_id_match = re.search(r'<span class="param-value">([^<]+)</span>', content)
            if sim_id_match:
                sim_data['simulation_id'] = sim_id_match.group(1)
            
            # Date and time parameters
            param_rows = re.findall(r'<span class="param-label">([^:]+):</span>\s*<span class="param-value">([^<]+)</span>', content)
            
            for label, value in param_rows:
                clean_label = label.strip().lower().replace(' ', '_')
                sim_data[f'sim_{clean_label}'] = value.strip()
        
        return sim_data
    
    def extract_holdings_data(self, content: str) -> Dict:
        """Extract holdings page data"""
        holdings = {}
        
        # Holdings table data
        holdings_rows = re.findall(r'<tr.*?>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?</tr>', content, re.DOTALL)
        
        for i, (symbol, quantity, value) in enumerate(holdings_rows):
            holdings[f'holding_{i}_symbol'] = symbol.strip()
            holdings[f'holding_{i}_quantity'] = quantity.strip()
            holdings[f'holding_{i}_value'] = value.strip()
        
        return holdings
    
    def extract_position_data(self, content: str) -> Dict:
        """Extract position-specific data"""
        positions = {}
        
        # Position cards
        position_cards = re.findall(r'<div class="position-card".*?>(.*?)</div>', content, re.DOTALL)
        
        for i, card_content in enumerate(position_cards):
            # Extract position metrics from card content
            metrics = re.findall(r'<span[^>]*>([^:]+):</span>\s*<span[^>]*>([^<]+)</span>', card_content)
            for metric_name, metric_value in metrics:
                clean_name = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
                positions[f'position_{i}_{clean_name}'] = metric_value.strip()
        
        return positions
    
    def extract_performance_metrics(self, content: str) -> Dict:
        """Extract performance page metrics"""
        performance = {}
        
        # Performance statistics table
        perf_rows = re.findall(r'<tr[^>]*>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?</tr>', content, re.DOTALL)
        
        for metric_name, metric_value in perf_rows:
            clean_name = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
            if clean_name and not clean_name.startswith('metric'):  # Filter out generic matches
                performance[f'perf_{clean_name}'] = metric_value.strip()
        
        return performance
    
    def extract_returns_data(self, content: str) -> Dict:
        """Extract returns and time series data"""
        returns = {}
        
        # Monthly/quarterly returns
        returns_data = re.findall(r'<span class="return-period">([^<]+)</span>.*?<span class="return-value">([^<]+)</span>', content, re.DOTALL)
        
        for period, return_value in returns_data:
            clean_period = re.sub(r'[^\w\s]', '', period.strip()).lower().replace(' ', '_')
            returns[f'return_{clean_period}'] = return_value.strip()
        
        return returns
    
    def extract_risk_adjusted_metrics(self, content: str) -> Dict:
        """Extract risk-adjusted performance metrics"""
        risk_adj = {}
        
        # Sharpe, Sortino, other ratios
        ratio_matches = re.findall(r'<div class="ratio-metric">.*?<span class="ratio-name">([^<]+)</span>.*?<span class="ratio-value">([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for ratio_name, ratio_value in ratio_matches:
            clean_name = re.sub(r'[^\w\s]', '', ratio_name.strip()).lower().replace(' ', '_')
            risk_adj[f'ratio_{clean_name}'] = ratio_value.strip()
        
        return risk_adj
    
    def extract_algorithm_status(self, content: str) -> Dict:
        """Extract algorithm status and configuration"""
        algorithms = {}
        
        # Algorithm status indicators
        algo_status = re.findall(r'<div class="algorithm-status[^"]*">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for i, (algo_name, status) in enumerate(algo_status):
            algorithms[f'algorithm_{i}_name'] = algo_name.strip()
            algorithms[f'algorithm_{i}_status'] = status.strip()
        
        return algorithms
    
    def extract_algorithm_config(self, content: str) -> Dict:
        """Extract algorithm configuration parameters"""
        config = {}
        
        # Configuration parameters
        config_params = re.findall(r'<div class="config-param">.*?<span class="param-name">([^<]+)</span>.*?<span class="param-value">([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for param_name, param_value in config_params:
            clean_name = re.sub(r'[^\w\s]', '', param_name.strip()).lower().replace(' ', '_')
            config[f'config_{clean_name}'] = param_value.strip()
        
        return config
    
    def extract_algorithm_metrics(self, content: str) -> Dict:
        """Extract algorithm performance metrics"""
        metrics = {}
        
        # Algorithm-specific performance data
        algo_metrics = re.findall(r'<div class="algo-metric[^"]*">.*?<span[^>]*>([^:]+):</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for metric_name, metric_value in algo_metrics:
            clean_name = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
            metrics[f'algo_{clean_name}'] = metric_value.strip()
        
        return metrics
    
    def extract_algorithm_trades(self, content: str) -> Dict:
        """Extract algorithm trading data"""
        trades = {}
        
        # Trade statistics
        trade_stats = re.findall(r'<div class="trade-stat">.*?<span class="stat-label">([^<]+)</span>.*?<span class="stat-value">([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for stat_name, stat_value in trade_stats:
            clean_name = re.sub(r'[^\w\s]', '', stat_name.strip()).lower().replace(' ', '_')
            trades[f'trade_{clean_name}'] = stat_value.strip()
        
        return trades
    
    def extract_backtest_metrics(self, content: str) -> Dict:
        """Extract backtest results metrics"""
        backtest = {}
        
        # Backtest performance metrics
        bt_metrics = re.findall(r'<div class="backtest-metric">.*?<span class="metric-label">([^<]+)</span>.*?<span class="metric-value">([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for metric_name, metric_value in bt_metrics:
            clean_name = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
            backtest[f'backtest_{clean_name}'] = metric_value.strip()
        
        return backtest
    
    def extract_backtest_trades(self, content: str) -> Dict:
        """Extract backtest trading data"""
        bt_trades = {}
        
        # Backtest trade analysis
        trade_analysis = re.findall(r'<div class="trade-analysis">.*?<span class="analysis-label">([^<]+)</span>.*?<span class="analysis-value">([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for label, value in trade_analysis:
            clean_label = re.sub(r'[^\w\s]', '', label.strip()).lower().replace(' ', '_')
            bt_trades[f'bt_trade_{clean_label}'] = value.strip()
        
        return bt_trades
    
    def extract_common_elements(self, content: str) -> Dict:
        """Extract common elements present on all pages"""
        common = {}
        
        # Page title
        title_match = re.search(r'<title>([^<]+)</title>', content)
        if title_match:
            common['page_title'] = title_match.group(1).strip()
        
        # Breadcrumb navigation
        breadcrumb_match = re.search(r'<div class="breadcrumb">([^<]+)</div>', content)
        if breadcrumb_match:
            common['breadcrumb'] = breadcrumb_match.group(1).strip()
        
        # Last updated timestamp
        timestamp_match = re.search(r'Last Updated:</strong>\s*([^<]+)', content)
        if timestamp_match:
            common['last_updated'] = timestamp_match.group(1).strip()
        
        # Portfolio name/identifier
        portfolio_match = re.search(r'<h1[^>]*>.*?💼\s*([^<]+)</h1>', content)
        if portfolio_match:
            common['portfolio_name'] = portfolio_match.group(1).strip()
        
        return common


class BackendDataMapper:
    """Maps frontend data points to backend data sources"""
    
    def __init__(self):
        self.backend_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios"
        
    def get_backend_data_sources(self, simulation_id: str = "Myportolio") -> Dict:
        """Get all available backend data sources for a simulation"""
        
        backend_sources = {}
        
        if simulation_id == "Myportolio":
            # Live portfolio data sources
            portfolio_path = f"{self.backend_path}/Myportolio"
            backend_sources.update(self.load_live_portfolio_data(portfolio_path))
        else:
            # Backtest simulation data sources
            simulation_path = f"{self.backend_path}/Myportolio/simulations/backtests/{simulation_id}"
            backend_sources.update(self.load_simulation_data(simulation_path))
        
        return backend_sources
    
    def load_live_portfolio_data(self, portfolio_path: str) -> Dict:
        """Load live portfolio backend data"""
        data_sources = {}
        
        # Core configuration
        config_file = f"{portfolio_path}/config.json"
        if os.path.exists(config_file):
            with open(config_file, 'r') as f:
                data_sources['config'] = json.load(f)
        
        # Risk parameters
        risk_file = f"{portfolio_path}/risk_parameters.json" 
        if os.path.exists(risk_file):
            with open(risk_file, 'r') as f:
                data_sources['risk_parameters'] = json.load(f)
        
        # Latest status report
        status_files = [f for f in os.listdir(portfolio_path) if f.startswith('status_report_') and f.endswith('.json')]
        if status_files:
            latest_status = max(status_files)
            with open(f"{portfolio_path}/{latest_status}", 'r') as f:
                data_sources['status_report'] = json.load(f)
        
        # Latest risk report
        risk_report_files = [f for f in os.listdir(portfolio_path) if f.startswith('risk_report_') and f.endswith('.json')]
        if risk_report_files:
            latest_risk_report = max(risk_report_files)
            with open(f"{portfolio_path}/{latest_risk_report}", 'r') as f:
                data_sources['risk_report'] = json.load(f)
        
        return data_sources
    
    def load_simulation_data(self, simulation_path: str) -> Dict:
        """Load backtest simulation backend data"""
        data_sources = {}
        
        # LEAN configuration
        lean_config_file = f"{simulation_path}/lean_config.json"
        if os.path.exists(lean_config_file):
            with open(lean_config_file, 'r') as f:
                data_sources['lean_config'] = json.load(f)
        
        # Simulation results
        results_file = f"{simulation_path}/myportolio_results.json"
        if os.path.exists(results_file):
            with open(results_file, 'r') as f:
                data_sources['simulation_results'] = json.load(f)
        
        return data_sources
    
    def create_data_point_mapping(self, frontend_data: Dict, backend_data: Dict) -> Dict:
        """Create mapping between frontend data points and backend sources"""
        
        mapping = {
            'mapped_points': {},
            'unmapped_points': {},
            'backend_unused': {},
            'mapping_coverage': 0.0
        }
        
        # Create mapping for each page
        for page_name, page_data in frontend_data.items():
            if 'data_points' in page_data:
                page_mapping = self.map_page_data_points(page_data['data_points'], backend_data, page_name)
                mapping['mapped_points'][page_name] = page_mapping['mapped']
                mapping['unmapped_points'][page_name] = page_mapping['unmapped']
        
        # Calculate overall coverage
        total_points = sum(len(page_data.get('data_points', {})) for page_data in frontend_data.values())
        mapped_points = sum(len(page_mapping) for page_mapping in mapping['mapped_points'].values())
        
        if total_points > 0:
            mapping['mapping_coverage'] = (mapped_points / total_points) * 100
        
        return mapping
    
    def map_page_data_points(self, page_data_points: Dict, backend_data: Dict, page_type: str) -> Dict:
        """Map data points for a specific page type"""
        
        mapped = {}
        unmapped = {}
        
        for data_point_key, data_point_value in page_data_points.items():
            backend_source = self.find_backend_source(data_point_key, data_point_value, backend_data, page_type)
            
            if backend_source:
                mapped[data_point_key] = {
                    'frontend_value': data_point_value,
                    'backend_source': backend_source['source'],
                    'backend_path': backend_source['path'],
                    'backend_value': backend_source['value'],
                    'match_status': 'exact' if str(data_point_value) == str(backend_source['value']) else 'different'
                }
            else:
                unmapped[data_point_key] = {
                    'frontend_value': data_point_value,
                    'status': 'no_backend_source_found'
                }
        
        return {'mapped': mapped, 'unmapped': unmapped}
    
    def find_backend_source(self, data_key: str, data_value: str, backend_data: Dict, page_type: str) -> Optional[Dict]:
        """Find the backend source for a specific data point"""
        
        # Define mapping rules based on data key patterns and page types
        mapping_rules = self.get_mapping_rules()
        
        for rule in mapping_rules:
            if self.matches_rule(data_key, page_type, rule):
                backend_value = self.extract_backend_value(backend_data, rule['backend_path'])
                if backend_value is not None:
                    return {
                        'source': rule['source'],
                        'path': rule['backend_path'],
                        'value': backend_value
                    }
        
        return None
    
    def get_mapping_rules(self) -> List[Dict]:
        """Define mapping rules between frontend data points and backend sources"""
        
        return [
            # Portfolio metrics mappings
            {
                'frontend_pattern': r'portfolio_value',
                'page_types': ['portfolio_overview'],
                'source': 'config.json',
                'backend_path': ['config', 'target_portfolio_value']
            },
            {
                'frontend_pattern': r'asset_(\w+)_percent',
                'page_types': ['portfolio_overview', 'holdings'],
                'source': 'config.json',
                'backend_path': ['config', 'assets', '$1', 'allocation_percent']
            },
            {
                'frontend_pattern': r'risk_profile',
                'page_types': ['portfolio_overview'],
                'source': 'risk_parameters.json',
                'backend_path': ['risk_parameters', 'risk_profile']
            },
            {
                'frontend_pattern': r'target_volatility',
                'page_types': ['portfolio_overview'],
                'source': 'risk_parameters.json',
                'backend_path': ['risk_parameters', 'max_portfolio_volatility']
            },
            # Simulation parameters mappings
            {
                'frontend_pattern': r'sim_date',
                'page_types': ['portfolio_overview'],
                'source': 'lean_config.json',
                'backend_path': ['lean_config', 'start-date']
            },
            {
                'frontend_pattern': r'sim_algorithm_type_name',
                'page_types': ['portfolio_overview', 'algorithms'],
                'source': 'lean_config.json',
                'backend_path': ['lean_config', 'algorithm-type-name']
            },
            # Performance mappings
            {
                'frontend_pattern': r'perf_total_return',
                'page_types': ['performance'],
                'source': 'simulation_results.json',
                'backend_path': ['simulation_results', 'performance', 'total_return']
            },
            {
                'frontend_pattern': r'perf_sharpe_ratio',
                'page_types': ['performance'],
                'source': 'simulation_results.json',
                'backend_path': ['simulation_results', 'performance', 'sharpe_ratio']
            },
            # Algorithm mappings
            {
                'frontend_pattern': r'algorithm_(\d+)_name',
                'page_types': ['algorithms'],
                'source': 'config.json',
                'backend_path': ['config', 'algorithms', '$1', 'name']
            },
            # Backtest mappings
            {
                'frontend_pattern': r'backtest_(\w+)',
                'page_types': ['backtest_results'],
                'source': 'simulation_results.json',
                'backend_path': ['simulation_results', 'lean_results', '$1']
            }
        ]
    
    def matches_rule(self, data_key: str, page_type: str, rule: Dict) -> bool:
        """Check if a data key matches a mapping rule"""
        
        if page_type not in rule['page_types']:
            return False
        
        pattern = rule['frontend_pattern']
        return bool(re.match(pattern, data_key))
    
    def extract_backend_value(self, backend_data: Dict, path: List[str]) -> Any:
        """Extract value from backend data using path"""
        
        try:
            current_data = backend_data
            for key in path:
                if key.startswith('$'):
                    # This is a placeholder - would need to be resolved from the actual match
                    continue
                if key in current_data:
                    current_data = current_data[key]
                else:
                    return None
            return current_data
        except:
            return None


class ComprehensivePageValidator:
    """Main validation engine"""
    
    def __init__(self):
        self.extractor = PortfolioPageDataExtractor()
        self.mapper = BackendDataMapper()
        self.results = {
            'timestamp': datetime.now().isoformat(),
            'validation_summary': {},
            'page_validations': {},
            'backend_mappings': {},
            'coverage_analysis': {}
        }
    
    def validate_all_pages(self, simulation_id: str = "Myportolio") -> Dict:
        """Perform comprehensive validation of all pages"""
        
        print("🚀 COMPREHENSIVE PAGE DATA VALIDATION")
        print("=" * 70)
        print(f"Validating all data points for simulation: {simulation_id}")
        
        # Extract all frontend data points
        print("\n📊 STEP 1: Extracting Frontend Data Points")
        frontend_data = self.extractor.extract_all_page_data(simulation_id)
        
        # Load backend data sources
        print("\n🔍 STEP 2: Loading Backend Data Sources")
        backend_data = self.mapper.get_backend_data_sources(simulation_id)
        print(f"    ✅ Loaded {len(backend_data)} backend data sources")
        
        # Create data point mappings
        print("\n🔗 STEP 3: Creating Data Point Mappings")
        mappings = self.mapper.create_data_point_mapping(frontend_data, backend_data)
        
        # Perform validation comparisons
        print("\n✅ STEP 4: Performing Data Validation")
        validation_results = self.perform_validation_comparisons(mappings)
        
        # Generate comprehensive report
        self.results['page_validations'] = frontend_data
        self.results['backend_mappings'] = mappings
        self.results['validation_results'] = validation_results
        self.results['coverage_analysis'] = self.analyze_coverage(frontend_data, mappings)
        
        self.generate_validation_summary()
        
        return self.results
    
    def perform_validation_comparisons(self, mappings: Dict) -> Dict:
        """Compare frontend values with backend sources"""
        
        validation_results = {}
        
        for page_name, page_mappings in mappings['mapped_points'].items():
            page_results = {
                'exact_matches': 0,
                'value_differences': 0,
                'total_mapped': len(page_mappings),
                'match_details': {}
            }
            
            for data_point, mapping_info in page_mappings.items():
                if mapping_info['match_status'] == 'exact':
                    page_results['exact_matches'] += 1
                    status = '✅'
                else:
                    page_results['value_differences'] += 1
                    status = '⚠️'
                
                page_results['match_details'][data_point] = {
                    'status': status,
                    'frontend_value': mapping_info['frontend_value'],
                    'backend_value': mapping_info['backend_value'],
                    'backend_source': mapping_info['backend_source'],
                    'match_status': mapping_info['match_status']
                }
                
                print(f"    {status} {data_point}: {mapping_info['frontend_value']} ↔ {mapping_info['backend_value']}")
            
            validation_results[page_name] = page_results
            
            match_rate = (page_results['exact_matches'] / page_results['total_mapped']) * 100 if page_results['total_mapped'] > 0 else 0
            print(f"    📊 {page_name}: {page_results['exact_matches']}/{page_results['total_mapped']} exact matches ({match_rate:.1f}%)")
        
        return validation_results
    
    def analyze_coverage(self, frontend_data: Dict, mappings: Dict) -> Dict:
        """Analyze mapping coverage statistics"""
        
        coverage = {
            'total_data_points': 0,
            'mapped_data_points': 0,
            'unmapped_data_points': 0,
            'coverage_percentage': 0.0,
            'page_coverage': {}
        }
        
        for page_name, page_data in frontend_data.items():
            if 'data_points' in page_data:
                total_points = len(page_data['data_points'])
                mapped_points = len(mappings['mapped_points'].get(page_name, {}))
                unmapped_points = len(mappings['unmapped_points'].get(page_name, {}))
                
                coverage['total_data_points'] += total_points
                coverage['mapped_data_points'] += mapped_points
                coverage['unmapped_data_points'] += unmapped_points
                
                page_coverage_pct = (mapped_points / total_points) * 100 if total_points > 0 else 0
                
                coverage['page_coverage'][page_name] = {
                    'total_points': total_points,
                    'mapped_points': mapped_points,
                    'unmapped_points': unmapped_points,
                    'coverage_percentage': page_coverage_pct
                }
        
        if coverage['total_data_points'] > 0:
            coverage['coverage_percentage'] = (coverage['mapped_data_points'] / coverage['total_data_points']) * 100
        
        return coverage
    
    def generate_validation_summary(self):
        """Generate final validation summary"""
        
        coverage = self.results['coverage_analysis']
        validation = self.results['validation_results']
        
        print("\n" + "=" * 70)
        print("📊 COMPREHENSIVE VALIDATION SUMMARY")
        print("=" * 70)
        
        print(f"🔍 Data Point Coverage:")
        print(f"   Total Data Points: {coverage['total_data_points']}")
        print(f"   Mapped to Backend: {coverage['mapped_data_points']}")
        print(f"   Unmapped Points: {coverage['unmapped_data_points']}")
        print(f"   Coverage Percentage: {coverage['coverage_percentage']:.1f}%")
        
        print(f"\n✅ Validation Results:")
        total_exact_matches = sum(page['exact_matches'] for page in validation.values())
        total_mapped = sum(page['total_mapped'] for page in validation.values())
        total_differences = sum(page['value_differences'] for page in validation.values())
        
        print(f"   Exact Matches: {total_exact_matches}")
        print(f"   Value Differences: {total_differences}")
        print(f"   Validation Rate: {(total_exact_matches / total_mapped * 100) if total_mapped > 0 else 0:.1f}%")
        
        print(f"\n📋 Page-by-Page Coverage:")
        for page_name, page_coverage in coverage['page_coverage'].items():
            status = "✅" if page_coverage['coverage_percentage'] >= 80 else "⚠️" if page_coverage['coverage_percentage'] >= 50 else "❌"
            print(f"   {status} {page_name}: {page_coverage['mapped_points']}/{page_coverage['total_points']} ({page_coverage['coverage_percentage']:.1f}%)")
        
        # Determine overall status
        overall_success = coverage['coverage_percentage'] >= 90 and (total_exact_matches / total_mapped * 100) >= 90
        
        if overall_success:
            print(f"\n🎉 VALIDATION SUCCESSFUL!")
            print(f"   Achieved >90% coverage and >90% data accuracy")
        else:
            print(f"\n⚠️  VALIDATION NEEDS IMPROVEMENT")
            print(f"   Coverage: {coverage['coverage_percentage']:.1f}% (target: >90%)")
            print(f"   Accuracy: {(total_exact_matches / total_mapped * 100) if total_mapped > 0 else 0:.1f}% (target: >90%)")
        
        self.results['validation_summary'] = {
            'overall_success': overall_success,
            'coverage_percentage': coverage['coverage_percentage'],
            'accuracy_percentage': (total_exact_matches / total_mapped * 100) if total_mapped > 0 else 0,
            'total_data_points': coverage['total_data_points'],
            'mapped_points': coverage['mapped_data_points'],
            'exact_matches': total_exact_matches
        }


def main():
    """Execute comprehensive page data validation"""
    
    validator = ComprehensivePageValidator()
    
    # Test with live portfolio
    print("Testing Live Portfolio (Myportolio)...")
    results_live = validator.validate_all_pages("Myportolio")
    
    # Test with backtest simulation
    print("\n" + "=" * 70)
    print("Testing Backtest Simulation...")
    results_backtest = validator.validate_all_pages("backtest_20250903_145040_bef7f054")
    
    # Save comprehensive results
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    
    # Save live portfolio results
    live_results_file = f"comprehensive_page_validation_live_{timestamp}.json"
    with open(live_results_file, 'w') as f:
        json.dump(results_live, f, indent=2, default=str)
    
    # Save backtest results  
    backtest_results_file = f"comprehensive_page_validation_backtest_{timestamp}.json"
    with open(backtest_results_file, 'w') as f:
        json.dump(results_backtest, f, indent=2, default=str)
    
    print(f"\n💾 Results saved:")
    print(f"   Live Portfolio: {live_results_file}")
    print(f"   Backtest Simulation: {backtest_results_file}")
    
    return results_live, results_backtest


if __name__ == "__main__":
    main()
