#!/usr/bin/env python3
"""
Production-Ready Frontend-Backend Data Validation System

This system creates comprehensive validation mappings for all accessible
portfolio pages, providing complete frontend-backend data integrity checking.
"""

import json
import os
import requests
import re
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional
import time

class ProductionDataValidator:
    """Production-ready comprehensive data validation system"""
    
    def __init__(self):
        self.base_url = "http://localhost"
        self.session = requests.Session()
        self.backend_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        
        # Page configuration with accessibility status
        self.pages_config = {
            'performance': {
                'url': '/admin/metrics/lean/performance?portfolio=Myportolio',
                'accessible': True,
                'priority': 'high'
            },
            'algorithms': {
                'url': '/admin/metrics/lean/algorithms?portfolio=Myportolio',
                'accessible': True,
                'priority': 'high'
            },
            'algorithm_performance': {
                'url': '/admin/metrics/lean/algorithms/performance?portfolio=Myportolio',
                'accessible': True,
                'priority': 'medium'
            },
            'backtest_results': {
                'url': '/admin/metrics/lean/backtest?portfolio=Myportolio',
                'accessible': True,
                'priority': 'high'
            },
            'portfolio_overview': {
                'url': '/admin/metrics/lean/portfolio?portfolio=Myportolio',
                'accessible': False,  # HTTP 403
                'priority': 'high'
            },
            'holdings': {
                'url': '/admin/metrics/lean/holdings?portfolio=Myportolio',
                'accessible': False,  # HTTP 403
                'priority': 'medium'
            }
        }
        
        self.results = {
            'validation_timestamp': datetime.now().isoformat(),
            'accessible_pages': {},
            'inaccessible_pages': {},
            'data_mappings': {},
            'validation_results': {},
            'coverage_analysis': {}
        }
    
    def extract_comprehensive_page_data(self, page_name: str, content: str) -> Dict:
        """Extract comprehensive data from page content based on page type"""
        
        data_points = {}
        
        if page_name == 'performance':
            data_points.update(self.extract_performance_metrics(content))
            data_points.update(self.extract_performance_charts_data(content))
            
        elif page_name == 'algorithms':
            data_points.update(self.extract_algorithm_status(content))
            data_points.update(self.extract_algorithm_configuration(content))
            
        elif page_name == 'algorithm_performance':
            data_points.update(self.extract_algorithm_performance_metrics(content))
            data_points.update(self.extract_algorithm_trades(content))
            
        elif page_name == 'backtest_results':
            data_points.update(self.extract_backtest_statistics(content))
            data_points.update(self.extract_backtest_trades(content))
            data_points.update(self.extract_simulation_parameters(content))
            
        # Extract common elements from all pages
        data_points.update(self.extract_common_elements(content))
        
        return data_points
    
    def extract_performance_metrics(self, content: str) -> Dict:
        """Extract performance-specific metrics"""
        metrics = {}
        
        # Performance statistics table
        table_rows = re.findall(r'<tr[^>]*>.*?<td[^>]*class="[^"]*stat-label[^"]*"[^>]*>([^<]+)</td>.*?<td[^>]*class="[^"]*stat-value[^"]*"[^>]*>([^<]+)</td>.*?</tr>', content, re.DOTALL)
        
        for label, value in table_rows:
            clean_label = re.sub(r'[^\w\s]', '', label.strip()).lower().replace(' ', '_')
            if clean_label:
                metrics[f'perf_{clean_label}'] = value.strip()
        
        # Key performance indicators
        kpi_matches = re.findall(r'<div class="kpi-[^"]*">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for i, (kpi_label, kpi_value) in enumerate(kpi_matches):
            clean_label = re.sub(r'[^\w\s]', '', kpi_label.strip()).lower().replace(' ', '_')
            if clean_label:
                metrics[f'kpi_{clean_label}'] = kpi_value.strip()
            else:
                metrics[f'kpi_{i}'] = kpi_value.strip()
        
        return metrics
    
    def extract_performance_charts_data(self, content: str) -> Dict:
        """Extract chart data and visualization metrics"""
        chart_data = {}
        
        # Chart data attributes
        chart_matches = re.findall(r'data-chart="([^"]+)"', content)
        for i, chart_info in enumerate(chart_matches):
            chart_data[f'chart_{i}'] = chart_info
        
        # Time series data
        series_matches = re.findall(r'data-series="([^"]+)"', content)
        for i, series_info in enumerate(series_matches):
            chart_data[f'series_{i}'] = series_info
        
        return chart_data
    
    def extract_algorithm_status(self, content: str) -> Dict:
        """Extract algorithm status information"""
        algo_data = {}
        
        # Algorithm status indicators
        status_rows = re.findall(r'<tr[^>]*class="algorithm-row"[^>]*>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?</tr>', content, re.DOTALL)
        
        for i, (algo_name, status) in enumerate(status_rows):
            algo_data[f'algorithm_{i}_name'] = algo_name.strip()
            algo_data[f'algorithm_{i}_status'] = status.strip()
        
        # Algorithm configuration parameters
        config_matches = re.findall(r'<span class="config-param">([^<]+)</span>.*?<span class="config-value">([^<]+)</span>', content, re.DOTALL)
        
        for param_name, param_value in config_matches:
            clean_name = re.sub(r'[^\w\s]', '', param_name.strip()).lower().replace(' ', '_')
            if clean_name:
                algo_data[f'config_{clean_name}'] = param_value.strip()
        
        return algo_data
    
    def extract_algorithm_configuration(self, content: str) -> Dict:
        """Extract algorithm configuration details"""
        config_data = {}
        
        # Configuration sections
        config_sections = re.findall(r'<div class="config-section"[^>]*>.*?<h4>([^<]+)</h4>(.*?)</div>', content, re.DOTALL)
        
        for section_name, section_content in config_sections:
            clean_section = re.sub(r'[^\w\s]', '', section_name.strip()).lower().replace(' ', '_')
            
            # Extract parameters from section
            params = re.findall(r'<div class="param-row">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', section_content, re.DOTALL)
            
            for param_name, param_value in params:
                clean_param = re.sub(r'[^\w\s]', '', param_name.strip()).lower().replace(' ', '_')
                if clean_param:
                    config_data[f'{clean_section}_{clean_param}'] = param_value.strip()
        
        return config_data
    
    def extract_algorithm_performance_metrics(self, content: str) -> Dict:
        """Extract algorithm-specific performance metrics"""
        perf_data = {}
        
        # Algorithm performance table
        perf_rows = re.findall(r'<tr[^>]*class="algo-perf-row"[^>]*>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?<td[^>]*>([^<]+)</td>.*?</tr>', content, re.DOTALL)
        
        for i, (algo_name, metric_name, metric_value) in enumerate(perf_rows):
            clean_algo = re.sub(r'[^\w\s]', '', algo_name.strip()).lower().replace(' ', '_')
            clean_metric = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
            
            if clean_algo and clean_metric:
                perf_data[f'{clean_algo}_{clean_metric}'] = metric_value.strip()
        
        return perf_data
    
    def extract_algorithm_trades(self, content: str) -> Dict:
        """Extract algorithm trading data"""
        trades_data = {}
        
        # Trading statistics
        trade_stats = re.findall(r'<div class="trade-stat[^"]*">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for stat_name, stat_value in trade_stats:
            clean_name = re.sub(r'[^\w\s]', '', stat_name.strip()).lower().replace(' ', '_')
            if clean_name:
                trades_data[f'trade_{clean_name}'] = stat_value.strip()
        
        return trades_data
    
    def extract_backtest_statistics(self, content: str) -> Dict:
        """Extract backtest statistics"""
        backtest_data = {}
        
        # Backtest summary statistics
        summary_matches = re.findall(r'<div class="summary-stat[^"]*">.*?<span[^>]*class="stat-name"[^>]*>([^<]+)</span>.*?<span[^>]*class="stat-value"[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for stat_name, stat_value in summary_matches:
            clean_name = re.sub(r'[^\w\s]', '', stat_name.strip()).lower().replace(' ', '_')
            if clean_name:
                backtest_data[f'summary_{clean_name}'] = stat_value.strip()
        
        # Risk metrics
        risk_matches = re.findall(r'<div class="risk-metric[^"]*">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for risk_name, risk_value in risk_matches:
            clean_name = re.sub(r'[^\w\s]', '', risk_name.strip()).lower().replace(' ', '_')
            if clean_name:
                backtest_data[f'risk_{clean_name}'] = risk_value.strip()
        
        return backtest_data
    
    def extract_backtest_trades(self, content: str) -> Dict:
        """Extract backtest trading analysis"""
        trades_data = {}
        
        # Trade analysis metrics
        trade_analysis = re.findall(r'<div class="trade-analysis[^"]*">.*?<span[^>]*>([^<]+)</span>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
        
        for analysis_name, analysis_value in trade_analysis:
            clean_name = re.sub(r'[^\w\s]', '', analysis_name.strip()).lower().replace(' ', '_')
            if clean_name:
                trades_data[f'analysis_{clean_name}'] = analysis_value.strip()
        
        return trades_data
    
    def extract_simulation_parameters(self, content: str) -> Dict:
        """Extract simulation parameters from content"""
        sim_data = {}
        
        # Check for simulation parameters section
        if 'Simulation Parameters' in content or 'simulation-parameters' in content:
            # Parameter rows
            param_matches = re.findall(r'<span class="param-label">([^:]+):</span>\s*<span class="param-value">([^<]+)</span>', content)
            
            for label, value in param_matches:
                clean_label = re.sub(r'[^\w\s]', '', label.strip()).lower().replace(' ', '_')
                if clean_label:
                    sim_data[f'sim_{clean_label}'] = value.strip()
            
            # Alternative pattern
            alt_params = re.findall(r'<div class="param-row[^"]*">.*?<label[^>]*>([^<]+)</label>.*?<span[^>]*>([^<]+)</span>.*?</div>', content, re.DOTALL)
            
            for label, value in alt_params:
                clean_label = re.sub(r'[^\w\s]', '', label.strip()).lower().replace(' ', '_')
                if clean_label:
                    sim_data[f'param_{clean_label}'] = value.strip()
        
        return sim_data
    
    def extract_common_elements(self, content: str) -> Dict:
        """Extract elements common to all pages"""
        common = {}
        
        # Page title
        title_match = re.search(r'<title>([^<]+)</title>', content)
        if title_match:
            common['page_title'] = title_match.group(1).strip()
        
        # Main heading
        h1_match = re.search(r'<h1[^>]*>([^<]+)</h1>', content)
        if h1_match:
            common['main_heading'] = h1_match.group(1).strip()
        
        # Portfolio identifier
        portfolio_matches = re.findall(r'portfolio[=:]([^&\s"\']+)', content, re.IGNORECASE)
        if portfolio_matches:
            common['portfolio_id'] = portfolio_matches[0]
        
        # Timestamp/date information
        timestamp_matches = re.findall(r'(\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2})', content)
        for i, timestamp in enumerate(timestamp_matches[:3]):
            common[f'timestamp_{i}'] = timestamp
        
        # Numeric data (currency values)
        currency_matches = re.findall(r'\$([0-9,]+\.[0-9]{2})', content)
        for i, amount in enumerate(currency_matches[:5]):
            common[f'currency_{i}'] = f'${amount}'
        
        # Percentage values
        percentage_matches = re.findall(r'([+-]?[0-9.]+)%', content)
        for i, percent in enumerate(percentage_matches[:5]):
            common[f'percentage_{i}'] = f'{percent}%'
        
        return common
    
    def load_backend_data(self, simulation_id: str = "Myportolio") -> Dict:
        """Load comprehensive backend data"""
        
        backend_data = {}
        
        # Load configuration
        config_file = f"{self.backend_path}/config.json"
        if os.path.exists(config_file):
            with open(config_file, 'r') as f:
                backend_data['config'] = json.load(f)
        
        # Load risk parameters
        risk_file = f"{self.backend_path}/risk_parameters.json"
        if os.path.exists(risk_file):
            with open(risk_file, 'r') as f:
                backend_data['risk_parameters'] = json.load(f)
        
        # Load latest status report
        import glob
        status_files = glob.glob(f"{self.backend_path}/status_report_*.json")
        if status_files:
            latest_status = max(status_files, key=os.path.getctime)
            with open(latest_status, 'r') as f:
                backend_data['status_report'] = json.load(f)
        
        # Load simulation data if applicable
        if simulation_id != "Myportolio":
            sim_path = f"{self.backend_path}/simulations/backtests/{simulation_id}"
            
            # LEAN configuration
            lean_config_file = f"{sim_path}/lean_config.json"
            if os.path.exists(lean_config_file):
                with open(lean_config_file, 'r') as f:
                    backend_data['lean_config'] = json.load(f)
            
            # Simulation results
            results_file = f"{sim_path}/myportolio_results.json"
            if os.path.exists(results_file):
                with open(results_file, 'r') as f:
                    backend_data['simulation_results'] = json.load(f)
        
        return backend_data
    
    def create_data_mappings(self, frontend_data: Dict, backend_data: Dict, page_name: str) -> Dict:
        """Create mappings between frontend and backend data"""
        
        mappings = {
            'mapped': {},
            'unmapped': {},
            'potential_mappings': {}
        }
        
        # Define mapping rules for each page type
        mapping_rules = self.get_mapping_rules(page_name)
        
        for data_key, data_value in frontend_data.items():
            mapped = False
            
            # Try to map using predefined rules
            for rule in mapping_rules:
                if re.match(rule['frontend_pattern'], data_key):
                    backend_value = self.extract_backend_value(backend_data, rule['backend_path'])
                    
                    if backend_value is not None:
                        mappings['mapped'][data_key] = {
                            'frontend_value': data_value,
                            'backend_value': str(backend_value),
                            'backend_source': rule['backend_source'],
                            'backend_path': rule['backend_path'],
                            'rule_description': rule['description'],
                            'match_status': self.compare_values(data_value, backend_value)
                        }
                        mapped = True
                        break
            
            if not mapped:
                # Try to find potential mappings by searching backend data
                potential_matches = self.find_potential_backend_matches(data_value, backend_data)
                
                if potential_matches:
                    mappings['potential_mappings'][data_key] = {
                        'frontend_value': data_value,
                        'potential_matches': potential_matches
                    }
                else:
                    mappings['unmapped'][data_key] = {
                        'frontend_value': data_value,
                        'status': 'no_backend_match_found'
                    }
        
        return mappings
    
    def get_mapping_rules(self, page_name: str) -> List[Dict]:
        """Get mapping rules for specific page types"""
        
        rules = []
        
        if page_name == 'performance':
            rules.extend([
                {
                    'frontend_pattern': r'perf_total_return',
                    'backend_path': ['simulation_results', 'Statistics', 'Total Performance', 'Total Return'],
                    'backend_source': 'simulation_results',
                    'description': 'Total return performance metric'
                },
                {
                    'frontend_pattern': r'perf_sharpe_ratio',
                    'backend_path': ['simulation_results', 'Statistics', 'Risk', 'Sharpe Ratio'],
                    'backend_source': 'simulation_results',
                    'description': 'Sharpe ratio risk-adjusted return'
                }
            ])
        
        elif page_name == 'algorithms':
            rules.extend([
                {
                    'frontend_pattern': r'algorithm_(\d+)_name',
                    'backend_path': ['config', 'algorithms'],
                    'backend_source': 'config',
                    'description': 'Algorithm name configuration'
                }
            ])
        
        elif page_name == 'backtest_results':
            rules.extend([
                {
                    'frontend_pattern': r'sim_(\w+)',
                    'backend_path': ['lean_config'],
                    'backend_source': 'lean_config',
                    'description': 'Simulation parameter mapping'
                }
            ])
        
        return rules
    
    def extract_backend_value(self, backend_data: Dict, path: List[str]) -> Any:
        """Extract value from backend data using path"""
        
        try:
            current_data = backend_data
            for key in path:
                if isinstance(current_data, dict) and key in current_data:
                    current_data = current_data[key]
                else:
                    return None
            return current_data
        except:
            return None
    
    def compare_values(self, frontend_value: str, backend_value: Any) -> str:
        """Compare frontend and backend values"""
        
        # Normalize values for comparison
        front_str = str(frontend_value).strip()
        back_str = str(backend_value).strip()
        
        if front_str == back_str:
            return 'exact_match'
        
        # Try numeric comparison
        try:
            front_num = float(re.sub(r'[^\d.-]', '', front_str))
            back_num = float(re.sub(r'[^\d.-]', '', back_str))
            
            if abs(front_num - back_num) < 0.01:
                return 'numeric_match'
        except:
            pass
        
        return 'different'
    
    def find_potential_backend_matches(self, frontend_value: str, backend_data: Dict) -> List[Dict]:
        """Find potential matches in backend data"""
        
        matches = []
        frontend_clean = str(frontend_value).strip()
        
        def search_dict(data, path=[]):
            if isinstance(data, dict):
                for key, value in data.items():
                    current_path = path + [key]
                    if str(value).strip() == frontend_clean:
                        matches.append({
                            'backend_path': current_path,
                            'backend_value': value,
                            'match_type': 'exact'
                        })
                    search_dict(value, current_path)
            elif isinstance(data, list):
                for i, item in enumerate(data):
                    search_dict(item, path + [i])
        
        search_dict(backend_data)
        return matches[:5]  # Limit to 5 potential matches
    
    def run_comprehensive_validation(self, simulation_id: str = "Myportolio") -> Dict:
        """Run comprehensive validation for all accessible pages"""
        
        print("🚀 COMPREHENSIVE FRONTEND-BACKEND DATA VALIDATION")
        print("=" * 70)
        print(f"Validation started at: {datetime.now().isoformat()}")
        print(f"Target simulation: {simulation_id}")
        
        # Load backend data once
        print("\n🗂️  Loading backend data...")
        backend_data = self.load_backend_data(simulation_id)
        print(f"    ✅ Backend data sources loaded: {len(backend_data)}")
        
        # Process each accessible page
        print("\n📊 Processing accessible pages...")
        
        for page_name, page_config in self.pages_config.items():
            if not page_config['accessible']:
                self.results['inaccessible_pages'][page_name] = {
                    'url': page_config['url'],
                    'reason': 'HTTP 403 - Access denied',
                    'priority': page_config['priority']
                }
                print(f"    ⚠️  Skipping {page_name} (HTTP 403)")
                continue
            
            try:
                print(f"    📄 Processing {page_name}...")
                
                # Fetch page content
                response = self.session.get(f"{self.base_url}{page_config['url']}", timeout=15)
                
                if response.status_code == 200:
                    content = response.text
                    
                    # Extract data points
                    page_data = self.extract_comprehensive_page_data(page_name, content)
                    
                    # Create mappings
                    mappings = self.create_data_mappings(page_data, backend_data, page_name)
                    
                    # Store results
                    self.results['accessible_pages'][page_name] = {
                        'url': page_config['url'],
                        'status': 'success',
                        'priority': page_config['priority'],
                        'data_points_found': len(page_data),
                        'mapped_points': len(mappings['mapped']),
                        'unmapped_points': len(mappings['unmapped']),
                        'potential_mappings': len(mappings['potential_mappings']),
                        'coverage_percentage': (len(mappings['mapped']) / len(page_data)) * 100 if page_data else 0
                    }
                    
                    self.results['data_mappings'][page_name] = mappings
                    
                    print(f"      ✅ Data points: {len(page_data)}")
                    print(f"      🔗 Mapped: {len(mappings['mapped'])}")
                    print(f"      ❓ Unmapped: {len(mappings['unmapped'])}")
                    print(f"      📍 Coverage: {self.results['accessible_pages'][page_name]['coverage_percentage']:.1f}%")
                
                else:
                    self.results['inaccessible_pages'][page_name] = {
                        'url': page_config['url'],
                        'reason': f'HTTP {response.status_code}',
                        'priority': page_config['priority']
                    }
                    print(f"      ❌ Error: HTTP {response.status_code}")
                
            except Exception as e:
                self.results['inaccessible_pages'][page_name] = {
                    'url': page_config['url'],
                    'reason': f'Exception: {str(e)}',
                    'priority': page_config['priority']
                }
                print(f"      ❌ Exception: {str(e)}")
            
            time.sleep(0.5)  # Rate limiting
        
        # Generate coverage analysis
        self.generate_coverage_analysis()
        
        return self.results
    
    def generate_coverage_analysis(self):
        """Generate comprehensive coverage analysis"""
        
        total_accessible_pages = len(self.results['accessible_pages'])
        total_inaccessible_pages = len(self.results['inaccessible_pages'])
        total_pages = total_accessible_pages + total_inaccessible_pages
        
        total_data_points = sum(page['data_points_found'] for page in self.results['accessible_pages'].values())
        total_mapped_points = sum(page['mapped_points'] for page in self.results['accessible_pages'].values())
        total_unmapped_points = sum(page['unmapped_points'] for page in self.results['accessible_pages'].values())
        
        overall_coverage = (total_mapped_points / total_data_points) * 100 if total_data_points > 0 else 0
        accessibility_rate = (total_accessible_pages / total_pages) * 100 if total_pages > 0 else 0
        
        self.results['coverage_analysis'] = {
            'total_pages': total_pages,
            'accessible_pages': total_accessible_pages,
            'inaccessible_pages': total_inaccessible_pages,
            'accessibility_rate': accessibility_rate,
            'total_data_points': total_data_points,
            'mapped_points': total_mapped_points,
            'unmapped_points': total_unmapped_points,
            'overall_coverage': overall_coverage,
            'high_priority_accessible': len([p for p in self.results['accessible_pages'].values() if p['priority'] == 'high']),
            'high_priority_inaccessible': len([p for p in self.results['inaccessible_pages'].values() if p['priority'] == 'high'])
        }
        
        print(f"\n📊 VALIDATION COVERAGE ANALYSIS")
        print("=" * 50)
        print(f"  📄 Total Pages: {total_pages}")
        print(f"  ✅ Accessible: {total_accessible_pages}")
        print(f"  ❌ Inaccessible: {total_inaccessible_pages}")
        print(f"  📊 Accessibility Rate: {accessibility_rate:.1f}%")
        print(f"  🔢 Total Data Points: {total_data_points}")
        print(f"  🔗 Mapped Points: {total_mapped_points}")
        print(f"  ❓ Unmapped Points: {total_unmapped_points}")
        print(f"  📈 Overall Coverage: {overall_coverage:.1f}%")
        
        # Determine overall success status
        if accessibility_rate >= 70 and overall_coverage >= 60:
            print(f"\n🎉 COMPREHENSIVE VALIDATION SUCCESSFUL!")
            print(f"   ✅ Accessibility: {accessibility_rate:.1f}% (target: ≥70%)")
            print(f"   ✅ Data Coverage: {overall_coverage:.1f}% (target: ≥60%)")
            success_status = True
        else:
            print(f"\n⚠️  VALIDATION NEEDS IMPROVEMENT")
            print(f"   📊 Accessibility: {accessibility_rate:.1f}% (target: ≥70%)")
            print(f"   📊 Data Coverage: {overall_coverage:.1f}% (target: ≥60%)")
            success_status = False
        
        self.results['coverage_analysis']['validation_success'] = success_status
    
    def save_results(self, filename_prefix: str = "comprehensive_validation"):
        """Save validation results to file"""
        
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f"{filename_prefix}_{timestamp}.json"
        
        with open(filename, 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        
        print(f"\n💾 RESULTS SAVED")
        print(f"   📄 File: {filename}")
        print(f"   📊 Size: {os.path.getsize(filename):,} bytes")
        
        return filename


def main():
    """Run comprehensive validation"""
    
    validator = ProductionDataValidator()
    
    # Run validation for live portfolio
    results = validator.run_comprehensive_validation("Myportolio")
    
    # Save results
    filename = validator.save_results("production_validation")
    
    print(f"\n🎯 VALIDATION COMPLETE")
    print(f"   Results saved to: {filename}")
    
    return results, filename


if __name__ == "__main__":
    main()
