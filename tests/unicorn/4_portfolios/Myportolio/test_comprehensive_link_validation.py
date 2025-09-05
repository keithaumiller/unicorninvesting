#!/usr/bin/env python3
"""
Comprehensive Link Validation Test

This test validates all links on the unicornmetrics dashboard pages and ensures:
1. All links lead to pages with correct portfolio selection
2. All data on destination pages matches expected portfolio data
3. Cross-validation between live portfolio and backtest simulation pages
"""

import json
import os
import requests
import re
from datetime import datetime
from pathlib import Path
from urllib.parse import urljoin, urlparse, parse_qs
import time

class ComprehensiveLinkValidator:
    """Comprehensive validator for all dashboard links"""
    
    def __init__(self):
        self.base_url = "http://localhost"
        self.main_page_url = f"{self.base_url}/unicorn"
        self.backtest_url = f"{self.base_url}/unicorn?simulation=backtest_20250903_145040_bef7f054"
        self.session = requests.Session()
        self.results = {
            'timestamp': datetime.now().isoformat(),
            'main_page_validation': {},
            'backtest_page_validation': {},
            'cross_validation': {},
            'summary': {}
        }
        
    def extract_links(self, html_content, base_url):
        """Extract all links from HTML content"""
        # Pattern to match href attributes
        link_pattern = r'href=[\'"]([^\'"]+)[\'"]'
        links = re.findall(link_pattern, html_content, re.IGNORECASE)
        
        # Convert relative links to absolute
        absolute_links = []
        for link in links:
            if link.startswith('http'):
                absolute_links.append(link)
            elif link.startswith('/'):
                absolute_links.append(f"{self.base_url}{link}")
            elif not link.startswith('#') and not link.startswith('javascript:'):
                absolute_links.append(urljoin(base_url, link))
        
        return list(set(absolute_links))  # Remove duplicates
    
    def extract_portfolio_data(self, html_content):
        """Extract key portfolio data from page content"""
        data = {
            'portfolio_name': None,
            'description': None,
            'strategy_type': None,
            'selected_simulation': None,
            'simulation_options': [],
            'asset_allocations': {},
            'performance_metrics': {},
            'risk_metrics': {}
        }
        
        # Extract portfolio name from h2 tag
        portfolio_name_match = re.search(r'<h2[^>]*>💼\s*([^<]+)</h2>', html_content)
        if portfolio_name_match:
            data['portfolio_name'] = portfolio_name_match.group(1).strip()
        
        # Extract description
        desc_match = re.search(r'<p><strong>([^<]+)</strong>[^-]*-[^:]*:\s*([^<]+)</p>', html_content)
        if desc_match:
            data['description'] = desc_match.group(1).strip()
            data['strategy_type'] = desc_match.group(2).strip()
        
        # Extract selected simulation
        selected_match = re.search(r'<option[^>]*selected[^>]*>([^<]+)</option>', html_content)
        if selected_match:
            data['selected_simulation'] = selected_match.group(1).strip()
        
        # Extract all simulation options
        option_matches = re.findall(r'<option[^>]*value="([^"]+)"[^>]*>([^<]+)</option>', html_content)
        for value, text in option_matches:
            data['simulation_options'].append({'value': value, 'text': text.strip()})
        
        # Extract asset allocations
        allocation_matches = re.findall(r'<span class="asset-symbol">([^<]+)</span>\s*<span class="allocation-percent">([^<]+)</span>', html_content)
        for asset, percent in allocation_matches:
            data['asset_allocations'][asset.strip()] = percent.strip()
        
        # Extract performance metrics (for backtests)
        perf_matches = re.findall(r'<td[^>]*>([^<]*(?:Return|Sharpe|Drawdown)[^<]*)</td>\s*<td[^>]*>([^<]+)</td>', html_content, re.IGNORECASE)
        for metric_name, metric_value in perf_matches:
            clean_name = re.sub(r'[^\w\s]', '', metric_name.strip()).lower().replace(' ', '_')
            if clean_name:
                data['performance_metrics'][clean_name] = metric_value.strip()
        
        # Extract risk metrics
        risk_matches = re.findall(r'<span class="metric-name">([^:]+):</span>\s*<span class="metric-value[^"]*">([^<]+)</span>', html_content)
        for metric_name, metric_value in risk_matches:
            clean_name = metric_name.strip().lower().replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct')
            data['risk_metrics'][clean_name] = metric_value.strip()
        
        return data
    
    def validate_page_data_consistency(self, page_data, expected_simulation):
        """Validate that page data is consistent with expected simulation"""
        issues = []
        
        # Check if selected simulation matches expected
        if page_data['selected_simulation']:
            if expected_simulation == 'Myportolio':
                if '🟢 Myportolio' not in page_data['selected_simulation']:
                    issues.append(f"Expected Myportolio to be selected, but got: {page_data['selected_simulation']}")
            elif expected_simulation.startswith('backtest_'):
                if expected_simulation not in page_data['selected_simulation']:
                    issues.append(f"Expected {expected_simulation} to be selected, but got: {page_data['selected_simulation']}")
        
        # Check portfolio name consistency
        if expected_simulation == 'Myportolio':
            if page_data['portfolio_name'] != 'Myportolio':
                issues.append(f"Expected portfolio name 'Myportolio', but got: {page_data['portfolio_name']}")
        
        # Check asset allocations for live portfolio
        if expected_simulation == 'Myportolio':
            expected_allocations = {'ETH': '60.0%', 'BTC': '40.0%'}
            for asset, expected_percent in expected_allocations.items():
                if asset not in page_data['asset_allocations']:
                    issues.append(f"Missing asset allocation for {asset}")
                elif page_data['asset_allocations'][asset] != expected_percent:
                    issues.append(f"Expected {asset} allocation {expected_percent}, got {page_data['asset_allocations'][asset]}")
        
        return issues
    
    def validate_main_page(self):
        """Validate all links on the main page (live portfolio)"""
        print("🔍 VALIDATING MAIN PAGE LINKS")
        print("=" * 70)
        
        # Get main page content
        try:
            response = self.session.get(self.main_page_url)
            response.raise_for_status()
            main_content = response.text
        except Exception as e:
            self.results['main_page_validation']['error'] = f"Failed to load main page: {str(e)}"
            return
        
        # Extract main page data
        main_data = self.extract_portfolio_data(main_content)
        self.results['main_page_validation']['page_data'] = main_data
        
        # Validate main page data consistency
        main_issues = self.validate_page_data_consistency(main_data, 'Myportolio')
        self.results['main_page_validation']['data_consistency_issues'] = main_issues
        
        print(f"📊 Main Page Data Summary:")
        print(f"  Portfolio Name: {main_data['portfolio_name']}")
        print(f"  Selected Simulation: {main_data['selected_simulation']}")
        print(f"  Asset Allocations: {main_data['asset_allocations']}")
        print(f"  Data Consistency Issues: {len(main_issues)}")
        
        if main_issues:
            for issue in main_issues:
                print(f"    ❌ {issue}")
        else:
            print("    ✅ All data consistent")
        
        # Extract and validate links
        links = self.extract_links(main_content, self.main_page_url)
        unicorn_links = [link for link in links if '/unicorn' in link or 'localhost' in link]
        
        print(f"\n🔗 Found {len(unicorn_links)} Unicorn dashboard links")
        
        link_results = {}
        for i, link in enumerate(unicorn_links):
            print(f"  Testing link {i+1}/{len(unicorn_links)}: {link}")
            
            try:
                # Add delay to avoid overwhelming server
                time.sleep(0.5)
                
                link_response = self.session.get(link, timeout=10)
                link_response.raise_for_status()
                link_content = link_response.text
                
                # Extract data from linked page
                link_data = self.extract_portfolio_data(link_content)
                
                # Parse simulation parameter from URL
                parsed_url = urlparse(link)
                query_params = parse_qs(parsed_url.query)
                expected_sim = query_params.get('simulation', ['Myportolio'])[0]
                
                # Validate data consistency
                link_issues = self.validate_page_data_consistency(link_data, expected_sim)
                
                link_results[link] = {
                    'status': 'success',
                    'expected_simulation': expected_sim,
                    'page_data': link_data,
                    'data_issues': link_issues,
                    'issues_count': len(link_issues)
                }
                
                status = "✅" if len(link_issues) == 0 else f"⚠️ ({len(link_issues)} issues)"
                print(f"    {status} Expected: {expected_sim}, Selected: {link_data['selected_simulation']}")
                
            except Exception as e:
                link_results[link] = {
                    'status': 'error',
                    'error': str(e)
                }
                print(f"    ❌ Error: {str(e)}")
        
        self.results['main_page_validation']['links'] = link_results
        
        # Summary
        successful_links = sum(1 for result in link_results.values() if result['status'] == 'success' and result.get('issues_count', 0) == 0)
        total_links = len(link_results)
        print(f"\n📊 Main Page Links Summary: {successful_links}/{total_links} fully validated")
    
    def validate_backtest_page(self):
        """Validate all links on the backtest simulation page"""
        print("\n🔍 VALIDATING BACKTEST PAGE LINKS")
        print("=" * 70)
        
        # Get backtest page content
        try:
            response = self.session.get(self.backtest_url)
            response.raise_for_status()
            backtest_content = response.text
        except Exception as e:
            self.results['backtest_page_validation']['error'] = f"Failed to load backtest page: {str(e)}"
            return
        
        # Extract backtest page data
        backtest_data = self.extract_portfolio_data(backtest_content)
        self.results['backtest_page_validation']['page_data'] = backtest_data
        
        # Validate backtest page data consistency
        expected_backtest_id = 'backtest_20250903_145040_bef7f054'
        backtest_issues = self.validate_page_data_consistency(backtest_data, expected_backtest_id)
        self.results['backtest_page_validation']['data_consistency_issues'] = backtest_issues
        
        print(f"📊 Backtest Page Data Summary:")
        print(f"  Portfolio Name: {backtest_data['portfolio_name']}")
        print(f"  Selected Simulation: {backtest_data['selected_simulation']}")
        print(f"  Performance Metrics: {len(backtest_data['performance_metrics'])} found")
        print(f"  Risk Metrics: {len(backtest_data['risk_metrics'])} found")
        print(f"  Data Consistency Issues: {len(backtest_issues)}")
        
        if backtest_issues:
            for issue in backtest_issues:
                print(f"    ❌ {issue}")
        else:
            print("    ✅ All data consistent")
        
        # Extract and validate links
        links = self.extract_links(backtest_content, self.backtest_url)
        unicorn_links = [link for link in links if '/unicorn' in link or 'localhost' in link]
        
        print(f"\n🔗 Found {len(unicorn_links)} Unicorn dashboard links")
        
        link_results = {}
        for i, link in enumerate(unicorn_links):
            print(f"  Testing link {i+1}/{len(unicorn_links)}: {link}")
            
            try:
                # Add delay to avoid overwhelming server
                time.sleep(0.5)
                
                link_response = self.session.get(link, timeout=10)
                link_response.raise_for_status()
                link_content = link_response.text
                
                # Extract data from linked page
                link_data = self.extract_portfolio_data(link_content)
                
                # Parse simulation parameter from URL
                parsed_url = urlparse(link)
                query_params = parse_qs(parsed_url.query)
                expected_sim = query_params.get('simulation', ['Myportolio'])[0]
                
                # Validate data consistency
                link_issues = self.validate_page_data_consistency(link_data, expected_sim)
                
                link_results[link] = {
                    'status': 'success',
                    'expected_simulation': expected_sim,
                    'page_data': link_data,
                    'data_issues': link_issues,
                    'issues_count': len(link_issues)
                }
                
                status = "✅" if len(link_issues) == 0 else f"⚠️ ({len(link_issues)} issues)"
                print(f"    {status} Expected: {expected_sim}, Selected: {link_data['selected_simulation']}")
                
            except Exception as e:
                link_results[link] = {
                    'status': 'error',
                    'error': str(e)
                }
                print(f"    ❌ Error: {str(e)}")
        
        self.results['backtest_page_validation']['links'] = link_results
        
        # Summary
        successful_links = sum(1 for result in link_results.values() if result['status'] == 'success' and result.get('issues_count', 0) == 0)
        total_links = len(link_results)
        print(f"\n📊 Backtest Page Links Summary: {successful_links}/{total_links} fully validated")
    
    def cross_validate_pages(self):
        """Cross-validate data consistency between main and backtest pages"""
        print("\n🔄 CROSS-VALIDATION BETWEEN PAGES")
        print("=" * 70)
        
        main_data = self.results['main_page_validation'].get('page_data', {})
        backtest_data = self.results['backtest_page_validation'].get('page_data', {})
        
        cross_validation_results = {}
        
        # Check simulation options consistency
        main_options = {opt['value']: opt['text'] for opt in main_data.get('simulation_options', [])}
        backtest_options = {opt['value']: opt['text'] for opt in backtest_data.get('simulation_options', [])}
        
        if main_options == backtest_options:
            cross_validation_results['simulation_options'] = {'status': 'consistent', 'count': len(main_options)}
            print(f"✅ Simulation options consistent: {len(main_options)} options")
        else:
            cross_validation_results['simulation_options'] = {'status': 'inconsistent', 'main_count': len(main_options), 'backtest_count': len(backtest_options)}
            print(f"❌ Simulation options inconsistent: Main={len(main_options)}, Backtest={len(backtest_options)}")
        
        # Check that backtest shows different portfolio name than live
        if main_data.get('portfolio_name') != backtest_data.get('portfolio_name'):
            cross_validation_results['portfolio_names'] = {'status': 'different', 'main': main_data.get('portfolio_name'), 'backtest': backtest_data.get('portfolio_name')}
            print(f"✅ Portfolio names different as expected: '{main_data.get('portfolio_name')}' vs '{backtest_data.get('portfolio_name')}'")
        else:
            cross_validation_results['portfolio_names'] = {'status': 'same', 'value': main_data.get('portfolio_name')}
            print(f"⚠️ Portfolio names unexpectedly same: {main_data.get('portfolio_name')}")
        
        self.results['cross_validation'] = cross_validation_results
    
    def generate_summary(self):
        """Generate final validation summary"""
        print("\n" + "=" * 70)
        print("📊 COMPREHENSIVE LINK VALIDATION SUMMARY")
        print("=" * 70)
        
        # Main page summary
        main_links = self.results['main_page_validation'].get('links', {})
        main_successful = sum(1 for result in main_links.values() if result.get('status') == 'success' and result.get('issues_count', 0) == 0)
        main_total = len(main_links)
        main_issues = len(self.results['main_page_validation'].get('data_consistency_issues', []))
        
        # Backtest page summary
        backtest_links = self.results['backtest_page_validation'].get('links', {})
        backtest_successful = sum(1 for result in backtest_links.values() if result.get('status') == 'success' and result.get('issues_count', 0) == 0)
        backtest_total = len(backtest_links)
        backtest_issues = len(self.results['backtest_page_validation'].get('data_consistency_issues', []))
        
        total_successful = main_successful + backtest_successful
        total_links = main_total + backtest_total
        
        self.results['summary'] = {
            'total_links_tested': total_links,
            'total_successful': total_successful,
            'success_rate': round((total_successful / total_links * 100) if total_links > 0 else 0, 1),
            'main_page': {'successful': main_successful, 'total': main_total, 'data_issues': main_issues},
            'backtest_page': {'successful': backtest_successful, 'total': backtest_total, 'data_issues': backtest_issues}
        }
        
        print(f"🦄 Main Page (Live Portfolio):")
        print(f"   Links: {main_successful}/{main_total} fully validated")
        print(f"   Data Issues: {main_issues}")
        
        print(f"🔴 Backtest Page (backtest_20250903_145040_bef7f054):")
        print(f"   Links: {backtest_successful}/{backtest_total} fully validated")
        print(f"   Data Issues: {backtest_issues}")
        
        print(f"📈 Overall Results:")
        print(f"   Total Links: {total_links}")
        print(f"   Successful: {total_successful}")
        print(f"   Success Rate: {self.results['summary']['success_rate']}%")
        
        if self.results['summary']['success_rate'] >= 90:
            print("🎉 EXCELLENT: Link validation highly successful!")
        elif self.results['summary']['success_rate'] >= 75:
            print("✅ GOOD: Most links validated successfully")
        else:
            print("⚠️ NEEDS IMPROVEMENT: Several link validation issues found")
    
    def save_results(self):
        """Save detailed results to JSON file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"comprehensive_link_validation_{timestamp}.json"
        
        with open(filename, 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        
        print(f"\n💾 Detailed results saved to: {filename}")
    
    def run_validation(self):
        """Run complete link validation"""
        print("🚀 COMPREHENSIVE LINK VALIDATION")
        print("=" * 70)
        print("Testing all links on unicornmetrics dashboard pages")
        print("Validating portfolio selection and data consistency")
        print()
        
        # Validate main page
        self.validate_main_page()
        
        # Validate backtest page
        self.validate_backtest_page()
        
        # Cross-validate
        self.cross_validate_pages()
        
        # Generate summary
        self.generate_summary()
        
        # Save results
        self.save_results()

if __name__ == "__main__":
    validator = ComprehensiveLinkValidator()
    validator.run_validation()
