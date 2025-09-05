#!/usr/bin/env python3
"""
Focused Unicorn Metrics Link Validation Test

This test specifically validates unicorn metrics dashboard links and navigation,
focusing on portfolio selection consistency and data validation.
"""

import json
import os
import requests
import re
from datetime import datetime
from pathlib import Path
from urllib.parse import urljoin, urlparse, parse_qs
import time

class FocusedLinkValidator:
    """Focused validator for unicorn metrics dashboard links"""
    
    def __init__(self):
        self.base_url = "http://localhost"
        self.main_page_url = f"{self.base_url}/unicorn"
        self.backtest_url = f"{self.base_url}/unicorn?simulation=backtest_20250903_145040_bef7f054"
        self.session = requests.Session()
        self.results = {
            'timestamp': datetime.now().isoformat(),
            'main_page_test': {},
            'backtest_page_test': {},
            'navigation_links': {},
            'summary': {}
        }
        
    def extract_unicorn_links(self, html_content):
        """Extract only unicorn metrics related links"""
        # Look for links to unicorn pages and admin/metrics paths
        link_patterns = [
            r'href=[\'"]([^\'"]*unicorn[^\'"]*)[\'"]',
            r'href=[\'"]([^\'"]*admin/metrics[^\'"]*)[\'"]',
            r'href=[\'"](/[^\'"]*)[\'"]'  # All internal links
        ]
        
        links = set()
        for pattern in link_patterns:
            matches = re.findall(pattern, html_content, re.IGNORECASE)
            for match in matches:
                if any(keyword in match.lower() for keyword in ['unicorn', 'metrics', 'portfolio', 'lean']):
                    if match.startswith('http'):
                        links.add(match)
                    elif match.startswith('/'):
                        links.add(f"{self.base_url}{match}")
        
        return list(links)
    
    def extract_portfolio_data(self, html_content):
        """Extract key portfolio data for validation"""
        data = {
            'portfolio_name': None,
            'selected_simulation': None,
            'simulation_options': [],
            'asset_allocations': {},
            'performance_data': [],
            'has_backtest_data': False
        }
        
        # Extract portfolio name
        portfolio_match = re.search(r'<h2[^>]*>💼\s*([^<]+)</h2>', html_content)
        if portfolio_match:
            data['portfolio_name'] = portfolio_match.group(1).strip()
        
        # Extract selected simulation (look for selected option)
        selected_match = re.search(r'<option[^>]*value="([^"]*)"[^>]*selected[^>]*>([^<]+)</option>', html_content)
        if selected_match:
            data['selected_simulation'] = {
                'value': selected_match.group(1).strip(),
                'text': selected_match.group(2).strip()
            }
        
        # Extract all simulation options
        option_matches = re.findall(r'<option[^>]*value="([^"]+)"[^>]*>([^<]+)</option>', html_content)
        for value, text in option_matches:
            data['simulation_options'].append({'value': value.strip(), 'text': text.strip()})
        
        # Extract asset allocations
        allocation_matches = re.findall(r'<span class="asset-symbol">([^<]+)</span>\s*<span class="allocation-percent">([^<]+)</span>', html_content)
        for asset, percent in allocation_matches:
            data['asset_allocations'][asset.strip()] = percent.strip()
        
        # Check for backtest-specific data
        if 'backtest simulation' in html_content.lower():
            data['has_backtest_data'] = True
        
        return data
    
    def validate_simulation_consistency(self, page_data, expected_simulation_id):
        """Validate that page shows correct simulation data"""
        issues = []
        
        if not page_data['selected_simulation']:
            issues.append("No simulation selection found")
            return issues
        
        selected_value = page_data['selected_simulation']['value']
        selected_text = page_data['selected_simulation']['text']
        
        if expected_simulation_id == 'Myportolio':
            if selected_value != 'Myportolio':
                issues.append(f"Expected Myportolio selected, got {selected_value}")
            if '🟢 Myportolio' not in selected_text:
                issues.append(f"Expected live portfolio indicator, got {selected_text}")
        else:
            if selected_value != expected_simulation_id:
                issues.append(f"Expected {expected_simulation_id} selected, got {selected_value}")
            if '🔴' not in selected_text:
                issues.append(f"Expected backtest indicator, got {selected_text}")
        
        return issues
    
    def test_main_page(self):
        """Test main page (live portfolio) data and links"""
        print("🔍 TESTING MAIN PAGE (Live Portfolio)")
        print("=" * 60)
        
        try:
            response = self.session.get(self.main_page_url)
            response.raise_for_status()
            content = response.text
        except Exception as e:
            self.results['main_page_test']['error'] = str(e)
            print(f"❌ Failed to load main page: {e}")
            return
        
        # Extract and validate page data
        page_data = self.extract_portfolio_data(content)
        validation_issues = self.validate_simulation_consistency(page_data, 'Myportolio')
        
        self.results['main_page_test'] = {
            'page_data': page_data,
            'validation_issues': validation_issues,
            'status': 'success' if len(validation_issues) == 0 else 'issues'
        }
        
        print(f"📊 Main Page Data:")
        print(f"  Portfolio Name: {page_data['portfolio_name']}")
        print(f"  Selected: {page_data['selected_simulation']}")
        print(f"  Asset Allocations: {page_data['asset_allocations']}")
        print(f"  Simulation Options: {len(page_data['simulation_options'])}")
        
        if validation_issues:
            print(f"❌ Issues found: {len(validation_issues)}")
            for issue in validation_issues:
                print(f"    - {issue}")
        else:
            print("✅ All validations passed")
        
        # Extract and test unicorn links
        links = self.extract_unicorn_links(content)
        print(f"\n🔗 Found {len(links)} unicorn-related links")
        
        link_results = {}
        for link in links[:5]:  # Test first 5 links to avoid overwhelming
            print(f"  Testing: {link}")
            try:
                time.sleep(0.3)  # Rate limiting
                link_response = self.session.get(link, timeout=10)
                link_response.raise_for_status()
                
                # Basic validation - check if page loads
                link_results[link] = {'status': 'success', 'status_code': link_response.status_code}
                print(f"    ✅ Status: {link_response.status_code}")
                
            except Exception as e:
                link_results[link] = {'status': 'error', 'error': str(e)}
                print(f"    ❌ Error: {str(e)}")
        
        self.results['main_page_test']['links'] = link_results
        
    def test_backtest_page(self):
        """Test backtest page data and links"""
        print("\n🔍 TESTING BACKTEST PAGE")
        print("=" * 60)
        
        try:
            response = self.session.get(self.backtest_url)
            response.raise_for_status()
            content = response.text
        except Exception as e:
            self.results['backtest_page_test']['error'] = str(e)
            print(f"❌ Failed to load backtest page: {e}")
            return
        
        # Extract and validate page data
        expected_backtest_id = 'backtest_20250903_145040_bef7f054'
        page_data = self.extract_portfolio_data(content)
        validation_issues = self.validate_simulation_consistency(page_data, expected_backtest_id)
        
        self.results['backtest_page_test'] = {
            'page_data': page_data,
            'validation_issues': validation_issues,
            'expected_simulation': expected_backtest_id,
            'status': 'success' if len(validation_issues) == 0 else 'issues'
        }
        
        print(f"📊 Backtest Page Data:")
        print(f"  Portfolio Name: {page_data['portfolio_name']}")
        print(f"  Selected: {page_data['selected_simulation']}")
        print(f"  Has Backtest Data: {page_data['has_backtest_data']}")
        print(f"  Simulation Options: {len(page_data['simulation_options'])}")
        
        if validation_issues:
            print(f"❌ Issues found: {len(validation_issues)}")
            for issue in validation_issues:
                print(f"    - {issue}")
        else:
            print("✅ All validations passed")
        
        # Extract and test unicorn links
        links = self.extract_unicorn_links(content)
        print(f"\n🔗 Found {len(links)} unicorn-related links")
        
        link_results = {}
        for link in links[:5]:  # Test first 5 links
            print(f"  Testing: {link}")
            try:
                time.sleep(0.3)  # Rate limiting
                link_response = self.session.get(link, timeout=10)
                link_response.raise_for_status()
                
                link_results[link] = {'status': 'success', 'status_code': link_response.status_code}
                print(f"    ✅ Status: {link_response.status_code}")
                
            except Exception as e:
                link_results[link] = {'status': 'error', 'error': str(e)}
                print(f"    ❌ Error: {str(e)}")
        
        self.results['backtest_page_test']['links'] = link_results
    
    def test_simulation_switching(self):
        """Test switching between different simulations"""
        print("\n🔄 TESTING SIMULATION SWITCHING")
        print("=" * 60)
        
        # Test switching to different simulations
        test_simulations = [
            'Myportolio',
            'backtest_20250903_142955_5618caf5',
            'backtest_20250903_143119_fa83c2ff',
            'backtest_20250903_145040_bef7f054'
        ]
        
        switching_results = {}
        
        for sim_id in test_simulations:
            print(f"🔄 Testing simulation: {sim_id}")
            url = f"{self.base_url}/unicorn?simulation={sim_id}"
            
            try:
                response = self.session.get(url)
                response.raise_for_status()
                content = response.text
                
                page_data = self.extract_portfolio_data(content)
                validation_issues = self.validate_simulation_consistency(page_data, sim_id)
                
                switching_results[sim_id] = {
                    'status': 'success',
                    'selected_simulation': page_data['selected_simulation'],
                    'validation_issues': validation_issues,
                    'issues_count': len(validation_issues)
                }
                
                status = "✅" if len(validation_issues) == 0 else f"⚠️ ({len(validation_issues)} issues)"
                selected_text = page_data['selected_simulation']['text'] if page_data['selected_simulation'] else 'None'
                print(f"  {status} Selected: {selected_text}")
                
            except Exception as e:
                switching_results[sim_id] = {
                    'status': 'error',
                    'error': str(e)
                }
                print(f"  ❌ Error: {str(e)}")
        
        self.results['simulation_switching'] = switching_results
        
        # Summary
        successful_switches = sum(1 for result in switching_results.values() 
                                if result['status'] == 'success' and result.get('issues_count', 1) == 0)
        total_switches = len(switching_results)
        print(f"\n📊 Simulation Switching: {successful_switches}/{total_switches} successful")
    
    def generate_summary(self):
        """Generate final summary"""
        print("\n" + "=" * 60)
        print("📊 FOCUSED LINK VALIDATION SUMMARY")
        print("=" * 60)
        
        main_status = self.results['main_page_test'].get('status', 'error')
        backtest_status = self.results['backtest_page_test'].get('status', 'error')
        
        main_link_success = sum(1 for result in self.results['main_page_test'].get('links', {}).values() 
                              if result['status'] == 'success')
        main_link_total = len(self.results['main_page_test'].get('links', {}))
        
        backtest_link_success = sum(1 for result in self.results['backtest_page_test'].get('links', {}).values() 
                                  if result['status'] == 'success')
        backtest_link_total = len(self.results['backtest_page_test'].get('links', {}))
        
        switching_success = sum(1 for result in self.results.get('simulation_switching', {}).values() 
                              if result['status'] == 'success' and result.get('issues_count', 1) == 0)
        switching_total = len(self.results.get('simulation_switching', {}))
        
        print(f"🦄 Main Page (Live Portfolio):")
        print(f"   Data Validation: {'✅ PASS' if main_status == 'success' else '❌ FAIL'}")
        print(f"   Links Tested: {main_link_success}/{main_link_total}")
        
        print(f"🔴 Backtest Page:")
        print(f"   Data Validation: {'✅ PASS' if backtest_status == 'success' else '❌ FAIL'}")
        print(f"   Links Tested: {backtest_link_success}/{backtest_link_total}")
        
        print(f"🔄 Simulation Switching:")
        print(f"   Successful: {switching_success}/{switching_total}")
        
        overall_success = (main_status == 'success' and backtest_status == 'success' 
                          and switching_success == switching_total)
        
        if overall_success:
            print("\n🎉 ALL TESTS PASSED! Portfolio navigation is working correctly.")
        else:
            print("\n⚠️ Some issues found. Review the detailed results above.")
        
        self.results['summary'] = {
            'main_page_status': main_status,
            'backtest_page_status': backtest_status,
            'simulation_switching_success': switching_success,
            'simulation_switching_total': switching_total,
            'overall_success': overall_success
        }
    
    def save_results(self):
        """Save results to JSON file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"focused_link_validation_{timestamp}.json"
        
        with open(filename, 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        
        print(f"\n💾 Detailed results saved to: {filename}")
    
    def run_validation(self):
        """Run complete focused validation"""
        print("🚀 FOCUSED UNICORN METRICS LINK VALIDATION")
        print("=" * 60)
        print("Testing portfolio selection consistency and data validation")
        print()
        
        # Test main page
        self.test_main_page()
        
        # Test backtest page
        self.test_backtest_page()
        
        # Test simulation switching
        self.test_simulation_switching()
        
        # Generate summary
        self.generate_summary()
        
        # Save results
        self.save_results()

if __name__ == "__main__":
    validator = FocusedLinkValidator()
    validator.run_validation()
