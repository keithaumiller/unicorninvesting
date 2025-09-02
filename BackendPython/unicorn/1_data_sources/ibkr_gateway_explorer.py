#!/usr/bin/env python3
"""
IBKR Gateway Explorer
Systematically explore what endpoints and data are available in the paper trading environment
"""

import requests
import json
import time
from datetime import datetime
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class IBKRExplorer:
    """Explore IBKR Gateway capabilities and available data"""
    
    def __init__(self, base_url="http://localhost:5000"):
        self.base_url = base_url
        self.session = requests.Session()
        self.results = {}
        
    def test_endpoint(self, method, endpoint, data=None, params=None, description=""):
        """Test a single endpoint and record results"""
        
        try:
            url = f"{self.base_url}{endpoint}"
            
            if method.upper() == 'GET':
                response = self.session.get(url, params=params, timeout=10)
            elif method.upper() == 'POST':
                response = self.session.post(url, json=data, params=params, timeout=10)
            else:
                return None
                
            result = {
                'url': url,
                'method': method,
                'status_code': response.status_code,
                'description': description,
                'timestamp': datetime.now().isoformat()
            }
            
            if response.status_code == 200:
                try:
                    result['response'] = response.json()
                    result['success'] = True
                except:
                    result['response'] = response.text[:500]  # First 500 chars
                    result['success'] = True
            else:
                result['response'] = response.text[:200]
                result['success'] = False
                
            logger.info(f"{method} {endpoint}: {response.status_code} - {description}")
            return result
            
        except Exception as e:
            result = {
                'url': f"{self.base_url}{endpoint}",
                'method': method,
                'status_code': 'ERROR',
                'description': description,
                'error': str(e),
                'success': False,
                'timestamp': datetime.now().isoformat()
            }
            logger.error(f"{method} {endpoint}: ERROR - {e}")
            return result
    
    def explore_authentication(self):
        """Explore authentication-related endpoints"""
        
        print("\n" + "="*60)
        print("🔐 AUTHENTICATION ENDPOINTS")
        print("="*60)
        
        endpoints = [
            ('GET', '/sso/Dispatcher', None, None, 'SSO Dispatcher - Main auth check'),
            ('GET', '/v1/api/sso/validate', None, None, 'SSO validation'),
            ('GET', '/v1/api/iserver/auth/status', None, None, 'iServer auth status'),
            ('POST', '/v1/api/iserver/reauthenticate', None, None, 'Trigger reauthentication'),
            ('GET', '/v1/api/tickle', None, None, 'Session tickle')
        ]
        
        self.results['authentication'] = []
        
        for method, endpoint, data, params, description in endpoints:
            result = self.test_endpoint(method, endpoint, data, params, description)
            self.results['authentication'].append(result)
            
            if result['success']:
                print(f"✅ {description}")
                if 'response' in result and isinstance(result['response'], dict):
                    # Show key fields
                    if 'session' in result['response']:
                        print(f"   Session: {result['response']['session']}")
                    if 'authenticated' in result['response']:
                        print(f"   Authenticated: {result['response']['authenticated']}")
                elif 'response' in result and isinstance(result['response'], str):
                    if "Client login succeeds" in result['response']:
                        print(f"   ✅ Authentication successful")
            else:
                print(f"❌ {description} - Status: {result['status_code']}")
    
    def explore_account_info(self):
        """Explore account information endpoints"""
        
        print("\n" + "="*60)
        print("👤 ACCOUNT INFORMATION")
        print("="*60)
        
        endpoints = [
            ('GET', '/v1/api/portfolio/accounts', None, None, 'Portfolio accounts'),
            ('GET', '/v1/api/portfolio/subaccounts', None, None, 'Sub-accounts'),
            ('GET', '/v1/api/portfolio/DUM785491/summary', None, None, 'Account summary'),
            ('GET', '/v1/api/portfolio/DUM785491/positions/0', None, None, 'Account positions'),
            ('GET', '/v1/api/portfolio/DUM785491/ledger', None, None, 'Account ledger'),
            ('GET', '/v1/api/one/user', None, None, 'User information')
        ]
        
        self.results['account'] = []
        
        for method, endpoint, data, params, description in endpoints:
            result = self.test_endpoint(method, endpoint, data, params, description)
            self.results['account'].append(result)
            
            if result['success']:
                print(f"✅ {description}")
                if isinstance(result['response'], list) and len(result['response']) > 0:
                    print(f"   Found {len(result['response'])} items")
                elif isinstance(result['response'], dict):
                    print(f"   Response keys: {list(result['response'].keys())}")
            else:
                print(f"❌ {description} - Status: {result['status_code']}")
    
    def explore_contract_search(self):
        """Explore contract search capabilities"""
        
        print("\n" + "="*60)
        print("🔍 CONTRACT SEARCH")
        print("="*60)
        
        # Test different search methods
        search_tests = [
            ('GET', '/trsrv/stocks', None, {'symbols': 'AAPL'}, 'Stock search - AAPL'),
            ('GET', '/trsrv/stocks', None, {'symbols': 'TSLA'}, 'Stock search - TSLA'), 
            ('GET', '/trsrv/futures', None, {'symbols': 'ES'}, 'Futures search - ES'),
            ('GET', '/trsrv/futures', None, {'symbols': 'NQ'}, 'Futures search - NQ'),
            ('POST', '/v1/api/iserver/secdef/search', {'symbol': 'AAPL'}, None, 'General search - AAPL'),
            ('POST', '/v1/api/iserver/secdef/search', {'symbol': 'ETH'}, None, 'General search - ETH'),
            ('POST', '/v1/api/iserver/secdef/search', {'symbol': 'BTC'}, None, 'General search - BTC')
        ]
        
        self.results['contracts'] = []
        
        for method, endpoint, data, params, description in search_tests:
            result = self.test_endpoint(method, endpoint, data, params, description)
            self.results['contracts'].append(result)
            
            if result['success']:
                print(f"✅ {description}")
                if isinstance(result['response'], dict):
                    for symbol, contracts in result['response'].items():
                        if isinstance(contracts, list):
                            print(f"   {symbol}: {len(contracts)} contracts found")
                        else:
                            print(f"   {symbol}: {type(contracts)}")
                elif isinstance(result['response'], list):
                    print(f"   Found {len(result['response'])} results")
            else:
                print(f"❌ {description} - Status: {result['status_code']}")
                if 'response' in result:
                    print(f"   Error: {result['response'][:100]}")
    
    def explore_market_data(self):
        """Explore market data endpoints"""
        
        print("\n" + "="*60)
        print("📊 MARKET DATA")
        print("="*60)
        
        # Test with known AAPL contract ID (265598)
        market_tests = [
            ('GET', '/v1/api/iserver/marketdata/subscriptions', None, None, 'Market data subscriptions'),
            ('GET', '/v1/api/iserver/marketdata/snapshot', None, {'conids': '265598', 'fields': '31,84,86'}, 'Snapshot - AAPL'),
            ('GET', '/v1/api/iserver/marketdata/history', None, {'conid': '265598', 'period': '1d', 'bar': '1h'}, 'Historical - AAPL'),
            ('GET', '/v1/api/hmds/history', None, {'conid': '265598', 'period': '1d', 'bar': '1h'}, 'HMDS Historical - AAPL'),
            ('GET', '/v1/api/iserver/scanner/params', None, None, 'Scanner parameters'),
        ]
        
        self.results['market_data'] = []
        
        for method, endpoint, data, params, description in market_tests:
            result = self.test_endpoint(method, endpoint, data, params, description)
            self.results['market_data'].append(result)
            
            if result['success']:
                print(f"✅ {description}")
                if isinstance(result['response'], list):
                    print(f"   Found {len(result['response'])} items")
                elif isinstance(result['response'], dict):
                    if 'data' in result['response']:
                        print(f"   Data points: {len(result['response']['data'])}")
                    else:
                        print(f"   Response keys: {list(result['response'].keys())}")
            else:
                print(f"❌ {description} - Status: {result['status_code']}")
                if 'response' in result:
                    print(f"   Error: {result['response'][:100]}")
    
    def explore_trading(self):
        """Explore trading-related endpoints"""
        
        print("\n" + "="*60)
        print("💰 TRADING ENDPOINTS")
        print("="*60)
        
        trading_tests = [
            ('GET', '/v1/api/iserver/orders', None, None, 'Live orders'),
            ('GET', '/v1/api/iserver/trades', None, None, 'Recent trades'),
            ('GET', '/v1/api/portfolio/DUM785491/positions/0', None, None, 'Current positions'),
            ('GET', '/v1/api/iserver/account', None, None, 'Trading account info'),
            ('GET', '/v1/api/iserver/accounts', None, None, 'Available trading accounts')
        ]
        
        self.results['trading'] = []
        
        for method, endpoint, data, params, description in trading_tests:
            result = self.test_endpoint(method, endpoint, data, params, description)
            self.results['trading'].append(result)
            
            if result['success']:
                print(f"✅ {description}")
                if isinstance(result['response'], list):
                    print(f"   Found {len(result['response'])} items")
                elif isinstance(result['response'], dict):
                    print(f"   Response keys: {list(result['response'].keys())}")
            else:
                print(f"❌ {description} - Status: {result['status_code']}")
    
    def generate_summary(self):
        """Generate a summary of findings"""
        
        print("\n" + "="*60)
        print("📋 EXPLORATION SUMMARY")
        print("="*60)
        
        total_tests = 0
        successful_tests = 0
        
        for category, tests in self.results.items():
            category_success = sum(1 for test in tests if test.get('success', False))
            category_total = len(tests)
            total_tests += category_total
            successful_tests += category_success
            
            print(f"\n{category.upper()}:")
            print(f"  ✅ Successful: {category_success}/{category_total}")
            
            # Show successful endpoints
            for test in tests:
                if test.get('success', False):
                    print(f"     ✅ {test['method']} {test['url'].replace(self.base_url, '')}")
        
        print(f"\n🎯 OVERALL RESULTS:")
        print(f"   Total endpoints tested: {total_tests}")
        print(f"   Successful: {successful_tests}")
        print(f"   Success rate: {(successful_tests/total_tests*100):.1f}%")
        
        # Save detailed results to file
        with open('ibkr_exploration_results.json', 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        print(f"\n📄 Detailed results saved to: ibkr_exploration_results.json")
    
    def run_full_exploration(self):
        """Run complete exploration of IBKR Gateway"""
        
        print("🚀 Starting IBKR Gateway Exploration")
        print(f"⏰ Timestamp: {datetime.now()}")
        print(f"🌐 Gateway URL: {self.base_url}")
        
        self.explore_authentication()
        self.explore_account_info()
        self.explore_contract_search()
        self.explore_market_data()
        self.explore_trading()
        self.generate_summary()

def main():
    explorer = IBKRExplorer()
    explorer.run_full_exploration()

if __name__ == "__main__":
    main()
