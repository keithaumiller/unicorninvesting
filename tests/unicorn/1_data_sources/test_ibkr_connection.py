#!/usr/bin/env python3
"""
Test IBKR Gateway Connection
Simple script to test connectivity to IBKR Gateway after authentication
"""

import requests
import json
import time
from datetime import datetime

class IBKRConnectionTest:
    def __init__(self, base_url="http://localhost:5000"):
        self.base_url = base_url
        
    def test_auth_status(self):
        """Check authentication status"""
        try:
            response = requests.get(f"{self.base_url}/v1/api/iserver/auth/status")
            print(f"Auth Status: {response.json()}")
            return response.json()
        except Exception as e:
            print(f"Auth status check failed: {e}")
            return None
    
    def test_market_data(self, symbol="ETH"):
        """Test market data retrieval for ETH"""
        try:
            # First need to search for contract
            search_url = f"{self.base_url}/v1/api/iserver/secdef/search"
            search_data = {"symbol": symbol}
            
            response = requests.post(search_url, json=search_data)
            print(f"Symbol search for {symbol}: {response.status_code}")
            
            if response.status_code == 200:
                contracts = response.json()
                print(f"Found contracts: {json.dumps(contracts, indent=2)}")
                return contracts
            else:
                print(f"Search failed: {response.text}")
                return None
                
        except Exception as e:
            print(f"Market data test failed: {e}")
            return None
    
    def test_portfolio_info(self):
        """Test portfolio information retrieval"""
        try:
            response = requests.get(f"{self.base_url}/v1/api/portfolio/accounts")
            print(f"Portfolio accounts: {response.status_code}")
            
            if response.status_code == 200:
                accounts = response.json()
                print(f"Accounts: {json.dumps(accounts, indent=2)}")
                return accounts
            else:
                print(f"Portfolio info failed: {response.text}")
                return None
                
        except Exception as e:
            print(f"Portfolio test failed: {e}")
            return None

def main():
    """Run IBKR connection tests"""
    print("=" * 50)
    print("IBKR Gateway Connection Test")
    print(f"Timestamp: {datetime.now()}")
    print("=" * 50)
    
    tester = IBKRConnectionTest()
    
    # Test 1: Authentication Status
    print("\n1. Testing Authentication Status...")
    auth_status = tester.test_auth_status()
    
    if auth_status and auth_status.get('authenticated'):
        print("✅ Gateway is authenticated!")
        
        # Test 2: Market Data
        print("\n2. Testing Market Data Access...")
        market_data = tester.test_market_data("ETH")
        
        # Test 3: Portfolio Info
        print("\n3. Testing Portfolio Access...")
        portfolio_info = tester.test_portfolio_info()
        
        print("\n✅ All tests completed!")
        
    else:
        print("❌ Gateway not authenticated.")
        print("Please authenticate via the web interface: http://localhost:5000")
        print("Then run this test again.")

if __name__ == "__main__":
    main()
