#!/usr/bin/env python3
"""
IBKR Environment Explorer
Comprehensive testing of IBKR Gateway integration to determine available features and trading environment
"""

import requests
import json
import time
from datetime import datetime
from typing import Dict, List, Optional
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class IBKREnvironmentExplorer:
    """Explore and test IBKR Gateway environment capabilities"""
    
    def __init__(self, base_url="http://localhost:5000"):
        self.base_url = base_url
        self.session = requests.Session()
        self.results = {}
        
    def test_basic_connectivity(self) -> Dict:
        """Test basic gateway connectivity"""
        print("🔌 Testing Basic Connectivity...")
        
        try:
            response = self.session.get(f"{self.base_url}/", timeout=10)
            if response.status_code == 200:
                print("   ✅ Gateway is responsive")
                return {"status": "success", "code": response.status_code}
            else:
                print(f"   ❌ Gateway returned {response.status_code}")
                return {"status": "error", "code": response.status_code}
        except Exception as e:
            print(f"   ❌ Connection failed: {e}")
            return {"status": "error", "message": str(e)}
    
    def test_authentication(self) -> Dict:
        """Test authentication status"""
        print("\n🔐 Testing Authentication...")
        
        # Check SSO Dispatcher (primary auth check)
        try:
            response = self.session.get(f"{self.base_url}/sso/Dispatcher")
            if response.status_code == 200 and "Client login succeeds" in response.text:
                print("   ✅ SSO Authentication: Client login succeeds")
                sso_auth = True
            else:
                print(f"   ❌ SSO Authentication failed: {response.text[:100]}")
                sso_auth = False
        except Exception as e:
            print(f"   ❌ SSO check failed: {e}")
            sso_auth = False
        
        # Check SSO validation
        try:
            response = self.session.get(f"{self.base_url}/v1/api/sso/validate")
            if response.status_code == 200:
                sso_data = response.json()
                print(f"   ✅ SSO Validation: User {sso_data.get('USER_NAME', 'Unknown')}")
                print(f"      Real-time access: {sso_data.get('features', {}).get('realtime', False)}")
                print(f"      Paper user: {sso_data.get('PAPER_USER_NAME', 'N/A')}")
            else:
                print("   ❌ SSO validation failed")
                sso_data = {}
        except Exception as e:
            print(f"   ❌ SSO validation error: {e}")
            sso_data = {}
        
        return {"sso_auth": sso_auth, "sso_data": sso_data}
    
    def test_session_status(self) -> Dict:
        """Test session and bridge status"""
        print("\n🌉 Testing Session & Bridge Status...")
        
        try:
            response = self.session.get(f"{self.base_url}/v1/api/tickle")
            if response.status_code == 200:
                tickle_data = response.json()
                session_id = tickle_data.get('session')
                bridge_error = tickle_data.get('hmds', {}).get('error')
                iserver_auth = tickle_data.get('iserver', {}).get('authStatus', {})
                
                print(f"   ✅ Session ID: {session_id}")
                print(f"   Bridge Status: {'❌ ' + bridge_error if bridge_error else '✅ Connected'}")
                print(f"   iServer Authenticated: {'✅' if iserver_auth.get('authenticated') else '❌'}")
                print(f"   iServer Connected: {'✅' if iserver_auth.get('connected') else '❌'}")
                
                return {
                    "session_id": session_id,
                    "bridge_error": bridge_error,
                    "iserver_auth": iserver_auth
                }
            else:
                print(f"   ❌ Tickle failed: {response.status_code}")
                return {"error": f"Tickle failed: {response.status_code}"}
        except Exception as e:
            print(f"   ❌ Session check failed: {e}")
            return {"error": str(e)}
    
    def test_account_access(self) -> Dict:
        """Test account information access"""
        print("\n💼 Testing Account Access...")
        
        try:
            # Portfolio accounts
            response = self.session.get(f"{self.base_url}/v1/api/portfolio/accounts")
            if response.status_code == 200:
                accounts = response.json()
                print(f"   ✅ Found {len(accounts)} account(s)")
                
                for account in accounts:
                    account_id = account.get('accountId', 'Unknown')
                    account_type = account.get('type', 'Unknown')
                    trading_type = account.get('tradingType', 'Unknown')
                    currency = account.get('currency', 'Unknown')
                    
                    print(f"      Account: {account_id} ({account_type})")
                    print(f"      Trading: {trading_type} in {currency}")
                    print(f"      Crypto Enabled: Z={account.get('PrepaidCrypto-Z', False)}, P={account.get('PrepaidCrypto-P', False)}")
                
                return {"accounts": accounts}
            else:
                print(f"   ❌ Account access failed: {response.status_code}")
                return {"error": f"Account access failed: {response.status_code}"}
        except Exception as e:
            print(f"   ❌ Account access error: {e}")
            return {"error": str(e)}
    
    def test_portfolio_data(self) -> Dict:
        """Test portfolio data access"""
        print("\n📊 Testing Portfolio Data...")
        
        try:
            # Get account summary
            response = self.session.get(f"{self.base_url}/v1/api/portfolio/accounts")
            if response.status_code == 200:
                accounts = response.json()
                if accounts:
                    account_id = accounts[0]['accountId']
                    
                    # Test account summary
                    summary_response = self.session.get(f"{self.base_url}/v1/api/portfolio/{account_id}/summary")
                    if summary_response.status_code == 200:
                        summary = summary_response.json()
                        print(f"   ✅ Account Summary for {account_id}:")
                        
                        if 'accountready' in summary:
                            account_ready = summary['accountready']
                            net_liq = account_ready.get('netliquidationvalue', {}).get('amount', 'N/A')
                            buying_power = account_ready.get('buyingpower', {}).get('amount', 'N/A')
                            available_funds = account_ready.get('availablefunds', {}).get('amount', 'N/A')
                            
                            print(f"      Net Liquidation: ${net_liq}")
                            print(f"      Buying Power: ${buying_power}")
                            print(f"      Available Funds: ${available_funds}")
                        
                        return {"summary": summary}
                    else:
                        print(f"   ❌ Summary access failed: {summary_response.status_code}")
                        return {"error": f"Summary failed: {summary_response.status_code}"}
            else:
                print(f"   ❌ Account list failed: {response.status_code}")
                return {"error": f"Account list failed: {response.status_code}"}
        except Exception as e:
            print(f"   ❌ Portfolio data error: {e}")
            return {"error": str(e)}
    
    def test_market_data_access(self) -> Dict:
        """Test market data capabilities"""
        print("\n📈 Testing Market Data Access...")
        
        # Test contract search (requires bridge)
        print("   Testing contract search...")
        try:
            search_data = {"symbol": "AAPL"}
            response = self.session.post(f"{self.base_url}/v1/api/iserver/secdef/search", json=search_data)
            if response.status_code == 200:
                contracts = response.json()
                print(f"   ✅ Contract search works: Found {len(contracts)} AAPL contracts")
            else:
                print(f"   ❌ Contract search failed: {response.status_code} - {response.text[:100]}")
        except Exception as e:
            print(f"   ❌ Contract search error: {e}")
        
        # Test stock lookup (non-bridge)
        print("   Testing stock lookup...")
        try:
            response = self.session.get(f"{self.base_url}/trsrv/stocks?symbols=AAPL")
            if response.status_code == 200:
                stocks = response.json()
                print(f"   ✅ Stock lookup works: {len(stocks)} results")
            else:
                print(f"   ❌ Stock lookup failed: {response.status_code}")
        except Exception as e:
            print(f"   ❌ Stock lookup error: {e}")
        
        # Test market data snapshot (requires bridge)
        print("   Testing market data snapshot...")
        try:
            # Use AAPL contract ID
            response = self.session.get(f"{self.base_url}/v1/api/iserver/marketdata/snapshot?conids=265598&fields=31,84,86")
            if response.status_code == 200:
                snapshot = response.json()
                print(f"   ✅ Market data snapshot works: {len(snapshot)} data points")
            else:
                print(f"   ❌ Market data snapshot failed: {response.status_code} - {response.text[:100]}")
        except Exception as e:
            print(f"   ❌ Market data snapshot error: {e}")
        
        return {"tested": True}
    
    def test_trading_capabilities(self) -> Dict:
        """Test trading operation capabilities (read-only tests)"""
        print("\n🔄 Testing Trading Capabilities...")
        
        try:
            # Test orders endpoint (should show existing orders, if any)
            response = self.session.get(f"{self.base_url}/v1/api/iserver/orders")
            if response.status_code == 200:
                orders = response.json()
                print(f"   ✅ Orders endpoint accessible: {len(orders)} orders")
            else:
                print(f"   ❌ Orders endpoint failed: {response.status_code} - {response.text[:100]}")
        except Exception as e:
            print(f"   ❌ Orders endpoint error: {e}")
        
        try:
            # Test positions endpoint
            response = self.session.get(f"{self.base_url}/v1/api/portfolio/positions/0")
            if response.status_code == 200:
                positions = response.json()
                print(f"   ✅ Positions endpoint accessible: {len(positions)} positions")
            else:
                print(f"   ❌ Positions endpoint failed: {response.status_code}")
        except Exception as e:
            print(f"   ❌ Positions endpoint error: {e}")
        
        return {"tested": True}
    
    def run_comprehensive_test(self):
        """Run all tests and provide summary"""
        print("=" * 60)
        print("🔬 IBKR Environment Comprehensive Testing")
        print(f"Timestamp: {datetime.now()}")
        print("=" * 60)
        
        # Run all tests
        connectivity = self.test_basic_connectivity()
        authentication = self.test_authentication()
        session = self.test_session_status()
        accounts = self.test_account_access()
        portfolio = self.test_portfolio_data()
        market_data = self.test_market_data_access()
        trading = self.test_trading_capabilities()
        
        # Summary
        print("\n" + "=" * 60)
        print("📋 ENVIRONMENT SUMMARY")
        print("=" * 60)
        
        print(f"🌐 Gateway Status: {'✅ Running' if connectivity.get('status') == 'success' else '❌ Issues'}")
        print(f"🔐 Authentication: {'✅ Authenticated' if authentication.get('sso_auth') else '❌ Not Authenticated'}")
        
        if session.get('bridge_error'):
            print(f"🌉 Bridge Status: ❌ {session['bridge_error']}")
            print("   📝 Note: Bridge connection required for live trading and market data")
        else:
            print("🌉 Bridge Status: ✅ Connected")
        
        if accounts.get('accounts'):
            account = accounts['accounts'][0]
            print(f"💼 Trading Account: {account.get('accountId')} ({account.get('type')})")
            print(f"📈 Trading Type: {account.get('tradingType')}")
            crypto_z = account.get('PrepaidCrypto-Z', False)
            crypto_p = account.get('PrepaidCrypto-P', False)
            print(f"🪙 Crypto Access: {'✅ Enabled' if (crypto_z or crypto_p) else '❌ Not Enabled'}")
        
        # Recommendations
        print("\n🎯 RECOMMENDATIONS:")
        if session.get('bridge_error'):
            print("   1. ⚠️  Bridge connection needed for live market data")
            print("   2. 💡 Current capabilities: Account info, portfolio data, basic connectivity")
            print("   3. 🔄 For full trading: Need to establish iServer bridge connection")
        else:
            print("   1. ✅ Full IBKR integration available")
            print("   2. 🚀 Ready for live trading and market data")
        
        if not (accounts.get('accounts', [{}])[0].get('PrepaidCrypto-Z', False) or 
                accounts.get('accounts', [{}])[0].get('PrepaidCrypto-P', False)):
            print("   4. 🪙 For crypto trading: Enable crypto permissions in IBKR account")
        
        print("\n🎉 Environment analysis complete!")

def main():
    explorer = IBKREnvironmentExplorer()
    explorer.run_comprehensive_test()

if __name__ == "__main__":
    main()
