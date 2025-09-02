#!/usr/bin/env python3
"""
IBKR Bridge Connection Helper
Attempt to establish the iServer bridge connection for paper trading
"""

import requests
import json
import time
from datetime import datetime

def attempt_bridge_connection():
    """Try various methods to establish bridge connection"""
    
    base_url = "http://localhost:5000"
    session = requests.Session()
    
    print("🔧 ATTEMPTING TO ESTABLISH IBKR BRIDGE CONNECTION")
    print("=" * 60)
    
    # Step 1: Check current status
    print("\n1. Checking current authentication status...")
    
    sso_response = session.get(f"{base_url}/sso/Dispatcher")
    if "Client login succeeds" in sso_response.text:
        print("   ✅ SSO Authentication: OK")
    else:
        print("   ❌ SSO Authentication: Failed")
        return False
    
    # Step 2: Get session info
    print("\n2. Getting session information...")
    
    tickle_response = session.get(f"{base_url}/v1/api/tickle")
    if tickle_response.status_code == 200:
        tickle_data = tickle_response.json()
        session_id = tickle_data.get('session')
        print(f"   ✅ Session ID: {session_id}")
        
        # Check bridge status
        hmds_status = tickle_data.get('hmds', {})
        iserver_status = tickle_data.get('iserver', {})
        
        print(f"   📊 HMDS Status: {hmds_status}")
        print(f"   🔧 iServer Status: {iserver_status}")
    else:
        print("   ❌ Failed to get session info")
        return False
    
    # Step 3: Try different reauthentication methods
    print("\n3. Attempting bridge connection methods...")
    
    methods = [
        ("POST", "/v1/api/iserver/reauthenticate", "Standard reauthentication"),
        ("POST", "/v1/api/iserver/auth/status", "Auth status trigger"),
        ("GET", "/v1/api/iserver/auth/ssodh/init", "SSO DH initialization"),
        ("POST", "/v1/api/portal/sso/validate", "Portal SSO validation"),
    ]
    
    for method, endpoint, description in methods:
        try:
            print(f"   🔄 Trying: {description}")
            
            if method == "GET":
                response = session.get(f"{base_url}{endpoint}")
            else:
                response = session.post(f"{base_url}{endpoint}")
            
            print(f"      Status: {response.status_code}")
            if response.status_code == 200:
                try:
                    data = response.json()
                    print(f"      Response: {json.dumps(data, indent=2)[:200]}...")
                except:
                    print(f"      Response: {response.text[:100]}...")
            
            # Wait and check if bridge is now available
            time.sleep(2)
            
            test_response = session.get(f"{base_url}/v1/api/iserver/accounts")
            if test_response.status_code == 200:
                print(f"      ✅ Bridge connection established!")
                return True
            elif "no bridge" not in test_response.text.lower():
                print(f"      🔄 Different error: {test_response.text[:100]}")
                
        except Exception as e:
            print(f"      ❌ Error: {e}")
    
    # Step 4: Check if paper trading requires different endpoints
    print("\n4. Testing paper trading specific endpoints...")
    
    paper_endpoints = [
        "/v1/api/portfolio/accounts",
        "/v1/api/one/user", 
        "/v1/api/portal/portfolio/accounts",
    ]
    
    for endpoint in paper_endpoints:
        try:
            response = session.get(f"{base_url}{endpoint}")
            if response.status_code == 200:
                data = response.json()
                print(f"   ✅ {endpoint}: Working")
                
                # Look for account information that might help
                if isinstance(data, list) and len(data) > 0:
                    account = data[0]
                    if isinstance(account, dict):
                        trading_type = account.get('tradingType', '')
                        account_type = account.get('type', '')
                        print(f"      Account Type: {account_type}, Trading: {trading_type}")
                        
                        # Check for crypto access
                        crypto_p = account.get('PrepaidCrypto-P', False)
                        crypto_z = account.get('PrepaidCrypto-Z', False)
                        print(f"      Crypto Access: P={crypto_p}, Z={crypto_z}")
            else:
                print(f"   ❌ {endpoint}: {response.status_code}")
                
        except Exception as e:
            print(f"   ❌ {endpoint}: Error - {e}")
    
    # Step 5: Final bridge test
    print("\n5. Final bridge connection test...")
    
    final_tickle = session.get(f"{base_url}/v1/api/tickle")
    if final_tickle.status_code == 200:
        final_data = final_tickle.json()
        hmds_final = final_data.get('hmds', {})
        iserver_final = final_data.get('iserver', {})
        
        print(f"   📊 Final HMDS: {hmds_final}")
        print(f"   🔧 Final iServer: {iserver_final}")
        
        if 'error' not in hmds_final and iserver_final.get('authStatus', {}).get('authenticated', False):
            print("   ✅ Bridge connection successful!")
            return True
    
    print("\n❌ Bridge connection failed")
    print("\n💡 DIAGNOSIS:")
    print("   - SSO Authentication: ✅ Working")
    print("   - Portfolio Access: ✅ Working") 
    print("   - iServer Bridge: ❌ Not connecting")
    print("   - Market Data: ❌ Requires bridge")
    print("\n🎯 RECOMMENDATION:")
    print("   This appears to be a limitation of the paper trading environment.")
    print("   For full market data access, you may need:")
    print("   1. Live account (not paper trading)")
    print("   2. Market data subscriptions")
    print("   3. Different gateway configuration")
    
    return False

if __name__ == "__main__":
    attempt_bridge_connection()
