#!/usr/bin/env python3
"""
IBKR Gateway Status Checker

Simple script to check IBKR Gateway connectivity and authentication status.
"""

import requests
import json
import sys
from datetime import datetime

def check_gateway_status():
    """Check IBKR Gateway status and authentication."""
    
    base_url = "http://localhost:5000"
    
    print("🏦 IBKR Gateway Status Check")
    print("=" * 30)
    print(f"🕒 Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"🔗 URL: {base_url}")
    print()
    
    # Test 1: Basic connectivity
    print("1️⃣  Testing Gateway Connectivity...")
    try:
        response = requests.get(f"{base_url}/v1/api/iserver/auth/status", timeout=10)
        if response.status_code == 200:
            print("   ✅ Gateway is accessible")
            auth_data = response.json()
            print(f"   📊 Response: {json.dumps(auth_data, indent=2)}")
        else:
            print(f"   ❌ Gateway returned HTTP {response.status_code}")
            return False
    except requests.exceptions.ConnectionError:
        print("   ❌ Cannot connect to Gateway (Connection refused)")
        return False
    except requests.exceptions.Timeout:
        print("   ❌ Gateway connection timeout")
        return False
    except Exception as e:
        print(f"   ❌ Gateway error: {e}")
        return False
    
    print()
    
    # Test 2: Authentication check via SSO endpoint
    print("2️⃣  Testing Authentication Status...")
    try:
        # Try the external URL approach that we know works
        import os
        codespace_name = os.environ.get('CODESPACE_NAME')
        if codespace_name:
            sso_url = f"https://{codespace_name}-5000.app.github.dev/sso/Dispatcher"
            response = requests.get(sso_url, timeout=10)
            if "Client login succeeds" in response.text:
                print("   ✅ Authentication is active")
            else:
                print("   ⚠️  Authentication may be expired")
                print(f"   📄 Response: {response.text[:100]}...")
        else:
            print("   ⚠️  Cannot determine external URL (no CODESPACE_NAME)")
    except Exception as e:
        print(f"   ⚠️  Authentication check failed: {e}")
    
    print()
    
    # Test 3: Try to get account data
    print("3️⃣  Testing Account Data Access...")
    try:
        account_id = "U21748632"  # Known account ID
        
        # Try external URL for portfolio summary
        if codespace_name:
            portfolio_url = f"https://{codespace_name}-5000.app.github.dev/v1/api/portfolio/{account_id}/summary"
            response = requests.get(portfolio_url, timeout=10)
            if response.status_code == 200:
                data = response.json()
                net_liq = data.get('netliquidation', {}).get('amount', 'N/A')
                cash_bal = data.get('totalcashvalue', {}).get('amount', 'N/A')
                print("   ✅ Portfolio data accessible")
                print(f"   💰 Net Liquidation: ${net_liq}")
                print(f"   💵 Cash Balance: ${cash_bal}")
            else:
                print(f"   ❌ Portfolio data failed: HTTP {response.status_code}")
        else:
            print("   ⚠️  Cannot test portfolio data (no external URL)")
            
    except Exception as e:
        print(f"   ❌ Portfolio data error: {e}")
    
    print()
    return True

def main():
    """Main execution."""
    try:
        if check_gateway_status():
            print("✅ Gateway check completed")
            sys.exit(0)
        else:
            print("❌ Gateway check failed")
            sys.exit(1)
    except KeyboardInterrupt:
        print("\n⚠️  Check interrupted by user")
        sys.exit(1)

if __name__ == "__main__":
    main()
