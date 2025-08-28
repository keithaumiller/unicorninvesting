#!/bin/bash

# IBKR Client Portal Connection Test
# Unicorn Investing Platform

echo "🧪 IBKR Client Portal Connection Test"
echo "======================================"

# Load configuration
CONFIG_FILE="/workspaces/unicorninvesting/BackendPython/unicorn/config/ibkr/client_portal_config.json"

if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ Configuration file not found: $CONFIG_FILE"
    echo "   Run ./setup_client_portal.sh first"
    exit 1
fi

echo "✅ Configuration file found"

# Check if Client Portal Gateway is running
echo ""
echo "🔍 Checking Client Portal Gateway..."
if curl -k -s https://localhost:5000/v1/api/portal/iserver/auth/status >/dev/null 2>&1; then
    echo "✅ Client Portal Gateway is running on https://localhost:5000"
    
    # Test authentication status
    echo ""
    echo "🔐 Checking authentication status..."
    AUTH_RESPONSE=$(curl -k -s https://localhost:5000/v1/api/portal/iserver/auth/status)
    echo "Response: $AUTH_RESPONSE"
    
    if echo "$AUTH_RESPONSE" | grep -q '"authenticated":true'; then
        echo "✅ Authenticated successfully!"
        
        # Test basic market data endpoint
        echo ""
        echo "📊 Testing market data access..."
        python3 << 'EOF'
import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

from IBKRClientPortalConnector import IBKRClientPortalConnector

try:
    connector = IBKRClientPortalConnector()
    
    print("Testing health check...")
    health = connector.health_check()
    print(f"Health check result: {health}")
    
    print("\nTesting authentication status...")
    auth_result = connector.authenticate()
    print(f"Authentication: {auth_result}")
    
    if auth_result.get('success'):
        print("\n🎉 IBKR Client Portal integration working!")
        print("Ready for live market data and trading operations.")
    else:
        print("\n⚠️  Authentication not complete")
        print("Make sure you've logged in at https://localhost:5000")
        
except Exception as e:
    print(f"❌ Connection test failed: {e}")
    print("Make sure Client Portal Gateway is running and you're authenticated")
EOF
        
    else
        echo "⚠️  Not authenticated yet"
        echo "   Go to https://localhost:5000 and log in with your IBKR credentials"
    fi
    
else
    echo "❌ Client Portal Gateway not running"
    echo ""
    echo "📋 Setup Steps:"
    echo "1. Download Client Portal Gateway from IBKR"
    echo "2. Start the gateway application"
    echo "3. Navigate to https://localhost:5000"
    echo "4. Log in with your IBKR credentials"
    echo "5. Run this test again"
    echo ""
    echo "🔗 Download: https://www.interactivebrokers.com/en/trading/ib-api.php"
fi

echo ""
echo "📋 Configuration Details:"
echo "Account: $(grep -o '"account_number":"[^"]*"' "$CONFIG_FILE" | cut -d'"' -f4)"
echo "Username: $(grep -o '"username":"[^"]*"' "$CONFIG_FILE" | cut -d'"' -f4)"
echo "Trading Mode: $(grep -o '"trading_mode":"[^"]*"' "$CONFIG_FILE" | cut -d'"' -f4)"
