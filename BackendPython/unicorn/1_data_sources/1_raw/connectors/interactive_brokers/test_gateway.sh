#!/bin/bash

# Quick Gateway Test Script
# Tests if the IBKR Client Portal Gateway is running and accessible

echo "🧪 IBKR Gateway Quick Test"
echo "=========================="

echo "🔍 Testing gateway connection..."

# Test if gateway is running
if curl -k -s --connect-timeout 5 https://localhost:5000/v1/api/portal/iserver/auth/status >/dev/null 2>&1; then
    echo "✅ Gateway is running on https://localhost:5000"
    
    # Get authentication status
    AUTH_STATUS=$(curl -k -s https://localhost:5000/v1/api/portal/iserver/auth/status)
    
    echo ""
    echo "🔐 Authentication Status:"
    echo "$AUTH_STATUS" | python3 -m json.tool 2>/dev/null || echo "$AUTH_STATUS"
    
    if echo "$AUTH_STATUS" | grep -q '"authenticated":true'; then
        echo ""
        echo "✅ Authentication successful!"
        echo "🚀 Ready to test IBKR connector"
        echo ""
        echo "Run: /workspaces/unicorninvesting/.venv/bin/python -c \"
from IBKRClientPortalConnector import IBKRClientPortalConnector
connector = IBKRClientPortalConnector()
print('Health Check:', connector.health_check())
\""
    else
        echo ""
        echo "⚠️  Not authenticated yet"
        echo "👉 Go to https://localhost:5000 and login"
        echo "   Use: xyzyuc422 / DUM785491 (paper trading)"
        echo "   Or: [Your IBKR Username] / [Your Account Number] (live trading)"
    fi
    
else
    echo "❌ Gateway not running or not accessible"
    echo ""
    echo "🚀 To start gateway:"
    echo "./start_gateway.sh"
    echo ""
    echo "🔗 Or manually:"
    echo "cd tools && ./bin/run.sh"
fi

echo ""
echo "📋 Next Steps:"
echo "1. Start gateway: ./start_gateway.sh"
echo "2. Open browser: https://localhost:5000" 
echo "3. Login with IBKR credentials + 2FA"
echo "4. Test connector: ./test_ibkr_client_portal.sh"
