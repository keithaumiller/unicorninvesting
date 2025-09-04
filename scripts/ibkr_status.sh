#!/bin/bash

# IBKR Gateway Status and Management
# Quick script to check IBKR Gateway status and provide management options

echo "🏦 IBKR Gateway Status & Management"
echo "=================================="

# Check if gateway is running
if curl -s http://localhost:5000/v1/api/iserver/auth/status >/dev/null 2>&1; then
    echo "✅ IBKR Gateway: RUNNING"
    
    # Check authentication status
    AUTH_RESPONSE=$(curl -s http://localhost:5000/v1/api/iserver/auth/status 2>/dev/null)
    if echo "$AUTH_RESPONSE" | grep -q "authenticated.*true" 2>/dev/null; then
        echo "✅ Authentication: ACTIVE"
        echo "🚀 Status: READY FOR TRADING"
    else
        echo "⚠️  Authentication: REQUIRED"
        echo "🔗 Login URL: https://${CODESPACE_NAME:-codespace}-5000.app.github.dev/"
    fi
else
    echo "❌ IBKR Gateway: NOT RUNNING"
    echo ""
    echo "🚀 Start Options:"
    echo "  ibkr-start          - Start IBKR Gateway only"
    echo "  unicorn-env         - Start IBKR Gateway + full system check"
    echo "  unicorn-env --startup - Start all services + IBKR Gateway"
fi

echo ""
echo "🛠️  Available Commands:"
echo "  ibkr-start          - Start IBKR Gateway standalone"
echo "  unicorn-env         - Full environment setup (IBKR first)"
echo "  unicorn-root        - Change to project root"
echo "  drupal-start        - Start Drupal services"
echo ""
echo "📊 Port Status:"
echo "  Gateway: http://localhost:5000"
echo "  External: https://${CODESPACE_NAME:-codespace}-5000.app.github.dev/"
