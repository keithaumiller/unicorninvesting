#!/bin/bash

# IBKR Account Validation Test
# Unicorn Investing Platform

echo "🧪 IBKR Account Configuration Test"
echo "=================================="
echo ""

echo "📊 Your IBKR Account Details"
echo "============================="
echo ""
echo "🏦 Live Trading Account:"
echo "   Username: keithaumiller"
echo "   Account:  U21748632"
echo "   API Access: ✅ Enabled (IBKR ticket #T895507)"
echo ""
echo "🧪 Paper Trading Account:"
echo "   Username: xyzyuc422"
echo "   Account:  DUM785491"
echo "   Purpose:  Safe testing environment"
echo ""
echo "🔑 API Key Status:"
echo "   Status: 📋 Pending - needs to be obtained from IBKR Client Portal"
echo "   Location: Client Portal > Settings > API Settings"
echo ""

echo "✅ Account Setup Status"
echo "======================="
echo ""

# Check if configurations exist
CONFIG_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/config/ibkr"

if [ -f "$CONFIG_DIR/webapi_config.json" ]; then
    echo "✅ Web API configuration found"
    
    CONFIGURED_ACCOUNT=$(jq -r '.account_number' "$CONFIG_DIR/webapi_config.json" 2>/dev/null)
    CONFIGURED_MODE=$(jq -r '.trading_mode' "$CONFIG_DIR/webapi_config.json" 2>/dev/null)
    
    echo "   Account: $CONFIGURED_ACCOUNT"
    echo "   Mode: $CONFIGURED_MODE"
    
    if [ "$CONFIGURED_ACCOUNT" = "DUM785491" ]; then
        echo "   ✅ Using paper trading account (safe)"
    elif [ "$CONFIGURED_ACCOUNT" = "U21748632" ]; then
        echo "   ⚠️  Using live trading account (be careful!)"
    fi
else
    echo "❌ Web API configuration not found"
    echo "   Run: ./setup_webapi.sh"
fi

echo ""

if [ -f "$CONFIG_DIR/client_portal_config.json" ]; then
    echo "✅ Client Portal configuration found"
    
    CONFIGURED_ACCOUNT=$(jq -r '.account_id' "$CONFIG_DIR/client_portal_config.json" 2>/dev/null)
    CONFIGURED_MODE=$(jq -r '.trading_mode' "$CONFIG_DIR/client_portal_config.json" 2>/dev/null)
    
    echo "   Account: $CONFIGURED_ACCOUNT"
    echo "   Mode: $CONFIGURED_MODE"
    
    if [ "$CONFIGURED_ACCOUNT" = "DUM785491" ]; then
        echo "   ✅ Using paper trading account (safe)"
    elif [ "$CONFIGURED_ACCOUNT" = "U21748632" ]; then
        echo "   ⚠️  Using live trading account (be careful!)"
    fi
else
    echo "❌ Client Portal configuration not found"
    echo "   Run: ./setup_client_portal.sh"
fi

echo ""
echo "🚀 Next Steps"
echo "============="
echo ""
echo "1. 🔑 Obtain API Key:"
echo "   - Visit: https://www.interactivebrokers.com/portal"
echo "   - Login with: keithaumiller / U21748632"
echo "   - Go to: Settings > API Settings"
echo "   - Generate Web API key"
echo ""
echo "2. 🧪 Recommended: Start with Paper Trading"
echo "   - Account: DUM785491 (xyzyuc422)"
echo "   - Safe for testing and development"
echo "   - No real money at risk"
echo ""
echo "3. ⚙️ Configure Connector:"
echo "   ./setup_webapi.sh          # For standard Web API"
echo "   ./setup_client_portal.sh   # For Client Portal API"
echo ""
echo "4. ✅ Test Integration:"
echo "   python3 IBKRWebAPIConnector.py"
echo ""
echo "📚 Documentation:"
echo "   See README.md for complete setup guide"
echo ""
echo "🔒 Security Reminder:"
echo "   Always test with paper trading first!"
