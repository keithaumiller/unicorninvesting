#!/bin/bash

# IBKR Standard Web API Setup Script
# Unicorn Investing Platform

echo "🌐 IBKR Standard Web API Setup"
echo "=============================="
echo ""
echo "Simple, straightforward IBKR Web API integration."
echo "Uses standard API authentication - no OAuth complexity."
echo ""

# Configuration directory
CONFIG_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/config/ibkr"
WEBAPI_CONFIG="$CONFIG_DIR/webapi_config.json"

# Create directories
mkdir -p "$CONFIG_DIR"

echo "🔍 Checking Python dependencies..."
python3 -c "
import sys
packages = ['requests', 'pandas', 'json', 'datetime']
missing = []

for package in packages:
    try:
        if package == 'json' or package == 'datetime':
            # Built-in modules
            exec(f'import {package}')
        else:
            exec(f'import {package}')
        print(f'✅ {package}: Available')
    except ImportError:
        print(f'❌ {package}: Missing')
        missing.append(package)

if missing:
    print(f'Install missing packages: pip install {\" \".join(missing)}')
    sys.exit(1)
"

if [ $? -ne 0 ]; then
    echo ""
    echo "📦 Installing required packages..."
    pip install requests pandas
fi

echo ""
echo "📝 IBKR Web API Configuration"
echo "============================="

# Check if we already have configuration
if [ -f "$WEBAPI_CONFIG" ]; then
    echo "✅ Found existing configuration"
    echo "Current settings:"
    cat "$WEBAPI_CONFIG" | jq -r '
        "API Key: " + (.api_key // "Not set"),
        "Account: " + (.account_number // "Not set"),
        "Username: " + (.username // "Not set"),
        "Trading Mode: " + (.trading_mode // "Not set")
    ' 2>/dev/null || echo "Configuration file exists but may need updating"
    echo ""
    read -p "Update configuration? (y/n) [n]: " UPDATE_CONFIG
    UPDATE_CONFIG=${UPDATE_CONFIG:-n}
else
    UPDATE_CONFIG="y"
fi

if [ "$UPDATE_CONFIG" = "y" ]; then
    echo ""
    echo "📋 IBKR API Access Setup"
    echo "========================"
    echo ""
    echo "To use IBKR's Web API, you need:"
    echo "1. ✅ IBKR Account (you have: [Your Account Number])"
    echo "2. ✅ API Access Enabled (confirmed by IBKR support)"
    echo "3. 🔑 API Key (we'll configure this)"
    echo ""
    echo "📚 Getting Your API Key:"
    echo "1. Log into IBKR Client Portal: https://www.interactivebrokers.com/portal"
    echo "2. Go to: Settings > API Settings"
    echo "3. Look for 'Web API' or 'REST API' section"
    echo "4. Generate/copy your API key"
    echo ""
    echo "Note: If you don't see API settings, contact IBKR support."
    echo "Your ticket #T895507 confirms API access is enabled."
    echo ""
    
    read -p "Do you have your IBKR API key? (y/n): " HAS_API_KEY
    
    if [ "$HAS_API_KEY" != "y" ]; then
        echo ""
        echo "⏸️  Setup paused - get your API key first"
        echo ""
        echo "Steps to get API key:"
        echo "1. Visit: https://www.interactivebrokers.com/portal"
        echo "2. Settings > API Settings"
        echo "3. Generate Web API key"
        echo "4. Run this script again"
        echo ""
        exit 0
    fi
    
    echo ""
    echo "📊 Available Accounts:"
    echo "Live Trading: [Your IBKR Username] ([Your Account Number])"
    echo "Paper Trading: xyzyuc422 (DUM785491) ⭐ Recommended for testing"
    echo ""
    echo "Enter your IBKR configuration:"
    read -p "IBKR API Key: " API_KEY
    read -p "Trading Mode (paper/live) [paper]: " TRADING_MODE
    TRADING_MODE=${TRADING_MODE:-paper}
    
    # Set defaults based on trading mode
    if [ "$TRADING_MODE" = "paper" ]; then
        DEFAULT_ACCOUNT="DUM785491"
        DEFAULT_USERNAME="xyzyuc422"
        echo "📋 Using paper trading defaults (safe for testing)"
    else
        DEFAULT_ACCOUNT="[Your Account Number]"
        DEFAULT_USERNAME="[YOUR_IBKR_USERNAME]"
        echo "⚠️  Using live trading account - be careful!"
    fi
    
    read -p "IBKR Account Number [$DEFAULT_ACCOUNT]: " ACCOUNT_NUMBER
    ACCOUNT_NUMBER=${ACCOUNT_NUMBER:-$DEFAULT_ACCOUNT}
    read -p "IBKR Username [$DEFAULT_USERNAME]: " USERNAME
    USERNAME=${USERNAME:-$DEFAULT_USERNAME}
    
    # Create configuration file
    cat > "$WEBAPI_CONFIG" << EOF
{
    "api_key": "$API_KEY",
    "account_number": "$ACCOUNT_NUMBER",
    "username": "$USERNAME",
    "trading_mode": "$TRADING_MODE",
    "api_type": "Standard_WebAPI",
    "base_url": "https://api.ibkr.com/v1/api",
    "paper_url": "https://api.ibkr.com/v1/api/paper",
    "created_at": "$(date -Iseconds)",
    "notes": "Standard Web API configuration for IBKR"
}
EOF
    
    echo "✅ Configuration saved to: $WEBAPI_CONFIG"
    
    # Set up environment variables
    echo ""
    echo "🌍 Setting up environment variables..."
    
    # Add to current session
    export IBKR_API_KEY="$API_KEY"
    export IBKR_ACCOUNT="$ACCOUNT_NUMBER"
    export IBKR_USERNAME="$USERNAME"
    export IBKR_TRADING_MODE="$TRADING_MODE"
    export IBKR_API_TYPE="Standard_WebAPI"
    export IBKR_CONFIG_DIR="$CONFIG_DIR"
    
    # Add to .bashrc for persistence
    {
        echo ""
        echo "# IBKR Standard Web API Configuration"
        echo "export IBKR_API_KEY=\"$API_KEY\""
        echo "export IBKR_ACCOUNT=\"$ACCOUNT_NUMBER\""
        echo "export IBKR_USERNAME=\"$USERNAME\""
        echo "export IBKR_TRADING_MODE=\"$TRADING_MODE\""
        echo "export IBKR_API_TYPE=\"Standard_WebAPI\""
        echo "export IBKR_CONFIG_DIR=\"$CONFIG_DIR\""
    } >> ~/.bashrc
    
    echo "✅ Environment variables configured"
fi

echo ""
echo "🧪 Testing Connection"
echo "===================="

# Test the connector
python3 << 'EOF'
import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

try:
    from IBKRWebAPIConnector import IBKRWebAPIConnector
    
    print("✅ Connector module imported successfully")
    
    # Initialize connector
    connector = IBKRWebAPIConnector()
    print("✅ Connector initialized")
    
    # Run health check
    print("\n🔍 Running health check...")
    health = connector.health_check()
    
    print(f"\nHealth Status: {health['status'].upper()}")
    print(f"API URL: {health['api_url']}")
    print(f"Trading Mode: {health['trading_mode']}")
    print(f"Configuration: {'✅' if health['config_loaded'] else '❌'}")
    print(f"API Key: {'✅' if health['has_api_key'] else '❌'}")
    print(f"Account: {'✅' if health['has_account'] else '❌'}")
    print(f"Authentication: {'✅' if health['authenticated'] else '❌'}")
    
    if health.get('auth_error'):
        print(f"Auth Error: {health['auth_error']}")
    
    if health['status'] == 'healthy':
        print("\n🎉 IBKR Web API connector is ready!")
        print("\nTesting market data...")
        
        # Test market data
        market_result = connector.get_market_data('AAPL')
        if market_result['success']:
            print("✅ Market data test successful")
            print(f"AAPL Contract ID: {market_result.get('contract_id')}")
        else:
            print(f"❌ Market data test failed: {market_result.get('error')}")
    
    else:
        print(f"\n⚠️  Setup issues detected")
        if not health['has_api_key']:
            print("- Missing API key")
        if not health['authenticated']:
            print("- Authentication failed")
            
except ImportError as e:
    print(f"❌ Import error: {e}")
except Exception as e:
    print(f"❌ Test failed: {e}")
EOF

echo ""
echo "📊 Configuration Summary"
echo "========================"

if [ -f "$WEBAPI_CONFIG" ]; then
    echo "✅ Configuration file: $WEBAPI_CONFIG"
    echo "API Type: Standard Web API"
    echo "Account: $(jq -r '.account_number' "$WEBAPI_CONFIG" 2>/dev/null || echo 'Not configured')"
    echo "Username: $(jq -r '.username' "$WEBAPI_CONFIG" 2>/dev/null || echo 'Not configured')"
    echo "Trading Mode: $(jq -r '.trading_mode' "$WEBAPI_CONFIG" 2>/dev/null || echo 'Not configured')"
    echo "API Key: $(jq -r '.api_key' "$WEBAPI_CONFIG" 2>/dev/null | sed 's/./*/g' || echo 'Not configured')"
else
    echo "❌ Configuration file not created"
fi

echo ""
echo "🚀 Quick Start Commands"
echo "======================="
echo ""
echo "# Test the connector"
echo "python3 IBKRWebAPIConnector.py"
echo ""
echo "# Get market data"
echo "python3 -c \"
from IBKRWebAPIConnector import IBKRWebAPIConnector
connector = IBKRWebAPIConnector()
result = connector.get_market_data('TSLA')
print(result)
\""
echo ""
echo "# Get account info"
echo "python3 -c \"
from IBKRWebAPIConnector import IBKRWebAPIConnector
connector = IBKRWebAPIConnector()
result = connector.get_accounts()
print(result)
\""
echo ""
echo "🔒 Security Notes"
echo "================="
echo "- API key is stored locally and in environment variables"
echo "- Paper trading mode is enabled by default"
echo "- Change to 'live' mode only when ready for real trading"
echo "- Never share your API key or commit it to version control"
echo ""
echo "🎉 IBKR Standard Web API Setup Complete!"
echo ""
echo "📚 Resources:"
echo "- IBKR Web API Docs: https://www.interactivebrokers.com/api/doc/rest/"
echo "- Client Portal: https://www.interactivebrokers.com/portal"
echo "- API Settings: https://www.interactivebrokers.com/portal (Settings > API)"
