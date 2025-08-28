#!/bin/bash

# IBKR OAuth 2.0 Setup Script
# Unicorn Investing Platform

echo "🔐 IBKR OAuth 2.0 Web API Setup"
echo "==============================="
echo ""
echo "This script sets up IBKR's modern OAuth 2.0 Web API integration."
echo "Benefits over Client Portal:"
echo "  ✅ No local gateway software required"
echo "  ✅ Industry-standard OAuth 2.0 security"
echo "  ✅ JWT-based authentication with private keys"
echo "  ✅ Perfect for web applications like Drupal"
echo "  ✅ More secure and scalable"
echo ""

# Configuration directory
CONFIG_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/config/ibkr"
OAUTH_CONFIG="$CONFIG_DIR/oauth_config.json"
PRIVATE_KEY_DIR="$CONFIG_DIR/keys"

# Create directories
mkdir -p "$CONFIG_DIR"
mkdir -p "$PRIVATE_KEY_DIR"

echo "🔍 Checking Python dependencies..."
python3 -c "
import sys
try:
    import jwt
    print('✅ PyJWT: Available')
except ImportError:
    print('❌ PyJWT: Missing - installing...')
    sys.exit(1)

try:
    import cryptography
    print('✅ cryptography: Available')
except ImportError:
    print('❌ cryptography: Missing - installing...')
    sys.exit(1)

try:
    import requests
    print('✅ requests: Available')
except ImportError:
    print('❌ requests: Missing - installing...')
    sys.exit(1)

try:
    import pandas
    print('✅ pandas: Available')
except ImportError:
    print('❌ pandas: Missing - installing...')
    sys.exit(1)
"

if [ $? -ne 0 ]; then
    echo ""
    echo "📦 Installing required Python packages..."
    pip install PyJWT cryptography requests pandas
fi

echo ""
echo "📝 OAuth Application Configuration"
echo "=================================="
echo ""
echo "To use IBKR's OAuth 2.0 API, you need to register an OAuth application."
echo "This is different from your trading account - it's a separate app registration."
echo ""

# Check if we already have configuration
if [ -f "$OAUTH_CONFIG" ]; then
    echo "✅ Found existing OAuth configuration"
    echo "Current settings:"
    cat "$OAUTH_CONFIG" | jq -r '
        "Client ID: " + (.client_id // "Not set"),
        "Account: " + (.account_number // "Not set"),
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
    echo "📋 OAuth Application Registration Required"
    echo "==========================================="
    echo ""
    echo "1. Go to IBKR Client Portal: https://www.interactivebrokers.com/portal"
    echo "2. Navigate to: Settings > API Settings > OAuth Applications"
    echo "3. Click 'Create New Application'"
    echo "4. Fill in application details:"
    echo "   - Application Name: Unicorn Investing Platform"
    echo "   - Application Type: Web Application"
    echo "   - Redirect URIs: https://localhost:8080/auth/callback"
    echo "   - Scopes: Select all needed scopes for trading and data"
    echo ""
    echo "5. After creation, you'll receive a CLIENT_ID"
    echo "6. Download or copy the public key requirement details"
    echo ""
    echo "❓ Have you completed the OAuth application registration? (y/n)"
    read -p "Enter y when you have your CLIENT_ID: " OAUTH_READY
    
    if [ "$OAUTH_READY" != "y" ]; then
        echo ""
        echo "⏸️  Setup paused - complete OAuth registration first"
        echo ""
        echo "📚 Resources:"
        echo "- IBKR OAuth Guide: https://www.interactivebrokers.com/api/doc/oauth/"
        echo "- Client Portal: https://www.interactivebrokers.com/portal"
        echo ""
        echo "Run this script again after completing OAuth registration."
        exit 0
    fi
    
    echo ""
    echo "Enter your OAuth application details:"
    read -p "IBKR Client ID: " CLIENT_ID
    read -p "IBKR Account Number: " ACCOUNT_NUMBER
    read -p "Trading Mode (paper/live) [paper]: " TRADING_MODE
    TRADING_MODE=${TRADING_MODE:-paper}
    
    # Generate private key
    PRIVATE_KEY_PATH="$PRIVATE_KEY_DIR/ibkr_oauth_private.pem"
    echo ""
    echo "🔑 Generating RSA Private Key..."
    python3 << EOF
import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

from IBKROAuthConnector import IBKROAuthConnector

connector = IBKROAuthConnector()
key_path = connector.generate_private_key('$PRIVATE_KEY_PATH')
print(f"✅ Private key generated: {key_path}")
print(f"✅ Public key generated: {key_path.replace('.pem', '_public.pem')}")
EOF
    
    # Create configuration file
    cat > "$OAUTH_CONFIG" << EOF
{
    "client_id": "$CLIENT_ID",
    "account_number": "$ACCOUNT_NUMBER",
    "trading_mode": "$TRADING_MODE",
    "api_type": "OAuth2_WebAPI",
    "private_key_path": "$PRIVATE_KEY_PATH",
    "created_at": "$(date -Iseconds)",
    "notes": "OAuth 2.0 configuration for IBKR Web API"
}
EOF
    
    echo "✅ Configuration saved to: $OAUTH_CONFIG"
    
    # Set up environment variables
    echo ""
    echo "🌍 Setting up environment variables..."
    
    # Add to current session
    export IBKR_CLIENT_ID="$CLIENT_ID"
    export IBKR_ACCOUNT="$ACCOUNT_NUMBER"
    export IBKR_TRADING_MODE="$TRADING_MODE"
    export IBKR_API_TYPE="OAuth2_WebAPI"
    export IBKR_PRIVATE_KEY_PATH="$PRIVATE_KEY_PATH"
    export IBKR_CONFIG_DIR="$CONFIG_DIR"
    
    # Add to .bashrc for persistence
    {
        echo ""
        echo "# IBKR OAuth 2.0 Configuration"
        echo "export IBKR_CLIENT_ID=\"$CLIENT_ID\""
        echo "export IBKR_ACCOUNT=\"$ACCOUNT_NUMBER\""
        echo "export IBKR_TRADING_MODE=\"$TRADING_MODE\""
        echo "export IBKR_API_TYPE=\"OAuth2_WebAPI\""
        echo "export IBKR_PRIVATE_KEY_PATH=\"$PRIVATE_KEY_PATH\""
        echo "export IBKR_CONFIG_DIR=\"$CONFIG_DIR\""
    } >> ~/.bashrc
    
    echo "✅ Environment variables configured"
fi

echo ""
echo "🔐 Public Key Registration Required"
echo "==================================="
echo ""
echo "📋 Next Steps:"
echo "1. Register your public key with IBKR:"
echo "   - Go to IBKR Client Portal > API Settings > OAuth Applications"
echo "   - Select your application: 'Unicorn Investing Platform'"
echo "   - Upload/paste your public key from:"
echo "     $PRIVATE_KEY_DIR/ibkr_oauth_private_public.pem"
echo ""
echo "2. Test the OAuth connection:"
echo "   python3 IBKROAuthConnector.py"
echo ""
echo "3. Or run the comprehensive test:"
echo "   ./test_oauth_connector.sh"
echo ""

# Show public key for easy copying
if [ -f "$PRIVATE_KEY_DIR/ibkr_oauth_private_public.pem" ]; then
    echo "📋 Your Public Key (copy this to IBKR):"
    echo "======================================"
    cat "$PRIVATE_KEY_DIR/ibkr_oauth_private_public.pem"
    echo "======================================"
fi

echo ""
echo "📊 Configuration Summary:"
echo "========================"
echo "API Type: OAuth 2.0 Web API"
echo "Config File: $OAUTH_CONFIG"
echo "Private Key: $PRIVATE_KEY_PATH"
echo "Public Key: $PRIVATE_KEY_DIR/ibkr_oauth_private_public.pem"

if [ -f "$OAUTH_CONFIG" ]; then
    echo "Client ID: $(jq -r '.client_id' "$OAUTH_CONFIG")"
    echo "Account: $(jq -r '.account_number' "$OAUTH_CONFIG")"
    echo "Trading Mode: $(jq -r '.trading_mode' "$OAUTH_CONFIG")"
fi

echo ""
echo "🔒 Security Notes:"
echo "=================="
echo "- Private key is stored securely with 600 permissions"
echo "- Never share your private key or client credentials"
echo "- OAuth tokens expire automatically for security"
echo "- Public key must be registered with IBKR for authentication"
echo ""
echo "🎉 OAuth 2.0 Setup Complete!"
echo ""
echo "📚 Resources:"
echo "- IBKR OAuth Documentation: https://www.interactivebrokers.com/api/doc/oauth/"
echo "- API Reference: https://www.interactivebrokers.com/api/doc/rest/"
echo "- Client Portal: https://www.interactivebrokers.com/portal"
