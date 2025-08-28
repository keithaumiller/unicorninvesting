#!/bin/bash
"""
IBKR Client Portal Setup Script
Unicorn Investing Platform

This script sets up the IBKR Client Portal Web API integration.
No TWS or IB Gateway download required!
"""

echo "🚀 IBKR Client Portal API Setup"
echo "==============================="
echo ""
echo "📋 Overview:"
echo "The Client Portal Web API provides access to IBKR services through a web interface."
echo "Benefits:"
echo "  ✅ No TWS or IB Gateway software required"
echo "  ✅ Web-based authentication"
echo "  ✅ Full market data and trading capabilities"
echo "  ✅ Cross-platform compatibility"
echo ""

# Check Python dependencies
echo "🔍 Checking Python dependencies..."

python3 -c "
import requests
import pandas as pd
import json
print('✅ All required Python packages available')
" 2>/dev/null || {
    echo "❌ Missing Python dependencies"
    echo "Installing required packages..."
    pip install requests pandas
}

# Create IBKR configuration directory
IBKR_CONFIG_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/config/ibkr"
mkdir -p "$IBKR_CONFIG_DIR"

echo ""
echo "📝 IBKR Account Configuration"
echo "============================="
echo ""
echo "📊 Available Accounts:"
echo "Live Trading: [Your IBKR Username] ([Your Account Number])"
echo "Paper Trading: xyzyuc422 (DUM785491) ⭐ Recommended for testing"
echo ""

# Get account details
read -p "Enter your IBKR Account Number [DUM785491]: " IB_ACCOUNT
IB_ACCOUNT=${IB_ACCOUNT:-DUM785491}
read -p "Enter your IBKR Username [xyzyuc422]: " IB_USERNAME
IB_USERNAME=${IB_USERNAME:-xyzyuc422}
read -p "Trading Mode (paper/live) [paper]: " IB_TRADING_MODE
IB_TRADING_MODE=${IB_TRADING_MODE:-paper}

# Client Portal API doesn't need password for environment variables
# It uses web-based authentication

echo ""
echo "📊 Configuration Summary:"
echo "Account: $IB_ACCOUNT"
echo "Username: $IB_USERNAME"
echo "Trading Mode: $IB_TRADING_MODE"
echo "API Type: Client Portal Web API"
echo ""

read -p "Save configuration? (y/n): " CONFIRM

if [ "$CONFIRM" = "y" ] || [ "$CONFIRM" = "Y" ]; then
    # Create configuration file
    cat > "$IBKR_CONFIG_DIR/client_portal_config.json" << EOF
{
    "account_id": "$IB_ACCOUNT",
    "username": "$IB_USERNAME",
    "trading_mode": "$IB_TRADING_MODE",
    "api_type": "client_portal",
    "base_url": "https://localhost:5000/v1/api",
    "setup_date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

    # Add environment variables
    echo "" >> ~/.bashrc
    echo "# IBKR Client Portal Configuration" >> ~/.bashrc
    echo "export IBKR_ACCOUNT=\"$IB_ACCOUNT\"" >> ~/.bashrc
    echo "export IBKR_USERNAME=\"$IB_USERNAME\"" >> ~/.bashrc
    echo "export IBKR_TRADING_MODE=\"$IB_TRADING_MODE\"" >> ~/.bashrc
    echo "export IBKR_API_TYPE=\"client_portal\"" >> ~/.bashrc
    echo "export IBKR_CONFIG_DIR=\"$IBKR_CONFIG_DIR\"" >> ~/.bashrc
    
    # Export for current session
    export IBKR_ACCOUNT="$IB_ACCOUNT"
    export IBKR_USERNAME="$IB_USERNAME"
    export IBKR_TRADING_MODE="$IB_TRADING_MODE"
    export IBKR_API_TYPE="client_portal"
    export IBKR_CONFIG_DIR="$IBKR_CONFIG_DIR"
    
    echo "✅ Configuration saved successfully!"
    echo ""
    
    # Setup instructions
    echo "🔧 Next Steps - Client Portal Setup:"
    echo "===================================="
    echo ""
    echo "1. 📱 Download the IBKR Client Portal Gateway:"
    echo "   Visit: https://www.interactivebrokers.com/en/trading/ib-api.php"
    echo "   Download 'Client Portal Gateway' for your OS"
    echo ""
    echo "2. 🚀 Start the Client Portal Gateway:"
    echo "   - Run the downloaded gateway application"
    echo "   - It will start a local web server on https://localhost:5000"
    echo "   - You'll see a web interface for authentication"
    echo ""
    echo "3. 🔐 Authenticate through Web Interface:"
    echo "   - Open https://localhost:5000 in your browser"
    echo "   - Log in with your IBKR credentials"
    echo "   - Accept the security certificate warning (it's expected)"
    echo ""
    echo "4. ✅ Test the Connection:"
    echo "   ./test_ibkr_client_portal.sh"
    echo ""
    echo "📋 Configuration Details:"
    echo "Config file: $IBKR_CONFIG_DIR/client_portal_config.json"
    echo "Environment variables added to ~/.bashrc"
    echo ""
    echo "🔒 Security Notes:"
    echo "- Client Portal uses a self-signed SSL certificate (expected)"
    echo "- Authentication is done through the web interface"
    echo "- No passwords stored in environment variables"
    echo ""
    
    # Create a simple test script
    cat > "$IBKR_CONFIG_DIR/../test_ibkr_client_portal.sh" << 'EOF'
#!/bin/bash
echo "🧪 Testing IBKR Client Portal Connection"
echo "======================================="

# Check if Client Portal is running
if curl -k -s https://localhost:5000/v1/api/iserver/auth/status >/dev/null 2>&1; then
    echo "✅ Client Portal Gateway is running"
    
    # Run Python connector test
    echo "🐍 Testing Python connector..."
    cd "$(dirname "$0")/../../1_data_sources/1_raw/connectors/interactive_brokers"
    python3 IBKRClientPortalConnector.py
else
    echo "❌ Client Portal Gateway not accessible"
    echo ""
    echo "Please ensure:"
    echo "1. Client Portal Gateway is running"
    echo "2. You've authenticated through https://localhost:5000"
    echo "3. The gateway is accessible on port 5000"
fi
EOF
    
    chmod +x "$IBKR_CONFIG_DIR/../test_ibkr_client_portal.sh"
    
    echo "🎉 Setup Complete!"
    echo ""
    echo "Quick Start:"
    echo "1. Download and start Client Portal Gateway"
    echo "2. Authenticate at https://localhost:5000"
    echo "3. Run: $IBKR_CONFIG_DIR/../test_ibkr_client_portal.sh"
    
else
    echo "❌ Setup cancelled"
    exit 1
fi

echo ""
echo "📚 Additional Resources:"
echo "IBKR Client Portal API Docs: https://www.interactivebrokers.com/api/doc/portal/"
echo "Download Gateway: https://www.interactivebrokers.com/en/trading/ib-api.php#api-clients"
