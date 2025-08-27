#!/bin/bash
# Test IBKR Connection Script

echo "🔌 Testing IBKR Connection"
echo "=========================="

# Check environment variables
if [ -z "$QC_IB_ACCOUNT" ]; then
    echo "❌ Environment variables not set. Run ./scripts/setup_ibkr_env.sh first"
    exit 1
fi

echo "Testing with account: $QC_IB_ACCOUNT"
echo "Trading mode: $QC_IB_TRADING_MODE"

# Test network connectivity to IB Gateway
PORT=$([ "$QC_IB_TRADING_MODE" = "paper" ] && echo "4002" || echo "4001")
echo "Testing connection to localhost:$PORT..."

if nc -z localhost $PORT 2>/dev/null; then
    echo "✅ IB Gateway is accessible on port $PORT"
else
    echo "❌ Cannot connect to IB Gateway on port $PORT"
    echo ""
    echo "Please ensure:"
    echo "1. IB Gateway is running"
    echo "2. API is enabled in IB Gateway settings"
    echo "3. Socket port is set to $PORT"
    echo "4. 127.0.0.1 is in trusted IPs"
    echo ""
    echo "IB Gateway Setup Instructions:"
    echo "1. Start IB Gateway"
    echo "2. Go to Configure > Settings > API"
    echo "3. Enable 'Enable ActiveX and Socket Clients'"
    echo "4. Set Socket port to $PORT"
    echo "5. Add 127.0.0.1 to Trusted IPs"
    exit 1
fi

# Test Python LEAN integration
echo "Testing Python LEAN integration..."

cd /workspaces/unicorninvesting/BackendPython/Lean

python3 << EOF
import sys
import os

try:
    # Test basic imports
    print("Testing LEAN imports...")
    sys.path.append('.')
    
    # This will test if LEAN is properly configured
    from QuantConnect import *
    print("✅ QuantConnect core imported successfully")
    
    # Test IBKR specific imports
    from QuantConnect.Brokerages.InteractiveBrokers import InteractiveBrokersBrokerage
    print("✅ Interactive Brokers brokerage imported successfully")
    
    # Test configuration loading
    from QuantConnect.Configuration import Config
    print("✅ Configuration system imported successfully")
    
    print("\n🎉 All imports successful! IBKR integration is ready.")
    print("\nNext steps:")
    print("1. Ensure IB Gateway is running")
    print("2. Test live connection with sample algorithm")
    print("3. Integrate with Drupal portfolio interface")
    
except ImportError as e:
    print(f"❌ Import error: {e}")
    print("Please check LEAN installation")
    sys.exit(1)
except Exception as e:
    print(f"❌ Error: {e}")
    sys.exit(1)
EOF

echo ""
echo "✅ IBKR connection test completed!"
echo ""
echo "If all tests passed, you can now:"
echo "1. Create live trading algorithms"
echo "2. Connect your Drupal interface to live IBKR data"
echo "3. Start paper trading to test the integration"
