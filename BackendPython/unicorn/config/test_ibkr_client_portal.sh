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
