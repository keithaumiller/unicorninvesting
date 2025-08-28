#!/bin/bash

# IBKR Client Portal Gateway Startup Script
# Unicorn Investing Platform

echo "🚀 Starting IBKR Client Portal Gateway"
echo "======================================"

GATEWAY_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/tools"

echo "📍 Gateway Location: $GATEWAY_DIR"
echo ""

# Check if Java is available
if ! command -v java &> /dev/null; then
    echo "❌ Java not found. Installing Java..."
    sudo apt update
    sudo apt install -y default-jre
fi

JAVA_VERSION=$(java -version 2>&1 | head -n1)
echo "☕ Java Version: $JAVA_VERSION"
echo ""

# Check if gateway files exist
if [ ! -f "$GATEWAY_DIR/bin/run.sh" ]; then
    echo "❌ Gateway files not found in $GATEWAY_DIR"
    echo "Run the download script first"
    exit 1
fi

echo "✅ Gateway files found"
echo ""

echo "🔧 Configuration Notes:"
echo "======================"
echo "• Gateway will start on: https://localhost:5000"
echo "• Use paper trading account: xyzyuc422 (DUM785491)"
echo "• Live trading account: keithaumiller (U21748632)"
echo "• Requires 2FA authentication"
echo "• Accept SSL certificate warning (self-signed)"
echo ""

echo "🚀 Starting Gateway..."
echo "====================="
echo ""

cd "$GATEWAY_DIR"

# Start the gateway
echo "Starting Client Portal Gateway..."
echo "Press Ctrl+C to stop the gateway"
echo ""

# Run the gateway (this will block)
./bin/run.sh

echo ""
echo "🛑 Gateway stopped"
