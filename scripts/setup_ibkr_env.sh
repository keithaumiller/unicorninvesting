#!/bin/bash
# IBKR Configuration Script for Unicorn Investing
# Run this script to set up your IBKR environment variables

echo "🚀 Setting up IBKR Integration for Unicorn Investing Platform"
echo "============================================================="

# Check if user has IBKR account details
echo ""
echo "Please provide your Interactive Brokers account details:"
echo "(These will be stored as environment variables)"
echo ""

# Get IBKR account details
read -p "Enter your IBKR Account Number: " IB_ACCOUNT
read -p "Enter your IBKR Username: " IB_USERNAME
read -s -p "Enter your IBKR Password: " IB_PASSWORD
echo ""
read -p "Trading Mode (paper/live) [paper]: " IB_TRADING_MODE
IB_TRADING_MODE=${IB_TRADING_MODE:-paper}

echo ""
echo "Configuration Summary:"
echo "Account: $IB_ACCOUNT"
echo "Username: $IB_USERNAME"
echo "Password: [HIDDEN]"
echo "Trading Mode: $IB_TRADING_MODE"
echo ""

read -p "Proceed with configuration? (y/n): " CONFIRM

if [ "$CONFIRM" = "y" ] || [ "$CONFIRM" = "Y" ]; then
    # Add to current session
    export QC_IB_ACCOUNT="$IB_ACCOUNT"
    export QC_IB_USER_NAME="$IB_USERNAME" 
    export QC_IB_PASSWORD="$IB_PASSWORD"
    export QC_IB_TRADING_MODE="$IB_TRADING_MODE"
    
    # Add to .bashrc for persistence
    echo "" >> ~/.bashrc
    echo "# IBKR Configuration for Unicorn Investing" >> ~/.bashrc
    echo "export QC_IB_ACCOUNT=\"$IB_ACCOUNT\"" >> ~/.bashrc
    echo "export QC_IB_USER_NAME=\"$IB_USERNAME\"" >> ~/.bashrc
    echo "export QC_IB_PASSWORD=\"$IB_PASSWORD\"" >> ~/.bashrc
    echo "export QC_IB_TRADING_MODE=\"$IB_TRADING_MODE\"" >> ~/.bashrc
    
    echo "✅ Environment variables configured successfully!"
    echo ""
    echo "Next steps:"
    echo "1. Enable API access in your IBKR account"
    echo "2. Install and configure IB Gateway"
    echo "3. Run: ./setup_lean_ibkr.sh"
    echo ""
else
    echo "❌ Configuration cancelled"
fi
