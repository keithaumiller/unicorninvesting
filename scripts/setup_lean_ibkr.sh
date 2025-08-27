#!/bin/bash

# LEAN Environment Configuration Script
# Sets up different trading environments (backtest, paper, live)

set -e

LEAN_ROOT="/workspaces/unicorninvesting/BackendPython/Lean"
CONFIG_FILE="$LEAN_ROOT/Launcher/config.json"

echo "🔧 LEAN Environment Configuration"
echo "=================================="

# Function to backup current config
backup_config() {
    cp "$CONFIG_FILE" "$CONFIG_FILE.backup.$(date +%Y%m%d_%H%M%S)"
    echo "✅ Configuration backed up"
}

# Function to set environment
set_environment() {
    local env_type=$1
    echo "🔄 Setting environment to: $env_type"
    
    # Create backup first
    backup_config
    
    # Update environment in config.json
    sed -i "s/"environment": ".*"/"environment": "$env_type"/" "$CONFIG_FILE"
    
    echo "✅ Environment set to: $env_type"
}

# Function to configure IBKR settings for different environments
configure_ibkr() {
    local trading_mode=$1
    local port=$2
    
    echo "🔄 Configuring IBKR for $trading_mode mode (port $port)"
    
    # Update trading mode and port
    sed -i "s/"ib-trading-mode": ".*"/"ib-trading-mode": "$trading_mode"/" "$CONFIG_FILE"
    sed -i "s/"ib-port": ".*"/"ib-port": "$port"/" "$CONFIG_FILE"
    
    echo "✅ IBKR configured for $trading_mode mode"
}

# Function to show current configuration
show_config() {
    echo "📋 Current Configuration:"
    echo "========================"
    
    local env=$(grep '"environment"' "$CONFIG_FILE" | cut -d'"' -f4)
    local trading_mode=$(grep '"ib-trading-mode"' "$CONFIG_FILE" | cut -d'"' -f4)
    local port=$(grep '"ib-port"' "$CONFIG_FILE" | cut -d'"' -f4)
    
    echo "Environment: $env"
    echo "IBKR Trading Mode: $trading_mode"
    echo "IBKR Port: $port"
    echo ""
}
