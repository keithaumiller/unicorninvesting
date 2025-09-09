#!/bin/bash

# FRED Historical Data Collection Script
# Collects comprehensive historical economic data with throttling

echo "🏦 FRED Historical Data Collection"
echo "=================================="
echo ""
echo "This script will collect 25+ years of historical economic data from FRED"
echo "including Federal Reserve policy, inflation, employment, and market indicators."
echo ""
echo "⏱️  Collection details:"
echo "   • 25+ economic series across 7 categories"
echo "   • Historical data from 2000 to present"
echo "   • 2-second throttling between API calls"
echo "   • Estimated time: 3-5 minutes"
echo ""
echo "💾 Data will be saved to:"
echo "   /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/economic_indicators/fred/"
echo ""

# Set working directory
cd /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred

# Set API key
export FRED_API_KEY="YOUR_FRED_API_KEY"

# Check if virtual environment is activated
if [ -z "$VIRTUAL_ENV" ]; then
    echo "🔄 Activating Python virtual environment..."
    source /workspaces/unicorninvesting/.venv/bin/activate
fi

# Run comprehensive data collection
echo "🚀 Starting comprehensive FRED data collection..."
echo ""

python fred_connector.py --comprehensive
