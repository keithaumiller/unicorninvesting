#!/bin/bash
#
# Automated Data Refresh Script
# Updates silver layer data every 5 minutes for real-time trading
#

# Set paths
UNICORN_ROOT="/workspaces/unicorninvesting"
VENV_PYTHON="$UNICORN_ROOT/.venv/bin/python"
SILVER_PROCESSOR="$UNICORN_ROOT/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/yahoo_finance_silver_processor.py"
LOG_DIR="$UNICORN_ROOT/logs/data_refresh"
LOG_FILE="$LOG_DIR/data_refresh_$(date +%Y%m%d).log"

# Create log directory if it doesn't exist
mkdir -p "$LOG_DIR"

echo "===============================================" >> "$LOG_FILE"
echo "🔄 Data Refresh Started: $(date)" >> "$LOG_FILE"
echo "===============================================" >> "$LOG_FILE"

# Function to log with timestamp
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

# Function to refresh bronze layer (fetch new data)
refresh_bronze_layer() {
    log_message "🔄 Refreshing bronze layer..."
    
    # Run bronze layer data collection
    cd "$UNICORN_ROOT/BackendPython/unicorn/1_data_sources/2_bronze/yahoo_finance_assets"
    
    if [ -f "yahoo_finance_bronze_processor.py" ]; then
        $VENV_PYTHON yahoo_finance_bronze_processor.py >> "$LOG_FILE" 2>&1
        if [ $? -eq 0 ]; then
            log_message "✅ Bronze layer refresh completed successfully"
        else
            log_message "❌ Bronze layer refresh failed"
            return 1
        fi
    else
        log_message "⚠️ Bronze processor not found, skipping bronze refresh"
    fi
    
    return 0
}

# Function to refresh silver layer (process data)
refresh_silver_layer() {
    log_message "🔄 Refreshing silver layer..."
    
    cd "$UNICORN_ROOT/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets"
    
    $VENV_PYTHON yahoo_finance_silver_processor.py >> "$LOG_FILE" 2>&1
    if [ $? -eq 0 ]; then
        log_message "✅ Silver layer refresh completed successfully"
        return 0
    else
        log_message "❌ Silver layer refresh failed"
        return 1
    fi
}

# Function to update portfolio data cache
refresh_portfolio_cache() {
    log_message "🔄 Refreshing portfolio data cache..."
    
    cd "$UNICORN_ROOT/BackendPython/unicorn/4_portfolios/Myportolio"
    
    # Test silver layer connector to ensure data is accessible
    $VENV_PYTHON -c "
from core.silver_layer_data_connector import SilverLayerDataConnector
connector = SilverLayerDataConnector()
print('🎯 Validating data access...')
try:
    price = connector.get_live_price('ETH')
    print(f'✅ ETH price accessible: \${price:.2f}')
    
    # Test multiple assets
    for asset in ['BTC', 'EURUSD', 'USDJPY']:
        try:
            test_price = connector.get_live_price(asset)
            print(f'✅ {asset} price accessible: \${test_price:.4f}')
        except Exception as e:
            print(f'⚠️ {asset} price issue: {e}')
            
    print('🎉 Portfolio cache validation complete')
except Exception as e:
    print(f'❌ Portfolio cache validation failed: {e}')
    exit(1)
" >> "$LOG_FILE" 2>&1
    
    if [ $? -eq 0 ]; then
        log_message "✅ Portfolio cache refresh completed successfully"
        return 0
    else
        log_message "❌ Portfolio cache refresh failed"
        return 1
    fi
}

# Main execution
log_message "🚀 Starting automated data refresh cycle"

# Step 1: Refresh bronze layer (if available)
refresh_bronze_layer

# Step 2: Refresh silver layer (required)
if refresh_silver_layer; then
    # Step 3: Validate portfolio cache
    if refresh_portfolio_cache; then
        log_message "🎉 Complete data refresh cycle successful"
        echo "✅ SUCCESS: Data refresh completed at $(date)" >> "$LOG_FILE"
    else
        log_message "⚠️ Portfolio cache refresh failed, but silver layer updated"
        echo "⚠️ PARTIAL: Silver updated but portfolio cache failed at $(date)" >> "$LOG_FILE"
    fi
else
    log_message "❌ Critical: Silver layer refresh failed"
    echo "❌ FAILED: Data refresh failed at $(date)" >> "$LOG_FILE"
    exit 1
fi

echo "===============================================" >> "$LOG_FILE"
echo "🏁 Data Refresh Completed: $(date)" >> "$LOG_FILE"
echo "===============================================" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

# Cleanup old log files (keep 7 days)
find "$LOG_DIR" -name "data_refresh_*.log" -mtime +7 -delete

log_message "🧹 Log cleanup completed"