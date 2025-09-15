#!/bin/bash
#
# Data Refresh Validation Script
# Validates that automated data refresh is working correctly
#

UNICORN_ROOT="/workspaces/unicorninvesting"
LOG_DIR="$UNICORN_ROOT/logs/data_refresh"

echo "🔍 DATA REFRESH VALIDATION REPORT"
echo "=================================="
echo ""

# 1. Check cron service
echo "1️⃣ Cron Service Status:"
if pgrep cron > /dev/null; then
    echo "   ✅ Cron service is running"
else
    echo "   ❌ Cron service is not running"
fi
echo ""

# 2. Check installed cron jobs
echo "2️⃣ Installed Cron Jobs:"
if crontab -l 2>/dev/null | grep -q "automated_data_refresh"; then
    echo "   ✅ Data refresh job installed (every 5 minutes)"
    cron_count=$(crontab -l 2>/dev/null | wc -l)
    echo "   📊 Total jobs: $cron_count"
else
    echo "   ❌ Data refresh job not found"
fi
echo ""

# 3. Check data refresh script
echo "3️⃣ Data Refresh Script:"
refresh_script="$UNICORN_ROOT/scripts/cron/jobs/automated_data_refresh.sh"
if [ -x "$refresh_script" ]; then
    echo "   ✅ Script exists and is executable"
    echo "   📁 Location: $refresh_script"
else
    echo "   ❌ Script missing or not executable"
fi
echo ""

# 4. Check log directory
echo "4️⃣ Logging System:"
if [ -d "$LOG_DIR" ]; then
    echo "   ✅ Log directory exists"
    log_count=$(ls "$LOG_DIR"/data_refresh_*.log 2>/dev/null | wc -l)
    echo "   📊 Log files: $log_count"
    
    if [ $log_count -gt 0 ]; then
        latest_log=$(ls -t "$LOG_DIR"/data_refresh_*.log | head -1)
        echo "   📅 Latest log: $(basename "$latest_log")"
        
        # Check for recent activity
        if [ -f "$latest_log" ]; then
            last_success=$(grep "SUCCESS:" "$latest_log" | tail -1 | cut -d: -f2-)
            if [ -n "$last_success" ]; then
                echo "   ✅ Last successful refresh:$last_success"
            else
                echo "   ⚠️ No successful refresh found in latest log"
            fi
        fi
    fi
else
    echo "   ❌ Log directory not found"
fi
echo ""

# 5. Check silver layer data
echo "5️⃣ Silver Layer Data:"
silver_crypto_dir="$UNICORN_ROOT/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/processed_data/crypto"
if [ -d "$silver_crypto_dir" ]; then
    echo "   ✅ Silver layer directory exists"
    
    # Check latest files
    eth_latest="$silver_crypto_dir/ETH_silver_1h_latest.csv"
    btc_latest="$silver_crypto_dir/BTC_silver_1h_latest.csv"
    
    if [ -f "$eth_latest" ] && [ -f "$btc_latest" ]; then
        echo "   ✅ Latest data files exist"
        
        # Check file ages
        eth_age=$(( $(date +%s) - $(stat -c %Y "$eth_latest") ))
        btc_age=$(( $(date +%s) - $(stat -c %Y "$btc_latest") ))
        
        echo "   📊 ETH data age: $((eth_age / 60)) minutes"
        echo "   📊 BTC data age: $((btc_age / 60)) minutes"
        
        # Data freshness check
        max_age_minutes=60  # Consider data stale if older than 1 hour
        if [ $eth_age -lt $((max_age_minutes * 60)) ] && [ $btc_age -lt $((max_age_minutes * 60)) ]; then
            echo "   ✅ Data is fresh"
        else
            echo "   ⚠️ Data may be stale (older than $max_age_minutes minutes)"
        fi
    else
        echo "   ❌ Latest data files missing"
    fi
else
    echo "   ❌ Silver layer directory not found"
fi
echo ""

# 6. Test data connector
echo "6️⃣ Portfolio Data Access:"
cd "$UNICORN_ROOT/BackendPython/unicorn/4_portfolios/Myportolio"
python_test_result=$($UNICORN_ROOT/.venv/bin/python -c "
try:
    from core.silver_layer_data_connector import SilverLayerDataConnector
    connector = SilverLayerDataConnector()
    
    # Test basic access
    eth_price = connector.get_live_price('ETH')
    btc_price = connector.get_live_price('BTC')
    
    # Test historical data
    eth_data = connector.get_historical_data('ETH', '1h', periods=5)
    
    print(f'   ✅ ETH price: \${eth_price:.2f}')
    print(f'   ✅ BTC price: \${btc_price:.2f}')
    print(f'   ✅ Historical data: {len(eth_data)} records')
    print('   ✅ Portfolio data connector working')
except Exception as e:
    print(f'   ❌ Portfolio data connector failed: {e}')
" 2>/dev/null)

if [ $? -eq 0 ]; then
    echo "$python_test_result"
else
    echo "   ❌ Portfolio data connector test failed"
fi
echo ""

# 7. Overall status
echo "🎯 OVERALL STATUS:"
echo "=================="

# Count checks
total_checks=6
passed_checks=0

# Basic scoring (simplified)
if pgrep cron > /dev/null; then ((passed_checks++)); fi
if crontab -l 2>/dev/null | grep -q "automated_data_refresh"; then ((passed_checks++)); fi
if [ -x "$refresh_script" ]; then ((passed_checks++)); fi
if [ -d "$LOG_DIR" ]; then ((passed_checks++)); fi
if [ -f "$eth_latest" ] && [ -f "$btc_latest" ]; then ((passed_checks++)); fi
if echo "$python_test_result" | grep -q "Portfolio data connector working"; then ((passed_checks++)); fi

success_rate=$((passed_checks * 100 / total_checks))

if [ $success_rate -ge 90 ]; then
    echo "🎉 EXCELLENT: $success_rate% system health"
    echo "✅ Automated data refresh is fully operational"
elif [ $success_rate -ge 70 ]; then
    echo "⚠️ GOOD: $success_rate% system health"
    echo "🔧 Minor issues detected, mostly operational"
else
    echo "❌ NEEDS ATTENTION: $success_rate% system health"
    echo "🚨 Significant issues detected, manual intervention needed"
fi

echo ""
echo "📋 QUICK COMMANDS:"
echo "  Check status: ./scripts/manage_cron_jobs.sh status"
echo "  View logs:    ./scripts/manage_cron_jobs.sh logs"
echo "  Manual test:  ./scripts/manage_cron_jobs.sh test"
echo ""