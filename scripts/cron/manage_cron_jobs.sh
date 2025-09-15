#!/bin/bash
#
# Cron Job Management Script
# Manages automated data refresh and portfolio monitoring
#

UNICORN_ROOT="/workspaces/unicorninvesting"
CRONTAB_FILE="$UNICORN_ROOT/scripts/cron/configs/crontab_data_refresh.txt"
LOG_DIR="$UNICORN_ROOT/logs/data_refresh"

show_help() {
    echo "🕒 Unicorn Investing - Cron Job Management"
    echo "=========================================="
    echo ""
    echo "Usage: $0 [command]"
    echo ""
    echo "Commands:"
    echo "  install    - Install/update cron jobs for data refresh"
    echo "  remove     - Remove all data refresh cron jobs"
    echo "  status     - Show current cron job status"
    echo "  logs       - Show recent data refresh logs"
    echo "  test       - Run manual data refresh test"
    echo "  help       - Show this help message"
    echo ""
}

install_cron_jobs() {
    echo "🔄 Installing data refresh cron jobs..."
    
    if [ -f "$CRONTAB_FILE" ]; then
        crontab "$CRONTAB_FILE"
        echo "✅ Cron jobs installed successfully"
        echo ""
        echo "📋 Installed jobs:"
        crontab -l
        echo ""
        echo "🕒 Next refresh: $(date -d '+5 minutes' '+%H:%M:%S')"
    else
        echo "❌ Crontab file not found: $CRONTAB_FILE"
        exit 1
    fi
}

remove_cron_jobs() {
    echo "🗑️ Removing data refresh cron jobs..."
    crontab -r
    echo "✅ All cron jobs removed"
}

show_status() {
    echo "📊 Cron Job Status"
    echo "=================="
    echo ""
    
    # Check if cron is running
    if pgrep cron > /dev/null; then
        echo "✅ Cron service: Running"
    else
        echo "❌ Cron service: Not running"
    fi
    
    # Show current jobs
    echo ""
    echo "📋 Current cron jobs:"
    if crontab -l 2>/dev/null | grep -q .; then
        crontab -l
    else
        echo "  No cron jobs installed"
    fi
    
    # Show recent refresh status
    echo ""
    echo "📅 Recent refresh activity:"
    if [ -d "$LOG_DIR" ]; then
        latest_log=$(ls -t "$LOG_DIR"/data_refresh_*.log 2>/dev/null | head -1)
        if [ -n "$latest_log" ]; then
            echo "  Latest log: $(basename "$latest_log")"
            echo "  Last refresh: $(grep "Data Refresh Completed" "$latest_log" | tail -1 | cut -d: -f2-)"
            echo "  Status: $(grep -E "(SUCCESS|FAILED|PARTIAL)" "$latest_log" | tail -1)"
        else
            echo "  No refresh logs found"
        fi
    else
        echo "  Log directory not found"
    fi
}

show_logs() {
    echo "📜 Recent Data Refresh Logs"
    echo "==========================="
    echo ""
    
    if [ -d "$LOG_DIR" ]; then
        latest_log=$(ls -t "$LOG_DIR"/data_refresh_*.log 2>/dev/null | head -1)
        if [ -n "$latest_log" ]; then
            echo "📁 Latest log file: $(basename "$latest_log")"
            echo ""
            tail -20 "$latest_log"
        else
            echo "❌ No log files found"
        fi
    else
        echo "❌ Log directory not found"
    fi
}

test_refresh() {
    echo "🧪 Testing Manual Data Refresh"
    echo "==============================="
    echo ""
    
    if [ -x "$UNICORN_ROOT/scripts/cron/jobs/automated_data_refresh.sh" ]; then
        echo "🚀 Running data refresh script..."
        "$UNICORN_ROOT/scripts/cron/jobs/automated_data_refresh.sh"
        echo ""
        echo "✅ Manual test completed - check logs for details"
    else
        echo "❌ Data refresh script not found or not executable"
        exit 1
    fi
}

# Main command handling
case "${1:-help}" in
    install)
        install_cron_jobs
        ;;
    remove)
        remove_cron_jobs
        ;;
    status)
        show_status
        ;;
    logs)
        show_logs
        ;;
    test)
        test_refresh
        ;;
    help|*)
        show_help
        ;;
esac