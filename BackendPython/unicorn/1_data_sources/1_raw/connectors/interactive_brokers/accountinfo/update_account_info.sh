#!/bin/bash
#
# IBKR Account Info Update Script
#
# Simple automation script to update account information and generate reports.
# Can be scheduled via cron for periodic updates.
#
# Author: Unicorn Investing Platform
# Date: September 2, 2025
#

# Set script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Configuration
LOG_FILE="$SCRIPT_DIR/update.log"
NOTIFICATION_FILE="$SCRIPT_DIR/last_update_status.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to log with timestamp
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Function to show usage
show_usage() {
    echo "IBKR Account Information Update Script"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --collect-only    Only collect data, don't show summary"
    echo "  --summary-only    Only show summary from existing data"
    echo "  --detailed        Show detailed summary after collection"
    echo "  --quiet           Minimal output"
    echo "  --help            Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                 # Collect data and show quick summary"
    echo "  $0 --detailed      # Collect data and show detailed summary"
    echo "  $0 --summary-only  # Show summary without collecting new data"
}

# Parse command line arguments
COLLECT_DATA=true
SHOW_SUMMARY=true
DETAILED_SUMMARY=false
QUIET=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --collect-only)
            SHOW_SUMMARY=false
            shift
            ;;
        --summary-only)
            COLLECT_DATA=false
            shift
            ;;
        --detailed)
            DETAILED_SUMMARY=true
            shift
            ;;
        --quiet)
            QUIET=true
            shift
            ;;
        --help)
            show_usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Start update process
if [ "$QUIET" = false ]; then
    echo -e "${BLUE}🦄 IBKR Account Information Update${NC}"
    echo "===================================="
    echo ""
fi

log_message "Starting IBKR account information update"

# Check if IBKR Gateway is accessible
if [ "$COLLECT_DATA" = true ]; then
    if [ "$QUIET" = false ]; then
        echo -e "${YELLOW}🔍 Checking IBKR Gateway connection...${NC}"
    fi
    
    if curl -s http://localhost:5000/v1/api/iserver/auth/status >/dev/null 2>&1; then
        if [ "$QUIET" = false ]; then
            echo -e "${GREEN}✅ IBKR Gateway is accessible${NC}"
        fi
        log_message "IBKR Gateway connection successful"
    else
        echo -e "${RED}❌ IBKR Gateway is not accessible${NC}"
        log_message "ERROR: IBKR Gateway connection failed"
        echo "FAILED: IBKR Gateway not accessible" > "$NOTIFICATION_FILE"
        exit 1
    fi
fi

# Collect account information
if [ "$COLLECT_DATA" = true ]; then
    if [ "$QUIET" = false ]; then
        echo -e "${YELLOW}📊 Collecting account information...${NC}"
    fi
    
    log_message "Starting account information collection"
    
    if python3 collect_account_info.py >> "$LOG_FILE" 2>&1; then
        if [ "$QUIET" = false ]; then
            echo -e "${GREEN}✅ Account information collected successfully${NC}"
        fi
        log_message "Account information collection completed successfully"
        COLLECTION_STATUS="SUCCESS"
    else
        echo -e "${RED}❌ Failed to collect account information${NC}"
        log_message "ERROR: Account information collection failed"
        echo "FAILED: Account information collection error" > "$NOTIFICATION_FILE"
        exit 1
    fi
else
    COLLECTION_STATUS="SKIPPED"
    log_message "Account information collection skipped"
fi

# Show summary
if [ "$SHOW_SUMMARY" = true ]; then
    if [ "$QUIET" = false ]; then
        echo ""
        echo -e "${BLUE}📋 Account Capabilities Summary${NC}"
        echo "================================"
        echo ""
    fi
    
    if [ "$DETAILED_SUMMARY" = true ]; then
        python3 show_capabilities.py --detailed
    else
        python3 show_capabilities.py
    fi
    
    log_message "Account capabilities summary displayed"
fi

# Create status notification
STATUS_MESSAGE="SUCCESS: Account info updated on $(date '+%Y-%m-%d %H:%M:%S')"
if [ "$COLLECTION_STATUS" = "SKIPPED" ]; then
    STATUS_MESSAGE="SUCCESS: Summary displayed (collection skipped) on $(date '+%Y-%m-%d %H:%M:%S')"
fi

echo "$STATUS_MESSAGE" > "$NOTIFICATION_FILE"
log_message "Update process completed successfully"

if [ "$QUIET" = false ]; then
    echo ""
    echo -e "${GREEN}✅ IBKR account information update complete!${NC}"
    echo ""
    echo -e "${BLUE}📁 Files updated:${NC}"
    echo "   • Account capabilities: account_capabilities.json"
    echo "   • Market data access: market_data_access.json"
    echo "   • API endpoints: api_endpoints.json"
    echo "   • Risk parameters: risk_parameters.json"
    echo "   • Reports: reports/*.md"
    echo ""
    echo -e "${BLUE}📋 To view capabilities:${NC}"
    echo "   • Quick summary: python show_capabilities.py"
    echo "   • Detailed view: python show_capabilities.py --detailed"
    echo ""
    echo -e "${BLUE}🔄 To schedule automatic updates:${NC}"
    echo "   • Add to crontab: 0 */6 * * * $SCRIPT_DIR/update_account_info.sh --quiet"
fi
