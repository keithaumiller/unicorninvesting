#!/bin/bash

# IBKR Account Information Update Script
# Designed for cron execution - updates account data from IBKR Gateway
# 
# This script:
# 1. Checks IBKR Gateway authentication status
# 2. Fetches current portfolio summary and positions
# 3. Updates JSON files used by the web interface
# 4. Logs results for monitoring

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
IBKR_DATA_DIR="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/accountinfo"
LOG_FILE="$IBKR_DATA_DIR/cron_update.log"
ACCOUNT_ID="YOU SHOULD BE LOOKING IN THE SECRETS FILE FOR YOUR ACCOUNT ID"

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$LOG_FILE"
    echo "$1"
}

# Check if IBKR Gateway is accessible
check_gateway() {
    if ! curl -s --max-time 5 http://localhost:5000/v1/api/iserver/auth/status >/dev/null 2>&1; then
        log "ERROR: IBKR Gateway not accessible"
        return 1
    fi
    return 0
}

# Check authentication status
check_authentication() {
    local auth_response
    auth_response=$(curl -s --max-time 5 "http://localhost:5000/sso/Dispatcher" 2>/dev/null)
    
    if echo "$auth_response" | grep -q "Client login succeeds" 2>/dev/null; then
        log "INFO: IBKR authentication active"
        return 0
    else
        log "WARNING: IBKR authentication may be expired"
        return 1
    fi
}

# Fetch portfolio summary
fetch_portfolio_summary() {
    # Dynamically determine the external URL
    local external_url
    if [ -n "$CODESPACE_NAME" ]; then
        external_url="https://${CODESPACE_NAME}-5000.app.github.dev"
    else
        # Fallback to localhost (may not work if authentication is session-based)
        external_url="http://localhost:5000"
    fi
    
    local summary_url="${external_url}/v1/api/portfolio/${ACCOUNT_ID}/summary"
    local temp_file=$(mktemp)
    
    if curl -s --max-time 10 -H "Accept: application/json" "$summary_url" > "$temp_file" 2>/dev/null; then
        # Validate JSON
        if jq empty "$temp_file" 2>/dev/null; then
            # Extract key metrics and create simplified portfolio data
            local net_liquidation=$(jq -r '.netliquidation.amount // 0' "$temp_file")
            local cash_balance=$(jq -r '.totalcashvalue.amount // 0' "$temp_file")
            local available_funds=$(jq -r '.availablefunds.amount // 0' "$temp_file")
            
            # Create current_portfolio.json structure
            cat > "$IBKR_DATA_DIR/current_portfolio.json" << EOF
{
  "account_id": "$ACCOUNT_ID",
  "positions": [],
  "summary": {
    "total_positions": 0,
    "net_liquidation": $net_liquidation,
    "market_value": 0.0,
    "cash_balance": $cash_balance,
    "unrealized_pnl": 0.0,
    "available_funds": $available_funds
  },
  "last_updated": "$(date -Iseconds)"
}
EOF
            log "INFO: Portfolio summary updated - Net Liquidation: \$$net_liquidation"
            rm "$temp_file"
            return 0
        else
            log "ERROR: Invalid JSON response from portfolio summary API"
            rm "$temp_file"
            return 1
        fi
    else
        log "ERROR: Failed to fetch portfolio summary from API"
        rm "$temp_file"
        return 1
    fi
}

# Fetch positions
fetch_positions() {
    # Dynamically determine the external URL
    local external_url
    if [ -n "$CODESPACE_NAME" ]; then
        external_url="https://${CODESPACE_NAME}-5000.app.github.dev"
    else
        # Fallback to localhost (may not work if authentication is session-based)
        external_url="http://localhost:5000"
    fi
    
    local positions_url="${external_url}/v1/api/portfolio/${ACCOUNT_ID}/positions/0"
    local temp_file=$(mktemp)
    
    if curl -s --max-time 10 -H "Accept: application/json" "$positions_url" > "$temp_file" 2>/dev/null; then
        # Validate JSON and check if it's an array
        if jq empty "$temp_file" 2>/dev/null && jq -e 'type == "array"' "$temp_file" >/dev/null 2>&1; then
            local position_count=$(jq 'length' "$temp_file")
            log "INFO: Positions updated - Count: $position_count"
            
            # If we have positions, update the portfolio file
            if [ "$position_count" -gt 0 ]; then
                # Update current_portfolio.json with actual positions
                local temp_portfolio=$(mktemp)
                jq --slurpfile positions "$temp_file" '.positions = $positions[0] | .summary.total_positions = ($positions[0] | length)' "$IBKR_DATA_DIR/current_portfolio.json" > "$temp_portfolio"
                mv "$temp_portfolio" "$IBKR_DATA_DIR/current_portfolio.json"
                log "INFO: Portfolio updated with $position_count positions"
            fi
            
            rm "$temp_file"
            return 0
        else
            log "WARNING: Invalid or empty positions response"
            rm "$temp_file"
            return 0  # Not critical failure - empty positions is valid
        fi
    else
        log "WARNING: Failed to fetch positions from API (non-critical)"
        rm "$temp_file"
        return 0  # Not critical failure
    fi
}

# Update data freshness indicator
update_freshness() {
    cat > "$IBKR_DATA_DIR/data_freshness.json" << EOF
{
  "status": "fresh",
  "last_successful_refresh": "$(date -Iseconds)",
  "safe_for_live_trading": true,
  "data_source": "live_gateway_api_cron",
  "cron_update": true
}
EOF
    log "INFO: Data freshness updated"
}

# Main execution
main() {
    log "START: IBKR account data update"
    
    # Ensure data directory exists
    mkdir -p "$IBKR_DATA_DIR"
    
    # Check gateway accessibility
    if ! check_gateway; then
        log "FAIL: Gateway check failed"
        exit 1
    fi
    
    # Check authentication (warning only, not fatal)
    check_authentication || true
    
    # Fetch data
    local success=true
    
    if ! fetch_portfolio_summary; then
        log "FAIL: Portfolio summary fetch failed"
        success=false
    fi
    
    # Fetch positions (non-critical)
    fetch_positions || true
    
    if [ "$success" = true ]; then
        update_freshness
        log "SUCCESS: Account data update completed"
        exit 0
    else
        log "FAIL: Account data update failed"
        exit 1
    fi
}

# Execute main function
main "$@"
