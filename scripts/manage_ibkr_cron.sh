#!/bin/bash

# IBKR Cron Management Script
# Manages automated IBKR account data collection via cron
#
# Usage:
#   ./manage_ibkr_cron.sh install   - Install cron job for 1-minute updates
#   ./manage_ibkr_cron.sh remove    - Remove cron job
#   ./manage_ibkr_cron.sh status    - Show cron job status
#   ./manage_ibkr_cron.sh logs      - Show recent update logs
#   ./manage_ibkr_cron.sh test      - Test update script manually

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
UPDATE_SCRIPT="$SCRIPT_DIR/ibkr_account_update.sh"
CRON_JOB_COMMENT="# Unicorn Investing - IBKR Account Data Update"
CRON_JOB_CMD="* * * * * $UPDATE_SCRIPT >/dev/null 2>&1"
LOG_FILE="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/accountinfo/cron_update.log"

# Color output functions
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

success() { echo -e "${GREEN}✅ $1${NC}"; }
warning() { echo -e "${YELLOW}⚠️  $1${NC}"; }
error() { echo -e "${RED}❌ $1${NC}"; }
info() { echo -e "${BLUE}ℹ️  $1${NC}"; }

# Check if cron job exists
cron_job_exists() {
    crontab -l 2>/dev/null | grep -F "$UPDATE_SCRIPT" >/dev/null
}

# Install cron job
install_cron() {
    echo "🔧 Installing IBKR account update cron job..."
    
    # Check if update script exists and is executable
    if [[ ! -f "$UPDATE_SCRIPT" ]]; then
        error "Update script not found: $UPDATE_SCRIPT"
        return 1
    fi
    
    if [[ ! -x "$UPDATE_SCRIPT" ]]; then
        warning "Making update script executable..."
        chmod +x "$UPDATE_SCRIPT"
    fi
    
    # Check if cron job already exists
    if cron_job_exists; then
        warning "Cron job already exists"
        return 0
    fi
    
    # Add cron job
    (crontab -l 2>/dev/null || true; echo "$CRON_JOB_COMMENT"; echo "$CRON_JOB_CMD") | crontab -
    
    if cron_job_exists; then
        success "Cron job installed successfully"
        info "IBKR account data will be updated every minute"
        info "Logs: $LOG_FILE"
        return 0
    else
        error "Failed to install cron job"
        return 1
    fi
}

# Remove cron job
remove_cron() {
    echo "🗑️  Removing IBKR account update cron job..."
    
    if ! cron_job_exists; then
        warning "Cron job not found"
        return 0
    fi
    
    # Remove cron job and comment
    crontab -l 2>/dev/null | grep -v -F "$UPDATE_SCRIPT" | grep -v -F "$CRON_JOB_COMMENT" | crontab -
    
    if ! cron_job_exists; then
        success "Cron job removed successfully"
        return 0
    else
        error "Failed to remove cron job"
        return 1
    fi
}

# Show cron job status
show_status() {
    echo "📊 IBKR Cron Job Status"
    echo "======================"
    
    if cron_job_exists; then
        success "Cron job is installed"
        echo ""
        info "Current cron job:"
        crontab -l 2>/dev/null | grep -A1 -B1 -F "$UPDATE_SCRIPT"
    else
        warning "Cron job is not installed"
    fi
    
    echo ""
    info "Update script: $UPDATE_SCRIPT"
    info "Log file: $LOG_FILE"
    
    # Check if log file exists and show recent activity
    if [[ -f "$LOG_FILE" ]]; then
        local log_size=$(wc -l < "$LOG_FILE" 2>/dev/null || echo "0")
        local last_update=$(tail -1 "$LOG_FILE" 2>/dev/null | cut -d' ' -f1-2)
        info "Log entries: $log_size"
        info "Last update: $last_update"
        
        # Show recent success/failure summary
        local recent_success=$(tail -50 "$LOG_FILE" 2>/dev/null | grep -c "SUCCESS:" || echo "0")
        local recent_errors=$(tail -50 "$LOG_FILE" 2>/dev/null | grep -c "ERROR:" || echo "0")
        echo ""
        info "Recent activity (last 50 entries):"
        echo "   Success: $recent_success"
        echo "   Errors: $recent_errors"
    else
        warning "Log file not found (no updates yet)"
    fi
}

# Show recent logs
show_logs() {
    local lines=${1:-50}
    
    echo "📜 Recent IBKR Update Logs (last $lines lines)"
    echo "================================================"
    
    if [[ -f "$LOG_FILE" ]]; then
        tail -"$lines" "$LOG_FILE"
    else
        warning "Log file not found: $LOG_FILE"
    fi
}

# Test update script
test_update() {
    echo "🧪 Testing IBKR account update script..."
    
    if [[ ! -f "$UPDATE_SCRIPT" ]]; then
        error "Update script not found: $UPDATE_SCRIPT"
        return 1
    fi
    
    if [[ ! -x "$UPDATE_SCRIPT" ]]; then
        error "Update script is not executable"
        return 1
    fi
    
    info "Running update script manually..."
    echo ""
    
    # Run the update script
    "$UPDATE_SCRIPT"
    local exit_code=$?
    
    echo ""
    if [[ $exit_code -eq 0 ]]; then
        success "Update script completed successfully"
    else
        error "Update script failed with exit code $exit_code"
    fi
    
    # Show last few log entries
    echo ""
    info "Recent log entries:"
    if [[ -f "$LOG_FILE" ]]; then
        tail -5 "$LOG_FILE"
    else
        warning "No log file found"
    fi
    
    return $exit_code
}

# Show help
show_help() {
    echo "IBKR Cron Management Script"
    echo "==========================="
    echo ""
    echo "Manages automated IBKR account data collection via cron"
    echo ""
    echo "Usage: $0 <command>"
    echo ""
    echo "Commands:"
    echo "  install    Install cron job for 1-minute IBKR data updates"
    echo "  remove     Remove the cron job"
    echo "  status     Show current cron job status and recent activity"
    echo "  logs       Show recent update logs (default: 50 lines)"
    echo "  logs N     Show last N lines of logs"
    echo "  test       Test the update script manually"
    echo "  help       Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 install          # Set up automatic updates every minute"
    echo "  $0 status           # Check if cron job is running"
    echo "  $0 logs 100         # Show last 100 log entries"
    echo "  $0 test             # Run update script manually to test"
    echo ""
}

# Main command processing
case "${1:-help}" in
    install)
        install_cron
        ;;
    remove)
        remove_cron
        ;;
    status)
        show_status
        ;;
    logs)
        show_logs "${2:-50}"
        ;;
    test)
        test_update
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        error "Unknown command: $1"
        echo ""
        show_help
        exit 1
        ;;
esac
