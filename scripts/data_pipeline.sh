#!/bin/bash

# Unicorn Data Processing Pipeline
# Orchestrates raw data collection and bronze layer processing
#
# This script:
# 1. Collects raw data from FRED and BEA APIs
# 2. Processes raw data into bronze layer standardized datasets
# 3. Logs all operations for monitoring
# 4. Handles errors gracefully with proper notifications

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
LOG_FILE="$PROJECT_ROOT/logs/data_pipeline.log"
PROCESSING_DIR="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/2_bronze/economic_indicators"

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

# Logging function
log_message() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
    
    case "$level" in
        "SUCCESS") success "$message" ;;
        "WARNING") warning "$message" ;;
        "ERROR") error "$message" ;;
        *) info "$message" ;;
    esac
}

# Check if virtual environment is activated
check_venv() {
    if [[ -z "$VIRTUAL_ENV" ]]; then
        log_message "INFO" "Activating virtual environment..."
        source "$PROJECT_ROOT/.venv/bin/activate"
        if [[ $? -ne 0 ]]; then
            log_message "ERROR" "Failed to activate virtual environment"
            return 1
        fi
    fi
    return 0
}

# Collect FRED data
collect_fred_data() {
    local update_type="$1"
    local fred_dir="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred"
    
    log_message "INFO" "🏦 Collecting FRED data ($update_type)..."
    
    cd "$fred_dir" || {
        log_message "ERROR" "Failed to change to FRED directory"
        return 1
    }
    
    if [[ "$update_type" == "daily" ]]; then
        python3 fred_connector.py --daily-update
    else
        python3 fred_connector.py --delta-update
    fi
    
    local exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        log_message "SUCCESS" "FRED data collection completed successfully"
        return 0
    else
        log_message "ERROR" "FRED data collection failed (exit code: $exit_code)"
        return 1
    fi
}

# Collect BEA data
collect_bea_data() {
    local update_type="$1"
    local bea_dir="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/bureau_of_economic_analysis"
    
    log_message "INFO" "🏛️ Collecting BEA data ($update_type)..."
    
    cd "$bea_dir" || {
        log_message "ERROR" "Failed to change to BEA directory"
        return 1
    }
    
    if [[ "$update_type" == "daily" ]]; then
        python3 bea_connector.py --daily-update
    else
        python3 bea_connector.py --delta-update
    fi
    
    local exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        log_message "SUCCESS" "BEA data collection completed successfully"
        return 0
    else
        log_message "ERROR" "BEA data collection failed (exit code: $exit_code)"
        return 1
    fi
}

# Collect Yahoo Finance asset data (ETH, Forex, etc.)
collect_yahoo_finance_data() {
    local interval="$1"
    local yahoo_dir="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance"
    
    log_message "INFO" "📊 Collecting Yahoo Finance asset data (${interval} interval)..."
    
    cd "$yahoo_dir" || {
        log_message "ERROR" "Failed to change to Yahoo Finance directory"
        return 1
    }
    
    # Collect all assets for the specified interval using unified collector
    python3 unified_asset_collector.py --all-assets --interval "$interval"
    
    local exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        log_message "SUCCESS" "Yahoo Finance data collection completed successfully ($interval)"
        return 0
    else
        log_message "ERROR" "Yahoo Finance data collection failed (exit code: $exit_code, interval: $interval)"
        return 1
    fi
}

# Process bronze layer economic indicators
process_bronze_layer() {
    local intervals="$1"
    
    log_message "INFO" "⚡ Processing bronze layer economic indicators ($intervals)..."
    
    cd "$PROCESSING_DIR" || {
        log_message "ERROR" "Failed to change to processing directory"
        return 1
    }
    
    python3 process_indicators.py --intervals "$intervals"
    
    local exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        log_message "SUCCESS" "Bronze layer processing completed successfully"
        return 0
    else
        log_message "ERROR" "Bronze layer processing failed (exit code: $exit_code)"
        return 1
    fi
}

# Process Yahoo Finance assets in bronze layer
process_yahoo_finance_bronze() {
    local categories="$1"  # crypto, forex, or "all"
    local yahoo_bronze_dir="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/2_bronze/yahoo_finance_assets"
    
    log_message "INFO" "⚡ Processing Yahoo Finance assets in bronze layer (${categories})..."
    
    cd "$yahoo_bronze_dir" || {
        log_message "ERROR" "Failed to change to Yahoo Finance bronze directory"
        return 1
    }
    
    if [[ "$categories" == "all" ]]; then
        python3 process_assets.py
    else
        python3 process_assets.py --category "$categories"
    fi
    
    local exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        log_message "SUCCESS" "Yahoo Finance bronze layer processing completed successfully ($categories)"
        return 0
    else
        log_message "ERROR" "Yahoo Finance bronze layer processing failed (exit code: $exit_code, categories: $categories)"
        return 1
    fi
}

# Comprehensive daily pipeline
run_daily_pipeline() {
    log_message "INFO" "🚀 Starting comprehensive daily data pipeline..."
    echo "🚀 UNICORN DAILY DATA PIPELINE - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "=============================================="
    
    local success_count=0
    local total_steps=7
    
    # Step 1: Check virtual environment
    if check_venv; then
        ((success_count++))
    fi
    
    # Step 2: Collect FRED daily data
    if collect_fred_data "daily"; then
        ((success_count++))
    fi
    
    # Step 3: Collect BEA daily data
    if collect_bea_data "daily"; then
        ((success_count++))
    fi
    
    # Step 4: Collect Yahoo Finance daily data (ETH, Forex)
    if collect_yahoo_finance_data "1d"; then
        ((success_count++))
    fi
    
    # Step 5: Collect Yahoo Finance hourly data (ETH, Forex)
    if collect_yahoo_finance_data "1h"; then
        ((success_count++))
    fi
    
    # Step 6: Process bronze layer (economic indicators)
    if process_bronze_layer "1_day"; then
        ((success_count++))
    fi
    
    # Step 7: Process Yahoo Finance bronze layer (crypto + forex assets)
    if process_yahoo_finance_bronze "all"; then
        ((success_count++))
    fi
    
    # Pipeline summary
    echo ""
    echo "📊 PIPELINE SUMMARY"
    echo "==================="
    echo "Steps completed: $success_count/$total_steps"
    echo "Success rate: $(( success_count * 100 / total_steps ))%"
    
    if [[ $success_count -eq $total_steps ]]; then
        log_message "SUCCESS" "Daily pipeline completed successfully ($success_count/$total_steps steps)"
        return 0
    else
        log_message "WARNING" "Daily pipeline completed with issues ($success_count/$total_steps steps)"
        return 1
    fi
}

# Delta (quick) pipeline
run_delta_pipeline() {
    log_message "INFO" "⚡ Starting delta data pipeline..."
    echo "⚡ UNICORN DELTA DATA PIPELINE - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "==========================================="
    
    local success_count=0
    local total_steps=6
    
    # Step 1: Check virtual environment
    if check_venv; then
        ((success_count++))
    fi
    
    # Step 2: Collect FRED delta data
    if collect_fred_data "delta"; then
        ((success_count++))
    fi
    
    # Step 3: Collect BEA delta data
    if collect_bea_data "delta"; then
        ((success_count++))
    fi
    
    # Step 4: Collect Yahoo Finance minute data (ETH, Forex - high frequency)
    if collect_yahoo_finance_data "1m"; then
        ((success_count++))
    fi
    
    # Step 5: Process bronze layer (daily intervals, quick update)
    if process_bronze_layer "1_day"; then
        ((success_count++))
    fi
    
    # Step 6: Process Yahoo Finance bronze layer (crypto only, quick update)
    if process_yahoo_finance_bronze "crypto"; then
        ((success_count++))
    fi
    
    # Pipeline summary
    echo ""
    echo "📊 PIPELINE SUMMARY"
    echo "==================="
    echo "Steps completed: $success_count/$total_steps"
    echo "Success rate: $(( success_count * 100 / total_steps ))%"
    
    if [[ $success_count -eq $total_steps ]]; then
        log_message "SUCCESS" "Delta pipeline completed successfully ($success_count/$total_steps steps)"
        return 0
    else
        log_message "WARNING" "Delta pipeline completed with issues ($success_count/$total_steps steps)"
        return 1
    fi
}

# High-frequency processing (1-hour intervals)
run_hourly_processing() {
    log_message "INFO" "🕐 Starting hourly processing pipeline..."
    echo "🕐 UNICORN HOURLY PROCESSING - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "======================================="
    
    local success_count=0
    local total_steps=3
    
    # Step 1: Check virtual environment
    if check_venv; then
        ((success_count++))
    fi
    
    # Step 2: Collect Yahoo Finance hourly data (ETH, Forex)
    if collect_yahoo_finance_data "1h"; then
        ((success_count++))
    fi
    
    # Step 3: Process Yahoo Finance bronze layer (all assets)
    if process_yahoo_finance_bronze "all"; then
        ((success_count++))
    fi
    
    # Pipeline summary
    echo ""
    echo "📊 HOURLY PROCESSING SUMMARY"
    echo "============================"
    echo "Steps completed: $success_count/$total_steps"
    echo "Success rate: $(( success_count * 100 / total_steps ))%"
    
    if [[ $success_count -eq $total_steps ]]; then
        log_message "SUCCESS" "Hourly processing completed successfully ($success_count/$total_steps steps)"
        return 0
    else
        log_message "WARNING" "Hourly processing completed with issues ($success_count/$total_steps steps)"
        return 1
    fi
}

# Show pipeline status
show_status() {
    echo "📊 UNICORN DATA PIPELINE STATUS"
    echo "================================"
    
    # Check virtual environment
    if [[ -n "$VIRTUAL_ENV" ]]; then
        success "Virtual environment: Active ($VIRTUAL_ENV)"
    else
        warning "Virtual environment: Not active"
    fi
    
    # Check cron service
    if pgrep cron > /dev/null; then
        success "Cron service: Running"
    else
        warning "Cron service: Not running"
    fi
    
    # Check log file
    if [[ -f "$LOG_FILE" ]]; then
        local log_lines=$(wc -l < "$LOG_FILE")
        local last_run=$(tail -1 "$LOG_FILE" 2>/dev/null | cut -d']' -f1 | tr -d '[]')
        success "Log file: $log_lines entries"
        info "Last activity: $last_run"
    else
        warning "Log file: Not found"
    fi
    
    # Check data directories
    local raw_data_dir="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/data/economic_indicators"
    local bronze_data_dir="$PROJECT_ROOT/BackendPython/unicorn/1_data_sources/2_bronze/economic_indicators/processed_data"
    
    if [[ -d "$raw_data_dir" ]]; then
        local raw_files=$(find "$raw_data_dir" -name "*.csv" 2>/dev/null | wc -l)
        success "Raw data files: $raw_files CSV files"
    else
        warning "Raw data directory: Not found"
    fi
    
    if [[ -d "$bronze_data_dir" ]]; then
        local bronze_files=$(find "$bronze_data_dir" -name "*.csv" 2>/dev/null | wc -l)
        success "Bronze layer files: $bronze_files CSV files"
    else
        warning "Bronze data directory: Not found"
    fi
    
    echo ""
    echo "🕒 Current cron jobs:"
    crontab -l 2>/dev/null | grep -E "(fred_connector|bea_connector|data_pipeline)" || echo "   No data pipeline cron jobs found"
}

# Show recent logs
show_logs() {
    local lines=${1:-20}
    
    echo "📜 Recent Pipeline Logs (last $lines lines)"
    echo "=========================================="
    
    if [[ -f "$LOG_FILE" ]]; then
        tail -n "$lines" "$LOG_FILE"
    else
        warning "Log file not found: $LOG_FILE"
    fi
}

# Main script logic
main() {
    # Create logs directory if it doesn't exist
    mkdir -p "$(dirname "$LOG_FILE")"
    
    case "${1:-}" in
        "daily")
            run_daily_pipeline
            ;;
        "delta")
            run_delta_pipeline
            ;;
        "hourly")
            run_hourly_processing
            ;;
        "status")
            show_status
            ;;
        "logs")
            show_logs "${2:-20}"
            ;;
        "help"|"-h"|"--help")
            echo "Unicorn Data Processing Pipeline"
            echo "================================"
            echo ""
            echo "USAGE:"
            echo "  $0 [COMMAND] [OPTIONS]"
            echo ""
            echo "COMMANDS:"
            echo "  daily           Run full daily pipeline (FRED + BEA + Yahoo Finance + bronze processing)"
            echo "  delta           Run delta pipeline (quick updates + minute-level asset data + crypto bronze)"
            echo "  hourly          Process hourly asset data collection (ETH, Forex + bronze processing)"
            echo "  status          Show pipeline and system status"
            echo "  logs [N]        Show recent N log entries (default: 20)"
            echo "  help            Show this help message"
            echo ""
            echo "DATA SOURCES:"
            echo "  • FRED API: Economic indicators and macro data"
            echo "  • BEA API: Bureau of Economic Analysis datasets"
            echo "  • Yahoo Finance: ETH, Forex (EURUSD, USDJPY, GBPUSD, AUDUSD, etc.)"
            echo "  • Intervals: 1-minute (delta), 1-hour (hourly), 1-day (daily)"
            echo ""
            echo "EXAMPLES:"
            echo "  $0 daily        # Run complete daily data pipeline"
            echo "  $0 delta        # Run quick delta updates (includes 1m asset data)"
            echo "  $0 hourly       # Collect hourly ETH and Forex data"
            echo "  $0 status       # Check system status"
            echo "  $0 logs 50      # Show last 50 log entries"
            echo ""
            echo "AUTOMATION:"
            echo "  This script is designed to be run via cron jobs:"
            echo "  • Daily pipeline: Comprehensive data collection (1d, 1h intervals) + full bronze processing"
            echo "  • Delta pipeline: Quick updates + minute-level asset data (1m interval) + crypto bronze"
            echo "  • Hourly processing: High-frequency asset data collection (1h interval) + bronze processing"
            ;;
        *)
            error "Invalid command: ${1:-}"
            echo "Use '$0 help' for usage information"
            exit 1
            ;;
    esac
}

# Run main function
main "$@"
