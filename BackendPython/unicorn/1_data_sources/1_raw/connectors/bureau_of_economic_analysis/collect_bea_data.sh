#!/bin/bash
#
# BEA Data Collection Script
#
# This script facilitates automated collection of Bureau of Economic Analysis data
# for the Unicorn Investing platform. It provides wrapper functionality for
# the BEAConnector Python module with logging and error handling.
#
# Author: Unicorn Investing Platform
# Version: 1.0.0
# Status: Production Ready
#

# Set script directory and paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BEA_CONNECTOR_DIR="$SCRIPT_DIR"
PYTHON_CMD="python3"
LOG_DIR="/workspaces/unicorninvesting/logs"
DATA_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/economic_indicators/bea"

# Create directories if they don't exist
mkdir -p "$LOG_DIR"
mkdir -p "$DATA_DIR"

# Function to log messages with timestamp
log_message() {
    local level=$1
    local message=$2
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message"
    echo "[$timestamp] [$level] $message" >> "$LOG_DIR/bea_collection.log"
}

# Function to check BEA API key
check_api_key() {
    if [[ -z "$BEA_API_KEY" ]]; then
        log_message "ERROR" "BEA_API_KEY environment variable not set"
        echo "❌ BEA API key required!"
        echo ""
        echo "Setup Instructions:"
        echo "1. Get free BEA API key: https://apps.bea.gov/API/signup/"
        echo "2. Set environment variable:"
        echo "   export BEA_API_KEY='your_key_here'"
        echo "3. Add to your ~/.bashrc for persistence:"
        echo "   echo 'export BEA_API_KEY=\"your_key_here\"' >> ~/.bashrc"
        echo ""
        return 1
    fi
    return 0
}

# Function to run BEA connector with error handling
run_bea_connector() {
    local command=$1
    local log_suffix=$2
    local timestamp=$(date '+%Y%m%d_%H%M%S')
    local log_file="$LOG_DIR/bea_${log_suffix}_${timestamp}.log"
    
    log_message "INFO" "Starting BEA data collection: $command"
    log_message "INFO" "Log file: $log_file"
    
    cd "$BEA_CONNECTOR_DIR" || {
        log_message "ERROR" "Could not change to BEA connector directory: $BEA_CONNECTOR_DIR"
        return 1
    }
    
    # Run the Python connector with logging
    if $PYTHON_CMD bea_connector.py $command 2>&1 | tee "$log_file"; then
        log_message "SUCCESS" "BEA data collection completed: $command"
        return 0
    else
        log_message "ERROR" "BEA data collection failed: $command"
        return 1
    fi
}

# Function for comprehensive historical data collection
collect_comprehensive() {
    log_message "INFO" "🏛️ Starting comprehensive BEA historical data collection"
    echo "⚠️  COMPREHENSIVE DATA COLLECTION"
    echo "📅 This will collect 20+ years of BEA economic data"
    echo "⏱️  Estimated time: 5-10 minutes with 2-second throttling"
    echo "💾 Data will be saved to: $DATA_DIR"
    echo ""
    
    # Check if running interactively
    if [[ -t 0 ]]; then
        read -p "Proceed with comprehensive collection? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_message "INFO" "Comprehensive collection cancelled by user"
            echo "❌ Collection cancelled"
            return 0
        fi
    fi
    
    run_bea_connector "--comprehensive" "comprehensive"
}

# Function for daily data update
collect_daily() {
    log_message "INFO" "📅 Starting daily BEA data update"
    echo "📅 DAILY BEA DATA UPDATE - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "📊 Collecting all important series (last 5 years)"
    echo "⏱️  Estimated time: 2-3 minutes with 1-second throttling"
    echo ""
    
    run_bea_connector "--daily-update" "daily"
}

# Function for delta data update (quick update)
collect_delta() {
    log_message "INFO" "⚡ Starting delta BEA data update"
    echo "⚡ DELTA BEA DATA UPDATE - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "📊 Collecting critical series only (last 2 years)"
    echo "⏱️  Estimated time: 30-60 seconds"
    echo ""
    
    run_bea_connector "--delta-update" "delta"
}

# Function for test run
test_connection() {
    log_message "INFO" "🧪 Testing BEA connector"
    echo "🧪 TESTING BEA CONNECTION"
    echo "📊 This will test the BEA API connection and collect sample data"
    echo ""
    
    run_bea_connector "--test" "test"
}

# Function to check system status
check_status() {
    echo "🔍 BEA DATA COLLECTION STATUS"
    echo "================================"
    echo ""
    
    # Check API key
    if check_api_key; then
        echo "✅ BEA API Key: Configured"
    else
        echo "❌ BEA API Key: Not configured"
    fi
    
    # Check Python environment
    if command -v $PYTHON_CMD &> /dev/null; then
        echo "✅ Python: $($PYTHON_CMD --version)"
    else
        echo "❌ Python: Not found"
    fi
    
    # Check required libraries
    echo ""
    echo "📦 Python Library Status:"
    
    libraries=("pandas" "numpy" "beaapi")
    for lib in "${libraries[@]}"; do
        if $PYTHON_CMD -c "import $lib" 2>/dev/null; then
            version=$($PYTHON_CMD -c "import $lib; print(getattr($lib, '__version__', 'unknown'))" 2>/dev/null)
            echo "   ✅ $lib: $version"
        else
            echo "   ❌ $lib: Not installed"
        fi
    done
    
    # Check data directory
    echo ""
    echo "📁 Directory Status:"
    echo "   Data directory: $DATA_DIR"
    if [[ -d "$DATA_DIR" ]]; then
        file_count=$(find "$DATA_DIR" -name "*.csv" 2>/dev/null | wc -l)
        echo "   ✅ Directory exists ($file_count CSV files)"
    else
        echo "   ⚠️  Directory will be created on first run"
    fi
    
    echo "   Log directory: $LOG_DIR"
    if [[ -d "$LOG_DIR" ]]; then
        log_count=$(find "$LOG_DIR" -name "bea_*.log" 2>/dev/null | wc -l)
        echo "   ✅ Directory exists ($log_count log files)"
    else
        echo "   ⚠️  Directory will be created on first run"
    fi
    
    # Check recent data files
    echo ""
    echo "📊 Recent BEA Data Files:"
    if [[ -d "$DATA_DIR" ]]; then
        recent_files=$(find "$DATA_DIR" -name "*.csv" -mtime -7 2>/dev/null | sort -r | head -5)
        if [[ -n "$recent_files" ]]; then
            echo "$recent_files" | while read -r file; do
                filename=$(basename "$file")
                filesize=$(ls -lh "$file" | awk '{print $5}')
                filedate=$(ls -l "$file" | awk '{print $6, $7, $8}')
                echo "   📄 $filename ($filesize, $filedate)"
            done
        else
            echo "   📄 No recent CSV files found"
        fi
    fi
}

# Function to show usage
show_usage() {
    echo "🏛️ BEA Data Collection Script"
    echo "============================="
    echo ""
    echo "USAGE:"
    echo "  $0 [command]"
    echo ""
    echo "COMMANDS:"
    echo "  comprehensive    Collect complete historical data (20+ years)"
    echo "  daily           Daily update of all important series (5 years)"
    echo "  delta           Quick update of critical series (2 years)"  
    echo "  test            Test BEA API connection"
    echo "  status          Check system and data status"
    echo "  help            Show this help message"
    echo ""
    echo "EXAMPLES:"
    echo "  $0 test              # Test API connection"
    echo "  $0 delta             # Quick critical data update"
    echo "  $0 daily             # Full daily data update"
    echo "  $0 comprehensive     # Complete historical collection"
    echo ""
    echo "AUTOMATION:"
    echo "  Add to cron for automated data collection:"
    echo "  # Delta updates every 6 hours"
    echo "  0 */6 * * * $0 delta"
    echo "  # Daily updates at 6 AM"
    echo "  0 6 * * * $0 daily"
    echo ""
    echo "SETUP:"
    echo "  1. Get BEA API key: https://apps.bea.gov/API/signup/"
    echo "  2. Set environment variable: export BEA_API_KEY='your_key'"
    echo "  3. Install dependencies: pip install pandas numpy beaapi"
    echo ""
}

# Main script logic
main() {
    local command=${1:-help}
    
    log_message "INFO" "BEA collection script started with command: $command"
    
    case $command in
        "comprehensive")
            if check_api_key; then
                collect_comprehensive
            fi
            ;;
        "daily")
            if check_api_key; then
                collect_daily
            fi
            ;;
        "delta")
            if check_api_key; then
                collect_delta
            fi
            ;;
        "test")
            if check_api_key; then
                test_connection
            fi
            ;;
        "status")
            check_status
            ;;
        "help"|"-h"|"--help")
            show_usage
            ;;
        *)
            echo "❌ Unknown command: $command"
            echo ""
            show_usage
            exit 1
            ;;
    esac
    
    log_message "INFO" "BEA collection script completed"
}

# Run main function with all arguments
main "$@"
