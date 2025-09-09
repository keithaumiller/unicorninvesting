#!/bin/bash

# Setup Data Processing Cron Jobs
# Configures comprehensive data pipeline cron jobs for FRED + BEA + Bronze Layer Processing
#
# This script sets up the following cron schedule:
# - Daily Pipeline (10 PM): Full FRED + BEA + Bronze Layer Processing  
# - Delta Pipeline (every 30 min): Quick updates + Bronze Processing
# - Hourly Processing (hourly): High-frequency bronze datasets
# - Legacy individual connectors (backup/compatibility)

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

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

echo "🚀 UNICORN DATA PROCESSING CRON SETUP"
echo "======================================"

# Ensure scripts are executable
chmod +x "$PROJECT_ROOT/scripts/data_pipeline.sh"
success "Made data pipeline script executable"

# Create logs directory
mkdir -p "$PROJECT_ROOT/logs"
success "Created logs directory"

# Define cron jobs
DAILY_PIPELINE_JOB="0 22 * * * cd $PROJECT_ROOT && $PROJECT_ROOT/scripts/data_pipeline.sh --daily >> $PROJECT_ROOT/logs/daily_pipeline.log 2>&1"
DELTA_PIPELINE_JOB="*/30 * * * * cd $PROJECT_ROOT && $PROJECT_ROOT/scripts/data_pipeline.sh --delta >> $PROJECT_ROOT/logs/delta_pipeline.log 2>&1"
HOURLY_PROCESSING_JOB="0 * * * * cd $PROJECT_ROOT && $PROJECT_ROOT/scripts/data_pipeline.sh --hourly >> $PROJECT_ROOT/logs/hourly_processing.log 2>&1"

# Legacy individual connector jobs (for backup/compatibility)
DAILY_FRED_JOB="0 21 * * * cd $PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred && $PROJECT_ROOT/.venv/bin/python fred_connector.py --daily-update >> $PROJECT_ROOT/logs/fred_daily.log 2>&1"
DELTA_FRED_JOB="*/15 * * * * cd $PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred && $PROJECT_ROOT/.venv/bin/python fred_connector.py --delta-update >> $PROJECT_ROOT/logs/fred_delta.log 2>&1"
DAILY_BEA_JOB="0 6 * * * cd $PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/bureau_of_economic_analysis && $PROJECT_ROOT/.venv/bin/python bea_connector.py --daily-update >> $PROJECT_ROOT/logs/bea_daily.log 2>&1"
DELTA_BEA_JOB="0 */6 * * * cd $PROJECT_ROOT/BackendPython/unicorn/1_data_sources/1_raw/connectors/bureau_of_economic_analysis && $PROJECT_ROOT/.venv/bin/python bea_connector.py --delta-update >> $PROJECT_ROOT/logs/bea_delta.log 2>&1"

info "Setting up comprehensive data pipeline cron jobs..."

# Set up primary data pipeline cron jobs
if ! crontab -l 2>/dev/null | grep -q "data_pipeline.sh --daily"; then
    (crontab -l 2>/dev/null; echo "$DAILY_PIPELINE_JOB") | crontab -
    success "Daily data pipeline cron job added (10 PM daily)"
else
    success "Daily data pipeline cron job already exists"
fi

if ! crontab -l 2>/dev/null | grep -q "data_pipeline.sh --delta"; then
    (crontab -l 2>/dev/null; echo "$DELTA_PIPELINE_JOB") | crontab -
    success "Delta data pipeline cron job added (every 30 minutes)"
else
    success "Delta data pipeline cron job already exists"
fi

if ! crontab -l 2>/dev/null | grep -q "data_pipeline.sh --hourly"; then
    (crontab -l 2>/dev/null; echo "$HOURLY_PROCESSING_JOB") | crontab -
    success "Hourly bronze processing cron job added (every hour)"
else
    success "Hourly bronze processing cron job already exists"
fi

info "Setting up legacy connector cron jobs (backup/compatibility)..."

# Set up legacy individual connector jobs
if ! crontab -l 2>/dev/null | grep -q "fred_connector.py --daily-update"; then
    (crontab -l 2>/dev/null; echo "$DAILY_FRED_JOB") | crontab -
    success "Daily FRED connector cron job added (9 PM daily)"
else
    success "Daily FRED connector cron job already exists"
fi

if ! crontab -l 2>/dev/null | grep -q "fred_connector.py --delta-update"; then
    (crontab -l 2>/dev/null; echo "$DELTA_FRED_JOB") | crontab -
    success "Delta FRED connector cron job added (every 15 minutes)"
else
    success "Delta FRED connector cron job already exists"
fi

if ! crontab -l 2>/dev/null | grep -q "bea_connector.py --daily-update"; then
    (crontab -l 2>/dev/null; echo "$DAILY_BEA_JOB") | crontab -
    success "Daily BEA connector cron job added (6 AM daily)"
else
    success "Daily BEA connector cron job already exists"
fi

if ! crontab -l 2>/dev/null | grep -q "bea_connector.py --delta-update"; then
    (crontab -l 2>/dev/null; echo "$DELTA_BEA_JOB") | crontab -
    success "Delta BEA connector cron job added (every 6 hours)"
else
    success "Delta BEA connector cron job already exists"
fi

# Start cron service if not running
if ! pgrep cron > /dev/null; then
    sudo service cron start
    success "Cron service started"
else
    success "Cron service already running"
fi

echo ""
info "📅 Complete cron schedule configured:"
echo "  🕙 Daily Pipeline (10:00 PM): Full FRED + BEA + Bronze Layer Processing"
echo "  ⚡ Delta Pipeline (every 30 min): Quick updates + Bronze Processing"
echo "  🕐 Hourly Processing (every hour): High-frequency bronze datasets"
echo "  📊 Legacy FRED Daily (9:00 PM): Individual FRED connector backup"
echo "  ⏱️  Legacy FRED Delta (every 15 min): Individual FRED connector backup"
echo "  🌅 Legacy BEA Daily (6:00 AM): Individual BEA connector backup"
echo "  🔄 Legacy BEA Delta (every 6 hours): Individual BEA connector backup"

echo ""
info "🎯 Primary pipeline handles:"
echo "  • Raw data collection (FRED + BEA APIs)"
echo "  • Bronze layer processing (standardized datasets)"
echo "  • Feature engineering (lag, momentum, volatility, regime)"
echo "  • Multi-timeframe processing (1-day, 1-hour intervals)"
echo "  • XGBoost-ready dataset generation"

echo ""
info "📊 Log files location: $PROJECT_ROOT/logs/"
echo "  • daily_pipeline.log: Daily comprehensive processing"
echo "  • delta_pipeline.log: Quick update processing"
echo "  • hourly_processing.log: High-frequency dataset processing"
echo "  • fred_daily.log & fred_delta.log: FRED connector logs"
echo "  • bea_daily.log & bea_delta.log: BEA connector logs"

echo ""
success "✅ Data processing cron jobs setup complete!"
info "Use 'crontab -l' to view all active cron jobs"
info "Use '$PROJECT_ROOT/scripts/data_pipeline.sh --status' to check pipeline status"
