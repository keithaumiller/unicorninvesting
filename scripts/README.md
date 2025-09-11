# Unicorn Investing - Scripts Directory

This directory contains essential scripts for managing the Unicorn Investing platform.

## 🚀 Primary Scripts

### `unicorn_environment.sh`
**The main comprehensive environment and health check script** - Use this for overall system validation!

```bash
# Run full setup and health check
./scripts/unicorn_environment.sh

# Setup environment only
./scripts/unicorn_environment.sh --setup-only

# Health check only
./scripts/unicorn_environment.sh --check-only
```

### `startup_drupal.sh`
**Comprehensive Drupal startup, installation, and validation script** - Use this for Drupal-specific operations!

```bash
# Complete Drupal startup and validation
./scripts/startup_drupal.sh
```

**Features:**
- Automatic Apache and MySQL service management
- GitHub Codespaces port forwarding automation
- Database setup and user configuration
- Drupal installation via Drush or web installer
- Apache virtual host configuration
- Homepage content validation
- Comprehensive troubleshooting guidance

### `export_drupal_baseline.sh`
**Creates exportable baselines of working Drupal installations** - Use this to backup working configurations!

```bash
# Export current Drupal installation as baseline
./scripts/export_drupal_baseline.sh
```

**Creates:**
- Complete database dump
- Drupal configuration files
- Site files and uploads
- Settings and configuration
- Automated restore script
- Compressed archive for distribution

### `data_pipeline.sh`
**Comprehensive data processing pipeline with Yahoo Finance integration** - **UPDATED SEPTEMBER 2025**

```bash
# Run complete daily data pipeline
./scripts/data_pipeline.sh daily

# Run quick delta updates with minute-level asset data
./scripts/data_pipeline.sh delta

# Run hourly asset data collection
./scripts/data_pipeline.sh hourly

# Check pipeline status
./scripts/data_pipeline.sh status

# View recent logs
./scripts/data_pipeline.sh logs [N]
```

**Features:**
- ✅ **Multi-source data collection**: FRED + BEA APIs + Yahoo Finance assets
- ✅ **Yahoo Finance integration**: 9 assets (ETH, BTC, 7 forex pairs) across 3 intervals
- ✅ **Interval management**: 1-minute (delta), 1-hour (hourly), 1-day (daily)
- ✅ **Automated bronze layer processing**: Standardized datasets with feature engineering
- ✅ **End-to-end processing**: Raw data → Bronze layer → Feature engineering
- ✅ **Comprehensive logging**: Pipeline status and error tracking
- ✅ **Cron integration**: Automated scheduling via `setup_data_cron.sh`

**Bronze Layer Processing:**
- **Price Features**: price_change, price_change_abs, hl_range, oc_range
- **Technical Indicators**: RSI, moving averages (10, 20, 50), volatility (14-day, annualized)
- **Volume Analysis**: volume_change, volume_ma_20, volume_ratio
- **Price Position**: high_20, low_20, price_position (relative to recent range)
- **Temporal Features**: hour, day_of_week, day_of_month, month
- **Processing Metadata**: timestamps and validation markers

**Pipeline Integration:**
- **Daily Pipeline**: 7 steps including full bronze processing (all assets)
- **Delta Pipeline**: 6 steps including crypto bronze processing (quick updates)
- **Hourly Pipeline**: 3 steps including bronze processing (real-time features)

**Data Sources:**
- **FRED API**: Economic indicators and macro data
- **BEA API**: Bureau of Economic Analysis datasets  
- **Yahoo Finance**: ETH-USD, BTC-USD, EURUSD, USDJPY, GBPUSD, AUDUSD, USDCAD, USDCHF, NZDUSD

**Pipeline Schedule:**
- **Daily (10 PM)**: Comprehensive collection (1d + 1h intervals) + full bronze processing
- **Delta (every 30min)**: Quick updates + minute-level asset data (1m) + crypto bronze
- **Hourly (every hour)**: High-frequency asset data (1h) + complete bronze processing

### `setup_data_cron.sh`
**Setup comprehensive data pipeline cron jobs** - **UPDATED SEPTEMBER 2025**

```bash
# Setup all cron jobs including Yahoo Finance automation
./scripts/setup_data_cron.sh
```

**Configures:**
- Primary data pipeline jobs (daily, delta, hourly)
- Legacy individual connector jobs (backup/compatibility)
- Yahoo Finance asset collection automation
- Comprehensive logging and monitoring

### `comprehensive_security_audit.sh`
**Comprehensive security audit and credential scanning tool** - Use this to validate security posture!

```bash
# Run full security audit
./scripts/comprehensive_security_audit.sh

# Scan only (no setup guidance)
./scripts/comprehensive_security_audit.sh --scan-only

# Setup credentials interactively
./scripts/comprehensive_security_audit.sh --setup-only

# Verbose output for detailed analysis
./scripts/comprehensive_security_audit.sh --verbose
```

**Features:**
- ✅ **Dynamic pattern loading** from `config/secrets.json`
- ✅ **Credential exposure scanning** across entire codebase
- ✅ **Zero false positives** - uses actual credential values for scanning
- ✅ **Specific migration guidance** based on your configuration
- ✅ **Security configuration validation** 
- ✅ **SecretsManager integration testing**
- ✅ **Safe operation** - no file deletion capabilities
- ✅ **Intelligent reporting** with actionable recommendations

**Scans for:**
- API keys (FRED, BEA, Alpha Vantage)
- Database credentials (development, production, testing)
- IBKR account configuration
- Application secrets and tokens
- Generic credential patterns

**Replaces:** `security_audit.sh`, `security_cleanup.sh`, `migration_helper.py`
- Automated restore script
- Compressed archive for distribution

**Restore Usage:**
```bash
# Extract exported baseline
tar -xzf scripts/drupal_baseline_exports/[export_name].tar.gz

# Restore the baseline
cd scripts/drupal_baseline_exports/[export_name]
./restore_baseline.sh

# Validate restored installation
./scripts/startup_drupal.sh
```
./scripts/unicorn_environment.sh --startup

# Show help
./scripts/unicorn_environment.sh --help
```

**Features:**
- ✅ Environment variables setup
- ✅ Bash aliases configuration
- ✅ Complete system health checks (31 validation checks)
- ✅ Drupal services startup and validation
- ✅ Python environment validation
- ✅ Database connectivity tests
- ✅ Directory structure verification
- ✅ LEAN framework validation
- ✅ Data sources checking
- ✅ **Enhanced IBKR Gateway integration** with:
  - ✅ Automatic gateway startup using codespace configuration
  - ✅ Proper path and configuration validation
  - ✅ Process monitoring and error recovery
  - ✅ Authentication flow integration
  - ✅ Comprehensive troubleshooting guidance
- ✅ Yahoo Finance connector verification
- ✅ Alpha Vantage connector validation

**Available Aliases After Setup:**
- `drupal-start` - Start Drupal services and run full platform validation (integrated)
- `drupal-status` - Check Apache and MySQL status
- `drupal-logs` - View recent Drupal error logs
- `drupal-restart` - Restart Apache and MySQL services
- `drupal-cd` - Change to Drupal root directory
- `unicorn-root` - Change to project root directory
- `unicorn-env` - Run this comprehensive environment script

## 📋 Legacy Scripts (Moved to `legacy/` directory)

**All legacy scripts have been moved to `/workspaces/unicorninvesting/scripts/legacy/`**

### Original Scripts (Now in `legacy/`)
- `health_check.sh` - Original comprehensive health check (317 lines)
- `setup_environment.sh` - Original environment setup (59 lines)
- `health_check_deprecated.sh` - Deprecation wrapper 
- `setup_environment_deprecated.sh` - Deprecation wrapper

**Note:** All functionality from the legacy scripts has been successfully consolidated into `unicorn_environment.sh` with enhanced features and better maintainability.

## 🔧 Individual Scripts

### `startup_drupal.sh`
**Status**: ⚠️ **SUPERSEDED** by `unicorn_environment.sh --startup`

Drupal-specific startup script for web frontend. This functionality has been integrated into the main `unicorn_environment.sh` script for better consolidation.

**Migration**: Use `./scripts/unicorn_environment.sh --startup` instead

## 🔗 Data Source Integrations

### Interactive Brokers (IBKR) Integration
- **Status**: ✅ Fully operational with Client Portal Gateway
- **Authentication**: Manual login required via web interface
- **Gateway URL**: https://${CODESPACE_NAME:-your-codespace}-5000.app.github.dev/ (dynamic)
- **Data Sources**: Real-time and historical cryptocurrency data (ETH validated)
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/`

#### 🔧 IBKR Gateway Best Practices
Based on successful manual testing and integration:

**Configuration:**
- ✅ Use `conf-codespace.yaml` for GitHub Codespaces
- ✅ Execute from tools directory: `cd /path/to/tools && ./bin/run.sh`
- ✅ Required files: `bin/run.sh` and `root/conf-codespace.yaml`

**Startup Process:**
```bash
cd BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/tools
./bin/run.sh root/conf-codespace.yaml
```

**Monitoring:**
- ✅ Gateway logs: `tools/gateway.log`
- ✅ Process monitoring: Look for `iblink.router.clientportal.gw.jar`
- ✅ Health check: `curl http://localhost:5000/v1/api/iserver/auth/status`
- ✅ Authentication: Manual login via external URL

**Security:**
- ✅ All log files are in `.gitignore`
- ✅ No hardcoded credentials in codebase
- ✅ Manual authentication prevents credential exposure

### Yahoo Finance Integration  
- **Status**: ✅ **FULLY OPERATIONAL WITH UNIFIED ASSET COLLECTION** - **UPDATED SEPTEMBER 2025**
- **Authentication**: No authentication required
- **Assets**: 9 total (ETH, BTC, 7 major forex pairs)
- **Intervals**: 1-minute, 1-hour, 1-day data collection
- **Automation**: Integrated with data pipeline and cron scheduling
- **Data Sources**: ETH-USD, BTC-USD, EURUSD, USDJPY, GBPUSD, AUDUSD, USDCAD, USDCHF, NZDUSD
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/`
- **Performance**: 100% success rate, ~85KB per asset per interval, 2 seconds per collection

**Key Files:**
- `unified_asset_collector.py` - Multi-asset collector with organized storage
- `YahooFinanceMinuteData.py` - LEAN framework integration
- `eth_data_collector.py` - Legacy ETH-specific collector (still supported)

**Usage:**
```bash
# Collect all assets for specific interval
python unified_asset_collector.py --all-assets --interval 1h

# View asset summary and configuration
python unified_asset_collector.py --summary
```

**Directory Structure:**
```
yahoo_finance/
├── crypto/ETH/1m/,1h/,1d/    (latest.csv + timestamped files)
├── crypto/BTC/1m/,1h/,1d/
└── forex/[PAIR]/1m/,1h/,1d/  (7 forex pairs, each with all intervals)
```

### Alpha Vantage Integration
- **Status**: ⚠️ Requires API key configuration
- **Authentication**: API key required
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/alpha_vantage/`

## 🎯 Quick Start

1. **First Time Setup:**
   ```bash
   ./scripts/unicorn_environment.sh
   source ~/.bashrc  # Load new aliases
   ```

2. **Regular Health Checks:**
   ```bash
   unicorn-env --check-only
   ```

3. **Navigate to Key Directories:**
   ```bash
   unicorn-root     # Go to project root
   drupal-cd        # Go to Drupal directory
   ```

4. **Drupal Operations:**
   ```bash
   drupal-start     # Start Drupal services
   drupal-status    # Check service status
   drupal-logs      # View error logs
   ```

## 📊 Health Check Results

The health check validates:
- ✅ System requirements (OS, disk, memory)
- ✅ Python environment (Python, Conda, venv)
- ✅ Required libraries (FastAPI, pandas, scikit-learn, etc.)
- ✅ Web services (MySQL, Apache, PHP)
- ✅ Directory structure
- ✅ LEAN framework
- ✅ Data sources (Yahoo Finance connector)

**Success Rates:**
- 100% = All systems operational
- 80%+ = Platform functional with minor issues
- <80% = Significant issues requiring attention

## 🏗️ Migration Notes

The `unicorn_environment.sh` script combines and improves upon:
- All functionality from `health_check.sh`
- All functionality from `setup_environment.sh`
- Additional data warehouse validations
- Better error reporting and suggestions
- Modular execution options

**Migration Benefits:**
- Single script for all environment operations
- Reduced maintenance overhead
- Consistent user experience
- Better error handling and reporting
