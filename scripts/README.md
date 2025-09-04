# Unicorn Investing - Scripts Directory

This directory contains essential scripts for managing the Unicorn Investing platform.

## 🚀 Primary Script

### `unicorn_environment.sh`
**The main comprehensive environment and health check script** - Use this one for all operations!

```bash
# Run full setup and health check
./scripts/unicorn_environment.sh

# Setup environment only
./scripts/unicorn_environment.sh --setup-only

# Health check only
./scripts/unicorn_environment.sh --check-only

# Start Drupal services and run full validation
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
- **Status**: ✅ Operational
- **Authentication**: No authentication required
- **Data Sources**: Historical and real-time market data
- **Location**: `/BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/`

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
