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

# Show help
./scripts/unicorn_environment.sh --help
```

**Features:**
- ✅ Environment variables setup
- ✅ Bash aliases configuration
- ✅ Complete system health checks
- ✅ Python environment validation
- ✅ Database connectivity tests
- ✅ Directory structure verification
- ✅ LEAN framework validation
- ✅ Data sources checking

**Available Aliases After Setup:**
- `drupal-start` - Start and validate Drupal system
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

## 🔧 Other Scripts

### `startup_drupal.sh`
Drupal-specific startup script for web frontend

### `setup_ibkr_env.sh`
Interactive Brokers (IBKR) environment setup

### `setup_lean_ibkr.sh`
LEAN framework integration with IBKR

### `test_ibkr_connection.sh`
Test IBKR connectivity

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
