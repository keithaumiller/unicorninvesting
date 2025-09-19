# Legacy Scripts Directory

This directory contains scripts that have been deprecated and consolidated into the unified `unicorn_environment.sh` script.

## ⚠️ **NEWLY DEPRECATED - September 2025**

### `setup_environment.sh` → **DEPRECATED**
**Use:** `./scripts/unicorn_environment.sh --install-env`
- **Purpose:** Comprehensive environment installation with Python packages and services
- **Migration:** Full functionality integrated into unified script with enhanced error handling

### `setup_data_cron.sh` → **DEPRECATED** 
**Use:** `./scripts/unicorn_environment.sh --data-cron`
- **Purpose:** Data pipeline cron job automation setup for Yahoo Finance + FRED + BEA
- **Migration:** Enhanced with better logging and consolidated interface

## 🚀 **New Unified Approach**

Instead of running multiple scripts, use the single consolidated script:

```bash
# Full environment installation (replaces setup_environment.sh)
./scripts/unicorn_environment.sh --install-env

# Data pipeline automation setup (replaces setup_data_cron.sh)
./scripts/unicorn_environment.sh --data-cron

# Complete startup with health checks
./scripts/unicorn_environment.sh --startup

# Environment setup only
./scripts/unicorn_environment.sh --setup-only

# Health validation only
./scripts/unicorn_environment.sh --check-only
```

## 📜 **Previously Deprecated Scripts**

### `health_check.sh` (Original - 317 lines)
**Purpose:** Comprehensive system health validation
- System requirements checking
- Python environment validation  
- Web server & database status
- Directory structure verification
- Unicorninvesting framework validation
- Functional testing

**Replaced by:** `../unicorn_environment.sh --check-only`

### `setup_environment.sh` (Original - 59 lines)
**Purpose:** Environment setup and bash aliases configuration
- Bash aliases setup for Drupal operations
- Environment variables configuration
- Persistent .bashrc modifications

**Replaced by:** `../unicorn_environment.sh --setup-only`

### `health_check_deprecated.sh` (Wrapper)
**Purpose:** Deprecation wrapper that redirects to new script
- Shows deprecation notice
- Redirects to `unicorn_environment.sh`
- Maintains backward compatibility

### `setup_environment_deprecated.sh` (Wrapper)
**Purpose:** Deprecation wrapper that redirects to new script
- Shows deprecation notice  
- Redirects to `unicorn_environment.sh --setup-only`
- Maintains backward compatibility

## 🔄 Migration Summary

The legacy scripts have been **successfully consolidated** into:
- **Primary Script:** `/workspaces/unicorninvesting/scripts/unicorn_environment.sh`

### Benefits of Consolidation:
✅ **Single Maintenance Point** - One script to maintain instead of two
✅ **Enhanced Functionality** - Combined features from both scripts
✅ **Better Error Handling** - Improved error reporting and suggestions
✅ **Modular Execution** - Can run setup-only, check-only, or both
✅ **Consistent Interface** - Unified command-line options
✅ **Better Documentation** - Comprehensive help and usage information

### Feature Mapping:
- `health_check.sh` functionality → `unicorn_environment.sh --check-only`
- `setup_environment.sh` functionality → `unicorn_environment.sh --setup-only`
- Combined functionality → `unicorn_environment.sh` (default)

## 🚀 Usage Migration

### Old Usage:
```bash
# OLD - Legacy scripts
./scripts/health_check.sh
./scripts/setup_environment.sh
```

### New Usage:
```bash
# NEW - Consolidated script
./scripts/unicorn_environment.sh                # Both setup and health check
./scripts/unicorn_environment.sh --setup-only   # Setup only
./scripts/unicorn_environment.sh --check-only   # Health check only
./scripts/unicorn_environment.sh --help         # Help information

# Or use the alias (after setup):
unicorn-env --check-only
```

## 📊 Script Comparison

| Feature | health_check.sh | setup_environment.sh | unicorn_environment.sh |
|---------|----------------|---------------------|----------------------|
| Health Checks | ✅ | ❌ | ✅ |
| Environment Setup | ❌ | ✅ | ✅ |
| Modular Execution | ❌ | ❌ | ✅ |
| Error Suggestions | ✅ | ❌ | ✅ Enhanced |
| Colored Output | ✅ | Limited | ✅ Enhanced |
| Data Warehouse Checks | ❌ | ❌ | ✅ |
| Lines of Code | 317 | 59 | ~400 (combined) |
| Maintenance | Separate | Separate | Single |

## 🗂️ File Status

- **Archived:** Original scripts moved to legacy directory
- **Maintained:** Deprecation wrappers for backward compatibility
- **Active:** New consolidated script in main scripts directory

All functionality has been preserved and enhanced in the new consolidated script.
