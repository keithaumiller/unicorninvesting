# Drupal Baseline Exports Directory

This directory contains exported Drupal baselines created by the `export_drupal_baseline.sh` script.

## Current Baseline

**🎯 ACTIVE BASELINE:** `unicorn_drupal_baseline_20250905_001311`
- **Export Date:** September 5, 2025
- **Status:** ✅ Fully Working - unicornmetrics module enabled, homepage configured
- **Features:**
  - ✅ unicornmetrics module enabled and working
  - ✅ Homepage set to unicorn dashboard (/unicorn)
  - ✅ All validation constraint errors fixed
  - ✅ Apache restart container-friendly fallback logic
  - ✅ Fresh data validation system implemented
  - ✅ IBKR Gateway integration operational

## Legacy Baselines

**📋 OLD BASELINES:** (kept for reference)
- `old_unicorn_drupal_baseline_20250904_190455` - Initial baseline
- `old_unicorn_drupal_baseline_20250904_190621` - Early configuration

## Directory Structure

```
drupal_baseline_exports/
├── unicorn_drupal_baseline_20250905_001311/     # CURRENT WORKING BASELINE
│   ├── database_dump.sql                         # Complete database backup
│   ├── config/                                   # Drupal configuration files
│   ├── files/                                    # Site files and uploads  
│   ├── settings/                                 # Settings and configuration files
│   ├── restore_baseline.sh                       # Automated restore script
│   └── README.md                                 # Export documentation
├── unicorn_drupal_baseline_20250905_001311.tar.gz  # CURRENT ARCHIVE
└── old_*                                         # Legacy baselines (reference only)
```

## Usage

### Create New Baseline
```bash
cd /workspaces/unicorninvesting
./scripts/export_drupal_baseline.sh
```

### Restore from Baseline
```bash
# Extract baseline
tar -xzf unicorn_drupal_baseline_[timestamp].tar.gz

# Restore installation
cd unicorn_drupal_baseline_[timestamp]
./restore_baseline.sh

# Validate installation
cd /workspaces/unicorninvesting
./scripts/startup_drupal.sh
```

## Use Cases

- **Fresh Environment Setup**: Quickly restore a working Drupal installation
- **Development Reset**: Return to a clean baseline state
- **Deployment**: Package working configuration for other environments
- **Backup**: Archive working states before major changes

Each baseline is timestamped and self-contained with its own restore script.
