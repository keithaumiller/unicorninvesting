# Drupal Baseline Exports Directory

This directory contains exported Drupal baselines created by the `export_drupal_baseline.sh` script.

## Directory Structure

```
drupal_baseline_exports/
├── unicorn_drupal_baseline_[timestamp]/     # Extracted baseline directory
│   ├── database_dump.sql                    # Complete database backup
│   ├── config/                              # Drupal configuration files
│   ├── files/                               # Site files and uploads
│   ├── settings/                            # Settings and configuration files
│   ├── restore_baseline.sh                  # Automated restore script
│   └── README.md                            # Export documentation
└── unicorn_drupal_baseline_[timestamp].tar.gz  # Compressed archive
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
