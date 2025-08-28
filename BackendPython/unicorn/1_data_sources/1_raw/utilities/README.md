# Utilities - Raw Layer Operations

## Purpose
Utility scripts for creating, maintaining, and managing the raw data layer infrastructure and operations.

## Typical Scripts
- **setup_raw_storage.py** - Initialize raw data storage directories and permissions
- **cleanup_old_files.py** - Remove expired raw data files based on retention policies
- **monitor_disk_space.py** - Monitor storage usage and alert when thresholds are reached
- **validate_connector_health.py** - Check status of all data connectors
- **backup_raw_data.py** - Create backups of critical raw data
- **restore_raw_data.py** - Restore raw data from backups
- **test_all_connectors.py** - Comprehensive testing of all data source connectors

## Maintenance Operations
- **Data Retention**: Automatic cleanup of old raw files
- **Storage Management**: Monitor and manage disk space usage
- **Connector Health**: Regular health checks of API connections
- **Backup/Restore**: Data protection and recovery operations
- **Performance Monitoring**: Track ingestion rates and latency

## Usage Pattern
```bash
# Daily maintenance
python utilities/cleanup_old_files.py --days=7
python utilities/monitor_disk_space.py --alert-threshold=80

# Health checks
python utilities/validate_connector_health.py
python utilities/test_all_connectors.py --quick

# Setup operations
python utilities/setup_raw_storage.py --initialize
```

Raw layer utilities focus on:
- Data ingestion infrastructure management
- Storage and retention policies
- Connector monitoring and validation
- Performance optimization
