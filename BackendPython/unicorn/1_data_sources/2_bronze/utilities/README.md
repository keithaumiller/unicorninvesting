# Utilities - Bronze Layer Operations

## Purpose
Utility scripts for creating, maintaining, and managing the bronze data layer infrastructure and data operations.

## Typical Scripts
- **setup_bronze_tables.py** - Create and initialize bronze layer database tables/schemas
- **repair_corrupted_data.py** - Identify and repair data corruption issues
- **update_reference_data.py** - Update exchange lists, currency pairs, stock symbols
- **compact_bronze_storage.py** - Optimize storage and remove duplicates
- **bronze_data_lineage.py** - Track data lineage from raw to bronze
- **performance_tuning.py** - Optimize bronze layer query and storage performance

**Note**: Validation scripts have been moved to `/tests/unicorn/1_data_sources/` for centralized test management.

## Reference Data Management
- **update_forex_pairs.py** - Refresh forex pair definitions
- **sync_stock_exchanges.py** - Update stock exchange listings (NYSE, NASDAQ, AMEX)
- **currency_master_update.py** - Maintain currency reference data
- **feature_list_maintenance.py** - Manage master feature definitions

## Quality Control
- **data_validation_suite.py** - Run comprehensive data quality checks
- **duplicate_detection.py** - Find and resolve duplicate records
- **schema_compliance.py** - Ensure data conforms to bronze layer schemas
- **integrity_checks.py** - Validate referential integrity

## Usage Pattern
```bash
# Setup operations
python utilities/setup_bronze_tables.py --create-indexes
python utilities/update_reference_data.py --all

# Daily maintenance
python utilities/compact_bronze_storage.py --optimize

# Quality checks and validation (centralized location)
cd /workspaces/unicorninvesting
python -m pytest tests/unicorn/1_data_sources/ --verbose
```

Bronze layer utilities focus on:
- Structured data storage setup
- Reference data maintenance
- Data quality and validation
- Performance optimization
