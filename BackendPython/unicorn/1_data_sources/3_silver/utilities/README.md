# Utilities - Silver Layer Operations

## Purpose
Utility scripts for creating, maintaining, and managing the silver data layer with cleaned, normalized datasets.

## Typical Scripts
- **setup_silver_schemas.py** - Create silver layer normalized database schemas
- **data_cleansing_suite.py** - Comprehensive data cleaning and normalization
- **outlier_detection.py** - Identify and handle statistical outliers
- **time_series_alignment.py** - Standardize timestamps across data sources
- **missing_data_imputation.py** - Handle missing values with appropriate strategies
- **data_enrichment.py** - Add calculated fields and derived metrics
- **silver_quality_metrics.py** - Calculate and monitor data quality scores

## Data Cleaning Operations
- **price_data_normalizer.py** - Standardize price formats and adjust for splits/dividends
- **volume_data_cleaner.py** - Clean and validate volume data
- **timestamp_standardizer.py** - Ensure consistent timezone and format handling
- **currency_converter.py** - Convert prices to standard base currencies
- **corporate_actions_adjuster.py** - Adjust historical data for corporate actions

## Validation & Monitoring
- **data_consistency_checker.py** - Verify data consistency across silver tables
- **anomaly_detector.py** - Detect unusual patterns in cleaned data
- **coverage_analyzer.py** - Analyze data coverage and completeness
- **drift_detector.py** - Monitor for data distribution changes over time

## Usage Pattern
```bash
# Setup operations
python utilities/setup_silver_schemas.py --with-indexes
python utilities/data_enrichment.py --calculate-indicators

# Daily processing
python utilities/data_cleansing_suite.py --incremental
python utilities/outlier_detection.py --auto-fix

# Quality monitoring
python utilities/silver_quality_metrics.py --dashboard
python utilities/data_consistency_checker.py --report
```

Silver layer utilities focus on:
- Data cleaning and normalization
- Quality improvement processes
- Statistical analysis and outlier detection
- Time series data standardization
