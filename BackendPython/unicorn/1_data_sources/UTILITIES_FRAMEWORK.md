# Data Warehouse Utilities Framework 🛠️

## Overview
Each data warehouse layer now includes a dedicated `utilities/` directory containing layer-specific maintenance, setup, and operational scripts.

## 📁 Utilities Structure

```
1_data_sources/
├── 1_raw/utilities/         # Raw layer operations
├── 2_bronze/utilities/      # Bronze layer maintenance  
├── 3_silver/utilities/      # Silver layer data quality
├── 4_gold/utilities/        # Gold layer analytics
├── 5_data_marts/utilities/  # Data mart optimization
└── 8_metadata/utilities/    # Governance & monitoring
```

## 🎯 Purpose by Layer

### 1_raw/utilities/ - Data Ingestion Operations
- **Storage Management**: Disk space monitoring, cleanup policies
- **Connector Health**: API status checks, connection validation
- **Backup/Restore**: Raw data protection and recovery
- **Performance**: Ingestion rate monitoring, latency tracking

### 2_bronze/utilities/ - Structured Data Foundation  
- **Schema Setup**: Database table creation, index management
- **Reference Data**: Exchange listings, currency pairs, stock symbols
- **Quality Control**: Data validation, duplicate detection
- **Integrity**: Referential integrity checks, constraint validation

### 3_silver/utilities/ - Data Quality & Cleansing
- **Data Cleaning**: Outlier detection, missing value imputation
- **Normalization**: Price adjustments, timestamp standardization
- **Enrichment**: Calculated fields, derived metrics
- **Quality Metrics**: Data quality scoring, coverage analysis

### 4_gold/utilities/ - Business Analytics
- **Trading Indicators**: Technical analysis calculations (RSI, MACD, Bollinger Bands)
- **Performance Metrics**: Portfolio returns, risk measures, benchmarking
- **Business Rules**: Trading logic, signal generation
- **Optimization**: Query performance for analytical workloads

### 5_data_marts/utilities/ - Subject-Specific Operations
- **Mart Creation**: Forex, equity, crypto, performance, risk data marts
- **Access Management**: User permissions, data access patterns
- **Optimization**: Index tuning, query performance, caching
- **Reporting**: Custom views, client-specific datasets

### 8_metadata/utilities/ - Governance & Compliance
- **Data Lineage**: Track data flow across all layers
- **Quality Monitoring**: Dashboard creation, alerting, trend analysis
- **Schema Management**: Version control, compatibility checking
- **Compliance**: Regulatory reporting, audit trails, privacy controls

## 🚀 Operational Benefits

### **Separation of Concerns**
- **Processing**: `transformscripts/` handle data transformation
- **Operations**: `utilities/` handle maintenance and management
- **Integration**: `connectors/` handle data ingestion

### **Layer-Specific Expertise**
- Each layer has utilities tailored to its specific function
- Raw layer focuses on ingestion reliability
- Bronze layer focuses on data structure
- Silver layer focuses on quality
- Gold layer focuses on business value
- Data marts focus on user experience
- Metadata layer focuses on governance

### **Comprehensive Coverage**
- **Setup & Initialization**: Get layers operational quickly
- **Daily Operations**: Automated maintenance tasks
- **Monitoring & Alerting**: Proactive issue detection
- **Performance Optimization**: Keep systems running efficiently
- **Quality Assurance**: Maintain data integrity
- **Compliance**: Meet regulatory requirements

## 💡 Usage Patterns

### Setup Operations
```bash
# Initialize layer infrastructure
python utilities/setup_raw_storage.py --initialize
python utilities/setup_bronze_tables.py --create-indexes
python utilities/setup_silver_schemas.py --with-partitions
```

### Daily Maintenance
```bash
# Automated daily tasks
python utilities/cleanup_old_files.py --retention-days=7
python utilities/validate_bronze_data.py --incremental
python utilities/data_cleansing_suite.py --auto-fix
```

### Monitoring & Health Checks
```bash
# System health validation
python utilities/validate_connector_health.py --all
python utilities/silver_quality_metrics.py --dashboard
python utilities/data_lineage_tracker.py --update
```

### Performance Optimization
```bash
# Performance tuning
python utilities/compact_bronze_storage.py --optimize
python utilities/gold_performance_optimizer.py --analyze
python utilities/mart_indexing_optimizer.py --recommend
```

## 🎉 Complete Operational Framework

The utilities framework provides a comprehensive operational foundation for:

- ✅ **Infrastructure Management** - Setup, configuration, maintenance
- ✅ **Data Quality Assurance** - Validation, cleaning, monitoring  
- ✅ **Performance Optimization** - Tuning, indexing, caching
- ✅ **Governance & Compliance** - Lineage, auditing, reporting
- ✅ **User Experience** - Access management, custom views
- ✅ **System Reliability** - Health checks, backup/recovery

Each layer now has dedicated operational capabilities matching its specific responsibilities in the data warehouse architecture! 🦄✨
