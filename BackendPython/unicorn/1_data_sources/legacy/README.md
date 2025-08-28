# 📚 Legacy Data Sources Archive

## Overview
This directory contains historical R scripts, legacy data files, and deprecated analysis results that have been migrated from the original unicorn investing platform. The structure mirrors the modern data warehouse architecture to preserve context.

## 🏗️ Legacy Directory Structure

```
legacy/
├── 1_raw/                  # Legacy raw data staging files
├── 2_bronze/               # Historical analysis results and deprecated data
│   ├── old/               # Deprecated prediction models and analysis
│   └── results/           # Neural network and genetic algorithm results
├── 3_silver/              # Legacy cleaned data (if any)
├── 4_gold/                # Legacy aggregated data (if any)
├── 5_data_marts/          # Legacy data marts (if any)
├── 6_etl_pipelines/       # Original R-based ETL scripts and legacy data
│   ├── data/             # Historical stock data samples
│   ├── Combinestocks.R   # Legacy R script for combining stock data
│   ├── Generatefeatureslist.R  # Legacy feature generation
│   ├── downloadstockdata.R     # Legacy data download script
│   ├── generatetrainingoutput.R # Legacy training data generation
│   ├── mysqlconnector.R  # Legacy MySQL integration
│   └── debugframework.R  # Legacy debugging utilities
├── 7_connectors/          # Legacy data connectors (if any)
├── 8_metadata/            # Legacy metadata and schemas
├── README.md              # This documentation
├── Dump20170424.sql       # Historical database schema
├── Dump20170529.sql       # Updated historical database schema
└── PRODUCTION_SETUP.md    # Legacy production setup guide
```
Database schemas provide the **structural foundation** for all data storage and processing.

## Current Files

### Dump20170424.sql
**Purpose**: MySQL database dump from April 24, 2017
**Contents**: Complete database backup including schema and data
**Tables**: Early version of unicorn database schema

### Dump20170529.sql  
**Purpose**: MySQL database dump from May 29, 2017
**Contents**: Updated database backup with schema changes
**Tables**: Evolved version of unicorn database with additional features

## Database Schema (Based on R Code Analysis)

### Core Tables
- `unicorn_portfolios` - Portfolio definitions and stock/forex symbols
- `unicorn_best_featurelist` - GA-optimized feature selections per portfolio
- `unicorn_allocationhistory` - Daily portfolio allocation decisions
- `unicorn_portfolios_details` - Portfolio performance metrics and metadata
- `unicorn_universalfeaturelist_daily` - Master list of available features
- `unicorn_portfolio_attributes` - Portfolio configuration and settings

### User Management (from uniquant framework)
- `uniquant_users` - User authentication with bcrypt passwords
- `uniquant_portfolio` - User-owned portfolio definitions
- `uniquant_holding` - Individual security holdings
- `uniquant_holding_forex` - Forex-specific holding details
- `uniquant_history` - Historical performance tracking
- `uniquant_trade` - Trade execution records

## Migration Strategy

### Phase 1: Schema Modernization
- Normalize existing unicorn tables
- Add proper foreign key constraints
- Implement proper indexing strategy
- Add audit trails and timestamps

### Phase 2: Data Migration
- Migrate file-based data to database tables
- Convert R data types to SQL equivalents
- Implement data validation and constraints

### Phase 3: Performance Optimization
- Add indexes for frequently queried columns
- Implement read replicas for analytics queries
- Optimize for high-frequency trading data access

## Future Migration Files
- `schema_v2.sql` - Modernized database schema
- `migrate_from_r.sql` - Data migration from R/file-based storage
- `indexes.sql` - Performance optimization indexes
- `constraints.sql` - Data integrity constraints
- `seed_data.sql` - Initial reference data for development
