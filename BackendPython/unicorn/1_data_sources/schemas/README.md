# Database Schemas & Data Models

## 📊 Purpose

This directory contains **database schemas, data models, and structure definitions** for all layers of the data warehouse. Schemas ensure consistent data structure and relationships across the platform.

## 🏗️ Architecture Role

**Schema Management**: Defines structure for Bronze → Silver → Gold → Data Marts

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
