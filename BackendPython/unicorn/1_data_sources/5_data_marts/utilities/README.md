# Utilities - Data Marts Operations

## Purpose
Utility scripts for creating, maintaining, and managing subject-specific data marts optimized for analytical use cases.

## Typical Scripts
- **setup_data_marts.py** - Create and configure all subject-specific data marts
- **forex_mart_builder.py** - Build and maintain forex-specific analytical datasets
- **equity_mart_builder.py** - Create equity market analysis data mart
- **crypto_mart_builder.py** - Build cryptocurrency analysis data mart
- **performance_mart_builder.py** - Create performance analysis data mart
- **risk_mart_builder.py** - Build risk management data mart
- **user_access_manager.py** - Manage user permissions and data access patterns

## Subject-Specific Marts
- **forex_analytics_mart.py** - Currency pair analysis, carry trades, volatility
- **equity_sector_mart.py** - Sector analysis, industry comparisons, stock screening
- **portfolio_analysis_mart.py** - Portfolio composition, allocation, rebalancing
- **trading_strategy_mart.py** - Strategy performance, signal analysis, backtesting
- **compliance_reporting_mart.py** - Regulatory reporting and compliance data
- **client_reporting_mart.py** - Client-specific views and custom reports

## Performance Optimization
- **mart_indexing_optimizer.py** - Optimize indexes for analytical queries
- **partition_manager.py** - Manage data partitioning for large datasets
- **cache_optimizer.py** - Implement caching strategies for frequent queries
- **query_performance_analyzer.py** - Analyze and optimize slow queries
- **data_compression.py** - Implement compression for archival data

## Usage Pattern
```bash
# Setup operations
python utilities/setup_data_marts.py --initialize-all
python utilities/forex_mart_builder.py --create-views

# Daily updates
python utilities/equity_mart_builder.py --refresh-daily
python utilities/performance_mart_builder.py --update-metrics

# Optimization
python utilities/mart_indexing_optimizer.py --analyze-usage
python utilities/query_performance_analyzer.py --report
```

Data mart utilities focus on:
- Subject-specific data organization
- Analytical query optimization
- User access management
- Performance tuning for reporting workloads
