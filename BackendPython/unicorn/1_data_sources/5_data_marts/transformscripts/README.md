# Transform Scripts - Data Marts Processing

## Purpose
Scripts for creating subject-specific data marts from gold layer data optimized for specific analytical use cases.

## Typical Scripts
- **forex_mart.py** - Create forex-specific analytical datasets
- **equity_mart.py** - Build equity market data marts
- **performance_mart.py** - Generate performance analysis datasets
- **risk_mart.py** - Create risk management data marts

## Processing Pattern
```
4_gold → transformscripts/ → subject-specific data marts
```

Data mart transform scripts focus on:
- Subject-specific optimizations
- Analytical dataset creation
- Performance-optimized views
- User-specific data access patterns
