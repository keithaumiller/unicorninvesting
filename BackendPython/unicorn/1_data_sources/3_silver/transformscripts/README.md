# Transform Scripts - Silver Layer Processing

## Purpose
Scripts for cleaning, normalizing, and enriching bronze data into high-quality silver layer datasets.

## Typical Scripts
- **bronze_to_silver.py** - Clean and normalize bronze data
- **data_enrichment.py** - Add calculated fields and indicators
- **outlier_detection.py** - Identify and handle data anomalies
- **time_series_alignment.py** - Standardize timestamps and frequencies

## Processing Pattern
```
2_bronze → transformscripts/ → silver cleaned/normalized data
```

Silver transform scripts focus on:
- Data cleaning and normalization
- Feature engineering and enrichment
- Outlier detection and handling
- Time series standardization
