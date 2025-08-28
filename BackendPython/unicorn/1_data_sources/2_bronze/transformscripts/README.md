# Transform Scripts - Bronze Layer Processing

## Purpose
Scripts for processing validated raw data into structured bronze layer format with basic data typing and minimal transformations.

## Typical Scripts
- **raw_to_bronze.py** - Convert validated raw data to bronze format
- **data_typing.py** - Apply proper data types and constraints
- **duplicate_detection.py** - Identify and handle duplicate records
- **reference_data_updates.py** - Update exchange and currency reference data

## Processing Pattern
```
1_raw → transformscripts/ → bronze structured storage
```

Bronze transform scripts focus on:
- Data type conversion and validation
- Basic deduplication
- Reference data maintenance
- Historical data preservation
