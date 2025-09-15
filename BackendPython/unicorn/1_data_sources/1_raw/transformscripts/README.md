# Transform Scripts - Raw Layer Processing

## Purpose
Scripts for processing and validating data as it enters the raw layer from external connectors.

## Typical Scripts
- **clean_api_responses.py** - Initial data cleaning from API responses  
- **stage_for_bronze.py** - Prepare validated data for bronze layer
- **data_quality_checks.py** - Basic quality control measures

**Note**: Validation scripts have been moved to `/tests/unicorn/1_data_sources/` for centralized test management.

## Processing Pattern
```
External API → connectors/ → transformscripts/ → bronze layer
```

Raw transform scripts focus on:
- Data validation and basic cleaning
- Format standardization
- Initial quality gates
- Metadata extraction
