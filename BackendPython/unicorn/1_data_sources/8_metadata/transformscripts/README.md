# Transform Scripts - Metadata Processing

## Purpose
Scripts for managing data lineage, quality metrics, and governance across all data warehouse layers.

## Typical Scripts
- **lineage_tracker.py** - Track data flow and transformations
- **quality_monitoring.py** - Monitor data quality metrics
- **schema_evolution.py** - Manage schema changes and versioning
- **governance_reports.py** - Generate data governance reports

## Processing Pattern
```
All layers → transformscripts/ → metadata and governance
```

Metadata transform scripts focus on:
- Data lineage documentation
- Quality metric calculation
- Schema management
- Governance and compliance reporting
