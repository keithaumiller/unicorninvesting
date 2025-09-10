# Validation Results

This directory contains historical validation and test result files.

## Files Moved from Root Directory

The following result files were moved from the project root to maintain clean architecture:

### Validation Result Files
- `basic_validation_results_20250905_172605.json` - Basic frontend-backend validation results
- `final_enhanced_validation_20250905_172625.json` - Enhanced validation with comprehensive mapping

### File Format

These JSON files contain validation results with structure:
```json
{
  "timestamp": "ISO timestamp",
  "frontend_results": {...},
  "backend_results": {...}, 
  "summary": {
    "success_rate": "percentage",
    "total_data_points": "count"
  }
}
```

### Purpose

These files document:
- ✅ Frontend-backend data integration validation
- ✅ API endpoint testing results  
- ✅ Data mapping success rates
- ✅ System performance metrics
- ✅ Historical testing baselines

### Architecture Compliance

These files were moved from the root directory to comply with the enforced directory structure that keeps result files organized in the `tests/validation_results/` hierarchy.
