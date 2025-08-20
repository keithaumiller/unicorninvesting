# Data Cleaning (Legacy)

Legacy R scripts for data preprocessing, validation, and debugging utilities.

## Purpose
- Data quality assurance and validation
- Debugging utilities for R analytics pipeline  
- Data preprocessing and normalization functions
- **Status**: Being migrated to Python equivalents

## Files

### debugframework.R
**Purpose**: Debug utilities and data validation framework for R analytics
**Key Functions**:
- `mydebug()` - Custom debugging function used throughout R codebase
- Data validation and error checking utilities
- Debug output formatting and logging

**Usage**: 
- Referenced by multiple R scripts for debugging and validation
- Provides consistent debug output across the analytics pipeline
- Used in GA optimization, neural network training, and data processing

## Migration Notes
- Debug functionality will be replaced by Python logging framework
- Data validation will use pandas and numpy validation functions
- Error handling will follow Python best practices with try/catch blocks

## Dependencies
- Used by most R scripts in the BackendPython directory
- Provides shared debugging utilities across the analytics codebase

**Future Migration**: This functionality will be incorporated into Python backend utilities with proper logging and error handling frameworks.
