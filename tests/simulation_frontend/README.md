# Simulation Frontend Tests

This directory contains test files for simulation and frontend integration functionality.

## Files Moved from Root Directory

The following files were moved from the project root to maintain clean architecture:

### Test Files (`test_*`)
- `test_php_simulation_detection.php` - PHP test for simulation detection logic
- `test_simulation_detection.py` - Python test for simulation detection functionality  
- `test_simulation_frontend_integration.py` - Frontend integration tests
- `test_simulation_selector.py` - Enhanced simulation selector validation

### Usage

These are standalone test files that can be run individually:

```bash
# PHP tests
php test_php_simulation_detection.php

# Python tests  
python3 test_simulation_detection.py
python3 test_simulation_frontend_integration.py
python3 test_simulation_selector.py
```

### Purpose

These tests validate:
- ✅ Simulation detection from backend portfolio structure
- ✅ PHP-Python integration compatibility
- ✅ Frontend dropdown functionality
- ✅ Backtest result processing
- ✅ Performance calculation logic

### Architecture Compliance

These files were moved from the root directory to comply with the enforced directory structure that keeps test files organized in the `tests/` hierarchy.
