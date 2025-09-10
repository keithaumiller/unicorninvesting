# Debug Files

This directory contains debugging scripts for development and troubleshooting.

## Files Moved from Root Directory

The following debug files were moved from the project root to maintain clean architecture:

### Debug Scripts (`debug_*`)
- `debug_drupal_service.php` - Debug Drupal PortfolioApiService methods
- `debug_exact_drupal.php` - Test exact Drupal PortfolioApiService logic
- `debug_simulation_validation.php` - Debug simulation validation logic

### Usage

These are standalone debug scripts for development use:

```bash
php debug_drupal_service.php
php debug_exact_drupal.php  
php debug_simulation_validation.php
```

### Purpose

These debug scripts help with:
- ✅ Testing Drupal service integration
- ✅ Validating simulation detection logic
- ✅ Debugging frontend-backend communication
- ✅ Troubleshooting PHP-Python data flow

### Architecture Compliance

These files were moved from the root directory to comply with the enforced directory structure that keeps debug files organized in the `tests/debug/` hierarchy.
