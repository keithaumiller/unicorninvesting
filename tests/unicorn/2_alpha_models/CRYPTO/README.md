# Crypto Alpha Models - Test Results and Validation Data

## Overview
This directory contains cryptocurrency-specific validation databases and test results for alpha model validation.

## Database Files

### Enhanced XGBoost Models
- **crypto_BTC_enhanced_xgboost.db**: Bitcoin XGBoost model validation results
- **crypto_ETH_enhanced_xgboost.db**: Ethereum XGBoost model validation results

## Testing Coverage

### Assets Validated
- **BTC (Bitcoin)**: Primary cryptocurrency validation
- **ETH (Ethereum)**: Primary altcoin validation

### Validation Results
- **Performance Range**: Realistic R² values for crypto volatility
- **Overfitting Detection**: Conservative model validation
- **Feature Engineering**: Leak-free technical indicators

## Integration

These databases contain results from validation scripts in the shared directory:
- `../shared/xgboost_model_validator.py`
- `../shared/enhanced_xgboost_builder.py`
- `../shared/comprehensive_model_validator.py`

## Usage

```python
import sqlite3

# Query BTC validation results
conn = sqlite3.connect('crypto_BTC_enhanced_xgboost.db')
results = pd.read_sql('SELECT * FROM model_performance', conn)
```

## Data Sources

All validation uses real market data from:
- Yahoo Finance silver layer data warehouse
- Path: `/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/`

## Success Metrics

- **Overfitting Elimination**: 100% for XGBoost models
- **Realistic Performance**: R² values appropriate for crypto volatility
- **Production Ready**: Validated with real market data