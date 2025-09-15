# Federal Reserve FRED Data

This directory contains economic data files downloaded from the Federal Reserve Economic Data (FRED) API.

## ⚠️ Important: Data Files Not Tracked in Git

**All data files (*.csv) in this directory are automatically excluded from git tracking** to prevent repository bloat.

## Data Sources

- Federal Reserve Economic Data (FRED)
- Critical economic indicators
- Interest rates, inflation, employment data
- GDP and economic sentiment indicators

## File Naming Convention

- `fred_critical_indicators_{timestamp}.csv`

## Data Collection

Data is collected via the FRED API connector:
```bash
python BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/fred_connector.py
```

This ensures the latest economic data is available for market analysis and trading algorithms.