# Data

**Status**: 📦 Legacy data preserved, ready for Python processing pipeline

Market data storage, reference datasets, and analysis results for the Unicorn Investing platform.

## Current Status (August 2025)

### ✅ Data Preservation
- **Legacy Data**: All historical market data preserved during reorganization
- **Reference Data**: Exchange listings and currency pairs maintained
- **Structure**: Original directory structure preserved for migration
- **Accessibility**: Ready for Python-based data processing pipeline

### 🔄 Migration Progress
- **Raw Data**: Legacy CSV files available for Python processing
- **Database Integration**: Ready for MySQL import and normalization
- **API Integration**: Prepared for real-time data feed integration
- **Feature Engineering**: Historical feature lists available for ML pipeline

## Purpose
- Store raw and processed market data (stocks, forex, indices)
- Maintain reference datasets for exchanges and securities
- Archive analysis results and model outputs
- Historical data preservation and versioning
- Support Python-based data processing and ML pipelines

## Subdirectories

### exchangedata/
**Status**: ✅ Complete reference data available
**Purpose**: Reference data for securities and exchanges
**Contents**:
- `all_stocks.csv` - Comprehensive list of available stocks (ready for database import)
- `amex.csv` - American Stock Exchange listings
- `nasdaq.csv` - NASDAQ exchange listings  
- `nyse.csv` - New York Stock Exchange listings
- `Orig_nyse.csv` - Original NYSE data backup
- `currencies.csv` - Available currency pairs for forex trading
- `FOREX.csv` - Forex pair definitions and metadata
- `FOREX.txt` - Forex trading notes and documentation
- `stockstouse.csv` - Curated list of stocks for analysis
- `master_featurelist.csv` - Master list of features for ML models

### old/
**Status**: 📦 Archived legacy files
**Purpose**: Archive of legacy data files and deprecated datasets
**Contents**:
- `Finalsetoutputstructure.ods` - Legacy output structure documentation
- `Finalsetoutputstructurev2.csv` - Updated output structure reference
- `OLD_Architecture.ods` - Previous system architecture documentation
- `Predict.csv` - Historical prediction results
- `stock_returns_base150.csv` - Base return calculations for 150 stocks
- `table.csv` - Generic data table (purpose unclear)

### results/
**Purpose**: Analysis outputs, model results, and performance tracking
**Structure**: Organized by user ID and portfolio ID `/results/{userid}/{portfolioid}/`
**Contents**:
- Model training results and performance metrics
- Generated allocation recommendations
- Portfolio performance tracking over time
- Neural network outputs and GA optimization results
- Performance plots and visualization outputs

**Typical Result Files**:
- `bestnetfile` - Serialized best-performing neural network
- `GAResults.csv` - Genetic algorithm optimization results
- `NNresults.csv` - Neural network training results
- Performance plots in `/plots/` subdirectory
- Portfolio allocation history
- Feature selection results from GA optimization

## Data Flow
1. **Input**: Raw market data downloaded to `/stockdata/` (organized by symbol)
2. **Processing**: Reference data from `exchangedata/` used for symbol validation
3. **Analysis**: Results stored in `results/` with hierarchical organization
4. **Archive**: Old versions preserved in `old/` for historical reference

## File Formats
- **CSV**: Primary format for structured data exchange
- **RData**: R serialized objects for model persistence
- **PNG**: Performance charts and visualization outputs
- **TXT**: Documentation and configuration notes
