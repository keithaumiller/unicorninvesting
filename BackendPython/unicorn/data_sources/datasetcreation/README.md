# Dataset Creation

Feature engineering and training set generation for machine learning models.

## Purpose
- Combine multiple stock/forex datasets into unified training matrices
- Generate features from raw market data (price, volume, technical indicators)
- Create training and evaluation datasets for neural network training
- Data preprocessing, normalization, and cleaning

## Files

### Combinestocks.R
**Purpose**: Primary dataset assembly and feature matrix creation
**Key Functions**:
- `combinestocksfunction(numberofstockstouse, featurelistforNN, outputdirectory)` - Main dataset assembly function
- `loadstockdata(symbol)` - Loads individual stock data from CSV files
- Combines multiple stock time series into unified percentage change matrix
- Handles missing data, infinite values, and NaN cleanup
- Creates `adjustedmatrix` with percentage changes for all securities
- Generates `trainingmatrix` with target allocation outputs
- Splits data into training (75%) and evaluation (25%) sets
- Data normalization and cleaning using `is.nan.data.frame()`, `is.na.data.frame()`, `is.infinite.data.frame()`

**Output Variables**:
- `percentchangedcombined` - Master matrix with all stock percentage changes
- `percentchangedcombined_train` - Training dataset (first 75% of data)
- `percentchangedcombined_eval` - Evaluation dataset (last 25% of data)
- `adjustedmatrix` - Portfolio-specific percentage change matrix
- `trainingmatrix` - Target allocation matrix for supervised learning

### Generatefeatureslist.R
**Purpose**: Automated feature list generation from available stock data
**Key Functions**:
- `rebuildstocklistfeatures()` - Builds comprehensive feature list from stock universe
- Reads from `data/exchangedata/stockstouse.csv` for stock symbols
- Creates feature combinations from stock symbols and data points (Open, High, Low, Close, Volume, Adjusted)
- **Status**: Currently deprecated in favor of manual feature list maintenance

### generatetrainingoutput.R
**Purpose**: Training target generation and data preprocessing utilities
**Key Functions**:
- `is.nan.data.frame(x)` - Custom function to detect NaN values in dataframes
- `is.na.data.frame(x)` - Custom function to detect NA values in dataframes  
- `is.infinite.data.frame(x)` - Custom function to detect infinite values in dataframes
- Data cleaning pipeline that sets NaN, NA, and infinite values to 0
- Splits cleaned data into training (75%) and evaluation (25%) sets
- Creates `percentchangedcombined_train` and `percentchangedcombined_eval` datasets

**Data Flow**:
1. Raw market data → Percentage change calculation
2. Data cleaning (NaN/NA/Inf → 0)
3. Train/test split based on temporal order
4. Feature matrix preparation for neural network input

**Dependencies**: Integrates with data from `datagathering/` and outputs to `recomendationsystems/` for model training
