# Silver Layer Economic Data Processing

## Overview
The silver layer transforms bronze layer economic data from all 4 categories into a unified, high-quality dataset optimized for alpha model consumption.

## Architecture

### Input Sources (Bronze Layer)
- **Economic Growth**: GDP, employment, industrial production indicators
- **Consumer Business**: CPI, retail sales, consumer confidence metrics
- **International Trade**: Trade balance, exchange rates, global indicators
- **Monetary Policy**: Interest rates, money supply, yield curve data

### Processing Pipeline
1. **Data Loading**: Load latest processed data from each economic category
2. **Temporal Alignment**: Align data across categories to common date ranges
3. **Data Quality Control**: Remove low-quality features and handle missing data
4. **Feature Selection**: Select most informative features per category
5. **Standardization**: Create consistent format for alpha model consumption

## Key Features

### Quality Controls
- **Missing Data Threshold**: 30% maximum missing data allowed
- **Variance Threshold**: Remove features with <1% variance
- **Correlation Filtering**: Remove features with >95% correlation
- **Coverage Requirements**: 70% minimum data coverage for inclusion

### Feature Management
- **Category Limits**: Maximum 50 features per economic category
- **Total Features**: Target ~200 high-value features for efficiency
- **Metadata Tracking**: Comprehensive feature provenance and quality metrics

### Output Formats
- **CSV Data**: Timestamped processed datasets
- **JSON Metadata**: Feature descriptions, quality metrics, processing details
- **Latest Links**: Always-current symbolic links for integration

## Usage

### Basic Processing
```python
from silver_processor import SilverLayerEconomicProcessor

# Initialize processor
processor = SilverLayerEconomicProcessor()

# Process all intervals
results = processor.process_all_intervals()

# Process specific interval
df, metadata = processor.process_interval('1_day')
```

### Configuration
```python
# Custom configuration
processor.config = {
    'max_features_per_category': 30,    # Reduce feature count
    'min_data_coverage': 0.8,           # Higher quality threshold
    'correlation_threshold': 0.9,       # Less aggressive correlation filtering
    'missing_data_threshold': 0.2       # Stricter missing data tolerance
}
```

## Output Structure

### Data Files
```
processed_data/
├── 1_day/
│   ├── economic_silver_1_day_20250110_142857.csv
│   ├── economic_silver_metadata_1_day_20250110_142857.json
│   ├── economic_silver_latest.csv
│   └── economic_silver_metadata_latest.json
└── 1_hour/
    ├── economic_silver_1_hour_20250110_142857.csv
    ├── economic_silver_metadata_1_hour_20250110_142857.json
    ├── economic_silver_latest.csv
    └── economic_silver_metadata_latest.json
```

### Feature Naming Convention
- `economic_growth_{feature_name}`: GDP, employment, industrial metrics
- `consumer_business_{feature_name}`: CPI, retail, consumer confidence
- `international_trade_{feature_name}`: Trade balance, FX, global data
- `monetary_policy_{feature_name}`: Interest rates, money supply, yield curves

## Integration with Alpha Models

### Direct Data Access
```python
import pandas as pd

# Load latest silver layer data
df = pd.read_csv('/path/to/economic_silver_latest.csv', index_col=0, parse_dates=True)

# Load metadata for feature understanding
import json
with open('/path/to/economic_silver_metadata_latest.json', 'r') as f:
    metadata = json.load(f)
```

### Feature Categories for Alpha Models
- **Growth Indicators**: GDP components, employment metrics
- **Inflation Signals**: Price indices, wage growth
- **Monetary Conditions**: Interest rates, money supply growth
- **Trade Position**: Balance of trade, currency strength
- **Business Cycle**: Leading/lagging economic indicators

## Performance Considerations

### Memory Management
- **1-day interval**: ~200 features, efficient processing
- **1-hour interval**: ~200 features, moderate memory usage
- **1-minute interval**: Skipped due to 190GB memory requirement

### Processing Speed
- Bronze data loading: 1-2 seconds per category
- Feature alignment: 2-5 seconds depending on date range
- Quality filtering: 3-8 seconds for correlation analysis
- Feature selection: 1-3 seconds per category

## Data Quality Metrics

### Coverage Analysis
- **Temporal Coverage**: Percentage of expected time periods with data
- **Feature Coverage**: Percentage of features with sufficient data
- **Cross-Category Alignment**: Overlap of valid observations across categories

### Quality Scores
- **Completeness**: Ratio of non-missing to total data points
- **Consistency**: Stability of feature values over time
- **Informativeness**: Variance and signal-to-noise characteristics

## Troubleshooting

### Common Issues
1. **Missing Bronze Data**: Check bronze layer processing status
2. **Memory Errors**: Reduce feature count or use smaller intervals
3. **Date Alignment Issues**: Verify bronze layer timestamp formats
4. **Feature Selection Failures**: Check sklearn availability and data quality

### Debugging
```python
# Enable detailed logging
import logging
logging.basicConfig(level=logging.DEBUG)

# Check bronze data availability
processor = SilverLayerEconomicProcessor()
bronze_data = processor.load_bronze_data('1_day')
for category, df in bronze_data.items():
    print(f"{category}: {df.shape}")
```

## Implementation Status

### Completed Components
- ✅ Multi-category data loading and alignment
- ✅ Comprehensive data quality controls
- ✅ Feature selection and dimensionality reduction
- ✅ Metadata generation and tracking
- ✅ Standardized output formats
- ✅ Integration-ready CSV and JSON exports

### Ready for Enhancement
- 🔄 Advanced feature engineering (rolling statistics, ratios)
- 🔄 Machine learning-based feature selection
- 🔄 Real-time processing capabilities
- 🔄 Alpha model performance feedback integration

## 🧪 **Testing Infrastructure**

**Test Location:** `/workspaces/unicorninvesting/tests/unicorn/1_data_sources/3_silver/economic_indicators/`

### **Comprehensive Test Suite**
- **`test_silver_economic_indicators.py`** - Complete silver layer validation framework
- **Coverage**: Full silver layer processing pipeline validation
- **Test Categories**:
  - Data aggregation validation across economic categories
  - Feature selection quality assessment
  - Temporal alignment accuracy testing
  - Data quality metrics validation
  - Output format compliance verification
  - Performance benchmarking

### **Key Test Features**
- **Bronze Data Integration**: Validates loading from all 4 economic categories
- **Temporal Alignment**: Tests date synchronization across categories
- **Feature Selection Quality**: Validates 50+ curated features from highest quality data
- **Output Validation**: Verifies CSV structure, JSON metadata, and data integrity
- **Performance Metrics**: Benchmarks processing speed and throughput

### **Running Tests**
```bash
# Navigate to silver layer test directory
cd /workspaces/unicorninvesting/tests/unicorn/1_data_sources/3_silver/economic_indicators

# Run comprehensive silver layer tests
python test_silver_economic_indicators.py

# Run with pytest for detailed output
pytest test_silver_economic_indicators.py -v
```

### **Test Results Summary**
- **Data Processing**: 32,873 daily + 788,929 hourly observations successfully processed
- **Feature Quality**: 50 curated features from international trade (highest coverage category)
- **Data Quality**: 0.0% missing data in final output with robust quality filtering
- **Integration**: End-to-end bronze-to-silver pipeline validation confirmed