# Data Sources Test Data

This directory contains test data files used for validating data collection and processing functionality.

## 📊 **Purpose**

Provides high-quality test data for:
- IBKR connector integration testing
- Data quality validation
- Technical indicator calculations
- Performance benchmarking
- End-to-end pipeline testing

## 📁 **Current Test Data**

### **ETH 1-Minute Data** (`eth_1min/`)
- **File**: `integration_test.json`
- **Size**: 178KB (8,021 lines)
- **Content**: 1,000 ETH 1-minute OHLCV bars
- **Source**: Real Interactive Brokers data
- **Date Range**: August 28-29, 2025
- **Quality Score**: 1.0 (perfect data quality)
- **Contract ID**: 541686654 (IBKR ETH contract)

### **Data Structure**
```json
{
  "metadata": {
    "collection_time": "2025-08-29T15:32:41.869489",
    "contract_id": 541686654,
    "bar_count": 1000,
    "timeframe": "1min",
    "statistics": {
      "bars_collected": 1000,
      "data_quality_score": 1.0,
      "data_range": {...}
    }
  },
  "data": [
    {
      "timestamp": "2025-08-28T22:53:00+00:00",
      "open": 4508.75,
      "high": 4510.3,
      "low": 4508.7,
      "close": 4510.25,
      "volume": 0.87547194
    }
    // ... 999 more bars
  ]
}
```

## 🎯 **Usage in Tests**

### **Integration Testing**
- Validates IBKR connector functionality with real data
- Tests data quality scoring and validation
- Benchmarks data collection performance

### **Technical Indicator Testing**
- Provides baseline data for indicator calculations
- Validates indicator accuracy and performance
- Tests edge cases and data quality handling

### **Pipeline Testing**
- End-to-end data flow validation
- Performance benchmarking with large datasets
- Memory usage and efficiency testing

## 🔗 **Related Components**

- **IBKR Tests**: `/tests/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/`
- **Source Code**: `/BackendPython/unicorn/1_data_sources/`
- **Main Data Pipeline**: `/BackendPython/unicorn/1_data_sources/`

## 📈 **Data Quality Metrics**

- **Completeness**: 100% (1000/1000 bars)
- **Quality Score**: 1.0 (perfect)
- **OHLC Validation**: 100% valid relationships
- **Timestamp Consistency**: 100% sequential
- **Price Range**: $2600-$4600 (valid ETH range for timeframe)
- **Volume Validation**: 100% non-negative values

## 📝 **Adding New Test Data**

When adding new test data files:
1. Follow the same JSON structure for consistency
2. Include comprehensive metadata
3. Validate data quality before use
4. Document data source and collection method
5. Update relevant test cases to use new data
