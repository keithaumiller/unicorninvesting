# Alpha Models Scripts Directory

This directory contains utility scripts for managing and operating alpha models across the Unicorn Investing platform.

## 🚀 **Available Scripts**

### **📊 Model Training & Retraining**

#### **`comprehensive_model_retraining.py`** ⭐ **NEW**
Comprehensive alpha model retraining campaign for BTC and ETH across all timeframes.

**Purpose**: Retrain all alpha models with specified requirements:
- 5 models for each methodology (Prophet, XGBoost, Ensemble) for each interval
- At least 600 datapoints each  
- Covers 1min, 1hour, and 1day timeframes

**Usage**:
```bash
# Dry run to validate requirements
python comprehensive_model_retraining.py --dry-run

# Execute full retraining campaign (5 models per methodology per timeframe)
python comprehensive_model_retraining.py --execute

# Custom configuration
python comprehensive_model_retraining.py --execute --models-per-method 10 --min-datapoints 800

# Specific assets or timeframes
python comprehensive_model_retraining.py --execute --assets ETH --timeframes 1min,1hour
python comprehensive_model_retraining.py --execute --assets BTC --timeframes 1day
```

**Target Output**:
- ETH: 5 models × 3 methodologies × 3 timeframes = 45 models
- BTC: 5 models × 3 methodologies × 3 timeframes = 45 models  
- **Total**: 90+ models with 600+ datapoints each

**Features**:
- Automatic data validation (600+ datapoints requirement)
- Comprehensive error handling and reporting
- Detailed JSON reports with training statistics
- Support for both live IBKR data and simulated data fallback
- Progress logging and performance monitoring

### **📈 Model Performance Management**

#### **`model_performance_manager.py`**
Legacy model performance tracking and management system.

#### **`model_performance_manager_v2.py`**
Enhanced version of the model performance manager with additional features.

#### **`performance_summary.py`**
Generates performance summaries and reports for trained models.

#### **`performance_tools.py`**
Utility functions for model performance analysis and reporting.

### **🔬 Legacy Alpha Models**

#### **`AdvancedForexForecastingAlpha.py`**
Advanced forex forecasting alpha model implementation.

#### **`EthFocusedAlpha.py`**
ETH-focused alpha model for cryptocurrency trading.

## 🎯 **Quick Start Guide**

### **Complete Model Retraining Campaign**
```bash
# Navigate to scripts directory
cd /workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/scripts

# Validate all requirements first
python comprehensive_model_retraining.py --dry-run

# Execute comprehensive retraining if validation passes
python comprehensive_model_retraining.py --execute
```

### **Custom Retraining Scenarios**
```bash
# High-volume training (10 models per methodology)
python comprehensive_model_retraining.py --execute --models-per-method 10

# Extended data requirements (1000 datapoints minimum)
python comprehensive_model_retraining.py --execute --min-datapoints 1000

# ETH-only intensive retraining
python comprehensive_model_retraining.py --execute --assets ETH --models-per-method 15

# Specific timeframe focus
python comprehensive_model_retraining.py --execute --timeframes 1min --models-per-method 20
```

## 📋 **Script Dependencies**

### **Required for `comprehensive_model_retraining.py`**
- **ETH Models**: `BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/production_model_manager.py`
- **BTC Models**: `BackendPython/unicorn/2_alpha_models/CRYPTO/BTC/btc_production_framework.py`
- **Data Integration**: IBKR live data integration (with simulated data fallback)
- **Python Libraries**: pandas, numpy, sklearn, xgboost, prophet (if available)

### **Shared Dependencies**
- Model storage systems
- Performance databases
- Logging infrastructure

## 🔧 **Development Guidelines**

### **Adding New Scripts**
1. Follow the existing naming convention: `descriptive_script_name.py`
2. Include comprehensive docstring with purpose, usage, and examples
3. Add error handling and logging
4. Update this README with script documentation
5. Ensure compatibility with existing model frameworks

### **Script Organization**
- **Model Training**: Scripts that create or retrain models
- **Performance Management**: Scripts that analyze and report on model performance  
- **Legacy Models**: Older alpha model implementations for reference

### **Best Practices**
- Use absolute paths for cross-platform compatibility
- Include `--dry-run` options for validation
- Provide detailed logging and progress reporting
- Generate comprehensive reports in JSON format
- Handle both live and simulated data scenarios

## 📊 **Expected Outputs**

### **Comprehensive Retraining Results**
After running `comprehensive_model_retraining.py --execute`, expect:

```
📊 CAMPAIGN SUMMARY:
   Total models trained: 90
   Successful: 85 (94.4%)
   Failed: 5 (5.6%)
   Total duration: 1847.3 seconds (30.8 minutes)
   Average time per model: 20.5 seconds

📋 DETAILED RESULTS:
   ETH 1min: 15/15 (100.0%) in 245.2s
   ETH 1hour: 15/15 (100.0%) in 198.7s
   ETH 1day: 15/15 (100.0%) in 156.3s
   BTC 1min: 13/15 (86.7%) in 267.1s
   BTC 1hour: 14/15 (93.3%) in 201.8s
   BTC 1day: 13/15 (86.7%) in 178.2s
```

## 🚀 **Integration with Portfolio Systems**

The retrained models automatically integrate with:
- **Myportolio**: Enhanced model availability for portfolio status checks
- **Production Systems**: Models become available for live trading strategies
- **Simulation Framework**: Enhanced backtesting with improved model coverage

---

**Last Updated**: September 3, 2025  
**Directory**: `/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/scripts/`  
**Maintenance**: Update this README when adding new scripts or modifying existing ones
