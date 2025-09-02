# ETH Alpha Model

## Overview

This directory contains a complete ETH (Ethereum) alpha model implementation featuring **three production-ready forecasting methodologies** with an organized model storage system. The implementation provides ETH price predictions and trading signals using advanced machine learning and time series analysis techniques.

## 🎯 **Current Implementation Status**

### **✅ Production Ready (3 Methodologies)**
1. **Prophet Framework** - Time series forecasting with 4 model variants
2. **XGBoost Framework** - Gradient boosting with advanced feature engineering  
3. **Ensemble Framework** - Combines Prophet + XGBoost with 3 weighting strategies

### **📊 Model Performance Summary**
- **Prophet Models**: 4 variants with MAPE 9.96% - 14.37%
- **XGBoost Models**: 5 variants with MAPE 0.26% - 12.75%, R² up to 0.9817
- **Ensemble Models**: 6 models with 82% average confidence across strategies
- **Total Storage**: 15 models (3.7MB) with complete metadata tracking

## 🚀 **Quick Start Guide**

### **1. Best Performing Model (XGBoost)**
```python
from model_storage_manager import ModelStorageManager

# Load the best performing model
storage = ModelStorageManager()
models = storage.list_models(methodology='xgboost')
best_model = min(models, key=lambda x: x.performance_metrics.get('mape', float('inf')))
model, metadata = storage.load_model(best_model.model_id)

# Make predictions
predictions = model.predict(your_data)
```

### **2. Ensemble Predictions (Recommended)**
```python
from eth_ensemble_framework import ETHEnsembleForecastFramework

# Initialize ensemble with automatic best model loading
ensemble = ETHEnsembleForecastFramework()
ensemble.load_best_models()

# Create weighted ensemble forecast
forecast = ensemble.create_weighted_ensemble(data, strategy='performance_weighted')
```

### **3. Train New Models**
```python
# Prophet variant
from eth_prophet_framework import ETHProphetFrameworkWithStorage
prophet = ETHProphetFrameworkWithStorage()
prophet_id = prophet.train_and_store_model(data, variant='enhanced')

# XGBoost variant  
from eth_xgboost_framework import ETHXGBoostFrameworkWithStorage
xgboost = ETHXGBoostFrameworkWithStorage()
xgboost_id = xgboost.train_and_store_model(data, variant='tuned')
```

## 📁 **Directory Structure**

### **Core Frameworks (Root Level)**
- **`eth_prophet_framework.py`** - Prophet time series forecasting (4 variants)
- **`eth_xgboost_framework.py`** - XGBoost with 17-feature engineering pipeline
- **`eth_ensemble_framework.py`** - Advanced ensemble combining methodologies

### **Organized Subdirectories**
```
ETH/
├── /algorithms/    # LEAN algorithm implementations
├── /config/        # Configuration files
├── /deployment/    # Deployment scripts and automation
├── /legacy/        # Legacy implementations and standalone versions
├── /model_storage/ # Production model storage (15 models)
├── /models/        # Individual model implementations + model_management/
├── /research/      # Research and development files
├── /tests/         # Testing framework and demo scripts
└── /utilities/     # Utility scripts and model building automation
```

### **Model Storage System**
```
model_storage/
├── prophet/     # 4 Prophet models (209KB)
├── xgboost/     # 5 XGBoost models (3.5MB) 
├── ensemble/    # 6 Ensemble models (1.4KB)
└── lstm/        # Ready for LSTM models
```

### **Model Management System**
```
models/model_management/
├── model_storage_manager.py  # Central model storage and versioning
├── __init__.py              # Package initialization
└── README.md                # Model management documentation
```

### **Quick Access**
- **Core Demos**: See `/tests/demo_*.py` for methodology comparisons
- **LEAN Algorithms**: See `/algorithms/` for production algorithm implementations
- **Research**: See `/research/` for standalone research implementations
- **Deployment**: See `/deployment/` for framework deployment scripts
- **Utilities**: See `/utilities/` for model building and automation tools
- **Legacy Code**: See `/legacy/` for historical implementations
- **Configuration**: See `/config/` for framework settings

## 🔬 **Model Methodology Details**

### **Prophet Framework (4 Variants)**
- **Basic**: Standard Prophet with seasonality
- **Enhanced**: Holiday effects + custom seasonality  
- **Optimized**: Hyperparameter tuning + cross-validation
- **Performance**: 9.96% - 14.37% MAPE

### **XGBoost Framework (3 Variants + Feature Engineering)**
- **Standard**: Basic XGBoost with 17 engineered features
- **Tuned**: Hyperparameter optimization via GridSearchCV
- **Ensemble**: Multiple XGBoost models combined
- **Features**: Technical indicators, rolling statistics, lag features
- **Performance**: 0.26% - 12.75% MAPE, up to 98.17% R²

### **Ensemble Framework (3 Weighting Strategies)**
- **Equal Weighting**: Simple average of Prophet + XGBoost
- **Performance Weighted**: Based on historical MAPE scores
- **Inverse Error**: Weighted by inverse prediction errors
- **Confidence Scoring**: Dynamic confidence based on agreement

## 🎯 **Performance Benchmarks**

| Methodology | Best Model | MAPE | R² Score | Model Count |
|-------------|------------|------|----------|-------------|
| Prophet | v001_basic | 9.96% | - | 4 models |
| XGBoost | v003_standard | 0.26% | 0.9817 | 5 models |
| Ensemble | performance_weighted | - | - | 6 models |

## 🔧 **Extension Ready**

The architecture is designed for easy methodology expansion:

### **Ready Directories**
- **`/lstm/`** - LSTM neural networks
- **`/arima/`** - Classical time series analysis  
- **`/transformer/`** - Attention-based models
- **`/garch/`** - Volatility modeling

### **Scalable Design**
- Automatic version control (v001, v002, etc.)
- Methodology-specific storage organization
- Unified API via ModelStorageManager
- Performance tracking databases per methodology

## � **Next Steps**

1. **Real-Time Integration** - Connect to live ETH price feeds
2. **LSTM Implementation** - Add deep learning for sequence modeling
3. **API Development** - REST endpoints for model serving
4. **Model Monitoring** - Automated performance degradation detection
5. **ARIMA Integration** - Classical time series decomposition
6. **Production Deployment** - Dockerized model serving

## � **Additional Documentation**

- **`DIRECTORY_OVERVIEW.md`** - Complete file-by-file breakdown
- **`model_storage/README.md`** - Storage system documentation
- **`IMPLEMENTATION_SUMMARY.md`** - Technical implementation details

**Status**: ✅ Production Ready | **Models**: 15 across 3 methodologies | **Best Performance**: 0.26% MAPE (XGBoost)
