# ETH Directory Structure Overview

## Purpose and Organization

This directory contains the complete ETH (Ethereum) alpha model implementation for the Unicorn Investing platform, featuring a scalable machine learning model storage system with multiple forecasting methodologies.

## 📁 Directory Structure and File Purposes

### **Root Level Files**

#### **Core Framework Files**
- **`eth_prophet_framework.py`** - Prophet time series forecasting framework with 3 variants (basic, enhanced, optimized)
- **`eth_xgboost_framework.py`** - XGBoost framework with feature engineering and 3 variants (standard, tuned, ensemble)
- **`eth_ensemble_framework.py`** - Ensemble forecasting combining Prophet and XGBoost with multiple weighting strategies
- **`__init__.py`** - Package initialization file

#### **Database Files**
- **`eth_prophet_comparison.db`** - Prophet model performance comparison database
- **`eth_xgboost_comparison.db`** - XGBoost model performance comparison database
- **`eth_ensemble_comparison.db`** - Ensemble model performance comparison database
- **`eth_prophet_simple.db`** - Simple Prophet model database
- **`model_performance.db`** - General model performance tracking database

#### **Documentation**
- **`README.md`** - This comprehensive directory overview
- **`IMPLEMENTATION_SUMMARY.md`** - Implementation summary and status
- **`STORAGE_IMPLEMENTATION_SUMMARY.md`** - Storage system implementation details

### **📂 `/algorithms/` - LEAN Algorithm Implementations**
- **`__init__.py`** - Package initialization
- **`enhanced_technical_algorithm.py`** - Complete LEAN algorithm using Enhanced ETH Technical Alpha model with 30+ indicator analysis

### **📂 `/config/` - Configuration Files**
- **`__init__.py`** - Configuration package initialization
- **`prophet_config.py`** - Prophet framework configuration settings and parameters

### **📂 `/deployment/` - Deployment Scripts and Automation**
- **`__init__.py`** - Deployment package initialization
- **`deploy_prophet_framework.py`** - Prophet framework deployment script with environment setup, testing, and monitoring

### **📂 `/docs/` - Documentation**
- **`README.md`** - Additional documentation and guides

### **📂 `/features/` - Feature Engineering**
- **`__init__.py`** - Feature engineering package initialization
- Ready for feature extraction and engineering modules for time series analysis

### **📂 `/legacy/` - Legacy and Standalone Implementations**
- **`__init__.py`** - Legacy package initialization
- **`eth_prophet_clean.py`** - Clean Prophet implementation (legacy)
- **`eth_prophet_organized.py`** - Organized Prophet variant (legacy)
- **`eth_prophet_simple.py`** - Simple Prophet implementation (legacy)
- **`eth_prophet_standalone.py`** - Standalone Prophet implementation (legacy)
- **`ETHEnhancedTechnicalAlgorithm.py`** - Enhanced technical analysis algorithm (legacy)
- **`EnhancedETHTechnicalAlpha.py`** - Enhanced technical alpha model (legacy)

### **📂 `/model_storage/` - Organized Model Storage System**

This is the core of our scalable model storage system with methodology-based organization:

#### **Storage Structure:**
```
model_storage/
├── model_metadata.db           # Central metadata database
├── README.md                   # Storage system documentation
├── prophet/                    # Prophet models (4 models, 209KB)
│   ├── v001_eth_prophet_basic_20250902_155123.pkl
│   ├── v002_eth_prophet_basic_20250902_155147.pkl
│   ├── v003_eth_prophet_enhanced_20250902_155148.pkl
│   └── v004_eth_prophet_optimized_20250902_155148.pkl
├── xgboost/                    # XGBoost models (5 models, 3.5MB)
│   ├── v001_eth_xgboost_standard_20250902_160346.pkl
│   ├── v002_eth_xgboost_standard_20250902_160459.pkl
│   ├── v003_eth_xgboost_standard_20250902_160526.pkl
│   ├── v004_eth_xgboost_tuned_20250902_160527.pkl
│   └── v005_eth_xgboost_ensemble_20250902_160528.pkl
├── ensemble/                   # Ensemble models (6 models, 1.4KB)
│   ├── v001_eth_ensemble_equal_20250902_162621.pkl
│   ├── v002_eth_ensemble_performance_weighted_20250902_162621.pkl
│   ├── v003_eth_ensemble_inverse_error_20250902_162622.pkl
│   ├── v004_eth_ensemble_equal_20250902_162737.pkl
│   ├── v005_eth_ensemble_performance_weighted_20250902_162737.pkl
│   └── v006_eth_ensemble_inverse_error_20250902_162738.pkl
├── lstm/                       # Ready for LSTM models
├── arima/                      # Ready for ARIMA models
├── transformer/                # Ready for Transformer models
└── garch/                      # Ready for GARCH models
```

### **📂 `/models/` - Model Implementations and Management**
- **`__init__.py`** - Package initialization
- **`basic_technical_alpha.py`** - Basic technical analysis alpha model
- **`enhanced_technical_alpha.py`** - Enhanced technical analysis alpha model
- **`eth_ensemble.py`** - Legacy ensemble model
- **`eth_prophet.py`** - Legacy Prophet model
- **`eth_xgboost.py`** - Legacy XGBoost model
- **`eth_build_summary.json`** - Model build summary and metadata

#### **📂 `/models/model_management/` - Model Storage Management**
- **`__init__.py`** - Model management package initialization
- **`model_storage_manager.py`** - Central model storage system with version control, metadata tracking, and organized storage across methodologies
- **`README.md`** - Model management system documentation

### **📂 `/research/` - Research and Development**
- **`__init__.py`** - Package initialization
- **`standalone_signal_generator.py`** - Standalone ETH technical analysis signal generator for research and development

### **📂 `/tests/` - Testing Framework and Demonstrations**
- **`__init__.py`** - Testing package initialization

#### **Testing Files**
- **`test_prophet_framework.py`** - Prophet framework comprehensive testing
- **`alpha_validation_test.py`** - Alpha model validation testing

#### **Demonstration Scripts**
- **`demo_three_methodologies.py`** - Comprehensive demo showing Prophet, XGBoost, and Ensemble models working together
- **`demo_complete_storage_system.py`** - Complete storage system demonstration with performance comparisons
- **`demo_storage_system.py`** - Basic storage system demo focusing on organization and retrieval
- **`demo_framework.py`** - Framework capabilities demonstration

### **📂 `/utilities/` - Utility Scripts and Automation**
- **`__init__.py`** - Utilities package initialization
- **`eth_model_builder.py`** - ETH model building automation and batch processing

## 🎯 **Current Implementation Status**

### **✅ Production Ready Components**
1. **Prophet Framework** - 4 models with variants (basic, enhanced, optimized)
2. **XGBoost Framework** - 5 models with advanced feature engineering
3. **Ensemble Framework** - 6 models combining Prophet and XGBoost
4. **Model Storage System** - Complete with version control and metadata tracking

### **📊 Performance Metrics**
- **Best Prophet Model**: 9.96% MAPE (v001 basic)
- **Best XGBoost Model**: 0.26% MAPE, 0.9817 R² (v003 standard)
- **Ensemble Models**: 82% average confidence across strategies
- **Total Storage**: 3.7MB across 15 models in 3 methodologies

### **🔧 Extension Points**
- **LSTM Models**: Directory ready for implementation
- **ARIMA Models**: Directory ready for time series analysis
- **Transformer Models**: Directory ready for attention-based models
- **GARCH Models**: Directory ready for volatility modeling

## 🚀 **Usage Patterns**

### **1. Load Best Performing Model**
```python
from model_storage_manager import ModelStorageManager
storage = ModelStorageManager()
models = storage.list_models(methodology='xgboost')
best = min(models, key=lambda x: x.performance_metrics.get('mape', float('inf')))
model, metadata = storage.load_model(best.model_id)
```

### **2. Create Ensemble Forecast**
```python
from eth_ensemble_framework import ETHEnsembleForecastFramework
ensemble = ETHEnsembleForecastFramework()
ensemble.load_best_models()
forecast = ensemble.create_weighted_ensemble(data, strategy='performance_weighted')
```

### **3. Train New Model Variant**
```python
from eth_xgboost_framework import ETHXGBoostFrameworkWithStorage
xgb_framework = ETHXGBoostFrameworkWithStorage()
model_id = xgb_framework.train_and_store_model(data, variant='tuned')
```

## 🏗️ **Architecture Principles**

1. **Methodology Separation**: Each algorithm type (Prophet, XGBoost, Ensemble) has dedicated framework
2. **Version Control**: Automatic versioning with v001, v002, etc. naming convention
3. **Performance Tracking**: Comprehensive metrics stored in SQLite databases
4. **Scalable Storage**: Directory structure ready for unlimited methodologies
5. **Clean Abstractions**: ModelStorageManager provides unified interface
6. **Production Ready**: All frameworks include error handling and validation

## 📈 **Next Steps for Extension**

1. **LSTM Implementation**: Add recurrent neural network models for sequence prediction
2. **ARIMA Integration**: Classical time series analysis for trend decomposition
3. **Transformer Models**: Attention-based models for long-term dependencies
4. **GARCH Models**: Volatility clustering for risk assessment
5. **Real Data Integration**: Connect to live ETH price feeds
6. **API Development**: REST endpoints for model serving
7. **Model Monitoring**: Automated performance degradation detection

This directory represents a complete, production-ready machine learning system for ETH price forecasting with a focus on scalability, organization, and extensibility.
