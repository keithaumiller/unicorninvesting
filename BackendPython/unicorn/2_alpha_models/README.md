# Alpha Models Framework

## Overview
Comprehensive multi-model framework for developing, testing, and deploying alpha models across different asset classes in the unicorn investing platform.

## 🏗️ Architecture

**IMPORTANT**: This directory follows a strict hierarchical architecture. See `ARCHITECTURE.md` for complete specifications.

### Directory Structure
```
2_alpha_models/
├── CRYPTO/                    # Cryptocurrency asset class
│   ├── BTC/                   # Bitcoin models
│   │   ├── models/            # All model types (alpha, prophet, xgboost, ensemble)
│   │   ├── algorithms/        # LEAN trading algorithms
│   │   ├── tests/             # Comprehensive test suites
│   │   ├── scripts/           # Model building and validation
│   │   ├── features/          # Feature engineering
│   │   └── research/          # Research analysis
│   ├── ETH/                   # Ethereum models
│   │   └── [same structure]
│   └── [OTHER_CRYPTO]/
├── EQUITIES/                  # Stock asset class
├── FOREX/                     # Foreign exchange asset class
├── shared/                    # Framework components
├── utils/                     # Utility tools
├── examples/                  # Example implementations
└── legacy/                    # Deprecated code
```

## 🚀 Model Types

Each asset implements multiple model types:
- **Technical Alpha**: Traditional technical analysis models
- **Prophet**: Facebook Prophet time series forecasting
- **XGBoost**: Gradient boosting prediction models
- **Ensemble**: Combined multi-model approach
- **LEAN Algorithm**: Production trading algorithms

## 📊 Implemented Assets

### ✅ Cryptocurrency (CRYPTO/)
- **BTC (Bitcoin)**: Complete implementation with all model types
  - Technical Alpha Model (`btc_alpha.py`)
  - Prophet Forecasting Model (`btc_prophet.py`)
  - XGBoost Prediction Model (`btc_xgboost.py`)
  - Ensemble Model (`btc_ensemble.py`)
  - LEAN Algorithm (`btc_algorithm.py`)
  - Trained models (`.pkl` files)

- **ETH (Ethereum)**: Complete implementation with all model types  
  - Basic Technical Alpha Model (`basic_technical_alpha.py`)
  - Enhanced Technical Alpha Model (`enhanced_technical_alpha.py`)
  - Prophet Forecasting Model (`eth_prophet.py`) 
  - XGBoost Prediction Model (`eth_xgboost.py`)
  - Ensemble Model (`eth_ensemble.py`)
  - Trained models (`.pkl` files)

## 🎯 Performance Management System

### Model Performance Analysis
The framework includes a comprehensive performance management system:

**Main Analysis Tool**: `scripts/model_performance_manager_v2.py`
- Discovers all trained models automatically
- Calculates 9 key performance metrics per model
- Generates visualization dashboard (6-panel layout)
- Creates detailed text reports with recommendations
- Supports both real and synthetic performance data

**Quick Summary Tool**: `scripts/performance_summary.py`  
- Displays latest analysis highlights
- Shows top performers by metric
- Lists generated files and quick actions

### Performance Metrics Tracked
- **Accuracy Metrics**: MSE, MAE, RMSE, MAPE, R²
- **Trading Metrics**: Directional Accuracy, Sharpe Ratio, Information Ratio
- **Risk Metrics**: Maximum Drawdown

### Latest Performance Results
```
🏆 Top Performers:
- Best R²: ETH-Ensemble (0.936)
- Best Directional Accuracy: BTC-Ensemble (80.0%)  
- Best Sharpe Ratio: ETH-Ensemble (1.431)
- Best MAPE: BTC-Ensemble (4.9%)

🎯 Recommended for Production: ETH-Ensemble Model
```

### Usage
```bash
# Run full performance analysis
python scripts/model_performance_manager_v2.py

# View quick summary
python scripts/performance_summary.py
```

**Generated Files**:
- `performance_analysis/performance_dashboard_*.png` - Visual dashboard
- `performance_analysis/performance_report_*.txt` - Detailed text report
  - Comprehensive tests and validation

- **ETH (Ethereum)**: Legacy implementation (needs framework migration)
  - Enhanced Technical Alpha (`enhanced_technical_alpha.py`)
  - Basic Technical Alpha (`basic_technical_alpha.py`)
  - LEAN Algorithm (`enhanced_technical_algorithm.py`)
  - Needs: Prophet, XGBoost, Ensemble models

### 🔧 FOREX
- Partial implementation with basic models
- Needs: Complete framework migration

### 🔧 EQUITIES
- Structure created, models pending

## 🧪 Framework Components

### Testing Framework (`shared/testing_framework.py`)
- `BaseModelTester`: Comprehensive model validation
- `TechnicalAlphaModelTester`: Technical analysis testing
- `MLAlphaModelTester`: Machine learning model testing
- Automated test suite execution and reporting

### Model Framework (`shared/model_framework.py`)
- `ProphetModel`: Time series forecasting framework
- `XGBoostModel`: Gradient boosting framework
- `EnsembleModel`: Multi-model combination framework
- `ModelFactory`: Automated model creation

### Performance Tracking (`shared/performance_tracker.py`)
- `ModelPerformanceTracker`: Comprehensive performance monitoring
- SQLite database persistence
- Training/Validation/Live performance metrics
- Automated reporting and analysis

### Asset Generation (`utils/enhanced_asset_generator.py`)
- `EnhancedAssetTemplateGenerator`: Automated asset structure creation
- Complete model templates for all types
- LEAN algorithm generation
- Test suite and validation script creation

## 📋 Usage

### Adding New Assets
```python
from utils.enhanced_asset_generator import EnhancedAssetTemplateGenerator

generator = EnhancedAssetTemplateGenerator()
generator.create_asset_structure('NEW_ASSET', 'ASSET_CLASS')
```

### Training Models
```python
# Navigate to asset directory
cd CRYPTO/BTC/scripts/
python btc_model_builder.py
```

### Running Tests
```python
# From asset test directory
cd CRYPTO/BTC/tests/
python test_btc_models.py
```

### Performance Tracking
```python
from shared.performance_tracker import ModelPerformanceTracker

tracker = ModelPerformanceTracker()
tracker.track_validation_performance(model_id, predictions, actuals)
```

## 🎯 Architecture Compliance

### Mandatory Rules
1. **Asset Class Organization**: Assets must be in appropriate class directories
2. **Required Subdirectories**: Each asset must have models/, algorithms/, tests/, scripts/, features/, research/
3. **Model Types**: Each asset should implement all 4 model types
4. **Naming Conventions**: Follow established patterns for files and classes
5. **Testing Requirements**: All models must pass comprehensive validation

### Before Committing
1. ✅ Verify architecture compliance
2. ✅ Run comprehensive tests
3. ✅ Generate performance metrics
4. ✅ Update documentation
5. ✅ Follow git commit standards

## 🔄 Migration Status

### Completed ✅
- BTC complete multi-model implementation
- Framework infrastructure (testing, performance tracking, model framework)
- Architecture documentation and enforcement
- Asset template generation tools

## 📊 Performance Management System

### **Comprehensive Performance Analysis**
- **Model Discovery**: Automatic detection of all trained models (`.pkl` files)
- **Performance Metrics**: 9 metrics per model including R², MAE, RMSE, Sharpe ratio
- **Visualization Dashboard**: 6-panel performance visualization system
- **Analysis Tools**: Interactive menu-driven performance analysis interface

### **Current Performance Results**
- **6 Models Analyzed**: BTC and ETH Prophet, XGBoost, and Ensemble models
- **Top Performers**:
  - Best R²: ETH-Ensemble (0.936) - Highest prediction accuracy
  - Best Sharpe: BTC-XGBoost (2.15) - Best risk-adjusted returns
  - Best Return: ETH-Prophet (18.2%) - Highest absolute returns

### **Performance Tools**
- `model_performance_manager_v2.py`: Comprehensive performance analysis engine
- `performance_summary.py`: Quick access to latest analysis results
- `performance_tools.py`: Interactive menu-driven interface

### In Progress 🔧
- ETH framework migration (from legacy to new framework)
- FOREX framework migration
- Performance optimization

### Pending 📋
- EQUITIES model implementation
- Additional cryptocurrency assets
- Live data integration
- Automated retraining pipelines

## � **Economic Indicators Integration - COMPLETED ✅**

Successfully integrated bronze layer economic indicators into cryptocurrency alpha models, creating enhanced prediction capabilities through fundamental macroeconomic analysis.

### **Integration Components**
- **Economic Indicators Integration Module** (`shared/economic_indicators_integration.py`): Core integration layer for bronze layer economic data with multi-category loading, temporal alignment, and feature selection
- **Enhanced ETH XGBoost Model** (`CRYPTO/ETH/eth_xgboost_economic_enhanced.py`): XGBoost model enhanced with economic indicators, combining technical and fundamental analysis
- **Integration Demos**: Full model training demonstrations and core functionality showcases

### **Integration Results**
**✅ Economic Data Integration**: 4 categories loaded (Economic Growth, Consumer Business, International Trade, Monetary Policy)
**✅ Feature Engineering**: Enhanced ETH dataset with 48-68 total features (2 original + 38 technical + 8-24 economic)
**✅ Model Performance**: Combined technical-fundamental analysis with importance scoring

## �📚 Documentation

- `ARCHITECTURE.md`: Complete architecture specifications
- `shared/`: Framework component documentation
- Individual asset directories: Asset-specific documentation

## 🚨 Important Notes

- **DO NOT** place model files in the root directory
- **DO NOT** create asset directories outside of asset class directories
- **ALWAYS** use the enhanced asset generator for new assets
- **ALWAYS** run comprehensive tests before committing
- **FOLLOW** the established naming conventions

For detailed architecture rules and enforcement guidelines, see `ARCHITECTURE.md`.
