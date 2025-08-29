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

### In Progress 🔧
- ETH framework migration (from legacy to new framework)
- FOREX framework migration
- Performance optimization

### Pending 📋
- EQUITIES model implementation
- Additional cryptocurrency assets
- Live data integration
- Automated retraining pipelines

## 📚 Documentation

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
