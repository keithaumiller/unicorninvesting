# Alpha Models Architecture

This document defines the standardized directory structure for alpha models in the unicorn investing platform.

## Directory Structure

```
2_alpha_models/
├── ARCHITECTURE.md                    # This file - architecture documentation
├── README.md                         # Overall project documentation
├── __init__.py                       # Package initialization
├── 
├── ASSET_CLASSES/                    # Asset class directories
│   ├── CRYPTO/                       # Cryptocurrency assets
│   │   ├── ETH/                      # Ethereum-specific models
│   │   │   ├── algorithms/           # LEAN algorithms
│   │   │   ├── models/               # Alpha models (technical, prophet, xgboost, ensemble)
│   │   │   ├── tests/                # Test suites
│   │   │   ├── scripts/              # Utility scripts
│   │   │   ├── research/             # Research notebooks/analysis
│   │   │   └── features/             # Feature engineering
│   │   ├── BTC/                      # Bitcoin-specific models
│   │   │   ├── algorithms/           # LEAN algorithms
│   │   │   ├── models/               # Alpha models
│   │   │   ├── tests/                # Test suites
│   │   │   ├── scripts/              # Utility scripts
│   │   │   ├── research/             # Research notebooks/analysis
│   │   │   ├── features/             # Feature engineering
│   │   │   ├── *.pkl                 # Trained model files
│   │   │   └── *_report.txt          # Validation reports
│   │   └── [OTHER_CRYPTO]/           # Additional crypto assets
│   ├── EQUITIES/                     # Equity assets
│   │   ├── [STOCK_SYMBOL]/           # Individual stock models
│   │   └── [SECTOR]/                 # Sector-based models
│   ├── FOREX/                        # Foreign exchange assets
│   │   ├── [CURRENCY_PAIR]/          # Currency pair models
│   │   └── [REGION]/                 # Regional models
│   └── [OTHER_ASSET_CLASS]/          # Additional asset classes
│
├── shared/                           # Shared framework components
│   ├── base_alpha.py                 # Base alpha model classes
│   ├── testing_framework.py          # Comprehensive testing utilities
│   ├── model_framework.py            # Multi-model development framework
│   ├── performance_tracker.py        # Performance tracking system
│   └── simple_alpha_test.py          # Simple test utilities
│
├── utils/                            # Utility tools
│   ├── asset_template_generator.py   # Basic asset template generator
│   ├── enhanced_asset_generator.py   # Enhanced template generator
│   └── __init__.py
│
├── examples/                         # Example implementations
├── legacy/                           # Legacy code (deprecated)
└── model_performance.db              # SQLite performance database
```

## Asset Organization Rules

### 1. Asset Class Hierarchy
- **Top Level**: Asset classes (CRYPTO, EQUITIES, FOREX, etc.)
- **Second Level**: Specific assets or groupings within that class
- **Third Level**: Model components (algorithms, models, tests, scripts, research, features)

### 2. Asset Naming Conventions
- **Asset Classes**: UPPERCASE (CRYPTO, EQUITIES, FOREX)
- **Asset Names**: UPPERCASE for symbols (BTC, ETH, AAPL)
- **File Names**: lowercase_with_underscores for Python files
- **Model Files**: {asset_name}_{model_type}.py (btc_alpha.py, eth_prophet.py)

### 3. Required Subdirectories for Each Asset
Each asset directory MUST contain:
- `algorithms/` - LEAN trading algorithms
- `models/` - Alpha models (technical, prophet, xgboost, ensemble)
- `tests/` - Comprehensive test suites
- `scripts/` - Model building and validation scripts
- `research/` - Research analysis and notebooks
- `features/` - Feature engineering modules

### 4. Model Types Per Asset
Each asset should implement:
- **Technical Alpha Model**: Traditional technical analysis
- **Prophet Model**: Time series forecasting
- **XGBoost Model**: Gradient boosting prediction
- **Ensemble Model**: Combined approach
- **LEAN Algorithm**: Production trading algorithm

## File Naming Standards

### Model Files
- `{asset}_alpha.py` - Technical alpha model
- `{asset}_prophet.py` - Prophet forecasting model
- `{asset}_xgboost.py` - XGBoost prediction model
- `{asset}_ensemble.py` - Ensemble model
- `{asset}_algorithm.py` - LEAN algorithm

### Test Files
- `test_{asset}_models.py` - Comprehensive model tests
- `test_{asset}_integration.py` - Integration tests

### Script Files
- `{asset}_model_builder.py` - Model training script
- `{asset}_validation.py` - Model validation script
- `{asset}_research.py` - Research analysis script

### Output Files
- `{asset}_{model_type}_model.pkl` - Trained model files
- `{asset}_validation_report.txt` - Validation reports
- `{asset}_performance_metrics.json` - Performance metrics

## Framework Integration

### Shared Components
All asset models should inherit from and utilize:
- `shared.base_alpha.TechnicalAlphaModel` - Base technical alpha class
- `shared.model_framework.ProphetModel` - Prophet model framework
- `shared.model_framework.XGBoostModel` - XGBoost model framework
- `shared.model_framework.EnsembleModel` - Ensemble model framework
- `shared.testing_framework.BaseModelTester` - Testing framework
- `shared.performance_tracker.ModelPerformanceTracker` - Performance tracking

### Template Generation
Use the enhanced asset generator for new assets:
```python
from utils.enhanced_asset_generator import EnhancedAssetTemplateGenerator
generator = EnhancedAssetTemplateGenerator()
generator.create_asset_structure('NEW_ASSET', 'ASSET_CLASS')
```

## Validation Requirements

### Before Committing
1. All models must pass comprehensive tests
2. Models must be trained and validated
3. Performance metrics must be generated
4. Documentation must be updated
5. Architecture compliance must be verified

### Testing Standards
- Unit tests for all model classes
- Integration tests for complete workflows
- Performance benchmarking
- Validation against historical data

## Migration Guidelines

### Adding New Assets
1. Use `EnhancedAssetTemplateGenerator` to create structure
2. Implement required model types
3. Run comprehensive validation
4. Update documentation
5. Commit with detailed message

### Modifying Existing Assets
1. Maintain backward compatibility
2. Update tests for changes
3. Re-validate model performance
4. Update documentation
5. Follow git commit standards

## Enforcement

This architecture is enforced through:
- Automated testing in CI/CD
- Code review requirements
- Template generators
- Documentation standards
- Git commit hooks (future)

Any deviations from this structure must be documented and approved through the architecture review process.
