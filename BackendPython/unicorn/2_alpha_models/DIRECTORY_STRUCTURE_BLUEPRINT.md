# Alpha Models Directory Structure - Methodology-First Architecture

## 🎯 **TARGET DIRECTORY STRUCTURE**

This is the optimal directory structure for the methodology-first architecture. All current files will be mapped to this structure.

```
/BackendPython/unicorn/2_alpha_models/
├── ARCHITECTURE.md                   # ✅ KEEP - Architecture documentation
├── README.md                         # ✅ KEEP - Main documentation  
├── __init__.py                       # ✅ KEEP - Package initialization
│
├── methodologies/                    # 🆕 NEW - Methodology-first organization
│   ├── __init__.py                   # 🆕 NEW
│   │
│   ├── prophet/                      # 🔄 MIGRATE from enhanced_prophet_models/
│   │   ├── __init__.py               # 🆕 NEW
│   │   ├── core/                     # 🆕 NEW - Core Prophet implementation
│   │   │   ├── __init__.py
│   │   │   ├── prophet_methodology.py    # 🆕 NEW - Main methodology class
│   │   │   ├── feature_engineering.py   # 🔄 MIGRATE from existing Prophet files
│   │   │   └── validation.py             # 🔄 MIGRATE from forex_model_validator.py
│   │   ├── adapters/                 # 🆕 NEW - Asset-specific adapters  
│   │   │   ├── __init__.py
│   │   │   ├── crypto_adapter.py         # 🔄 MIGRATE from CRYPTO/
│   │   │   ├── forex_adapter.py          # 🔄 MIGRATE from FOREX/
│   │   │   └── equity_adapter.py         # 🔄 MIGRATE from EQUITIES/
│   │   ├── configs/                  # 🆕 NEW - Methodology configurations
│   │   │   ├── default_config.json       # 🔄 MIGRATE from existing configs
│   │   │   ├── crypto_overrides.json     
│   │   │   └── forex_overrides.json      
│   │   └── models/                   # 🔄 MIGRATE trained models
│   │       ├── crypto/                   
│   │       ├── forex/                    
│   │       └── metadata/                 
│   │
│   ├── xgboost/                      # 🆕 NEW - XGBoost methodology
│   │   ├── __init__.py
│   │   ├── core/                     # 🆕 NEW
│   │   │   ├── __init__.py
│   │   │   ├── xgboost_methodology.py   
│   │   │   ├── feature_engineering.py   
│   │   │   └── validation.py             
│   │   ├── adapters/                 # 🆕 NEW
│   │   │   ├── __init__.py
│   │   │   ├── crypto_adapter.py         
│   │   │   ├── forex_adapter.py          
│   │   │   └── equity_adapter.py         
│   │   ├── configs/                  # 🆕 NEW
│   │   │   ├── default_config.json       
│   │   │   ├── crypto_overrides.json     
│   │   │   └── forex_overrides.json      
│   │   └── models/                   # 🆕 NEW
│   │       ├── crypto/                   
│   │       ├── forex/                    
│   │       └── metadata/                 
│   │
│   ├── ensemble/                     # 🔄 MIGRATE from multi_asset_models/
│   │   ├── __init__.py
│   │   ├── core/                     # 🔄 MIGRATE ensemble logic
│   │   │   ├── __init__.py
│   │   │   ├── ensemble_methodology.py  # 🔄 MIGRATE from multi_asset_model_builder.py
│   │   │   ├── combination_strategies.py 
│   │   │   └── validation.py             
│   │   ├── adapters/                 # 🆕 NEW
│   │   │   ├── __init__.py
│   │   │   ├── crypto_adapter.py         
│   │   │   ├── forex_adapter.py          
│   │   │   └── equity_adapter.py         
│   │   ├── configs/                  # 🆕 NEW
│   │   │   ├── default_config.json       
│   │   │   ├── crypto_overrides.json     
│   │   │   └── forex_overrides.json      
│   │   └── models/                   # 🔄 MIGRATE existing ensemble models
│   │       ├── crypto/                   
│   │       ├── forex/                    
│   │       └── metadata/                 
│   │
│   └── lstm/                         # 🆕 NEW - Future expansion
│       ├── __init__.py
│       ├── core/
│       ├── adapters/
│       ├── configs/
│       └── models/
│
├── assets/                           # 🆕 NEW - Asset adapter framework
│   ├── __init__.py                   # 🆕 NEW
│   ├── base_adapter.py               # 🆕 NEW - Abstract asset adapter
│   │
│   ├── crypto/                       # 🔄 RESTRUCTURE from CRYPTO/
│   │   ├── __init__.py
│   │   ├── crypto_adapter.py             # 🔄 MIGRATE from CRYPTO/
│   │   ├── eth_adapter.py                # 🔄 MIGRATE ETH-specific logic
│   │   ├── btc_adapter.py                # 🔄 MIGRATE BTC-specific logic
│   │   └── market_characteristics.py    # 🆕 NEW - Crypto market behavior
│   │
│   ├── forex/                        # 🔄 RESTRUCTURE from FOREX/
│   │   ├── __init__.py
│   │   ├── forex_adapter.py              # 🔄 MIGRATE from FOREX/
│   │   ├── major_pairs_adapter.py        # 🔄 MIGRATE forex logic
│   │   ├── minor_pairs_adapter.py        # 🔄 MIGRATE forex logic
│   │   └── market_characteristics.py    # 🆕 NEW - Forex market behavior
│   │
│   ├── equities/                     # 🔄 RESTRUCTURE from EQUITIES/
│   │   ├── __init__.py
│   │   ├── equity_adapter.py             # 🔄 MIGRATE from EQUITIES/
│   │   ├── individual_stocks.py          # 🔄 MIGRATE equity logic
│   │   ├── sectors.py                    # 🆕 NEW - Sector logic
│   │   └── market_characteristics.py    # 🆕 NEW - Equity market behavior
│   │
│   └── commodities/                  # 🆕 NEW - Future expansion
│       ├── __init__.py
│       ├── commodity_adapter.py
│       └── market_characteristics.py
│
├── core/                             # ✅ KEEP/EXPAND - Shared framework
│   ├── __init__.py                   # ✅ KEEP
│   │
│   ├── interfaces/                   # ✅ KEEP - Already implemented
│   │   ├── __init__.py               # ✅ KEEP
│   │   ├── methodology_interface.py      # ✅ KEEP - Already implemented
│   │   ├── asset_adapter_interface.py   # ✅ KEEP - Already implemented
│   │   ├── model_interface.py            # ✅ KEEP - Already implemented
│   │   └── data_interfaces.py            # ✅ KEEP - Already implemented
│   │
│   ├── data_pipeline/                # ✅ KEEP/EXPAND - Already started
│   │   ├── __init__.py               # ✅ KEEP - Already implemented
│   │   ├── data_loader.py                # ✅ KEEP - Already implemented
│   │   ├── data_normalizer.py            # ✅ KEEP - Already implemented
│   │   └── feature_pipeline.py          # 🆕 NEW - Need to implement
│   │
│   ├── validation/                   # 🔄 MIGRATE from utils/, examples/
│   │   ├── __init__.py               # 🆕 NEW
│   │   ├── performance_metrics.py        # 🔄 MIGRATE from utils/
│   │   ├── cross_validation.py           # 🔄 MIGRATE from examples/
│   │   └── backtesting.py                # 🔄 MIGRATE from validation_results/
│   │
│   ├── orchestration/                # 🆕 NEW - Multi-methodology coordination
│   │   ├── __init__.py               # 🆕 NEW
│   │   ├── model_orchestrator.py         # 🆕 NEW - Main orchestration engine
│   │   ├── training_coordinator.py       # 🆕 NEW - Training coordination
│   │   └── forecast_coordinator.py       # 🆕 NEW - Forecast coordination
│   │
│   └── configuration/                # ✅ KEEP/EXPAND - Already implemented
│       ├── __init__.py               # ✅ KEEP
│       ├── config_manager.py             # ✅ KEEP - Already implemented
│       ├── methodology_config.py         # ✅ KEEP - Already implemented
│       └── asset_config.py               # ✅ KEEP - Already implemented
│
├── storage/                          # ✅ KEEP/EXPAND - Already implemented
│   ├── __init__.py                   # ✅ KEEP
│   │
│   ├── metadata/                     # ✅ KEEP - Already implemented
│   │   ├── model_registry.py             # ✅ KEEP - Already implemented
│   │   ├── performance_tracker.py        # ✅ KEEP - Already implemented
│   │   └── model_metadata.db             # ✅ KEEP - Database file
│   │
│   ├── performance/                  # ✅ KEEP/EXPAND - Already implemented
│   │   ├── performance_database.py       # 🔄 MIGRATE from model_performance.db logic
│   │   ├── benchmark_tracker.py          # 🔄 MIGRATE from performance_analysis/
│   │   └── model_comparison.py           # 🔄 MIGRATE from performance_analysis/
│   │
│   └── artifacts/                    # 🔄 REORGANIZE existing model storage
│       ├── prophet/                      # 🔄 MIGRATE Prophet model files
│       ├── xgboost/                      # 🔄 MIGRATE XGBoost model files
│       ├── ensemble/                     # 🔄 MIGRATE ensemble model files
│       └── metadata/                     # 🔄 MIGRATE artifact metadata
│
├── examples/                         # 🔄 RESTRUCTURE - Usage examples
│   ├── __init__.py                   # 🆕 NEW
│   ├── methodology_examples/         # 🔄 RESTRUCTURE from examples/
│   │   ├── prophet_example.py            # 🔄 MIGRATE from examples/
│   │   ├── xgboost_example.py            # 🔄 MIGRATE from examples/
│   │   └── ensemble_example.py           # 🔄 MIGRATE from examples/
│   ├── asset_examples/               # 🆕 NEW
│   │   ├── crypto_example.py             # 🔄 MIGRATE from examples/
│   │   ├── forex_example.py              # 🔄 MIGRATE from examples/
│   │   └── equity_example.py             # 🔄 MIGRATE from examples/
│   └── integration_examples/         # 🆕 NEW
│       ├── full_pipeline_example.py      # 🆕 NEW
│       └── multi_asset_example.py        # 🔄 MIGRATE from examples/
│
├── scripts/                          # ✅ KEEP/REORGANIZE - Utility scripts
│   ├── __init__.py                   # 🆕 NEW
│   ├── migration/                    # 🆕 NEW - Migration utilities
│   │   ├── migrate_existing_models.py   # 🆕 NEW - Model migration script
│   │   ├── validate_migration.py        # 🆕 NEW - Migration validation
│   │   └── cleanup_legacy.py            # 🆕 NEW - Legacy cleanup
│   ├── training/                     # 🔄 MIGRATE from scripts/
│   │   ├── batch_training.py             # 🔄 MIGRATE from scripts/
│   │   ├── model_comparison.py           # 🔄 MIGRATE from scripts/
│   │   └── performance_analysis.py       # 🔄 MIGRATE from scripts/
│   └── utilities/                    # 🔄 MIGRATE from utils/
│       ├── data_diagnostics.py           # 🔄 MIGRATE from utils/
│       ├── model_diagnostics.py          # 🔄 MIGRATE from utils/
│       └── system_health.py              # 🆕 NEW
│
├── tests/                            # 🔄 MIGRATE to /tests/unicorn/2_alpha_models/
│   └── [MOVED TO ROOT TESTS DIRECTORY]  # Follow instructions compliance
│
├── docs/                             # 🆕 NEW - Enhanced documentation
│   ├── methodology_guides/           # 🆕 NEW - Methodology-specific docs
│   │   ├── prophet_guide.md              # 🆕 NEW
│   │   ├── xgboost_guide.md              # 🆕 NEW
│   │   └── ensemble_guide.md             # 🆕 NEW
│   ├── asset_guides/                 # 🆕 NEW - Asset-specific docs
│   │   ├── crypto_guide.md               # 🆕 NEW
│   │   ├── forex_guide.md                # 🆕 NEW
│   │   └── equity_guide.md               # 🆕 NEW
│   ├── api_reference/                # 🆕 NEW - API documentation
│   │   ├── interfaces.md                 # 🆕 NEW
│   │   ├── data_pipeline.md              # 🆕 NEW
│   │   └── storage.md                    # 🆕 NEW
│   └── migration_guide.md            # 🆕 NEW - Migration documentation
│
└── legacy/                           # 🔄 PRESERVE - Legacy files during transition
    ├── CRYPTO/                       # 🔄 MOVE HERE temporarily during migration
    ├── FOREX/                        # 🔄 MOVE HERE temporarily during migration
    ├── EQUITIES/                     # 🔄 MOVE HERE temporarily during migration
    ├── enhanced_prophet_models/      # 🔄 MOVE HERE temporarily during migration
    ├── multi_asset_models/           # 🔄 MOVE HERE temporarily during migration
    ├── fixed_multi_asset_models/     # 🔄 MOVE HERE temporarily during migration
    └── deprecated_builders/          # 🔄 MOVE builder scripts here
        ├── enhanced_forex_prophet_builder.py
        ├── enhanced_forex_prophet_model_builder.py
        ├── enhanced_prophet_model_builder.py
        ├── fixed_multi_asset_model_builder.py
        ├── multi_asset_model_builder.py
        ├── multi_asset_model_builder_fixed.py
        └── individual_asset_model_generator.py
```

## 📋 **MIGRATION MAPPING TABLE**

| Current Location | Target Location | Migration Type | Priority |
|------------------|----------------|----------------|----------|
| `/CRYPTO/` | `/assets/crypto/` + `/methodologies/*/adapters/crypto_adapter.py` | SPLIT | HIGH |
| `/FOREX/` | `/assets/forex/` + `/methodologies/*/adapters/forex_adapter.py` | SPLIT | HIGH |
| `/EQUITIES/` | `/assets/equities/` + `/methodologies/*/adapters/equity_adapter.py` | SPLIT | HIGH |
| `/enhanced_prophet_models/` | `/methodologies/prophet/` | RESTRUCTURE | HIGH |
| `/multi_asset_models/` | `/methodologies/ensemble/` | RESTRUCTURE | HIGH |
| `/fixed_multi_asset_models/` | `/methodologies/ensemble/` | MERGE | MEDIUM |
| `/enhanced_*_builder.py` | `/legacy/deprecated_builders/` | MOVE | LOW |
| `/multi_asset_model_builder*.py` | `/legacy/deprecated_builders/` | MOVE | LOW |
| `/individual_asset_model_generator.py` | `/legacy/deprecated_builders/` | MOVE | LOW |
| `/forex_model_validator.py` | `/methodologies/prophet/core/validation.py` | REFACTOR | MEDIUM |
| `/shared/` | `/core/` (merge) | MERGE | MEDIUM |
| `/utils/` | `/scripts/utilities/` | MOVE | MEDIUM |
| `/examples/` | `/examples/` (restructure) | RESTRUCTURE | LOW |
| `/performance_analysis/` | `/storage/performance/` | MOVE | MEDIUM |
| `/validation_results/` | `/core/validation/` | MERGE | MEDIUM |
| `/model_performance.db` | `/storage/metadata/` | MOVE | HIGH |

## 🎯 **MIGRATION PHASES**

### **Phase 1: Core Framework (COMPLETE)**
- ✅ Core interfaces implemented
- ✅ Configuration management implemented  
- ✅ Storage framework implemented
- ✅ Data pipeline foundation implemented

### **Phase 2: Directory Structure Setup (NEXT)**
- 🔄 Create new directory structure
- 🔄 Move legacy files to `/legacy/`
- 🔄 Set up new package structure

### **Phase 3: Asset Adapter Migration**
- 🔄 Implement base asset adapter
- 🔄 Migrate crypto adapter logic
- 🔄 Migrate forex adapter logic
- 🔄 Migrate equity adapter logic

### **Phase 4: Methodology Migration**
- 🔄 Migrate Prophet methodology
- 🔄 Migrate ensemble methodology  
- 🔄 Implement XGBoost methodology
- 🔄 Update all configurations

### **Phase 5: Integration & Testing**
- 🔄 Integration testing
- 🔄 Performance validation
- 🔄 Documentation completion
- 🔄 Legacy cleanup

## 📊 **FILE COUNT ANALYSIS**

**Current Structure:** ~50+ files scattered across asset-first directories
**Target Structure:** ~100+ files organized in methodology-first hierarchy
**Migration Complexity:** Medium-High (requires code refactoring)
**Estimated Timeline:** 2-3 development sessions

---

**Next Step:** Systematically iterate through each current directory and file to determine exact migration path and required code changes.