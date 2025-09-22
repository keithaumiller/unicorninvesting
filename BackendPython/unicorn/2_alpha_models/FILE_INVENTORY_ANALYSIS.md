# File Inventory Analysis - Alpha Models Migration

## 📋 **CURRENT FILE INVENTORY**

This analysis maps every existing file to its target location in the methodology-first architecture.

### **🔍 Asset-First Directories (TO BE MIGRATED)**

#### **CRYPTO Directory**
```
/BackendPython/unicorn/2_alpha_models/CRYPTO/
├── __init__.py                               → /assets/crypto/__init__.py
├── simple_model_demo.py                      → /examples/asset_examples/crypto_example.py
├── multi_asset_model_generator.py            → /methodologies/ensemble/core/ensemble_methodology.py
├── executive_report.py                       → /scripts/utilities/data_diagnostics.py
├── model_benchmark_framework.py              → /core/validation/performance_metrics.py
├── enhanced_crypto_prophet_builder.py        → /methodologies/prophet/adapters/crypto_adapter.py
├── crypto_model_validator.py                 → /methodologies/prophet/core/validation.py
├── query_results.py                          → /scripts/utilities/model_diagnostics.py
├── enhanced_crypto_prophet_builder_broken.py → /legacy/deprecated_builders/
│
├── BTC/                                      → Split between /assets/crypto/btc_adapter.py + /methodologies/*/adapters/
│   ├── __init__.py
│   ├── btc_production_framework.py          → /assets/crypto/btc_adapter.py
│   ├── btc_ensemble_economic_enhanced.py    → /methodologies/ensemble/adapters/crypto_adapter.py
│   ├── btc_portfolio_integration.py         → /assets/crypto/btc_adapter.py
│   ├── btc_xgboost_economic_enhanced.py     → /methodologies/xgboost/adapters/crypto_adapter.py
│   │
│   ├── models/                               → /storage/artifacts/*/crypto/
│   │   ├── __init__.py
│   │   ├── btc_xgboost.py                    → /methodologies/xgboost/core/xgboost_methodology.py
│   │   ├── btc_prophet.py                    → /methodologies/prophet/core/prophet_methodology.py
│   │   ├── btc_ensemble_model.py             → /methodologies/ensemble/core/ensemble_methodology.py
│   │   ├── btc_prophet_model.py              → /methodologies/prophet/adapters/crypto_adapter.py
│   │   ├── btc_alpha.py                      → /assets/crypto/btc_adapter.py
│   │   ├── btc_ensemble.py                   → /methodologies/ensemble/adapters/crypto_adapter.py
│   │   └── btc_xgboost_model.py              → /methodologies/xgboost/adapters/crypto_adapter.py
│   │
│   ├── tests/                                → /tests/unicorn/2_alpha_models/assets/crypto/
│   │   ├── __init__.py
│   │   └── test_btc_models.py                → /tests/unicorn/2_alpha_models/assets/crypto/test_btc_adapter.py
│   │
│   ├── algorithms/                           → /assets/crypto/
│   │   ├── __init__.py
│   │   └── btc_algorithm.py                  → /assets/crypto/btc_adapter.py
│   │
│   └── scripts/                              → /scripts/training/
│       ├── __init__.py
│       ├── btc_validation.py                 → /scripts/training/model_comparison.py
│       └── btc_model_builder.py              → /legacy/deprecated_builders/
│
└── ETH/                                      → Split between /assets/crypto/eth_adapter.py + /methodologies/*/adapters/
    ├── eth_forecast_generator.py             → /assets/crypto/eth_adapter.py
    ├── eth_xgboost_framework.py              → /methodologies/xgboost/adapters/crypto_adapter.py
    ├── test_prophet_framework.py             → /tests/unicorn/2_alpha_models/methodologies/prophet/
    ├── production_model_manager.py           → /core/orchestration/model_orchestrator.py
    ├── prophet_config.py                     → /methodologies/prophet/configs/crypto_overrides.json
    ├── eth_xgboost_economic_enhanced.py      → /methodologies/xgboost/adapters/crypto_adapter.py
    ├── eth_forecast_scheduler.py             → /core/orchestration/forecast_coordinator.py
    │
    ├── utilities/                            → /scripts/utilities/
    │   ├── __init__.py
    │   └── eth_model_builder.py               → /legacy/deprecated_builders/
    │
    ├── config/                               → /methodologies/prophet/configs/
    │   └── prophet_config.py                 → /methodologies/prophet/configs/crypto_overrides.json
    │
    └── demos/                                → /examples/asset_examples/
        └── __init__.py
```

#### **FOREX Directory**
```
/BackendPython/unicorn/2_alpha_models/FOREX/
├── __init__.py                               → /assets/forex/__init__.py
│
├── economic_enhanced_xgboost/                → /methodologies/xgboost/adapters/
│   └── eurusd_alpha_model.py                 → /methodologies/xgboost/adapters/forex_adapter.py
│
├── EURUSD/models/                            → Split between methodologies
│   ├── __init__.py
│   ├── eurusd_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
│   ├── eurusd_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
│   └── eurusd_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
│
├── GBPUSD/models/                            → Split between methodologies
│   ├── __init__.py
│   ├── gbpusd_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
│   ├── gbpusd_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
│   └── gbpusd_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
│
├── AUDUSD/models/                            → Split between methodologies
│   ├── __init__.py
│   ├── audusd_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
│   ├── audusd_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
│   └── audusd_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
│
├── USDJPY/models/                            → Split between methodologies
│   ├── __init__.py
│   ├── usdjpy_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
│   ├── usdjpy_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
│   └── usdjpy_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
│
├── USDCHF/models/                            → Split between methodologies
│   ├── __init__.py
│   ├── usdchf_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
│   ├── usdchf_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
│   └── usdchf_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
│
├── USDCAD/models/                            → Split between methodologies
│   ├── __init__.py
│   ├── usdcad_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
│   ├── usdcad_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
│   └── usdcad_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
│
└── NZDUSD/models/                            → Split between methodologies
    ├── __init__.py
    ├── nzdusd_xgboost_model.py               → /methodologies/xgboost/adapters/forex_adapter.py
    ├── nzdusd_ensemble_model.py              → /methodologies/ensemble/adapters/forex_adapter.py
    └── nzdusd_prophet_model.py               → /methodologies/prophet/adapters/forex_adapter.py
```

#### **EQUITIES Directory**
```
/BackendPython/unicorn/2_alpha_models/EQUITIES/
└── __init__.py                               → /assets/equities/__init__.py
```

### **🔧 Root-Level Files (TO BE REORGANIZED)**

#### **Builder Scripts (TO LEGACY)**
```
enhanced_forex_prophet_builder.py            → /legacy/deprecated_builders/
enhanced_forex_prophet_model_builder.py      → /legacy/deprecated_builders/
enhanced_prophet_model_builder.py            → /legacy/deprecated_builders/
fixed_multi_asset_model_builder.py           → /legacy/deprecated_builders/
multi_asset_model_builder.py                 → /legacy/deprecated_builders/
multi_asset_model_builder_fixed.py           → /legacy/deprecated_builders/
individual_asset_model_generator.py          → /legacy/deprecated_builders/
```

#### **Validation Scripts**
```
validate_architecture.py                     → /scripts/utilities/system_health.py
forex_model_validator.py                     → /methodologies/prophet/core/validation.py
```

### **📁 Directory-Level Migrations**

#### **enhanced_prophet_models/**
```
enhanced_prophet_models/                     → /methodologies/prophet/
├── [All Prophet-specific code and models]   → /methodologies/prophet/core/
└── [Trained models]                         → /storage/artifacts/prophet/
```

#### **multi_asset_models/**
```
multi_asset_models/                          → /methodologies/ensemble/
├── [Ensemble methodology code]              → /methodologies/ensemble/core/
└── [Ensemble models]                        → /storage/artifacts/ensemble/
```

#### **fixed_multi_asset_models/**
```
fixed_multi_asset_models/                    → /methodologies/ensemble/ (merge)
├── [Fixed ensemble code]                    → /methodologies/ensemble/core/ (merge)
└── [Fixed models]                           → /storage/artifacts/ensemble/ (merge)
```

#### **shared/**
```
shared/                                      → /core/ (merge)
├── [Shared utilities]                       → /core/validation/ or /scripts/utilities/
└── [Common interfaces]                      → /core/interfaces/ (already exists)
```

#### **utils/**
```
utils/                                       → /scripts/utilities/
├── __init__.py                              → /scripts/utilities/__init__.py
├── asset_template_generator.py              → /scripts/utilities/data_diagnostics.py
└── enhanced_asset_generator.py              → /scripts/utilities/model_diagnostics.py
```

#### **scripts/**
```
scripts/                                     → /scripts/training/
├── model_performance_manager_v2.py          → /storage/performance/performance_database.py
├── EthFocusedAlpha.py                       → /examples/asset_examples/crypto_example.py
├── model_performance_manager.py             → /storage/performance/benchmark_tracker.py
├── comprehensive_model_retraining_fixed.py  → /scripts/training/batch_training.py
├── AdvancedForexForecastingAlpha.py         → /examples/asset_examples/forex_example.py
├── comprehensive_model_retraining.py        → /scripts/training/batch_training.py (merge)
├── performance_summary.py                   → /storage/performance/model_comparison.py
└── performance_tools.py                     → /scripts/utilities/model_diagnostics.py
```

#### **examples/**
```
examples/                                    → /examples/ (restructure)
├── [Asset-specific examples]                → /examples/asset_examples/
├── [Methodology-specific examples]          → /examples/methodology_examples/
└── [Integration examples]                   → /examples/integration_examples/
```

#### **performance_analysis/**
```
performance_analysis/                        → /storage/performance/
├── [Performance analysis code]              → /storage/performance/benchmark_tracker.py
└── [Performance data]                       → /storage/performance/performance_database.py
```

#### **validation_results/**
```
validation_results/                          → /core/validation/
├── [Validation utilities]                   → /core/validation/cross_validation.py
└── [Validation data]                        → /storage/performance/ (merge)
```

### **📊 MIGRATION STATISTICS**

| Category | Current Count | Target Locations | Complexity |
|----------|---------------|------------------|------------|
| **Asset-First Directories** | 3 main (CRYPTO, FOREX, EQUITIES) | Split across 6+ methodology adapters | HIGH |
| **Individual Model Files** | ~30 model files | Reorganized by methodology type | MEDIUM |
| **Builder Scripts** | 7 builder files | Move to legacy (deprecated) | LOW |
| **Utility Scripts** | ~15 utility files | Reorganize by function | MEDIUM |
| **Configuration Files** | Scattered configs | Centralized in methodology configs | MEDIUM |
| **Test Files** | Asset-specific tests | Methodology and adapter tests | MEDIUM |
| **Example Files** | Mixed examples | Categorized examples | LOW |
| **Performance Data** | Multiple locations | Unified storage framework | HIGH |

### **🎯 MIGRATION COMPLEXITY ANALYSIS**

#### **HIGH COMPLEXITY (Requires Code Refactoring)**
- **Asset model files**: Need to extract methodology logic from asset-specific implementations
- **Ensemble models**: Complex coordination logic needs abstraction
- **Performance tracking**: Distributed across multiple files, needs consolidation

#### **MEDIUM COMPLEXITY (Structural Reorganization)**
- **Configuration management**: Consolidate scattered config files
- **Utility scripts**: Categorize by function and eliminate duplicates
- **Test reorganization**: Restructure tests to match new architecture

#### **LOW COMPLEXITY (Simple File Moves)**
- **Builder scripts**: Move to legacy without modification
- **Example files**: Reorganize without code changes
- **Documentation**: Update paths and references

---

**Next Step**: Begin systematic migration starting with creating the new directory structure and moving files to appropriate locations.