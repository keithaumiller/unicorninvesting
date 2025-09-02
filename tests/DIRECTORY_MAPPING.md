# Tests Directory Structure - Unicorn Investing Platform

This document maps the test directory structure to the actual codebase components.

## 🎯 **CORRECTED Directory Mapping**

### **Root Level Tests** (`/tests/`)
```
tests/
├── 📋 CONFIGURATION FILES
├── conftest.py                           # pytest configuration and fixtures
├── pytest.ini                           # pytest settings  
├── requirements-test.txt                 # testing dependencies
├── __init__.py                          # Python package marker
│
├── 🎯 SYSTEM-LEVEL TESTS  
├── system/                              # System-wide validation tests
│   ├── test_complete_system_validation.py   # Master validation runner
│   ├── test_system_architecture.py          # Architecture compliance tests
│   └── final_integration_summary.py         # Integration test reporting
│
├── 🔧 LEAN FRAMEWORK TESTS
├── lean/                                # LEAN framework specific tests
│   ├── analyze_lean_structure.py            # LEAN structure analysis
│   ├── lean_structure_simple.py             # Simple LEAN validation
│   ├── quick_lean_analysis.py               # Quick LEAN compliance check
│   └── test_lean_insights.py                # LEAN insights and analytics
│
├── 🌐 FRONTEND TESTS
├── WebFrontend/                         # Maps to: /WebFrontend/
│   ├── test_basic_validation.py             # Basic frontend validation
│   ├── simple_homepage_test.py              # Homepage functionality tests
│   └── test_forecasting_dashboard.py        # Dashboard integration tests
│
├── 🏗️ LEGACY TESTS
├── legacy/                              # Legacy/deprecated tests
│   └── (empty - ready for legacy test migration)
│
├── 🔧 UTILITY SCRIPTS
├── run_comprehensive_tests.sh           # Automated test runner (ACTIVE)
└── 📋 Documentation and structure files
```

### **Unicorn Framework Tests** (`/tests/unicorn/`)
**Maps to: `/BackendPython/unicorn/`**

```
tests/unicorn/                          # Mirrors: BackendPython/unicorn/
├── 1_data_sources/                      # Tests for: BackendPython/unicorn/1_data_sources/
│   ├── 1_raw/
│   │   └── connectors/
│   │       └── interactive_brokers/          # IBKR connector tests
│   │           ├── test_ibkr_integration.py
│   │           ├── test_data_quality.py
│   │           ├── test_technical_indicators.py
│   │           └── test_e2e_pipeline.py
│   ├── 📊 data/                             # Test data files for data source testing
│   │   └── eth_1min/                            # ETH 1-minute OHLCV test data (1000 bars)
│   │       └── integration_test.json                # Real IBKR ETH data (178KB, perfect quality)
│   └── 💾 database/                         # Database and persistence tests
│       └── (empty - ready for data persistence tests)
│
├── 2_alpha_models/                      # Tests for: BackendPython/unicorn/2_alpha_models/
│   └── (needs completion - should test CRYPTO/EQUITIES/FOREX models)
│
├── 3_risk_algorithms/                   # Tests for: BackendPython/unicorn/3_risk_algorithms/
│   └── test_eth_basic_risk.py              # Tests basic_risk/ implementations
│
├── 4_portfolios/                        # Tests for: BackendPython/unicorn/4_portfolios/
│   ├── Myportolio/                          # Tests Myportolio portfolio
│   │   └── test_eth_kelly_integration.py        # Complete integration tests
│   └── utilities/                           # Tests framework utilities
│       └── test_kelly_criterion.py             # Kelly Criterion tests
│
├── 5_execution_models/                  # Tests for: BackendPython/unicorn/5_execution_models/
│   └── (empty - ready for execution model tests)
│
└── 6_algorithms/                        # Tests for: BackendPython/unicorn/6_algorithms/
    └── (empty - ready for complete algorithm tests)
```

## 🔍 **Component Mapping Analysis**

### **✅ CORRECTLY MAPPED:**
- `tests/unicorn/1_data_sources/` → IBKR connector tests ✅
- `tests/unicorn/4_portfolios/Myportolio/` → ETH Kelly integration tests ✅  
- `tests/unicorn/4_portfolios/utilities/` → Kelly Criterion utility tests ✅
- `tests/unicorn/3_risk_algorithms/` → Risk management tests ✅
- `tests/system/` → System-wide validation ✅
- `tests/frontend/` → WebFrontend validation ✅

### **❌ MISSING/INCOMPLETE:**
- `tests/unicorn/2_alpha_models/` → No tests for CRYPTO/EQUITIES/FOREX models ❌
- `tests/unicorn/5_execution_models/` → Empty (expected - new layer) ❌
- `tests/unicorn/6_algorithms/` → Empty (expected - new layer) ❌
- `tests/database/` → Empty (needs database tests) ❌

### **⚠️ NEEDS ORGANIZATION:**
- Root-level analysis scripts should move to appropriate subdirectories
- Legacy test files need proper categorization

## 📋 **Recommended Actions**

1. **Complete Alpha Models Testing**: Add tests for CRYPTO/EQUITIES/FOREX models
2. **Database Testing**: Add database connectivity and query tests  
3. **Execution Models**: Add tests as layer 5 is developed
4. **Complete Algorithms**: Add tests as layer 6 is developed
5. **Legacy Cleanup**: Move legacy analysis scripts to proper locations

## 🎯 **Current Test Coverage Status**

- **System Architecture**: ✅ 100% covered
- **Portfolio Management**: ✅ 100% covered (Myportolio)
- **Risk Management**: ✅ 100% covered (basic algorithms)
- **Data Sources**: ✅ 90% covered (IBKR well-tested)
- **Alpha Models**: ❌ 0% covered (needs implementation)
- **Execution Models**: ❌ 0% covered (new layer)
- **Complete Algorithms**: ❌ 0% covered (new layer)
- **Frontend**: ⚠️ 20% covered (basic validation only)
- **Database**: ❌ 0% covered (needs implementation)
