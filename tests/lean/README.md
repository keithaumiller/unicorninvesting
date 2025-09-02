# LEAN Framework Testing Suite

This directory contains tests and analysis tools specific to the QuantConnect LEAN framework integration.

## 🎯 **Purpose**

The LEAN framework provides a 6-layer algorithmic trading architecture. This test suite validates:
- LEAN framework integration and compliance
- Architecture layer structure (Data → Alpha → Risk → Portfolio → Execution → Algorithm)
- LEAN-specific functionality and performance
- Framework insights and analytics

## 📁 **Test Files**

### **Structure Analysis**
- `analyze_lean_structure.py` - Comprehensive LEAN architecture analysis
- `lean_structure_simple.py` - Basic LEAN structure validation
- `quick_lean_analysis.py` - Rapid LEAN compliance check

### **Integration Testing**
- `test_lean_insights.py` - LEAN insights and analytics validation

## 🚀 **Usage**

```bash
# Run all LEAN tests
cd /workspaces/unicorninvesting/tests
pytest lean/ -v

# Run specific LEAN analysis
python lean/quick_lean_analysis.py

# Include in comprehensive testing
./run_comprehensive_tests.sh
```

## 🏗️ **LEAN 6-Layer Architecture**

The tests validate compliance with LEAN's architecture:

1. **Data Sources** (Layer 1) → Raw market data collection
2. **Alpha Models** (Layer 2) → Signal generation and trading ideas  
3. **Risk Management** (Layer 3) → Risk controls and limits
4. **Portfolio Construction** (Layer 4) → Position sizing and allocation
5. **Execution Models** (Layer 5) → Order placement and execution
6. **Algorithms** (Layer 6) → Complete trading algorithms

## 📊 **Current Status**

- ✅ **Directory Created**: Ready for LEAN-specific tests
- ⚠️ **Implementation**: All files are placeholders (empty)
- 🎯 **Priority**: Medium - Important for framework validation

## 🔗 **Related Components**

- **Main LEAN Code**: `/BackendPython/Lean/`
- **Unicorn Integration**: `/BackendPython/unicorn/`
- **System Architecture Tests**: `/tests/system/`
- **Portfolio Tests**: `/tests/unicorn/4_portfolios/`

## 📝 **Development Notes**

These tests should validate:
- LEAN framework proper installation and configuration
- Integration between LEAN and Unicorn platform
- Performance benchmarks for LEAN-based algorithms
- Compliance with LEAN architecture patterns
