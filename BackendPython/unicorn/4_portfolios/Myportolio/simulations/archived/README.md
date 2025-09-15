# Archived Simulation Scripts

## 🗄️ **Archive Directory Structure**

This directory contains historical scripts that were consolidated during the January 19, 2025 architecture cleanup. These scripts are preserved for reference but should NOT be used for new simulations.

## 📂 **Directory Contents**

### **analysis_scripts/**
Historical analysis tools that were specific to particular simulations or debugging sessions:
- `analyze_6month_results.py` - Analysis for specific 6-month simulation (March-September 2024)
- `analyze_performance_issues.py` - Root cause analysis for over-trading issues  
- `comprehensive_performance_report.py` - Detailed performance report for zero-trade issue

### **diagnostic_tools/**
Debugging and troubleshooting scripts used during development:
- `emergency_diagnostic_test.py` - Direct algorithm testing bypassing LEAN framework
- `test_enhanced_logging.py` - Performance logger validation scripts
- `create_working_algorithm.py` - Quick fix for generating functional LEAN algorithms
- `working_lean_algorithm_generator.py` - LEAN algorithm code generator

### **legacy_engines/**
Deprecated simulation engines that were replaced by the unified architecture:
- `lean_simulation_engine.py` - Legacy LEAN-integrated engine (duplicate of python_simulation_engine.py)

## ⚠️ **IMPORTANT - Do Not Use These Scripts**

### **Use Instead:**
- **All Simulations**: `myportolio_simulator.py` (Master simulator with mandatory enhanced logging)
- **Analysis**: Built-in analysis capabilities in the master simulator
- **CLI**: `simulation_cli.py` (auto-redirects to master simulator)

### **Why These Are Archived:**
1. **Script Duplication**: Multiple scripts doing the same thing caused confusion
2. **Inconsistent Entry Points**: 7 different ways to run simulations
3. **Logging Bypass**: Some scripts could skip enhanced logging
4. **One-off Tools**: Scripts created for specific debugging sessions
5. **Legacy Code**: Outdated approaches replaced by unified architecture

## 🎯 **New Unified Architecture**

The new system provides:
- ✅ **Single Entry Point**: `myportolio_simulator.py`
- ✅ **Mandatory Enhanced Logging**: Cannot be bypassed
- ✅ **Standardized Results**: Consistent output format
- ✅ **Strategy Templates**: Pre-configured simulation scenarios
- ✅ **Clean Documentation**: Clear usage instructions

## 📚 **For Historical Reference Only**

These scripts may contain useful patterns or solutions for specific problems, but should be used as reference material only. All new simulation work should use the unified master simulator system.

---
**Archive Date**: January 19, 2025  
**Reason**: Architecture consolidation and cleanup  
**Status**: Reference material only - DO NOT USE for production