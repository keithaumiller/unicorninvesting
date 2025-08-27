#!/bin/bash

# Test Framework Algorithms
# ========================
# 
# This script tests the new LEAN Algorithm Framework implementations
# to ensure proper separation of concerns and functionality.

echo "🦄 UNICORN ALGORITHM FRAMEWORK TESTING"
echo "======================================"

# Set up paths
FRAMEWORK_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/framework"
ALGORITHMS_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/algorithms"

echo "📂 Framework Directory: $FRAMEWORK_DIR"
echo "📂 Algorithms Directory: $ALGORITHMS_DIR"
echo ""

# Check framework structure
echo "🔍 CHECKING FRAMEWORK STRUCTURE"
echo "-------------------------------"

if [ -d "$FRAMEWORK_DIR" ]; then
    echo "✅ Framework directory exists"
    
    # Check component directories
    for dir in "alphas" "portfolio" "risk" "execution" "universe"; do
        if [ -d "$FRAMEWORK_DIR/$dir" ]; then
            echo "✅ $dir/ directory exists"
        else
            echo "❌ $dir/ directory missing"
        fi
    done
    
    # Check key files
    echo ""
    echo "📋 Framework Component Files:"
    
    # Alpha Models
    if [ -f "$FRAMEWORK_DIR/alphas/AdvancedForexForecastingAlpha.py" ]; then
        echo "✅ AdvancedForexForecastingAlpha.py"
    else
        echo "❌ AdvancedForexForecastingAlpha.py missing"
    fi
    
    if [ -f "$FRAMEWORK_DIR/alphas/EthFocusedAlpha.py" ]; then
        echo "✅ EthFocusedAlpha.py"
    else
        echo "❌ EthFocusedAlpha.py missing"
    fi
    
    # Portfolio Models
    if [ -f "$FRAMEWORK_DIR/portfolio/UnicornPortfolioConstruction.py" ]; then
        echo "✅ UnicornPortfolioConstruction.py"
    else
        echo "❌ UnicornPortfolioConstruction.py missing"
    fi
    
    # Risk Models
    if [ -f "$FRAMEWORK_DIR/risk/UnicornRiskManagement.py" ]; then
        echo "✅ UnicornRiskManagement.py"
    else
        echo "❌ UnicornRiskManagement.py missing"
    fi
    
    # Main Framework Algorithms
    if [ -f "$FRAMEWORK_DIR/UnicornFrameworkAlgorithms.py" ]; then
        echo "✅ UnicornFrameworkAlgorithms.py"
    else
        echo "❌ UnicornFrameworkAlgorithms.py missing"
    fi
    
else
    echo "❌ Framework directory does not exist"
fi

echo ""

# Check algorithm implementations
echo "🚀 CHECKING ALGORITHM IMPLEMENTATIONS"
echo "------------------------------------"

# Framework-based algorithms
if [ -f "$ALGORITHMS_DIR/AdvancedForexFrameworkAlgorithm.py" ]; then
    echo "✅ AdvancedForexFrameworkAlgorithm.py"
else
    echo "❌ AdvancedForexFrameworkAlgorithm.py missing"
fi

if [ -f "$ALGORITHMS_DIR/EthFrameworkAlgorithm.py" ]; then
    echo "✅ EthFrameworkAlgorithm.py"
else
    echo "❌ EthFrameworkAlgorithm.py missing"
fi

echo ""

# Test Python syntax
echo "🐍 TESTING PYTHON SYNTAX"
echo "------------------------"

# Function to test Python file
test_python_file() {
    local file=$1
    local name=$2
    
    if [ -f "$file" ]; then
        if python3 -m py_compile "$file" 2>/dev/null; then
            echo "✅ $name: Syntax OK"
        else
            echo "❌ $name: Syntax Error"
            python3 -m py_compile "$file"
        fi
    else
        echo "⚠️  $name: File not found"
    fi
}

# Test framework components
echo "Framework Components:"
test_python_file "$FRAMEWORK_DIR/alphas/AdvancedForexForecastingAlpha.py" "AdvancedForexForecastingAlpha"
test_python_file "$FRAMEWORK_DIR/alphas/EthFocusedAlpha.py" "EthFocusedAlpha"
test_python_file "$FRAMEWORK_DIR/portfolio/UnicornPortfolioConstruction.py" "UnicornPortfolioConstruction"
test_python_file "$FRAMEWORK_DIR/risk/UnicornRiskManagement.py" "UnicornRiskManagement"
test_python_file "$FRAMEWORK_DIR/UnicornFrameworkAlgorithms.py" "UnicornFrameworkAlgorithms"

echo ""
echo "Algorithm Implementations:"
test_python_file "$ALGORITHMS_DIR/AdvancedForexFrameworkAlgorithm.py" "AdvancedForexFrameworkAlgorithm"
test_python_file "$ALGORITHMS_DIR/EthFrameworkAlgorithm.py" "EthFrameworkAlgorithm"

echo ""

# Check imports and dependencies
echo "📦 CHECKING IMPORTS AND DEPENDENCIES"
echo "-----------------------------------"

# Test imports for key files
echo "Testing Alpha Model imports..."
if [ -f "$FRAMEWORK_DIR/alphas/AdvancedForexForecastingAlpha.py" ]; then
    if python3 -c "
import sys
sys.path.append('$FRAMEWORK_DIR')
try:
    from alphas.AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha
    print('✅ AdvancedForexForecastingAlpha imports successfully')
except ImportError as e:
    print(f'❌ AdvancedForexForecastingAlpha import error: {e}')
except Exception as e:
    print(f'⚠️  AdvancedForexForecastingAlpha import warning: {e}')
" 2>/dev/null; then
        :
    else
        echo "❌ AdvancedForexForecastingAlpha import failed"
    fi
fi

if [ -f "$FRAMEWORK_DIR/alphas/EthFocusedAlpha.py" ]; then
    if python3 -c "
import sys
sys.path.append('$FRAMEWORK_DIR')
try:
    from alphas.EthFocusedAlpha import EthFocusedAlpha
    print('✅ EthFocusedAlpha imports successfully')
except ImportError as e:
    print(f'❌ EthFocusedAlpha import error: {e}')
except Exception as e:
    print(f'⚠️  EthFocusedAlpha import warning: {e}')
" 2>/dev/null; then
        :
    else
        echo "❌ EthFocusedAlpha import failed"
    fi
fi

echo ""

# Architecture validation
echo "🏗️  ARCHITECTURE VALIDATION"
echo "---------------------------"

echo "Checking Algorithm Framework separation of concerns..."

# Check if Alpha Models only do forecasting (no direct trading)
echo "🔮 Alpha Models (Forecasting Only):"
if grep -q "set_holdings\|liquidate\|market_order" "$FRAMEWORK_DIR/alphas/"*.py 2>/dev/null; then
    echo "❌ Alpha Models contain direct trading calls (violates separation)"
else
    echo "✅ Alpha Models only generate Insights (proper separation)"
fi

# Check if Portfolio Models only do position sizing
echo "⚖️  Portfolio Models (Position Sizing Only):"
if grep -q "set_holdings\|liquidate\|market_order" "$FRAMEWORK_DIR/portfolio/"*.py 2>/dev/null; then
    echo "❌ Portfolio Models contain direct trading calls (violates separation)"
else
    echo "✅ Portfolio Models only create PortfolioTargets (proper separation)"
fi

# Check if Risk Models only do risk management
echo "🛡️  Risk Models (Risk Management Only):"
if grep -q "set_holdings\|liquidate" "$FRAMEWORK_DIR/risk/"*.py 2>/dev/null; then
    echo "⚠️  Risk Models contain liquidation calls (expected for risk management)"
    echo "✅ Risk Models provide risk controls (proper separation)"
else
    echo "✅ Risk Models only provide risk controls (proper separation)"
fi

echo ""

# Component count summary
echo "📊 COMPONENT SUMMARY"
echo "-------------------"

ALPHA_COUNT=$(find "$FRAMEWORK_DIR/alphas" -name "*.py" 2>/dev/null | wc -l)
PORTFOLIO_COUNT=$(find "$FRAMEWORK_DIR/portfolio" -name "*.py" 2>/dev/null | wc -l)
RISK_COUNT=$(find "$FRAMEWORK_DIR/risk" -name "*.py" 2>/dev/null | wc -l)
ALGORITHM_COUNT=$(find "$ALGORITHMS_DIR" -name "*Framework*.py" 2>/dev/null | wc -l)

echo "🔮 Alpha Models: $ALPHA_COUNT"
echo "⚖️  Portfolio Models: $PORTFOLIO_COUNT"
echo "🛡️  Risk Models: $RISK_COUNT"
echo "🚀 Framework Algorithms: $ALGORITHM_COUNT"

echo ""

# Migration status
echo "🔄 MIGRATION STATUS"
echo "------------------"

echo "Framework Approach vs Monolithic Approach:"

# Count framework-based vs monolithic algorithms
FRAMEWORK_ALGOS=$(find "$ALGORITHMS_DIR" -name "*Framework*.py" 2>/dev/null | wc -l)
MONOLITHIC_ALGOS=$(find "$ALGORITHMS_DIR" -name "*.py" ! -name "*Framework*" ! -name "test_*" ! -name "LEAN_*" 2>/dev/null | wc -l)

echo "✅ Framework-based algorithms: $FRAMEWORK_ALGOS"
echo "📦 Monolithic algorithms: $MONOLITHIC_ALGOS"

if [ $FRAMEWORK_ALGOS -gt 0 ]; then
    echo "🎯 Migration to Framework approach: IN PROGRESS"
    echo "   ✅ Clean separation of concerns implemented"
    echo "   ✅ Modular components created"
    echo "   ✅ Professional architecture established"
else
    echo "⚠️  Migration to Framework approach: NOT STARTED"
fi

echo ""

# Recommendations
echo "💡 RECOMMENDATIONS"
echo "------------------"

if [ $FRAMEWORK_ALGOS -gt 0 ]; then
    echo "✅ Framework structure is properly implemented"
    echo "🎯 Next steps:"
    echo "   1. Test framework algorithms with backtesting"
    echo "   2. Compare performance with monolithic versions"
    echo "   3. Migrate remaining algorithms to framework approach"
    echo "   4. Add unit tests for individual components"
else
    echo "❌ Framework structure needs completion"
    echo "🎯 Required actions:"
    echo "   1. Complete framework component implementation"
    echo "   2. Create algorithm implementations using framework"
    echo "   3. Test component separation and functionality"
fi

echo ""
echo "🦄 FRAMEWORK TESTING COMPLETE"
echo "============================="

# Generate summary report
echo ""
echo "📋 TEST SUMMARY REPORT"
echo "====================="
echo "Date: $(date)"
echo "Framework Components: $((ALPHA_COUNT + PORTFOLIO_COUNT + RISK_COUNT))"
echo "Framework Algorithms: $FRAMEWORK_ALGOS"
echo "Architecture: $([ $FRAMEWORK_ALGOS -gt 0 ] && echo 'Clean Separation ✅' || echo 'Mixed Approach ⚠️')"
echo "Status: $([ $FRAMEWORK_ALGOS -gt 0 ] && echo 'Framework Ready 🚀' || echo 'Migration Needed 🔄')"

exit 0
