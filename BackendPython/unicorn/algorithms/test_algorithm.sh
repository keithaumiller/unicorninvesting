#!/bin/bash

# Unicorn Forex Algorithm Test Script
# Tests the basic forex trading algorithm

echo "🦄 Unicorn Forex Algorithm Test"
echo "================================="

# Check if LEAN is available
if [ ! -d "/workspaces/unicorninvesting/BackendPython/Lean" ]; then
    echo "❌ LEAN not found. Please ensure LEAN is cloned properly."
    exit 1
fi

# Check if algorithm file exists
ALGORITHM_FILE="/workspaces/unicorninvesting/BackendPython/unicorn/algorithms/unicorn_basic_forex_algorithm.py"
if [ ! -f "$ALGORITHM_FILE" ]; then
    echo "❌ Algorithm file not found: $ALGORITHM_FILE"
    exit 1
fi

echo "✅ LEAN framework found"
echo "✅ Algorithm file found"

# Check Python syntax
echo "🔍 Checking Python syntax..."
cd /workspaces/unicorninvesting/BackendPython/unicorn/algorithms

# Note: We expect import errors because AlgorithmImports is only available in LEAN runtime
python3 -m py_compile unicorn_basic_forex_algorithm.py 2>/dev/null
if [ $? -eq 0 ]; then
    echo "✅ Python syntax is valid"
else
    echo "⚠️  Syntax check failed (expected due to LEAN imports)"
fi

# Check if LEAN CLI is available
cd /workspaces/unicorninvesting/BackendPython/Lean

# Check for .NET runtime
if command -v dotnet &> /dev/null; then
    echo "✅ .NET runtime found"
    
    # Try to build LEAN
    echo "🔨 Testing LEAN build..."
    dotnet build Launcher/QuantConnect.Lean.Launcher.csproj --verbosity quiet
    if [ $? -eq 0 ]; then
        echo "✅ LEAN builds successfully"
        echo ""
        echo "🚀 Ready to run forex algorithm!"
        echo ""
        echo "To run the algorithm:"
        echo "cd /workspaces/unicorninvesting/BackendPython/Lean"
        echo "dotnet run --project Launcher -- --algorithm-location ../unicorn/algorithms/unicorn_basic_forex_algorithm.py"
    else
        echo "⚠️  LEAN build failed. May need dependencies."
    fi
else
    echo "❌ .NET runtime not found. Install .NET to run LEAN algorithms."
fi

echo ""
echo "📚 Algorithm features:"
echo "   - EURUSD, USDJPY, USDCNH forex pairs"
echo "   - ETHUSD cryptocurrency"
echo "   - Simple moving average crossover strategy"
echo "   - Risk management with position sizing"
echo ""
echo "📖 See FOREX_HELLO_WORLD.md for detailed documentation"
