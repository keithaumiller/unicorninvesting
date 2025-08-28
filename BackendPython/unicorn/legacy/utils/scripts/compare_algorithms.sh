#!/bin/bash

# Algorithm Comparison Runner - Unicorn Investing Platform
# Compares XGBoost Forex Algorithm vs ETH Portfolio performance

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN_DIprint("ETH Portfolio Algorithm:")
print("  • Focused 95% allocation to Ethereum (ETHUSD)")
print("  • Technical analysis with SMA crossover strategy")
print("  • Conservative 2% stop loss and position sizing")
print("  • Rebalances monthly for risk management")

print("
🎯 CONCLUSION:")
print("Choose Advanced Ensemble for:")
print("  • Multi-currency diversification")
print("  • Sophisticated ML forecasting")
print("  • Complex pattern recognition")
print("  • Higher potential returns (with higher risk)")

print("Choose ETH Portfolio for:")
print("  • Cryptocurrency focus")
print("  • Simple, proven strategy")
print("  • Lower complexity")
print("  • Ethereum growth exposure")orkspaces/unicorninvesting/BackendPython/Lean"
ALGORITHM_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/algorithms"
RESULTS_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/results"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# Unicode symbols
ROCKET="🚀"
VS="⚔️"
CHART="📊"
MONEY="💰"
SUCCESS="✅"
ERROR="❌"

echo -e "${PURPLE}${ROCKET} Unicorn Algorithm Comparison Suite${NC}"
echo -e "${BLUE}${VS} Advanced Ensemble Forex vs ETH Portfolio Backtest Comparison${NC}"
echo ""

# Function to run algorithm backtest
run_algorithm_backtest() {
    local algo_name="$1"
    local algo_file="$2"
    local description="$3"
    
    echo -e "${GREEN}${ROCKET} Running $description...${NC}"
    echo -e "${BLUE}Algorithm: $algo_name${NC}"
    echo ""
    
    cd "$LEAN_DIR"
    
    # Copy algorithm to LEAN directory
    cp "$ALGORITHM_DIR/$algo_file" "$LEAN_DIR/Algorithm.Python/"
    
    # Create backup of original config
    cp "$LEAN_DIR/Launcher/config.json" "$LEAN_DIR/Launcher/config.json.backup"
    
    # Update configuration for this algorithm
    sed -i "s/\"algorithm-type-name\": \".*\"/\"algorithm-type-name\": \"$algo_name\"/" "$LEAN_DIR/Launcher/config.json"
    sed -i 's/"algorithm-language": ".*"/"algorithm-language": "Python"/' "$LEAN_DIR/Launcher/config.json"
    sed -i "s|\"algorithm-location\": \".*\"|\"algorithm-location\": \"../../../Algorithm.Python/$algo_file\"|" "$LEAN_DIR/Launcher/config.json"
    sed -i 's/"environment": ".*"/"environment": "backtesting"/' "$LEAN_DIR/Launcher/config.json"
    
    echo -e "${SUCCESS} Configuration updated for $algo_name"
    
    # Run the backtest
    echo -e "${BLUE}Running backtest...${NC}"
    dotnet run --project Launcher/QuantConnect.Lean.Launcher.csproj > "$RESULTS_DIR/${algo_name}_backtest.log" 2>&1
    
    # Restore original configuration
    if [ -f "$LEAN_DIR/Launcher/config.json.backup" ]; then
        mv "$LEAN_DIR/Launcher/config.json.backup" "$LEAN_DIR/Launcher/config.json"
    fi
    
    # Copy results with algorithm-specific naming
    if [ -d "./Results" ]; then
        mkdir -p "$RESULTS_DIR/$algo_name"
        cp -r ./Results/* "$RESULTS_DIR/$algo_name/" 2>/dev/null || true
        echo -e "${SUCCESS} Results saved to: $RESULTS_DIR/$algo_name"
    fi
    
    echo -e "${SUCCESS} $description backtest complete!"
    echo ""
}

# Function to analyze and compare results
analyze_comparison() {
    echo -e "${PURPLE}${CHART} Performance Analysis & Comparison${NC}"
    echo ""
    
    # Check if we have results for both algorithms
    if [ -d "$RESULTS_DIR/AdvancedForexForecastingAlgorithm" ] && [ -d "$RESULTS_DIR/EthOnlyPortfolio" ]; then
        echo -e "${SUCCESS} Both algorithm results found. Analyzing performance..."
        echo ""
        
        python3 << EOF
import json
import os
import glob
import pandas as pd
from datetime import datetime

def extract_performance_metrics(results_dir, algo_name):
    """Extract key performance metrics from algorithm results."""
    metrics = {
        'algorithm': algo_name,
        'total_return': 'N/A',
        'sharpe_ratio': 'N/A',
        'max_drawdown': 'N/A',
        'total_trades': 'N/A',
        'win_rate': 'N/A',
        'profit_factor': 'N/A'
    }
    
    # Look for JSON result files
    json_files = glob.glob(os.path.join(results_dir, "*.json"))
    
    if json_files:
        try:
            # Use the most recent file
            latest_file = max(json_files, key=os.path.getctime)
            
            with open(latest_file, 'r') as f:
                results = json.load(f)
            
            # Extract metrics from different possible locations
            if 'Statistics' in results:
                stats = results['Statistics']
                metrics['total_return'] = stats.get('Total Return', stats.get('Compounding Annual Return', 'N/A'))
                metrics['sharpe_ratio'] = stats.get('Sharpe Ratio', 'N/A')
                metrics['max_drawdown'] = stats.get('Drawdown', stats.get('Maximum Drawdown', 'N/A'))
                metrics['total_trades'] = stats.get('Total Orders', stats.get('Total Trades', 'N/A'))
                metrics['win_rate'] = stats.get('Win Rate', 'N/A')
                metrics['profit_factor'] = stats.get('Profit-Loss Ratio', 'N/A')
            
            # Alternative locations for metrics
            for key in ['RuntimeStatistics', 'Charts', 'AlgorithmConfiguration']:
                if key in results and isinstance(results[key], dict):
                    for stat_key, stat_value in results[key].items():
                        if 'return' in stat_key.lower():
                            metrics['total_return'] = stat_value
                        elif 'sharpe' in stat_key.lower():
                            metrics['sharpe_ratio'] = stat_value
                        elif 'drawdown' in stat_key.lower():
                            metrics['max_drawdown'] = stat_value
                        elif 'trades' in stat_key.lower() or 'orders' in stat_key.lower():
                            metrics['total_trades'] = stat_value
                        elif 'win' in stat_key.lower() and 'rate' in stat_key.lower():
                            metrics['win_rate'] = stat_value
            
        except Exception as e:
            print(f"❌ Error parsing results for {algo_name}: {e}")
    
    return metrics

# Analyze both algorithms
results_base = "$RESULTS_DIR"
ensemble_metrics = extract_performance_metrics(f"{results_base}/AdvancedForexForecastingAlgorithm", "Advanced Ensemble")
eth_metrics = extract_performance_metrics(f"{results_base}/EthOnlyPortfolio", "ETH Portfolio")

# Print comparison table
print("📊 ALGORITHM PERFORMANCE COMPARISON")
print("=" * 60)
print(f"{'Metric':<20} {'Advanced Ensemble':<20} {'ETH Portfolio':<15}")
print("-" * 60)
print(f"{'Total Return':<20} {ensemble_metrics['total_return']:<20} {eth_metrics['total_return']:<15}")
print(f"{'Sharpe Ratio':<20} {ensemble_metrics['sharpe_ratio']:<20} {eth_metrics['sharpe_ratio']:<15}")
print(f"{'Max Drawdown':<20} {ensemble_metrics['max_drawdown']:<20} {eth_metrics['max_drawdown']:<15}")
print(f"{'Total Trades':<20} {ensemble_metrics['total_trades']:<20} {eth_metrics['total_trades']:<15}")
print(f"{'Win Rate':<20} {ensemble_metrics['win_rate']:<20} {eth_metrics['win_rate']:<15}")
print(f"{'Profit Factor':<20} {ensemble_metrics['profit_factor']:<20} {eth_metrics['profit_factor']:<15}")
print("=" * 60)

# Determine winner
print("\n🏆 PERFORMANCE ANALYSIS:")

def safe_float_convert(value):
    """Safely convert string values to float for comparison."""
    if isinstance(value, str):
        # Remove percentage signs and other characters
        clean_value = value.replace('%', '').replace('\$', '').replace(',', '')
        try:
            return float(clean_value)
        except:
            return None
    return value

# Compare returns
ensemble_return = safe_float_convert(ensemble_metrics['total_return'])
eth_return = safe_float_convert(eth_metrics['total_return'])

if ensemble_return is not None and eth_return is not None:
    if ensemble_return > eth_return:
        print(f"🥇 Advanced Ensemble wins on Total Return: {ensemble_return:.2f}% vs {eth_return:.2f}%")
    else:
        print(f"🥇 ETH Portfolio wins on Total Return: {eth_return:.2f}% vs {ensemble_return:.2f}%")
else:
    print("⚠️  Unable to compare returns due to data format")

# Compare Sharpe ratios
ensemble_sharpe = safe_float_convert(ensemble_metrics['sharpe_ratio'])
eth_sharpe = safe_float_convert(eth_metrics['sharpe_ratio'])

if ensemble_sharpe is not None and eth_sharpe is not None:
    if ensemble_sharpe > eth_sharpe:
        print(f"📈 Advanced Ensemble has better risk-adjusted returns: {ensemble_sharpe:.2f} vs {eth_sharpe:.2f}")
    else:
        print(f"📈 ETH Portfolio has better risk-adjusted returns: {eth_sharpe:.2f} vs {ensemble_sharpe:.2f}")
else:
    print("⚠️  Unable to compare Sharpe ratios due to data format")

print("\n💡 STRATEGY INSIGHTS:")
print("Advanced Ensemble Forex Algorithm:")
print("  • Uses 4 ML models: ARIMA + Neural + Prophet + XGBoost")
print("  • Diversified across EURUSD, USDJPY, USDCNH, and ETHUSD")
print("  • Ensemble confidence scoring and dynamic weighting")
print("  • Advanced feature engineering and pattern recognition")

print("\nETH Portfolio Algorithm:")
print("  • Focused single-asset strategy on Ethereum")
print("  • Uses traditional technical analysis (SMA, RSI)")
print("  • 95% ETH allocation with 5% cash buffer")
print("  • Strong risk management with stop losses")

EOF
    
    else
        echo -e "${ERROR} Missing results for comparison. Please run both algorithms first."
    fi
}

# Function to show step-by-step explanation
explain_algorithms() {
    echo -e "${PURPLE}${CHART} ALGORITHM EXPLANATION - How They Work${NC}"
    echo ""
    
    echo -e "${BLUE}📱 Advanced Ensemble Forex Algorithm - Step by Step:${NC}"
    echo ""
    echo "1. 📊 Data Collection (Every Hour):"
    echo "   • Collects price data for EURUSD, USDJPY, USDCNH, ETHUSD"
    echo "   • Builds historical datasets for each forecasting model"
    echo "   • Updates technical indicators and features continuously"
    echo ""
    echo "2. 🤖 Four Forecasting Models Working Together:"
    echo ""
    echo "   🔹 ARIMA Models (25% weight):"
    echo "     - ARIMA(1,1,1) and ARIMA(2,1,2) configurations"
    echo "     - Uses LEAN's built-in ARIMA forecasting"
    echo "     - Excellent for trend following and time series patterns"
    echo ""
    echo "   🔹 Neural Network Simulation (25% weight):"
    echo "     - RSI, SMA, EMA, Momentum indicators"
    echo "     - Weighted signal combination mimicking neural network"
    echo "     - Pattern recognition and non-linear relationships"
    echo ""
    echo "   🔹 Prophet Model (25% weight):"
    echo "     - Facebook's Prophet for seasonality and trends"
    echo "     - Handles missing data and holiday effects"
    echo "     - Provides uncertainty intervals"
    echo ""
    echo "   🔹 XGBoost Ensemble (25% weight):"
    echo "     - Gradient boosting with feature engineering"
    echo "     - Momentum, mean reversion, volatility features"
    echo "     - Simulated decision tree ensemble"
    echo ""
    echo "3. 🎯 Ensemble Prediction (Every 24 Hours):"
    echo "   • Weighted average of all 4 model predictions"
    echo "   • Confidence scoring based on model agreement"
    echo "   • Dynamic weight adjustment based on performance"
    echo ""
    echo "4. 💹 Trading Decisions (Every Hour):"
    echo "   • Requires >0.5% predicted price move"
    echo "   • Requires >60% ensemble confidence score"
    echo "   • 15% maximum position size per currency"
    echo "   • 2-hour cooldown between trades"
    echo ""
    
    echo -e "${GREEN}🪙 ETH Portfolio Algorithm - Step by Step:${NC}"
    echo ""
    echo "1. 📊 Data Collection (Every Minute):"
    echo "   • Collects ETH/USD price data from Coinbase"
    echo "   • Calculates technical indicators continuously"
    echo "   • Updates 20-minute and 50-minute SMAs, 14-period RSI"
    echo ""
    echo "2. 🎯 Portfolio Management:"
    echo "   • Maintains 95% ETH / 5% cash allocation"
    echo "   • Rebalances every 4 hours if drift >5%"
    echo "   • \$10 minimum trade size"
    echo ""
    echo "3. 📈 Trading Strategy:"
    echo "   • Initial purchase when SMA-fast > SMA-slow + RSI < 70"
    echo "   • Uses set_holdings() for proper position sizing"
    echo "   • 1-hour minimum between trades"
    echo ""
    echo "4. 🛡️ Risk Management:"
    echo "   • 5% stop loss on all positions"
    echo "   • 10% daily loss limit"
    echo "   • Real-time performance monitoring"
    echo ""
    
    echo -e "${YELLOW}⚔️ Key Differences:${NC}"
    echo ""
    echo "• Strategy: Ensemble = 4-model ML prediction | ETH = Single-asset technical analysis"
    echo "• Diversification: Ensemble = 4 currencies | ETH = 1 cryptocurrency"
    echo "• Complexity: Ensemble = ARIMA+Neural+Prophet+XGBoost | ETH = SMA+RSI indicators"
    echo "• Frequency: Ensemble = Hourly decisions, daily retraining | ETH = Minute-level data"
    echo "• Risk: Ensemble = 15% per position (60% max) | ETH = 95% single asset"
    echo ""
}

# Main execution
main() {
    # Create results directory
    mkdir -p "$RESULTS_DIR"
    
    echo "Select comparison mode:"
    echo "1) ${GREEN}Run Both Algorithms${NC} - Execute XGBoost vs ETH comparison"
    echo "2) ${BLUE}Explain How They Work${NC} - Step-by-step algorithm walkthrough"
    echo "3) ${YELLOW}Analyze Existing Results${NC} - Compare previous backtest results"
    echo "4) ${PURPLE}Exit${NC}"
    echo ""
    
    read -p "Choose option (1-4): " choice
    
    case $choice in
        1)
            explain_algorithms
            echo ""
            read -p "Press Enter to start backtesting both algorithms..."
            echo ""
            
            # Run XGBoost Forex Algorithm
            run_algorithm_backtest "XGBoostForexAlgorithm" "XGBoostForexAlgorithm.py" "XGBoost Forex Algorithm"
            
            # Run ETH Portfolio Algorithm  
            run_algorithm_backtest "EthOnlyPortfolio" "EthOnlyPortfolio.py" "ETH Portfolio Algorithm"
            
            # Analyze results
            analyze_comparison
            ;;
        2)
            explain_algorithms
            ;;
        3)
            analyze_comparison
            ;;
        4)
            echo -e "${SUCCESS} Goodbye!"
            exit 0
            ;;
        *)
            echo -e "${ERROR} Invalid option. Please choose 1-4."
            ;;
    esac
}

# Run the main function
main
