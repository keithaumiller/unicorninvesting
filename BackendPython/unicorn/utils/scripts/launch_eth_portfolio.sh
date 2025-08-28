#!/bin/bash

# ETH Portfolio Launcher - Unicorn Investing Platform
# Launches ETH-only portfolio in different environments (backtest, paper, live)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN_DIR="/workspaces/unicorninvesting/BackendPython/Lean"
ALGORITHM_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/algorithms"
CONFIG_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/config"
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
ETH="🪙"
MONEY="💰"
WARNING="⚠️"
SUCCESS="✅"
ERROR="❌"

echo -e "${PURPLE}${ROCKET} Unicorn Investing - ETH Portfolio Launcher${NC}"
echo -e "${BLUE}${ETH} Ethereum Only Portfolio - \$1,000 Initial Capital${NC}"
echo ""

# Function to check prerequisites
check_prerequisites() {
    echo -e "${BLUE}Checking prerequisites...${NC}"
    
    # Check if LEAN directory exists
    if [ ! -d "$LEAN_DIR" ]; then
        echo -e "${ERROR} LEAN directory not found: $LEAN_DIR"
        exit 1
    fi
    
    # Check if algorithm exists
    if [ ! -f "$ALGORITHM_DIR/EthOnlyPortfolio.py" ]; then
        echo -e "${ERROR} ETH portfolio algorithm not found: $ALGORITHM_DIR/EthOnlyPortfolio.py"
        exit 1
    fi
    
    # Check ETH data availability
    ETH_DATA_DIR="$LEAN_DIR/Data/crypto/coinbase/minute/ethusd"
    if [ ! -d "$ETH_DATA_DIR" ]; then
        echo -e "${WARNING} ETH data directory not found: $ETH_DATA_DIR"
        echo -e "Portfolio will run but may have limited data"
    else
        echo -e "${SUCCESS} ETH data found at: $ETH_DATA_DIR"
    fi
    
    # Create results directory if it doesn't exist
    mkdir -p "$RESULTS_DIR"
    
    echo -e "${SUCCESS} Prerequisites check complete"
    echo ""
}

# Function to display environment options
show_environment_menu() {
    echo -e "${YELLOW}Select trading environment:${NC}"
    echo "1) ${GREEN}Backtest${NC} - Historical simulation (SAFE)"
    echo "2) ${YELLOW}Paper Trading${NC} - Live data simulation (SAFE)"
    echo "3) ${RED}Live Trading${NC} - Real money trading (RISKY)"
    echo "4) ${BLUE}Quick Analysis${NC} - Portfolio performance analysis"
    echo "5) ${PURPLE}Exit${NC}"
    echo ""
}

# Function to run backtest
run_backtest() {
    echo -e "${GREEN}${ROCKET} Starting ETH Portfolio Backtest...${NC}"
    echo -e "${MONEY} Testing with \$1,000 virtual capital"
    echo ""
    
    cd "$LEAN_DIR"
    
    # Copy algorithm to LEAN directory
    cp "$ALGORITHM_DIR/EthOnlyPortfolio.py" "$LEAN_DIR/Algorithm.Python/"
    
    # Update config for backtesting
    echo "✅ Configuring LEAN for ETH portfolio backtesting..."
    
    # Create backup of original config
    cp "$LEAN_DIR/Launcher/config.json" "$LEAN_DIR/Launcher/config.json.backup"
    
    # Use sed to update the key configuration values
    sed -i 's/"algorithm-type-name": ".*"/"algorithm-type-name": "EthOnlyPortfolio"/' "$LEAN_DIR/Launcher/config.json"
    sed -i 's/"algorithm-language": ".*"/"algorithm-language": "Python"/' "$LEAN_DIR/Launcher/config.json"
    sed -i 's|"algorithm-location": ".*"|"algorithm-location": "../../../Algorithm.Python/EthOnlyPortfolio.py"|' "$LEAN_DIR/Launcher/config.json"
    sed -i 's/"environment": ".*"/"environment": "backtesting"/' "$LEAN_DIR/Launcher/config.json"
    
    echo "✅ Configuration updated for ETH portfolio backtesting"
    
    # Run the backtest
    echo -e "${BLUE}Running backtest...${NC}"
    dotnet run --project Launcher/QuantConnect.Lean.Launcher.csproj
    
    # Restore original configuration
    if [ -f "$LEAN_DIR/Launcher/config.json.backup" ]; then
        mv "$LEAN_DIR/Launcher/config.json.backup" "$LEAN_DIR/Launcher/config.json"
        echo "✅ Original configuration restored"
    fi
    
    # Copy results
    if [ -d "./Results" ]; then
        cp -r ./Results/* "$RESULTS_DIR/" 2>/dev/null || true
        echo -e "${SUCCESS} Results saved to: $RESULTS_DIR"
    fi
    
    echo -e "${SUCCESS} Backtest complete!"
}

# Function to run paper trading
run_paper_trading() {
    echo -e "${YELLOW}${WARNING} Paper Trading Setup${NC}"
    echo "Paper trading uses live market data but virtual money"
    echo "This is SAFE for testing but requires live data feeds"
    echo ""
    
    read -p "Continue with paper trading? (y/N): " confirm
    if [[ ! $confirm =~ ^[Yy]$ ]]; then
        return
    fi
    
    echo -e "${YELLOW}${ROCKET} Starting ETH Paper Trading...${NC}"
    echo -e "${MONEY} Virtual \$1,000 capital with live ETH prices"
    
    cd "$LEAN_DIR"
    cp "$ALGORITHM_DIR/EthOnlyPortfolio.py" "$LEAN_DIR/Algorithm.Python/"
    
    # Update config for paper trading
    echo "✅ Configuring LEAN for paper trading..."
    
    # Create backup of original config
    cp "$LEAN_DIR/Launcher/config.json" "$LEAN_DIR/Launcher/config.json.backup"
    
    # Use sed to update configuration
    sed -i 's/"algorithm-type-name": ".*"/"algorithm-type-name": "EthOnlyPortfolio"/' "$LEAN_DIR/Launcher/config.json"
    sed -i 's/"algorithm-language": ".*"/"algorithm-language": "Python"/' "$LEAN_DIR/Launcher/config.json"
    sed -i 's|"algorithm-location": ".*"|"algorithm-location": "../../../Algorithm.Python/EthOnlyPortfolio.py"|' "$LEAN_DIR/Launcher/config.json"
    sed -i 's/"environment": ".*"/"environment": "live-paper"/' "$LEAN_DIR/Launcher/config.json"
    
    echo "✅ Configuration updated for paper trading"
    
    echo -e "${BLUE}Starting paper trading session...${NC}"
    dotnet run --project Launcher/QuantConnect.Lean.Launcher.csproj
}

# Function to setup live trading (with warnings)
setup_live_trading() {
    echo -e "${RED}${WARNING} LIVE TRADING WARNING ${WARNING}${NC}"
    echo -e "${RED}This will use REAL MONEY with your IBKR account!${NC}"
    echo -e "${RED}Ensure you understand the risks before proceeding.${NC}"
    echo ""
    echo "Requirements for live trading:"
    echo "1. Active Interactive Brokers account"
    echo "2. TWS or IB Gateway running"
    echo "3. Sufficient account balance (minimum \$1,000 + fees)"
    echo "4. Approved for cryptocurrency trading"
    echo ""
    
    read -p "Do you have all requirements and want to proceed? (type 'YES' to confirm): " confirm
    if [[ $confirm != "YES" ]]; then
        echo "Live trading setup cancelled"
        return
    fi
    
    echo -e "${RED}Setting up live trading configuration...${NC}"
    echo "Please refer to IBKR_INTEGRATION_SETUP.md for complete setup"
    echo "You will need to configure:"
    echo "- IBKR account credentials"
    echo "- TWS/Gateway connection"
    echo "- Risk management settings"
    echo ""
    echo "For safety, start with paper trading first!"
}

# Function to analyze portfolio performance
analyze_portfolio() {
    echo -e "${BLUE}${ROCKET} ETH Portfolio Analysis${NC}"
    echo ""
    
    if [ ! -d "$RESULTS_DIR" ] || [ -z "$(ls -A $RESULTS_DIR)" ]; then
        echo -e "${WARNING} No results found. Run a backtest first."
        return
    fi
    
    echo "Available result files:"
    ls -la "$RESULTS_DIR"
    echo ""
    
    # Look for JSON results
    if ls "$RESULTS_DIR"/*.json 1> /dev/null 2>&1; then
        echo -e "${SUCCESS} Found result files for analysis"
        
        python3 << EOF
import json
import os
import glob

results_dir = "$RESULTS_DIR"
json_files = glob.glob(os.path.join(results_dir, "*.json"))

if json_files:
    latest_file = max(json_files, key=os.path.getctime)
    print(f"📊 Analyzing: {os.path.basename(latest_file)}")
    
    try:
        with open(latest_file, 'r') as f:
            results = json.load(f)
        
        # Extract key metrics
        if 'Statistics' in results:
            stats = results['Statistics']
            print(f"💰 Total Return: {stats.get('Total Return', 'N/A')}")
            print(f"📈 Sharpe Ratio: {stats.get('Sharpe Ratio', 'N/A')}")
            print(f"📉 Max Drawdown: {stats.get('Drawdown', 'N/A')}")
            print(f"🎯 Win Rate: {stats.get('Win Rate', 'N/A')}")
        else:
            print("📊 Raw results available but need parsing")
            
    except Exception as e:
        print(f"❌ Error parsing results: {e}")
else:
    print("❌ No JSON result files found")
EOF
    else
        echo -e "${WARNING} No JSON result files found for detailed analysis"
        echo "Available files:"
        find "$RESULTS_DIR" -type f -name "*" | head -10
    fi
}

# Main execution
main() {
    check_prerequisites
    
    while true; do
        show_environment_menu
        read -p "Choose option (1-5): " choice
        
        case $choice in
            1)
                run_backtest
                ;;
            2)
                run_paper_trading
                ;;
            3)
                setup_live_trading
                ;;
            4)
                analyze_portfolio
                ;;
            5)
                echo -e "${SUCCESS} Goodbye!"
                exit 0
                ;;
            *)
                echo -e "${ERROR} Invalid option. Please choose 1-5."
                ;;
        esac
        
        echo ""
        read -p "Press Enter to continue..."
        echo ""
    done
}

# Run the main function
main
