#!/bin/bash

# Unicorn Investing - Data Warehouse Testing Suite
# Usage: ./test_data_warehouse.sh [options]

set -e

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Function to print test results
test_result() {
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    elif [ $1 -eq 2 ]; then
        echo -e "${YELLOW}⏭️  $2 (SKIPPED)${NC}"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    else
        echo -e "${RED}❌ $2${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        if [ -n "$3" ]; then
            echo -e "   ${YELLOW}💡 $3${NC}"
        fi
    fi
}

# Function to display help
show_help() {
    echo "🏗️  Unicorn Investing - Data Warehouse Testing Suite"
    echo ""
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  --layer=LAYER       Test specific layer only (raw|bronze|silver|gold|all)"
    echo "  --connector=NAME    Test specific connector only (yahoo|fred|ibkr|forex)"
    echo "  --quick             Run quick tests only (skip integration tests)"
    echo "  --verbose          Show detailed test output"
    echo "  --help             Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                           # Test all layers"
    echo "  $0 --layer=raw               # Test only raw layer"
    echo "  $0 --connector=yahoo         # Test only Yahoo Finance connector"
    echo ""
}

# Parse command line arguments
LAYER="all"
CONNECTOR=""
QUICK_MODE=false
VERBOSE_MODE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --layer=*)
            LAYER="${1#*=}"
            shift
            ;;
        --connector=*)
            CONNECTOR="${1#*=}"
            shift
            ;;
        --quick)
            QUICK_MODE=true
            shift
            ;;
        --verbose)
            VERBOSE_MODE=true
            shift
            ;;
        --help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# Navigate to project root
cd /workspaces/unicorninvesting

# Activate virtual environment
if [ -f ".venv/bin/activate" ]; then
    source .venv/bin/activate
    echo -e "${BLUE}🐍 Python virtual environment activated${NC}"
else
    echo -e "${RED}❌ Python virtual environment not found${NC}"
    exit 1
fi

echo "🏗️  UNICORN INVESTING - DATA WAREHOUSE TESTING SUITE"
echo "====================================================="
echo -e "${BLUE}📅 Test Run: $(date)${NC}"
echo -e "${BLUE}🎯 Layer: $LAYER${NC}"
echo -e "${BLUE}🔗 Connector: ${CONNECTOR:-'all'}${NC}"
echo ""

# Function to run pytest with error handling
run_pytest() {
    local test_path="$1"
    local test_name="$2"
    local is_optional="${3:-false}"
    
    if [ ! -d "$test_path" ] && [ ! -f "$test_path" ]; then
        if [ "$is_optional" = true ]; then
            test_result 2 "$test_name - Path not found"
            return
        else
            test_result 1 "$test_name - Path not found"
            return
        fi
    fi
    
    echo -e "${CYAN}🧪 Running: $test_name${NC}"
    
    if python -m pytest "$test_path" -q >/dev/null 2>&1; then
        test_result 0 "$test_name"
    else
        test_result 1 "$test_name" "Run: python -m pytest $test_path -v for details"
    fi
}

# Function to test raw layer
test_raw_layer() {
    echo -e "${PURPLE}🗃️  TESTING RAW LAYER (Layer 1)${NC}"
    echo "================================"
    
    if [ -n "$CONNECTOR" ]; then
        case $CONNECTOR in
            yahoo)
                run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/" "Yahoo Finance Connector"
                ;;
            fred)
                run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/" "FRED Connector"
                ;;
            ibkr)
                run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/" "IBKR Connector"
                ;;
            forex)
                run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/forex/" "Forex Connector"
                ;;
            *)
                echo -e "${RED}❌ Unknown connector: $CONNECTOR${NC}"
                exit 1
                ;;
        esac
    else
        run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/" "Yahoo Finance Connector"
        run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/" "FRED Connector"
        run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/" "IBKR Connector"
        run_pytest "tests/unicorn/1_data_sources/1_raw/connectors/forex/" "Forex Connector"
        run_pytest "tests/unicorn/1_data_sources/test_ibkr_connection.py" "IBKR Gateway Connection"
        run_pytest "tests/unicorn/1_data_sources/data/" "Raw Data Validation" true
        run_pytest "tests/unicorn/1_data_sources/database/" "Database Integration" true
    fi
    
    echo ""
}

# Function to test other layers (simplified for now)
test_other_layers() {
    echo -e "${PURPLE}🥉 TESTING BRONZE/SILVER/GOLD LAYERS${NC}"
    echo "===================================="
    
    # Bronze layer checks
    if [ -d "BackendPython/unicorn/1_data_sources/2_bronze" ]; then
        test_result 0 "Bronze Layer - Directory exists"
    else
        test_result 1 "Bronze Layer - Directory missing"
    fi
    
    # Silver layer checks
    if [ -d "BackendPython/unicorn/1_data_sources/3_silver" ]; then
        test_result 0 "Silver Layer - Directory exists"
        if [ -f "BackendPython/unicorn/4_portfolios/Myportolio/core/silver_layer_data_connector.py" ]; then
            test_result 0 "Silver Layer Data Connector - File exists"
        else
            test_result 1 "Silver Layer Data Connector - File missing"
        fi
    else
        test_result 1 "Silver Layer - Directory missing"
    fi
    
    # Gold layer checks
    if [ -d "BackendPython/unicorn/1_data_sources/4_gold" ]; then
        test_result 0 "Gold Layer - Directory exists"
    else
        test_result 2 "Gold Layer - Not implemented yet"
    fi
    
    echo ""
}

# Main execution
echo -e "${BLUE}🚀 Starting Data Warehouse Testing Suite...${NC}"
echo ""

case $LAYER in
    raw)
        test_raw_layer
        ;;
    bronze|silver|gold)
        test_other_layers
        ;;
    all)
        test_raw_layer
        test_other_layers
        ;;
    *)
        echo -e "${RED}❌ Invalid layer: $LAYER${NC}"
        show_help
        exit 1
        ;;
esac

# Print summary
echo "📊 TEST SUMMARY"
echo "==============="
echo -e "Total Tests: ${BLUE}$TOTAL_TESTS${NC}"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"
echo -e "Skipped: ${YELLOW}$SKIPPED_TESTS${NC}"

if [ $TOTAL_TESTS -gt 0 ]; then
    SUCCESS_RATE=$(( (PASSED_TESTS * 100) / TOTAL_TESTS ))
    echo -e "Success Rate: ${BLUE}$SUCCESS_RATE%${NC}"
    
    if [ $FAILED_TESTS -eq 0 ]; then
        echo -e "\n${GREEN}🎉 All tests passed! Data warehouse is healthy.${NC}"
        exit 0
    else
        echo -e "\n${YELLOW}⚠️  Some tests failed. Check details above.${NC}"
        exit 1
    fi
else
    echo -e "\n${YELLOW}⚠️  No tests were executed.${NC}"
    exit 1
fi
