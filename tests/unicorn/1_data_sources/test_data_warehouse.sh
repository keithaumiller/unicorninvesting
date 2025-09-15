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

# Results storage
RESULTS_DIR="/workspaces/unicorninvesting/tests/unicorn/1_data_sources/datawarehousetestingresults"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
RESULTS_FILE="$RESULTS_DIR/test_results_$TIMESTAMP.json"
SUMMARY_FILE="$RESULTS_DIR/summary_$TIMESTAMP.json"
TEST_RESULTS=()

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# Function to print test results
test_result() {
    local exit_code=$1
    local test_name="$2"
    local error_message="${3:-}"
    local duration="${4:-0.0}"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    # Determine status
    local status=""
    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}✅ $test_name${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        status="PASSED"
    elif [ $exit_code -eq 2 ]; then
        echo -e "${YELLOW}⏭️  $test_name (SKIPPED)${NC}"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        status="SKIPPED"
    else
        echo -e "${RED}❌ $test_name${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        status="FAILED"
        if [ -n "$error_message" ]; then
            echo -e "   ${YELLOW}💡 $error_message${NC}"
        fi
    fi
    
    # Store result for JSON output
    local result_json=$(cat <<EOF
{
    "name": "$test_name",
    "status": "$status",
    "duration": $duration,
    "error": "$error_message"
}
EOF
)
    TEST_RESULTS+=("$result_json")
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
            test_result 2 "$test_name" "Path not found" 0.0
            return
        else
            test_result 1 "$test_name" "Path not found" 0.0
            return
        fi
    fi
    
    echo -e "${CYAN}🧪 Running: $test_name${NC}"
    
    # Measure execution time
    local start_time=$(date +%s.%N)
    
    if python -m pytest "$test_path" -q >/dev/null 2>&1; then
        local end_time=$(date +%s.%N)
        local duration=$(echo "$end_time - $start_time" | bc -l)
        test_result 0 "$test_name" "" "$duration"
    else
        local end_time=$(date +%s.%N)
        local duration=$(echo "$end_time - $start_time" | bc -l)
        test_result 1 "$test_name" "Run: python -m pytest $test_path -v for details" "$duration"
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

# Record start time for performance measurement
START_TIME=$(date +%s)

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

# Generate JSON results
generate_json_results() {
    local success_rate=0
    if [ $TOTAL_TESTS -gt 0 ]; then
        success_rate=$(echo "scale=2; ($PASSED_TESTS * 100) / $TOTAL_TESTS" | bc -l)
    fi
    
    # Create detailed results JSON
    cat > "$RESULTS_FILE" <<EOF
{
  "metadata": {
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
    "test_version": "1.0",
    "environment": "development",
    "python_version": "$(python --version 2>&1 | cut -d' ' -f2)",
    "pytest_version": "$(python -m pytest --version 2>&1 | head -1 | cut -d' ' -f3)"
  },
  "configuration": {
    "layer": "$LAYER",
    "connector": "${CONNECTOR:-all}",
    "mode": "full",
    "quick_mode": $QUICK_MODE,
    "verbose": $VERBOSE_MODE
  },
  "summary": {
    "total_tests": $TOTAL_TESTS,
    "passed": $PASSED_TESTS,
    "failed": $FAILED_TESTS,
    "skipped": $SKIPPED_TESTS,
    "success_rate": $success_rate,
    "execution_time": $(echo "$(date +%s) - $START_TIME" | bc -l)
  },
  "test_results": [
$(IFS=','; echo "${TEST_RESULTS[*]}")
  ]
}
EOF

    # Create summary JSON
    local overall_status="SUCCESS"
    if [ $FAILED_TESTS -gt 0 ]; then
        overall_status="FAILED"
    elif [ $SKIPPED_TESTS -gt 0 ]; then
        overall_status="PARTIAL_SUCCESS"
    fi
    
    cat > "$SUMMARY_FILE" <<EOF
{
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "test_run_id": "$TIMESTAMP",
  "overall_status": "$overall_status",
  "summary": {
    "total_tests": $TOTAL_TESTS,
    "passed": $PASSED_TESTS,
    "failed": $FAILED_TESTS,
    "skipped": $SKIPPED_TESTS,
    "success_rate": $success_rate
  },
  "configuration": {
    "layer": "$LAYER",
    "connector": "${CONNECTOR:-all}"
  }
}
EOF

    # Create symlinks to latest results
    cd "$RESULTS_DIR"
    ln -sf "test_results_$TIMESTAMP.json" "latest_results.json"
    ln -sf "summary_$TIMESTAMP.json" "latest_summary.json"
    
    echo -e "${BLUE}📄 Results saved to: $RESULTS_FILE${NC}"
    echo -e "${BLUE}📄 Summary saved to: $SUMMARY_FILE${NC}"
}

if [ $TOTAL_TESTS -gt 0 ]; then
    SUCCESS_RATE=$(( (PASSED_TESTS * 100) / TOTAL_TESTS ))
    echo -e "Success Rate: ${BLUE}$SUCCESS_RATE%${NC}"
    
    # Generate JSON results
    generate_json_results
    
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
