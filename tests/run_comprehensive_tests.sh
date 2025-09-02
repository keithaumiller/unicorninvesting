#!/bin/bash
#
# Comprehensive Test Runner for Unicorn Investing Platform
# Executes all validation tests and provides detailed reporting
#

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Test results tracking
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    case $status in
        "SUCCESS") echo -e "${GREEN}✅ $message${NC}" ;;
        "FAILED")  echo -e "${RED}❌ $message${NC}" ;;
        "WARNING") echo -e "${YELLOW}⚠️  $message${NC}" ;;
        "INFO")    echo -e "${BLUE}ℹ️  $message${NC}" ;;
        "RUNNING") echo -e "${CYAN}🔄 $message${NC}" ;;
    esac
}

# Function to run a test and track results
run_test() {
    local test_name=$1
    local test_command=$2
    local test_description=$3
    
    print_status "RUNNING" "Running $test_description..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if eval "$test_command" > /tmp/test_${test_name}.log 2>&1; then
        print_status "SUCCESS" "$test_description: PASSED"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        print_status "FAILED" "$test_description: FAILED"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        echo "   📄 Log: /tmp/test_${test_name}.log"
        return 1
    fi
}

# Function to show test summary
show_summary() {
    echo
    print_status "INFO" "TEST EXECUTION SUMMARY"
    echo "================================"
    echo "Total Tests: $TOTAL_TESTS"
    echo "Passed: $PASSED_TESTS"
    echo "Failed: $FAILED_TESTS"
    
    if [ $TOTAL_TESTS -gt 0 ]; then
        SUCCESS_RATE=$(( (PASSED_TESTS * 100) / TOTAL_TESTS ))
        echo "Success Rate: ${SUCCESS_RATE}%"
        
        if [ $SUCCESS_RATE -ge 80 ]; then
            print_status "SUCCESS" "OVERALL STATUS: SYSTEM READY"
            echo "💯 Platform validated and ready for deployment"
        else
            print_status "WARNING" "OVERALL STATUS: ISSUES DETECTED"
            echo "🔧 Platform needs attention before deployment"
        fi
    fi
}

# Main execution
main() {
    echo -e "${PURPLE}🚀 UNICORN PLATFORM COMPREHENSIVE TESTING${NC}"
    echo "=========================================="
    echo "Started: $(date)"
    echo
    
    # Change to tests directory
    cd /workspaces/unicorninvesting/tests
    
    # Test 1: Complete System Validation (Master Test)
    run_test "complete_system" \
             "python system/test_complete_system_validation.py" \
             "Complete System Validation"
    
    # Test 2: System Architecture Validation
    run_test "architecture" \
             "python system/test_system_architecture.py" \
             "System Architecture Validation"
    
    # Test 3: Kelly Criterion Algorithm
    run_test "kelly" \
             "python unicorn/4_portfolios/utilities/test_kelly_criterion.py" \
             "Kelly Criterion Algorithm Test"
    
    # Test 4: ETH Basic Risk Management
    run_test "risk" \
             "python unicorn/3_risk_algorithms/test_eth_basic_risk.py" \
             "ETH Basic Risk Management Test"
    
    # Test 5: ETH Kelly Integration
    run_test "integration" \
             "python unicorn/4_portfolios/Myportolio/test_eth_kelly_integration.py" \
             "ETH Kelly Integration Test"
    
    # Test 6: Legacy pytest suite (if available)
    if [ -f "pytest.ini" ]; then
        run_test "pytest" \
                 "python -m pytest -x --tb=short" \
                 "Legacy Pytest Suite"
    fi
    
    # Test 7: System Health Check (via script)
    run_test "health" \
             "cd /workspaces/unicorninvesting && ./scripts/unicorn_environment.sh --check-only" \
             "System Health Check"
    
    show_summary
    echo
    echo "Completed: $(date)"
    
    # Exit with appropriate code
    if [ $FAILED_TESTS -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Unicorn Platform Test Runner"
        echo ""
        echo "Usage: $0 [OPTIONS]"
        echo ""
        echo "Options:"
        echo "  --help, -h          Show this help message"
        echo "  --quick, -q         Run only essential tests (faster)"
        echo "  --verbose, -v       Show detailed output"
        echo "  --clean, -c         Clean test artifacts before running"
        echo ""
        echo "Examples:"
        echo "  $0                  # Run all tests"
        echo "  $0 --quick          # Run essential tests only"
        echo "  $0 --verbose        # Run with detailed output"
        exit 0
        ;;
    --quick|-q)
        # Quick mode - run only essential tests
        main() {
            echo -e "${PURPLE}🚀 UNICORN PLATFORM QUICK TESTING${NC}"
            echo "====================================="
            echo "Started: $(date)"
            echo
            
            cd /workspaces/unicorninvesting/tests
            
            run_test "complete_system" \
                     "python system/test_complete_system_validation.py" \
                     "Complete System Validation"
            
            run_test "integration" \
                     "python unicorn/4_portfolios/Myportolio/test_eth_kelly_integration.py" \
                     "ETH Kelly Integration Test"
            
            show_summary
            echo "Completed: $(date)"
            
            if [ $FAILED_TESTS -eq 0 ]; then exit 0; else exit 1; fi
        }
        main
        ;;
    --clean|-c)
        echo "🧹 Cleaning test artifacts..."
        rm -f /tmp/test_*.log
        find /workspaces/unicorninvesting/tests -name "*.pyc" -delete
        find /workspaces/unicorninvesting/tests -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
        echo "✅ Test artifacts cleaned"
        main
        ;;
    --verbose|-v)
        # Verbose mode - show all output
        run_test() {
            local test_name=$1
            local test_command=$2
            local test_description=$3
            
            print_status "RUNNING" "Running $test_description..."
            TOTAL_TESTS=$((TOTAL_TESTS + 1))
            
            if eval "$test_command"; then
                print_status "SUCCESS" "$test_description: PASSED"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                return 0
            else
                print_status "FAILED" "$test_description: FAILED"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                return 1
            fi
        }
        main
        ;;
    *)
        main
        ;;
esac
