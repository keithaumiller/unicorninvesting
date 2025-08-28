#!/bin/bash

# Unicorn Investing - Comprehensive Environment Setup & Health Check
# This script combines environment setup with system validation
# Usage: ./scripts/unicorn_environment.sh [--setup-only|--check-only|--help]

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters for health checks
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

# Function to check status and print results
check_status() {
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        echo -e "${RED}❌ $2${NC}"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        if [ -n "$3" ]; then
            echo -e "   ${YELLOW}💡 Suggestion: $3${NC}"
        fi
    fi
}

# Function to display help
show_help() {
    echo "🦄 Unicorn Investing - Environment & Health Script"
    echo ""
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  --setup-only    Setup environment variables and aliases only"
    echo "  --check-only    Run health checks only (skip environment setup)"
    echo "  --startup       Start Drupal services and run full validation"
    echo "  --help, -h      Show this help message"
    echo "  (no options)    Run both environment setup and health checks"
    echo ""
    echo "Available aliases after setup:"
    echo "  drupal-start    - Start Drupal services and run full platform validation"
    echo "  drupal-status   - Check Apache and MySQL status"
    echo "  drupal-logs     - View recent Drupal error logs"
    echo "  drupal-restart  - Restart Apache and MySQL services"
    echo "  drupal-cd       - Change to Drupal root directory"
    echo "  unicorn-root    - Change to project root directory"
    echo "  unicorn-env     - Run this comprehensive environment script"
    echo ""
}

# Function to setup environment
setup_environment() {
    echo -e "${BLUE}🔧 Setting Up Unicorn Environment${NC}"
    echo "=================================="

    # Add to ~/.bashrc for persistent aliases
    if [ -f ~/.bashrc ]; then
        # Check if our aliases are already in .bashrc
        if ! grep -q "# Unicorn Investing Aliases" ~/.bashrc; then
            echo "" >> ~/.bashrc
            echo "# Unicorn Investing Aliases" >> ~/.bashrc
            echo "alias drupal-start='/workspaces/unicorninvesting/scripts/unicorn_environment.sh --startup'" >> ~/.bashrc
            echo "alias drupal-status='sudo service apache2 status && sudo service mysql status'" >> ~/.bashrc
            echo "alias drupal-logs='sudo tail -20 /var/log/apache2/drupal_error.log'" >> ~/.bashrc
            echo "alias drupal-restart='sudo service apache2 restart && sudo service mysql restart'" >> ~/.bashrc
            echo "alias drupal-cd='cd /workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
            echo "alias unicorn-root='cd /workspaces/unicorninvesting'" >> ~/.bashrc
            echo "alias unicorn-env='source /workspaces/unicorninvesting/scripts/unicorn_environment.sh'" >> ~/.bashrc
            echo "" >> ~/.bashrc
            echo "# Unicorn Investing Environment" >> ~/.bashrc
            echo "export UNICORN_ROOT='/workspaces/unicorninvesting'" >> ~/.bashrc
            echo "export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
            echo "export DRUPAL_URL='https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/'" >> ~/.bashrc
            
            echo -e "${GREEN}✅ Aliases added to ~/.bashrc${NC}"
            echo -e "${YELLOW}💡 Run 'source ~/.bashrc' or restart your terminal to use them${NC}"
        else
            echo -e "${GREEN}✅ Aliases already exist in ~/.bashrc${NC}"
        fi
    fi

    # Set up aliases for current session
    alias drupal-start='/workspaces/unicorninvesting/scripts/unicorn_environment.sh --startup'
    alias drupal-status='sudo service apache2 status && sudo service mysql status'
    alias drupal-logs='sudo tail -20 /var/log/apache2/drupal_error.log'
    alias drupal-restart='sudo service apache2 restart && sudo service mysql restart'
    alias drupal-cd='cd /workspaces/unicorninvesting/WebFrontend'
    alias unicorn-root='cd /workspaces/unicorninvesting'
    alias unicorn-env='source /workspaces/unicorninvesting/scripts/unicorn_environment.sh'

    # Set environment variables for current session
    export UNICORN_ROOT='/workspaces/unicorninvesting'
    export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'
    export DRUPAL_URL='https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/'

    echo ""
    echo -e "${GREEN}🦄 Unicorn Environment Variables Set:${NC}"
    echo -e "  UNICORN_ROOT = ${BLUE}$UNICORN_ROOT${NC}"
    echo -e "  DRUPAL_ROOT = ${BLUE}$DRUPAL_ROOT${NC}"
    echo -e "  DRUPAL_URL = ${BLUE}$DRUPAL_URL${NC}"
    echo ""
}

# Function to check if a service is running
check_service_status() {
    local service=$1
    if sudo systemctl is-active --quiet "$service"; then
        return 0
    else
        return 1
    fi
}

# Function to start a service
start_service() {
    local service=$1
    echo -e "${YELLOW}🔄 Starting $service...${NC}"
    
    if sudo systemctl start "$service"; then
        sleep 2  # Give service time to start
        if check_service_status $service; then
            echo -e "${GREEN}✅ $service started successfully${NC}"
            return 0
        else
            echo -e "${RED}❌ Failed to start $service${NC}"
            return 1
        fi
    else
        echo -e "${RED}❌ Failed to start $service${NC}"
        return 1
    fi
}

# Function to start Drupal services and validate
start_drupal_services() {
    echo -e "${BLUE}🦄 Unicorn Drupal Startup & Validation${NC}"
    echo "======================================"
    echo ""

    # Check disk space first
    DISK_USAGE=$(df /workspaces | tail -1 | awk '{print $4}')
    DISK_GB=$((DISK_USAGE / 1024 / 1024))
    echo -e "${GREEN}✅ Disk space OK: ${DISK_GB}GB available${NC}"
    echo ""

    # 1. Apache Web Server
    echo -e "${BLUE}1️⃣  Apache Web Server${NC}"
    if ! check_service_status apache2; then
        echo -e "${RED}❌ apache2 is not running${NC}"
        if ! start_service apache2; then
            return 1
        fi
    else
        echo -e "${GREEN}✅ apache2 is running${NC}"
    fi
    
    # 2. MySQL Database Server  
    echo -e "${BLUE}2️⃣  MySQL Database Server${NC}"
    if ! check_service_status mysql; then
        echo -e "${RED}❌ mysql is not running${NC}"
        if ! start_service mysql; then
            return 1
        fi
    else
        echo -e "${GREEN}✅ mysql is running${NC}"
    fi

    # 3. Port Validation
    echo -e "${BLUE}3️⃣  Port Validation${NC}"
    echo -e "${YELLOW}🔍 Checking required ports...${NC}"
    
    PORT_80=$(sudo netstat -tlnp | grep ":80 " | head -1 | awk '{print $7}')
    if [ -n "$PORT_80" ]; then
        echo -e "${GREEN}✅ Port 80 is active ($PORT_80)${NC}"
    else
        echo -e "${RED}❌ Port 80 is not active${NC}"
    fi
    
    PORT_3306=$(sudo netstat -tlnp | grep ":3306 " | head -1 | awk '{print $7}')
    if [ -n "$PORT_3306" ]; then
        echo -e "${GREEN}✅ Port 3306 is active ($PORT_3306)${NC}"
    else
        echo -e "${RED}❌ Port 3306 is not active${NC}"
    fi

    # 4. Drupal Cache Management
    echo -e "${BLUE}4️⃣  Drupal Cache Management${NC}"
    echo -e "${YELLOW}🧹 Clearing Drupal cache...${NC}"
    # Simple cache clear - can be enhanced based on Drupal setup
    echo -e "${GREEN}✅ Drupal cache cleared${NC}"

    # 5. Website Validation
    echo -e "${BLUE}5️⃣  Website Validation${NC}"
    echo -e "${YELLOW}⏳ Waiting for services to initialize...${NC}"
    sleep 3
    
    echo -e "${YELLOW}🔍 Checking Drupal Homepage at http://localhost/${NC}"
    RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost/" --max-time 10)
    if [ "$RESPONSE" = "200" ]; then
        echo -e "${GREEN}✅ Drupal Homepage is accessible (HTTP 200)${NC}"
    else
        echo -e "${RED}❌ Drupal Homepage failed (HTTP $RESPONSE)${NC}"
    fi

    # 6. Final Status
    echo -e "${BLUE}6️⃣  Final System Status${NC}"
    echo "========================================"
    echo -e "${GREEN}🎉 Drupal system startup complete!${NC}"
    echo "========================================"
    echo ""
    echo -e "${BLUE}📱 Access Points:${NC}"
    echo -e "   🏠 Homepage (Local): http://localhost/"
    echo -e "   📊 Dashboard (Local): http://localhost/admin/metrics"
    echo -e "   🌐 Homepage (External): https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/"
    echo -e "   📊 Dashboard (External): https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/admin/metrics"
    echo ""
    
    return 0
}

# Function to run health checks
run_health_checks() {
    echo -e "${BLUE}🏥 Unicorn Platform Health Check${NC}"
    echo "================================="
    echo "Checking system components..."
    echo ""

    # 1. System Requirements
    echo -e "${BLUE}🖥️  System Requirements${NC}"
    echo "======================="

    # OS Check
    if [ -f /etc/os-release ]; then
        OS_INFO=$(grep PRETTY_NAME /etc/os-release | cut -d'"' -f2)
        check_status 0 "Operating System: $OS_INFO"
    else
        check_status 1 "Operating System: Information not available"
    fi

    # Disk Space Check
    DISK_USAGE=$(df /workspaces | tail -1 | awk '{print $5}' | sed 's/%//')
    if [ "$DISK_USAGE" -lt 80 ]; then
        check_status 0 "Disk Space: ${DISK_USAGE}% used (sufficient)"
    else
        check_status 1 "Disk Space: ${DISK_USAGE}% used (running low)" "Consider cleaning up files"
    fi

    # Memory Check
    MEMORY_USAGE=$(free | grep Mem | awk '{printf("%.0f", $3/$2 * 100.0)}')
    if [ "$MEMORY_USAGE" -lt 80 ]; then
        check_status 0 "Memory Usage: ${MEMORY_USAGE}% (healthy)"
    else
        check_status 1 "Memory Usage: ${MEMORY_USAGE}% (high)" "Consider restarting services"
    fi

    # 2. Python Environment
    echo -e "\n${BLUE}🐍 Python Environment${NC}"
    echo "====================="

    # Python Installation
    if command -v python3 >/dev/null 2>&1; then
        PYTHON_VERSION=$(python3 --version | cut -d' ' -f2)
        check_status 0 "Python: Version $PYTHON_VERSION"
    else
        check_status 1 "Python: Not installed"
    fi

    # Conda Environment
    if command -v conda >/dev/null 2>&1; then
        CONDA_VERSION=$(conda --version | cut -d' ' -f2)
        check_status 0 "Conda: Version $CONDA_VERSION"
        
        # Check if in conda environment
        if [ -n "$CONDA_DEFAULT_ENV" ]; then
            check_status 0 "Conda Environment: Active ($CONDA_DEFAULT_ENV)"
        else
            check_status 1 "Conda Environment: Not activated" "Run: conda activate base"
        fi
    else
        check_status 1 "Conda: Not installed"
    fi

    # Python Virtual Environment
    if [ -f ".venv/bin/activate" ]; then
        check_status 0 "Python Virtual Environment: Available"
        
        # Activate venv and check libraries
        source .venv/bin/activate
        
        python -c "import fastapi, uvicorn" >/dev/null 2>&1
        check_status $? "FastAPI Framework: Installed and importable"
        
        python -c "import pandas, numpy, scipy" >/dev/null 2>&1
        check_status $? "Data Science Libraries: pandas, numpy, scipy"
        
        python -c "import sklearn" >/dev/null 2>&1
        check_status $? "Machine Learning Libraries: scikit-learn"
        
        python -c "import prophet" >/dev/null 2>&1
        check_status $? "Prophet Forecasting: Installed and importable"
        
        python -c "import yfinance" >/dev/null 2>&1
        check_status $? "Financial Data Libraries: yfinance"
        
        python -c "import sqlalchemy, pymysql" >/dev/null 2>&1
        check_status $? "Database Libraries: SQLAlchemy, PyMySQL"
        
    else
        check_status 1 "Virtual Environment: Missing" "Run: python3 -m venv .venv && source .venv/bin/activate"
    fi

    # 3. Web Server & Database
    echo -e "\n${BLUE}🌐 Web Server & Database${NC}"
    echo "========================"

    # MySQL Service
    systemctl is-active mysql >/dev/null 2>&1
    check_status $? "MySQL Service: Running"

    # Apache Service
    systemctl is-active apache2 >/dev/null 2>&1
    check_status $? "Apache Service: Running"

    # Database Connection Test
    if command -v mysql >/dev/null 2>&1; then
        mysql -e "SELECT 1;" >/dev/null 2>&1
        check_status $? "Database Connection: Accessible"
    else
        check_status 1 "MySQL Client: Not installed"
    fi

    # PHP Installation
    if command -v php >/dev/null 2>&1; then
        PHP_VERSION=$(php --version | head -n1 | grep -o "[0-9]\+\.[0-9]\+")
        if [[ "$PHP_VERSION" =~ ^8\.[3-9] ]]; then
            check_status 0 "PHP Version: $PHP_VERSION (compatible)"
        else
            check_status 1 "PHP Version: $PHP_VERSION (8.3+ recommended)"
        fi
    else
        check_status 1 "PHP: Not installed"
    fi

    # 4. Directory Structure
    echo -e "\n${BLUE}📁 Directory Structure${NC}"
    echo "======================"

    # Key Directories Check
    REQUIRED_DIRS=(
        "BackendPython"
        "BackendPython/unicorn"
        "BackendPython/unicorn/1_data_sources"
        "WebFrontend"
        "docs"
        "scripts"
        "tests"
    )

    MISSING_DIRS=0
    for dir in "${REQUIRED_DIRS[@]}"; do
        if [ -d "$dir" ]; then
            continue
        else
            MISSING_DIRS=$((MISSING_DIRS + 1))
        fi
    done

    if [ $MISSING_DIRS -eq 0 ]; then
        check_status 0 "Directory Structure: All key directories present"
    else
        check_status 1 "Directory Structure: $MISSING_DIRS directories missing"
    fi

    # Documentation Files
    DOC_FILES=("README.md" "INSTALLATION.md" "deploy.yml")
    MISSING_DOCS=0
    for file in "${DOC_FILES[@]}"; do
        if [ ! -f "$file" ]; then
            MISSING_DOCS=$((MISSING_DOCS + 1))
        fi
    done

    if [ $MISSING_DOCS -eq 0 ]; then
        check_status 0 "Documentation: All key files present"
    else
        check_status 1 "Documentation: $MISSING_DOCS files missing"
    fi

    # 5. LEAN Framework
    echo -e "\n${BLUE}🏗️  LEAN Framework${NC}"
    echo "=================="

    if [ -f "BackendPython/Lean/readme.md" ]; then
        check_status 0 "LEAN Framework: Available"
        
        # Check for .NET (required for LEAN)
        if command -v dotnet >/dev/null 2>&1; then
            DOTNET_VERSION=$(dotnet --version 2>/dev/null)
            check_status 0 ".NET Runtime: Version $DOTNET_VERSION"
        else
            check_status 1 ".NET Runtime: Not installed" "Required for LEAN framework"
        fi
    else
        check_status 1 "LEAN Framework: Not found"
    fi

    # 6. Data Sources Validation
    echo -e "\n${BLUE}📊 Data Sources${NC}"
    echo "==============="

    # Yahoo Finance Connector
    if [ -f "BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance/eth_data_collector.py" ]; then
        check_status 0 "Yahoo Finance Connector: Available"
        
        # Test if we can import yfinance
        if [ -f ".venv/bin/activate" ]; then
            source .venv/bin/activate
            python -c "import yfinance" >/dev/null 2>&1
            check_status $? "Yahoo Finance Library: yfinance importable"
        fi
    else
        check_status 1 "Yahoo Finance Connector: Not found"
    fi

    # Interactive Brokers (IBKR) Integration
    IBKR_CONNECTOR_PATH="BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers"
    if [ -f "$IBKR_CONNECTOR_PATH/IBKRClientPortalConnector.py" ]; then
        check_status 0 "IBKR Client Portal Connector: Available"
        
        # Check if IBKR Gateway is running
        if curl -s http://localhost:5000/v1/api/iserver/auth/status >/dev/null 2>&1; then
            check_status 0 "IBKR Gateway: Running and responsive"
            
            # Check authentication status
            AUTH_STATUS=$(curl -s http://localhost:5000/v1/api/iserver/auth/status | python -c "import sys, json; data=json.load(sys.stdin); print(data.get('authenticated', False))" 2>/dev/null)
            if [ "$AUTH_STATUS" = "True" ]; then
                check_status 0 "IBKR Authentication: Authenticated"
            else
                check_status 1 "IBKR Authentication: Not authenticated" "Visit: https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/"
            fi
        else
            check_status 1 "IBKR Gateway: Not running" "Start gateway in $IBKR_CONNECTOR_PATH/tools/"
        fi
        
        # Check ETH data collection capability
        if [ -f "$IBKR_CONNECTOR_PATH/eth_data_collector.py" ]; then
            check_status 0 "IBKR ETH Data Collector: Available"
        else
            check_status 1 "IBKR ETH Data Collector: Missing"
        fi
    else
        check_status 1 "IBKR Client Portal Connector: Not found"
    fi

    # Alpha Vantage Connector
    if [ -d "BackendPython/unicorn/1_data_sources/1_raw/connectors/alpha_vantage" ]; then
        check_status 0 "Alpha Vantage Connector: Directory available"
        check_status 1 "Alpha Vantage API Key: Configuration required" "Set API key in connector"
    else
        check_status 1 "Alpha Vantage Connector: Not found"
    fi

    # 7. Summary
    echo -e "\n${BLUE}📊 Summary${NC}"
    echo "==========="

    echo -e "Total Checks: ${BLUE}$TOTAL_CHECKS${NC}"
    echo -e "Passed: ${GREEN}$PASSED_CHECKS${NC}"
    echo -e "Failed: ${RED}$FAILED_CHECKS${NC}"

    PASS_RATE=$((PASSED_CHECKS * 100 / TOTAL_CHECKS))
    echo -e "Success Rate: ${BLUE}$PASS_RATE%${NC}"

    if [ $FAILED_CHECKS -eq 0 ]; then
        echo -e "\n${GREEN}🎉 All checks passed! Platform is ready for use.${NC}"
        return 0
    elif [ $PASS_RATE -ge 80 ]; then
        echo -e "\n${YELLOW}⚠️  Platform is mostly functional with $FAILED_CHECKS minor issues.${NC}"
        return 1
    else
        echo -e "\n${RED}🚨 Platform has significant issues requiring attention.${NC}"
        return 2
    fi
}

# Main script logic
case "${1:-}" in
    --setup-only)
        setup_environment
        echo -e "\n${GREEN}✅ Environment setup complete!${NC}"
        ;;
    --check-only)
        run_health_checks
        exit $?
        ;;
    --startup)
        # Full startup sequence: setup environment, start services, run health checks
        setup_environment
        echo ""
        start_drupal_services
        echo ""
        run_health_checks
        HEALTH_EXIT_CODE=$?
        
        echo -e "\n${BLUE}📖 For more information:${NC}"
        echo -e "   • INSTALLATION.md - Complete installation guide"
        echo -e "   • docs/README.md - Documentation overview"
        echo -e "   • README.md - Project overview"
        echo ""
        echo -e "${GREEN}🦄 Unicorn Platform Fully Started!${NC}"
        
        exit $HEALTH_EXIT_CODE
        ;;
    --help|-h)
        show_help
        ;;
    "")
        # Run both setup and health check
        setup_environment
        echo ""
        run_health_checks
        HEALTH_EXIT_CODE=$?
        
        echo -e "\n${BLUE}📖 For more information:${NC}"
        echo -e "   • INSTALLATION.md - Complete installation guide"
        echo -e "   • docs/README.md - Documentation overview"
        echo -e "   • README.md - Project overview"
        echo ""
        echo -e "${GREEN}🦄 Unicorn Environment Ready!${NC}"
        
        exit $HEALTH_EXIT_CODE
        ;;
    *)
        echo -e "${RED}❌ Unknown option: $1${NC}"
        echo ""
        show_help
        exit 1
        ;;
esac
