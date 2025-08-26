#!/bin/bash

# Unicorn Investing Platform - Health Check Script
# Verifies all components are properly configured and functional

echo "🦄 Unicorn Investing Platform - Comprehensive Health Check"
echo "=========================================================="

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

# Function to check and report status
check_status() {
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        echo -e "${RED}❌ $2${NC}"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        if [ ! -z "$3" ]; then
            echo -e "${YELLOW}   → $3${NC}"
        fi
    fi
}

echo -e "${BLUE}🔧 System Components${NC}"
echo "===================="

# Check if we're in the right directory
if [ ! -f "README.md" ] || [ ! -d "BackendPython" ]; then
    echo -e "${RED}❌ ERROR: Must run from /workspaces/unicorninvesting directory${NC}"
    exit 1
fi

# 1. Operating System Check
OS_CHECK=$(lsb_release -rs 2>/dev/null | grep -E "24\.04|22\.04")
if [ ! -z "$OS_CHECK" ]; then
    check_status 0 "Operating System: Ubuntu $OS_CHECK"
else
    check_status 1 "Operating System: Ubuntu 24.04/22.04 recommended"
fi

# 2. Git Repository Status
git status >/dev/null 2>&1
check_status $? "Git Repository: Initialized and accessible"

# 3. Git Submodules (LEAN)
if [ -d "BackendPython/Lean/.git" ]; then
    check_status 0 "LEAN Submodule: Properly initialized"
else
    check_status 1 "LEAN Submodule: Missing or not initialized" "Run: git submodule update --init --recursive"
fi

echo -e "\n${BLUE}🐍 Python Environment${NC}"
echo "====================="

# 4. Python Virtual Environment
if [ -f ".venv/bin/python" ]; then
    check_status 0 "Virtual Environment: Available at .venv/"
    
    # Activate virtual environment for subsequent checks
    source .venv/bin/activate
    
    # 5. Python Version
    PYTHON_VERSION=$(python --version 2>&1 | grep -o "[0-9]\+\.[0-9]\+\.[0-9]\+")
    if [[ "$PYTHON_VERSION" =~ ^3\.1[2-9]\. ]]; then
        check_status 0 "Python Version: $PYTHON_VERSION (compatible)"
    else
        check_status 1 "Python Version: $PYTHON_VERSION (3.12+ recommended)"
    fi
    
    # 6. Core Package Tests
    python -c "import fastapi, uvicorn" >/dev/null 2>&1
    check_status $? "FastAPI Framework: Installed and importable"
    
    python -c "import pandas, numpy, scipy" >/dev/null 2>&1
    check_status $? "Data Science Libraries: pandas, numpy, scipy"
    
    python -c "import sklearn, tensorflow" >/dev/null 2>&1
    check_status $? "Machine Learning Libraries: scikit-learn, tensorflow"
    
    python -c "import prophet" >/dev/null 2>&1
    check_status $? "Prophet Forecasting: Installed and importable"
    
    python -c "import yfinance, alpha_vantage" >/dev/null 2>&1
    check_status $? "Financial Data Libraries: yfinance, alpha_vantage"
    
    python -c "import sqlalchemy, pymysql" >/dev/null 2>&1
    check_status $? "Database Libraries: SQLAlchemy, PyMySQL"
    
else
    check_status 1 "Virtual Environment: Missing" "Run: python3 -m venv .venv && source .venv/bin/activate"
fi

echo -e "\n${BLUE}🌐 Web Server & Database${NC}"
echo "========================"

# 7. MySQL Service
systemctl is-active mysql >/dev/null 2>&1
check_status $? "MySQL Service: Running"

# 8. Apache Service
systemctl is-active apache2 >/dev/null 2>&1
check_status $? "Apache Service: Running"

# 9. Database Connection Test
if command -v mysql >/dev/null 2>&1; then
    mysql -e "SELECT 1;" >/dev/null 2>&1
    check_status $? "Database Connection: Accessible"
else
    check_status 1 "MySQL Client: Not installed"
fi

# 10. PHP Installation
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

echo -e "\n${BLUE}🔒 Security & SSL${NC}"
echo "=================="

# 11. SSL Certificates
if command -v certbot >/dev/null 2>&1; then
    CERT_COUNT=$(sudo certbot certificates 2>/dev/null | grep -c "Certificate Name:")
    if [ "$CERT_COUNT" -gt 0 ]; then
        check_status 0 "SSL Certificates: $CERT_COUNT certificates found"
    else
        check_status 1 "SSL Certificates: No certificates found"
    fi
else
    check_status 1 "Certbot: Not installed"
fi

# 12. Firewall Status
if command -v ufw >/dev/null 2>&1; then
    ufw status | grep -q "Status: active"
    check_status $? "UFW Firewall: Active"
else
    check_status 1 "UFW Firewall: Not available"
fi

echo -e "\n${BLUE}🚀 Application Services${NC}"
echo "======================="

# 13. FastAPI Application Test
if [ -f ".venv/bin/python" ]; then
    source .venv/bin/activate
    cd BackendPython/unicorn/backend
    timeout 10s python -c "
from api.main import app
print('FastAPI application initialized successfully')
" >/dev/null 2>&1
    check_status $? "FastAPI Application: Loads without errors"
    cd - >/dev/null
else
    check_status 1 "FastAPI Application: Cannot test (no Python environment)"
fi

# 14. Prophet Functionality Test
if [ -f ".venv/bin/python" ] && [ -f "BackendPython/unicorn/algorithms/test_prophet.py" ]; then
    source .venv/bin/activate
    cd BackendPython/unicorn/algorithms
    python test_prophet.py >/dev/null 2>&1
    check_status $? "Prophet Forecasting: Functional test passed"
    cd - >/dev/null
else
    check_status 1 "Prophet Forecasting: Test script not found"
fi

# 15. LEAN Framework
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

echo -e "\n${BLUE}📁 Directory Structure${NC}"
echo "======================"

# 16. Key Directories Check
REQUIRED_DIRS=(
    "backend"
    "BackendPython"
    "BackendPython/unicorn"
    "BackendPython/unicorn/backend"
    "BackendPython/unicorn/algorithms"
    "database"
    "frontend"
    "WebFrontend"
    "docs"
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

# 17. Documentation Files
DOC_FILES=("README.md" "INSTALLATION.md" "deploy.yml" "MIGRATION_SUMMARY.md")
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

echo -e "\n${BLUE}🧪 Functional Tests${NC}"
echo "==================="

# 18. Prophet Demo Test
if [ -f ".venv/bin/python" ] && [ -f "BackendPython/unicorn/algorithms/prophet_forex_demo.py" ]; then
    source .venv/bin/activate
    cd BackendPython/unicorn/algorithms
    timeout 30s python prophet_forex_demo.py >/dev/null 2>&1
    check_status $? "Prophet Demo: Completed successfully"
    cd - >/dev/null
else
    check_status 1 "Prophet Demo: Script not available"
fi

# 19. API Health Check (if running)
if command -v curl >/dev/null 2>&1; then
    curl -s http://localhost:8000/health >/dev/null 2>&1
    if [ $? -eq 0 ]; then
        check_status 0 "API Endpoint: Responding at localhost:8000"
    else
        check_status 1 "API Endpoint: Not responding" "Start with: uvicorn api.main:app"
    fi
else
    check_status 1 "curl: Not available for API testing"
fi

echo -e "\n${BLUE}📊 Summary${NC}"
echo "==========="

echo -e "Total Checks: ${BLUE}$TOTAL_CHECKS${NC}"
echo -e "Passed: ${GREEN}$PASSED_CHECKS${NC}"
echo -e "Failed: ${RED}$FAILED_CHECKS${NC}"

PASS_RATE=$((PASSED_CHECKS * 100 / TOTAL_CHECKS))
echo -e "Success Rate: ${BLUE}$PASS_RATE%${NC}"

if [ $FAILED_CHECKS -eq 0 ]; then
    echo -e "\n${GREEN}🎉 All checks passed! Platform is ready for use.${NC}"
    exit 0
elif [ $PASS_RATE -ge 80 ]; then
    echo -e "\n${YELLOW}⚠️  Platform is mostly functional with $FAILED_CHECKS minor issues.${NC}"
    exit 1
else
    echo -e "\n${RED}🚨 Platform has significant issues requiring attention.${NC}"
    echo -e "\n${BLUE}📖 For setup instructions, see:${NC}"
    echo -e "   • INSTALLATION.md - Complete installation guide"
    echo -e "   • deploy.yml - Deployment configuration"
    echo -e "   • README.md - Project overview"
    exit 2
fi
