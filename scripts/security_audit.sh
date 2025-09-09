#!/bin/bash

# Security Audit Script for Unicorn Investing Platform
# Scans for hardcoded credentials and provides migration guidance

echo "🔍 UNICORN INVESTING PLATFORM - SECURITY AUDIT"
echo "=============================================="
echo

# Set colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

PROJECT_ROOT="/workspaces/unicorninvesting"
cd "$PROJECT_ROOT"

echo "📂 Scanning project directory: $PROJECT_ROOT"
echo

# Function to check if a file should be scanned
should_scan() {
    local file=$1
    
    # Skip vendor directories, node_modules, .git, etc.
    if [[ $file == *"/vendor/"* ]] || [[ $file == *"/node_modules/"* ]] || [[ $file == *"/.git/"* ]] || [[ $file == *"/.venv/"* ]]; then
        return 1
    fi
    
    # Skip binary files
    if [[ $file == *".db"* ]] || [[ $file == *".sqlite"* ]] || [[ $file == *".gz"* ]] || [[ $file == *".zip"* ]]; then
        return 1
    fi
    
    return 0
}

# Check for API keys
echo "🔑 SCANNING FOR API KEYS..."
echo "----------------------------"
api_key_found=false

# Look for FRED API keys
while IFS= read -r -d '' file; do
    if should_scan "$file"; then
        if grep -l "e4de78babaac7891e9896f8fa390e675\|FRED.*KEY.*=" "$file" >/dev/null 2>&1; then
            echo -e "${RED}❌ FRED API key found in:${NC} $file"
            api_key_found=true
        fi
    fi
done < <(find . -type f -print0)

# Look for BEA API keys  
while IFS= read -r -d '' file; do
    if should_scan "$file"; then
        if grep -l "8E9AE912-2B48-435A-8910-521609627585\|BEA.*KEY.*=" "$file" >/dev/null 2>&1; then
            echo -e "${RED}❌ BEA API key found in:${NC} $file"
            api_key_found=true
        fi
    fi
done < <(find . -type f -print0)

# Look for generic API key patterns
while IFS= read -r -d '' file; do
    if should_scan "$file"; then
        if grep -l "api_key.*=.*['\"][^YOUR]" "$file" >/dev/null 2>&1; then
            echo -e "${YELLOW}⚠️  API key pattern found in:${NC} $file"
            grep -n "api_key.*=.*['\"][^YOUR]" "$file" | head -3
        fi
    fi
done < <(find . -type f -name "*.py" -print0)

if [ "$api_key_found" = false ]; then
    echo -e "${GREEN}✅ No hardcoded API keys found${NC}"
fi
echo

# Check for database credentials
echo "🗄️  SCANNING FOR DATABASE CREDENTIALS..."
echo "----------------------------------------"
db_creds_found=false

# Look for the specific hardcoded password
while IFS= read -r -d '' file; do
    if should_scan "$file"; then
        if grep -l "unicorn123\|password.*=.*unicorn" "$file" >/dev/null 2>&1; then
            echo -e "${RED}❌ Database password found in:${NC} $file"
            db_creds_found=true
        fi
    fi
done < <(find . -type f -print0)

# Look for database connection strings
while IFS= read -r -d '' file; do
    if should_scan "$file"; then
        if grep -l "mysql://.*:.*@\|host.*=.*localhost.*password" "$file" >/dev/null 2>&1; then
            echo -e "${YELLOW}⚠️  Database connection string found in:${NC} $file"
            grep -n "mysql://\|host.*=.*localhost" "$file" | head -2
        fi
    fi
done < <(find . -type f -name "*.py" -o -name "*.json" -o -name "*.yml" -o -name "*.yaml" -print0)

if [ "$db_creds_found" = false ]; then
    echo -e "${GREEN}✅ No hardcoded database credentials found${NC}"
fi
echo

# Check for IBKR credentials
echo "📈 SCANNING FOR IBKR CREDENTIALS..."
echo "-----------------------------------"
ibkr_creds_found=false

# Look for specific IBKR account details
while IFS= read -r -d '' file; do
    if should_scan "$file"; then
        if grep -l "DUM785491\|xyzyuc422" "$file" >/dev/null 2>&1; then
            echo -e "${RED}❌ IBKR credentials found in:${NC} $file"
            ibkr_creds_found=true
        fi
    fi
done < <(find . -type f -print0)

if [ "$ibkr_creds_found" = false ]; then
    echo -e "${GREEN}✅ No hardcoded IBKR credentials found${NC}"
fi
echo

# Check configuration setup
echo "⚙️  CHECKING CONFIGURATION SETUP..."
echo "-----------------------------------"

if [ -f "config/secrets.json" ]; then
    echo -e "${GREEN}✅ config/secrets.json exists${NC}"
    
    # Check if it still contains template values
    if grep -q "YOUR_" config/secrets.json; then
        echo -e "${YELLOW}⚠️  config/secrets.json contains template values (YOUR_*)${NC}"
        echo "   Please replace all YOUR_* placeholders with actual values"
    else
        echo -e "${GREEN}✅ config/secrets.json appears to be configured${NC}"
    fi
else
    echo -e "${RED}❌ config/secrets.json not found${NC}"
    echo "   Run: cp config/secrets.json.template config/secrets.json"
fi

if [ -f "config/secrets.json.template" ]; then
    echo -e "${GREEN}✅ config/secrets.json.template exists${NC}"
else
    echo -e "${YELLOW}⚠️  config/secrets.json.template missing${NC}"
fi

if [ -f "config/config_manager.py" ]; then
    echo -e "${GREEN}✅ config/config_manager.py exists${NC}"
else
    echo -e "${RED}❌ config/config_manager.py missing${NC}"
fi
echo

# Check .gitignore
echo "🚫 CHECKING .GITIGNORE..."
echo "-------------------------"
if grep -q "secrets.json" .gitignore; then
    echo -e "${GREEN}✅ secrets.json properly ignored by git${NC}"
else
    echo -e "${RED}❌ secrets.json not in .gitignore${NC}"
    echo "   Add 'config/secrets.json' to .gitignore"
fi

if grep -q "\*\*/.env" .gitignore; then
    echo -e "${GREEN}✅ .env files properly ignored${NC}"
else
    echo -e "${YELLOW}⚠️  .env files not in .gitignore${NC}"
fi
echo

# Security recommendations
echo "🛡️  SECURITY RECOMMENDATIONS..."
echo "-------------------------------"

echo -e "${BLUE}📋 Migration Checklist:${NC}"
echo "  1. Copy config/secrets.json.template to config/secrets.json"
echo "  2. Replace all YOUR_* placeholders with actual credentials"
echo "  3. Update any hardcoded credentials found above"
echo "  4. Test configuration with: python3 config/usage_examples.py"
echo "  5. Remove any old credential files"
echo

echo -e "${BLUE}🔧 Code Update Examples:${NC}"
echo
echo "  Replace hardcoded API keys:"
echo "  # Before:"
echo "  api_key = 'e4de78babaac7891e9896f8fa390e675'"
echo "  # After:"
echo "  from config.config_manager import get_api_key"
echo "  api_key = get_api_key('fred')"
echo
echo "  Replace database connections:"
echo "  # Before:"
echo "  connection = pymysql.connect(host='localhost', user='unicorn', password='unicorn123')"
echo "  # After:"
echo "  from config.config_manager import get_database_config"
echo "  db_config = get_database_config()"
echo "  connection = pymysql.connect(**db_config)"
echo

echo -e "${GREEN}🎉 Security audit complete!${NC}"
echo
echo "Next steps:"
echo "1. Address any security issues found above"
echo "2. Run: python3 config/usage_examples.py"
echo "3. Test your application with the new configuration system"
