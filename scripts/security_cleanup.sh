#!/bin/bash

# Security Cleanup Script - Unicorn Investing Platform
# Removes log files and temporary files containing sensitive data

echo "🧹 UNICORN INVESTING PLATFORM - SECURITY CLEANUP"
echo "================================================="
echo

PROJECT_ROOT="/workspaces/unicorninvesting"
cd "$PROJECT_ROOT"

# Set colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "📂 Cleaning up sensitive data from logs and temporary files..."
echo

# Clean up log files with API keys
echo "🗄️ Cleaning log files..."
if [ -f "logs/fred_delta.log" ]; then
    echo -e "${YELLOW}⚠️  Removing logs/fred_delta.log (contains FRED API key)${NC}"
    rm -f logs/fred_delta.log
fi

if [ -f "logs/bea_delta.log" ]; then
    echo -e "${YELLOW}⚠️  Removing logs/bea_delta.log (contains BEA API key)${NC}"
    rm -f logs/bea_delta.log
fi

# Clean up Python cache files that might contain credentials
echo "🐍 Cleaning Python cache files..."
find . -name "*.pyc" -path "*__pycache__*" -exec grep -l "DUM785491\|e4de78babaac7891e9896f8fa390e675\|8E9AE912-2B48-435A-8910-521609627585\|unicorn123" {} \; 2>/dev/null | while read file; do
    echo -e "${YELLOW}⚠️  Removing $file (contains credentials)${NC}"
    rm -f "$file"
done

# Clean up any temporary files with credentials
echo "🗂️ Cleaning temporary files..."
find . -name "*.tmp" -o -name "*.temp" | while read file; do
    if grep -l "DUM785491\|e4de78babaac7891e9896f8fa390e675\|8E9AE912-2B48-435A-8910-521609627585\|unicorn123" "$file" 2>/dev/null; then
        echo -e "${YELLOW}⚠️  Removing $file (contains credentials)${NC}"
        rm -f "$file"
    fi
done

# Create updated .gitignore entries for logs
echo "📝 Updating .gitignore for sensitive logs..."
if ! grep -q "# Sensitive logs" .gitignore; then
    cat >> .gitignore << EOF

# Sensitive logs and temporary files
logs/*_delta.log
logs/api_*.log
*.credentials
*.secrets
**/debug_*.log
EOF
    echo -e "${GREEN}✅ Updated .gitignore with sensitive log patterns${NC}"
fi

echo
echo "🔍 Verifying cleanup..."

# Check for remaining sensitive data in key locations
remaining_issues=0

# Check logs directory
if find logs/ -name "*.log" -exec grep -l "e4de78babaac7891e9896f8fa390e675\|8E9AE912-2B48-435A-8910-521609627585" {} \; 2>/dev/null | head -1; then
    echo -e "${RED}❌ API keys still found in logs directory${NC}"
    remaining_issues=$((remaining_issues + 1))
else
    echo -e "${GREEN}✅ Logs directory clean${NC}"
fi

# Check for sensitive cache files
if find . -name "*.pyc" -path "*__pycache__*" -exec grep -l "DUM785491\|unicorn123" {} \; 2>/dev/null | head -1; then
    echo -e "${RED}❌ Credentials still found in Python cache files${NC}"
    remaining_issues=$((remaining_issues + 1))
else
    echo -e "${GREEN}✅ Python cache files clean${NC}"
fi

echo
if [ $remaining_issues -eq 0 ]; then
    echo -e "${GREEN}🎉 Security cleanup complete!${NC}"
else
    echo -e "${YELLOW}⚠️  $remaining_issues issues still need attention${NC}"
fi

echo
echo "📋 Next steps:"
echo "1. Review any remaining issues above"
echo "2. Run: ./scripts/security_audit.sh"
echo "3. Commit the cleanup changes"
echo "4. Configure your actual credentials in config/secrets.json"
