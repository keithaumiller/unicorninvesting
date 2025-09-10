#!/bin/bash

# Comprehensive Security Audit Script - Unicorn Investing Platform  
# Dynamically loads credentials from secrets.json and scans for exposure elsewhere
# This script ONLY performs scanning and reporting - it does NOT delete files

echo "🔐 UNICORN INVESTING PLATFORM - COMPREHENSIVE SECURITY AUDIT"
echo "=============================================================="
echo

# Set colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

PROJECT_ROOT="/workspaces/unicorninvesting"
cd "$PROJECT_ROOT"

# Global counters
TOTAL_ISSUES=0
CRITICAL_ISSUES=0
WARNINGS=0
FILES_SCANNED=0

# Global arrays to store credential patterns from secrets.json
declare -A CREDENTIAL_PATTERNS
declare -A CREDENTIAL_TYPES
declare -A CREDENTIAL_VALUES

# Parse command line arguments
SCAN_ONLY=false
SETUP_ONLY=false
VERBOSE=false

show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Comprehensive security audit for Unicorn Investing Platform"
    echo ""
    echo "Options:"
    echo "  --scan-only     Only perform security scanning (no cleanup or setup)"
    echo "  --setup-only    Only run credential setup wizard"
    echo "  --verbose       Show detailed output"
    echo "  --help          Show this help message"
    echo ""
    echo "Default behavior: Run full audit (scan + recommendations)"
    echo ""
}

while [[ $# -gt 0 ]]; do
    case $1 in
        --scan-only)
            SCAN_ONLY=true
            shift
            ;;
        --setup-only)
            SETUP_ONLY=true
            shift
            ;;
        --verbose)
            VERBOSE=true
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

log_verbose() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${CYAN}[VERBOSE] $1${NC}"
    fi
}

increment_counter() {
    case $1 in
        "critical")
            CRITICAL_ISSUES=$((CRITICAL_ISSUES + 1))
            TOTAL_ISSUES=$((TOTAL_ISSUES + 1))
            ;;
        "warning")
            WARNINGS=$((WARNINGS + 1))
            TOTAL_ISSUES=$((TOTAL_ISSUES + 1))
            ;;
    esac
}

# Function to load credential patterns from secrets.json
load_credential_patterns() {
    echo "📋 Loading credential patterns from secrets.json..."
    
    if [ ! -f "config/secrets.json" ]; then
        echo -e "${RED}❌ config/secrets.json not found! Cannot load credential patterns.${NC}"
        echo "   Run: python3 scripts/setup_credentials.py"
        return 1
    fi
    
    # Use Python to extract credentials from JSON and create search patterns
    python3 << 'EOF'
import json
import sys
import os

try:
    with open('config/secrets.json', 'r') as f:
        config = json.load(f)
    
    patterns_found = 0
    
    # Extract API keys
    if 'api_keys' in config:
        for key, value in config['api_keys'].items():
            if value and value != f"YOUR_{key.upper()}" and not value.startswith("YOUR_"):
                print(f"CREDENTIAL_PATTERN_{key.upper()}='{value}'")
                print(f"CREDENTIAL_TYPE_{key.upper()}='API Key'")
                patterns_found += 1
    
    # Extract database credentials
    if 'database' in config and 'mysql' in config['database']:
        for env, db_config in config['database']['mysql'].items():
            if 'password' in db_config and db_config['password']:
                pattern_key = f"DB_PASSWORD_{env.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{db_config['password']}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='Database Password'")
                patterns_found += 1
            if 'username' in db_config and db_config['username']:
                pattern_key = f"DB_USERNAME_{env.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{db_config['username']}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='Database Username'")
                patterns_found += 1
    
    # Extract IBKR credentials
    if 'ibkr' in config:
        for key, value in config['ibkr'].items():
            if value and str(value) != f"YOUR_{key.upper()}":
                pattern_key = f"IBKR_{key.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{value}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='IBKR {key.title()}'")
                patterns_found += 1
    
    # Extract application secrets
    if 'application' in config:
        for key, value in config['application'].items():
            if value and not value.startswith("your-") and len(str(value)) > 10:
                pattern_key = f"APP_{key.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{value}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='Application Secret'")
                patterns_found += 1
    
    print(f"PATTERNS_COUNT={patterns_found}")
    
except Exception as e:
    print(f"ERROR: Failed to load credentials: {e}", file=sys.stderr)
    sys.exit(1)
EOF
    
    # Capture the Python output and evaluate it
    local python_output
    python_output=$(python3 << 'EOF'
import json
import sys

try:
    with open('config/secrets.json', 'r') as f:
        config = json.load(f)
    
    patterns_found = 0
    
    # Extract API keys
    if 'api_keys' in config:
        for key, value in config['api_keys'].items():
            if value and value != f"YOUR_{key.upper()}" and not value.startswith("YOUR_"):
                print(f"CREDENTIAL_PATTERN_{key.upper()}='{value}'")
                print(f"CREDENTIAL_TYPE_{key.upper()}='API Key'")
                patterns_found += 1
    
    # Extract database credentials
    if 'database' in config and 'mysql' in config['database']:
        for env, db_config in config['database']['mysql'].items():
            if 'password' in db_config and db_config['password']:
                pattern_key = f"DB_PASSWORD_{env.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{db_config['password']}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='Database Password'")
                patterns_found += 1
            if 'username' in db_config and db_config['username']:
                pattern_key = f"DB_USERNAME_{env.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{db_config['username']}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='Database Username'")
                patterns_found += 1
    
    # Extract IBKR credentials
    if 'ibkr' in config:
        for key, value in config['ibkr'].items():
            if value and str(value) != f"YOUR_{key.upper()}":
                pattern_key = f"IBKR_{key.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{value}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='IBKR {key.title()}'")
                patterns_found += 1
    
    # Extract application secrets
    if 'application' in config:
        for key, value in config['application'].items():
            if value and not value.startswith("your-") and len(str(value)) > 10:
                pattern_key = f"APP_{key.upper()}"
                print(f"CREDENTIAL_PATTERN_{pattern_key}='{value}'")
                print(f"CREDENTIAL_TYPE_{pattern_key}='Application Secret'")
                patterns_found += 1
    
    print(f"PATTERNS_COUNT={patterns_found}")
    
except Exception as e:
    print(f"ERROR: Failed to load credentials: {e}", file=sys.stderr)
    sys.exit(1)
EOF
)
    
    if [ $? -ne 0 ]; then
        echo -e "${RED}❌ Failed to load credentials from secrets.json${NC}"
        return 1
    fi
    
    # Evaluate the Python output to set environment variables
    eval "$python_output"
    
    if [ -z "$PATTERNS_COUNT" ] || [ "$PATTERNS_COUNT" -eq 0 ]; then
        echo -e "${YELLOW}⚠️  No credential patterns loaded from secrets.json${NC}"
        echo "   This may indicate secrets.json contains only template values"
        increment_counter "warning"
        return 1
    else
        echo -e "${GREEN}✅ Loaded $PATTERNS_COUNT credential patterns from secrets.json${NC}"
        log_verbose "Credential patterns loaded successfully"
        return 0
    fi
}

# Function to scan for specific credential in files
scan_for_credential() {
    local credential_var="$1"
    local credential_type="$2"
    local credential_value="$3"
    local found_issues=0
    
    log_verbose "Scanning for $credential_type: $credential_var"
    
    # Skip very short values that might cause false positives
    if [ ${#credential_value} -lt 6 ]; then
        log_verbose "Skipping short credential value: $credential_value"
        return 0
    fi
    
    # Search for the credential value in files
    while IFS= read -r -d '' file; do
        if should_scan "$file"; then
            # Use grep to find exact matches, excluding the secrets.json file itself
            if [[ "$file" != *"config/secrets.json"* ]] && grep -l -F "$credential_value" "$file" >/dev/null 2>&1; then
                echo -e "${RED}❌ $credential_type exposed in:${NC} $file"
                if [ "$VERBOSE" = true ]; then
                    echo -e "${CYAN}   Value: ${credential_value:0:10}...${NC}"
                    grep -n -F "$credential_value" "$file" | head -2 | sed 's/^/   /'
                fi
                found_issues=$((found_issues + 1))
                increment_counter "critical"
            fi
        fi
    done < <(find . -type f \( -name "*.py" -o -name "*.js" -o -name "*.json" -o -name "*.yml" -o -name "*.yaml" -o -name "*.sh" -o -name "*.md" -o -name "*.txt" \) -print0)
    
    return $found_issues
}
# Function to check if a file should be scanned
should_scan() {
    local file=$1
    
    # Skip vendor directories, node_modules, .git, etc.
    if [[ $file == *"/vendor/"* ]] || [[ $file == *"/node_modules/"* ]] || [[ $file == *"/.git/"* ]] || [[ $file == *"/.venv/"* ]]; then
        return 1
    fi
    
    # Skip binary files
    if [[ $file == *".db"* ]] || [[ $file == *".sqlite"* ]] || [[ $file == *".gz"* ]] || [[ $file == *".zip"* ]] || [[ $file == *".pyc"* ]]; then
        return 1
    fi
    
    # Skip the security audit scripts themselves to avoid false positives
    if [[ $file == *"security_audit"* ]] || [[ $file == *"security_cleanup"* ]] || [[ $file == *"migration_helper"* ]]; then
        return 1
    fi
    
    # Skip the secrets.json file itself (we don't want to flag it for containing secrets)
    if [[ $file == *"config/secrets.json" ]]; then
        return 1
    fi
    
    FILES_SCANNED=$((FILES_SCANNED + 1))
    return 0
}

# Core security scanning function - now uses dynamic patterns from secrets.json
perform_security_scan() {
    echo "🔍 PERFORMING COMPREHENSIVE SECURITY SCAN..."
    echo "============================================="
    echo "📂 Scanning project directory: $PROJECT_ROOT"
    echo
    
    # Load credential patterns from secrets.json
    echo "🔑 LOADING AND SCANNING FOR CREDENTIAL EXPOSURE..."
    echo "--------------------------------------------------"
    
    if ! load_credential_patterns; then
        echo -e "${RED}❌ Cannot perform comprehensive scan without credential patterns${NC}"
        echo "   Please ensure config/secrets.json is properly configured"
        increment_counter "critical"
        return 1
    fi
    
    local credentials_found=false
    local scan_count=0
    
    # Scan for each loaded credential pattern
    for var in $(env | grep '^CREDENTIAL_PATTERN_' | cut -d= -f1); do
        local credential_value="${!var}"
        local type_var="${var/PATTERN/TYPE}"
        local credential_type="${!type_var}"
        
        if [ -n "$credential_value" ] && [ -n "$credential_type" ]; then
            log_verbose "Scanning for $credential_type exposure..."
            
            if scan_for_credential "$var" "$credential_type" "$credential_value"; then
                credentials_found=true
                scan_count=$((scan_count + $?))
            fi
        fi
    done
    
    if [ "$credentials_found" = false ]; then
        echo -e "${GREEN}✅ No credential exposure found in codebase${NC}"
    else
        echo -e "${RED}❌ Found $scan_count instances of credential exposure${NC}"
    fi
    
    echo
    
    # Additional pattern-based scanning for common security issues
    echo "� SCANNING FOR COMMON SECURITY PATTERNS..."
    echo "--------------------------------------------"
    
    local pattern_issues=0
    
    # Look for potential credential patterns that might not be in secrets.json
    while IFS= read -r -d '' file; do
        if should_scan "$file"; then
            log_verbose "Pattern scanning $file"
            
            # Look for potential API key patterns
            if grep -l "api[_-]key.*=.*['\"][A-Za-z0-9\-_]{20,}['\"]" "$file" >/dev/null 2>&1; then
                echo -e "${YELLOW}⚠️  Potential API key pattern found in:${NC} $file"
                if [ "$VERBOSE" = true ]; then
                    grep -n "api[_-]key.*=.*['\"][A-Za-z0-9\-_]{20,}['\"]" "$file" | head -2 | sed 's/^/   /'
                fi
                increment_counter "warning"
                pattern_issues=$((pattern_issues + 1))
            fi
            
            # Look for potential password patterns
            if grep -l "password.*=.*['\"][^'\"]{8,}['\"]" "$file" >/dev/null 2>&1; then
                echo -e "${YELLOW}⚠️  Potential password pattern found in:${NC} $file"
                if [ "$VERBOSE" = true ]; then
                    grep -n "password.*=.*['\"][^'\"]{8,}['\"]" "$file" | head -2 | sed 's/^/   /' | sed 's/password.*=.*['\''"][^'\''"]*/password=***HIDDEN***/g'
                fi
                increment_counter "warning"
                pattern_issues=$((pattern_issues + 1))
            fi
            
            # Look for potential secret/token patterns
            if grep -l "\(secret\|token\).*=.*['\"][A-Za-z0-9\-_]{16,}['\"]" "$file" >/dev/null 2>&1; then
                echo -e "${YELLOW}⚠️  Potential secret/token pattern found in:${NC} $file"
                if [ "$VERBOSE" = true ]; then
                    grep -n "\(secret\|token\).*=.*['\"][A-Za-z0-9\-_]{16,}['\"]" "$file" | head -2 | sed 's/^/   /' | sed 's/\(secret\|token\).*=.*['\''"][^'\''"]*/\1=***HIDDEN***/g'
                fi
                increment_counter "warning"
                pattern_issues=$((pattern_issues + 1))
            fi
        fi
    done < <(find . -type f \( -name "*.py" -o -name "*.js" -o -name "*.json" -o -name "*.yml" -o -name "*.yaml" -o -name "*.sh" \) -print0)
    
    if [ $pattern_issues -eq 0 ]; then
        echo -e "${GREEN}✅ No suspicious credential patterns found${NC}"
    else
        echo -e "${YELLOW}⚠️  Found $pattern_issues potential credential patterns to review${NC}"
    fi
    
    echo
}

# Configuration and environment check
check_security_configuration() {
    echo "⚙️  CHECKING SECURITY CONFIGURATION..."
    echo "-------------------------------------"

    if [ -f "config/secrets.json" ]; then
        echo -e "${GREEN}✅ config/secrets.json exists${NC}"
        
        # Check if it still contains template values (avoid exposing actual values)
        if grep -q "YOUR_" config/secrets.json; then
            echo -e "${YELLOW}⚠️  config/secrets.json contains template values (YOUR_*)${NC}"
            echo "   Please replace all YOUR_* placeholders with actual values"
            increment_counter "warning"
        else
            echo -e "${GREEN}✅ config/secrets.json appears to be configured${NC}"
        fi
    else
        echo -e "${RED}❌ config/secrets.json not found${NC}"
        echo "   Run: python3 scripts/setup_credentials.py"
        increment_counter "critical"
    fi

    if [ -f "scripts/secrets_manager.py" ]; then
        echo -e "${GREEN}✅ scripts/secrets_manager.py exists${NC}"
    else
        echo -e "${RED}❌ scripts/secrets_manager.py missing${NC}"
        increment_counter "critical"
    fi

    # Check .gitignore protection
    echo "🚫 Checking .gitignore protection..."
    if grep -q "config/secrets.json" .gitignore; then
        echo -e "${GREEN}✅ secrets.json properly ignored by git${NC}"
    else
        echo -e "${RED}❌ config/secrets.json not in .gitignore${NC}"
        echo "   Add 'config/secrets.json' to .gitignore"
        increment_counter "critical"
    fi

    if grep -q "\*\*/.env" .gitignore; then
        echo -e "${GREEN}✅ .env files properly ignored${NC}"
    else
        echo -e "${YELLOW}⚠️  .env files not in .gitignore${NC}"
        increment_counter "warning"
    fi
    echo
}

# Migration guidance - now provides dynamic examples based on loaded patterns
show_migration_guidance() {
    echo "🔧 MIGRATION GUIDANCE..."
    echo "========================"
    echo
    echo -e "${BLUE}📋 Security Migration Checklist:${NC}"
    echo "  1. Run credential setup: python3 scripts/setup_credentials.py"
    echo "  2. Update hardcoded credentials using SecretsManager"
    echo "  3. Test configuration: python3 scripts/secrets_manager.py"
    echo "  4. Clean up old credential files"
    echo "  5. Verify .gitignore protections"
    echo
    echo -e "${BLUE}🔧 Code Update Examples Based on Your Configuration:${NC}"
    echo
    
    # Show specific examples based on what's actually in secrets.json
    if [ "$PATTERNS_COUNT" -gt 0 ]; then
        echo "Based on your secrets.json configuration:"
        echo
        
        # API Keys examples
        if [ -n "$CREDENTIAL_PATTERN_FRED_API_KEY" ]; then
            echo "  🏦 FRED API Key Migration:"
            echo "  # Before:"
            echo "  fred_api_key = '${CREDENTIAL_PATTERN_FRED_API_KEY:0:10}...'"
            echo "  # After:"
            echo "  from scripts.secrets_manager import SecretsManager"
            echo "  secrets = SecretsManager()"
            echo "  fred_api_key = secrets.get_api_key('fred_api_key')"
            echo
        fi
        
        if [ -n "$CREDENTIAL_PATTERN_BEA_API_KEY" ]; then
            echo "  📊 BEA API Key Migration:"
            echo "  # Before:"
            echo "  bea_api_key = '${CREDENTIAL_PATTERN_BEA_API_KEY:0:10}...'"
            echo "  # After:"
            echo "  from scripts.secrets_manager import SecretsManager"
            echo "  secrets = SecretsManager()"
            echo "  bea_api_key = secrets.get_api_key('bea_api_key')"
            echo
        fi
        
        # Database examples
        if [ -n "$CREDENTIAL_PATTERN_DB_PASSWORD_DEVELOPMENT" ]; then
            echo "  🗄️  Database Connection Migration:"
            echo "  # Before:"
            echo "  connection = pymysql.connect(host='localhost', user='unicorn', password='***')"
            echo "  # After:"
            echo "  from scripts.secrets_manager import SecretsManager"
            echo "  secrets = SecretsManager()"
            echo "  db_config = secrets.get_database_config('development')"
            echo "  connection = pymysql.connect(**db_config)"
            echo
        fi
        
        # IBKR examples
        if [ -n "$CREDENTIAL_PATTERN_IBKR_ACCOUNT_ID" ]; then
            echo "  📈 IBKR Configuration Migration:"
            echo "  # Before:"
            echo "  account_id = '${CREDENTIAL_PATTERN_IBKR_ACCOUNT_ID}'"
            echo "  # After:"
            echo "  from scripts.secrets_manager import SecretsManager"
            echo "  secrets = SecretsManager()"
            echo "  ibkr_config = secrets.get_ibkr_config()"
            echo "  account_id = ibkr_config['account_id']"
            echo
        fi
        
    else
        # Fallback to generic examples
        echo "  Generic Migration Patterns:"
        echo
        echo "  Replace hardcoded API keys:"
        echo "  # Before:"
        echo "  api_key = 'your_actual_key_here'"
        echo "  # After:"
        echo "  from scripts.secrets_manager import SecretsManager"
        echo "  secrets = SecretsManager()"
        echo "  api_key = secrets.get_api_key('service_name_api_key')"
        echo
        echo "  Replace database connections:"
        echo "  # Before:"
        echo "  connection = pymysql.connect(host='localhost', password='your_password')"
        echo "  # After:"
        echo "  from scripts.secrets_manager import SecretsManager"
        echo "  secrets = SecretsManager()"
        echo "  db_config = secrets.get_database_config('development')"
        echo "  connection = pymysql.connect(**db_config)"
        echo
    fi
    
    echo -e "${BLUE}⚙️  Available SecretsManager Methods:${NC}"
    echo "  • secrets.get_api_key('key_name')           - Get API keys"
    echo "  • secrets.get_database_config('env')        - Get database config"
    echo "  • secrets.get_mysql_connection_string('env') - Get connection string"
    echo "  • secrets.get_ibkr_config()                 - Get IBKR configuration"
    echo "  • secrets.get_application_config()          - Get app secrets"
    echo
}

# Test security configuration
test_security_configuration() {
    echo "🧪 TESTING SECURITY CONFIGURATION..."
    echo "====================================="
    
    if [ -f "scripts/secrets_manager.py" ] && [ -f "config/secrets.json" ]; then
        echo "Running SecretsManager validation..."
        if python3 scripts/secrets_manager.py; then
            echo -e "${GREEN}✅ Security configuration test passed${NC}"
        else
            echo -e "${RED}❌ Security configuration test failed${NC}"
            increment_counter "critical"
        fi
    else
        echo -e "${YELLOW}⚠️  Cannot test configuration - missing required files${NC}"
        increment_counter "warning"
    fi
    echo
}

# Generate security report
generate_security_report() {
    echo "📊 SECURITY AUDIT SUMMARY"
    echo "=========================="
    echo
    echo -e "${CYAN}📈 Scan Statistics:${NC}"
    echo "  Files Scanned: $FILES_SCANNED"
    echo "  Total Issues: $TOTAL_ISSUES"
    echo "  Critical Issues: $CRITICAL_ISSUES"
    echo "  Warnings: $WARNINGS"
    echo
    
    if [ $CRITICAL_ISSUES -eq 0 ] && [ $WARNINGS -eq 0 ]; then
        echo -e "${GREEN}🎉 EXCELLENT! No security issues found.${NC}"
        echo -e "${GREEN}   Your security configuration is properly set up.${NC}"
    elif [ $CRITICAL_ISSUES -eq 0 ] && [ $WARNINGS -gt 0 ]; then
        echo -e "${YELLOW}⚠️  GOOD with minor issues. $WARNINGS warning(s) found.${NC}"
        echo -e "${YELLOW}   Consider addressing the warnings above.${NC}"
    elif [ $CRITICAL_ISSUES -gt 0 ]; then
        echo -e "${RED}❌ ATTENTION REQUIRED! $CRITICAL_ISSUES critical issue(s) found.${NC}"
        echo -e "${RED}   Please address critical security issues immediately.${NC}"
    fi
    
    echo
    echo -e "${BLUE}📋 Recommended Next Steps:${NC}"
    if [ $CRITICAL_ISSUES -gt 0 ]; then
        echo "  1. 🚨 Address critical security issues above"
        echo "  2. Run credential setup: python3 scripts/setup_credentials.py"
        echo "  3. Update hardcoded credentials to use SecretsManager"
        echo "  4. Re-run this audit to verify fixes"
    elif [ $WARNINGS -gt 0 ]; then
        echo "  1. Review and address warnings above"
        echo "  2. Run: python3 scripts/secrets_manager.py (to verify configuration)"
        echo "  3. Consider manually cleaning sensitive log files if needed"
    else
        echo "  1. ✅ Maintain current security practices"
        echo "  2. Periodically re-run security audits"
        echo "  3. Keep credentials secure and never commit them"
    fi
    echo
}

# Main execution logic
main() {
    if [ "$SETUP_ONLY" = true ]; then
        echo "🔐 Running credential setup only..."
        if [ -f "scripts/setup_credentials.py" ]; then
            python3 scripts/setup_credentials.py
        else
            echo -e "${RED}❌ setup_credentials.py not found${NC}"
            exit 1
        fi
        exit 0
    fi
    
    # Full audit or scan-only mode
    perform_security_scan
    check_security_configuration
    
    if [ "$SCAN_ONLY" = false ]; then
        test_security_configuration
        show_migration_guidance
    fi
    
    generate_security_report
    
    # Exit with appropriate code
    if [ $CRITICAL_ISSUES -gt 0 ]; then
        exit 1
    elif [ $WARNINGS -gt 0 ]; then
        exit 2
    else
        exit 0
    fi
}

# Execute main function
main "$@"
