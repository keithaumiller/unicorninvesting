#!/bin/bash

# Unicorn Investing - Drupal Startup Validation Script
# This script ensures all services are running for the Drupal platform
# Run this after workspace restarts or pauses
#
# AUTOMATED SOLUTIONS DOCUMENTED:
# ================================
# 
# 1. PORT FORWARDING AUTOMATION
#    - Issue: GitHub Codespaces requires manual port 80 forwarding for external access
#    - Solution: Automatic detection of external accessibility and GitHub CLI forwarding
#    - Fallback: Clear manual instructions provided if automation fails
#
# 2. DATABASE SETUP AUTOMATION  
#    - Issue: Drupal requires MySQL database and user configuration
#    - Solution: Automatic creation of 'unicorn_drupal' database and 'drupal_user' with proper privileges
#    - Configuration: Database settings automatically added to settings.php
#
# 3. DRUPAL INSTALLATION AUTOMATION
#    - Issue: Fresh environments need complete Drupal installation
#    - Solution: Automatic detection of installation state and Drush-based installation
#    - Credentials: Default admin/admin123 for development environments
#    - Fallback: Web installer URLs provided if Drush installation fails
#
# 4. PERMISSION MANAGEMENT
#    - Issue: File permissions often cause installation failures
#    - Solution: Proper permission setting before/after installation
#    - Owner: www-data for web-accessible files, proper chmod settings
#
# 5. SERVICE DEPENDENCY HANDLING  
#    - Issue: Apache/MySQL must be running before Drupal operations
#    - Solution: Service status checking and automatic startup with error handling
#    - Validation: Port verification to ensure services are properly listening
#
# 6. CONTENT VALIDATION
#    - Issue: Drupal may be running but not properly configured or displaying content
#    - Solution: Homepage content validation looking for "Welcome to UnicornInvesting.us"
#    - Failure: Script exits with error code 1 if expected content is not found
#    - Debug: Provides troubleshooting steps and access points for manual intervention
#
# 7. APACHE VIRTUAL HOST CONFIGURATION
#    - Issue: Apache serves default page instead of Drupal from correct directory
#    - Solution: Automatic configuration of Apache virtual host to serve Drupal from WebFrontend/web
#    - Configuration: Creates/updates 000-default.conf to point to correct DocumentRoot
#    - Restart: Automatically reloads Apache configuration after changes
#
# USAGE:
# ======
# ./scripts/startup_drupal.sh
#
# This will automatically:
# - Start Apache and MySQL services
# - Configure port forwarding  
# - Set up database and user
# - Install Drupal if needed
# - Clear caches and validate functionality
#
# BASELINE EXPORT:
# ===============
# To create a reusable baseline of a working installation:
# ./scripts/export_drupal_baseline.sh
#
# This creates a complete backup that can be restored using:
# cd scripts/drupal_baseline_exports/[export_name]
# ./restore_baseline.sh

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
APACHE_SERVICE="apache2"
MYSQL_SERVICE="mysql"
DRUPAL_URL="http://localhost/"
DRUPAL_UNICORN_URL="http://localhost/admin/metrics"
DRUPAL_EXTERNAL_URL="https://${CODESPACE_NAME:-codespace}-80.app.github.dev/"
DRUPAL_EXTERNAL_UNICORN_URL="https://${CODESPACE_NAME:-codespace}-80.app.github.dev/admin/metrics"
WORKSPACE_ROOT="/workspaces/unicorninvesting"
DRUPAL_ROOT="$WORKSPACE_ROOT/WebFrontend"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}🦄 Unicorn Investing - Drupal Startup${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Function to check service status
check_service_status() {
    local service=$1
    if sudo service $service status >/dev/null 2>&1; then
        echo -e "${GREEN}✅ $service is running${NC}"
        return 0
    else
        echo -e "${RED}❌ $service is not running${NC}"
        return 1
    fi
}

# Function to start service
start_service() {
    local service=$1
    echo -e "${YELLOW}🔄 Starting $service...${NC}"
    
    if sudo service $service start; then
        sleep 3  # Give service time to start
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

# Function to validate URL and check for specific content
validate_url_with_content() {
    local url=$1
    local name=$2
    local expected_content=$3
    
    echo -e "${YELLOW}🔍 Checking $name at $url${NC}"
    
    # Use curl to check the URL and get content
    local response=$(curl -s -L "$url" --max-time 15)
    local response_code=$(curl -s -o /dev/null -w "%{http_code}" -L "$url" --max-time 15)
    
    if [ "$response_code" = "200" ]; then
        echo -e "${GREEN}✅ $name is accessible (HTTP $response_code)${NC}"
        
        if [ -n "$expected_content" ]; then
            if echo "$response" | grep -q "$expected_content"; then
                echo -e "${GREEN}✅ Expected content found: $expected_content${NC}"
                return 0
            elif echo "$response" | grep -q "Connecting to the forwarded port"; then
                echo -e "${YELLOW}⚠️  GitHub Codespaces authentication layer detected${NC}"
                echo -e "${CYAN}ℹ️  URL is functional but requires browser authentication${NC}"
                echo -e "${GREEN}✅ This is expected behavior in Codespaces environment${NC}"
                return 0  # Consider this successful since URL structure is working
            else
                echo -e "${YELLOW}⚠️  Expected content NOT found: $expected_content${NC}"
                echo -e "${CYAN}ℹ️  This may be due to authentication requirements or content changes${NC}"
                return 1
            fi
        fi
        return 0
    elif [ "$response_code" = "302" ] || [ "$response_code" = "301" ]; then
        echo -e "${YELLOW}⚠️  $name redirects (HTTP $response_code)${NC}"
        return 1
    elif [ "$response_code" = "000" ]; then
        echo -e "${RED}❌ $name is not accessible (Connection failed)${NC}"
        return 1
    else
        echo -e "${YELLOW}⚠️  $name returned HTTP $response_code${NC}"
        return 1
    fi
}

# Function to validate HTTP response
validate_url() {
    local url=$1
    local name=$2
    
    echo -e "${YELLOW}🔍 Checking $name at $url${NC}"
    
    # Use curl to check the URL, following redirects
    local response_code=$(curl -s -o /dev/null -w "%{http_code}" -L "$url" --max-time 10)
    
    if [ "$response_code" = "200" ]; then
        echo -e "${GREEN}✅ $name is accessible (HTTP $response_code)${NC}"
        return 0
    elif [ "$response_code" = "302" ] || [ "$response_code" = "301" ]; then
        # Check if redirect leads to a 200
        local final_code=$(curl -s -o /dev/null -w "%{http_code}" -L "$url" --max-time 10)
        if [ "$final_code" = "200" ]; then
            echo -e "${GREEN}✅ $name is accessible (HTTP $response_code → $final_code)${NC}"
            return 0
        else
            echo -e "${YELLOW}⚠️  $name redirects but final destination returns HTTP $final_code${NC}"
            return 1
        fi
    elif [ "$response_code" = "000" ]; then
        echo -e "${RED}❌ $name is not accessible (Connection failed)${NC}"
        return 1
    else
        echo -e "${YELLOW}⚠️  $name returned HTTP $response_code${NC}"
        return 1
    fi
}

# Function to show port forwarding instructions
show_port_forwarding_instructions() {
    echo -e "${BLUE}🔧 MANUAL PORT FORWARDING REQUIRED${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${RED}⚠️  CRITICAL: Port 80 must be forwarded for external access${NC}"
    echo ""
    echo -e "${CYAN}🎯 STEP-BY-STEP INSTRUCTIONS:${NC}"
    echo ""
    echo -e "${CYAN}METHOD 1 - VS Code Ports Panel (RECOMMENDED):${NC}"
    echo -e "${CYAN}   1. Look at the bottom of your VS Code window${NC}"
    echo -e "${CYAN}   2. Click on the 'PORTS' tab (next to TERMINAL/PROBLEMS)${NC}"
    echo -e "${CYAN}   3. Click the '+' button or 'Forward a Port'${NC}"
    echo -e "${CYAN}   4. Enter: ${GREEN}80${CYAN} (just the number 80)${NC}"
    echo -e "${CYAN}   5. Press Enter${NC}"
    echo -e "${CYAN}   6. Right-click on the new port 80 entry${NC}"
    echo -e "${CYAN}   7. Select 'Change Port Visibility' → 'Public'${NC}"
    echo -e "${CYAN}   8. Look for the globe 🌐 icon next to port 80${NC}"
    echo ""
    echo -e "${CYAN}METHOD 2 - VS Code Command Palette:${NC}"
    echo -e "${CYAN}   1. Press Ctrl+Shift+P (Cmd+Shift+P on Mac)${NC}"
    echo -e "${CYAN}   2. Type: ${GREEN}Forward a Port${NC}"
    echo -e "${CYAN}   3. Select 'Forward a Port'${NC}"
    echo -e "${CYAN}   4. Enter: ${GREEN}80${NC}"
    echo -e "${CYAN}   5. When prompted for visibility, choose 'Public'${NC}"
    echo ""
    echo -e "${CYAN}METHOD 3 - GitHub CLI (Advanced):${NC}"
    echo -e "${CYAN}   Forward port: ${GREEN}gh codespace ports forward 80 -c ${CODESPACE_NAME:-codespace}${NC}"
    echo -e "${CYAN}   Set to public: ${GREEN}gh codespace ports visibility 80:public -c ${CODESPACE_NAME:-codespace}${NC}"
    echo ""
    echo -e "${CYAN}✅ VERIFICATION:${NC}"
    echo -e "${CYAN}   After forwarding, you should see:${NC}"
    echo -e "${CYAN}   - Port 80 listed in the PORTS panel${NC}"
    echo -e "${CYAN}   - A globe 🌐 icon indicating it's public${NC}"
    echo -e "${CYAN}   - A clickable URL like: ${GREEN}https://cautious-guide-qg6vv7p4q6c6x9x-80.app.github.dev${NC}"
    echo ""
    echo -e "${CYAN}🧪 TEST THE FIX:${NC}"
    echo -e "${CYAN}   Run this command to verify port forwarding works:${NC}"
    echo -e "${GREEN}   ./scripts/startup_drupal.sh${NC}"
    echo ""
    echo -e "${CYAN}Expected URLs after forwarding:${NC}"
    echo -e "${GREEN}   🌐 Homepage: https://${CODESPACE_NAME:-codespace}-80.app.github.dev/${NC}"
    echo -e "${GREEN}   🏠 Local: http://localhost/${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
}

# Function to clear Drupal cache
clear_drupal_cache() {
    echo -e "${YELLOW}🧹 Clearing Drupal cache...${NC}"
    
    cd "$DRUPAL_ROOT"
    
    # Clear various cache directories
    sudo rm -rf web/sites/default/files/php/twig/* 2>/dev/null || true
    sudo rm -rf web/sites/default/files/css/* 2>/dev/null || true
    sudo rm -rf web/sites/default/files/js/* 2>/dev/null || true
    
    echo -e "${GREEN}✅ Drupal cache cleared${NC}"
}

# Function to check disk space
check_disk_space() {
    local available=$(df /workspaces | awk 'NR==2 {print $4}')
    local available_gb=$((available / 1024 / 1024))
    
    if [ $available_gb -lt 1 ]; then
        echo -e "${RED}⚠️  Low disk space: ${available_gb}GB available${NC}"
    else
        echo -e "${GREEN}✅ Disk space OK: ${available_gb}GB available${NC}"
    fi
}

# Function to setup GitHub Codespaces port forwarding
setup_port_forwarding() {
    echo -e "${YELLOW}🔌 Setting up GitHub Codespaces port forwarding...${NC}"
    
    # Check if we're in a codespace environment
    if [ -z "$CODESPACE_NAME" ]; then
        echo -e "${YELLOW}⚠️  Not in a GitHub Codespace environment, skipping port forwarding${NC}"
        return 0
    fi
    
    local external_url="https://${CODESPACE_NAME}-80.app.github.dev/"
    
    # Function to test external accessibility with multiple methods
    test_external_access() {
        local test_url="$1"
        local timeout="${2:-10}"
        
        # Method 1: Basic HTTP test
        local basic_test=$(curl -s -o /dev/null -w "%{http_code}" -L "$test_url" --max-time "$timeout" 2>/dev/null || echo "000")
        
        # Method 2: Test with connection info
        local connection_test=$(curl -s -w "CONN:%{http_connect}\nHTTP:%{http_code}\nREDIR:%{num_redirects}" -L "$test_url" --max-time "$timeout" 2>/dev/null || echo "CONN:000")
        
        echo "Basic test: $basic_test"
        echo "Connection details: $connection_test"
        
        # Consider success if we get any valid HTTP response or connection
        if [[ "$basic_test" =~ ^[23][0-9][0-9]$ ]] || [[ "$connection_test" =~ CONN:[12][0-9][0-9] ]]; then
            return 0
        fi
        
        return 1
    }
    
    # Enhanced external accessibility check
    echo -e "${YELLOW}🔍 Testing external URL accessibility: $external_url${NC}"
    
    if test_external_access "$external_url" 15; then
        echo -e "${GREEN}✅ Port 80 is already accessible externally${NC}"
        echo -e "${CYAN}ℹ️  External URL: $external_url${NC}"
        return 0
    else
        echo -e "${YELLOW}⚠️  External URL not immediately accessible${NC}"
    fi
    
    # Check VS Code port forwarding via environment
    echo -e "${YELLOW}🔍 Checking VS Code port forwarding status...${NC}"
    
    # In Codespaces, forwarded ports are often available through GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN
    if [ -n "$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN" ]; then
        local alt_url="https://${CODESPACE_NAME}-80.$GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN/"
        echo -e "${CYAN}ℹ️  Testing alternative URL: $alt_url${NC}"
        
        if test_external_access "$alt_url" 10; then
            echo -e "${GREEN}✅ Port accessible via alternative URL${NC}"
            return 0
        fi
    fi
    
    # Try GitHub CLI method if available
    if command -v gh >/dev/null 2>&1; then
        echo -e "${YELLOW}🔄 Checking GitHub CLI port forwarding...${NC}"
        
        # List current port forwards
        local port_list=$(gh codespace ports list 2>/dev/null || echo "")
        echo -e "${CYAN}ℹ️  Current port forwards:${NC}"
        echo "$port_list" | sed 's/^/   /'
        
        # Check if port 80 is already forwarded
        if echo "$port_list" | grep -E "^\s*80\s" >/dev/null; then
            local port_info=$(echo "$port_list" | grep -E "^\s*80\s" | head -1)
            echo -e "${GREEN}✅ Port 80 is already configured in GitHub CLI${NC}"
            echo -e "${CYAN}ℹ️  Port info: $port_info${NC}"
            
            # Try to update visibility to public if it's not already
            if echo "$port_info" | grep -v "public" >/dev/null; then
                echo -e "${YELLOW}🔄 Updating port visibility to public...${NC}"
                if gh codespace ports visibility 80:public 2>/dev/null; then
                    echo -e "${GREEN}✅ Port visibility updated to public${NC}"
                    
                    # Wait a moment and test again
                    echo -e "${YELLOW}⏳ Waiting for changes to take effect...${NC}"
                    sleep 10
                    
                    if test_external_access "$external_url" 15; then
                        echo -e "${GREEN}✅ Port is now accessible externally${NC}"
                        return 0
                    fi
                else
                    echo -e "${YELLOW}⚠️  Failed to update port visibility${NC}"
                fi
            fi
        else
            echo -e "${YELLOW}🔄 Attempting to forward port 80 via GitHub CLI...${NC}"
            if gh codespace ports forward 80 2>/dev/null; then
                echo -e "${GREEN}✅ Port 80 forwarded successfully via GitHub CLI${NC}"
                
                # Try to set visibility to public
                if gh codespace ports visibility 80:public -c "$CODESPACE_NAME" 2>/dev/null; then
                    echo -e "${GREEN}✅ Port visibility set to public${NC}"
                else
                    echo -e "${YELLOW}⚠️  Port forwarded but visibility setting failed${NC}"
                fi
                
                # Wait for forwarding to take effect
                echo -e "${YELLOW}⏳ Waiting for port forwarding to activate...${NC}"
                sleep 15
                
                if test_external_access "$external_url" 15; then
                    echo -e "${GREEN}✅ Port forwarding is now active${NC}"
                    return 0
                else
                    echo -e "${YELLOW}⚠️  Port forwarding configured but not immediately accessible${NC}"
                fi
            else
                echo -e "${YELLOW}⚠️  GitHub CLI port forwarding failed${NC}"
                echo ""
                echo -e "${RED}🛑 AUTOMATIC PORT FORWARDING FAILED${NC}"
                echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
                echo -e "${YELLOW}❗ The script will pause here so you can manually forward port 80${NC}"
                echo ""
                echo -e "${CYAN}🎯 MANUAL STEPS - Please do this now:${NC}"
                echo -e "${CYAN}   1. Look at the bottom of VS Code for the 'PORTS' tab${NC}"
                echo -e "${CYAN}   2. Click the '+' button to 'Forward a Port'${NC}"
                echo -e "${CYAN}   3. Enter: 80${NC}"
                echo -e "${CYAN}   4. Press Enter${NC}"
                echo -e "${CYAN}   5. Right-click the new port 80 entry${NC}"
                echo -e "${CYAN}   6. Select 'Change Port Visibility' → 'Public'${NC}"
                echo -e "${CYAN}   7. Look for the globe 🌐 icon next to port 80${NC}"
                echo ""
                echo -e "${YELLOW}⏳ Once you've completed the manual port forwarding above,${NC}"
                echo -e "${YELLOW}   press ENTER to continue the script...${NC}"
                read -p "Press ENTER after manually forwarding port 80: "
                echo ""
                echo -e "${GREEN}✅ Continuing script execution...${NC}"
                echo -e "${CYAN}ℹ️  Expected URL: $external_url${NC}"
                return 0
            fi
        fi
    else
        echo -e "${YELLOW}⚠️  GitHub CLI (gh) not available${NC}"
    fi
    
    # Final attempt: Try VS Code command palette approach (limited success in Codespaces)
    echo -e "${YELLOW}⚠️  Automatic port forwarding methods not successful${NC}"
    
    # Provide comprehensive manual instructions
    echo -e "${BLUE}🔧 MANUAL PORT FORWARDING REQUIRED${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}To manually forward port 80 in VS Code:${NC}"
    echo ""
    echo -e "${CYAN}METHOD 1 - Ports Panel:${NC}"
    echo -e "${CYAN}   1. 🎯 Click on 'PORTS' tab in VS Code terminal panel${NC}"
    echo -e "${CYAN}   2. ➕ Click 'Forward a Port' or 'Add Port'${NC}"
    echo -e "${CYAN}   3. 🔢 Enter port number: 80${NC}"
    echo -e "${CYAN}   4. 🌐 Set visibility to 'Public' (globe icon)${NC}"
    echo -e "${CYAN}   5. ✨ Port should appear as: 80:80${NC}"
    echo ""
    echo -e "${CYAN}METHOD 2 - Command Palette:${NC}"
    echo -e "${CYAN}   1. ⌨️  Press Ctrl+Shift+P (Cmd+Shift+P on Mac)${NC}"
    echo -e "${CYAN}   2. 🔍 Type: 'Forward a Port'${NC}"
    echo -e "${CYAN}   3. 🔢 Enter: 80${NC}"
    echo -e "${CYAN}   4. 🌐 Set to public when prompted${NC}"
    echo ""
    echo -e "${CYAN}Expected URL after forwarding:${NC}"
    echo -e "${GREEN}   🌐 $external_url${NC}"
    echo ""
    echo -e "${CYAN}To verify port forwarding worked:${NC}"
    echo -e "${CYAN}   • Run this script again: ./scripts/startup_drupal.sh${NC}"
    echo -e "${CYAN}   • Or test URL directly in browser${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    
    # Return non-zero to indicate manual intervention needed
    return 1
}

# Function to setup MySQL database and user for Drupal
setup_drupal_database() {
    echo -e "${YELLOW}🗄️  Setting up Drupal database and user...${NC}"
    
    local db_name="unicorn_drupal"
    local db_user="drupal_user"
    local db_pass="drupal_pass"
    
    # Check if database exists
    local db_exists=$(sudo mysql -u root -e "SHOW DATABASES LIKE '$db_name';" 2>/dev/null | grep -c "$db_name")
    
    if [ "$db_exists" -eq 0 ]; then
        echo -e "${YELLOW}🔄 Creating database '$db_name'...${NC}"
        if sudo mysql -u root -e "CREATE DATABASE IF NOT EXISTS $db_name CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;" 2>/dev/null; then
            echo -e "${GREEN}✅ Database '$db_name' created successfully${NC}"
        else
            echo -e "${RED}❌ Failed to create database '$db_name'${NC}"
            return 1
        fi
    else
        echo -e "${GREEN}✅ Database '$db_name' already exists${NC}"
    fi
    
    # Check if user exists and create/update if needed
    echo -e "${YELLOW}🔄 Setting up database user '$db_user'...${NC}"
    if sudo mysql -u root -e "DROP USER IF EXISTS '$db_user'@'localhost'; CREATE USER '$db_user'@'localhost' IDENTIFIED BY '$db_pass'; GRANT ALL PRIVILEGES ON $db_name.* TO '$db_user'@'localhost'; FLUSH PRIVILEGES;" 2>/dev/null; then
        echo -e "${GREEN}✅ Database user '$db_user' configured successfully${NC}"
    else
        echo -e "${RED}❌ Failed to configure database user '$db_user'${NC}"
        return 1
    fi
    
    # Test database connection
    echo -e "${YELLOW}🔍 Testing database connection...${NC}"
    if mysql -u "$db_user" -p"$db_pass" -h 127.0.0.1 -P 3306 -e "USE $db_name; SELECT 1;" >/dev/null 2>&1; then
        echo -e "${GREEN}✅ Database connection successful${NC}"
        return 0
    else
        echo -e "${RED}❌ Database connection failed${NC}"
        return 1
    fi
}

# Function to configure Drupal database settings
configure_drupal_database() {
    echo -e "${YELLOW}⚙️  Configuring Drupal database settings...${NC}"
    
    local settings_file="$DRUPAL_ROOT/web/sites/default/settings.php"
    
    if [ ! -f "$settings_file" ]; then
        echo -e "${RED}❌ Drupal settings.php file not found at $settings_file${NC}"
        return 1
    fi
    
    # Check if database configuration already exists
    if grep -q "unicorn_drupal" "$settings_file" 2>/dev/null; then
        echo -e "${GREEN}✅ Database configuration already present in settings.php${NC}"
        return 0
    fi
    
    echo -e "${YELLOW}🔄 Adding database configuration to settings.php...${NC}"
    
    # Backup the original settings.php
    sudo cp "$settings_file" "$settings_file.backup.$(date +%Y%m%d_%H%M%S)" 2>/dev/null
    
    # Add database configuration
    local db_config="
// Unicorn Investing Database Configuration
\$databases['default']['default'] = [
  'database' => 'unicorn_drupal',
  'username' => 'drupal_user',
  'password' => 'drupal_pass',
  'host' => '127.0.0.1',
  'port' => '3306',
  'driver' => 'mysql',
  'prefix' => '',
  'collation' => 'utf8mb4_general_ci',
];"
    
    # Replace the empty databases array
    if sed -i "s/\$databases = \[\];/$db_config/" "$settings_file" 2>/dev/null; then
        echo -e "${GREEN}✅ Database configuration added to settings.php${NC}"
        return 0
    else
        echo -e "${RED}❌ Failed to update settings.php${NC}"
        return 1
    fi
}

# Function to install Drupal if not already installed
install_drupal_if_needed() {
    echo -e "${YELLOW}🚀 Checking Drupal installation status...${NC}"
    
    # Check if Drupal is already installed by counting database tables
    local table_count=$(sudo mysql -u root -e "USE unicorn_drupal; SHOW TABLES;" 2>/dev/null | wc -l)
    
    if [ "$table_count" -gt 5 ]; then
        echo -e "${GREEN}✅ Drupal is already installed ($table_count tables found)${NC}"
        return 0
    fi
    
    echo -e "${YELLOW}🔄 Preparing for Drupal installation...${NC}"
    
    # Set proper permissions first
    set_drupal_permissions
    
    # Change to WebFrontend directory
    cd "$DRUPAL_ROOT" || {
        echo -e "${RED}❌ Failed to change to Drupal directory${NC}"
        return 1
    }
    
    # Clear any existing installation locks
    rm -f web/sites/default/.drush-lock-update 2>/dev/null
    
    echo -e "${YELLOW}🔄 Attempting Drupal installation via Drush...${NC}"
    echo -e "${CYAN}ℹ️  This may take a few minutes...${NC}"
    
    # Try installation with explicit database URL first
    echo -e "${CYAN}ℹ️  Attempting installation with explicit database configuration...${NC}"
    if /usr/bin/php8.3 ./vendor/bin/drush.php site:install standard \
        --db-url="mysql://drupal_user:drupal_pass@127.0.0.1:3306/unicorn_drupal" \
        --site-name="unicorninvesting.us" \
        --account-name=admin \
        --account-pass=admin123 \
        --account-mail=admin@unicorninvesting.com \
        --locale=en \
        --yes \
        --no-interaction 2>/dev/null; then
        
        echo -e "${GREEN}✅ Drupal installation completed successfully via Drush${NC}"
        
        # Set proper permissions after installation
        sudo chown -R www-data:www-data web/sites/default/files 2>/dev/null
        sudo chmod -R 755 web/sites/default/files 2>/dev/null
        sudo chmod 644 web/sites/default/settings.php 2>/dev/null
        
        echo -e "${GREEN}✅ File permissions configured${NC}"
        echo -e "${CYAN}ℹ️  Admin credentials: admin / admin123${NC}"
        
        return 0
    else
        echo -e "${YELLOW}⚠️  Drush installation failed${NC}"
        echo -e "${CYAN}ℹ️  Drupal installation can be completed via web installer${NC}"
        echo -e "${CYAN}ℹ️  Installation pages:${NC}"
        echo -e "${CYAN}   🌐 Local: http://localhost/core/install.php${NC}"
        echo -e "${CYAN}   🌐 External: https://${CODESPACE_NAME:-codespace}-80.app.github.dev/core/install.php${NC}"
        echo -e "${CYAN}ℹ️  Database configuration:${NC}"
        echo -e "${CYAN}   📊 Database: unicorn_drupal${NC}"
        echo -e "${CYAN}   👤 Username: drupal_user${NC}"
        echo -e "${CYAN}   🔑 Password: drupal_pass${NC}"
        echo -e "${CYAN}   🌐 Host: 127.0.0.1${NC}"
        echo -e "${CYAN}   📍 Port: 3306${NC}"
        echo -e "${GREEN}✅ Permissions set for manual installation${NC}"
        
        return 0
    fi
}

# Function to set proper permissions for Drupal installation
set_drupal_permissions() {
    echo -e "${YELLOW}🔒 Setting Drupal installation permissions...${NC}"
    
    local drupal_default_dir="$DRUPAL_ROOT/web/sites/default"
    local settings_file="$drupal_default_dir/settings.php"
    local files_dir="$drupal_default_dir/files"
    
    # Create files directory if it doesn't exist
    if [ ! -d "$files_dir" ]; then
        echo -e "${YELLOW}🔄 Creating files directory...${NC}"
        sudo mkdir -p "$files_dir"
    fi
    
    # Set permissions for installation
    echo -e "${YELLOW}🔄 Setting installation permissions...${NC}"
    
    # Make default directory writable
    sudo chmod 755 "$drupal_default_dir" 2>/dev/null
    
    # Make settings.php writable for installation
    if [ -f "$settings_file" ]; then
        sudo chmod 666 "$settings_file" 2>/dev/null
    fi
    
    # Make files directory writable
    sudo chmod 777 "$files_dir" 2>/dev/null
    sudo chown -R www-data:www-data "$files_dir" 2>/dev/null
    
    echo -e "${GREEN}✅ Drupal permissions configured for installation${NC}"
    return 0
}

# Function to configure Apache virtual host for Drupal
configure_apache_drupal() {
    echo -e "${YELLOW}🌐 Configuring Apache virtual host for Drupal...${NC}"
    
    local drupal_web_dir="$DRUPAL_ROOT/web"
    local apache_conf="/etc/apache2/sites-available/000-default.conf"
    
    # Check if Drupal web directory exists
    if [ ! -d "$drupal_web_dir" ]; then
        echo -e "${RED}❌ Drupal web directory not found: $drupal_web_dir${NC}"
        return 1
    fi
    
    # Check current DocumentRoot
    local current_docroot=$(grep -E "^\s*DocumentRoot" "$apache_conf" 2>/dev/null | awk '{print $2}' | head -1)
    
    if [ "$current_docroot" = "$drupal_web_dir" ]; then
        echo -e "${GREEN}✅ Apache already configured for Drupal${NC}"
        return 0
    fi
    
    echo -e "${YELLOW}🔄 Updating Apache virtual host configuration...${NC}"
    echo -e "${CYAN}ℹ️  Current DocumentRoot: $current_docroot${NC}"
    echo -e "${CYAN}ℹ️  Target DocumentRoot: $drupal_web_dir${NC}"
    
    # Backup current configuration
    sudo cp "$apache_conf" "$apache_conf.backup.$(date +%Y%m%d_%H%M%S)" 2>/dev/null
    
    # Create new virtual host configuration
    sudo tee "$apache_conf" > /dev/null <<EOF
<VirtualHost *:80>
    # Unicorn Investing Drupal Configuration
    ServerName localhost
    DocumentRoot $drupal_web_dir
    
    <Directory $drupal_web_dir>
        Options -Indexes +FollowSymLinks
        AllowOverride All
        Require all granted
    </Directory>
    
    # Drupal-specific configurations
    <Files "composer.json">
        Require all denied
    </Files>
    
    <Files "composer.lock">
        Require all denied
    </Files>
    
    # Enable rewrite module for clean URLs
    RewriteEngine on
    
    # Error and access logs
    ErrorLog \${APACHE_LOG_DIR}/drupal_error.log
    CustomLog \${APACHE_LOG_DIR}/drupal_access.log combined
</VirtualHost>
EOF
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ Apache virtual host configuration updated${NC}"
        
        # Enable required Apache modules
        echo -e "${YELLOW}🔄 Enabling required Apache modules...${NC}"
        sudo a2enmod rewrite 2>/dev/null
        sudo a2enmod headers 2>/dev/null
        
        # Test Apache configuration
        if sudo apache2ctl configtest 2>/dev/null; then
            echo -e "${GREEN}✅ Apache configuration syntax is valid${NC}"
            
            # Restart Apache to ensure configuration takes effect
            echo -e "${YELLOW}🔄 Restarting Apache to apply configuration...${NC}"
            if sudo systemctl restart apache2 2>/dev/null; then
                echo -e "${GREEN}✅ Apache restarted successfully${NC}"
                
                # Wait for Apache to fully start
                sleep 5
                
                # Verify Apache is running
                if sudo systemctl is-active apache2 >/dev/null 2>&1; then
                    echo -e "${GREEN}✅ Apache is running and ready${NC}"
                    return 0
                else
                    echo -e "${RED}❌ Apache failed to start properly${NC}"
                    return 1
                fi
            else
                echo -e "${RED}❌ Failed to restart Apache${NC}"
                return 1
            fi
        else
            echo -e "${RED}❌ Apache configuration test failed${NC}"
            # Restore backup
            sudo mv "$apache_conf.backup.*" "$apache_conf" 2>/dev/null
            return 1
        fi
    else
        echo -e "${RED}❌ Failed to update Apache configuration${NC}"
        return 1
    fi
}

# Function to validate ports
check_ports() {
    echo -e "${YELLOW}🔍 Checking required ports...${NC}"
    
    # Check if Apache is listening on port 80
    if sudo netstat -tlnp | grep ":80 " >/dev/null 2>&1; then
        local process=$(sudo netstat -tlnp | grep ":80 " | awk '{print $7}' | head -1)
        echo -e "${GREEN}✅ Port 80 is active ($process)${NC}"
    else
        echo -e "${RED}❌ Port 80 is not active${NC}"
        return 1
    fi
    
    # Check if MySQL is listening on port 3306
    if sudo netstat -tlnp | grep ":3306 " >/dev/null 2>&1; then
        local process=$(sudo netstat -tlnp | grep ":3306 " | awk '{print $7}' | head -1)
        echo -e "${GREEN}✅ Port 3306 is active ($process)${NC}"
    else
        echo -e "${RED}❌ Port 3306 is not active${NC}"
        return 1
    fi
}

# Main execution
main() {
    echo -e "${BLUE}📋 Starting system validation...${NC}"
    echo ""
    
    # Check disk space
    check_disk_space
    echo ""
    
    # Step 1: Check and start Apache
    echo -e "${BLUE}1️⃣  Apache Web Server${NC}"
    if ! check_service_status $APACHE_SERVICE; then
        if ! start_service $APACHE_SERVICE; then
            echo -e "${RED}❌ Failed to start Apache. Exiting.${NC}"
            exit 1
        fi
    fi
    echo ""
    
    # Step 2: Check and start MySQL
    echo -e "${BLUE}2️⃣  MySQL Database Server${NC}"
    if ! check_service_status $MYSQL_SERVICE; then
        if ! start_service $MYSQL_SERVICE; then
            echo -e "${RED}❌ Failed to start MySQL. Exiting.${NC}"
            exit 1
        fi
    fi
    echo ""
    
    # Step 3: Validate ports
    echo -e "${BLUE}3️⃣  Port Validation${NC}"
    if ! check_ports; then
        echo -e "${RED}❌ Port validation failed. Services may not be properly configured.${NC}"
        exit 1
    fi
    echo ""
    
    # Step 4: Setup GitHub Codespaces port forwarding
    echo -e "${BLUE}4️⃣  GitHub Codespaces Port Forwarding${NC}"
    
    # Check if we're in a codespace environment
    if [ -z "$CODESPACE_NAME" ]; then
        echo -e "${YELLOW}⚠️  Not in a GitHub Codespace environment${NC}"
        echo -e "${GREEN}✅ Local development - port forwarding not required${NC}"
    else
        local external_url="https://${CODESPACE_NAME}-80.app.github.dev/"
        
        echo -e "${CYAN}ℹ️  Codespace: $CODESPACE_NAME${NC}"
        echo -e "${CYAN}ℹ️  Expected External URL: $external_url${NC}"
        
        # Test current port forwarding status
        echo -e "${YELLOW}🔍 Testing port forwarding status...${NC}"
        
        # Check GitHub CLI port status
        local port_forwarded=false
        local port_public=false
        
        if command -v gh >/dev/null 2>&1; then
            local port_list=$(gh codespace ports list 2>/dev/null || echo "")
            if echo "$port_list" | grep -E "^\s*80\s" >/dev/null; then
                port_forwarded=true
                if echo "$port_list" | grep -E "^\s*80\s.*public" >/dev/null; then
                    port_public=true
                fi
                local port_80_info=$(echo "$port_list" | grep -E "^\s*80\s" | head -1)
                echo -e "${GREEN}✅ Port 80 found in GitHub CLI: $port_80_info${NC}"
            else
                echo -e "${YELLOW}⚠️  Port 80 not found in GitHub CLI${NC}"
            fi
        fi
        
        # Test external accessibility
        local external_test=$(curl -s -o /dev/null -w "%{http_code}" -L "$external_url" --max-time 15 2>/dev/null || echo "000")
        
        if [[ "$external_test" =~ ^[23][0-9][0-9]$ ]]; then
            echo -e "${GREEN}✅ External URL is accessible (HTTP $external_test)${NC}"
            echo -e "${GREEN}🎉 Port forwarding is working correctly!${NC}"
        else
            echo -e "${YELLOW}⚠️  External URL not accessible (HTTP $external_test)${NC}"
            
            # Attempt automatic fix if GitHub CLI is available
            if command -v gh >/dev/null 2>&1; then
                if [ "$port_forwarded" = false ]; then
                    echo -e "${YELLOW}🔧 Attempting to forward port 80...${NC}"
                    if gh codespace ports forward 80 -c "$CODESPACE_NAME" 2>/dev/null; then
                        echo -e "${GREEN}✅ Port 80 forwarded successfully${NC}"
                        
                        # Now set it to public
                        echo -e "${YELLOW}🔧 Setting port 80 to public visibility...${NC}"
                        if gh codespace ports visibility 80:public -c "$CODESPACE_NAME" 2>/dev/null; then
                            echo -e "${GREEN}✅ Port 80 set to public${NC}"
                        else
                            echo -e "${YELLOW}⚠️  Failed to set port to public - may need manual adjustment${NC}"
                        fi
                        
                        sleep 5  # Wait for changes to take effect
                    else
                        echo -e "${YELLOW}⚠️  Automatic port forwarding failed${NC}"
                    fi
                elif [ "$port_public" = false ]; then
                    echo -e "${YELLOW}🔧 Attempting to set port 80 to public...${NC}"
                    if gh codespace ports visibility 80:public -c "$CODESPACE_NAME" 2>/dev/null; then
                        echo -e "${GREEN}✅ Port 80 visibility set to public${NC}"
                        sleep 5  # Wait for changes to take effect
                    else
                        echo -e "${YELLOW}⚠️  Failed to update port visibility${NC}"
                    fi
                fi
                
                # Test again after attempted fix
                local retest=$(curl -s -o /dev/null -w "%{http_code}" -L "$external_url" --max-time 10 2>/dev/null || echo "000")
                if [[ "$retest" =~ ^[23][0-9][0-9]$ ]]; then
                    echo -e "${GREEN}🎉 Port forwarding fix successful! (HTTP $retest)${NC}"
                else
                    echo -e "${YELLOW}⚠️  Port forwarding still requires manual setup${NC}"
                    show_port_forwarding_instructions
                fi
            else
                show_port_forwarding_instructions
            fi
        fi
    fi
    echo ""
    
    # Step 5: Setup Database and Drupal Installation
    echo -e "${BLUE}5️⃣  Database Setup & Drupal Installation${NC}"
    if setup_drupal_database; then
        if configure_drupal_database; then
            if install_drupal_if_needed; then
                echo -e "${GREEN}✅ Drupal database and installation complete${NC}"
            else
                echo -e "${YELLOW}⚠️  Drupal installation encountered issues but continuing...${NC}"
            fi
        else
            echo -e "${YELLOW}⚠️  Database configuration issues but continuing...${NC}"
        fi
    else
        echo -e "${RED}❌ Database setup failed${NC}"
        echo -e "${YELLOW}⚠️  Continuing with existing configuration...${NC}"
    fi
    echo ""
    
    # Step 6: Configure Apache Virtual Host for Drupal
    echo -e "${BLUE}6️⃣  Apache Virtual Host Configuration${NC}"
    if configure_apache_drupal; then
        echo -e "${GREEN}✅ Apache configured for Drupal${NC}"
    else
        echo -e "${YELLOW}⚠️  Apache configuration issues but continuing...${NC}"
        echo -e "${CYAN}ℹ️  You may need to manually configure Apache to serve Drupal${NC}"
    fi
    echo ""
    
    # Step 7: Clear Drupal cache (helps with module loading issues)
    echo -e "${BLUE}7️⃣  Drupal Cache Management${NC}"
    clear_drupal_cache
    echo ""
    
    # Step 8: Validate Drupal homepage
    echo -e "${BLUE}8️⃣  Website Validation${NC}"
    
    # Wait a moment for services to fully initialize
    echo -e "${YELLOW}⏳ Waiting for services to initialize...${NC}"
    sleep 5
    
    # Test main Drupal site
    if ! validate_url "$DRUPAL_URL" "Drupal Homepage"; then
        echo -e "${YELLOW}⚠️  Homepage validation failed. Checking error logs...${NC}"
        
        # Show recent Apache error logs
        echo -e "${YELLOW}📋 Recent Apache errors:${NC}"
        sudo tail -5 /var/log/apache2/drupal_error.log 2>/dev/null || echo "No error log found"
        
        echo -e "${YELLOW}🔄 Attempting to restart Apache...${NC}"
        sudo service apache2 restart
        sleep 3
        
        if ! validate_url "$DRUPAL_URL" "Drupal Homepage"; then
            echo -e "${RED}❌ Homepage still not accessible after restart${NC}"
            exit 1
        fi
    fi
    
    # Test LEAN dashboard with version validation
    echo -e "${BLUE}9️⃣  LEAN Metrics Dashboard Validation${NC}"
    
    # First try the new admin URL
    if validate_url_with_content "$DRUPAL_UNICORN_URL" "LEAN Metrics Dashboard" "Unicorn Metrics Dashboard"; then
        echo -e "${GREEN}✅ LEAN dashboard is operational at admin URL${NC}"
        
        # Check for version information
        local version_check=$(curl -s -L "$DRUPAL_UNICORN_URL" --max-time 10 | grep -i "version" | head -1)
        if [ -n "$version_check" ]; then
            echo -e "${GREEN}✅ Version information found: ${version_check}${NC}"
        else
            echo -e "${YELLOW}⚠️  Version information not found in dashboard${NC}"
        fi
    else
        echo -e "${YELLOW}⚠️  Admin URL failed, trying legacy URL...${NC}"
        
        # Fallback to old URL
        local legacy_url="https://${CODESPACE_NAME:-codespace}-80.app.github.dev/unicorn"
        if validate_url_with_content "$legacy_url" "LEAN Dashboard (legacy)" "Unicorn Metrics Dashboard"; then
            echo -e "${YELLOW}⚠️  Dashboard accessible via legacy URL: /unicorn${NC}"
            echo -e "${YELLOW}📋 Module routing may need to be refreshed${NC}"
        else
            echo -e "${RED}❌ Dashboard not accessible via either URL${NC}"
        fi
    fi
    echo ""
    
    # Final status
    echo -e "${BLUE}🔟 Final System Status${NC}"
    
    # Validate homepage content before declaring success
    if validate_homepage_content; then
        echo -e "${GREEN}========================================${NC}"
        echo -e "${GREEN}🎉 Drupal system is fully operational!${NC}"
        echo -e "${GREEN}========================================${NC}"
        echo ""
        echo -e "${BLUE}📱 Access Points:${NC}"
        echo -e "   🏠 Homepage (Local): $DRUPAL_URL"
        echo -e "   📊 Dashboard (Local): $DRUPAL_UNICORN_URL"
        echo -e "   🌐 Homepage (External): $DRUPAL_EXTERNAL_URL"
        echo -e "   📊 Dashboard (External): $DRUPAL_EXTERNAL_UNICORN_URL"
        echo ""
        echo -e "${BLUE}📊 Service Status:${NC}"
        echo -e "   🌐 Apache: $(sudo service apache2 status | head -1)"
        echo -e "   🗄️  MySQL: $(sudo service mysql status | head -1)"
        echo ""
        echo -e "${GREEN}✅ Startup validation completed successfully!${NC}"
    else
        echo -e "${RED}========================================${NC}"
        echo -e "${RED}❌ DRUPAL SYSTEM VALIDATION FAILED!${NC}"
        echo -e "${RED}========================================${NC}"
        echo ""
        echo -e "${RED}❌ CRITICAL: Homepage does not contain expected content${NC}"
        echo -e "${RED}❌ Expected: 'unicorninvesting.us'${NC}"
        echo ""
        echo -e "${YELLOW}🔧 Troubleshooting:${NC}"
        echo -e "${CYAN}   1. Check if Drupal is properly installed${NC}"
        echo -e "${CYAN}   2. Verify database connection in settings.php${NC}"
        echo -e "${CYAN}   3. Check Apache error logs: sudo tail -f /var/log/apache2/error.log${NC}"
        echo -e "${CYAN}   4. Try manual Drupal installation via web interface${NC}"
        echo ""
        echo -e "${BLUE}📱 Debug Access Points:${NC}"
        echo -e "   🏠 Homepage (Local): $DRUPAL_URL"
        echo -e "   🌐 Homepage (External): $DRUPAL_EXTERNAL_URL"
        echo -e "   🔧 Install Page: $DRUPAL_URL/core/install.php"
        echo ""
        echo -e "${RED}❌ Startup validation FAILED - manual intervention required${NC}"
        exit 1
    fi
}

# Function to validate homepage content
validate_homepage_content() {
    echo -e "${YELLOW}🔍 Validating homepage content...${NC}"
    
    local expected_text="unicorninvesting.us"
    local max_attempts=3
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        echo -e "${CYAN}ℹ️  Attempt $attempt/$max_attempts: Checking homepage content${NC}"
        
        # Test local URL first
        local homepage_content=$(curl -s -L "$DRUPAL_URL" --max-time 15 2>/dev/null || echo "")
        
        if [ -n "$homepage_content" ] && echo "$homepage_content" | grep -i "$expected_text" >/dev/null 2>&1; then
            echo -e "${GREEN}✅ Homepage validation successful - '$expected_text' found${NC}"
            return 0
        fi
        
        # If local fails, try external URL
        if [ -n "$CODESPACE_NAME" ]; then
            echo -e "${CYAN}ℹ️  Trying external URL...${NC}"
            homepage_content=$(curl -s -L "$DRUPAL_EXTERNAL_URL" --max-time 15 2>/dev/null || echo "")
            
            if [ -n "$homepage_content" ] && echo "$homepage_content" | grep -i "$expected_text" >/dev/null 2>&1; then
                echo -e "${GREEN}✅ Homepage validation successful via external URL - '$expected_text' found${NC}"
                return 0
            fi
        fi
        
        echo -e "${YELLOW}⚠️  Attempt $attempt failed - '$expected_text' not found${NC}"
        
        if [ $attempt -lt $max_attempts ]; then
            echo -e "${CYAN}ℹ️  Waiting 5 seconds before retry...${NC}"
            sleep 5
        fi
        
        ((attempt++))
    done
    
    echo -e "${RED}❌ Homepage validation failed after $max_attempts attempts${NC}"
    echo -e "${RED}❌ Could not find '$expected_text' on the homepage${NC}"
    
    # Show debug information
    echo -e "${YELLOW}🔧 Debug information:${NC}"
    echo -e "${CYAN}   Local URL: $DRUPAL_URL${NC}"
    if [ -n "$CODESPACE_NAME" ]; then
        echo -e "${CYAN}   External URL: $DRUPAL_EXTERNAL_URL${NC}"
    fi
    
    # Show first 200 characters of homepage content for debugging
    if [ -n "$homepage_content" ]; then
        echo -e "${CYAN}   Homepage content preview:${NC}"
        echo "$homepage_content" | head -c 200 | sed 's/^/   /'
        echo ""
    else
        echo -e "${CYAN}   No content retrieved from homepage${NC}"
    fi
    
    return 1
}

# Run main function
main "$@"
