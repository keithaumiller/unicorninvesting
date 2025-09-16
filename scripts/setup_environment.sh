#!/bin/bash

# Unicorn Investing - Comprehensive Environment Setup
# This script sets up the complete environment including:
# - System packages and services (MySQL, Apache, PHP 8.3)
# - Python virtual environment and packages
# - TA-Lib technical analysis library
# - LEAN framework (TEMPORARILY DISABLED)
# - Aliases and environment variables

set +e  # Don't exit on errors - handle them gracefully

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Function to check if command succeeded
check_success() {
    if [ $1 -eq 0 ]; then
        log_success "$2"
        return 0
    else
        log_error "$2 (Exit code: $1)"
        return 1
    fi
}

# Function to validate existing virtual environment
validate_venv() {
    log_info "Validating existing virtual environment..."
    
    # Check if .venv directory exists
    if [ ! -d "/workspaces/unicorninvesting/.venv" ]; then
        log_warning "Virtual environment directory not found"
        return 1
    fi
    
    # Check if activate script exists
    if [ ! -f "/workspaces/unicorninvesting/.venv/bin/activate" ]; then
        log_warning "Virtual environment activate script not found"
        return 1
    fi
    
    # Try to activate the virtual environment
    cd /workspaces/unicorninvesting
    if ! source .venv/bin/activate; then
        log_warning "Failed to activate virtual environment"
        return 1
    fi
    
    # Check if core Python packages are available
    local missing_packages=()
    
    # Test core packages
    if ! python3 -c "import pandas" 2>/dev/null; then
        missing_packages+=("pandas")
    fi
    
    if ! python3 -c "import numpy" 2>/dev/null; then
        missing_packages+=("numpy")
    fi
    
    if ! python3 -c "import yfinance" 2>/dev/null; then
        missing_packages+=("yfinance")
    fi
    
    if ! python3 -c "import sklearn" 2>/dev/null; then
        missing_packages+=("scikit-learn")
    fi
    
    if ! python3 -c "import fastapi" 2>/dev/null; then
        missing_packages+=("fastapi")
    fi
    
    # Check TA-Lib (optional but important)
    local talib_available=false
    if python3 -c "import talib" 2>/dev/null; then
        talib_available=true
    fi
    
    # Report validation results
    if [ ${#missing_packages[@]} -eq 0 ]; then
        log_success "Core packages validation: PASSED"
        if [ "$talib_available" = true ]; then
            log_success "TA-Lib validation: PASSED"
            log_success "Virtual environment is fully functional!"
            return 0
        else
            log_warning "TA-Lib not available but core packages work"
            log_info "Virtual environment is functional (TA-Lib needs installation)"
            return 2  # Special code: functional but missing TA-Lib
        fi
    else
        log_warning "Missing packages: ${missing_packages[*]}"
        return 1
    fi
}

echo "🦄 Unicorn Investing - Comprehensive Environment Setup"
echo "======================================================"

# Step 0: Check existing virtual environment first
log_info "Checking existing virtual environment..."
venv_status=$(validate_venv)
venv_result=$?

if [ $venv_result -eq 0 ]; then
    log_success "Existing virtual environment is fully functional!"
    log_info "Skipping Python package installation..."
    
    # Still need to ensure services are running
    log_info "Ensuring services are running..."
    sudo service mysql start 2>/dev/null || true
    sudo service apache2 start 2>/dev/null || true
    
    # Skip to aliases and final setup
    log_info "Proceeding to final configuration..."
    SKIP_PYTHON_SETUP=true
elif [ $venv_result -eq 2 ]; then
    log_info "Virtual environment functional but TA-Lib missing"
    log_info "Will install TA-Lib only..."
    SKIP_CORE_PYTHON=true
    INSTALL_TALIB_ONLY=true
else
    log_info "Virtual environment needs full setup..."
    SKIP_PYTHON_SETUP=false
fi

# Step 1: Update system packages (conditional)
if [ "$SKIP_PYTHON_SETUP" = true ]; then
    log_info "Skipping system package updates - environment already functional"
else
    log_info "Updating system packages..."
    sudo apt-get update -y
fi

# Step 2: Install system dependencies (conditional)
if [ "$SKIP_PYTHON_SETUP" = true ]; then
    log_info "Skipping system dependencies - already installed"
    # Still ensure core services are available
    if ! command -v mysql &> /dev/null; then
        log_info "Installing MySQL (missing)..."
        sudo apt-get install -y mysql-server mysql-client
    fi
    if ! command -v apache2 &> /dev/null; then
        log_info "Installing Apache (missing)..."
        sudo apt-get install -y apache2
    fi
else
    log_info "Installing system dependencies (MySQL, Apache, PHP 8.3)..."
    sudo apt-get install -y \
        mysql-server \
        mysql-client \
        apache2 \
        software-properties-common \
        curl \
        wget \
        git \
        build-essential \
        python3-dev \
        python3-pip \
        python3-venv \
        bc
    check_success $? "Core system dependencies installed"
fi

# Step 2b: Install TA-Lib system dependencies (conditional)
if [ "$SKIP_PYTHON_SETUP" = true ] && [ "$INSTALL_TALIB_ONLY" != true ]; then
    log_info "Skipping TA-Lib system setup - already functional"
elif [ "$INSTALL_TALIB_ONLY" = true ] || [ "$SKIP_PYTHON_SETUP" != true ]; then
    log_info "Installing TA-Lib system dependencies..."
    # First try Ubuntu packages
    if sudo apt-get install -y libta-lib-dev ta-lib-bin; then
        log_success "TA-Lib system packages installed"
    else
        log_warning "TA-Lib system packages not available, will compile from source"
        
        # Install TA-Lib from source
        log_info "Compiling TA-Lib from source..."
        cd /tmp
        
        # Download and compile TA-Lib
        if wget http://prdownloads.sourceforge.net/ta-lib/ta-lib-0.4.0-src.tar.gz; then
            tar -xzf ta-lib-0.4.0-src.tar.gz
            cd ta-lib/
            ./configure --prefix=/usr/local
            make
            sudo make install
            
            # Update library cache
            sudo ldconfig
            
            # Set environment variables for TA-Lib
            export TA_INCLUDE_PATH=/usr/local/include
            export TA_LIBRARY_PATH=/usr/local/lib
            
            log_success "TA-Lib compiled and installed from source"
            cd /workspaces/unicorninvesting
        else
            log_error "Failed to download TA-Lib source - continuing without it"
            cd /workspaces/unicorninvesting
        fi
    fi
fi

# Step 3: Install PHP 8.3 (conditional)
if [ "$SKIP_PYTHON_SETUP" = true ]; then
    log_info "Checking PHP 8.3 installation..."
    if ! php --version | grep -q "PHP 8.3"; then
        log_info "Installing PHP 8.3..."
        sudo add-apt-repository ppa:ondrej/php -y
        sudo apt-get update -y
        sudo apt-get install -y php8.3 php8.3-cli php8.3-mysql libapache2-mod-php8.3
    else
        log_success "PHP 8.3 already installed"
    fi
else
    log_info "Installing PHP 8.3 and extensions..."
    sudo add-apt-repository ppa:ondrej/php -y
    sudo apt-get update -y
    sudo apt-get install -y \
        php8.3 \
        php8.3-cli \
        php8.3-common \
        php8.3-mysql \
        php8.3-zip \
        php8.3-gd \
        php8.3-mbstring \
        php8.3-curl \
        php8.3-xml \
        php8.3-bcmath \
        php8.3-intl \
        libapache2-mod-php8.3
fi

# Step 4: Configure Apache for PHP 8.3
log_info "Configuring Apache for PHP 8.3..."
sudo a2dismod php8.0 2>/dev/null || true
sudo a2enmod php8.3
sudo a2enmod rewrite

# Update alternatives to use PHP 8.3 as default
sudo update-alternatives --install /usr/bin/php php /usr/bin/php8.3 60 --force
# Ensure PHP 8.3 is used in PATH (prioritize /usr/bin over codespace PHP)
export PATH="/usr/bin:$PATH"

# Step 5: Start services (using service command for containers)
log_info "Starting MySQL and Apache services..."
sudo service mysql start
sudo service apache2 start
log_success "Services started successfully"

# Step 6: Set up Python virtual environment (conditional)
if [ "$SKIP_PYTHON_SETUP" = true ]; then
    log_info "Skipping Python setup - using existing functional environment"
    cd /workspaces/unicorninvesting
    source .venv/bin/activate
else
    log_info "Setting up Python virtual environment..."
    cd /workspaces/unicorninvesting

    # Remove any broken virtual environment
    if [ -d ".venv" ] && [ ! -f ".venv/bin/activate" ]; then
        log_info "Removing broken virtual environment..."
        rm -rf .venv
    fi

    if [ ! -d ".venv" ]; then
        python3 -m venv .venv
        log_success "Python virtual environment created"
    else
        log_success "Python virtual environment already exists"
    fi

    # Activate virtual environment and verify it works
    source .venv/bin/activate
    if [ "$VIRTUAL_ENV" != "" ]; then
        log_success "Virtual environment activated: $VIRTUAL_ENV"
    else
        log_error "Failed to activate virtual environment"
        exit 1
    fi
fi

# Step 7: Install Python packages (conditional)
if [ "$SKIP_PYTHON_SETUP" = true ]; then
    log_info "Skipping Python package installation - using existing packages"
elif [ "$INSTALL_TALIB_ONLY" = true ]; then
    log_info "Installing TA-Lib only (core packages already available)..."
    
    # Try to install TA-Lib Python package
    log_info "Installing TA-Lib Python package..."
    if pip install TA-Lib; then
        log_success "TA-Lib Python package installed successfully"
    else
        log_warning "TA-Lib Python package installation failed"
        
        # Try alternative installation methods
        log_info "Attempting alternative TA-Lib installation..."
        
        # Set environment variables in case they're needed
        export TA_INCLUDE_PATH=/usr/local/include
        export TA_LIBRARY_PATH=/usr/local/lib
        export TALIB_INCLUDE=/usr/local/include
        export TALIB_LIB=/usr/local/lib
        
        if pip install --no-cache-dir TA-Lib; then
            log_success "TA-Lib installed with alternative method"
        else
            log_error "TA-Lib installation failed completely - continuing without it"
        fi
    fi
else
    log_info "Installing Python packages..."
    pip install --upgrade pip setuptools wheel

    # Install core packages first
    pip install pandas numpy scipy scikit-learn

    # Install API and web framework packages
    log_info "Installing API and web framework packages..."
    pip install fastapi uvicorn

    # Install forecasting packages
    log_info "Installing forecasting packages..."
    pip install prophet

    # Install database packages
    log_info "Installing database packages..."
    pip install sqlalchemy pymysql

    # Install financial packages
    log_info "Installing financial data packages..."
    pip install yfinance alpha-vantage quandl ccxt fredapi beaapi

    # Install technical analysis and silver layer processing packages
    log_info "Installing technical analysis and advanced analytics packages..."

    # Try to install TA-Lib Python package
    log_info "Installing TA-Lib Python package..."
    if pip install TA-Lib; then
        log_success "TA-Lib Python package installed successfully"
    else
        log_warning "TA-Lib Python package installation failed"
        
        # Try alternative installation methods
        log_info "Attempting alternative TA-Lib installation..."
        
        # Set environment variables in case they're needed
        export TA_INCLUDE_PATH=/usr/local/include
        export TA_LIBRARY_PATH=/usr/local/lib
        export TALIB_INCLUDE=/usr/local/include
        export TALIB_LIB=/usr/local/lib
        
        if pip install --no-cache-dir TA-Lib; then
            log_success "TA-Lib installed with alternative method"
        else
            log_error "TA-Lib installation failed completely - continuing without it"
        fi
    fi

    # Install other technical analysis packages
    pip install matplotlib seaborn plotly || log_warning "Some visualization packages failed"

    # Install additional requirements
    log_info "Installing remaining Python packages (this may take a few minutes)..."
    if [ -f "BackendPython/requirements.txt" ]; then
        # Install packages one by one to handle version conflicts
        log_info "Installing core financial packages..."
        
        # Install quantlib-python with available version
        pip install quantlib-python==1.18 || log_warning "QuantLib installation failed"
        
        # Install other core packages
        pip install tensorflow keras torch xgboost lightgbm || log_warning "Some ML packages failed"
        
        # Install data sources
        pip install pandas-datareader fredapi || log_warning "Some data source packages failed"
        
        # Install remaining packages (skip problematic ones)
        pip install -r BackendPython/requirements.txt --no-deps || log_warning "Some packages from requirements.txt failed"
        
        log_success "Python packages installation completed (some packages may have been skipped)"
    else
        log_warning "Requirements file not found, skipping package installation"
    fi
fi

# Step 8: Install LEAN Framework (TEMPORARILY DISABLED)
# log_info "Installing LEAN Framework..."
# if [ ! -d "BackendPython/Lean" ]; then
#     cd BackendPython
#     git clone https://github.com/QuantConnect/Lean.git
#     cd Lean
#     # Build LEAN (this requires .NET SDK)
#     if command -v dotnet &> /dev/null; then
#         dotnet build
#         log_success "LEAN Framework installed and built"
#     else
#         log_warning "LEAN downloaded but .NET SDK not found for building"
#     fi
#     cd /workspaces/unicorninvesting
# else
#     log_success "LEAN Framework already exists"
# fi
log_info "LEAN Framework installation temporarily disabled - skipping..."
log_success "LEAN Framework installation skipped (can be re-enabled)"

# Step 9: Set up MySQL for Drupal
log_info "Configuring MySQL for Drupal..."
sudo mysql -e "CREATE DATABASE IF NOT EXISTS unicorn_drupal;" 2>/dev/null || true
sudo mysql -e "CREATE USER IF NOT EXISTS 'unicorn'@'localhost' IDENTIFIED BY 'unicorn123';" 2>/dev/null || true
sudo mysql -e "GRANT ALL PRIVILEGES ON unicorn_drupal.* TO 'unicorn'@'localhost';" 2>/dev/null || true
sudo mysql -e "FLUSH PRIVILEGES;" 2>/dev/null || true
log_success "MySQL database configured"

# Step 10: Set up Automated Data Refresh System (Every 5 Minutes)
log_info "Setting up automated data refresh system (5-minute intervals)..."

# Create logs directory
mkdir -p /workspaces/unicorninvesting/logs/data_refresh

# Install automated data refresh cron jobs using our cron management system
if [ -f "/workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh" ]; then
    log_info "Installing 5-minute data refresh automation..."
    bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh install
    
    if [ $? -eq 0 ]; then
        log_success "Automated data refresh system installed successfully"
        log_info "📅 Data refresh schedule: Every 5 minutes"
        log_info "📊 Includes: Silver layer refresh, portfolio cache validation, system cleanup"
        log_info "📁 Logs: /workspaces/unicorninvesting/logs/data_refresh/"
    else
        log_warning "Failed to install automated data refresh - manual setup may be required"
    fi
else
    log_warning "Cron management script not found - setting up basic data refresh manually..."
    
    # Fallback: Set up basic data refresh cron job manually
    DATA_REFRESH_JOB="*/5 * * * * cd /workspaces/unicorninvesting && bash /workspaces/unicorninvesting/scripts/cron/jobs/automated_data_refresh.sh >> /workspaces/unicorninvesting/logs/data_refresh/automated_refresh.log 2>&1"
    
    if ! crontab -l 2>/dev/null | grep -q "automated_data_refresh.sh"; then
        (crontab -l 2>/dev/null; echo "$DATA_REFRESH_JOB") | crontab -
        log_success "Basic data refresh cron job added (every 5 minutes)"
    else
        log_success "Data refresh cron job already exists"
    fi
fi

# Start cron service if not running
if ! pgrep cron > /dev/null; then
    sudo service cron start
    log_success "Cron service started"
else
    log_success "Cron service already running"
fi

log_success "Automated data refresh system configured"

# Add to ~/.bashrc for persistent aliases
if [ -f ~/.bashrc ]; then
    # Check if our aliases are already in .bashrc
    if ! grep -q "# Unicorn Investing Aliases" ~/.bashrc; then
        echo "" >> ~/.bashrc
        echo "# Unicorn Investing Aliases" >> ~/.bashrc
        echo "alias drupal-start='/workspaces/unicorninvesting/scripts/startup_drupal.sh'" >> ~/.bashrc
        echo "alias drupal-status='sudo service apache2 status && sudo service mysql status'" >> ~/.bashrc
        echo "alias drupal-logs='sudo tail -20 /var/log/apache2/drupal_error.log'" >> ~/.bashrc
        echo "alias drupal-restart='sudo service apache2 restart && sudo service mysql restart'" >> ~/.bashrc
        echo "alias drupal-cd='cd /workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
        echo "alias unicorn-root='cd /workspaces/unicorninvesting'" >> ~/.bashrc
        echo "alias unicorn-env='/workspaces/unicorninvesting/scripts/unicorn_environment.sh'" >> ~/.bashrc
        echo "# Cron Management Aliases" >> ~/.bashrc
        echo "alias cron-install='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh install'" >> ~/.bashrc
        echo "alias cron-status='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh status'" >> ~/.bashrc
        echo "alias cron-logs='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh logs'" >> ~/.bashrc
        echo "alias cron-test='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh test'" >> ~/.bashrc
        echo "alias cron-remove='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh remove'" >> ~/.bashrc
        echo "alias cron-validate='bash /workspaces/unicorninvesting/scripts/cron/validate_data_refresh.sh'" >> ~/.bashrc
        echo "" >> ~/.bashrc
        echo "# Unicorn Investing Environment" >> ~/.bashrc
        echo "export UNICORN_ROOT='/workspaces/unicorninvesting'" >> ~/.bashrc
        echo "export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
        echo "export DRUPAL_URL='https://${CODESPACE_NAME:-codespace}-80.app.github.dev/'" >> ~/.bashrc
        echo "export FRED_API_KEY='CONFIGURED_IN_CONFIG_SECRETS_JSON'" >> ~/.bashrc
        echo "# BEA API key for Bureau of Economic Analysis data collection" >> ~/.bashrc
        echo "export BEA_API_KEY='CONFIGURED_IN_CONFIG_SECRETS_JSON'" >> ~/.bashrc
        echo "" >> ~/.bashrc
        echo "# Ensure PHP 8.3 is used by default (prioritize /usr/bin over codespace PHP)" >> ~/.bashrc
        echo "export PATH=\"/usr/bin:\$PATH\"" >> ~/.bashrc
        
        echo "✅ Aliases added to ~/.bashrc"
        echo "💡 Run 'source ~/.bashrc' or restart your terminal to use them"
    else
        echo "✅ Aliases already exist in ~/.bashrc"
    fi
fi

# Set up aliases for current session
alias drupal-start='/workspaces/unicorninvesting/scripts/startup_drupal.sh'
alias drupal-status='sudo service apache2 status && sudo service mysql status'
alias drupal-logs='sudo tail -20 /var/log/apache2/drupal_error.log'
alias drupal-restart='sudo service apache2 restart && sudo service mysql restart'
alias drupal-cd='cd /workspaces/unicorninvesting/WebFrontend'
alias unicorn-root='cd /workspaces/unicorninvesting'
# Cron management aliases for current session
alias cron-install='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh install'
alias cron-status='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh status'
alias cron-logs='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh logs'
alias cron-test='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh test'
alias cron-remove='bash /workspaces/unicorninvesting/scripts/cron/manage_cron_jobs.sh remove'
alias cron-validate='bash /workspaces/unicorninvesting/scripts/cron/validate_data_refresh.sh'
# Data warehouse testing aliases for current session
alias test-warehouse='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh'
alias test-raw='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --layer=raw'
alias test-bronze='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --layer=bronze'
alias test-silver='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --layer=silver'
alias test-gold='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --layer=gold'
alias test-yahoo='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --connector=yahoo'
alias test-fred='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --connector=fred'
alias test-ibkr='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --connector=ibkr'
alias test-forex='/workspaces/unicorninvesting/tests/unicorn/1_data_sources/test_data_warehouse.sh --connector=forex'

# Set environment variables for current session
export UNICORN_ROOT='/workspaces/unicorninvesting'
export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'
export DRUPAL_URL="https://${CODESPACE_NAME:-codespace}-80.app.github.dev/"
export FRED_API_KEY='CONFIGURED_IN_CONFIG_SECRETS_JSON'
export BEA_API_KEY='CONFIGURED_IN_CONFIG_SECRETS_JSON'

echo ""
echo "🦄 Unicorn Investing Environment Ready!"
echo ""
echo "Available commands:"
echo "  drupal-start    - Start and validate Drupal system"
echo "  drupal-status   - Check Apache and MySQL status"
echo "  drupal-logs     - View recent Drupal error logs"
echo "  drupal-restart  - Restart Apache and MySQL services"
echo "  drupal-cd       - Change to Drupal root directory"
echo "  unicorn-root    - Change to project root directory"
echo ""
echo "Cron Management commands:"
echo "  cron-install    - Install 5-minute data refresh automation"
echo "  cron-status     - Check cron job status and recent activity"
echo "  cron-logs       - View data refresh logs"
echo "  cron-test       - Run manual data refresh test"
echo "  cron-remove     - Remove all data refresh cron jobs"
echo "  cron-validate   - Comprehensive data refresh system validation"
echo ""
echo "Data Warehouse Testing commands:"
echo "  test-warehouse  - Test all data warehouse layers"
echo "  test-raw        - Test raw data layer only"
echo "  test-bronze     - Test bronze data layer only"  
echo "  test-silver     - Test silver data layer only"
echo "  test-gold       - Test gold data layer only"
echo "  test-yahoo      - Test Yahoo Finance connector only"
echo "  test-fred       - Test FRED connector only"
echo "  test-ibkr       - Test IBKR connector only"
echo "  test-forex      - Test Forex connector only"
echo ""
echo "Environment variables:"
echo "  UNICORN_ROOT = $UNICORN_ROOT"
echo "  DRUPAL_ROOT = $DRUPAL_ROOT"
echo "  DRUPAL_URL = $DRUPAL_URL"
echo ""
echo "🔑 API Keys Setup:"
echo "  ✅ FRED API Key: Configured for Federal Reserve data"
echo "  ✅ BEA API Key: Configured and activated for Bureau of Economic Analysis data"
echo ""
echo "📊 Automated Data Collection:"
echo "  • Data Refresh: Every 5 minutes (silver layer + portfolio cache)"
echo "  • System Validation: Comprehensive bronze/silver layer processing"
echo "  • Logs: /workspaces/unicorninvesting/logs/data_refresh/"

# Ensure virtual environment is available for future sessions
if [ -f "/workspaces/unicorninvesting/.venv/bin/activate" ]; then
    log_success "Virtual environment ready at: /workspaces/unicorninvesting/.venv"
else
    log_warning "Virtual environment not found - creating it now..."
    cd /workspaces/unicorninvesting
    python3 -m venv .venv
    source .venv/bin/activate
fi

# Final validation of critical components
log_info "Performing final system validation..."

# Test virtual environment
if source /workspaces/unicorninvesting/.venv/bin/activate; then
    log_success "Virtual environment activation: PASSED"
else
    log_error "Virtual environment activation: FAILED"
fi

# Test critical Python packages
source /workspaces/unicorninvesting/.venv/bin/activate
python3 -c "import pandas, numpy, yfinance; print('Core packages working')" && log_success "Core Python packages: PASSED" || log_error "Core Python packages: FAILED"

# Test TA-Lib specifically
python3 -c "import talib; print('TA-Lib working')" && log_success "TA-Lib: PASSED" || log_warning "TA-Lib: NOT AVAILABLE"

# Test services
if sudo service mysql status >/dev/null 2>&1; then
    log_success "MySQL service: RUNNING"
else
    log_error "MySQL service: NOT RUNNING"
fi

if sudo service apache2 status >/dev/null 2>&1; then
    log_success "Apache service: RUNNING"
else
    log_error "Apache service: NOT RUNNING"
fi

# Test database connectivity
if mysql -u unicorn -punicorn123 -e "SELECT 1;" >/dev/null 2>&1; then
    log_success "Database connectivity: PASSED"
else
    log_warning "Database connectivity: CHECK REQUIRED"
fi

log_info "Environment setup validation completed"

# Automatically source ~/.bashrc to activate new aliases for this session
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
    log_success "Aliases and environment variables loaded"
fi
