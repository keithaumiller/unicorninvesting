#!/bin/bash

# Unicorn Investing - Comprehensive Environment Setup
# This script sets up the complete environment including:
# - System packages and services (MySQL, Apache, PHP 8.3)
# - Python virtual environment and packages
# - LEAN framework (TEMPORARILY DISABLED)
# - Aliases and environment variables

set -e  # Exit on any error

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

echo "🦄 Unicorn Investing - Comprehensive Environment Setup"
echo "======================================================"

# Step 1: Update system packages
log_info "Updating system packages..."
sudo apt-get update -y

# Step 2: Install system dependencies
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
    python3-venv

# Step 3: Install PHP 8.3
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

# Step 6: Set up Python virtual environment
log_info "Setting up Python virtual environment..."
cd /workspaces/unicorninvesting
if [ ! -d ".venv" ]; then
    python3 -m venv .venv
    log_success "Python virtual environment created"
else
    log_success "Python virtual environment already exists"
fi

# Activate virtual environment
source .venv/bin/activate

# Step 7: Upgrade pip and install Python packages
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
pip install yfinance alpha-vantage quandl ccxt

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

log_success "MySQL database configured"

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
        echo "" >> ~/.bashrc
        echo "# Unicorn Investing Environment" >> ~/.bashrc
        echo "export UNICORN_ROOT='/workspaces/unicorninvesting'" >> ~/.bashrc
        echo "export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
        echo "export DRUPAL_URL='https://${CODESPACE_NAME:-codespace}-80.app.github.dev/'" >> ~/.bashrc
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

# Set environment variables for current session
export UNICORN_ROOT='/workspaces/unicorninvesting'
export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'
export DRUPAL_URL="https://${CODESPACE_NAME:-codespace}-80.app.github.dev/"

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
echo "Environment variables:"
echo "  UNICORN_ROOT = $UNICORN_ROOT"
echo "  DRUPAL_ROOT = $DRUPAL_ROOT"
echo "  DRUPAL_URL = $DRUPAL_URL"
