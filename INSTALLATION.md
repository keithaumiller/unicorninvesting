# Unicorn Investing Platform - Installation Guide

This guide provides complete step-by-step instructions for setting up the Unicorn Investing Platform from scratch, ensuring all components are properly configured for production deployment.

## Table of Contents

1. [System Requirements](#system-requirements)
2. [Environment Setup](#environment-setup)
3. [Database Configuration](#database-configuration)
4. [Python Environment](#python-environment)
5. [LEAN Framework Integration](#lean-framework-integration)
6. [Data Sources Integration](#data-sources-integration)
7. [Prophet Forecasting Setup](#prophet-forecasting-setup)
8. [Web Server Configuration](#web-server-configuration)
9. [Drupal Frontend Setup](#drupal-frontend-setup)
10. [SSL Certificate Setup](#ssl-certificate-setup)
11. [Verification and Testing](#verification-and-testing)
12. [Production Deployment](#production-deployment)
13. [Troubleshooting](#troubleshooting)

---

## System Requirements

### Hardware Requirements
- **CPU**: 4+ cores (8+ recommended for ML workloads)
- **RAM**: 8GB minimum (16GB+ recommended)
- **Storage**: 100GB+ SSD storage
- **Network**: Stable internet connection for market data

### Software Requirements
- **OS**: Ubuntu 24.04 LTS (recommended) or Ubuntu 22.04 LTS
- **Python**: 3.12.3 (will be installed in virtual environment)
- **MySQL**: 8.0+
- **Apache**: 2.4+
- **PHP**: 8.3+
- **Node.js**: 18+ (for some development tools)

---

## Environment Setup

### 1. System Update and Base Packages

```bash
# Update system packages
sudo apt update && sudo apt upgrade -y

# Install essential development tools
sudo apt install -y \
    software-properties-common \
    curl \
    wget \
    git \
    build-essential \
    pkg-config \
    libssl-dev \
    libffi-dev \
    python3-dev \
    python3-pip \
    python3-venv \
    zip \
    unzip
```

### 2. Directory Structure Creation

```bash
# Create main project directory
sudo mkdir -p /workspaces/unicorninvesting
sudo chown $(whoami):$(whoami) /workspaces/unicorninvesting
cd /workspaces/unicorninvesting

# Clone the repository
git clone https://github.com/keithaumiller/unicorninvesting.git .

# Initialize git submodules (for LEAN)
git submodule update --init --recursive
```

---

## Database Configuration

### 1. MySQL Installation and Setup

```bash
# Install MySQL Server
sudo apt install -y mysql-server

# Secure MySQL installation
sudo mysql_secure_installation
```

### 2. Database Creation and User Setup

```sql
-- Connect to MySQL as root
sudo mysql -u root -p

-- Create databases
CREATE DATABASE unicorn_analytics CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE DATABASE unicorninvesting_drupal CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE DATABASE stlouisintegration_drupal CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE DATABASE angelicafeliciano_drupal CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- Create users with strong passwords
CREATE USER 'unicorn_user'@'localhost' IDENTIFIED BY 'STRONG_PASSWORD_HERE';
CREATE USER 'drupal_main'@'localhost' IDENTIFIED BY 'STRONG_PASSWORD_HERE';
CREATE USER 'drupal_stlouis'@'localhost' IDENTIFIED BY 'STRONG_PASSWORD_HERE';
CREATE USER 'drupal_angelica'@'localhost' IDENTIFIED BY 'STRONG_PASSWORD_HERE';

-- Grant privileges
GRANT ALL PRIVILEGES ON unicorn_analytics.* TO 'unicorn_user'@'localhost';
GRANT ALL PRIVILEGES ON unicorninvesting_drupal.* TO 'drupal_main'@'localhost';
GRANT ALL PRIVILEGES ON stlouisintegration_drupal.* TO 'drupal_stlouis'@'localhost';
GRANT ALL PRIVILEGES ON angelicafeliciano_drupal.* TO 'drupal_angelica'@'localhost';

FLUSH PRIVILEGES;
EXIT;
```

### 3. Database Configuration File

Create `/workspaces/unicorninvesting/database/config/database.env`:

```bash
# Database Configuration
DB_HOST=localhost
DB_PORT=3306

# Main Analytics Database
ANALYTICS_DB_NAME=unicorn_analytics
ANALYTICS_DB_USER=unicorn_user
ANALYTICS_DB_PASSWORD=YOUR_STRONG_PASSWORD

# Drupal Databases
DRUPAL_MAIN_DB_NAME=unicorninvesting_drupal
DRUPAL_MAIN_DB_USER=drupal_main
DRUPAL_MAIN_DB_PASSWORD=YOUR_STRONG_PASSWORD

DRUPAL_STLOUIS_DB_NAME=stlouisintegration_drupal
DRUPAL_STLOUIS_DB_USER=drupal_stlouis
DRUPAL_STLOUIS_DB_PASSWORD=YOUR_STRONG_PASSWORD

DRUPAL_ANGELICA_DB_NAME=angelicafeliciano_drupal
DRUPAL_ANGELICA_DB_USER=drupal_angelica
DRUPAL_ANGELICA_DB_PASSWORD=YOUR_STRONG_PASSWORD
```

---

## Python Environment

### 1. Python Virtual Environment Setup

```bash
cd /workspaces/unicorninvesting

# Create virtual environment
python3 -m venv .venv

# Activate virtual environment
source .venv/bin/activate

# Upgrade pip
pip install --upgrade pip setuptools wheel
```

### 2. Core Dependencies Installation

```bash
# Install base requirements
pip install -r BackendPython/requirements.txt

# Install additional unicorn-specific packages
pip install -r BackendPython/requirements-unicorn.txt

# Install Prophet for forecasting
pip install prophet

# Install development tools
pip install pytest black flake8 mypy jupyter
```

### 3. LEAN-Specific Dependencies

```bash
# Install LEAN requirements
pip install -r BackendPython/requirements-lean.txt

# Additional numerical and ML libraries
pip install \
    tensorflow==2.20.0 \
    torch==2.5.1 \
    xgboost==3.0.2 \
    lightgbm==4.5.0 \
    catboost==1.2.7 \
    prophet==1.1.7
```

### 4. Verify Python Installation

```bash
# Test core functionality
cd /workspaces/unicorninvesting/BackendPython/unicorn/backend
python -c "from api.main import app; print('✅ FastAPI application loads successfully')"

# Test Prophet
cd /workspaces/unicorninvesting/BackendPython/unicorn/algorithms
python test_prophet.py
```

---

## LEAN Framework Integration

### 1. LEAN Setup

```bash
cd /workspaces/unicorninvesting/BackendPython/Lean

# Install .NET 8.0 SDK (required for LEAN)
wget https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
sudo apt update
sudo apt install -y dotnet-sdk-8.0

# Verify .NET installation
dotnet --version
```

### 2. LEAN Configuration

Create `/workspaces/unicorninvesting/BackendPython/Lean/Launcher/config.json`:

```json
{
    "algorithm-type-name": "BasicTemplateAlgorithm",
    "algorithm-language": "Python",
    "algorithm-location": "Algorithm.Python/BasicTemplateAlgorithm.py",
    
    "data-folder": "/workspaces/unicorninvesting/BackendPython/Lean/Data",
    "cache-location": "/tmp/lean-cache",
    
    "debugging": true,
    "debugging-method": "LocalCmdline",
    
    "log-handler": "ConsoleLogHandler",
    "messaging-handler": "ConsoleMessagingHandler",
    "job-queue-handler": "ConsoleQueueHandler",
    "api-handler": "LocalDiskApiHandler",
    
    "environments": {
        "backtesting": {
            "live-mode": false,
            "setup-handler": "ConsoleSetupHandler",
            "result-handler": "BacktestingResultHandler",
            "data-feed-handler": "FileSystemDataFeed",
            "real-time-handler": "BacktestingRealTimeHandler",
            "history-provider": "SubscriptionDataReaderHistoryProvider",
            "transaction-handler": "BacktestingTransactionHandler"
        }
    }
}
```

---

## Data Sources Integration

### 1. Interactive Brokers (IBKR) Setup

The platform integrates with Interactive Brokers for live trading and market data through the Client Portal Gateway.

#### Prerequisites
- Active Interactive Brokers account
- Java 8+ installed for running the gateway

#### Download and Setup IBKR Gateway

```bash
# Navigate to IBKR tools directory
cd /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/tools

# Download the Client Portal Gateway (if not already present)
# Manual download required from: https://www.interactivebrokers.com/en/trading/ib-api.php

# Verify Java installation
java -version

# Start the IBKR Gateway
cd clientportal.gw
./bin/run.sh root/conf.yaml
```

#### Gateway Configuration

Create or modify `root/conf.yaml`:

```yaml
ssl: false
enableFeatures:
  - bond
  - cryptocurrency
  - futures
  - stocks
  - forex
listenPort: 5000
proxyRemotehostForLocalhost: true
```

#### Authentication Setup

1. **Start the Gateway**: Run the gateway with the configuration above
2. **Access Web Interface**: Navigate to https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/
3. **Login**: Use your IBKR credentials and complete 2FA if required
4. **Verify Connection**: The gateway should show "authenticated: true" status

#### Test IBKR Integration

```bash
# Activate Python environment
source /workspaces/unicorninvesting/.venv/bin/activate

# Test IBKR connection
cd /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers
python IBKRClientPortalConnector.py

# Test ETH data collection
python eth_data_collector.py
```

### 2. Yahoo Finance Setup

Yahoo Finance integration requires no authentication and provides free market data.

```bash
# Install yfinance (already included in requirements.txt)
source /workspaces/unicorninvesting/.venv/bin/activate
pip install yfinance

# Test Yahoo Finance connector
cd /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/yahoo_finance
python eth_data_collector.py
```

### 3. Alpha Vantage Setup

Alpha Vantage requires an API key for access to financial data.

#### Get API Key
1. Visit https://www.alphavantage.co/support/#api-key
2. Sign up for a free account
3. Copy your API key

#### Configure Alpha Vantage

```bash
# Create configuration file
cd /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/alpha_vantage

# Create config.json with your API key
cat > config.json << EOF
{
    "api_key": "YOUR_ALPHA_VANTAGE_API_KEY_HERE",
    "base_url": "https://www.alphavantage.co/query"
}
EOF
```

### 4. Data Sources Validation

Use the health check script to validate all data source integrations:

```bash
# Run comprehensive health check including data sources
/workspaces/unicorninvesting/scripts/unicorn_environment.sh --check-only
```

The health check will validate:
- ✅ IBKR Gateway connectivity and authentication status
- ✅ Yahoo Finance library availability and functionality  
- ✅ Alpha Vantage API key configuration
- ✅ Data collection capabilities for each source

---

## Prophet Forecasting Setup

### 1. Prophet Installation Verification

```bash
cd /workspaces/unicorninvesting/BackendPython/unicorn/algorithms

# Run Prophet test
python test_prophet.py

# Run Prophet demo
python prophet_forex_demo.py
```

### 2. Prophet Configuration Optimization

The following Prophet configurations are optimized for forex:

```python
# Optimal Prophet settings for forex (reference)
model = Prophet(
    daily_seasonality=True,      # Trading sessions
    weekly_seasonality=True,     # Weekend effects
    yearly_seasonality=False,    # Not enough data typically
    changepoint_prior_scale=0.05,    # Conservative trend changes
    seasonality_prior_scale=15.0,    # Strong forex seasonality
    seasonality_mode='multiplicative',  # Percentage-based movements
    interval_width=0.8,               # 80% confidence intervals
)
```

---

## Web Server Configuration

### 1. Apache Installation and Setup

```bash
# Install Apache and modules
sudo apt install -y apache2 apache2-utils

# Enable required modules
sudo a2enmod rewrite
sudo a2enmod ssl
sudo a2enmod headers
sudo a2enmod expires

# Start and enable Apache
sudo systemctl start apache2
sudo systemctl enable apache2
```

### 2. PHP Installation

```bash
# Add PHP repository
sudo add-apt-repository ppa:ondrej/php -y
sudo apt update

# Install PHP 8.3 and extensions
sudo apt install -y \
    php8.3 \
    php8.3-apache2 \
    php8.3-mysql \
    php8.3-curl \
    php8.3-gd \
    php8.3-mbstring \
    php8.3-xml \
    php8.3-zip \
    php8.3-bcmath \
    php8.3-intl \
    php8.3-soap \
    php8.3-opcache

# Configure PHP
sudo sed -i 's/memory_limit = .*/memory_limit = 512M/' /etc/php/8.3/apache2/php.ini
sudo sed -i 's/upload_max_filesize = .*/upload_max_filesize = 100M/' /etc/php/8.3/apache2/php.ini
sudo sed -i 's/post_max_size = .*/post_max_size = 100M/' /etc/php/8.3/apache2/php.ini
```

### 3. Composer Installation

```bash
# Install Composer
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer
sudo chmod +x /usr/local/bin/composer
```

---

## Drupal Frontend Setup

### 1. Drupal Installation

```bash
# Create web directories
sudo mkdir -p /var/www/unicorninvesting.com
sudo mkdir -p /var/www/stlouisintegration.com
sudo mkdir -p /var/www/angelicafeliciano.com

# Set ownership
sudo chown -R www-data:www-data /var/www/
sudo chmod -R 755 /var/www/

# Install Drupal 11 for main site
cd /var/www/unicorninvesting.com
sudo -u www-data composer create-project drupal/recommended-project .
```

### 2. Virtual Host Configuration

Create `/etc/apache2/sites-available/unicorninvesting.conf`:

```apache
<VirtualHost *:80>
    ServerName unicorninvesting.com
    ServerAlias www.unicorninvesting.com
    DocumentRoot /var/www/unicorninvesting.com/web
    
    <Directory /var/www/unicorninvesting.com/web>
        Options -Indexes +FollowSymLinks
        AllowOverride All
        Require all granted
    </Directory>
    
    ErrorLog ${APACHE_LOG_DIR}/unicorninvesting_error.log
    CustomLog ${APACHE_LOG_DIR}/unicorninvesting_access.log combined
</VirtualHost>
```

Enable the site:

```bash
sudo a2ensite unicorninvesting.conf
sudo systemctl reload apache2
```

---

## SSL Certificate Setup

### 1. Certbot Installation

```bash
# Install Certbot
sudo apt install -y certbot python3-certbot-apache

# Obtain SSL certificates
sudo certbot --apache -d unicorninvesting.com -d www.unicorninvesting.com
sudo certbot --apache -d stlouisintegration.com -d www.stlouisintegration.com
sudo certbot --apache -d angelicafeliciano.com -d www.angelicafeliciano.com

# Set up automatic renewal
sudo crontab -e
# Add: 0 12 * * * /usr/bin/certbot renew --quiet
```

---

## Verification and Testing

### 1. System Health Checks

Create `/workspaces/unicorninvesting/scripts/health_check.sh`:

```bash
#!/bin/bash

echo "🦄 Unicorn Investing Platform - Health Check"
echo "=============================================="

# Check MySQL
echo "📊 Database Status:"
sudo systemctl is-active mysql && echo "✅ MySQL: Running" || echo "❌ MySQL: Failed"

# Check Apache
echo "🌐 Web Server Status:"
sudo systemctl is-active apache2 && echo "✅ Apache: Running" || echo "❌ Apache: Failed"

# Check Python Environment
echo "🐍 Python Environment:"
if [ -f "/workspaces/unicorninvesting/.venv/bin/python" ]; then
    echo "✅ Virtual Environment: Available"
    source /workspaces/unicorninvesting/.venv/bin/activate
    python -c "import fastapi, prophet, pandas; print('✅ Core packages: Available')"
else
    echo "❌ Virtual Environment: Missing"
fi

# Check LEAN
echo "🔧 LEAN Framework:"
if [ -d "/workspaces/unicorninvesting/BackendPython/Lean" ]; then
    echo "✅ LEAN: Available"
else
    echo "❌ LEAN: Missing"
fi

# Check SSL Certificates
echo "🔒 SSL Certificates:"
sudo certbot certificates | grep -q "unicorninvesting.com" && echo "✅ SSL: Configured" || echo "❌ SSL: Missing"

echo "=============================================="
echo "Health check complete!"
```

Make it executable and run:

```bash
chmod +x /workspaces/unicorninvesting/scripts/health_check.sh
./scripts/health_check.sh
```

### 2. Component Testing

```bash
# Test FastAPI backend
cd /workspaces/unicorninvesting/BackendPython/unicorn/backend
source /workspaces/unicorninvesting/.venv/bin/activate
python -c "from api.main import app; print('✅ FastAPI: Working')"

# Test Prophet forecasting
cd /workspaces/unicorninvesting/BackendPython/unicorn/algorithms
python -c "from prophet import Prophet; print('✅ Prophet: Working')"

# Test database connection
python -c "import pymysql; print('✅ Database drivers: Available')"
```

---

## Production Deployment

### 1. Environment Variables

Create `/workspaces/unicorninvesting/.env`:

```bash
# Production Environment Configuration
ENVIRONMENT=production
DEBUG=false

# Database
DATABASE_URL=mysql://unicorn_user:PASSWORD@localhost/unicorn_analytics

# API Configuration
API_HOST=0.0.0.0
API_PORT=8000
API_WORKERS=4

# Security
SECRET_KEY=your-secret-key-here
ALLOWED_HOSTS=unicorninvesting.com,www.unicorninvesting.com

# External APIs
ALPHA_VANTAGE_API_KEY=your-key-here
IEX_CLOUD_API_KEY=your-key-here
```

### 2. Systemd Service Setup

Create `/etc/systemd/system/unicorn-api.service`:

```ini
[Unit]
Description=Unicorn Investing API
After=network.target mysql.service

[Service]
Type=exec
User=www-data
Group=www-data
WorkingDirectory=/workspaces/unicorninvesting/BackendPython/unicorn/backend
Environment=PATH=/workspaces/unicorninvesting/.venv/bin
ExecStart=/workspaces/unicorninvesting/.venv/bin/uvicorn api.main:app --host 0.0.0.0 --port 8000 --workers 4
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
```

Enable and start the service:

```bash
sudo systemctl enable unicorn-api.service
sudo systemctl start unicorn-api.service
sudo systemctl status unicorn-api.service
```

### 3. Log Configuration

Create `/etc/logrotate.d/unicorn-investing`:

```
/var/log/unicorn/*.log {
    daily
    missingok
    rotate 52
    compress
    delaycompress
    notifempty
    create 644 www-data www-data
    postrotate
        systemctl reload unicorn-api.service > /dev/null 2>&1 || true
    endscript
}
```

---

## Troubleshooting

### Common Issues and Solutions

#### 1. Python Package Installation Failures

```bash
# If Prophet installation fails
sudo apt install -y python3-dev build-essential
pip install --upgrade setuptools wheel
pip install prophet --no-cache-dir

# For TensorFlow GPU issues
pip install tensorflow[and-cuda]
```

#### 2. Database Connection Issues

```bash
# Check MySQL service
sudo systemctl status mysql

# Check user permissions
sudo mysql -u root -p
SHOW GRANTS FOR 'unicorn_user'@'localhost';

# Reset password if needed
ALTER USER 'unicorn_user'@'localhost' IDENTIFIED BY 'NEW_PASSWORD';
FLUSH PRIVILEGES;
```

#### 3. Apache Permission Issues

```bash
# Fix ownership
sudo chown -R www-data:www-data /var/www/

# Fix permissions
sudo chmod -R 755 /var/www/
sudo chmod -R 644 /var/www/*/web/sites/*/files
```

#### 4. SSL Certificate Issues

```bash
# Renew certificates manually
sudo certbot renew --dry-run

# Check certificate status
sudo certbot certificates

# Force renewal
sudo certbot renew --force-renewal
```

### Log Locations

- **Apache Logs**: `/var/log/apache2/`
- **MySQL Logs**: `/var/log/mysql/`
- **Unicorn API Logs**: `/var/log/unicorn/`
- **System Logs**: `journalctl -u unicorn-api.service`

---

## Backup and Maintenance

### 1. Database Backup Script

Create `/workspaces/unicorninvesting/scripts/backup_database.sh`:

```bash
#!/bin/bash

BACKUP_DIR="/var/backups/unicorn"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p $BACKUP_DIR

# Backup all databases
mysqldump --single-transaction --routines --triggers unicorn_analytics > $BACKUP_DIR/analytics_$DATE.sql
mysqldump --single-transaction --routines --triggers unicorninvesting_drupal > $BACKUP_DIR/drupal_main_$DATE.sql

# Compress backups
gzip $BACKUP_DIR/*_$DATE.sql

# Remove backups older than 30 days
find $BACKUP_DIR -name "*.sql.gz" -mtime +30 -delete

echo "Backup completed: $DATE"
```

### 2. System Update Script

Create `/workspaces/unicorninvesting/scripts/system_update.sh`:

```bash
#!/bin/bash

echo "🦄 Unicorn Platform Update"

# Update system packages
sudo apt update && sudo apt upgrade -y

# Update Python packages
source /workspaces/unicorninvesting/.venv/bin/activate
pip install --upgrade pip
pip install --upgrade -r /workspaces/unicorninvesting/BackendPython/requirements.txt

# Update Composer packages
cd /var/www/unicorninvesting.com
sudo -u www-data composer update

# Restart services
sudo systemctl restart unicorn-api.service
sudo systemctl restart apache2

echo "✅ Update completed!"
```

---

## Next Steps

After completing this installation:

1. **Configure API Keys**: Add your market data API keys to the environment
2. **Import Historical Data**: Load historical market data for backtesting
3. **Configure Trading Parameters**: Set up your trading strategies and risk parameters
4. **Test Algorithms**: Run backtests on your trading algorithms
5. **Monitor Performance**: Set up monitoring and alerting
6. **Deploy to Production**: Move from development to live trading

## Support

For additional support:
- Check the [README.md](README.md) for architecture details
- Review component-specific README files in each directory
- Check logs in `/var/log/` for error details
- Review the troubleshooting section above

---

*This installation guide ensures a complete, production-ready deployment of the Unicorn Investing Platform.*
