# 🔐 Security & Credentials Management

## Overview

The Unicorn Investing Platform uses a centralized, secure credentials management system to protect API keys, database passwords, and other sensitive information.

## 🚨 Important Security Notice

**NEVER commit actual credentials to version control!**

- ✅ Use the `scripts/setup_credentials.py` script to configure your credentials
- ✅ The `config/secrets.json` file is gitignored and will not be committed
- ✅ All scripts use the centralized `SecretsManager` for credential access
- ❌ Never hardcode API keys or passwords directly in your code

## Quick Setup

### 1. Initial Credentials Setup

```bash
# Run the interactive setup script
python3 scripts/setup_credentials.py
```

This script will:
- Prompt you for your actual API keys and credentials
- Generate secure application keys
- Save everything to `config/secrets.json` with proper permissions
- Verify the setup works correctly

### 2. Verify Setup

```bash
# Test the secrets manager
python3 scripts/secrets_manager.py

# Run comprehensive integration examples
python3 config/usage_examples_new.py
```

## Required Credentials

### API Keys (Free Registration Required)

1. **FRED API Key**
   - Service: Federal Reserve Economic Data
   - URL: https://fred.stlouisfed.org/docs/api/api_key.html
   - Purpose: Economic indicators and macroeconomic data

2. **BEA API Key**
   - Service: Bureau of Economic Analysis
   - URL: https://apps.bea.gov/API/signup/
   - Purpose: Economic analysis and GDP data

3. **Alpha Vantage API Key** (Optional)
   - Service: Alpha Vantage
   - URL: https://www.alphavantage.co/support/#api-key
   - Purpose: Additional financial data

### Database Credentials

- **MySQL Password**: For local development and production databases
- **IBKR Credentials**: Interactive Brokers paper trading account (for testing)

## Using Credentials in Your Code

### Basic Usage

```python
from scripts.secrets_manager import SecretsManager

# Initialize secrets manager
secrets = SecretsManager()

# Get API keys
fred_api_key = secrets.get_api_key('fred_api_key')
bea_api_key = secrets.get_api_key('bea_api_key')

# Get database configuration
db_config = secrets.get_database_config('development')
connection_string = secrets.get_mysql_connection_string('development')

# Get IBKR configuration
ibkr_config = secrets.get_ibkr_config()
account_id = ibkr_config['account_id']
```

### Using with Connectors

```python
# FRED Connector automatically uses secrets manager
from BackendPython.unicorn.1_data_sources.1_raw.connectors.federal_reserve_fred.fred_connector import FredConnector
fred = FredConnector()  # Automatically loads API key

# BEA Connector automatically uses secrets manager  
from BackendPython.unicorn.1_data_sources.1_raw.connectors.bureau_of_economic_analysis.bea_connector import BEAConnector
bea = BEAConnector()  # Automatically loads API key
```

## File Structure

```
config/
├── secrets.json           # Your actual credentials (gitignored)
├── secrets.json.template  # Template with placeholders
└── usage_examples_new.py  # Comprehensive usage examples

scripts/
├── secrets_manager.py     # Centralized secrets management
├── setup_credentials.py   # Interactive setup script
└── migration_helper.py    # Security audit and migration
```

## Security Features

### 1. File Protection
- `config/secrets.json` is automatically added to `.gitignore`
- File permissions set to 600 (user read/write only)
- Template approach prevents accidental credential commits

### 2. Validation
- Automatic detection of placeholder values
- Clear error messages with setup instructions
- Comprehensive validation of all credential categories

### 3. Migration Support
- `migration_helper.py` scans for hardcoded credentials
- Provides specific migration instructions for each issue
- Tracks progress and suggests next steps

## Troubleshooting

### Error: "API key contains placeholder value"

**Solution**: Run the setup script to configure your actual credentials:
```bash
python3 scripts/setup_credentials.py
```

### Error: "Secrets file not found"

**Solution**: The `config/secrets.json` file doesn't exist. Run setup:
```bash
python3 scripts/setup_credentials.py
```

### Error: "Invalid JSON in secrets file"

**Solution**: The JSON file is corrupted. Restore from template:
```bash
cp config/secrets.json.template config/secrets.json
python3 scripts/setup_credentials.py
```

### Permissions Issues

**Solution**: Fix file permissions:
```bash
chmod 600 config/secrets.json
```

## Best Practices

### ✅ Do This
- Use the `SecretsManager` class for all credential access
- Run `setup_credentials.py` on new environments
- Keep credentials in the centralized `config/secrets.json`
- Use environment-specific database configurations
- Regularly audit with `migration_helper.py`

### ❌ Don't Do This
- Never hardcode credentials directly in Python files
- Don't commit `config/secrets.json` to version control
- Don't share credentials via email or chat
- Don't use production credentials in development environments
- Don't store credentials in environment variables for this project

## Migration from Legacy Code

If you have existing code with hardcoded credentials:

1. **Scan for issues**:
   ```bash
   python3 scripts/migration_helper.py BackendPython/unicorn
   ```

2. **Follow the migration suggestions** provided by the tool

3. **Update imports**:
   ```python
   # Old way
   API_KEY = "hardcoded_key"
   
   # New way
   from scripts.secrets_manager import SecretsManager
   secrets = SecretsManager()
   API_KEY = secrets.get_api_key('service_name')
   ```

4. **Test the changes**:
   ```bash
   python3 scripts/secrets_manager.py
   python3 config/usage_examples_new.py
   ```

## Support

If you encounter issues with the credentials management system:

1. Check this README for troubleshooting steps
2. Run the validation examples: `python3 config/usage_examples_new.py`
3. Check the migration helper: `python3 scripts/migration_helper.py .`
4. Verify your `config/secrets.json` file exists and has valid JSON

---

**Security First**: This system prioritizes security and ease of use. Always use the provided tools and never compromise on credential security.
