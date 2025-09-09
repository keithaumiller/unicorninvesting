# Configuration Directory - Security Guide

This directory contains the centralized configuration management system for the Unicorn Investing Platform. All sensitive data including API keys, database credentials, and application secrets are managed through this system.

## 🔒 Security Overview

**IMPORTANT**: This directory contains both secure configuration files and templates. Only template files should be committed to version control.

## Files

### 📁 **Secure Files** (NOT in version control)
- `secrets.json` - **Contains actual sensitive data** - Never commit this file
- `*.key`, `*.pem` - Certificate and key files
- `*_credentials.*` - Any credential files

### 📋 **Template Files** (Safe to commit)
- `secrets.json.template` - Template for secrets configuration
- `config_manager.py` - Configuration management library
- `usage_examples.py` - Examples of how to use the config manager
- `database.json` - **Deprecated** - Migration notice only

## 🚀 Quick Start

### 1. Initial Setup

```bash
# Copy template to create your configuration
cp secrets.json.template secrets.json

# Edit with your actual credentials
nano secrets.json
```

### 2. Configure Required Values

**Replace ALL placeholder values in `secrets.json`:**

```json
{
  "api_keys": {
    "fred_api_key": "YOUR_ACTUAL_FRED_API_KEY",
    "bea_api_key": "YOUR_ACTUAL_BEA_API_KEY",
    "alpha_vantage_api_key": "YOUR_ACTUAL_ALPHA_VANTAGE_API_KEY"
  },
  "database": {
    "mysql": {
      "development": {
        "username": "your_secure_username",
        "password": "your_secure_password_123!"
      }
    }
  }
}
```

### 3. Test Configuration

```bash
# Run the usage examples
python3 usage_examples.py

# Test specific components
python3 config_manager.py
```

## 💻 Using Configuration in Your Code

### Basic Usage

```python
from config.config_manager import get_api_key, get_database_config

# Get API keys
fred_key = get_api_key('fred')
bea_key = get_api_key('bea')

# Get database configuration
db_config = get_database_config('development')
db_url = get_database_url('development')
```

### Advanced Usage

```python
from config.config_manager import ConfigManager

# Create custom config manager
config = ConfigManager('/custom/config/path')

# Get full configuration
full_config = config.load_config()

# Get environment variables for shell scripts
env_vars = config.get_environment_variables()
```

## 🛡️ Security Best Practices

### 1. **Never Commit Sensitive Data**
- `secrets.json` is automatically ignored by `.gitignore`
- Always use template files for examples
- Use placeholders like `YOUR_API_KEY` in templates

### 2. **Use Strong Passwords**
```bash
# Generate secure random keys
python3 -c "import secrets; print(secrets.token_urlsafe(32))"
```

### 3. **Environment-Specific Configuration**
- Use different credentials for development, testing, and production
- Keep production credentials separate and more secure
- Regularly rotate API keys and passwords

### 4. **Validate Configuration**
- Always test configuration after changes
- Use the provided examples to verify connectivity
- Monitor for configuration errors in application logs

## 🚨 Security Checklist

Before deploying or sharing code:

- [ ] `secrets.json` contains no placeholder values (YOUR_*)
- [ ] All API keys are valid and functional
- [ ] Database credentials match your MySQL setup
- [ ] Application secrets are unique random strings
- [ ] `secrets.json` is properly ignored by git
- [ ] No hardcoded credentials remain in Python files

## 🔧 Migration from Legacy Configuration

If you're migrating from the old `database.json` system:

### Before (Legacy)
```python
import json
with open('config/database.json') as f:
    config = json.load(f)
```

### After (Secure)
```python
from config.config_manager import get_database_config
config = get_database_config('development')
```

## 📚 API Key Resources

**Get your API keys from these sources:**

- **FRED (Federal Reserve Economic Data)**: https://fred.stlouisfed.org/docs/api/api_key.html
  - Free, no rate limits for most usage
  - Required for economic indicators processing

- **BEA (Bureau of Economic Analysis)**: https://www.bea.gov/API/signup/index.cfm
  - Free government data API
  - Required for comprehensive economic analysis

- **Alpha Vantage**: https://www.alphavantage.co/support/#api-key
  - Free tier: 5 API requests per minute, 500 per day
  - Premium plans available for higher limits

## 🆘 Troubleshooting

### Common Errors

#### `FileNotFoundError: Configuration file not found`
```bash
# Solution: Copy the template
cp secrets.json.template secrets.json
# Then edit with your credentials
```

#### `ValueError: Please configure fred_api_key`
```bash
# Solution: Replace placeholder values in secrets.json
# Change "YOUR_FRED_API_KEY" to your actual API key
```

#### `KeyError: API key not found for service`
```bash
# Solution: Check the service name in your code
# Valid services: 'fred', 'bea', 'alpha_vantage'
```

### Validation Commands

```bash
# Test all configuration
python3 config/usage_examples.py

# Test specific API key
python3 -c "from config.config_manager import get_api_key; print(get_api_key('fred'))"

# Test database connection
python3 -c "from config.config_manager import get_database_url; print(get_database_url())"
```

## 📝 Support

If you need help with configuration:

1. Check that `secrets.json` exists and contains valid JSON
2. Verify all placeholder values (`YOUR_*`) have been replaced
3. Test individual components using the usage examples
4. Check application logs for specific error messages

The configuration system provides detailed error messages to help identify and resolve issues quickly.
