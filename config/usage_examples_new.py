#!/usr/bin/env python3
"""
Secrets Manager Usage Examples

This script demonstrates how to use the centralized secrets manager
for accessing API keys, database configurations, and other sensitive data.
"""

import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    from scripts.secrets_manager import SecretsManager
except ImportError as e:
    print(f"❌ Could not import SecretsManager: {e}")
    print("Make sure you're running this from the project root directory.")
    sys.exit(1)

def demonstrate_api_keys():
    """Demonstrate API key usage."""
    print("🔑 API Keys Examples")
    print("-" * 30)
    
    secrets = SecretsManager()
    
    try:
        # FRED API Key
        fred_key = secrets.get_api_key('fred_api_key')
        print(f"✅ FRED API Key: {fred_key[:10]}...{fred_key[-4:]}")
        
        # BEA API Key
        bea_key = secrets.get_api_key('bea_api_key')
        print(f"✅ BEA API Key: {bea_key[:10]}...{bea_key[-4:]}")
        
    except Exception as e:
        print(f"❌ Error accessing API keys: {e}")

def demonstrate_database_config():
    """Demonstrate database configuration usage."""
    print("\n🗄️ Database Configuration Examples")
    print("-" * 40)
    
    secrets = SecretsManager()
    
    try:
        # Get database config for different environments
        for env in ['development', 'production', 'testing']:
            print(f"\n📊 {env.title()} Environment:")
            db_config = secrets.get_database_config(env)
            print(f"   Host: {db_config['host']}")
            print(f"   Database: {db_config['database']}")
            print(f"   Username: {db_config['username']}")
            print(f"   Port: {db_config['port']}")
            
            # Show connection string
            connection_string = secrets.get_mysql_connection_string(env)
            # Mask password in output
            masked_string = connection_string.replace(db_config['password'], '***')
            print(f"   Connection String: {masked_string}")
            
    except Exception as e:
        print(f"❌ Error accessing database config: {e}")

def demonstrate_ibkr_config():
    """Demonstrate IBKR configuration usage."""
    print("\n📈 IBKR Configuration Examples")
    print("-" * 35)
    
    secrets = SecretsManager()
    
    try:
        ibkr_config = secrets.get_ibkr_config()
        print(f"✅ Account ID: {ibkr_config['account_id']}")
        print(f"✅ Username: {ibkr_config['username']}")
        print(f"✅ Trading Mode: {ibkr_config['trading_mode']}")
        print(f"✅ API Type: {ibkr_config['api_type']}")
        print(f"✅ Base URL: {ibkr_config['base_url']}")
        
    except Exception as e:
        print(f"❌ Error accessing IBKR config: {e}")

def demonstrate_validation():
    """Demonstrate secrets validation."""
    print("\n✅ Validation Examples")
    print("-" * 25)
    
    secrets = SecretsManager()
    
    try:
        validation_results = secrets.validate_all_secrets()
        print("Validation Results:")
        for key, is_valid in validation_results.items():
            status = "✅" if is_valid else "❌"
            print(f"  {status} {key}")
            
    except Exception as e:
        print(f"❌ Error during validation: {e}")

def demonstrate_integration_examples():
    """Show practical integration examples."""
    print("\n🔧 Integration Examples")
    print("-" * 30)
    
    print("""
# Example 1: Using FRED API in your code
from scripts.secrets_manager import SecretsManager

secrets = SecretsManager()
fred_api_key = secrets.get_api_key('fred_api_key')

# Use with requests
import requests
response = requests.get(
    f'https://api.stlouisfed.org/fred/series/observations',
    params={
        'series_id': 'GDP',
        'api_key': fred_api_key,
        'file_type': 'json'
    }
)

# Example 2: Database connection with SQLAlchemy
from sqlalchemy import create_engine
from scripts.secrets_manager import SecretsManager

secrets = SecretsManager()
connection_string = secrets.get_mysql_connection_string('development')
engine = create_engine(connection_string)

# Example 3: Using IBKR configuration
from scripts.secrets_manager import SecretsManager

secrets = SecretsManager()
ibkr_config = secrets.get_ibkr_config()

class IBKRClient:
    def __init__(self):
        config = secrets.get_ibkr_config()
        self.account_id = config['account_id']
        self.base_url = config['base_url']
        self.trading_mode = config['trading_mode']
""")

def main():
    """Run all demonstrations."""
    print("🦄 Unicorn Investing Platform - Secrets Manager Examples")
    print("=" * 60)
    
    try:
        demonstrate_api_keys()
        demonstrate_database_config()
        demonstrate_ibkr_config()
        demonstrate_validation()
        demonstrate_integration_examples()
        
        print("\n🎉 All examples completed successfully!")
        print("\n📋 Next Steps:")
        print("1. Update your existing code to use SecretsManager")
        print("2. Remove hardcoded credentials from your files")
        print("3. Test your integrations with: python3 config/usage_examples.py")
        print("4. Run security audit: python3 scripts/migration_helper.py")
        
    except Exception as e:
        print(f"\n❌ Error running examples: {e}")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
