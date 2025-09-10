#!/usr/bin/env python3
"""
Secure Credentials Setup Script - Unicorn Investing Platform

This script helps users securely set up their API keys and credentials
without exposing them in version control or configuration files.
"""

import os
import json
import getpass
from pathlib import Path
import secrets
import string

def generate_secure_key(length=32):
    """Generate a cryptographically secure random key."""
    alphabet = string.ascii_letters + string.digits + "!@#$%^&*"
    return ''.join(secrets.choice(alphabet) for _ in range(length))

def setup_credentials():
    """Interactive setup of credentials."""
    print("🔐 Unicorn Investing Platform - Secure Credentials Setup")
    print("=" * 60)
    print("This script will help you securely configure your API keys and credentials.")
    print("Your credentials will be stored in config/secrets.json (which is gitignored).")
    print()
    
    # Load template
    config_dir = Path(__file__).parent.parent / "config"
    template_path = config_dir / "secrets.json.template"
    secrets_path = config_dir / "secrets.json"
    
    if template_path.exists():
        with open(template_path, 'r') as f:
            config = json.load(f)
    else:
        print("❌ Error: secrets.json.template not found!")
        return False
    
    print("📋 API Keys Configuration")
    print("-" * 30)
    
    # FRED API Key
    print("🏦 FRED (Federal Reserve Economic Data) API Key")
    print("   Get your free API key from: https://fred.stlouisfed.org/docs/api/api_key.html")
    fred_key = getpass.getpass("   Enter your FRED API key (hidden): ").strip()
    if fred_key:
        config['api_keys']['fred_api_key'] = fred_key
    
    print()
    
    # BEA API Key  
    print("📊 BEA (Bureau of Economic Analysis) API Key")
    print("   Get your free API key from: https://apps.bea.gov/API/signup/")
    bea_key = getpass.getpass("   Enter your BEA API key (hidden): ").strip()
    if bea_key:
        config['api_keys']['bea_api_key'] = bea_key
    
    print()
    
    # Alpha Vantage API Key (optional)
    print("📈 Alpha Vantage API Key (Optional)")
    print("   Get your free API key from: https://www.alphavantage.co/support/#api-key")
    alpha_key = input("   Enter your Alpha Vantage API key (or press Enter to skip): ").strip()
    if alpha_key:
        config['api_keys']['alpha_vantage_api_key'] = alpha_key
    
    print()
    print("🗄️ Database Configuration")
    print("-" * 30)
    
    # Database password
    print("Database password for development and production environments:")
    db_password = getpass.getpass("   Enter MySQL password (hidden): ").strip()
    if db_password:
        config['database']['mysql']['development']['password'] = db_password
        config['database']['mysql']['production']['password'] = db_password
    
    print()
    print("📈 IBKR Configuration")
    print("-" * 30)
    
    # IBKR Account
    print("Interactive Brokers Paper Trading Account (for testing):")
    ibkr_account = input("   Enter your IBKR account ID: ").strip()
    if ibkr_account:
        config['ibkr']['account_id'] = ibkr_account
        
    ibkr_username = input("   Enter your IBKR username: ").strip()
    if ibkr_username:
        config['ibkr']['username'] = ibkr_username
    
    print()
    print("🔐 Application Security Keys")
    print("-" * 30)
    print("Generating secure application keys...")
    
    # Generate secure keys
    config['application']['secret_key'] = generate_secure_key(64)
    config['application']['jwt_secret'] = generate_secure_key(64)
    config['application']['encryption_key'] = generate_secure_key(32)
    print("✅ Generated secure application keys")
    
    print()
    
    # Save configuration
    print("💾 Saving Configuration")
    print("-" * 30)
    
    try:
        # Ensure config directory exists
        config_dir.mkdir(exist_ok=True)
        
        # Save with proper formatting
        with open(secrets_path, 'w') as f:
            json.dump(config, f, indent=2, sort_keys=True)
        
        # Set restrictive permissions (user read/write only)
        os.chmod(secrets_path, 0o600)
        
        print(f"✅ Configuration saved to: {secrets_path}")
        print("✅ File permissions set to 600 (user read/write only)")
        
        # Verify gitignore protection
        gitignore_path = config_dir.parent / ".gitignore"
        if gitignore_path.exists():
            with open(gitignore_path, 'r') as f:
                gitignore_content = f.read()
                if 'config/secrets.json' in gitignore_content:
                    print("✅ File is protected by .gitignore")
                else:
                    print("⚠️ Warning: File may not be protected by .gitignore")
        
        return True
        
    except Exception as e:
        print(f"❌ Error saving configuration: {e}")
        return False

def verify_setup():
    """Verify the credentials setup."""
    print("\n🔍 Verifying Setup")
    print("-" * 20)
    
    try:
        from scripts.secrets_manager import SecretsManager
        secrets_mgr = SecretsManager()
        
        # Test API keys
        try:
            fred_key = secrets_mgr.get_api_key('fred_api_key')
            print(f"✅ FRED API Key: {fred_key[:10]}..." if len(fred_key) > 10 else "✅ FRED API Key: Set")
        except Exception as e:
            print(f"❌ FRED API Key: {e}")
        
        try:
            bea_key = secrets_mgr.get_api_key('bea_api_key')
            print(f"✅ BEA API Key: {bea_key[:10]}..." if len(bea_key) > 10 else "✅ BEA API Key: Set")
        except Exception as e:
            print(f"❌ BEA API Key: {e}")
            
        # Test database config
        try:
            db_config = secrets_mgr.get_database_config('development')
            print(f"✅ Database Config: {db_config['username']}@{db_config['host']}")
        except Exception as e:
            print(f"❌ Database Config: {e}")
            
        return True
        
    except Exception as e:
        print(f"❌ Verification failed: {e}")
        return False

def main():
    """Main setup flow."""
    if setup_credentials():
        if verify_setup():
            print("\n🎉 Setup Complete!")
            print("\n📋 Next Steps:")
            print("1. Test your setup: python3 scripts/secrets_manager.py")
            print("2. Run integration examples: python3 config/usage_examples_new.py") 
            print("3. Start using the platform with secure credentials!")
            print("\n🔒 Security Note:")
            print("- Your secrets.json file is gitignored and won't be committed")
            print("- File permissions are set to 600 (user read/write only)")
            print("- Never share or commit your actual API keys")
        else:
            print("\n⚠️ Setup completed but verification failed.")
            print("Please check your credentials and try running the verification manually.")
    else:
        print("\n❌ Setup failed. Please try again.")

if __name__ == "__main__":
    main()
