#!/usr/bin/env python3
"""
Centralized Secrets Management for Unicorn Investing Platform

This module provides secure access to API keys, database credentials,
and other sensitive configuration data stored in config/secrets.json.

Usage:
    from scripts.secrets_manager import SecretsManager
    
    secrets = SecretsManager()
    fred_key = secrets.get_api_key('fred_api_key')
    db_config = secrets.get_database_config('development')
"""

import json
import os
from pathlib import Path
from typing import Dict, Any, Optional

class SecretsManager:
    """Centralized secrets management for the Unicorn platform."""
    
    def __init__(self, secrets_path: Optional[str] = None):
        """Initialize secrets manager.
        
        Args:
            secrets_path: Path to secrets.json file. If None, uses default location.
        """
        if secrets_path is None:
            # Default to config/secrets.json relative to project root
            project_root = Path(__file__).parent.parent
            secrets_path = project_root / "config" / "secrets.json"
        
        self.secrets_path = Path(secrets_path)
        self._secrets = None
        self._load_secrets()
    
    def _load_secrets(self):
        """Load secrets from the JSON file."""
        if not self.secrets_path.exists():
            raise FileNotFoundError(
                f"Secrets file not found: {self.secrets_path}\n"
                f"Please copy config/secrets.json.template to config/secrets.json "
                f"and fill in your actual credentials."
            )
        
        try:
            with open(self.secrets_path, 'r') as f:
                self._secrets = json.load(f)
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in secrets file {self.secrets_path}: {e}")
    
    def get_api_key(self, key_name: str) -> str:
        """Get an API key by name.
        
        Args:
            key_name: Name of the API key (e.g., 'fred_api_key', 'bea_api_key')
            
        Returns:
            The API key value
            
        Raises:
            KeyError: If the API key is not found
        """
        if 'api_keys' not in self._secrets:
            raise KeyError("No 'api_keys' section found in secrets file")
        
        if key_name not in self._secrets['api_keys']:
            raise KeyError(f"API key '{key_name}' not found in secrets file")
        
        key_value = self._secrets['api_keys'][key_name]
        
        # Simple validation - just check if it's not empty
        if not key_value or key_value.strip() == "":
            raise ValueError(
                f"API key '{key_name}' is empty or not configured.\n"
                f"Please ensure your config/secrets.json file contains valid credentials."
            )
        
        return key_value
    
    def get_database_config(self, environment: str = 'development') -> Dict[str, Any]:
        """Get database configuration for a specific environment.
        
        Args:
            environment: Environment name ('development', 'production', 'testing')
            
        Returns:
            Database configuration dictionary
        """
        if 'database' not in self._secrets or 'mysql' not in self._secrets['database']:
            raise KeyError("No database configuration found in secrets file")
        
        if environment not in self._secrets['database']['mysql']:
            available = list(self._secrets['database']['mysql'].keys())
            raise KeyError(
                f"Database environment '{environment}' not found. "
                f"Available: {available}"
            )
        
        return self._secrets['database']['mysql'][environment]
    
    def get_ibkr_config(self) -> Dict[str, Any]:
        """Get IBKR configuration.
        
        Returns:
            IBKR configuration dictionary
        """
        if 'ibkr' not in self._secrets:
            raise KeyError("No IBKR configuration found in secrets file")
        
        return self._secrets['ibkr']
    
    def get_application_config(self) -> Dict[str, Any]:
        """Get application configuration (secret keys, etc.).
        
        Returns:
            Application configuration dictionary
        """
        if 'application' not in self._secrets:
            raise KeyError("No application configuration found in secrets file")
        
        return self._secrets['application']
    
    def get_mysql_connection_string(self, environment: str = 'development') -> str:
        """Get MySQL connection string for SQLAlchemy.
        
        Args:
            environment: Environment name
            
        Returns:
            SQLAlchemy connection string
        """
        db_config = self.get_database_config(environment)
        
        return (
            f"mysql+pymysql://{db_config['username']}:{db_config['password']}"
            f"@{db_config['host']}:{db_config['port']}/{db_config['database']}"
        )
    
    def validate_all_secrets(self) -> Dict[str, bool]:
        """Validate that all secrets are properly configured.
        
        Returns:
            Dictionary mapping secret names to validation status
        """
        validation_results = {}
        
        # Check API keys
        api_keys_to_check = ['fred_api_key', 'bea_api_key']
        for key_name in api_keys_to_check:
            try:
                self.get_api_key(key_name)
                validation_results[f'api_keys.{key_name}'] = True
            except (KeyError, ValueError):
                validation_results[f'api_keys.{key_name}'] = False
        
        # Check database configurations
        environments = ['development', 'production', 'testing']
        for env in environments:
            try:
                self.get_database_config(env)
                validation_results[f'database.mysql.{env}'] = True
            except KeyError:
                validation_results[f'database.mysql.{env}'] = False
        
        # Check IBKR config
        try:
            self.get_ibkr_config()
            validation_results['ibkr'] = True
        except KeyError:
            validation_results['ibkr'] = False
        
        return validation_results


# Convenience functions for backwards compatibility
def get_fred_api_key() -> str:
    """Get FRED API key."""
    return SecretsManager().get_api_key('fred_api_key')

def get_bea_api_key() -> str:
    """Get BEA API key.""" 
    return SecretsManager().get_api_key('bea_api_key')

def get_database_config(environment: str = 'development') -> Dict[str, Any]:
    """Get database configuration."""
    return SecretsManager().get_database_config(environment)

def get_ibkr_config() -> Dict[str, Any]:
    """Get IBKR configuration."""
    return SecretsManager().get_ibkr_config()


if __name__ == "__main__":
    """Test the secrets manager."""
    print("🔐 Testing Secrets Manager")
    print("=" * 40)
    
    try:
        secrets = SecretsManager()
        
        # Test API keys
        print("📊 API Keys:")
        try:
            fred_key = secrets.get_api_key('fred_api_key')
            print(f"  FRED: {fred_key[:10]}..." if fred_key else "  FRED: Not set")
        except Exception as e:
            print(f"  FRED: ERROR - {e}")
        
        try:
            bea_key = secrets.get_api_key('bea_api_key')
            print(f"  BEA: {bea_key[:10]}..." if bea_key else "  BEA: Not set")
        except Exception as e:
            print(f"  BEA: ERROR - {e}")
        
        # Test database config
        print("\n🗄️ Database Configuration:")
        try:
            db_config = secrets.get_database_config('development')
            print(f"  Host: {db_config['host']}")
            print(f"  Database: {db_config['database']}")
            print(f"  Username: {db_config['username']}")
        except Exception as e:
            print(f"  ERROR - {e}")
        
        # Test IBKR config
        print("\n📈 IBKR Configuration:")
        try:
            ibkr_config = secrets.get_ibkr_config()
            print(f"  Account: {ibkr_config['account_id']}")
            print(f"  Trading Mode: {ibkr_config['trading_mode']}")
        except Exception as e:
            print(f"  ERROR - {e}")
        
        # Validation summary
        print("\n✅ Validation Summary:")
        validation_results = secrets.validate_all_secrets()
        for key, is_valid in validation_results.items():
            status = "✅" if is_valid else "❌"
            print(f"  {status} {key}")
            
    except Exception as e:
        print(f"❌ Failed to initialize secrets manager: {e}")
