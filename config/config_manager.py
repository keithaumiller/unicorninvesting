"""
Configuration Manager for Unicorn Investing Platform
Provides secure access to API keys, database credentials, and other sensitive configuration.
"""
import json
import os
from pathlib import Path
from typing import Dict, Any, Optional

class ConfigManager:
    """Centralized configuration management for sensitive data."""
    
    def __init__(self, config_dir: str = None):
        """Initialize configuration manager.
        
        Args:
            config_dir: Path to config directory. Defaults to project config/ directory.
        """
        if config_dir is None:
            # Default to config directory relative to this file's location
            self.config_dir = Path(__file__).parent.parent / "config"
        else:
            self.config_dir = Path(config_dir)
            
        self.secrets_file = self.config_dir / "secrets.json"
        self._config_cache = None
    
    def load_config(self) -> Dict[str, Any]:
        """Load configuration from secrets.json file."""
        if not self.secrets_file.exists():
            raise FileNotFoundError(
                f"Configuration file not found: {self.secrets_file}\n"
                f"Please copy secrets.json.template to secrets.json and configure your values."
            )
        
        if self._config_cache is None:
            with open(self.secrets_file, 'r') as f:
                self._config_cache = json.load(f)
        
        return self._config_cache
    
    def get_api_key(self, service: str) -> str:
        """Get API key for specified service.
        
        Args:
            service: Service name ('fred', 'bea', 'alpha_vantage')
            
        Returns:
            API key string
            
        Raises:
            KeyError: If service not found
        """
        config = self.load_config()
        service_key = f"{service}_api_key"
        
        if service_key not in config['api_keys']:
            raise KeyError(f"API key not found for service: {service}")
        
        api_key = config['api_keys'][service_key]
        
        # Check if still using template placeholder
        if api_key.startswith("YOUR_"):
            raise ValueError(f"Please configure {service_key} in {self.secrets_file}")
        
        return api_key
    
    def get_database_config(self, environment: str = 'development') -> Dict[str, Any]:
        """Get database configuration for specified environment.
        
        Args:
            environment: Environment name ('development', 'production', 'testing')
            
        Returns:
            Database configuration dictionary
        """
        config = self.load_config()
        
        if environment not in config['database']['mysql']:
            raise KeyError(f"Database environment not found: {environment}")
        
        db_config = config['database']['mysql'][environment].copy()
        
        # Check if still using template placeholders
        if db_config.get('username', '').startswith("YOUR_"):
            raise ValueError(f"Please configure database credentials in {self.secrets_file}")
        
        return db_config
    
    def get_database_url(self, environment: str = 'development') -> str:
        """Get SQLAlchemy database URL for specified environment.
        
        Args:
            environment: Environment name
            
        Returns:
            SQLAlchemy database URL
        """
        db_config = self.get_database_config(environment)
        
        return (f"mysql+pymysql://{db_config['username']}:{db_config['password']}"
                f"@{db_config['host']}:{db_config['port']}/{db_config['database']}")
    
    def get_ibkr_config(self) -> Dict[str, Any]:
        """Get Interactive Brokers configuration.
        
        Returns:
            IBKR configuration dictionary
        """
        config = self.load_config()
        ibkr_config = config['ibkr'].copy()
        
        # Check if still using template placeholders
        if ibkr_config.get('account_id', '').startswith("YOUR_"):
            raise ValueError(f"Please configure IBKR credentials in {self.secrets_file}")
        
        return ibkr_config
    
    def get_app_secret(self, key: str) -> str:
        """Get application secret key.
        
        Args:
            key: Secret key name ('secret_key', 'jwt_secret', 'encryption_key')
            
        Returns:
            Secret key value
        """
        config = self.load_config()
        
        if key not in config['application']:
            raise KeyError(f"Application secret not found: {key}")
        
        secret = config['application'][key]
        
        if secret.startswith("YOUR_"):
            raise ValueError(f"Please configure {key} in {self.secrets_file}")
        
        return secret
    
    def get_environment_variables(self) -> Dict[str, str]:
        """Get environment variables for all configured services.
        
        Returns:
            Dictionary of environment variables
        """
        config = self.load_config()
        env_vars = {}
        
        # API Keys
        for service, key in config['api_keys'].items():
            if not key.startswith("YOUR_"):
                env_vars[service.upper()] = key
        
        # Database URL (for development environment)
        try:
            env_vars['DATABASE_URL'] = self.get_database_url('development')
        except ValueError:
            pass  # Skip if not configured
        
        return env_vars

# Global configuration manager instance
config_manager = ConfigManager()

# Convenience functions for easy access
def get_api_key(service: str) -> str:
    """Get API key for specified service."""
    return config_manager.get_api_key(service)

def get_database_config(environment: str = 'development') -> Dict[str, Any]:
    """Get database configuration."""
    return config_manager.get_database_config(environment)

def get_database_url(environment: str = 'development') -> str:
    """Get database URL."""
    return config_manager.get_database_url(environment)

def get_ibkr_config() -> Dict[str, Any]:
    """Get IBKR configuration."""
    return config_manager.get_ibkr_config()

def get_app_secret(key: str) -> str:
    """Get application secret."""
    return config_manager.get_app_secret(key)

if __name__ == "__main__":
    # Test configuration access
    try:
        print("🔧 Testing configuration access...")
        
        # Test API keys
        try:
            fred_key = get_api_key('fred')
            print(f"✅ FRED API key: {fred_key[:8]}...")
        except (KeyError, ValueError) as e:
            print(f"❌ FRED API key: {e}")
        
        # Test database config
        try:
            db_config = get_database_config()
            print(f"✅ Database config: {db_config['host']}:{db_config['port']}/{db_config['database']}")
        except ValueError as e:
            print(f"❌ Database config: {e}")
        
        # Test IBKR config
        try:
            ibkr_config = get_ibkr_config()
            print(f"✅ IBKR config: {ibkr_config['account_id']} ({ibkr_config['trading_mode']})")
        except ValueError as e:
            print(f"❌ IBKR config: {e}")
        
        print("🔧 Configuration test complete.")
        
    except Exception as e:
        print(f"❌ Configuration error: {e}")
