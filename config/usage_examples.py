"""
Example: Using the Configuration Manager in Unicorn Investing Platform
Shows how to replace hardcoded credentials with secure configuration management.
"""
import sys
from pathlib import Path

# Add config directory to Python path
sys.path.insert(0, str(Path(__file__).parent))

from config_manager import get_api_key, get_database_config, get_database_url, get_ibkr_config

def example_economic_data_connector():
    """Example: Economic data connector using secure API keys."""
    
    try:
        # Get API keys securely
        fred_api_key = get_api_key('fred')
        bea_api_key = get_api_key('bea')
        
        print("🔑 Economic Data API Configuration:")
        print(f"   FRED API Key: {fred_api_key[:8]}...***")
        print(f"   BEA API Key: {bea_api_key[:8]}...***")
        
        # Your economic data fetching code here...
        # fred_client = FredAPI(api_key=fred_api_key)
        # bea_client = BeaAPI(api_key=bea_api_key)
        
    except (KeyError, ValueError) as e:
        print(f"❌ Configuration Error: {e}")
        print("📝 Please configure your API keys in config/secrets.json")

def example_database_connection():
    """Example: Database connection using secure credentials."""
    
    try:
        # Get database configuration for development environment
        db_config = get_database_config('development')
        db_url = get_database_url('development')
        
        print("🗄️  Database Configuration:")
        print(f"   Host: {db_config['host']}:{db_config['port']}")
        print(f"   Database: {db_config['database']}")
        print(f"   Username: {db_config['username']}")
        print(f"   SQLAlchemy URL: {db_url.split('@')[0]}@***")
        
        # Your database connection code here...
        # from sqlalchemy import create_engine
        # engine = create_engine(db_url)
        
    except ValueError as e:
        print(f"❌ Database Configuration Error: {e}")
        print("📝 Please configure your database credentials in config/secrets.json")

def example_ibkr_connection():
    """Example: IBKR connection using secure credentials."""
    
    try:
        # Get IBKR configuration
        ibkr_config = get_ibkr_config()
        
        print("📈 IBKR Configuration:")
        print(f"   Account ID: {ibkr_config['account_id']}")
        print(f"   Trading Mode: {ibkr_config['trading_mode']}")
        print(f"   Base URL: {ibkr_config['base_url']}")
        
        # Your IBKR connection code here...
        # ibkr_client = IBKRClient(
        #     account_id=ibkr_config['account_id'],
        #     username=ibkr_config['username']
        # )
        
    except ValueError as e:
        print(f"❌ IBKR Configuration Error: {e}")
        print("📝 Please configure your IBKR credentials in config/secrets.json")

if __name__ == "__main__":
    print("🔧 Unicorn Investing Platform - Configuration Manager Examples")
    print("=" * 60)
    
    example_economic_data_connector()
    print()
    
    example_database_connection()
    print()
    
    example_ibkr_connection()
    print()
    
    print("✨ All examples completed!")
    print("💡 Use 'from config.config_manager import get_api_key' in your own modules.")
