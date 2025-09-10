"""
LEGACY CONFIGURATION - DEPRECATED

This file is deprecated. Use scripts/secrets_manager.py for centralized secrets management.

Example of modern approach:
    from scripts.secrets_manager import SecretsManager
    
    secrets = SecretsManager()
    db_config = secrets.get_database_config('development')
    connection_string = secrets.get_mysql_connection_string('development')
"""
import os
from typing import Optional
from pydantic_settings import BaseSettings
from dotenv import load_dotenv

# Load environment variables
load_dotenv()


class DatabaseConfig(BaseSettings):
    """Database configuration settings"""
    
    host: str = os.getenv("MYSQL_HOST", "localhost")
    port: int = int(os.getenv("MYSQL_PORT", "3306"))
    user: str = os.getenv("MYSQL_USER", "unicorn_admin")
    password: str = os.getenv("MYSQL_PASSWORD", "")
    database: str = os.getenv("MYSQL_DATABASE", "unicorn_analytics")
    
    @property
    def url(self) -> str:
        """Database connection URL"""
        return f"mysql+pymysql://{self.user}:{self.password}@{self.host}:{self.port}/{self.database}"


class APIConfig(BaseSettings):
    """API configuration settings"""
    
    host: str = os.getenv("API_HOST", "0.0.0.0")
    port: int = int(os.getenv("API_PORT", "8000"))
    debug: bool = os.getenv("DEBUG", "False").lower() == "true"
    secret_key: str = os.getenv("SECRET_KEY", "your-secret-key-here")
    
    # External API keys
    alpha_vantage_key: Optional[str] = os.getenv("ALPHA_VANTAGE_API_KEY")
    news_api_key: Optional[str] = os.getenv("NEWS_API_KEY")


class LEANConfig(BaseSettings):
    """QuantConnect LEAN configuration"""
    
    lean_path: str = os.getenv("LEAN_PATH", "/workspaces/unicorninvesting/BackendPython/Lean")
    data_path: str = os.getenv("LEAN_DATA_PATH", "/workspaces/unicorninvesting/BackendPython/Lean/Data")
    algorithm_path: str = os.getenv("LEAN_ALGORITHM_PATH", "/workspaces/unicorninvesting/BackendPython/unicorn/algorithms")


class MLConfig(BaseSettings):
    """Machine learning configuration"""
    
    model_path: str = os.getenv("MODEL_PATH", "/workspaces/unicorninvesting/BackendPython/unicorn/models")
    feature_store_path: str = os.getenv("FEATURE_STORE_PATH", "/workspaces/unicorninvesting/BackendPython/unicorn/data")
    training_data_path: str = os.getenv("TRAINING_DATA_PATH", "/workspaces/unicorninvesting/BackendPython/unicorn/data/training")


class Config:
    """Main configuration class"""
    
    def __init__(self):
        self.database = DatabaseConfig()
        self.api = APIConfig()
        self.lean = LEANConfig()
        self.ml = MLConfig()


# Global configuration instance
config = Config()
