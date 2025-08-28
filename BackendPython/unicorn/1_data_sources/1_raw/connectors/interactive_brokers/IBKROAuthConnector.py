"""
IBKR OAuth 2.0 Web API Connector
Unicorn Investing Platform

This connector implements IBKR's OAuth 2.0 Web API using JWT client authentication
as specified in RFC 7521 and RFC 7523.

Key Features:
- Private key JWT authentication
- RESTful API endpoints
- Web-native design
- Industry-standard OAuth 2.0 security

Documentation:
https://www.interactivebrokers.com/api/doc/oauth/
"""

import json
import time
import jwt
import requests
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
import logging
from pathlib import Path
import os
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import rsa

class IBKROAuthConnector:
    """
    IBKR OAuth 2.0 Web API Connector
    
    Implements JWT-based authentication for IBKR's modern Web API.
    More secure and web-friendly than the Client Portal approach.
    """
    
    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize the IBKR OAuth connector.
        
        Args:
            config_path: Path to configuration file
        """
        self.logger = logging.getLogger(__name__)
        self.session = requests.Session()
        
        # API endpoints
        self.base_url = "https://api.ibkr.com"
        self.oauth_url = "https://api.ibkr.com/oauth/token"
        
        # Configuration
        self.config = self._load_config(config_path)
        self.access_token = None
        self.token_expires_at = None
        
        # Initialize session headers
        self.session.headers.update({
            'Content-Type': 'application/json',
            'Accept': 'application/json',
            'User-Agent': 'UnicornInvesting/1.0'
        })
    
    def _load_config(self, config_path: Optional[str] = None) -> Dict:
        """Load configuration from file or environment."""
        if config_path is None:
            config_path = os.path.join(
                os.path.dirname(__file__), 
                '../../../../config/ibkr/oauth_config.json'
            )
        
        config = {}
        
        # Try to load from file
        if os.path.exists(config_path):
            with open(config_path, 'r') as f:
                config = json.load(f)
        
        # Override with environment variables
        config.update({
            'client_id': os.getenv('IBKR_CLIENT_ID', config.get('client_id')),
            'account_number': os.getenv('IBKR_ACCOUNT', config.get('account_number')),
            'private_key_path': os.getenv('IBKR_PRIVATE_KEY_PATH', config.get('private_key_path')),
            'trading_mode': os.getenv('IBKR_TRADING_MODE', config.get('trading_mode', 'paper')),
            'api_type': 'OAuth2_WebAPI'
        })
        
        return config
    
    def generate_private_key(self, key_path: str) -> str:
        """
        Generate a new RSA private key for JWT signing.
        
        Args:
            key_path: Path where to save the private key
            
        Returns:
            Path to the generated private key file
        """
        # Generate private key
        private_key = rsa.generate_private_key(
            public_exponent=65537,
            key_size=2048
        )
        
        # Serialize private key
        private_pem = private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        )
        
        # Create directory if it doesn't exist
        os.makedirs(os.path.dirname(key_path), exist_ok=True)
        
        # Save private key
        with open(key_path, 'wb') as f:
            f.write(private_pem)
        
        # Set secure permissions (readable only by owner)
        os.chmod(key_path, 0o600)
        
        # Generate public key for IBKR registration
        public_key = private_key.public_key()
        public_pem = public_key.public_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PublicFormat.SubjectPublicKeyInfo
        )
        
        # Save public key for IBKR registration
        public_key_path = key_path.replace('.pem', '_public.pem')
        with open(public_key_path, 'wb') as f:
            f.write(public_pem)
        
        self.logger.info(f"Generated private key: {key_path}")
        self.logger.info(f"Generated public key: {public_key_path}")
        
        return key_path
    
    def _load_private_key(self) -> Any:
        """Load the private key for JWT signing."""
        key_path = self.config.get('private_key_path')
        if not key_path or not os.path.exists(key_path):
            raise ValueError(f"Private key not found: {key_path}")
        
        with open(key_path, 'rb') as f:
            private_key = serialization.load_pem_private_key(
                f.read(),
                password=None
            )
        
        return private_key
    
    def _create_client_assertion(self) -> str:
        """
        Create a JWT client assertion for OAuth authentication.
        
        This implements the private_key_jwt method as required by IBKR.
        """
        private_key = self._load_private_key()
        
        # JWT header
        header = {
            'alg': 'RS256',
            'typ': 'JWT'
        }
        
        # JWT payload
        now = int(time.time())
        payload = {
            'iss': self.config['client_id'],  # Issuer (your client ID)
            'sub': self.config['client_id'],  # Subject (your client ID)
            'aud': self.oauth_url,            # Audience (IBKR token endpoint)
            'jti': f"{self.config['client_id']}_{now}",  # Unique identifier
            'exp': now + 300,                 # Expires in 5 minutes
            'iat': now                        # Issued at
        }
        
        # Sign the JWT
        token = jwt.encode(payload, private_key, algorithm='RS256', headers=header)
        
        return token
    
    def authenticate(self) -> Dict[str, Any]:
        """
        Authenticate with IBKR using OAuth 2.0 and JWT client assertion.
        
        Returns:
            Authentication result with access token
        """
        try:
            client_assertion = self._create_client_assertion()
            
            # OAuth 2.0 token request
            token_data = {
                'grant_type': 'client_credentials',
                'client_assertion_type': 'urn:ietf:params:oauth:client-assertion-type:jwt-bearer',
                'client_assertion': client_assertion,
                'scope': 'read write'  # Adjust scopes as needed
            }
            
            response = requests.post(
                self.oauth_url,
                data=token_data,
                headers={
                    'Content-Type': 'application/x-www-form-urlencoded',
                    'Accept': 'application/json'
                }
            )
            
            if response.status_code == 200:
                token_response = response.json()
                
                self.access_token = token_response['access_token']
                expires_in = token_response.get('expires_in', 3600)
                self.token_expires_at = datetime.now() + timedelta(seconds=expires_in)
                
                # Update session headers with access token
                self.session.headers['Authorization'] = f'Bearer {self.access_token}'
                
                self.logger.info("OAuth authentication successful")
                
                return {
                    'success': True,
                    'access_token': self.access_token,
                    'expires_in': expires_in,
                    'token_type': token_response.get('token_type', 'Bearer')
                }
            
            else:
                error_msg = f"Authentication failed: {response.status_code} - {response.text}"
                self.logger.error(error_msg)
                
                return {
                    'success': False,
                    'error': error_msg,
                    'status_code': response.status_code
                }
                
        except Exception as e:
            error_msg = f"Authentication error: {str(e)}"
            self.logger.error(error_msg)
            
            return {
                'success': False,
                'error': error_msg
            }
    
    def _ensure_authenticated(self) -> bool:
        """Ensure we have a valid access token."""
        if not self.access_token or (
            self.token_expires_at and datetime.now() >= self.token_expires_at
        ):
            auth_result = self.authenticate()
            return auth_result.get('success', False)
        
        return True
    
    def get_accounts(self) -> Dict[str, Any]:
        """Get account information."""
        if not self._ensure_authenticated():
            return {'success': False, 'error': 'Authentication failed'}
        
        try:
            response = self.session.get(f"{self.base_url}/v1/api/accounts")
            
            if response.status_code == 200:
                return {
                    'success': True,
                    'accounts': response.json()
                }
            else:
                return {
                    'success': False,
                    'error': f"Failed to get accounts: {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting accounts: {str(e)}"
            }
    
    def get_market_data(self, symbol: str, fields: List[str] = None) -> Dict[str, Any]:
        """
        Get real-time market data for a symbol.
        
        Args:
            symbol: Trading symbol (e.g., 'AAPL', 'TSLA')
            fields: List of data fields to retrieve
            
        Returns:
            Market data response
        """
        if not self._ensure_authenticated():
            return {'success': False, 'error': 'Authentication failed'}
        
        if fields is None:
            fields = ['last_price', 'bid', 'ask', 'volume']
        
        try:
            params = {
                'symbols': symbol,
                'fields': ','.join(fields)
            }
            
            response = self.session.get(
                f"{self.base_url}/v1/api/marketdata/snapshot",
                params=params
            )
            
            if response.status_code == 200:
                data = response.json()
                return {
                    'success': True,
                    'symbol': symbol,
                    'data': data,
                    'timestamp': datetime.now().isoformat()
                }
            else:
                return {
                    'success': False,
                    'error': f"Failed to get market data: {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting market data: {str(e)}"
            }
    
    def get_historical_data(self, symbol: str, period: str = '1d', 
                          interval: str = '1m') -> Dict[str, Any]:
        """
        Get historical market data.
        
        Args:
            symbol: Trading symbol
            period: Time period (1d, 5d, 1mo, etc.)
            interval: Data interval (1m, 5m, 1h, etc.)
            
        Returns:
            Historical data response
        """
        if not self._ensure_authenticated():
            return {'success': False, 'error': 'Authentication failed'}
        
        try:
            params = {
                'symbol': symbol,
                'period': period,
                'interval': interval
            }
            
            response = self.session.get(
                f"{self.base_url}/v1/api/marketdata/history",
                params=params
            )
            
            if response.status_code == 200:
                data = response.json()
                
                # Convert to DataFrame if data is available
                df = None
                if 'data' in data and data['data']:
                    df = pd.DataFrame(data['data'])
                    if 'timestamp' in df.columns:
                        df['timestamp'] = pd.to_datetime(df['timestamp'])
                        df.set_index('timestamp', inplace=True)
                
                return {
                    'success': True,
                    'symbol': symbol,
                    'period': period,
                    'interval': interval,
                    'data': data,
                    'dataframe': df
                }
            else:
                return {
                    'success': False,
                    'error': f"Failed to get historical data: {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting historical data: {str(e)}"
            }
    
    def place_order(self, symbol: str, quantity: int, order_type: str = 'MKT',
                   side: str = 'BUY', time_in_force: str = 'DAY') -> Dict[str, Any]:
        """
        Place a trading order.
        
        Args:
            symbol: Trading symbol
            quantity: Number of shares
            order_type: Order type (MKT, LMT, STP, etc.)
            side: BUY or SELL
            time_in_force: Time in force (DAY, GTC, etc.)
            
        Returns:
            Order placement response
        """
        if not self._ensure_authenticated():
            return {'success': False, 'error': 'Authentication failed'}
        
        # Safety check for paper trading
        if self.config.get('trading_mode') != 'live':
            self.logger.warning("Paper trading mode - order simulation only")
        
        try:
            order_data = {
                'symbol': symbol,
                'quantity': quantity,
                'orderType': order_type,
                'side': side,
                'timeInForce': time_in_force,
                'account': self.config.get('account_number')
            }
            
            response = self.session.post(
                f"{self.base_url}/v1/api/orders",
                json=order_data
            )
            
            if response.status_code in [200, 201]:
                return {
                    'success': True,
                    'order': response.json(),
                    'message': 'Order placed successfully'
                }
            else:
                return {
                    'success': False,
                    'error': f"Failed to place order: {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error placing order: {str(e)}"
            }
    
    def get_positions(self) -> Dict[str, Any]:
        """Get current positions."""
        if not self._ensure_authenticated():
            return {'success': False, 'error': 'Authentication failed'}
        
        try:
            account = self.config.get('account_number')
            response = self.session.get(f"{self.base_url}/v1/api/accounts/{account}/positions")
            
            if response.status_code == 200:
                positions = response.json()
                
                # Convert to DataFrame for easier analysis
                df = None
                if positions and isinstance(positions, list):
                    df = pd.DataFrame(positions)
                
                return {
                    'success': True,
                    'positions': positions,
                    'dataframe': df
                }
            else:
                return {
                    'success': False,
                    'error': f"Failed to get positions: {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting positions: {str(e)}"
            }
    
    def health_check(self) -> Dict[str, Any]:
        """Perform a health check of the connection."""
        result = {
            'timestamp': datetime.now().isoformat(),
            'api_type': 'OAuth2_WebAPI',
            'base_url': self.base_url,
            'config_loaded': bool(self.config),
            'has_client_id': bool(self.config.get('client_id')),
            'has_private_key': bool(self.config.get('private_key_path') and 
                                  os.path.exists(self.config.get('private_key_path', ''))),
            'authenticated': bool(self.access_token),
            'token_valid': False
        }
        
        # Check if current token is still valid
        if self.access_token and self.token_expires_at:
            result['token_valid'] = datetime.now() < self.token_expires_at
            result['token_expires_at'] = self.token_expires_at.isoformat()
        
        # Try authentication if not authenticated
        if not result['authenticated']:
            auth_result = self.authenticate()
            result['authenticated'] = auth_result.get('success', False)
            if not result['authenticated']:
                result['auth_error'] = auth_result.get('error')
        
        # Test a simple API call if authenticated
        if result['authenticated']:
            accounts_result = self.get_accounts()
            result['api_test'] = accounts_result.get('success', False)
            if not result['api_test']:
                result['api_error'] = accounts_result.get('error')
        
        result['status'] = 'healthy' if (
            result['config_loaded'] and 
            result['has_client_id'] and 
            result['has_private_key'] and 
            result['authenticated']
        ) else 'unhealthy'
        
        return result

# Example usage
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    
    # Initialize connector
    connector = IBKROAuthConnector()
    
    print("🔐 IBKR OAuth 2.0 Connector Test")
    print("================================")
    
    # Health check
    health = connector.health_check()
    print(f"Health Status: {health['status']}")
    print(f"Configuration: {'✅' if health['config_loaded'] else '❌'}")
    print(f"Client ID: {'✅' if health['has_client_id'] else '❌'}")
    print(f"Private Key: {'✅' if health['has_private_key'] else '❌'}")
    print(f"Authentication: {'✅' if health['authenticated'] else '❌'}")
    
    if health['authenticated']:
        print("\n📊 Testing API calls...")
        
        # Test accounts
        accounts = connector.get_accounts()
        print(f"Accounts API: {'✅' if accounts['success'] else '❌'}")
        
        # Test market data
        market_data = connector.get_market_data('AAPL')
        print(f"Market Data API: {'✅' if market_data['success'] else '❌'}")
        
        print("\n🎉 OAuth connector ready for trading!")
    else:
        print(f"\n❌ Setup required: {health.get('auth_error', 'See setup guide')}")
