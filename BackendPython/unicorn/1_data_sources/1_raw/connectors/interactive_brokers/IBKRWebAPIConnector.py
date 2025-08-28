"""
IBKR Standard Web API Connector
Unicorn Investing Platform

Simple, direct integration with IBKR's standard Web API.
No OAuth complexity - uses standard API key authentication.

This connector provides:
- Real-time market data
- Account information
- Order management
- Portfolio tracking
- Historical data

Documentation: https://www.interactivebrokers.com/api/doc/rest/
"""

import json
import requests
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
import logging
import os
import time

class IBKRWebAPIConnector:
    """
    IBKR Standard Web API Connector
    
    Simple, straightforward integration with IBKR's REST API.
    Uses standard authentication and is easier to set up than OAuth.
    """
    
    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize the IBKR Web API connector.
        
        Args:
            config_path: Path to configuration file
        """
        self.logger = logging.getLogger(__name__)
        self.session = requests.Session()
        
        # API Configuration
        self.base_url = "https://api.ibkr.com/v1/api"
        self.paper_url = "https://api.ibkr.com/v1/api/paper"  # Paper trading endpoint
        
        # Load configuration
        self.config = self._load_config(config_path)
        
        # Set base URL based on trading mode
        if self.config.get('trading_mode') == 'paper':
            self.api_url = self.paper_url
        else:
            self.api_url = self.base_url
        
        # Session configuration
        self.session.headers.update({
            'Content-Type': 'application/json',
            'Accept': 'application/json',
            'User-Agent': 'UnicornInvesting/1.0 IBKR-WebAPI'
        })
        
        # Add API key if available
        api_key = self.config.get('api_key')
        if api_key:
            self.session.headers['Authorization'] = f'Bearer {api_key}'
        
        self.authenticated = False
        self.session_token = None
    
    def _load_config(self, config_path: Optional[str] = None) -> Dict:
        """Load configuration from file or environment."""
        if config_path is None:
            config_path = os.path.join(
                os.path.dirname(__file__), 
                '../../../../config/ibkr/webapi_config.json'
            )
        
        config = {}
        
        # Try to load from file
        if os.path.exists(config_path):
            with open(config_path, 'r') as f:
                config = json.load(f)
        
        # Override with environment variables
        config.update({
            'api_key': os.getenv('IBKR_API_KEY', config.get('api_key')),
            'account_number': os.getenv('IBKR_ACCOUNT', config.get('account_number')),
            'username': os.getenv('IBKR_USERNAME', config.get('username')),
            'trading_mode': os.getenv('IBKR_TRADING_MODE', config.get('trading_mode', 'paper')),
            'api_type': 'Standard_WebAPI'
        })
        
        return config
    
    def authenticate(self) -> Dict[str, Any]:
        """
        Authenticate with IBKR Web API.
        
        For the standard Web API, this typically involves:
        1. API key validation
        2. Session establishment
        3. Account verification
        
        Returns:
            Authentication result
        """
        try:
            # Check if we have required credentials
            if not self.config.get('api_key'):
                return {
                    'success': False,
                    'error': 'API key not configured. Set IBKR_API_KEY environment variable.',
                    'setup_required': True
                }
            
            # Test API connectivity with a simple endpoint
            response = self.session.get(f"{self.api_url}/portal/iserver/auth/status")
            
            if response.status_code == 200:
                auth_data = response.json()
                
                # Check authentication status
                if auth_data.get('authenticated', False):
                    self.authenticated = True
                    self.session_token = auth_data.get('session_id')
                    
                    self.logger.info("IBKR Web API authentication successful")
                    
                    return {
                        'success': True,
                        'authenticated': True,
                        'session_id': self.session_token,
                        'message': 'Successfully authenticated with IBKR Web API'
                    }
                else:
                    # May need additional authentication steps
                    return {
                        'success': False,
                        'authenticated': False,
                        'error': 'Authentication required - may need to log in via web portal',
                        'auth_url': 'https://www.interactivebrokers.com/portal',
                        'details': auth_data
                    }
            
            elif response.status_code == 401:
                return {
                    'success': False,
                    'error': 'Invalid API key or credentials',
                    'status_code': 401
                }
            
            else:
                return {
                    'success': False,
                    'error': f'Authentication failed: HTTP {response.status_code}',
                    'details': response.text
                }
                
        except requests.exceptions.ConnectionError:
            return {
                'success': False,
                'error': 'Cannot connect to IBKR API - check internet connection',
                'connection_error': True
            }
        
        except Exception as e:
            error_msg = f"Authentication error: {str(e)}"
            self.logger.error(error_msg)
            
            return {
                'success': False,
                'error': error_msg
            }
    
    def get_accounts(self) -> Dict[str, Any]:
        """Get account information."""
        try:
            response = self.session.get(f"{self.api_url}/portfolio/accounts")
            
            if response.status_code == 200:
                accounts = response.json()
                return {
                    'success': True,
                    'accounts': accounts,
                    'count': len(accounts) if isinstance(accounts, list) else 1
                }
            
            elif response.status_code == 401:
                return {
                    'success': False,
                    'error': 'Authentication required',
                    'auth_required': True
                }
            
            else:
                return {
                    'success': False,
                    'error': f"Failed to get accounts: HTTP {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting accounts: {str(e)}"
            }
    
    def get_market_data(self, symbol: str, contract_type: str = 'STK') -> Dict[str, Any]:
        """
        Get real-time market data for a symbol.
        
        Args:
            symbol: Trading symbol (e.g., 'AAPL', 'TSLA')
            contract_type: Contract type (STK=Stock, OPT=Option, FUT=Future, etc.)
            
        Returns:
            Market data response
        """
        try:
            # IBKR uses contract IDs (conids) for market data
            # First, search for the contract
            search_params = {
                'symbol': symbol,
                'secType': contract_type
            }
            
            search_response = self.session.get(
                f"{self.api_url}/trsrv/secdef/search",
                params=search_params
            )
            
            if search_response.status_code != 200:
                return {
                    'success': False,
                    'error': f"Symbol search failed: {search_response.status_code}",
                    'symbol': symbol
                }
            
            search_data = search_response.json()
            
            if not search_data or len(search_data) == 0:
                return {
                    'success': False,
                    'error': f"Symbol not found: {symbol}",
                    'symbol': symbol
                }
            
            # Get the first matching contract
            contract = search_data[0]
            conid = contract.get('conid')
            
            if not conid:
                return {
                    'success': False,
                    'error': f"No contract ID found for {symbol}",
                    'symbol': symbol
                }
            
            # Get market data for the contract
            market_params = {
                'conids': str(conid),
                'fields': '31,55,70,71,84,86'  # Last, Bid, High, Low, Volume, etc.
            }
            
            market_response = self.session.get(
                f"{self.api_url}/iserver/marketdata/snapshot",
                params=market_params
            )
            
            if market_response.status_code == 200:
                market_data = market_response.json()
                
                return {
                    'success': True,
                    'symbol': symbol,
                    'contract_id': conid,
                    'contract_info': contract,
                    'market_data': market_data,
                    'timestamp': datetime.now().isoformat()
                }
            else:
                return {
                    'success': False,
                    'error': f"Market data request failed: {market_response.status_code}",
                    'details': market_response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting market data: {str(e)}",
                'symbol': symbol
            }
    
    def get_historical_data(self, symbol: str, period: str = '1d', 
                          bar_size: str = '1min') -> Dict[str, Any]:
        """
        Get historical market data.
        
        Args:
            symbol: Trading symbol
            period: Time period (1d, 1w, 1m, etc.)
            bar_size: Bar size (1min, 5mins, 1h, 1d, etc.)
            
        Returns:
            Historical data response
        """
        try:
            # First get the contract ID
            search_params = {
                'symbol': symbol,
                'secType': 'STK'
            }
            
            search_response = self.session.get(
                f"{self.api_url}/trsrv/secdef/search",
                params=search_params
            )
            
            if search_response.status_code != 200:
                return {
                    'success': False,
                    'error': f"Symbol search failed for historical data: {search_response.status_code}"
                }
            
            search_data = search_response.json()
            if not search_data:
                return {
                    'success': False,
                    'error': f"Symbol not found for historical data: {symbol}"
                }
            
            conid = search_data[0].get('conid')
            
            # Get historical data
            hist_params = {
                'conid': conid,
                'period': period,
                'bar': bar_size,
                'outsideRth': 'false'
            }
            
            hist_response = self.session.get(
                f"{self.api_url}/iserver/marketdata/history",
                params=hist_params
            )
            
            if hist_response.status_code == 200:
                hist_data = hist_response.json()
                
                # Convert to DataFrame if data is available
                df = None
                if 'data' in hist_data and hist_data['data']:
                    df = pd.DataFrame(hist_data['data'])
                    
                    # Convert timestamp if present
                    if 't' in df.columns:
                        df['timestamp'] = pd.to_datetime(df['t'], unit='ms')
                        df.set_index('timestamp', inplace=True)
                
                return {
                    'success': True,
                    'symbol': symbol,
                    'period': period,
                    'bar_size': bar_size,
                    'contract_id': conid,
                    'data': hist_data,
                    'dataframe': df,
                    'bars_count': len(hist_data.get('data', []))
                }
            else:
                return {
                    'success': False,
                    'error': f"Historical data request failed: {hist_response.status_code}",
                    'details': hist_response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting historical data: {str(e)}"
            }
    
    def place_order(self, symbol: str, quantity: int, order_type: str = 'MKT',
                   side: str = 'BUY') -> Dict[str, Any]:
        """
        Place a trading order.
        
        Args:
            symbol: Trading symbol
            quantity: Number of shares
            order_type: Order type (MKT, LMT, STP, etc.)
            side: BUY or SELL
            
        Returns:
            Order placement response
        """
        # Safety check for paper trading
        if self.config.get('trading_mode') != 'live':
            self.logger.warning("Paper trading mode active")
        
        try:
            # Get contract ID first
            search_params = {
                'symbol': symbol,
                'secType': 'STK'
            }
            
            search_response = self.session.get(
                f"{self.api_url}/trsrv/secdef/search",
                params=search_params
            )
            
            if search_response.status_code != 200:
                return {
                    'success': False,
                    'error': f"Symbol search failed for order: {search_response.status_code}"
                }
            
            search_data = search_response.json()
            if not search_data:
                return {
                    'success': False,
                    'error': f"Symbol not found for order: {symbol}"
                }
            
            conid = search_data[0].get('conid')
            account_id = self.config.get('account_number')
            
            # Prepare order data
            order_data = {
                'conid': conid,
                'secType': 'STK',
                'orderType': order_type,
                'side': side,
                'quantity': quantity,
                'tif': 'DAY'
            }
            
            # Place the order
            order_response = self.session.post(
                f"{self.api_url}/iserver/account/{account_id}/orders",
                json={'orders': [order_data]}
            )
            
            if order_response.status_code in [200, 201]:
                order_result = order_response.json()
                return {
                    'success': True,
                    'order': order_result,
                    'symbol': symbol,
                    'quantity': quantity,
                    'side': side,
                    'order_type': order_type,
                    'message': 'Order placed successfully'
                }
            else:
                return {
                    'success': False,
                    'error': f"Order placement failed: {order_response.status_code}",
                    'details': order_response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error placing order: {str(e)}"
            }
    
    def get_positions(self) -> Dict[str, Any]:
        """Get current positions."""
        try:
            account_id = self.config.get('account_number')
            
            if not account_id:
                return {
                    'success': False,
                    'error': 'Account number not configured'
                }
            
            response = self.session.get(f"{self.api_url}/portfolio/{account_id}/positions/0")
            
            if response.status_code == 200:
                positions = response.json()
                
                # Convert to DataFrame for easier analysis
                df = None
                if positions and isinstance(positions, list):
                    df = pd.DataFrame(positions)
                
                return {
                    'success': True,
                    'positions': positions,
                    'dataframe': df,
                    'count': len(positions) if isinstance(positions, list) else 0
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
    
    def get_account_summary(self) -> Dict[str, Any]:
        """Get account summary information."""
        try:
            account_id = self.config.get('account_number')
            
            if not account_id:
                return {
                    'success': False,
                    'error': 'Account number not configured'
                }
            
            response = self.session.get(f"{self.api_url}/portfolio/{account_id}/summary")
            
            if response.status_code == 200:
                summary = response.json()
                return {
                    'success': True,
                    'account_summary': summary,
                    'account_id': account_id
                }
            else:
                return {
                    'success': False,
                    'error': f"Failed to get account summary: {response.status_code}",
                    'details': response.text
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': f"Error getting account summary: {str(e)}"
            }
    
    def health_check(self) -> Dict[str, Any]:
        """Perform a comprehensive health check."""
        result = {
            'timestamp': datetime.now().isoformat(),
            'api_type': 'Standard_WebAPI',
            'api_url': self.api_url,
            'trading_mode': self.config.get('trading_mode', 'unknown'),
            'config_loaded': bool(self.config),
            'has_api_key': bool(self.config.get('api_key')),
            'has_account': bool(self.config.get('account_number')),
            'authenticated': self.authenticated
        }
        
        # Test authentication
        if not self.authenticated:
            auth_result = self.authenticate()
            result['auth_test'] = auth_result.get('success', False)
            result['authenticated'] = auth_result.get('success', False)
            
            if not result['auth_test']:
                result['auth_error'] = auth_result.get('error')
        
        # Test basic API calls if authenticated
        if result['authenticated']:
            # Test accounts endpoint
            accounts_result = self.get_accounts()
            result['accounts_test'] = accounts_result.get('success', False)
            
            if accounts_result.get('success'):
                result['accounts_count'] = accounts_result.get('count', 0)
            else:
                result['accounts_error'] = accounts_result.get('error')
            
            # Test market data with a common symbol
            market_result = self.get_market_data('AAPL')
            result['market_data_test'] = market_result.get('success', False)
            
            if not market_result.get('success'):
                result['market_data_error'] = market_result.get('error')
        
        # Overall health status
        result['status'] = 'healthy' if (
            result['config_loaded'] and 
            result['has_api_key'] and 
            result['has_account'] and 
            result['authenticated'] and
            result.get('accounts_test', False)
        ) else 'unhealthy'
        
        return result

# Example usage and testing
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    
    print("🌐 IBKR Standard Web API Connector Test")
    print("======================================")
    
    # Initialize connector
    connector = IBKRWebAPIConnector()
    
    # Run health check
    health = connector.health_check()
    
    print(f"\n📊 Health Check Results:")
    print(f"Status: {health['status'].upper()}")
    print(f"API URL: {health['api_url']}")
    print(f"Trading Mode: {health['trading_mode']}")
    print(f"Configuration: {'✅' if health['config_loaded'] else '❌'}")
    print(f"API Key: {'✅' if health['has_api_key'] else '❌'}")
    print(f"Account: {'✅' if health['has_account'] else '❌'}")
    print(f"Authentication: {'✅' if health['authenticated'] else '❌'}")
    
    if health.get('accounts_test'):
        print(f"Accounts API: ✅ ({health.get('accounts_count', 0)} accounts)")
    
    if health.get('market_data_test'):
        print(f"Market Data API: ✅")
    
    if health['status'] == 'healthy':
        print("\n🎉 IBKR Web API connector ready!")
        print("\nAvailable methods:")
        print("- get_market_data('AAPL')")
        print("- get_historical_data('TSLA', '1d', '5mins')")
        print("- get_positions()")
        print("- get_account_summary()")
        print("- place_order('MSFT', 100, 'MKT', 'BUY')")
    else:
        print(f"\n❌ Setup required")
        if 'auth_error' in health:
            print(f"Auth Error: {health['auth_error']}")
        if not health['has_api_key']:
            print("Missing: IBKR API key")
        if not health['has_account']:
            print("Missing: IBKR account number")
