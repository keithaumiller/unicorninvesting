#!/usr/bin/env python3
"""
Interactive Brokers Client Portal API Connector
Unicorn Investing Platform

This connector uses the IBKR Client Portal Web API, which doesn't require 
TWS or IB Gateway installation.
"""

import requests
import json
import time
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
import pandas as pd

class IBKRClientPortalConnector:
    """
    Interactive Brokers Client Portal API connector for market data and trading.
    
    Features:
    - No TWS/Gateway required
    - Web-based authentication
    - Real-time market data
    - Order management
    - Account information
    """
    
    def __init__(self, base_url: str = "https://localhost:5000/v1/api"):
        """
        Initialize the IBKR Client Portal connector.
        
        Args:
            base_url: Base URL for the Client Portal API
        """
        self.base_url = base_url
        self.session = requests.Session()
        self.session.verify = False  # Client Portal uses self-signed cert
        self.authenticated = False
        self.account_id = None
        
        # Setup logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
        
        # Disable SSL warnings for self-signed cert
        import urllib3
        urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)
    
    def authenticate(self) -> bool:
        """
        Authenticate with IBKR Client Portal.
        
        Returns:
            bool: True if authentication successful
        """
        try:
            # Check if already authenticated
            auth_status = self._get("/iserver/auth/status")
            if auth_status.get("authenticated", False):
                self.authenticated = True
                self.logger.info("Already authenticated with IBKR")
                return True
            
            # Start authentication process
            auth_response = self._post("/iserver/auth/sso/validate")
            
            if auth_response.status_code == 200:
                self.authenticated = True
                self.logger.info("Successfully authenticated with IBKR")
                return True
            else:
                self.logger.error(f"Authentication failed: {auth_response.text}")
                return False
                
        except Exception as e:
            self.logger.error(f"Authentication error: {e}")
            return False
    
    def get_accounts(self) -> List[Dict]:
        """
        Get list of available accounts.
        
        Returns:
            List of account dictionaries
        """
        if not self.authenticated:
            if not self.authenticate():
                return []
        
        try:
            response = self._get("/iserver/accounts")
            if response:
                accounts = response.get("accounts", [])
                if accounts:
                    self.account_id = accounts[0]["id"]  # Use first account
                return accounts
        except Exception as e:
            self.logger.error(f"Error getting accounts: {e}")
        
        return []
    
    def get_market_data(self, symbol: str, fields: List[str] = None) -> Dict:
        """
        Get real-time market data for a symbol.
        
        Args:
            symbol: Trading symbol (e.g., "AAPL", "ETH-USD")
            fields: List of fields to retrieve
            
        Returns:
            Dictionary with market data
        """
        if not fields:
            fields = ["31", "55", "70", "71", "83", "84", "85", "86"]  # OHLCV data
        
        try:
            # First, search for the contract
            contract_id = self._search_contract(symbol)
            if not contract_id:
                self.logger.error(f"Contract not found for symbol: {symbol}")
                return {}
            
            # Get market data
            params = {
                "conids": contract_id,
                "fields": ",".join(fields)
            }
            
            response = self._get("/iserver/marketdata/snapshot", params=params)
            if response:
                return response
                
        except Exception as e:
            self.logger.error(f"Error getting market data for {symbol}: {e}")
        
        return {}
    
    def get_historical_data(self, symbol: str, period: str = "1d", 
                          bar_size: str = "1min") -> pd.DataFrame:
        """
        Get historical market data.
        
        Args:
            symbol: Trading symbol
            period: Time period (1d, 1w, 1m, etc.)
            bar_size: Bar size (1min, 5min, 1h, 1d, etc.)
            
        Returns:
            DataFrame with OHLCV data
        """
        try:
            contract_id = self._search_contract(symbol)
            if not contract_id:
                return pd.DataFrame()
            
            params = {
                "conid": contract_id,
                "period": period,
                "bar": bar_size
            }
            
            response = self._get("/iserver/marketdata/history", params=params)
            if response and "data" in response:
                # Convert to DataFrame
                data = response["data"]
                df = pd.DataFrame(data)
                
                # Convert timestamp and set proper column names
                if not df.empty:
                    df["datetime"] = pd.to_datetime(df["t"], unit="ms")
                    df = df.rename(columns={
                        "o": "Open",
                        "h": "High", 
                        "l": "Low",
                        "c": "Close",
                        "v": "Volume"
                    })
                    df.set_index("datetime", inplace=True)
                
                return df
                
        except Exception as e:
            self.logger.error(f"Error getting historical data for {symbol}: {e}")
        
        return pd.DataFrame()
    
    def place_order(self, symbol: str, quantity: int, order_type: str = "MKT",
                   side: str = "BUY") -> Dict:
        """
        Place a trading order.
        
        Args:
            symbol: Trading symbol
            quantity: Number of shares/units
            order_type: Order type (MKT, LMT, STP, etc.)
            side: Order side (BUY/SELL)
            
        Returns:
            Order response dictionary
        """
        if not self.account_id:
            self.get_accounts()
        
        try:
            contract_id = self._search_contract(symbol)
            if not contract_id:
                return {}
            
            order_data = {
                "orders": [{
                    "conid": contract_id,
                    "orderType": order_type,
                    "side": side,
                    "quantity": quantity,
                    "tif": "DAY"
                }]
            }
            
            response = self._post(f"/iserver/account/{self.account_id}/orders", 
                                json=order_data)
            return response
            
        except Exception as e:
            self.logger.error(f"Error placing order for {symbol}: {e}")
        
        return {}
    
    def get_account_summary(self) -> Dict:
        """
        Get account summary information.
        
        Returns:
            Dictionary with account information
        """
        if not self.account_id:
            self.get_accounts()
        
        try:
            response = self._get(f"/iserver/account/{self.account_id}/summary")
            return response
        except Exception as e:
            self.logger.error(f"Error getting account summary: {e}")
        
        return {}
    
    def get_positions(self) -> List[Dict]:
        """
        Get current positions.
        
        Returns:
            List of position dictionaries
        """
        if not self.account_id:
            self.get_accounts()
        
        try:
            response = self._get(f"/iserver/account/{self.account_id}/positions/0")
            return response if response else []
        except Exception as e:
            self.logger.error(f"Error getting positions: {e}")
        
        return []
    
    def _search_contract(self, symbol: str) -> Optional[str]:
        """
        Search for contract ID by symbol.
        
        Args:
            symbol: Trading symbol
            
        Returns:
            Contract ID string or None
        """
        try:
            params = {"symbol": symbol}
            response = self._get("/iserver/secdef/search", params=params)
            
            if response and len(response) > 0:
                return str(response[0]["conid"])
                
        except Exception as e:
            self.logger.error(f"Error searching contract for {symbol}: {e}")
        
        return None
    
    def _get(self, endpoint: str, params: Dict = None) -> Any:
        """Make GET request to API."""
        url = f"{self.base_url}{endpoint}"
        response = self.session.get(url, params=params)
        
        if response.status_code == 200:
            return response.json()
        else:
            self.logger.error(f"GET {endpoint} failed: {response.status_code} - {response.text}")
            return None
    
    def _post(self, endpoint: str, json: Dict = None) -> Any:
        """Make POST request to API."""
        url = f"{self.base_url}{endpoint}"
        response = self.session.post(url, json=json)
        
        if response.status_code in [200, 201]:
            return response.json()
        else:
            self.logger.error(f"POST {endpoint} failed: {response.status_code} - {response.text}")
            return None
    
    def health_check(self) -> Dict[str, Any]:
        """
        Perform comprehensive health check.
        
        Returns:
            Dictionary with health check results
        """
        results = {
            "timestamp": datetime.now().isoformat(),
            "connection_status": "disconnected",
            "authentication_status": "not_authenticated",
            "accounts_available": 0,
            "market_data_access": False,
            "trading_permissions": False,
            "errors": []
        }
        
        try:
            # Test basic connectivity
            auth_status = self._get("/iserver/auth/status")
            if auth_status:
                results["connection_status"] = "connected"
                
                # Test authentication
                if self.authenticate():
                    results["authentication_status"] = "authenticated"
                    
                    # Test account access
                    accounts = self.get_accounts()
                    results["accounts_available"] = len(accounts)
                    
                    if accounts:
                        # Test market data
                        market_data = self.get_market_data("AAPL")
                        results["market_data_access"] = bool(market_data)
                        
                        # Test account summary (indicates trading permissions)
                        account_summary = self.get_account_summary()
                        results["trading_permissions"] = bool(account_summary)
                
        except Exception as e:
            results["errors"].append(str(e))
        
        return results


def main():
    """Test the IBKR Client Portal connector."""
    print("🔌 Testing IBKR Client Portal API Connector")
    print("=" * 50)
    
    # Initialize connector
    connector = IBKRClientPortalConnector()
    
    # Run health check
    health = connector.health_check()
    
    print(f"Connection Status: {health['connection_status']}")
    print(f"Authentication: {health['authentication_status']}")
    print(f"Accounts Available: {health['accounts_available']}")
    print(f"Market Data Access: {health['market_data_access']}")
    print(f"Trading Permissions: {health['trading_permissions']}")
    
    if health["errors"]:
        print("\nErrors:")
        for error in health["errors"]:
            print(f"  - {error}")
    
    # Test market data if authenticated
    if health["authentication_status"] == "authenticated":
        print("\n📊 Testing Market Data:")
        
        # Test with popular symbols
        symbols = ["AAPL", "GOOGL", "MSFT"]
        for symbol in symbols:
            try:
                data = connector.get_market_data(symbol)
                if data:
                    print(f"  ✅ {symbol}: Market data retrieved")
                else:
                    print(f"  ❌ {symbol}: No market data")
            except Exception as e:
                print(f"  ❌ {symbol}: Error - {e}")


if __name__ == "__main__":
    main()
