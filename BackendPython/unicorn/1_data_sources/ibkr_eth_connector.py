#!/usr/bin/env python3
"""
IBKR Data Connector for ETH Trading
Connects IBKR Gateway to ETH momentum and risk algorithms
"""

import requests
import json
import time
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class IBKRETHConnector:
    """Real-time ETH data connector for IBKR Gateway"""
    
    def __init__(self, base_url="http://localhost:5000"):
        self.base_url = base_url
        self.eth_conid = None  # Contract ID for ETH
        self.session = requests.Session()
        
    def authenticate_status(self) -> bool:
        """Check if gateway is authenticated and establish bridge"""
        try:
            # First establish session with tickle
            tickle_response = self.session.get(f"{self.base_url}/v1/api/tickle")
            if tickle_response.status_code == 200:
                tickle_data = tickle_response.json()
                logger.info(f"Session established: {tickle_data.get('session', 'Unknown')}")
            
            # Check auth status endpoint
            response = self.session.get(f"{self.base_url}/v1/api/iserver/auth/status")
            if response.status_code == 200:
                status = response.json()
                if status.get('authenticated', False):
                    return True
            
            # If auth status shows false, try accessing portfolio accounts as backup
            # This endpoint requires authentication and will fail if not authenticated
            portfolio_response = self.session.get(f"{self.base_url}/v1/api/portfolio/accounts")
            if portfolio_response.status_code == 200:
                accounts = portfolio_response.json()
                if isinstance(accounts, list) and len(accounts) > 0:
                    logger.info("Authentication verified via portfolio access")
                    return True
            
            return False
        except Exception as e:
            logger.error(f"Auth check failed: {e}")
            return False
    
    def find_eth_contract(self) -> Optional[str]:
        """Find ETH contract ID"""
        try:
            # Search for ETH futures or crypto
            search_data = {"symbol": "ETH"}
            response = self.session.post(
                f"{self.base_url}/v1/api/iserver/secdef/search",
                json=search_data
            )
            
            if response.status_code == 200:
                contracts = response.json()
                logger.info(f"Found ETH contracts: {len(contracts)}")
                
                # Look for crypto or futures contracts
                for contract in contracts:
                    if 'conid' in contract:
                        # Prefer crypto over futures for ETH
                        if contract.get('description', '').upper().find('CRYPTO') != -1:
                            self.eth_conid = contract['conid']
                            logger.info(f"Selected ETH crypto contract: {contract}")
                            return self.eth_conid
                        elif contract.get('description', '').upper().find('FUTURE') != -1:
                            self.eth_conid = contract['conid']
                            logger.info(f"Selected ETH futures contract: {contract}")
                            return self.eth_conid
                
                # If no specific type found, use first available
                if contracts and 'conid' in contracts[0]:
                    self.eth_conid = contracts[0]['conid']
                    logger.info(f"Selected first ETH contract: {contracts[0]}")
                    return self.eth_conid
                    
            return None
            
        except Exception as e:
            logger.error(f"Contract search failed: {e}")
            return None
    
    def get_eth_snapshot(self) -> Optional[Dict]:
        """Get current ETH market snapshot"""
        if not self.eth_conid:
            logger.warning("No ETH contract ID available")
            return None
            
        try:
            # Request market data snapshot
            response = self.session.get(
                f"{self.base_url}/v1/api/iserver/marketdata/snapshot",
                params={
                    'conids': self.eth_conid,
                    'fields': '31,84,86'  # Last price, bid, ask
                }
            )
            
            if response.status_code == 200:
                data = response.json()
                logger.info(f"ETH snapshot: {data}")
                return data
            else:
                logger.warning(f"Snapshot request failed: {response.status_code}")
                return None
                
        except Exception as e:
            logger.error(f"Snapshot request failed: {e}")
            return None
    
    def get_eth_historical(self, period="1h", bars=50) -> Optional[pd.DataFrame]:
        """Get ETH historical data"""
        if not self.eth_conid:
            logger.warning("No ETH contract ID available")
            return None
            
        try:
            # Request historical data
            response = self.session.get(
                f"{self.base_url}/v1/api/iserver/marketdata/history",
                params={
                    'conid': self.eth_conid,
                    'period': period,
                    'bar': '1min'  # 1-minute bars
                }
            )
            
            if response.status_code == 200:
                data = response.json()
                
                # Convert to DataFrame if data available
                if 'data' in data:
                    df = pd.DataFrame(data['data'])
                    if not df.empty:
                        # Convert timestamp and set as index
                        df['timestamp'] = pd.to_datetime(df['t'], unit='ms')
                        df.set_index('timestamp', inplace=True)
                        
                        # Rename columns for consistency
                        column_mapping = {
                            'o': 'open',
                            'h': 'high', 
                            'l': 'low',
                            'c': 'close',
                            'v': 'volume'
                        }
                        df.rename(columns=column_mapping, inplace=True)
                        
                        logger.info(f"Retrieved {len(df)} historical bars")
                        return df
                        
            logger.warning(f"Historical data request failed: {response.status_code}")
            return None
            
        except Exception as e:
            logger.error(f"Historical data request failed: {e}")
            return None
    
    def create_eth_data_feed(self) -> Dict:
        """Create real-time data feed for ETH algorithms"""
        
        # Check authentication
        if not self.authenticate_status():
            logger.error("Gateway not authenticated")
            return {
                'status': 'error',
                'message': 'Gateway not authenticated',
                'timestamp': datetime.now()
            }
        
        # Find ETH contract
        if not self.eth_conid:
            self.find_eth_contract()
            
        if not self.eth_conid:
            logger.error("Could not find ETH contract")
            return {
                'status': 'error', 
                'message': 'ETH contract not found',
                'timestamp': datetime.now()
            }
        
        # Get current data
        snapshot = self.get_eth_snapshot()
        historical = self.get_eth_historical()
        
        return {
            'status': 'success',
            'contract_id': self.eth_conid,
            'snapshot': snapshot,
            'historical': historical,
            'timestamp': datetime.now()
        }

def main():
    """Test ETH data connectivity"""
    print("=" * 50)
    print("IBKR ETH Data Connector Test")
    print(f"Timestamp: {datetime.now()}")
    print("=" * 50)
    
    connector = IBKRETHConnector()
    
    # Test data feed creation
    print("\nCreating ETH data feed...")
    data_feed = connector.create_eth_data_feed()
    
    print(f"\nData Feed Status: {data_feed['status']}")
    
    if data_feed['status'] == 'success':
        print(f"✅ ETH Contract ID: {data_feed['contract_id']}")
        
        if data_feed['snapshot']:
            print("✅ Real-time snapshot available")
            
        if data_feed['historical'] is not None:
            df = data_feed['historical']
            print(f"✅ Historical data: {len(df)} bars")
            print(f"   Latest close: {df['close'].iloc[-1] if not df.empty else 'N/A'}")
            
        print("\n🎯 Ready for algorithm integration!")
        
    else:
        print(f"❌ Error: {data_feed['message']}")
        print("Please ensure IBKR Gateway is authenticated")

if __name__ == "__main__":
    main()
