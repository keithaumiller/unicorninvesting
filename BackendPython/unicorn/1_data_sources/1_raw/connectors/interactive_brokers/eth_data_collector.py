"""
IBKR ETH Data Collector
Unicorn Investing Platform

Collects Ethereum (ETH) data using Interactive Brokers Client Portal API.
Saves data to the organized data directory structure.
"""

import os
import sys
import pandas as pd
import requests
from datetime import datetime, timedelta
import json
import time

# Add the IBKR connector to the path
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

class IBKREthDataCollector:
    """
    ETH data collector using IBKR Client Portal API.
    """
    
    def __init__(self):
        self.base_url = "http://localhost:5000/v1/api"
        self.session = requests.Session()
        
        # Data storage path
        self.data_dir = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/interactive_brokers/ETH"
        os.makedirs(self.data_dir, exist_ok=True)
        
        print(f"📁 Data directory: {self.data_dir}")
    
    def check_authentication(self):
        """Check if IBKR gateway is authenticated."""
        try:
            response = self.session.get(f"{self.base_url}/iserver/auth/status")
            if response.status_code == 200:
                auth_data = response.json()
                authenticated = auth_data.get('authenticated', False)
                connected = auth_data.get('connected', False)
                
                print(f"🔐 Authentication: {'✅' if authenticated else '❌'}")
                print(f"🔗 Connected: {'✅' if connected else '❌'}")
                
                return authenticated and connected
            else:
                print(f"❌ Auth check failed: HTTP {response.status_code}")
                return False
        except Exception as e:
            print(f"❌ Authentication error: {e}")
            return False
    
    def search_eth_contracts(self):
        """Search for ETH/cryptocurrency contracts in IBKR."""
        print("\n🔍 Searching for ETH contracts...")
        
        # Try different search terms for Ethereum
        search_terms = [
            {'symbol': 'ETH', 'secType': 'CRYPTO'},
            {'symbol': 'ETHUSD', 'secType': 'CRYPTO'},
            {'symbol': 'ETH', 'secType': 'CFD'},
            {'symbol': 'ETH', 'secType': 'STK'},
            {'symbol': 'ETHEREUM', 'secType': 'CRYPTO'},
            {'symbol': 'BTC', 'secType': 'CRYPTO'},  # Test with Bitcoin
        ]
        
        found_contracts = []
        
        for search_params in search_terms:
            try:
                response = self.session.get(
                    f"{self.base_url}/iserver/secdef/search",
                    params=search_params
                )
                
                if response.status_code == 200:
                    contracts = response.json()
                    if contracts:
                        print(f"✅ Found {len(contracts)} contracts for {search_params}")
                        for contract in contracts:
                            desc = contract.get('description') or f"Contract {contract.get('conid')}"
                            print(f"   - {desc} (ID: {contract.get('conid')})")
                            found_contracts.append(contract)
                    else:
                        print(f"❌ No contracts found for {search_params}")
                else:
                    print(f"❌ Search failed for {search_params}: HTTP {response.status_code}")
                    
            except Exception as e:
                print(f"❌ Search error for {search_params}: {e}")
        
        return found_contracts
    
    def get_market_data(self, contract_id, fields='31,55,70,71,84,86'):
        """Get real-time market data for a contract."""
        try:
            params = {
                'conids': str(contract_id),
                'fields': fields  # Last, Bid, High, Low, Volume, etc.
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/snapshot",
                params=params
            )
            
            if response.status_code == 200:
                return response.json()
            else:
                print(f"❌ Market data failed: HTTP {response.status_code}")
                return None
                
        except Exception as e:
            print(f"❌ Market data error: {e}")
            return None
    
    def get_historical_data(self, contract_id, period='1d', bar_size='1min'):
        """Get historical data for a contract."""
        try:
            params = {
                'conid': contract_id,
                'period': period,
                'bar': bar_size,
                'outsideRth': 'true'  # Include outside regular trading hours
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/history",
                params=params
            )
            
            if response.status_code == 200:
                return response.json()
            else:
                print(f"❌ Historical data failed: HTTP {response.status_code} - {response.text}")
                return None
                
        except Exception as e:
            print(f"❌ Historical data error: {e}")
            return None
    
    def save_data(self, data, filename_prefix="eth_data"):
        """Save data to file."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{filename_prefix}_{timestamp}.json"
        filepath = os.path.join(self.data_dir, filename)
        
        try:
            with open(filepath, 'w') as f:
                json.dump(data, f, indent=2, default=str)
            
            print(f"💾 Data saved: {filepath}")
            return filepath
        except Exception as e:
            print(f"❌ Save error: {e}")
            return None
    
    def collect_eth_data(self):
        """Main method to collect ETH data."""
        print("🚀 IBKR ETH Data Collection")
        print("===========================")
        
        # Check authentication
        if not self.check_authentication():
            print("❌ Please ensure IBKR Client Portal Gateway is running and authenticated")
            return False
        
        # Search for ETH contracts
        contracts = self.search_eth_contracts()
        
        if not contracts:
            print("❌ No ETH contracts found. IBKR may not support crypto trading for your account.")
            print("💡 Suggestion: Check if your IBKR account has cryptocurrency permissions")
            return False
        
        # Use the first found contract (or let user choose)
        target_contract = contracts[0]
        contract_id = target_contract.get('conid')
        contract_desc = target_contract.get('description', 'Unknown')
        
        print(f"\n📊 Using contract: {contract_desc} (ID: {contract_id})")
        
        # Collect real-time data
        print("\n📈 Collecting real-time market data...")
        market_data = self.get_market_data(contract_id)
        
        if market_data:
            print(f"✅ Market data collected")
            market_file = self.save_data({
                'contract': target_contract,
                'market_data': market_data,
                'timestamp': datetime.now().isoformat(),
                'data_type': 'realtime'
            }, 'eth_realtime')
        else:
            print("❌ Failed to collect market data")
        
        # Collect historical data
        print("\n📊 Collecting historical data...")
        historical_data = self.get_historical_data(contract_id, period='1d', bar_size='5mins')
        
        if historical_data:
            print(f"✅ Historical data collected")
            
            # Convert to DataFrame for analysis
            if 'data' in historical_data:
                bars = historical_data['data']
                if bars:
                    df = pd.DataFrame(bars)
                    print(f"📊 Data points: {len(df)}")
                    
                    # Show sample data
                    if len(df) > 0:
                        print("Sample data:")
                        print(df.head())
                    
                    # Save historical data
                    hist_file = self.save_data({
                        'contract': target_contract,
                        'historical_data': historical_data,
                        'timestamp': datetime.now().isoformat(),
                        'data_type': 'historical',
                        'period': '1d',
                        'bar_size': '5mins'
                    }, 'eth_historical')
            else:
                print("⚠️ No historical data bars found")
        else:
            print("❌ Failed to collect historical data")
        
        print("\n🎉 ETH data collection complete!")
        return True

def main():
    """Run ETH data collection."""
    collector = IBKREthDataCollector()
    collector.collect_eth_data()

if __name__ == "__main__":
    main()
