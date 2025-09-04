#!/usr/bin/env python3
"""
IBKR Account Data Refresher
Actively queries IBKR Gateway and updates cached account information files.
"""

import os
import sys
import json
import requests
import logging
from datetime import datetime
from pathlib import Path

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class IBKRAccountRefresher:
    """Refreshes IBKR account data by querying live Gateway API"""
    
    def __init__(self, base_url="http://localhost:5000/v1/api"):
        self.base_url = base_url
        self.account_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers/accountinfo")
        self.account_dir.mkdir(parents=True, exist_ok=True)
        
    def test_gateway_connection(self) -> bool:
        """Test if IBKR Gateway is accessible"""
        try:
            response = requests.get(f"{self.base_url}/iserver/auth/status", timeout=5)
            return response.status_code == 200
        except Exception as e:
            logger.error(f"Gateway connection test failed: {e}")
            return False
    
    def refresh_account_summary(self) -> bool:
        """Refresh account summary information"""
        try:
            logger.info("Refreshing account summary...")
            response = requests.get(f"{self.base_url}/iserver/accounts", timeout=10)
            
            if response.status_code == 200:
                account_data = response.json()
                
                # Save complete account info
                complete_info = {
                    'account_summary': {'accounts': account_data},
                    'auth_status': {'authenticated': True, 'connected': True},
                    'last_updated': datetime.now().isoformat()
                }
                
                with open(self.account_dir / "complete_account_info.json", 'w') as f:
                    json.dump(complete_info, f, indent=2)
                
                logger.info("✅ Account summary refreshed successfully")
                return True
            else:
                logger.error(f"Account summary API returned status {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"Failed to refresh account summary: {e}")
            return False
    
    def refresh_portfolio_positions(self) -> bool:
        """Refresh current portfolio positions"""
        try:
            logger.info("Refreshing portfolio positions...")
            
            # Get account list first
            accounts_response = requests.get(f"{self.base_url}/portfolio/accounts", timeout=10)
            if accounts_response.status_code != 200:
                logger.error(f"Portfolio accounts API returned status {accounts_response.status_code}")
                return False
                
            accounts = accounts_response.json()
            if not accounts:
                logger.warning("No accounts found in portfolio response")
                return False
            
            account_id = accounts[0]['id']
            
            # Get positions for the account
            positions_response = requests.get(f"{self.base_url}/portfolio/{account_id}/positions/0", timeout=10)
            
            if positions_response.status_code == 200:
                positions_data = positions_response.json()
                
                # Calculate portfolio summary
                portfolio_summary = {
                    'account_id': account_id,
                    'positions': positions_data,
                    'summary': {
                        'total_positions': len(positions_data),
                        'net_liquidation': 0.0,
                        'market_value': 0.0,
                        'cash_balance': 0.0,
                        'unrealized_pnl': 0.0
                    },
                    'last_updated': datetime.now().isoformat()
                }
                
                # Calculate totals from positions
                for position in positions_data:
                    if 'mktValue' in position:
                        portfolio_summary['summary']['market_value'] += float(position.get('mktValue', 0))
                    if 'unrealizedPnl' in position:
                        portfolio_summary['summary']['unrealized_pnl'] += float(position.get('unrealizedPnl', 0))
                
                # Save portfolio data
                with open(self.account_dir / "current_portfolio.json", 'w') as f:
                    json.dump(portfolio_summary, f, indent=2)
                
                logger.info("✅ Portfolio positions refreshed successfully")
                return True
            else:
                logger.error(f"Portfolio positions API returned status {positions_response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"Failed to refresh portfolio positions: {e}")
            return False
    
    def refresh_market_data_permissions(self) -> bool:
        """Refresh market data access permissions"""
        try:
            logger.info("Refreshing market data permissions...")
            
            # Test contract search to determine market data access
            test_contracts = [
                {"symbol": "ETH", "exchange": "PAXOS", "secType": "CRYPTO"},
                {"symbol": "AAPL", "exchange": "SMART", "secType": "STK"},
                {"symbol": "EUR", "exchange": "IDEALPRO", "secType": "CASH"}
            ]
            
            market_data_access = {
                'contract_types': {},
                'data_permissions': {},
                'last_updated': datetime.now().isoformat()
            }
            
            for contract in test_contracts:
                try:
                    search_response = requests.post(
                        f"{self.base_url}/iserver/secdef/search", 
                        json=contract,
                        timeout=10
                    )
                    
                    if search_response.status_code == 200:
                        contracts_found = search_response.json()
                        asset_type = contract['secType']
                        
                        if asset_type == 'CRYPTO':
                            asset_type = 'Cryptocurrency'
                        elif asset_type == 'STK':
                            asset_type = 'US Stocks'
                        elif asset_type == 'CASH':
                            asset_type = 'Forex'
                        
                        market_data_access['contract_types'][asset_type] = {
                            'accessible': len(contracts_found) > 0,
                            'contracts_found': len(contracts_found),
                            'contract': contracts_found[0] if contracts_found else {}
                        }
                        
                except Exception as contract_error:
                    logger.warning(f"Failed to test {contract['symbol']}: {contract_error}")
            
            # Save market data access info
            with open(self.account_dir / "market_data_access.json", 'w') as f:
                json.dump(market_data_access, f, indent=2)
            
            logger.info("✅ Market data permissions refreshed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Failed to refresh market data permissions: {e}")
            return False
    
    def refresh_all(self) -> dict:
        """Refresh all account data components"""
        logger.info("🔄 Starting comprehensive IBKR account data refresh...")
        
        results = {
            'gateway_connected': False,
            'account_summary_updated': False,
            'portfolio_updated': False,
            'market_data_updated': False,
            'timestamp': datetime.now().isoformat(),
            'refresh_successful': False
        }
        
        # Test gateway connection first
        if not self.test_gateway_connection():
            logger.error("❌ IBKR Gateway is not accessible - cannot refresh account data")
            # Mark cached data as stale
            self._mark_cached_data_stale()
            return results
        
        results['gateway_connected'] = True
        logger.info("✅ IBKR Gateway connection confirmed")
        
        # Refresh each component
        results['account_summary_updated'] = self.refresh_account_summary()
        results['portfolio_updated'] = self.refresh_portfolio_positions()
        results['market_data_updated'] = self.refresh_market_data_permissions()
        
        # Determine overall success - ALL components must succeed for live trading
        success_count = sum(1 for v in results.values() if v is True)
        total_critical_tests = 4  # gateway, account, portfolio, market_data
        results['refresh_successful'] = (success_count >= total_critical_tests)
        
        # Save refresh status
        with open(self.account_dir / "last_refresh.json", 'w') as f:
            json.dump(results, f, indent=2)
        
        if results['refresh_successful']:
            logger.info(f"🎉 Account data refresh completed successfully ({success_count}/{total_critical_tests})")
            # Mark data as fresh
            self._mark_cached_data_fresh()
        else:
            logger.error(f"❌ Account data refresh FAILED ({success_count}/{total_critical_tests}) - live trading not safe")
            # Mark cached data as stale/unreliable
            self._mark_cached_data_stale()
        
        return results
    
    def _mark_cached_data_fresh(self):
        """Mark cached account data as fresh and reliable"""
        freshness_marker = {
            'status': 'fresh',
            'last_successful_refresh': datetime.now().isoformat(),
            'safe_for_live_trading': True,
            'data_source': 'live_gateway_api'
        }
        with open(self.account_dir / "data_freshness.json", 'w') as f:
            json.dump(freshness_marker, f, indent=2)
    
    def _mark_cached_data_stale(self):
        """Mark cached account data as stale and unreliable for live trading"""
        freshness_marker = {
            'status': 'stale',
            'last_failed_refresh': datetime.now().isoformat(),
            'safe_for_live_trading': False,
            'data_source': 'cached_data_only',
            'warning': 'LIVE TRADING NOT SAFE - Gateway connection failed'
        }
        with open(self.account_dir / "data_freshness.json", 'w') as f:
            json.dump(freshness_marker, f, indent=2)

def main():
    """Main CLI interface"""
    refresher = IBKRAccountRefresher()
    
    if len(sys.argv) > 1:
        command = sys.argv[1].lower()
        
        if command == "--account":
            refresher.refresh_account_summary()
        elif command == "--portfolio":
            refresher.refresh_portfolio_positions()
        elif command == "--market-data":
            refresher.refresh_market_data_permissions()
        elif command == "--test":
            connected = refresher.test_gateway_connection()
            print(f"Gateway connection: {'✅ Connected' if connected else '❌ Disconnected'}")
        elif command == "--test-failure":
            # Simulate a failure by marking data as stale
            refresher._mark_cached_data_stale()
            print("✅ Data marked as stale for testing fail-fast behavior")
            print("ℹ️  Run statuscheck.py to verify it fails on stale data")
        else:
            print("Usage: python refresh_ibkr_data.py [--account|--portfolio|--market-data|--test|--test-failure]")
            print("       python refresh_ibkr_data.py (refresh all)")
    else:
        # Refresh all by default
        refresher.refresh_all()

if __name__ == "__main__":
    main()
