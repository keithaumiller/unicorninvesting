#!/usr/bin/env python3
"""
IBKR Account Information Collector

This script comprehensively collects all available account information from the IBKR API
including capabilities, limitations, permissions, and access levels.

Author: Unicorn Investing Platform
Date: September 2, 2025
"""

import json
import requests
import datetime
import os
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class IBKRAccountInfoCollector:
    """Comprehensive IBKR account information collector."""
    
    def __init__(self, base_url: str = "http://localhost:5000", timeout: int = 30):
        """Initialize the collector with IBKR Gateway connection parameters."""
        self.base_url = base_url
        self.timeout = timeout
        self.session = requests.Session()
        
        # Set up output directories
        self.base_dir = Path(__file__).parent
        self.reports_dir = self.base_dir / "reports"
        self.historical_dir = self.base_dir / "historical"
        
        # Create date-specific historical directory
        today = datetime.datetime.now().strftime("%Y-%m-%d")
        self.today_dir = self.historical_dir / today
        self.today_dir.mkdir(parents=True, exist_ok=True)
        
        # Account information storage
        self.account_info = {}
        
    def test_connection(self) -> bool:
        """Test connection to IBKR Gateway."""
        try:
            response = self.session.get(f"{self.base_url}/v1/api/iserver/auth/status", timeout=self.timeout)
            if response.status_code == 200:
                logger.info("✅ IBKR Gateway connection successful")
                return True
            else:
                logger.error(f"❌ IBKR Gateway connection failed: HTTP {response.status_code}")
                return False
        except Exception as e:
            logger.error(f"❌ IBKR Gateway connection error: {e}")
            return False
    
    def collect_auth_status(self) -> Dict[str, Any]:
        """Collect authentication status and session information."""
        logger.info("🔐 Collecting authentication status...")
        
        try:
            response = self.session.get(f"{self.base_url}/v1/api/iserver/auth/status", timeout=self.timeout)
            if response.status_code == 200:
                auth_data = response.json()
                logger.info(f"   Authentication: {'✅ Authenticated' if auth_data.get('authenticated') else '❌ Not Authenticated'}")
                return auth_data
            else:
                logger.warning(f"   Auth status request failed: HTTP {response.status_code}")
                return {"error": f"HTTP {response.status_code}", "authenticated": False}
        except Exception as e:
            logger.error(f"   Auth status collection failed: {e}")
            return {"error": str(e), "authenticated": False}
    
    def collect_account_summary(self) -> Dict[str, Any]:
        """Collect basic account summary information."""
        logger.info("📊 Collecting account summary...")
        
        try:
            response = self.session.get(f"{self.base_url}/v1/api/portfolio/accounts", timeout=self.timeout)
            if response.status_code == 200:
                accounts_data = response.json()
                logger.info(f"   Found {len(accounts_data)} account(s)")
                
                # Get detailed info for each account
                detailed_accounts = []
                for account in accounts_data:
                    account_id = account.get('id', account.get('accountId', 'unknown'))
                    logger.info(f"   Processing account: {account_id}")
                    
                    # Get account metadata
                    try:
                        meta_response = self.session.get(f"{self.base_url}/v1/api/portfolio/{account_id}/meta", timeout=self.timeout)
                        if meta_response.status_code == 200:
                            account['metadata'] = meta_response.json()
                        else:
                            account['metadata'] = {"error": f"HTTP {meta_response.status_code}"}
                    except Exception as e:
                        account['metadata'] = {"error": str(e)}
                    
                    detailed_accounts.append(account)
                
                return {"accounts": detailed_accounts, "timestamp": datetime.datetime.now().isoformat()}
            else:
                logger.warning(f"   Account summary request failed: HTTP {response.status_code}")
                return {"error": f"HTTP {response.status_code}", "accounts": []}
        except Exception as e:
            logger.error(f"   Account summary collection failed: {e}")
            return {"error": str(e), "accounts": []}
    
    def collect_trading_permissions(self) -> Dict[str, Any]:
        """Collect trading permissions and capabilities."""
        logger.info("🎯 Collecting trading permissions...")
        
        permissions = {
            "timestamp": datetime.datetime.now().isoformat(),
            "trading_capabilities": {},
            "market_access": {},
            "order_types": {},
            "errors": []
        }
        
        # Test various endpoints to determine capabilities
        test_endpoints = [
            ("/v1/api/iserver/secdef/search", "Contract Search"),
            ("/v1/api/iserver/marketdata/snapshot", "Market Data Snapshots"),
            ("/v1/api/portfolio/positions/0", "Portfolio Positions"),
            ("/v1/api/iserver/account/orders", "Order Management"),
            ("/v1/api/iserver/scanner/params", "Market Scanner"),
            ("/v1/api/hmds/history", "Historical Data"),
            ("/v1/api/iserver/account/trades", "Trade History")
        ]
        
        for endpoint, description in test_endpoints:
            try:
                response = self.session.get(f"{self.base_url}{endpoint}", timeout=10)
                permissions["trading_capabilities"][description] = {
                    "endpoint": endpoint,
                    "status_code": response.status_code,
                    "accessible": response.status_code not in [401, 403, 404],
                    "response_type": response.headers.get('content-type', 'unknown')
                }
                
                if response.status_code == 200:
                    logger.info(f"   ✅ {description}: Accessible")
                else:
                    logger.info(f"   ⚠️  {description}: HTTP {response.status_code}")
                    
            except Exception as e:
                permissions["trading_capabilities"][description] = {
                    "endpoint": endpoint,
                    "error": str(e),
                    "accessible": False
                }
                permissions["errors"].append(f"{description}: {str(e)}")
                logger.warning(f"   ❌ {description}: {e}")
        
        return permissions
    
    def collect_market_data_permissions(self) -> Dict[str, Any]:
        """Collect market data access permissions and limitations."""
        logger.info("📈 Collecting market data permissions...")
        
        market_data = {
            "timestamp": datetime.datetime.now().isoformat(),
            "data_subscriptions": {},
            "real_time_access": {},
            "contract_types": {},
            "errors": []
        }
        
        # Test market data access for different asset types
        test_contracts = [
            {"symbol": "AAPL", "secType": "STK", "exchange": "SMART", "description": "US Stocks"},
            {"symbol": "EUR", "secType": "CASH", "currency": "USD", "description": "Forex"},
            {"symbol": "ETH", "secType": "CRYPTO", "exchange": "PAXOS", "description": "Cryptocurrency"},
            {"symbol": "ES", "secType": "FUT", "exchange": "CME", "description": "Futures"},
            {"symbol": "SPY", "secType": "OPT", "exchange": "SMART", "description": "Options"}
        ]
        
        for contract in test_contracts:
            try:
                # Try to search for the contract
                search_params = {"symbol": contract["symbol"]}
                response = self.session.post(
                    f"{self.base_url}/v1/api/iserver/secdef/search",
                    json=search_params,
                    timeout=10
                )
                
                market_data["contract_types"][contract["description"]] = {
                    "contract": contract,
                    "search_status": response.status_code,
                    "accessible": response.status_code == 200,
                    "contracts_found": len(response.json()) if response.status_code == 200 else 0
                }
                
                if response.status_code == 200:
                    logger.info(f"   ✅ {contract['description']}: {len(response.json())} contracts found")
                else:
                    logger.info(f"   ⚠️  {contract['description']}: HTTP {response.status_code}")
                    
            except Exception as e:
                market_data["contract_types"][contract["description"]] = {
                    "contract": contract,
                    "error": str(e),
                    "accessible": False
                }
                market_data["errors"].append(f"{contract['description']}: {str(e)}")
                logger.warning(f"   ❌ {contract['description']}: {e}")
        
        return market_data
    
    def collect_api_endpoints(self) -> Dict[str, Any]:
        """Collect information about available API endpoints."""
        logger.info("🔗 Collecting API endpoint information...")
        
        # Comprehensive list of IBKR API endpoints to test
        api_endpoints = {
            "timestamp": datetime.datetime.now().isoformat(),
            "authentication": {},
            "portfolio": {},
            "market_data": {},
            "trading": {},
            "account": {},
            "scanner": {},
            "historical": {},
            "utilities": {}
        }
        
        endpoint_categories = {
            "authentication": [
                "/v1/api/iserver/auth/status",
                "/v1/api/iserver/reauthenticate",
                "/v1/api/logout"
            ],
            "portfolio": [
                "/v1/api/portfolio/accounts",
                "/v1/api/portfolio/subaccounts",
                "/v1/api/portfolio/account/summary",
                "/v1/api/portfolio/positions/0",
                "/v1/api/portfolio/allocation"
            ],
            "market_data": [
                "/v1/api/iserver/marketdata/snapshot",
                "/v1/api/iserver/marketdata/history",
                "/v1/api/md/snapshot",
                "/v1/api/iserver/scanner/run"
            ],
            "trading": [
                "/v1/api/iserver/account/orders",
                "/v1/api/iserver/account/trades",
                "/v1/api/iserver/reply",
                "/v1/api/iserver/account/order/preview"
            ],
            "account": [
                "/v1/api/iserver/accounts",
                "/v1/api/iserver/account/pnl/partitioned",
                "/v1/api/iserver/account/information"
            ],
            "scanner": [
                "/v1/api/iserver/scanner/params",
                "/v1/api/iserver/scanner/run"
            ],
            "historical": [
                "/v1/api/hmds/history",
                "/v1/api/hmds/scanner"
            ],
            "utilities": [
                "/v1/api/tickle",
                "/v1/api/sso/validate",
                "/v1/api/one/user"
            ]
        }
        
        for category, endpoints in endpoint_categories.items():
            api_endpoints[category] = {}
            
            for endpoint in endpoints:
                try:
                    response = self.session.get(f"{self.base_url}{endpoint}", timeout=5)
                    api_endpoints[category][endpoint] = {
                        "status_code": response.status_code,
                        "accessible": response.status_code not in [404],
                        "authenticated_required": response.status_code == 401,
                        "content_type": response.headers.get('content-type', 'unknown'),
                        "response_size": len(response.content)
                    }
                    
                    status = "✅" if response.status_code == 200 else "⚠️" if response.status_code in [401, 403] else "❌"
                    logger.info(f"   {status} {endpoint}: HTTP {response.status_code}")
                    
                except Exception as e:
                    api_endpoints[category][endpoint] = {
                        "error": str(e),
                        "accessible": False
                    }
                    logger.warning(f"   ❌ {endpoint}: {e}")
        
        return api_endpoints
    
    def collect_risk_parameters(self) -> Dict[str, Any]:
        """Collect account risk parameters and limitations."""
        logger.info("⚠️  Collecting risk parameters...")
        
        risk_data = {
            "timestamp": datetime.datetime.now().isoformat(),
            "account_risk": {},
            "trading_limits": {},
            "margin_requirements": {},
            "errors": []
        }
        
        # Try to get account information with risk parameters
        risk_endpoints = [
            ("/v1/api/iserver/account/information", "Account Information"),
            ("/v1/api/portfolio/account/summary", "Account Summary"),
            ("/v1/api/iserver/account/pnl/partitioned", "P&L Information")
        ]
        
        for endpoint, description in risk_endpoints:
            try:
                response = self.session.get(f"{self.base_url}{endpoint}", timeout=10)
                
                if response.status_code == 200:
                    risk_data[description.lower().replace(" ", "_")] = response.json()
                    logger.info(f"   ✅ {description}: Data collected")
                else:
                    risk_data[description.lower().replace(" ", "_")] = {
                        "error": f"HTTP {response.status_code}"
                    }
                    logger.info(f"   ⚠️  {description}: HTTP {response.status_code}")
                    
            except Exception as e:
                risk_data[description.lower().replace(" ", "_")] = {"error": str(e)}
                risk_data["errors"].append(f"{description}: {str(e)}")
                logger.warning(f"   ❌ {description}: {e}")
        
        return risk_data
    
    def save_data(self, filename: str, data: Dict[str, Any]) -> None:
        """Save collected data to JSON files."""
        # Save to current directory
        current_file = self.base_dir / filename
        with open(current_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
        
        # Save to historical directory
        historical_file = self.today_dir / filename
        with open(historical_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
        
        logger.info(f"   💾 Saved: {filename}")
    
    def generate_reports(self) -> None:
        """Generate human-readable reports from collected data."""
        logger.info("📋 Generating human-readable reports...")
        
        # Generate account analysis report
        self.generate_account_analysis()
        self.generate_capabilities_matrix()
        self.generate_limitations_summary()
    
    def generate_account_analysis(self) -> None:
        """Generate comprehensive account analysis report."""
        report_content = f"""# IBKR Account Analysis Report

**Generated**: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## 📊 Account Summary

"""
        
        # Add account summary if available
        if 'account_summary' in self.account_info:
            accounts = self.account_info['account_summary'].get('accounts', [])
            for account in accounts:
                account_id = account.get('id', account.get('accountId', 'Unknown'))
                account_type = account.get('type', 'Unknown')
                
                report_content += f"""### Account: {account_id}
- **Type**: {account_type}
- **Status**: {"✅ Active" if account.get('status') == 'Active' else "⚠️ Status Unknown"}

"""
        
        # Add authentication status
        if 'auth_status' in self.account_info:
            auth = self.account_info['auth_status']
            authenticated = auth.get('authenticated', False)
            report_content += f"""## 🔐 Authentication Status

- **Authenticated**: {"✅ Yes" if authenticated else "❌ No"}
- **Session**: {"✅ Active" if auth.get('connected', False) else "❌ Inactive"}

"""
        
        # Add trading capabilities
        if 'trading_permissions' in self.account_info:
            capabilities = self.account_info['trading_permissions'].get('trading_capabilities', {})
            report_content += """## 🎯 Trading Capabilities

| Feature | Status | Access Level |
|---------|---------|--------------|
"""
            for feature, details in capabilities.items():
                status = "✅ Available" if details.get('accessible', False) else "❌ Limited"
                http_status = details.get('status_code', 'Unknown')
                report_content += f"| {feature} | {status} | HTTP {http_status} |\n"
        
        # Save report
        report_file = self.reports_dir / "account_analysis.md"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(report_content)
        
        logger.info("   📋 Account analysis report generated")
    
    def generate_capabilities_matrix(self) -> None:
        """Generate capabilities matrix report."""
        report_content = f"""# IBKR Capabilities Matrix

**Generated**: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## 📈 Market Data Access

"""
        
        if 'market_data_permissions' in self.account_info:
            contract_types = self.account_info['market_data_permissions'].get('contract_types', {})
            
            report_content += """| Asset Type | Search Available | Contracts Found | Status |
|------------|------------------|-----------------|--------|
"""
            for asset_type, details in contract_types.items():
                accessible = "✅ Yes" if details.get('accessible', False) else "❌ No"
                contract_count = details.get('contracts_found', 0)
                status_code = details.get('search_status', 'Unknown')
                report_content += f"| {asset_type} | {accessible} | {contract_count} | HTTP {status_code} |\n"
        
        # Save report
        report_file = self.reports_dir / "capabilities_matrix.md"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(report_content)
        
        logger.info("   📊 Capabilities matrix report generated")
    
    def generate_limitations_summary(self) -> None:
        """Generate limitations and restrictions summary."""
        report_content = f"""# IBKR Account Limitations Summary

**Generated**: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## 🚨 Identified Limitations

"""
        
        limitations = []
        
        # Check authentication limitations
        if 'auth_status' in self.account_info:
            if not self.account_info['auth_status'].get('authenticated', False):
                limitations.append("❌ **Authentication Required**: Account is not authenticated for API access")
        
        # Check trading limitations
        if 'trading_permissions' in self.account_info:
            capabilities = self.account_info['trading_permissions'].get('trading_capabilities', {})
            for feature, details in capabilities.items():
                if not details.get('accessible', False):
                    status_code = details.get('status_code', 'Unknown')
                    limitations.append(f"⚠️  **{feature}**: Limited access (HTTP {status_code})")
        
        # Check market data limitations
        if 'market_data_permissions' in self.account_info:
            contract_types = self.account_info['market_data_permissions'].get('contract_types', {})
            for asset_type, details in contract_types.items():
                if not details.get('accessible', False):
                    limitations.append(f"❌ **{asset_type} Data**: Not accessible or restricted")
        
        if limitations:
            for limitation in limitations:
                report_content += f"- {limitation}\n"
        else:
            report_content += "✅ No significant limitations identified.\n"
        
        report_content += f"""

## 📋 Recommendations

1. **Authentication**: Ensure IBKR Gateway is authenticated via web interface
2. **Permissions**: Verify account has necessary trading permissions enabled
3. **Market Data**: Check market data subscriptions and permissions
4. **Paper Trading**: Confirm account is in appropriate trading mode
5. **API Access**: Verify API access is enabled in IBKR account settings

## 🔄 Next Steps

1. Address any authentication issues
2. Enable missing permissions through IBKR account management
3. Subscribe to required market data feeds
4. Test trading capabilities in paper trading mode
5. Monitor and update capabilities as account permissions change
"""
        
        # Save report
        report_file = self.reports_dir / "limitations_summary.md"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(report_content)
        
        logger.info("   🚨 Limitations summary report generated")
    
    def run_full_collection(self) -> None:
        """Run complete account information collection process."""
        logger.info("🚀 Starting comprehensive IBKR account information collection...")
        
        # Test connection first
        if not self.test_connection():
            logger.error("❌ Cannot connect to IBKR Gateway. Please ensure it's running.")
            return
        
        # Collect all account information
        logger.info("\n📋 Collecting account information...")
        
        self.account_info['auth_status'] = self.collect_auth_status()
        self.account_info['account_summary'] = self.collect_account_summary()
        self.account_info['trading_permissions'] = self.collect_trading_permissions()
        self.account_info['market_data_permissions'] = self.collect_market_data_permissions()
        self.account_info['api_endpoints'] = self.collect_api_endpoints()
        self.account_info['risk_parameters'] = self.collect_risk_parameters()
        
        # Save all collected data
        logger.info("\n💾 Saving collected data...")
        
        self.save_data('account_capabilities.json', {
            'auth_status': self.account_info['auth_status'],
            'account_summary': self.account_info['account_summary'],
            'trading_permissions': self.account_info['trading_permissions']
        })
        
        self.save_data('api_endpoints.json', self.account_info['api_endpoints'])
        self.save_data('market_data_access.json', self.account_info['market_data_permissions'])
        self.save_data('risk_parameters.json', self.account_info['risk_parameters'])
        
        # Generate comprehensive data file
        self.save_data('complete_account_info.json', self.account_info)
        
        # Generate reports
        self.generate_reports()
        
        logger.info(f"\n✅ Account information collection complete!")
        logger.info(f"📁 Data saved to: {self.base_dir}")
        logger.info(f"📋 Reports generated in: {self.reports_dir}")
        logger.info(f"🕒 Historical snapshot: {self.today_dir}")

def main():
    """Main execution function."""
    collector = IBKRAccountInfoCollector()
    collector.run_full_collection()

if __name__ == "__main__":
    main()
