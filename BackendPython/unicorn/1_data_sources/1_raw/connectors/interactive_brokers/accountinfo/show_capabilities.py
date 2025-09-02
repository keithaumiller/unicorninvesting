#!/usr/bin/env python3
"""
IBKR Account Capabilities Summary

Quick reference script to display current account capabilities and limitations
for development planning and reference.

Author: Unicorn Investing Platform
Date: September 2, 2025
"""

import json
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

class AccountCapabilitiesSummary:
    """Quick access to IBKR account capabilities and limitations."""
    
    def __init__(self):
        """Initialize with account info directory."""
        self.base_dir = Path(__file__).parent
        self.data_files = {
            'capabilities': self.base_dir / 'account_capabilities.json',
            'market_data': self.base_dir / 'market_data_access.json',
            'api_endpoints': self.base_dir / 'api_endpoints.json',
            'risk_params': self.base_dir / 'risk_parameters.json'
        }
        
        # Load all data
        self.data = {}
        for key, file_path in self.data_files.items():
            if file_path.exists():
                with open(file_path, 'r') as f:
                    self.data[key] = json.load(f)
            else:
                self.data[key] = {}
    
    def print_header(self, title: str):
        """Print formatted section header."""
        print(f"\n{'='*60}")
        print(f"🔍 {title}")
        print('='*60)
    
    def print_authentication_status(self):
        """Display authentication status."""
        self.print_header("AUTHENTICATION STATUS")
        
        if 'capabilities' in self.data and 'auth_status' in self.data['capabilities']:
            auth = self.data['capabilities']['auth_status']
            authenticated = auth.get('authenticated', False)
            connected = auth.get('connected', False)
            
            print(f"✅ Authenticated: {'Yes' if authenticated else 'No'}")
            print(f"✅ Connected: {'Yes' if connected else 'No'}")
            
            if 'serverInfo' in auth:
                server_info = auth['serverInfo']
                print(f"🖥️  Server: {server_info.get('serverName', 'Unknown')}")
                print(f"📋 Version: {server_info.get('serverVersion', 'Unknown')}")
        else:
            print("❌ Authentication data not available")
    
    def print_account_summary(self):
        """Display account summary information."""
        self.print_header("ACCOUNT SUMMARY")
        
        if 'capabilities' in self.data and 'account_summary' in self.data['capabilities']:
            accounts = self.data['capabilities']['account_summary'].get('accounts', [])
            
            for account in accounts:
                print(f"🏦 Account ID: {account.get('id', 'Unknown')}")
                print(f"👤 Name: {account.get('accountTitle', 'Unknown')}")
                print(f"📊 Type: {account.get('type', 'Unknown')}")
                print(f"💼 Trading Type: {account.get('tradingType', 'Unknown')}")
                print(f"💰 Currency: {account.get('currency', 'Unknown')}")
                print(f"🏢 Entity: {account.get('ibEntity', 'Unknown')}")
                print(f"🔐 Brokerage Access: {'Yes' if account.get('brokerageAccess') else 'No'}")
        else:
            print("❌ Account summary not available")
    
    def print_market_data_capabilities(self):
        """Display market data access capabilities."""
        self.print_header("MARKET DATA CAPABILITIES")
        
        if 'market_data' in self.data and 'contract_types' in self.data['market_data']:
            contract_types = self.data['market_data']['contract_types']
            
            print("Asset Type".ljust(20) + "Status".ljust(15) + "Contracts Found")
            print("-" * 50)
            
            for asset_type, details in contract_types.items():
                accessible = "✅ Available" if details.get('accessible', False) else "❌ Limited"
                contract_count = details.get('contracts_found', 0)
                print(f"{asset_type.ljust(20)}{accessible.ljust(15)}{contract_count}")
        else:
            print("❌ Market data capabilities not available")
    
    def print_trading_capabilities(self):
        """Display trading capabilities and limitations."""
        self.print_header("TRADING CAPABILITIES")
        
        if 'capabilities' in self.data and 'trading_permissions' in self.data['capabilities']:
            capabilities = self.data['capabilities']['trading_permissions'].get('trading_capabilities', {})
            
            print("Feature".ljust(25) + "Status".ljust(15) + "HTTP Code")
            print("-" * 55)
            
            for feature, details in capabilities.items():
                accessible = "✅ Available" if details.get('accessible', False) else "❌ Limited"
                status_code = details.get('status_code', 'Unknown')
                print(f"{feature[:24].ljust(25)}{accessible.ljust(15)}{status_code}")
        else:
            print("❌ Trading capabilities not available")
    
    def print_api_endpoints(self):
        """Display API endpoint accessibility."""
        self.print_header("API ENDPOINT ACCESS")
        
        if 'api_endpoints' in self.data:
            categories = self.data['api_endpoints']
            
            for category, endpoints in categories.items():
                if category == 'timestamp':
                    continue
                    
                print(f"\n📁 {category.upper()}")
                print("-" * 30)
                
                for endpoint, details in endpoints.items():
                    if isinstance(details, dict):
                        accessible = "✅" if details.get('accessible', False) else "❌"
                        status_code = details.get('status_code', 'Unknown')
                        print(f"  {accessible} {endpoint} (HTTP {status_code})")
        else:
            print("❌ API endpoint data not available")
    
    def print_key_limitations(self):
        """Display key limitations for development planning."""
        self.print_header("KEY LIMITATIONS & CONSIDERATIONS")
        
        limitations = []
        
        # Check authentication
        if 'capabilities' in self.data:
            auth = self.data['capabilities'].get('auth_status', {})
            if not auth.get('authenticated', False):
                limitations.append("🚨 Authentication required for API access")
        
        # Check trading capabilities
        if 'capabilities' in self.data:
            trading = self.data['capabilities'].get('trading_permissions', {}).get('trading_capabilities', {})
            limited_features = [feature for feature, details in trading.items() 
                              if not details.get('accessible', False)]
            if limited_features:
                limitations.append(f"⚠️  Limited trading features: {', '.join(limited_features[:3])}")
        
        # Check market data
        if 'market_data' in self.data:
            market_data = self.data['market_data'].get('contract_types', {})
            limited_assets = [asset for asset, details in market_data.items() 
                            if not details.get('accessible', False)]
            if limited_assets:
                limitations.append(f"⚠️  Limited market data: {', '.join(limited_assets)}")
        
        if limitations:
            for limitation in limitations:
                print(f"• {limitation}")
        else:
            print("✅ No significant limitations identified for current development needs")
        
        print("\n💡 DEVELOPMENT RECOMMENDATIONS:")
        print("• Focus on available endpoints with HTTP 200 status")
        print("• Use contract search for instrument discovery")
        print("• Implement proper error handling for limited endpoints")
        print("• Consider paper trading mode for development and testing")
        print("• Monitor account permissions for production deployment")
    
    def print_data_freshness(self):
        """Display when data was last collected."""
        print(f"\n{'='*60}")
        
        timestamps = []
        for data_type, data in self.data.items():
            if isinstance(data, dict) and 'timestamp' in data:
                timestamps.append(data['timestamp'])
        
        if timestamps:
            latest = max(timestamps)
            print(f"📅 Data last collected: {latest}")
        else:
            print("📅 Data collection timestamp not available")
        
        print(f"📁 Data location: {self.base_dir}")
        print(f"🔄 To update: python collect_account_info.py")
    
    def show_quick_summary(self):
        """Display quick summary of key capabilities."""
        print("🦄 IBKR Account Capabilities Quick Summary")
        self.print_authentication_status()
        self.print_account_summary()
        self.print_market_data_capabilities()
        self.print_key_limitations()
        self.print_data_freshness()
    
    def show_detailed_summary(self):
        """Display comprehensive summary of all capabilities."""
        print("🦄 IBKR Account Capabilities Detailed Summary")
        self.print_authentication_status()
        self.print_account_summary()
        self.print_market_data_capabilities()
        self.print_trading_capabilities()
        self.print_api_endpoints()
        self.print_key_limitations()
        self.print_data_freshness()

def main():
    """Main execution function."""
    summary = AccountCapabilitiesSummary()
    
    if len(sys.argv) > 1 and sys.argv[1] == '--detailed':
        summary.show_detailed_summary()
    else:
        summary.show_quick_summary()
        print(f"\n💡 For detailed analysis, run: python {Path(__file__).name} --detailed")

if __name__ == "__main__":
    main()
