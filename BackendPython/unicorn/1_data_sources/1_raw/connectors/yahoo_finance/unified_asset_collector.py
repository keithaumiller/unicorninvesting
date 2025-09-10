#!/usr/bin/env python3
"""
Unified Yahoo Finance Asset Data Collector
==========================================

Unified data collection for ETH, Forex, and other Yahoo Finance assets.
Supports multiple intervals (1m, 1h, 1d) with organized directory structure.

Integrates with existing data pipeline and cron infrastructure.
"""

import yfinance as yf
import pandas as pd
import os
import sys
from datetime import datetime, timedelta
import logging
import json
from typing import Dict, List, Optional

class UnifiedAssetDataCollector:
    """
    Unified data collector for Yahoo Finance assets (ETH, Forex, Stocks, etc.)
    """
    
    # Asset configurations
    ASSET_CONFIGS = {
        'ETH': {
            'yahoo_symbol': 'ETH-USD',
            'name': 'Ethereum',
            'category': 'crypto',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'BTC': {
            'yahoo_symbol': 'BTC-USD',
            'name': 'Bitcoin',
            'category': 'crypto',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'EURUSD': {
            'yahoo_symbol': 'EURUSD=X',
            'name': 'EUR/USD',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'USDJPY': {
            'yahoo_symbol': 'USDJPY=X',
            'name': 'USD/JPY',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'GBPUSD': {
            'yahoo_symbol': 'GBPUSD=X',
            'name': 'GBP/USD',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'AUDUSD': {
            'yahoo_symbol': 'AUDUSD=X',
            'name': 'AUD/USD',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'USDCAD': {
            'yahoo_symbol': 'USDCAD=X',
            'name': 'USD/CAD',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'USDCHF': {
            'yahoo_symbol': 'USDCHF=X',
            'name': 'USD/CHF',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        },
        'NZDUSD': {
            'yahoo_symbol': 'NZDUSD=X',
            'name': 'NZD/USD',
            'category': 'forex',
            'intervals': {
                '1m': {'period': '5d', 'description': '1-minute (5 days)'},
                '1h': {'period': '1mo', 'description': '1-hour (1 month)'},
                '1d': {'period': '1y', 'description': '1-day (1 year)'}
            }
        }
    }
    
    def __init__(self, base_data_dir=None):
        """
        Initialize the unified asset data collector.
        
        Args:
            base_data_dir: Base directory for data storage
        """
        if base_data_dir is None:
            # Use the standard data directory structure
            project_root = '/workspaces/unicorninvesting'
            self.base_data_dir = os.path.join(project_root, 'BackendPython', 'unicorn', '1_data_sources', '1_raw', 'data')
        else:
            self.base_data_dir = base_data_dir
        
        self.logger = self._setup_logger()
        self._setup_directories()
    
    def _setup_logger(self):
        """Setup logging for the collector."""
        logger = logging.getLogger('UnifiedAssetDataCollector')
        logger.setLevel(logging.INFO)
        
        if not logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
            handler.setFormatter(formatter)
            logger.addHandler(handler)
        
        return logger
    
    def _setup_directories(self):
        """Setup directory structure for all assets."""
        for asset_code, config in self.ASSET_CONFIGS.items():
            category = config['category']
            
            if category == 'crypto':
                asset_dir = os.path.join(self.base_data_dir, 'yahoo_finance', 'crypto', asset_code)
            elif category == 'forex':
                asset_dir = os.path.join(self.base_data_dir, 'yahoo_finance', 'forex', asset_code)
            else:
                asset_dir = os.path.join(self.base_data_dir, 'yahoo_finance', 'stocks', asset_code)
            
            os.makedirs(asset_dir, exist_ok=True)
            
            # Create interval subdirectories
            for interval in config['intervals'].keys():
                interval_dir = os.path.join(asset_dir, interval)
                os.makedirs(interval_dir, exist_ok=True)
    
    def collect_asset_data(self, asset_code: str, interval: str) -> Optional[pd.DataFrame]:
        """
        Collect data for a specific asset and interval.
        
        Args:
            asset_code: Asset code (e.g., 'ETH', 'EURUSD')
            interval: Data interval (e.g., '1m', '1h', '1d')
            
        Returns:
            DataFrame with asset data or None if failed
        """
        if asset_code not in self.ASSET_CONFIGS:
            self.logger.error(f"Unknown asset code: {asset_code}")
            return None
        
        config = self.ASSET_CONFIGS[asset_code]
        if interval not in config['intervals']:
            self.logger.error(f"Unsupported interval {interval} for {asset_code}")
            return None
        
        yahoo_symbol = config['yahoo_symbol']
        name = config['name']
        period = config['intervals'][interval]['period']
        description = config['intervals'][interval]['description']
        
        self.logger.info(f"📊 Collecting {name} ({yahoo_symbol}) data - {description}")
        
        try:
            # Create ticker and fetch data
            ticker = yf.Ticker(yahoo_symbol)
            data = ticker.history(period=period, interval=interval)
            
            if data.empty:
                self.logger.warning(f"No data returned for {asset_code} {interval}")
                return None
            
            # Add metadata
            data['Symbol'] = yahoo_symbol
            data['AssetCode'] = asset_code
            data['Name'] = name
            data['Category'] = config['category']
            data['Interval'] = interval
            data['Source'] = 'yahoo_finance'
            
            # Standardize column names
            data.columns = [col.lower().replace(' ', '_') for col in data.columns]
            
            self.logger.info(f"✅ Retrieved {len(data)} records for {asset_code} {interval}")
            self.logger.info(f"📈 Price range: {data['close'].min():.5f} - {data['close'].max():.5f}")
            self.logger.info(f"📅 Date range: {data.index.min()} to {data.index.max()}")
            
            return data
            
        except Exception as e:
            self.logger.error(f"Error collecting {asset_code} {interval} data: {e}")
            return None
    
    def save_asset_data(self, data: pd.DataFrame, asset_code: str, interval: str) -> Optional[str]:
        """
        Save asset data to organized directory structure.
        
        Args:
            data: DataFrame with market data
            asset_code: Asset code
            interval: Data interval
            
        Returns:
            Path to saved file or None if failed
        """
        if data is None or data.empty:
            self.logger.warning("No data to save")
            return None
        
        try:
            config = self.ASSET_CONFIGS[asset_code]
            category = config['category']
            
            # Determine directory path
            if category == 'crypto':
                asset_dir = os.path.join(self.base_data_dir, 'yahoo_finance', 'crypto', asset_code, interval)
            elif category == 'forex':
                asset_dir = os.path.join(self.base_data_dir, 'yahoo_finance', 'forex', asset_code, interval)
            else:
                asset_dir = os.path.join(self.base_data_dir, 'yahoo_finance', 'stocks', asset_code, interval)
            
            # Generate filename with timestamp
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            filename = f"{asset_code}_{interval}_{timestamp}.csv"
            filepath = os.path.join(asset_dir, filename)
            
            # Save timestamped file
            data.to_csv(filepath)
            
            # Save as latest
            latest_path = os.path.join(asset_dir, 'latest.csv')
            data.to_csv(latest_path)
            
            # File info
            file_size = os.path.getsize(filepath)
            self.logger.info(f"💾 Saved {asset_code} {interval} data: {filepath}")
            self.logger.info(f"📁 File size: {file_size:,} bytes ({file_size/1024:.1f} KB)")
            
            return filepath
            
        except Exception as e:
            self.logger.error(f"Error saving {asset_code} {interval} data: {e}")
            return None
    
    def collect_and_save_asset(self, asset_code: str, interval: str) -> bool:
        """
        Complete workflow: collect and save asset data.
        
        Args:
            asset_code: Asset code
            interval: Data interval
            
        Returns:
            True if successful, False otherwise
        """
        data = self.collect_asset_data(asset_code, interval)
        if data is not None:
            filepath = self.save_asset_data(data, asset_code, interval)
            return filepath is not None
        return False
    
    def collect_all_intervals(self, asset_code: str) -> Dict[str, bool]:
        """
        Collect data for all intervals of a specific asset.
        
        Args:
            asset_code: Asset code
            
        Returns:
            Dict mapping interval to success status
        """
        if asset_code not in self.ASSET_CONFIGS:
            self.logger.error(f"Unknown asset code: {asset_code}")
            return {}
        
        config = self.ASSET_CONFIGS[asset_code]
        results = {}
        
        self.logger.info(f"🚀 Collecting all intervals for {config['name']} ({asset_code})")
        
        for interval in config['intervals'].keys():
            success = self.collect_and_save_asset(asset_code, interval)
            results[interval] = success
            
            if not success:
                self.logger.error(f"Failed to collect {asset_code} {interval} data")
        
        return results
    
    def collect_all_assets(self, interval: str) -> Dict[str, bool]:
        """
        Collect data for all assets at a specific interval.
        
        Args:
            interval: Data interval
            
        Returns:
            Dict mapping asset_code to success status
        """
        results = {}
        
        self.logger.info(f"🌍 Collecting {interval} data for all assets")
        
        for asset_code in self.ASSET_CONFIGS.keys():
            success = self.collect_and_save_asset(asset_code, interval)
            results[asset_code] = success
            
            if not success:
                self.logger.error(f"Failed to collect {asset_code} {interval} data")
        
        return results
    
    def get_asset_summary(self) -> Dict:
        """Get summary of available assets and configurations."""
        summary = {
            'total_assets': len(self.ASSET_CONFIGS),
            'categories': {},
            'intervals': set(),
            'assets': {}
        }
        
        for asset_code, config in self.ASSET_CONFIGS.items():
            category = config['category']
            
            # Count by category
            if category not in summary['categories']:
                summary['categories'][category] = 0
            summary['categories'][category] += 1
            
            # Collect all intervals
            summary['intervals'].update(config['intervals'].keys())
            
            # Asset info
            summary['assets'][asset_code] = {
                'name': config['name'],
                'yahoo_symbol': config['yahoo_symbol'],
                'category': category,
                'intervals': list(config['intervals'].keys())
            }
        
        summary['intervals'] = sorted(list(summary['intervals']))
        
        return summary

def main():
    """Main function for command line usage."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Unified Yahoo Finance Asset Data Collector')
    parser.add_argument('--asset', '-a', help='Asset code (e.g., ETH, EURUSD)')
    parser.add_argument('--interval', '-i', choices=['1m', '1h', '1d'], help='Data interval')
    parser.add_argument('--all-intervals', action='store_true', help='Collect all intervals for specified asset')
    parser.add_argument('--all-assets', action='store_true', help='Collect specified interval for all assets')
    parser.add_argument('--summary', action='store_true', help='Show asset summary')
    
    args = parser.parse_args()
    
    collector = UnifiedAssetDataCollector()
    
    if args.summary:
        summary = collector.get_asset_summary()
        print(f"\n📊 UNIFIED ASSET DATA COLLECTOR SUMMARY")
        print("=" * 60)
        print(f"Total Assets: {summary['total_assets']}")
        print(f"Categories: {summary['categories']}")
        print(f"Available Intervals: {summary['intervals']}")
        print(f"\n🎯 Asset Details:")
        for asset_code, info in summary['assets'].items():
            print(f"   {asset_code:>8} | {info['name']:<15} | {info['category']:<6} | {info['yahoo_symbol']}")
        return True
    
    if args.all_assets and args.interval:
        results = collector.collect_all_assets(args.interval)
        success_count = sum(results.values())
        total_count = len(results)
        
        print(f"\n🎉 Collected {args.interval} data for {success_count}/{total_count} assets")
        return success_count == total_count
    
    if args.all_intervals and args.asset:
        results = collector.collect_all_intervals(args.asset)
        success_count = sum(results.values())
        total_count = len(results)
        
        print(f"\n🎉 Collected all intervals for {args.asset}: {success_count}/{total_count} successful")
        return success_count == total_count
    
    if args.asset and args.interval:
        success = collector.collect_and_save_asset(args.asset, args.interval)
        
        if success:
            print(f"\n✅ Successfully collected {args.asset} {args.interval} data")
            return True
        else:
            print(f"\n❌ Failed to collect {args.asset} {args.interval} data")
            return False
    
    parser.print_help()
    return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
