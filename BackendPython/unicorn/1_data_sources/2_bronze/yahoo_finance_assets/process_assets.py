#!/usr/bin/env python3
"""
Yahoo Finance Assets Processing Pipeline

Main script to process Yahoo Finance asset data (ETH, BTC, Forex) from raw data into
standardized bronze layer datasets suitable for XGBoost alpha models.

This script coordinates all asset processors to generate features at
1-minute, 1-hour, and 1-day intervals for crypto and forex assets.
"""

import os
import sys
import argparse
import logging
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..', '..'))
sys.path.append(project_root)

# Import our processors
from processors.crypto_asset_processor import CryptoAssetProcessor
from processors.forex_asset_processor import ForexAssetProcessor

class YahooFinanceAssetsProcessor:
    """
    Main coordinator for processing all Yahoo Finance asset categories.
    """
    
    SUPPORTED_INTERVALS = ['1m', '1h', '1d']
    
    def __init__(self):
        """Initialize the main asset processor coordinator."""
        # Set up logging
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler('yahoo_finance_assets_processing.log'),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger('YahooFinanceAssetsProcessor')
        
        # Set correct raw data path
        raw_data_path = '/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/yahoo_finance'
        
        # Output path for processed data
        self.output_path = '/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/yahoo_finance_assets/processed_data'
        
        # Initialize all category processors
        self.processors = {
            'crypto': CryptoAssetProcessor(raw_data_path=raw_data_path, output_path=self.output_path),
            'forex': ForexAssetProcessor(raw_data_path=raw_data_path, output_path=self.output_path)
        }
        
        self.logger.info("Initialized Yahoo Finance Assets Processing Pipeline")
        self.logger.info(f"Raw data path: {raw_data_path}")
        self.logger.info(f"Output path: {self.output_path}")
    
    def process_all_categories(self, intervals: List[str] = None, assets: Dict[str, List[str]] = None) -> Dict[str, Dict[str, object]]:
        """
        Process all Yahoo Finance asset categories.
        
        Args:
            intervals: List of time intervals to process (1m, 1h, 1d)
            assets: Dictionary with category-specific asset lists {'crypto': [...], 'forex': [...]}
                   (None = all assets for each category)
            
        Returns:
            Dictionary with processing results for each category
        """
        if intervals is None:
            intervals = ['1d', '1h', '1m']
        
        # Validate intervals
        invalid_intervals = [i for i in intervals if i not in self.SUPPORTED_INTERVALS]
        if invalid_intervals:
            raise ValueError(f"Unsupported intervals: {invalid_intervals}. Supported: {self.SUPPORTED_INTERVALS}")
        
        # If assets not provided, use defaults for each category
        if assets is None:
            assets = {
                'crypto': CryptoAssetProcessor.CRYPTO_ASSETS,
                'forex': ForexAssetProcessor.FOREX_ASSETS
            }
        
        results = {}
        
        self.logger.info("=" * 70)
        self.logger.info("STARTING COMPREHENSIVE YAHOO FINANCE ASSETS PROCESSING")
        self.logger.info("=" * 70)
        self.logger.info(f"Intervals: {intervals}")
        self.logger.info(f"Crypto assets: {assets.get('crypto', [])}")
        self.logger.info(f"Forex assets: {assets.get('forex', [])}")
        
        for category_name, processor in self.processors.items():
            self.logger.info(f"\n🔄 Processing {category_name.upper()} assets...")
            
            try:
                # Get assets for this category
                category_assets = assets.get(category_name, [])
                if not category_assets:
                    self.logger.info(f"⏭️  Skipping {category_name} - no assets specified")
                    continue
                
                # Process the category
                category_results = processor.process(intervals, category_assets)
                
                if category_results:
                    results[category_name] = {
                        'success': True,
                        'data': category_results,
                        'summary': self._generate_category_summary(category_name, category_results)
                    }
                    
                    self.logger.info(f"✅ {category_name} processing completed successfully")
                else:
                    results[category_name] = {
                        'success': False,
                        'data': {},
                        'summary': f"No data available for {category_name}"
                    }
                    
                    self.logger.warning(f"⚠️  {category_name} processing returned no data")
                    
            except Exception as e:
                self.logger.error(f"❌ Error processing {category_name}: {e}")
                results[category_name] = {
                    'success': False,
                    'data': {},
                    'error': str(e),
                    'summary': f"Error processing {category_name}: {e}"
                }
        
        # Generate overall summary
        self._log_overall_summary(results, intervals)
        
        return results
    
    def _generate_category_summary(self, category_name: str, category_results: Dict) -> str:
        """Generate a summary string for a category's results."""
        if not category_results:
            return f"No data processed for {category_name}"
        
        total_intervals = len(category_results)
        total_assets = sum(len(interval_data) for interval_data in category_results.values())
        total_records = 0
        
        for interval_data in category_results.values():
            for asset_data in interval_data.values():
                if isinstance(asset_data, dict) and 'records' in asset_data:
                    total_records += asset_data['records']
        
        return f"{category_name}: {total_assets} assets, {total_intervals} intervals, {total_records} records"
        
        total_files = 0
        total_records = 0
        intervals_processed = set()
        assets_processed = set()
        
        for interval, interval_data in category_results.items():
            if isinstance(interval_data, dict):
                for asset, asset_data in interval_data.items():
                    if isinstance(asset_data, dict) and 'records' in asset_data:
                        total_files += 1
                        total_records += asset_data.get('records', 0)
                        intervals_processed.add(interval)
                        assets_processed.add(asset)
        
        return (f"{category_name}: {total_files} files, {total_records:,} records, "
                f"{len(assets_processed)} assets, {len(intervals_processed)} intervals")
    
    def _log_overall_summary(self, results: Dict, intervals: List[str]):
        """Log overall processing summary."""
        successful_categories = [k for k, v in results.items() if v['success']]
        failed_categories = [k for k, v in results.items() if not v['success']]
        
        self.logger.info("\n" + "=" * 50)
        self.logger.info("📊 YAHOO FINANCE ASSETS PROCESSING SUMMARY")
        self.logger.info("=" * 50)
        self.logger.info(f"✅ Successful categories: {len(successful_categories)}/{len(results)}")
        self.logger.info(f"❌ Failed categories: {len(failed_categories)}/{len(results)}")
        
        if successful_categories:
            self.logger.info("\n🎉 SUCCESSFUL PROCESSING:")
            for category in successful_categories:
                self.logger.info(f"   • {results[category]['summary']}")
        
        if failed_categories:
            self.logger.info("\n⚠️  FAILED PROCESSING:")
            for category in failed_categories:
                self.logger.info(f"   • {results[category]['summary']}")
        
        # Log intervals processed
        self.logger.info(f"\n📅 Intervals processed: {', '.join(intervals)}")
        
        # Log output location
        self.logger.info(f"📁 Processed data location: {self.output_path}")
        
        self.logger.info("=" * 50)
    
    def get_processing_status(self) -> Dict[str, Dict]:
        """Get current processing status and available data."""
        status = {
            'last_run': None,
            'categories': {},
            'output_files': {},
            'data_freshness': {}
        }
        
        # Check for log file to determine last run
        log_file = 'yahoo_finance_assets_processing.log'
        if os.path.exists(log_file):
            status['last_run'] = datetime.fromtimestamp(os.path.getmtime(log_file)).isoformat()
        
        # Check processed data directory
        if os.path.exists(self.output_path):
            for category in ['crypto', 'forex']:
                category_path = os.path.join(self.output_path, category)
                if os.path.exists(category_path):
                    files = [f for f in os.listdir(category_path) if f.endswith('.csv')]
                    status['output_files'][category] = len(files)
                    
                    if files:
                        # Find most recent file for freshness
                        latest_file = max([os.path.join(category_path, f) for f in files], 
                                        key=os.path.getmtime)
                        status['data_freshness'][category] = datetime.fromtimestamp(
                            os.path.getmtime(latest_file)
                        ).isoformat()
        
        return status

def main():
    """Main function for command line usage."""
    parser = argparse.ArgumentParser(description='Yahoo Finance Assets Processing Pipeline')
    parser.add_argument('--intervals', '-i', nargs='+', 
                       choices=['1m', '1h', '1d'], 
                       default=['1d', '1h'], 
                       help='Time intervals to process')
    parser.add_argument('--assets', '-a', nargs='+',
                       help='Specific assets to process (default: all)')
    parser.add_argument('--category', '-c', choices=['crypto', 'forex'],
                       help='Process only specific category')
    parser.add_argument('--status', action='store_true',
                       help='Show processing status')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose logging')
    
    args = parser.parse_args()
    
    # Set logging level
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # Initialize processor
    processor = YahooFinanceAssetsProcessor()
    
    if args.status:
        status = processor.get_processing_status()
        print("\n📊 Yahoo Finance Assets Processing Status:")
        print("=" * 50)
        print(f"Last run: {status.get('last_run', 'Never')}")
        print(f"Output files: {status.get('output_files', {})}")
        print(f"Data freshness: {status.get('data_freshness', {})}")
        return True
    
    # Process assets
    try:
        results = processor.process_all_categories(
            intervals=args.intervals,
            assets=args.assets
        )
        
        # Success if any category succeeded
        success = any(result['success'] for result in results.values())
        
        return success
        
    except Exception as e:
        logging.error(f"Processing failed: {e}")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
