#!/usr/bin/env python3
"""
Economic Indicators Processing Pipeline

Main script to process all economic indicators from raw FRED/BEA data into
standardized bronze layer datasets suitable for XGBoost alpha models.

This script coordinates all processor classes to generate features at
1-minute, 1-hour, and 1-day intervals.
"""

import os
import sys
import argparse
import logging
from datetime import datetime
from typing import Dict, List

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..', '..'))
sys.path.append(project_root)

# Import our processors
from processors.economic_growth_processor import EconomicGrowthProcessor
from processors.consumer_business_processor import ConsumerBusinessProcessor
from processors.international_trade_processor import InternationalTradeProcessor
from processors.monetary_policy_processor import MonetaryPolicyProcessor

class EconomicIndicatorsProcessor:
    """
    Main coordinator for processing all economic indicator categories.
    """
    
    def __init__(self):
        """Initialize the main processor coordinator."""
        # Set up logging
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler('economic_indicators_processing.log'),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger('EconomicIndicatorsProcessor')
        
        # Set correct raw data path
        raw_data_path = '/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/economic_indicators'
        
        # Initialize all category processors
        self.processors = {
            'economic_growth': EconomicGrowthProcessor(raw_data_path=raw_data_path),
            'consumer_business': ConsumerBusinessProcessor(raw_data_path=raw_data_path),
            'international_trade': InternationalTradeProcessor(raw_data_path=raw_data_path),
            'monetary_policy': MonetaryPolicyProcessor(raw_data_path=raw_data_path)
        }
        
        self.logger.info("Initialized Economic Indicators Processing Pipeline")
    
    def process_all_categories(self, intervals: List[str] = None) -> Dict[str, Dict[str, object]]:
        """
        Process all economic indicator categories.
        
        Args:
            intervals: List of time intervals to process
            
        Returns:
            Dictionary with processing results for each category
        """
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
        
        results = {}
        
        self.logger.info("=" * 60)
        self.logger.info("STARTING COMPREHENSIVE ECONOMIC INDICATORS PROCESSING")
        self.logger.info("=" * 60)
        
        for category_name, processor in self.processors.items():
            self.logger.info(f"\n🔄 Processing {category_name.upper()} indicators...")
            
            try:
                # Process the category
                category_results = processor.process(intervals)
                
                if category_results:
                    results[category_name] = {
                        'success': True,
                        'data': category_results,
                        'summary': self._generate_summary(category_name, category_results)
                    }
                    
                    self.logger.info(f"✅ {category_name} processing completed successfully")
                else:
                    results[category_name] = {
                        'success': False,
                        'data': {},
                        'summary': f"No data available for {category_name}"
                    }
                    
                    self.logger.warning(f"⚠️  {category_name} processing completed with no data")
                
            except Exception as e:
                results[category_name] = {
                    'success': False,
                    'data': {},
                    'error': str(e),
                    'summary': f"Error processing {category_name}: {str(e)}"
                }
                
                self.logger.error(f"❌ {category_name} processing failed: {str(e)}")
        
        # Generate overall summary
        self._log_overall_summary(results)
        
        return results
    
    def _generate_summary(self, category_name: str, results: Dict) -> str:
        """
        Generate a summary for a category's processing results.
        
        Args:
            category_name: Name of the processed category
            results: Processing results dictionary
            
        Returns:
            Summary string
        """
        summary_lines = [f"{category_name.upper()} PROCESSING SUMMARY:"]
        
        total_features = 0
        total_observations = 0
        
        for interval, df in results.items():
            if hasattr(df, 'shape'):
                observations = df.shape[0]
                features = df.shape[1]
                total_observations = max(total_observations, observations)
                total_features += features
                
                summary_lines.append(f"  📊 {interval}: {observations:,} observations, {features} features")
        
        summary_lines.append(f"  🎯 Total unique features: {total_features}")
        summary_lines.append(f"  📈 Max observations: {total_observations:,}")
        
        return "\n".join(summary_lines)
    
    def _log_overall_summary(self, results: Dict):
        """
        Log overall processing summary.
        
        Args:
            results: Complete processing results
        """
        self.logger.info("\n" + "=" * 60)
        self.logger.info("ECONOMIC INDICATORS PROCESSING COMPLETE")
        self.logger.info("=" * 60)
        
        successful_categories = []
        failed_categories = []
        
        for category_name, result in results.items():
            if result['success']:
                successful_categories.append(category_name)
                self.logger.info(f"✅ {category_name}: SUCCESS")
            else:
                failed_categories.append(category_name)
                self.logger.error(f"❌ {category_name}: FAILED")
        
        self.logger.info(f"\n📊 SUMMARY:")
        self.logger.info(f"  • Successful categories: {len(successful_categories)}/{len(results)}")
        self.logger.info(f"  • Failed categories: {len(failed_categories)}/{len(results)}")
        
        if successful_categories:
            self.logger.info(f"  • Success: {', '.join(successful_categories)}")
        
        if failed_categories:
            self.logger.info(f"  • Failed: {', '.join(failed_categories)}")
        
        # Log detailed summaries for successful categories
        for category_name in successful_categories:
            if 'summary' in results[category_name]:
                self.logger.info(f"\n{results[category_name]['summary']}")
    
    def process_single_category(self, category_name: str, intervals: List[str] = None):
        """
        Process a single category of economic indicators.
        
        Args:
            category_name: Name of category to process
            intervals: List of time intervals to process
        """
        if category_name not in self.processors:
            self.logger.error(f"Unknown category: {category_name}")
            self.logger.info(f"Available categories: {list(self.processors.keys())}")
            return
        
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
        
        self.logger.info(f"Processing single category: {category_name}")
        
        processor = self.processors[category_name]
        results = processor.process(intervals)
        
        if results:
            summary = self._generate_summary(category_name, results)
            self.logger.info(f"\n{summary}")
        else:
            self.logger.warning(f"No results generated for {category_name}")
    
    def get_latest_indicators(self) -> Dict[str, Dict]:
        """
        Get latest values for key economic indicators across all categories.
        
        Returns:
            Dictionary with latest indicator values
        """
        latest_indicators = {}
        
        # Economic Growth indicators
        growth_processor = self.processors['economic_growth']
        try:
            latest_indicators['gdp_growth'] = growth_processor.get_latest_gdp_growth()
            latest_indicators['gdp_regime'] = growth_processor.get_gdp_regime()
        except:
            pass
        
        # Consumer & Business indicators
        consumer_processor = self.processors['consumer_business']
        try:
            latest_indicators['consumer_sentiment'] = consumer_processor.get_latest_consumer_sentiment()
            latest_indicators['consumer_spending_growth'] = consumer_processor.get_consumer_spending_growth()
        except:
            pass
        
        # International Trade indicators
        trade_processor = self.processors['international_trade']
        try:
            latest_indicators['trade_balance'] = trade_processor.get_latest_trade_balance()
            latest_indicators['trade_regime'] = trade_processor.get_trade_regime()
        except:
            pass
        
        # Monetary Policy indicators
        monetary_processor = self.processors['monetary_policy']
        try:
            latest_indicators['fed_rate'] = monetary_processor.get_current_fed_rate()
            latest_indicators['yield_curve_status'] = monetary_processor.get_yield_curve_status()
        except:
            pass
        
        return latest_indicators

def main():
    """Main function for command-line usage."""
    parser = argparse.ArgumentParser(description='Process Economic Indicators for Alpha Models')
    parser.add_argument('--category', type=str, choices=['economic_growth', 'consumer_business', 
                                                        'international_trade', 'monetary_policy', 'all'],
                       default='all', help='Category to process')
    parser.add_argument('--intervals', nargs='+', choices=['1_minute', '1_hour', '1_day'],
                       default=['1_day', '1_hour', '1_minute'], help='Time intervals to process')
    parser.add_argument('--latest', action='store_true', help='Show latest indicator values')
    
    args = parser.parse_args()
    
    # Initialize processor
    processor = EconomicIndicatorsProcessor()
    
    if args.latest:
        # Show latest indicator values
        latest = processor.get_latest_indicators()
        print("\n📊 LATEST ECONOMIC INDICATORS:")
        print("=" * 40)
        for indicator, value in latest.items():
            if value is not None:
                print(f"{indicator}: {value}")
        return
    
    # Process indicators
    if args.category == 'all':
        processor.process_all_categories(args.intervals)
    else:
        processor.process_single_category(args.category, args.intervals)

if __name__ == '__main__':
    main()
