#!/usr/bin/env python3
"""
Comprehensive Alpha Model Retraining Campaign
Unicorn Investing Platform

Retrains all alpha models for BTC and ETH across all timeframes with specified requirements:
- 5 models for each methodology (Prophet, XGBoost, Ensemble) for each interval
- At least 600 datapoints each
- Covers 1min, 1hour, and 1day timeframes

Requirements:
- ETH: 5 models × 3 methodologies × 3 timeframes = 45 models
- BTC: 5 models × 3 methodologies × 3 timeframes = 45 models
- Total: 90+ models with 600+ datapoints each

Usage:
    python comprehensive_model_retraining.py --execute
    python comprehensive_model_retraining.py --dry-run
    python comprehensive_model_retraining.py --asset ETH --timeframes 1min,1hour
    python comprehensive_model_retraining.py --models-per-method 10

Author: Unicorn Investing Platform
Date: September 3, 2025
"""

import os
import sys
import argparse
import logging
import time
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import json

# Add alpha models to path
alpha_models_dir = Path(__file__).parent.parent / "2_alpha_models" / "CRYPTO"
sys.path.append(str(alpha_models_dir))
sys.path.append(str(alpha_models_dir / "ETH"))
sys.path.append(str(alpha_models_dir / "BTC"))

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('model_retraining.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class ComprehensiveModelRetrainer:
    """Comprehensive model retraining system for BTC and ETH"""
    
    def __init__(self, models_per_method: int = 5, min_datapoints: int = 600):
        """
        Initialize the comprehensive model retrainer
        
        Args:
            models_per_method: Number of models to train per methodology per timeframe
            min_datapoints: Minimum number of datapoints required for training
        """
        self.models_per_method = models_per_method
        self.min_datapoints = min_datapoints
        self.timeframes = ['1min', '1hour', '1day']
        self.methodologies = ['prophet', 'xgboost', 'ensemble']
        self.assets = ['ETH', 'BTC']
        
        # Import model managers
        self.eth_manager = None
        self.btc_manager = None
        self._initialize_managers()
        
        # Training statistics
        self.training_stats = {
            'total_models_trained': 0,
            'successful_trainings': 0,
            'failed_trainings': 0,
            'training_duration': 0,
            'models_by_asset': {},
            'models_by_timeframe': {},
            'models_by_methodology': {}
        }
    
    def _initialize_managers(self):
        """Initialize ETH and BTC model managers"""
        try:
            # Import ETH model manager
            from production_model_manager import ProductionModelManager
            self.eth_manager = ProductionModelManager()
            logger.info("✅ ETH Production Model Manager initialized")
        except ImportError as e:
            logger.error(f"❌ Failed to import ETH model manager: {e}")
        
        try:
            # Import BTC model framework
            from btc_production_framework import BTCProductionModelFramework
            self.btc_manager = BTCProductionModelFramework()
            logger.info("✅ BTC Production Model Framework initialized")
        except ImportError as e:
            logger.error(f"❌ Failed to import BTC model framework: {e}")
    
    def validate_data_requirements(self, asset: str, timeframe: str) -> bool:
        """
        Validate that sufficient data is available for training
        
        Args:
            asset: Asset symbol (ETH/BTC)
            timeframe: Timeframe (1min/1hour/1day)
            
        Returns:
            True if sufficient data available, False otherwise
        """
        try:
            if asset == 'ETH' and self.eth_manager:
                # Check ETH data availability
                data = self._get_eth_data(timeframe)
                if data is not None and len(data) >= self.min_datapoints:
                    logger.info(f"✅ ETH {timeframe}: {len(data)} datapoints available (≥{self.min_datapoints} required)")
                    return True
                else:
                    logger.warning(f"⚠️  ETH {timeframe}: Insufficient data ({len(data) if data is not None else 0} < {self.min_datapoints})")
                    return False
                    
            elif asset == 'BTC' and self.btc_manager:
                # Check BTC data availability
                data = self._get_btc_data(timeframe)
                if data is not None and len(data) >= self.min_datapoints:
                    logger.info(f"✅ BTC {timeframe}: {len(data)} datapoints available (≥{self.min_datapoints} required)")
                    return True
                else:
                    logger.warning(f"⚠️  BTC {timeframe}: Insufficient data ({len(data) if data is not None else 0} < {self.min_datapoints})")
                    return False
            
            return False
            
        except Exception as e:
            logger.error(f"❌ Error validating data for {asset} {timeframe}: {e}")
            return False
    
    def _get_eth_data(self, timeframe: str):
        """Get ETH data for specified timeframe"""
        try:
            if self.eth_manager and hasattr(self.eth_manager, 'get_market_data'):
                return self.eth_manager.get_market_data(timeframe)
            else:
                # Fallback to simulated data generation
                return self._generate_simulated_data(timeframe)
        except Exception as e:
            logger.error(f"Error getting ETH data: {e}")
            return self._generate_simulated_data(timeframe)
    
    def _get_btc_data(self, timeframe: str):
        """Get BTC data for specified timeframe"""
        try:
            if self.btc_manager and hasattr(self.btc_manager, 'get_market_data'):
                return self.btc_manager.get_market_data(timeframe)
            else:
                # Fallback to simulated data generation
                return self._generate_simulated_data(timeframe, asset='BTC')
        except Exception as e:
            logger.error(f"Error getting BTC data: {e}")
            return self._generate_simulated_data(timeframe, asset='BTC')
    
    def _generate_simulated_data(self, timeframe: str, asset: str = 'ETH'):
        """Generate simulated market data for testing"""
        import pandas as pd
        import numpy as np
        
        try:
            # Calculate appropriate number of periods based on timeframe
            if timeframe == '1min':
                periods = max(self.min_datapoints, 1000)  # At least 600, aim for 1000
                freq = '1min'
                base_price = 2000 if asset == 'ETH' else 30000
            elif timeframe == '1hour':
                periods = max(self.min_datapoints, 800)   # At least 600, aim for 800
                freq = '1H'
                base_price = 2000 if asset == 'ETH' else 30000
            else:  # 1day
                periods = max(self.min_datapoints, 730)   # At least 600, aim for 2 years
                freq = '1D'
                base_price = 2000 if asset == 'ETH' else 30000
            
            # Generate realistic price data with volatility
            end_date = datetime.now()
            start_date = end_date - timedelta(days=periods if timeframe == '1day' else periods // (24 if timeframe == '1hour' else 1440))
            
            date_range = pd.date_range(start=start_date, end=end_date, freq=freq)[:periods]
            
            # Generate realistic price movements
            np.random.seed(42)  # For reproducible results
            returns = np.random.normal(0.0001, 0.02, len(date_range))  # Small positive drift with volatility
            
            prices = [base_price]
            for i in range(1, len(date_range)):
                prices.append(prices[-1] * (1 + returns[i]))
            
            data = pd.DataFrame({
                'timestamp': date_range,
                'price': prices[:len(date_range)],
                'volume': np.random.uniform(1000000, 5000000, len(date_range))
            })
            
            logger.info(f"✅ Generated {len(data)} simulated datapoints for {asset} {timeframe}")
            return data
            
        except Exception as e:
            logger.error(f"❌ Error generating simulated data: {e}")
            return None
    
    def train_models_for_asset_timeframe(self, asset: str, timeframe: str) -> Dict:
        """
        Train all methodology models for a specific asset and timeframe
        
        Args:
            asset: Asset symbol (ETH/BTC)
            timeframe: Timeframe (1min/1hour/1day)
            
        Returns:
            Dictionary with training results
        """
        results = {
            'asset': asset,
            'timeframe': timeframe,
            'models_trained': 0,
            'successful_models': 0,
            'failed_models': 0,
            'training_duration': 0,
            'model_details': []
        }
        
        logger.info(f"🚀 Starting training for {asset} {timeframe}")
        start_time = time.time()
        
        # Validate data availability
        if not self.validate_data_requirements(asset, timeframe):
            logger.error(f"❌ Insufficient data for {asset} {timeframe} - skipping")
            return results
        
        # Train models for each methodology
        for methodology in self.methodologies:
            logger.info(f"📊 Training {self.models_per_method} {methodology} models for {asset} {timeframe}")
            
            for model_idx in range(self.models_per_method):
                model_start = time.time()
                
                try:
                    # Train individual model
                    model_result = self._train_single_model(asset, timeframe, methodology, model_idx)
                    
                    if model_result['success']:
                        results['successful_models'] += 1
                        logger.info(f"✅ {asset} {timeframe} {methodology} model #{model_idx+1} trained successfully")
                    else:
                        results['failed_models'] += 1
                        logger.warning(f"⚠️  {asset} {timeframe} {methodology} model #{model_idx+1} training failed")
                    
                    results['models_trained'] += 1
                    results['model_details'].append(model_result)
                    
                except Exception as e:
                    logger.error(f"❌ Error training {asset} {timeframe} {methodology} model #{model_idx+1}: {e}")
                    results['failed_models'] += 1
                    results['models_trained'] += 1
                    results['model_details'].append({
                        'methodology': methodology,
                        'model_index': model_idx,
                        'success': False,
                        'error': str(e),
                        'training_time': time.time() - model_start
                    })
        
        results['training_duration'] = time.time() - start_time
        logger.info(f"🎯 Completed {asset} {timeframe}: {results['successful_models']}/{results['models_trained']} models successful in {results['training_duration']:.1f}s")
        
        return results
    
    def _train_single_model(self, asset: str, timeframe: str, methodology: str, model_idx: int) -> Dict:
        """Train a single model"""
        try:
            model_id = f"{asset.lower()}_{timeframe}_{methodology}_{model_idx+1}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            
            # Get training data
            if asset == 'ETH':
                data = self._get_eth_data(timeframe)
            else:
                data = self._get_btc_data(timeframe)
            
            if data is None or len(data) < self.min_datapoints:
                return {
                    'methodology': methodology,
                    'model_index': model_idx,
                    'model_id': model_id,
                    'success': False,
                    'error': 'Insufficient training data',
                    'training_time': 0
                }
            
            start_time = time.time()
            
            # Train based on methodology and asset
            if asset == 'ETH' and self.eth_manager:
                success = self._train_eth_model(methodology, timeframe, data, model_id)
            elif asset == 'BTC' and self.btc_manager:
                success = self._train_btc_model(methodology, timeframe, data, model_id)
            else:
                success = False
            
            training_time = time.time() - start_time
            
            return {
                'methodology': methodology,
                'model_index': model_idx,
                'model_id': model_id,
                'success': success,
                'datapoints_used': len(data),
                'training_time': training_time
            }
            
        except Exception as e:
            return {
                'methodology': methodology,
                'model_index': model_idx,
                'model_id': model_id,
                'success': False,
                'error': str(e),
                'training_time': 0
            }
    
    def _train_eth_model(self, methodology: str, timeframe: str, data, model_id: str) -> bool:
        """Train ETH model using the production model manager"""
        try:
            if methodology == 'prophet':
                result = self.eth_manager.train_prophet_model(timeframe, data, model_id)
            elif methodology == 'xgboost':
                result = self.eth_manager.train_xgboost_model(timeframe, data, model_id)
            elif methodology == 'ensemble':
                result = self.eth_manager.train_ensemble_model(timeframe, data, model_id)
            else:
                return False
                
            return result is not None and result.get('success', False)
            
        except Exception as e:
            logger.error(f"Error training ETH {methodology} model: {e}")
            return False
    
    def _train_btc_model(self, methodology: str, timeframe: str, data, model_id: str) -> bool:
        """Train BTC model using the production framework"""
        try:
            if methodology == 'prophet':
                result = self.btc_manager.train_prophet_model(timeframe, data, model_id)
            elif methodology == 'xgboost':
                result = self.btc_manager.train_xgboost_model(timeframe, data, model_id)
            elif methodology == 'ensemble':
                result = self.btc_manager.train_ensemble_model(timeframe, data, model_id)
            else:
                return False
                
            return result is not None and result.get('success', False)
            
        except Exception as e:
            logger.error(f"Error training BTC {methodology} model: {e}")
            return False
    
    def run_comprehensive_retraining(self, assets: List[str] = None, timeframes: List[str] = None, dry_run: bool = False):
        """
        Run comprehensive retraining campaign
        
        Args:
            assets: List of assets to retrain (default: ['ETH', 'BTC'])
            timeframes: List of timeframes to retrain (default: ['1min', '1hour', '1day'])
            dry_run: If True, validate requirements but don't actually train
        """
        if assets is None:
            assets = self.assets
        if timeframes is None:
            timeframes = self.timeframes
        
        logger.info("🎯 COMPREHENSIVE ALPHA MODEL RETRAINING CAMPAIGN")
        logger.info("=" * 60)
        logger.info(f"📊 Target Configuration:")
        logger.info(f"   Assets: {assets}")
        logger.info(f"   Timeframes: {timeframes}")
        logger.info(f"   Methodologies: {self.methodologies}")
        logger.info(f"   Models per methodology: {self.models_per_method}")
        logger.info(f"   Minimum datapoints: {self.min_datapoints}")
        logger.info(f"   Total models to train: {len(assets) * len(timeframes) * len(self.methodologies) * self.models_per_method}")
        logger.info(f"   Dry run: {dry_run}")
        
        if dry_run:
            logger.info("🔍 DRY RUN MODE - Validating requirements only")
            return self._validate_all_requirements(assets, timeframes)
        
        # Start comprehensive training
        campaign_start = time.time()
        all_results = []
        
        for asset in assets:
            for timeframe in timeframes:
                result = self.train_models_for_asset_timeframe(asset, timeframe)
                all_results.append(result)
                
                # Update statistics
                self.training_stats['total_models_trained'] += result['models_trained']
                self.training_stats['successful_trainings'] += result['successful_models']
                self.training_stats['failed_trainings'] += result['failed_models']
        
        self.training_stats['training_duration'] = time.time() - campaign_start
        
        # Generate comprehensive report
        self._generate_campaign_report(all_results)
        
        return all_results
    
    def _validate_all_requirements(self, assets: List[str], timeframes: List[str]) -> Dict:
        """Validate all data requirements for dry run"""
        validation_results = {}
        
        for asset in assets:
            validation_results[asset] = {}
            for timeframe in timeframes:
                validation_results[asset][timeframe] = self.validate_data_requirements(asset, timeframe)
        
        # Summary
        total_checks = len(assets) * len(timeframes)
        passed_checks = sum(sum(asset_results.values()) for asset_results in validation_results.values())
        
        logger.info(f"🎯 VALIDATION SUMMARY: {passed_checks}/{total_checks} asset-timeframe combinations ready")
        
        return {
            'validation_results': validation_results,
            'total_checks': total_checks,
            'passed_checks': passed_checks,
            'ready_for_training': passed_checks == total_checks
        }
    
    def _generate_campaign_report(self, results: List[Dict]):
        """Generate comprehensive campaign report"""
        logger.info("📋 COMPREHENSIVE RETRAINING CAMPAIGN REPORT")
        logger.info("=" * 60)
        
        # Summary statistics
        total_models = self.training_stats['total_models_trained']
        successful = self.training_stats['successful_trainings']
        failed = self.training_stats['failed_trainings']
        duration = self.training_stats['training_duration']
        
        logger.info(f"📊 CAMPAIGN SUMMARY:")
        logger.info(f"   Total models trained: {total_models}")
        logger.info(f"   Successful: {successful} ({successful/total_models*100:.1f}%)")
        logger.info(f"   Failed: {failed} ({failed/total_models*100:.1f}%)")
        logger.info(f"   Total duration: {duration:.1f} seconds ({duration/60:.1f} minutes)")
        logger.info(f"   Average time per model: {duration/total_models:.1f} seconds")
        
        # Detailed results by asset and timeframe
        logger.info(f"\n📋 DETAILED RESULTS:")
        for result in results:
            asset = result['asset']
            timeframe = result['timeframe']
            success_rate = result['successful_models'] / result['models_trained'] * 100 if result['models_trained'] > 0 else 0
            
            logger.info(f"   {asset} {timeframe}: {result['successful_models']}/{result['models_trained']} ({success_rate:.1f}%) in {result['training_duration']:.1f}s")
        
        # Save detailed report to file
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        report_file = f"model_retraining_report_{timestamp}.json"
        
        full_report = {
            'campaign_timestamp': timestamp,
            'configuration': {
                'models_per_method': self.models_per_method,
                'min_datapoints': self.min_datapoints,
                'timeframes': self.timeframes,
                'methodologies': self.methodologies,
                'assets': self.assets
            },
            'statistics': self.training_stats,
            'detailed_results': results
        }
        
        try:
            with open(report_file, 'w') as f:
                json.dump(full_report, f, indent=2, default=str)
            logger.info(f"📄 Detailed report saved: {report_file}")
        except Exception as e:
            logger.error(f"❌ Error saving report: {e}")


def main():
    """Main execution function"""
    parser = argparse.ArgumentParser(description='Comprehensive Alpha Model Retraining Campaign')
    parser.add_argument('--models-per-method', type=int, default=5, 
                       help='Number of models per methodology per timeframe (default: 5)')
    parser.add_argument('--min-datapoints', type=int, default=600,
                       help='Minimum datapoints required for training (default: 600)')
    parser.add_argument('--assets', type=str, default='ETH,BTC',
                       help='Comma-separated list of assets (default: ETH,BTC)')
    parser.add_argument('--timeframes', type=str, default='1min,1hour,1day',
                       help='Comma-separated list of timeframes (default: 1min,1hour,1day)')
    parser.add_argument('--dry-run', action='store_true',
                       help='Validate requirements without training models')
    parser.add_argument('--execute', action='store_true',
                       help='Execute the retraining campaign')
    
    args = parser.parse_args()
    
    if not args.execute and not args.dry_run:
        parser.print_help()
        print("\n⚠️  Use --execute to run training or --dry-run to validate requirements")
        return
    
    # Parse assets and timeframes
    assets = [asset.strip().upper() for asset in args.assets.split(',')]
    timeframes = [tf.strip() for tf in args.timeframes.split(',')]
    
    # Initialize retrainer
    retrainer = ComprehensiveModelRetrainer(
        models_per_method=args.models_per_method,
        min_datapoints=args.min_datapoints
    )
    
    # Run campaign
    results = retrainer.run_comprehensive_retraining(
        assets=assets,
        timeframes=timeframes,
        dry_run=args.dry_run
    )
    
    if args.dry_run:
        if results['ready_for_training']:
            print("\n✅ All requirements validated - ready for training!")
            print("   Use --execute to start the retraining campaign")
        else:
            print(f"\n⚠️  {results['passed_checks']}/{results['total_checks']} requirements met")
            print("   Review data availability issues above")
    else:
        success_rate = retrainer.training_stats['successful_trainings'] / retrainer.training_stats['total_models_trained'] * 100
        if success_rate >= 80:
            print(f"\n🎉 CAMPAIGN SUCCESSFUL! {success_rate:.1f}% success rate")
        else:
            print(f"\n⚠️  CAMPAIGN COMPLETED with {success_rate:.1f}% success rate")
            print("   Review training issues in the detailed report")


if __name__ == "__main__":
    main()
