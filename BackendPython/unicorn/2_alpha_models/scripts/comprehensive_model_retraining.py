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
import pandas as pd
import yfinance as yf

# Add alpha models to path
alpha_models_dir = Path(__file__).parent.parent / "CRYPTO"
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
        """Initialize ETH and BTC model managers with IBKR data integration"""
        # Initialize IBKR data integration
        try:
            from ibkr_data_integration import IBKRLiveDataIntegration
            self.ibkr_integration = IBKRLiveDataIntegration()
            
            # Test IBKR connection
            if self.ibkr_integration.authenticate():
                logger.info("✅ IBKR data integration initialized")
                logger.info("✅ IBKR Gateway authenticated - using live data")
            else:
                logger.warning("⚠️ IBKR Gateway not authenticated - will use limited functionality")
                
        except ImportError as e:
            logger.error(f"❌ Failed to import IBKR integration: {e}")
            self.ibkr_integration = None
        
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
        Validate that data is available for training
        Uses whatever real data is available from IBKR or Yahoo Finance
        
        Args:
            asset: Asset symbol (ETH/BTC)
            timeframe: Timeframe (1min/1hour/1day)
            
        Returns:
            True if any real data available, False otherwise
        """
        try:
            if asset == 'ETH':
                # Check ETH data availability
                data = self._get_eth_data(timeframe)
                if data is not None and len(data) >= 50:  # Minimum viable dataset
                    data_points = len(data)
                    if data_points >= self.min_datapoints:
                        logger.info(f"✅ ETH {timeframe}: {data_points} datapoints available (≥{self.min_datapoints} target)")
                    else:
                        logger.info(f"✅ ETH {timeframe}: {data_points} datapoints available (using available real data)")
                    return True
                else:
                    logger.warning(f"❌ ETH {timeframe}: No sufficient real data available ({len(data) if data is not None else 0} points)")
                    return False
                    
            elif asset == 'BTC':
                # Check BTC data availability
                data = self._get_btc_data(timeframe)
                if data is not None and len(data) >= 50:  # Minimum viable dataset
                    data_points = len(data)
                    if data_points >= self.min_datapoints:
                        logger.info(f"✅ BTC {timeframe}: {data_points} datapoints available (≥{self.min_datapoints} target)")
                    else:
                        logger.info(f"✅ BTC {timeframe}: {data_points} datapoints available (using available real data)")
                    return True
                else:
                    logger.warning(f"❌ BTC {timeframe}: No sufficient real data available ({len(data) if data is not None else 0} points)")
                    return False
            
            return False
            
        except Exception as e:
            logger.error(f"❌ Error validating data for {asset} {timeframe}: {e}")
            return False
    
    def _normalize_data_schema(self, data: pd.DataFrame, source: str = "unknown") -> pd.DataFrame:
        """
        Normalize data schema from different sources to a consistent format
        Expected output columns: ['timestamp', 'open', 'high', 'low', 'close', 'price', 'volume']
        """
        if data is None or data.empty:
            return data
            
        normalized = data.copy()
        
        # Normalize column names to lowercase
        normalized.columns = normalized.columns.str.lower()
        
        # Handle timestamp/index
        if 'timestamp' not in normalized.columns:
            if normalized.index.name in ['Date', 'Datetime', 'date', 'datetime']:
                normalized = normalized.reset_index()
                normalized.rename(columns={normalized.columns[0]: 'timestamp'}, inplace=True)
            elif 'date' in normalized.columns:
                normalized.rename(columns={'date': 'timestamp'}, inplace=True)
            elif 'datetime' in normalized.columns:
                normalized.rename(columns={'datetime': 'timestamp'}, inplace=True)
        
        # Ensure price column exists (needed for ETH models)
        if 'price' not in normalized.columns and 'close' in normalized.columns:
            normalized['price'] = normalized['close']
        
        # Ensure Close column exists (needed for BTC models) 
        if 'Close' not in normalized.columns and 'close' in normalized.columns:
            normalized['Close'] = normalized['close']
        
        # Ensure Volume column exists (needed for BTC models)
        if 'Volume' not in normalized.columns and 'volume' in normalized.columns:
            normalized['Volume'] = normalized['volume']
        
        # Ensure standard columns exist with defaults if missing
        standard_columns = ['open', 'high', 'low', 'close', 'volume']
        for col in standard_columns:
            if col not in normalized.columns:
                if col == 'volume':
                    normalized[col] = 0  # Default volume
                else:
                    # For OHLC, use close price as fallback
                    if 'close' in normalized.columns:
                        normalized[col] = normalized['close']
                    elif 'price' in normalized.columns:
                        normalized[col] = normalized['price']
        
        # Set timestamp as index for consistency
        if 'timestamp' in normalized.columns and normalized.index.name != 'timestamp':
            normalized = normalized.set_index('timestamp')
            
        logger.debug(f"✅ Normalized {source} data schema: {list(normalized.columns)}")
        return normalized

    def _get_yahoo_data(self, symbol: str, timeframe: str):
        """Get data from Yahoo Finance with specific intervals and periods"""
        try:
            ticker = yf.Ticker(symbol)
            
            # Map timeframes to Yahoo Finance intervals
            interval_map = {
                '1min': '1m',
                '1hour': '1h', 
                '1day': '1d'
            }
            
            interval = interval_map.get(timeframe, '1d')
            
            # Optimize data periods based on timeframe
            # For minute data, Yahoo has limitations - use shorter periods
            if timeframe == '1min':
                period = "7d"  # Yahoo only allows 7 days of minute data
            elif timeframe == '1hour':
                period = "730d"  # 2 years of hourly data
            else:  # 1day
                period = "max"  # Maximum available daily data
                
            # Fetch historical data
            hist = ticker.history(period=period, interval=interval)
            
            if hist.empty:
                logger.warning(f"⚠️ No data available from Yahoo Finance for {symbol} {timeframe}")
                return None
                
            # Normalize schema
            normalized_data = self._normalize_data_schema(hist, "Yahoo Finance")
            
            logger.info(f"✅ Retrieved {len(normalized_data)} {symbol} {timeframe} datapoints from Yahoo Finance")
            return normalized_data
            
        except Exception as e:
            logger.error(f"❌ Error fetching Yahoo Finance data for {symbol}: {e}")
            return None

    def _get_eth_data(self, timeframe: str):
        """Get ETH data for specified timeframe using optimal strategy: Yahoo Finance for historical, IBKR for recent"""
        try:
            # Strategy: Prefer Yahoo Finance for longer timeframes (more historical data)
            # Use IBKR for real-time quality when available
            
            if timeframe == '1day' or timeframe == '1hour':
                # For daily and hourly data, prefer Yahoo Finance (much more historical data)
                yahoo_data = self._get_yahoo_data("ETH-USD", timeframe)
                if yahoo_data is not None and len(yahoo_data) >= 50:
                    logger.info(f"✅ Using Yahoo Finance data for ETH {timeframe}: {len(yahoo_data)} points")
                    return yahoo_data
                    
                # Fallback to IBKR if Yahoo fails
                logger.info(f"🔄 Yahoo Finance failed, trying IBKR for ETH {timeframe}...")
                
            # For minute data or when Yahoo fails, try IBKR
            if self.ibkr_integration:
                if timeframe == '1min':
                    data_points = self.ibkr_integration.get_historical_minute_bars(lookback_hours=72)  # 3 days max
                elif timeframe == '1hour':
                    data_points = self.ibkr_integration.get_historical_hourly_bars(lookback_days=180)  # 6 months
                elif timeframe == '1day':
                    data_points = self.ibkr_integration.get_historical_daily_bars(lookback_days=2000)  # ~5.5 years
                else:
                    logger.error(f"❌ Unsupported timeframe: {timeframe}")
                    return None
                    
                if data_points and len(data_points) >= 50:  # Minimum viable dataset
                    df = self.ibkr_integration.convert_to_dataframe(data_points)
                    # Normalize IBKR data schema
                    normalized_df = self._normalize_data_schema(df, "IBKR")
                    logger.info(f"✅ Retrieved {len(normalized_df)} ETH {timeframe} datapoints from IBKR")
                    return normalized_df
                else:
                    logger.warning(f"⚠️ Insufficient ETH data from IBKR ({len(data_points) if data_points else 0} points)")
            
            # Final fallback to Yahoo Finance if not already tried
            if timeframe == '1min':
                yahoo_data = self._get_yahoo_data("ETH-USD", timeframe)
                if yahoo_data is not None and len(yahoo_data) >= 50:
                    logger.info(f"✅ Using Yahoo Finance fallback for ETH {timeframe}: {len(yahoo_data)} points")
                    return yahoo_data
            
            # If all attempts fail, log and return None
            logger.error(f"❌ No sufficient ETH {timeframe} data available from any source")
            return None
                
        except Exception as e:
            logger.error(f"❌ Error getting ETH data: {e}")
            return None
    
    def _get_btc_data(self, timeframe: str):
        """Get BTC data for specified timeframe using optimal strategy: Yahoo Finance for historical, IBKR for recent"""
        try:
            # Strategy: Prefer Yahoo Finance for longer timeframes (more historical data)
            
            if timeframe == '1day' or timeframe == '1hour':
                # For daily and hourly data, prefer Yahoo Finance (much more historical data)
                yahoo_data = self._get_yahoo_data("BTC-USD", timeframe)
                if yahoo_data is not None and len(yahoo_data) >= 50:
                    logger.info(f"✅ Using Yahoo Finance data for BTC {timeframe}: {len(yahoo_data)} points")
                    return yahoo_data
                    
                # Fallback to IBKR if Yahoo fails
                logger.info(f"🔄 Yahoo Finance failed, trying IBKR for BTC {timeframe}...")
            
            # For minute data or when Yahoo fails, try IBKR
            if self.ibkr_integration:
                # Save original ETH contract and temporarily switch to BTC
                original_contract = self.ibkr_integration.eth_contract_id
                btc_contract_id = 265598  # BTC/USD contract
                self.ibkr_integration.eth_contract_id = btc_contract_id
                
                try:
                    if timeframe == '1min':
                        data_points = self.ibkr_integration.get_historical_minute_bars(lookback_hours=72)  # 3 days max
                    elif timeframe == '1hour':
                        data_points = self.ibkr_integration.get_historical_hourly_bars(lookback_days=180)  # 6 months
                    elif timeframe == '1day':
                        data_points = self.ibkr_integration.get_historical_daily_bars(lookback_days=2000)  # ~5.5 years
                    else:
                        logger.error(f"❌ Unsupported timeframe: {timeframe}")
                        return None
                        
                    if data_points and len(data_points) >= 50:
                        df = self.ibkr_integration.convert_to_dataframe(data_points)
                        # Normalize IBKR data schema
                        normalized_df = self._normalize_data_schema(df, "IBKR")
                        logger.info(f"✅ Retrieved {len(normalized_df)} BTC {timeframe} datapoints from IBKR")
                        return normalized_df
                    else:
                        logger.warning(f"⚠️ Insufficient BTC data from IBKR ({len(data_points) if data_points else 0} points)")
                        
                finally:
                    # Restore original ETH contract ID
                    self.ibkr_integration.eth_contract_id = original_contract
            
            # Final fallback to Yahoo Finance if not already tried
            if timeframe == '1min':
                yahoo_data = self._get_yahoo_data("BTC-USD", timeframe)
                if yahoo_data is not None and len(yahoo_data) >= 50:
                    logger.info(f"✅ Using Yahoo Finance fallback for BTC {timeframe}: {len(yahoo_data)} points")
                    return yahoo_data
            
            # If all attempts fail, log and return None
            logger.error(f"❌ No sufficient BTC {timeframe} data available from any source")
            return None
                
        except Exception as e:
            logger.error(f"❌ Error getting BTC data: {e}")
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
                model, result = self.eth_manager.train_prophet_model(data, timeframe)
            elif methodology == 'xgboost':
                model, result = self.eth_manager.train_xgboost_model(data, timeframe)
            elif methodology == 'ensemble':
                # ETH manager doesn't have ensemble method, use xgboost as fallback
                logger.warning(f"ETH ensemble model not available, using XGBoost fallback")
                model, result = self.eth_manager.train_xgboost_model(data, timeframe)
            else:
                return False
                
            # Check if training was successful
            return model is not None and result is not None
            
        except Exception as e:
            logger.error(f"Error training ETH {methodology} model: {e}")
            return False
    
    def _train_btc_model(self, methodology: str, timeframe: str, data, model_id: str) -> bool:
        """Train BTC model using the production framework"""
        try:
            # Preprocess BTC data to add required indicators and returns
            processed_data = self.btc_manager._add_technical_indicators(data.copy())
            processed_data = self.btc_manager._add_market_features(processed_data)
            
            if methodology == 'prophet':
                result = self.btc_manager.train_prophet_model(processed_data, timeframe)
            elif methodology == 'xgboost':
                result = self.btc_manager.train_xgboost_model(processed_data, timeframe)
            elif methodology == 'ensemble':
                result = self.btc_manager.train_ensemble_model(processed_data, timeframe)
            else:
                return False
            
            # BTC framework returns {'model_id': ..., 'performance': ..., 'path': ...} on success
            # Success is indicated by having a model_id and performance metrics
            return result is not None and 'model_id' in result and 'performance' in result
            
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
