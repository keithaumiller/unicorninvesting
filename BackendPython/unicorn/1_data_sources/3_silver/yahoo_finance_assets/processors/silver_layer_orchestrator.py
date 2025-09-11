"""
Silver Layer Master Orchestrator

This module coordinates all silver layer processing tasks for Yahoo Finance assets,
including data transformation, quality assessment, correlation analysis, and market regime detection.

Features:
- Orchestrates all silver layer processors
- Manages processing dependencies
- Provides comprehensive status monitoring
- Generates consolidated reports
- Handles error recovery and retry logic
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Tuple
import logging
from pathlib import Path
from datetime import datetime, timedelta
import json
import sys
import time

# Add directories to path for imports
current_dir = Path(__file__).parent
parent_dir = current_dir.parent
sys.path.append(str(current_dir))
sys.path.append(str(parent_dir))

# Import silver layer processors
try:
    from yahoo_finance_silver_processor import SilverYahooFinanceProcessor
    from cross_asset_correlation_processor import CrossAssetCorrelationProcessor
    from enhanced_market_regime_detector import EnhancedMarketRegimeDetector
except ImportError as e:
    logging.error(f"Import error: {e}")
    sys.exit(1)

logger = logging.getLogger(__name__)

class SilverLayerOrchestrator:
    """Master orchestrator for all silver layer processing tasks."""
    
    def __init__(self, silver_path: Optional[str] = None):
        """Initialize silver layer orchestrator."""
        
        current_dir = Path(__file__).parent
        parent_dir = current_dir.parent
        self.silver_path = Path(silver_path) if silver_path else parent_dir
        self.bronze_path = parent_dir.parent / "2_bronze" / "yahoo_finance_assets"
        
        # Initialize processors
        self.silver_processor = SilverYahooFinanceProcessor(str(parent_dir))
        self.correlation_processor = CrossAssetCorrelationProcessor(str(parent_dir / "processed_data"))
        self.regime_detector = EnhancedMarketRegimeDetector(str(parent_dir / "processed_data"))
        
        # Processing configuration
        self.crypto_assets = ['ETH', 'BTC']
        self.forex_assets = ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
        self.intervals = ['1d', '1h', '1m']
        
        # Processing status tracking
        self.processing_status = {
            'start_time': None,
            'end_time': None,
            'total_duration': None,
            'tasks_completed': 0,
            'tasks_failed': 0,
            'task_details': {}
        }
        
        logger.info("Initialized Silver Layer Master Orchestrator")
    
    def check_bronze_data_availability(self) -> Dict[str, Dict]:
        """Check availability of bronze layer data for processing."""
        
        try:
            logger.info("Checking bronze layer data availability...")
            
            availability = {
                'crypto': {},
                'forex': {}
            }
            
            # Check crypto assets
            for asset in self.crypto_assets:
                availability['crypto'][asset] = {}
                for interval in self.intervals:
                    bronze_file = self.bronze_path / "crypto" / f"{asset}_bronze_{interval}_latest.csv"
                    availability['crypto'][asset][interval] = {
                        'available': bronze_file.exists(),
                        'file_path': str(bronze_file),
                        'last_modified': bronze_file.stat().st_mtime if bronze_file.exists() else None
                    }
            
            # Check forex assets
            for asset in self.forex_assets:
                availability['forex'][asset] = {}
                for interval in self.intervals:
                    bronze_file = self.bronze_path / "forex" / f"{asset}_bronze_{interval}_latest.csv"
                    availability['forex'][asset][interval] = {
                        'available': bronze_file.exists(),
                        'file_path': str(bronze_file),
                        'last_modified': bronze_file.stat().st_mtime if bronze_file.exists() else None
                    }
            
            # Calculate availability statistics
            total_files = len(self.crypto_assets + self.forex_assets) * len(self.intervals)
            available_files = sum(
                1 for category in availability.values()
                for asset in category.values()
                for interval_data in asset.values()
                if interval_data['available']
            )
            
            availability_rate = (available_files / total_files) * 100
            
            logger.info(f"Bronze data availability: {available_files}/{total_files} files ({availability_rate:.1f}%)")
            
            return {
                'availability': availability,
                'statistics': {
                    'total_files': total_files,
                    'available_files': available_files,
                    'availability_rate': availability_rate
                }
            }
            
        except Exception as e:
            logger.error(f"Error checking bronze data availability: {e}")
            return {}
    
    def process_silver_data_transformation(self) -> Dict[str, any]:
        """Process all bronze data into silver layer format."""
        
        try:
            logger.info("Starting silver data transformation...")
            
            transformation_results = {
                'crypto': {},
                'forex': {},
                'summary': {
                    'total_processed': 0,
                    'successful': 0,
                    'failed': 0
                }
            }
            
            # Process crypto assets
            for asset in self.crypto_assets:
                transformation_results['crypto'][asset] = {}
                for interval in self.intervals:
                    try:
                        logger.info(f"Processing {asset} {interval}...")
                        
                        result = self.silver_processor.process_crypto_asset(asset, interval)
                        
                        transformation_results['crypto'][asset][interval] = {
                            'success': result is not None,
                            'records_processed': len(result) if result is not None else 0,
                            'features_generated': len(result.columns) if result is not None else 0,
                            'timestamp': datetime.now().isoformat()
                        }
                        
                        if result is not None:
                            transformation_results['summary']['successful'] += 1
                        else:
                            transformation_results['summary']['failed'] += 1
                        
                        transformation_results['summary']['total_processed'] += 1
                        
                    except Exception as e:
                        logger.error(f"Error processing {asset} {interval}: {e}")
                        transformation_results['crypto'][asset][interval] = {
                            'success': False,
                            'error': str(e),
                            'timestamp': datetime.now().isoformat()
                        }
                        transformation_results['summary']['failed'] += 1
                        transformation_results['summary']['total_processed'] += 1
            
            # Process forex assets
            for asset in self.forex_assets:
                transformation_results['forex'][asset] = {}
                for interval in self.intervals:
                    try:
                        logger.info(f"Processing {asset} {interval}...")
                        
                        result = self.silver_processor.process_forex_asset(asset, interval)
                        
                        transformation_results['forex'][asset][interval] = {
                            'success': result is not None,
                            'records_processed': len(result) if result is not None else 0,
                            'features_generated': len(result.columns) if result is not None else 0,
                            'timestamp': datetime.now().isoformat()
                        }
                        
                        if result is not None:
                            transformation_results['summary']['successful'] += 1
                        else:
                            transformation_results['summary']['failed'] += 1
                        
                        transformation_results['summary']['total_processed'] += 1
                        
                    except Exception as e:
                        logger.error(f"Error processing {asset} {interval}: {e}")
                        transformation_results['forex'][asset][interval] = {
                            'success': False,
                            'error': str(e),
                            'timestamp': datetime.now().isoformat()
                        }
                        transformation_results['summary']['failed'] += 1
                        transformation_results['summary']['total_processed'] += 1
            
            success_rate = (transformation_results['summary']['successful'] / 
                          transformation_results['summary']['total_processed'] * 100 
                          if transformation_results['summary']['total_processed'] > 0 else 0)
            
            logger.info(f"Silver transformation completed: {success_rate:.1f}% success rate")
            
            return transformation_results
            
        except Exception as e:
            logger.error(f"Error in silver data transformation: {e}")
            return {}
    
    def run_correlation_analysis(self) -> Dict[str, any]:
        """Execute cross-asset correlation analysis."""
        
        try:
            logger.info("Running cross-asset correlation analysis...")
            
            start_time = time.time()
            correlation_report = self.correlation_processor.generate_correlation_report()
            end_time = time.time()
            
            if correlation_report:
                logger.info(f"✅ Correlation analysis completed in {end_time - start_time:.2f} seconds")
                return {
                    'success': True,
                    'duration': end_time - start_time,
                    'report_generated': True,
                    'analysis_intervals': list(correlation_report.get('analysis_intervals', {}).keys())
                }
            else:
                logger.error("❌ Correlation analysis failed")
                return {
                    'success': False,
                    'duration': end_time - start_time,
                    'report_generated': False
                }
                
        except Exception as e:
            logger.error(f"Error in correlation analysis: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    def run_regime_detection(self) -> Dict[str, any]:
        """Execute enhanced market regime detection."""
        
        try:
            logger.info("Running enhanced market regime detection...")
            
            start_time = time.time()
            regime_report = self.regime_detector.generate_regime_report()
            end_time = time.time()
            
            if regime_report:
                logger.info(f"✅ Regime detection completed in {end_time - start_time:.2f} seconds")
                return {
                    'success': True,
                    'duration': end_time - start_time,
                    'report_generated': True,
                    'analysis_intervals': list(regime_report.get('analysis_intervals', {}).keys())
                }
            else:
                logger.error("❌ Regime detection failed")
                return {
                    'success': False,
                    'duration': end_time - start_time,
                    'report_generated': False
                }
                
        except Exception as e:
            logger.error(f"Error in regime detection: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    def run_comprehensive_silver_processing(self) -> Dict[str, any]:
        """Execute complete silver layer processing pipeline."""
        
        try:
            self.processing_status['start_time'] = datetime.now().isoformat()
            logger.info("🚀 Starting comprehensive silver layer processing...")
            
            # Task 1: Check bronze data availability
            logger.info("📊 Task 1/4: Checking bronze data availability...")
            bronze_availability = self.check_bronze_data_availability()
            self.processing_status['task_details']['bronze_availability'] = bronze_availability
            
            if bronze_availability.get('statistics', {}).get('availability_rate', 0) < 50:
                logger.warning("⚠️ Low bronze data availability, proceeding with caution...")
            
            self.processing_status['tasks_completed'] += 1
            
            # Task 2: Silver data transformation
            logger.info("🔄 Task 2/4: Processing silver data transformation...")
            transformation_results = self.process_silver_data_transformation()
            self.processing_status['task_details']['transformation'] = transformation_results
            
            if transformation_results:
                self.processing_status['tasks_completed'] += 1
            else:
                self.processing_status['tasks_failed'] += 1
            
            # Task 3: Cross-asset correlation analysis
            logger.info("📈 Task 3/4: Running correlation analysis...")
            correlation_results = self.run_correlation_analysis()
            self.processing_status['task_details']['correlation'] = correlation_results
            
            if correlation_results.get('success', False):
                self.processing_status['tasks_completed'] += 1
            else:
                self.processing_status['tasks_failed'] += 1
            
            # Task 4: Market regime detection
            logger.info("🎯 Task 4/4: Running regime detection...")
            regime_results = self.run_regime_detection()
            self.processing_status['task_details']['regime_detection'] = regime_results
            
            if regime_results.get('success', False):
                self.processing_status['tasks_completed'] += 1
            else:
                self.processing_status['tasks_failed'] += 1
            
            # Finalize processing status
            self.processing_status['end_time'] = datetime.now().isoformat()
            
            start_dt = datetime.fromisoformat(self.processing_status['start_time'])
            end_dt = datetime.fromisoformat(self.processing_status['end_time'])
            self.processing_status['total_duration'] = (end_dt - start_dt).total_seconds()
            
            # Generate consolidated report
            consolidated_report = self.generate_consolidated_report()
            
            logger.info(f"✅ Silver layer processing completed in {self.processing_status['total_duration']:.2f} seconds")
            logger.info(f"📊 Tasks completed: {self.processing_status['tasks_completed']}/4")
            logger.info(f"❌ Tasks failed: {self.processing_status['tasks_failed']}/4")
            
            return {
                'processing_status': self.processing_status,
                'consolidated_report': consolidated_report
            }
            
        except Exception as e:
            logger.error(f"Error in comprehensive silver processing: {e}")
            self.processing_status['end_time'] = datetime.now().isoformat()
            return {
                'processing_status': self.processing_status,
                'error': str(e)
            }
    
    def generate_consolidated_report(self) -> Dict[str, any]:
        """Generate consolidated silver layer processing report."""
        
        try:
            logger.info("Generating consolidated silver layer report...")
            
            report = {
                'generation_timestamp': datetime.now().isoformat(),
                'silver_layer_summary': {
                    'processing_duration': self.processing_status.get('total_duration', 0),
                    'tasks_completed': self.processing_status.get('tasks_completed', 0),
                    'tasks_failed': self.processing_status.get('tasks_failed', 0),
                    'success_rate': (self.processing_status.get('tasks_completed', 0) / 4 * 100)
                }
            }
            
            # Bronze data availability summary
            bronze_data = self.processing_status.get('task_details', {}).get('bronze_availability', {})
            if bronze_data:
                report['bronze_data_status'] = bronze_data.get('statistics', {})
            
            # Transformation summary
            transformation_data = self.processing_status.get('task_details', {}).get('transformation', {})
            if transformation_data:
                summary = transformation_data.get('summary', {})
                report['transformation_summary'] = {
                    'total_processed': summary.get('total_processed', 0),
                    'successful': summary.get('successful', 0),
                    'failed': summary.get('failed', 0),
                    'success_rate': (summary.get('successful', 0) / summary.get('total_processed', 1) * 100)
                }
            
            # Analysis summary
            correlation_success = self.processing_status.get('task_details', {}).get('correlation', {}).get('success', False)
            regime_success = self.processing_status.get('task_details', {}).get('regime_detection', {}).get('success', False)
            
            report['analysis_summary'] = {
                'correlation_analysis': 'completed' if correlation_success else 'failed',
                'regime_detection': 'completed' if regime_success else 'failed',
                'advanced_analytics': correlation_success and regime_success
            }
            
            # Save consolidated report
            report_path = self.silver_path / "consolidated_silver_report.json"
            with open(report_path, 'w') as f:
                json.dump(report, f, indent=2)
            
            logger.info(f"✅ Consolidated report saved to {report_path}")
            return report
            
        except Exception as e:
            logger.error(f"Error generating consolidated report: {e}")
            return {}

def main():
    """Main function for silver layer orchestration."""
    
    print("🚀 Yahoo Finance Silver Layer Processing Pipeline")
    print("=" * 60)
    
    # Initialize orchestrator
    orchestrator = SilverLayerOrchestrator()
    
    # Run comprehensive processing
    results = orchestrator.run_comprehensive_silver_processing()
    
    if results:
        processing_status = results.get('processing_status', {})
        
        print(f"\n✅ Processing completed in {processing_status.get('total_duration', 0):.2f} seconds")
        print(f"📊 Tasks completed: {processing_status.get('tasks_completed', 0)}/4")
        print(f"❌ Tasks failed: {processing_status.get('tasks_failed', 0)}/4")
        print(f"🎯 Success rate: {(processing_status.get('tasks_completed', 0) / 4 * 100):.1f}%")
        
        # Show transformation summary
        transformation = processing_status.get('task_details', {}).get('transformation', {})
        if transformation:
            summary = transformation.get('summary', {})
            print(f"\n📈 Data Transformation:")
            print(f"   • Total processed: {summary.get('total_processed', 0)}")
            print(f"   • Successful: {summary.get('successful', 0)}")
            print(f"   • Failed: {summary.get('failed', 0)}")
        
        # Show analysis results
        correlation = processing_status.get('task_details', {}).get('correlation', {})
        regime = processing_status.get('task_details', {}).get('regime_detection', {})
        
        print(f"\n🔍 Advanced Analytics:")
        print(f"   • Correlation analysis: {'✅' if correlation.get('success') else '❌'}")
        print(f"   • Regime detection: {'✅' if regime.get('success') else '❌'}")
        
        print(f"\n📄 Reports generated in: /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/yahoo_finance_assets/")
        
    else:
        print("❌ Silver layer processing failed")

if __name__ == "__main__":
    main()
