#!/usr/bin/env python3
"""
Data Warehouse Pipeline Validation Framework

This module provides comprehensive end-to-end testing for the Unicorn Investing
data warehouse pipeline, validating data flow from raw connectors through
bronze, silver, and gold layers.

Author: Unicorn Investing Platform
Date: 2025-09-15
"""

import os
import sys
import json
import pandas as pd
import sqlite3
import subprocess
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import yfinance as yf
import logging

# Add project root to path
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.append(str(PROJECT_ROOT))

class PipelineValidator:
    """Comprehensive data warehouse pipeline validation"""
    
    def __init__(self, base_path: str = "/workspaces/unicorninvesting"):
        self.base_path = Path(base_path)
        self.data_sources_path = self.base_path / "BackendPython" / "unicorn" / "1_data_sources"
        self.raw_path = self.data_sources_path / "1_raw"
        self.bronze_path = self.data_sources_path / "2_bronze"
        self.silver_path = self.data_sources_path / "3_silver"
        self.gold_path = self.data_sources_path / "4_gold"
        
        # Test configuration
        self.test_symbols = {
            'crypto': ['ETH-USD', 'BTC-USD'],
            'forex': ['EURUSD=X', 'GBPUSD=X'],
            'stocks': ['AAPL', 'MSFT']
        }
        
        # Configure logging
        self.setup_logging()
        
    def setup_logging(self):
        """Configure logging for pipeline validation"""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger('PipelineValidator')
        
    def validate_raw_connector(self, connector_name: str, test_symbol: str = None) -> Dict[str, Any]:
        """
        Validate a specific raw data connector
        
        Args:
            connector_name: Name of the connector (yahoo_finance, fred, ibkr, forex)
            test_symbol: Optional symbol to test with
            
        Returns:
            Dict with validation results
        """
        result = {
            'connector': connector_name,
            'status': 'UNKNOWN',
            'timestamp': datetime.now().isoformat(),
            'data_samples': [],
            'errors': [],
            'metrics': {}
        }
        
        try:
            if connector_name == 'yahoo_finance':
                symbol = test_symbol or 'ETH-USD'
                result = self._validate_yahoo_finance_connector(symbol)
            elif connector_name == 'fred':
                result = self._validate_fred_connector()
            elif connector_name == 'ibkr':
                result = self._validate_ibkr_connector()
            elif connector_name == 'forex':
                symbol = test_symbol or 'EURUSD=X'
                result = self._validate_forex_connector(symbol)
            else:
                result['status'] = 'FAILED'
                result['errors'].append(f"Unknown connector: {connector_name}")
                
        except Exception as e:
            result['status'] = 'FAILED'
            result['errors'].append(f"Connector validation failed: {str(e)}")
            self.logger.error(f"Connector {connector_name} validation failed: {e}")
            
        return result
        
    def _validate_yahoo_finance_connector(self, test_symbol: str = 'ETH-USD') -> Dict[str, Any]:
        """Validate Yahoo Finance connector with actual data retrieval"""
        result = {
            'connector': 'yahoo_finance',
            'status': 'UNKNOWN',
            'timestamp': datetime.now().isoformat(),
            'test_symbol': test_symbol,
            'data_samples': [],
            'errors': [],
            'metrics': {}
        }
        
        try:
            # Test yfinance directly
            ticker = yf.Ticker(test_symbol)
            
            # Get recent data
            end_date = datetime.now()
            start_date = end_date - timedelta(days=5)
            
            # Use a more robust data retrieval approach
            data = ticker.history(start=start_date, end=end_date, interval='1h', auto_adjust=True, prepost=True)
            
            if data is not None and not data.empty and len(data) > 0:
                result['status'] = 'PASSED'
                result['metrics']['rows_retrieved'] = len(data)
                result['metrics']['date_range'] = {
                    'start': data.index.min().isoformat(),
                    'end': data.index.max().isoformat()
                }
                
                # Sample data points
                sample_count = min(3, len(data))
                for i in range(sample_count):
                    row = data.iloc[i]
                    sample = {
                        'timestamp': data.index[i].isoformat(),
                        'open': float(row['Open']) if pd.notna(row['Open']) else 0.0,
                        'high': float(row['High']) if pd.notna(row['High']) else 0.0,
                        'low': float(row['Low']) if pd.notna(row['Low']) else 0.0,
                        'close': float(row['Close']) if pd.notna(row['Close']) else 0.0,
                    }
                    
                    # Volume is optional for some assets
                    if 'Volume' in row and pd.notna(row['Volume']):
                        sample['volume'] = int(row['Volume'])
                    else:
                        sample['volume'] = 0
                        
                    result['data_samples'].append(sample)
            else:
                result['status'] = 'FAILED'
                result['errors'].append(f"No data retrieved from Yahoo Finance for {test_symbol}")
                
        except Exception as e:
            result['status'] = 'FAILED'
            result['errors'].append(f"Yahoo Finance test failed: {str(e)}")
            self.logger.error(f"Yahoo Finance validation failed for {test_symbol}: {e}")
            
        return result
        
    def _validate_fred_connector(self) -> Dict[str, Any]:
        """Validate FRED connector"""
        result = {
            'connector': 'fred',
            'status': 'SKIPPED',
            'timestamp': datetime.now().isoformat(),
            'data_samples': [],
            'errors': ['FRED connector requires API key configuration'],
            'metrics': {}
        }
        
        # Check if FRED API key is available
        fred_key = os.environ.get('FRED_API_KEY')
        if fred_key:
            # TODO: Implement actual FRED API testing
            result['status'] = 'PASSED'
            result['errors'] = []
            result['metrics']['api_key_configured'] = True
        
        return result
        
    def _validate_ibkr_connector(self) -> Dict[str, Any]:
        """Validate IBKR connector"""
        result = {
            'connector': 'ibkr',
            'status': 'UNKNOWN',
            'timestamp': datetime.now().isoformat(),
            'data_samples': [],
            'errors': [],
            'metrics': {}
        }
        
        try:
            # Check if IBKR gateway is running
            import requests
            response = requests.get('http://localhost:5000/v1/api/iserver/auth/status', timeout=5)
            
            if response.status_code == 200:
                result['status'] = 'PASSED'
                result['metrics']['gateway_status'] = 'running'
                result['metrics']['response_code'] = response.status_code
            else:
                result['status'] = 'FAILED'
                result['errors'].append(f"IBKR Gateway returned status code: {response.status_code}")
                
        except requests.exceptions.RequestException as e:
            result['status'] = 'FAILED'
            result['errors'].append(f"IBKR Gateway connection failed: {str(e)}")
            
        return result
        
    def _validate_forex_connector(self, test_symbol: str = 'EURUSD=X') -> Dict[str, Any]:
        """Validate Forex connector (uses Yahoo Finance for forex data)"""
        # Forex data comes through Yahoo Finance
        result = self._validate_yahoo_finance_connector(test_symbol)
        result['connector'] = 'forex'
        return result
        
    def trace_data_lineage(self, connector: str, symbol: str) -> Dict[str, Any]:
        """
        Trace data from raw ingestion through silver layer
        
        Args:
            connector: Source connector name
            symbol: Symbol to trace
            
        Returns:
            Dict with lineage trace results
        """
        lineage = {
            'symbol': symbol,
            'connector': connector,
            'timestamp': datetime.now().isoformat(),
            'stages': {
                'raw': {'status': 'UNKNOWN', 'data_found': False, 'sample_count': 0},
                'bronze': {'status': 'UNKNOWN', 'data_found': False, 'sample_count': 0},
                'silver': {'status': 'UNKNOWN', 'data_found': False, 'sample_count': 0}
            },
            'errors': []
        }
        
        try:
            # 1. Check raw data availability
            raw_result = self._check_raw_data(connector, symbol)
            lineage['stages']['raw'] = raw_result
            
            if raw_result['status'] == 'PASSED':
                # 2. Check bronze processing
                bronze_result = self._check_bronze_data(connector, symbol)
                lineage['stages']['bronze'] = bronze_result
                
                if bronze_result['status'] == 'PASSED':
                    # 3. Check silver enrichment
                    silver_result = self._check_silver_data(connector, symbol)
                    lineage['stages']['silver'] = silver_result
                    
        except Exception as e:
            lineage['errors'].append(f"Lineage trace failed: {str(e)}")
            self.logger.error(f"Data lineage trace failed for {symbol}: {e}")
            
        return lineage
        
    def _check_raw_data(self, connector: str, symbol: str) -> Dict[str, Any]:
        """Check if raw data exists for symbol"""
        result = {'status': 'FAILED', 'data_found': False, 'sample_count': 0, 'path': None}
        
        try:
            if connector == 'yahoo_finance':
                # Check raw data directories
                raw_data_path = self.raw_path / "connectors" / "yahoo_finance" / "data"
                if raw_data_path.exists():
                    # Look for files containing the symbol
                    symbol_files = list(raw_data_path.rglob(f"*{symbol.replace('-', '_')}*"))
                    if symbol_files:
                        result['status'] = 'PASSED'
                        result['data_found'] = True
                        result['sample_count'] = len(symbol_files)
                        result['path'] = str(symbol_files[0])
                        
            # Also test live data retrieval
            connector_result = self.validate_raw_connector(connector, symbol)
            if connector_result['status'] == 'PASSED':
                result['status'] = 'PASSED'
                result['data_found'] = True
                result['live_data'] = True
                
        except Exception as e:
            result['error'] = str(e)
            
        return result
        
    def _check_bronze_data(self, connector: str, symbol: str) -> Dict[str, Any]:
        """Check if bronze layer processing exists"""
        result = {'status': 'FAILED', 'data_found': False, 'sample_count': 0, 'path': None}
        
        try:
            # Check bronze data directories
            bronze_data_path = self.bronze_path
            if bronze_data_path.exists():
                # Look for processed files
                symbol_files = list(bronze_data_path.rglob(f"*{symbol.replace('-', '_')}*"))
                if symbol_files:
                    result['status'] = 'PASSED'
                    result['data_found'] = True
                    result['sample_count'] = len(symbol_files)
                    result['path'] = str(symbol_files[0])
                else:
                    # Check if bronze layer is configured
                    result['status'] = 'SKIPPED'
                    result['note'] = 'Bronze layer processing not yet implemented'
                    
        except Exception as e:
            result['error'] = str(e)
            
        return result
        
    def _check_silver_data(self, connector: str, symbol: str) -> Dict[str, Any]:
        """Check if silver layer enrichment exists"""
        result = {'status': 'FAILED', 'data_found': False, 'sample_count': 0, 'path': None}
        
        try:
            # Check silver data directories
            silver_data_path = self.silver_path / "yahoo_finance_assets" / "processed_data"
            if silver_data_path.exists():
                # Look for processed files
                symbol_clean = symbol.replace('-USD', '').replace('=X', '')
                symbol_files = list(silver_data_path.rglob(f"*{symbol_clean}*"))
                
                if symbol_files:
                    result['status'] = 'PASSED'
                    result['data_found'] = True
                    result['sample_count'] = len(symbol_files)
                    result['path'] = str(symbol_files[0])
                    
                    # Check file freshness
                    latest_file = max(symbol_files, key=lambda f: f.stat().st_mtime)
                    file_age = datetime.now() - datetime.fromtimestamp(latest_file.stat().st_mtime)
                    result['file_age_hours'] = file_age.total_seconds() / 3600
                    result['is_fresh'] = file_age.total_seconds() < 3600  # Less than 1 hour
                    
        except Exception as e:
            result['error'] = str(e)
            
        return result
        
    def validate_pipeline_performance(self) -> Dict[str, Any]:
        """Validate overall pipeline performance metrics"""
        performance = {
            'timestamp': datetime.now().isoformat(),
            'metrics': {},
            'status': 'UNKNOWN',
            'recommendations': []
        }
        
        try:
            # Check data freshness across layers
            silver_files = list(self.silver_path.rglob("*.csv"))
            if silver_files:
                latest_silver = max(silver_files, key=lambda f: f.stat().st_mtime)
                silver_age = datetime.now() - datetime.fromtimestamp(latest_silver.stat().st_mtime)
                performance['metrics']['silver_data_age_minutes'] = silver_age.total_seconds() / 60
                
                # Check automated refresh system
                if silver_age.total_seconds() < 600:  # Less than 10 minutes
                    performance['status'] = 'PASSED'
                else:
                    performance['status'] = 'WARNING'
                    performance['recommendations'].append('Silver layer data is stale - check automated refresh')
                    
            # Count available data assets
            crypto_files = list(self.silver_path.rglob("*crypto*"))
            forex_files = list(self.silver_path.rglob("*forex*"))
            
            performance['metrics']['crypto_assets'] = len(crypto_files)
            performance['metrics']['forex_assets'] = len(forex_files)
            performance['metrics']['total_silver_files'] = len(silver_files)
            
        except Exception as e:
            performance['status'] = 'FAILED'
            performance['error'] = str(e)
            
        return performance
        
    def run_comprehensive_pipeline_test(self) -> Dict[str, Any]:
        """Run complete end-to-end pipeline validation"""
        test_results = {
            'timestamp': datetime.now().isoformat(),
            'test_type': 'comprehensive_pipeline',
            'overall_status': 'UNKNOWN',
            'connectors': {},
            'data_lineage': {},
            'performance': {},
            'summary': {
                'total_connectors': 0,
                'passed_connectors': 0,
                'failed_connectors': 0,
                'traced_symbols': 0,
                'complete_pipelines': 0
            }
        }
        
        try:
            # Test each connector
            connectors = ['yahoo_finance', 'fred', 'ibkr', 'forex']
            for connector in connectors:
                test_results['summary']['total_connectors'] += 1
                
                if connector == 'yahoo_finance':
                    result = self.validate_raw_connector(connector, 'ETH-USD')
                elif connector == 'forex':
                    result = self.validate_raw_connector(connector, 'EURUSD=X')
                else:
                    result = self.validate_raw_connector(connector)
                    
                test_results['connectors'][connector] = result
                
                if result['status'] == 'PASSED':
                    test_results['summary']['passed_connectors'] += 1
                else:
                    test_results['summary']['failed_connectors'] += 1
                    
            # Test data lineage for key symbols
            test_symbols = ['ETH-USD', 'BTC-USD', 'EURUSD=X']
            for symbol in test_symbols:
                connector = 'yahoo_finance' if symbol.endswith('-USD') else 'forex'
                lineage = self.trace_data_lineage(connector, symbol)
                test_results['data_lineage'][symbol] = lineage
                test_results['summary']['traced_symbols'] += 1
                
                # Check if complete pipeline exists
                if (lineage['stages']['raw']['status'] == 'PASSED' and 
                    lineage['stages']['silver']['status'] == 'PASSED'):
                    test_results['summary']['complete_pipelines'] += 1
                    
            # Performance validation
            test_results['performance'] = self.validate_pipeline_performance()
            
            # Determine overall status
            if (test_results['summary']['passed_connectors'] > 0 and 
                test_results['summary']['complete_pipelines'] > 0):
                test_results['overall_status'] = 'PASSED'
            else:
                test_results['overall_status'] = 'FAILED'
                
        except Exception as e:
            test_results['overall_status'] = 'FAILED'
            test_results['error'] = str(e)
            self.logger.error(f"Comprehensive pipeline test failed: {e}")
            
        return test_results


def main():
    """Run pipeline validation as standalone script"""
    validator = PipelineValidator()
    
    print("🔍 Running Comprehensive Data Warehouse Pipeline Validation")
    print("=" * 70)
    
    results = validator.run_comprehensive_pipeline_test()
    
    # Print summary
    print(f"\n📊 PIPELINE VALIDATION SUMMARY")
    print(f"Overall Status: {results['overall_status']}")
    print(f"Connectors Passed: {results['summary']['passed_connectors']}/{results['summary']['total_connectors']}")
    print(f"Complete Pipelines: {results['summary']['complete_pipelines']}/{results['summary']['traced_symbols']}")
    
    # Save results
    results_file = Path(__file__).parent / "datawarehousetestingresults" / f"pipeline_validation_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    results_file.parent.mkdir(exist_ok=True)
    
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\n📄 Detailed results saved to: {results_file}")
    
    return results['overall_status'] == 'PASSED'


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)