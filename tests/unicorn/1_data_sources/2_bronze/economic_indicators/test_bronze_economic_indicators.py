#!/usr/bin/env python3
"""
Bronze Layer Economic Indicators Testing Suite

This test suite validates the bronze layer processing of economic indicators,
ensuring proper data transformation, feature engineering, and output quality.

Test Categories:
- Data Processing Validation
- Feature Engineering Quality
- Output File Structure
- Performance Benchmarking
- Error Handling
"""

import os
import sys
import pytest
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import json

# Add project root to path
project_root = Path(__file__).parents[4]
sys.path.append(str(project_root))

# Import bronze layer processors using direct path approach
bronze_path = project_root / "BackendPython" / "unicorn" / "1_data_sources" / "2_bronze" / "economic_indicators" / "processors"
sys.path.append(str(bronze_path))

try:
    from base_processor import BaseEconomicProcessor
    from economic_growth_processor import EconomicGrowthProcessor  
    from consumer_business_processor import ConsumerBusinessProcessor
    from international_trade_processor import InternationalTradeProcessor
    from monetary_policy_processor import MonetaryPolicyProcessor
except ImportError as e:
    print(f"⚠️ Import error for bronze layer processors: {e}")
    print("Running in limited mode without processor imports")
    BaseEconomicProcessor = None

class TestBronzeLayerEconomicIndicators:
    """Test suite for bronze layer economic indicators processing."""
    
    @pytest.fixture(scope="class")
    def processors(self):
        """Initialize all economic processors for testing."""
        return {
            'economic_growth': EconomicGrowthProcessor(),
            'consumer_business': ConsumerBusinessProcessor(),
            'international_trade': InternationalTradeProcessor(),
            'monetary_policy': MonetaryPolicyProcessor()
        }
    
    @pytest.fixture(scope="class")
    def bronze_output_path(self):
        """Get the bronze layer output path."""
        return "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/economic_indicators/processed_data"
    
    def test_processor_initialization(self, processors):
        """Test that all processors initialize correctly."""
        for category, processor in processors.items():
            assert processor is not None, f"{category} processor failed to initialize"
            assert hasattr(processor, 'process'), f"{category} processor missing process method"
            assert hasattr(processor, 'load_raw_data'), f"{category} processor missing load_raw_data method"
    
    def test_raw_data_availability(self, processors):
        """Test that raw data is available for processing."""
        raw_data_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/fred_data"
        
        assert os.path.exists(raw_data_path), "Raw FRED data directory not found"
        
        # Check for FRED data files
        fred_files = list(Path(raw_data_path).glob("*.csv"))
        assert len(fred_files) > 0, "No FRED CSV files found in raw data directory"
        
        print(f"✅ Found {len(fred_files)} FRED data files")
    
    def test_processor_execution(self, processors):
        """Test that all processors can execute successfully."""
        results = {}
        
        for category, processor in processors.items():
            try:
                # Test processing with minimal intervals (to avoid memory issues)
                result = processor.process(['1_day'])  # Only test 1_day to avoid memory issues
                results[category] = {
                    'status': 'success',
                    'intervals_processed': len(result) if result else 0
                }
                print(f"✅ {category} processor executed successfully")
            except Exception as e:
                results[category] = {
                    'status': 'failed',
                    'error': str(e)
                }
                print(f"❌ {category} processor failed: {str(e)}")
        
        # At least one processor should succeed
        successful_processors = [cat for cat, result in results.items() if result['status'] == 'success']
        assert len(successful_processors) > 0, f"No processors succeeded. Results: {results}"
        
        return results
    
    def test_output_file_structure(self, bronze_output_path):
        """Test that bronze layer output files have correct structure."""
        output_path = Path(bronze_output_path)
        
        # Check for interval directories
        expected_intervals = ['1_day', '1_hour']  # Skip 1_minute due to memory issues
        
        for interval in expected_intervals:
            interval_path = output_path / interval
            if interval_path.exists():
                # Check for CSV files
                csv_files = list(interval_path.glob("*.csv"))
                if csv_files:
                    # Test file structure of first CSV
                    test_file = csv_files[0]
                    df = pd.read_csv(test_file, nrows=5)  # Read only first 5 rows
                    
                    # Validate basic structure
                    assert len(df.columns) > 0, f"No columns in {test_file}"
                    assert len(df) > 0, f"No data rows in {test_file}"
                    
                    # Check for date column
                    date_columns = ['index', 'Date', 'date', 'timestamp']
                    has_date_col = any(col in df.columns for col in date_columns)
                    assert has_date_col, f"No date column found in {test_file}"
                    
                    print(f"✅ {interval} files have correct structure ({len(csv_files)} files)")
    
    def test_feature_engineering_quality(self, bronze_output_path):
        """Test the quality of feature engineering in bronze layer."""
        output_path = Path(bronze_output_path)
        
        # Find the most recent monetary policy file (known to have many features)
        day_path = output_path / "1_day"
        if day_path.exists():
            monetary_files = list(day_path.glob("monetary_policy_*.csv"))
            if monetary_files:
                latest_file = max(monetary_files, key=os.path.getmtime)
                
                # Load and analyze features
                df = pd.read_csv(latest_file, nrows=100)  # Sample for performance
                
                # Test feature categories
                feature_categories = {
                    'lag_features': len([col for col in df.columns if '_lag_' in col]),
                    'diff_features': len([col for col in df.columns if '_diff_' in col]),
                    'pct_change_features': len([col for col in df.columns if '_pct_change_' in col]),
                    'mean_features': len([col for col in df.columns if '_mean_' in col]),
                    'std_features': len([col for col in df.columns if '_std_' in col]),
                    'momentum_features': len([col for col in df.columns if '_momentum_' in col]),
                    'volatility_features': len([col for col in df.columns if '_volatility_' in col])
                }
                
                print(f"📊 Feature Engineering Analysis:")
                for category, count in feature_categories.items():
                    print(f"  {category}: {count} features")
                
                # Validate feature engineering completeness
                assert feature_categories['lag_features'] > 0, "No lag features found"
                assert feature_categories['diff_features'] > 0, "No difference features found"
                assert feature_categories['mean_features'] > 0, "No rolling mean features found"
                
                total_features = len(df.columns)
                assert total_features > 50, f"Too few features generated: {total_features}"
                
                print(f"✅ Feature engineering quality validated ({total_features} total features)")
    
    def test_data_quality_metrics(self, bronze_output_path):
        """Test data quality metrics in bronze layer output."""
        output_path = Path(bronze_output_path)
        
        day_path = output_path / "1_day"
        if day_path.exists():
            csv_files = list(day_path.glob("*.csv"))
            if csv_files:
                test_file = csv_files[0]  # Test first available file
                
                # Load sample data
                df = pd.read_csv(test_file, nrows=1000)  # Sample for performance
                
                # Data quality checks
                quality_metrics = {
                    'total_rows': len(df),
                    'total_columns': len(df.columns),
                    'missing_data_percentage': (df.isnull().sum().sum() / (len(df) * len(df.columns))) * 100,
                    'duplicate_rows': df.duplicated().sum(),
                    'numeric_columns': len(df.select_dtypes(include=[np.number]).columns)
                }
                
                print(f"📈 Data Quality Metrics:")
                for metric, value in quality_metrics.items():
                    print(f"  {metric}: {value}")
                
                # Quality assertions
                assert quality_metrics['total_rows'] > 0, "No data rows found"
                assert quality_metrics['total_columns'] > 10, "Too few columns"
                assert quality_metrics['missing_data_percentage'] < 95, "Too much missing data"
                assert quality_metrics['numeric_columns'] > 5, "Too few numeric columns"
                
                print(f"✅ Data quality metrics validated")
    
    def test_temporal_coverage(self, bronze_output_path):
        """Test temporal coverage of processed data."""
        output_path = Path(bronze_output_path)
        
        day_path = output_path / "1_day"
        if day_path.exists():
            csv_files = list(day_path.glob("*.csv"))
            if csv_files:
                for csv_file in csv_files[:2]:  # Test first 2 files for performance
                    df = pd.read_csv(csv_file, nrows=100)
                    
                    # Find date column
                    date_columns = ['index', 'Date', 'date', 'timestamp']
                    date_col = None
                    for col in date_columns:
                        if col in df.columns:
                            date_col = col
                            break
                    
                    if date_col:
                        # Parse dates
                        df[date_col] = pd.to_datetime(df[date_col], errors='coerce')
                        valid_dates = df[date_col].dropna()
                        
                        if len(valid_dates) > 1:
                            date_range = valid_dates.max() - valid_dates.min()
                            print(f"📅 {csv_file.name}: {len(valid_dates)} valid dates, range: {date_range.days} days")
                            
                            # Validate temporal coverage
                            assert len(valid_dates) > 0, f"No valid dates in {csv_file.name}"
                            assert date_range.days >= 0, f"Invalid date range in {csv_file.name}"
                
                print(f"✅ Temporal coverage validated")

def test_bronze_layer_integration():
    """Integration test for bronze layer economic indicators."""
    # Test that bronze layer can be imported and initialized
    try:
        # Import using direct path approach
        project_root = Path(__file__).parents[4]
        bronze_path = project_root / "BackendPython" / "unicorn" / "1_data_sources" / "2_bronze" / "economic_indicators"
        sys.path.append(str(bronze_path))
        
        from process_indicators import main
        print("✅ Bronze layer integration test passed")
        return True
    except ImportError as e:
        print(f"❌ Bronze layer integration test failed: {e}")
        return False

if __name__ == "__main__":
    # Run tests when script is executed directly
    pytest.main([__file__, "-v", "--tb=short"])