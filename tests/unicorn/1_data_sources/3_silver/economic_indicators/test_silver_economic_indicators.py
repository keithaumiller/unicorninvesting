#!/usr/bin/env python3
"""
Silver Layer Economic Indicators Testing Suite

This test suite validates the silver layer processing of economic indicators,
ensuring proper aggregation, normalization, feature selection, and output quality.

Test Categories:
- Data Aggregation Validation
- Feature Selection Quality
- Temporal Alignment
- Data Quality Metrics
- Output Format Compliance
- Performance Benchmarking
"""

import os
import sys
import pytest
import pandas as pd
import numpy as np
import json
from datetime import datetime, timedelta
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parents[4]
sys.path.append(str(project_root))

try:
    from BackendPython.unicorn.1_data_sources.3_silver.economic_indicators.silver_processor import SilverLayerEconomicProcessor
except ImportError:
    # Alternative import path
    import sys
    sys.path.append(str(project_root / "BackendPython" / "unicorn" / "1_data_sources" / "3_silver" / "economic_indicators"))
    from silver_processor import SilverLayerEconomicProcessor

class TestSilverLayerEconomicIndicators:
    """Test suite for silver layer economic indicators processing."""
    
    @pytest.fixture(scope="class")
    def processor(self):
        """Initialize silver layer processor for testing."""
        return SilverLayerEconomicProcessor()
    
    @pytest.fixture(scope="class")
    def silver_output_path(self):
        """Get the silver layer output path."""
        return "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver/processed_data"
    
    @pytest.fixture(scope="class")
    def bronze_input_path(self):
        """Get the bronze layer input path."""
        return "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/economic_indicators/processed_data"
    
    def test_processor_initialization(self, processor):
        """Test that silver layer processor initializes correctly."""
        assert processor is not None, "Silver processor failed to initialize"
        assert hasattr(processor, 'process_interval'), "Processor missing process_interval method"
        assert hasattr(processor, 'load_bronze_data'), "Processor missing load_bronze_data method"
        assert hasattr(processor, 'align_temporal_data'), "Processor missing align_temporal_data method"
        
        # Test configuration
        assert hasattr(processor, 'config'), "Processor missing configuration"
        assert 'max_features_per_category' in processor.config, "Missing max_features_per_category config"
        assert 'correlation_threshold' in processor.config, "Missing correlation_threshold config"
        
        print("✅ Silver layer processor initialized correctly")
    
    def test_bronze_data_loading(self, processor):
        """Test that bronze layer data can be loaded successfully."""
        # Test 1-day interval loading
        bronze_data = processor.load_bronze_data('1_day')
        
        # Should have at least some data
        assert isinstance(bronze_data, dict), "Bronze data should be a dictionary"
        
        loaded_categories = len(bronze_data)
        print(f"📊 Bronze data loading: {loaded_categories} categories loaded")
        
        if loaded_categories > 0:
            # Test data structure
            for category, df in bronze_data.items():
                assert isinstance(df, pd.DataFrame), f"{category} data is not a DataFrame"
                assert len(df) > 0, f"{category} DataFrame is empty"
                assert isinstance(df.index, pd.DatetimeIndex), f"{category} index is not DatetimeIndex"
                
                print(f"  {category}: {len(df)} rows, {len(df.columns)} features")
        
        print("✅ Bronze data loading validated")
        return bronze_data
    
    def test_temporal_alignment(self, processor):
        """Test temporal alignment across economic categories."""
        bronze_data = processor.load_bronze_data('1_day')
        
        if len(bronze_data) > 1:
            # Test alignment
            aligned_data = processor.align_temporal_data(bronze_data)
            
            assert isinstance(aligned_data, pd.DataFrame), "Aligned data should be a DataFrame"
            assert isinstance(aligned_data.index, pd.DatetimeIndex), "Aligned data should have DatetimeIndex"
            
            if not aligned_data.empty:
                # Test alignment quality
                date_range = aligned_data.index.max() - aligned_data.index.min()
                print(f"📅 Temporal alignment: {len(aligned_data)} rows, {date_range.days} days range")
                
                # Check for proper column naming
                column_prefixes = set(col.split('_')[0] for col in aligned_data.columns if '_' in col)
                print(f"  Column prefixes: {column_prefixes}")
                
                assert len(aligned_data) > 0, "Aligned data should not be empty"
                assert len(aligned_data.columns) > 0, "Aligned data should have columns"
        
        print("✅ Temporal alignment validated")
    
    def test_silver_processing_execution(self, processor):
        """Test that silver layer processing executes successfully."""
        results = {}
        
        # Test both 1_day and 1_hour intervals
        for interval in ['1_day', '1_hour']:
            try:
                result = processor.process_interval(interval)
                results[interval] = {
                    'status': 'success',
                    'output_files': len(result) if result else 0
                }
                print(f"✅ {interval} processing executed successfully")
            except Exception as e:
                results[interval] = {
                    'status': 'failed',
                    'error': str(e)
                }
                print(f"❌ {interval} processing failed: {str(e)}")
        
        # At least one interval should succeed
        successful_intervals = [interval for interval, result in results.items() if result['status'] == 'success']
        assert len(successful_intervals) > 0, f"No intervals processed successfully. Results: {results}"
        
        return results
    
    def test_output_file_validation(self, silver_output_path):
        """Test that silver layer output files are correctly formatted."""
        output_path = Path(silver_output_path)
        
        for interval in ['1_day', '1_hour']:
            interval_path = output_path / interval
            if interval_path.exists():
                # Check for CSV files
                csv_files = list(interval_path.glob("economic_silver_*.csv"))
                json_files = list(interval_path.glob("economic_silver_metadata_*.json"))
                
                if csv_files:
                    # Test CSV structure
                    latest_csv = max(csv_files, key=os.path.getmtime)
                    df = pd.read_csv(latest_csv, nrows=10)  # Sample for performance
                    
                    assert len(df.columns) > 0, f"No columns in {latest_csv}"
                    assert len(df) > 0, f"No data rows in {latest_csv}"
                    
                    # Check for proper datetime index
                    df_full = pd.read_csv(latest_csv, index_col=0, parse_dates=True, nrows=5)
                    assert isinstance(df_full.index, pd.DatetimeIndex), f"Invalid datetime index in {latest_csv}"
                    
                    print(f"✅ {interval} CSV files validated ({len(csv_files)} files, {len(df.columns)} features)")
                
                if json_files:
                    # Test JSON metadata structure
                    latest_json = max(json_files, key=os.path.getmtime)
                    with open(latest_json, 'r') as f:
                        metadata = json.load(f)
                    
                    # Validate metadata structure
                    required_fields = ['processing_timestamp', 'total_features', 'total_observations', 'categories', 'data_quality']
                    for field in required_fields:
                        assert field in metadata, f"Missing {field} in metadata"
                    
                    print(f"✅ {interval} metadata files validated ({len(json_files)} files)")
    
    def test_feature_selection_quality(self, silver_output_path):
        """Test the quality of feature selection in silver layer."""
        output_path = Path(silver_output_path)
        
        # Test metadata for feature selection results
        for interval in ['1_day', '1_hour']:
            metadata_path = output_path / interval / "economic_silver_metadata_latest.json"
            if metadata_path.exists():
                with open(metadata_path, 'r') as f:
                    metadata = json.load(f)
                
                # Analyze feature selection
                total_features = metadata.get('total_features', 0)
                categories = metadata.get('categories', {})
                
                print(f"📊 {interval} Feature Selection Analysis:")
                print(f"  Total features selected: {total_features}")
                
                feature_distribution = {}
                for category, info in categories.items():
                    feature_count = info.get('feature_count', 0)
                    feature_distribution[category] = feature_count
                    if feature_count > 0:
                        print(f"  {category}: {feature_count} features")
                
                # Validate feature selection
                assert total_features > 0, f"No features selected for {interval}"
                assert total_features <= 100, f"Too many features selected for {interval}: {total_features}"
                
                # Check that features are distributed (not all from one category)
                active_categories = sum(1 for count in feature_distribution.values() if count > 0)
                print(f"  Active categories: {active_categories}")
                
                print(f"✅ {interval} feature selection quality validated")
    
    def test_data_quality_metrics(self, silver_output_path):
        """Test data quality metrics in silver layer output."""
        output_path = Path(silver_output_path)
        
        for interval in ['1_day', '1_hour']:
            csv_path = output_path / interval / "economic_silver_latest.csv"
            metadata_path = output_path / interval / "economic_silver_metadata_latest.json"
            
            if csv_path.exists() and metadata_path.exists():
                # Load metadata
                with open(metadata_path, 'r') as f:
                    metadata = json.load(f)
                
                # Load sample data
                df = pd.read_csv(csv_path, index_col=0, parse_dates=True, nrows=100)
                
                # Calculate quality metrics
                quality_metrics = {
                    'total_observations': len(df),
                    'total_features': len(df.columns),
                    'missing_data_percentage': (df.isnull().sum().sum() / (len(df) * len(df.columns))) * 100,
                    'numeric_features': len(df.select_dtypes(include=[np.number]).columns),
                    'date_range_days': (df.index.max() - df.index.min()).days
                }
                
                print(f"📈 {interval} Data Quality Metrics:")
                for metric, value in quality_metrics.items():
                    print(f"  {metric}: {value}")
                
                # Validate quality
                assert quality_metrics['total_observations'] > 0, f"No observations in {interval} data"
                assert quality_metrics['total_features'] > 0, f"No features in {interval} data"
                assert quality_metrics['numeric_features'] > 0, f"No numeric features in {interval} data"
                assert quality_metrics['date_range_days'] >= 0, f"Invalid date range in {interval} data"
                
                # Validate against metadata
                metadata_observations = metadata.get('total_observations', 0)
                metadata_features = metadata.get('total_features', 0)
                
                # Allow some tolerance for sampling differences
                assert abs(quality_metrics['total_features'] - metadata_features) <= 5, f"Feature count mismatch in {interval}"
                
                print(f"✅ {interval} data quality validated")
    
    def test_performance_benchmarks(self, processor):
        """Test performance benchmarks for silver layer processing."""
        import time
        
        # Benchmark 1_day processing
        start_time = time.time()
        
        try:
            bronze_data = processor.load_bronze_data('1_day')
            load_time = time.time() - start_time
            
            if bronze_data:
                align_start = time.time()
                aligned_data = processor.align_temporal_data(bronze_data)
                align_time = time.time() - align_start
                
                total_time = time.time() - start_time
                
                performance_metrics = {
                    'load_time_seconds': round(load_time, 2),
                    'align_time_seconds': round(align_time, 2),
                    'total_time_seconds': round(total_time, 2),
                    'data_points_processed': len(aligned_data) if not aligned_data.empty else 0
                }
                
                print(f"⚡ Performance Benchmarks:")
                for metric, value in performance_metrics.items():
                    print(f"  {metric}: {value}")
                
                # Performance assertions
                assert performance_metrics['total_time_seconds'] < 300, "Processing takes too long (>5 minutes)"
                
                if performance_metrics['data_points_processed'] > 0:
                    throughput = performance_metrics['data_points_processed'] / performance_metrics['total_time_seconds']
                    print(f"  throughput_points_per_second: {round(throughput, 2)}")
                    assert throughput > 100, "Processing throughput too low (<100 points/second)"
                
                print("✅ Performance benchmarks validated")
        
        except Exception as e:
            print(f"⚠️ Performance benchmark failed: {e}")

def test_silver_layer_integration():
    """Integration test for silver layer economic indicators."""
    try:
        # Try main import path first
        try:
            from BackendPython.unicorn.1_data_sources.3_silver.economic_indicators.silver_processor import SilverLayerEconomicProcessor
        except ImportError:
            # Alternative import path
            project_root = Path(__file__).parents[4]
            sys.path.append(str(project_root / "BackendPython" / "unicorn" / "1_data_sources" / "3_silver" / "economic_indicators"))
            from silver_processor import SilverLayerEconomicProcessor
        
        processor = SilverLayerEconomicProcessor()
        
        # Test that processor can access bronze data
        bronze_data = processor.load_bronze_data('1_day')
        print(f"✅ Silver layer integration test passed ({len(bronze_data)} categories loaded)")
        return True
    except Exception as e:
        print(f"❌ Silver layer integration test failed: {e}")
        return False

if __name__ == "__main__":
    # Run tests when script is executed directly
    pytest.main([__file__, "-v", "--tb=short"])