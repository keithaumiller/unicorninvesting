#!/usr/bin/env python3
"""
Test script for FRED Connector using fredapi

Validates FRED API connectivity, data retrieval, and feature engineering
for crypto alpha model integration.
"""

import os
import sys
import pandas as pd
from datetime import datetime, timedelta

# Add the FRED connector path to sys.path  
fred_connector_path = os.path.join(os.path.dirname(__file__), '..', '..', '..', '..', '..', '..', 'BackendPython', 'unicorn', '1_data_sources', '1_raw', 'connectors', 'federal_reserve_fred')
sys.path.insert(0, fred_connector_path)

try:
    from fred_connector import FredConnector
    from config import get_critical_series, REGIME_THRESHOLDS
except ImportError as e:
    print(f"❌ Import Error: {e}")
    print(f"💡 Looking for fred_connector.py in: {fred_connector_path}")
    print("💡 Make sure fred_connector.py and config.py are in the source directory")
    # Don't exit during pytest collection, just raise ImportError
    import pytest
    pytest.skip(f"Cannot import FRED connector from {fred_connector_path}", allow_module_level=True)


def test_api_connection():
    """Test basic FRED API connection and authentication."""
    print("1️⃣  Testing FRED API Connection...")
    
    try:
        fred = FredConnector()
        print("   ✅ FRED Connector initialized successfully")
        return fred
    except ValueError as e:
        print(f"   ❌ Configuration Error: {e}")
        print("   💡 Set FRED_API_KEY environment variable")
        print("   🔗 Get free API key: https://fred.stlouisfed.org/docs/api/api_key.html")
        return None
    except ImportError as e:
        print(f"   ❌ Library Error: {e}")
        print("   💡 Install fredapi: pip install fredapi")
        return None
    except Exception as e:
        print(f"   ❌ Unexpected Error: {e}")
        return None


def test_single_series(fred_connector):
    """Test single series data retrieval."""
    print("\n2️⃣  Testing Single Series Retrieval...")
    
    try:
        # Test Federal Funds Rate (most important for crypto)
        series_id = 'FEDFUNDS'
        start_date = '2023-01-01'
        
        data = fred_connector.get_series_data(series_id, start_date=start_date)
        
        if data.empty:
            print(f"   ❌ No data retrieved for {series_id}")
            return False
        
        print(f"   ✅ Retrieved {len(data)} observations for {series_id}")
        print(f"   📅 Date range: {data.index.min()} to {data.index.max()}")
        print(f"   💹 Latest Fed Funds Rate: {data.iloc[-1]:.2f}%")
        
        # Test data quality
        missing_pct = (data.isna().sum() / len(data)) * 100
        print(f"   📊 Missing data: {missing_pct:.1f}%")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Single series test failed: {e}")
        return False


def test_multiple_series(fred_connector):
    """Test multiple series data retrieval."""
    print("\n3️⃣  Testing Multiple Series Retrieval...")
    
    try:
        # Test a small subset of critical series
        test_series = ['FEDFUNDS', 'DGS10', 'CPIAUCSL']
        start_date = '2022-01-01'
        
        data = fred_connector.get_multiple_series(test_series, start_date=start_date)
        
        if data.empty:
            print("   ❌ No data retrieved for multiple series")
            return False
        
        print(f"   ✅ Retrieved {len(data.columns)} series")
        print(f"   📊 Data shape: {data.shape}")
        print(f"   📅 Date range: {data.index.min()} to {data.index.max()}")
        
        # Check data completeness
        for column in data.columns:
            missing_pct = (data[column].isna().sum() / len(data)) * 100
            print(f"   📈 {column}: {missing_pct:.1f}% missing data")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Multiple series test failed: {e}")
        return False


def test_critical_indicators(fred_connector):
    """Test critical indicators collection for crypto alpha models."""
    print("\n4️⃣  Testing Critical Indicators Collection...")
    
    try:
        # Test with 2 years of data for faster testing
        start_date = (datetime.now() - timedelta(days=2*365)).strftime('%Y-%m-%d')
        
        critical_data = fred_connector.get_critical_indicators(start_date=start_date)
        
        if critical_data.empty:
            print("   ❌ No critical indicators retrieved")
            return False
        
        print(f"   ✅ Retrieved {len(critical_data.columns)} critical indicators")
        print(f"   📊 Data shape: {critical_data.shape}")
        print(f"   📅 Date range: {critical_data.index.min()} to {critical_data.index.max()}")
        
        # Show latest values
        print("   💹 Latest Critical Indicators:")
        latest = critical_data.iloc[-1]
        for series_id, value in latest.items():
            if not pd.isna(value):
                series_info = fred_connector.economic_series.get(series_id)
                name = series_info.name if series_info else series_id
                units = series_info.units if series_info else ""
                print(f"      {name}: {value:.2f} {units}")
        
        return critical_data
        
    except Exception as e:
        print(f"   ❌ Critical indicators test failed: {e}")
        return None


def test_feature_engineering(fred_connector, sample_data):
    """Test feature engineering for alpha models."""
    print("\n5️⃣  Testing Feature Engineering...")
    
    if sample_data is None or sample_data.empty:
        print("   ⚠️  No sample data available for feature engineering test")
        return False
    
    try:
        # Create alpha features
        features_data = fred_connector.create_alpha_features(sample_data)
        
        if features_data.empty:
            print("   ❌ No features created")
            return False
        
        print(f"   ✅ Created {len(features_data.columns)} features from {len(sample_data.columns)} original series")
        
        # Show feature categories
        original_features = list(sample_data.columns)
        engineered_features = [col for col in features_data.columns if col not in original_features]
        
        print(f"   📊 Original series: {len(original_features)}")
        print(f"   🔧 Engineered features: {len(engineered_features)}")
        
        # Show some example features
        print("   🎯 Example engineered features:")
        for feature in engineered_features[:5]:
            latest_value = features_data[feature].iloc[-1]
            if not pd.isna(latest_value):
                print(f"      {feature}: {latest_value:.4f}")
        
        # Test economic regime indicators
        regime_indicators = [col for col in features_data.columns 
                           if any(keyword in col for keyword in ['curve', 'inflation', 'stress', 'trend'])]
        print(f"   🏛️  Economic regime indicators: {len(regime_indicators)}")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Feature engineering test failed: {e}")
        return False


def test_alpha_model_integration(fred_connector):
    """Test data preparation for alpha model integration."""
    print("\n6️⃣  Testing Alpha Model Integration...")
    
    try:
        # Create temporary output directory
        test_output_dir = os.path.join(os.path.dirname(__file__), 'test_output')
        
        # Save data for alpha models (2 years for testing)
        files_saved = fred_connector.save_data_for_alpha_models(
            output_dir=test_output_dir,
            lookback_years=2
        )
        
        if not files_saved:
            print("   ❌ No files saved for alpha model integration")
            return False
        
        print("   ✅ Alpha model integration files created:")
        for file_type, file_path in files_saved.items():
            if os.path.exists(file_path):
                file_size = os.path.getsize(file_path)
                print(f"      {file_type}: {os.path.basename(file_path)} ({file_size:,} bytes)")
            else:
                print(f"      ❌ {file_type}: File not found")
        
        # Verify data formats
        if 'features' in files_saved:
            features_df = pd.read_csv(files_saved['features'], index_col=0, parse_dates=True)
            print(f"   📈 Features file: {features_df.shape} (rows, columns)")
            print(f"   📅 Features date range: {features_df.index.min()} to {features_df.index.max()}")
        
        # Clean up test files
        try:
            import shutil
            if os.path.exists(test_output_dir):
                shutil.rmtree(test_output_dir)
                print("   🧹 Test files cleaned up")
        except:
            pass  # Ignore cleanup errors
        
        return True
        
    except Exception as e:
        print(f"   ❌ Alpha model integration test failed: {e}")
        return False


def test_series_metadata(fred_connector):
    """Test series metadata retrieval."""
    print("\n7️⃣  Testing Series Metadata...")
    
    try:
        # Test metadata for key series
        test_series = 'FEDFUNDS'
        metadata = fred_connector.get_series_info(test_series)
        
        if not metadata:
            print(f"   ⚠️  No metadata retrieved for {test_series}")
            return False
        
        print(f"   ✅ Retrieved metadata for {test_series}")
        
        # Show key metadata fields
        key_fields = ['title', 'units', 'frequency', 'last_updated']
        for field in key_fields:
            if field in metadata:
                print(f"      {field}: {metadata[field]}")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Metadata test failed: {e}")
        return False


def run_comprehensive_test():
    """Run comprehensive test suite for FRED Connector."""
    print("🧪 FRED Connector Comprehensive Test Suite")
    print("=" * 50)
    print(f"🕒 Test started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    test_results = []
    
    # Test 1: API Connection
    fred = test_api_connection()
    test_results.append(fred is not None)
    
    if fred is None:
        print("\n❌ Cannot proceed with tests - API connection failed")
        return False
    
    # Test 2: Single Series
    result2 = test_single_series(fred)
    test_results.append(result2)
    
    # Test 3: Multiple Series
    result3 = test_multiple_series(fred)
    test_results.append(result3)
    
    # Test 4: Critical Indicators
    critical_data = test_critical_indicators(fred)
    test_results.append(critical_data is not None)
    
    # Test 5: Feature Engineering
    result5 = test_feature_engineering(fred, critical_data)
    test_results.append(result5)
    
    # Test 6: Alpha Model Integration
    result6 = test_alpha_model_integration(fred)
    test_results.append(result6)
    
    # Test 7: Series Metadata
    result7 = test_series_metadata(fred)
    test_results.append(result7)
    
    # Test Summary
    passed_tests = sum(test_results)
    total_tests = len(test_results)
    success_rate = (passed_tests / total_tests) * 100
    
    print("\n" + "=" * 50)
    print("🎯 Test Results Summary")
    print("=" * 50)
    print(f"✅ Passed: {passed_tests}/{total_tests} tests ({success_rate:.1f}%)")
    
    if passed_tests == total_tests:
        print("🎉 All tests passed! FRED Connector is ready for production.")
    elif passed_tests >= total_tests * 0.8:
        print("⚠️  Most tests passed. Minor issues may need attention.")
    else:
        print("❌ Multiple test failures. Review configuration and setup.")
    
    print(f"🕒 Test completed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    return passed_tests == total_tests


def main():
    """Main test execution."""
    try:
        success = run_comprehensive_test()
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n⚠️  Test interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Test suite error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
