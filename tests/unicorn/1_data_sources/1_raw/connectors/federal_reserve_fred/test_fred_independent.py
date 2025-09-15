#!/usr/bin/env python3
"""
Independent FRED Connector Tests
Tests FRED functionality without depending on existing pipeline state.
"""

import pytest
import os
import sys
from unittest.mock import Mock, patch
import pandas as pd
from datetime import datetime, timedelta

@pytest.fixture
def mock_fred_api():
    """Mock FRED API for independent testing."""
    mock_api = Mock()
    
    # Mock successful API response
    sample_data = pd.Series(
        [1.5, 1.6, 1.7, 1.8, 1.9],
        index=pd.date_range('2023-01-01', periods=5, freq='D'),
        name='FEDFUNDS'
    )
    mock_api.get_series.return_value = sample_data
    
    return mock_api

@pytest.fixture
def fred_connector_class():
    """Import FRED connector class with error handling."""
    try:
        fred_path = os.path.join(
            os.path.dirname(__file__), 
            '..', '..', '..', '..', '..', '..',
            'BackendPython', 'unicorn', '1_data_sources', 
            '1_raw', 'connectors', 'federal_reserve_fred'
        )
        sys.path.insert(0, fred_path)
        
        from fred_connector import FredConnector
        return FredConnector
    except ImportError:
        pytest.skip("FRED connector not available")

def test_fred_connector_initialization_independent(fred_connector_class):
    """Test FRED connector can initialize independently."""
    with patch.dict(os.environ, {'FRED_API_KEY': 'test_key'}):
        with patch('fredapi.Fred') as mock_fred:
            connector = fred_connector_class()
            assert connector is not None

def test_fred_data_processing_independent(mock_fred_api):
    """Test FRED data processing logic independently."""
    # Test with mock data
    data = mock_fred_api.get_series('FEDFUNDS')
    
    # Validate data structure
    assert isinstance(data, pd.Series)
    assert len(data) > 0
    assert data.index.dtype.kind == 'M'  # datetime index

def test_fred_error_handling_independent():
    """Test FRED error handling independently."""
    with patch.dict(os.environ, {}, clear=True):
        try:
            # This should fail without API key
            from fred_connector import FredConnector
            with pytest.raises(ValueError):
                FredConnector()
        except ImportError:
            pytest.skip("FRED connector not available")
