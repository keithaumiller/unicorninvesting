"""
pytest configuration and test runner for Unicorn ETH Data Collection
This file configures pytest for the testing framework

Usage:
    cd /workspaces/unicorninvesting/tests
    pytest --verbose
    pytest unicorn/1_data_sources/ -v
    pytest unicorn/1_data_sources/1_raw/connectors/interactive_brokers/ -v
"""

import pytest
import sys
import os
from pathlib import Path

# Add source directories to Python path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root / "BackendPython"))
sys.path.insert(0, str(project_root / "BackendPython" / "unicorn"))

# Configure pytest markers
pytest_plugins = []

def pytest_configure(config):
    """Configure pytest markers and settings"""
    config.addinivalue_line(
        "markers", "integration: mark test as integration test requiring live IBKR connection"
    )
    config.addinivalue_line(
        "markers", "performance: mark test as performance benchmark"
    )
    config.addinivalue_line(
        "markers", "slow: mark test as slow running"
    )
    config.addinivalue_line(
        "markers", "unit: mark test as unit test (fast, no external dependencies)"
    )

def pytest_collection_modifyitems(config, items):
    """Modify test collection to add markers automatically"""
    for item in items:
        # Add integration marker to tests that connect to IBKR
        if "ibkr" in item.nodeid.lower() and ("real_" in item.name or "live_" in item.name):
            item.add_marker(pytest.mark.integration)
        
        # Add performance marker to performance tests
        if "performance" in item.name.lower() or "benchmark" in item.name.lower():
            item.add_marker(pytest.mark.performance)
        
        # Add slow marker to tests that take longer
        if any(keyword in item.name.lower() for keyword in ["stress", "load", "endurance"]):
            item.add_marker(pytest.mark.slow)
        
        # Add unit marker to fast tests
        if "test_" in item.name and not any(marker.name in ["integration", "performance", "slow"] for marker in item.iter_markers()):
            item.add_marker(pytest.mark.unit)

@pytest.fixture(scope="session")
def project_root():
    """Project root directory"""
    return Path(__file__).parent.parent

@pytest.fixture(scope="session") 
def test_data_dir(project_root):
    """Test data directory"""
    test_data = project_root / "tests" / "data"
    test_data.mkdir(exist_ok=True)
    return test_data

@pytest.fixture
def sample_eth_data():
    """Sample ETH data for testing"""
    import pandas as pd
    from datetime import datetime, timedelta
    
    base_time = pd.Timestamp('2025-01-15 12:00:00', tz='UTC')
    
    sample_data = []
    for i in range(60):  # 1 hour of minute data
        timestamp = base_time + timedelta(minutes=i)
        price = 4000 + i * 0.1
        
        data_point = {
            'timestamp': timestamp,
            'open': price,
            'high': price * 1.001,
            'low': price * 0.999,
            'close': price,
            'volume': 1000 + i * 10
        }
        sample_data.append(data_point)
    
    return sample_data

@pytest.fixture
def ibkr_gateway_available():
    """Check if IBKR Gateway is available"""
    import requests
    try:
        response = requests.get("http://localhost:5000/v1/api/portal/sso/validate", timeout=5)
        return response.status_code in [200, 401]  # 401 is also valid (not authenticated)
    except:
        return False
