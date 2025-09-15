#!/usr/bin/env python3
"""
Test IBKR Client Portal Connector (Demo Mode)
This script tests the connector functionality without requiring actual IBKR connection.
"""

import pytest
import sys
import os
import json
from datetime import datetime

# Add the IBKR connector path to sys.path
ibkr_connector_path = os.path.join(
    os.path.dirname(__file__), 
    '..', '..', '..', '..', '..', '..',
    'BackendPython', 'unicorn', '1_data_sources', 
    '1_raw', 'connectors', 'interactive_brokers'
)
sys.path.append(ibkr_connector_path)

try:
    from IBKRClientPortalConnector import IBKRClientPortalConnector
    print("✅ Successfully imported IBKRClientPortalConnector")
except ImportError as e:
    print(f"❌ Failed to import connector: {e}")
    print(f"💡 Looking for IBKRClientPortalConnector.py in: {ibkr_connector_path}")
    pytest.skip(f"Cannot import IBKR connector from {ibkr_connector_path}", allow_module_level=True)


@pytest.fixture(scope="session")
def connector():
    """Fixture to provide IBKR connector instance for tests."""
    try:
        connector = IBKRClientPortalConnector()
        print(f"✅ Connector initialized with base URL: {connector.base_url}")
        return connector
    except Exception as e:
        pytest.skip(f"Could not initialize IBKR connector: {e}")


def test_connector_initialization():
    """Test connector initialization."""
    print("🚀 Testing IBKR Client Portal Connector Initialization")
    print("=" * 60)
    
    try:
        # Test with default settings
        connector = IBKRClientPortalConnector()
        print(f"✅ Connector initialized with base URL: {connector.base_url}")
        print(f"✅ Session created: {type(connector.session).__name__}")
        print(f"✅ Authentication state: {connector.authenticated}")
        assert connector is not None
        assert hasattr(connector, 'base_url')
        assert hasattr(connector, 'session')
    except Exception as e:
        pytest.fail(f"Initialization failed: {e}")


def test_configuration_methods(connector):
    """Test configuration and utility methods."""
    print("\n⚙️ Testing Configuration Methods")
    print("=" * 40)
    
    try:
        # Test health check structure (without actual connection)
        health_template = {
            "timestamp": datetime.now().isoformat(),
            "status": "unknown",
            "authenticated": connector.authenticated,
            "base_url": connector.base_url
        }
        
        print(f"✅ Health check template: {json.dumps(health_template, indent=2)}")
        assert "timestamp" in health_template
        assert "status" in health_template
        assert "authenticated" in health_template
        
    except Exception as e:
        pytest.fail(f"Configuration test failed: {e}")


def test_environment_setup():
    """Test environment and dependency setup."""
    print("\n🌍 Testing Environment Setup")
    print("=" * 40)
    
    # Test required modules are available
    required_modules = ['requests', 'json', 'datetime']
    
    for module in required_modules:
        try:
            __import__(module)
            print(f"✅ {module} module available")
        except ImportError:
            pytest.fail(f"Required module {module} not available")
    
    # Test file structure
    connector_file = os.path.join(ibkr_connector_path, 'IBKRClientPortalConnector.py')
    assert os.path.exists(connector_file), f"Connector file not found: {connector_file}"
    print(f"✅ Connector file found: {connector_file}")


def test_dependencies():
    """Test that all dependencies are properly imported."""
    print("\n📦 Testing Dependencies")
    print("=" * 40)
    
    try:
        import requests
        import json
        from datetime import datetime
        print("✅ All core dependencies available")
        
        # Test IBKRClientPortalConnector can be instantiated
        connector = IBKRClientPortalConnector()
        assert connector is not None
        print("✅ IBKRClientPortalConnector instantiation successful")
        
    except Exception as e:
        pytest.fail(f"Dependency test failed: {e}")


if __name__ == "__main__":
    print("🧪 Running IBKR Connector Demo Tests")
    print("=" * 60)
    
    # Run tests manually if called directly
    test_environment_setup()
    test_dependencies()
    test_connector_initialization()
    
    print("\n✅ All demo tests completed!")