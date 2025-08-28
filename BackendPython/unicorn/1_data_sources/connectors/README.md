# Data Source Connectors & Integrations

## 🔌 Purpose

This directory contains **data source connectors** that integrate with external APIs, databases, and data feeds. Connectors handle the initial data ingestion from external sources into our data warehouse.

## 🏗️ Architecture Role

**Data Flow**: External APIs → **Connectors** → Staging → Bronze → Silver → Gold

Connectors are the **entry point** for all external data into our system.

## 📊 Current Integrations

### **Free Data Sources (No API Key Required)**

#### **Yahoo Finance Integration**
- **File**: `YahooFinanceMinuteData.py`
- **Coverage**: US stocks, ETFs, some forex pairs, major cryptocurrencies
- **Resolution**: 1-minute to daily data
- **Advantages**: Free, reliable, good coverage
- **Limitations**: Unofficial API, rate limited, recent data only for minute resolution

#### **Documentation**
- **File**: `YAHOO_FINANCE_INTEGRATION_GUIDE.md`
- **Content**: Complete integration guide, examples, troubleshooting

### **API Key Required Sources**

#### **Alpha Vantage Integration**
- **File**: `AlphaVantageMinuteData.py`
- **Coverage**: Global stocks, forex, cryptocurrencies, economic indicators
- **Resolution**: 1-minute to monthly data
- **Advantages**: Official API, comprehensive data, global coverage
- **Limitations**: 500 free calls/month, requires API key

## 🔧 Connector Standards

### **1. Base Class Structure**
```python
from AlgorithmImports import *

class DataConnector(PythonData):
    def __init__(self):
        self.source_name = "ConnectorName"
        self.rate_limit = 60  # seconds between calls
        self.retry_count = 3
        
    def get_source(self, config, date, is_live_mode):
        """Main data retrieval method"""
        pass
        
    def reader(self, config, line, date, is_live_mode):
        """Parse data from source"""
        pass
```

### **2. Error Handling Requirements**
```python
def get_source(self, config, date, is_live_mode):
    try:
        # Attempt data retrieval
        data = self._fetch_data(config, date)
        return SubscriptionDataSource(data, SubscriptionTransportMedium.REST)
    except RateLimitError:
        # Handle rate limiting
        self._handle_rate_limit()
    except APIError as e:
        # Log and handle API errors
        self._log_error(f"API Error: {e}")
    except Exception as e:
        # Handle unexpected errors
        self._log_error(f"Unexpected error: {e}")
        return None
```

### **3. Data Validation**
```python
def _validate_data(self, data):
    """Validate data before passing to staging"""
    if not data:
        raise ValueError("Empty data received")
    
    # Check required fields
    required_fields = ['timestamp', 'symbol', 'price']
    for field in required_fields:
        if field not in data:
            raise ValueError(f"Missing required field: {field}")
    
    # Validate data types and ranges
    if data['price'] <= 0:
        raise ValueError("Invalid price value")
    
    return True
```

### **4. Rate Limiting**
```python
import time
from datetime import datetime, timedelta

class RateLimiter:
    def __init__(self, max_calls_per_minute=60):
        self.max_calls = max_calls_per_minute
        self.calls = []
    
    def wait_if_needed(self):
        now = datetime.now()
        # Remove calls older than 1 minute
        self.calls = [call for call in self.calls if now - call < timedelta(minutes=1)]
        
        if len(self.calls) >= self.max_calls:
            sleep_time = 60 - (now - self.calls[0]).seconds
            time.sleep(sleep_time)
        
        self.calls.append(now)
```

## 📋 Adding New Connectors

### **Step 1: Create Connector Class**
```python
# File: connectors/MyNewDataSource.py
from AlgorithmImports import *
import requests
import json

class MyNewDataSource(PythonData):
    def __init__(self):
        self.source_name = "MyNewDataSource"
        self.base_url = "https://api.example.com"
        self.api_key = self._get_api_key()
        
    def get_source(self, config, date, is_live_mode):
        symbol = config.symbol.value
        url = f"{self.base_url}/data/{symbol}"
        
        try:
            response = requests.get(url, headers={"API-Key": self.api_key})
            response.raise_for_status()
            
            data = response.json()
            self._validate_data(data)
            
            return SubscriptionDataSource(
                json.dumps(data), 
                SubscriptionTransportMedium.REST
            )
        except Exception as e:
            self._log_error(f"Error fetching data: {e}")
            return None
    
    def reader(self, config, line, date, is_live_mode):
        try:
            data = json.loads(line)
            
            point = MyNewDataSource()
            point.symbol = config.symbol
            point.time = datetime.fromisoformat(data['timestamp'])
            point.value = float(data['price'])
            point.close = point.value
            
            return point
        except Exception as e:
            self._log_error(f"Error parsing data: {e}")
            return None
```

### **Step 2: Add Configuration**
```python
# In algorithms/6_algorithms/MyAlgorithm.py
def initialize(self):
    # Add custom data source
    self.add_data(MyNewDataSource, "SYMBOL", Resolution.MINUTE)
```

### **Step 3: Create Documentation**
```markdown
# File: connectors/MY_NEW_DATA_SOURCE_GUIDE.md
# MyNewDataSource Integration Guide

## Overview
- Source: Example Data Provider
- Coverage: [Describe coverage]
- Cost: [Free/Paid/API limits]
- Documentation: [Link to official docs]

## Setup
1. Get API key from [provider]
2. Add to environment variables
3. Configure rate limits

## Usage Examples
[Provide code examples]

## Troubleshooting
[Common issues and solutions]
```

## 🔍 **Connector Testing**

### **Unit Tests**
```python
# File: utils/tests/test_connectors.py
import unittest
from connectors.YahooFinanceMinuteData import YahooFinanceMinuteData

class TestYahooFinanceConnector(unittest.TestCase):
    def setUp(self):
        self.connector = YahooFinanceMinuteData()
    
    def test_data_retrieval(self):
        # Test successful data retrieval
        config = MockConfig("AAPL")
        source = self.connector.get_source(config, datetime.now(), False)
        self.assertIsNotNone(source)
    
    def test_error_handling(self):
        # Test error handling
        config = MockConfig("INVALID_SYMBOL")
        source = self.connector.get_source(config, datetime.now(), False)
        # Should handle gracefully
    
    def test_rate_limiting(self):
        # Test rate limiting compliance
        pass
```

### **Integration Tests**
```python
def test_full_pipeline():
    # Test connector → staging → bronze pipeline
    connector = YahooFinanceMinuteData()
    
    # Fetch data
    data = connector.get_source(config, date, False)
    
    # Validate staging
    staging_data = process_to_staging(data)
    assert_valid_staging_data(staging_data)
    
    # Validate bronze layer
    bronze_data = process_to_bronze(staging_data)
    assert_valid_bronze_data(bronze_data)
```

## 🚨 **Best Practices**

### **1. Security**
- Never hardcode API keys
- Use environment variables or secure config
- Encrypt API keys at rest
- Rotate keys regularly

### **2. Reliability**
- Implement comprehensive error handling
- Use circuit breaker pattern for failing APIs
- Log all API calls and responses
- Monitor connector health

### **3. Performance**
- Respect rate limits strictly
- Use connection pooling for HTTP requests
- Implement caching where appropriate
- Monitor response times

### **4. Data Quality**
- Validate all incoming data
- Handle missing or null values
- Check data freshness
- Implement data quality metrics

## 📊 **Monitoring & Alerting**

### **Key Metrics**
- API call success rate
- Response time percentiles
- Rate limit adherence
- Data quality scores
- Error rates by connector

### **Alerts**
- API failures exceeding threshold
- Rate limit violations
- Data quality degradation
- Extended downtime

## 🦄 **Unicorn Platform Standards**

All connectors must follow:
- **Emoji Logging**: Use emojis for log clarity
- **Error Recovery**: Graceful degradation
- **Performance**: Sub-second response targets
- **Documentation**: Complete integration guides
- **Testing**: Unit and integration test coverage

---

*Connectors are the foundation of our data pipeline - ensure they are robust, reliable, and well-monitored!*
