# 📊 ETH Data Requirements Specification
## IBKR Data Collection Strategy for All Use Cases

---

## 🎯 **DATA REQUIREMENTS SUMMARY**

### **📈 Core ETH Data Requirements**
```
Symbol: ETHUSD (or ETH-USD depending on IBKR contract)
Asset Class: Cryptocurrency
Exchange: Multiple (Coinbase, Binance, etc. via IBKR)
Data Type: OHLCV (Open, High, Low, Close, Volume)
```

### **⏱️ Timeframe Requirements by Use Case**

| Use Case | Timeframes Needed | Lookback Period | Update Frequency |
|----------|------------------|-----------------|------------------|
| Model Development | 1min, 5min, 15min, 1hr, 1day | 2+ years | Daily (historical) |
| Live Trading | 1min (primary), 5min | Real-time | Real-time |
| Risk Analysis | 1min, 1hr, 1day | 3+ years | Daily |

---

## 📊 **USE CASE 1: FORECASTING MODEL DEVELOPMENT**

### **Historical Data Requirements**

#### **Primary Dataset**
```yaml
Data Specification:
  Symbol: ETHUSD
  Timeframes: [1min, 5min, 15min, 1hr, 1day]
  Period: 730 days (2 years minimum)
  Fields: [open, high, low, close, volume]
  Quality: Complete trading days only
  Timezone: UTC
```

#### **IBKR API Calls Required**
```python
# Historical data collection calls
historical_datasets = [
    {
        'timeframe': '1min',
        'period': '730d',
        'bar_size': '1min',
        'estimated_records': 1051200,  # ~730 days * 24h * 60min
        'use_case': 'High-frequency feature engineering'
    },
    {
        'timeframe': '5min', 
        'period': '730d',
        'bar_size': '5mins',
        'estimated_records': 210240,   # ~730 days * 24h * 12 (5min bars)
        'use_case': 'Primary trading signals'
    },
    {
        'timeframe': '15min',
        'period': '730d', 
        'bar_size': '15mins',
        'estimated_records': 70080,    # ~730 days * 24h * 4 (15min bars)
        'use_case': 'Medium-term trend analysis'
    },
    {
        'timeframe': '1hr',
        'period': '730d',
        'bar_size': '1h',
        'estimated_records': 17520,    # ~730 days * 24h
        'use_case': 'Trend and regime detection'
    },
    {
        'timeframe': '1day',
        'period': '1095d',  # 3 years for daily
        'bar_size': '1d',
        'estimated_records': 1095,     # ~3 years daily
        'use_case': 'Long-term trend analysis'
    }
]
```

#### **Feature Engineering Data Needs**
```yaml
Technical Indicators:
  Moving Averages: SMA(10,20,50,200), EMA(12,26)
  Momentum: RSI(14), MACD(12,26,9), Stochastic(14,3,3)
  Volatility: Bollinger Bands(20,2), ATR(14), VIX-like measures
  Volume: Volume SMA(20), VWAP, On-Balance Volume
  
Price Features:
  Returns: 1min, 5min, 15min, 1hr, daily returns
  Log Returns: Natural log of price returns
  Volatility: Rolling volatility (multiple windows)
  Price Ratios: High/Low, Close/Open ratios
  
Time Features:
  Hour of day: 0-23 (crypto trades 24/7)
  Day of week: 0-6 
  Month: 1-12
  Quarter: 1-4
  
Market Microstructure:
  Bid-Ask Spread: If available via market data
  Order Book: Level 2 data if available
  Trade Size: Average trade size analysis
```

#### **Data Quality Requirements**
```yaml
Completeness: >95% of expected data points
Accuracy: No obvious price spikes or errors
Consistency: Timestamps in correct sequence
Validation: Cross-check with external sources (CoinGecko, etc.)
```

---

## 📈 **USE CASE 2: LIVE TRADING**

### **Real-time Data Requirements**

#### **Primary Data Stream**
```yaml
Real-time Specification:
  Symbol: ETHUSD
  Primary Timeframe: 1min bars
  Latency: <1 second from market
  Fields: [open, high, low, close, volume, timestamp]
  Update Frequency: Every minute
  Backup Timeframe: 5min bars (for redundancy)
```

#### **IBKR API Integration**
```python
# Real-time data subscription
realtime_subscription = {
    'symbol': 'ETHUSD',
    'data_type': 'OHLCV',
    'fields': [31, 55, 70, 71, 84, 86],  # IBKR field codes
    'update_frequency': 'real_time',
    'streaming': True,
    'quality_checks': {
        'latency_threshold': 1000,  # milliseconds
        'data_validation': True,
        'heartbeat_interval': 30    # seconds
    }
}

# IBKR Field Codes for real-time data
IBKR_FIELDS = {
    31: 'Last Price',
    55: 'Symbol', 
    70: 'High',
    71: 'Low',
    84: 'Volume',
    86: 'Close'
}
```

#### **Real-time Feature Calculation**
```yaml
Live Features Required:
  Technical Indicators:
    - SMA(10, 20) on 1min data
    - RSI(14) on 5min data  
    - MACD on 15min data
    - Bollinger Bands(20,2) on 5min data
    
  Market State:
    - Current volatility (20-period rolling)
    - Recent price momentum (5-period)
    - Volume trend (10-period SMA)
    
  Risk Metrics:
    - Unrealized P&L
    - Current position size
    - Distance from entry price
    - Current drawdown
```

#### **Data Buffering Strategy**
```python
# Data buffering for real-time feature calculation
buffer_requirements = {
    '1min_buffer': {
        'size': 200,  # Keep 200 1-min bars (3.3 hours)
        'purpose': 'Short-term indicators'
    },
    '5min_buffer': {
        'size': 100,  # Keep 100 5-min bars (8.3 hours) 
        'purpose': 'Medium-term indicators'
    },
    '15min_buffer': {
        'size': 50,   # Keep 50 15-min bars (12.5 hours)
        'purpose': 'Trend indicators'
    }
}
```

---

## 🛡️ **USE CASE 3: RISK ANALYSIS**

### **Extended Historical Data Requirements**

#### **Risk Analysis Dataset**
```yaml
Extended Historical Data:
  Symbol: ETHUSD
  Timeframes: [1min, 1hr, 1day]
  Period: 1095+ days (3+ years)
  Purpose: Capture multiple market cycles
  Special Requirements:
    - Include extreme events (2020 crash, 2022 crypto winter)
    - High-quality data during volatile periods
    - Cross-market correlation data
```

#### **Market Event Data**
```python
# Special historical periods for stress testing
critical_periods = [
    {
        'name': 'COVID Crash',
        'start_date': '2020-03-01',
        'end_date': '2020-05-01',
        'description': 'Major market crash and recovery',
        'data_requirements': 'minute-level data for precise stress testing'
    },
    {
        'name': 'Crypto Winter 2022',
        'start_date': '2022-05-01', 
        'end_date': '2022-12-31',
        'description': 'Crypto bear market and volatility',
        'data_requirements': 'Daily and hourly data for regime analysis'
    },
    {
        'name': 'FTX Collapse',
        'start_date': '2022-11-01',
        'end_date': '2022-12-01', 
        'description': 'Liquidity crisis and contagion',
        'data_requirements': 'minute-level for liquidity stress testing'
    }
]
```

#### **Risk Metrics Data Requirements**
```yaml
Portfolio Data:
  Position History: Daily snapshots for 2+ years
  P&L History: Trade-level and daily P&L
  Drawdown Analysis: Peak-to-trough calculations
  
Market Data:
  Correlation Data: ETH vs BTC, ETH vs equity markets
  Volatility Data: Realized and implied volatility
  Liquidity Data: Bid-ask spreads, trading volumes
  
External Data:
  Market Regime Indicators: VIX, fear/greed index
  Macro Economic Data: Interest rates, inflation
  Crypto-specific: Hash rates, on-chain metrics
```

---

## 🔄 **DATA COLLECTION IMPLEMENTATION**

### **IBKR Data Collection Script Enhancement**

```python
# Enhanced ETH data collector for all use cases
class ComprehensiveETHDataCollector:
    def __init__(self, ibkr_gateway):
        self.ibkr = ibkr_gateway
        self.storage = ETHDataStorage()
        
    def collect_for_model_development(self):
        """Collect comprehensive historical data for model training"""
        timeframes = ['1min', '5mins', '15mins', '1h', '1d']
        periods = ['730d', '730d', '730d', '730d', '1095d']
        
        for tf, period in zip(timeframes, periods):
            print(f"Collecting {tf} data for {period}...")
            data = self.ibkr.get_historical_data(
                contract_id=self.eth_contract_id,
                period=period,
                bar_size=tf,
                outside_rth='true'  # Include outside regular trading hours
            )
            
            # Validate and store
            validated_data = self.validate_data(data)
            self.storage.save_historical_data(validated_data, tf)
            
    def setup_live_data_stream(self):
        """Setup real-time data streaming for live trading"""
        # Subscribe to real-time market data
        subscription = self.ibkr.subscribe_market_data(
            contract_id=self.eth_contract_id,
            fields='31,55,70,71,84,86',  # Last, Symbol, High, Low, Volume, Close
            snapshot=False  # Streaming updates
        )
        
        return subscription
        
    def collect_risk_analysis_data(self):
        """Collect extended historical data for risk analysis"""
        # Get 3+ years of data with focus on extreme events
        extended_data = self.ibkr.get_historical_data(
            contract_id=self.eth_contract_id,
            period='1095d',  # 3 years
            bar_size='1d',
            outside_rth='true'
        )
        
        # Also collect minute data for critical periods
        for period in self.critical_periods:
            minute_data = self.ibkr.get_historical_data(
                contract_id=self.eth_contract_id,
                start_date=period['start_date'],
                end_date=period['end_date'],
                bar_size='1min'
            )
            self.storage.save_stress_test_data(minute_data, period['name'])
```

### **Data Storage Strategy**

```python
# Efficient data storage for different use cases
class ETHDataStorage:
    def __init__(self):
        self.base_path = '/workspaces/unicorninvesting/BackendPython/unicorn/data/eth'
        
    def save_historical_data(self, data, timeframe):
        """Save historical data optimized for model development"""
        # Use parquet for efficient storage and fast loading
        filepath = f"{self.base_path}/historical/{timeframe}/eth_{timeframe}_historical.parquet"
        df = pd.DataFrame(data)
        df.to_parquet(filepath, compression='snappy')
        
    def save_realtime_data(self, data):
        """Save real-time data for live trading"""
        # Use time-partitioned storage for real-time data
        timestamp = datetime.now()
        filepath = f"{self.base_path}/realtime/{timestamp.strftime('%Y/%m/%d')}/eth_realtime.parquet"
        # Append to daily file
        
    def save_risk_data(self, data, scenario_name):
        """Save data for risk analysis"""
        filepath = f"{self.base_path}/risk_scenarios/{scenario_name}/data.parquet"
        # Store with metadata for stress testing
```

### **Data Quality Monitoring**

```python
class ETHDataQualityMonitor:
    def __init__(self):
        self.quality_thresholds = {
            'completeness': 0.95,  # 95% of expected data points
            'latency': 1.0,        # <1 second for real-time
            'accuracy': 0.99       # 99% accuracy vs external sources
        }
        
    def monitor_realtime_quality(self, data_stream):
        """Monitor real-time data quality"""
        metrics = {
            'latency': self.calculate_latency(data_stream),
            'completeness': self.check_completeness(data_stream),
            'accuracy': self.validate_prices(data_stream)
        }
        
        # Alert if quality degrades
        for metric, value in metrics.items():
            if value < self.quality_thresholds[metric]:
                self.send_quality_alert(metric, value)
        
        return metrics
```

This comprehensive data specification ensures we collect the right data at the right frequency for each use case while maintaining quality and efficiency.
