# Federal Reserve Economic Data (FRED) Connector

Production-ready economic data connector utilizing the `fredapi` library to collect critical macroeconomic indicators for crypto alpha model enhancement.

## 🎯 **Purpose**
Provides essential economic context for crypto trading strategies by collecting Federal Reserve economic data that influences market sentiment and capital flows. Critical for understanding monetary policy impacts on crypto markets.

## 📊 **Key Economic Indicators**

### **Priority 1: Critical for Crypto Models** 
- **Federal Funds Rate** (`FEDFUNDS`, `DFF`) - Monetary policy stance
- **Consumer Price Index** (`CPIAUCSL`, `CPILFESL`) - Inflation indicators  
- **Treasury Yields** (`DGS10`, `DGS2`, `DGS5`) - Risk-free rate benchmarks
- **M2 Money Supply** (`M2SL`) - Liquidity conditions

### **Priority 2: Important Context**
- **Employment** (`UNRATE`, `PAYEMS`) - Economic strength
- **GDP Growth** (`GDP`, `GDPC1`) - Economic health
- **Market Stress** (`VIXCLS`, `NFCI`) - Risk sentiment
- **USD Strength** (`DEXUSEU`, `TWEXBMTH`) - Currency impact

## 🚀 **Quick Start**

### **1. Installation**
```bash
# Install required dependencies
pip install fredapi pandas numpy

# Navigate to connector directory
cd BackendPython/unicorn/1_data_sources/1_raw/connectors/federal_reserve_fred/
```

### **2. API Key Setup**
```bash
# Set environment variable (recommended)
export FRED_API_KEY="your_api_key_here"

# Or pass directly to connector
python -c "from fred_connector import FredConnector; fred = FredConnector(api_key='your_key')"
```

### **3. Basic Usage**
```python
from fred_connector import FredConnector

# Initialize connector
fred = FredConnector()

# Get critical economic indicators (5 years of data)
critical_data = fred.get_critical_indicators()
print(f"Retrieved {len(critical_data.columns)} series, {len(critical_data)} observations")

# Create alpha model features
features = fred.create_alpha_features(critical_data)
print(f"Created {len(features.columns)} features for alpha models")

# Save for alpha model integration
files = fred.save_data_for_alpha_models()
print(f"Files saved: {list(files.keys())}")
```

## 📈 **Crypto Alpha Model Integration**

### **Economic Context for Crypto Trading**
```python
# Get latest economic snapshot
latest_indicators = fred.get_latest_values()

# Check economic regime
fed_funds_rate = latest_indicators.get('FEDFUNDS', 0)
inflation_rate = latest_indicators.get('CPIAUCSL', 0)
vix_level = latest_indicators.get('VIXCLS', 0)

# Economic regime analysis
if fed_funds_rate < 2.0:
    print("Low rate environment - Crypto favorable")
if inflation_rate > 5.0:
    print("High inflation - Alternative asset demand")
if vix_level > 30:
    print("High volatility - Risk-off sentiment")
```

### **Feature Engineering for ETH Models**
```python
# Create features with multiple timeframes
features_data = fred.create_alpha_features(economic_data)

# Key features for crypto models:
# - FEDFUNDS_roc_20: Fed policy momentum (20-day rate of change)
# - yield_curve_slope: 10Y - 2Y spread (recession indicator)
# - inflation_yoy: Year-over-year inflation rate
# - monetary_tightening: Count of rate hikes (60-day window)
# - market_stress: VIX spike indicator

# Integration with existing ETH data
eth_data = load_eth_market_data()  # Your existing ETH data
enhanced_data = eth_data.join(features_data, how='left')
```

## 🔧 **Advanced Features**

### **Economic Regime Detection**
```python
# Automatic regime classification
features = fred.create_alpha_features(economic_data)

# Built-in regime indicators:
# - yield_curve_inversion: 2Y > 10Y (recession signal)
# - vix_spike: VIX > 80th percentile (market stress)  
# - monetary_tightening: Fed hiking cycle detection
# - market_stress: VIX > 30 (high volatility regime)
```

### **Priority-Based Data Collection**
```python
# Get only critical indicators (Priority 1)
critical_data = fred.get_critical_indicators()

# Get important context (Priority 2)  
important_data = fred.get_series_by_priority(2)

# Get specific economic categories
monetary_policy = fred.get_multiple_series(['FEDFUNDS', 'M2SL', 'DFF'])
inflation_data = fred.get_multiple_series(['CPIAUCSL', 'CPILFESL'])
```

### **Custom Date Ranges**
```python
# Recent data for real-time trading
recent_data = fred.get_critical_indicators(
    start_date='2024-01-01',
    end_date='2024-12-31'
)

# Historical analysis
historical_data = fred.get_critical_indicators(
    start_date='2010-01-01',
    end_date='2020-01-01'
)
```

## 📁 **Output Files & Integration**

### **Generated Files**
```
data/
├── fred_critical_indicators_YYYYMMDD_HHMMSS.csv    # Raw economic data
└── fred_raw_latest.csv                             # Latest raw data

../../../processed/economic_indicators/
├── fred_raw_data_YYYYMMDD_HHMMSS.csv              # Timestamped raw data
├── fred_features_YYYYMMDD_HHMMSS.csv              # Engineered features
├── fred_metadata_YYYYMMDD_HHMMSS.json             # Collection metadata
├── fred_raw_latest.csv                             # Latest raw (for automation)
└── fred_features_latest.csv                       # Latest features (for models)
```

### **CSV Format Example**
```csv
date,FEDFUNDS,CPIAUCSL,DGS10,M2SL,FEDFUNDS_roc_20,yield_curve_slope,inflation_yoy
2024-01-01,5.25,307.789,4.02,20830.3,0.024,1.23,3.4
2024-02-01,5.33,308.417,4.18,20854.7,0.031,1.15,3.2
```

## ⚡ **Performance & Reliability**

### **fredapi Library Benefits**
- **Automatic Rate Limiting**: Handles FRED's 120 requests/minute limit
- **Error Handling**: Built-in retry logic and connection management
- **Data Quality**: Automatic NaN handling and data validation
- **Official Support**: Maintained library specifically for FRED API

### **Data Freshness**
- **Update Frequency**: Most series updated daily/monthly by FRED
- **Lag Time**: 1-3 days for most recent data
- **Historical Coverage**: Extensive data back to 1950s+ for many series
- **Reliability**: 99.9%+ uptime from Federal Reserve

## 🧪 **Testing & Validation**

### **Run Test Suite**
```bash
# Comprehensive testing
python test_fred_connector.py

# Quick connection test
python fred_connector.py
```

### **Test Coverage**
- ✅ API connectivity and authentication
- ✅ Single and multiple series retrieval  
- ✅ Critical indicators collection
- ✅ Feature engineering pipeline
- ✅ Alpha model integration
- ✅ Metadata and series information
- ✅ Error handling and edge cases

## 🔍 **Monitoring & Maintenance**

### **Data Quality Checks**
- Missing data detection and interpolation
- Outlier identification using z-score analysis
- Series continuity validation
- Cross-validation between related series

### **Operational Monitoring**
- API response times and success rates
- Data freshness validation
- Feature engineering pipeline health
- Integration with alpha models validation

## 📋 **Configuration**

### **Economic Series Categories**
```python
from config import ECONOMIC_SERIES_CATALOG

# View all available series by category
categories = ['monetary_policy', 'inflation', 'interest_rates', 'employment', 
              'economic_growth', 'market_indicators', 'currency']

for category in categories:
    series_list = ECONOMIC_SERIES_CATALOG[category]['series'].keys()
    print(f"{category}: {list(series_list)}")
```

### **Feature Engineering Settings**
```python
from config import FEATURE_CONFIG

# Customizable feature windows
rate_of_change_windows = [5, 10, 20, 60, 252]  # 1w, 2w, 1m, 3m, 1y
moving_average_windows = [5, 10, 20, 60, 252] 
volatility_windows = [20, 60, 252]
```

### **Economic Regime Thresholds**
```python
from config import REGIME_THRESHOLDS

# Predefined thresholds for regime detection
high_inflation = 5.0          # CPI YoY > 5%
low_rates = 2.0              # Fed Funds < 2%  
inverted_curve = 0.0         # 2Y > 10Y yield
high_volatility = 30.0       # VIX > 30
```

## 🔗 **Integration Points**

### **Alpha Models Integration**
- **Compatible with**: Existing ETH models in `2_alpha_models/`
- **Output format**: Standardized CSV with date index
- **Feature naming**: Consistent `{series}_{transformation}_{window}` pattern
- **Missing data**: Forward-fill and interpolation strategies

### **Portfolio Construction**
- **Risk management**: Economic regime-based position sizing
- **Asset allocation**: Macro factor-based portfolio weights  
- **Rebalancing signals**: Economic cycle-based rebalancing

### **Automation Integration**
- **Cron-ready**: Designed for scheduled data collection
- **Error handling**: Robust failure modes and logging
- **Monitoring**: Comprehensive logging for operational monitoring

## 🚨 **Important Notes**

### **API Usage**
- **Free API Key**: Required from https://fred.stlouisfed.org/docs/api/api_key.html
- **Rate Limits**: 120 requests/minute (handled automatically by fredapi)
- **Data Rights**: Public domain U.S. government data
- **Commercial Use**: Permitted without restrictions

### **Data Considerations**
- **Frequency Alignment**: Mixed frequencies (daily, monthly, quarterly) require careful handling
- **Publication Lags**: Economic data published with 1-4 week delays
- **Revisions**: Historical data subject to revisions (especially GDP, employment)
- **Missing Values**: Holidays and weekends create gaps in daily series

## 📊 **Current Status**

- **Implementation**: ✅ **COMPLETE** - Production ready
- **Testing**: ✅ **COMPREHENSIVE** - Full test suite available
- **Documentation**: ✅ **COMPLETE** - Usage examples and API reference
- **Integration**: 🔄 **READY** - Compatible with existing ETH alpha models
- **Automation**: 🔄 **READY** - Designed for scheduled collection

## 🔄 **Next Steps**

1. **Set API Key**: Export FRED_API_KEY environment variable
2. **Run Tests**: Execute `test_fred_connector.py` for validation
3. **Collect Data**: Run `fred_connector.py` for initial data collection
4. **Integrate Models**: Use generated features in ETH alpha models
5. **Automate**: Set up cron jobs for regular data updates

---

**Dependencies**: `fredapi`, `pandas`, `numpy`  
**API Documentation**: https://fred.stlouisfed.org/docs/api/  
**Library Documentation**: https://github.com/mortada/fredapi  
**Status**: ✅ Production Ready - Full economic data pipeline operational
