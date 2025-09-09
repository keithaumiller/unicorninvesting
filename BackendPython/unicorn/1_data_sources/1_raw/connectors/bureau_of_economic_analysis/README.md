# Bureau of Economic Analysis (BEA) API Connector

## Overview

The BEA Connector is a production-ready Python module that collects comprehensive macroeconomic data from the U.S. Bureau of Economic Analysis. It provides essential economic context for crypto trading strategies by collecting GDP, consumer spending, business investment, and international trade data that influences market cycles and capital allocation decisions.

## 🚀 Quick Start

### 1. Get BEA API Key
```bash
# Get free API key from: https://apps.bea.gov/API/signup/
export BEA_API_KEY="your_api_key_here"

# Add to ~/.bashrc for persistence
echo 'export BEA_API_KEY="your_api_key_here"' >> ~/.bashrc
```

### 2. Test Connection
```bash
# Test the connector
python3 bea_connector.py --test

# Or use the collection script
./collect_bea_data.sh test
```

### 3. Collect Data
```bash
# Quick update (critical indicators, last 2 years)
python3 bea_connector.py --delta-update

# Daily update (all important indicators, last 5 years)  
python3 bea_connector.py --daily-update

# Full historical collection (all datasets, 20+ years)
python3 bea_connector.py --comprehensive
```

## 📊 Economic Data Coverage

### Critical Indicators (High Crypto Relevance)
- **Real GDP Growth** - Quarterly and annual economic growth rates
- **Personal Consumption Expenditures (PCE)** - Consumer spending patterns  
- **Gross Private Investment** - Business and residential investment
- **Current Account Balance** - International trade and capital flows
- **Personal Saving Rate** - Consumer financial behavior

### Important Indicators
- **GDP by Industry** - Sector-specific economic performance
- **International Trade in Services** - Services trade balance
- **Regional GDP** - State and metropolitan area growth
- **Fixed Asset Investment** - Infrastructure and equipment spending

### Comprehensive Coverage
- **15+ Economic Datasets** spanning GDP, consumption, investment, and trade
- **6 Economic Categories** with crypto relevance ratings (1-3)
- **20+ Years Historical Data** from 2000 to present
- **Multiple Frequencies** - Annual, quarterly, and monthly data

## 🔧 Features

### Data Collection
- **Smart Throttling** - Respects BEA API rate limits with adaptive delays
- **Comprehensive Error Handling** - Robust retry logic and graceful failures
- **Data Quality Validation** - Automatic data cleaning and validation
- **Multiple Update Modes** - Delta, daily, and comprehensive collection

### Alpha Model Integration
- **Feature Engineering** - Creates 50+ alpha model features from raw economic data
- **Economic Regime Detection** - Recession, growth, and transition indicators  
- **Growth Acceleration Metrics** - Second-order economic momentum signals
- **Economic Strength Index** - Composite indicator from multiple series

### Automation Support
- **Command-Line Interface** - Easy integration with cron jobs and scripts
- **Comprehensive Logging** - Detailed logs for monitoring and debugging
- **File Management** - Organized data storage with timestamps and metadata
- **Status Monitoring** - Health checks and system validation

## 📁 File Structure

```
bureau_of_economic_analysis/
├── bea_connector.py           # Main BEA API connector (900+ lines)
├── config.py                  # Dataset configuration and parameters
├── collect_bea_data.sh        # Shell script for automation
├── README.md                  # This documentation
└── data/                      # Generated data directory
    ├── bea_critical_latest.csv      # Latest critical indicators
    ├── bea_comprehensive_latest.csv # Complete dataset
    ├── bea_features_latest.csv      # Alpha model features
    └── *.csv                        # Timestamped data files
```

## 💻 Usage Examples

### Basic Usage
```python
from bea_connector import BEAConnector

# Initialize connector
bea = BEAConnector(api_key="your_key_here")

# Get critical economic indicators
critical_data = bea.get_critical_indicators(start_year=2020)

# Create alpha model features
alpha_features = bea.create_alpha_features(critical_data)

# Save data for integration
files_saved = bea.save_data_for_alpha_models()
```

### Command Line Usage
```bash
# Test connection and collect sample data
python3 bea_connector.py --test

# Quick delta update (recommended for frequent updates)
python3 bea_connector.py --delta-update

# Complete daily update (recommended for daily cron jobs)
python3 bea_connector.py --daily-update

# Full historical collection (one-time setup)
python3 bea_connector.py --comprehensive
```

## 🔄 Automation & Scheduling

### Cron Job Setup
```bash
# Add to crontab for automated data collection
crontab -e

# Delta updates every 6 hours (recommended)
0 */6 * * * /path/to/collect_bea_data.sh delta

# Daily updates at 6 AM (after BEA data releases)
0 6 * * * /path/to/collect_bea_data.sh daily

# Weekly comprehensive check (Sunday at 2 AM)
0 2 * * 0 /path/to/collect_bea_data.sh comprehensive
```

### Integration with FRED Data
```bash
# Combine with FRED data collection for complete economic picture
# Morning: BEA data (GDP, spending, investment)
0 6 * * * /path/to/collect_bea_data.sh daily

# Evening: FRED data (monetary policy, rates)
0 21 * * * /path/to/collect_fred_historical.sh
```

## 🎯 Success Metrics

Current implementation provides:
- ✅ **15+ Economic Datasets** across 6 categories
- ✅ **20+ Years Historical Data** (2000-present)
- ✅ **50+ Alpha Model Features** automatically generated
- ✅ **3 Update Modes** for different automation needs
- ✅ **Comprehensive Error Handling** with retry logic
- ✅ **Production-Ready Logging** and monitoring

## 📚 Additional Resources

### BEA Documentation
- **API Documentation**: https://apps.bea.gov/API/signup/
- **Data Release Schedule**: https://www.bea.gov/news/schedule
- **User Guide**: https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf

### Economic Data Context
- **GDP Reports**: Released quarterly with preliminary, revised, and final estimates
- **Personal Income/Spending**: Released monthly with ~30-day lag
- **Trade Data**: Released monthly with ~45-60 day lag
- **Investment Data**: Released quarterly as part of GDP accounts

---

**Status**: ✅ Production Ready  
**Integration**: 🔄 Ready for Alpha Model Development  
**Documentation**: ✅ Complete  
**Architecture**: ✅ LEAN Framework Compatible