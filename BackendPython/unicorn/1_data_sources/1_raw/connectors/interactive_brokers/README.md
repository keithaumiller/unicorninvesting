# Interactive Brokers (IBKR) Integration

Interactive Brokers provides professional trading capabilities and real-time market data. **Your account already has API access enabled!** 🎉

## ✅ Current Status (August 2025)

**IBKR Integration**: 🟢 **FULLY OPERATIONAL**
- **Gateway Status**: ✅ Running and authenticated
- **Authentication URL**: https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/
- **Data Collection**: ✅ ETH data successfully collected
- **Health Check**: ✅ Passing all validation checks
- **Last Validated**: Real-time and historical ETH data with OHLCV bars

**⚠️ Authentication Required**: Manual login required via web interface above

## 🔐 Account Information

### Live Trading Account
- **Username**: [Your IBKR Username]
- **Account Number**: U21748632
- **API Access**: ✅ Enabled (confirmed by IBKR support ticket #T895507)
- **API Key**: 🔄 Pending (obtain from Client Portal > Settings > API Settings)

### Paper Trading Account
- **Username**: xyzyuc422
- **Account Number**: DUM785491
- **Purpose**: Safe testing environment for development and validation

> **Note**: Use paper trading account for all testing and development. Switch to live account only when ready for actual trading.

## 🚀 Quick Start - Client Portal Gateway (Required for Individual Clients)

**Individual Clients**: Must use Client Portal Gateway with manual login + 2FA

**⚠️ Important**: OAuth 2.0 authentication is NOT available for individual clients

### Step 1: Download Client Portal Gateway
1. Visit: [IBKR API Downloads](https://www.interactivebrokers.com/en/trading/ib-api.php#api-clients)
2. Download "Client Portal Gateway" (Java application)
3. Extract and run the gateway application

### Step 2: Manual Authentication
1. Gateway requires manual login with IBKR credentials
2. Must complete two-factor authentication (2FA)
3. Gateway starts local server at `https://localhost:5000`
4. Authenticates all API requests automatically

### Step 3: Setup & Test
```bash
./setup_client_portal.sh
./test_ibkr_client_portal.sh
```

## 📊 Available APIs for Individual Clients

### 1. Client Portal Gateway ⭐ **REQUIRED FOR INDIVIDUAL CLIENTS**
**Manual Authentication Required** - Java application with 2FA

**Individual Client Requirements:**
- 🔐 Manual login with IBKR username/password
- 📱 Two-factor authentication (2FA) required
- ☕ Local Java application (Client Portal Gateway)
- 🚫 OAuth 2.0 NOT available for individual clients

**Advantages:**
- ✅ Works with individual IBKR accounts
- ✅ Full market data and trading access
- ✅ Handles authentication automatically after login
- ✅ No API key management required
- ✅ Official IBKR solution for individual clients

**Setup:** Use `./setup_client_portal.sh`

### 2. OAuth 2.0 JWT API (Institutional Only)
**NOT AVAILABLE for Individual Clients** ❌

**Advantages:**
- ✅ No TWS/Gateway software installation
- ✅ Cross-platform compatibility  
- ✅ Web-based authentication
- ✅ Full market data access
- ✅ Complete trading capabilities
- ✅ Modern REST API interface

**Setup:** Use `./setup_client_portal.sh`

### 2. TWS API (Traditional)
**Requires TWS Download** - Desktop application

**Advantages:**
- ✅ Most mature and feature-rich
- ✅ Extensive documentation
- ✅ High-frequency trading support
- ✅ Advanced order types

**Setup:** Use `./setup_ibkr_env.sh` + `./setup_lean_ibkr.sh`

## 🔌 Available Files

### **Modern Client Portal Integration**

#### `IBKRClientPortalConnector.py` ⭐ **NEW**
**Purpose:** Python connector for Client Portal Web API
- **Features:** Real-time data, trading, account management
- **Requirements:** Client Portal Gateway running
- **Authentication:** Web-based (no credentials in code)
- **Status:** ✅ Ready to use

#### `setup_client_portal.sh` ⭐ **NEW**  
**Purpose:** Setup Client Portal API integration
- Configures environment for web API
- Creates configuration files
- Provides setup instructions
- No software downloads during script

### **Traditional TWS Integration**

#### `setup_ibkr_env.sh`
**Purpose:** Environment setup for TWS API
- Configures TWS API credentials
- Sets up authentication
- Requires TWS/Gateway download

#### `setup_lean_ibkr.sh`
**Purpose:** LEAN framework integration with TWS
- Configures LEAN for IBKR brokerage
- Sets up TWS data feeds
- Establishes TWS trading connections

#### `test_ibkr_connection.sh`
**Purpose:** Connection testing for TWS API
- Tests TWS/Gateway connectivity
- Validates authentication
- Checks data feed availability

## 💡 Which API Should I Use?

### Use **Client Portal API** if:
- ✅ You want the simplest setup
- ✅ You prefer web-based authentication
- ✅ You don't want to download TWS software
- ✅ You're building modern applications
- ✅ You want cross-platform compatibility

### Use **TWS API** if:
- ✅ You need maximum performance
- ✅ You're doing high-frequency trading
- ✅ You need advanced order types
- ✅ You're already familiar with TWS

## 🔧 IBKR Capabilities

### Trading Features
- ✅ **Multi-Asset Trading:** Stocks, Options, Futures, Forex, Bonds
- ✅ **Global Markets:** 150+ markets worldwide
- ✅ **Algorithmic Trading:** API-driven automated trading
- ✅ **Real-time Data:** Professional-grade market data
- ✅ **Low Commissions:** Competitive pricing structure
- ✅ **Advanced Orders:** Complex order types and strategies

### Data Access
- **Real-time Market Data:** Level I and Level II
- **Historical Data:** Extensive historical coverage
- **Fundamental Data:** Company financials and metrics
- **Options Data:** Greeks, implied volatility, chains
- **News and Research:** Reuters, Dow Jones, proprietary research

## 📋 Prerequisites

### Account Requirements ✅ **ALREADY COMPLETE**
1. **✅ IBKR Account:** You have an active account
2. **✅ API Permissions:** Already enabled per IBKR support
3. **Market Data Subscriptions:** Check your subscriptions
4. **Trading Permissions:** Verify your authorizations

### Software Requirements
```bash
# Python packages (auto-installed)
pip install requests pandas

# Choose ONE:
# Option 1: Client Portal Gateway (recommended)
# Download from IBKR website

# Option 2: TWS Platform (traditional)  
# Download TWS software
```

## 🚀 Getting Started (Recommended Path)

### 1. Use Client Portal API
```bash
# Setup the integration
./setup_client_portal.sh

# Download Client Portal Gateway from IBKR website
# Start the gateway and authenticate via web browser
# Test the connection
./test_ibkr_client_portal.sh
```

### 2. Python Usage Example
```python
from IBKRClientPortalConnector import IBKRClientPortalConnector

# Initialize connector
ibkr = IBKRClientPortalConnector()

# Authenticate (web-based)
if ibkr.authenticate():
    print("✅ Connected to IBKR")
    
    # Get account info
    accounts = ibkr.get_accounts()
    print(f"Accounts: {len(accounts)}")
    
    # Get market data
    aapl_data = ibkr.get_market_data("AAPL")
    print(f"AAPL Price: {aapl_data}")
    
    # Get historical data
    historical = ibkr.get_historical_data("AAPL", period="1d")
    print(f"Historical bars: {len(historical)}")
```

## 🔒 Security & Best Practices

### Authentication
- **Client Portal:** Web-based login (recommended)
- **TWS API:** Username/password in environment variables
- **No hardcoded credentials** in any code

### Network Security
- **Local Connections:** APIs run on localhost
- **SSL/TLS:** Self-signed certificates (expected)
- **Firewall:** Ensure local ports are accessible

### Risk Management
- **Paper Trading:** Start with paper account
- **Position Limits:** Configure appropriate limits
- **Error Handling:** Comprehensive error checking
- **Logging:** Detailed activity logging

## 📊 Market Data Subscriptions

### Free Data (Basic Account)
- **Delayed Data:** 15-20 minute delayed quotes
- **Basic Symbols:** Major US stocks and ETFs

### Real-time Data (Subscription Required)
- **US Securities:** NASDAQ, NYSE real-time data (~$1-10/month)
- **Options:** OPRA options data
- **Futures:** CME, CBOT, NYMEX data  
- **Forex:** Real-time forex quotes
- **International:** Global market data

### Cost Optimization
- **Bundle Packages:** Often more cost-effective
- **Usage-based:** Pay only for what you use
- **Professional vs Non-professional:** Different rates

## 🔧 Troubleshooting

### Client Portal Issues

1. **Gateway Not Starting**
   - Check Java installation
   - Verify download integrity
   - Check port 5000 availability

2. **Authentication Problems**
   - Clear browser cache
   - Check IBKR account status
   - Verify credentials

3. **SSL Certificate Warnings**
   - Expected behavior (self-signed cert)
   - Click "Advanced" → "Continue to localhost"

### TWS API Issues

1. **Connection Problems**
   - Verify TWS/Gateway is running
   - Check port configuration (4001/4002)
   - Validate API permissions

2. **Authentication Errors**
   - Confirm account credentials
   - Check API user settings
   - Verify trading permissions

### General Issues

1. **Market Data Problems**
   - Check data subscriptions
   - Verify market hours
   - Confirm symbol formats

2. **Order Rejections**
   - Check trading permissions
   - Validate order parameters
   - Review risk management rules

## 🎯 Next Steps

### Immediate Actions
1. **✅ Choose API:** Client Portal (recommended) or TWS
2. **🔧 Run Setup:** Execute appropriate setup script
3. **📱 Download Gateway/TWS:** From IBKR website
4. **🔐 Authenticate:** Complete login process
5. **🧪 Test:** Run connection tests

### Development Path
1. **📊 Market Data:** Test real-time data feeds
2. **💼 Account Info:** Verify account access
3. **📈 Paper Trading:** Test order placement
4. **🤖 Integration:** Connect with LEAN framework
5. **🚀 Production:** Move to live trading (when ready)

## 📚 Resources

### IBKR Documentation
- **Client Portal API:** https://www.interactivebrokers.com/api/doc/portal/
- **TWS API:** https://www.interactivebrokers.com/api/doc/
- **Downloads:** https://www.interactivebrokers.com/en/trading/ib-api.php

### Support
- **IBKR API Support:** Available 24/7
- **Your Support Ticket:** #T895507 (API access confirmed)
- **Community Forums:** Active developer community

### LEAN Integration
- **QuantConnect LEAN:** https://github.com/QuantConnect/Lean
- **IBKR Brokerage:** Built-in LEAN support
- **Documentation:** Extensive LEAN-IBKR guides

---

**🎉 Your IBKR account is ready for API trading! Choose your integration method and get started.**
