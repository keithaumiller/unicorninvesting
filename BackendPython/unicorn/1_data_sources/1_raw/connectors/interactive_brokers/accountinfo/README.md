# IBKR Account Information Repository

This directory contains comprehensive information about IBKR account capabilities, limitations, and API access details for reference and development planning.

## 📁 Directory Structure

```
accountinfo/
├── README.md                    # This file - overview and documentation
├── collect_account_info.py      # Script to gather all account information
├── account_capabilities.json    # Current account capabilities and permissions
├── api_endpoints.json          # Available API endpoints and access levels
├── trading_permissions.json    # Trading permissions and restrictions
├── market_data_access.json     # Market data permissions and limitations
├── account_summary.json        # Basic account information and status
├── risk_parameters.json        # Risk management settings and limits
├── reports/                     # Generated reports and analysis
│   ├── account_analysis.md     # Human-readable account analysis
│   ├── limitations_summary.md  # Summary of account limitations
│   └── capabilities_matrix.md  # Capability matrix for different assets
└── historical/                 # Historical snapshots for comparison
    └── YYYY-MM-DD/             # Date-based snapshots
```

## 🎯 Purpose

- **Reference Documentation**: Comprehensive account capabilities for development planning
- **Limitation Tracking**: Understanding what we can and cannot do with the current account
- **API Mapping**: Complete mapping of available endpoints and access levels
- **Development Planning**: Information needed for implementing trading features
- **Compliance**: Documentation for regulatory and risk management purposes

## 🔧 Collection Process

The `collect_account_info.py` script automatically gathers:

1. **Account Summary**: Basic account information, type, status
2. **Trading Permissions**: What instruments can be traded
3. **Market Data Access**: Real-time vs delayed data permissions
4. **API Endpoints**: Available endpoints and access levels
5. **Risk Parameters**: Account-level risk settings and limits
6. **Portfolio Information**: Current positions and account value
7. **Order Capabilities**: Order types and execution permissions

## 📊 Data Format

All data is stored in JSON format for easy programmatic access, with human-readable Markdown reports generated for reference.

## 🔄 Update Schedule

- **Manual**: Run collection script when account changes occur
- **Automatic**: Can be scheduled to run periodically for monitoring
- **Historical**: Snapshots saved for tracking changes over time

## 🚨 Security Note

This directory contains sensitive account information. Ensure proper access controls and never commit real account credentials or sensitive data to version control.
