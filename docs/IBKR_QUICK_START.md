# IBKR Integration Quick Start

## Immediate Setup Steps

### 1. Enable IBKR API Access
1. Log into your Interactive Brokers account
2. Go to **Account Management > Settings > Account Settings**
3. Navigate to **Trading Permissions > API**
4. Enable **API Access** 
5. Note your account number

### 2. Configure Environment Variables
```bash
# Add to your shell profile (.bashrc, .zshrc, etc.)
export QC_IB_ACCOUNT="YOUR_ACCOUNT_NUMBER"
export QC_IB_USER_NAME="YOUR_IBKR_USERNAME"  
export QC_IB_PASSWORD="YOUR_IBKR_PASSWORD"
export QC_IB_TRADING_MODE="paper"  # Start with paper trading
```

### 3. Install IB Gateway
1. Download IB Gateway from IBKR website
2. Install and configure with your credentials
3. Start IB Gateway
4. Configure API settings:
   - Socket Port: 4002
   - Enable Socket Clients: ✓
   - Trusted IPs: 127.0.0.1

### 4. Update LEAN Configuration
Edit `/workspaces/unicorninvesting/BackendPython/Lean/Launcher/config.json`:

```json
{
  "environment": "live-interactive",
  "live-mode-brokerage": "InteractiveBrokersBrokerage",
  "data-queue-handler": "InteractiveBrokersBrokerage",
  "ib-port": "4002",
  "ib-trading-mode": "paper"
}
```

### 5. Test Connection
```bash
cd /workspaces/unicorninvesting/BackendPython/Lean
python -c "
from QuantConnect.Brokerages.InteractiveBrokers import InteractiveBrokersBrokerage
print('IBKR integration available!')
"
```

## Integration Points

### Python Backend
- Real-time portfolio data from IBKR
- Order execution through LEAN engine
- Risk management and position sizing

### Drupal Frontend  
- Live portfolio values in dashboard
- Real-time P&L updates
- Order management interface

### Key Benefits
✅ **Professional Trading Platform**: Direct access to IBKR's global markets  
✅ **Real-time Data**: Live quotes, positions, and account values  
✅ **Order Management**: Full order types (market, limit, stop, etc.)  
✅ **Risk Controls**: Built-in position sizing and risk management  
✅ **Commission Tracking**: Accurate cost basis and fee calculations  

## Next Actions
1. Set up IBKR account API access
2. Configure paper trading environment  
3. Test basic connectivity
4. Integrate with existing portfolio system
5. Scale to live trading

The QuantConnect LEAN engine provides enterprise-grade IBKR integration that's production-ready for your unicorn investing platform.
