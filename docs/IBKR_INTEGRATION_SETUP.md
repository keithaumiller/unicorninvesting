# Interactive Brokers (IBKR) Integration Setup

## Overview

Your system can leverage QuantConnect's LEAN engine which has **comprehensive Interactive Brokers integration built-in**. LEAN provides a complete IBKR brokerage implementation with:

- Live trading through TWS or IB Gateway
- Real-time market data
- Order management (market, limit, stop, etc.)
- Portfolio synchronization
- Commission and fee models
- Account management

## Prerequisites

### 1. Interactive Brokers Account
- Active IBKR account (Individual or Professional)
- API access enabled in your account settings
- TWS (Trader Workstation) or IB Gateway installed

### 2. Enable API Access
1. Log into your IBKR account
2. Go to **Settings > Account Settings > Trading Permissions**
3. Enable **Enable API**
4. Set **Socket port** (default: 4001 for TWS, 4002 for IB Gateway)
5. Enable **Download open orders on connection**
6. Configure **Trusted IPs** if needed

## Configuration Setup

### 1. Update LEAN Configuration

Edit `/workspaces/unicorninvesting/BackendPython/Lean/Launcher/config.json`:

```json
{
  "environment": "live-interactive",
  
  // Interactive Brokers configuration
  "ib-account": "YOUR_ACCOUNT_NUMBER",
  "ib-user-name": "YOUR_USERNAME", 
  "ib-password": "YOUR_PASSWORD",
  "ib-host": "127.0.0.1",
  "ib-port": "4002",  // 4001 for TWS, 4002 for IB Gateway
  "ib-agent-description": "Individual",
  "ib-tws-dir": "C:\\Jts",  // Path to IB Gateway/TWS
  "ib-trading-mode": "paper",  // "paper" or "live"
  "ib-enable-delayed-streaming-data": false,
  
  // Live trading settings
  "live-mode-brokerage": "InteractiveBrokersBrokerage",
  "data-queue-handler": "InteractiveBrokersBrokerage",
  "real-time-handler": "InteractiveBrokersBrokerage",
  "transaction-handler": "InteractiveBrokersBrokerage",
  "history-provider": "InteractiveBrokersBrokerage"
}
```

### 2. Environment Variables (Recommended)
For security, use environment variables:

```bash
export QC_IB_ACCOUNT="YOUR_ACCOUNT_NUMBER"
export QC_IB_USER_NAME="YOUR_USERNAME"
export QC_IB_PASSWORD="YOUR_PASSWORD"
export QC_IB_TRADING_MODE="paper"  # or "live"
```

### 3. Live Algorithm Configuration

Create a live trading algorithm configuration:

```python
# /workspaces/unicorninvesting/BackendPython/unicorn/algorithms/live_trading_config.py

class LiveTradingConfig:
    """Configuration for live IBKR trading"""
    
    # Brokerage settings
    BROKERAGE = "InteractiveBrokersBrokerage"
    ENVIRONMENT = "live-interactive"
    
    # Account settings
    ACCOUNT = os.getenv("QC_IB_ACCOUNT", "")
    USERNAME = os.getenv("QC_IB_USER_NAME", "")
    PASSWORD = os.getenv("QC_IB_PASSWORD", "")
    
    # Connection settings
    HOST = "127.0.0.1"
    PORT = 4002  # IB Gateway
    TRADING_MODE = os.getenv("QC_IB_TRADING_MODE", "paper")
    
    # Risk settings
    MAX_POSITION_SIZE = 10000  # USD
    MAX_DAILY_TRADES = 50
    STOP_LOSS_PERCENT = 0.02  # 2%
```

## Installation Steps

### 1. Install IB Gateway
```bash
# Download IB Gateway from IBKR website
# Install and configure with your credentials
```

### 2. Install LEAN IBKR Extension
The IBKR integration is already included in the main LEAN repository at:
`/workspaces/unicorninvesting/BackendPython/Lean/`

### 3. Configure TWS/Gateway
1. Start IB Gateway or TWS
2. Navigate to **Configure > Settings > API**
3. Set **Socket port**: 4002 (Gateway) or 4001 (TWS)
4. Check **Enable ActiveX and Socket Clients**
5. Add **127.0.0.1** to trusted IPs
6. Set **Master API client ID**: 0

### 4. Test Connection
```python
# Test script to verify IBKR connection
import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/Lean')

from QuantConnect.Brokerages.InteractiveBrokers import InteractiveBrokersBrokerage
from QuantConnect import *

# Create brokerage instance
brokerage = InteractiveBrokersBrokerage(
    algorithm=None,
    orderProvider=None,
    securityProvider=None
)

# Test connection
try:
    brokerage.Connect()
    print("✅ IBKR Connection successful!")
    print(f"Connected: {brokerage.IsConnected}")
    brokerage.Disconnect()
except Exception as e:
    print(f"❌ Connection failed: {e}")
```

## Integration with Unicorn Portfolio System

### 1. Create IBKR Data Provider
```python
# /workspaces/unicorninvesting/BackendPython/unicorn/backend/data_providers/ibkr_provider.py

class IBKRDataProvider:
    """Interactive Brokers data provider for Unicorn system"""
    
    def __init__(self, config):
        self.config = config
        self.brokerage = None
        self._initialize_brokerage()
    
    def _initialize_brokerage(self):
        """Initialize IBKR brokerage connection"""
        from QuantConnect.Brokerages.InteractiveBrokers import InteractiveBrokersBrokerage
        
        self.brokerage = InteractiveBrokersBrokerage(
            algorithm=None,
            orderProvider=self._create_order_provider(),
            securityProvider=self._create_security_provider(),
            account=self.config.ACCOUNT,
            host=self.config.HOST,
            port=self.config.PORT,
            ibDirectory=self.config.TWS_DIR,
            userName=self.config.USERNAME,
            password=self.config.PASSWORD,
            tradingMode=self.config.TRADING_MODE
        )
    
    def connect(self):
        """Connect to IBKR"""
        self.brokerage.Connect()
        return self.brokerage.IsConnected
    
    def get_portfolio_positions(self):
        """Get current portfolio positions"""
        if not self.brokerage.IsConnected:
            self.connect()
        
        holdings = self.brokerage.GetAccountHoldings()
        return self._convert_to_unicorn_format(holdings)
    
    def place_order(self, symbol, quantity, order_type="market"):
        """Place order through IBKR"""
        # Implementation for order placement
        pass
    
    def get_market_data(self, symbols):
        """Get real-time market data"""
        # Implementation for market data
        pass
```

### 2. Create Portfolio Sync Service
```python
# /workspaces/unicorninvesting/BackendPython/unicorn/backend/services/portfolio_sync.py

class PortfolioSyncService:
    """Sync between Unicorn portfolio and IBKR account"""
    
    def __init__(self, ibkr_provider, unicorn_db):
        self.ibkr = ibkr_provider
        self.db = unicorn_db
    
    def sync_positions(self):
        """Sync positions from IBKR to Unicorn database"""
        ibkr_positions = self.ibkr.get_portfolio_positions()
        
        for position in ibkr_positions:
            self._update_unicorn_position(position)
    
    def execute_rebalancing(self, target_allocations):
        """Execute portfolio rebalancing through IBKR"""
        current_positions = self.ibkr.get_portfolio_positions()
        trades = self._calculate_required_trades(current_positions, target_allocations)
        
        for trade in trades:
            result = self.ibkr.place_order(
                symbol=trade.symbol,
                quantity=trade.quantity,
                order_type=trade.order_type
            )
            self._log_trade_execution(trade, result)
```

## Trading Modes

### Paper Trading (Recommended for Testing)
```json
{
  "ib-trading-mode": "paper",
  "ib-port": "4002"
}
```

### Live Trading
```json
{
  "ib-trading-mode": "live", 
  "ib-port": "4001"
}
```

## Risk Management Features

### 1. Built-in LEAN Risk Management
- Position sizing limits
- Maximum order quantities
- Daily loss limits
- Market hours validation

### 2. Custom Risk Controls
```python
class UnicornRiskManager:
    """Custom risk management for IBKR integration"""
    
    def validate_order(self, order):
        """Validate order before submission"""
        checks = [
            self._check_position_size(order),
            self._check_account_equity(order),
            self._check_daily_limits(order),
            self._check_market_hours(order)
        ]
        return all(checks)
    
    def monitor_positions(self):
        """Continuous position monitoring"""
        positions = self.ibkr.get_portfolio_positions()
        
        for position in positions:
            if self._requires_stop_loss(position):
                self._place_stop_loss_order(position)
```

## Monitoring and Logging

### 1. Connection Monitoring
```python
class IBKRMonitor:
    """Monitor IBKR connection health"""
    
    def __init__(self, brokerage):
        self.brokerage = brokerage
        self.last_heartbeat = None
    
    def monitor_connection(self):
        """Continuous connection monitoring"""
        while True:
            if not self.brokerage.IsConnected:
                self._handle_disconnect()
            time.sleep(30)  # Check every 30 seconds
    
    def _handle_disconnect(self):
        """Handle connection loss"""
        logging.error("IBKR connection lost - attempting reconnection")
        try:
            self.brokerage.Connect()
            logging.info("IBKR connection restored")
        except Exception as e:
            logging.error(f"Reconnection failed: {e}")
```

### 2. Trade Logging
```python
class TradeLogger:
    """Log all IBKR trades for audit trail"""
    
    def log_order(self, order, result):
        """Log order submission"""
        log_entry = {
            'timestamp': datetime.utcnow(),
            'order_id': order.id,
            'symbol': order.symbol,
            'quantity': order.quantity,
            'price': order.price,
            'status': result.status,
            'brokerage_id': result.brokerage_order_id
        }
        
        # Store in database and files
        self._store_trade_log(log_entry)
```

## Drupal Integration

### 1. Live Portfolio Display
Update your Drupal portfolio pages to show live IBKR data:

```php
// WebFrontend/web/modules/custom/unicorn_metrics/src/Controller/LivePortfolioController.php

class LivePortfolioController extends ControllerBase {
    
    public function getLivePositions() {
        // Call Python backend to get IBKR positions
        $positions = $this->unicornApi->getIBKRPositions();
        
        return [
            '#theme' => 'live_portfolio',
            '#positions' => $positions,
            '#account_value' => $this->getAccountValue(),
            '#daily_pnl' => $this->getDailyPnL()
        ];
    }
}
```

### 2. Real-time Updates
Implement WebSocket or AJAX polling for real-time portfolio updates.

## Security Considerations

### 1. Credential Management
- Store credentials in environment variables
- Use encrypted configuration files
- Implement credential rotation

### 2. Access Control
- Restrict API access to specific IPs
- Use VPN for remote access
- Implement two-factor authentication

### 3. Data Protection
- Encrypt sensitive data at rest
- Use secure communication protocols
- Regular security audits

## Testing Checklist

- [ ] IBKR account configured with API access
- [ ] IB Gateway/TWS running and accessible
- [ ] LEAN configuration updated with credentials
- [ ] Test connection successful
- [ ] Paper trading environment tested
- [ ] Order placement working
- [ ] Portfolio sync functioning
- [ ] Risk management active
- [ ] Monitoring and logging operational
- [ ] Drupal integration displaying live data

## Troubleshooting

### Common Issues

1. **Connection Refused**
   - Check IB Gateway is running
   - Verify port configuration
   - Ensure API is enabled in IBKR settings

2. **Authentication Failed**
   - Verify username/password
   - Check account permissions
   - Ensure 2FA is properly configured

3. **Market Data Issues**
   - Verify market data subscriptions
   - Check exchange permissions
   - Confirm data feed settings

### Support Resources
- [QuantConnect IBKR Documentation](https://www.quantconnect.com/docs/v2/our-platform/live-trading/brokerages/interactive-brokers)
- [IBKR API Documentation](https://interactivebrokers.github.io/tws-api/)
- LEAN Community Forums

## Next Steps

1. **Configure your IBKR account for API access**
2. **Update configuration files with your credentials**
3. **Start with paper trading to test the integration**
4. **Implement portfolio synchronization**
5. **Add live data feeds to your Drupal interface**
6. **Deploy risk management and monitoring**
7. **Scale to live trading when ready**

This integration will provide your Unicorn Investing platform with professional-grade live trading capabilities through Interactive Brokers.
