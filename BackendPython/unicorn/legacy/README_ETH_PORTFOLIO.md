# ETH Portfolio Integration with Drupal

This document outlines the integration between the ETH-only portfolio system and the Drupal 11 frontend.

## Portfolio Structure

### ETH Portfolio Algorithm (`EthOnlyPortfolio.py`)
- **Purpose**: Automated ETH trading with $1,000 initial capital
- **Risk Management**: 5% stop loss, 10% daily loss limit
- **Allocation**: 95% ETH, 5% cash buffer
- **Data Source**: Coinbase ETH/USD minute data

### Key Features:
1. **Automated Rebalancing**: Every 4 hours if allocation drifts >5%
2. **Risk Controls**: Stop loss, position sizing, daily limits
3. **Performance Tracking**: Real-time metrics and reporting
4. **Environment Support**: Backtest → Paper → Live trading

## Drupal Integration Points

### 1. Portfolio Navigation Block
**Location**: `/workspaces/unicorninvesting/WebFrontend/web/modules/custom/portfolio_navigation/`

The existing Portfolio Navigation Block needs extension to display ETH portfolio:

```php
// Add to portfolio_navigation.module
function portfolio_navigation_eth_portfolio_data() {
  return [
    'name' => 'ETH Only Portfolio',
    'value' => '$1,000.00',
    'allocation' => '95% ETH, 5% Cash',
    'performance' => '+0.00%',
    'status' => 'Active',
    'last_update' => date('Y-m-d H:i:s'),
  ];
}
```

### 2. API Endpoints for LEAN Integration

Create REST endpoints in Drupal to receive portfolio updates:

**Endpoint Structure**:
- `POST /api/portfolio/update` - Receive portfolio value updates
- `POST /api/metrics/performance` - Performance metrics
- `POST /api/alerts/risk` - Risk management alerts

### 3. Portfolio Dashboard Page

Create dedicated Drupal page for ETH portfolio:
- Real-time ETH price display
- Portfolio value tracking
- Performance charts
- Risk metrics
- Trading history

## LEAN Configuration Integration

### Configuration File
**Location**: `/workspaces/unicorninvesting/BackendPython/unicorn/config/eth_portfolio_config.json`

Key configuration sections:
```json
{
  "portfolio-name": "ETH Only Portfolio",
  "initial-capital": 1000,
  "target-allocation": {
    "ETHUSD": 0.95,
    "cash": 0.05
  },
  "unicorn-platform": {
    "enable-drupal-integration": true,
    "portfolio-updates-endpoint": "http://localhost/api/portfolio/update"
  }
}
```

## Data Flow Architecture

### Real-time Updates
1. **LEAN Algorithm** → Executes trades and calculates metrics
2. **HTTP API Calls** → Sends updates to Drupal endpoints
3. **Drupal Database** → Stores portfolio data
4. **Frontend Display** → Shows live portfolio status

### Historical Data
1. **LEAN Results** → Stored in `/results/` directory
2. **Batch Processing** → Periodic import to Drupal database
3. **Performance Reports** → Generated for user dashboard

## Database Schema Extensions

### Portfolio Tables
Add to Drupal database:

```sql
-- Portfolio positions
CREATE TABLE unicorn_portfolio_positions (
  id INT AUTO_INCREMENT PRIMARY KEY,
  portfolio_name VARCHAR(100),
  symbol VARCHAR(20),
  quantity DECIMAL(18,8),
  market_value DECIMAL(12,2),
  allocation_percent DECIMAL(5,2),
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Performance metrics
CREATE TABLE unicorn_portfolio_performance (
  id INT AUTO_INCREMENT PRIMARY KEY,
  portfolio_name VARCHAR(100),
  total_value DECIMAL(12,2),
  total_return_percent DECIMAL(8,4),
  daily_return_percent DECIMAL(8,4),
  sharpe_ratio DECIMAL(8,4),
  max_drawdown DECIMAL(8,4),
  recorded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## Security Considerations

### API Authentication
- Implement API key authentication for LEAN→Drupal communication
- Rate limiting on portfolio update endpoints
- Input validation for all financial data

### Data Validation
- Verify portfolio values within reasonable ranges
- Validate ETH prices against external sources
- Alert on unusual trading patterns

## Deployment Strategy

### Phase 1: Backtest Integration
1. Run ETH portfolio backtest using launcher script
2. Import results into Drupal database
3. Display historical performance on dashboard

### Phase 2: Paper Trading
1. Configure paper trading environment
2. Enable real-time updates to Drupal
3. Test all integration endpoints

### Phase 3: Live Trading (Future)
1. Complete IBKR integration setup
2. Implement additional risk controls
3. Enable live portfolio monitoring

## Usage Instructions

### Running ETH Portfolio
```bash
# Navigate to scripts directory
cd /workspaces/unicorninvesting/BackendPython/unicorn/scripts/

# Launch ETH portfolio (interactive menu)
./launch_eth_portfolio.sh

# Choose option 1 for safe backtesting
```

### Viewing Results in Drupal
1. Access Portfolio Navigation Block
2. Click "ETH Only Portfolio" 
3. View real-time metrics and performance
4. Access detailed analytics dashboard

## Files Created

### Algorithm Files
- `algorithms/EthOnlyPortfolio.py` - Main trading algorithm
- `config/eth_portfolio_config.json` - Portfolio configuration
- `scripts/launch_eth_portfolio.sh` - Launch script

### Integration Files
- Portfolio Navigation Block (existing, needs extension)
- API endpoints (to be created)
- Database schema (to be implemented)

## Next Steps

1. **Test Backtest**: Run initial backtest to validate algorithm
2. **Extend Drupal Module**: Add ETH portfolio to navigation
3. **Create API Endpoints**: Implement LEAN→Drupal communication
4. **Build Dashboard**: Create comprehensive portfolio view
5. **Enable Paper Trading**: Test with live data feeds

This integration provides a foundation for the ETH-only portfolio while maintaining the safety and professionalism required for a financial platform.
