# BTC_ETH_Mixed Portfolio

## 🎯 Portfolio Overview

**Strategy**: Balanced Bitcoin (60%) and Ethereum (40%) allocation for diversified cryptocurrency exposure
**Risk Profile**: Medium volatility with crypto diversification benefits
**Target Audience**: Investors seeking balanced exposure to major cryptocurrencies
**Status**: 📋 Planned - Awaiting BTC model completion

## 📊 Portfolio Configuration

### Asset Allocation
- **BTC (Bitcoin)**: 60% - Digital gold, store of value
- **ETH (Ethereum)**: 40% - Smart contract platform, ecosystem growth
- **Cash Reserve**: 5% (for execution and risk management)
- **Currency**: USD base

### Key Metrics
- **Target Annual Volatility**: 22%
- **Expected Sharpe Ratio**: 1.4+
- **Maximum Drawdown**: 18%
- **Rebalancing Frequency**: Weekly

## 🔧 Technical Implementation

### Data Sources
- **Primary**: Interactive Brokers (IBKR Gateway)
  - BTC: BTCUSD contract
  - ETH: Contract ID 541686654 (ETHUSD)
  - Real-time 1-minute bars
  - 24/7 cryptocurrency trading

### Alpha Models
- **BTC Models**: Ensemble (Prophet + XGBoost + Technical) - *In Development*
- **ETH Models**: Enhanced Technical Alpha - ✅ Ready
- **Correlation Models**: Cross-asset signal optimization

### Risk Management
- **Position Limits**: 65% max single asset, 5% cash minimum
- **Cross-Asset Correlation**: Monitor BTC-ETH correlation (target <0.9)
- **VaR Limits**: 7% daily, 13% weekly
- **Diversification Benefit**: Target 10-15% volatility reduction vs single asset

## 📈 Performance Expectations

### Diversification Benefits
- **Volatility Reduction**: 10-15% vs single asset portfolios
- **Correlation**: BTC-ETH correlation ~0.7-0.8
- **Risk-Adjusted Returns**: Higher Sharpe ratio through diversification
- **Drawdown Protection**: Improved maximum drawdown metrics

### Target Metrics
- **Sharpe Ratio**: 1.4-1.7
- **Maximum Drawdown**: 15-18%
- **Win Rate**: 60-65%
- **Annual Return**: 30-50% (medium-high volatility)

## 🚀 Implementation Status

### ✅ Completed
- Portfolio configuration and risk parameters
- ETH model integration ready
- Risk management framework
- Execution infrastructure

### 🔧 In Progress
- BTC ensemble model development
- Cross-asset correlation optimization
- Rebalancing algorithm refinement

### 📋 Pending
- BTC model validation and deployment
- Live trading integration
- Performance monitoring dashboard

## ⚠️ Risk Considerations

### Controlled Risk Factors
- **Diversification**: Reduced single-asset concentration risk
- **Correlation Management**: Monitor and manage BTC-ETH correlation
- **Rebalancing**: Weekly rebalancing to maintain target allocation

### Remaining Risks
- **Crypto Market Risk**: Both assets subject to crypto market cycles
- **Regulatory Risk**: Cryptocurrency regulation affects both assets
- **Technology Risk**: Blockchain-specific risks for both protocols

## 📚 Documentation References

- [Main Portfolio Architecture](../README.md)
- [BTC Alpha Models](../../BackendPython/unicorn/2_alpha_models/CRYPTO/BTC/README.md)
- [ETH Alpha Models](../../BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/README.md)
- [Risk Management Framework](../../docs/RISK_MANAGEMENT_EXECUTIVE_SUMMARY.md)

---

**Last Updated**: August 30, 2025  
**Version**: 1.0  
**Status**: Planned - Awaiting BTC Model Completion  
**Target Deployment**: September 2025
