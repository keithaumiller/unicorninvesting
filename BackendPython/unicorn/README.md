# Unicorn Investing - Algorithm Organization Structure

## Current Directory Structure

```
BackendPython/unicorn/
├── algorithms/                 # ✅ LEAN Trading Algorithms (Python)
│   ├── EthOnlyPortfolio.py            # ETH-focused portfolio
│   ├── advanced_forex_forecasting_algorithm.py  # Multi-model ensemble (ARIMA + Neural + Prophet)
│   ├── prophet_forex_algorithm.py     # Prophet-only forecasting
│   ├── unicorn_basic_forex_algorithm.py  # Basic SMA crossover
│   ├── XGBoostForexAlgorithm.py       # XGBoost ML trading
│   └── ensemble_forex_algorithm.py    # (empty - needs consolidation)
├── scripts/                    # ✅ Algorithm Runners & Utilities
│   ├── launch_eth_portfolio.sh        # ETH portfolio launcher
│   └── compare_algorithms.sh          # Algorithm comparison tool
├── config/                     # ✅ Configuration Files
│   └── eth_portfolio_config.json      # ETH portfolio configuration
├── backtesting/               # ✅ Backtesting Results & Analysis
├── tests/                     # ✅ Unit Tests & Validation
└── results/                   # ✅ Algorithm Output & Performance Data
```

## Recommended Consolidation

### 1. Algorithm Files (Keep in `/algorithms/`)
- ✅ **Keep existing**: Advanced forecasting algorithms using Prophet + ARIMA
- ❌ **Remove duplicates**: XGBoost should enhance existing ensemble, not replace
- 🔄 **Consolidate**: Combine forecasting approaches into single ensemble

### 2. Script Files (Keep in `/scripts/`)
- ✅ **Algorithm launchers**: Individual algorithm runners
- ✅ **Comparison tools**: Multi-algorithm backtesting
- ✅ **Utilities**: Setup, configuration, analysis scripts

### 3. Configuration Files (Keep in `/config/`)
- ✅ **Algorithm configs**: Per-algorithm JSON configurations
- ✅ **Environment configs**: Backtest, paper, live settings

## Algorithm Capabilities Matrix

| Algorithm | Forecasting Method | Assets | Complexity | Status |
|-----------|-------------------|---------|------------|---------|
| `advanced_forex_forecasting_algorithm.py` | ARIMA + Neural + Prophet | Multi-forex | High | ✅ Complete |
| `prophet_forex_algorithm.py` | Prophet only | Forex | Medium | ✅ Complete |
| `unicorn_basic_forex_algorithm.py` | SMA crossover | Forex + ETH | Low | ✅ Complete |
| `EthOnlyPortfolio.py` | Technical analysis | ETH only | Medium | ✅ Complete |
| `XGBoostForexAlgorithm.py` | XGBoost ML | Multi-forex + ETH | High | 🔄 Should enhance ensemble |

## Recommended Actions

1. **Enhance Existing Ensemble**: Add XGBoost as 4th model to `advanced_forex_forecasting_algorithm.py`
2. **Consolidate Scripts**: Move algorithm-specific scripts to `/scripts/`
3. **Standardize Configs**: Ensure all algorithms have JSON configs in `/config/`
4. **Consistent Naming**: Follow `[Method][Asset]Algorithm.py` pattern

## Current Best Practices ✅

- All algorithms inherit from `QCAlgorithm`
- Standard LEAN imports and structure
- Unicorn logging conventions with emojis
- Proper risk management patterns
- Environment separation (backtest/paper/live)
