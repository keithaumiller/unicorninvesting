# Trading Algorithms

## 🚀 Purpose
This directory contains **pure trading strategy algorithms** that are separated from risk calculation logic.

## 🎯 Scope
Trading algorithms focus exclusively on:
- Signal generation and alpha models
- Entry and exit logic
- Portfolio optimization strategies
- Rebalancing algorithms
- Asset allocation decisions
- Market timing strategies

## 🚫 What NOT to Include
- Risk calculations (VaR, volatility, etc.)
- Risk limit enforcement
- Portfolio risk assessment
- Stress testing

## 📁 Structure
```
trading_algorithms/
├── README.md                   # This file
├── alpha_models.py            # Signal generation models
├── portfolio_optimizer.py     # Portfolio optimization algorithms
├── rebalancer.py              # Rebalancing strategies
├── signal_combiner.py         # Multi-signal combination
├── market_timer.py            # Market timing algorithms
└── asset_selector.py          # Asset selection logic
```

## 🔗 Integration
These algorithms generate trading signals and allocation decisions that are then validated by risk algorithms before execution.

---
**Last Updated**: August 30, 2025
