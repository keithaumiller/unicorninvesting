# Risk Algorithms

## 🛡️ Purpose
This directory contains **pure risk calculation algorithms** that are separated from trading strategy logic.

## 🎯 Scope
Risk algorithms focus exclusively on:
- Risk metric calculations (VaR, CVaR, volatility)
- Portfolio risk assessment
- Correlation analysis
- Drawdown calculations
- Risk budgeting algorithms
- Stress testing scenarios

## 🚫 What NOT to Include
- Trading signals or entry/exit logic
- Portfolio optimization algorithms
- Alpha generation models
- Execution strategies

## 📁 Structure
```
risk_algorithms/
├── README.md                   # This file
├── var_calculator.py          # Value at Risk calculations
├── correlation_analyzer.py    # Asset correlation analysis
├── volatility_estimator.py    # Volatility estimation models
├── drawdown_calculator.py     # Maximum drawdown analysis
├── stress_tester.py           # Stress testing scenarios
└── risk_budgeting.py          # Risk budget allocation
```

## 🔗 Integration
These algorithms are consumed by the risk management layer but remain independent of trading strategies.

---
**Last Updated**: August 30, 2025
