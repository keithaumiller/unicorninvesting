# Monte Carlo Risk Simulation

## Overview

Monte Carlo methods for portfolio risk assessment and stress testing.

## Components

- **monte_carlo_risk.py** - Monte Carlo simulation methods

## Features

- Portfolio return simulation
- Geometric Brownian motion modeling
- Risk metrics calculation (VaR, Expected Shortfall, Max Drawdown)

## Usage

```python
from monte_carlo_risk import MonteCarloRisk

mc_risk = MonteCarloRisk(num_simulations=10000)
returns = mc_risk.simulate_portfolio_returns(0.12, 0.20, 252)
metrics = mc_risk.calculate_risk_metrics(returns)
```
