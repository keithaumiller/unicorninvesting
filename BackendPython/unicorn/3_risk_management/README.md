# Risk Management Layer 3

## Overview

This directory contains the LEAN Layer 3 risk management components for the Unicorn Investing platform.

## Structure

- Portfolio-specific risk algorithms are located in: `4_portfolios/Myportolio/risk_algorithms/`
- Global risk management controls and limits are implemented here

## Risk Management Methodologies

The platform implements risk management through:

1. **Basic Risk** - Simple position and exposure limits
2. **Kelly Criterion** - Optimal position sizing
3. **VaR Models** - Value at Risk calculations
4. **Monte Carlo** - Risk simulation and stress testing

## Integration

Risk management integrates with:
- Portfolio construction (Layer 4)
- Alpha models (Layer 2) 
- Execution models (Layer 5)

Refer to `4_portfolios/Myportolio/risk_algorithms/` for implementation details.
