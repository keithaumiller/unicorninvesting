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

## Advanced Execution Methodologies - Out of Scope

The following advanced order execution methodologies are designed for **large-scale volume traders** and are **not in scope** for this system at this time:

### **Large Volume Trading Algorithms (Not Implemented)**
- **TWAP (Time-Weighted Average Price)**: Systematic time-based order distribution for large institutional orders
- **VWAP (Volume-Weighted Average Price)**: Volume-matched execution algorithms for minimizing market impact
- **Implementation Shortfall**: Advanced order optimization balancing market impact vs. timing risk
- **Iceberg Orders**: Large order fragmentation and concealment for institutional-size positions

### **Scope Rationale**
These methodologies are specifically designed for:
- **Institutional Volume**: Multi-million dollar order sizes
- **Market Impact Minimization**: Required for orders that significantly affect market prices
- **Professional Trading Desks**: Large-scale algorithmic trading operations

**Current System Focus**: Optimized for individual and small institutional trading with standard market and limit order execution through IBKR integration.

## Integration

Risk management integrates with:
- Portfolio construction (Layer 4)
- Alpha models (Layer 2) 
- Execution models (Layer 5)

Refer to `4_portfolios/Myportolio/risk_algorithms/` for implementation details.
