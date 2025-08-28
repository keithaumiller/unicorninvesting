# Custom Trading Algorithms

This directory contains custom trading algorithms designed to work with the QuantConnect LEAN framework.

## Algorithm Types

- `genetic_portfolio_algorithm.py` - Portfolio optimization using genetic algorithms
- `neural_network_algorithm.py` - Price prediction using neural networks
- `risk_management_algorithm.py` - Risk management and position sizing
- `signal_aggregation_algorithm.py` - Combining multiple signal sources

## Integration with LEAN

These algorithms extend LEAN's base algorithm classes and integrate with:
- LEAN's data feeds and market data
- LEAN's execution engine for trade placement
- LEAN's backtesting and live trading infrastructure
- Unicorn's machine learning models and analytics

## Dependencies

- QuantConnect LEAN framework
- Unicorn backend services
- TensorFlow/Scikit-learn for ML models
