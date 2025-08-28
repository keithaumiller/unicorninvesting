# Utilities - Gold Layer Operations

## Purpose
Utility scripts for creating, maintaining, and managing the gold data layer with business-ready analytics and aggregations.

## Typical Scripts
- **setup_gold_views.py** - Create business-ready views and materialized tables
- **calculate_trading_indicators.py** - Compute technical indicators (RSI, MACD, Bollinger Bands)
- **portfolio_metrics_engine.py** - Calculate portfolio performance and risk metrics
- **benchmark_calculator.py** - Generate benchmark comparisons and relative performance
- **aggregation_engine.py** - Create time-based aggregations (daily, weekly, monthly)
- **business_rules_engine.py** - Apply business logic and trading rules
- **gold_performance_optimizer.py** - Optimize query performance for analytical workloads

## Business Analytics
- **profit_loss_calculator.py** - Calculate P&L for positions and portfolios
- **risk_metrics_engine.py** - Compute VaR, Sharpe ratio, maximum drawdown
- **correlation_analyzer.py** - Calculate asset correlations and portfolio diversification
- **volatility_calculator.py** - Compute various volatility measures
- **performance_attribution.py** - Analyze performance attribution by sector/asset

## Trading Signals & Indicators
- **technical_indicators.py** - RSI, MACD, Moving Averages, Bollinger Bands
- **momentum_indicators.py** - Price momentum and trend indicators
- **volume_indicators.py** - Volume-based trading signals
- **sentiment_aggregator.py** - Aggregate market sentiment indicators
- **signal_validation.py** - Backtest and validate trading signals

## Usage Pattern
```bash
# Setup operations
python utilities/setup_gold_views.py --create-indexes
python utilities/calculate_trading_indicators.py --all-symbols

# Daily analytics
python utilities/portfolio_metrics_engine.py --update-all
python utilities/business_rules_engine.py --apply-rules

# Performance monitoring
python utilities/benchmark_calculator.py --compare-sp500
python utilities/gold_performance_optimizer.py --analyze
```

Gold layer utilities focus on:
- Business logic implementation
- Trading signal generation
- Performance and risk analytics
- Aggregation and reporting optimization
