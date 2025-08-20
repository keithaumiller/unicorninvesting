# Unicorn Investing Platform

An advanced algorithmic trading platform that combines Genetic Algorithms with Neural Networks for automated portfolio optimization and quantitative trading.

## Overview

Unicorn Investing is a sophisticated time-series data balancing algorithm designed for trading management, risk mitigation, and executive decision assistance. The platform uses machine learning to optimize portfolio allocations across stocks and forex markets.

## Core Technology

### Algorithm Architecture
- **Genetic Algorithm (GA)**: Feature selection and neural network hyperparameter optimization
- **Neural Networks**: Portfolio allocation decisions and risk balancing
- **Portfolio Optimization**: Daily rebalancing based on ML predictions
- **Performance Tracking**: Continuous backtesting and performance evaluation

### Key Capabilities
- **Sensor Extension**: Easily add new data sources and market indicators
- **Portfolio Management**: Multi-asset portfolio optimization with risk controls
- **Feature Selection**: Automated filtering of redundant or unnecessary data
- **Neural Balancing**: ML-driven portfolio rebalancing for optimal returns

## Project Structure

### BackendPython/
Contains all analytics, data processing, and machine learning code:
- **Legacy R Scripts**: Original quantitative models (being migrated to Python)
- **Data Processing**: Market data collection, cleaning, and feature engineering
- **ML Algorithms**: GA + Neural Network optimization for portfolio management
- **Backtesting**: Historical strategy validation and performance testing
- **Database**: MySQL schemas and data management

### WebFrontend/
Modern web interface replacing legacy desktop applications:
- **Drupal 11**: Web-based portfolio management and trading interface
- **Responsive Design**: Mobile-optimized trading dashboard
- **Real-time Data**: Live market feeds and portfolio monitoring
- **User Management**: Multi-user platform with role-based access

## Architecture Transition

### Current State (Legacy)
- R-based analytics and machine learning (FCNN4R, GA packages)
- WPF desktop applications for Windows
- File-based data storage and processing
- Direct MySQL database access from R scripts

### Future State (Target)
- Python-based analytics (scikit-learn, TensorFlow, pandas)
- Drupal 11 web interface with responsive design
- FastAPI backend services with REST APIs
- Optimized MySQL schema with proper ORM integration

## Getting Started

### Prerequisites
- R 4.0+ (current legacy system)
- Python 3.9+ (target migration)
- MySQL 8.0+
- PHP 8.2+ and Drupal 11 (web interface)

### Current System (R-based)
```bash
# Install R dependencies
Rscript -e "install.packages(c('quantmod','FCNN4R','GA','forecast','RMySQL'))"

# Run single neural network optimization
Rscript BackendPython/quickstartsingleNN.R

# Run genetic algorithm portfolio optimization  
Rscript BackendPython/quickstartGAportfolio.R
```

### Migration to Python (In Progress)
```bash
# Setup Python environment
pip install pandas numpy scikit-learn tensorflow fastapi sqlalchemy

# Future: Run Python equivalent
python backend/main.py
```

## Key Features

### Algorithmic Trading
- **Daily Rebalancing**: Automated portfolio optimization based on ML predictions
- **Risk Management**: Position sizing and risk-adjusted returns
- **Multi-Asset Support**: Stocks, forex, and other financial instruments
- **Backtesting**: Historical performance validation over multiple time periods

### Machine Learning Pipeline
1. **Data Collection**: Real-time market data from multiple sources
2. **Feature Engineering**: Technical indicators and market metrics
3. **GA Optimization**: Feature selection and hyperparameter tuning
4. **Neural Training**: Portfolio allocation model training
5. **Performance Evaluation**: Backtesting and risk assessment
6. **Live Trading**: Daily allocation recommendations

### Performance Metrics
- Portfolio value tracking from $1000 seed money
- Risk-adjusted returns and Sharpe ratio analysis
- Drawdown analysis and risk management
- Comparison against market benchmarks

## License

[Add license information]

## Contributing

[Add contribution guidelines]

---

*This platform represents a comprehensive approach to quantitative trading, combining advanced machine learning techniques with robust portfolio management principles for institutional-grade algorithmic trading.*