# BackendPython

This directory contains all backend analytics, data processing, and machine learning code for Unicorn Investing, including legacy R scripts to be converted to Python.

## Purpose
- Data processing, analytics, machine learning, and API services
- Quantitative trading algorithms using genetic algorithms + neural networks
- Portfolio optimization and risk management
- Real-time market data processing and feature engineering

## Directory Structure
- `backend/` - Future Python backend API structure
- `backtesting/` - Historical strategy validation and performance testing
- `batchjobs/` - Automated trading execution and daily processing
- `blotterscripts/` - Trade blotter management and reconciliation
- `data/` - Market data storage (stocks, forex, portfolios)
- `database/` - MySQL schemas and migration scripts
- `datagathering/` - Market data collection and ingestion
- `datasetcreation/` - Feature engineering and training set generation
- `deployment/` - Production deployment configurations
- `docs/` - Technical documentation
- `predictiveanalytics/` - Regression models and exploratory analysis
- `recomendationsystems/` - GA+NN portfolio optimization algorithms
- `tests/` - Test suites for validation
- `wpf-app/` & `wpf-app-1/` - Legacy Windows desktop applications

## Root Files
- `quickstartGAportfolio.R` - Main entry point for GA portfolio optimization
- `quickstartsingleNN.R` - Single neural network training and evaluation
- `unicorn.RData` - Serialized R workspace with trained models
- `unicorninvesting.Rproj` - RStudio project configuration
- `unicorninvesting.Rproj.RData` - RStudio project workspace data

## Migration Notes
All R scripts are being converted to Python equivalents using:
- pandas/numpy for data processing
- scikit-learn/TensorFlow for machine learning
- FastAPI for web APIs
- SQLAlchemy for database operations
