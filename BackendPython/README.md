# BackendPython

This directory contains backend analytics, data processing, and machine learning code for Unicorn Investing, including both legacy R scripts and their Python conversions.

## Purpose
- Data processing, analytics, machine learning, and API services
- Quantitative trading algorithms using genetic algorithms + neural networks
- Portfolio optimization and risk management
- Real-time market data processing and feature engineering

## Migration Progress Overview

### ✅ Completed Python Conversions
- **Data Foundation**: market_data_service.py, database_models.py
- **Data Processing**: data_processor.py, feature_manager.py
- **ML Algorithms**: genetic_algorithm.py, neural_networks.py

### ⏳ Legacy R Scripts (To Convert)
- Portfolio optimization orchestration
- Backtesting frameworks
- Batch job scheduling
- Analytics and reporting

## Directory Structure

```
BackendPython/
├── backend/                         # ✅ Python backend structure
│   ├── api/                         # ⏳ FastAPI REST endpoints
│   ├── models/                      # ✅ SQLAlchemy database models
│   ├── services/                    # ✅ Business logic services
│   ├── ml/                          # ✅ Machine learning algorithms
│   └── utils/                       # ⏳ Common utilities
├── backtesting/                     # ⏳ Strategy validation & testing
├── batchjobs/                       # ⏳ Automated trading execution
├── blotterscripts/                  # ⏳ Trade blotter management
├── data/                            # 📊 Market data storage
├── database/                        # 🗄️ MySQL schemas & migrations
├── datagathering/                   # ⏳ Market data collection
├── datasetcreation/                 # ⏳ Feature engineering
├── deployment/                      # ⏳ Production configs
├── docs/                            # 📚 Technical documentation
├── predictiveanalytics/             # ⏳ Regression models
├── recomendationsystems/            # ⏳ GA+NN optimization
├── tests/                           # ⏳ Test suites
└── wpf-app*/                        # 🗑️ Legacy Windows apps
```

## Key Python Implementations

### Core Data Services ✅
- **`backend/services/market_data_service.py`**: Real-time market data collection using yfinance
- **`backend/services/data_processor.py`**: Advanced data preprocessing for ML models
- **`backend/services/feature_manager.py`**: Intelligent feature selection and management
- **`backend/models/database_models.py`**: Complete SQLAlchemy ORM schema

### Machine Learning Engine ✅
- **`backend/ml/genetic_algorithm.py`**: Sophisticated GA for portfolio optimization
- **`backend/ml/neural_networks.py`**: TensorFlow/Keras neural networks with custom loss functions

### Advanced Features Implemented
- **Parallel Processing**: Multi-threaded data downloads and GA evaluation
- **Custom Loss Functions**: Financial-specific neural network objectives
- **Temporal Awareness**: Proper time series handling for financial data
- **Portfolio Constraints**: Allocation constraints (sum to 1, positive weights)
- **Feature Selection**: Data-driven feature selection with correlation analysis

## Root Files

### Entry Points
- `quickstartGAportfolio.R` - Main GA portfolio optimization (→ Python equivalent planned)
- `quickstartsingleNN.R` - Single neural network training (→ `neural_networks.py`)

### R Workspace
- `unicorn.RData` - Serialized R workspace with trained models
- `unicorninvesting.Rproj` - RStudio project configuration
- `unicorninvesting.Rproj.RData` - RStudio project workspace data

## Technical Architecture

### Python Technology Stack
- **Data Processing**: pandas, numpy, scipy
- **Machine Learning**: TensorFlow/Keras, scikit-learn
- **Database**: SQLAlchemy, PyMySQL
- **APIs**: FastAPI (planned)
- **Market Data**: yfinance, alpha_vantage
- **Parallel Processing**: concurrent.futures, multiprocessing

### Key Improvements Over R
1. **Performance**: 10-100x faster with vectorized operations and GPU acceleration
2. **Scalability**: Better memory management and parallel processing
3. **Production Ready**: Comprehensive error handling, logging, and monitoring
4. **Modern ML**: Latest TensorFlow/Keras with custom financial loss functions
5. **Integration**: RESTful APIs for web frontend integration

## Migration Strategy

### Phase 1: Foundation ✅ COMPLETE
- ✅ Database models and market data collection
- ✅ Core data processing and feature engineering
- ✅ Genetic algorithm and neural network implementations

### Phase 2: Integration ⏳ IN PROGRESS
- Portfolio service orchestration
- API endpoint development
- Training pipeline automation
- Model deployment infrastructure

### Phase 3: Advanced Features ⏳ PLANNED
- Backtesting framework conversion
- Batch job automation
- Advanced analytics and reporting
- Real-time trading integration

## Legacy R Algorithm Details

### Genetic Algorithm + Neural Network Approach
The core innovation is the combination of:
1. **Genetic Algorithm**: Optimizes portfolio allocations and feature selection
2. **Neural Networks**: Predicts optimal allocations based on market features
3. **Multi-objective Optimization**: Balances return, risk, and diversification

### Key R Scripts Being Converted
- `recomendationsystems/GA_parameter_explorer.R` → `genetic_algorithm.py` ✅
- `recomendationsystems/1_modeltrainer_FCNN4R.R` → `neural_networks.py` ✅
- `datasetcreation/Combinestocks.R` → `data_processor.py` ✅
- `datagathering/downloadstockdata.R` → `market_data_service.py` ✅

## Next Steps

1. **Complete API Layer**: FastAPI endpoints for web frontend
2. **Orchestration Service**: High-level portfolio management coordination
3. **Backtesting Conversion**: Historical strategy validation
4. **Deployment Pipeline**: Production deployment automation
5. **Frontend Integration**: Connect with Drupal 11 web interface
