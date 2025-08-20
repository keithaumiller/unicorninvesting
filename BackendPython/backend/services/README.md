# Backend Services

This directory contains business logic services that coordinate between data models, machine learning algorithms, and API endpoints.

## Services Structure

```
services/
├── market_data_service.py     # ✅ Market data collection and processing
├── data_processor.py         # ✅ Data preprocessing and feature engineering  
├── feature_manager.py        # ✅ Feature selection and management
├── portfolio_service.py       # ⏳ Portfolio management operations
├── ml_service.py              # ⏳ Machine learning model coordination
├── risk_service.py            # ⏳ Risk analysis and assessment
├── recommendation_service.py   # ⏳ Investment recommendations
├── backtesting_service.py     # ⏳ Strategy backtesting
└── notification_service.py    # ⏳ User notifications and alerts
```

## Implemented Services

### market_data_service.py ✅
**Migrated from**: `datagathering/downloadstockdata.R`

- Real-time and historical market data retrieval using yfinance
- Parallel downloads for multiple symbols
- Data validation and cleaning
- Database integration with SQLAlchemy models
- Support for stocks, forex, and ETF data
- Comprehensive error handling and logging

**Key Classes**:
- `MarketDataCollector`: Main data collection orchestrator
- `DataDownloadConfig`: Configuration management

### data_processor.py ✅
**Migrated from**: `datasetcreation/Combinestocks.R`

- Stock data combination into unified training matrices
- Percentage change calculations and normalization
- Training/evaluation dataset splitting with temporal ordering
- Multiple target generation strategies (top_movers, momentum, mean_reversion)
- Missing value handling and data cleaning
- ML-ready dataset preparation

**Key Classes**:
- `StockDataProcessor`: Main data processing engine
- `DatasetConfig`: Processing configuration

### feature_manager.py ✅
**Migrated from**: `datasetcreation/Generatefeatureslist.R`

- Master feature list management and loading
- Dynamic feature selection based on data availability
- Portfolio-specific feature filtering (tech, dividend, growth)
- Feature correlation analysis and redundancy removal
- Feature list persistence and loading
- Data availability scanning

**Key Classes**:
- `FeatureListManager`: Feature list orchestration
- `FeatureConfig`: Feature selection configuration

## Planned Services

### portfolio_service.py
- Portfolio creation, modification, and deletion
- Position tracking and performance calculation
- Rebalancing logic and execution
- Portfolio optimization coordination

### ml_service.py
- Model training orchestration
- Feature engineering coordination
- Model deployment and versioning
- Prediction generation and caching

### risk_service.py
- Risk metric calculations (VaR, Sharpe ratio, etc.)
- Portfolio risk assessment
- Stress testing and scenario analysis
- Risk alert generation

### recommendation_service.py
- Investment recommendation generation
- Personalized portfolio suggestions
- Market analysis and insights
- Integration with machine learning predictions

### backtesting_service.py
- Historical strategy simulation
- Performance metric calculation
- Strategy comparison and analysis
- Result visualization data preparation

### notification_service.py
- User alert management
- Email and push notification coordination
- Event-driven notification triggers
- Communication preferences management

## Key Features of Implemented Services

### Advanced Data Processing
- **Temporal Awareness**: Maintains chronological order in train/test splits
- **Multiple Strategies**: Various target generation methods for different trading strategies
- **Robust Scaling**: Handles missing data and edge cases gracefully
- **Portfolio Focus**: Specialized for financial time series data

### Intelligent Feature Management
- **Portfolio-Specific**: Different feature sets for different portfolio types
- **Correlation Analysis**: Removes redundant features automatically
- **Data-Driven**: Only includes features with available data
- **Configurable**: Flexible thresholds and selection criteria

### Production-Ready Design
- **Type Hints**: Full type annotation for better code quality
- **Error Handling**: Comprehensive exception handling
- **Logging**: Detailed logging for debugging and monitoring
- **Configuration**: Dataclass-based configuration management

## Integration Points

These services integrate with:
- **Models**: Database operations through SQLAlchemy models
- **ML**: Machine learning algorithms and trained models
- **API**: REST API endpoints for external access
- **Utils**: Common utilities and helper functions

## Migration Progress

- ✅ **Complete**: market_data_service.py, data_processor.py, feature_manager.py
- ⏳ **In Progress**: Core services are implemented, remaining services planned
- 🎯 **Next Priority**: portfolio_service.py and ml_service.py
- 📋 **Dependencies**: Models and ML modules ready for integration
