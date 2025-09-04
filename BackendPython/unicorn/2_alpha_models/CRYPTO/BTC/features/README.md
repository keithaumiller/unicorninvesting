# BTC Features Directory

## Purpose
This directory contains feature engineering components and feature definitions for Bitcoin (BTC) alpha model development.

## Contents
- **Feature Engineering**: Scripts to generate technical indicators and market features
- **Feature Selection**: Analysis and selection of optimal features for BTC prediction
- **Data Preprocessing**: Feature transformation and normalization utilities
- **Feature Validation**: Testing and validation of feature effectiveness
- **Custom Indicators**: BTC-specific technical indicators and market signals

## Directory Structure
```
features/
├── technical_indicators/   # Technical analysis features (RSI, MACD, etc.)
├── market_microstructure/  # Order book and volume-based features
├── sentiment_features/     # Social sentiment and news-based features
├── macroeconomic/         # Macro indicators affecting BTC
├── blockchain_metrics/    # On-chain BTC metrics
├── feature_selection/     # Feature importance and selection analysis
├── preprocessing/         # Data cleaning and transformation
└── validation/           # Feature effectiveness testing
```

## Key Feature Categories

### Technical Indicators
- Price-based: SMA, EMA, Bollinger Bands, RSI, MACD
- Volume-based: Volume oscillators, OBV, Volume Profile
- Volatility: ATR, Volatility bands, VIX correlation

### Market Microstructure
- Order book imbalance
- Bid-ask spread dynamics
- Trade size distribution
- Market impact metrics

### Blockchain Metrics
- Network hash rate
- Transaction volume
- Active addresses
- Mining difficulty
- HODL metrics

### Sentiment Features
- Social media sentiment
- News sentiment
- Fear & Greed Index
- Google Trends correlation

## File Naming Convention
- Feature generators: `btc_feature_[category].py`
- Feature sets: `btc_features_[timeframe].py`
- Validation scripts: `validate_[feature_type].py`
- Config files: `feature_config_[purpose].json`

## Integration Points
- **Data Sources**: Connects to `/1_data_sources/` for raw market data
- **Alpha Models**: Provides features to BTC prediction models
- **Research**: Documented in `/research/` directory
- **Production**: Deployed features used in `/production_models/`

## Feature Engineering Pipeline
1. **Raw Data Ingestion**: Market data from multiple sources
2. **Feature Generation**: Apply technical indicators and transformations
3. **Feature Selection**: Statistical and ML-based feature importance
4. **Validation**: Out-of-sample feature effectiveness testing
5. **Production Deployment**: Integration with live trading models

## Purpose in LEAN Architecture
This directory supports the **Alpha Models** layer (Layer 2) by providing:
- Standardized feature engineering pipeline
- Robust feature selection methodology
- Validated predictive features for BTC models
- Scalable feature generation infrastructure

## Dependencies
- `pandas`, `numpy` for data manipulation
- `ta-lib`, `pandas-ta` for technical indicators
- `scikit-learn` for feature selection
- Custom blockchain data APIs
- Sentiment analysis libraries

## Usage Example
```python
from features.technical_indicators import BTCTechnicalFeatures
from features.blockchain_metrics import BTCOnChainFeatures

# Generate feature set for BTC prediction
btc_features = BTCTechnicalFeatures(timeframe='1H')
tech_features = btc_features.generate_features(price_data)

onchain_features = BTCOnChainFeatures()
blockchain_features = onchain_features.get_network_metrics()
```

## Related Components
- **Research Directory**: `/2_alpha_models/CRYPTO/BTC/research/`
- **Models Directory**: `/2_alpha_models/CRYPTO/BTC/models/`
- **Data Sources**: `/1_data_sources/1_raw/`

---
*Part of the Unicorn Investing LEAN Architecture - Layer 2: Alpha Models*
