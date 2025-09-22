# Forex Alpha Models

Issue #36: Multi-Currency Forex Alpha Models & Forecasting System

This directory contains forex-specific alpha models that leverage our existing economic data integration and enhanced model selection framework for comprehensive currency pair forecasting.

## 🌍 Architecture Overview

### Integration with Existing Infrastructure
- **Economic Data Pipeline**: Leverages existing 580+ economic indicators from FRED, BEA
- **Enhanced Model Selection**: Uses proven multi-criteria model selector
- **Silver Layer Processing**: Integrates with existing feature engineering framework
- **Portfolio Construction**: Extends existing utilities for multi-currency allocation

### LEAN Framework Layer 2 Integration
This implements **Layer 2: Alpha Models** of the LEAN framework specifically for forex markets, generating trading signals based on currency pair predictions.

## 📊 Supported Currency Categories

### Major Currency Pairs (Phase 1 - High Priority)
- **EUR/USD**: Euro/US Dollar - Most traded globally
- **USD/JPY**: US Dollar/Japanese Yen - Safe haven dynamics
- **GBP/USD**: British Pound/US Dollar - "The Cable"
- **AUD/USD**: Australian Dollar/US Dollar - Commodity-linked
- **USD/CAD**: US Dollar/Canadian Dollar - Oil-linked
- **USD/CHF**: US Dollar/Swiss Franc - Safe haven
- **NZD/USD**: New Zealand Dollar/US Dollar - Agricultural commodity-linked

### Cross Currency Pairs (Phase 2 - Medium Priority)
- **EUR/JPY**: Euro/Japanese Yen - European/Asian cross
- **EUR/GBP**: Euro/British Pound - European cross
- **GBP/JPY**: British Pound/Japanese Yen - High volatility
- **EUR/AUD**: Euro/Australian Dollar - European/Oceanic cross

### Emerging Market Pairs (Phase 3 - Future)
- **USD/CNY**: US Dollar/Chinese Renminbi - Major emerging market

## 🏗️ Model Architecture

### Economic-Enhanced Models
Each forex model leverages economic fundamentals specific to the currency pair:

```python
class ForexAlphaModel:
    def __init__(self, currency_pair: str, economic_features: pd.DataFrame):
        self.currency_pair = currency_pair
        self.economic_features = economic_features
        
    def generate_signals(self) -> TradingSignals:
        # Economic-enhanced signal generation
        pass
```

### Model Types

#### 1. Economic-Enhanced XGBoost
- **Location**: `economic_enhanced_xgboost/`
- **Features**: Leverages 580+ economic indicators
- **Specialization**: Currency-specific fundamental analysis
- **Target**: Medium to long-term trends (1-30 days)

#### 2. Prophet Time Series Forecasting
- **Location**: `prophet_forecasting/`
- **Features**: Time series patterns with economic seasonality
- **Specialization**: Trend and seasonal component analysis
- **Target**: Short to medium-term forecasts (1-14 days)

#### 3. Ensemble Models
- **Location**: `ensemble_models/`
- **Features**: Combines multiple forecasting approaches
- **Specialization**: Robust predictions across market conditions
- **Target**: All timeframes with confidence intervals

#### 4. Carry Trade Models
- **Location**: `carry_trade_models/`
- **Features**: Interest rate differential optimization
- **Specialization**: Central bank policy and yield curve analysis
- **Target**: Long-term carry trade opportunities

#### 5. Interest Rate Differential Models
- **Location**: `interest_rate_models/`
- **Features**: Central bank policy impact modeling
- **Specialization**: Monetary policy stance and rate expectations
- **Target**: Policy-driven currency movements

## 📈 Economic Feature Categories

### Central Bank Policy Features
- Interest rate differentials between currency pairs
- Monetary policy stance indicators (hawkish/dovish)
- Quantitative easing metrics and policy changes
- Central bank communication sentiment analysis

### Economic Fundamentals
- GDP growth differentials between countries
- Inflation rate comparisons (CPI, core inflation)
- Trade balance and current account indicators
- Employment data differentials (unemployment rates, job creation)

### Market Sentiment Features
- VIX and risk-on/risk-off indicators
- Commodity price correlations (for commodity currencies)
- Safe haven flow indicators during market stress
- Cross-asset correlations and portfolio flows

## 🔄 Data Integration

### Existing Economic Data (✅ Available)
```python
# Leverages existing FRED/BEA integration
ECONOMIC_FEATURES = {
    'USD': ['FEDFUNDS', 'GDP', 'CPIAUCSL', 'UNRATE', 'PAYEMS'],
    'EUR': ['ECBDFR', 'GDPQS_EUR', 'CP0000EZ19M086NEST'],
    'JPY': ['JPNIR', 'JPNGDP', 'JPNCPI'],
    # ... additional currency-specific indicators
}
```

### Currency-Specific Enhancements
- **USD Pairs**: Full leverage of existing US economic data (580+ indicators)
- **Commodity Currencies**: Integration with commodity price feeds
- **Safe Haven Currencies**: Risk sentiment and volatility indicators
- **European Currencies**: European Central Bank and Eurostat data

## 🎯 Implementation Status

### ✅ Completed Infrastructure
- **Forex Data Connectors**: Yahoo Finance and IBKR integration
- **Symbol Mapping**: 12+ currency pairs with metadata
- **Directory Structure**: Organized alpha model framework
- **Economic Integration**: Ready for 580+ indicator leverage

### 🚧 Implementation Phases

#### Phase 1: Major Pairs Foundation (Current)
- [ ] EUR/USD economic-enhanced model
- [ ] USD/JPY safe haven dynamics model
- [ ] GBP/USD Brexit and policy impact model
- [ ] Commodity currency models (AUD/USD, USD/CAD, NZD/USD)

#### Phase 2: Advanced Modeling
- [ ] Cross-currency pair models
- [ ] Ensemble model integration
- [ ] Carry trade strategy models
- [ ] Central bank policy impact models

#### Phase 3: Portfolio Integration
- [ ] Multi-currency portfolio construction
- [ ] Cross-currency correlation analysis
- [ ] Currency hedging strategies
- [ ] Risk management for forex positions

## 📊 Success Metrics

### Model Performance Targets
- **Prediction Accuracy**: >55% directional accuracy
- **Sharpe Ratio**: >1.0 for major currency pairs
- **Maximum Drawdown**: <15% for currency portfolio
- **Information Ratio**: >0.5 vs currency benchmarks

### Data Quality Standards
- **Data Availability**: 95%+ for major currency pairs
- **Latency**: <1 minute for real-time forex data
- **Economic Integration**: Complete integration with 580+ indicators
- **Time Zone Handling**: Robust 24/7 forex market coverage

## 🔗 Integration Points

### Upstream Dependencies
- **Data Sources**: Yahoo Finance, Interactive Brokers forex data
- **Economic Data**: FRED, BEA, international economic indicators
- **Silver Layer**: Feature engineering and data preprocessing

### Downstream Integration
- **Risk Management**: Layer 3 risk controls for forex positions
- **Portfolio Construction**: Layer 4 multi-currency optimization
- **Execution Models**: Layer 5 forex order execution
- **Complete Algorithms**: Layer 6 integrated forex trading systems

## 🌟 Competitive Advantages

### Economic Integration
- **Unique Dataset**: 580+ economic indicators for currency forecasting
- **Fundamental Analysis**: Deep economic driver integration
- **Multi-Asset Correlation**: Cross-asset portfolio optimization
- **Real-time Processing**: Live economic data for currency signals

### Professional Framework
- **LEAN Integration**: Institutional-grade architecture
- **Multi-Currency Support**: Comprehensive forex coverage
- **Risk Management**: Professional forex risk controls
- **Backtesting Ready**: Full historical data and simulation support

---

**Status**: 🚧 Phase 1 Implementation Ready  
**Next**: Economic-Enhanced XGBoost EUR/USD model development  
**Priority**: High - Strategic platform expansion leveraging existing infrastructure
