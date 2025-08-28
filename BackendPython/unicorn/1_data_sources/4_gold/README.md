# Gold Layer - Business-Ready Analytics Data

## 🥇 Purpose

The **Gold Layer** contains business-ready, aggregated data optimized for analytics and algorithmic trading. This layer provides pre-calculated metrics, performance indicators, and analysis-ready datasets.

## 🏗️ Architecture Role

**Data Flow**: Silver Layer → **Gold Layer** → Data Marts → Trading Algorithms

The Gold Layer delivers **high-performance, analysis-optimized data** for real-time trading decisions.

## 🎯 **Data Aggregations & Analytics**

### **1. Market Metrics**
Pre-calculated financial indicators and technical analysis:

- **Technical Indicators**: SMA, EMA, RSI, MACD, Bollinger Bands
- **Price Analytics**: Returns, volatility, momentum indicators
- **Volume Analytics**: VWAP, volume patterns, liquidity metrics
- **Market Microstructure**: Bid-ask spreads, market impact measures

### **2. Risk Metrics**
Risk management and portfolio analytics:

- **Value at Risk (VaR)**: 1-day, 5-day, 30-day VaR calculations
- **Volatility Measures**: Historical volatility, GARCH models
- **Correlation Analysis**: Asset correlation matrices
- **Drawdown Metrics**: Maximum drawdown, recovery periods

### **3. Performance Analytics**
Trading and portfolio performance metrics:

- **Return Metrics**: Alpha, beta, Sharpe ratio, information ratio
- **Attribution Analysis**: Factor-based performance attribution
- **Benchmark Comparisons**: Relative performance metrics
- **Risk-Adjusted Returns**: Sortino ratio, Calmar ratio

### **4. Market Intelligence**
Higher-level market insights and patterns:

- **Trend Analysis**: Market regime detection
- **Sentiment Indicators**: Market sentiment scores
- **Event Impact**: News and earnings impact analysis
- **Seasonal Patterns**: Calendar and time-based patterns

## 📊 **Gold Layer Schema Standards**

### **Market Analytics Schema**
```python
# Daily market analytics
daily_analytics_schema = {
    "symbol": "string",              # Asset identifier
    "date": "date",                  # Trading date
    "close_price": "decimal",        # Closing price
    "returns_1d": "decimal",         # 1-day return
    "returns_5d": "decimal",         # 5-day return
    "returns_21d": "decimal",        # 21-day (monthly) return
    "volatility_21d": "decimal",     # 21-day volatility
    "volume_avg_21d": "decimal",     # 21-day average volume
    "rsi_14": "decimal",            # 14-day RSI
    "sma_50": "decimal",            # 50-day simple moving average
    "ema_20": "decimal",            # 20-day exponential moving average
    "bollinger_upper": "decimal",    # Bollinger upper band
    "bollinger_lower": "decimal",    # Bollinger lower band
    "macd": "decimal",              # MACD indicator
    "macd_signal": "decimal",       # MACD signal line
    "vwap": "decimal",              # Volume weighted average price
    "sharpe_ratio_21d": "decimal",  # 21-day Sharpe ratio
    "max_drawdown_21d": "decimal",  # 21-day maximum drawdown
    "beta_market": "decimal",       # Beta vs market index
    "data_quality_score": "decimal", # Data quality metric
    "last_updated": "datetime"      # Processing timestamp
}
```

### **Portfolio Analytics Schema**
```python
# Portfolio-level analytics
portfolio_analytics_schema = {
    "portfolio_id": "string",        # Portfolio identifier
    "date": "date",                  # Analysis date
    "total_value": "decimal",        # Total portfolio value
    "daily_return": "decimal",       # Daily portfolio return
    "cumulative_return": "decimal",  # Cumulative return from inception
    "volatility_21d": "decimal",     # Portfolio volatility
    "sharpe_ratio": "decimal",       # Risk-adjusted return
    "max_drawdown": "decimal",       # Maximum drawdown
    "var_95": "decimal",            # 95% Value at Risk
    "var_99": "decimal",            # 99% Value at Risk
    "beta_market": "decimal",       # Portfolio beta
    "alpha_market": "decimal",      # Portfolio alpha
    "tracking_error": "decimal",    # Tracking error vs benchmark
    "information_ratio": "decimal", # Information ratio
    "positions_count": "integer",   # Number of positions
    "concentration_risk": "decimal", # Portfolio concentration metric
    "liquidity_score": "decimal",   # Portfolio liquidity score
    "last_updated": "datetime"      # Processing timestamp
}
```

## 🔧 **Data Processing Pipeline**

### **1. Silver to Gold ETL**
```python
def silver_to_gold_pipeline(silver_data_path, gold_output_path):
    """
    Process silver layer data to gold layer analytics
    """
    # Load clean silver data
    clean_data = load_silver_data(silver_data_path)
    
    # Calculate technical indicators
    technical_data = calculate_technical_indicators(clean_data)
    
    # Calculate risk metrics
    risk_data = calculate_risk_metrics(technical_data)
    
    # Calculate performance metrics
    performance_data = calculate_performance_metrics(risk_data)
    
    # Aggregate to business reporting frequency
    aggregated_data = aggregate_to_reporting_frequency(performance_data)
    
    # Optimize for query performance
    optimized_data = optimize_for_queries(aggregated_data)
    
    # Write to gold layer with partitioning
    write_gold_data(optimized_data, gold_output_path)
    
    # Update analytics catalog
    update_analytics_catalog(optimized_data, gold_output_path)
```

### **2. Technical Indicators Calculation**
```python
class TechnicalIndicatorCalculator:
    """Calculate standard technical indicators"""
    
    def calculate_all_indicators(self, price_data):
        """Calculate comprehensive technical indicators"""
        
        indicators = {}
        
        # Moving averages
        indicators['sma_20'] = self.simple_moving_average(price_data['close'], 20)
        indicators['sma_50'] = self.simple_moving_average(price_data['close'], 50)
        indicators['ema_20'] = self.exponential_moving_average(price_data['close'], 20)
        
        # Momentum indicators
        indicators['rsi_14'] = self.relative_strength_index(price_data['close'], 14)
        indicators['macd'], indicators['macd_signal'] = self.macd(price_data['close'])
        
        # Volatility indicators
        indicators['bollinger_upper'], indicators['bollinger_lower'] = self.bollinger_bands(price_data['close'])
        indicators['atr_14'] = self.average_true_range(price_data, 14)
        
        # Volume indicators
        indicators['vwap'] = self.volume_weighted_average_price(price_data)
        indicators['volume_sma_20'] = self.simple_moving_average(price_data['volume'], 20)
        
        return indicators
    
    def relative_strength_index(self, prices, period=14):
        """Calculate RSI indicator"""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def bollinger_bands(self, prices, period=20, std_dev=2):
        """Calculate Bollinger Bands"""
        sma = prices.rolling(window=period).mean()
        std = prices.rolling(window=period).std()
        upper_band = sma + (std * std_dev)
        lower_band = sma - (std * std_dev)
        return upper_band, lower_band
```

### **3. Risk Metrics Calculation**
```python
class RiskMetricsCalculator:
    """Calculate comprehensive risk metrics"""
    
    def calculate_portfolio_risk(self, returns_data, confidence_levels=[0.95, 0.99]):
        """Calculate portfolio risk metrics"""
        
        risk_metrics = {}
        
        # Volatility measures
        risk_metrics['volatility_daily'] = returns_data.std()
        risk_metrics['volatility_annualized'] = returns_data.std() * np.sqrt(252)
        
        # Value at Risk
        for confidence in confidence_levels:
            var_key = f'var_{int(confidence*100)}'
            risk_metrics[var_key] = returns_data.quantile(1 - confidence)
        
        # Expected Shortfall (Conditional VaR)
        for confidence in confidence_levels:
            es_key = f'expected_shortfall_{int(confidence*100)}'
            var_threshold = returns_data.quantile(1 - confidence)
            risk_metrics[es_key] = returns_data[returns_data <= var_threshold].mean()
        
        # Maximum Drawdown
        cumulative_returns = (1 + returns_data).cumprod()
        running_max = cumulative_returns.expanding().max()
        drawdown = (cumulative_returns - running_max) / running_max
        risk_metrics['max_drawdown'] = drawdown.min()
        
        # Downside Deviation
        downside_returns = returns_data[returns_data < 0]
        risk_metrics['downside_deviation'] = downside_returns.std()
        
        return risk_metrics
    
    def calculate_correlation_matrix(self, returns_data):
        """Calculate asset correlation matrix"""
        correlation_matrix = returns_data.corr()
        
        # Add correlation analytics
        analytics = {
            "average_correlation": correlation_matrix.mean().mean(),
            "max_correlation": correlation_matrix.max().max(),
            "min_correlation": correlation_matrix.min().min(),
            "highly_correlated_pairs": self.find_high_correlations(correlation_matrix, threshold=0.8)
        }
        
        return correlation_matrix, analytics
```

### **4. Performance Attribution**
```python
class PerformanceAttributionCalculator:
    """Calculate performance attribution and factor analysis"""
    
    def calculate_factor_attribution(self, portfolio_returns, factor_returns):
        """Perform factor-based performance attribution"""
        
        # Regression analysis
        from sklearn.linear_model import LinearRegression
        
        # Prepare factor data
        X = factor_returns.values
        y = portfolio_returns.values
        
        # Fit regression model
        model = LinearRegression().fit(X, y)
        
        attribution = {
            "factor_exposures": dict(zip(factor_returns.columns, model.coef_)),
            "alpha": model.intercept_,
            "r_squared": model.score(X, y),
            "factor_contributions": self.calculate_factor_contributions(model, factor_returns),
            "idiosyncratic_risk": self.calculate_idiosyncratic_risk(portfolio_returns, model, factor_returns)
        }
        
        return attribution
    
    def calculate_risk_adjusted_returns(self, returns, risk_free_rate=0.02):
        """Calculate risk-adjusted return metrics"""
        
        excess_returns = returns - risk_free_rate / 252  # Daily risk-free rate
        
        metrics = {
            "sharpe_ratio": excess_returns.mean() / returns.std() * np.sqrt(252),
            "sortino_ratio": excess_returns.mean() / returns[returns < 0].std() * np.sqrt(252),
            "calmar_ratio": returns.mean() * 252 / abs(self.calculate_max_drawdown(returns)),
            "information_ratio": self.calculate_information_ratio(returns, benchmark_returns=None)
        }
        
        return metrics
```

## 📊 **Query Optimization**

### **1. Partitioning Strategy**
```python
# Gold layer partitioning structure
gold_layer/
├── market_analytics/
│   ├── year=2024/
│   │   ├── month=01/
│   │   │   ├── symbol=AAPL/
│   │   │   ├── symbol=GOOGL/
│   │   │   └── symbol=MSFT/
│   │   └── month=02/
│   └── year=2023/
├── portfolio_analytics/
│   ├── year=2024/
│   └── portfolio_id=portfolio_001/
└── risk_metrics/
    ├── daily/
    ├── weekly/
    └── monthly/
```

### **2. Indexing Strategy**
```python
def create_gold_layer_indexes():
    """Create indexes for fast querying"""
    
    indexes = {
        "market_analytics": [
            "symbol",
            "date",
            ("symbol", "date"),  # Composite index
            "data_quality_score"
        ],
        "portfolio_analytics": [
            "portfolio_id",
            "date",
            ("portfolio_id", "date"),
            "total_value"
        ],
        "risk_metrics": [
            "symbol",
            "metric_type",
            "calculation_date"
        ]
    }
    
    for table, index_list in indexes.items():
        create_indexes(table, index_list)
```

### **3. Caching Strategy**
```python
class GoldLayerCache:
    """Intelligent caching for gold layer queries"""
    
    def __init__(self):
        self.cache = {}
        self.cache_ttl = {
            "intraday": 300,    # 5 minutes
            "daily": 3600,      # 1 hour
            "weekly": 86400     # 24 hours
        }
    
    def get_cached_analytics(self, query_key, data_frequency):
        """Get cached analytics if available and fresh"""
        
        if query_key in self.cache:
            cached_data, timestamp = self.cache[query_key]
            ttl = self.cache_ttl.get(data_frequency, 3600)
            
            if time.time() - timestamp < ttl:
                return cached_data
        
        return None
    
    def cache_analytics(self, query_key, data, data_frequency):
        """Cache analytics data with appropriate TTL"""
        
        self.cache[query_key] = (data, time.time())
        
        # Implement LRU eviction if cache gets too large
        if len(self.cache) > 1000:
            self.evict_oldest_entries()
```

## 🔍 **Analytics Query Interface**

### **Standard Analytics API**
```python
class GoldLayerAnalytics:
    """High-level analytics interface for gold layer"""
    
    def get_technical_indicators(self, symbols, date_range, indicators=None):
        """Get pre-calculated technical indicators"""
        
        if indicators is None:
            indicators = ['sma_20', 'rsi_14', 'macd', 'bollinger_upper', 'bollinger_lower']
        
        query = f"""
        SELECT symbol, date, {', '.join(indicators)}
        FROM market_analytics 
        WHERE symbol IN ({','.join([f"'{s}'" for s in symbols])})
        AND date BETWEEN '{date_range[0]}' AND '{date_range[1]}'
        ORDER BY symbol, date
        """
        
        return self.execute_query(query)
    
    def get_portfolio_performance(self, portfolio_id, date_range):
        """Get portfolio performance analytics"""
        
        query = f"""
        SELECT date, total_value, daily_return, cumulative_return,
               sharpe_ratio, max_drawdown, var_95, var_99
        FROM portfolio_analytics
        WHERE portfolio_id = '{portfolio_id}'
        AND date BETWEEN '{date_range[0]}' AND '{date_range[1]}'
        ORDER BY date
        """
        
        return self.execute_query(query)
    
    def get_risk_dashboard(self, symbols, as_of_date):
        """Get current risk dashboard data"""
        
        query = f"""
        SELECT symbol, volatility_21d, var_95, var_99, 
               max_drawdown_21d, beta_market, correlation_score
        FROM market_analytics
        WHERE symbol IN ({','.join([f"'{s}'" for s in symbols])})
        AND date = '{as_of_date}'
        ORDER BY var_99 DESC
        """
        
        return self.execute_query(query)
```

## 🚨 **Best Practices**

### **1. Performance Optimization**
- ✅ Pre-calculate all common analytics
- ✅ Use appropriate partitioning strategies
- ✅ Implement intelligent caching
- ✅ Optimize queries with proper indexing

### **2. Data Freshness**
- ✅ Monitor data lag and freshness
- ✅ Implement real-time updates for critical metrics
- ✅ Set appropriate refresh frequencies
- ✅ Alert on stale data conditions

### **3. Quality Assurance**
- ✅ Validate all calculated metrics
- ✅ Implement consistency checks
- ✅ Monitor calculation accuracy
- ✅ Provide data lineage for all analytics

## 🦄 **Unicorn Platform Integration**

Gold layer powers algorithmic trading by providing:
- **Real-time Analytics**: Pre-calculated indicators for trading decisions
- **Risk Monitoring**: Instant access to risk metrics and alerts
- **Performance Tracking**: Comprehensive portfolio analytics
- **Market Intelligence**: Advanced market insights and patterns

---

*The Gold Layer delivers business intelligence at the speed of trading - optimized for performance and accuracy!*
