# Data Marts - Subject-Specific Analytics

## 🏢 Purpose

**Data Marts** are subject-specific data stores optimized for particular business domains. Each mart contains pre-aggregated, domain-focused analytics designed for specific use cases in algorithmic trading.

## 🏗️ Architecture Role

**Data Flow**: Gold Layer → **Data Marts** → Trading Algorithms & Business Analytics

Data Marts provide **domain-optimized analytics** tailored for specific trading strategies and risk management.

## 📊 **Planned Data Mart Structure**

### **💱 Forex Data Mart** (`forex/`)
Foreign exchange trading analytics and metrics:

**Key Datasets**:
- **Currency Pair Analytics**: Major, minor, and exotic pair metrics
- **Economic Indicators**: Central bank rates, GDP, inflation impacts
- **Technical Patterns**: Chart patterns, support/resistance levels
- **Volatility Analysis**: Currency volatility and correlation matrices
- **Carry Trade Analytics**: Interest rate differentials and carry opportunities

**Optimizations**:
- Partitioned by currency pair and date
- Pre-calculated technical indicators
- Real-time rate feeds integration
- Cross-currency correlation analysis

### **📈 Equities Data Mart** (`equities/`)
Stock market analytics and screening tools:

**Key Datasets**:
- **Sector Analytics**: Sector rotation and performance metrics
- **Company Fundamentals**: P/E, EPS, revenue growth analytics
- **Technical Screening**: Momentum, value, growth stock screens
- **Earnings Analytics**: Earnings surprise and impact analysis
- **Market Microstructure**: Order flow and liquidity analysis

**Optimizations**:
- Indexed by sector, market cap, and trading volume
- Pre-calculated screening criteria
- Real-time earnings calendar integration
- Factor-based analytics and rankings

### **🪙 Cryptocurrency Data Mart** (`crypto/`)
Cryptocurrency and DeFi analytics:

**Key Datasets**:
- **Token Analytics**: Price, volume, market cap analytics
- **DeFi Metrics**: TVL, yield farming, liquidity pool analytics
- **On-chain Analytics**: Wallet activity, transaction patterns
- **Exchange Analytics**: Cross-exchange arbitrage opportunities
- **Sentiment Analysis**: Social media sentiment and fear/greed index

**Optimizations**:
- Real-time price feeds from multiple exchanges
- On-chain data integration
- Social sentiment scoring
- Cross-exchange arbitrage detection

### **📊 Portfolio Data Mart** (`portfolio/`)
Portfolio management and performance analytics:

**Key Datasets**:
- **Performance Attribution**: Factor-based performance analysis
- **Risk Analytics**: Portfolio VaR, tracking error, concentration risk
- **Rebalancing Analytics**: Optimal rebalancing strategies
- **Cost Analysis**: Transaction costs and slippage analysis
- **Benchmark Comparisons**: Performance vs various benchmarks

**Optimizations**:
- Portfolio-level aggregations
- Multi-timeframe performance metrics
- Real-time position tracking
- Custom benchmark calculations

### **🛡️ Risk Data Mart** (`risk/`)
Comprehensive risk management analytics:

**Key Datasets**:
- **Market Risk**: VaR, stress testing, scenario analysis
- **Credit Risk**: Counterparty exposure and credit metrics
- **Liquidity Risk**: Market liquidity and redemption analysis
- **Operational Risk**: System reliability and operational metrics
- **Regulatory Risk**: Compliance monitoring and reporting

**Optimizations**:
- Real-time risk monitoring
- Stress test scenario modeling
- Regulatory reporting automation
- Risk limit monitoring and alerting

### **📋 Compliance Data Mart** (`compliance/`)
Regulatory compliance and audit analytics:

**Key Datasets**:
- **Trade Surveillance**: Market manipulation detection
- **Position Limits**: Regulatory position limit monitoring
- **Audit Trails**: Complete transaction audit logs
- **Reporting**: Regulatory reporting datasets
- **Documentation**: Compliance documentation and evidence

**Optimizations**:
- Complete audit trail preservation
- Real-time compliance monitoring
- Automated regulatory reporting
- Evidence preservation for audits

## 🔧 **Data Mart Implementation Framework**

### **1. Base Data Mart Class**
```python
from abc import ABC, abstractmethod

class BaseDataMart(ABC):
    """Base class for all data marts"""
    
    def __init__(self, mart_name, gold_layer_path, mart_path):
        self.mart_name = mart_name
        self.gold_layer_path = gold_layer_path
        self.mart_path = mart_path
        self.refresh_schedule = self.get_refresh_schedule()
    
    @abstractmethod
    def extract_source_data(self):
        """Extract data from gold layer"""
        pass
    
    @abstractmethod
    def transform_for_domain(self, data):
        """Apply domain-specific transformations"""
        pass
    
    @abstractmethod
    def create_domain_analytics(self, data):
        """Create domain-specific analytics"""
        pass
    
    def refresh_mart(self):
        """Refresh data mart with latest data"""
        
        # Extract from gold layer
        gold_data = self.extract_source_data()
        
        # Transform for domain
        domain_data = self.transform_for_domain(gold_data)
        
        # Create analytics
        analytics_data = self.create_domain_analytics(domain_data)
        
        # Load to mart
        self.load_to_mart(analytics_data)
        
        # Update metadata
        self.update_mart_metadata()
```

### **2. Forex Data Mart Implementation**
```python
class ForexDataMart(BaseDataMart):
    """Forex-specific data mart implementation"""
    
    def __init__(self, gold_layer_path, mart_path):
        super().__init__("forex", gold_layer_path, mart_path)
        self.major_pairs = ["EURUSD", "GBPUSD", "USDJPY", "AUDUSD", "USDCAD", "USDCHF"]
        self.minor_pairs = ["EURGBP", "EURJPY", "GBPJPY", "AUDJPY"]
    
    def extract_source_data(self):
        """Extract forex data from gold layer"""
        
        # Get forex market data
        forex_data = self.load_gold_data(asset_class="forex")
        
        # Get economic indicators
        economic_data = self.load_gold_data(data_type="economic_indicators")
        
        return {
            "market_data": forex_data,
            "economic_data": economic_data
        }
    
    def transform_for_domain(self, data):
        """Apply forex-specific transformations"""
        
        market_data = data["market_data"]
        
        # Calculate forex-specific metrics
        transformed_data = {
            "pair_analytics": self.calculate_pair_analytics(market_data),
            "correlation_matrix": self.calculate_currency_correlations(market_data),
            "volatility_analysis": self.calculate_volatility_metrics(market_data),
            "carry_trade_analysis": self.calculate_carry_opportunities(market_data),
            "economic_impact": self.analyze_economic_impact(data["economic_data"])
        }
        
        return transformed_data
    
    def create_domain_analytics(self, data):
        """Create forex-specific analytics"""
        
        analytics = {
            "major_pairs_dashboard": self.create_major_pairs_dashboard(data),
            "volatility_ranking": self.create_volatility_ranking(data),
            "carry_trade_opportunities": self.create_carry_trade_ranking(data),
            "economic_calendar_impact": self.create_economic_impact_analysis(data),
            "technical_signals": self.create_technical_signals_summary(data)
        }
        
        return analytics
    
    def calculate_pair_analytics(self, market_data):
        """Calculate comprehensive pair analytics"""
        
        pair_analytics = {}
        
        for pair in self.major_pairs + self.minor_pairs:
            pair_data = market_data[market_data['symbol'] == pair]
            
            analytics = {
                "current_price": pair_data['close'].iloc[-1],
                "daily_change": self.calculate_daily_change(pair_data),
                "volatility_21d": self.calculate_volatility(pair_data, 21),
                "trend_strength": self.calculate_trend_strength(pair_data),
                "support_resistance": self.calculate_support_resistance(pair_data),
                "rsi_14": pair_data['rsi_14'].iloc[-1],
                "macd_signal": self.get_macd_signal(pair_data),
                "volume_profile": self.analyze_volume_profile(pair_data)
            }
            
            pair_analytics[pair] = analytics
        
        return pair_analytics
```

### **3. Portfolio Data Mart Implementation**
```python
class PortfolioDataMart(BaseDataMart):
    """Portfolio-specific data mart implementation"""
    
    def create_domain_analytics(self, data):
        """Create portfolio-specific analytics"""
        
        analytics = {
            "performance_dashboard": self.create_performance_dashboard(data),
            "risk_analytics": self.create_risk_analytics(data),
            "attribution_analysis": self.create_attribution_analysis(data),
            "rebalancing_recommendations": self.create_rebalancing_recommendations(data),
            "cost_analysis": self.create_cost_analysis(data)
        }
        
        return analytics
    
    def create_performance_dashboard(self, data):
        """Create comprehensive performance dashboard"""
        
        dashboard = {
            "total_return": self.calculate_total_return(data),
            "risk_adjusted_returns": {
                "sharpe_ratio": self.calculate_sharpe_ratio(data),
                "sortino_ratio": self.calculate_sortino_ratio(data),
                "calmar_ratio": self.calculate_calmar_ratio(data)
            },
            "drawdown_analysis": {
                "max_drawdown": self.calculate_max_drawdown(data),
                "current_drawdown": self.calculate_current_drawdown(data),
                "recovery_time": self.calculate_recovery_time(data)
            },
            "benchmark_comparison": self.compare_to_benchmarks(data)
        }
        
        return dashboard
```

## 📊 **Query Optimization for Data Marts**

### **1. Mart-Specific Indexing**
```python
def create_data_mart_indexes():
    """Create optimized indexes for each data mart"""
    
    mart_indexes = {
        "forex": {
            "pair_analytics": ["currency_pair", "date", "volatility_rank"],
            "economic_calendar": ["event_date", "currency", "impact_level"],
            "technical_signals": ["pair", "signal_type", "signal_strength"]
        },
        "portfolio": {
            "performance_metrics": ["portfolio_id", "date", "return_rank"],
            "risk_analytics": ["portfolio_id", "risk_metric", "value"],
            "holdings": ["portfolio_id", "symbol", "weight"]
        },
        "risk": {
            "var_metrics": ["portfolio_id", "confidence_level", "date"],
            "stress_tests": ["scenario_id", "portfolio_id", "impact"],
            "limit_monitoring": ["limit_type", "portfolio_id", "utilization"]
        }
    }
    
    for mart, tables in mart_indexes.items():
        for table, indexes in tables.items():
            create_table_indexes(f"{mart}_{table}", indexes)
```

### **2. Materialized Views**
```python
def create_mart_materialized_views():
    """Create materialized views for fast query performance"""
    
    views = {
        "forex_dashboard_view": """
            SELECT 
                currency_pair,
                current_price,
                daily_change_pct,
                volatility_rank,
                technical_score,
                last_updated
            FROM forex_pair_analytics
            WHERE date = CURRENT_DATE
        """,
        
        "portfolio_summary_view": """
            SELECT 
                portfolio_id,
                total_value,
                daily_return,
                sharpe_ratio,
                max_drawdown,
                risk_score
            FROM portfolio_performance_metrics
            WHERE date = CURRENT_DATE
        """
    }
    
    for view_name, query in views.items():
        create_materialized_view(view_name, query)
```

## 🔍 **Data Mart Query Interface**

### **Cross-Mart Analytics API**
```python
class DataMartAnalytics:
    """Unified interface for querying across data marts"""
    
    def __init__(self, data_marts_path):
        self.marts_path = data_marts_path
        self.mart_connections = self.initialize_mart_connections()
    
    def get_forex_dashboard(self, pairs=None):
        """Get forex trading dashboard data"""
        
        query = """
        SELECT currency_pair, current_price, daily_change_pct, 
               volatility_rank, technical_score, economic_impact
        FROM forex_dashboard_view
        """
        
        if pairs:
            pair_list = "','".join(pairs)
            query += f" WHERE currency_pair IN ('{pair_list}')"
        
        return self.query_mart("forex", query)
    
    def get_portfolio_risk_summary(self, portfolio_ids):
        """Get portfolio risk summary across risk and portfolio marts"""
        
        # Query portfolio mart for performance data
        portfolio_query = f"""
        SELECT portfolio_id, total_value, sharpe_ratio, max_drawdown
        FROM portfolio_summary_view
        WHERE portfolio_id IN ({','.join([f"'{p}'" for p in portfolio_ids])})
        """
        
        # Query risk mart for risk metrics
        risk_query = f"""
        SELECT portfolio_id, var_95, var_99, stress_test_result
        FROM risk_var_metrics
        WHERE portfolio_id IN ({','.join([f"'{p}'" for p in portfolio_ids])})
        AND date = CURRENT_DATE
        """
        
        portfolio_data = self.query_mart("portfolio", portfolio_query)
        risk_data = self.query_mart("risk", risk_query)
        
        # Combine results
        return self.merge_portfolio_risk_data(portfolio_data, risk_data)
    
    def get_cross_asset_opportunities(self):
        """Get opportunities across multiple asset classes"""
        
        opportunities = {
            "forex": self.query_mart("forex", "SELECT * FROM carry_trade_opportunities LIMIT 5"),
            "equities": self.query_mart("equities", "SELECT * FROM value_opportunities LIMIT 5"),
            "crypto": self.query_mart("crypto", "SELECT * FROM arbitrage_opportunities LIMIT 5")
        }
        
        return opportunities
```

## 🚨 **Best Practices**

### **1. Domain Optimization**
- ✅ Design schemas specific to business domain needs
- ✅ Pre-calculate frequently requested analytics
- ✅ Optimize for domain-specific query patterns
- ✅ Implement domain-specific data quality rules

### **2. Performance**
- ✅ Use appropriate partitioning for each domain
- ✅ Create materialized views for complex analytics
- ✅ Implement intelligent caching strategies
- ✅ Monitor and optimize query performance

### **3. Data Freshness**
- ✅ Define appropriate refresh frequencies by domain
- ✅ Implement real-time updates for critical metrics
- ✅ Monitor data staleness and lag
- ✅ Alert on delayed updates

## 🦄 **Unicorn Platform Integration**

Data Marts enable the platform by providing:
- **Domain Expertise**: Specialized analytics for each trading domain
- **Fast Queries**: Pre-aggregated data for real-time trading decisions
- **Business Intelligence**: Domain-specific dashboards and insights
- **Scalable Analytics**: Independent scaling by business domain

---

*Data Marts deliver domain expertise at query speed - the final mile of data warehouse optimization!*
