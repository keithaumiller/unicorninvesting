"""
Yahoo Finance Forex Framework Algorithm
======================================

Modified version of AdvancedForexFrameworkAlgorithm using Yahoo Finance as the data source.
This provides a free alternative to paid forex data feeds.

Yahoo Finance Advantages for Forex:
- Free access without API keys
- Real-time minute-level data
- Major currency pairs available
- Reliable data quality

Supported Yahoo Finance Forex Pairs:
- EURUSD=X, GBPUSD=X, USDJPY=X, USDCHF=X
- AUDUSD=X, USDCAD=X, NZDUSD=X
- Plus crypto pairs: BTC-USD, ETH-USD

Framework Architecture:
- Alpha: Advanced forecasting with Yahoo Finance data
- Portfolio: Confidence-weighted allocation
- Risk: Forex-specific risk management
- Execution: Immediate order execution
"""

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/framework')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/data_sources')

from AlgorithmImports import *
from datetime import datetime, timedelta

# Yahoo Finance data sources
try:
    from YahooFinanceMinuteData import YahooFinanceForexData, YahooFinanceCryptoData
    YAHOO_AVAILABLE = True
except ImportError:
    YAHOO_AVAILABLE = False

# Framework components
try:
    from alphas.AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha
    from portfolio.UnicornPortfolioConstruction import UnicornConfidenceWeightedPortfolioConstruction
    from risk.UnicornRiskManagement import UnicornForexRiskManagement
    FRAMEWORK_AVAILABLE = True
except ImportError:
    FRAMEWORK_AVAILABLE = False


class YahooFinanceForexFrameworkAlgorithm(QCAlgorithm):
    """
    Yahoo Finance-powered Forex Framework Algorithm
    
    Features:
    - Free Yahoo Finance forex data (no API keys required)
    - Minute-level data for major currency pairs
    - Advanced ML forecasting (ARIMA, Neural Networks, Prophet, XGBoost)
    - Framework-based clean architecture
    - Comprehensive risk management
    
    Currency Pairs: EURUSD, GBPUSD, USDJPY, AUDUSD, BTC-USD, ETH-USD
    Data Source: Yahoo Finance (free)
    Prediction Horizon: 4 hours
    """
    
    def initialize(self):
        """Initialize the Yahoo Finance forex framework algorithm."""
        
        # Basic algorithm setup
        self.set_start_date(2024, 8, 1)
        self.set_end_date(2024, 8, 27)
        self.set_cash(100000)
        
        # Yahoo Finance forex symbols mapping
        self.yahoo_symbols = {}
        self.symbol_mapping = {}
        
        # ===========================================
        # YAHOO FINANCE DATA SOURCES
        # ===========================================
        
        if YAHOO_AVAILABLE:
            try:
                # Major forex pairs
                forex_pairs = {
                    'EURUSD': 'EURUSD',
                    'GBPUSD': 'GBPUSD', 
                    'USDJPY': 'USDJPY',
                    'AUDUSD': 'AUDUSD',
                    'USDCAD': 'USDCAD',
                    'USDCHF': 'USDCHF'
                }
                
                for lean_symbol, yahoo_symbol in forex_pairs.items():
                    symbol = self.add_data(YahooFinanceForexData, yahoo_symbol, Resolution.MINUTE).symbol
                    self.yahoo_symbols[lean_symbol] = symbol
                    self.symbol_mapping[symbol] = lean_symbol
                    self.debug(f"✅ Added Yahoo Finance forex: {lean_symbol} ({yahoo_symbol}=X)")
                
                # Crypto pairs (as forex alternatives)
                crypto_pairs = {
                    'BTCUSD': 'BTCUSD',
                    'ETHUSD': 'ETHUSD'
                }
                
                for lean_symbol, yahoo_symbol in crypto_pairs.items():
                    symbol = self.add_data(YahooFinanceCryptoData, yahoo_symbol, Resolution.MINUTE).symbol
                    self.yahoo_symbols[lean_symbol] = symbol
                    self.symbol_mapping[symbol] = lean_symbol
                    self.debug(f"✅ Added Yahoo Finance crypto: {lean_symbol} ({yahoo_symbol}-USD)")
                
                self.data_source = "Yahoo Finance"
                self.debug("🌐 Yahoo Finance data sources configured successfully")
                
            except Exception as e:
                self.debug(f"⚠️ Yahoo Finance setup failed: {e}")
                self.setup_fallback_data()
        else:
            self.debug("⚠️ Yahoo Finance not available, using fallback")
            self.setup_fallback_data()
        
        # ===========================================
        # FRAMEWORK COMPONENTS WITH YAHOO DATA
        # ===========================================
        
        if FRAMEWORK_AVAILABLE and len(self.yahoo_symbols) > 0:
            self.setup_framework_components()
        else:
            self.debug("⚠️ Framework not available, using simple strategy")
            self.setup_simple_strategy()
        
        # ===========================================
        # MONITORING AND TRACKING
        # ===========================================
        
        self.yahoo_data_points = 0
        self.insights_count = 0
        self.trades_count = 0
        self.last_rebalance = datetime.min
        self.rebalance_frequency = timedelta(hours=4)  # 4-hour rebalancing
        
        # Performance tracking
        self.daily_returns = []
        self.max_drawdown = 0
        self.peak_value = self.portfolio.total_portfolio_value
        
        self.debug("🚀 YAHOO FINANCE FOREX FRAMEWORK INITIALIZED")
        self.debug(f"📊 Data Source: {self.data_source}")
        self.debug(f"💰 Initial Capital: ${self.portfolio.total_portfolio_value:,.2f}")
        self.debug(f"🎯 Currency Pairs: {list(self.yahoo_symbols.keys())}")
    
    def setup_fallback_data(self):
        """Setup fallback data sources if Yahoo Finance fails."""
        
        try:
            # Use regular forex data
            fallback_pairs = ["EURUSD", "GBPUSD", "USDJPY"]
            for pair in fallback_pairs:
                symbol = self.add_forex(pair, Resolution.MINUTE, Market.OANDA).symbol
                self.yahoo_symbols[pair] = symbol
                self.symbol_mapping[symbol] = pair
            
            self.data_source = "Fallback Forex"
            self.debug("🔄 Fallback forex data configured")
            
        except Exception as e:
            self.debug(f"❌ Fallback setup failed: {e}")
            self.data_source = "None"
    
    def setup_framework_components(self):
        """Setup LEAN Algorithm Framework components."""
        
        # Convert to symbols for framework
        framework_symbols = list(self.yahoo_symbols.values())
        
        # 1. Universe Selection
        self.set_universe_selection(ManualUniverseSelectionModel(framework_symbols))
        
        # 2. Alpha Model (Forecasting)
        self.set_alpha(AdvancedForexForecastingAlpha(
            prediction_horizon_hours=4,
            confidence_threshold=0.005
        ))
        
        # 3. Portfolio Construction
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction(
            rebalance_frequency=Resolution.DAILY,
            max_total_leverage=1.0
        ))
        
        # 4. Execution Model
        self.set_execution(ImmediateExecutionModel())
        
        # 5. Risk Management
        self.set_risk_management(UnicornForexRiskManagement(
            max_position_size=0.20,           # 20% max per position (more conservative)
            stop_loss_percentage=0.015,       # 1.5% stop loss
            max_portfolio_drawdown=0.08,      # 8% max drawdown
            max_currency_exposure=0.5,        # 50% max currency exposure
            use_volatility_stops=True,
            volatility_multiplier=2.0
        ))
        
        self.framework_enabled = True
        self.debug("🎯 Framework components configured for Yahoo Finance data")
    
    def setup_simple_strategy(self):
        """Setup simple momentum strategy if framework unavailable."""
        
        self.framework_enabled = False
        self.momentum_lookback = 60  # 60 minutes
        self.momentum_threshold = 0.001  # 0.1% threshold
        self.position_size = 0.15  # 15% per position
        
        self.debug("📈 Simple momentum strategy configured")
    
    def on_data(self, data):
        """Process Yahoo Finance data."""
        
        # Count data points
        for symbol in self.yahoo_symbols.values():
            if data.contains_key(symbol):
                self.yahoo_data_points += 1
        
        # Update performance tracking
        self.update_performance_metrics()
        
        # Framework handles trading automatically if enabled
        if not self.framework_enabled:
            self.execute_simple_strategy(data)
    
    def execute_simple_strategy(self, data):
        """Execute simple momentum strategy using Yahoo Finance data."""
        
        if self.time - self.last_rebalance < self.rebalance_frequency:
            return
        
        momentum_signals = {}
        
        # Calculate momentum for each Yahoo Finance symbol
        for pair_name, symbol in self.yahoo_symbols.items():
            if not data.contains_key(symbol):
                continue
            
            # Get historical data
            history = self.history(symbol, self.momentum_lookback, Resolution.MINUTE)
            
            if history.empty or len(history) < self.momentum_lookback:
                continue
            
            # Calculate momentum
            if 'close' in history.columns:
                start_price = history['close'].iloc[0]
                current_price = history['close'].iloc[-1]
            else:
                start_price = history['value'].iloc[0]
                current_price = data[symbol].value
            
            if start_price > 0:
                momentum = (current_price - start_price) / start_price
                momentum_signals[symbol] = {
                    'momentum': momentum,
                    'price': current_price,
                    'pair': pair_name
                }
                
                self.debug(f"Yahoo {pair_name}: Price=${current_price:.5f}, "
                          f"Momentum={momentum:.4f} ({momentum*100:.2f}%)")
        
        # Execute trades based on momentum
        self.execute_momentum_trades(momentum_signals)
        self.last_rebalance = self.time
    
    def execute_momentum_trades(self, signals):
        """Execute trades based on momentum signals."""
        
        # Sort by momentum strength
        sorted_signals = sorted(signals.items(), 
                              key=lambda x: abs(x[1]['momentum']), reverse=True)
        
        # Buy/Sell decisions
        for symbol, signal in sorted_signals:
            momentum = signal['momentum']
            pair_name = signal['pair']
            
            current_position = self.portfolio[symbol]
            
            # Strong positive momentum - buy signal
            if momentum > self.momentum_threshold and not current_position.is_long:
                if current_position.is_short:
                    self.liquidate(symbol)  # Close short first
                
                target_value = self.portfolio.total_portfolio_value * self.position_size
                if signal['price'] > 0:
                    quantity = int(target_value / signal['price'])
                    if quantity > 0:
                        self.market_order(symbol, quantity)
                        self.trades_count += 1
                        self.debug(f"🟢 BUY {pair_name}: Quantity={quantity}, "
                                  f"Momentum={momentum:.4f}")
            
            # Strong negative momentum - sell signal
            elif momentum < -self.momentum_threshold and not current_position.is_short:
                if current_position.is_long:
                    self.liquidate(symbol)  # Close long first
                
                target_value = self.portfolio.total_portfolio_value * self.position_size
                if signal['price'] > 0:
                    quantity = -int(target_value / signal['price'])
                    if quantity < 0:
                        self.market_order(symbol, quantity)
                        self.trades_count += 1
                        self.debug(f"🔴 SELL {pair_name}: Quantity={quantity}, "
                                  f"Momentum={momentum:.4f}")
    
    def update_performance_metrics(self):
        """Update performance tracking metrics."""
        
        current_value = self.portfolio.total_portfolio_value
        
        # Track peak and drawdown
        if current_value > self.peak_value:
            self.peak_value = current_value
        
        current_drawdown = (self.peak_value - current_value) / self.peak_value
        if current_drawdown > self.max_drawdown:
            self.max_drawdown = current_drawdown
    
    def on_insights_generated(self, algorithm, data):
        """Monitor framework-generated insights."""
        
        if not self.framework_enabled:
            return
        
        self.insights_count += len(data.insights)
        
        for insight in data.insights:
            # Get pair name from symbol mapping
            pair_name = self.symbol_mapping.get(insight.symbol, str(insight.symbol))
            
            direction_emoji = "🟢" if insight.direction == InsightDirection.UP else "🔴"
            direction_text = "BUY" if insight.direction == InsightDirection.UP else "SELL"
            
            self.debug(f"{direction_emoji} YAHOO FORECAST {pair_name}: "
                      f"{direction_text} - Expected Return: {insight.magnitude:.2%}, "
                      f"Period: {insight.period}")
    
    def on_order_event(self, order_event):
        """Monitor order executions."""
        
        if order_event.status == OrderStatus.FILLED:
            # Get pair name
            pair_name = self.symbol_mapping.get(order_event.symbol, str(order_event.symbol))
            
            direction = "BUY" if order_event.quantity > 0 else "SELL"
            emoji = "✅" if order_event.quantity > 0 else "❌"
            
            self.debug(f"{emoji} FILLED {pair_name}: {direction} "
                      f"{abs(order_event.quantity)} @ ${order_event.fill_price:.5f}")
    
    def on_end_of_algorithm(self):
        """Algorithm completion summary."""
        
        final_value = self.portfolio.total_portfolio_value
        total_return = (final_value / 100000 - 1) * 100
        
        self.debug("=" * 60)
        self.debug("YAHOO FINANCE FOREX FRAMEWORK - FINAL SUMMARY")
        self.debug("=" * 60)
        self.debug(f"📊 Data Source: {self.data_source}")
        self.debug(f"🎯 Currency Pairs Tracked: {len(self.yahoo_symbols)}")
        self.debug(f"📈 Yahoo Data Points: {self.yahoo_data_points:,}")
        self.debug(f"🔮 Insights Generated: {self.insights_count}")
        self.debug(f"💼 Total Trades: {self.trades_count}")
        self.debug(f"💰 Final Portfolio Value: ${final_value:,.2f}")
        self.debug(f"📊 Total Return: {total_return:.2f}%")
        self.debug(f"📉 Maximum Drawdown: {self.max_drawdown:.2%}")
        
        # Final positions
        self.debug("\n🏦 Final Positions:")
        for pair_name, symbol in self.yahoo_symbols.items():
            position = self.portfolio[symbol]
            if position.invested:
                pnl = position.unrealized_profit
                pnl_percent = position.unrealized_profit_percent
                self.debug(f"   {pair_name}: Quantity={position.quantity}, "
                          f"Value=${position.holdings_value:,.2f}, "
                          f"P&L=${pnl:,.2f} ({pnl_percent:.2%})")
        
        self.debug("=" * 60)
        
        # Data quality summary
        if self.yahoo_data_points > 0:
            avg_data_per_symbol = self.yahoo_data_points / len(self.yahoo_symbols)
            self.debug(f"📊 Yahoo Finance Data Quality: {avg_data_per_symbol:.0f} points per symbol")
        
        # Performance summary
        if self.framework_enabled:
            self.debug("🎯 Framework-based execution completed successfully")
        else:
            self.debug("📈 Simple momentum strategy completed")


# Test algorithm for Yahoo Finance connectivity
class YahooFinanceForexTest(QCAlgorithm):
    """Simple test to verify Yahoo Finance forex data connectivity."""
    
    def initialize(self):
        self.set_start_date(2024, 8, 20)
        self.set_end_date(2024, 8, 27)
        self.set_cash(10000)
        
        try:
            from YahooFinanceMinuteData import YahooFinanceForexData
            self.eurusd = self.add_data(YahooFinanceForexData, "EURUSD", Resolution.MINUTE).symbol
            self.data_source = "Yahoo Finance"
            self.debug("✅ Yahoo Finance EURUSD connected")
        except:
            self.eurusd = self.add_forex("EURUSD", Resolution.MINUTE).symbol
            self.data_source = "Fallback"
            self.debug("⚠️ Using fallback data")
        
        self.data_count = 0
    
    def on_data(self, data):
        if data.contains_key(self.eurusd):
            self.data_count += 1
            if self.data_count % 60 == 0:  # Log hourly
                price = data[self.eurusd].value
                self.debug(f"{self.data_source} EURUSD #{self.data_count}: ${price:.5f}")
    
    def on_end_of_algorithm(self):
        self.debug(f"{self.data_source} Test Complete: {self.data_count} data points")


if __name__ == "__main__":
    print("Yahoo Finance Forex Framework Algorithm")
    print("Free forex data source for LEAN framework")
    print("Run YahooFinanceForexTest for simple connectivity testing")
