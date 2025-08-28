"""
Advanced Forex Framework Algorithm
=================================

Framework-based implementation of the advanced forex forecasting algorithm.
This demonstrates the clean separation between forecasting (Alpha Model) and 
trading strategy (Portfolio Construction + Risk Management).

Migration from Monolithic to Framework:
- OLD: Everything mixed in AdvancedForexForecastingAlgorithm.py
- NEW: Clean separation using LEAN Algorithm Framework

Framework Components:
- Alpha: AdvancedForexForecastingAlpha (Pure forecasting)
- Portfolio: UnicornConfidenceWeightedPortfolioConstruction
- Execution: ImmediateExecutionModel
- Risk: UnicornForexRiskManagement

YAHOO FINANCE INTEGRATION:
- Added support for Yahoo Finance as free data source
- No API keys required
- Supports major forex pairs: EURUSD, GBPUSD, USDJPY, etc.
- Fallback to regular data if Yahoo Finance unavailable
"""

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/framework')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/data_sources')

from AlgorithmImports import *
from alphas.AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha
from portfolio.UnicornPortfolioConstruction import UnicornConfidenceWeightedPortfolioConstruction
from risk.UnicornRiskManagement import UnicornForexRiskManagement

# Yahoo Finance data integration
try:
    from YahooFinanceMinuteData import YahooFinanceForexData, YahooFinanceCryptoData
    YAHOO_FINANCE_AVAILABLE = True
except ImportError:
    YAHOO_FINANCE_AVAILABLE = False


class AdvancedForexFrameworkAlgorithm(QCAlgorithm):
    """
    Advanced Forex Framework Algorithm - Clean Architecture Version
    
    Pure separation of concerns:
    1. Forecasting: ARIMA + Neural Networks + Prophet + XGBoost (Alpha Model)
    2. Position Sizing: Confidence-weighted allocation (Portfolio Model)
    3. Risk Management: Stop losses, drawdown limits, currency exposure (Risk Model)
    4. Execution: Immediate order placement (Execution Model)
    
    Currency Pairs: EURUSD, USDJPY, USDCNH, ETHUSD
    Prediction Horizon: 4 hours
    """
    
    def initialize(self):
        """Initialize the framework-based algorithm with Yahoo Finance support."""
        
        # Basic algorithm setup
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(100000)
        
        # Yahoo Finance integration flag
        self.using_yahoo_finance = False
        self.yahoo_symbols = {}
        
        # ===========================================
        # DATA SOURCE SELECTION: Yahoo Finance or Default
        # ===========================================
        
        if YAHOO_FINANCE_AVAILABLE:
            try:
                # Try Yahoo Finance first (free, no API key required)
                forex_symbols = self.setup_yahoo_finance_data()
                self.using_yahoo_finance = True
                self.debug("🌐 Using Yahoo Finance as primary data source")
            except Exception as e:
                self.debug(f"⚠️ Yahoo Finance setup failed: {e}")
                forex_symbols = self.setup_default_forex_data()
        else:
            self.debug("ℹ️ Yahoo Finance not available, using default data")
            forex_symbols = self.setup_default_forex_data()
        
        # ===========================================
        # LEAN ALGORITHM FRAMEWORK COMPONENTS
        # ===========================================
        
        # 1. UNIVERSE SELECTION: Manual selection of forex pairs
        self.set_universe_selection(ManualUniverseSelectionModel(forex_symbols))
        
        # 2. ALPHA MODEL: Pure forecasting using ensemble of ML models
        self.set_alpha(AdvancedForexForecastingAlpha(
            prediction_horizon_hours=4,      # 4-hour prediction horizon
            confidence_threshold=0.005       # 0.5% minimum expected return to trade
        ))
        
        # 3. PORTFOLIO CONSTRUCTION: Confidence-weighted position sizing
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction(
            rebalance_frequency=Resolution.DAILY,   # Daily rebalancing
            max_total_leverage=1.0                  # No leverage for safety
        ))
        
        # 4. EXECUTION MODEL: Immediate order execution
        self.set_execution(ImmediateExecutionModel())
        
        # 5. RISK MANAGEMENT: Forex-specific risk controls
        self.set_risk_management(UnicornForexRiskManagement(
            max_position_size=0.25,           # 25% max per position
            stop_loss_percentage=0.02,        # 2% stop loss
            max_portfolio_drawdown=0.10,      # 10% max portfolio drawdown
            max_currency_exposure=0.6,        # 60% max exposure to any currency
            use_volatility_stops=True,        # ATR-based dynamic stops
            volatility_multiplier=2.0         # 2x ATR for stop distance
        ))
        
        # ===========================================
        # MONITORING AND LOGGING
        # ===========================================
        
        self.insights_count = 0
        self.trades_count = 0
        self.last_insight_time = datetime.min
        self.yahoo_data_points = 0
        
        # Log initialization
        data_source = "Yahoo Finance (Free)" if self.using_yahoo_finance else "Default Forex Data"
        self.debug("🚀 ADVANCED FOREX FRAMEWORK ALGORITHM INITIALIZED")
        self.debug(f"� Data Source: {data_source}")
        self.debug("�🔮 Forecasting: ARIMA + Neural Networks + Prophet + XGBoost")
        self.debug("⚖️ Portfolio: Confidence-weighted allocation")
        self.debug("🛡️ Risk: Multi-layer forex risk management")
        self.debug("💰 Capital: $100,000")
    
    def setup_yahoo_finance_data(self):
        """
        Setup Yahoo Finance data sources for major forex pairs.
        
        Yahoo Finance provides free forex data without API keys.
        Format: EURUSD=X, GBPUSD=X, etc.
        
        Returns:
            List of symbols for framework universe selection
        """
        forex_symbols = []
        
        # Major forex pairs to track with Yahoo Finance
        yahoo_forex_pairs = {
            'EURUSD': 'EURUSD',
            'GBPUSD': 'GBPUSD', 
            'USDJPY': 'USDJPY',
            'AUDUSD': 'AUDUSD'
        }
        
        for lean_pair, yahoo_pair in yahoo_forex_pairs.items():
            try:
                symbol = self.add_data(YahooFinanceForexData, yahoo_pair, Resolution.MINUTE).symbol
                forex_symbols.append(symbol)
                self.yahoo_symbols[lean_pair] = symbol
                self.debug(f"✅ Yahoo Finance: {lean_pair} -> {yahoo_pair}=X")
            except Exception as e:
                self.debug(f"❌ Failed to add Yahoo Finance {lean_pair}: {e}")
        
        # Add crypto as forex alternatives
        yahoo_crypto_pairs = {
            'BTCUSD': 'BTCUSD',
            'ETHUSD': 'ETHUSD'
        }
        
        for lean_pair, yahoo_pair in yahoo_crypto_pairs.items():
            try:
                symbol = self.add_data(YahooFinanceCryptoData, yahoo_pair, Resolution.MINUTE).symbol
                forex_symbols.append(symbol)
                self.yahoo_symbols[lean_pair] = symbol
                self.debug(f"✅ Yahoo Finance Crypto: {lean_pair} -> {yahoo_pair}-USD")
            except Exception as e:
                self.debug(f"❌ Failed to add Yahoo Finance crypto {lean_pair}: {e}")
        
        if len(forex_symbols) == 0:
            raise Exception("No Yahoo Finance symbols successfully added")
        
        self.debug(f"🌐 Yahoo Finance setup complete: {len(forex_symbols)} symbols")
        return forex_symbols
    
    def setup_default_forex_data(self):
        """
        Setup default forex data as fallback.
        
        Returns:
            List of symbols for framework universe selection  
        """
        # Define forex universe using traditional LEAN forex symbols
        forex_symbols = [
            Symbol.create("EURUSD", SecurityType.FOREX, Market.OANDA),
            Symbol.create("USDJPY", SecurityType.FOREX, Market.OANDA),
            Symbol.create("USDCNH", SecurityType.FOREX, Market.OANDA),
        ]
        
        # Add crypto ETH if available
        try:
            eth_symbol = Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.GDAX)
            forex_symbols.append(eth_symbol)
            self.debug("✅ Added ETHUSD crypto")
        except:
            try:
                eth_symbol = Symbol.create("ETHUSD", SecurityType.FOREX, Market.OANDA)
                forex_symbols.append(eth_symbol)
                self.debug("✅ Added ETHUSD forex")
            except:
                self.debug("⚠️ ETHUSD not available")
        
        self.debug(f"🔄 Default forex setup complete: {len(forex_symbols)} symbols")
        return forex_symbols
    
    def on_insights_generated(self, algorithm, data):
        """
        Called when Alpha Model generates new Insights.
        
        This is for monitoring only - the framework handles trading automatically.
        Enhanced with Yahoo Finance data source tracking.
        """
        self.insights_count += len(data.insights)
        self.last_insight_time = self.time
        
        for insight in data.insights:
            # Parse insight details
            direction_emoji = "🟢" if insight.direction == InsightDirection.UP else "🔴"
            direction_text = "BUY" if insight.direction == InsightDirection.UP else "SELL"
            
            # Get confidence if available
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            
            # Get pair name for Yahoo Finance symbols
            pair_name = self.get_pair_name_for_symbol(insight.symbol)
            data_source_indicator = "📊" if self.using_yahoo_finance else "💼"
            
            # Log the insight with data source info
            self.debug(f"{direction_emoji} {data_source_indicator} FORECAST {pair_name}: "
                      f"{direction_text} - Expected Return: {insight.magnitude:.2%}, "
                      f"Confidence: {confidence:.2f}, "
                      f"Horizon: {insight.period}")
            
            # Add insight metadata if available
            if hasattr(insight, 'tag') and insight.tag:
                self.debug(f"   📊 Details: {insight.tag}")
                
            # Log Yahoo Finance specific info
            if self.using_yahoo_finance:
                self.debug(f"   🌐 Yahoo Finance Data Source: FREE")
    
    def get_pair_name_for_symbol(self, symbol):
        """Get human-readable pair name for symbol, especially for Yahoo Finance."""
        
        if self.using_yahoo_finance:
            # Find the pair name from yahoo_symbols mapping
            for pair_name, yahoo_symbol in self.yahoo_symbols.items():
                if yahoo_symbol == symbol:
                    return pair_name
        
        # Fallback to symbol string
        return str(symbol)
    
    def on_data(self, data):
        """
        Monitor incoming data, especially Yahoo Finance data quality.
        
        Framework handles trading automatically, this is for monitoring.
        """
        if self.using_yahoo_finance:
            # Count Yahoo Finance data points
            for pair_name, symbol in self.yahoo_symbols.items():
                if data.contains_key(symbol):
                    self.yahoo_data_points += 1
                    
                    # Log occasional data quality checks
                    if self.yahoo_data_points % 100 == 0:
                        price = data[symbol].value
                        self.debug(f"🌐 Yahoo Finance {pair_name}: "
                                  f"Data Point #{self.yahoo_data_points}, "
                                  f"Price=${price:.5f}")
    
    def on_order_event(self, order_event):
        """
        Monitor order executions from the framework.
        
        Framework handles all order placement - this is just for tracking.
        Enhanced with Yahoo Finance symbol mapping.
        """
        if order_event.status == OrderStatus.FILLED:
            self.trades_count += 1
            
            # Parse order details
            symbol = order_event.symbol
            quantity = order_event.fill_quantity
            price = order_event.fill_price
            value = abs(quantity * price)
            
            action = "BOUGHT" if quantity > 0 else "SOLD"
            action_emoji = "📈" if quantity > 0 else "📉"
            
            # Log the trade execution
            self.debug(f"{action_emoji} EXECUTED: {action} {symbol}")
            self.debug(f"   💰 Quantity: {abs(quantity):,.0f}, Price: {price:.5f}")
            self.debug(f"   💵 Trade Value: ${value:,.2f}")
            
            # Update risk management with entry price for stop losses
            if hasattr(self.risk_management, 'update_position_entry_price'):
                self.risk_management.update_position_entry_price(self, symbol, price)
    
    def on_end_of_day(self):
        """
        Daily monitoring and reporting.
        """
        # Log daily portfolio status
        portfolio_value = self.portfolio.total_portfolio_value
        daily_return = self.portfolio.total_return
        
        self.debug(f"📊 Daily Update: Portfolio Value: ${portfolio_value:,.2f} "
                  f"(Return: {daily_return:.2%})")
        
        # Log position summary
        active_positions = sum(1 for security in self.securities.values() if security.invested)
        if active_positions > 0:
            self.debug(f"🎯 Active Positions: {active_positions}")
            
            for security in self.securities.values():
                if security.invested:
                    holdings_value = security.holdings.holdings_value
                    weight = holdings_value / portfolio_value if portfolio_value > 0 else 0
                    self.debug(f"   📍 {security.symbol}: ${holdings_value:,.2f} ({weight:.1%})")
    
    def on_end_of_algorithm(self):
        """
        Final algorithm statistics and framework performance analysis.
        """
        # Calculate final metrics
        final_value = self.portfolio.total_portfolio_value
        total_return = self.portfolio.total_return
        profit_loss = final_value - 100000
        
        self.debug("=" * 70)
        self.debug("🏁 ADVANCED FOREX FRAMEWORK ALGORITHM COMPLETE")
        self.debug("=" * 70)
        
        # Performance metrics
        self.debug("📈 PERFORMANCE METRICS:")
        self.debug(f"   💰 Starting Capital: $100,000")
        self.debug(f"   💎 Final Portfolio Value: ${final_value:,.2f}")
        self.debug(f"   📊 Total Return: {total_return:.2%}")
        self.debug(f"   💵 Profit/Loss: ${profit_loss:,.2f}")
        
        # Trading activity
        self.debug("\n⚡ TRADING ACTIVITY:")
        self.debug(f"   🔮 Total Insights Generated: {self.insights_count}")
        self.debug(f"   📊 Total Trades Executed: {self.trades_count}")
        self.debug(f"   🕐 Last Insight Time: {self.last_insight_time}")
        
        # Framework benefits summary
        self.debug("\n🎯 FRAMEWORK BENEFITS ACHIEVED:")
        self.debug("   ✅ Clean separation: Forecasting vs Trading")
        self.debug("   ✅ Modular components: Easy to test and modify")
        self.debug("   ✅ Professional risk management: Multi-layer protection")
        self.debug("   ✅ Reusable Alpha Model: Can be used in other strategies")
        self.debug("   ✅ Scalable architecture: Easy to add new models")
        
        # Get risk management summary if available
        if hasattr(self.risk_management, 'get_risk_summary'):
            risk_summary = self.risk_management.get_risk_summary(self)
            self.debug("\n🛡️ RISK MANAGEMENT SUMMARY:")
            self.debug(f"   📉 Maximum Drawdown: {risk_summary.get('current_drawdown', 0):.2%}")
            self.debug(f"   🎯 Active Positions: {risk_summary.get('active_positions', 0)}")
            self.debug(f"   ⚠️ Risk Events: {risk_summary.get('risk_events_count', 0)}")
        
        self.debug("=" * 70)
        self.debug("🦄 UNICORN INVESTING - ADVANCED FOREX FRAMEWORK")
        self.debug("=" * 70)
