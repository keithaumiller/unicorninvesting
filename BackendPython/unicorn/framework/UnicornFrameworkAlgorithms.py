"""
Unicorn Framework-Based Algorithms
=================================

Main algorithms using LEAN's Algorithm Framework with modular components.
These demonstrate clean separation between forecasting, portfolio construction,
execution, and risk management.

Algorithm Types:
1. AdvancedForexFrameworkAlgorithm - Multi-currency forecasting with ensemble models
2. EthFrameworkAlgorithm - ETH-focused algorithm with technical analysis
3. UnicornMasterFrameworkAlgorithm - Combined multi-asset algorithm
"""

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/framework')

from AlgorithmImports import *
from alphas.AdvancedForexForecastingAlpha import AdvancedForexForecastingAlpha
from alphas.EthFocusedAlpha import EthFocusedAlpha
from portfolio.UnicornPortfolioConstruction import UnicornEqualWeightPortfolioConstruction, UnicornConfidenceWeightedPortfolioConstruction
from risk.UnicornRiskManagement import UnicornRiskManagementModel, UnicornForexRiskManagement


class AdvancedForexFrameworkAlgorithm(QCAlgorithm):
    """
    Advanced Forex Algorithm using LEAN Algorithm Framework.
    
    Framework Components:
    - Alpha: AdvancedForexForecastingAlpha (ARIMA + Neural + Prophet + XGBoost)
    - Portfolio: UnicornConfidenceWeightedPortfolioConstruction
    - Execution: ImmediateExecutionModel  
    - Risk: UnicornForexRiskManagement
    - Universe: Manual selection of forex pairs
    
    Clean separation: Forecasting -> Portfolio -> Execution -> Risk
    """
    
    def initialize(self):
        """Initialize the framework-based forex algorithm."""
        
        # Algorithm setup
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(100000)
        
        # Currency pairs for advanced forecasting
        forex_symbols = [
            Symbol.create("EURUSD", SecurityType.FOREX, Market.OANDA),
            Symbol.create("USDJPY", SecurityType.FOREX, Market.OANDA),
            Symbol.create("USDCNH", SecurityType.FOREX, Market.OANDA),
        ]
        
        # Add crypto if available
        try:
            eth_symbol = Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.GDAX)
            forex_symbols.append(eth_symbol)
        except:
            self.debug("⚠️ ETHUSD crypto not available, using forex pairs only")
        
        # FRAMEWORK COMPONENTS - Clean separation of concerns
        
        # 1. UNIVERSE SELECTION: Manual selection of currency pairs
        self.set_universe_selection(ManualUniverseSelectionModel(forex_symbols))
        
        # 2. ALPHA MODEL: Advanced forecasting (ARIMA + Neural + Prophet + XGBoost)
        self.set_alpha(AdvancedForexForecastingAlpha(
            prediction_horizon_hours=4,
            confidence_threshold=0.005  # 0.5% minimum expected return
        ))
        
        # 3. PORTFOLIO CONSTRUCTION: Confidence-weighted allocation
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction(
            rebalance_frequency=Resolution.DAILY,
            max_total_leverage=1.0
        ))
        
        # 4. EXECUTION MODEL: Immediate execution
        self.set_execution(ImmediateExecutionModel())
        
        # 5. RISK MANAGEMENT: Forex-specific risk controls
        self.set_risk_management(UnicornForexRiskManagement(
            max_position_size=0.25,        # 25% max per position
            stop_loss_percentage=0.02,     # 2% stop loss
            max_portfolio_drawdown=0.10,   # 10% max drawdown
            max_currency_exposure=0.6,     # 60% max currency exposure
            use_volatility_stops=True,
            volatility_multiplier=2.0
        ))
        
        # Algorithm monitoring
        self.last_rebalance = self.time
        self.trades_executed = 0
        self.insights_generated = 0
        
        self.debug("🚀 Advanced Forex Framework Algorithm Initialized")
        self.debug("📊 Components: Advanced Forecasting + Confidence Weighting + Forex Risk Management")
    
    def on_insights_generated(self, algorithm, data):
        """
        Called when Alpha Model generates new Insights.
        
        This is for monitoring - the framework handles everything automatically.
        """
        self.insights_generated += len(data.insights)
        
        for insight in data.insights:
            direction_str = "🟢 BUY" if insight.direction == InsightDirection.UP else "🔴 SELL"
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            
            self.debug(f"{direction_str} {insight.symbol}: "
                      f"Return {insight.magnitude:.2%}, Confidence {confidence:.2f}, "
                      f"Horizon {insight.period}")
    
    def on_order_event(self, order_event):
        """
        Monitor order executions.
        
        Framework handles order placement - this is just for tracking.
        """
        if order_event.status == OrderStatus.FILLED:
            self.trades_executed += 1
            
            fill_price = order_event.fill_price
            fill_quantity = order_event.fill_quantity
            
            action = "BUY" if fill_quantity > 0 else "SELL"
            self.debug(f"✅ {action} {order_event.symbol}: "
                      f"{abs(fill_quantity)} @ {fill_price:.5f}")
            
            # Update risk management with entry price
            if hasattr(self.risk_management, 'update_position_entry_price'):
                self.risk_management.update_position_entry_price(self, order_event.symbol, fill_price)
    
    def on_end_of_algorithm(self):
        """Final algorithm statistics."""
        self.debug("=" * 50)
        self.debug("🏁 ADVANCED FOREX FRAMEWORK ALGORITHM COMPLETE")
        self.debug(f"📈 Total Return: {self.portfolio.total_return:.2%}")
        self.debug(f"📊 Insights Generated: {self.insights_generated}")
        self.debug(f"⚡ Trades Executed: {self.trades_executed}")
        self.debug(f"💰 Final Portfolio Value: ${self.portfolio.total_portfolio_value:,.2f}")
        
        # Get risk summary if available
        if hasattr(self.risk_management, 'get_risk_summary'):
            risk_summary = self.risk_management.get_risk_summary(self)
            self.debug(f"🛡️ Max Drawdown: {risk_summary.get('current_drawdown', 0):.2%}")
            self.debug(f"🔍 Risk Events: {risk_summary.get('risk_events_count', 0)}")


class EthFrameworkAlgorithm(QCAlgorithm):
    """
    ETH-focused Algorithm using LEAN Algorithm Framework.
    
    Framework Components:
    - Alpha: EthFocusedAlpha (Technical analysis for ETH)
    - Portfolio: UnicornEqualWeightPortfolioConstruction (95% ETH allocation)
    - Execution: ImmediateExecutionModel
    - Risk: UnicornRiskManagementModel
    - Universe: ETH only
    
    $1000 starting capital focused on Ethereum trading.
    """
    
    def initialize(self):
        """Initialize the ETH-focused framework algorithm."""
        
        # Algorithm setup - $1000 ETH focused
        self.set_start_date(2023, 6, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(1000)  # $1000 as requested
        
        # ETH symbols
        eth_symbols = []
        try:
            # Try crypto first
            eth_symbols.append(Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.GDAX))
        except:
            try:
                # Fallback to forex
                eth_symbols.append(Symbol.create("ETHUSD", SecurityType.FOREX, Market.OANDA))
            except:
                self.debug("⚠️ Unable to add ETHUSD - check available markets")
        
        if not eth_symbols:
            raise Exception("No ETH symbols available")
        
        # FRAMEWORK COMPONENTS for ETH focus
        
        # 1. UNIVERSE: ETH only
        self.set_universe_selection(ManualUniverseSelectionModel(eth_symbols))
        
        # 2. ALPHA MODEL: ETH technical analysis
        self.set_alpha(EthFocusedAlpha(
            prediction_horizon_hours=2,
            confidence_threshold=0.01  # 1% minimum for ETH volatility
        ))
        
        # 3. PORTFOLIO CONSTRUCTION: Equal weight (95% ETH as designed)
        self.set_portfolio_construction(UnicornEqualWeightPortfolioConstruction(
            rebalance_frequency=Resolution.DAILY,
            max_position_size=0.95  # 95% ETH allocation
        ))
        
        # 4. EXECUTION: Immediate execution
        self.set_execution(ImmediateExecutionModel())
        
        # 5. RISK MANAGEMENT: Conservative for $1000 portfolio
        self.set_risk_management(UnicornRiskManagementModel(
            max_position_size=0.95,        # 95% max (ETH focused)
            stop_loss_percentage=0.02,     # 2% stop loss
            max_portfolio_drawdown=0.15,   # 15% max drawdown for crypto
            use_volatility_stops=True,
            volatility_multiplier=1.5      # Tighter stops for crypto
        ))
        
        # ETH-specific monitoring
        self.eth_trades = 0
        self.max_eth_value = 0
        
        self.debug("🟡 ETH Framework Algorithm Initialized")
        self.debug("💰 Starting Capital: $1,000")
        self.debug("🎯 Target: 95% ETH allocation with technical analysis")
    
    def on_insights_generated(self, algorithm, data):
        """Monitor ETH insights."""
        for insight in data.insights:
            direction = "🟢 BUY ETH" if insight.direction == InsightDirection.UP else "🔴 SELL ETH"
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            
            self.debug(f"{direction}: Expected {insight.magnitude:.2%}, "
                      f"Confidence {confidence:.2f}")
    
    def on_order_event(self, order_event):
        """Monitor ETH trades."""
        if order_event.status == OrderStatus.FILLED:
            self.eth_trades += 1
            
            action = "BOUGHT" if order_event.fill_quantity > 0 else "SOLD"
            value = abs(order_event.fill_quantity * order_event.fill_price)
            
            self.debug(f"💎 {action} ETH: ${value:,.2f} @ {order_event.fill_price:.2f}")
            
            # Track max ETH position value
            current_eth_value = self.portfolio.total_portfolio_value
            if current_eth_value > self.max_eth_value:
                self.max_eth_value = current_eth_value
    
    def on_end_of_algorithm(self):
        """ETH algorithm final stats."""
        self.debug("=" * 40)
        self.debug("🟡 ETH FRAMEWORK ALGORITHM COMPLETE")
        self.debug(f"💎 Total Return: {self.portfolio.total_return:.2%}")
        self.debug(f"⚡ ETH Trades: {self.eth_trades}")
        self.debug(f"📊 Final Value: ${self.portfolio.total_portfolio_value:,.2f}")
        self.debug(f"🚀 Max Value Reached: ${self.max_eth_value:,.2f}")
        
        # Calculate profit/loss
        profit_loss = self.portfolio.total_portfolio_value - 1000
        self.debug(f"💰 P&L: ${profit_loss:,.2f} ({profit_loss/10:.1f}%)")


class UnicornMasterFrameworkAlgorithm(QCAlgorithm):
    """
    Master Unicorn Algorithm combining multiple Alpha Models.
    
    Framework Components:
    - Alpha: CompositeAlphaModel (Advanced Forex + ETH Focused)
    - Portfolio: UnicornConfidenceWeightedPortfolioConstruction
    - Execution: ImmediateExecutionModel
    - Risk: UnicornRiskManagementModel
    - Universe: Multi-asset (Forex + Crypto)
    
    Demonstrates how to combine multiple forecasting approaches.
    """
    
    def initialize(self):
        """Initialize the master framework algorithm."""
        
        # Algorithm setup
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(250000)  # Larger capital for multi-asset
        
        # Multi-asset universe
        symbols = [
            Symbol.create("EURUSD", SecurityType.FOREX, Market.OANDA),
            Symbol.create("USDJPY", SecurityType.FOREX, Market.OANDA),
            Symbol.create("USDCNH", SecurityType.FOREX, Market.OANDA),
        ]
        
        # Add ETH
        try:
            symbols.append(Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.GDAX))
        except:
            try:
                symbols.append(Symbol.create("ETHUSD", SecurityType.FOREX, Market.OANDA))
            except:
                self.debug("⚠️ ETH not available")
        
        # MASTER FRAMEWORK - Composite approach
        
        # 1. UNIVERSE: Multi-asset
        self.set_universe_selection(ManualUniverseSelectionModel(symbols))
        
        # 2. ALPHA MODEL: Composite of multiple models
        composite_alpha = CompositeAlphaModel([
            AdvancedForexForecastingAlpha(
                prediction_horizon_hours=4,
                confidence_threshold=0.005
            ),
            EthFocusedAlpha(
                prediction_horizon_hours=2,
                confidence_threshold=0.01
            )
        ])
        self.set_alpha(composite_alpha)
        
        # 3. PORTFOLIO: Confidence-weighted with larger scale
        self.set_portfolio_construction(UnicornConfidenceWeightedPortfolioConstruction(
            rebalance_frequency=Resolution.DAILY,
            max_total_leverage=1.2  # Slightly more aggressive
        ))
        
        # 4. EXECUTION: Immediate
        self.set_execution(ImmediateExecutionModel())
        
        # 5. RISK: Comprehensive risk management
        self.set_risk_management(UnicornRiskManagementModel(
            max_position_size=0.3,         # 30% max per position
            stop_loss_percentage=0.02,     # 2% stop loss
            max_portfolio_drawdown=0.12,   # 12% max drawdown
            use_volatility_stops=True,
            volatility_multiplier=2.0
        ))
        
        # Master algorithm tracking
        self.forex_insights = 0
        self.eth_insights = 0
        self.total_trades = 0
        
        self.debug("🦄 UNICORN MASTER FRAMEWORK ALGORITHM INITIALIZED")
        self.debug("🌍 Multi-Asset: Advanced Forex + ETH Technical Analysis")
        self.debug("💰 Capital: $250,000")
    
    def on_insights_generated(self, algorithm, data):
        """Track insights from multiple Alpha Models."""
        for insight in data.insights:
            if 'ETH' in str(insight.symbol):
                self.eth_insights += 1
                prefix = "🟡 ETH"
            else:
                self.forex_insights += 1
                prefix = "🌍 FOREX"
            
            direction = "BUY" if insight.direction == InsightDirection.UP else "SELL"
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            
            self.debug(f"{prefix} {direction} {insight.symbol}: "
                      f"{insight.magnitude:.2%}, Confidence {confidence:.2f}")
    
    def on_order_event(self, order_event):
        """Track all order executions."""
        if order_event.status == OrderStatus.FILLED:
            self.total_trades += 1
            
            action = "BUY" if order_event.fill_quantity > 0 else "SELL"
            value = abs(order_event.fill_quantity * order_event.fill_price)
            
            self.debug(f"✅ {action} {order_event.symbol}: "
                      f"${value:,.2f} @ {order_event.fill_price:.5f}")
    
    def on_end_of_algorithm(self):
        """Master algorithm final statistics."""
        self.debug("=" * 60)
        self.debug("🦄 UNICORN MASTER FRAMEWORK ALGORITHM COMPLETE")
        self.debug(f"📈 Total Return: {self.portfolio.total_return:.2%}")
        self.debug(f"🌍 Forex Insights: {self.forex_insights}")
        self.debug(f"🟡 ETH Insights: {self.eth_insights}")
        self.debug(f"⚡ Total Trades: {self.total_trades}")
        self.debug(f"💰 Final Value: ${self.portfolio.total_portfolio_value:,.2f}")
        self.debug(f"🏆 Profit/Loss: ${self.portfolio.total_portfolio_value - 250000:,.2f}")
        
        self.debug("\n🎯 FRAMEWORK BENEFITS DEMONSTRATED:")
        self.debug("✅ Clean separation of forecasting and trading")
        self.debug("✅ Modular components for easy testing")
        self.debug("✅ Professional risk management")
        self.debug("✅ Scalable multi-asset approach")
        self.debug("✅ Reusable Alpha Models across strategies")
