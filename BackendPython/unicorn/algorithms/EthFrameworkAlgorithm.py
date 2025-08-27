"""
ETH Framework Algorithm
======================

Framework-based implementation of the ETH-focused trading algorithm.
This demonstrates clean separation for cryptocurrency trading using
technical analysis and the LEAN Algorithm Framework.

Migration from Monolithic to Framework:
- OLD: Everything mixed in EthOnlyPortfolio.py
- NEW: Clean separation using LEAN Algorithm Framework

Framework Components:
- Alpha: EthFocusedAlpha (Technical analysis for ETH)
- Portfolio: UnicornEqualWeightPortfolioConstruction (95% ETH allocation)
- Execution: ImmediateExecutionModel
- Risk: UnicornRiskManagementModel (Crypto-specific settings)

Target: $1,000 starting capital focused on Ethereum
"""

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/framework')

from AlgorithmImports import *
from alphas.EthFocusedAlpha import EthFocusedAlpha
from portfolio.UnicornPortfolioConstruction import UnicornEqualWeightPortfolioConstruction
from risk.UnicornRiskManagement import UnicornRiskManagementModel


class EthFrameworkAlgorithm(QCAlgorithm):
    """
    ETH Framework Algorithm - Clean Architecture Version
    
    Pure separation of concerns:
    1. Forecasting: Technical analysis (SMA, RSI, Bollinger Bands) (Alpha Model)
    2. Position Sizing: 95% ETH allocation with equal weighting (Portfolio Model)
    3. Risk Management: Crypto-specific risk controls (Risk Model)
    4. Execution: Immediate order placement (Execution Model)
    
    Asset Focus: ETHUSD only
    Starting Capital: $1,000
    Prediction Horizon: 2 hours (suitable for crypto volatility)
    """
    
    def initialize(self):
        """Initialize the ETH-focused framework algorithm."""
        
        # Basic algorithm setup - $1,000 ETH focus as requested
        self.set_start_date(2023, 6, 1)   # Start from mid-2023
        self.set_end_date(2024, 1, 1)     # 6-month test period
        self.set_cash(1000)               # $1,000 starting capital
        
        # Define ETH universe - try multiple markets
        eth_symbols = []
        
        # Try to add ETHUSD from different markets
        try:
            # Primary: Crypto exchange
            eth_symbol = Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.GDAX)
            eth_symbols.append(eth_symbol)
            self.debug("✅ Added ETHUSD from GDAX crypto market")
        except:
            try:
                # Fallback: Forex market
                eth_symbol = Symbol.create("ETHUSD", SecurityType.FOREX, Market.OANDA)
                eth_symbols.append(eth_symbol)
                self.debug("✅ Added ETHUSD from OANDA forex market")
            except:
                # Final fallback: Try different crypto exchanges
                try:
                    eth_symbol = Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.Coinbase)
                    eth_symbols.append(eth_symbol)
                    self.debug("✅ Added ETHUSD from Coinbase")
                except:
                    raise Exception("❌ Unable to add ETHUSD from any market")
        
        # ===========================================
        # LEAN ALGORITHM FRAMEWORK COMPONENTS
        # ===========================================
        
        # 1. UNIVERSE SELECTION: ETH only
        self.set_universe_selection(ManualUniverseSelectionModel(eth_symbols))
        
        # 2. ALPHA MODEL: ETH technical analysis
        self.set_alpha(EthFocusedAlpha(
            prediction_horizon_hours=2,      # 2-hour horizon for crypto volatility
            confidence_threshold=0.01        # 1% minimum expected return for ETH
        ))
        
        # 3. PORTFOLIO CONSTRUCTION: 95% ETH allocation
        self.set_portfolio_construction(UnicornEqualWeightPortfolioConstruction(
            rebalance_frequency=Resolution.DAILY,   # Daily rebalancing
            max_position_size=0.95                  # 95% ETH allocation as designed
        ))
        
        # 4. EXECUTION MODEL: Immediate execution for crypto
        self.set_execution(ImmediateExecutionModel())
        
        # 5. RISK MANAGEMENT: Crypto-specific risk controls
        self.set_risk_management(UnicornRiskManagementModel(
            max_position_size=0.95,           # 95% max (ETH focused)
            stop_loss_percentage=0.02,        # 2% stop loss (conservative for $1K)
            max_portfolio_drawdown=0.15,      # 15% max drawdown (crypto volatility)
            use_volatility_stops=True,        # ATR-based stops for crypto
            volatility_multiplier=1.5         # Tighter stops for crypto (1.5x ATR)
        ))
        
        # ===========================================
        # ETH-SPECIFIC MONITORING
        # ===========================================
        
        self.eth_insights = 0
        self.eth_trades = 0
        self.max_portfolio_value = 1000
        self.min_portfolio_value = 1000
        self.eth_buy_count = 0
        self.eth_sell_count = 0
        
        # Log initialization
        self.debug("🟡 ETH FRAMEWORK ALGORITHM INITIALIZED")
        self.debug("💎 Focus: 95% Ethereum allocation")
        self.debug("📊 Analysis: SMA + RSI + Bollinger Bands")
        self.debug("💰 Capital: $1,000")
        self.debug("🎯 Target: Ethereum growth exposure with risk management")
    
    def on_insights_generated(self, algorithm, data):
        """
        Monitor ETH-specific insights from technical analysis.
        """
        for insight in data.insights:
            self.eth_insights += 1
            
            # Parse ETH insight
            direction_emoji = "🟢" if insight.direction == InsightDirection.UP else "🔴"
            direction_text = "BUY ETH" if insight.direction == InsightDirection.UP else "SELL ETH"
            
            # Get confidence and technical details
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            
            self.debug(f"{direction_emoji} ETH SIGNAL: {direction_text}")
            self.debug(f"   📈 Expected Return: {insight.magnitude:.2%}")
            self.debug(f"   🎯 Confidence: {confidence:.2f}")
            self.debug(f"   ⏱️ Horizon: {insight.period}")
            
            # Log technical analysis details if available
            if hasattr(insight, 'tag') and insight.tag:
                self.debug(f"   📊 Technical: {insight.tag}")
    
    def on_order_event(self, order_event):
        """
        Monitor ETH order executions.
        """
        if order_event.status == OrderStatus.FILLED:
            self.eth_trades += 1
            
            # Parse ETH trade details
            quantity = order_event.fill_quantity
            price = order_event.fill_price
            trade_value = abs(quantity * price)
            
            if quantity > 0:
                self.eth_buy_count += 1
                action = "BOUGHT ETH"
                action_emoji = "💎📈"
            else:
                self.eth_sell_count += 1
                action = "SOLD ETH"
                action_emoji = "💎📉"
            
            # Log ETH trade
            self.debug(f"{action_emoji} {action}")
            self.debug(f"   💰 Quantity: {abs(quantity):,.4f} ETH")
            self.debug(f"   💵 Price: ${price:,.2f}")
            self.debug(f"   📊 Trade Value: ${trade_value:,.2f}")
            
            # Update portfolio tracking
            current_value = self.portfolio.total_portfolio_value
            if current_value > self.max_portfolio_value:
                self.max_portfolio_value = current_value
            if current_value < self.min_portfolio_value:
                self.min_portfolio_value = current_value
            
            # Calculate current ETH allocation
            eth_holdings_value = 0
            for security in self.securities.values():
                if security.invested and 'ETH' in str(security.symbol):
                    eth_holdings_value = abs(security.holdings.holdings_value)
            
            eth_allocation = (eth_holdings_value / current_value * 100) if current_value > 0 else 0
            
            self.debug(f"   🎯 Current ETH Allocation: {eth_allocation:.1f}%")
            self.debug(f"   💼 Portfolio Value: ${current_value:,.2f}")
    
    def on_end_of_day(self):
        """
        Daily ETH portfolio monitoring.
        """
        current_value = self.portfolio.total_portfolio_value
        daily_return = ((current_value - 1000) / 1000) * 100
        
        # Get ETH position details
        eth_position_value = 0
        eth_allocation = 0
        
        for security in self.securities.values():
            if 'ETH' in str(security.symbol) and security.invested:
                eth_position_value = abs(security.holdings.holdings_value)
                eth_allocation = (eth_position_value / current_value * 100) if current_value > 0 else 0
                break
        
        self.debug(f"🟡 Daily ETH Update:")
        self.debug(f"   💼 Portfolio: ${current_value:,.2f} ({daily_return:+.1f}%)")
        self.debug(f"   💎 ETH Allocation: {eth_allocation:.1f}%")
        self.debug(f"   📊 ETH Value: ${eth_position_value:,.2f}")
    
    def on_end_of_algorithm(self):
        """
        Final ETH algorithm statistics and performance analysis.
        """
        # Calculate final metrics
        final_value = self.portfolio.total_portfolio_value
        total_return = ((final_value - 1000) / 1000) * 100
        profit_loss = final_value - 1000
        
        self.debug("=" * 50)
        self.debug("🟡 ETH FRAMEWORK ALGORITHM COMPLETE")
        self.debug("=" * 50)
        
        # ETH Performance metrics
        self.debug("💎 ETH PERFORMANCE METRICS:")
        self.debug(f"   💰 Starting Capital: $1,000")
        self.debug(f"   💼 Final Portfolio Value: ${final_value:,.2f}")
        self.debug(f"   📈 Total Return: {total_return:+.1f}%")
        self.debug(f"   💵 Profit/Loss: ${profit_loss:+,.2f}")
        self.debug(f"   🚀 Max Value Reached: ${self.max_portfolio_value:,.2f}")
        self.debug(f"   📉 Min Value Reached: ${self.min_portfolio_value:,.2f}")
        
        # ETH Trading activity
        self.debug("\n⚡ ETH TRADING ACTIVITY:")
        self.debug(f"   🔮 ETH Insights Generated: {self.eth_insights}")
        self.debug(f"   📊 Total ETH Trades: {self.eth_trades}")
        self.debug(f"   📈 ETH Buys: {self.eth_buy_count}")
        self.debug(f"   📉 ETH Sells: {self.eth_sell_count}")
        
        # Calculate some additional metrics
        max_drawdown = ((self.max_portfolio_value - self.min_portfolio_value) / self.max_portfolio_value * 100) if self.max_portfolio_value > 0 else 0
        
        self.debug(f"\n📊 RISK METRICS:")
        self.debug(f"   📉 Maximum Drawdown: {max_drawdown:.1f}%")
        
        # Framework benefits for ETH trading
        self.debug("\n🎯 ETH FRAMEWORK BENEFITS:")
        self.debug("   ✅ Clean ETH technical analysis: SMA + RSI + Bollinger")
        self.debug("   ✅ Focused 95% ETH allocation strategy")
        self.debug("   ✅ Crypto-specific risk management")
        self.debug("   ✅ Modular design: Easy to test different indicators")
        self.debug("   ✅ Professional execution: Framework handles all orders")
        
        # Performance evaluation
        if total_return > 0:
            self.debug("\n🏆 RESULT: Profitable ETH strategy")
        elif total_return > -5:
            self.debug("\n⚖️ RESULT: Conservative performance within risk limits")
        else:
            self.debug("\n⚠️ RESULT: Strategy needs optimization")
        
        self.debug("=" * 50)
        self.debug("🦄 UNICORN INVESTING - ETH FRAMEWORK")
        self.debug("=" * 50)
