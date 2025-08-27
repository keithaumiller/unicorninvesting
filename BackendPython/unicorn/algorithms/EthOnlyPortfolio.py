# QUANTCONNECT.COM - Democratizing Finance, Empowering Individuals.
# Lean Algorithmic Trading Engine v2.0. Copyright 2014 QuantConnect Corporation.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from AlgorithmImports import *

### <summary>
### Unicorn Investing Platform - ETH Only Portfolio Algorithm
### Single cryptocurrency portfolio focused on Ethereum with $1,000 capital
### Follows established unicorn platform patterns and risk management
### </summary>
### <meta name="tag" content="crypto" />
### <meta name="tag" content="ethereum" />
### <meta name="tag" content="unicorn" />
class EthOnlyPortfolio(QCAlgorithm):

    def initialize(self):
        """Initialize the ETH-only portfolio with established unicorn patterns"""
        
        # Set the cash we'd like to use for our backtest - $1,000 as requested
        self.set_cash(1000)  # $1,000 starting capital as specified

        # Start and end dates for the backtest (can be overridden in config)
        self.set_start_date(2024, 1, 1)
        self.set_end_date(2024, 8, 27)

        # Set timezone for crypto trading
        self.set_time_zone("UTC")

        # Add Ethereum - using Coinbase data (following existing pattern)
        self.ethusd = self.add_crypto("ETHUSD", Resolution.MINUTE, Market.COINBASE).symbol

        # Portfolio allocation settings (following unicorn risk management)
        self.target_allocation = 0.95  # 95% in ETH, 5% cash buffer (as in config)
        self.rebalance_threshold = 0.05  # Rebalance if allocation drifts 5%
        self.min_trade_amount = 10  # Minimum $10 trade size
        
        # Risk management (following established patterns)
        self.stop_loss_percent = 0.05  # 5% stop loss
        self.max_position_size = 0.95  # Max 95% of portfolio in ETH
        self.daily_loss_limit = 0.10  # Max 10% daily loss
        
        # Indicators for decision making (following forex algorithm pattern)
        self.sma_fast = self.sma(self.ethusd, 20, Resolution.MINUTE)  # 20-minute fast SMA
        self.sma_slow = self.sma(self.ethusd, 50, Resolution.MINUTE)  # 50-minute slow SMA
        self.rsi = self.rsi(self.ethusd, 14, Resolution.MINUTE)  # RSI indicator
        
        # Performance tracking
        self.initial_portfolio_value = 1000  # Track from starting value
        self.daily_high_water_mark = 1000
        self.last_rebalance_time = self.time
        self.last_trade_time = None  # Track to avoid over-trading
        
        # Log initialization (following unicorn logging pattern)
        self.log("🦄 Unicorn ETH-Only Portfolio Initialized!")
        self.log(f"💰 Starting Cash: ${self.portfolio.cash:,.2f}")
        self.log(f"🎯 Target ETH allocation: {self.target_allocation * 100:.1f}%")
        self.log(f"📊 Trading: ETHUSD on Coinbase")
        
        # Schedule rebalancing check (following established patterns)
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=4)),
            self.check_rebalancing
        )
        
        # Daily portfolio review
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.at(0, 0),  # Midnight UTC
            self.daily_portfolio_review
        )
    
    def on_data(self, data):
        """Handle incoming market data and execute trading logic"""
        
        # Ensure we have ETH data (following established pattern)
        if self.ethusd not in data:
            return
        
        # Get current ETH price
        current_price = data[self.ethusd].close
        
        # Risk management: Check daily loss limit (following unicorn patterns)
        current_value = self.portfolio.total_portfolio_value
        daily_return = (current_value - self.daily_high_water_mark) / self.daily_high_water_mark
        
        if daily_return < -self.daily_loss_limit:
            self.liquidate(self.ethusd)
            self.log(f"🚨 Daily loss limit exceeded: {daily_return * 100:.2f}%")
            return
        
        # Update high water mark
        if current_value > self.daily_high_water_mark:
            self.daily_high_water_mark = current_value
        
        # Skip if indicators aren't ready (following forex pattern)
        if not (self.sma_fast.is_ready and self.sma_slow.is_ready and self.rsi.is_ready):
            return
        
        # Check if we should trade (avoid over-trading, following forex pattern)
        if self.last_trade_time:
            if self.time - self.last_trade_time < timedelta(hours=1):  # 1 hour minimum between trades
                return
        
        # Get indicator values
        fast_sma = self.sma_fast.current.value
        slow_sma = self.sma_slow.current.value
        rsi_value = self.rsi.current.value
        
        # Check if we need to make initial purchase or are not invested
        if not self.portfolio[self.ethusd].invested:
            # Initial purchase logic (conservative entry)
            if fast_sma > slow_sma and rsi_value < 70:  # Not overbought
                self.initial_eth_purchase(current_price)
                
        else:
            # Monitor existing position for risk management
            self.monitor_position(current_price)
            
            # Simple rebalancing based on indicators (following established patterns)
            eth_holdings = self.portfolio[self.ethusd]
            
            # Stop loss check (following risk management patterns)
            if eth_holdings.unrealized_profit_percent < -self.stop_loss_percent:
                self.liquidate(self.ethusd)
                self.last_trade_time = self.time
                self.log(f"🛑 Stop loss triggered at ${current_price:.2f}")
                self.log(f"📉 Loss: {eth_holdings.unrealized_profit_percent * 100:.2f}%")

    def initial_eth_purchase(self, eth_price):
        """Make initial ETH purchase (following established patterns)"""
        
        available_cash = self.portfolio.cash
        target_investment = available_cash * self.target_allocation
        
        if target_investment >= self.min_trade_amount:
            # Use set_holdings for proper allocation (following unicorn patterns)
            self.set_holdings(self.ethusd, self.target_allocation)
            self.last_trade_time = self.time
            
            self.log(f"� Initial ETH purchase at ${eth_price:.2f}")
            self.log(f"💵 Target investment: ${target_investment:.2f} ({self.target_allocation * 100:.1f}%)")

    def check_rebalancing(self):
        """Check if portfolio needs rebalancing (following established patterns)"""
        
        if not self.portfolio[self.ethusd].invested:
            return
        
        # Calculate current allocation
        eth_value = self.portfolio[self.ethusd].holdings_value
        total_value = self.portfolio.total_portfolio_value
        current_allocation = eth_value / total_value if total_value > 0 else 0
        
        # Check if rebalancing is needed
        allocation_drift = abs(current_allocation - self.target_allocation)
        
        if allocation_drift > self.rebalance_threshold:
            self.rebalance_portfolio(current_allocation)

    def rebalance_portfolio(self, current_allocation):
        """Rebalance portfolio to target allocation (following established patterns)"""
        
        # Use set_holdings for proper rebalancing (following unicorn patterns)
        self.set_holdings(self.ethusd, self.target_allocation)
        self.last_rebalance_time = self.time
        
        self.log(f"� Rebalancing: {current_allocation * 100:.1f}% → {self.target_allocation * 100:.1f}% ETH")

    def monitor_position(self, eth_price):
        """Monitor ETH position for risk management (following established patterns)"""
        
        if not self.portfolio[self.ethusd].invested:
            return
        
        eth_holdings = self.portfolio[self.ethusd]
        
        # Log significant price movements (following forex logging patterns)
        if eth_holdings.unrealized_profit_percent > 0.10:  # 10% profit
            self.log(f"📈 ETH profit: {eth_holdings.unrealized_profit_percent * 100:.2f}% at ${eth_price:.2f}")
        elif eth_holdings.unrealized_profit_percent < -0.03:  # 3% loss warning
            self.log(f"📉 ETH loss: {eth_holdings.unrealized_profit_percent * 100:.2f}% at ${eth_price:.2f}")

    def daily_portfolio_review(self):
        """Daily portfolio performance review (following established patterns)"""
        
        current_value = self.portfolio.total_portfolio_value
        total_return = (current_value - self.initial_portfolio_value) / self.initial_portfolio_value
        
        # Get ETH position details (following forex summary pattern)
        if self.portfolio[self.ethusd].invested:
            eth_holdings = self.portfolio[self.ethusd]
            eth_quantity = eth_holdings.quantity
            eth_value = eth_holdings.holdings_value
            eth_percent = (eth_value / current_value) * 100 if current_value > 0 else 0
            
            self.log(f"📊 Daily Review - Portfolio Value: ${current_value:.2f}")
            self.log(f"📊 Total Return: {total_return * 100:.2f}%")
            self.log(f"📊 ETH Holdings: {eth_quantity:.6f} ETH (${eth_value:.2f})")
            self.log(f"📊 ETH Allocation: {eth_percent:.1f}%")
            self.log(f"📊 Available Cash: ${self.portfolio.cash:.2f}")
        else:
            self.log(f"📊 Daily Review - Portfolio Value: ${current_value:.2f} (100% Cash)")
        
        # Reset daily high water mark
        self.daily_high_water_mark = current_value

    def on_order_event(self, order_event):
        """Handle order events (following established patterns)"""
        if order_event.status == OrderStatus.FILLED:
            self.log(f"✅ Order Filled: {order_event.symbol} - {order_event.direction} - Quantity: {order_event.fill_quantity} - Price: ${order_event.fill_price:.4f}")

    def on_end_of_algorithm(self):
        """Called at the end of the algorithm (following established patterns)"""
        final_value = self.portfolio.total_portfolio_value
        total_return = (final_value - self.initial_portfolio_value) / self.initial_portfolio_value * 100
        
        self.log("🏁 ETH Portfolio Algorithm Completed!")
        self.log(f"💰 Initial Capital: ${self.initial_portfolio_value:.2f}")
        self.log(f"💰 Final Portfolio Value: ${final_value:.2f}")
        self.log(f"📊 Total Return: {total_return:.2f}%")
        
        # Log holdings summary (following forex pattern)
        if self.portfolio[self.ethusd].invested:
            eth_holdings = self.portfolio[self.ethusd]
            self.log("📋 Final Holdings:")
            self.log(f"   {self.ethusd}: {eth_holdings.quantity:.6f} units @ ${eth_holdings.average_price:.4f}")
        else:
            self.log("� Final Holdings: 100% Cash")
