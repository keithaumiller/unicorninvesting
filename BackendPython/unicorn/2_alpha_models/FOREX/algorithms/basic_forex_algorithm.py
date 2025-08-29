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
### Basic "Hello World" Forex Trading Algorithm for Unicorn Investing Platform
### Demonstrates basic forex trading with the specified currency pairs
### </summary>
### <meta name="tag" content="forex" />
### <meta name="tag" content="unicorn" />
### <meta name="tag" content="hello world" />
class UnicornBasicForexAlgorithm(QCAlgorithm):

    def initialize(self):
        """Initialize the algorithm with basic settings and forex pairs"""
        
        # Set the cash we'd like to use for our backtest
        self.set_cash(100000)  # $100,000 starting capital

        # Start and end dates for the backtest
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2023, 12, 31)

        # Set timezone for forex trading
        self.set_time_zone("UTC")

        # Add the forex pairs you specified
        # Note: LEAN uses specific symbol conventions for forex
        self.eurusd = self.add_forex("EURUSD", Resolution.HOUR).symbol
        self.usdjpy = self.add_forex("USDJPY", Resolution.HOUR).symbol
        self.usdcnh = self.add_forex("USDCNH", Resolution.HOUR).symbol  # USD/CNY (offshore)
        
        # Add ETH as crypto (Ethereum)
        self.ethusd = self.add_crypto("ETHUSD", Resolution.HOUR).symbol

        # Store forex symbols for easy access
        self.forex_symbols = [self.eurusd, self.usdjpy, self.usdcnh]
        self.crypto_symbols = [self.ethusd]
        
        # Simple moving average indicators for basic strategy
        self.sma_fast = {}
        self.sma_slow = {}
        
        # Create indicators for each symbol
        for symbol in self.forex_symbols + self.crypto_symbols:
            self.sma_fast[symbol] = self.sma(symbol, 20, Resolution.HOUR)  # 20-hour fast SMA
            self.sma_slow[symbol] = self.sma(symbol, 50, Resolution.HOUR)  # 50-hour slow SMA

        # Track last trade time to avoid over-trading
        self.last_trade_time = {}
        
        # Log initialization
        self.log("🦄 Unicorn Forex Algorithm Initialized!")
        self.log(f"💰 Starting Cash: ${self.portfolio.cash:,.2f}")
        self.log(f"📊 Trading: {len(self.forex_symbols)} Forex + {len(self.crypto_symbols)} Crypto pairs")

    def on_data(self, data):
        """Handle incoming market data and execute trading logic"""
        
        # Check if we have data for our symbols
        if not all(symbol in data for symbol in self.forex_symbols + self.crypto_symbols):
            return

        # Simple "Hello World" trading logic
        for symbol in self.forex_symbols + self.crypto_symbols:
            # Skip if we don't have enough data for indicators
            if not (self.sma_fast[symbol].is_ready and self.sma_slow[symbol].is_ready):
                continue
                
            # Get current price
            current_price = data[symbol].close
            fast_sma = self.sma_fast[symbol].current.value
            slow_sma = self.sma_slow[symbol].current.value
            
            # Check if we should trade (avoid over-trading)
            if symbol in self.last_trade_time:
                if self.time - self.last_trade_time[symbol] < timedelta(hours=4):
                    continue
            
            # Simple moving average crossover strategy
            if fast_sma > slow_sma and not self.portfolio[symbol].is_long:
                # Golden cross - buy signal
                self.set_holdings(symbol, 0.1)  # 10% of portfolio
                self.last_trade_time[symbol] = self.time
                self.log(f"🚀 BUY Signal: {symbol} at {current_price:.4f} (SMA Fast: {fast_sma:.4f} > SMA Slow: {slow_sma:.4f})")
                
            elif fast_sma < slow_sma and self.portfolio[symbol].is_long:
                # Death cross - sell signal
                self.liquidate(symbol)
                self.last_trade_time[symbol] = self.time
                self.log(f"🔻 SELL Signal: {symbol} at {current_price:.4f} (SMA Fast: {fast_sma:.4f} < SMA Slow: {slow_sma:.4f})")

        # Log portfolio value periodically
        if self.time.hour == 0 and self.time.minute == 0:  # Once per day
            self.log(f"📈 Portfolio Value: ${self.portfolio.total_portfolio_value:,.2f}")

    def on_order_event(self, order_event):
        """Handle order events (fills, cancellations, etc.)"""
        if order_event.status == OrderStatus.FILLED:
            self.log(f"✅ Order Filled: {order_event.symbol} - {order_event.direction} - Quantity: {order_event.fill_quantity} - Price: ${order_event.fill_price:.4f}")

    def on_end_of_algorithm(self):
        """Called at the end of the algorithm"""
        final_value = self.portfolio.total_portfolio_value
        total_return = (final_value - 100000) / 100000 * 100
        
        self.log("🏁 Algorithm Completed!")
        self.log(f"💰 Final Portfolio Value: ${final_value:,.2f}")
        self.log(f"📊 Total Return: {total_return:.2f}%")
        
        # Log holdings summary
        self.log("📋 Final Holdings:")
        for symbol in self.forex_symbols + self.crypto_symbols:
            if self.portfolio[symbol].invested:
                holding = self.portfolio[symbol]
                self.log(f"   {symbol}: {holding.quantity:.4f} units @ ${holding.average_price:.4f}")
