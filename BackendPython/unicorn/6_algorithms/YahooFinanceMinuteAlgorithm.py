"""
Yahoo Finance Minute Data Algorithm
===================================

Example algorithm showing how to use Yahoo Finance as a free data source
for minute-level trading data in the LEAN framework.

Advantages of Yahoo Finance:
- No API key required
- Free unlimited access (with rate limits)
- Good coverage of US stocks, ETFs, major forex pairs, crypto
- Real-time and historical data
- Reliable and fast

Strategy:
- Uses minute-level data from Yahoo Finance
- Implements simple momentum strategy
- Trades multiple assets (stocks, ETF, forex)
- Includes risk management
"""

from AlgorithmImports import *
from datetime import datetime, timedelta
import sys
import os

# Add the data sources directory to the path
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'data_sources'))

try:
    from YahooFinanceMinuteData import YahooFinanceMinuteData, YahooFinanceETFData, YahooFinanceForexData
except ImportError:
    # Fallback if import fails
    class YahooFinanceMinuteData(PythonData):
        pass
    class YahooFinanceETFData(PythonData):
        pass
    class YahooFinanceForexData(PythonData):
        pass


class YahooFinanceMinuteAlgorithm(QCAlgorithm):
    """
    Algorithm demonstrating Yahoo Finance minute data integration.
    
    Features:
    - Multiple asset classes (stocks, ETFs, forex)
    - Minute-level momentum strategy
    - Risk management with position sizing
    - Yahoo Finance as free data source
    """
    
    def initialize(self):
        """Initialize algorithm with Yahoo Finance data sources."""
        
        # Set algorithm parameters
        self.set_start_date(2024, 1, 1)
        self.set_end_date(2024, 12, 31)
        self.set_cash(100000)
        
        # Enable automatic handling of splits and dividends
        self.universe_settings.resolution = Resolution.MINUTE
        self.universe_settings.data_normalization_mode = DataNormalizationMode.ADJUSTED
        
        # Yahoo Finance symbols to trade
        self.symbols = {}
        
        # Add stocks using Yahoo Finance data
        try:
            # Major tech stocks
            self.symbols['AAPL'] = self.add_data(YahooFinanceMinuteData, "AAPL", Resolution.MINUTE).symbol
            self.symbols['MSFT'] = self.add_data(YahooFinanceMinuteData, "MSFT", Resolution.MINUTE).symbol
            self.symbols['GOOGL'] = self.add_data(YahooFinanceMinuteData, "GOOGL", Resolution.MINUTE).symbol
            
            # ETF
            self.symbols['SPY'] = self.add_data(YahooFinanceETFData, "SPY", Resolution.MINUTE).symbol
            
            # Forex (Yahoo format: EURUSD=X)
            self.symbols['EURUSD'] = self.add_data(YahooFinanceForexData, "EURUSD", Resolution.MINUTE).symbol
            
            self.debug("Successfully added Yahoo Finance data sources")
            
        except Exception as e:
            # Fallback to regular equity data if custom data fails
            self.debug(f"Yahoo Finance data not available, using default data: {e}")
            self.symbols['AAPL'] = self.add_equity("AAPL", Resolution.MINUTE).symbol
            self.symbols['MSFT'] = self.add_equity("MSFT", Resolution.MINUTE).symbol
            self.symbols['SPY'] = self.add_equity("SPY", Resolution.MINUTE).symbol
        
        # Algorithm state
        self.lookback_minutes = 20  # Momentum lookback period
        self.momentum_threshold = 0.002  # 0.2% momentum threshold
        self.position_size = 0.15  # 15% of portfolio per position
        
        # Tracking variables
        self.last_rebalance = datetime.min
        self.rebalance_frequency = timedelta(minutes=5)  # Rebalance every 5 minutes
        
        # Risk management
        self.max_positions = 3
        self.stop_loss_percent = 0.02  # 2% stop loss
        
        # Performance tracking
        self.trade_count = 0
        self.yahoo_data_points = 0
        
        self.debug("Yahoo Finance Algorithm initialized successfully")
    
    def on_data(self, data):
        """Process incoming Yahoo Finance minute data."""
        
        # Count data points for monitoring
        for symbol in self.symbols.values():
            if data.contains_key(symbol):
                self.yahoo_data_points += 1
        
        # Check if it's time to rebalance
        if self.time - self.last_rebalance < self.rebalance_frequency:
            return
        
        # Get current prices and calculate momentum
        momentum_scores = {}
        
        for name, symbol in self.symbols.items():
            if not data.contains_key(symbol):
                continue
            
            # Get historical data for momentum calculation
            history = self.history(symbol, self.lookback_minutes, Resolution.MINUTE)
            
            if history.empty or len(history) < self.lookback_minutes:
                continue
            
            # Calculate momentum (price change over lookback period)
            if 'close' in history.columns:
                start_price = history['close'].iloc[0]
                current_price = history['close'].iloc[-1]
            else:
                # For custom data, use 'value' column
                start_price = history['value'].iloc[0] if 'value' in history.columns else data[symbol].value
                current_price = data[symbol].value
            
            if start_price > 0:
                momentum = (current_price - start_price) / start_price
                momentum_scores[symbol] = {
                    'momentum': momentum,
                    'price': current_price,
                    'name': name
                }
                
                self.debug(f"Yahoo Finance {name}: Price=${current_price:.2f}, "
                          f"Momentum={momentum:.4f} ({momentum*100:.2f}%)")
        
        # Execute trading logic
        self.execute_momentum_strategy(momentum_scores)
        self.last_rebalance = self.time
    
    def execute_momentum_strategy(self, momentum_scores):
        """
        Execute momentum-based trading strategy using Yahoo Finance data.
        
        Strategy:
        1. Buy assets with positive momentum above threshold
        2. Sell assets with negative momentum below threshold
        3. Limit total positions for risk management
        """
        
        # Sort by momentum (highest first)
        sorted_momentum = sorted(momentum_scores.items(), 
                               key=lambda x: x[1]['momentum'], reverse=True)
        
        # Current positions
        current_positions = [symbol for symbol in self.symbols.values() 
                           if self.portfolio[symbol].invested]
        
        # Buy signals (positive momentum above threshold)
        buy_candidates = [
            (symbol, data) for symbol, data in sorted_momentum
            if data['momentum'] > self.momentum_threshold 
            and not self.portfolio[symbol].invested
        ]
        
        # Sell signals (negative momentum or stop loss)
        sell_candidates = []
        for symbol in current_positions:
            if symbol in dict(sorted_momentum):
                momentum_data = dict(sorted_momentum)[symbol]
                current_momentum = momentum_data['momentum']
                
                # Sell if momentum turned negative or stop loss hit
                position = self.portfolio[symbol]
                unrealized_pnl_percent = position.unrealized_profit_percent
                
                if (current_momentum < -self.momentum_threshold or 
                    unrealized_pnl_percent < -self.stop_loss_percent):
                    sell_candidates.append((symbol, momentum_data))
        
        # Execute sells first
        for symbol, data in sell_candidates:
            if self.portfolio[symbol].invested:
                self.liquidate(symbol)
                self.trade_count += 1
                self.debug(f"SELL {data['name']}: Momentum={data['momentum']:.4f}, "
                          f"Price=${data['price']:.2f}")
        
        # Execute buys (limit to max positions)
        positions_to_add = min(len(buy_candidates), 
                              self.max_positions - len(current_positions) + len(sell_candidates))
        
        for i in range(positions_to_add):
            if i < len(buy_candidates):
                symbol, data = buy_candidates[i]
                
                # Calculate position size
                target_value = self.portfolio.total_portfolio_value * self.position_size
                
                if data['price'] > 0:
                    quantity = int(target_value / data['price'])
                    
                    if quantity > 0:
                        self.market_order(symbol, quantity)
                        self.trade_count += 1
                        self.debug(f"BUY {data['name']}: Quantity={quantity}, "
                                  f"Momentum={data['momentum']:.4f}, "
                                  f"Price=${data['price']:.2f}")
    
    def on_end_of_algorithm(self):
        """Algorithm completion summary."""
        
        self.debug("=" * 50)
        self.debug("YAHOO FINANCE ALGORITHM SUMMARY")
        self.debug("=" * 50)
        self.debug(f"Total Portfolio Value: ${self.portfolio.total_portfolio_value:,.2f}")
        self.debug(f"Total Return: {(self.portfolio.total_portfolio_value / 100000 - 1)*100:.2f}%")
        self.debug(f"Total Trades: {self.trade_count}")
        self.debug(f"Yahoo Data Points Processed: {self.yahoo_data_points}")
        
        # Final positions
        self.debug("\nFinal Positions:")
        for name, symbol in self.symbols.items():
            if self.portfolio[symbol].invested:
                position = self.portfolio[symbol]
                self.debug(f"{name}: Quantity={position.quantity}, "
                          f"Value=${position.holdings_value:,.2f}, "
                          f"P&L=${position.unrealized_profit:,.2f}")
        
        self.debug("=" * 50)


class YahooFinanceTestAlgorithm(QCAlgorithm):
    """
    Simple test algorithm to verify Yahoo Finance data connectivity.
    """
    
    def initialize(self):
        """Initialize with minimal Yahoo Finance setup."""
        
        self.set_start_date(2024, 8, 1)
        self.set_end_date(2024, 8, 27)
        self.set_cash(10000)
        
        # Test single Yahoo Finance stock
        try:
            self.aapl = self.add_data(YahooFinanceMinuteData, "AAPL", Resolution.MINUTE).symbol
            self.debug("Yahoo Finance AAPL data source added successfully")
            self.yahoo_enabled = True
        except Exception as e:
            self.debug(f"Yahoo Finance not available, using default: {e}")
            self.aapl = self.add_equity("AAPL", Resolution.MINUTE).symbol
            self.yahoo_enabled = False
        
        self.data_count = 0
        self.last_price = 0
    
    def on_data(self, data):
        """Test Yahoo Finance data reception."""
        
        if data.contains_key(self.aapl):
            self.data_count += 1
            current_price = data[self.aapl].value
            
            # Log every 60 data points (approximately hourly)
            if self.data_count % 60 == 0:
                source_type = "Yahoo Finance" if self.yahoo_enabled else "Default"
                self.debug(f"{source_type} AAPL Data Point #{self.data_count}: "
                          f"Time={self.time}, Price=${current_price:.2f}")
            
            self.last_price = current_price
    
    def on_end_of_algorithm(self):
        """Test completion summary."""
        source_type = "Yahoo Finance" if self.yahoo_enabled else "Default"
        self.debug(f"{source_type} Test Complete: {self.data_count} data points, "
                  f"Final Price: ${self.last_price:.2f}")


# Entry point for testing
if __name__ == "__main__":
    # This would be used for local testing
    print("Yahoo Finance LEAN Algorithm")
    print("Run this algorithm in LEAN framework for live execution")
    print("Use YahooFinanceTestAlgorithm for simple connectivity testing")
