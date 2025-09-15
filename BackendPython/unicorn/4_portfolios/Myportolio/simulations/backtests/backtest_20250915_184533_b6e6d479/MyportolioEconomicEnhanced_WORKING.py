
"""
Working ETH Momentum Algorithm - FIXED VERSION
"""

from clr import AddReference
AddReference("System")
AddReference("QuantConnect.Algorithm")
AddReference("QuantConnect.Common")

from System import *
from QuantConnect import *
from QuantConnect.Algorithm import *
from QuantConnect.Data import *

class MyportolioEconomicEnhanced(QCAlgorithm):
    """Working ETH momentum algorithm with actual trading logic."""
    
    def Initialize(self):
        """Initialize algorithm with working configuration."""
        
        # Set cash and dates
        self.SetCash(100000)
        
        # Add ETH crypto data
        self.eth = self.AddCrypto("ETHUSD", Resolution.Hour)
        self.eth_symbol = self.eth.Symbol
        
        # Strategy parameters
        self.ma_short = 5
        self.ma_long = 20
        self.kelly_fraction = 0.15  # Conservative for testing
        self.confidence_threshold = 0.30  # Lower threshold for testing
        
        # Risk management
        self.max_position_pct = 0.25
        self.max_drawdown = 0.15
        
        # Initialize indicators
        self.ma_short_indicator = self.SMA(self.eth_symbol, self.ma_short, Resolution.Hour)
        self.ma_long_indicator = self.SMA(self.eth_symbol, self.ma_long, Resolution.Hour)
        self.rsi_indicator = self.RSI(self.eth_symbol, 14, Resolution.Hour)
        
        # State tracking
        self.high_water_mark = 100000
        self.trade_count = 0
        self.last_signal = "HOLD"
        
        self.Log("Working ETH Algorithm Initialized with REAL trading logic")
    
    def OnData(self, data):
        """Handle new market data with ACTUAL trading logic."""
        
        # Ensure we have data and indicators are ready
        if not self.eth.HasData:
            return
            
        if not (self.ma_short_indicator.IsReady and self.ma_long_indicator.IsReady and self.rsi_indicator.IsReady):
            return
        
        # Get current market data
        eth_price = self.Securities[self.eth_symbol].Price
        if eth_price <= 0:
            return
        
        # Get indicator values
        ma_short = float(self.ma_short_indicator.Current.Value)
        ma_long = float(self.ma_long_indicator.Current.Value)
        rsi = float(self.rsi_indicator.Current.Value)
        
        # Generate trading signal
        signal = self._generate_signal(ma_short, ma_long, rsi)
        
        # Apply risk management
        if self._check_risk_limits():
            self._execute_trade(signal, eth_price)
    
    def _generate_signal(self, ma_short, ma_long, rsi):
        """Generate momentum signal."""
        
        if ma_long <= 0:
            return "HOLD"
        
        ma_ratio = ma_short / ma_long
        
        # Buy signal: short MA > long MA and RSI not overbought
        if ma_ratio > 1.01 and rsi < 75:
            strength = min((ma_ratio - 1.0) * 20, 1.0)
            if strength > self.confidence_threshold:
                return "BUY"
        
        # Sell signal: short MA < long MA and RSI not oversold  
        elif ma_ratio < 0.99 and rsi > 25:
            strength = min((1.0 - ma_ratio) * 20, 1.0) 
            if strength > self.confidence_threshold:
                return "SELL"
        
        return "HOLD"
    
    def _check_risk_limits(self):
        """Basic risk management."""
        
        portfolio_value = self.Portfolio.TotalPortfolioValue
        
        # Update high water mark
        if portfolio_value > self.high_water_mark:
            self.high_water_mark = portfolio_value
        
        # Check drawdown
        current_drawdown = (self.high_water_mark - portfolio_value) / self.high_water_mark
        if current_drawdown > self.max_drawdown:
            return False
        
        return True
    
    def _execute_trade(self, signal, price):
        """Execute trade based on signal."""
        
        if signal == self.last_signal:
            return
        
        portfolio_value = self.Portfolio.TotalPortfolioValue
        current_holdings = self.Portfolio[self.eth_symbol].Quantity
        
        if signal == "BUY" and current_holdings <= 0:
            # Calculate position size using Kelly fraction
            position_value = portfolio_value * self.kelly_fraction
            shares_to_buy = int(position_value / price)
            
            if shares_to_buy > 0:
                self.MarketOrder(self.eth_symbol, shares_to_buy)
                self.trade_count += 1
                self.Log("BUY " + str(shares_to_buy) + " ETH at $" + str(round(price, 2)))
        
        elif signal == "SELL" and current_holdings > 0:
            # Sell all holdings
            self.MarketOrder(self.eth_symbol, -current_holdings)
            self.trade_count += 1
            self.Log("SELL " + str(current_holdings) + " ETH at $" + str(round(price, 2)))
        
        self.last_signal = signal
    
    def OnEndOfAlgorithm(self):
        """Log final results."""
        final_value = self.Portfolio.TotalPortfolioValue
        total_return = (final_value / 100000) - 1
        
        self.Log("FINAL RESULTS:")
        self.Log("Total Return: " + str(round(total_return * 100, 2)) + "%")
        self.Log("Total Trades: " + str(self.trade_count))
        self.Log("Final Value: $" + str(round(final_value, 2)))
