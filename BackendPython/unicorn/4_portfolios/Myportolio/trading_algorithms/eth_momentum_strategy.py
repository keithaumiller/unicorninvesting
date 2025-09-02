"""
ETH Trading Algorithm - Hello World Implementation
Simple momentum-based trading strategy for ETH using LEAN framework integration
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging

# LEAN Framework integration (when available)
try:
    from QuantConnect import *
    from QuantConnect.Algorithm import *
    from QuantConnect.Data import *
    LEAN_AVAILABLE = True
except ImportError:
    LEAN_AVAILABLE = False
    logging.info("LEAN framework not available - running in standalone mode")

class ETHMomentumStrategy:
    """
    Simple ETH momentum trading strategy
    
    Strategy Logic:
    - Buy when 5-day MA > 20-day MA (bullish momentum)
    - Sell when 5-day MA < 20-day MA (bearish momentum)
    - Position sizing based on volatility
    """
    
    def __init__(self, config: Dict):
        """
        Initialize ETH momentum strategy
        
        Args:
            config: Strategy configuration dictionary
        """
        self.config = config
        self.symbol = "ETHUSD"
        self.short_window = config.get('short_ma_period', 5)
        self.long_window = config.get('long_ma_period', 20)
        self.max_position_size = config.get('max_position_size', 0.1)  # 10% of portfolio
        self.volatility_window = config.get('volatility_window', 14)
        
        # Strategy state
        self.current_position = 0.0
        self.last_signal = None
        self.price_history = []
        
        logging.info(f"ETH Momentum Strategy initialized: {self.short_window}/{self.long_window} MA crossover")
    
    def calculate_moving_averages(self, prices: pd.Series) -> Tuple[float, float]:
        """
        Calculate short and long term moving averages
        
        Args:
            prices: Historical price series
            
        Returns:
            Tuple of (short_ma, long_ma)
        """
        if len(prices) < self.long_window:
            return None, None
            
        short_ma = prices.tail(self.short_window).mean()
        long_ma = prices.tail(self.long_window).mean()
        
        return short_ma, long_ma
    
    def calculate_volatility(self, prices: pd.Series) -> float:
        """
        Calculate price volatility for position sizing
        
        Args:
            prices: Historical price series
            
        Returns:
            Volatility measure (standard deviation of returns)
        """
        if len(prices) < self.volatility_window:
            return 0.02  # Default 2% volatility
            
        returns = prices.pct_change().dropna()
        volatility = returns.tail(self.volatility_window).std()
        
        return volatility if volatility > 0 else 0.02
    
    def generate_signal(self, market_data: pd.DataFrame) -> Dict:
        """
        Generate trading signal based on momentum strategy
        
        Args:
            market_data: DataFrame with OHLCV data
            
        Returns:
            Dictionary with signal information
        """
        if market_data.empty or len(market_data) < self.long_window:
            return {
                'signal': 'HOLD',
                'confidence': 0.0,
                'target_position': self.current_position,
                'reason': 'Insufficient data'
            }
        
        prices = market_data['close']
        short_ma, long_ma = self.calculate_moving_averages(prices)
        
        if short_ma is None or long_ma is None:
            return {
                'signal': 'HOLD',
                'confidence': 0.0,
                'target_position': self.current_position,
                'reason': 'Insufficient data for MA calculation'
            }
        
        # Generate signal based on MA crossover
        if short_ma > long_ma:
            signal = 'BUY'
            confidence = min((short_ma - long_ma) / long_ma * 10, 1.0)  # Scale confidence
        elif short_ma < long_ma:
            signal = 'SELL'
            confidence = min((long_ma - short_ma) / long_ma * 10, 1.0)
        else:
            signal = 'HOLD'
            confidence = 0.0
        
        # Calculate position size based on volatility
        volatility = self.calculate_volatility(prices)
        volatility_adj = min(0.02 / volatility, 1.0) if volatility > 0 else 1.0
        
        if signal == 'BUY':
            target_position = self.max_position_size * volatility_adj * confidence
        elif signal == 'SELL':
            target_position = 0.0
        else:
            target_position = self.current_position
        
        return {
            'signal': signal,
            'confidence': confidence,
            'target_position': target_position,
            'current_position': self.current_position,
            'short_ma': short_ma,
            'long_ma': long_ma,
            'volatility': volatility,
            'reason': f"MA Crossover: {short_ma:.2f} vs {long_ma:.2f}"
        }
    
    def update_position(self, new_position: float):
        """Update current position"""
        self.current_position = new_position
    
    def get_strategy_stats(self) -> Dict:
        """Get current strategy statistics"""
        return {
            'strategy_name': 'ETH_Momentum',
            'symbol': self.symbol,
            'current_position': self.current_position,
            'short_window': self.short_window,
            'long_window': self.long_window,
            'max_position_size': self.max_position_size
        }

# LEAN Framework Algorithm Class
if LEAN_AVAILABLE:
    class ETHMomentumLeanAlgorithm(QCAlgorithm):
        """
        LEAN Framework implementation of ETH Momentum Strategy
        """
        
        def Initialize(self):
            """Initialize LEAN algorithm"""
            self.SetStartDate(2023, 1, 1)
            self.SetEndDate(2024, 12, 31)
            self.SetCash(100000)
            
            # Add ETH data
            self.eth = self.AddCrypto("ETHUSD", Resolution.Hour)
            
            # Initialize strategy
            config = {
                'short_ma_period': 5,
                'long_ma_period': 20,
                'max_position_size': 0.1,
                'volatility_window': 14
            }
            self.strategy = ETHMomentumStrategy(config)
            
            # Set up indicators
            self.short_ma = self.SMA(self.eth.Symbol, 5, Resolution.Hour)
            self.long_ma = self.SMA(self.eth.Symbol, 20, Resolution.Hour)
            
            self.Debug("ETH Momentum Algorithm Initialized")
        
        def OnData(self, data):
            """Process new market data"""
            if not data.ContainsKey(self.eth.Symbol):
                return
            
            if not self.short_ma.IsReady or not self.long_ma.IsReady:
                return
            
            # Create market data DataFrame for strategy
            market_data = pd.DataFrame({
                'close': [data[self.eth.Symbol].Close],
                'timestamp': [self.Time]
            })
            
            # Generate signal
            signal_data = self.strategy.generate_signal(market_data)
            
            # Execute trades based on signal
            if signal_data['signal'] == 'BUY' and signal_data['confidence'] > 0.6:
                target_quantity = signal_data['target_position'] * self.Portfolio.Cash / data[self.eth.Symbol].Close
                self.SetHoldings(self.eth.Symbol, signal_data['target_position'])
                self.Debug(f"BUY signal: {signal_data['reason']}, Confidence: {signal_data['confidence']:.2f}")
                
            elif signal_data['signal'] == 'SELL':
                self.Liquidate(self.eth.Symbol)
                self.Debug(f"SELL signal: {signal_data['reason']}")
            
            # Update strategy position
            current_holdings = self.Portfolio[self.eth.Symbol].Quantity * data[self.eth.Symbol].Close / self.Portfolio.TotalPortfolioValue
            self.strategy.update_position(current_holdings)

# Standalone testing function
def test_eth_momentum_strategy():
    """Test the ETH momentum strategy with sample data"""
    print("🚀 Testing ETH Momentum Strategy - Hello World")
    
    # Create sample ETH price data
    dates = pd.date_range(start='2024-01-01', end='2024-08-30', freq='D')
    np.random.seed(42)  # For reproducible results
    
    # Generate realistic ETH price movement
    initial_price = 2500.0
    returns = np.random.normal(0.001, 0.03, len(dates))  # 0.1% daily return, 3% volatility
    prices = [initial_price]
    
    for ret in returns[1:]:
        prices.append(prices[-1] * (1 + ret))
    
    market_data = pd.DataFrame({
        'timestamp': dates,
        'close': prices,
        'volume': np.random.randint(1000, 10000, len(dates))
    })
    
    # Initialize strategy
    config = {
        'short_ma_period': 5,
        'long_ma_period': 20,
        'max_position_size': 0.1,
        'volatility_window': 14
    }
    
    strategy = ETHMomentumStrategy(config)
    
    # Test signal generation
    signal = strategy.generate_signal(market_data)
    
    print(f"📊 Strategy Stats: {strategy.get_strategy_stats()}")
    print(f"📈 Signal Generated: {signal}")
    print(f"💰 Current ETH Price: ${market_data['close'].iloc[-1]:.2f}")
    print(f"📊 Price Range: ${market_data['close'].min():.2f} - ${market_data['close'].max():.2f}")
    
    return strategy, market_data, signal

if __name__ == "__main__":
    # Run standalone test
    test_eth_momentum_strategy()
