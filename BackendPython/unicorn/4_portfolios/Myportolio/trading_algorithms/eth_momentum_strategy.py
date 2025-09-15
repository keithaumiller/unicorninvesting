"""
ETH Momentum Trading Strategy with Enhanced Performance Logging

Simple momentum-based trading strategy for ETH using Python backtesting framework
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging
import sys
from pathlib import Path

# Python Framework integration (standalone)
try:
    # Optional future framework integration
    FRAMEWORK_AVAILABLE = False
except ImportError:
    FRAMEWORK_AVAILABLE = False
    logging.info("Running in standalone Python mode")

# Performance logging integration
try:
    sys.path.append(str(Path(__file__).parent.parent / "simulations"))
    from performance_logger import PerformanceLogger
    PERFORMANCE_LOGGING_AVAILABLE = True
except ImportError:
    PERFORMANCE_LOGGING_AVAILABLE = False
    logging.warning("Performance logging not available")

class ETHMomentumStrategy:
    """
    Enhanced ETH momentum trading strategy with comprehensive performance logging
    
    Strategy Logic:
    - Buy when 5-day MA > 20-day MA (bullish momentum)
    - Sell when 5-day MA < 20-day MA (bearish momentum)
    - Position sizing based on volatility
    - Detailed logging of all trading decisions and rationale
    """
    
    def __init__(self, config: Dict, performance_logger: 'PerformanceLogger' = None):
        """
        Initialize ETH momentum strategy with performance logging
        
        Args:
            config: Strategy configuration dictionary
            performance_logger: Performance logger instance for detailed tracking
        """
        self.config = config
        self.symbol = "ETHUSD"
        self.short_window = config.get('short_ma_period', 5)
        self.long_window = config.get('long_ma_period', 20)
        self.max_position_size = config.get('max_position_size', 0.1)  # 10% of portfolio
        self.volatility_window = config.get('volatility_window', 14)
        
        # Performance logging
        self.performance_logger = performance_logger
        self.log_enabled = performance_logger is not None
        
        # Strategy state
        self.current_position = 0.0
        self.last_signal = None
        self.price_history = []
        self.signal_history = []
        
        # Performance tracking
        self.total_signals_generated = 0
        self.buy_signals = 0
        self.sell_signals = 0
        self.hold_signals = 0
        
        logging.info(f"ETH Momentum Strategy initialized: {self.short_window}/{self.long_window} MA crossover")
        logging.info(f"Performance logging: {'ENABLED' if self.log_enabled else 'DISABLED'}")
    
    def calculate_moving_averages(self, prices: pd.Series) -> Tuple[float, float]:
        """
        Calculate short and long term moving averages with logging
        
        Args:
            prices: Historical price series
            
        Returns:
            Tuple of (short_ma, long_ma)
        """
        if len(prices) < self.long_window:
            if self.log_enabled:
                self.performance_logger.logger.debug(f"Insufficient data for MA calculation: {len(prices)} < {self.long_window}")
            return None, None
            
        short_ma = prices.tail(self.short_window).mean()
        long_ma = prices.tail(self.long_window).mean()
        
        if self.log_enabled:
            self.performance_logger.logger.debug(f"MA calculated: short={short_ma:.2f}, long={long_ma:.2f}")
        
        return short_ma, long_ma
    
    def calculate_volatility(self, prices: pd.Series) -> float:
        """
        Calculate price volatility for position sizing with detailed logging
        
        Args:
            prices: Historical price series
            
        Returns:
            Volatility measure (standard deviation of returns)
        """
        if len(prices) < self.volatility_window:
            default_vol = 0.02
            if self.log_enabled:
                self.performance_logger.logger.debug(f"Using default volatility {default_vol:.3f} due to insufficient data")
            return default_vol
            
        returns = prices.pct_change().dropna()
        volatility = returns.tail(self.volatility_window).std()
        
        if volatility <= 0:
            volatility = 0.02
            
        if self.log_enabled:
            self.performance_logger.logger.debug(f"Volatility calculated: {volatility:.4f} over {self.volatility_window} periods")
        
        return volatility
    
    def generate_signal(self, market_data: pd.DataFrame) -> Dict:
        """
        Generate trading signal with comprehensive logging and analysis
        
        Args:
            market_data: DataFrame with OHLCV data
            
        Returns:
            Dictionary with detailed signal information
        """
        self.total_signals_generated += 1
        current_price = market_data['close'].iloc[-1] if not market_data.empty else 0.0
        
        # Check for insufficient data
        if market_data.empty or len(market_data) < self.long_window:
            signal_result = {
                'signal': 'HOLD',
                'confidence': 0.0,
                'target_position': self.current_position,
                'reason': 'Insufficient data',
                'current_price': current_price,
                'short_ma': None,
                'long_ma': None,
                'volatility': None
            }
            
            self.hold_signals += 1
            
            if self.log_enabled:
                self.performance_logger.log_trading_signal(
                    asset=self.symbol,
                    signal_type='HOLD',
                    confidence=0.0,
                    current_price=current_price,
                    target_position=self.current_position,
                    current_position=self.current_position,
                    signal_reason='Insufficient market data for analysis',
                    technical_indicators={}
                )
            
            return signal_result
        
        prices = market_data['close']
        short_ma, long_ma = self.calculate_moving_averages(prices)
        
        if short_ma is None or long_ma is None:
            signal_result = {
                'signal': 'HOLD',
                'confidence': 0.0,
                'target_position': self.current_position,
                'reason': 'Insufficient data for MA calculation',
                'current_price': current_price,
                'short_ma': None,
                'long_ma': None,
                'volatility': None
            }
            
            self.hold_signals += 1
            
            if self.log_enabled:
                self.performance_logger.log_trading_signal(
                    asset=self.symbol,
                    signal_type='HOLD',
                    confidence=0.0,
                    current_price=current_price,
                    target_position=self.current_position,
                    current_position=self.current_position,
                    signal_reason='Unable to calculate moving averages',
                    technical_indicators={}
                )
            
            return signal_result
        
        # Calculate technical indicators
        volatility = self.calculate_volatility(prices)
        ma_spread = abs(short_ma - long_ma) / long_ma
        
        # Generate signal based on MA crossover with detailed analysis
        if short_ma > long_ma:
            signal = 'BUY'
            confidence = min(ma_spread * 10, 1.0)  # Scale confidence based on MA spread
            signal_reason = f"Bullish momentum: {self.short_window}MA ({short_ma:.2f}) > {self.long_window}MA ({long_ma:.2f})"
            self.buy_signals += 1
        elif short_ma < long_ma:
            signal = 'SELL'
            confidence = min(ma_spread * 10, 1.0)
            signal_reason = f"Bearish momentum: {self.short_window}MA ({short_ma:.2f}) < {self.long_window}MA ({long_ma:.2f})"
            self.sell_signals += 1
        else:
            signal = 'HOLD'
            confidence = 0.0
            signal_reason = f"Neutral momentum: {self.short_window}MA ≈ {self.long_window}MA"
            self.hold_signals += 1
        
        # Calculate position size with volatility adjustment
        volatility_adj = min(0.02 / volatility, 1.0) if volatility > 0 else 1.0
        
        if signal == 'BUY':
            target_position = self.max_position_size * volatility_adj * confidence
        elif signal == 'SELL':
            target_position = 0.0
        else:
            target_position = self.current_position
        
        # Prepare technical indicators for logging
        technical_indicators = {
            'short_ma': short_ma,
            'long_ma': long_ma,
            'ma_spread': ma_spread,
            'volatility': volatility,
            'volatility_adjustment': volatility_adj,
            'price_momentum': (current_price / prices.tail(5).mean() - 1) if len(prices) >= 5 else 0
        }
        
        # Log the trading signal with comprehensive details
        if self.log_enabled:
            self.performance_logger.log_trading_signal(
                asset=self.symbol,
                signal_type=signal,
                confidence=confidence,
                current_price=current_price,
                target_position=target_position,
                current_position=self.current_position,
                signal_reason=signal_reason,
                technical_indicators=technical_indicators
            )
        
        # Store signal in history for analysis
        signal_data = {
            'timestamp': datetime.now().isoformat(),
            'signal': signal,
            'confidence': confidence,
            'price': current_price,
            'short_ma': short_ma,
            'long_ma': long_ma,
            'volatility': volatility
        }
        self.signal_history.append(signal_data)
        
        # Keep only last 100 signals in memory
        if len(self.signal_history) > 100:
            self.signal_history = self.signal_history[-100:]
        
        return {
            'signal': signal,
            'confidence': confidence,
            'target_position': target_position,
            'current_position': self.current_position,
            'reason': signal_reason,
            'current_price': current_price,
            'short_ma': short_ma,
            'long_ma': long_ma,
            'volatility': volatility,
            'technical_indicators': technical_indicators
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

# Removed LEAN Framework implementation section
# This strategy now runs exclusively in standalone Python mode

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
