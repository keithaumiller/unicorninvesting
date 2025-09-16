"""
BTC Momentum Trading Strategy with Enhanced Performance Logging

Simple momentum-based trading strategy for BTC using Python backtesting framework
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

class BTCMomentumStrategy:
    """
    Enhanced BTC momentum trading strategy with comprehensive performance logging
    
    Strategy Logic:
    - Buy when 5-day MA > 20-day MA (bullish momentum)
    - Sell when 5-day MA < 20-day MA (bearish momentum)
    - Position sizing based on volatility and Kelly criterion
    - Detailed logging of all trading decisions and rationale
    """
    
    def __init__(self, config: Dict, performance_logger: 'PerformanceLogger' = None):
        """
        Initialize BTC momentum strategy with performance logging
        
        Args:
            config: Strategy configuration dictionary
            performance_logger: Performance logger instance for detailed tracking
        """
        self.config = config
        self.performance_logger = performance_logger
        
        # Strategy parameters
        self.symbol = config.get('symbol', 'BTCUSD')
        self.short_ma_period = config.get('short_ma_period', 5)
        self.long_ma_period = config.get('long_ma_period', 20)
        self.volatility_window = config.get('volatility_window', 14)
        self.max_position_size = config.get('max_position_size', 0.10)  # 10% max position
        self.confidence_threshold = config.get('confidence_threshold', 0.0)
        self.kelly_fraction = config.get('kelly_fraction', 0.25)  # Conservative Kelly
        
        # Strategy state
        self.current_position = 0.0
        self.last_signal = None
        self.last_price = None
        self.trade_count = 0
        
        # Performance tracking
        self.signals_generated = []
        self.trades_executed = []
        
        # Setup logging
        self.logger = logging.getLogger(f'btc_momentum_strategy')
        
        self.logger.info(f"BTC Momentum Strategy initialized: {self.symbol}")
        self.logger.info(f"Parameters: MA({self.short_ma_period},{self.long_ma_period}), "
                        f"Vol({self.volatility_window}), Max Pos({self.max_position_size})")
    
    def calculate_moving_averages(self, data: pd.DataFrame) -> Tuple[float, float]:
        """Calculate short and long moving averages"""
        if len(data) < self.long_ma_period:
            return np.nan, np.nan
            
        prices = data['Close'].values
        short_ma = np.mean(prices[-self.short_ma_period:])
        long_ma = np.mean(prices[-self.long_ma_period:])
        
        return short_ma, long_ma
    
    def calculate_volatility(self, data: pd.DataFrame) -> float:
        """Calculate rolling volatility for position sizing"""
        if len(data) < self.volatility_window:
            return 0.02  # Default 2% volatility
            
        returns = data['Close'].pct_change().dropna()
        volatility = returns.tail(self.volatility_window).std()
        
        return max(volatility, 0.001)  # Minimum volatility threshold
    
    def calculate_kelly_position_size(self, win_probability: float, win_loss_ratio: float, 
                                    volatility: float) -> float:
        """
        Calculate position size using Kelly criterion
        
        Args:
            win_probability: Estimated probability of winning trade
            win_loss_ratio: Average win / average loss ratio
            volatility: Current market volatility
        
        Returns:
            Recommended position size (fraction of portfolio)
        """
        if win_probability <= 0.5 or win_loss_ratio <= 1.0:
            return 0.0
            
        # Kelly formula: f* = (bp - q) / b
        # where b = win_loss_ratio, p = win_probability, q = 1 - p
        kelly_fraction = ((win_loss_ratio * win_probability) - (1 - win_probability)) / win_loss_ratio
        
        # Conservative scaling and volatility adjustment
        adjusted_kelly = kelly_fraction * self.kelly_fraction * (1 / max(volatility * 10, 1))
        
        # Cap at maximum position size
        return min(abs(adjusted_kelly), self.max_position_size)
    
    def generate_signal(self, data: pd.DataFrame) -> Dict:
        """
        Generate trading signal based on momentum strategy
        
        Args:
            data: Historical price data
            
        Returns:
            Trading signal dictionary
        """
        if len(data) < self.long_ma_period:
            return self._create_signal('HOLD', 0.0, 0.0, 'Insufficient data for analysis')
        
        current_price = data['Close'].iloc[-1]
        short_ma, long_ma = self.calculate_moving_averages(data)
        volatility = self.calculate_volatility(data)
        
        # Log technical indicators
        self.logger.debug(f"MA calculated: short={short_ma:.2f}, long={long_ma:.2f}")
        self.logger.debug(f"Volatility calculated: {volatility:.4f} over {self.volatility_window} periods")
        
        # Determine signal direction
        if np.isnan(short_ma) or np.isnan(long_ma):
            return self._create_signal('HOLD', 0.0, 0.0, 'Invalid moving average calculation')
        
        # Calculate signal strength (confidence)
        ma_diff = abs(short_ma - long_ma)
        ma_diff_pct = ma_diff / long_ma
        confidence = min(ma_diff_pct / 0.01, 1.0)  # Normalize to 0-1 scale
        
        # Generate signal based on moving average crossover
        if short_ma > long_ma:
            # Bullish signal
            signal_type = 'BUY'
            # Estimate win probability based on momentum strength
            win_probability = 0.5 + (confidence * 0.3)  # 50-80% range
            win_loss_ratio = 1.2  # Assume slight positive expectancy
            
            position_size = self.calculate_kelly_position_size(win_probability, win_loss_ratio, volatility)
            reasoning = f'Bullish momentum: 5MA ({short_ma:.2f}) > 20MA ({long_ma:.2f})'
            
        elif short_ma < long_ma:
            # Bearish signal
            signal_type = 'SELL'
            # For sell signals, we typically close positions rather than short
            position_size = abs(self.current_position)  # Close current position
            reasoning = f'Bearish momentum: 5MA ({short_ma:.2f}) < 20MA ({long_ma:.2f})'
            
        else:
            # Neutral signal
            signal_type = 'HOLD'
            position_size = 0.0
            reasoning = 'Moving averages converged - no clear momentum'
        
        # Apply confidence threshold
        if confidence < self.confidence_threshold:
            signal_type = 'HOLD'
            position_size = 0.0
            reasoning += f' (Low confidence: {confidence:.3f} < {self.confidence_threshold})'
        
        return self._create_signal(signal_type, confidence, position_size, reasoning, 
                                 current_price, short_ma, long_ma, volatility)
    
    def _create_signal(self, signal_type: str, confidence: float, position_size: float, 
                      reasoning: str, price: float = None, short_ma: float = None, 
                      long_ma: float = None, volatility: float = None) -> Dict:
        """Create standardized signal dictionary"""
        
        # Calculate position change
        position_change = 0.0
        if signal_type == 'BUY':
            position_change = position_size - self.current_position
        elif signal_type == 'SELL':
            position_change = -position_size
        
        signal = {
            'timestamp': datetime.now(),
            'symbol': self.symbol,
            'signal_type': signal_type,
            'confidence': confidence,
            'position_size': position_size,
            'position_change': position_change,
            'current_position': self.current_position,
            'price': price,
            'reasoning': reasoning,
            'technical_indicators': {
                'short_ma': short_ma,
                'long_ma': long_ma,
                'volatility': volatility
            }
        }
        
        # Log signal generation
        if price:
            pos_change_str = f"{'+' if position_change >= 0 else ''}{position_change:.4f}"
            self.logger.info(f"SIGNAL | {self.symbol} {signal_type}: confidence={confidence:.3f} "
                           f"price={price:.2f} pos_change={pos_change_str} reason='{reasoning}'")
        
        # Store signal for performance tracking
        self.signals_generated.append(signal)
        
        # Log to performance logger if available
        if self.performance_logger:
            self.performance_logger.log_trading_signal(
                asset=signal.get('asset', 'BTCUSD'),
                signal_type=signal.get('signal', 'HOLD'),
                confidence=signal.get('confidence', 0.0),
                current_price=signal.get('price', 0.0),
                target_position=signal.get('position_change', 0.0),
                current_position=0.0,  # Would need to track this
                signal_reason=signal.get('reason', 'No reason provided'),
                technical_indicators={'ma_short': short_ma, 'ma_long': long_ma, 'volatility': volatility}
            )
        
        return signal
    
    def execute_trade(self, signal: Dict) -> Dict:
        """
        Execute trade based on signal
        
        Args:
            signal: Trading signal from generate_signal()
            
        Returns:
            Trade execution result
        """
        if signal['signal_type'] == 'HOLD':
            return {'status': 'no_trade', 'message': 'Hold signal - no trade executed'}
        
        # Update position
        old_position = self.current_position
        self.current_position += signal['position_change']
        self.trade_count += 1
        
        # Create trade record
        trade = {
            'trade_id': self.trade_count,
            'timestamp': signal['timestamp'],
            'symbol': signal['symbol'],
            'action': signal['signal_type'],
            'quantity': abs(signal['position_change']),
            'price': signal['price'],
            'position_before': old_position,
            'position_after': self.current_position,
            'confidence': signal['confidence'],
            'reasoning': signal['reasoning']
        }
        
        # Store trade for performance tracking
        self.trades_executed.append(trade)
        
        # Log to performance logger if available
        if self.performance_logger:
            self.performance_logger.log_trade_execution(
                asset=trade.get('asset', 'BTCUSD'),
                action=trade.get('action', 'HOLD'),
                intended_quantity=trade.get('quantity', 0.0),
                executed_quantity=trade.get('quantity', 0.0),
                intended_price=trade.get('price', 0.0),
                executed_price=trade.get('price', 0.0),
                execution_delay_ms=0.0,
                trade_cost=trade.get('cost', 0.0)
            )
        
        self.logger.info(f"TRADE EXECUTED | {signal['signal_type']}: "
                        f"{abs(signal['position_change']):.4f} {self.symbol} "
                        f"at ${signal['price']:.2f} (confidence: {signal['confidence']:.3f})")
        
        return {'status': 'executed', 'trade': trade}
    
    def get_current_position(self) -> float:
        """Get current position size"""
        return self.current_position
    
    def get_performance_summary(self) -> Dict:
        """Get strategy performance summary"""
        return {
            'signals_generated': len(self.signals_generated),
            'trades_executed': len(self.trades_executed),
            'current_position': self.current_position,
            'strategy_name': 'BTC_Momentum',
            'parameters': {
                'short_ma': self.short_ma_period,
                'long_ma': self.long_ma_period,
                'volatility_window': self.volatility_window,
                'max_position_size': self.max_position_size,
                'kelly_fraction': self.kelly_fraction
            }
        }
    
    def reset(self):
        """Reset strategy state"""
        self.current_position = 0.0
        self.last_signal = None
        self.last_price = None
        self.trade_count = 0
        self.signals_generated = []
        self.trades_executed = []
        
        self.logger.info("BTC Momentum Strategy reset")

def create_btc_momentum_strategy(config: Dict, performance_logger: 'PerformanceLogger' = None) -> BTCMomentumStrategy:
    """
    Factory function to create BTC momentum strategy
    
    Args:
        config: Strategy configuration
        performance_logger: Optional performance logger
        
    Returns:
        Configured BTCMomentumStrategy instance
    """
    return BTCMomentumStrategy(config, performance_logger)