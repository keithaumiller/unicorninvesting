"""
Multi-Timeframe Trading Strategies for Myportolio

This module implements comprehensive trading strategies across multiple timeframes
(1-minute, 1-hour, 1-day) with unified signal generation and portfolio management.
Integrates with ETH forecast generation system for production-grade predictions.

Strategy Components:
- ScalpStrategy: 1-minute high-frequency trading
- SwingStrategy: 1-hour medium-term trading  
- PositionStrategy: Daily long-term trading
- InvestmentStrategy: Weekly strategic allocation
- MultiTimeframePortfolioManager: Unified coordination

Key Features:
- Kelly Criterion position sizing
- Timeframe-specific risk management
- Coordinated signal generation
- Performance tracking
- Risk-adjusted returns optimization
- Forecast-driven decision making
"""

import pandas as pd
import numpy as np
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Tuple, Any
from datetime import datetime, timedelta
from dataclasses import dataclass
from enum import Enum
import warnings
import sys
from pathlib import Path

# Add ETH models directory to path for forecast reader
sys.path.append(str(Path(__file__).parent.parent.parent.parent / "2_alpha_models" / "CRYPTO" / "ETH"))

try:
    from eth_forecast_reader import ETHForecastReader, get_eth_forecast_signal, get_eth_price_prediction, get_eth_trend_direction
    FORECASTS_AVAILABLE = True
except ImportError:
    print("⚠️  ETH forecast reader not available. Using fallback prediction methods.")
    FORECASTS_AVAILABLE = False

warnings.filterwarnings('ignore', category=RuntimeWarning)

class SignalType(Enum):
    BUY = "buy"
    SELL = "sell" 
    HOLD = "hold"

class RiskLevel(Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"

@dataclass
class TradingSignal:
    """Unified trading signal structure."""
    timeframe: str
    signal_type: SignalType
    strength: float  # 0.0 to 1.0
    price_target: Optional[float]
    stop_loss: Optional[float]
    take_profit: Optional[float]
    position_size: float
    confidence: float
    risk_level: RiskLevel
    timestamp: datetime
    forecast_based: bool = False
    forecast_confidence: float = 0.0


class BaseMultiTimeframeStrategy(ABC):
    """
    Abstract base class for multi-timeframe trading strategies.
    Implements common functionality and forecast integration.
    """
    
    def __init__(self, timeframe: str, max_position_size: float, 
                 stop_loss_pct: float, take_profit_pct: float):
        self.timeframe = timeframe
        self.max_position_size = max_position_size
        self.stop_loss_pct = stop_loss_pct
        self.take_profit_pct = take_profit_pct
        
        # Initialize forecast reader if available
        self.forecast_reader = ETHForecastReader() if FORECASTS_AVAILABLE else None
        
        # Performance tracking
        self.performance_history = []
        self.signal_history = []
        
        # Risk management
        self.max_daily_trades = self._get_max_daily_trades()
        self.daily_trade_count = 0
        self.last_trade_date = None
        
    def _get_max_daily_trades(self) -> int:
        """Get maximum daily trades based on timeframe."""
        if self.timeframe == '1min':
            return 50  # High-frequency trading
        elif self.timeframe == '1hour':
            return 12  # Moderate frequency
        else:  # 1day
            return 3   # Conservative
    
    def _reset_daily_counters(self):
        """Reset daily trade counters if new day."""
        today = datetime.now().date()
        if self.last_trade_date != today:
            self.daily_trade_count = 0
            self.last_trade_date = today
    
    def get_forecast_signal(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Get signal from forecast system if available."""
        if not FORECASTS_AVAILABLE or self.forecast_reader is None:
            return {'signal': 'hold', 'strength': 0.0, 'forecast_based': False}
        
        try:
            # Get forecast signal for this timeframe
            signal_data = get_eth_forecast_signal(self.timeframe, threshold=0.01)
            
            return {
                'signal': signal_data.get('signal', 'hold'),
                'strength': signal_data.get('strength', 0.0),
                'price_change_pct': signal_data.get('price_change_pct', 0.0),
                'trend_direction': signal_data.get('trend_direction', 'neutral'),
                'confidence': signal_data.get('confidence', 0.0),
                'forecast_based': True
            }
        except Exception as e:
            print(f"⚠️  Error getting forecast signal: {e}")
            return {'signal': 'hold', 'strength': 0.0, 'forecast_based': False}
    
    def calculate_kelly_position_size(self, win_rate: float, avg_win: float, 
                                    avg_loss: float, confidence: float = 1.0) -> float:
        """
        Calculate optimal position size using Kelly Criterion.
        
        Args:
            win_rate: Probability of winning trade (0-1)
            avg_win: Average win amount (positive)
            avg_loss: Average loss amount (positive) 
            confidence: Confidence in the signal (0-1)
        """
        if avg_loss <= 0:
            return 0.0
            
        # Kelly formula: f = (bp - q) / b
        # where b = avg_win/avg_loss, p = win_rate, q = 1-win_rate
        b = avg_win / avg_loss
        p = win_rate
        q = 1 - win_rate
        
        kelly_fraction = (b * p - q) / b
        
        # Apply confidence adjustment
        adjusted_kelly = kelly_fraction * confidence
        
        # Apply conservative scaling (max 25% of Kelly recommendation)
        conservative_kelly = adjusted_kelly * 0.25
        
        # Ensure within position limits
        return max(0, min(conservative_kelly, self.max_position_size))
    
    def calculate_technical_indicators(self, data: pd.DataFrame) -> Dict[str, float]:
        """Calculate common technical indicators."""
        if len(data) < 20:
            return {}
        
        close = data['close']
        
        # Moving averages
        sma_10 = close.rolling(10).mean().iloc[-1]
        sma_20 = close.rolling(20).mean().iloc[-1]
        ema_12 = close.ewm(span=12).mean().iloc[-1]
        ema_26 = close.ewm(span=26).mean().iloc[-1]
        
        # RSI
        delta = close.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        
        # MACD
        macd = ema_12 - ema_26
        signal_line = macd.ewm(span=9).mean()
        macd_histogram = macd - signal_line
        
        # Bollinger Bands
        bb_middle = close.rolling(20).mean()
        bb_std = close.rolling(20).std()
        bb_upper = bb_middle + (bb_std * 2)
        bb_lower = bb_middle - (bb_std * 2)
        
        # Current price relative to bands
        current_price = close.iloc[-1]
        bb_position = (current_price - bb_lower.iloc[-1]) / (bb_upper.iloc[-1] - bb_lower.iloc[-1])
        
        return {
            'sma_10': sma_10,
            'sma_20': sma_20,
            'ema_12': ema_12,
            'ema_26': ema_26,
            'rsi': rsi.iloc[-1] if not rsi.empty else 50,
            'macd': macd.iloc[-1] if not macd.empty else 0,
            'macd_signal': signal_line.iloc[-1] if not signal_line.empty else 0,
            'macd_histogram': macd_histogram.iloc[-1] if not macd_histogram.empty else 0,
            'bb_position': bb_position,
            'bb_upper': bb_upper.iloc[-1],
            'bb_lower': bb_lower.iloc[-1]
        }
    
    @abstractmethod
    def generate_signal(self, market_data: pd.DataFrame) -> TradingSignal:
        """Generate trading signal based on strategy logic."""
        pass
    
    @abstractmethod
    def update_performance(self, realized_return: float, signal: TradingSignal):
        """Update strategy performance metrics."""
        pass


class ScalpStrategy(BaseMultiTimeframeStrategy):
    """
    1-minute scalping strategy for high-frequency trading.
    Focus on small, quick profits with tight risk controls.
    """
    
    def __init__(self):
        super().__init__(
            timeframe='1min',
            max_position_size=0.02,  # 2% max position
            stop_loss_pct=0.002,     # 0.2% stop loss
            take_profit_pct=0.003    # 0.3% take profit
        )
        
        # Scalping-specific parameters
        self.min_spread_bps = 2      # Minimum 2 basis points spread
        self.max_holding_minutes = 5  # Maximum 5 minutes holding
        self.momentum_threshold = 0.001  # 0.1% momentum threshold
        
    def generate_signal(self, market_data: pd.DataFrame) -> TradingSignal:
        """Generate scalping signal based on micro-movements and forecasts."""
        self._reset_daily_counters()
        
        # Check daily trade limit
        if self.daily_trade_count >= self.max_daily_trades:
            return self._create_hold_signal("Daily trade limit reached")
        
        current_price = market_data['close'].iloc[-1]
        
        # Get forecast signal
        forecast_signal = self.get_forecast_signal(market_data)
        
        # Calculate technical indicators
        indicators = self.calculate_technical_indicators(market_data)
        
        # Scalping logic: Quick momentum + forecast confirmation
        signal_type = SignalType.HOLD
        strength = 0.0
        confidence = 0.5
        
        # Check for forecast signal
        if forecast_signal.get('forecast_based', False):
            forecast_strength = forecast_signal.get('strength', 0.0)
            forecast_confidence = forecast_signal.get('confidence', 0.0)
            
            if forecast_signal['signal'] == 'buy' and forecast_strength > 0.3:
                signal_type = SignalType.BUY
                strength = min(forecast_strength * 1.5, 1.0)  # Amplify for scalping
                confidence = forecast_confidence
            elif forecast_signal['signal'] == 'sell' and forecast_strength > 0.3:
                signal_type = SignalType.SELL
                strength = min(forecast_strength * 1.5, 1.0)
                confidence = forecast_confidence
        
        # Technical confirmation
        if indicators:
            rsi = indicators.get('rsi', 50)
            bb_position = indicators.get('bb_position', 0.5)
            macd_histogram = indicators.get('macd_histogram', 0)
            
            # RSI momentum
            if rsi > 70 and signal_type == SignalType.SELL:
                strength *= 1.2  # Strengthen sell signal in overbought
            elif rsi < 30 and signal_type == SignalType.BUY:
                strength *= 1.2  # Strengthen buy signal in oversold
            
            # Bollinger Band position
            if bb_position > 0.8 and signal_type == SignalType.SELL:
                strength *= 1.1  # Near upper band
            elif bb_position < 0.2 and signal_type == SignalType.BUY:
                strength *= 1.1  # Near lower band
            
            # MACD momentum
            if macd_histogram > 0 and signal_type == SignalType.BUY:
                strength *= 1.1
            elif macd_histogram < 0 and signal_type == SignalType.SELL:
                strength *= 1.1
        
        # Limit strength for scalping
        strength = min(strength, 0.8)  # Max 80% strength for scalping
        
        # Calculate position size
        if signal_type != SignalType.HOLD:
            # Use recent performance for Kelly calculation
            win_rate = self._get_recent_win_rate()
            avg_win = self.take_profit_pct
            avg_loss = self.stop_loss_pct
            
            position_size = self.calculate_kelly_position_size(
                win_rate, avg_win, avg_loss, confidence
            )
        else:
            position_size = 0.0
        
        # Calculate price targets
        if signal_type == SignalType.BUY:
            stop_loss = current_price * (1 - self.stop_loss_pct)
            take_profit = current_price * (1 + self.take_profit_pct)
        elif signal_type == SignalType.SELL:
            stop_loss = current_price * (1 + self.stop_loss_pct)
            take_profit = current_price * (1 - self.take_profit_pct)
        else:
            stop_loss = None
            take_profit = None
        
        # Determine risk level
        if strength > 0.6:
            risk_level = RiskLevel.HIGH
        elif strength > 0.3:
            risk_level = RiskLevel.MEDIUM
        else:
            risk_level = RiskLevel.LOW
        
        signal = TradingSignal(
            timeframe=self.timeframe,
            signal_type=signal_type,
            strength=strength,
            price_target=take_profit,
            stop_loss=stop_loss,
            take_profit=take_profit,
            position_size=position_size,
            confidence=confidence,
            risk_level=risk_level,
            timestamp=datetime.now(),
            forecast_based=forecast_signal.get('forecast_based', False),
            forecast_confidence=forecast_signal.get('confidence', 0.0)
        )
        
        # Track signal
        self.signal_history.append(signal)
        if signal_type != SignalType.HOLD:
            self.daily_trade_count += 1
        
        return signal
    
    def _create_hold_signal(self, reason: str) -> TradingSignal:
        """Create a hold signal with reason."""
        return TradingSignal(
            timeframe=self.timeframe,
            signal_type=SignalType.HOLD,
            strength=0.0,
            price_target=None,
            stop_loss=None,
            take_profit=None,
            position_size=0.0,
            confidence=0.0,
            risk_level=RiskLevel.LOW,
            timestamp=datetime.now()
        )
    
    def _get_recent_win_rate(self) -> float:
        """Calculate recent win rate for Kelly Criterion."""
        if len(self.performance_history) < 10:
            return 0.55  # Default assumption
        
        recent_trades = self.performance_history[-20:]  # Last 20 trades
        wins = sum(1 for trade in recent_trades if trade > 0)
        return wins / len(recent_trades)
    
    def update_performance(self, realized_return: float, signal: TradingSignal):
        """Update scalping performance metrics."""
        self.performance_history.append(realized_return)
        
        # Keep only recent history
        if len(self.performance_history) > 1000:
            self.performance_history = self.performance_history[-1000:]


class SwingStrategy(BaseMultiTimeframeStrategy):
    """
    1-hour swing trading strategy for medium-term positions.
    Focus on trend following with moderate risk tolerance.
    """
    
    def __init__(self):
        super().__init__(
            timeframe='1hour',
            max_position_size=0.10,  # 10% max position
            stop_loss_pct=0.05,      # 5% stop loss
            take_profit_pct=0.10     # 10% take profit
        )
        
        # Swing-specific parameters
        self.trend_lookback = 24     # 24 hours for trend
        self.volatility_window = 12  # 12 hours for volatility
        self.min_trend_strength = 0.02  # 2% minimum trend
        
    def generate_signal(self, market_data: pd.DataFrame) -> TradingSignal:
        """Generate swing signal based on trends and forecasts."""
        self._reset_daily_counters()
        
        if self.daily_trade_count >= self.max_daily_trades:
            return self._create_hold_signal("Daily trade limit reached")
        
        current_price = market_data['close'].iloc[-1]
        
        # Get forecast signal
        forecast_signal = self.get_forecast_signal(market_data)
        
        # Calculate technical indicators
        indicators = self.calculate_technical_indicators(market_data)
        
        # Trend analysis
        trend_data = self._analyze_trend(market_data)
        
        # Base signal from forecast
        signal_type = SignalType.HOLD
        strength = 0.0
        confidence = 0.6
        
        if forecast_signal.get('forecast_based', False):
            trend_direction = forecast_signal.get('trend_direction', 'neutral')
            forecast_strength = forecast_signal.get('strength', 0.0)
            
            if trend_direction == 'bullish' and forecast_strength > 0.4:
                signal_type = SignalType.BUY
                strength = forecast_strength
                confidence = forecast_signal.get('confidence', 0.6)
            elif trend_direction == 'bearish' and forecast_strength > 0.4:
                signal_type = SignalType.SELL
                strength = forecast_strength
                confidence = forecast_signal.get('confidence', 0.6)
        
        # Technical trend confirmation
        if indicators and trend_data:
            sma_10 = indicators.get('sma_10', current_price)
            sma_20 = indicators.get('sma_20', current_price)
            rsi = indicators.get('rsi', 50)
            macd = indicators.get('macd', 0)
            macd_signal = indicators.get('macd_signal', 0)
            
            trend_strength = trend_data.get('strength', 0)
            trend_direction = trend_data.get('direction', 'neutral')
            
            # Moving average confirmation
            if signal_type == SignalType.BUY:
                if sma_10 > sma_20 and current_price > sma_10:
                    strength *= 1.2
                if trend_direction == 'bullish' and trend_strength > self.min_trend_strength:
                    strength *= 1.3
            elif signal_type == SignalType.SELL:
                if sma_10 < sma_20 and current_price < sma_10:
                    strength *= 1.2
                if trend_direction == 'bearish' and trend_strength > self.min_trend_strength:
                    strength *= 1.3
            
            # MACD confirmation
            if signal_type == SignalType.BUY and macd > macd_signal:
                strength *= 1.1
            elif signal_type == SignalType.SELL and macd < macd_signal:
                strength *= 1.1
            
            # RSI filter (avoid extreme conditions)
            if signal_type == SignalType.BUY and rsi > 75:
                strength *= 0.7  # Reduce buy strength when overbought
            elif signal_type == SignalType.SELL and rsi < 25:
                strength *= 0.7  # Reduce sell strength when oversold
        
        # Limit strength for swing trading
        strength = min(strength, 0.9)
        
        # Calculate position size
        if signal_type != SignalType.HOLD:
            win_rate = self._get_recent_win_rate()
            avg_win = self.take_profit_pct
            avg_loss = self.stop_loss_pct
            
            position_size = self.calculate_kelly_position_size(
                win_rate, avg_win, avg_loss, confidence
            )
        else:
            position_size = 0.0
        
        # Calculate price targets
        if signal_type == SignalType.BUY:
            stop_loss = current_price * (1 - self.stop_loss_pct)
            take_profit = current_price * (1 + self.take_profit_pct)
        elif signal_type == SignalType.SELL:
            stop_loss = current_price * (1 + self.stop_loss_pct)
            take_profit = current_price * (1 - self.take_profit_pct)
        else:
            stop_loss = None
            take_profit = None
        
        # Risk level based on volatility and strength
        volatility = self._calculate_volatility(market_data)
        if strength > 0.7 or volatility > 0.05:
            risk_level = RiskLevel.HIGH
        elif strength > 0.4 or volatility > 0.03:
            risk_level = RiskLevel.MEDIUM
        else:
            risk_level = RiskLevel.LOW
        
        signal = TradingSignal(
            timeframe=self.timeframe,
            signal_type=signal_type,
            strength=strength,
            price_target=take_profit,
            stop_loss=stop_loss,
            take_profit=take_profit,
            position_size=position_size,
            confidence=confidence,
            risk_level=risk_level,
            timestamp=datetime.now(),
            forecast_based=forecast_signal.get('forecast_based', False),
            forecast_confidence=forecast_signal.get('confidence', 0.0)
        )
        
        # Track signal
        self.signal_history.append(signal)
        if signal_type != SignalType.HOLD:
            self.daily_trade_count += 1
        
        return signal
    
    def _analyze_trend(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Analyze price trend over the lookback period."""
        if len(data) < self.trend_lookback:
            return {'direction': 'neutral', 'strength': 0}
        
        recent_data = data.tail(self.trend_lookback)
        first_price = recent_data['close'].iloc[0]
        last_price = recent_data['close'].iloc[-1]
        
        # Calculate trend strength
        price_change = (last_price - first_price) / first_price
        
        # Linear regression slope
        x = np.arange(len(recent_data))
        y = recent_data['close'].values
        slope, _ = np.polyfit(x, y, 1)
        normalized_slope = slope / np.mean(y)
        
        # Trend direction and strength
        if price_change > 0.01:  # More than 1% increase
            direction = 'bullish'
            strength = abs(price_change)
        elif price_change < -0.01:  # More than 1% decrease
            direction = 'bearish'
            strength = abs(price_change)
        else:
            direction = 'neutral'
            strength = 0
        
        return {
            'direction': direction,
            'strength': strength,
            'price_change': price_change,
            'slope': normalized_slope
        }
    
    def _calculate_volatility(self, data: pd.DataFrame) -> float:
        """Calculate recent volatility."""
        if len(data) < self.volatility_window:
            return 0.02  # Default volatility
        
        recent_data = data.tail(self.volatility_window)
        returns = recent_data['close'].pct_change().dropna()
        return returns.std() if len(returns) > 0 else 0.02
    
    def _create_hold_signal(self, reason: str) -> TradingSignal:
        """Create a hold signal with reason."""
        return TradingSignal(
            timeframe=self.timeframe,
            signal_type=SignalType.HOLD,
            strength=0.0,
            price_target=None,
            stop_loss=None,
            take_profit=None,
            position_size=0.0,
            confidence=0.0,
            risk_level=RiskLevel.LOW,
            timestamp=datetime.now()
        )
    
    def _get_recent_win_rate(self) -> float:
        """Calculate recent win rate for Kelly Criterion."""
        if len(self.performance_history) < 5:
            return 0.60  # Default assumption for swing trading
        
        recent_trades = self.performance_history[-10:]  # Last 10 trades
        wins = sum(1 for trade in recent_trades if trade > 0)
        return wins / len(recent_trades)
    
    def update_performance(self, realized_return: float, signal: TradingSignal):
        """Update swing trading performance metrics."""
        self.performance_history.append(realized_return)
        
        # Keep only recent history
        if len(self.performance_history) > 500:
            self.performance_history = self.performance_history[-500:]


class PositionStrategy(BaseMultiTimeframeStrategy):
    """
    Daily position strategy for long-term trend following.
    Focus on major moves with higher risk tolerance.
    """
    
    def __init__(self):
        super().__init__(
            timeframe='1day',
            max_position_size=0.25,  # 25% max position
            stop_loss_pct=0.15,      # 15% stop loss
            take_profit_pct=0.25     # 25% take profit
        )
        
        # Position-specific parameters
        self.trend_lookback = 30     # 30 days for trend
        self.momentum_window = 14    # 14 days for momentum
        self.min_position_days = 3   # Minimum 3 days holding
        
    def generate_signal(self, market_data: pd.DataFrame) -> TradingSignal:
        """Generate position signal based on long-term trends."""
        self._reset_daily_counters()
        
        if self.daily_trade_count >= self.max_daily_trades:
            return self._create_hold_signal("Daily trade limit reached")
        
        current_price = market_data['close'].iloc[-1]
        
        # Get forecast signal
        forecast_signal = self.get_forecast_signal(market_data)
        
        # Calculate technical indicators
        indicators = self.calculate_technical_indicators(market_data)
        
        # Long-term trend analysis
        trend_data = self._analyze_long_term_trend(market_data)
        momentum_data = self._analyze_momentum(market_data)
        
        # Base signal from forecast
        signal_type = SignalType.HOLD
        strength = 0.0
        confidence = 0.7
        
        if forecast_signal.get('forecast_based', False):
            trend_direction = forecast_signal.get('trend_direction', 'neutral')
            forecast_strength = forecast_signal.get('strength', 0.0)
            
            if trend_direction == 'bullish' and forecast_strength > 0.5:
                signal_type = SignalType.BUY
                strength = forecast_strength * 0.8  # More conservative for long-term
                confidence = forecast_signal.get('confidence', 0.7)
            elif trend_direction == 'bearish' and forecast_strength > 0.5:
                signal_type = SignalType.SELL
                strength = forecast_strength * 0.8
                confidence = forecast_signal.get('confidence', 0.7)
        
        # Long-term technical confirmation
        if indicators and trend_data and momentum_data:
            sma_10 = indicators.get('sma_10', current_price)
            sma_20 = indicators.get('sma_20', current_price)
            rsi = indicators.get('rsi', 50)
            
            trend_strength = trend_data.get('strength', 0)
            trend_direction = trend_data.get('direction', 'neutral')
            momentum_score = momentum_data.get('score', 0)
            
            # Strong trend confirmation
            if signal_type == SignalType.BUY:
                if (trend_direction == 'bullish' and trend_strength > 0.1 and
                    momentum_score > 0.05 and sma_10 > sma_20):
                    strength *= 1.5
            elif signal_type == SignalType.SELL:
                if (trend_direction == 'bearish' and trend_strength > 0.1 and
                    momentum_score < -0.05 and sma_10 < sma_20):
                    strength *= 1.5
            
            # RSI trend confirmation (less restrictive for position trading)
            if signal_type == SignalType.BUY and rsi > 55:
                strength *= 1.1
            elif signal_type == SignalType.SELL and rsi < 45:
                strength *= 1.1
        
        # Limit strength
        strength = min(strength, 1.0)
        
        # Calculate position size (more conservative for long-term)
        if signal_type != SignalType.HOLD:
            win_rate = self._get_recent_win_rate()
            avg_win = self.take_profit_pct
            avg_loss = self.stop_loss_pct
            
            position_size = self.calculate_kelly_position_size(
                win_rate, avg_win, avg_loss, confidence
            ) * 0.6  # Conservative multiplier for position trading
        else:
            position_size = 0.0
        
        # Calculate price targets
        if signal_type == SignalType.BUY:
            stop_loss = current_price * (1 - self.stop_loss_pct)
            take_profit = current_price * (1 + self.take_profit_pct)
        elif signal_type == SignalType.SELL:
            stop_loss = current_price * (1 + self.stop_loss_pct)
            take_profit = current_price * (1 - self.take_profit_pct)
        else:
            stop_loss = None
            take_profit = None
        
        # Risk level for position trading
        if strength > 0.8:
            risk_level = RiskLevel.HIGH
        elif strength > 0.5:
            risk_level = RiskLevel.MEDIUM
        else:
            risk_level = RiskLevel.LOW
        
        signal = TradingSignal(
            timeframe=self.timeframe,
            signal_type=signal_type,
            strength=strength,
            price_target=take_profit,
            stop_loss=stop_loss,
            take_profit=take_profit,
            position_size=position_size,
            confidence=confidence,
            risk_level=risk_level,
            timestamp=datetime.now(),
            forecast_based=forecast_signal.get('forecast_based', False),
            forecast_confidence=forecast_signal.get('confidence', 0.0)
        )
        
        # Track signal
        self.signal_history.append(signal)
        if signal_type != SignalType.HOLD:
            self.daily_trade_count += 1
        
        return signal
    
    def _analyze_long_term_trend(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Analyze long-term price trend."""
        if len(data) < self.trend_lookback:
            return {'direction': 'neutral', 'strength': 0}
        
        recent_data = data.tail(self.trend_lookback)
        
        # Multiple timeframe analysis
        weekly_change = (recent_data['close'].iloc[-1] - recent_data['close'].iloc[-7]) / recent_data['close'].iloc[-7]
        monthly_change = (recent_data['close'].iloc[-1] - recent_data['close'].iloc[0]) / recent_data['close'].iloc[0]
        
        # Moving average slopes
        ma_short = recent_data['close'].rolling(7).mean()
        ma_long = recent_data['close'].rolling(21).mean()
        
        ma_trend = 1 if ma_short.iloc[-1] > ma_long.iloc[-1] else -1
        
        # Combine signals
        if weekly_change > 0.05 and monthly_change > 0.1 and ma_trend > 0:
            direction = 'bullish'
            strength = min(abs(monthly_change), 0.5)
        elif weekly_change < -0.05 and monthly_change < -0.1 and ma_trend < 0:
            direction = 'bearish'
            strength = min(abs(monthly_change), 0.5)
        else:
            direction = 'neutral'
            strength = 0
        
        return {
            'direction': direction,
            'strength': strength,
            'weekly_change': weekly_change,
            'monthly_change': monthly_change,
            'ma_trend': ma_trend
        }
    
    def _analyze_momentum(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Analyze price momentum."""
        if len(data) < self.momentum_window:
            return {'score': 0}
        
        recent_data = data.tail(self.momentum_window)
        
        # Rate of change
        roc = (recent_data['close'].iloc[-1] - recent_data['close'].iloc[0]) / recent_data['close'].iloc[0]
        
        # Momentum oscillator
        momentum = recent_data['close'].iloc[-1] / recent_data['close'].rolling(self.momentum_window).mean().iloc[-1] - 1
        
        # Volume momentum (if available)
        volume_momentum = 0
        if 'volume' in recent_data.columns:
            avg_volume = recent_data['volume'].mean()
            recent_volume = recent_data['volume'].tail(3).mean()
            volume_momentum = (recent_volume - avg_volume) / avg_volume
        
        # Composite momentum score
        momentum_score = (roc * 0.5 + momentum * 0.3 + volume_momentum * 0.2)
        
        return {
            'score': momentum_score,
            'roc': roc,
            'momentum': momentum,
            'volume_momentum': volume_momentum
        }
    
    def _create_hold_signal(self, reason: str) -> TradingSignal:
        """Create a hold signal with reason."""
        return TradingSignal(
            timeframe=self.timeframe,
            signal_type=SignalType.HOLD,
            strength=0.0,
            price_target=None,
            stop_loss=None,
            take_profit=None,
            position_size=0.0,
            confidence=0.0,
            risk_level=RiskLevel.LOW,
            timestamp=datetime.now()
        )
    
    def _get_recent_win_rate(self) -> float:
        """Calculate recent win rate for Kelly Criterion."""
        if len(self.performance_history) < 3:
            return 0.65  # Default assumption for position trading
        
        recent_trades = self.performance_history[-5:]  # Last 5 trades
        wins = sum(1 for trade in recent_trades if trade > 0)
        return wins / len(recent_trades)
    
    def update_performance(self, realized_return: float, signal: TradingSignal):
        """Update position trading performance metrics."""
        self.performance_history.append(realized_return)
        
        # Keep only recent history
        if len(self.performance_history) > 100:
            self.performance_history = self.performance_history[-100:]


class InvestmentStrategy(BaseMultiTimeframeStrategy):
    """
    Weekly investment strategy for strategic allocation.
    Focus on long-term value with portfolio rebalancing.
    """
    
    def __init__(self):
        super().__init__(
            timeframe='1week',
            max_position_size=0.80,  # 80% max allocation to ETH
            stop_loss_pct=0.30,      # 30% stop loss (wide for long-term)
            take_profit_pct=0.50     # 50% take profit
        )
        
        # Investment-specific parameters
        self.rebalance_threshold = 0.10  # 10% deviation triggers rebalance
        self.min_allocation = 0.20       # 20% minimum ETH allocation
        self.max_allocation = 0.80       # 80% maximum ETH allocation
        self.target_allocation = 0.50    # 50% target ETH allocation
        
    def generate_signal(self, market_data: pd.DataFrame) -> TradingSignal:
        """Generate investment allocation signal."""
        current_price = market_data['close'].iloc[-1]
        
        # Get long-term forecast
        forecast_signal = self.get_forecast_signal(market_data)
        
        # Strategic allocation logic
        signal_type = SignalType.HOLD
        strength = 0.0
        confidence = 0.8
        
        if forecast_signal.get('forecast_based', False):
            trend_direction = forecast_signal.get('trend_direction', 'neutral')
            
            # Strategic allocation based on long-term forecast
            if trend_direction == 'bullish':
                # Increase ETH allocation towards maximum
                signal_type = SignalType.BUY
                strength = 0.6
                target_allocation = min(self.target_allocation * 1.3, self.max_allocation)
            elif trend_direction == 'bearish':
                # Decrease ETH allocation towards minimum
                signal_type = SignalType.SELL
                strength = 0.6
                target_allocation = max(self.target_allocation * 0.7, self.min_allocation)
            else:
                # Maintain target allocation
                target_allocation = self.target_allocation
        else:
            target_allocation = self.target_allocation
        
        # Calculate position size based on allocation strategy
        current_allocation = 0.50  # Assume current allocation (would come from portfolio)
        allocation_difference = target_allocation - current_allocation
        
        if abs(allocation_difference) > self.rebalance_threshold:
            if allocation_difference > 0:
                signal_type = SignalType.BUY
                position_size = abs(allocation_difference)
            else:
                signal_type = SignalType.SELL
                position_size = abs(allocation_difference)
            
            strength = min(abs(allocation_difference) / self.rebalance_threshold, 1.0)
        else:
            position_size = 0.0
        
        # Very wide stops for investment strategy
        if signal_type == SignalType.BUY:
            stop_loss = current_price * (1 - self.stop_loss_pct)
            take_profit = current_price * (1 + self.take_profit_pct)
        elif signal_type == SignalType.SELL:
            stop_loss = current_price * (1 + self.stop_loss_pct)
            take_profit = current_price * (1 - self.take_profit_pct)
        else:
            stop_loss = None
            take_profit = None
        
        signal = TradingSignal(
            timeframe=self.timeframe,
            signal_type=signal_type,
            strength=strength,
            price_target=take_profit,
            stop_loss=stop_loss,
            take_profit=take_profit,
            position_size=position_size,
            confidence=confidence,
            risk_level=RiskLevel.LOW,  # Conservative for investment strategy
            timestamp=datetime.now(),
            forecast_based=forecast_signal.get('forecast_based', False),
            forecast_confidence=forecast_signal.get('confidence', 0.0)
        )
        
        return signal
    
    def update_performance(self, realized_return: float, signal: TradingSignal):
        """Update investment performance metrics."""
        self.performance_history.append(realized_return)
        
        # Keep longer history for investment strategy
        if len(self.performance_history) > 50:
            self.performance_history = self.performance_history[-50:]


class MultiTimeframePortfolioManager:
    """
    Coordinates signals across multiple timeframes for unified portfolio management.
    Integrates forecast-driven strategies with risk management.
    """
    
    def __init__(self):
        # Initialize all strategies
        self.strategies = {
            '1min': ScalpStrategy(),
            '1hour': SwingStrategy(), 
            '1day': PositionStrategy(),
            '1week': InvestmentStrategy()
        }
        
        # Timeframe weights for signal combination
        self.timeframe_weights = {
            '1min': 0.15,   # 15% weight
            '1hour': 0.25,  # 25% weight
            '1day': 0.35,   # 35% weight
            '1week': 0.25   # 25% weight
        }
        
        # Risk management
        self.max_total_position = 0.50  # 50% max total position across all timeframes
        self.correlation_threshold = 0.7  # Reduce position if signals highly correlated
        
        # Performance tracking
        self.portfolio_performance = []
        self.signal_correlation_history = []
        
    def generate_unified_signals(self, market_data: pd.DataFrame) -> Dict[str, TradingSignal]:
        """Generate signals from all timeframe strategies."""
        signals = {}
        
        for timeframe, strategy in self.strategies.items():
            try:
                signal = strategy.generate_signal(market_data)
                signals[timeframe] = signal
            except Exception as e:
                print(f"⚠️  Error generating {timeframe} signal: {e}")
                # Create default hold signal
                signals[timeframe] = TradingSignal(
                    timeframe=timeframe,
                    signal_type=SignalType.HOLD,
                    strength=0.0,
                    price_target=None,
                    stop_loss=None,
                    take_profit=None,
                    position_size=0.0,
                    confidence=0.0,
                    risk_level=RiskLevel.LOW,
                    timestamp=datetime.now()
                )
        
        return signals
    
    def calculate_unified_position(self, signals: Dict[str, TradingSignal]) -> Dict[str, Any]:
        """Calculate unified position based on all timeframe signals."""
        # Separate buy and sell signals
        buy_signals = {tf: sig for tf, sig in signals.items() 
                      if sig.signal_type == SignalType.BUY}
        sell_signals = {tf: sig for tf, sig in signals.items() 
                       if sig.signal_type == SignalType.SELL}
        
        # Calculate weighted signal strengths
        buy_strength = sum(sig.strength * self.timeframe_weights[tf] 
                          for tf, sig in buy_signals.items())
        sell_strength = sum(sig.strength * self.timeframe_weights[tf] 
                           for tf, sig in sell_signals.items())
        
        # Determine net signal
        net_strength = buy_strength - sell_strength
        
        if abs(net_strength) < 0.1:  # Threshold for hold
            unified_signal_type = SignalType.HOLD
            unified_strength = 0.0
        elif net_strength > 0:
            unified_signal_type = SignalType.BUY
            unified_strength = min(net_strength, 1.0)
        else:
            unified_signal_type = SignalType.SELL
            unified_strength = min(abs(net_strength), 1.0)
        
        # Calculate unified position size
        total_position_size = 0.0
        if unified_signal_type != SignalType.HOLD:
            # Sum position sizes weighted by confidence
            for tf, signal in signals.items():
                if signal.signal_type == unified_signal_type:
                    weight = self.timeframe_weights[tf] * signal.confidence
                    total_position_size += signal.position_size * weight
        
        # Apply risk limits
        total_position_size = min(total_position_size, self.max_total_position)
        
        # Calculate signal correlation
        signal_correlation = self._calculate_signal_correlation(signals)
        
        # Reduce position if signals highly correlated (potential false signal)
        if signal_correlation > self.correlation_threshold:
            correlation_penalty = (signal_correlation - self.correlation_threshold) / (1 - self.correlation_threshold)
            total_position_size *= (1 - correlation_penalty * 0.3)  # Up to 30% reduction
        
        # Forecast confidence weighting
        forecast_based_signals = [sig for sig in signals.values() if sig.forecast_based]
        if forecast_based_signals:
            avg_forecast_confidence = np.mean([sig.forecast_confidence for sig in forecast_based_signals])
            total_position_size *= avg_forecast_confidence
        
        return {
            'signal_type': unified_signal_type,
            'strength': unified_strength,
            'position_size': total_position_size,
            'buy_strength': buy_strength,
            'sell_strength': sell_strength,
            'signal_correlation': signal_correlation,
            'forecast_based_count': len(forecast_based_signals),
            'timeframe_signals': {tf: sig.signal_type.value for tf, sig in signals.items()}
        }
    
    def _calculate_signal_correlation(self, signals: Dict[str, TradingSignal]) -> float:
        """Calculate correlation between timeframe signals."""
        signal_strengths = []
        for tf in ['1min', '1hour', '1day', '1week']:
            if tf in signals:
                sig = signals[tf]
                if sig.signal_type == SignalType.BUY:
                    signal_strengths.append(sig.strength)
                elif sig.signal_type == SignalType.SELL:
                    signal_strengths.append(-sig.strength)
                else:
                    signal_strengths.append(0.0)
        
        if len(signal_strengths) < 2:
            return 0.0
        
        # Calculate correlation (simplified as standard deviation)
        # Higher std = lower correlation, lower std = higher correlation
        std_dev = np.std(signal_strengths)
        max_possible_std = 1.0  # Maximum possible standard deviation
        
        # Convert to correlation-like measure (0-1, where 1 = high correlation)
        correlation = max(0, 1 - (std_dev / max_possible_std))
        
        return correlation
    
    def get_portfolio_summary(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Get comprehensive portfolio summary with forecast integration."""
        # Generate all signals
        signals = self.generate_unified_signals(market_data)
        
        # Calculate unified position
        unified_position = self.calculate_unified_position(signals)
        
        # Forecast status summary
        forecast_status = {}
        if FORECASTS_AVAILABLE:
            try:
                reader = ETHForecastReader()
                forecast_status = reader.get_forecast_status()
            except Exception as e:
                forecast_status = {'error': str(e)}
        
        # Performance summary
        performance_summary = self._calculate_performance_summary()
        
        return {
            'timestamp': datetime.now(),
            'unified_position': unified_position,
            'timeframe_signals': {tf: {
                'signal_type': sig.signal_type.value,
                'strength': sig.strength,
                'confidence': sig.confidence,
                'position_size': sig.position_size,
                'forecast_based': sig.forecast_based,
                'risk_level': sig.risk_level.value
            } for tf, sig in signals.items()},
            'forecast_status': forecast_status,
            'performance': performance_summary,
            'risk_metrics': {
                'max_total_position': self.max_total_position,
                'signal_correlation': unified_position.get('signal_correlation', 0),
                'forecast_based_count': unified_position.get('forecast_based_count', 0)
            }
        }
    
    def _calculate_performance_summary(self) -> Dict[str, Any]:
        """Calculate overall portfolio performance summary."""
        if not self.portfolio_performance:
            return {'status': 'no_data'}
        
        recent_performance = self.portfolio_performance[-30:]  # Last 30 periods
        
        total_return = sum(recent_performance)
        avg_return = np.mean(recent_performance)
        volatility = np.std(recent_performance)
        sharpe_ratio = avg_return / volatility if volatility > 0 else 0
        
        win_rate = sum(1 for r in recent_performance if r > 0) / len(recent_performance)
        max_drawdown = self._calculate_max_drawdown(recent_performance)
        
        return {
            'total_return': total_return,
            'avg_return': avg_return,
            'volatility': volatility,
            'sharpe_ratio': sharpe_ratio,
            'win_rate': win_rate,
            'max_drawdown': max_drawdown,
            'sample_size': len(recent_performance)
        }
    
    def _calculate_max_drawdown(self, returns: List[float]) -> float:
        """Calculate maximum drawdown from returns."""
        cumulative = np.cumsum(returns)
        running_max = np.maximum.accumulate(cumulative)
        drawdown = cumulative - running_max
        return float(np.min(drawdown))
    
    def update_portfolio_performance(self, period_return: float):
        """Update portfolio performance tracking."""
        self.portfolio_performance.append(period_return)
        
        # Keep reasonable history
        if len(self.portfolio_performance) > 1000:
            self.portfolio_performance = self.portfolio_performance[-1000:]


# Usage example
if __name__ == "__main__":
    # Example usage
    portfolio_manager = MultiTimeframePortfolioManager()
    
    # Generate sample market data
    dates = pd.date_range(end=pd.Timestamp.now(), periods=100, freq='1H')
    np.random.seed(42)
    
    sample_data = pd.DataFrame({
        'open': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'high': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) + np.abs(np.random.randn(len(dates))),
        'low': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1) - np.abs(np.random.randn(len(dates))),
        'close': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.1),
        'volume': np.random.randint(1000, 10000, len(dates))
    }, index=dates)
    
    print("🚀 Multi-Timeframe Strategy Demo")
    print("=" * 40)
    
    # Get portfolio summary
    summary = portfolio_manager.get_portfolio_summary(sample_data)
    
    print(f"📊 Portfolio Summary ({summary['timestamp'].strftime('%Y-%m-%d %H:%M:%S')})")
    print(f"Unified Signal: {summary['unified_position']['signal_type']} "
          f"(strength: {summary['unified_position']['strength']:.3f})")
    print(f"Position Size: {summary['unified_position']['position_size']:.1%}")
    print(f"Signal Correlation: {summary['unified_position']['signal_correlation']:.3f}")
    
    print(f"\n📈 Timeframe Signals:")
    for tf, sig_data in summary['timeframe_signals'].items():
        forecast_indicator = "🔮" if sig_data['forecast_based'] else "📊"
        print(f"  {forecast_indicator} {tf:>5}: {sig_data['signal_type']:>4} "
              f"(str: {sig_data['strength']:.2f}, conf: {sig_data['confidence']:.2f})")
    
    if summary['forecast_status'] and 'error' not in summary['forecast_status']:
        print(f"\n🔮 Forecast Status:")
        for tf, status in summary['forecast_status'].items():
            availability = "✅" if status['available'] else "❌"
            print(f"  {tf:>5}: {availability} {status['quality']} "
                  f"(age: {status['age_minutes']:.1f}m)")
    
    print(f"\n⚡ Performance:")
    perf = summary['performance']
    if perf.get('status') != 'no_data':
        print(f"  Win Rate: {perf['win_rate']:.1%}")
        print(f"  Sharpe Ratio: {perf['sharpe_ratio']:.2f}")
        print(f"  Max Drawdown: {perf['max_drawdown']:.1%}")
