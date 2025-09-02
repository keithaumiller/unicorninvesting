"""
Multi-Timeframe ETH Trading Strategies for Myportolio

This module implements four different timeframe-based trading strategies:
1. ScalpStrategy (1-minute) - High-frequency scalping based on micro-movements
2. SwingStrategy (1-hour) - Medium-term momentum and mean reversion
3. PositionStrategy (1-day) - Longer-term trend following
4. InvestmentStrategy (1-week+) - Strategic position allocation

Each strategy leverages the corresponding Prophet, XGBoost, and Ensemble models
trained on the specific timeframe data.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime, timedelta
from abc import ABC, abstractmethod
import sys
import os
from pathlib import Path

# Add paths for model imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent))

try:
    from prophet import Prophet
except ImportError:
    print("⚠️  Prophet not installed. Install with: pip install prophet")

from ..utilities.kelly_criterion import KellyCriterion

class BaseMultiTimeframeStrategy(ABC):
    """
    Base class for all multi-timeframe trading strategies.
    """
    
    def __init__(self, timeframe: str, lookback_periods: int = 100, 
                 confidence_threshold: float = 0.65):
        self.timeframe = timeframe
        self.lookback_periods = lookback_periods
        self.confidence_threshold = confidence_threshold
        self.kelly_calculator = KellyCriterion()
        
        # Model paths based on timeframe
        self.model_dir = Path(__file__).parent.parent.parent.parent / "2_alpha_models" / "CRYPTO" / "ETH"
        
        # Performance tracking
        self.signal_history = []
        self.performance_metrics = {
            'total_signals': 0,
            'profitable_signals': 0,
            'win_rate': 0.0,
            'avg_return': 0.0,
            'sharpe_ratio': 0.0,
            'max_drawdown': 0.0
        }
        
    @abstractmethod
    def generate_signal(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Generate trading signal based on market data."""
        pass
    
    @abstractmethod
    def calculate_position_size(self, signal: Dict[str, Any], 
                              current_portfolio: Dict[str, float]) -> float:
        """Calculate optimal position size for the signal."""
        pass
    
    def _load_timeframe_models(self) -> Dict[str, Any]:
        """Load Prophet, XGBoost, and Ensemble models for this timeframe."""
        models = {
            'prophet': None,
            'xgboost': None,
            'ensemble': None,
            'loaded': False
        }
        
        try:
            # Load models specific to timeframe (to be implemented)
            # For now, use existing models and adapt
            models['loaded'] = True
        except Exception as e:
            print(f"Warning: Could not load models for {self.timeframe}: {e}")
            
        return models
    
    def _prepare_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """Prepare technical indicators and features for the timeframe."""
        df = data.copy()
        
        # Price-based features
        df['returns'] = df['close'].pct_change()
        df['log_returns'] = np.log(df['close'] / df['close'].shift(1))
        
        # Volatility features (adapted to timeframe)
        if self.timeframe == '1min':
            # High-frequency features
            df['volatility_5'] = df['returns'].rolling(5).std()
            df['volatility_15'] = df['returns'].rolling(15).std()
            df['rsi_fast'] = self._calculate_rsi(df['close'], 7)
            df['rsi_slow'] = self._calculate_rsi(df['close'], 14)
            
        elif self.timeframe == '1hour':
            # Medium-term features
            df['volatility_24'] = df['returns'].rolling(24).std()
            df['volatility_168'] = df['returns'].rolling(168).std()  # 1 week
            df['rsi_fast'] = self._calculate_rsi(df['close'], 14)
            df['rsi_slow'] = self._calculate_rsi(df['close'], 28)
            
        elif self.timeframe == '1day':
            # Daily features
            df['volatility_7'] = df['returns'].rolling(7).std()
            df['volatility_30'] = df['returns'].rolling(30).std()
            df['rsi_fast'] = self._calculate_rsi(df['close'], 14)
            df['rsi_slow'] = self._calculate_rsi(df['close'], 30)
            
        # Moving averages (adapted to timeframe)
        if self.timeframe == '1min':
            df['sma_fast'] = df['close'].rolling(5).mean()
            df['sma_slow'] = df['close'].rolling(20).mean()
            df['ema_fast'] = df['close'].ewm(span=8).mean()
            df['ema_slow'] = df['close'].ewm(span=21).mean()
        elif self.timeframe == '1hour':
            df['sma_fast'] = df['close'].rolling(12).mean()  # 12 hours
            df['sma_slow'] = df['close'].rolling(48).mean()   # 48 hours (2 days)
            df['ema_fast'] = df['close'].ewm(span=12).mean()
            df['ema_slow'] = df['close'].ewm(span=48).mean()
        elif self.timeframe == '1day':
            df['sma_fast'] = df['close'].rolling(7).mean()   # 1 week
            df['sma_slow'] = df['close'].rolling(30).mean()  # 1 month
            df['ema_fast'] = df['close'].ewm(span=10).mean()
            df['ema_slow'] = df['close'].ewm(span=30).mean()
            
        # Momentum indicators
        df['momentum'] = df['close'] / df['close'].shift(10) - 1
        df['rate_of_change'] = df['close'].pct_change(10)
        
        return df
    
    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate RSI indicator."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def update_performance(self, signal: Dict[str, Any], actual_return: float):
        """Update strategy performance metrics."""
        self.signal_history.append({
            'timestamp': datetime.now(),
            'signal': signal,
            'actual_return': actual_return
        })
        
        # Update metrics
        self.performance_metrics['total_signals'] += 1
        if actual_return > 0:
            self.performance_metrics['profitable_signals'] += 1
            
        # Calculate running metrics
        returns = [s['actual_return'] for s in self.signal_history]
        self.performance_metrics['win_rate'] = (
            self.performance_metrics['profitable_signals'] / 
            self.performance_metrics['total_signals']
        )
        self.performance_metrics['avg_return'] = np.mean(returns)
        
        if len(returns) > 1:
            self.performance_metrics['sharpe_ratio'] = (
                np.mean(returns) / np.std(returns) * np.sqrt(252)  # Annualized
            )
            
            # Calculate max drawdown
            cumulative = np.cumprod(1 + np.array(returns))
            running_max = np.maximum.accumulate(cumulative)
            drawdown = (cumulative - running_max) / running_max
            self.performance_metrics['max_drawdown'] = np.min(drawdown)


class ScalpStrategy(BaseMultiTimeframeStrategy):
    """
    1-Minute Scalping Strategy
    
    Focuses on:
    - Quick momentum reversals
    - Small, frequent profits
    - Tight risk management
    - High win rate, small average wins
    """
    
    def __init__(self, confidence_threshold: float = 0.70):
        super().__init__(timeframe='1min', lookback_periods=50, 
                        confidence_threshold=confidence_threshold)
        self.models = self._load_timeframe_models()
        
        # Scalping-specific parameters
        self.min_price_movement = 0.001  # 0.1% minimum movement
        self.max_hold_time = 5  # Maximum 5 minutes
        self.stop_loss = 0.002  # 0.2% stop loss
        self.take_profit = 0.003  # 0.3% take profit
        
    def generate_signal(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Generate 1-minute scalping signals."""
        if len(market_data) < self.lookback_periods:
            return {'action': 'HOLD', 'confidence': 0.0, 'reason': 'Insufficient data'}
            
        # Prepare features
        df = self._prepare_features(market_data)
        latest = df.iloc[-1]
        
        signal = {
            'action': 'HOLD',
            'confidence': 0.0,
            'timeframe': self.timeframe,
            'timestamp': datetime.now(),
            'features': {},
            'reasons': []
        }
        
        # Fast momentum detection
        momentum_score = 0
        
        # 1. Price momentum (last 5 minutes)
        recent_returns = df['returns'].iloc[-5:].sum()
        if abs(recent_returns) > self.min_price_movement:
            momentum_score += 0.3
            signal['features']['recent_momentum'] = recent_returns
            
        # 2. RSI divergence (oversold/overbought in short term)
        if latest['rsi_fast'] < 30 and latest['rsi_fast'] > df['rsi_fast'].iloc[-5]:
            momentum_score += 0.25
            signal['reasons'].append('RSI oversold reversal')
        elif latest['rsi_fast'] > 70 and latest['rsi_fast'] < df['rsi_fast'].iloc[-5]:
            momentum_score += 0.25
            signal['reasons'].append('RSI overbought reversal')
            
        # 3. Moving average crossover (very short term)
        if latest['ema_fast'] > latest['ema_slow'] and df['ema_fast'].iloc[-2] <= df['ema_slow'].iloc[-2]:
            momentum_score += 0.25
            signal['reasons'].append('EMA crossover bullish')
            signal['action'] = 'BUY'
        elif latest['ema_fast'] < latest['ema_slow'] and df['ema_fast'].iloc[-2] >= df['ema_slow'].iloc[-2]:
            momentum_score += 0.25
            signal['reasons'].append('EMA crossover bearish')
            signal['action'] = 'SELL'
            
        # 4. Volume spike (if available)
        if 'volume' in df.columns:
            vol_sma = df['volume'].rolling(20).mean().iloc[-1]
            if latest['volume'] > vol_sma * 1.5:
                momentum_score += 0.2
                signal['reasons'].append('Volume spike')
                
        signal['confidence'] = min(momentum_score, 1.0)
        signal['features']['momentum_score'] = momentum_score
        
        # Only act if confidence is high enough
        if signal['confidence'] < self.confidence_threshold:
            signal['action'] = 'HOLD'
            
        return signal
    
    def calculate_position_size(self, signal: Dict[str, Any], 
                              current_portfolio: Dict[str, float]) -> float:
        """Calculate position size for scalping (small, frequent trades)."""
        if signal['action'] == 'HOLD':
            return 0.0
            
        # Use Kelly Criterion but with conservative scaling for scalping
        kelly_result = self.kelly_calculator.calculate_kelly_fraction(
            signal_confidence=signal['confidence'],
            signal_type=signal['action']
        )
        
        # Scale down for scalping (max 2% of portfolio per trade)
        base_position = kelly_result.get('kelly_fraction', 0.1)
        scalping_position = min(base_position * 0.5, 0.02)  # Max 2%
        
        return scalping_position


class SwingStrategy(BaseMultiTimeframeStrategy):
    """
    1-Hour Swing Strategy
    
    Focuses on:
    - Medium-term momentum
    - Support/resistance levels
    - Trend continuation
    - Balanced risk/reward
    """
    
    def __init__(self, confidence_threshold: float = 0.65):
        super().__init__(timeframe='1hour', lookback_periods=168, 
                        confidence_threshold=confidence_threshold)
        self.models = self._load_timeframe_models()
        
        # Swing-specific parameters
        self.min_trend_strength = 0.02  # 2% minimum trend
        self.max_hold_time = 72  # Maximum 72 hours (3 days)
        self.stop_loss = 0.05  # 5% stop loss
        self.take_profit = 0.10  # 10% take profit
        
    def generate_signal(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Generate 1-hour swing trading signals."""
        if len(market_data) < self.lookback_periods:
            return {'action': 'HOLD', 'confidence': 0.0, 'reason': 'Insufficient data'}
            
        df = self._prepare_features(market_data)
        latest = df.iloc[-1]
        
        signal = {
            'action': 'HOLD',
            'confidence': 0.0,
            'timeframe': self.timeframe,
            'timestamp': datetime.now(),
            'features': {},
            'reasons': []
        }
        
        swing_score = 0
        
        # 1. Trend identification (24-hour trend)
        trend_24h = (latest['close'] / df['close'].iloc[-24] - 1) if len(df) >= 24 else 0
        signal['features']['trend_24h'] = trend_24h
        
        if abs(trend_24h) > self.min_trend_strength:
            swing_score += 0.3
            if trend_24h > 0:
                signal['reasons'].append('Strong 24h uptrend')
            else:
                signal['reasons'].append('Strong 24h downtrend')
                
        # 2. Moving average alignment
        ma_bullish = (latest['sma_fast'] > latest['sma_slow'] and 
                     latest['close'] > latest['sma_fast'])
        ma_bearish = (latest['sma_fast'] < latest['sma_slow'] and 
                     latest['close'] < latest['sma_fast'])
        
        if ma_bullish:
            swing_score += 0.25
            signal['reasons'].append('MA alignment bullish')
            if signal['action'] == 'HOLD':
                signal['action'] = 'BUY'
        elif ma_bearish:
            swing_score += 0.25
            signal['reasons'].append('MA alignment bearish')
            if signal['action'] == 'HOLD':
                signal['action'] = 'SELL'
                
        # 3. RSI momentum
        if 30 < latest['rsi_fast'] < 70:  # Not in extreme territory
            if latest['rsi_fast'] > 50 and trend_24h > 0:
                swing_score += 0.2
                signal['reasons'].append('RSI momentum bullish')
            elif latest['rsi_fast'] < 50 and trend_24h < 0:
                swing_score += 0.2
                signal['reasons'].append('RSI momentum bearish')
                
        # 4. Volatility consideration
        current_vol = latest['volatility_24']
        avg_vol = df['volatility_24'].rolling(48).mean().iloc[-1]
        
        if current_vol < avg_vol * 1.5:  # Not too volatile
            swing_score += 0.15
            signal['reasons'].append('Volatility acceptable')
            
        # 5. Volume confirmation (if available)
        if 'volume' in df.columns:
            vol_trend = df['volume'].rolling(12).mean().iloc[-1] / df['volume'].rolling(24).mean().iloc[-1]
            if vol_trend > 1.1:
                swing_score += 0.1
                signal['reasons'].append('Volume confirming')
                
        signal['confidence'] = min(swing_score, 1.0)
        signal['features']['swing_score'] = swing_score
        
        if signal['confidence'] < self.confidence_threshold:
            signal['action'] = 'HOLD'
            
        return signal
    
    def calculate_position_size(self, signal: Dict[str, Any], 
                              current_portfolio: Dict[str, float]) -> float:
        """Calculate position size for swing trading."""
        if signal['action'] == 'HOLD':
            return 0.0
            
        kelly_result = self.kelly_calculator.calculate_kelly_fraction(
            signal_confidence=signal['confidence'],
            signal_type=signal['action']
        )
        
        # Moderate position sizing for swing trades (max 10% per trade)
        base_position = kelly_result.get('kelly_fraction', 0.1)
        swing_position = min(base_position * 0.8, 0.10)  # Max 10%
        
        return swing_position


class PositionStrategy(BaseMultiTimeframeStrategy):
    """
    1-Day Position Strategy
    
    Focuses on:
    - Long-term trends
    - Fundamental momentum
    - Lower frequency, higher conviction
    - Strategic allocation
    """
    
    def __init__(self, confidence_threshold: float = 0.60):
        super().__init__(timeframe='1day', lookback_periods=90, 
                        confidence_threshold=confidence_threshold)
        self.models = self._load_timeframe_models()
        
        # Position-specific parameters
        self.min_trend_strength = 0.05  # 5% minimum trend
        self.max_hold_time = 30  # Maximum 30 days
        self.stop_loss = 0.15  # 15% stop loss
        self.take_profit = 0.25  # 25% take profit
        
    def generate_signal(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Generate daily position trading signals."""
        if len(market_data) < self.lookback_periods:
            return {'action': 'HOLD', 'confidence': 0.0, 'reason': 'Insufficient data'}
            
        df = self._prepare_features(market_data)
        latest = df.iloc[-1]
        
        signal = {
            'action': 'HOLD',
            'confidence': 0.0,
            'timeframe': self.timeframe,
            'timestamp': datetime.now(),
            'features': {},
            'reasons': []
        }
        
        position_score = 0
        
        # 1. Long-term trend (30-day)
        trend_30d = (latest['close'] / df['close'].iloc[-30] - 1) if len(df) >= 30 else 0
        signal['features']['trend_30d'] = trend_30d
        
        if abs(trend_30d) > self.min_trend_strength:
            position_score += 0.35
            if trend_30d > 0:
                signal['reasons'].append('Strong 30-day uptrend')
                signal['action'] = 'BUY'
            else:
                signal['reasons'].append('Strong 30-day downtrend')
                signal['action'] = 'SELL'
                
        # 2. Moving average trend
        ma_trend = (latest['sma_fast'] / latest['sma_slow'] - 1)
        signal['features']['ma_trend'] = ma_trend
        
        if ma_trend > 0.02:  # 2% above long MA
            position_score += 0.25
            signal['reasons'].append('Above long-term MA')
        elif ma_trend < -0.02:  # 2% below long MA
            position_score += 0.25
            signal['reasons'].append('Below long-term MA')
            
        # 3. Momentum consistency (trend in multiple timeframes)
        trend_7d = (latest['close'] / df['close'].iloc[-7] - 1) if len(df) >= 7 else 0
        trend_14d = (latest['close'] / df['close'].iloc[-14] - 1) if len(df) >= 14 else 0
        
        trends_aligned = (
            (trend_7d > 0 and trend_14d > 0 and trend_30d > 0) or
            (trend_7d < 0 and trend_14d < 0 and trend_30d < 0)
        )
        
        if trends_aligned:
            position_score += 0.2
            signal['reasons'].append('Multi-timeframe alignment')
            
        # 4. Volatility regime
        current_vol = latest['volatility_30']
        long_term_vol = df['volatility_30'].rolling(60).mean().iloc[-1]
        
        if current_vol < long_term_vol * 1.2:  # Not in high vol regime
            position_score += 0.15
            signal['reasons'].append('Stable volatility regime')
            
        # 5. RSI not in extreme territory
        if 25 < latest['rsi_slow'] < 75:
            position_score += 0.05
            signal['reasons'].append('RSI in tradable range')
            
        signal['confidence'] = min(position_score, 1.0)
        signal['features']['position_score'] = position_score
        
        if signal['confidence'] < self.confidence_threshold:
            signal['action'] = 'HOLD'
            
        return signal
    
    def calculate_position_size(self, signal: Dict[str, Any], 
                              current_portfolio: Dict[str, float]) -> float:
        """Calculate position size for position trading."""
        if signal['action'] == 'HOLD':
            return 0.0
            
        kelly_result = self.kelly_calculator.calculate_kelly_fraction(
            signal_confidence=signal['confidence'],
            signal_type=signal['action']
        )
        
        # Larger position sizing for position trades (max 25% per trade)
        base_position = kelly_result.get('kelly_fraction', 0.1)
        position_size = min(base_position, 0.25)  # Max 25%
        
        return position_size


class InvestmentStrategy(BaseMultiTimeframeStrategy):
    """
    Weekly/Monthly Investment Strategy
    
    Focuses on:
    - Strategic allocation
    - Long-term value
    - Portfolio balance
    - Lower turnover
    """
    
    def __init__(self, confidence_threshold: float = 0.55):
        super().__init__(timeframe='1week', lookback_periods=52, 
                        confidence_threshold=confidence_threshold)
        self.models = self._load_timeframe_models()
        
        # Investment-specific parameters
        self.min_allocation_change = 0.05  # 5% minimum allocation change
        self.rebalance_threshold = 0.10  # 10% drift before rebalance
        
    def generate_signal(self, market_data: pd.DataFrame) -> Dict[str, Any]:
        """Generate strategic investment allocation signals."""
        if len(market_data) < self.lookback_periods:
            return {'action': 'HOLD', 'confidence': 0.0, 'reason': 'Insufficient data'}
            
        # Resample to weekly data
        df = market_data.resample('W').agg({
            'open': 'first',
            'high': 'max',
            'low': 'min',
            'close': 'last',
            'volume': 'sum' if 'volume' in market_data.columns else 'mean'
        }).dropna()
        
        df = self._prepare_features(df)
        latest = df.iloc[-1]
        
        signal = {
            'action': 'REBALANCE',
            'confidence': 0.0,
            'timeframe': self.timeframe,
            'timestamp': datetime.now(),
            'features': {},
            'allocation_target': 0.60,  # Default 60% ETH allocation
            'reasons': []
        }
        
        allocation_score = 0
        
        # 1. Long-term trend (6 months)
        trend_26w = (latest['close'] / df['close'].iloc[-26] - 1) if len(df) >= 26 else 0
        signal['features']['trend_26w'] = trend_26w
        
        # Adjust allocation based on long-term trend
        if trend_26w > 0.20:  # Strong 6-month uptrend
            signal['allocation_target'] = 0.70  # Increase to 70%
            allocation_score += 0.4
            signal['reasons'].append('Strong 6-month uptrend - increase allocation')
        elif trend_26w < -0.20:  # Strong 6-month downtrend
            signal['allocation_target'] = 0.45  # Decrease to 45%
            allocation_score += 0.4
            signal['reasons'].append('Strong 6-month downtrend - decrease allocation')
        else:
            signal['allocation_target'] = 0.60  # Maintain 60%
            allocation_score += 0.2
            
        # 2. Volatility-adjusted allocation
        current_vol = df['volatility_7'].iloc[-1]
        avg_vol = df['volatility_7'].rolling(26).mean().iloc[-1]
        
        vol_ratio = current_vol / avg_vol if avg_vol > 0 else 1
        
        if vol_ratio > 1.5:  # High volatility - reduce allocation
            signal['allocation_target'] *= 0.85
            allocation_score += 0.2
            signal['reasons'].append('High volatility - reduce allocation')
        elif vol_ratio < 0.7:  # Low volatility - can increase allocation
            signal['allocation_target'] *= 1.1
            allocation_score += 0.2
            signal['reasons'].append('Low volatility - increase allocation')
            
        # 3. Risk-adjusted momentum
        sharpe_estimate = (
            df['returns'].rolling(12).mean().iloc[-1] / 
            df['returns'].rolling(12).std().iloc[-1]
        ) if len(df) >= 12 else 0
        
        if sharpe_estimate > 1.0:
            allocation_score += 0.2
            signal['reasons'].append('Strong risk-adjusted returns')
        elif sharpe_estimate < -0.5:
            allocation_score += 0.2
            signal['reasons'].append('Poor risk-adjusted returns')
            
        # Cap allocation at reasonable bounds
        signal['allocation_target'] = max(0.20, min(0.80, signal['allocation_target']))
        
        signal['confidence'] = min(allocation_score, 1.0)
        signal['features']['allocation_score'] = allocation_score
        
        return signal
    
    def calculate_position_size(self, signal: Dict[str, Any], 
                              current_portfolio: Dict[str, float]) -> float:
        """Calculate rebalancing position size."""
        current_eth_allocation = current_portfolio.get('ETH', 0.60)
        target_allocation = signal.get('allocation_target', 0.60)
        
        # Return the difference that needs to be traded
        allocation_change = target_allocation - current_eth_allocation
        
        # Only rebalance if change is significant
        if abs(allocation_change) > self.min_allocation_change:
            return allocation_change
        else:
            return 0.0


class MultiTimeframePortfolioManager:
    """
    Coordinates multiple timeframe strategies for the Myportolio.
    """
    
    def __init__(self):
        self.strategies = {
            '1min': ScalpStrategy(),
            '1hour': SwingStrategy(), 
            '1day': PositionStrategy(),
            '1week': InvestmentStrategy()
        }
        
        self.active_positions = {
            'scalp': [],
            'swing': [],
            'position': [],
            'investment': 0.60  # Current ETH allocation
        }
        
    def generate_unified_signal(self, market_data_1min: pd.DataFrame,
                               market_data_1hour: pd.DataFrame,
                               market_data_1day: pd.DataFrame) -> Dict[str, Any]:
        """Generate unified signal from all timeframe strategies."""
        
        # Get signals from each strategy
        signals = {}
        
        # Resample 1-min data for other timeframes if needed
        if len(market_data_1hour) < 100:
            market_data_1hour = market_data_1min.resample('1H').agg({
                'open': 'first', 'high': 'max', 'low': 'min', 'close': 'last',
                'volume': 'sum' if 'volume' in market_data_1min.columns else 'mean'
            }).dropna()
            
        if len(market_data_1day) < 50:
            market_data_1day = market_data_1min.resample('1D').agg({
                'open': 'first', 'high': 'max', 'low': 'min', 'close': 'last',
                'volume': 'sum' if 'volume' in market_data_1min.columns else 'mean'
            }).dropna()
        
        signals['scalp'] = self.strategies['1min'].generate_signal(market_data_1min)
        signals['swing'] = self.strategies['1hour'].generate_signal(market_data_1hour)
        signals['position'] = self.strategies['1day'].generate_signal(market_data_1day)
        signals['investment'] = self.strategies['1week'].generate_signal(market_data_1day)
        
        # Combine signals with weighting
        unified_signal = {
            'timestamp': datetime.now(),
            'timeframes': signals,
            'unified_action': 'HOLD',
            'unified_confidence': 0.0,
            'allocation_changes': {},
            'execution_plan': []
        }
        
        # Weight the signals by timeframe importance and confidence
        weights = {
            'scalp': 0.15,    # 15% weight for scalping
            'swing': 0.35,    # 35% weight for swing
            'position': 0.35,  # 35% weight for position
            'investment': 0.15  # 15% weight for strategic
        }
        
        weighted_confidence = 0
        buy_pressure = 0
        sell_pressure = 0
        
        for timeframe, signal in signals.items():
            weight = weights[timeframe]
            confidence = signal['confidence']
            
            weighted_confidence += weight * confidence
            
            if signal['action'] == 'BUY':
                buy_pressure += weight * confidence
            elif signal['action'] == 'SELL':
                sell_pressure += weight * confidence
                
        unified_signal['unified_confidence'] = weighted_confidence
        
        # Determine unified action
        if buy_pressure > sell_pressure and buy_pressure > 0.3:
            unified_signal['unified_action'] = 'BUY'
        elif sell_pressure > buy_pressure and sell_pressure > 0.3:
            unified_signal['unified_action'] = 'SELL'
        else:
            unified_signal['unified_action'] = 'HOLD'
            
        # Strategic allocation from investment strategy
        if signals['investment']['action'] == 'REBALANCE':
            unified_signal['allocation_changes']['ETH'] = signals['investment']['allocation_target']
            
        return unified_signal
    
    def execute_unified_strategy(self, unified_signal: Dict[str, Any], 
                                current_portfolio: Dict[str, float]) -> List[Dict[str, Any]]:
        """Execute the unified multi-timeframe strategy."""
        execution_orders = []
        
        # Strategic rebalancing (lowest frequency, highest priority)
        if 'ETH' in unified_signal.get('allocation_changes', {}):
            target_allocation = unified_signal['allocation_changes']['ETH']
            current_allocation = current_portfolio.get('ETH', 0.60)
            
            rebalance_amount = target_allocation - current_allocation
            if abs(rebalance_amount) > 0.05:  # 5% threshold
                execution_orders.append({
                    'type': 'REBALANCE',
                    'symbol': 'ETH',
                    'amount': rebalance_amount,
                    'reason': 'Strategic allocation adjustment',
                    'timeframe': 'investment',
                    'priority': 1
                })
        
        # Tactical position adjustments
        if unified_signal['unified_action'] in ['BUY', 'SELL']:
            # Calculate position sizes from each strategy
            scalp_size = self.strategies['1min'].calculate_position_size(
                unified_signal['timeframes']['scalp'], current_portfolio)
            swing_size = self.strategies['1hour'].calculate_position_size(
                unified_signal['timeframes']['swing'], current_portfolio)
            position_size = self.strategies['1day'].calculate_position_size(
                unified_signal['timeframes']['position'], current_portfolio)
            
            # Aggregate tactical position
            total_tactical = scalp_size + swing_size + position_size
            
            if abs(total_tactical) > 0.01:  # 1% minimum
                execution_orders.append({
                    'type': 'TACTICAL',
                    'symbol': 'ETH',
                    'action': unified_signal['unified_action'],
                    'amount': total_tactical,
                    'confidence': unified_signal['unified_confidence'],
                    'breakdown': {
                        'scalp': scalp_size,
                        'swing': swing_size, 
                        'position': position_size
                    },
                    'timeframe': 'tactical',
                    'priority': 2
                })
        
        return execution_orders


# Usage example and testing
if __name__ == "__main__":
    # Example usage
    portfolio_manager = MultiTimeframePortfolioManager()
    
    # Generate sample data for testing
    dates = pd.date_range(start='2025-01-01', end='2025-09-02', freq='1min')
    np.random.seed(42)
    
    sample_data = pd.DataFrame({
        'open': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.5),
        'high': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.5) + np.abs(np.random.randn(len(dates)) * 2),
        'low': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.5) - np.abs(np.random.randn(len(dates)) * 2),
        'close': 3000 + np.cumsum(np.random.randn(len(dates)) * 0.5),
        'volume': np.random.randint(1000, 10000, len(dates))
    }, index=dates)
    
    # Test unified signal generation
    unified_signal = portfolio_manager.generate_unified_signal(
        sample_data,
        sample_data.resample('1H').agg({'open': 'first', 'high': 'max', 'low': 'min', 'close': 'last', 'volume': 'sum'}).dropna(),
        sample_data.resample('1D').agg({'open': 'first', 'high': 'max', 'low': 'min', 'close': 'last', 'volume': 'sum'}).dropna()
    )
    
    print("Multi-Timeframe Strategy Test:")
    print(f"Unified Action: {unified_signal['unified_action']}")
    print(f"Unified Confidence: {unified_signal['unified_confidence']:.3f}")
    print("\nTimeframe Breakdown:")
    for tf, signal in unified_signal['timeframes'].items():
        print(f"  {tf}: {signal['action']} (confidence: {signal['confidence']:.3f})")
