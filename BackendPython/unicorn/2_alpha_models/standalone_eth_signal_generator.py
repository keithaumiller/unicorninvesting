"""
Standalone ETH Technical Analysis Signal Generator
=================================================

Standalone implementation of ETH technical analysis using Phase 1 indicators
without LEAN framework dependencies. This demonstrates the core signal
generation logic that will be integrated into LEAN.

Features:
- 30+ technical indicators from Phase 1
- Multi-component signal ensemble
- Confidence scoring system
- Real-time signal generation
- Performance optimization

Use Cases:
- Testing signal generation logic
- Research and backtesting
- Integration validation
- Standalone analysis
"""

import numpy as np
import sys
import os
from datetime import datetime, timedelta
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple

# Add path to Phase 1 technical indicators
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

try:
    from technical_indicators import (
        CircularBuffer, IndicatorValue,
        # Trend Indicators
        simple_moving_average, exponential_moving_average, macd_calculation,
        # Momentum Indicators  
        rsi_calculation, stochastic_oscillator, williams_r, rate_of_change,
        # Volatility Indicators
        bollinger_bands, atr_calculation, keltner_channels,
        # Volume Indicators
        vwap_calculation, obv_calculation, mfi_calculation, volume_sma
    )
    INDICATORS_AVAILABLE = True
except ImportError as e:
    print(f"⚠️ Technical indicators not available: {e}")
    INDICATORS_AVAILABLE = False

@dataclass
class MarketData:
    """Market data point for ETH."""
    timestamp: datetime
    open: float
    high: float
    low: float
    close: float
    volume: float

@dataclass 
class TechnicalSignal:
    """Technical analysis signal result."""
    timestamp: datetime
    symbol: str
    signal_strength: float  # -1 to +1 (sell to buy)
    confidence: float       # 0 to 1
    expected_return: float  # Expected return percentage
    components: Dict[str, float]  # Individual signal components
    active_indicators: List[str]  # Which indicators contributed
    signal_quality: str     # "HIGH", "MEDIUM", "LOW"

class StandaloneETHSignalGenerator:
    """
    Standalone ETH technical analysis signal generator.
    
    This class implements the same signal generation logic as the LEAN
    alpha model but without LEAN dependencies for testing and research.
    """
    
    def __init__(self, 
                 lookback_periods: int = 200,
                 confidence_threshold: float = 0.015,
                 min_indicators_required: int = 5):
        """
        Initialize the signal generator.
        
        Args:
            lookback_periods: Number of historical periods to maintain
            confidence_threshold: Minimum confidence to generate signal
            min_indicators_required: Minimum number of indicators needed
        """
        self.lookback_periods = lookback_periods
        self.confidence_threshold = confidence_threshold
        self.min_indicators_required = min_indicators_required
        
        # Data storage
        self.price_buffer = CircularBuffer(lookback_periods)
        self.high_buffer = CircularBuffer(lookback_periods)
        self.low_buffer = CircularBuffer(lookback_periods)
        self.volume_buffer = CircularBuffer(lookback_periods)
        
        # Performance tracking
        self.signals_generated = 0
        self.last_signal_time = None
        self.performance_metrics = {
            'total_signals': 0,
            'high_confidence_signals': 0,
            'signal_distribution': {'BUY': 0, 'SELL': 0, 'NEUTRAL': 0}
        }
        
        self.indicators_ready = INDICATORS_AVAILABLE
        
    def add_market_data(self, data: MarketData) -> None:
        """Add new market data point to buffers."""
        self.price_buffer.append(data.close)
        self.high_buffer.append(data.high)
        self.low_buffer.append(data.low)
        self.volume_buffer.append(data.volume)
        
    def has_sufficient_data(self) -> bool:
        """Check if we have sufficient data for signal generation."""
        return (self.price_buffer.count >= 50 and
                self.volume_buffer.count >= 50 and
                self.indicators_ready)
    
    def generate_signal(self, current_data: MarketData) -> Optional[TechnicalSignal]:
        """
        Generate technical analysis signal for current market data.
        
        Returns:
            TechnicalSignal object or None if insufficient data/confidence
        """
        if not self.has_sufficient_data():
            return None
            
        try:
            # Add current data
            self.add_market_data(current_data)
            
            # Get data arrays
            prices = np.array(self.price_buffer.to_array())
            highs = np.array(self.high_buffer.to_array())
            lows = np.array(self.low_buffer.to_array())
            volumes = np.array(self.volume_buffer.to_array())
            
            # Calculate signal components
            trend_signals = self._calculate_trend_signals(prices, highs, lows, volumes)
            momentum_signals = self._calculate_momentum_signals(prices, highs, lows)
            volatility_signals = self._calculate_volatility_signals(prices, highs, lows)
            volume_signals = self._calculate_volume_signals(prices, volumes)
            
            # Combine all signals
            combined_signal = self._combine_signals({
                'trend': trend_signals,
                'momentum': momentum_signals,
                'volatility': volatility_signals,
                'volume': volume_signals
            }, current_data.close)
            
            if combined_signal and combined_signal['confidence'] >= self.confidence_threshold:
                signal = TechnicalSignal(
                    timestamp=current_data.timestamp,
                    symbol="ETHUSD",
                    signal_strength=combined_signal['signal_strength'],
                    confidence=combined_signal['confidence'],
                    expected_return=combined_signal['expected_return'],
                    components=combined_signal['components'],
                    active_indicators=combined_signal['active_indicators'],
                    signal_quality=self._determine_signal_quality(combined_signal['confidence'])
                )
                
                self._update_performance_metrics(signal)
                return signal
                
        except Exception as e:
            print(f"⚠️ Error generating signal: {e}")
            
        return None
    
    def _calculate_trend_signals(self, prices, highs, lows, volumes) -> List[Dict]:
        """Calculate trend-based signals."""
        signals = []
        
        if not self.indicators_ready:
            return signals
            
        try:
            # Simple Moving Averages
            sma_10 = simple_moving_average(prices, 10)
            sma_20 = simple_moving_average(prices, 20)
            sma_50 = simple_moving_average(prices, 50)
            
            if sma_10.is_valid and sma_20.is_valid and sma_50.is_valid:
                # SMA alignment signal
                if sma_10.value > sma_20.value > sma_50.value:
                    signals.append({
                        'name': 'sma_bullish_alignment',
                        'value': 0.3,
                        'weight': 0.4
                    })
                elif sma_10.value < sma_20.value < sma_50.value:
                    signals.append({
                        'name': 'sma_bearish_alignment', 
                        'value': -0.3,
                        'weight': 0.4
                    })
                    
                # SMA momentum
                sma_momentum = (sma_10.value - sma_20.value) / sma_20.value
                if abs(sma_momentum) > 0.01:
                    signals.append({
                        'name': 'sma_momentum',
                        'value': np.tanh(sma_momentum * 10),
                        'weight': 0.3
                    })
            
            # Exponential Moving Averages
            ema_12 = exponential_moving_average(prices, 12)
            ema_26 = exponential_moving_average(prices, 26)
            
            if ema_12.is_valid and ema_26.is_valid:
                ema_signal = (ema_12.value - ema_26.value) / ema_26.value
                if abs(ema_signal) > 0.005:
                    signals.append({
                        'name': 'ema_crossover',
                        'value': np.tanh(ema_signal * 8),
                        'weight': 0.35
                    })
            
            # MACD
            macd_result = macd_calculation(prices, 12, 26, 9)
            if macd_result.is_valid and hasattr(macd_result, 'macd') and hasattr(macd_result, 'signal'):
                macd_diff = macd_result.macd - macd_result.signal
                if abs(macd_diff) > 0.5:
                    signals.append({
                        'name': 'macd_crossover',
                        'value': np.tanh(macd_diff / 10),
                        'weight': 0.35
                    })
                    
        except Exception as e:
            pass  # Continue with other indicators
            
        return signals
    
    def _calculate_momentum_signals(self, prices, highs, lows) -> List[Dict]:
        """Calculate momentum-based signals."""
        signals = []
        
        if not self.indicators_ready:
            return signals
            
        try:
            # RSI
            rsi = rsi_calculation(prices, 14)
            if rsi.is_valid:
                if rsi.value < 30:
                    signals.append({
                        'name': 'rsi_oversold',
                        'value': (30 - rsi.value) / 30 * 0.4,
                        'weight': 0.4
                    })
                elif rsi.value > 70:
                    signals.append({
                        'name': 'rsi_overbought',
                        'value': -(rsi.value - 70) / 30 * 0.4,
                        'weight': 0.4
                    })
                    
                # RSI momentum
                rsi_momentum = (rsi.value - 50) / 50
                signals.append({
                    'name': 'rsi_momentum',
                    'value': rsi_momentum * 0.2,
                    'weight': 0.25
                })
            
            # Stochastic
            stoch = stochastic_oscillator(highs, lows, prices, 14, 3)
            if stoch.is_valid:
                if stoch.value < 20:
                    signals.append({
                        'name': 'stoch_oversold',
                        'value': (20 - stoch.value) / 20 * 0.3,
                        'weight': 0.3
                    })
                elif stoch.value > 80:
                    signals.append({
                        'name': 'stoch_overbought',
                        'value': -(stoch.value - 80) / 20 * 0.3,
                        'weight': 0.3
                    })
            
            # Williams %R
            williams = williams_r(highs, lows, prices, 14)
            if williams.is_valid:
                if williams.value > -20:
                    signals.append({
                        'name': 'williams_overbought',
                        'value': (williams.value + 20) / 20 * -0.25,
                        'weight': 0.25
                    })
                elif williams.value < -80:
                    signals.append({
                        'name': 'williams_oversold',
                        'value': (-80 - williams.value) / 20 * 0.25,
                        'weight': 0.25
                    })
            
            # Rate of Change
            roc = rate_of_change(prices, 10)
            if roc.is_valid and abs(roc.value) > 1:
                signals.append({
                    'name': 'rate_of_change',
                    'value': np.tanh(roc.value / 50),
                    'weight': 0.3
                })
                
        except Exception as e:
            pass
            
        return signals
    
    def _calculate_volatility_signals(self, prices, highs, lows) -> List[Dict]:
        """Calculate volatility-based signals."""
        signals = []
        
        if not self.indicators_ready:
            return signals
            
        try:
            current_price = prices[-1]
            
            # Bollinger Bands
            bb = bollinger_bands(prices, 20, 2)
            if bb.is_valid:
                bb_position = (current_price - bb.lower) / (bb.upper - bb.lower)
                
                if bb_position < 0.2:
                    signals.append({
                        'name': 'bb_oversold',
                        'value': (0.2 - bb_position) * 2,
                        'weight': 0.4
                    })
                elif bb_position > 0.8:
                    signals.append({
                        'name': 'bb_overbought',
                        'value': -(bb_position - 0.8) * 2,
                        'weight': 0.4
                    })
                    
                # Bollinger squeeze
                bb_width = (bb.upper - bb.lower) / bb.middle
                if bb_width < 0.1:
                    price_momentum = (current_price - bb.middle) / bb.middle
                    signals.append({
                        'name': 'bb_squeeze',
                        'value': price_momentum * 0.5,
                        'weight': 0.3
                    })
            
            # ATR-based volatility signal
            atr = atr_calculation(highs, lows, prices, 14)
            if atr.is_valid:
                atr_percent = atr.value / current_price
                if atr_percent > 0.05:
                    signals.append({
                        'name': 'high_volatility',
                        'value': -0.1,
                        'weight': 0.2
                    })
                elif atr_percent < 0.02:
                    signals.append({
                        'name': 'low_volatility',
                        'value': 0.1,
                        'weight': 0.2
                    })
            
            # Keltner Channels
            keltner = keltner_channels(highs, lows, prices, 20, 2)
            if keltner.is_valid:
                if current_price > keltner.upper:
                    signals.append({
                        'name': 'keltner_breakout_sell',
                        'value': -0.2,
                        'weight': 0.3
                    })
                elif current_price < keltner.lower:
                    signals.append({
                        'name': 'keltner_breakout_buy',
                        'value': 0.2,
                        'weight': 0.3
                    })
                    
        except Exception as e:
            pass
            
        return signals
    
    def _calculate_volume_signals(self, prices, volumes) -> List[Dict]:
        """Calculate volume-based signals."""
        signals = []
        
        if not self.indicators_ready:
            return signals
            
        try:
            current_price = prices[-1]
            
            # VWAP
            vwap = vwap_calculation(prices, volumes)
            if vwap.is_valid:
                vwap_deviation = (current_price - vwap.value) / vwap.value
                if abs(vwap_deviation) > 0.01:
                    signals.append({
                        'name': 'vwap_deviation',
                        'value': vwap_deviation * 5,
                        'weight': 0.4
                    })
            
            # Volume SMA
            vol_sma = volume_sma(volumes, 20)
            if vol_sma.is_valid:
                current_volume = volumes[-1]
                volume_ratio = current_volume / vol_sma.value
                if volume_ratio > 2:
                    signals.append({
                        'name': 'high_volume',
                        'value': 0.15,
                        'weight': 0.3
                    })
                elif volume_ratio < 0.5:
                    signals.append({
                        'name': 'low_volume',
                        'value': -0.1,
                        'weight': 0.2
                    })
                    
        except Exception as e:
            pass
            
        return signals
    
    def _combine_signals(self, signal_categories: Dict[str, List[Dict]], current_price: float) -> Optional[Dict]:
        """Combine all signal categories into final signal."""
        category_weights = {
            'trend': 0.40,
            'momentum': 0.30,
            'volume': 0.20,
            'volatility': 0.10
        }
        
        total_signal = 0
        total_weight = 0
        components = {}
        active_indicators = []
        
        for category, signals in signal_categories.items():
            if signals:
                category_signal = 0
                category_weight = 0
                
                for signal in signals:
                    signal_value = signal['value']
                    signal_weight = signal['weight']
                    
                    category_signal += signal_value * signal_weight
                    category_weight += signal_weight
                    active_indicators.append(signal['name'])
                
                if category_weight > 0:
                    avg_category_signal = category_signal / category_weight
                    weighted_signal = avg_category_signal * category_weights[category]
                    
                    total_signal += weighted_signal
                    total_weight += category_weights[category]
                    components[category] = avg_category_signal
        
        if total_weight == 0 or len(active_indicators) < self.min_indicators_required:
            return None
        
        # Normalize signal
        signal_strength = total_signal / total_weight
        signal_strength = max(-1, min(1, signal_strength))  # Clamp to [-1, 1]
        
        # Calculate confidence based on signal strength and indicator count
        confidence = min(len(active_indicators) / 10.0, 1.0)  # More indicators = higher confidence
        confidence *= min(abs(signal_strength) * 2, 1.0)  # Stronger signal = higher confidence
        confidence = min(confidence, 0.95)  # Cap at 95%
        
        # Expected return is proportional to signal strength
        expected_return = signal_strength * 0.05  # Max 5% expected return
        
        return {
            'signal_strength': signal_strength,
            'confidence': confidence,
            'expected_return': expected_return,
            'components': components,
            'active_indicators': active_indicators
        }
    
    def _determine_signal_quality(self, confidence: float) -> str:
        """Determine signal quality based on confidence."""
        if confidence >= 0.7:
            return "HIGH"
        elif confidence >= 0.4:
            return "MEDIUM"
        else:
            return "LOW"
    
    def _update_performance_metrics(self, signal: TechnicalSignal) -> None:
        """Update performance tracking metrics."""
        self.signals_generated += 1
        self.last_signal_time = signal.timestamp
        
        self.performance_metrics['total_signals'] += 1
        
        if signal.confidence >= 0.6:
            self.performance_metrics['high_confidence_signals'] += 1
        
        if signal.signal_strength > 0.1:
            self.performance_metrics['signal_distribution']['BUY'] += 1
        elif signal.signal_strength < -0.1:
            self.performance_metrics['signal_distribution']['SELL'] += 1
        else:
            self.performance_metrics['signal_distribution']['NEUTRAL'] += 1
    
    def get_performance_summary(self) -> Dict:
        """Get performance summary statistics."""
        return {
            'signals_generated': self.signals_generated,
            'last_signal_time': self.last_signal_time,
            'indicators_available': self.indicators_ready,
            'data_points': self.price_buffer.count,
            'metrics': self.performance_metrics.copy()
        }
    
    def reset(self) -> None:
        """Reset all buffers and performance metrics."""
        self.price_buffer = CircularBuffer(self.lookback_periods)
        self.high_buffer = CircularBuffer(self.lookback_periods)
        self.low_buffer = CircularBuffer(self.lookback_periods)
        self.volume_buffer = CircularBuffer(self.lookback_periods)
        
        self.signals_generated = 0
        self.last_signal_time = None
        self.performance_metrics = {
            'total_signals': 0,
            'high_confidence_signals': 0,
            'signal_distribution': {'BUY': 0, 'SELL': 0, 'NEUTRAL': 0}
        }

# Demo function to show usage
def demo_signal_generation():
    """Demonstrate signal generation with sample data."""
    print("🚀 ETH Technical Analysis Signal Generator Demo")
    print("=" * 60)
    
    generator = StandaloneETHSignalGenerator()
    
    # Generate sample ETH price data (simulated trending market)
    base_price = 2500.0
    base_time = datetime.now()
    
    print(f"📊 Indicators Available: {generator.indicators_ready}")
    print(f"🎯 Confidence Threshold: {generator.confidence_threshold:.1%}")
    print(f"📈 Generating signals for simulated ETH data...")
    print()
    
    signals_generated = []
    
    # Generate 100 data points
    for i in range(100):
        # Simulate price movement with trend and noise
        trend = i * 0.5  # Slight upward trend
        noise = np.random.normal(0, 15)  # Price volatility
        price = base_price + trend + noise
        
        # Simulate realistic OHLC
        high = price + abs(np.random.normal(3, 1))
        low = price - abs(np.random.normal(3, 1))
        volume = 1000 + abs(np.random.normal(0, 300))
        
        market_data = MarketData(
            timestamp=base_time + timedelta(minutes=i),
            open=price - np.random.normal(0, 2),
            high=high,
            low=low,
            close=price,
            volume=volume
        )
        
        signal = generator.generate_signal(market_data)
        
        if signal:
            signals_generated.append(signal)
            
            # Log significant signals
            if signal.confidence > 0.3:
                direction = "🔼 BUY" if signal.signal_strength > 0 else "🔽 SELL"
                print(f"{direction} SIGNAL: "
                      f"Strength: {signal.signal_strength:+.3f}, "
                      f"Confidence: {signal.confidence:.3f}, "
                      f"Return: {signal.expected_return:+.2%}, "
                      f"Quality: {signal.signal_quality}")
                print(f"   Indicators: {len(signal.active_indicators)} active")
                print(f"   Components: {signal.components}")
                print()
    
    # Performance summary
    performance = generator.get_performance_summary()
    print("📈 PERFORMANCE SUMMARY")
    print("=" * 40)
    print(f"💡 Total Signals Generated: {len(signals_generated)}")
    print(f"🎯 High Confidence Signals: {performance['metrics']['high_confidence_signals']}")
    print(f"📊 Signal Distribution: {performance['metrics']['signal_distribution']}")
    print(f"📉 Data Points Processed: {performance['data_points']}")
    
    if signals_generated:
        avg_confidence = sum(s.confidence for s in signals_generated) / len(signals_generated)
        avg_strength = sum(abs(s.signal_strength) for s in signals_generated) / len(signals_generated)
        print(f"📊 Average Confidence: {avg_confidence:.3f}")
        print(f"⚡ Average Signal Strength: {avg_strength:.3f}")
    
    return signals_generated, performance

if __name__ == "__main__":
    # Run demo
    signals, perf = demo_signal_generation()
