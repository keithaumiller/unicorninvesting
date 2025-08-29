"""
Technical Indicators Calculation Engine
Unicorn Investing Platform - Phase 1 Implementation

Real-time calculation of 30+ technical indicators for ETH feature engineering.
Optimized for streaming data with efficient update mechanisms.
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from collections import deque
import logging

logger = logging.getLogger(__name__)

@dataclass
class IndicatorValue:
    """Container for indicator values with metadata"""
    value: float
    timestamp: pd.Timestamp
    is_valid: bool = True
    confidence: float = 1.0

class CircularBuffer:
    """Memory-efficient circular buffer for streaming data"""
    
    def __init__(self, maxlen: int):
        self.buffer = deque(maxlen=maxlen)
        self.maxlen = maxlen
        
    def append(self, value):
        self.buffer.append(value)
        
    def get_array(self) -> np.ndarray:
        return np.array(self.buffer)
        
    def is_full(self) -> bool:
        return len(self.buffer) == self.maxlen
        
    def __len__(self):
        return len(self.buffer)

class TechnicalIndicatorEngine:
    """
    High-performance technical indicator calculation engine
    
    Features:
    - 30+ technical indicators
    - Real-time streaming updates
    - Memory-efficient circular buffers
    - Optimized for low-latency computation
    """
    
    def __init__(self, max_history: int = 1000):
        self.max_history = max_history
        
        # Price data buffers
        self.close_buffer = CircularBuffer(max_history)
        self.high_buffer = CircularBuffer(max_history)
        self.low_buffer = CircularBuffer(max_history)
        self.open_buffer = CircularBuffer(max_history)
        self.volume_buffer = CircularBuffer(max_history)
        
        # Indicator cache for efficiency
        self.indicator_cache = {}
        self.last_update_time = None
        
        # Indicator parameters
        self.params = {
            # Moving Averages
            'sma_periods': [10, 20, 50, 200],
            'ema_periods': [12, 26, 50],
            
            # Momentum Indicators
            'rsi_period': 14,
            'stoch_k': 14,
            'stoch_d': 3,
            'williams_r_period': 14,
            'roc_period': 12,
            
            # Trend Indicators
            'macd_fast': 12,
            'macd_slow': 26,
            'macd_signal': 9,
            'adx_period': 14,
            'aroon_period': 25,
            
            # Volatility Indicators
            'bb_period': 20,
            'bb_std': 2,
            'atr_period': 14,
            'keltner_period': 20,
            'keltner_multiplier': 2,
            
            # Volume Indicators
            'vwap_period': 20,
            'obv_period': 20,
            'mfi_period': 14,
        }
        
        logger.info(f"✅ Technical Indicator Engine initialized with {max_history} period history")
        
    def update(self, open_price: float, high: float, low: float, close: float, volume: float, timestamp: pd.Timestamp) -> Dict[str, IndicatorValue]:
        """
        Update all indicators with new price data
        
        Returns:
            Dictionary of all indicator values
        """
        # Add new data to buffers
        self.open_buffer.append(open_price)
        self.high_buffer.append(high)
        self.low_buffer.append(low)
        self.close_buffer.append(close)
        self.volume_buffer.append(volume)
        
        self.last_update_time = timestamp
        
        # Calculate all indicators
        indicators = {}
        
        # Only calculate if we have enough data
        if len(self.close_buffer) >= 10:
            indicators.update(self._calculate_trend_indicators(timestamp))
            indicators.update(self._calculate_momentum_indicators(timestamp))
            indicators.update(self._calculate_volatility_indicators(timestamp))
            indicators.update(self._calculate_volume_indicators(timestamp))
            
        return indicators
        
    def _calculate_trend_indicators(self, timestamp: pd.Timestamp) -> Dict[str, IndicatorValue]:
        """Calculate trend-based indicators"""
        indicators = {}
        
        close_array = self.close_buffer.get_array()
        high_array = self.high_buffer.get_array()
        low_array = self.low_buffer.get_array()
        
        try:
            # Simple Moving Averages
            for period in self.params['sma_periods']:
                if len(close_array) >= period:
                    sma_value = np.mean(close_array[-period:])
                    indicators[f'sma_{period}'] = IndicatorValue(sma_value, timestamp)
                    
            # Exponential Moving Averages
            for period in self.params['ema_periods']:
                if len(close_array) >= period:
                    ema_value = self._calculate_ema(close_array, period)
                    indicators[f'ema_{period}'] = IndicatorValue(ema_value, timestamp)
                    
            # MACD
            if len(close_array) >= self.params['macd_slow']:
                macd_line, macd_signal, macd_histogram = self._calculate_macd(close_array)
                indicators['macd'] = IndicatorValue(macd_line, timestamp)
                indicators['macd_signal'] = IndicatorValue(macd_signal, timestamp)
                indicators['macd_histogram'] = IndicatorValue(macd_histogram, timestamp)
                
            # ADX (Average Directional Index)
            if len(close_array) >= self.params['adx_period'] + 1:
                adx_value = self._calculate_adx(high_array, low_array, close_array)
                indicators['adx'] = IndicatorValue(adx_value, timestamp)
                
            # Aroon Oscillator
            if len(close_array) >= self.params['aroon_period']:
                aroon_up, aroon_down = self._calculate_aroon(high_array, low_array)
                indicators['aroon_up'] = IndicatorValue(aroon_up, timestamp)
                indicators['aroon_down'] = IndicatorValue(aroon_down, timestamp)
                indicators['aroon_oscillator'] = IndicatorValue(aroon_up - aroon_down, timestamp)
                
        except Exception as e:
            logger.warning(f"⚠️ Trend indicator calculation error: {e}")
            
        return indicators
        
    def _calculate_momentum_indicators(self, timestamp: pd.Timestamp) -> Dict[str, IndicatorValue]:
        """Calculate momentum-based indicators"""
        indicators = {}
        
        close_array = self.close_buffer.get_array()
        high_array = self.high_buffer.get_array()
        low_array = self.low_buffer.get_array()
        
        try:
            # RSI (Relative Strength Index)
            if len(close_array) >= self.params['rsi_period'] + 1:
                rsi_value = self._calculate_rsi(close_array)
                indicators['rsi'] = IndicatorValue(rsi_value, timestamp)
                
            # Stochastic Oscillator
            if len(close_array) >= self.params['stoch_k']:
                stoch_k, stoch_d = self._calculate_stochastic(high_array, low_array, close_array)
                indicators['stoch_k'] = IndicatorValue(stoch_k, timestamp)
                indicators['stoch_d'] = IndicatorValue(stoch_d, timestamp)
                
            # Williams %R
            if len(close_array) >= self.params['williams_r_period']:
                williams_r = self._calculate_williams_r(high_array, low_array, close_array)
                indicators['williams_r'] = IndicatorValue(williams_r, timestamp)
                
            # Rate of Change (ROC)
            if len(close_array) >= self.params['roc_period'] + 1:
                roc_value = self._calculate_roc(close_array)
                indicators['roc'] = IndicatorValue(roc_value, timestamp)
                
        except Exception as e:
            logger.warning(f"⚠️ Momentum indicator calculation error: {e}")
            
        return indicators
        
    def _calculate_volatility_indicators(self, timestamp: pd.Timestamp) -> Dict[str, IndicatorValue]:
        """Calculate volatility-based indicators"""
        indicators = {}
        
        close_array = self.close_buffer.get_array()
        high_array = self.high_buffer.get_array()
        low_array = self.low_buffer.get_array()
        
        try:
            # Bollinger Bands
            if len(close_array) >= self.params['bb_period']:
                bb_upper, bb_middle, bb_lower = self._calculate_bollinger_bands(close_array)
                indicators['bb_upper'] = IndicatorValue(bb_upper, timestamp)
                indicators['bb_middle'] = IndicatorValue(bb_middle, timestamp)
                indicators['bb_lower'] = IndicatorValue(bb_lower, timestamp)
                indicators['bb_width'] = IndicatorValue(bb_upper - bb_lower, timestamp)
                indicators['bb_position'] = IndicatorValue((close_array[-1] - bb_lower) / (bb_upper - bb_lower), timestamp)
                
            # Average True Range (ATR)
            if len(close_array) >= self.params['atr_period'] + 1:
                atr_value = self._calculate_atr(high_array, low_array, close_array)
                indicators['atr'] = IndicatorValue(atr_value, timestamp)
                
            # Keltner Channels
            if len(close_array) >= self.params['keltner_period']:
                keltner_upper, keltner_middle, keltner_lower = self._calculate_keltner_channels(
                    high_array, low_array, close_array
                )
                indicators['keltner_upper'] = IndicatorValue(keltner_upper, timestamp)
                indicators['keltner_middle'] = IndicatorValue(keltner_middle, timestamp)
                indicators['keltner_lower'] = IndicatorValue(keltner_lower, timestamp)
                
        except Exception as e:
            logger.warning(f"⚠️ Volatility indicator calculation error: {e}")
            
        return indicators
        
    def _calculate_volume_indicators(self, timestamp: pd.Timestamp) -> Dict[str, IndicatorValue]:
        """Calculate volume-based indicators"""
        indicators = {}
        
        close_array = self.close_buffer.get_array()
        volume_array = self.volume_buffer.get_array()
        high_array = self.high_buffer.get_array()
        low_array = self.low_buffer.get_array()
        
        try:
            # Volume Weighted Average Price (VWAP)
            if len(close_array) >= self.params['vwap_period']:
                vwap_value = self._calculate_vwap(high_array, low_array, close_array, volume_array)
                indicators['vwap'] = IndicatorValue(vwap_value, timestamp)
                
            # On Balance Volume (OBV)
            if len(close_array) >= 2:
                obv_value = self._calculate_obv(close_array, volume_array)
                indicators['obv'] = IndicatorValue(obv_value, timestamp)
                
            # Money Flow Index (MFI)
            if len(close_array) >= self.params['mfi_period'] + 1:
                mfi_value = self._calculate_mfi(high_array, low_array, close_array, volume_array)
                indicators['mfi'] = IndicatorValue(mfi_value, timestamp)
                
            # Volume SMA
            if len(volume_array) >= 20:
                volume_sma = np.mean(volume_array[-20:])
                indicators['volume_sma'] = IndicatorValue(volume_sma, timestamp)
                
        except Exception as e:
            logger.warning(f"⚠️ Volume indicator calculation error: {e}")
            
        return indicators
        
    # Individual indicator calculation methods
    def _calculate_ema(self, prices: np.ndarray, period: int) -> float:
        """Calculate Exponential Moving Average"""
        alpha = 2.0 / (period + 1.0)
        ema = prices[0]
        for price in prices[1:]:
            ema = alpha * price + (1 - alpha) * ema
        return ema
        
    def _calculate_macd(self, prices: np.ndarray) -> Tuple[float, float, float]:
        """Calculate MACD line, signal line, and histogram"""
        ema_12 = self._calculate_ema(prices, 12)
        ema_26 = self._calculate_ema(prices, 26)
        macd_line = ema_12 - ema_26
        
        # For signal line, we'd need to calculate EMA of MACD values
        # Simplified version - would need proper implementation
        macd_signal = macd_line * 0.9  # Placeholder
        macd_histogram = macd_line - macd_signal
        
        return macd_line, macd_signal, macd_histogram
        
    def _calculate_rsi(self, prices: np.ndarray) -> float:
        """Calculate Relative Strength Index"""
        deltas = np.diff(prices)
        gains = np.where(deltas > 0, deltas, 0)
        losses = np.where(deltas < 0, -deltas, 0)
        
        avg_gain = np.mean(gains[-self.params['rsi_period']:])
        avg_loss = np.mean(losses[-self.params['rsi_period']:])
        
        if avg_loss == 0:
            return 100
        
        rs = avg_gain / avg_loss
        rsi = 100 - (100 / (1 + rs))
        return rsi
        
    def _calculate_stochastic(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray) -> Tuple[float, float]:
        """Calculate Stochastic Oscillator %K and %D"""
        period = self.params['stoch_k']
        highest_high = np.max(highs[-period:])
        lowest_low = np.min(lows[-period:])
        
        if highest_high == lowest_low:
            stoch_k = 50
        else:
            stoch_k = 100 * (closes[-1] - lowest_low) / (highest_high - lowest_low)
        
        # %D is typically a moving average of %K
        # Simplified version
        stoch_d = stoch_k * 0.9  # Placeholder
        
        return stoch_k, stoch_d
        
    def _calculate_williams_r(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray) -> float:
        """Calculate Williams %R"""
        period = self.params['williams_r_period']
        highest_high = np.max(highs[-period:])
        lowest_low = np.min(lows[-period:])
        
        if highest_high == lowest_low:
            return -50
        
        williams_r = -100 * (highest_high - closes[-1]) / (highest_high - lowest_low)
        return williams_r
        
    def _calculate_roc(self, prices: np.ndarray) -> float:
        """Calculate Rate of Change"""
        period = self.params['roc_period']
        if len(prices) <= period:
            return 0
        
        roc = 100 * (prices[-1] - prices[-period-1]) / prices[-period-1]
        return roc
        
    def _calculate_bollinger_bands(self, prices: np.ndarray) -> Tuple[float, float, float]:
        """Calculate Bollinger Bands"""
        period = self.params['bb_period']
        std_multiplier = self.params['bb_std']
        
        sma = np.mean(prices[-period:])
        std = np.std(prices[-period:])
        
        bb_upper = sma + (std_multiplier * std)
        bb_middle = sma
        bb_lower = sma - (std_multiplier * std)
        
        return bb_upper, bb_middle, bb_lower
        
    def _calculate_atr(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray) -> float:
        """Calculate Average True Range"""
        if len(closes) < 2:
            return 0
            
        true_ranges = []
        for i in range(1, len(closes)):
            high_low = highs[i] - lows[i]
            high_close_prev = abs(highs[i] - closes[i-1])
            low_close_prev = abs(lows[i] - closes[i-1])
            
            true_range = max(high_low, high_close_prev, low_close_prev)
            true_ranges.append(true_range)
        
        period = min(self.params['atr_period'], len(true_ranges))
        atr = np.mean(true_ranges[-period:])
        return atr
        
    def _calculate_keltner_channels(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray) -> Tuple[float, float, float]:
        """Calculate Keltner Channels"""
        period = self.params['keltner_period']
        multiplier = self.params['keltner_multiplier']
        
        ema = self._calculate_ema(closes, period)
        atr = self._calculate_atr(highs, lows, closes)
        
        keltner_upper = ema + (multiplier * atr)
        keltner_middle = ema
        keltner_lower = ema - (multiplier * atr)
        
        return keltner_upper, keltner_middle, keltner_lower
        
    def _calculate_vwap(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray, volumes: np.ndarray) -> float:
        """Calculate Volume Weighted Average Price"""
        period = min(self.params['vwap_period'], len(closes))
        
        typical_prices = (highs[-period:] + lows[-period:] + closes[-period:]) / 3
        volume_sum = np.sum(volumes[-period:])
        
        if volume_sum == 0:
            return closes[-1]
        
        vwap = np.sum(typical_prices * volumes[-period:]) / volume_sum
        return vwap
        
    def _calculate_obv(self, closes: np.ndarray, volumes: np.ndarray) -> float:
        """Calculate On Balance Volume"""
        if len(closes) < 2:
            return 0
            
        obv = 0
        for i in range(1, len(closes)):
            if closes[i] > closes[i-1]:
                obv += volumes[i]
            elif closes[i] < closes[i-1]:
                obv -= volumes[i]
        
        return obv
        
    def _calculate_mfi(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray, volumes: np.ndarray) -> float:
        """Calculate Money Flow Index"""
        period = self.params['mfi_period']
        
        typical_prices = (highs + lows + closes) / 3
        raw_money_flows = typical_prices * volumes
        
        positive_flows = []
        negative_flows = []
        
        for i in range(1, len(typical_prices)):
            if typical_prices[i] > typical_prices[i-1]:
                positive_flows.append(raw_money_flows[i])
                negative_flows.append(0)
            else:
                positive_flows.append(0)
                negative_flows.append(raw_money_flows[i])
        
        period = min(period, len(positive_flows))
        positive_mf = np.sum(positive_flows[-period:])
        negative_mf = np.sum(negative_flows[-period:])
        
        if negative_mf == 0:
            return 100
        
        mf_ratio = positive_mf / negative_mf
        mfi = 100 - (100 / (1 + mf_ratio))
        return mfi
        
    def _calculate_adx(self, highs: np.ndarray, lows: np.ndarray, closes: np.ndarray) -> float:
        """Calculate Average Directional Index (simplified)"""
        # This is a simplified version - full ADX calculation is more complex
        period = self.params['adx_period']
        
        # Calculate directional movements
        plus_dm = []
        minus_dm = []
        
        for i in range(1, len(highs)):
            plus_dm_val = max(highs[i] - highs[i-1], 0)
            minus_dm_val = max(lows[i-1] - lows[i], 0)
            
            if plus_dm_val > minus_dm_val:
                plus_dm.append(plus_dm_val)
                minus_dm.append(0)
            else:
                plus_dm.append(0)
                minus_dm.append(minus_dm_val)
        
        # Average the movements
        period = min(period, len(plus_dm))
        avg_plus_dm = np.mean(plus_dm[-period:])
        avg_minus_dm = np.mean(minus_dm[-period:])
        
        # Simplified ADX calculation
        adx = abs(avg_plus_dm - avg_minus_dm) / (avg_plus_dm + avg_minus_dm + 1e-10) * 100
        return adx
        
    def _calculate_aroon(self, highs: np.ndarray, lows: np.ndarray) -> Tuple[float, float]:
        """Calculate Aroon Up and Aroon Down"""
        period = self.params['aroon_period']
        period = min(period, len(highs))
        
        # Find periods since highest high and lowest low
        highest_idx = np.argmax(highs[-period:])
        lowest_idx = np.argmin(lows[-period:])
        
        periods_since_high = period - 1 - highest_idx
        periods_since_low = period - 1 - lowest_idx
        
        aroon_up = 100 * (period - periods_since_high) / period
        aroon_down = 100 * (period - periods_since_low) / period
        
        return aroon_up, aroon_down
        
    def get_indicator_summary(self) -> Dict:
        """Get summary of all available indicators"""
        return {
            "trend_indicators": [
                f"sma_{p}" for p in self.params['sma_periods']
            ] + [
                f"ema_{p}" for p in self.params['ema_periods']
            ] + ["macd", "macd_signal", "macd_histogram", "adx", "aroon_up", "aroon_down", "aroon_oscillator"],
            
            "momentum_indicators": [
                "rsi", "stoch_k", "stoch_d", "williams_r", "roc"
            ],
            
            "volatility_indicators": [
                "bb_upper", "bb_middle", "bb_lower", "bb_width", "bb_position",
                "atr", "keltner_upper", "keltner_middle", "keltner_lower"
            ],
            
            "volume_indicators": [
                "vwap", "obv", "mfi", "volume_sma"
            ]
        }

# Example usage
if __name__ == "__main__":
    # Initialize indicator engine
    engine = TechnicalIndicatorEngine(max_history=500)
    
    # Simulate some price data
    import random
    base_price = 3000
    
    print("🧮 Testing Technical Indicator Engine...")
    print(f"📊 Available indicators: {sum(len(v) for v in engine.get_indicator_summary().values())}")
    
    # Generate sample data and calculate indicators
    for i in range(100):
        # Simulate price movement
        change = random.uniform(-0.05, 0.05)
        base_price *= (1 + change)
        
        # Simulate OHLCV data
        open_price = base_price * random.uniform(0.995, 1.005)
        high = max(open_price, base_price) * random.uniform(1.0, 1.01)
        low = min(open_price, base_price) * random.uniform(0.99, 1.0)
        close = base_price
        volume = random.uniform(1000, 10000)
        
        timestamp = pd.Timestamp.now()
        
        # Update indicators
        indicators = engine.update(open_price, high, low, close, volume, timestamp)
        
        # Print sample of indicators every 10 updates
        if i % 10 == 0 and indicators:
            print(f"\n📈 Update {i+1}:")
            print(f"   Price: ${close:.2f}")
            
            # Show a few key indicators
            if 'sma_20' in indicators:
                print(f"   SMA(20): ${indicators['sma_20'].value:.2f}")
            if 'rsi' in indicators:
                print(f"   RSI: {indicators['rsi'].value:.1f}")
            if 'bb_position' in indicators:
                print(f"   BB Position: {indicators['bb_position'].value:.3f}")
                
    print(f"\n✅ Technical Indicator Engine test completed!")
    print(f"📊 Final indicator count: {len(indicators)}")
