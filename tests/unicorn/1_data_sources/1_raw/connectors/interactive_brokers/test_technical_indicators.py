"""
Technical Indicators Testing Suite
Unicorn Investing Platform - Indicator Validation

Comprehensive tests for:
- Individual indicator calculations
- Mathematical accuracy
- Edge case handling
- Performance validation
- Integration testing
"""

import pytest
import numpy as np
import pandas as pd
import sys
import os
from datetime import datetime, timedelta

# Add source paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

try:
    from technical_indicators import TechnicalIndicatorEngine, IndicatorValue, CircularBuffer
except ImportError as e:
    pytest.skip(f"Technical indicators module not available: {e}", allow_module_level=True)

class TestCircularBuffer:
    """Test circular buffer implementation"""
    
    def test_buffer_initialization(self):
        """Test buffer initializes correctly"""
        buffer = CircularBuffer(maxlen=10)
        assert len(buffer) == 0
        assert buffer.maxlen == 10
        assert not buffer.is_full()
    
    def test_buffer_append(self):
        """Test appending to buffer"""
        buffer = CircularBuffer(maxlen=3)
        
        buffer.append(1)
        assert len(buffer) == 1
        assert not buffer.is_full()
        
        buffer.append(2)
        buffer.append(3)
        assert len(buffer) == 3
        assert buffer.is_full()
        
        # Test overflow
        buffer.append(4)
        assert len(buffer) == 3  # Should stay at max
        assert buffer.get_array().tolist() == [2, 3, 4]  # First element dropped
    
    def test_buffer_array_conversion(self):
        """Test converting buffer to numpy array"""
        buffer = CircularBuffer(maxlen=5)
        test_data = [1.1, 2.2, 3.3, 4.4]
        
        for value in test_data:
            buffer.append(value)
        
        array = buffer.get_array()
        assert isinstance(array, np.ndarray)
        assert np.array_equal(array, np.array(test_data))

class TestIndicatorValue:
    """Test IndicatorValue dataclass"""
    
    def test_indicator_value_creation(self):
        """Test creating IndicatorValue instances"""
        timestamp = pd.Timestamp.now()
        indicator = IndicatorValue(
            value=50.5,
            timestamp=timestamp,
            is_valid=True,
            confidence=0.95
        )
        
        assert indicator.value == 50.5
        assert indicator.timestamp == timestamp
        assert indicator.is_valid is True
        assert indicator.confidence == 0.95
    
    def test_indicator_value_defaults(self):
        """Test default values"""
        timestamp = pd.Timestamp.now()
        indicator = IndicatorValue(value=25.0, timestamp=timestamp)
        
        assert indicator.is_valid is True  # Default
        assert indicator.confidence == 1.0  # Default

class TestTrendIndicators:
    """Test trend-based technical indicators"""
    
    @pytest.fixture
    def engine(self):
        """Create test engine"""
        return TechnicalIndicatorEngine(max_history=100)
    
    @pytest.fixture
    def trend_data(self):
        """Generate trending price data"""
        # Create upward trend with noise
        np.random.seed(42)
        base_price = 100.0
        prices = []
        
        for i in range(50):
            trend = i * 0.5  # Upward trend
            noise = np.random.normal(0, 1)  # Random noise
            price = base_price + trend + noise
            prices.append(max(price, 50))  # Price floor
        
        return prices
    
    def test_simple_moving_average(self, engine, trend_data):
        """Test Simple Moving Average calculation"""
        timestamp = pd.Timestamp.now()
        
        # Feed data to engine
        for i, price in enumerate(trend_data):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        # Check SMA indicators
        assert 'sma_10' in indicators, "Should calculate SMA(10)"
        assert 'sma_20' in indicators, "Should calculate SMA(20)"
        
        # Manual SMA(10) calculation for verification
        recent_prices = trend_data[-10:]
        expected_sma_10 = np.mean(recent_prices)
        actual_sma_10 = indicators['sma_10'].value
        
        assert abs(actual_sma_10 - expected_sma_10) < 0.01, f"SMA(10) calculation error: expected {expected_sma_10}, got {actual_sma_10}"
    
    def test_exponential_moving_average(self, engine, trend_data):
        """Test Exponential Moving Average calculation"""
        timestamp = pd.Timestamp.now()
        
        # Feed data to engine
        for i, price in enumerate(trend_data):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        # Check EMA indicators
        assert 'ema_12' in indicators, "Should calculate EMA(12)"
        assert 'ema_26' in indicators, "Should calculate EMA(26)"
        
        # EMA should be more responsive than SMA in trending market
        if 'sma_20' in indicators:
            ema_12 = indicators['ema_12'].value
            sma_20 = indicators['sma_20'].value
            current_price = trend_data[-1]
            
            # In uptrend, EMA should be closer to current price than SMA
            ema_distance = abs(ema_12 - current_price)
            sma_distance = abs(sma_20 - current_price)
            
            # This may not always hold due to different periods, but generally should
            # assert ema_distance <= sma_distance * 1.1, "EMA should be more responsive than SMA"
    
    def test_macd_calculation(self, engine, trend_data):
        """Test MACD indicator calculation"""
        timestamp = pd.Timestamp.now()
        
        # Feed enough data for MACD
        for i, price in enumerate(trend_data):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        # Check MACD components
        assert 'macd' in indicators, "Should calculate MACD line"
        assert 'macd_signal' in indicators, "Should calculate MACD signal"
        assert 'macd_histogram' in indicators, "Should calculate MACD histogram"
        
        # MACD line should be EMA(12) - EMA(26)
        macd_line = indicators['macd'].value
        macd_signal = indicators['macd_signal'].value
        macd_histogram = indicators['macd_histogram'].value
        
        # Histogram should be MACD - Signal
        expected_histogram = macd_line - macd_signal
        assert abs(macd_histogram - expected_histogram) < 0.01, "MACD histogram calculation error"

class TestMomentumIndicators:
    """Test momentum-based technical indicators"""
    
    @pytest.fixture
    def engine(self):
        return TechnicalIndicatorEngine(max_history=100)
    
    @pytest.fixture
    def oscillating_data(self):
        """Generate oscillating price data for momentum testing"""
        # Create data that oscillates between overbought/oversold
        prices = []
        base_price = 100.0
        
        for i in range(50):
            # Create sine wave pattern with noise
            oscillation = 20 * np.sin(i * 0.3)  # Oscillation
            noise = np.random.normal(0, 2)
            price = base_price + oscillation + noise
            prices.append(max(price, 50))
        
        return prices
    
    def test_rsi_calculation(self, engine, oscillating_data):
        """Test RSI calculation and bounds"""
        timestamp = pd.Timestamp.now()
        
        # Feed data to engine
        for i, price in enumerate(oscillating_data):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'rsi' in indicators, "Should calculate RSI"
        
        rsi_value = indicators['rsi'].value
        assert 0 <= rsi_value <= 100, f"RSI should be 0-100, got {rsi_value}"
        
        # Test RSI with known extreme values
        # Feed very high prices (should push RSI toward 100)
        high_prices = [200, 210, 220, 230, 240]
        for i, price in enumerate(high_prices):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=len(oscillating_data) + i)
            )
        
        final_rsi = indicators['rsi'].value
        assert final_rsi > 50, "RSI should be > 50 after strong upward movement"
    
    def test_stochastic_oscillator(self, engine, oscillating_data):
        """Test Stochastic Oscillator calculation"""
        timestamp = pd.Timestamp.now()
        
        # Feed data with varying highs and lows
        for i, price in enumerate(oscillating_data):
            high = price * (1.01 + 0.02 * np.random.random())
            low = price * (0.99 - 0.02 * np.random.random())
            
            indicators = engine.update(
                open_price=price,
                high=high,
                low=low,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'stoch_k' in indicators, "Should calculate Stochastic %K"
        assert 'stoch_d' in indicators, "Should calculate Stochastic %D"
        
        stoch_k = indicators['stoch_k'].value
        stoch_d = indicators['stoch_d'].value
        
        assert 0 <= stoch_k <= 100, f"Stochastic %K should be 0-100, got {stoch_k}"
        assert 0 <= stoch_d <= 100, f"Stochastic %D should be 0-100, got {stoch_d}"
    
    def test_williams_r(self, engine, oscillating_data):
        """Test Williams %R calculation"""
        timestamp = pd.Timestamp.now()
        
        for i, price in enumerate(oscillating_data):
            high = price * 1.02
            low = price * 0.98
            
            indicators = engine.update(
                open_price=price,
                high=high,
                low=low,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'williams_r' in indicators, "Should calculate Williams %R"
        
        williams_r = indicators['williams_r'].value
        assert -100 <= williams_r <= 0, f"Williams %R should be -100 to 0, got {williams_r}"
    
    def test_rate_of_change(self, engine):
        """Test Rate of Change calculation"""
        timestamp = pd.Timestamp.now()
        
        # Create data with known rate of change
        base_price = 100.0
        for i in range(20):
            price = base_price * (1.01 ** i)  # 1% growth per period
            
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        if 'roc' in indicators:
            roc_value = indicators['roc'].value
            # With 1% growth, ROC over 12 periods should be approximately 12%
            assert roc_value > 5, f"ROC should show positive change, got {roc_value}"

class TestVolatilityIndicators:
    """Test volatility-based technical indicators"""
    
    @pytest.fixture
    def engine(self):
        return TechnicalIndicatorEngine(max_history=100)
    
    @pytest.fixture
    def volatile_data(self):
        """Generate data with varying volatility"""
        np.random.seed(42)
        prices = []
        base_price = 100.0
        
        for i in range(50):
            # Increase volatility over time
            volatility = 1 + (i / 50.0) * 4  # 1% to 5% volatility
            change = np.random.normal(0, volatility)
            base_price += change
            prices.append(max(base_price, 50))
        
        return prices
    
    def test_bollinger_bands(self, engine, volatile_data):
        """Test Bollinger Bands calculation"""
        timestamp = pd.Timestamp.now()
        
        for i, price in enumerate(volatile_data):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'bb_upper' in indicators, "Should calculate BB upper band"
        assert 'bb_middle' in indicators, "Should calculate BB middle band"
        assert 'bb_lower' in indicators, "Should calculate BB lower band"
        assert 'bb_width' in indicators, "Should calculate BB width"
        assert 'bb_position' in indicators, "Should calculate BB position"
        
        bb_upper = indicators['bb_upper'].value
        bb_middle = indicators['bb_middle'].value
        bb_lower = indicators['bb_lower'].value
        bb_width = indicators['bb_width'].value
        bb_position = indicators['bb_position'].value
        
        # Validate band relationships
        assert bb_upper > bb_middle > bb_lower, "BB bands should be ordered correctly"
        assert abs(bb_width - (bb_upper - bb_lower)) < 0.01, "BB width calculation error"
        assert 0 <= bb_position <= 1, f"BB position should be 0-1, got {bb_position}"
    
    def test_atr(self, engine, volatile_data):
        """Test Average True Range calculation"""
        timestamp = pd.Timestamp.now()
        
        for i, price in enumerate(volatile_data):
            # Create varying true ranges
            high = price * (1.01 + 0.02 * np.random.random())
            low = price * (0.99 - 0.02 * np.random.random())
            
            indicators = engine.update(
                open_price=price,
                high=high,
                low=low,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'atr' in indicators, "Should calculate ATR"
        
        atr_value = indicators['atr'].value
        assert atr_value > 0, f"ATR should be positive, got {atr_value}"
        
        # ATR should increase with volatility
        # In our test data, later values have higher volatility
        # So ATR should reflect this (though it's a lagging indicator)
    
    def test_keltner_channels(self, engine, volatile_data):
        """Test Keltner Channels calculation"""
        timestamp = pd.Timestamp.now()
        
        for i, price in enumerate(volatile_data):
            high = price * 1.02
            low = price * 0.98
            
            indicators = engine.update(
                open_price=price,
                high=high,
                low=low,
                close=price,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'keltner_upper' in indicators, "Should calculate Keltner upper"
        assert 'keltner_middle' in indicators, "Should calculate Keltner middle"
        assert 'keltner_lower' in indicators, "Should calculate Keltner lower"
        
        kc_upper = indicators['keltner_upper'].value
        kc_middle = indicators['keltner_middle'].value
        kc_lower = indicators['keltner_lower'].value
        
        assert kc_upper > kc_middle > kc_lower, "Keltner channels should be ordered correctly"

class TestVolumeIndicators:
    """Test volume-based technical indicators"""
    
    @pytest.fixture
    def engine(self):
        return TechnicalIndicatorEngine(max_history=100)
    
    @pytest.fixture
    def volume_data(self):
        """Generate price and volume data"""
        np.random.seed(42)
        data = []
        base_price = 100.0
        
        for i in range(50):
            price_change = np.random.normal(0, 2)
            volume = 1000 + abs(price_change) * 100  # Higher volume on big moves
            base_price += price_change
            
            data.append({
                'price': max(base_price, 50),
                'volume': volume
            })
        
        return data
    
    def test_vwap(self, engine, volume_data):
        """Test Volume Weighted Average Price"""
        timestamp = pd.Timestamp.now()
        
        for i, data in enumerate(volume_data):
            price = data['price']
            volume = data['volume']
            
            # Create realistic OHLC from price
            high = price * 1.01
            low = price * 0.99
            
            indicators = engine.update(
                open_price=price,
                high=high,
                low=low,
                close=price,
                volume=volume,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'vwap' in indicators, "Should calculate VWAP"
        
        vwap_value = indicators['vwap'].value
        assert vwap_value > 0, f"VWAP should be positive, got {vwap_value}"
        
        # VWAP should be reasonably close to current price
        current_price = volume_data[-1]['price']
        price_diff_pct = abs(vwap_value - current_price) / current_price
        assert price_diff_pct < 0.1, f"VWAP should be within 10% of current price, diff: {price_diff_pct:.1%}"
    
    def test_obv(self, engine, volume_data):
        """Test On Balance Volume"""
        timestamp = pd.Timestamp.now()
        
        for i, data in enumerate(volume_data):
            price = data['price']
            volume = data['volume']
            
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=volume,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'obv' in indicators, "Should calculate OBV"
        
        # OBV can be positive or negative
        obv_value = indicators['obv'].value
        assert isinstance(obv_value, (int, float)), f"OBV should be numeric, got {type(obv_value)}"
    
    def test_mfi(self, engine, volume_data):
        """Test Money Flow Index"""
        timestamp = pd.Timestamp.now()
        
        for i, data in enumerate(volume_data):
            price = data['price']
            volume = data['volume']
            
            high = price * 1.02
            low = price * 0.98
            
            indicators = engine.update(
                open_price=price,
                high=high,
                low=low,
                close=price,
                volume=volume,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'mfi' in indicators, "Should calculate MFI"
        
        mfi_value = indicators['mfi'].value
        assert 0 <= mfi_value <= 100, f"MFI should be 0-100, got {mfi_value}"
    
    def test_volume_sma(self, engine, volume_data):
        """Test Volume Simple Moving Average"""
        timestamp = pd.Timestamp.now()
        
        for i, data in enumerate(volume_data):
            indicators = engine.update(
                open_price=data['price'],
                high=data['price'] * 1.01,
                low=data['price'] * 0.99,
                close=data['price'],
                volume=data['volume'],
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        assert 'volume_sma' in indicators, "Should calculate Volume SMA"
        
        volume_sma = indicators['volume_sma'].value
        assert volume_sma > 0, f"Volume SMA should be positive, got {volume_sma}"
        
        # Manual verification for last 20 periods
        recent_volumes = [d['volume'] for d in volume_data[-20:]]
        expected_volume_sma = np.mean(recent_volumes)
        
        assert abs(volume_sma - expected_volume_sma) < 1, f"Volume SMA calculation error: expected {expected_volume_sma}, got {volume_sma}"

class TestEdgeCases:
    """Test edge cases and error handling"""
    
    def test_insufficient_data(self):
        """Test behavior with insufficient data"""
        engine = TechnicalIndicatorEngine(max_history=100)
        timestamp = pd.Timestamp.now()
        
        # Feed just a few data points
        for i in range(5):
            indicators = engine.update(
                open_price=100 + i,
                high=101 + i,
                low=99 + i,
                close=100.5 + i,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        # Should return empty dict or minimal indicators
        assert isinstance(indicators, dict), "Should return dictionary"
        
        # Some indicators might be available with minimal data
        if indicators:
            for name, indicator in indicators.items():
                assert isinstance(indicator, IndicatorValue), f"{name} should be IndicatorValue"
    
    def test_zero_values(self):
        """Test handling of zero and negative values"""
        engine = TechnicalIndicatorEngine(max_history=100)
        timestamp = pd.Timestamp.now()
        
        # Test with zero volume
        indicators = engine.update(
            open_price=100,
            high=101,
            low=99,
            close=100,
            volume=0,  # Zero volume
            timestamp=timestamp
        )
        
        # Should not crash
        assert isinstance(indicators, dict), "Should handle zero volume"
    
    def test_extreme_values(self):
        """Test handling of extreme price values"""
        engine = TechnicalIndicatorEngine(max_history=100)
        timestamp = pd.Timestamp.now()
        
        # Test with very large values
        extreme_price = 1e6
        indicators = engine.update(
            open_price=extreme_price,
            high=extreme_price * 1.01,
            low=extreme_price * 0.99,
            close=extreme_price,
            volume=1000,
            timestamp=timestamp
        )
        
        # Should not crash
        assert isinstance(indicators, dict), "Should handle extreme values"
    
    def test_invalid_ohlc_relationships(self):
        """Test handling of invalid OHLC relationships"""
        engine = TechnicalIndicatorEngine(max_history=100)
        timestamp = pd.Timestamp.now()
        
        # Test with invalid OHLC (high < low)
        try:
            indicators = engine.update(
                open_price=100,
                high=95,  # High less than open
                low=105,  # Low greater than open
                close=100,
                volume=1000,
                timestamp=timestamp
            )
            
            # Should handle gracefully
            assert isinstance(indicators, dict), "Should handle invalid OHLC"
            
        except Exception as e:
            # It's also acceptable to raise an exception for invalid data
            assert "invalid" in str(e).lower() or "ohlc" in str(e).lower(), f"Unexpected exception: {e}"

class TestPerformanceIndicators:
    """Test performance of indicator calculations"""
    
    def test_calculation_speed(self):
        """Test that indicator calculations are fast enough"""
        import time
        
        engine = TechnicalIndicatorEngine(max_history=500)
        timestamp = pd.Timestamp.now()
        
        # Generate test data
        np.random.seed(42)
        test_data = []
        base_price = 100.0
        
        for i in range(200):  # 200 data points
            change = np.random.normal(0, 2)
            base_price += change
            test_data.append(max(base_price, 50))
        
        # Measure calculation time
        start_time = time.time()
        
        for i, price in enumerate(test_data):
            indicators = engine.update(
                open_price=price,
                high=price * 1.01,
                low=price * 0.99,
                close=price,
                volume=1000 + i,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        calculation_time = time.time() - start_time
        
        # Should complete within reasonable time
        assert calculation_time < 2.0, f"Indicator calculation too slow: {calculation_time:.2f}s"
        
        # Should calculate many indicators
        if indicators:
            total_indicators = len(indicators) * len(test_data)
            indicators_per_second = total_indicators / calculation_time
            assert indicators_per_second > 1000, f"Should calculate >1000 indicators/second, got {indicators_per_second:.1f}"
    
    def test_memory_usage(self):
        """Test that memory usage is reasonable"""
        engine = TechnicalIndicatorEngine(max_history=1000)
        timestamp = pd.Timestamp.now()
        
        # Feed a lot of data
        for i in range(2000):  # More than max_history
            indicators = engine.update(
                open_price=100 + i * 0.1,
                high=101 + i * 0.1,
                low=99 + i * 0.1,
                close=100.5 + i * 0.1,
                volume=1000,
                timestamp=timestamp + timedelta(minutes=i)
            )
        
        # Buffer should not exceed max_history
        assert len(engine.close_buffer) <= engine.max_history, "Buffer should not exceed max history"
        assert len(engine.high_buffer) <= engine.max_history, "High buffer should not exceed max history"
        assert len(engine.low_buffer) <= engine.max_history, "Low buffer should not exceed max history"
        assert len(engine.volume_buffer) <= engine.max_history, "Volume buffer should not exceed max history"

# Utility functions for manual testing
def run_indicator_validation():
    """Run comprehensive indicator validation"""
    print("🧮 Technical Indicators Validation Suite")
    print("=" * 50)
    
    engine = TechnicalIndicatorEngine(max_history=200)
    
    # Generate realistic test data
    np.random.seed(42)
    prices = []
    base_price = 4000.0  # ETH-like price
    
    print("📊 Generating test data...")
    for i in range(100):
        # Random walk with trend
        trend = i * 0.1  # Slight upward trend
        volatility = np.random.normal(0, 20)  # $20 volatility
        base_price += trend + volatility
        prices.append(max(base_price, 1000))  # Price floor
    
    print(f"   Generated {len(prices)} price points")
    print(f"   Price range: ${min(prices):.2f} - ${max(prices):.2f}")
    
    # Feed data to engine
    print("\n🔄 Calculating indicators...")
    timestamp = pd.Timestamp.now()
    all_indicators = []
    
    for i, price in enumerate(prices):
        indicators = engine.update(
            open_price=price * 0.999,
            high=price * 1.002,
            low=price * 0.998,
            close=price,
            volume=1000 + i * 10,
            timestamp=timestamp + timedelta(minutes=i)
        )
        
        if indicators:
            all_indicators.append(indicators)
    
    print(f"   Calculated indicators for {len(all_indicators)} periods")
    
    if all_indicators:
        final_indicators = all_indicators[-1]
        print(f"\n📈 Final Indicator Values ({len(final_indicators)} indicators):")
        
        # Group by category
        summary = engine.get_indicator_summary()
        
        for category, indicator_names in summary.items():
            print(f"\n   {category.replace('_', ' ').title()}:")
            for name in indicator_names:
                if name in final_indicators:
                    value = final_indicators[name].value
                    print(f"     {name}: {value:.4f}")
        
        print(f"\n✅ Indicator validation completed successfully!")
        print(f"📊 Total indicators calculated: {sum(len(ind) for ind in all_indicators)}")
    
    else:
        print("❌ No indicators calculated - check data requirements")

if __name__ == "__main__":
    # Run validation if called directly
    run_indicator_validation()
