"""
Phase 2 Technical Analysis Alpha - Simple Test
==============================================

Simple test implementation that works with our actual Phase 1 technical
indicators to validate the alpha model concept without LEAN dependencies.

This test demonstrates:
- Integration with Phase 1 TechnicalIndicatorEngine
- Signal generation logic
- Confidence scoring
- Performance tracking
"""

import sys
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import List, Dict, Optional

# Add path to Phase 1 implementation
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

try:
    from technical_indicators import TechnicalIndicatorEngine, IndicatorValue
    INDICATORS_AVAILABLE = True
    print("✅ Phase 1 Technical Indicators Successfully Imported")
except ImportError as e:
    print(f"❌ Failed to import technical indicators: {e}")
    INDICATORS_AVAILABLE = False

class SimpleETHAlpha:
    """
    Simple ETH Alpha Model using Phase 1 Technical Indicators.
    
    This demonstrates the core concept of Phase 2 implementation
    using our validated Phase 1 infrastructure.
    """
    
    def __init__(self, confidence_threshold: float = 0.02):
        """Initialize the alpha model."""
        self.confidence_threshold = confidence_threshold
        self.indicator_engine = None
        self.signal_count = 0
        self.successful_signals = 0
        
        if INDICATORS_AVAILABLE:
            self.indicator_engine = TechnicalIndicatorEngine(max_history=200)
            print(f"🚀 Alpha Model Initialized (Confidence Threshold: {confidence_threshold:.1%})")
        else:
            print("⚠️ Alpha Model Initialized WITHOUT Technical Indicators")
    
    def generate_signal(self, open_price: float, high: float, low: float, 
                       close: float, volume: float) -> Optional[Dict]:
        """
        Generate trading signal based on technical analysis.
        
        Returns:
            Signal dictionary with strength, confidence, and reasoning
        """
        if not self.indicator_engine:
            return None
            
        timestamp = pd.Timestamp.now()
        
        # Update technical indicators
        indicators = self.indicator_engine.update(
            open_price, high, low, close, volume, timestamp
        )
        
        if not indicators:
            return None
        
        # Generate signal from indicators
        signal_components = []
        confidence_factors = []
        
        # 1. Moving Average Analysis
        if 'sma_10' in indicators and 'sma_20' in indicators and 'sma_50' in indicators:
            sma_10 = indicators['sma_10'].value
            sma_20 = indicators['sma_20'].value
            sma_50 = indicators['sma_50'].value
            
            # Bullish/Bearish alignment
            if sma_10 > sma_20 > sma_50:
                signal_components.append(0.3)  # Bullish
                confidence_factors.append(0.4)
            elif sma_10 < sma_20 < sma_50:
                signal_components.append(-0.3)  # Bearish
                confidence_factors.append(0.4)
                
            # Short-term momentum
            sma_momentum = (sma_10 - sma_20) / sma_20
            if abs(sma_momentum) > 0.01:
                signal_components.append(sma_momentum * 10)
                confidence_factors.append(0.3)
        
        # 2. RSI Analysis
        if 'rsi' in indicators:
            rsi = indicators['rsi'].value
            if rsi < 30:  # Oversold
                signal_components.append((30 - rsi) / 30 * 0.4)
                confidence_factors.append(0.3)
            elif rsi > 70:  # Overbought
                signal_components.append(-(rsi - 70) / 30 * 0.4)
                confidence_factors.append(0.3)
        
        # 3. Bollinger Bands Analysis
        if 'bb_upper' in indicators and 'bb_lower' in indicators and 'bb_middle' in indicators:
            bb_upper = indicators['bb_upper'].value
            bb_lower = indicators['bb_lower'].value
            bb_middle = indicators['bb_middle'].value
            
            bb_position = (close - bb_lower) / (bb_upper - bb_lower)
            if bb_position < 0.2:  # Near lower band - potential buy
                signal_components.append((0.2 - bb_position) * 2)
                confidence_factors.append(0.35)
            elif bb_position > 0.8:  # Near upper band - potential sell
                signal_components.append(-(bb_position - 0.8) * 2)
                confidence_factors.append(0.35)
        
        # 4. MACD Analysis
        if 'macd' in indicators and 'macd_signal' in indicators:
            macd = indicators['macd'].value
            macd_signal = indicators['macd_signal'].value
            macd_diff = macd - macd_signal
            
            if abs(macd_diff) > 0.5:
                signal_components.append(np.tanh(macd_diff / 10))
                confidence_factors.append(0.3)
        
        # Combine signals
        if not signal_components:
            return None
            
        # Weighted average
        total_weight = sum(confidence_factors)
        if total_weight == 0:
            return None
            
        signal_strength = sum(s * w for s, w in zip(signal_components, confidence_factors)) / total_weight
        
        # Calculate confidence
        confidence = min(len(signal_components) / 4.0, 1.0)  # More signals = higher confidence
        confidence *= min(abs(signal_strength) * 3, 1.0)    # Stronger signal = higher confidence
        confidence = min(confidence, 0.95)                   # Cap at 95%
        
        # Only return signal if above threshold
        if confidence < self.confidence_threshold:
            return None
            
        self.signal_count += 1
        if confidence > 0.5:
            self.successful_signals += 1
        
        return {
            'timestamp': timestamp,
            'signal_strength': signal_strength,
            'confidence': confidence,
            'expected_return': signal_strength * 0.03,  # Max 3% expected return
            'components': len(signal_components),
            'indicators_used': list(indicators.keys()),
            'price': close
        }
    
    def get_performance_summary(self) -> Dict:
        """Get performance summary."""
        success_rate = (self.successful_signals / self.signal_count * 100) if self.signal_count > 0 else 0
        
        return {
            'total_signals': self.signal_count,
            'successful_signals': self.successful_signals,
            'success_rate': success_rate,
            'indicators_available': INDICATORS_AVAILABLE
        }

def run_alpha_test():
    """Run a comprehensive test of the alpha model."""
    print("🧪 ETH Technical Analysis Alpha - Phase 2 Test")
    print("=" * 60)
    
    # Initialize alpha model
    alpha = SimpleETHAlpha(confidence_threshold=0.015)
    
    if not INDICATORS_AVAILABLE:
        print("❌ Cannot run test - Phase 1 indicators not available")
        return False
    
    # Generate sample ETH price data
    base_price = 2500.0
    prices = []
    signals = []
    
    print("📈 Generating signals for sample ETH price data...")
    print()
    
    # Generate 50 data points with realistic price movement
    for i in range(50):
        # Simulate trending market with volatility
        trend = i * 1.2  # Upward trend
        noise = np.random.normal(0, 20)  # Volatility
        price = base_price + trend + noise
        
        # Generate realistic OHLC data
        high = price + abs(np.random.normal(5, 2))
        low = price - abs(np.random.normal(5, 2))
        open_price = price + np.random.normal(0, 3)
        volume = 1000 + abs(np.random.normal(0, 300))
        
        prices.append(price)
        
        # Generate signal
        signal = alpha.generate_signal(open_price, high, low, price, volume)
        
        if signal:
            signals.append(signal)
            
            # Log significant signals
            direction = "🔼 BUY" if signal['signal_strength'] > 0 else "🔽 SELL"
            quality = "🌟 HIGH" if signal['confidence'] > 0.6 else ("⭐ MED" if signal['confidence'] > 0.3 else "💫 LOW")
            
            print(f"{direction} SIGNAL - {quality}")
            print(f"   Price: ${signal['price']:,.2f}")
            print(f"   Strength: {signal['signal_strength']:+.3f}")
            print(f"   Confidence: {signal['confidence']:.3f}")
            print(f"   Expected Return: {signal['expected_return']:+.2%}")
            print(f"   Components: {signal['components']} indicators")
            print(f"   Indicators: {len(signal['indicators_used'])} available")
            print()
    
    # Performance Summary
    performance = alpha.get_performance_summary()
    
    print("📊 ALPHA MODEL PERFORMANCE SUMMARY")
    print("=" * 45)
    print(f"🎯 Total Signals Generated: {performance['total_signals']}")
    print(f"✅ High-Confidence Signals: {performance['successful_signals']}")
    print(f"📈 Success Rate: {performance['success_rate']:.1f}%")
    print(f"🔧 Technical Indicators: {'✅ Available' if performance['indicators_available'] else '❌ Missing'}")
    print()
    
    if signals:
        avg_confidence = sum(s['confidence'] for s in signals) / len(signals)
        avg_strength = sum(abs(s['signal_strength']) for s in signals) / len(signals)
        buy_signals = sum(1 for s in signals if s['signal_strength'] > 0)
        sell_signals = len(signals) - buy_signals
        
        print("📊 SIGNAL ANALYSIS")
        print(f"   Average Confidence: {avg_confidence:.3f}")
        print(f"   Average Signal Strength: {avg_strength:.3f}")
        print(f"   Buy Signals: {buy_signals}")
        print(f"   Sell Signals: {sell_signals}")
        print(f"   Signal Distribution: {buy_signals/len(signals):.1%} Buy, {sell_signals/len(signals):.1%} Sell")
    
    print()
    print("🎉 Phase 2 Alpha Model Test Complete!")
    
    # Test success criteria
    test_success = (
        performance['indicators_available'] and
        performance['total_signals'] > 0 and
        performance['success_rate'] > 0
    )
    
    if test_success:
        print("✅ TEST PASSED: Alpha model successfully integrated with Phase 1 indicators")
    else:
        print("❌ TEST FAILED: Alpha model integration issues detected")
    
    return test_success

if __name__ == "__main__":
    success = run_alpha_test()
    exit(0 if success else 1)
