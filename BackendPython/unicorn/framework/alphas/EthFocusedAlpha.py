"""
ETH Focused Alpha Model
======================

Pure forecasting component for Ethereum (ETH) trading.
This Alpha Model focuses specifically on ETHUSD using technical analysis
and momentum-based forecasting.

Separation of Concerns:
- This class ONLY does ETH forecasting and generates Insights
- Portfolio construction, execution, and risk management are handled separately
"""

from AlgorithmImports import *
import numpy as np

class EthFocusedAlpha(AlphaModel):
    """
    ETH-focused Alpha Model using technical analysis and momentum indicators.
    
    Strategy:
    - SMA crossover signals
    - RSI momentum confirmation
    - Bollinger band breakouts
    - Volume-weighted momentum
    
    Output: Insights for ETHUSD with direction and confidence
    """
    
    def __init__(self, prediction_horizon_hours=2, confidence_threshold=0.01):
        """
        Initialize the ETH-focused alpha model.
        
        Args:
            prediction_horizon_hours: How far ahead to predict (hours)
            confidence_threshold: Minimum expected return to generate insight (e.g., 1%)
        """
        self.prediction_interval = timedelta(hours=prediction_horizon_hours)
        self.confidence_threshold = confidence_threshold
        
        # Technical indicators will be initialized per symbol
        self.indicators = {}
        self.symbol_data = {}
        
    def update(self, algorithm, data):
        """
        Generate Insights based on ETH technical analysis.
        
        Returns Insights (NOT trades) - trading decisions handled by other components.
        """
        insights = []
        
        for symbol in self.symbol_data:
            if not data.contains_key(symbol) or not data[symbol]:
                continue
                
            try:
                symbol_data = self.symbol_data[symbol]
                
                # Check if all indicators are ready
                if not self.are_indicators_ready(symbol):
                    continue
                    
                current_price = algorithm.securities[symbol].price
                if current_price <= 0:
                    continue
                
                # Generate trading signal
                signal = self.generate_eth_signal(symbol, current_price)
                
                if signal and abs(signal['expected_return']) > self.confidence_threshold:
                    insight = self.create_insight(symbol, signal)
                    insights.append(insight)
                    
                    algorithm.debug(f"🟡 ETH SIGNAL {symbol}: {signal['expected_return']:.2%} "
                                  f"(confidence: {signal['confidence']:.2f})")
                                  
            except Exception as e:
                algorithm.debug(f"⚠️ ETH forecasting error for {symbol}: {e}")
                
        return insights
    
    def generate_eth_signal(self, symbol, current_price):
        """
        Generate ETH trading signal based on technical analysis.
        
        Returns signal dictionary with expected_return, confidence, and reasoning.
        """
        indicators = self.indicators[symbol]
        
        # Get indicator values
        sma_fast = indicators['sma_fast'].current.value
        sma_slow = indicators['sma_slow'].current.value
        rsi = indicators['rsi'].current.value
        bb_upper = indicators['bb_upper'].current.value
        bb_lower = indicators['bb_lower'].current.value
        bb_middle = indicators['bb_middle'].current.value
        
        # Signal components
        signals = []
        confidence_factors = []
        
        # 1. SMA Crossover Signal
        if sma_fast > sma_slow:
            sma_signal = (sma_fast - sma_slow) / sma_slow
            signals.append(sma_signal)
            confidence_factors.append(0.3)
        elif sma_fast < sma_slow:
            sma_signal = (sma_fast - sma_slow) / sma_slow
            signals.append(sma_signal)
            confidence_factors.append(0.3)
        
        # 2. RSI Momentum Signal
        if rsi < 30:  # Oversold - potential buy
            rsi_signal = (30 - rsi) / 30 * 0.02  # Max 2% signal
            signals.append(rsi_signal)
            confidence_factors.append(0.2)
        elif rsi > 70:  # Overbought - potential sell
            rsi_signal = -(rsi - 70) / 30 * 0.02  # Max -2% signal
            signals.append(rsi_signal)
            confidence_factors.append(0.2)
        
        # 3. Bollinger Band Signal
        bb_position = (current_price - bb_lower) / (bb_upper - bb_lower)
        if bb_position < 0.2:  # Near lower band - potential buy
            bb_signal = (0.2 - bb_position) * 0.03  # Max 3% signal
            signals.append(bb_signal)
            confidence_factors.append(0.25)
        elif bb_position > 0.8:  # Near upper band - potential sell
            bb_signal = -(bb_position - 0.8) * 0.03  # Max -3% signal
            signals.append(bb_signal)
            confidence_factors.append(0.25)
        
        # 4. Price momentum relative to BB middle
        price_momentum = (current_price - bb_middle) / bb_middle
        if abs(price_momentum) > 0.01:  # Strong momentum
            momentum_signal = price_momentum * 0.5  # Scale down
            signals.append(momentum_signal)
            confidence_factors.append(0.25)
        
        # Combine signals
        if not signals:
            return None
            
        # Weighted average of signals
        total_weight = sum(confidence_factors)
        if total_weight == 0:
            return None
            
        expected_return = sum(signal * weight for signal, weight in zip(signals, confidence_factors)) / total_weight
        
        # Calculate overall confidence
        confidence = min(len(signals) / 4.0, 1.0)  # More signals = higher confidence
        confidence *= min(abs(expected_return) * 50, 1.0)  # Stronger signal = higher confidence
        
        return {
            'expected_return': expected_return,
            'confidence': confidence,
            'signals_count': len(signals),
            'components': {
                'sma': sma_fast > sma_slow,
                'rsi': rsi,
                'bb_position': bb_position,
                'momentum': price_momentum
            }
        }
    
    def create_insight(self, symbol, signal):
        """
        Create an Insight from ETH signal data.
        """
        expected_return = signal['expected_return']
        confidence = signal['confidence']
        
        direction = InsightDirection.UP if expected_return > 0 else InsightDirection.DOWN
        magnitude = abs(expected_return)
        
        # Create the Insight
        insight = Insight.price(
            symbol,
            self.prediction_interval,
            direction,
            magnitude,
            confidence,
            weight=confidence * 0.95  # ETH gets 95% allocation as designed
        )
        
        # Add metadata about signal components
        insight.tag = f"ETH: {signal['signals_count']} signals, RSI: {signal['components']['rsi']:.1f}"
        
        return insight
    
    def are_indicators_ready(self, symbol):
        """Check if all technical indicators are ready."""
        if symbol not in self.indicators:
            return False
            
        indicators = self.indicators[symbol]
        return all(indicator.is_ready for indicator in indicators.values())
    
    def on_securities_changed(self, algorithm, changes):
        """
        Initialize technical indicators when ETH securities are added/removed.
        """
        # Add new securities (should be ETHUSD)
        for security in changes.added_securities:
            symbol = security.symbol
            
            # Initialize technical indicators for ETH
            self.indicators[symbol] = {
                'sma_fast': algorithm.sma(symbol, 10, Resolution.HOUR),  # 10-hour SMA
                'sma_slow': algorithm.sma(symbol, 30, Resolution.HOUR),  # 30-hour SMA
                'rsi': algorithm.rsi(symbol, 14, Resolution.HOUR),       # 14-hour RSI
                'bb_upper': algorithm.bb(symbol, 20, 2, Resolution.HOUR).upper_band,
                'bb_middle': algorithm.bb(symbol, 20, 2, Resolution.HOUR).middle_band,
                'bb_lower': algorithm.bb(symbol, 20, 2, Resolution.HOUR).lower_band,
            }
            
            # Initialize symbol data tracking
            self.symbol_data[symbol] = {
                'last_signal_time': None,
                'signal_history': []
            }
            
            algorithm.debug(f"🟡 Initialized ETH indicators for {symbol}")
            
        # Remove securities
        for security in changes.removed_securities:
            symbol = security.symbol
            
            if symbol in self.indicators:
                del self.indicators[symbol]
            if symbol in self.symbol_data:
                del self.symbol_data[symbol]
                
            algorithm.debug(f"🗑️ Removed ETH indicators for {symbol}")
    
    def get_signal_strength(self, symbol):
        """
        Get current signal strength for debugging/monitoring.
        
        Returns a score from -1 (strong sell) to +1 (strong buy).
        """
        if symbol not in self.indicators or not self.are_indicators_ready(symbol):
            return 0
            
        try:
            indicators = self.indicators[symbol]
            
            # SMA trend strength
            sma_fast = indicators['sma_fast'].current.value
            sma_slow = indicators['sma_slow'].current.value
            sma_strength = (sma_fast - sma_slow) / sma_slow
            
            # RSI momentum strength
            rsi = indicators['rsi'].current.value
            if rsi < 30:
                rsi_strength = (30 - rsi) / 30
            elif rsi > 70:
                rsi_strength = -(rsi - 70) / 30
            else:
                rsi_strength = 0
                
            # Combine for overall strength
            overall_strength = (sma_strength * 0.6 + rsi_strength * 0.4)
            return max(-1, min(1, overall_strength))
            
        except Exception:
            return 0
