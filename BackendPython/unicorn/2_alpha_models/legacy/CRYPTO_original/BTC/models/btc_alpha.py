"""
BTC Technical Alpha Model for CRYPTO

Technical analysis based alpha model for BTC trading.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List
import sys
import os

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
alpha_models_dir = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
sys.path.append(alpha_models_dir)

from shared.base_alpha import TechnicalAlphaModel

class BTCAlphaModel(TechnicalAlphaModel):
    """
    Technical alpha model for BTC trading.
    
    Implements technical analysis based signals for BTC.
    """
    
    def __init__(self, lookback_window: int = 100):
        super().__init__(
            name=f"BTCAlphaModel",
            asset_class="CRYPTO",
            lookback_window=lookback_window
        )
        
        # Model-specific parameters
        self.short_window = 20
        self.long_window = 50
        self.rsi_window = 14
        self.rsi_overbought = 70
        self.rsi_oversold = 30
        
    def get_required_columns(self) -> List[str]:
        """Return required data columns."""
        return ['Open', 'High', 'Low', 'Close', 'Volume']
        
    def generate_signal(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Generate trading signal for BTC.
        
        Args:
            data: OHLCV data
            
        Returns:
            Signal dictionary with direction, confidence, and metadata
        """
        self.validate_data(data)
        
        if len(data) < self.lookback_window:
            return {
                'signal': 0,
                'confidence': 0.0,
                'metadata': {'reason': 'Insufficient data'}
            }
        
        # Calculate technical indicators
        close_prices = data['Close']
        
        # Moving averages
        sma_short = self.calculate_sma(close_prices, self.short_window)
        sma_long = self.calculate_sma(close_prices, self.long_window)
        
        # RSI
        rsi = self.calculate_rsi(close_prices, self.rsi_window)
        
        # Bollinger Bands
        bb = self.calculate_bollinger_bands(close_prices)
        
        # Get latest values
        current_price = close_prices.iloc[-1]
        current_sma_short = sma_short.iloc[-1]
        current_sma_long = sma_long.iloc[-1]
        current_rsi = rsi.iloc[-1]
        current_bb_upper = bb['upper'].iloc[-1]
        current_bb_lower = bb['lower'].iloc[-1]
        
        # Generate signal
        signal = 0
        confidence = 0.0
        metadata = {}
        
        # Moving average crossover
        if current_sma_short > current_sma_long:
            signal += 0.4
            metadata['ma_signal'] = 'bullish'
        else:
            signal -= 0.4
            metadata['ma_signal'] = 'bearish'
            
        # RSI oversold/overbought
        if current_rsi < self.rsi_oversold:
            signal += 0.3
            metadata['rsi_signal'] = 'oversold'
        elif current_rsi > self.rsi_overbought:
            signal -= 0.3
            metadata['rsi_signal'] = 'overbought'
        else:
            metadata['rsi_signal'] = 'neutral'
            
        # Bollinger Bands
        if current_price < current_bb_lower:
            signal += 0.3
            metadata['bb_signal'] = 'oversold'
        elif current_price > current_bb_upper:
            signal -= 0.3
            metadata['bb_signal'] = 'overbought'
        else:
            metadata['bb_signal'] = 'neutral'
            
        # Normalize signal and calculate confidence
        signal = np.clip(signal, -1, 1)
        confidence = abs(signal)
        
        # Convert to discrete signal
        if signal > 0.2:
            discrete_signal = 1
        elif signal < -0.2:
            discrete_signal = -1
        else:
            discrete_signal = 0
            
        self.signals_generated += 1
        self.last_signal_time = data.index[-1]
        
        return {
            'signal': discrete_signal,
            'confidence': confidence,
            'metadata': {
                'price': current_price,
                'sma_short': current_sma_short,
                'sma_long': current_sma_long,
                'rsi': current_rsi,
                'bb_position': (current_price - current_bb_lower) / (current_bb_upper - current_bb_lower),
                **metadata
            }
        }

if __name__ == "__main__":
    print("✅ BTC Technical Alpha Model Template Ready")
