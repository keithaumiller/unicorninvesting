#!/usr/bin/env python3
"""
Simulated ETH Data Feed for Algorithm Testing
Provides realistic ETH price data for testing algorithms while IBKR bridge is being configured
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import time
import json
from typing import Dict, List, Optional
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SimulatedETHConnector:
    """Simulated ETH data feed for algorithm testing"""
    
    def __init__(self, base_price=3500.0, volatility=0.15):
        self.base_price = base_price
        self.volatility = volatility
        self.current_price = base_price
        self.price_history = []
        self.start_time = datetime.now()
        
        # Generate initial historical data
        self._generate_initial_history()
        
    def _generate_initial_history(self, days=30):
        """Generate realistic historical ETH price data"""
        
        # Create time series for past 30 days
        end_time = datetime.now()
        start_time = end_time - timedelta(days=days)
        
        # Generate hourly timestamps
        timestamps = pd.date_range(start=start_time, end=end_time, freq='H')
        
        # Generate price movements using geometric brownian motion
        np.random.seed(42)  # For reproducible results
        
        prices = []
        current_price = self.base_price
        
        for i, timestamp in enumerate(timestamps):
            # Random walk with drift
            dt = 1.0 / (24 * 365)  # Hourly steps
            drift = 0.05 * dt  # 5% annual drift
            shock = np.random.normal(0, self.volatility * np.sqrt(dt))
            
            # Add some market hours effect (more volatile during certain hours)
            hour = timestamp.hour
            if 8 <= hour <= 16:  # Market hours volatility boost
                shock *= 1.2
            elif 0 <= hour <= 6:  # Low volatility overnight
                shock *= 0.7
                
            # Add some weekend effect
            if timestamp.weekday() >= 5:  # Weekend
                shock *= 0.5
                
            # Calculate new price
            price_change = current_price * (drift + shock)
            current_price = max(current_price + price_change, 100)  # Floor at $100
            
            # Create OHLCV data (simplified)
            high = current_price * (1 + abs(shock) * 0.5)
            low = current_price * (1 - abs(shock) * 0.5)
            open_price = prices[-1] if prices else current_price
            volume = np.random.randint(1000, 10000)
            
            prices.append({
                'timestamp': timestamp,
                'open': open_price,
                'high': high,
                'low': low,
                'close': current_price,
                'volume': volume
            })
        
        self.price_history = prices
        self.current_price = current_price
        logger.info(f"Generated {len(prices)} historical price points")
        
    def get_current_price(self) -> Dict:
        """Get current ETH price snapshot"""
        
        # Simulate small price movement
        change_pct = np.random.normal(0, 0.001)  # 0.1% standard deviation
        self.current_price *= (1 + change_pct)
        
        # Generate bid/ask spread
        spread = self.current_price * 0.0005  # 0.05% spread
        bid = self.current_price - spread/2
        ask = self.current_price + spread/2
        
        return {
            'symbol': 'ETHUSD',
            'timestamp': datetime.now(),
            'last_price': round(self.current_price, 2),
            'bid': round(bid, 2),
            'ask': round(ask, 2),
            'volume': np.random.randint(100, 1000),
            'change_24h': round(np.random.normal(0, 0.03), 4)  # 3% daily volatility
        }
    
    def get_historical_data(self, hours=24) -> pd.DataFrame:
        """Get historical ETH data as DataFrame"""
        
        # Get recent history
        if hours <= len(self.price_history):
            recent_data = self.price_history[-hours:]
        else:
            recent_data = self.price_history
            
        # Convert to DataFrame
        df = pd.DataFrame(recent_data)
        if not df.empty:
            df.set_index('timestamp', inplace=True)
            
        return df
    
    def create_market_data_feed(self) -> Dict:
        """Create comprehensive market data feed for algorithms"""
        
        current_snapshot = self.get_current_price()
        historical_df = self.get_historical_data(hours=48)
        
        # Calculate some technical indicators
        if not historical_df.empty:
            closes = historical_df['close']
            
            # Simple moving averages
            sma_10 = closes.rolling(window=10).mean().iloc[-1] if len(closes) >= 10 else closes.mean()
            sma_20 = closes.rolling(window=20).mean().iloc[-1] if len(closes) >= 20 else closes.mean()
            
            # Volatility (rolling std)
            volatility_24h = closes.rolling(window=24).std().iloc[-1] if len(closes) >= 24 else closes.std()
            
            # Recent performance
            if len(closes) >= 24:
                returns_24h = (closes.iloc[-1] / closes.iloc[-24]) - 1
            else:
                returns_24h = 0
        else:
            sma_10 = sma_20 = self.current_price
            volatility_24h = 0.05
            returns_24h = 0
        
        return {
            'status': 'success',
            'data_source': 'simulated',
            'symbol': 'ETHUSD',
            'current': current_snapshot,
            'historical': historical_df,
            'indicators': {
                'sma_10': round(sma_10, 2),
                'sma_20': round(sma_20, 2),
                'volatility_24h': round(volatility_24h, 4),
                'returns_24h': round(returns_24h, 4)
            },
            'timestamp': datetime.now()
        }

def main():
    """Test the simulated ETH connector"""
    print("=" * 50)
    print("Simulated ETH Data Connector Test")
    print(f"Timestamp: {datetime.now()}")
    print("=" * 50)
    
    connector = SimulatedETHConnector()
    
    # Test current price
    print("\n1. Current Price Snapshot:")
    current = connector.get_current_price()
    print(f"   ETH Price: ${current['last_price']}")
    print(f"   Bid/Ask: ${current['bid']} / ${current['ask']}")
    print(f"   24h Change: {current['change_24h']*100:.2f}%")
    
    # Test historical data
    print("\n2. Historical Data:")
    historical = connector.get_historical_data(hours=24)
    print(f"   Data points: {len(historical)}")
    if not historical.empty:
        print(f"   Price range: ${historical['low'].min():.2f} - ${historical['high'].max():.2f}")
        print(f"   Latest close: ${historical['close'].iloc[-1]:.2f}")
    
    # Test full market feed
    print("\n3. Market Data Feed:")
    feed = connector.create_market_data_feed()
    print(f"   Status: {feed['status']}")
    print(f"   SMA 10/20: ${feed['indicators']['sma_10']} / ${feed['indicators']['sma_20']}")
    print(f"   24h Volatility: {feed['indicators']['volatility_24h']*100:.2f}%")
    print(f"   24h Returns: {feed['indicators']['returns_24h']*100:.2f}%")
    
    print("\n✅ Simulated data connector ready for algorithm testing!")
    print("💡 Use this for testing while IBKR bridge is being configured")

if __name__ == "__main__":
    main()
