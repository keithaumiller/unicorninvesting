"""
Optimized IBKR ETH Data Collector - 1-Minute Bars
Unicorn Investing Platform - Phase 1 Implementation

Based on IBKR data feed analysis, this collector focuses on:
- 1-minute bar collection (1000+ bars available)
- Real-time data with 0-second delay
- HTTP polling (simpler than WebSocket)
- Professional-grade data quality via ZEROHASH
"""

import requests
import pandas as pd
import numpy as np
import time
import json
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
import os
import urllib3

# Disable SSL warnings for local IBKR Gateway
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

logger = logging.getLogger(__name__)

@dataclass
class ETHDataPoint:
    """Single ETH data point with all OHLCV data"""
    timestamp: pd.Timestamp
    open: float
    high: float
    low: float
    close: float
    volume: float
    
    def to_dict(self) -> Dict:
        return {
            'timestamp': self.timestamp.isoformat(),
            'open': self.open,
            'high': self.high,
            'low': self.low,
            'close': self.close,
            'volume': self.volume
        }

class OptimizedETHCollector:
    """
    Optimized ETH data collector using IBKR 1-minute bars
    
    Features:
    - 1-minute bar polling (optimal for ETH trading)
    - Real-time data with 0-second delay
    - Data quality validation
    - Efficient storage and retrieval
    - Technical indicator integration ready
    """
    
    def __init__(self, base_url: str = "http://localhost:5000/v1/api"):
        self.base_url = base_url
        self.session = requests.Session()
        self.session.verify = False
        
        # ETH contract details (from analysis)
        self.eth_contract_id = 541686654  # ETH via ZEROHASH
        
        # Data storage
        self.data_buffer = []
        self.max_buffer_size = 1440  # 24 hours of 1-minute bars
        
        # Configuration
        self.collection_interval = 60  # 1 minute
        self.retry_attempts = 3
        self.retry_delay = 5
        
        # Metrics
        self.stats = {
            'bars_collected': 0,
            'failed_requests': 0,
            'last_update': None,
            'data_quality_score': 0.0
        }
        
        logger.info("✅ Optimized ETH Collector initialized")
        
    def authenticate(self) -> bool:
        """Check IBKR Gateway authentication status"""
        try:
            response = self.session.get(f"{self.base_url}/iserver/auth/status")
            if response.status_code == 200:
                auth_data = response.json()
                authenticated = auth_data.get('authenticated', False)
                connected = auth_data.get('connected', False)
                
                if authenticated and connected:
                    logger.info("🔐 IBKR Gateway: ✅ Authenticated and Connected")
                    return True
                else:
                    logger.warning(f"🔐 IBKR Gateway: ❌ Auth: {authenticated}, Connected: {connected}")
                    return False
            else:
                logger.error(f"❌ Auth check failed: HTTP {response.status_code}")
                return False
        except Exception as e:
            logger.error(f"❌ Authentication error: {e}")
            return False
            
    def collect_minute_bars(self, lookback_minutes: int = 1440) -> List[ETHDataPoint]:
        """
        Collect 1-minute ETH bars from IBKR
        
        Args:
            lookback_minutes: Number of minutes to look back (default: 24 hours)
            
        Returns:
            List of ETH data points
        """
        try:
            # Request 1-minute bars
            params = {
                'conid': self.eth_contract_id,
                'period': '1d',  # Get today's data
                'bar': '1min',   # 1-minute bars
                'outsideRth': 'true'
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/history",
                params=params
            )
            
            if response.status_code == 200:
                data = response.json()
                
                if 'data' in data and data['data']:
                    bars = []
                    raw_bars = data['data']
                    
                    logger.info(f"📊 Received {len(raw_bars)} 1-minute bars")
                    
                    for bar_data in raw_bars:
                        try:
                            # Convert timestamp from milliseconds
                            timestamp = pd.Timestamp(bar_data['t'], unit='ms', tz='UTC')
                            
                            bar = ETHDataPoint(
                                timestamp=timestamp,
                                open=float(bar_data['o']),
                                high=float(bar_data['h']),
                                low=float(bar_data['l']),
                                close=float(bar_data['c']),
                                volume=float(bar_data.get('v', 0))
                            )
                            
                            bars.append(bar)
                            
                        except (KeyError, ValueError, TypeError) as e:
                            logger.warning(f"⚠️ Invalid bar data: {bar_data} - {e}")
                            continue
                    
                    # Update statistics
                    self.stats['bars_collected'] += len(bars)
                    self.stats['last_update'] = datetime.utcnow()
                    self.stats['data_quality_score'] = len(bars) / len(raw_bars) if raw_bars else 0
                    
                    logger.info(f"✅ Processed {len(bars)} valid ETH bars")
                    return bars
                    
                else:
                    logger.warning("⚠️ No data returned from IBKR")
                    return []
                    
            else:
                logger.error(f"❌ Data request failed: HTTP {response.status_code}")
                self.stats['failed_requests'] += 1
                return []
                
        except Exception as e:
            logger.error(f"❌ Data collection error: {e}")
            self.stats['failed_requests'] += 1
            return []
            
    def get_real_time_snapshot(self) -> Optional[Dict]:
        """Get real-time ETH market data snapshot"""
        try:
            params = {
                'conids': str(self.eth_contract_id),
                'fields': '31,55,70,71,84,86'  # Last, Bid, High, Low, Volume, Close
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/snapshot",
                params=params
            )
            
            if response.status_code == 200:
                data = response.json()
                if isinstance(data, list) and data:
                    snapshot = data[0]
                    
                    # Extract real-time data
                    real_time_data = {
                        'timestamp': datetime.utcnow().isoformat(),
                        'last_price': snapshot.get('31'),
                        'bid': snapshot.get('55'),
                        'high': snapshot.get('70'),
                        'low': snapshot.get('71'),
                        'volume': snapshot.get('84'),
                        'close': snapshot.get('86'),
                        'contract_id': snapshot.get('conid')
                    }
                    
                    logger.info(f"📈 Real-time ETH: ${real_time_data.get('last_price', 'N/A')}")
                    return real_time_data
                    
            return None
            
        except Exception as e:
            logger.error(f"❌ Real-time snapshot error: {e}")
            return None
            
    def update_data_buffer(self, new_bars: List[ETHDataPoint]):
        """Update internal data buffer with new bars"""
        if not new_bars:
            return
            
        # Add new bars to buffer
        self.data_buffer.extend(new_bars)
        
        # Sort by timestamp
        self.data_buffer.sort(key=lambda x: x.timestamp)
        
        # Trim buffer to max size (keep most recent)
        if len(self.data_buffer) > self.max_buffer_size:
            self.data_buffer = self.data_buffer[-self.max_buffer_size:]
            
        logger.info(f"📊 Data buffer updated: {len(self.data_buffer)} bars")
        
    def get_latest_bars(self, count: int = 100) -> List[ETHDataPoint]:
        """Get the most recent bars from buffer"""
        if not self.data_buffer:
            return []
        return self.data_buffer[-count:]
        
    def to_dataframe(self, bars: List[ETHDataPoint] = None) -> pd.DataFrame:
        """Convert bars to pandas DataFrame for analysis"""
        if bars is None:
            bars = self.data_buffer
            
        if not bars:
            return pd.DataFrame()
            
        data = []
        for bar in bars:
            data.append({
                'timestamp': bar.timestamp,
                'open': bar.open,
                'high': bar.high,
                'low': bar.low,
                'close': bar.close,
                'volume': bar.volume
            })
            
        df = pd.DataFrame(data)
        df.set_index('timestamp', inplace=True)
        df.sort_index(inplace=True)
        
        return df
        
    def calculate_basic_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """Calculate basic technical indicators"""
        if df.empty:
            return df
            
        # Simple Moving Averages
        df['sma_10'] = df['close'].rolling(window=10).mean()
        df['sma_20'] = df['close'].rolling(window=20).mean()
        df['sma_50'] = df['close'].rolling(window=50).mean()
        
        # Price changes
        df['price_change'] = df['close'].pct_change()
        df['price_change_1h'] = df['close'].pct_change(periods=60)  # 60 minutes
        
        # Volume indicators
        df['volume_sma'] = df['volume'].rolling(window=20).mean()
        df['volume_ratio'] = df['volume'] / df['volume_sma']
        
        # Volatility
        df['volatility'] = df['price_change'].rolling(window=20).std() * np.sqrt(60 * 24)  # Annualized
        
        return df
        
    def run_continuous_collection(self, duration_hours: Optional[int] = None):
        """
        Run continuous data collection
        
        Args:
            duration_hours: How long to run (None = forever)
        """
        start_time = datetime.utcnow()
        logger.info(f"🚀 Starting continuous ETH data collection...")
        
        # Check authentication
        if not self.authenticate():
            logger.error("❌ Authentication failed - cannot start collection")
            return
            
        try:
            while True:
                # Collect new bars
                bars = self.collect_minute_bars()
                
                if bars:
                    self.update_data_buffer(bars)
                    
                    # Log latest data
                    latest_bar = bars[-1]
                    logger.info(f"📊 Latest ETH: ${latest_bar.close:.2f} "
                              f"Vol: {latest_bar.volume:.2f} "
                              f"Time: {latest_bar.timestamp}")
                
                # Check duration limit
                if duration_hours:
                    elapsed = (datetime.utcnow() - start_time).total_seconds() / 3600
                    if elapsed >= duration_hours:
                        logger.info(f"✅ Collection completed: {elapsed:.1f} hours")
                        break
                
                # Wait for next collection
                logger.info(f"⏰ Waiting {self.collection_interval} seconds...")
                time.sleep(self.collection_interval)
                
        except KeyboardInterrupt:
            logger.info("⏹️ Collection stopped by user")
        except Exception as e:
            logger.error(f"❌ Collection error: {e}")
            
    def get_statistics(self) -> Dict:
        """Get collection statistics"""
        return {
            **self.stats,
            'buffer_size': len(self.data_buffer),
            'data_range': {
                'start': self.data_buffer[0].timestamp.isoformat() if self.data_buffer else None,
                'end': self.data_buffer[-1].timestamp.isoformat() if self.data_buffer else None
            },
            'collection_rate': f"{self.stats['bars_collected']}/{self.stats['bars_collected'] + self.stats['failed_requests']}" if self.stats['bars_collected'] > 0 else "0/0"
        }
        
    def save_data(self, filename: Optional[str] = None):
        """Save collected data to file"""
        if not self.data_buffer:
            logger.warning("⚠️ No data to save")
            return
            
        if filename is None:
            timestamp = datetime.utcnow().strftime("%Y%m%d_%H%M%S")
            filename = f"eth_1min_data_{timestamp}.json"
            
        # Convert to serializable format
        data = {
            'metadata': {
                'collection_time': datetime.utcnow().isoformat(),
                'contract_id': self.eth_contract_id,
                'bar_count': len(self.data_buffer),
                'timeframe': '1min',
                'statistics': {k: v.isoformat() if isinstance(v, datetime) else v for k, v in self.get_statistics().items()}
            },
            'data': [bar.to_dict() for bar in self.data_buffer]
        }
        
        # Ensure directory exists
        os.makedirs('data/eth_1min', exist_ok=True)
        filepath = os.path.join('data/eth_1min', filename)
        
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
            
        logger.info(f"💾 Data saved: {filepath} ({len(self.data_buffer)} bars)")

# Example usage and testing
if __name__ == "__main__":
    # Setup logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    print("🦄 Optimized ETH Data Collector")
    print("=" * 50)
    print("Based on IBKR data feed analysis:")
    print("✅ 1-minute bars: 1000+ available")
    print("✅ Real-time data: 0-second delay")
    print("✅ Professional-grade via ZEROHASH")
    print("✅ HTTP polling (simpler than WebSocket)")
    print()
    
    # Initialize collector
    collector = OptimizedETHCollector()
    
    # Test authentication
    if not collector.authenticate():
        print("❌ IBKR Gateway not available")
        exit(1)
    
    # Test data collection
    print("📊 Testing 1-minute bar collection...")
    bars = collector.collect_minute_bars()
    
    if bars:
        print(f"✅ Collected {len(bars)} bars")
        
        # Show sample data
        latest_bar = bars[-1]
        print(f"📈 Latest bar:")
        print(f"   Time: {latest_bar.timestamp}")
        print(f"   OHLC: ${latest_bar.open:.2f} ${latest_bar.high:.2f} ${latest_bar.low:.2f} ${latest_bar.close:.2f}")
        print(f"   Volume: {latest_bar.volume:.2f}")
        
        # Test DataFrame conversion
        collector.update_data_buffer(bars)
        df = collector.to_dataframe()
        df = collector.calculate_basic_indicators(df)
        
        print(f"\n📊 DataFrame created: {len(df)} rows")
        if not df.empty:
            print(f"   Latest SMA(20): ${df['sma_20'].iloc[-1]:.2f}")
            print(f"   1h Price Change: {df['price_change_1h'].iloc[-1]*100:.2f}%")
            print(f"   Volatility: {df['volatility'].iloc[-1]*100:.2f}%")
        
        # Test real-time snapshot
        print("\n📸 Testing real-time snapshot...")
        snapshot = collector.get_real_time_snapshot()
        if snapshot:
            print(f"   Real-time price: ${snapshot.get('last_price', 'N/A')}")
        
        # Save test data
        collector.save_data()
        
        # Show statistics
        stats = collector.get_statistics()
        print(f"\n📈 Collection Statistics:")
        print(f"   Bars collected: {stats['bars_collected']}")
        print(f"   Data quality: {stats['data_quality_score']*100:.1f}%")
        print(f"   Buffer size: {stats['buffer_size']}")
        
        print("\n✅ Test completed successfully!")
        print("\n💡 To run continuous collection:")
        print("   collector.run_continuous_collection(duration_hours=1)")
        
    else:
        print("❌ No data collected")
