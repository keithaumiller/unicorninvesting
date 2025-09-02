#!/usr/bin/env python3
"""
IBKR Live Data Integration for Production Model Manager
Unicorn Investing Platform

Integrates with IBKR Gateway to fetch live ETH data for model training
and production performance tracking.

Author: Unicorn Investing Platform
Date: September 2, 2025
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
import urllib3

# Disable SSL warnings for local IBKR Gateway
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

logger = logging.getLogger(__name__)

@dataclass
class IBKRETHDataPoint:
    """Single ETH data point from IBKR"""
    timestamp: datetime
    open: float
    high: float
    low: float
    close: float
    volume: float
    source: str = "IBKR"

class IBKRLiveDataIntegration:
    """
    IBKR Live Data Integration for ETH Trading Models
    
    Provides real-time and historical ETH data from IBKR Gateway for:
    - Model training with live data
    - Production performance tracking
    - Real-time forecast validation
    """
    
    def __init__(self, base_url: str = "http://localhost:5000/v1/api"):
        self.base_url = base_url
        self.eth_contract_id = 541686654  # ETH/USD contract from IBKR
        self.session = requests.Session()
        self.session.verify = False  # For local IBKR Gateway
        
        # Setup logging
        self.logger = logging.getLogger(__name__)
        
        # Connection state
        self.authenticated = False
        self.last_auth_check = None
        
    def authenticate(self) -> bool:
        """Check IBKR Gateway authentication status"""
        try:
            response = self.session.get(
                f"{self.base_url}/iserver/auth/status",
                timeout=5
            )
            
            if response.status_code == 200:
                auth_data = response.json()
                self.authenticated = auth_data.get('authenticated', False)
                self.last_auth_check = datetime.now()
                
                if self.authenticated:
                    self.logger.info("✅ IBKR Gateway authenticated successfully")
                else:
                    self.logger.warning("⚠️ IBKR Gateway not authenticated")
                    
                return self.authenticated
            else:
                self.logger.error(f"❌ IBKR Gateway auth check failed: HTTP {response.status_code}")
                return False
                
        except Exception as e:
            self.logger.error(f"❌ IBKR Gateway connection failed: {e}")
            return False
    
    def get_live_eth_price(self) -> Optional[Dict]:
        """Get current live ETH price from IBKR"""
        try:
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/snapshot",
                params={
                    'conids': str(self.eth_contract_id),
                    'fields': '31,55,70,71,84,86'  # Last, Symbol, High, Low, Bid, Ask
                },
                timeout=2
            )
            
            if response.status_code == 200:
                data = response.json()
                if data and len(data) > 0:
                    eth_data = data[0]
                    
                    try:
                        price = float(eth_data.get('31', 0))  # Last price
                        bid = float(eth_data.get('84', price))  # Bid
                        ask = float(eth_data.get('86', price))  # Ask
                        high = float(eth_data.get('70', price))  # High
                        low = float(eth_data.get('71', price))  # Low
                        
                        if price > 0:
                            return {
                                'price': price,
                                'bid': bid,
                                'ask': ask,
                                'high': high,
                                'low': low,
                                'timestamp': datetime.now(),
                                'source': 'IBKR_Live'
                            }
                    except (ValueError, TypeError) as e:
                        self.logger.warning(f"Price conversion error: {e}")
                        
            self.logger.warning(f"Live price fetch failed: HTTP {response.status_code}")
            return None
            
        except Exception as e:
            self.logger.error(f"Live price error: {e}")
            return None
    
    def get_historical_minute_bars(self, lookback_hours: int = 24) -> List[IBKRETHDataPoint]:
        """
        Get historical 1-minute ETH bars from IBKR
        
        Args:
            lookback_hours: Hours of data to retrieve (up to 24)
            
        Returns:
            List of ETH data points
        """
        try:
            # Calculate period based on lookback
            if lookback_hours <= 1:
                period = "1h"
            elif lookback_hours <= 24:
                period = "1d"
            else:
                period = "2d"
                
            params = {
                'conid': self.eth_contract_id,
                'period': period,
                'bar': '1min',
                'outsideRth': 'true'  # Include outside regular trading hours (important for crypto)
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/history",
                params=params,
                timeout=10
            )
            
            if response.status_code == 200:
                data = response.json()
                
                if 'data' in data and data['data']:
                    bars = []
                    raw_bars = data['data']
                    
                    # IBKR returns up to 1000 bars for 1-minute data
                    self.logger.info(f"Retrieved {len(raw_bars)} minute bars from IBKR")
                    
                    for bar in raw_bars:
                        try:
                            timestamp = datetime.fromtimestamp(bar['t'] / 1000)
                            
                            # Validate bar data
                            if all(key in bar for key in ['o', 'h', 'l', 'c', 'v']):
                                data_point = IBKRETHDataPoint(
                                    timestamp=timestamp,
                                    open=float(bar['o']),
                                    high=float(bar['h']),
                                    low=float(bar['l']),
                                    close=float(bar['c']),
                                    volume=float(bar['v']),
                                    source="IBKR_Historical"
                                )
                                bars.append(data_point)
                                
                        except (KeyError, ValueError, TypeError) as e:
                            self.logger.warning(f"Skipping invalid bar: {e}")
                            continue
                    
                    # Filter to requested lookback period
                    if bars:
                        cutoff_time = datetime.now() - timedelta(hours=lookback_hours)
                        bars = [bar for bar in bars if bar.timestamp >= cutoff_time]
                        
                    self.logger.info(f"Processed {len(bars)} valid minute bars")
                    return bars
                else:
                    self.logger.warning("No historical data returned from IBKR")
                    return []
                    
            else:
                self.logger.error(f"Historical data request failed: HTTP {response.status_code}")
                return []
                
        except Exception as e:
            self.logger.error(f"Historical data collection error: {e}")
            return []
    
    def get_historical_hourly_bars(self, lookback_days: int = 7) -> List[IBKRETHDataPoint]:
        """
        Get historical 1-hour ETH bars from IBKR
        
        Args:
            lookback_days: Days of data to retrieve
            
        Returns:
            List of ETH data points
        """
        try:
            # Determine period
            if lookback_days <= 1:
                period = "1d"
            elif lookback_days <= 7:
                period = "1w"
            else:
                period = "1m"
                
            params = {
                'conid': self.eth_contract_id,
                'period': period,
                'bar': '1h',
                'outsideRth': 'true'
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/history",
                params=params,
                timeout=10
            )
            
            if response.status_code == 200:
                data = response.json()
                
                if 'data' in data and data['data']:
                    bars = []
                    for bar in data['data']:
                        try:
                            timestamp = datetime.fromtimestamp(bar['t'] / 1000)
                            
                            if all(key in bar for key in ['o', 'h', 'l', 'c', 'v']):
                                data_point = IBKRETHDataPoint(
                                    timestamp=timestamp,
                                    open=float(bar['o']),
                                    high=float(bar['h']),
                                    low=float(bar['l']),
                                    close=float(bar['c']),
                                    volume=float(bar['v']),
                                    source="IBKR_Historical"
                                )
                                bars.append(data_point)
                                
                        except (KeyError, ValueError, TypeError):
                            continue
                    
                    # Filter to requested lookback period
                    if bars:
                        cutoff_time = datetime.now() - timedelta(days=lookback_days)
                        bars = [bar for bar in bars if bar.timestamp >= cutoff_time]
                        
                    self.logger.info(f"Retrieved {len(bars)} hourly bars")
                    return bars
                else:
                    return []
                    
            else:
                self.logger.error(f"Hourly data request failed: HTTP {response.status_code}")
                return []
                
        except Exception as e:
            self.logger.error(f"Hourly data collection error: {e}")
            return []
    
    def get_historical_daily_bars(self, lookback_days: int = 30) -> List[IBKRETHDataPoint]:
        """
        Get historical daily ETH bars from IBKR
        
        Args:
            lookback_days: Days of data to retrieve
            
        Returns:
            List of ETH data points
        """
        try:
            # Determine period
            if lookback_days <= 30:
                period = "1m"
            elif lookback_days <= 90:
                period = "3m"
            else:
                period = "6m"
                
            params = {
                'conid': self.eth_contract_id,
                'period': period,
                'bar': '1d',
                'outsideRth': 'true'
            }
            
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/history",
                params=params,
                timeout=10
            )
            
            if response.status_code == 200:
                data = response.json()
                
                if 'data' in data and data['data']:
                    bars = []
                    for bar in data['data']:
                        try:
                            timestamp = datetime.fromtimestamp(bar['t'] / 1000)
                            
                            if all(key in bar for key in ['o', 'h', 'l', 'c', 'v']):
                                data_point = IBKRETHDataPoint(
                                    timestamp=timestamp,
                                    open=float(bar['o']),
                                    high=float(bar['h']),
                                    low=float(bar['l']),
                                    close=float(bar['c']),
                                    volume=float(bar['v']),
                                    source="IBKR_Historical"
                                )
                                bars.append(data_point)
                                
                        except (KeyError, ValueError, TypeError):
                            continue
                    
                    # Filter to requested lookback period
                    if bars:
                        cutoff_time = datetime.now() - timedelta(days=lookback_days)
                        bars = [bar for bar in bars if bar.timestamp >= cutoff_time]
                        
                    self.logger.info(f"Retrieved {len(bars)} daily bars")
                    return bars
                else:
                    return []
                    
            else:
                self.logger.error(f"Daily data request failed: HTTP {response.status_code}")
                return []
                
        except Exception as e:
            self.logger.error(f"Daily data collection error: {e}")
            return []
    
    def convert_to_dataframe(self, data_points: List[IBKRETHDataPoint]) -> pd.DataFrame:
        """Convert IBKR data points to pandas DataFrame"""
        if not data_points:
            return pd.DataFrame()
            
        data = []
        for point in data_points:
            data.append({
                'timestamp': point.timestamp,
                'open': point.open,
                'high': point.high,
                'low': point.low,
                'close': point.close,
                'price': point.close,  # Alias for close
                'volume': point.volume,
                'source': point.source
            })
        
        df = pd.DataFrame(data)
        df.set_index('timestamp', inplace=True)
        df.sort_index(inplace=True)
        
        return df
    
    def health_check(self) -> Dict[str, bool]:
        """Perform health check on IBKR data integration"""
        results = {
            'gateway_accessible': False,
            'authenticated': False,
            'live_data_available': False,
            'historical_data_available': False
        }
        
        # Check gateway accessibility
        try:
            response = self.session.get(f"{self.base_url}/iserver/auth/status", timeout=5)
            if response.status_code == 200:
                results['gateway_accessible'] = True
                
                # Check authentication
                auth_data = response.json()
                results['authenticated'] = auth_data.get('authenticated', False)
                
        except Exception as e:
            self.logger.error(f"Gateway health check failed: {e}")
            return results
        
        # Check live data
        if results['authenticated']:
            live_price = self.get_live_eth_price()
            results['live_data_available'] = live_price is not None
            
            # Check historical data (small sample)
            historical_bars = self.get_historical_minute_bars(lookback_hours=1)
            results['historical_data_available'] = len(historical_bars) > 0
            
        return results

# Test integration
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    
    print("🔄 Testing IBKR Live Data Integration...")
    
    integration = IBKRLiveDataIntegration()
    
    # Health check
    health = integration.health_check()
    print("\n📊 Health Check Results:")
    for check, status in health.items():
        print(f"  {'✅' if status else '❌'} {check}: {'OK' if status else 'Failed'}")
    
    if health['authenticated']:
        # Test live price
        print("\n💰 Live ETH Price:")
        live_price = integration.get_live_eth_price()
        if live_price:
            print(f"  Price: ${live_price['price']:,.2f}")
            print(f"  Bid: ${live_price['bid']:,.2f}")
            print(f"  Ask: ${live_price['ask']:,.2f}")
            print(f"  Time: {live_price['timestamp']}")
        
        # Test historical data
        print("\n📈 Historical Data Sample:")
        minute_bars = integration.get_historical_minute_bars(lookback_hours=2)
        if minute_bars:
            df = integration.convert_to_dataframe(minute_bars)
            print(f"  Retrieved: {len(df)} minute bars")
            print(f"  Price range: ${df['price'].min():,.2f} - ${df['price'].max():,.2f}")
            print(f"  Time range: {df.index.min()} to {df.index.max()}")
    else:
        print("\n⚠️ IBKR Gateway not authenticated - limited functionality")
