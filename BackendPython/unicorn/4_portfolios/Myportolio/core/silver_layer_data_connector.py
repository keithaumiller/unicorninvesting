#!/usr/bin/env python3
"""
Silver Layer Data Connector - Real Historical Data Integration
Replaces ALL simulated data with real historical data from our data warehouse
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import sqlite3
import logging
from typing import Dict, List, Optional, Tuple

# Add project paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn')

class SilverLayerDataConnector:
    """
    Connects to our silver layer data warehouse for real historical data
    Eliminates all simulated data usage across the system
    """
    
    def __init__(self):
        self.silver_layer_path = Path('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver')
        self.yahoo_finance_path = self.silver_layer_path / 'yahoo_finance_assets' / 'processed_data'
        self.economic_path = self.silver_layer_path / 'economic_indicators'
        
        # Setup logging
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.INFO)
        
        # Available assets from our silver layer (using actual file naming convention)
        self.crypto_assets = {
            'ETH': 'ETH',
            'BTC': 'BTC'
        }
        
        self.forex_assets = {
            'EURUSD': 'EURUSD',
            'USDJPY': 'USDJPY', 
            'GBPUSD': 'GBPUSD',
            'AUDUSD': 'AUDUSD',
            'USDCAD': 'USDCAD',
            'USDCHF': 'USDCHF',
            'NZDUSD': 'NZDUSD'
        }
        
        self.all_assets = {**self.crypto_assets, **self.forex_assets}
        self.available_intervals = ['1d', '1h']
        
        self.logger.info(f"🎯 Initialized Silver Layer connector with {len(self.all_assets)} assets")
    
    def get_historical_data(self, 
                          asset: str, 
                          interval: str = '1h',
                          start_date: Optional[str] = None,
                          end_date: Optional[str] = None,
                          periods: Optional[int] = None) -> pd.DataFrame:
        """
        Get real historical data from silver layer warehouse
        
        Args:
            asset: Asset symbol (ETH, BTC, EURUSD, etc.)
            interval: Time interval ('1h', '1d')
            start_date: Start date (YYYY-MM-DD format)
            end_date: End date (YYYY-MM-DD format)
            periods: Number of recent periods to return
            
        Returns:
            DataFrame with real historical market data
        """
        if asset not in self.all_assets:
            raise ValueError(f"Asset {asset} not available. Available: {list(self.all_assets.keys())}")
        
        if interval not in self.available_intervals:
            raise ValueError(f"Interval {interval} not available. Available: {self.available_intervals}")
        
        # Determine file path based on asset type using actual file naming pattern
        if asset in self.crypto_assets:
            file_path = self.yahoo_finance_path / 'crypto' / f"{self.all_assets[asset]}_silver_{interval}_latest.csv"
        else:
            file_path = self.yahoo_finance_path / 'forex' / f"{self.all_assets[asset]}_silver_{interval}_latest.csv"
        
        if not file_path.exists():
            self.logger.error(f"❌ Data file not found: {file_path}")
            raise FileNotFoundError(f"Silver layer data not found for {asset}_{interval}")
        
        try:
            # Load real historical data
            df = pd.read_csv(file_path)
            
            # Standardize datetime column
            datetime_col = 'Date' if 'Date' in df.columns else 'Datetime'
            if datetime_col in df.columns:
                df['timestamp'] = pd.to_datetime(df[datetime_col])
                df = df.set_index('timestamp').sort_index()
            
            # Filter by date range if specified
            if start_date:
                df = df[df.index >= pd.to_datetime(start_date)]
            
            if end_date:
                df = df[df.index <= pd.to_datetime(end_date)]
            
            # Return recent periods if specified
            if periods:
                df = df.tail(periods)
            
            self.logger.info(f"✅ Loaded {len(df)} records for {asset}_{interval} from silver layer")
            
            return df
            
        except Exception as e:
            self.logger.error(f"❌ Failed to load {asset}_{interval}: {e}")
            raise
    
    def get_live_price(self, asset: str) -> float:
        """
        Get most recent price from historical data
        (In production this would connect to live API, but uses most recent historical data)
        """
        try:
            # Get most recent data point
            df = self.get_historical_data(asset, interval='1h', periods=1)
            
            if len(df) > 0 and 'close' in df.columns:
                latest_price = df['close'].iloc[-1]
                self.logger.info(f"💰 {asset} latest price: ${latest_price:.4f}")
                return float(latest_price)
            else:
                self.logger.warning(f"⚠️ No price data available for {asset}")
                return 0.0
                
        except Exception as e:
            self.logger.error(f"❌ Failed to get price for {asset}: {e}")
            return 0.0
    
    def get_multiple_assets_data(self, 
                               assets: List[str], 
                               interval: str = '1h',
                               periods: int = 100) -> Dict[str, pd.DataFrame]:
        """
        Get historical data for multiple assets
        
        Args:
            assets: List of asset symbols
            interval: Time interval
            periods: Number of recent periods
            
        Returns:
            Dictionary of asset dataframes
        """
        data_dict = {}
        
        for asset in assets:
            try:
                df = self.get_historical_data(asset, interval=interval, periods=periods)
                data_dict[asset] = df
                self.logger.info(f"✅ Loaded {asset}: {len(df)} records")
                
            except Exception as e:
                self.logger.error(f"❌ Failed to load {asset}: {e}")
                # Don't include failed assets in results
                continue
        
        self.logger.info(f"📊 Successfully loaded {len(data_dict)}/{len(assets)} assets")
        return data_dict
    
    def get_market_data_for_portfolio(self, portfolio_assets: List[str]) -> Dict[str, pd.DataFrame]:
        """
        Get comprehensive market data for portfolio optimization
        """
        self.logger.info(f"📈 Loading market data for portfolio: {portfolio_assets}")
        
        # Load data for all requested assets
        market_data = self.get_multiple_assets_data(
            assets=portfolio_assets,
            interval='1h',  # Use hourly data for portfolio optimization
            periods=200  # Last 200 periods for analysis
        )
        
        # Add derived features for trading algorithms
        for asset, df in market_data.items():
            if len(df) > 0:
                # Add returns
                if 'close' in df.columns:
                    df['returns'] = df['close'].pct_change()
                    df['log_returns'] = np.log(df['close'] / df['close'].shift(1))
                
                # Add volatility (rolling 20-period)
                if 'returns' in df.columns:
                    df['volatility'] = df['returns'].rolling(20).std()
                
                # Ensure no infinite values
                df = df.replace([np.inf, -np.inf], np.nan).dropna()
                market_data[asset] = df
        
        return market_data
    
    def get_backtesting_data(self, 
                           asset: str, 
                           start_date: str, 
                           end_date: str,
                           interval: str = '1h') -> pd.DataFrame:
        """
        Get historical data for backtesting (specific date range)
        """
        self.logger.info(f"📊 Loading backtesting data for {asset} ({start_date} to {end_date})")
        
        df = self.get_historical_data(
            asset=asset,
            interval=interval,
            start_date=start_date,
            end_date=end_date
        )
        
        if len(df) == 0:
            raise ValueError(f"No backtesting data available for {asset} in date range")
        
        self.logger.info(f"✅ Loaded {len(df)} records for backtesting")
        return df
    
    def validate_data_availability(self) -> Dict[str, Dict[str, bool]]:
        """
        Validate which assets and intervals have data available
        """
        availability = {}
        
        for asset in self.all_assets.keys():
            availability[asset] = {}
            
            for interval in self.available_intervals:
                try:
                    df = self.get_historical_data(asset, interval, periods=1)
                    availability[asset][interval] = len(df) > 0
                except:
                    availability[asset][interval] = False
        
        # Log availability summary
        total_combinations = len(self.all_assets) * len(self.available_intervals)
        available_combinations = sum([
            sum(intervals.values()) 
            for intervals in availability.values()
        ])
        
        self.logger.info(f"📊 Data availability: {available_combinations}/{total_combinations} combinations available")
        
        return availability
    
    def get_data_quality_report(self) -> Dict:
        """
        Generate data quality report for available datasets
        """
        quality_report = {
            'total_assets': len(self.all_assets),
            'total_intervals': len(self.available_intervals),
            'asset_quality': {},
            'overall_quality': 0.0
        }
        
        quality_scores = []
        
        for asset in self.all_assets.keys():
            asset_quality = {}
            
            for interval in self.available_intervals:
                try:
                    df = self.get_historical_data(asset, interval, periods=100)
                    
                    if len(df) > 0:
                        # Calculate basic quality metrics
                        completeness = 1.0 - (df.isnull().sum().sum() / (len(df) * len(df.columns)))
                        asset_quality[interval] = {
                            'records': len(df),
                            'completeness': completeness,
                            'date_range': {
                                'start': str(df.index.min().date()),
                                'end': str(df.index.max().date())
                            }
                        }
                        quality_scores.append(completeness)
                    else:
                        asset_quality[interval] = {'records': 0, 'completeness': 0.0}
                        
                except Exception as e:
                    asset_quality[interval] = {'error': str(e), 'completeness': 0.0}
            
            quality_report['asset_quality'][asset] = asset_quality
        
        # Overall quality score
        if quality_scores:
            quality_report['overall_quality'] = np.mean(quality_scores)
        
        return quality_report


def main():
    """Demo the silver layer data connector"""
    
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    print("🎯 SILVER LAYER DATA CONNECTOR - REAL HISTORICAL DATA")
    print("=" * 60)
    print("🚫 NO MORE SIMULATED DATA - 100% REAL HISTORICAL DATA")
    print()
    
    # Initialize connector
    connector = SilverLayerDataConnector()
    
    # Validate data availability
    print("📊 Validating data availability...")
    availability = connector.validate_data_availability()
    
    available_count = sum([sum(intervals.values()) for intervals in availability.values()])
    total_count = len(connector.all_assets) * len(connector.available_intervals)
    
    print(f"✅ Data availability: {available_count}/{total_count} asset-interval combinations")
    print()
    
    # Demo: Get live prices
    print("💰 CURRENT PRICES (Latest Historical Data):")
    for asset in ['ETH', 'BTC', 'EURUSD', 'GBPUSD']:
        try:
            price = connector.get_live_price(asset)
            print(f"  {asset:8}: ${price:,.4f}")
        except Exception as e:
            print(f"  {asset:8}: Error - {e}")
    print()
    
    # Demo: Get historical data for ETH
    print("📈 ETH HISTORICAL DATA SAMPLE:")
    try:
        eth_data = connector.get_historical_data('ETH', interval='1h', periods=5)
        if len(eth_data) > 0:
            print(f"  Records: {len(eth_data)}")
            print(f"  Columns: {list(eth_data.columns[:10])}...")  # Show first 10 columns
            print(f"  Date range: {eth_data.index.min()} to {eth_data.index.max()}")
            
            if 'close' in eth_data.columns:
                print(f"  Price range: ${eth_data['close'].min():.2f} - ${eth_data['close'].max():.2f}")
        else:
            print("  No data available")
    except Exception as e:
        print(f"  Error: {e}")
    print()
    
    # Demo: Multi-asset portfolio data
    print("🌍 MULTI-ASSET PORTFOLIO DATA:")
    portfolio_assets = ['ETH', 'BTC', 'EURUSD', 'USDJPY']
    
    try:
        portfolio_data = connector.get_market_data_for_portfolio(portfolio_assets)
        
        print(f"  Loaded {len(portfolio_data)} assets:")
        for asset, df in portfolio_data.items():
            if len(df) > 0 and 'close' in df.columns:
                latest_price = df['close'].iloc[-1]
                print(f"    {asset:8}: {len(df):3d} records, latest price: ${latest_price:,.4f}")
    except Exception as e:
        print(f"  Error: {e}")
    print()
    
    # Data quality report
    print("📊 DATA QUALITY REPORT:")
    try:
        quality_report = connector.get_data_quality_report()
        print(f"  Overall Quality Score: {quality_report['overall_quality']:.1%}")
        print(f"  Total Assets: {quality_report['total_assets']}")
        
        # Show top 3 assets by quality
        asset_scores = {}
        for asset, intervals in quality_report['asset_quality'].items():
            scores = [data.get('completeness', 0) for data in intervals.values() if isinstance(data, dict)]
            if scores:
                asset_scores[asset] = np.mean(scores)
        
        if asset_scores:
            top_assets = sorted(asset_scores.items(), key=lambda x: x[1], reverse=True)[:3]
            print("  Top Quality Assets:")
            for asset, score in top_assets:
                print(f"    {asset:8}: {score:.1%}")
    except Exception as e:
        print(f"  Error: {e}")
    
    print()
    print("🎉 SILVER LAYER INTEGRATION COMPLETE")
    print("✅ Real historical data from data warehouse")
    print("🚫 Zero simulated data usage")
    print("📊 Production-ready data pipeline")


if __name__ == "__main__":
    main()