"""
Forex Data Collector - Multi-Currency Trading System

Issue #36: Multi-Currency Forex Alpha Models & Forecasting System

Extends existing Yahoo Finance and Interactive Brokers connectors for forex data collection.
Leverages the proven ETH data collection infrastructure for forex markets.
"""

import yfinance as yf
import pandas as pd
import os
import sys
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging

# Add yahoo_finance connector to path
parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
yahoo_finance_dir = os.path.join(parent_dir, 'yahoo_finance')
if yahoo_finance_dir not in sys.path:
    sys.path.append(yahoo_finance_dir)

# Import existing Yahoo Finance infrastructure
try:
    from eth_data_collector import ETHDataCollector
except ImportError:
    # Fallback if direct import fails
    ETHDataCollector = None

from .forex_symbols import FOREX_SYMBOLS, get_major_pairs, get_yahoo_symbols, IMPLEMENTATION_PRIORITY


class ForexDataCollector:
    """
    Multi-Currency Forex Data Collector
    
    Extends the existing Yahoo Finance ETH data collector infrastructure
    for comprehensive forex market data collection across 12+ currency pairs.
    
    Features:
    - Leverages proven Yahoo Finance connector
    - Supports all major, cross, and emerging market pairs
    - Integrates with existing data directory structure
    - Economic data integration ready
    - Interactive Brokers connectivity
    """
    
    def __init__(self, base_data_dir: str = None):
        """
        Initialize forex data collector.
        
        Args:
            base_data_dir: Base directory for data storage. If None, uses standard unicorn data structure.
        """
        if base_data_dir is None:
            # Use the standard unicorn data directory structure
            project_root = '/workspaces/unicorninvesting'
            self.base_data_dir = os.path.join(project_root, 'BackendPython', 'unicorn', '1_data_sources', '1_raw', 'data')
        else:
            self.base_data_dir = base_data_dir
        self.logger = logging.getLogger(__name__)
        
        # Setup logging
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        
        # Initialize existing ETH collector if available (for infrastructure reuse)
        if ETHDataCollector:
            self.eth_collector = ETHDataCollector(self.base_data_dir)
        
        # Create forex-specific data directories
        self._setup_forex_directories()
        
        # Load forex symbols
        self.forex_symbols = FOREX_SYMBOLS
        self.major_pairs = get_major_pairs()
        
    def _setup_forex_directories(self):
        """Setup forex-specific data directory structure"""
        providers = ['yahoo_finance', 'interactive_brokers']
        
        for provider in providers:
            # Create provider-level forex directory
            forex_dir = os.path.join(self.base_data_dir, provider, 'forex')
            os.makedirs(forex_dir, exist_ok=True)
            
            # Create directories for each currency pair
            for pair_code, pair_info in FOREX_SYMBOLS.items():
                pair_dir = os.path.join(forex_dir, pair_code)
                os.makedirs(pair_dir, exist_ok=True)
                
        self.logger.info(f"✅ Created forex data directories for {len(FOREX_SYMBOLS)} currency pairs")
    
    def collect_yahoo_forex_data(self, 
                                currency_pairs: Optional[List[str]] = None,
                                period: str = '1mo',
                                interval: str = '1h') -> Dict[str, pd.DataFrame]:
        """
        Collect forex data from Yahoo Finance for specified currency pairs.
        
        Args:
            currency_pairs: List of currency pair codes (e.g., ['EURUSD', 'USDJPY'])
                          If None, collects major pairs
            period: Data period ('1d', '5d', '1mo', '3mo', '6mo', '1y', '2y', '5y', '10y', 'ytd', 'max')
            interval: Data interval ('1m', '2m', '5m', '15m', '30m', '60m', '90m', '1h', '1d', '5d', '1wk', '1mo', '3mo')
            
        Returns:
            Dictionary mapping currency pair codes to DataFrames with OHLCV data
        """
        if currency_pairs is None:
            # Default to major pairs for initial implementation
            currency_pairs = list(self.major_pairs.keys())
            
        results = {}
        
        print(f"\n🌍 === FOREX DATA COLLECTION STARTED ===")
        print(f"📊 Collecting data for {len(currency_pairs)} currency pairs")
        print(f"⏰ Period: {period}, Interval: {interval}")
        print(f"📅 Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        for pair_code in currency_pairs:
            try:
                if pair_code not in self.forex_symbols:
                    self.logger.warning(f"⚠️ Unknown currency pair: {pair_code}")
                    continue
                    
                pair_info = self.forex_symbols[pair_code]
                yahoo_symbol = pair_info.yahoo_symbol
                
                print(f"\n💱 Collecting {pair_info.name} ({yahoo_symbol})")
                print(f"   Category: {pair_info.category.title()}")
                print(f"   Description: {pair_info.description}")
                
                # Fetch data using yfinance
                ticker = yf.Ticker(yahoo_symbol)
                data = ticker.history(period=period, interval=interval)
                
                if data.empty:
                    self.logger.warning(f"⚠️ No data returned for {pair_code}")
                    continue
                
                # Add metadata columns
                data['Symbol'] = yahoo_symbol
                data['PairCode'] = pair_code
                data['BaseCurrency'] = pair_info.base_currency
                data['QuoteCurrency'] = pair_info.quote_currency
                data['Category'] = pair_info.category
                data['IsCommodityLinked'] = pair_info.is_commodity_linked
                data['IsSafeHaven'] = pair_info.is_safe_haven
                
                # Save to file
                self._save_forex_data(data, pair_code, 'yahoo_finance')
                
                results[pair_code] = data
                
                print(f"   ✅ Success: {len(data)} records collected")
                print(f"   📈 Latest Price: {data['Close'].iloc[-1]:.5f}")
                print(f"   📅 Date Range: {data.index[0].strftime('%Y-%m-%d')} to {data.index[-1].strftime('%Y-%m-%d')}")
                
            except Exception as e:
                self.logger.error(f"❌ Error collecting {pair_code}: {str(e)}")
                continue
        
        print(f"\n🎉 === FOREX DATA COLLECTION COMPLETED ===")
        print(f"✅ Successfully collected: {len(results)}/{len(currency_pairs)} currency pairs")
        
        return results
    
    def _save_forex_data(self, data: pd.DataFrame, pair_code: str, provider: str):
        """Save forex data to organized directory structure"""
        # Create filename with timestamp
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f"{pair_code}_{timestamp}.csv"
        
        # Save to provider-specific directory
        file_path = os.path.join(self.base_data_dir, provider, 'forex', pair_code, filename)
        data.to_csv(file_path)
        
        # Also save as latest.csv for easy access
        latest_path = os.path.join(self.base_data_dir, provider, 'forex', pair_code, 'latest.csv')
        data.to_csv(latest_path)
        
        self.logger.info(f"💾 Saved {pair_code} data: {file_path}")
    
    def get_currency_correlation_matrix(self, currency_pairs: Optional[List[str]] = None) -> pd.DataFrame:
        """
        Calculate correlation matrix between currency pairs.
        
        Args:
            currency_pairs: List of currency pair codes
            
        Returns:
            Correlation matrix DataFrame
        """
        if currency_pairs is None:
            currency_pairs = list(self.major_pairs.keys())
            
        # Collect recent data for correlation analysis
        forex_data = self.collect_yahoo_forex_data(currency_pairs, period='3mo', interval='1d')
        
        # Create correlation matrix
        price_data = {}
        for pair_code, data in forex_data.items():
            if not data.empty:
                price_data[pair_code] = data['Close']
        
        if price_data:
            correlation_df = pd.DataFrame(price_data).corr()
            
            print(f"\n📊 Currency Pair Correlation Matrix")
            print(f"📅 Based on 3-month daily data")
            print(correlation_df.round(3))
            
            return correlation_df
        
        return pd.DataFrame()
    
    def collect_priority_pairs(self) -> Dict[str, pd.DataFrame]:
        """
        Collect data for currency pairs in implementation priority order.
        
        Returns:
            Dictionary of collected data for priority pairs
        """
        print(f"\n🎯 Collecting Priority Currency Pairs")
        print(f"📋 Priority Order: {' → '.join(IMPLEMENTATION_PRIORITY[:7])}")
        
        # Start with Phase 1: Major pairs (first 7 in priority)
        phase1_pairs = IMPLEMENTATION_PRIORITY[:7]
        
        return self.collect_yahoo_forex_data(phase1_pairs, period='1mo', interval='1h')
    
    def validate_forex_data_quality(self, data: pd.DataFrame, pair_code: str) -> Dict[str, any]:
        """
        Validate quality of collected forex data.
        
        Args:
            data: Forex data DataFrame
            pair_code: Currency pair code
            
        Returns:
            Dictionary with validation results
        """
        validation = {
            'pair_code': pair_code,
            'total_records': len(data),
            'date_range_days': (data.index[-1] - data.index[0]).days if len(data) > 0 else 0,
            'missing_values': data.isnull().sum().sum(),
            'zero_volume_records': (data['Volume'] == 0).sum() if 'Volume' in data.columns else 0,
            'price_consistency': True,
            'data_quality_score': 0.0
        }
        
        if len(data) > 0:
            # Check price consistency (Close should be within OHLC range)
            price_consistency = (
                (data['Close'] >= data['Low']) & 
                (data['Close'] <= data['High']) &
                (data['Open'] >= data['Low']) & 
                (data['Open'] <= data['High'])
            ).all()
            
            validation['price_consistency'] = price_consistency
            
            # Calculate data quality score (0-100)
            completeness_score = (1 - validation['missing_values'] / (len(data) * len(data.columns))) * 50
            consistency_score = 50 if price_consistency else 0
            validation['data_quality_score'] = completeness_score + consistency_score
        
        return validation


def main():
    """
    Main function for standalone forex data collection.
    """
    print("🌍 Multi-Currency Forex Data Collector")
    print("=" * 50)
    
    # Initialize collector
    collector = ForexDataCollector()
    
    # Collect priority currency pairs
    forex_data = collector.collect_priority_pairs()
    
    # Display summary
    if forex_data:
        print(f"\n📊 COLLECTION SUMMARY:")
        for pair_code, data in forex_data.items():
            validation = collector.validate_forex_data_quality(data, pair_code)
            print(f"  {pair_code}: {validation['total_records']} records, "
                  f"Quality: {validation['data_quality_score']:.1f}%")
        
        # Calculate correlation matrix
        correlation_matrix = collector.get_currency_correlation_matrix()
        
        print(f"\n🎉 Forex data collection completed successfully!")
        print(f"📁 Data saved in: {collector.base_data_dir}")
    else:
        print("❌ No forex data collected")


if __name__ == "__main__":
    main()
