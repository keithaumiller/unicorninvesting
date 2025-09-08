#!/usr/bin/env python3
"""
Federal Reserve Economic Data (FRED) Connector

Utilizes the fredapi library to collect economic indicators from the Federal Reserve 
Bank of St. Louis for alpha model enhancement in crypto trading strategies.

Dependencies:
    pip install fredapi pandas numpy

Key Economic Series for Crypto Alpha Models:
- Federal Funds Rate (FEDFUNDS)
- Consumer Price Index (CPIAUCSL) 
- Treasury Yields (DGS10, DGS2)
- Money Supply (M2SL)
- VIX (VIXCLS)
- USD Exchange Rates
"""

import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Union
from dataclasses import dataclass
import os
import json
import time

try:
    from fredapi import Fred
except ImportError:
    raise ImportError("fredapi library required. Install with: pip install fredapi")


@dataclass
class EconomicSeries:
    """Configuration for economic data series."""
    series_id: str
    name: str
    description: str
    category: str
    priority: int  # 1=Critical, 2=Important, 3=Supplementary
    frequency: str
    units: str


class FredConnector:
    """
    Federal Reserve Economic Data (FRED) Connector using fredapi library.
    
    Provides access to critical economic indicators for crypto alpha model enhancement.
    The fredapi library handles rate limiting and API communication automatically.
    
    Usage:
        fred = FredConnector(api_key='your_fred_api_key')
        data = fred.get_critical_indicators()
        features = fred.create_alpha_features(data)
    """
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize FRED connector with fredapi library.
        
        Args:
            api_key: FRED API key. If None, looks for FRED_API_KEY environment variable.
        """
        self.api_key = api_key or os.getenv('FRED_API_KEY')
        
        if not self.api_key:
            raise ValueError(
                "FRED API key required. Either pass api_key parameter or set FRED_API_KEY environment variable.\n"
                "Get free API key at: https://fred.stlouisfed.org/docs/api/api_key.html"
            )
        
        # Initialize fredapi client
        self.fred = Fred(api_key=self.api_key)
        
        # Setup logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
        
        # Data storage directory
        self.data_dir = os.path.join(os.path.dirname(__file__), 'data')
        os.makedirs(self.data_dir, exist_ok=True)
        
        # Define critical economic series for crypto trading
        self.economic_series = self._define_economic_series()
        
        self.logger.info("FRED Connector initialized successfully")
    
    def _define_economic_series(self) -> Dict[str, EconomicSeries]:
        """Define economic data series critical for crypto alpha models."""
        return {
            # Monetary Policy (Priority 1 - Critical)
            'FEDFUNDS': EconomicSeries(
                series_id='FEDFUNDS',
                name='Federal Funds Rate',
                description='Effective Federal Funds Rate',
                category='monetary_policy',
                priority=1,
                frequency='Monthly',
                units='Percent'
            ),
            'DFF': EconomicSeries(
                series_id='DFF',
                name='Daily Federal Funds Rate',
                description='Effective Federal Funds Rate (Daily)',
                category='monetary_policy',
                priority=1,
                frequency='Daily',
                units='Percent'
            ),
            'M2SL': EconomicSeries(
                series_id='M2SL',
                name='M2 Money Supply',
                description='M2 Money Stock',
                category='monetary_policy',
                priority=1,
                frequency='Monthly',
                units='Billions of Dollars'
            ),
            
            # Inflation (Priority 1 - Critical)
            'CPIAUCSL': EconomicSeries(
                series_id='CPIAUCSL',
                name='Consumer Price Index',
                description='Consumer Price Index for All Urban Consumers: All Items',
                category='inflation',
                priority=1,
                frequency='Monthly',
                units='Index 1982-1984=100'
            ),
            'CPILFESL': EconomicSeries(
                series_id='CPILFESL',
                name='Core CPI',
                description='Consumer Price Index: All Items Less Food & Energy',
                category='inflation',
                priority=1,
                frequency='Monthly',
                units='Index 1982-1984=100'
            ),
            
            # Treasury Yields (Priority 1 - Critical)
            'DGS10': EconomicSeries(
                series_id='DGS10',
                name='10-Year Treasury',
                description='10-Year Treasury Constant Maturity Rate',
                category='interest_rates',
                priority=1,
                frequency='Daily',
                units='Percent'
            ),
            'DGS2': EconomicSeries(
                series_id='DGS2',
                name='2-Year Treasury',
                description='2-Year Treasury Constant Maturity Rate',
                category='interest_rates',
                priority=1,
                frequency='Daily',
                units='Percent'
            ),
            'DGS5': EconomicSeries(
                series_id='DGS5',
                name='5-Year Treasury',
                description='5-Year Treasury Constant Maturity Rate',
                category='interest_rates',
                priority=1,
                frequency='Daily',
                units='Percent'
            ),
            
            # Employment (Priority 2 - Important)
            'UNRATE': EconomicSeries(
                series_id='UNRATE',
                name='Unemployment Rate',
                description='Unemployment Rate',
                category='employment',
                priority=2,
                frequency='Monthly',
                units='Percent'
            ),
            'PAYEMS': EconomicSeries(
                series_id='PAYEMS',
                name='Nonfarm Payrolls',
                description='All Employees, Nonfarm Payrolls',
                category='employment',
                priority=2,
                frequency='Monthly',
                units='Thousands of Persons'
            ),
            
            # Economic Growth (Priority 2 - Important)
            'GDP': EconomicSeries(
                series_id='GDP',
                name='Gross Domestic Product',
                description='Gross Domestic Product',
                category='growth',
                priority=2,
                frequency='Quarterly',
                units='Billions of Dollars'
            ),
            'GDPC1': EconomicSeries(
                series_id='GDPC1',
                name='Real GDP',
                description='Real Gross Domestic Product',
                category='growth',
                priority=2,
                frequency='Quarterly',
                units='Billions of Chained 2017 Dollars'
            ),
            
            # Market Indicators (Priority 2 - Important)
            'VIXCLS': EconomicSeries(
                series_id='VIXCLS',
                name='VIX',
                description='CBOE Volatility Index: VIX',
                category='market_indicators',
                priority=2,
                frequency='Daily',
                units='Index'
            ),
            'NFCI': EconomicSeries(
                series_id='NFCI',
                name='Financial Conditions Index',
                description='Chicago Fed National Financial Conditions Index',
                category='market_indicators',
                priority=2,
                frequency='Weekly',
                units='Index'
            ),
            
            # Currency (Priority 2 - Important)
            'DEXUSEU': EconomicSeries(
                series_id='DEXUSEU',
                name='USD/EUR Exchange Rate',
                description='U.S. / Euro Foreign Exchange Rate',
                category='currency',
                priority=2,
                frequency='Daily',
                units='U.S. Dollars to One Euro'
            ),
            'TWEXBMTH': EconomicSeries(
                series_id='TWEXBMTH',
                name='Trade Weighted USD Index',
                description='Trade Weighted U.S. Dollar Index: Broad, Goods and Services',
                category='currency',
                priority=2,
                frequency='Monthly',
                units='Index Jan 1997=100'
            )
        }
    
    def get_series_data(self, series_id: str, start_date: Optional[str] = None,
                       end_date: Optional[str] = None) -> pd.Series:
        """
        Get data for a specific FRED series using fredapi.
        
        Args:
            series_id: FRED series identifier
            start_date: Start date in YYYY-MM-DD format
            end_date: End date in YYYY-MM-DD format
            
        Returns:
            Pandas Series with date index and values
        """
        try:
            self.logger.info(f"Fetching data for series: {series_id}")
            
            # Convert date strings to datetime objects if provided
            start_dt = pd.to_datetime(start_date) if start_date else None
            end_dt = pd.to_datetime(end_date) if end_date else None
            
            # Use fredapi to get data (handles rate limiting automatically)
            data = self.fred.get_series(series_id, start=start_dt, end=end_dt)
            
            if data.empty:
                self.logger.warning(f"No data returned for series {series_id}")
                return pd.Series(dtype=float)
            
            # Clean data - remove NaN values
            data = data.dropna()
            
            self.logger.info(f"Retrieved {len(data)} observations for {series_id} from {data.index.min()} to {data.index.max()}")
            return data
            
        except Exception as e:
            self.logger.error(f"Error fetching data for {series_id}: {str(e)}")
            return pd.Series(dtype=float)
    
    def get_multiple_series(self, series_list: List[str], start_date: Optional[str] = None,
                           end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get data for multiple FRED series and combine into DataFrame.
        
        Args:
            series_list: List of FRED series identifiers
            start_date: Start date in YYYY-MM-DD format
            end_date: End date in YYYY-MM-DD format
            
        Returns:
            DataFrame with date index and columns for each series
        """
        self.logger.info(f"Fetching data for {len(series_list)} series...")
        
        data_dict = {}
        for series_id in series_list:
            series_data = self.get_series_data(series_id, start_date, end_date)
            if not series_data.empty:
                data_dict[series_id] = series_data
            
            # Small delay to be respectful to API
            time.sleep(0.1)
        
        if not data_dict:
            self.logger.warning("No data retrieved for any series")
            return pd.DataFrame()
        
        # Combine all series into DataFrame
        combined_df = pd.DataFrame(data_dict)
        
        self.logger.info(f"Combined data shape: {combined_df.shape}")
        return combined_df
    
    def get_critical_indicators(self, start_date: Optional[str] = None,
                              end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get all Priority 1 (critical) economic indicators for crypto alpha models.
        
        Args:
            start_date: Start date in YYYY-MM-DD format (default: 5 years ago)
            end_date: End date in YYYY-MM-DD format (default: today)
            
        Returns:
            DataFrame with critical economic indicators
        """
        # Default to 5 years of data
        if not start_date:
            start_date = (datetime.now() - timedelta(days=5*365)).strftime('%Y-%m-%d')
        if not end_date:
            end_date = datetime.now().strftime('%Y-%m-%d')
        
        # Get Priority 1 (critical) series
        critical_series = [
            series_id for series_id, config in self.economic_series.items()
            if config.priority == 1
        ]
        
        self.logger.info(f"Collecting {len(critical_series)} critical economic indicators...")
        self.logger.info(f"Date range: {start_date} to {end_date}")
        
        data = self.get_multiple_series(critical_series, start_date, end_date)
        
        if not data.empty:
            # Save raw data
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            output_file = os.path.join(self.data_dir, f'fred_critical_indicators_{timestamp}.csv')
            data.to_csv(output_file)
            self.logger.info(f"Critical indicators saved to: {output_file}")
        
        return data
    
    def get_series_by_priority(self, priority: int, start_date: Optional[str] = None,
                              end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get all series of a specific priority level.
        
        Args:
            priority: Priority level (1=Critical, 2=Important, 3=Supplementary)
            start_date: Start date in YYYY-MM-DD format
            end_date: End date in YYYY-MM-DD format
            
        Returns:
            DataFrame with series of specified priority
        """
        series_list = [
            series_id for series_id, config in self.economic_series.items()
            if config.priority == priority
        ]
        
        self.logger.info(f"Collecting Priority {priority} economic indicators ({len(series_list)} series)...")
        return self.get_multiple_series(series_list, start_date, end_date)
    
    def get_series_info(self, series_id: str) -> Dict:
        """
        Get metadata information about a FRED series.
        
        Args:
            series_id: FRED series identifier
            
        Returns:
            Dictionary with series metadata
        """
        try:
            info = self.fred.get_series_info(series_id)
            return info.to_dict() if hasattr(info, 'to_dict') else dict(info)
        except Exception as e:
            self.logger.error(f"Error getting series info for {series_id}: {str(e)}")
            return {}
    
    def create_alpha_features(self, data: pd.DataFrame, 
                            lookback_windows: Optional[List[int]] = None) -> pd.DataFrame:
        """
        Create alpha model features from economic data.
        
        Args:
            data: DataFrame with economic time series
            lookback_windows: Lookback periods for feature engineering
            
        Returns:
            DataFrame with engineered features for alpha models
        """
        if lookback_windows is None:
            # Trading days: 5d, 10d, 20d, 60d, 252d (1w, 2w, 1m, 3m, 1y)
            lookback_windows = [5, 10, 20, 60, 252]
        
        self.logger.info("Creating alpha model features...")
        features_df = data.copy()
        
        # Generate time-based features for each series
        for column in data.columns:
            series = data[column].dropna()
            
            if len(series) < max(lookback_windows):
                self.logger.warning(f"Insufficient data for {column} feature engineering")
                continue
            
            # Rate of change features
            for window in lookback_windows:
                if window <= len(series):
                    features_df[f'{column}_roc_{window}'] = series.pct_change(window)
                    features_df[f'{column}_ma_{window}'] = series.rolling(window).mean()
                    features_df[f'{column}_std_{window}'] = series.rolling(window).std()
                    features_df[f'{column}_zscore_{window}'] = (
                        (series - series.rolling(window).mean()) / series.rolling(window).std()
                    )
        
        # Cross-series features (economic regime indicators)
        if 'DGS10' in data.columns and 'DGS2' in data.columns:
            features_df['yield_curve_slope'] = data['DGS10'] - data['DGS2']
            features_df['yield_curve_inversion'] = (data['DGS2'] > data['DGS10']).astype(int)
        
        if 'DGS10' in data.columns and 'FEDFUNDS' in data.columns:
            features_df['term_spread'] = data['DGS10'] - data['FEDFUNDS']
        
        if 'CPIAUCSL' in data.columns:
            features_df['inflation_yoy'] = data['CPIAUCSL'].pct_change(12) * 100
            features_df['inflation_mom'] = data['CPIAUCSL'].pct_change(1) * 100
        
        # Monetary policy stance indicators
        if 'FEDFUNDS' in data.columns:
            fed_funds = data['FEDFUNDS']
            features_df['fed_funds_trend'] = fed_funds.rolling(60).mean() - fed_funds.rolling(252).mean()
            features_df['monetary_tightening'] = (fed_funds.diff() > 0).rolling(60).sum()
        
        # Market stress indicators
        if 'VIXCLS' in data.columns:
            vix = data['VIXCLS']
            features_df['vix_spike'] = (vix > vix.rolling(252).quantile(0.8)).astype(int)
            features_df['market_stress'] = (vix > 30).astype(int)
        
        self.logger.info(f"Created {len(features_df.columns)} features from {len(data.columns)} original series")
        return features_df
    
    def save_data_for_alpha_models(self, output_dir: Optional[str] = None,
                                  lookback_years: int = 5) -> Dict[str, str]:
        """
        Collect and save economic data formatted for alpha model consumption.
        
        Args:
            output_dir: Directory to save processed data (default: ../processed/economic_indicators)
            lookback_years: Years of historical data to collect
            
        Returns:
            Dictionary with file paths of saved data
        """
        if output_dir is None:
            output_dir = os.path.join(
                os.path.dirname(__file__),
                '../../../processed/economic_indicators'
            )
        
        os.makedirs(output_dir, exist_ok=True)
        
        # Calculate date range
        end_date = datetime.now().strftime('%Y-%m-%d')
        start_date = (datetime.now() - timedelta(days=lookback_years*365)).strftime('%Y-%m-%d')
        
        self.logger.info(f"Collecting economic data for alpha models ({start_date} to {end_date})...")
        
        # Get critical indicators
        critical_data = self.get_critical_indicators(start_date, end_date)
        
        if critical_data.empty:
            self.logger.error("No critical data retrieved")
            return {}
        
        # Create alpha features
        features_data = self.create_alpha_features(critical_data)
        
        # Generate timestamps and file paths
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        files_saved = {}
        
        # Save raw economic data
        raw_file = os.path.join(output_dir, f'fred_raw_data_{timestamp}.csv')
        critical_data.to_csv(raw_file)
        files_saved['raw_data'] = raw_file
        
        # Save engineered features
        features_file = os.path.join(output_dir, f'fred_features_{timestamp}.csv')
        features_data.to_csv(features_file)
        files_saved['features'] = features_file
        
        # Save latest data (for automated processes)
        latest_raw = os.path.join(output_dir, 'fred_raw_latest.csv')
        latest_features = os.path.join(output_dir, 'fred_features_latest.csv')
        critical_data.to_csv(latest_raw)
        features_data.to_csv(latest_features)
        files_saved['latest_raw'] = latest_raw
        files_saved['latest_features'] = latest_features
        
        # Create metadata file
        metadata = {
            'collection_timestamp': timestamp,
            'date_range': {'start': start_date, 'end': end_date},
            'series_count': len(critical_data.columns),
            'features_count': len(features_data.columns),
            'observations': len(critical_data),
            'series_info': {
                sid: {
                    'name': config.name,
                    'category': config.category,
                    'priority': config.priority,
                    'frequency': config.frequency,
                    'description': config.description
                }
                for sid, config in self.economic_series.items()
                if config.priority == 1
            }
        }
        
        metadata_file = os.path.join(output_dir, f'fred_metadata_{timestamp}.json')
        with open(metadata_file, 'w') as f:
            json.dump(metadata, f, indent=2)
        files_saved['metadata'] = metadata_file
        
        self.logger.info("Economic data collection complete!")
        self.logger.info(f"Raw data: {len(critical_data)} observations, {len(critical_data.columns)} series")
        self.logger.info(f"Features: {len(features_data.columns)} engineered features")
        self.logger.info(f"Files saved: {list(files_saved.keys())}")
        
        return files_saved
    
    def get_latest_values(self) -> pd.Series:
        """Get the latest values for all critical economic indicators."""
        critical_data = self.get_critical_indicators(
            start_date=(datetime.now() - timedelta(days=30)).strftime('%Y-%m-%d')
        )
        
        if critical_data.empty:
            return pd.Series(dtype=float)
        
        return critical_data.iloc[-1]


def main():
    """Example usage and testing of FredConnector."""
    try:
        # Initialize connector
        print("🏦 Initializing FRED Connector...")
        fred = FredConnector()
        
        # Test single series
        print("\n📊 Testing single series (Fed Funds Rate)...")
        fed_funds = fred.get_series_data('FEDFUNDS', start_date='2020-01-01')
        if not fed_funds.empty:
            print(f"   Retrieved {len(fed_funds)} observations")
            print(f"   Latest Fed Funds Rate: {fed_funds.iloc[-1]:.2f}%")
        
        # Get critical indicators (last 2 years for testing)
        print("\n📈 Collecting critical economic indicators...")
        start_date = (datetime.now() - timedelta(days=2*365)).strftime('%Y-%m-%d')
        critical_data = fred.get_critical_indicators(start_date=start_date)
        
        if not critical_data.empty:
            print(f"   Retrieved {len(critical_data.columns)} series")
            print(f"   Date range: {critical_data.index.min()} to {critical_data.index.max()}")
            print(f"   Total observations: {len(critical_data)}")
            
            # Show latest values
            print("\n💹 Latest Economic Indicators:")
            latest = critical_data.iloc[-1]
            for series_id, value in latest.items():
                if not pd.isna(value):
                    series_info = fred.economic_series.get(series_id, None)
                    name = series_info.name if series_info else series_id
                    print(f"   {name}: {value:.2f}")
        
        # Create and save alpha model features
        print("\n🔧 Creating alpha model features...")
        files_saved = fred.save_data_for_alpha_models()
        
        if files_saved:
            print("\n💾 Files saved for alpha model integration:")
            for file_type, file_path in files_saved.items():
                print(f"   {file_type}: {os.path.basename(file_path)}")
        
        print("\n✅ FRED Connector test completed successfully!")
        
    except ValueError as e:
        print(f"❌ Configuration Error: {e}")
        print("\n💡 Setup Instructions:")
        print("1. Get free FRED API key: https://fred.stlouisfed.org/docs/api/api_key.html")
        print("2. Set environment variable: export FRED_API_KEY='your_key_here'")
        print("3. Or pass API key to FredConnector(api_key='your_key_here')")
        
    except ImportError as e:
        print(f"❌ Library Error: {e}")
        print("💡 Install required library: pip install fredapi pandas")
        
    except Exception as e:
        print(f"❌ Unexpected Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
