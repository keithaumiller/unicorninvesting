"""
Unified data loader for the alpha models framework.

Provides consistent data loading from the silver layer across all assets.
"""

from typing import Dict, Any, List, Optional, Union, Tuple
from pathlib import Path
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging
import glob
import json

from ...core.interfaces.data_interfaces import RawAssetData, AssetData

logger = logging.getLogger(__name__)

class DataLoader:
    """
    Unified data loader for all asset classes.
    
    Loads data from the silver layer with consistent formatting
    and provides caching for performance optimization.
    """
    
    def __init__(self, silver_layer_path: Optional[Union[str, Path]] = None,
                 cache_enabled: bool = True, cache_ttl_hours: int = 24):
        """
        Initialize data loader.
        
        Args:
            silver_layer_path: Path to silver layer data
            cache_enabled: Whether to enable data caching
            cache_ttl_hours: Cache time-to-live in hours
        """
        if silver_layer_path is None:
            # Default to silver layer path
            self.silver_layer_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/3_silver")
        else:
            self.silver_layer_path = Path(silver_layer_path)
        
        self.cache_enabled = cache_enabled
        self.cache_ttl = timedelta(hours=cache_ttl_hours)
        self._data_cache = {}
        self._cache_timestamps = {}
        
        logger.info(f"DataLoader initialized: {self.silver_layer_path}")
    
    def load_asset_data(self, asset_class: str, symbol: str, timeframe: str = '1hour',
                       start_date: Optional[datetime] = None, 
                       end_date: Optional[datetime] = None,
                       min_periods: int = 100) -> AssetData:
        """
        Load asset data from silver layer.
        
        Args:
            asset_class: Asset class (crypto, forex, equities)
            symbol: Asset symbol
            timeframe: Data timeframe
            start_date: Start date for data
            end_date: End date for data
            min_periods: Minimum required data points
            
        Returns:
            AssetData instance
        """
        cache_key = f"{asset_class}_{symbol}_{timeframe}_{start_date}_{end_date}"
        
        # Check cache first
        if self.cache_enabled and self._is_cache_valid(cache_key):
            logger.debug(f"Loading from cache: {cache_key}")
            return self._data_cache[cache_key]
        
        # Load data from silver layer
        raw_data = self._load_raw_data(asset_class, symbol, timeframe, start_date, end_date)
        
        if raw_data is None:
            raise ValueError(f"No data found for {asset_class}_{symbol}_{timeframe}")
        
        # Convert to AssetData format
        asset_data = self._convert_to_asset_data(raw_data, asset_class, symbol, timeframe)
        
        # Validate minimum periods
        if len(asset_data.data) < min_periods:
            logger.warning(f"Insufficient data: {len(asset_data.data)} < {min_periods} for {asset_class}_{symbol}")
        
        # Cache the result
        if self.cache_enabled:
            self._data_cache[cache_key] = asset_data
            self._cache_timestamps[cache_key] = datetime.now()
        
        logger.info(f"Loaded {len(asset_data.data)} records for {asset_class}_{symbol}_{timeframe}")
        return asset_data
    
    def _load_raw_data(self, asset_class: str, symbol: str, timeframe: str,
                      start_date: Optional[datetime], end_date: Optional[datetime]) -> Optional[RawAssetData]:
        """Load raw data from silver layer"""
        
        # Construct data path based on asset class
        if asset_class.lower() == 'crypto':
            data_path = (self.silver_layer_path / 
                        "yahoo_finance_assets" / "processed_data" / "crypto" / 
                        f"{symbol.upper()}-USD_{timeframe}.csv")
        elif asset_class.lower() == 'forex':
            data_path = (self.silver_layer_path / 
                        "yahoo_finance_assets" / "processed_data" / "forex" / 
                        f"{symbol.upper()}=X_{timeframe}.csv")
        elif asset_class.lower() == 'equities':
            data_path = (self.silver_layer_path / 
                        "yahoo_finance_assets" / "processed_data" / "equities" / 
                        f"{symbol.upper()}_{timeframe}.csv")
        else:
            logger.error(f"Unsupported asset class: {asset_class}")
            return None
        
        # Try to load data
        if not data_path.exists():
            logger.warning(f"Data file not found: {data_path}")
            
            # Try alternative paths
            alternative_paths = self._find_alternative_paths(asset_class, symbol, timeframe)
            for alt_path in alternative_paths:
                if alt_path.exists():
                    data_path = alt_path
                    logger.info(f"Using alternative path: {data_path}")
                    break
            else:
                return None
        
        try:
            # Load CSV data
            df = pd.read_csv(data_path)
            
            # Standardize column names
            df = self._standardize_columns(df)
            
            # Filter by date range if provided
            if start_date or end_date:
                df = self._filter_by_date_range(df, start_date, end_date)
            
            # Convert to RawAssetData
            raw_data = RawAssetData(
                timestamp=pd.to_datetime(df['timestamp']),
                open=df['open'],
                high=df['high'],
                low=df['low'],
                close=df['close'],
                volume=df['volume'] if 'volume' in df.columns else pd.Series([0] * len(df)),
                asset_class=asset_class,
                symbol=symbol,
                source='silver_layer',
                metadata={
                    'file_path': str(data_path),
                    'timeframe': timeframe,
                    'loaded_at': datetime.now().isoformat(),
                    'original_shape': df.shape
                }
            )
            
            return raw_data
            
        except Exception as e:
            logger.error(f"Failed to load data from {data_path}: {e}")
            return None
    
    def _find_alternative_paths(self, asset_class: str, symbol: str, timeframe: str) -> List[Path]:
        """Find alternative data file paths"""
        alternatives = []
        
        base_path = self.silver_layer_path / "yahoo_finance_assets" / "processed_data"
        
        # Different possible file naming patterns
        patterns = [
            f"*{symbol}*{timeframe}*.csv",
            f"*{symbol.upper()}*{timeframe}*.csv",
            f"*{symbol.lower()}*{timeframe}*.csv",
            f"*{symbol}*.csv"
        ]
        
        # Search in asset class directory and subdirectories
        search_dirs = [
            base_path / asset_class.lower(),
            base_path / asset_class.upper(),
            base_path,
        ]
        
        for search_dir in search_dirs:
            if search_dir.exists():
                for pattern in patterns:
                    matches = glob.glob(str(search_dir / "**" / pattern), recursive=True)
                    alternatives.extend([Path(match) for match in matches])
        
        return alternatives
    
    def _standardize_columns(self, df: pd.DataFrame) -> pd.DataFrame:
        """Standardize column names"""
        column_mapping = {
            'Date': 'timestamp',
            'date': 'timestamp',
            'Datetime': 'timestamp',
            'datetime': 'timestamp',
            'Time': 'timestamp',
            'time': 'timestamp',
            'Open': 'open',
            'High': 'high',
            'Low': 'low',
            'Close': 'close',
            'Volume': 'volume',
            'Adj Close': 'adj_close',
            'Adj_Close': 'adj_close'
        }
        
        # Rename columns
        df = df.rename(columns=column_mapping)
        
        # Ensure required columns exist
        required_columns = ['timestamp', 'open', 'high', 'low', 'close']
        for col in required_columns:
            if col not in df.columns:
                if col == 'timestamp' and 'index' in df.columns:
                    df['timestamp'] = df['index']
                else:
                    logger.warning(f"Missing required column: {col}")
        
        return df
    
    def _filter_by_date_range(self, df: pd.DataFrame, start_date: Optional[datetime],
                             end_date: Optional[datetime]) -> pd.DataFrame:
        """Filter dataframe by date range"""
        if 'timestamp' not in df.columns:
            return df
        
        df['timestamp'] = pd.to_datetime(df['timestamp'])
        
        if start_date:
            df = df[df['timestamp'] >= start_date]
        
        if end_date:
            df = df[df['timestamp'] <= end_date]
        
        return df
    
    def _convert_to_asset_data(self, raw_data: RawAssetData, asset_class: str, 
                              symbol: str, timeframe: str) -> AssetData:
        """Convert raw data to AssetData format"""
        
        # Create main dataframe
        data = pd.DataFrame({
            'timestamp': raw_data.timestamp,
            'open': raw_data.open,
            'high': raw_data.high,
            'low': raw_data.low,
            'close': raw_data.close,
            'volume': raw_data.volume
        })
        
        # Sort by timestamp
        data = data.sort_values('timestamp').reset_index(drop=True)
        
        # Basic feature engineering
        features = self._create_basic_features(data)
        
        # Create target (next period return)
        target = data['close'].pct_change().shift(-1)
        
        return AssetData(
            data=data,
            features=features,
            target=target,
            timestamps=data['timestamp'],
            asset_class=asset_class,
            symbol=symbol,
            timeframe=timeframe,
            metadata={
                'source': raw_data.source,
                'original_metadata': raw_data.metadata,
                'feature_count': len(features.columns) if features is not None else 0,
                'data_quality': self._assess_data_quality(data)
            }
        )
    
    def _create_basic_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """Create basic technical features"""
        features = pd.DataFrame(index=data.index)
        
        # Price features
        features['returns'] = data['close'].pct_change()
        features['log_returns'] = np.log(data['close'] / data['close'].shift(1))
        features['high_low_ratio'] = data['high'] / data['low']
        features['open_close_ratio'] = data['open'] / data['close']
        
        # Moving averages
        for period in [5, 10, 20]:
            features[f'sma_{period}'] = data['close'].rolling(window=period).mean()
            features[f'ema_{period}'] = data['close'].ewm(span=period).mean()
        
        # Volatility
        features['volatility_5'] = features['returns'].rolling(window=5).std()
        features['volatility_20'] = features['returns'].rolling(window=20).std()
        
        # Volume features (if available)
        if 'volume' in data.columns and not data['volume'].isna().all():
            features['volume_sma_10'] = data['volume'].rolling(window=10).mean()
            features['volume_ratio'] = data['volume'] / features['volume_sma_10']
        
        return features
    
    def _assess_data_quality(self, data: pd.DataFrame) -> Dict[str, Any]:
        """Assess data quality metrics"""
        quality_metrics = {
            'total_records': len(data),
            'missing_values': data.isnull().sum().to_dict(),
            'duplicate_timestamps': data['timestamp'].duplicated().sum(),
            'data_gaps': self._count_data_gaps(data),
            'price_anomalies': self._detect_price_anomalies(data),
            'completeness_pct': ((len(data) - data.isnull().sum().max()) / len(data)) * 100 if len(data) > 0 else 0
        }
        
        return quality_metrics
    
    def _count_data_gaps(self, data: pd.DataFrame) -> int:
        """Count gaps in time series data"""
        if len(data) < 2:
            return 0
        
        # Calculate expected frequency based on first few intervals
        time_diffs = data['timestamp'].diff().dropna()
        if len(time_diffs) == 0:
            return 0
        
        expected_freq = time_diffs.mode().iloc[0] if len(time_diffs.mode()) > 0 else time_diffs.median()
        
        # Count gaps larger than 1.5x expected frequency
        large_gaps = (time_diffs > expected_freq * 1.5).sum()
        
        return large_gaps
    
    def _detect_price_anomalies(self, data: pd.DataFrame) -> Dict[str, int]:
        """Detect price anomalies"""
        anomalies = {}
        
        # Zero or negative prices
        anomalies['zero_prices'] = (data['close'] <= 0).sum()
        anomalies['negative_prices'] = (data['close'] < 0).sum()
        
        # Extreme price jumps (>50% in one period)
        returns = data['close'].pct_change().abs()
        anomalies['extreme_jumps'] = (returns > 0.5).sum()
        
        # OHLC inconsistencies
        anomalies['ohlc_errors'] = (
            (data['high'] < data['low']) |
            (data['high'] < data['open']) |
            (data['high'] < data['close']) |
            (data['low'] > data['open']) |
            (data['low'] > data['close'])
        ).sum()
        
        return anomalies
    
    def _is_cache_valid(self, cache_key: str) -> bool:
        """Check if cached data is still valid"""
        if cache_key not in self._cache_timestamps:
            return False
        
        cache_age = datetime.now() - self._cache_timestamps[cache_key]
        return cache_age < self.cache_ttl
    
    def get_available_assets(self) -> Dict[str, List[str]]:
        """
        Get list of available assets from silver layer.
        
        Returns:
            Dictionary mapping asset classes to available symbols
        """
        available_assets = {
            'crypto': [],
            'forex': [],
            'equities': []
        }
        
        base_path = self.silver_layer_path / "yahoo_finance_assets" / "processed_data"
        
        # Search for crypto assets
        crypto_path = base_path / "crypto"
        if crypto_path.exists():
            for file_path in crypto_path.glob("*_1hour.csv"):
                symbol = file_path.stem.replace("-USD_1hour", "").replace("_1hour", "")
                if symbol not in available_assets['crypto']:
                    available_assets['crypto'].append(symbol)
        
        # Search for forex assets
        forex_path = base_path / "forex"
        if forex_path.exists():
            for file_path in forex_path.glob("*_1hour.csv"):
                symbol = file_path.stem.replace("=X_1hour", "").replace("_1hour", "")
                if symbol not in available_assets['forex']:
                    available_assets['forex'].append(symbol)
        
        # Search for equity assets
        equity_path = base_path / "equities"
        if equity_path.exists():
            for file_path in equity_path.glob("*_1hour.csv"):
                symbol = file_path.stem.replace("_1hour", "")
                if symbol not in available_assets['equities']:
                    available_assets['equities'].append(symbol)
        
        # Remove empty lists
        available_assets = {k: v for k, v in available_assets.items() if v}
        
        logger.info(f"Available assets: {available_assets}")
        return available_assets
    
    def clear_cache(self):
        """Clear data cache"""
        self._data_cache.clear()
        self._cache_timestamps.clear()
        logger.info("Data cache cleared")
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """
        Get cache statistics.
        
        Returns:
            Cache statistics
        """
        return {
            'cached_datasets': len(self._data_cache),
            'cache_keys': list(self._data_cache.keys()),
            'cache_enabled': self.cache_enabled,
            'cache_ttl_hours': self.cache_ttl.total_seconds() / 3600,
            'memory_usage_mb': sum(
                asset_data.data.memory_usage(deep=True).sum() 
                for asset_data in self._data_cache.values()
            ) / (1024 * 1024)
        }
    
    def __str__(self) -> str:
        """String representation of data loader"""
        return f"DataLoader({self.silver_layer_path})"
    
    def __repr__(self) -> str:
        """Detailed representation of data loader"""
        stats = self.get_cache_stats()
        return (f"DataLoader(silver_layer_path='{self.silver_layer_path}', "
                f"cache_enabled={self.cache_enabled}, "
                f"cached_datasets={stats['cached_datasets']})")