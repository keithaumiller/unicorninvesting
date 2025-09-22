"""
Abstract base class for asset adapters.

This interface defines the standard contract that all asset adapters
must implement to normalize different asset classes.

ENHANCED TO SUPPORT: Legacy crypto functionality including:
- Market session patterns (24/7 trading with time zones)
- Crypto-specific volatility characteristics
- Volume-based indicators for crypto markets
- Asset-specific configurations and base prices
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional, Union
import pandas as pd
from .data_interfaces import RawAssetData, StandardizedData, MarketCharacteristics, ConstrainedForecast, ForecastResult

class AssetAdapter(ABC):
    """
    Abstract adapter for different asset classes.
    
    Asset adapters handle asset-specific data normalization, market characteristics,
    and trading constraints while providing a unified interface to methodologies.
    """
    
    def __init__(self, asset_class: str, symbol: str):
        """
        Initialize the asset adapter.
        
        Args:
            asset_class: Asset class name (e.g., 'crypto', 'forex', 'equities')
            symbol: Specific asset symbol (e.g., 'ETH', 'EURUSD', 'AAPL')
        """
        self.asset_class = asset_class
        self.symbol = symbol
        self.market_characteristics = self.get_market_characteristics()
        self.normalization_params = {}
    
    @abstractmethod
    def normalize_time_series(self, raw_data: RawAssetData) -> StandardizedData:
        """
        Convert asset-specific data to standardized format.
        
        Args:
            raw_data: Raw asset data from data sources
            
        Returns:
            StandardizedData: Normalized data in standard format
        """
        pass
    
    @abstractmethod
    def get_market_characteristics(self) -> MarketCharacteristics:
        """
        Return asset-specific market behavior patterns.
        
        Returns:
            MarketCharacteristics: Market behavior and trading characteristics
        """
        pass
    
    @abstractmethod
    def apply_asset_constraints(self, forecast: ForecastResult) -> ConstrainedForecast:
        """
        Apply asset-specific trading constraints to forecast.
        
        Args:
            forecast: Raw forecast result
            
        Returns:
            ConstrainedForecast: Forecast with constraints applied
        """
        pass
    
    @abstractmethod
    def get_feature_engineering_params(self, methodology: str) -> Dict[str, Any]:
        """
        Get methodology-specific feature engineering parameters for this asset.
        
        Args:
            methodology: Name of the methodology
            
        Returns:
            Dictionary of feature engineering parameters
        """
        pass
    
    @abstractmethod
    def validate_data_quality(self, data: StandardizedData) -> Dict[str, Any]:
        """
        Validate data quality for this asset class.
        
        Args:
            data: Standardized data to validate
            
        Returns:
            Dictionary with validation results and quality metrics
        """
        pass
    
    def get_supported_timeframes(self) -> List[str]:
        """
        Get supported timeframes for this asset.
        
        Returns:
            List of supported timeframe strings
        """
        # Default timeframes
        return ['1min', '5min', '15min', '1hour', '4hour', '1day']
    
    def get_price_precision(self) -> int:
        """
        Get price precision (decimal places) for this asset.
        
        Returns:
            Number of decimal places for price precision
        """
        # Default: 5 decimal places
        return 5
    
    def get_volume_precision(self) -> int:
        """
        Get volume precision (decimal places) for this asset.
        
        Returns:
            Number of decimal places for volume precision
        """
        # Default: 8 decimal places
        return 8
    
    def calculate_returns(self, data: StandardizedData, 
                         return_type: str = 'simple') -> StandardizedData:
        """
        Calculate returns for the asset.
        
        Args:
            data: Standardized price data
            return_type: Type of returns ('simple', 'log')
            
        Returns:
            StandardizedData with returns calculated
        """
        # Default implementation for simple returns
        import pandas as pd
        
        data_copy = data
        if return_type == 'simple':
            data_copy.close_price = data.close_price.pct_change()
        elif return_type == 'log':
            data_copy.close_price = pd.np.log(data.close_price / data.close_price.shift(1))
        
        return data_copy
    
    def detect_outliers(self, data: StandardizedData, 
                       method: str = 'iqr') -> Dict[str, Any]:
        """
        Detect outliers in asset data.
        
        Args:
            data: Standardized data to analyze
            method: Outlier detection method ('iqr', 'zscore', 'isolation')
            
        Returns:
            Dictionary with outlier detection results
        """
        # Default IQR-based outlier detection
        import pandas as pd
        
        Q1 = data.close_price.quantile(0.25)
        Q3 = data.close_price.quantile(0.75)
        IQR = Q3 - Q1
        
        lower_bound = Q1 - 1.5 * IQR
        upper_bound = Q3 + 1.5 * IQR
        
        outliers = (data.close_price < lower_bound) | (data.close_price > upper_bound)
        
        return {
            'outlier_count': outliers.sum(),
            'outlier_percentage': (outliers.sum() / len(data.close_price)) * 100,
            'outlier_indices': data.close_price[outliers].index.tolist(),
            'method': method,
            'bounds': {'lower': lower_bound, 'upper': upper_bound}
        }
    
    def apply_filters(self, data: StandardizedData, 
                     filters: List[str]) -> StandardizedData:
        """
        Apply asset-specific data filters.
        
        Args:
            data: Input standardized data
            filters: List of filter names to apply
            
        Returns:
            Filtered standardized data
        """
        # Default: no filtering
        return data
    
    # ENHANCED METHODS TO SUPPORT LEGACY CRYPTO FUNCTIONALITY
    
    def add_market_sessions(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Add market session indicators for asset class.
        
        SUPPORTS: Legacy crypto market session patterns from enhanced_crypto_prophet_builder.py
        
        Args:
            data: OHLCV data with datetime index
            
        Returns:
            DataFrame with market session indicators
        """
        # Default implementation - adapters should override for specific markets
        return data
    
    def add_asset_specific_features(self, data: pd.DataFrame, asset: str) -> pd.DataFrame:
        """
        Add asset-specific features beyond basic OHLCV.
        
        SUPPORTS: Legacy crypto-specific feature engineering
        
        Args:
            data: OHLCV data
            asset: Specific asset symbol
            
        Returns:
            DataFrame with asset-specific features
        """
        # Default implementation - adapters should override
        return data
    
    def generate_sample_data(self, asset: str, timeframe: str = '1H', 
                           periods: int = 1000) -> pd.DataFrame:
        """
        Generate realistic sample data for testing.
        
        SUPPORTS: Legacy sample data generation from enhanced_crypto_prophet_builder.py
        
        Args:
            asset: Asset symbol
            timeframe: Data frequency
            periods: Number of data points
            
        Returns:
            Realistic sample OHLCV data
        """
        # Default implementation - adapters should override for realistic simulation
        import numpy as np
        from datetime import datetime, timedelta
        
        dates = pd.date_range(start=datetime.now() - timedelta(hours=periods), 
                             periods=periods, freq='H')
        
        # Generic price simulation
        base_price = 100
        returns = np.random.normal(0, 0.02, periods)
        prices = [base_price]
        for i in range(1, periods):
            prices.append(prices[-1] * (1 + returns[i]))
        
        return pd.DataFrame({
            'Open': prices,
            'High': [p * 1.01 for p in prices],
            'Low': [p * 0.99 for p in prices],
            'Close': prices,
            'Volume': np.random.lognormal(10, 1, periods)
        }, index=dates)
    
    def get_asset_config(self, asset: str) -> Dict[str, Any]:
        """
        Get asset-specific configuration parameters.
        
        SUPPORTS: Legacy asset configuration from enhanced_crypto_prophet_builder.py
        
        Args:
            asset: Asset symbol
            
        Returns:
            Asset-specific configuration parameters
        """
        # Default implementation - adapters should override
        return {
            'market_type': self.asset_class,
            'trading_hours': 'varies',
            'base_price': 100,
            'volatility_regime': 'medium'
        }
    
    def validate_data_quality(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Validate data quality specific to asset class.
        
        SUPPORTS: Legacy data quality validation
        
        Args:
            data: Asset data to validate
            
        Returns:
            Data quality assessment
        """
        # Default implementation
        return {
            'total_rows': len(data),
            'missing_values': data.isnull().sum().to_dict(),
            'quality_score': 1.0 - (data.isnull().sum().sum() / (len(data) * len(data.columns))),
            'data_issues': []
        }

    def __str__(self) -> str:
        """String representation of asset adapter"""
        return f"{self.asset_class}Adapter({self.symbol})"
    
    def __repr__(self) -> str:
        """Detailed representation of asset adapter"""
        return f"AssetAdapter(class='{self.asset_class}', symbol='{self.symbol}')"
    
    def __str__(self) -> str:
        """String representation of asset adapter"""
        return f"{self.asset_class}_{self.symbol}_adapter"
    
    def __repr__(self) -> str:
        """Detailed representation of asset adapter"""
        return f"AssetAdapter(asset_class='{self.asset_class}', symbol='{self.symbol}')"