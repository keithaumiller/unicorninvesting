"""
Data normalizer for the alpha models framework.

Provides cross-asset standardization and normalization of time series data.
"""

from typing import Dict, Any, List, Optional, Union, Tuple
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler, MinMaxScaler, RobustScaler
from datetime import datetime, timedelta
import logging
from abc import ABC, abstractmethod

from ...core.interfaces.data_interfaces import AssetData

logger = logging.getLogger(__name__)

class BaseNormalizer(ABC):
    """Abstract base class for data normalizers"""
    
    @abstractmethod
    def fit(self, data: pd.DataFrame) -> 'BaseNormalizer':
        """Fit normalizer to data"""
        pass
    
    @abstractmethod
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data using fitted normalizer"""
        pass
    
    @abstractmethod
    def inverse_transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Inverse transform normalized data"""
        pass
    
    def fit_transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Fit and transform data in one step"""
        return self.fit(data).transform(data)

class ReturnsNormalizer(BaseNormalizer):
    """Normalizes price data using returns"""
    
    def __init__(self, method: str = 'simple', lookback_periods: int = 252):
        """
        Initialize returns normalizer.
        
        Args:
            method: 'simple' or 'log' returns
            lookback_periods: Number of periods for rolling statistics
        """
        self.method = method
        self.lookback_periods = lookback_periods
        self.is_fitted = False
        self.reference_prices = {}
        
    def fit(self, data: pd.DataFrame) -> 'ReturnsNormalizer':
        """Fit normalizer by storing reference prices"""
        price_columns = self._get_price_columns(data)
        
        # Store last available price for each column as reference
        for col in price_columns:
            if col in data.columns:
                last_valid_idx = data[col].last_valid_index()
                if last_valid_idx is not None:
                    self.reference_prices[col] = data.loc[last_valid_idx, col]
        
        self.is_fitted = True
        logger.debug(f"ReturnsNormalizer fitted with reference prices: {self.reference_prices}")
        return self
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform prices to returns"""
        if not self.is_fitted:
            raise ValueError("Normalizer must be fitted before transform")
        
        transformed_data = data.copy()
        price_columns = self._get_price_columns(data)
        
        for col in price_columns:
            if col in data.columns:
                if self.method == 'simple':
                    transformed_data[f'{col}_returns'] = data[col].pct_change()
                elif self.method == 'log':
                    transformed_data[f'{col}_returns'] = np.log(data[col] / data[col].shift(1))
                
                # Drop original price column
                transformed_data = transformed_data.drop(columns=[col])
        
        return transformed_data
    
    def inverse_transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Convert returns back to prices"""
        if not self.is_fitted:
            raise ValueError("Normalizer must be fitted before inverse_transform")
        
        reconstructed_data = data.copy()
        
        for original_col, reference_price in self.reference_prices.items():
            returns_col = f'{original_col}_returns'
            
            if returns_col in data.columns:
                if self.method == 'simple':
                    # Reconstruct prices from simple returns
                    prices = [reference_price]
                    for ret in data[returns_col].fillna(0):
                        prices.append(prices[-1] * (1 + ret))
                    reconstructed_data[original_col] = prices[1:]  # Remove initial reference
                    
                elif self.method == 'log':
                    # Reconstruct prices from log returns
                    log_prices = np.log(reference_price) + data[returns_col].fillna(0).cumsum()
                    reconstructed_data[original_col] = np.exp(log_prices)
                
                # Drop returns column
                reconstructed_data = reconstructed_data.drop(columns=[returns_col])
        
        return reconstructed_data
    
    def _get_price_columns(self, data: pd.DataFrame) -> List[str]:
        """Identify price columns in dataframe"""
        price_columns = []
        for col in data.columns:
            if col.lower() in ['open', 'high', 'low', 'close', 'price']:
                price_columns.append(col)
        return price_columns

class StatisticalNormalizer(BaseNormalizer):
    """Statistical normalizer using sklearn scalers"""
    
    def __init__(self, method: str = 'standard', **scaler_kwargs):
        """
        Initialize statistical normalizer.
        
        Args:
            method: 'standard', 'minmax', or 'robust'
            **scaler_kwargs: Additional arguments for the scaler
        """
        self.method = method
        self.scaler_kwargs = scaler_kwargs
        self.scalers = {}
        self.feature_columns = []
        self.is_fitted = False
        
        self._scaler_map = {
            'standard': StandardScaler,
            'minmax': MinMaxScaler,
            'robust': RobustScaler
        }
        
        if method not in self._scaler_map:
            raise ValueError(f"Unknown method: {method}. Choose from {list(self._scaler_map.keys())}")
    
    def fit(self, data: pd.DataFrame) -> 'StatisticalNormalizer':
        """Fit scalers to data"""
        numeric_columns = data.select_dtypes(include=[np.number]).columns.tolist()
        
        # Exclude timestamp-like columns
        self.feature_columns = [col for col in numeric_columns 
                               if not any(keyword in col.lower() 
                                        for keyword in ['timestamp', 'time', 'date'])]
        
        scaler_class = self._scaler_map[self.method]
        
        # Fit individual scalers for each column
        for col in self.feature_columns:
            if col in data.columns:
                scaler = scaler_class(**self.scaler_kwargs)
                non_null_data = data[col].dropna()
                
                if len(non_null_data) > 0:
                    scaler.fit(non_null_data.values.reshape(-1, 1))
                    self.scalers[col] = scaler
        
        self.is_fitted = True
        logger.debug(f"StatisticalNormalizer fitted for {len(self.scalers)} columns")
        return self
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data using fitted scalers"""
        if not self.is_fitted:
            raise ValueError("Normalizer must be fitted before transform")
        
        transformed_data = data.copy()
        
        for col in self.feature_columns:
            if col in data.columns and col in self.scalers:
                # Transform non-null values
                mask = ~data[col].isna()
                if mask.sum() > 0:
                    transformed_values = self.scalers[col].transform(
                        data.loc[mask, col].values.reshape(-1, 1)
                    ).flatten()
                    transformed_data.loc[mask, col] = transformed_values
        
        return transformed_data
    
    def inverse_transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Inverse transform normalized data"""
        if not self.is_fitted:
            raise ValueError("Normalizer must be fitted before inverse_transform")
        
        original_data = data.copy()
        
        for col in self.feature_columns:
            if col in data.columns and col in self.scalers:
                # Inverse transform non-null values
                mask = ~data[col].isna()
                if mask.sum() > 0:
                    original_values = self.scalers[col].inverse_transform(
                        data.loc[mask, col].values.reshape(-1, 1)
                    ).flatten()
                    original_data.loc[mask, col] = original_values
        
        return original_data

class CrossAssetNormalizer(BaseNormalizer):
    """Cross-asset normalizer for comparable scaling across different assets"""
    
    def __init__(self, reference_asset: str = 'BTC', normalization_window: int = 252):
        """
        Initialize cross-asset normalizer.
        
        Args:
            reference_asset: Asset to use as reference for cross-asset scaling
            normalization_window: Window for rolling normalization
        """
        self.reference_asset = reference_asset
        self.normalization_window = normalization_window
        self.reference_stats = {}
        self.is_fitted = False
    
    def fit(self, data: pd.DataFrame) -> 'CrossAssetNormalizer':
        """Fit normalizer using reference asset statistics"""
        # Calculate rolling statistics for reference
        numeric_columns = data.select_dtypes(include=[np.number]).columns.tolist()
        
        for col in numeric_columns:
            if col in data.columns:
                rolling_mean = data[col].rolling(window=self.normalization_window).mean()
                rolling_std = data[col].rolling(window=self.normalization_window).std()
                
                # Store final statistics as reference
                self.reference_stats[col] = {
                    'mean': rolling_mean.iloc[-1] if not rolling_mean.empty else data[col].mean(),
                    'std': rolling_std.iloc[-1] if not rolling_std.empty else data[col].std()
                }
        
        self.is_fitted = True
        logger.debug(f"CrossAssetNormalizer fitted with reference stats: {self.reference_stats}")
        return self
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data using cross-asset normalization"""
        if not self.is_fitted:
            raise ValueError("Normalizer must be fitted before transform")
        
        transformed_data = data.copy()
        
        for col in self.reference_stats.keys():
            if col in data.columns:
                ref_mean = self.reference_stats[col]['mean']
                ref_std = self.reference_stats[col]['std']
                
                if ref_std > 0:
                    transformed_data[col] = (data[col] - ref_mean) / ref_std
        
        return transformed_data
    
    def inverse_transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Inverse transform using reference statistics"""
        if not self.is_fitted:
            raise ValueError("Normalizer must be fitted before inverse_transform")
        
        original_data = data.copy()
        
        for col in self.reference_stats.keys():
            if col in data.columns:
                ref_mean = self.reference_stats[col]['mean']
                ref_std = self.reference_stats[col]['std']
                
                original_data[col] = (data[col] * ref_std) + ref_mean
        
        return original_data

class DataNormalizer:
    """
    Main data normalizer for the alpha models framework.
    
    Provides unified interface for cross-asset data normalization.
    """
    
    def __init__(self, normalization_config: Optional[Dict[str, Any]] = None):
        """
        Initialize data normalizer.
        
        Args:
            normalization_config: Configuration for normalization methods
        """
        self.config = normalization_config or self._get_default_config()
        self.normalizers = {}
        self.fitted_assets = set()
        
        logger.info("DataNormalizer initialized")
    
    def _get_default_config(self) -> Dict[str, Any]:
        """Get default normalization configuration"""
        return {
            'price_normalization': {
                'method': 'returns',
                'returns_type': 'simple',
                'lookback_periods': 252
            },
            'feature_normalization': {
                'method': 'robust',
                'quantile_range': (25.0, 75.0)
            },
            'cross_asset_normalization': {
                'enabled': True,
                'reference_asset': 'BTC',
                'normalization_window': 252
            },
            'handle_outliers': {
                'enabled': True,
                'method': 'clip',
                'lower_quantile': 0.01,
                'upper_quantile': 0.99
            }
        }
    
    def normalize_asset_data(self, asset_data: AssetData, 
                           asset_key: Optional[str] = None) -> AssetData:
        """
        Normalize asset data using configured methods.
        
        Args:
            asset_data: Asset data to normalize
            asset_key: Unique key for this asset (for caching normalizers)
            
        Returns:
            Normalized AssetData
        """
        if asset_key is None:
            asset_key = f"{asset_data.asset_class}_{asset_data.symbol}_{asset_data.timeframe}"
        
        normalized_data = asset_data.data.copy()
        normalized_features = asset_data.features.copy() if asset_data.features is not None else None
        
        # Step 1: Price normalization
        if self.config['price_normalization']['method'] == 'returns':
            price_normalizer = self._get_or_create_normalizer(
                asset_key, 'price', ReturnsNormalizer,
                method=self.config['price_normalization']['returns_type'],
                lookback_periods=self.config['price_normalization']['lookback_periods']
            )
            
            if asset_key not in self.fitted_assets:
                price_normalizer.fit(normalized_data)
            
            normalized_data = price_normalizer.transform(normalized_data)
        
        # Step 2: Feature normalization
        if normalized_features is not None:
            feature_normalizer = self._get_or_create_normalizer(
                asset_key, 'features', StatisticalNormalizer,
                method=self.config['feature_normalization']['method'],
                **{k: v for k, v in self.config['feature_normalization'].items() if k != 'method'}
            )
            
            if asset_key not in self.fitted_assets:
                feature_normalizer.fit(normalized_features)
            
            normalized_features = feature_normalizer.transform(normalized_features)
        
        # Step 3: Cross-asset normalization (if enabled)
        if self.config['cross_asset_normalization']['enabled']:
            cross_asset_normalizer = self._get_or_create_normalizer(
                'cross_asset', 'global', CrossAssetNormalizer,
                reference_asset=self.config['cross_asset_normalization']['reference_asset'],
                normalization_window=self.config['cross_asset_normalization']['normalization_window']
            )
            
            # Note: Cross-asset normalizer needs to be fitted on reference asset first
            # This is handled separately by fit_cross_asset_normalizer method
        
        # Step 4: Handle outliers
        if self.config['handle_outliers']['enabled']:
            normalized_data = self._handle_outliers(normalized_data)
            if normalized_features is not None:
                normalized_features = self._handle_outliers(normalized_features)
        
        # Mark asset as fitted
        self.fitted_assets.add(asset_key)
        
        # Create normalized AssetData
        normalized_asset_data = AssetData(
            data=normalized_data,
            features=normalized_features,
            target=asset_data.target,  # Target typically not normalized
            timestamps=asset_data.timestamps,
            asset_class=asset_data.asset_class,
            symbol=asset_data.symbol,
            timeframe=asset_data.timeframe,
            metadata={
                **asset_data.metadata,
                'normalization_applied': True,
                'normalization_config': self.config,
                'normalization_timestamp': datetime.now().isoformat()
            }
        )
        
        logger.info(f"Normalized data for {asset_key}")
        return normalized_asset_data
    
    def _get_or_create_normalizer(self, asset_key: str, normalizer_type: str, 
                                 normalizer_class: type, **kwargs) -> BaseNormalizer:
        """Get existing normalizer or create new one"""
        normalizer_key = f"{asset_key}_{normalizer_type}"
        
        if normalizer_key not in self.normalizers:
            self.normalizers[normalizer_key] = normalizer_class(**kwargs)
        
        return self.normalizers[normalizer_key]
    
    def _handle_outliers(self, data: pd.DataFrame) -> pd.DataFrame:
        """Handle outliers in data"""
        method = self.config['handle_outliers']['method']
        lower_q = self.config['handle_outliers']['lower_quantile']
        upper_q = self.config['handle_outliers']['upper_quantile']
        
        processed_data = data.copy()
        numeric_columns = data.select_dtypes(include=[np.number]).columns
        
        for col in numeric_columns:
            if method == 'clip':
                # Clip outliers to quantile bounds
                lower_bound = data[col].quantile(lower_q)
                upper_bound = data[col].quantile(upper_q)
                processed_data[col] = data[col].clip(lower_bound, upper_bound)
            
            elif method == 'remove':
                # Mark outliers as NaN
                lower_bound = data[col].quantile(lower_q)
                upper_bound = data[col].quantile(upper_q)
                outlier_mask = (data[col] < lower_bound) | (data[col] > upper_bound)
                processed_data.loc[outlier_mask, col] = np.nan
        
        return processed_data
    
    def fit_cross_asset_normalizer(self, reference_data: AssetData):
        """
        Fit cross-asset normalizer using reference asset data.
        
        Args:
            reference_data: Reference asset data for cross-asset normalization
        """
        if not self.config['cross_asset_normalization']['enabled']:
            return
        
        cross_asset_normalizer = self._get_or_create_normalizer(
            'cross_asset', 'global', CrossAssetNormalizer,
            reference_asset=self.config['cross_asset_normalization']['reference_asset'],
            normalization_window=self.config['cross_asset_normalization']['normalization_window']
        )
        
        # Fit on combined data and features
        if reference_data.features is not None:
            combined_data = pd.concat([reference_data.data, reference_data.features], axis=1)
        else:
            combined_data = reference_data.data
        
        cross_asset_normalizer.fit(combined_data)
        logger.info(f"Cross-asset normalizer fitted on {self.config['cross_asset_normalization']['reference_asset']}")
    
    def inverse_normalize(self, normalized_data: AssetData, asset_key: str) -> AssetData:
        """
        Inverse normalize data back to original scale.
        
        Args:
            normalized_data: Normalized asset data
            asset_key: Asset key used during normalization
            
        Returns:
            AssetData with original scale
        """
        # Get normalizers for this asset
        price_normalizer_key = f"{asset_key}_price"
        feature_normalizer_key = f"{asset_key}_features"
        
        original_data = normalized_data.data.copy()
        original_features = normalized_data.features.copy() if normalized_data.features is not None else None
        
        # Inverse transform features
        if original_features is not None and feature_normalizer_key in self.normalizers:
            original_features = self.normalizers[feature_normalizer_key].inverse_transform(original_features)
        
        # Inverse transform prices
        if price_normalizer_key in self.normalizers:
            original_data = self.normalizers[price_normalizer_key].inverse_transform(original_data)
        
        return AssetData(
            data=original_data,
            features=original_features,
            target=normalized_data.target,
            timestamps=normalized_data.timestamps,
            asset_class=normalized_data.asset_class,
            symbol=normalized_data.symbol,
            timeframe=normalized_data.timeframe,
            metadata={
                **normalized_data.metadata,
                'normalization_applied': False,
                'inverse_transform_timestamp': datetime.now().isoformat()
            }
        )
    
    def get_normalization_stats(self) -> Dict[str, Any]:
        """
        Get normalization statistics.
        
        Returns:
            Dictionary with normalization statistics
        """
        stats = {
            'fitted_assets': list(self.fitted_assets),
            'normalizer_count': len(self.normalizers),
            'normalizer_types': {
                key: type(normalizer).__name__ 
                for key, normalizer in self.normalizers.items()
            },
            'config': self.config
        }
        
        return stats
    
    def reset_normalizers(self, asset_key: Optional[str] = None):
        """
        Reset normalizers.
        
        Args:
            asset_key: If provided, reset only normalizers for this asset
        """
        if asset_key:
            # Reset specific asset normalizers
            keys_to_remove = [key for key in self.normalizers.keys() if key.startswith(asset_key)]
            for key in keys_to_remove:
                del self.normalizers[key]
            
            self.fitted_assets.discard(asset_key)
            logger.info(f"Reset normalizers for {asset_key}")
        else:
            # Reset all normalizers
            self.normalizers.clear()
            self.fitted_assets.clear()
            logger.info("Reset all normalizers")
    
    def __str__(self) -> str:
        """String representation"""
        return f"DataNormalizer(fitted_assets={len(self.fitted_assets)})"
    
    def __repr__(self) -> str:
        """Detailed representation"""
        return (f"DataNormalizer(fitted_assets={len(self.fitted_assets)}, "
                f"normalizers={len(self.normalizers)})")