"""
Feature pipeline for the alpha models framework.

Provides unified feature engineering across all methodologies and assets.
"""

from typing import Dict, Any, List, Optional, Union, Tuple, Callable
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging
from abc import ABC, abstractmethod
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.feature_selection import SelectKBest, f_regression, mutual_info_regression
import talib
import warnings

from ...core.interfaces.data_interfaces import AssetData, FeatureSet

logger = logging.getLogger(__name__)

class BaseFeatureTransformer(ABC):
    """Abstract base class for feature transformers"""
    
    @abstractmethod
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data to create features"""
        pass
    
    @abstractmethod
    def get_feature_names(self) -> List[str]:
        """Get names of features created by this transformer"""
        pass

class PriceFeatureTransformer(BaseFeatureTransformer):
    """Price-based feature transformer"""
    
    def __init__(self, price_transforms: Optional[List[str]] = None):
        """
        Initialize price feature transformer.
        
        Args:
            price_transforms: List of price transformations to apply
        """
        self.price_transforms = price_transforms or ['returns', 'log_returns', 'volatility']
        self.feature_names = []
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform price data to create features"""
        features = pd.DataFrame(index=data.index)
        self.feature_names = []
        
        # Basic price features
        if 'returns' in self.price_transforms:
            features['returns'] = data['close'].pct_change()
            self.feature_names.append('returns')
        
        if 'log_returns' in self.price_transforms:
            features['log_returns'] = np.log(data['close'] / data['close'].shift(1))
            self.feature_names.append('log_returns')
        
        if 'volatility' in self.price_transforms:
            features['volatility'] = features['returns'].rolling(window=20).std()
            self.feature_names.append('volatility')
        
        # Price ratios
        if 'high_low_ratio' in self.price_transforms:
            features['high_low_ratio'] = data['high'] / data['low']
            self.feature_names.append('high_low_ratio')
        
        if 'open_close_ratio' in self.price_transforms:
            features['open_close_ratio'] = data['open'] / data['close']
            self.feature_names.append('open_close_ratio')
        
        # Price momentum
        if 'price_momentum' in self.price_transforms:
            for period in [1, 3, 5, 10]:
                mom_col = f'price_momentum_{period}'
                features[mom_col] = data['close'] / data['close'].shift(period) - 1
                self.feature_names.append(mom_col)
        
        return features
    
    def get_feature_names(self) -> List[str]:
        """Get feature names"""
        return self.feature_names

class TechnicalIndicatorTransformer(BaseFeatureTransformer):
    """Technical indicator feature transformer"""
    
    def __init__(self, indicators: Optional[List[str]] = None):
        """
        Initialize technical indicator transformer.
        
        Args:
            indicators: List of technical indicators to compute
        """
        self.indicators = indicators or ['sma', 'ema', 'rsi', 'macd', 'bollinger_bands']
        self.feature_names = []
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data to create technical indicators"""
        features = pd.DataFrame(index=data.index)
        self.feature_names = []
        
        # Suppress TA-Lib warnings
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            
            # Moving averages
            if 'sma' in self.indicators:
                for period in [5, 10, 20, 50]:
                    sma_col = f'sma_{period}'
                    features[sma_col] = talib.SMA(data['close'].values, timeperiod=period)
                    self.feature_names.append(sma_col)
            
            if 'ema' in self.indicators:
                for period in [5, 10, 20, 50]:
                    ema_col = f'ema_{period}'
                    features[ema_col] = talib.EMA(data['close'].values, timeperiod=period)
                    self.feature_names.append(ema_col)
            
            # RSI
            if 'rsi' in self.indicators:
                for period in [14, 21]:
                    rsi_col = f'rsi_{period}'
                    features[rsi_col] = talib.RSI(data['close'].values, timeperiod=period)
                    self.feature_names.append(rsi_col)
            
            # MACD
            if 'macd' in self.indicators:
                macd, macd_signal, macd_hist = talib.MACD(data['close'].values)
                features['macd'] = macd
                features['macd_signal'] = macd_signal
                features['macd_histogram'] = macd_hist
                self.feature_names.extend(['macd', 'macd_signal', 'macd_histogram'])
            
            # Bollinger Bands
            if 'bollinger_bands' in self.indicators:
                bb_upper, bb_middle, bb_lower = talib.BBANDS(data['close'].values)
                features['bb_upper'] = bb_upper
                features['bb_middle'] = bb_middle
                features['bb_lower'] = bb_lower
                features['bb_width'] = (bb_upper - bb_lower) / bb_middle
                features['bb_position'] = (data['close'] - bb_lower) / (bb_upper - bb_lower)
                self.feature_names.extend(['bb_upper', 'bb_middle', 'bb_lower', 'bb_width', 'bb_position'])
            
            # Stochastic Oscillator
            if 'stochastic' in self.indicators:
                slowk, slowd = talib.STOCH(data['high'].values, data['low'].values, data['close'].values)
                features['stoch_k'] = slowk
                features['stoch_d'] = slowd
                self.feature_names.extend(['stoch_k', 'stoch_d'])
            
            # Average True Range
            if 'atr' in self.indicators:
                for period in [14, 21]:
                    atr_col = f'atr_{period}'
                    features[atr_col] = talib.ATR(data['high'].values, data['low'].values, data['close'].values, timeperiod=period)
                    self.feature_names.append(atr_col)
            
            # Williams %R
            if 'williams_r' in self.indicators:
                features['williams_r'] = talib.WILLR(data['high'].values, data['low'].values, data['close'].values)
                self.feature_names.append('williams_r')
        
        return features
    
    def get_feature_names(self) -> List[str]:
        """Get feature names"""
        return self.feature_names

class LagFeatureTransformer(BaseFeatureTransformer):
    """Lag feature transformer"""
    
    def __init__(self, lag_periods: Optional[List[int]] = None, target_columns: Optional[List[str]] = None):
        """
        Initialize lag feature transformer.
        
        Args:
            lag_periods: List of lag periods to create
            target_columns: Columns to create lags for
        """
        self.lag_periods = lag_periods or [1, 2, 3, 5, 10]
        self.target_columns = target_columns or ['close', 'volume']
        self.feature_names = []
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data to create lag features"""
        features = pd.DataFrame(index=data.index)
        self.feature_names = []
        
        for column in self.target_columns:
            if column in data.columns:
                for lag in self.lag_periods:
                    lag_col = f'{column}_lag_{lag}'
                    features[lag_col] = data[column].shift(lag)
                    self.feature_names.append(lag_col)
        
        return features
    
    def get_feature_names(self) -> List[str]:
        """Get feature names"""
        return self.feature_names

class RollingFeatureTransformer(BaseFeatureTransformer):
    """Rolling window feature transformer"""
    
    def __init__(self, windows: Optional[List[int]] = None, functions: Optional[List[str]] = None):
        """
        Initialize rolling feature transformer.
        
        Args:
            windows: List of rolling window sizes
            functions: List of rolling functions to apply
        """
        self.windows = windows or [5, 10, 20, 50]
        self.functions = functions or ['mean', 'std', 'min', 'max']
        self.feature_names = []
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data to create rolling features"""
        features = pd.DataFrame(index=data.index)
        self.feature_names = []
        
        # Focus on close prices for rolling features
        if 'close' in data.columns:
            for window in self.windows:
                for func in self.functions:
                    feature_col = f'close_rolling_{func}_{window}'
                    
                    if func == 'mean':
                        features[feature_col] = data['close'].rolling(window=window).mean()
                    elif func == 'std':
                        features[feature_col] = data['close'].rolling(window=window).std()
                    elif func == 'min':
                        features[feature_col] = data['close'].rolling(window=window).min()
                    elif func == 'max':
                        features[feature_col] = data['close'].rolling(window=window).max()
                    
                    self.feature_names.append(feature_col)
        
        # Volume rolling features if available
        if 'volume' in data.columns and not data['volume'].isna().all():
            for window in [5, 10, 20]:
                vol_mean_col = f'volume_rolling_mean_{window}'
                features[vol_mean_col] = data['volume'].rolling(window=window).mean()
                self.feature_names.append(vol_mean_col)
        
        return features
    
    def get_feature_names(self) -> List[str]:
        """Get feature names"""
        return self.feature_names

class VolumeFeatureTransformer(BaseFeatureTransformer):
    """Volume-based feature transformer"""
    
    def __init__(self):
        """Initialize volume feature transformer"""
        self.feature_names = []
    
    def transform(self, data: pd.DataFrame) -> pd.DataFrame:
        """Transform data to create volume features"""
        features = pd.DataFrame(index=data.index)
        self.feature_names = []
        
        if 'volume' in data.columns and not data['volume'].isna().all():
            # Volume rate of change
            features['volume_roc'] = data['volume'].pct_change()
            self.feature_names.append('volume_roc')
            
            # Volume moving averages
            features['volume_sma_10'] = data['volume'].rolling(window=10).mean()
            features['volume_ratio'] = data['volume'] / features['volume_sma_10']
            self.feature_names.extend(['volume_sma_10', 'volume_ratio'])
            
            # Price-volume features
            if 'close' in data.columns:
                features['price_volume'] = data['close'] * data['volume']
                features['volume_weighted_price'] = (data['close'] * data['volume']).rolling(window=10).sum() / data['volume'].rolling(window=10).sum()
                self.feature_names.extend(['price_volume', 'volume_weighted_price'])
        
        return features
    
    def get_feature_names(self) -> List[str]:
        """Get feature names"""
        return self.feature_names

class FeaturePipeline:
    """
    Main feature engineering pipeline for alpha models.
    
    Provides unified feature engineering across all methodologies and assets.
    """
    
    def __init__(self, feature_config: Optional[Dict[str, Any]] = None):
        """
        Initialize feature pipeline.
        
        Args:
            feature_config: Configuration for feature engineering
        """
        self.config = feature_config or self._get_default_config()
        self.transformers = {}
        self.feature_selector = None
        self.fitted_features = None
        self.is_fitted = False
        
        self._initialize_transformers()
        logger.info("FeaturePipeline initialized")
    
    def _get_default_config(self) -> Dict[str, Any]:
        """Get default feature engineering configuration"""
        return {
            'price_features': {
                'enabled': True,
                'transforms': ['returns', 'log_returns', 'volatility', 'price_momentum']
            },
            'technical_indicators': {
                'enabled': True,
                'indicators': ['sma', 'ema', 'rsi', 'macd', 'bollinger_bands', 'stochastic']
            },
            'lag_features': {
                'enabled': True,
                'lag_periods': [1, 2, 3, 5, 10],
                'target_columns': ['close', 'volume']
            },
            'rolling_features': {
                'enabled': True,
                'windows': [5, 10, 20],
                'functions': ['mean', 'std', 'min', 'max']
            },
            'volume_features': {
                'enabled': True
            },
            'feature_selection': {
                'enabled': True,
                'method': 'mutual_info',
                'k_best': 50,
                'min_features': 10
            },
            'scaling': {
                'enabled': True,
                'method': 'standard'
            }
        }
    
    def _initialize_transformers(self):
        """Initialize feature transformers based on configuration"""
        
        if self.config['price_features']['enabled']:
            self.transformers['price'] = PriceFeatureTransformer(
                price_transforms=self.config['price_features']['transforms']
            )
        
        if self.config['technical_indicators']['enabled']:
            self.transformers['technical'] = TechnicalIndicatorTransformer(
                indicators=self.config['technical_indicators']['indicators']
            )
        
        if self.config['lag_features']['enabled']:
            self.transformers['lag'] = LagFeatureTransformer(
                lag_periods=self.config['lag_features']['lag_periods'],
                target_columns=self.config['lag_features']['target_columns']
            )
        
        if self.config['rolling_features']['enabled']:
            self.transformers['rolling'] = RollingFeatureTransformer(
                windows=self.config['rolling_features']['windows'],
                functions=self.config['rolling_features']['functions']
            )
        
        if self.config['volume_features']['enabled']:
            self.transformers['volume'] = VolumeFeatureTransformer()
    
    def create_features(self, asset_data: AssetData, methodology_name: Optional[str] = None) -> FeatureSet:
        """
        Create features from asset data.
        
        Args:
            asset_data: Input asset data
            methodology_name: Name of methodology (for methodology-specific features)
            
        Returns:
            FeatureSet with engineered features
        """
        logger.info(f"Creating features for {asset_data.asset_class}_{asset_data.symbol}")
        
        # Start with the raw data
        all_features = pd.DataFrame(index=asset_data.data.index)
        feature_metadata = {
            'transformers_used': [],
            'feature_counts': {},
            'methodology_specific': methodology_name is not None
        }
        
        # Apply each transformer
        for transformer_name, transformer in self.transformers.items():
            try:
                logger.debug(f"Applying {transformer_name} transformer")
                transformer_features = transformer.transform(asset_data.data)
                
                if not transformer_features.empty:
                    all_features = pd.concat([all_features, transformer_features], axis=1)
                    feature_metadata['transformers_used'].append(transformer_name)
                    feature_metadata['feature_counts'][transformer_name] = len(transformer.get_feature_names())
                    
            except Exception as e:
                logger.warning(f"Failed to apply {transformer_name} transformer: {e}")
                continue
        
        # Add methodology-specific features
        if methodology_name:
            methodology_features = self._create_methodology_specific_features(
                asset_data, methodology_name
            )
            if not methodology_features.empty:
                all_features = pd.concat([all_features, methodology_features], axis=1)
                feature_metadata['methodology_features'] = True
        
        # Handle missing values
        all_features = self._handle_missing_values(all_features)
        
        # Feature selection
        if self.config['feature_selection']['enabled'] and len(all_features.columns) > 0:
            all_features = self._apply_feature_selection(all_features, asset_data.target)
            feature_metadata['feature_selection_applied'] = True
        
        # Feature scaling
        if self.config['scaling']['enabled'] and len(all_features.columns) > 0:
            all_features = self._apply_scaling(all_features)
            feature_metadata['scaling_applied'] = True
        
        # Create target for supervised learning
        target = self._create_target(asset_data)
        
        # Create FeatureSet
        feature_set = FeatureSet(
            features=all_features,
            target=target,
            feature_names=list(all_features.columns),
            timestamps=asset_data.timestamps,
            asset_class=asset_data.asset_class,
            symbol=asset_data.symbol,
            methodology=methodology_name,
            metadata={
                **feature_metadata,
                'original_data_shape': asset_data.data.shape,
                'final_feature_shape': all_features.shape,
                'feature_creation_timestamp': datetime.now().isoformat(),
                'config_used': self.config
            }
        )
        
        logger.info(f"Created {len(all_features.columns)} features for {asset_data.asset_class}_{asset_data.symbol}")
        return feature_set
    
    def _create_methodology_specific_features(self, asset_data: AssetData, methodology_name: str) -> pd.DataFrame:
        """Create methodology-specific features"""
        methodology_features = pd.DataFrame(index=asset_data.data.index)
        
        if methodology_name.lower() == 'prophet':
            # Prophet-specific features
            methodology_features['trend'] = np.arange(len(asset_data.data))
            methodology_features['trend_squared'] = methodology_features['trend'] ** 2
            
            # Seasonality features
            methodology_features['hour_of_day'] = asset_data.timestamps.dt.hour
            methodology_features['day_of_week'] = asset_data.timestamps.dt.dayofweek
            methodology_features['month'] = asset_data.timestamps.dt.month
            
        elif methodology_name.lower() == 'xgboost':
            # XGBoost-specific features
            if 'close' in asset_data.data.columns:
                # More aggressive lag features for XGBoost
                for lag in range(1, 21):
                    methodology_features[f'close_lag_{lag}'] = asset_data.data['close'].shift(lag)
                
                # Target encoding features
                for window in [3, 7, 14]:
                    methodology_features[f'target_mean_{window}'] = asset_data.target.shift(1).rolling(window=window).mean()
        
        elif methodology_name.lower() == 'ensemble':
            # Ensemble-specific features
            methodology_features['regime_indicator'] = self._detect_regime(asset_data.data)
            methodology_features['volatility_regime'] = self._classify_volatility_regime(asset_data.data)
        
        return methodology_features
    
    def _detect_regime(self, data: pd.DataFrame) -> pd.Series:
        """Detect market regime (trend/sideways)"""
        if 'close' in data.columns:
            returns = data['close'].pct_change()
            volatility = returns.rolling(window=20).std()
            trend_strength = abs(returns.rolling(window=20).mean())
            
            # Simple regime classification
            regime = pd.Series(0, index=data.index)  # Default: sideways
            regime[trend_strength > volatility] = 1  # Trending
            return regime
        
        return pd.Series(0, index=data.index)
    
    def _classify_volatility_regime(self, data: pd.DataFrame) -> pd.Series:
        """Classify volatility regime (low/medium/high)"""
        if 'close' in data.columns:
            returns = data['close'].pct_change()
            volatility = returns.rolling(window=20).std()
            
            # Classify based on volatility quantiles
            low_vol_threshold = volatility.quantile(0.33)
            high_vol_threshold = volatility.quantile(0.67)
            
            regime = pd.Series(1, index=data.index)  # Default: medium volatility
            regime[volatility <= low_vol_threshold] = 0  # Low volatility
            regime[volatility >= high_vol_threshold] = 2  # High volatility
            return regime
        
        return pd.Series(1, index=data.index)
    
    def _handle_missing_values(self, features: pd.DataFrame) -> pd.DataFrame:
        """Handle missing values in features"""
        # Forward fill then backward fill
        features = features.fillna(method='ffill').fillna(method='bfill')
        
        # Drop columns with too many missing values (>50%)
        missing_pct = features.isnull().sum() / len(features)
        cols_to_drop = missing_pct[missing_pct > 0.5].index
        if len(cols_to_drop) > 0:
            logger.warning(f"Dropping {len(cols_to_drop)} columns with >50% missing values")
            features = features.drop(columns=cols_to_drop)
        
        # Fill remaining missing values with 0
        features = features.fillna(0)
        
        return features
    
    def _apply_feature_selection(self, features: pd.DataFrame, target: pd.Series) -> pd.DataFrame:
        """Apply feature selection"""
        if len(features.columns) <= self.config['feature_selection']['min_features']:
            return features
        
        try:
            # Align features and target
            aligned_features, aligned_target = features.align(target, join='inner', axis=0)
            
            # Remove rows where target is NaN
            valid_mask = ~aligned_target.isna()
            if valid_mask.sum() < len(aligned_target) * 0.5:
                logger.warning("Too many missing target values for feature selection")
                return features
            
            aligned_features = aligned_features[valid_mask]
            aligned_target = aligned_target[valid_mask]
            
            # Feature selection
            k_best = min(
                self.config['feature_selection']['k_best'],
                len(aligned_features.columns),
                max(self.config['feature_selection']['min_features'], len(aligned_features.columns) // 2)
            )
            
            if self.config['feature_selection']['method'] == 'mutual_info':
                selector = SelectKBest(score_func=mutual_info_regression, k=k_best)
            else:
                selector = SelectKBest(score_func=f_regression, k=k_best)
            
            # Fit selector on aligned data
            selector.fit(aligned_features.fillna(0), aligned_target)
            
            # Get selected features
            selected_mask = selector.get_support()
            selected_features = features.columns[selected_mask]
            
            logger.info(f"Selected {len(selected_features)} features out of {len(features.columns)}")
            return features[selected_features]
            
        except Exception as e:
            logger.warning(f"Feature selection failed: {e}, using all features")
            return features
    
    def _apply_scaling(self, features: pd.DataFrame) -> pd.DataFrame:
        """Apply feature scaling"""
        try:
            if self.config['scaling']['method'] == 'standard':
                scaler = StandardScaler()
            elif self.config['scaling']['method'] == 'minmax':
                scaler = MinMaxScaler()
            else:
                return features
            
            # Fit and transform
            scaled_features = scaler.fit_transform(features.fillna(0))
            
            return pd.DataFrame(
                scaled_features,
                index=features.index,
                columns=features.columns
            )
            
        except Exception as e:
            logger.warning(f"Feature scaling failed: {e}, using unscaled features")
            return features
    
    def _create_target(self, asset_data: AssetData) -> pd.Series:
        """Create target variable for supervised learning"""
        if asset_data.target is not None:
            return asset_data.target
        
        # Default: next period return
        if 'close' in asset_data.data.columns:
            return asset_data.data['close'].pct_change().shift(-1)
        
        return pd.Series(np.nan, index=asset_data.data.index)
    
    def get_feature_importance_for_methodology(self, methodology_name: str) -> Dict[str, float]:
        """
        Get feature importance scores for a specific methodology.
        
        Args:
            methodology_name: Name of the methodology
            
        Returns:
            Dictionary of feature importance scores
        """
        # This would be implemented based on the specific methodology
        # For now, return placeholder
        return {}
    
    def update_config(self, new_config: Dict[str, Any]):
        """
        Update feature pipeline configuration.
        
        Args:
            new_config: New configuration settings
        """
        self.config.update(new_config)
        self._initialize_transformers()
        self.is_fitted = False
        logger.info("Feature pipeline configuration updated")
    
    def get_feature_stats(self) -> Dict[str, Any]:
        """
        Get statistics about feature engineering.
        
        Returns:
            Dictionary with feature statistics
        """
        stats = {
            'transformers_available': list(self.transformers.keys()),
            'config': self.config,
            'is_fitted': self.is_fitted
        }
        
        if self.fitted_features is not None:
            stats['fitted_feature_count'] = len(self.fitted_features)
            stats['fitted_features'] = self.fitted_features
        
        return stats
    
    def __str__(self) -> str:
        """String representation"""
        return f"FeaturePipeline(transformers={len(self.transformers)})"
    
    def __repr__(self) -> str:
        """Detailed representation"""
        return (f"FeaturePipeline(transformers={list(self.transformers.keys())}, "
                f"config_keys={list(self.config.keys())})")