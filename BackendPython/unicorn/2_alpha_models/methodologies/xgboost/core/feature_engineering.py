"""
XGBoost Feature Engineering

Comprehensive feature engineering specifically optimized for crypto XGBoost models.
Includes technical indicators, lag features, and crypto-specific patterns.

Features:
- Crypto-specific technical indicators (RSI, MACD, Bollinger Bands)
- Volume-based features for crypto markets
- Lag features for time series prediction
- Volatility and momentum indicators
- Market session and time-based features
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional, Union
import logging

logger = logging.getLogger(__name__)

class XGBoostFeatureEngine:
    """
    Comprehensive feature engineering for XGBoost crypto models
    
    This class creates features optimized for cryptocurrency time series prediction
    using XGBoost methodology. Features are designed to capture crypto market
    dynamics, volatility patterns, and technical analysis signals.
    """
    
    def __init__(self, asset: str = "ETH"):
        """
        Initialize feature engine
        
        Args:
            asset: Asset symbol (ETH, BTC, etc.)
        """
        self.asset = asset
        self.feature_categories = {
            'price': [],
            'volume': [],
            'technical': [],
            'volatility': [],
            'momentum': [],
            'time': [],
            'lag': []
        }
    
    def create_comprehensive_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Create comprehensive feature set for XGBoost
        
        Args:
            data: OHLCV DataFrame with datetime index
            
        Returns:
            DataFrame with engineered features
        """
        try:
            logger.info(f"Creating comprehensive features for {self.asset}")
            
            df = data.copy()
            initial_features = len(df.columns)
            
            # Core price features
            df = self._add_price_features(df)
            
            # Volume features
            df = self._add_volume_features(df)
            
            # Technical indicators
            df = self._add_technical_indicators(df)
            
            # Volatility features
            df = self._add_volatility_features(df)
            
            # Momentum features
            df = self._add_momentum_features(df)
            
            # Time-based features
            df = self._add_time_features(df)
            
            # Lag features
            df = self._add_lag_features(df)
            
            # Statistical features
            df = self._add_statistical_features(df)
            
            # Clean and validate
            df = self._clean_and_validate_features(df)
            
            final_features = len(df.columns)
            added_features = final_features - initial_features
            
            logger.info(f"Added {added_features} features. Total: {final_features}")
            self._log_feature_summary()
            
            return df
            
        except Exception as e:
            logger.error(f"Error creating features: {e}")
            return data
    
    def _add_price_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add price-based features"""
        try:
            # Basic price changes
            df['price_change'] = df['close'].pct_change()
            df['price_change_abs'] = df['price_change'].abs()
            df['log_return'] = np.log(df['close'] / df['close'].shift(1))
            
            # OHLC relationships
            df['high_low_spread'] = (df['high'] - df['low']) / df['close']
            df['open_close_spread'] = (df['close'] - df['open']) / df['open']
            df['high_close_ratio'] = df['high'] / df['close']
            df['low_close_ratio'] = df['low'] / df['close']
            
            # Price position within daily range
            df['close_position'] = (df['close'] - df['low']) / (df['high'] - df['low'])
            
            # Gap analysis
            df['gap'] = (df['open'] - df['close'].shift(1)) / df['close'].shift(1)
            df['gap_abs'] = df['gap'].abs()
            
            self.feature_categories['price'].extend([
                'price_change', 'price_change_abs', 'log_return', 'high_low_spread',
                'open_close_spread', 'high_close_ratio', 'low_close_ratio',
                'close_position', 'gap', 'gap_abs'
            ])
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding price features: {e}")
            return df
    
    def _add_volume_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add volume-based features optimized for crypto"""
        try:
            # Basic volume features
            df['volume_change'] = df['volume'].pct_change()
            df['volume_change_abs'] = df['volume_change'].abs()
            
            # Volume trends
            for window in [5, 10, 20]:
                df[f'volume_ma_{window}'] = df['volume'].rolling(window).mean()
                df[f'volume_ratio_{window}'] = df['volume'] / df[f'volume_ma_{window}']
                df[f'volume_trend_{window}'] = df[f'volume_ma_{window}'].pct_change()
            
            # Price-volume relationships
            df['price_volume_trend'] = df['price_change'] * df['volume_change']
            df['volume_weighted_price'] = (df['volume'] * df['close']).rolling(20).sum() / df['volume'].rolling(20).sum()
            df['vwap_ratio'] = df['close'] / df['volume_weighted_price']
            
            # Volume momentum
            df['volume_momentum_5'] = df['volume'] / df['volume'].shift(5)
            df['volume_momentum_10'] = df['volume'] / df['volume'].shift(10)
            
            # On Balance Volume
            df['obv'] = (df['volume'] * np.sign(df['close'].diff())).cumsum()
            df['obv_change'] = df['obv'].pct_change()
            df['obv_ma_10'] = df['obv'].rolling(10).mean()
            df['obv_ratio'] = df['obv'] / df['obv_ma_10']
            
            self.feature_categories['volume'].extend([
                'volume_change', 'volume_change_abs', 'volume_ma_5', 'volume_ma_10', 'volume_ma_20',
                'volume_ratio_5', 'volume_ratio_10', 'volume_ratio_20', 'volume_trend_5',
                'volume_trend_10', 'volume_trend_20', 'price_volume_trend', 'volume_weighted_price',
                'vwap_ratio', 'volume_momentum_5', 'volume_momentum_10', 'obv', 'obv_change',
                'obv_ma_10', 'obv_ratio'
            ])
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding volume features: {e}")
            return df
    
    def _add_technical_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add comprehensive technical indicators"""
        try:
            # RSI (Relative Strength Index)
            delta = df['close'].diff()
            gain = delta.where(delta > 0, 0).rolling(14).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(14).mean()
            rs = gain / loss
            df['rsi'] = 100 - (100 / (1 + rs))
            df['rsi_oversold'] = (df['rsi'] < 30).astype(int)
            df['rsi_overbought'] = (df['rsi'] > 70).astype(int)
            
            # MACD
            ema_12 = df['close'].ewm(span=12).mean()
            ema_26 = df['close'].ewm(span=26).mean()
            df['macd'] = ema_12 - ema_26
            df['macd_signal'] = df['macd'].ewm(span=9).mean()
            df['macd_histogram'] = df['macd'] - df['macd_signal']
            df['macd_cross'] = ((df['macd'] > df['macd_signal']) & 
                              (df['macd'].shift(1) <= df['macd_signal'].shift(1))).astype(int)
            
            # Bollinger Bands
            for period in [20, 50]:
                bb_middle = df['close'].rolling(period).mean()
                bb_std = df['close'].rolling(period).std()
                df[f'bb_upper_{period}'] = bb_middle + (bb_std * 2)
                df[f'bb_lower_{period}'] = bb_middle - (bb_std * 2)
                df[f'bb_position_{period}'] = (df['close'] - df[f'bb_lower_{period}']) / (
                    df[f'bb_upper_{period}'] - df[f'bb_lower_{period}'])
                df[f'bb_width_{period}'] = (df[f'bb_upper_{period}'] - df[f'bb_lower_{period}']) / bb_middle
                df[f'bb_squeeze_{period}'] = (df[f'bb_width_{period}'] < df[f'bb_width_{period}'].rolling(20).mean()).astype(int)
            
            # Moving Averages and Cross-overs
            for ma_period in [7, 14, 30, 50, 100]:
                df[f'ma_{ma_period}'] = df['close'].rolling(ma_period).mean()
                df[f'price_ma_ratio_{ma_period}'] = df['close'] / df[f'ma_{ma_period}']
                df[f'ma_trend_{ma_period}'] = df[f'ma_{ma_period}'].pct_change()
            
            # MA Cross-overs
            df['ma_cross_7_14'] = ((df['ma_7'] > df['ma_14']) & 
                                  (df['ma_7'].shift(1) <= df['ma_14'].shift(1))).astype(int)
            df['ma_cross_14_30'] = ((df['ma_14'] > df['ma_30']) & 
                                   (df['ma_14'].shift(1) <= df['ma_30'].shift(1))).astype(int)
            
            # Williams %R
            high_14 = df['high'].rolling(14).max()
            low_14 = df['low'].rolling(14).min()
            df['williams_r'] = -100 * (high_14 - df['close']) / (high_14 - low_14)
            
            # Stochastic Oscillator
            df['stoch_k'] = 100 * (df['close'] - low_14) / (high_14 - low_14)
            df['stoch_d'] = df['stoch_k'].rolling(3).mean()
            
            self.feature_categories['technical'].extend([
                'rsi', 'rsi_oversold', 'rsi_overbought', 'macd', 'macd_signal', 'macd_histogram',
                'macd_cross', 'bb_upper_20', 'bb_lower_20', 'bb_position_20', 'bb_width_20',
                'bb_squeeze_20', 'bb_upper_50', 'bb_lower_50', 'bb_position_50', 'bb_width_50',
                'bb_squeeze_50', 'ma_7', 'ma_14', 'ma_30', 'ma_50', 'ma_100',
                'price_ma_ratio_7', 'price_ma_ratio_14', 'price_ma_ratio_30', 'price_ma_ratio_50',
                'price_ma_ratio_100', 'ma_trend_7', 'ma_trend_14', 'ma_trend_30', 'ma_trend_50',
                'ma_trend_100', 'ma_cross_7_14', 'ma_cross_14_30', 'williams_r', 'stoch_k', 'stoch_d'
            ])
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding technical indicators: {e}")
            return df
    
    def _add_volatility_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add volatility-based features"""
        try:
            # Rolling volatility (multiple windows)
            for window in [5, 10, 14, 20, 30]:
                df[f'volatility_{window}'] = df['price_change'].rolling(window).std()
                df[f'volatility_rank_{window}'] = df[f'volatility_{window}'].rank(pct=True)
            
            # Parkinson volatility (uses high-low range)
            df['parkinson_vol_20'] = np.sqrt(
                (1 / (4 * np.log(2))) * 
                np.log(df['high'] / df['low']).rolling(20).mean()
            )
            
            # Garman-Klass volatility
            df['gk_vol_20'] = np.sqrt(
                0.5 * np.log(df['high'] / df['low'])**2 - 
                (2 * np.log(2) - 1) * np.log(df['close'] / df['open'])**2
            ).rolling(20).mean()
            
            # Range-based volatility
            for window in [5, 14, 30]:
                df[f'true_range_{window}'] = np.maximum(
                    df['high'] - df['low'],
                    np.maximum(
                        np.abs(df['high'] - df['close'].shift(1)),
                        np.abs(df['low'] - df['close'].shift(1))
                    )
                ).rolling(window).mean()
                df[f'atr_{window}'] = df[f'true_range_{window}'] / df['close']
            
            # Volatility regime detection
            df['vol_regime'] = (df['volatility_14'] > df['volatility_14'].rolling(60).mean()).astype(int)
            
            self.feature_categories['volatility'].extend([
                'volatility_5', 'volatility_10', 'volatility_14', 'volatility_20', 'volatility_30',
                'volatility_rank_5', 'volatility_rank_10', 'volatility_rank_14', 'volatility_rank_20',
                'volatility_rank_30', 'parkinson_vol_20', 'gk_vol_20', 'true_range_5', 'true_range_14',
                'true_range_30', 'atr_5', 'atr_14', 'atr_30', 'vol_regime'
            ])
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding volatility features: {e}")
            return df
    
    def _add_momentum_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add momentum-based features"""
        try:
            # Rate of Change (ROC)
            for period in [5, 10, 20, 30]:
                df[f'roc_{period}'] = df['close'].pct_change(period)
                df[f'momentum_{period}'] = df['close'] / df['close'].shift(period) - 1
            
            # Price acceleration
            df['price_acceleration_5'] = df['price_change'] - df['price_change'].shift(5)
            df['price_acceleration_10'] = df['price_change'] - df['price_change'].shift(10)
            
            # Momentum indicators
            df['momentum_rank_20'] = df['roc_20'].rolling(60).rank(pct=True)
            df['momentum_strength'] = (df['roc_5'] + df['roc_10'] + df['roc_20']) / 3
            
            # Trend strength
            positive_changes = (df['close'] > df['close'].shift(1)).rolling(10).sum()
            df['trend_strength_10'] = (positive_changes - 5) / 5  # Normalized between -1 and 1
            
            self.feature_categories['momentum'].extend([
                'roc_5', 'roc_10', 'roc_20', 'roc_30', 'momentum_5', 'momentum_10', 'momentum_20',
                'momentum_30', 'price_acceleration_5', 'price_acceleration_10', 'momentum_rank_20',
                'momentum_strength', 'trend_strength_10'
            ])
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding momentum features: {e}")
            return df
    
    def _add_time_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add time-based features for crypto (24/7 markets)"""
        try:
            # Day-based features
            df['day_of_week'] = df.index.dayofweek
            df['is_weekend'] = (df['day_of_week'] >= 5).astype(int)
            df['day_of_month'] = df.index.day
            df['is_month_end'] = (df.index.day >= 28).astype(int)
            df['is_month_start'] = (df.index.day <= 3).astype(int)
            
            # Seasonal patterns (if we have enough data)
            df['month'] = df.index.month
            df['quarter'] = df.index.quarter
            
            # Cyclical encoding for day and month
            df['day_sin'] = np.sin(2 * np.pi * df['day_of_week'] / 7)
            df['day_cos'] = np.cos(2 * np.pi * df['day_of_week'] / 7)
            df['month_sin'] = np.sin(2 * np.pi * df['month'] / 12)
            df['month_cos'] = np.cos(2 * np.pi * df['month'] / 12)
            
            # Hour-based features (if available)
            if hasattr(df.index, 'hour'):
                df['hour'] = df.index.hour
                df['is_us_trading_hours'] = ((df['hour'] >= 14) & (df['hour'] <= 21)).astype(int)
                df['is_asia_trading_hours'] = ((df['hour'] >= 0) & (df['hour'] <= 8)).astype(int)
                df['is_europe_trading_hours'] = ((df['hour'] >= 8) & (df['hour'] <= 16)).astype(int)
                
                self.feature_categories['time'].extend(['hour', 'is_us_trading_hours', 'is_asia_trading_hours', 'is_europe_trading_hours'])
            
            self.feature_categories['time'].extend([
                'day_of_week', 'is_weekend', 'day_of_month', 'is_month_end', 'is_month_start',
                'month', 'quarter', 'day_sin', 'day_cos', 'month_sin', 'month_cos'
            ])
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding time features: {e}")
            return df
    
    def _add_lag_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add lag features for time series prediction"""
        try:
            # Price lags
            for lag in [1, 2, 3, 5, 7, 14, 21]:
                df[f'close_lag_{lag}'] = df['close'].shift(lag)
                df[f'volume_lag_{lag}'] = df['volume'].shift(lag)
                if f'volatility_14' in df.columns:
                    df[f'volatility_lag_{lag}'] = df['volatility_14'].shift(lag)
            
            # Price change lags
            for lag in [1, 2, 3, 5, 7]:
                df[f'price_change_lag_{lag}'] = df['price_change'].shift(lag)
                df[f'log_return_lag_{lag}'] = df['log_return'].shift(lag)
            
            # Technical indicator lags
            for lag in [1, 3, 7]:
                if 'rsi' in df.columns:
                    df[f'rsi_lag_{lag}'] = df['rsi'].shift(lag)
                if 'ma_14' in df.columns:
                    df[f'ma_14_lag_{lag}'] = df['ma_14'].shift(lag)
                if 'macd' in df.columns:
                    df[f'macd_lag_{lag}'] = df['macd'].shift(lag)
            
            # Create lag feature list dynamically
            lag_features = [col for col in df.columns if '_lag_' in col]
            self.feature_categories['lag'].extend(lag_features)
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding lag features: {e}")
            return df
    
    def _add_statistical_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add statistical features"""
        try:
            # Rolling statistics
            for window in [10, 20, 30]:
                df[f'price_skew_{window}'] = df['close'].rolling(window).skew()
                df[f'price_kurt_{window}'] = df['close'].rolling(window).kurt()
                df[f'volume_skew_{window}'] = df['volume'].rolling(window).skew()
                
            # Percentile ranks
            for window in [20, 60]:
                df[f'price_percentile_{window}'] = df['close'].rolling(window).rank(pct=True)
                df[f'volume_percentile_{window}'] = df['volume'].rolling(window).rank(pct=True)
            
            # Z-scores
            for window in [20, 60]:
                price_mean = df['close'].rolling(window).mean()
                price_std = df['close'].rolling(window).std()
                df[f'price_zscore_{window}'] = (df['close'] - price_mean) / price_std
                
                volume_mean = df['volume'].rolling(window).mean()
                volume_std = df['volume'].rolling(window).std()
                df[f'volume_zscore_{window}'] = (df['volume'] - volume_mean) / volume_std
            
            return df
            
        except Exception as e:
            logger.error(f"Error adding statistical features: {e}")
            return df
    
    def _clean_and_validate_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Clean and validate all features"""
        try:
            # Replace infinite values
            df = df.replace([np.inf, -np.inf], np.nan)
            
            # Forward fill then backward fill
            df = df.fillna(method='ffill').fillna(method='bfill')
            
            # Drop any remaining NaN rows
            initial_rows = len(df)
            df = df.dropna()
            dropped_rows = initial_rows - len(df)
            
            if dropped_rows > 0:
                logger.warning(f"Dropped {dropped_rows} rows with missing values")
            
            return df
            
        except Exception as e:
            logger.error(f"Error cleaning features: {e}")
            return df
    
    def _log_feature_summary(self):
        """Log summary of created features"""
        try:
            total_features = sum(len(features) for features in self.feature_categories.values())
            
            logger.info("Feature Engineering Summary:")
            for category, features in self.feature_categories.items():
                if features:
                    logger.info(f"  {category.capitalize()}: {len(features)} features")
            
            logger.info(f"Total engineered features: {total_features}")
            
        except Exception as e:
            logger.error(f"Error logging feature summary: {e}")
    
    def get_feature_names(self, exclude_base: bool = True) -> List[str]:
        """
        Get list of all feature names
        
        Args:
            exclude_base: If True, exclude base OHLCV columns
            
        Returns:
            List of feature names
        """
        all_features = []
        for features in self.feature_categories.values():
            all_features.extend(features)
        
        if exclude_base:
            base_cols = ['open', 'high', 'low', 'close', 'volume']
            all_features = [f for f in all_features if f not in base_cols]
        
        return all_features
    
    def get_features_by_category(self, category: str) -> List[str]:
        """Get features by category"""
        return self.feature_categories.get(category, [])
