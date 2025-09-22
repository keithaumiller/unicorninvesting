"""
XGBoost Feature Engineering for Cryptocurrency Forecasting

Advanced feature engineering specifically designed for XGBoost models
trading cryptocurrency markets. Includes technical indicators, market
regime detection, and time-series specific features.

Features:
- Comprehensive technical analysis indicators
- Crypto-specific volatility measures
- Market regime classification
- Multi-timeframe analysis
- Volume profile analysis
"""

import pandas as pd
import numpy as np
from typing import List, Dict, Optional, Tuple
import logging

logger = logging.getLogger(__name__)

class XGBoostFeatureEngineer:
    """
    Advanced feature engineering for XGBoost cryptocurrency models
    """
    
    def __init__(self, config: Optional[Dict] = None):
        """
        Initialize feature engineer with configuration
        
        Args:
            config: Configuration dictionary for feature engineering
        """
        self.config = config or self._get_default_config()
        
    def _get_default_config(self) -> Dict:
        """Get default feature engineering configuration"""
        return {
            'lookback_periods': [5, 10, 20, 50, 100],
            'volatility_periods': [10, 20, 30],
            'momentum_periods': [5, 10, 14, 20],
            'volume_periods': [10, 20, 30],
            'include_technical_indicators': True,
            'include_market_regime': True,
            'include_time_features': True,
            'include_lag_features': True,
            'max_lags': 10,
            'outlier_threshold': 3.0
        }
    
    def create_price_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create price-based features"""
        logger.info("Creating price-based features")
        
        # Basic price features
        df['returns'] = df['close'].pct_change()
        df['log_returns'] = np.log(df['close'] / df['close'].shift(1))
        df['price_change'] = df['close'] - df['close'].shift(1)
        df['price_change_pct'] = df['price_change'] / df['close'].shift(1) * 100
        
        # OHLC relationships
        df['hl_ratio'] = (df['high'] - df['low']) / df['close']
        df['oc_ratio'] = (df['close'] - df['open']) / df['open']
        df['body_size'] = abs(df['close'] - df['open']) / df['close']
        df['upper_shadow'] = (df['high'] - np.maximum(df['open'], df['close'])) / df['close']
        df['lower_shadow'] = (np.minimum(df['open'], df['close']) - df['low']) / df['close']
        
        # True Range and Average True Range
        high_low = df['high'] - df['low']
        high_close = np.abs(df['high'] - df['close'].shift())
        low_close = np.abs(df['low'] - df['close'].shift())
        df['true_range'] = np.maximum(high_low, np.maximum(high_close, low_close))
        df['atr'] = df['true_range'].rolling(14).mean()
        df['atr_ratio'] = df['true_range'] / df['atr']
        
        return df
    
    def create_moving_average_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create moving average based features"""
        logger.info("Creating moving average features")
        
        for period in self.config['lookback_periods']:
            # Simple Moving Average
            df[f'sma_{period}'] = df['close'].rolling(period).mean()
            df[f'sma_{period}_ratio'] = df['close'] / df[f'sma_{period}']
            df[f'sma_{period}_slope'] = df[f'sma_{period}'].diff(5) / df[f'sma_{period}'].shift(5)
            
            # Exponential Moving Average
            df[f'ema_{period}'] = df['close'].ewm(span=period).mean()
            df[f'ema_{period}_ratio'] = df['close'] / df[f'ema_{period}']
            df[f'ema_{period}_distance'] = (df['close'] - df[f'ema_{period}']) / df[f'ema_{period}']
            
            # Moving average convergence/divergence
            if period in [10, 20]:
                other_period = 20 if period == 10 else 50
                if other_period in self.config['lookback_periods']:
                    df[f'ma_convergence_{period}_{other_period}'] = (df[f'sma_{period}'] - df[f'sma_{other_period}']) / df[f'sma_{other_period}']
        
        # MACD
        ema_12 = df['close'].ewm(span=12).mean()
        ema_26 = df['close'].ewm(span=26).mean()
        df['macd'] = ema_12 - ema_26
        df['macd_signal'] = df['macd'].ewm(span=9).mean()
        df['macd_histogram'] = df['macd'] - df['macd_signal']
        df['macd_ratio'] = df['macd'] / df['close']
        
        return df
    
    def create_volatility_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create volatility-based features"""
        logger.info("Creating volatility features")
        
        for period in self.config['volatility_periods']:
            # Rolling volatility
            df[f'volatility_{period}'] = df['returns'].rolling(period).std()
            df[f'volatility_{period}_ma'] = df[f'volatility_{period}'].rolling(5).mean()
            df[f'volatility_ratio_{period}'] = df[f'volatility_{period}'] / df[f'volatility_{period}_ma']
            
            # Bollinger Bands
            sma = df['close'].rolling(period).mean()
            std = df['close'].rolling(period).std()
            df[f'bb_upper_{period}'] = sma + (2 * std)
            df[f'bb_lower_{period}'] = sma - (2 * std)
            df[f'bb_width_{period}'] = (df[f'bb_upper_{period}'] - df[f'bb_lower_{period}']) / sma
            df[f'bb_position_{period}'] = (df['close'] - df[f'bb_lower_{period}']) / (df[f'bb_upper_{period}'] - df[f'bb_lower_{period}'])
            
            # Keltner Channels
            df[f'kc_upper_{period}'] = df[f'ema_{period}'] + (2 * df['atr'])
            df[f'kc_lower_{period}'] = df[f'ema_{period}'] - (2 * df['atr'])
            df[f'kc_position_{period}'] = (df['close'] - df[f'kc_lower_{period}']) / (df[f'kc_upper_{period}'] - df[f'kc_lower_{period}'])
            
            # Donchian Channels
            df[f'dc_upper_{period}'] = df['high'].rolling(period).max()
            df[f'dc_lower_{period}'] = df['low'].rolling(period).min()
            df[f'dc_middle_{period}'] = (df[f'dc_upper_{period}'] + df[f'dc_lower_{period}']) / 2
            df[f'dc_position_{period}'] = (df['close'] - df[f'dc_lower_{period}']) / (df[f'dc_upper_{period}'] - df[f'dc_lower_{period}'])
        
        # GARCH-style volatility clustering
        df['volatility_clustering'] = df['returns'].rolling(20).apply(lambda x: (x ** 2).autocorr(lags=1))
        
        return df
    
    def create_momentum_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create momentum-based features"""
        logger.info("Creating momentum features")
        
        for period in self.config['momentum_periods']:
            # RSI
            delta = df['close'].diff()
            gain = (delta.where(delta > 0, 0)).rolling(period).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(period).mean()
            rs = gain / loss
            df[f'rsi_{period}'] = 100 - (100 / (1 + rs))
            df[f'rsi_{period}_normalized'] = (df[f'rsi_{period}'] - 50) / 50
            
            # Stochastic Oscillator
            high_roll = df['high'].rolling(period).max()
            low_roll = df['low'].rolling(period).min()
            df[f'stoch_k_{period}'] = ((df['close'] - low_roll) / (high_roll - low_roll)) * 100
            df[f'stoch_d_{period}'] = df[f'stoch_k_{period}'].rolling(3).mean()
            
            # Williams %R
            df[f'williams_r_{period}'] = ((high_roll - df['close']) / (high_roll - low_roll)) * -100
            
            # Rate of Change
            df[f'roc_{period}'] = df['close'].pct_change(period) * 100
            df[f'roc_{period}_ma'] = df[f'roc_{period}'].rolling(5).mean()
            
            # Momentum
            df[f'momentum_{period}'] = df['close'] - df['close'].shift(period)
            df[f'momentum_{period}_ratio'] = df[f'momentum_{period}'] / df['close'].shift(period)
            
            # CCI (Commodity Channel Index)
            typical_price = (df['high'] + df['low'] + df['close']) / 3
            sma_tp = typical_price.rolling(period).mean()
            mad_tp = typical_price.rolling(period).apply(lambda x: np.mean(np.abs(x - x.mean())))
            df[f'cci_{period}'] = (typical_price - sma_tp) / (0.015 * mad_tp)
        
        # ADX (Average Directional Index)
        high_diff = df['high'].diff()
        low_diff = df['low'].diff()
        plus_dm = np.where((high_diff > low_diff) & (high_diff > 0), high_diff, 0)
        minus_dm = np.where((low_diff > high_diff) & (low_diff > 0), low_diff, 0)
        
        tr = df['true_range']
        plus_di = 100 * (pd.Series(plus_dm).rolling(14).sum() / tr.rolling(14).sum())
        minus_di = 100 * (pd.Series(minus_dm).rolling(14).sum() / tr.rolling(14).sum())
        
        dx = 100 * np.abs(plus_di - minus_di) / (plus_di + minus_di)
        df['adx'] = dx.rolling(14).mean()
        df['plus_di'] = plus_di
        df['minus_di'] = minus_di
        
        return df
    
    def create_volume_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create volume-based features"""
        logger.info("Creating volume features")
        
        # Basic volume features
        df['volume_change'] = df['volume'].pct_change()
        df['volume_price_trend'] = df['volume'] * df['returns']
        df['volume_price_correlation'] = df['volume'].rolling(20).corr(df['close'])
        
        for period in self.config['volume_periods']:
            # Volume moving averages
            df[f'volume_sma_{period}'] = df['volume'].rolling(period).mean()
            df[f'volume_ratio_{period}'] = df['volume'] / df[f'volume_sma_{period}']
            df[f'volume_ema_{period}'] = df['volume'].ewm(span=period).mean()
            
            # Volume-weighted prices
            df[f'vwap_{period}'] = (df['volume'] * (df['high'] + df['low'] + df['close']) / 3).rolling(period).sum() / df['volume'].rolling(period).sum()
            df[f'vwap_deviation_{period}'] = (df['close'] - df[f'vwap_{period}']) / df[f'vwap_{period}']
        
        # On-Balance Volume
        obv = np.where(df['close'] > df['close'].shift(1), df['volume'], 
                      np.where(df['close'] < df['close'].shift(1), -df['volume'], 0))
        df['obv'] = pd.Series(obv).cumsum()
        df['obv_sma'] = df['obv'].rolling(20).mean()
        df['obv_ratio'] = df['obv'] / df['obv_sma']
        
        # Accumulation/Distribution Line
        clv = ((df['close'] - df['low']) - (df['high'] - df['close'])) / (df['high'] - df['low'])
        clv = clv.fillna(0)  # Handle division by zero
        ad_line = (clv * df['volume']).cumsum()
        df['ad_line'] = ad_line
        df['ad_line_sma'] = df['ad_line'].rolling(20).mean()
        
        # Money Flow Index
        typical_price = (df['high'] + df['low'] + df['close']) / 3
        money_flow = typical_price * df['volume']
        positive_flow = np.where(typical_price > typical_price.shift(1), money_flow, 0)
        negative_flow = np.where(typical_price < typical_price.shift(1), money_flow, 0)
        
        positive_flow_sum = pd.Series(positive_flow).rolling(14).sum()
        negative_flow_sum = pd.Series(negative_flow).rolling(14).sum()
        mfi = 100 - (100 / (1 + positive_flow_sum / negative_flow_sum))
        df['mfi'] = mfi
        
        return df
    
    def create_market_regime_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create market regime classification features"""
        logger.info("Creating market regime features")
        
        # Trend strength using linear regression slope
        def calculate_trend_strength(series, window=20):
            slopes = []
            for i in range(window, len(series)):
                y = series.iloc[i-window:i].values
                x = np.arange(len(y))
                slope = np.polyfit(x, y, 1)[0]
                slopes.append(slope)
            return pd.Series([np.nan] * window + slopes, index=series.index)
        
        df['trend_strength'] = calculate_trend_strength(df['close'])
        df['trend_strength_normalized'] = df['trend_strength'] / df['close']
        
        # Market regime based on moving averages
        df['ma_short'] = df['close'].rolling(10).mean()
        df['ma_medium'] = df['close'].rolling(20).mean()
        df['ma_long'] = df['close'].rolling(50).mean()
        
        # Bull/Bear/Sideways classification
        df['regime_bull'] = ((df['ma_short'] > df['ma_medium']) & (df['ma_medium'] > df['ma_long'])).astype(int)
        df['regime_bear'] = ((df['ma_short'] < df['ma_medium']) & (df['ma_medium'] < df['ma_long'])).astype(int)
        df['regime_sideways'] = (~df['regime_bull'].astype(bool) & ~df['regime_bear'].astype(bool)).astype(int)
        
        # Volatility regime
        vol_20 = df['returns'].rolling(20).std()
        vol_ma = vol_20.rolling(20).mean()
        df['volatility_regime_high'] = (vol_20 > vol_ma * 1.5).astype(int)
        df['volatility_regime_low'] = (vol_20 < vol_ma * 0.5).astype(int)
        df['volatility_regime_normal'] = (~df['volatility_regime_high'].astype(bool) & ~df['volatility_regime_low'].astype(bool)).astype(int)
        
        # Support and resistance levels
        df['resistance_level'] = df['high'].rolling(20).max()
        df['support_level'] = df['low'].rolling(20).min()
        df['distance_to_resistance'] = (df['resistance_level'] - df['close']) / df['close']
        df['distance_to_support'] = (df['close'] - df['support_level']) / df['close']
        
        return df
    
    def create_time_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create time-based features for 24/7 crypto markets"""
        logger.info("Creating time-based features")
        
        # Ensure we have a datetime index
        if not isinstance(df.index, pd.DatetimeIndex):
            return df
        
        # Remove timezone if present for processing
        if df.index.tz is not None:
            df.index = df.index.tz_localize(None)
        
        # Basic time features
        df['hour'] = df.index.hour
        df['day_of_week'] = df.index.dayofweek
        df['day_of_month'] = df.index.day
        df['month'] = df.index.month
        df['quarter'] = df.index.quarter
        df['is_weekend'] = (df.index.dayofweek >= 5).astype(int)
        
        # Cyclical encoding for better ML performance
        df['hour_sin'] = np.sin(2 * np.pi * df['hour'] / 24)
        df['hour_cos'] = np.cos(2 * np.pi * df['hour'] / 24)
        df['dow_sin'] = np.sin(2 * np.pi * df['day_of_week'] / 7)
        df['dow_cos'] = np.cos(2 * np.pi * df['day_of_week'] / 7)
        df['month_sin'] = np.sin(2 * np.pi * df['month'] / 12)
        df['month_cos'] = np.cos(2 * np.pi * df['month'] / 12)
        
        # Trading session indicators for crypto (approximate)
        # Asian session: 0-8 UTC, European: 8-16 UTC, US: 16-24 UTC
        df['session_asian'] = ((df['hour'] >= 0) & (df['hour'] < 8)).astype(int)
        df['session_european'] = ((df['hour'] >= 8) & (df['hour'] < 16)).astype(int)
        df['session_us'] = ((df['hour'] >= 16) & (df['hour'] < 24)).astype(int)
        
        return df
    
    def create_lag_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Create lagged features for time series patterns"""
        logger.info("Creating lag features")
        
        # Key variables to lag
        lag_variables = ['close', 'returns', 'volume', 'volatility_20', 'rsi_14']
        
        for var in lag_variables:
            if var in df.columns:
                for lag in range(1, min(self.config['max_lags'] + 1, 11)):  # Max 10 lags
                    df[f'{var}_lag_{lag}'] = df[var].shift(lag)
        
        # Rolling statistics on lags
        if 'returns' in df.columns:
            for window in [3, 5, 10]:
                df[f'returns_rolling_mean_{window}'] = df['returns'].rolling(window).mean()
                df[f'returns_rolling_std_{window}'] = df['returns'].rolling(window).std()
                df[f'returns_rolling_skew_{window}'] = df['returns'].rolling(window).skew()
                df[f'returns_rolling_kurt_{window}'] = df['returns'].rolling(window).kurtosis()
        
        return df
    
    def remove_outliers(self, df: pd.DataFrame) -> pd.DataFrame:
        """Remove or cap extreme outliers"""
        logger.info("Processing outliers")
        
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        
        for col in numeric_cols:
            if col not in ['target']:  # Don't modify target variable
                # Use IQR method for outlier detection
                Q1 = df[col].quantile(0.25)
                Q3 = df[col].quantile(0.75)
                IQR = Q3 - Q1
                
                lower_bound = Q1 - self.config['outlier_threshold'] * IQR
                upper_bound = Q3 + self.config['outlier_threshold'] * IQR
                
                # Cap outliers instead of removing them
                df[col] = np.clip(df[col], lower_bound, upper_bound)
        
        return df
    
    def create_all_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """Create all features for XGBoost model"""
        logger.info("🔧 Creating comprehensive XGBoost feature set")
        
        df = data.copy()
        
        # Ensure required columns exist
        required_cols = ['open', 'high', 'low', 'close', 'volume']
        for col in required_cols:
            if col not in df.columns:
                raise ValueError(f"Required column '{col}' not found in data")
        
        # Create all feature categories
        df = self.create_price_features(df)
        df = self.create_moving_average_features(df)
        df = self.create_volatility_features(df)
        df = self.create_momentum_features(df)
        df = self.create_volume_features(df)
        
        if self.config['include_market_regime']:
            df = self.create_market_regime_features(df)
        
        if self.config['include_time_features']:
            df = self.create_time_features(df)
        
        if self.config['include_lag_features']:
            df = self.create_lag_features(df)
        
        # Process outliers
        df = self.remove_outliers(df)
        
        # Replace infinite values
        df = df.replace([np.inf, -np.inf], np.nan)
        
        logger.info(f"✅ Created {df.shape[1]} total features")
        
        return df

# TODO: Implement XGBoostFeatureEngine
# Migrate functionality from legacy asset-first structure

class XGBoostFeatureEngine:
    """Placeholder for XGBoost Feature Engineering"""
    
    def __init__(self):
        """Initialize XGBoostFeatureEngine"""
        raise NotImplementedError("This class will be implemented in migration Phase 2-4")

# TODO: Add implementation from legacy files
