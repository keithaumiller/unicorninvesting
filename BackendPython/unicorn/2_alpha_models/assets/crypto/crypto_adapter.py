"""
Crypto Asset Adapter Implementation

EXTRACTED FROM: /legacy/CRYPTO_original/enhanced_crypto_prophet_builder.py

This module implements crypto-specific asset handling, preserving all crypto market
characteristics and data processing logic from the legacy implementation.

LEGACY REFERENCE: enhanced_crypto_prophet_builder.py crypto-specific features
Key Features Preserved:
- Crypto market session patterns (24/7 trading with time zones)
- Crypto-specific volatility characteristics
- Volume-based indicators for crypto markets
- Asset-specific Prophet configurations (ETH, BTC, etc.)
- Realistic crypto data simulation and base prices
- Crypto time patterns and market behavior
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional, Union

from ...core.interfaces import AssetAdapter
from ...core.configuration import AssetConfig


class CryptoAdapter(AssetAdapter):
    """
    Crypto asset adapter with market-specific characteristics.
    
    PRESERVES ALL FUNCTIONALITY FROM: enhanced_crypto_prophet_builder.py
    
    Key Features:
    - 24/7 trading session patterns with timezone awareness
    - Crypto-specific volatility and return characteristics
    - Volume patterns for major crypto assets
    - Asset-specific base prices and market behavior
    - Realistic data generation for testing and simulation
    """
    
    def __init__(self, config: AssetConfig):
        """
        Initialize crypto adapter with asset-specific configurations.
        
        Args:
            config: Crypto-specific configuration parameters
        """
        super().__init__(config)
        
        # Crypto-specific base prices (preserved from legacy)
        self.base_prices = {
            'ETH': 2000,
            'BTC': 45000,
            'ADA': 0.5,
            'DOT': 8,
            'LINK': 15,
            'MATIC': 0.8,
            'SOL': 100,
            'AVAX': 25
        }
        
        # Crypto-specific volatility parameters
        self.volatility_params = {
            'hourly_volatility': 0.03,  # 3% hourly volatility for crypto
            'daily_volatility': 0.15,   # 15% daily volatility
            'weekly_volatility': 0.40   # 40% weekly volatility
        }
        
        print(f"🪙 CryptoAdapter initialized for {config.asset_type}")
        print(f"   💰 Base prices available for: {list(self.base_prices.keys())}")
    
    def normalize_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize crypto data to standard OHLCV format.
        
        EXTRACTED FROM: Legacy crypto data handling patterns
        
        Args:
            data: Raw crypto data in various formats
            
        Returns:
            Standardized OHLCV DataFrame with datetime index
        """
        normalized_data = data.copy()
        
        # Ensure datetime index
        if not isinstance(normalized_data.index, pd.DatetimeIndex):
            if 'timestamp' in normalized_data.columns:
                normalized_data.index = pd.to_datetime(normalized_data['timestamp'])
                normalized_data = normalized_data.drop('timestamp', axis=1)
            elif 'date' in normalized_data.columns:
                normalized_data.index = pd.to_datetime(normalized_data['date'])
                normalized_data = normalized_data.drop('date', axis=1)
            else:
                print(f"⚠️  Warning: Creating default datetime index for crypto data")
                normalized_data.index = pd.date_range(
                    start='2020-01-01', 
                    periods=len(normalized_data), 
                    freq='1H'
                )
        
        # Standardize column names
        column_mapping = {
            'open': 'Open',
            'high': 'High', 
            'low': 'Low',
            'close': 'Close',
            'price': 'Close',  # Use price as close if available
            'volume': 'Volume',
            'vol': 'Volume'
        }
        
        for old_col, new_col in column_mapping.items():
            if old_col in normalized_data.columns:
                normalized_data[new_col] = normalized_data[old_col]
                if old_col != new_col:
                    normalized_data = normalized_data.drop(old_col, axis=1)
        
        # Ensure required OHLCV columns exist
        required_columns = ['Open', 'High', 'Low', 'Close', 'Volume']
        for col in required_columns:
            if col not in normalized_data.columns:
                if col == 'Volume':
                    # Generate realistic volume if missing
                    normalized_data[col] = np.random.lognormal(16, 1.5, len(normalized_data))
                else:
                    # For OHLC, use Close price as fallback
                    if 'Close' in normalized_data.columns:
                        normalized_data[col] = normalized_data['Close']
                    else:
                        # Last resort - use first available price column
                        price_col = normalized_data.select_dtypes(include=[np.number]).columns[0]
                        normalized_data[col] = normalized_data[price_col]
        
        # Sort by datetime index
        normalized_data = normalized_data.sort_index()
        
        print(f"📊 Normalized crypto data: {len(normalized_data)} rows, {len(normalized_data.columns)} columns")
        print(f"   📅 Date range: {normalized_data.index[0]} to {normalized_data.index[-1]}")
        
        return normalized_data
    
    def add_market_sessions(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Add crypto market session indicators.
        
        PRESERVED FROM: enhanced_crypto_prophet_builder.py session features
        
        Args:
            data: OHLCV crypto data with datetime index
            
        Returns:
            DataFrame with crypto market session indicators
        """
        session_data = data.copy()
        
        # Crypto market session indicators (24/7 trading but has patterns)
        session_data['asian_hours'] = ((data.index.hour >= 0) & (data.index.hour < 8)).astype(int)
        session_data['european_hours'] = ((data.index.hour >= 8) & (data.index.hour < 16)).astype(int)
        session_data['american_hours'] = ((data.index.hour >= 16) & (data.index.hour < 24)).astype(int)
        
        # Traditional market time indicators (for cross-market analysis)
        session_data['is_new_york_close'] = (data.index.hour == 16).astype(int)  # Traditional market close
        session_data['is_london_open'] = (data.index.hour == 8).astype(int)     # London market open
        session_data['is_asian_morning'] = (data.index.hour == 4).astype(int)   # Asian morning
        
        # Weekend patterns (crypto still trades but volume patterns change)
        session_data['is_weekend'] = (data.index.dayofweek >= 5).astype(int)
        session_data['is_weekday'] = (data.index.dayofweek < 5).astype(int)
        
        print(f"🕐 Added {sum('hours' in col or 'open' in col or 'close' in col or 'weekend' in col for col in session_data.columns)} session indicators")
        
        return session_data
    
    def add_crypto_specific_features(self, data: pd.DataFrame, asset: str = 'ETH') -> pd.DataFrame:
        """
        Add crypto-specific features beyond basic OHLCV.
        
        EXTRACTED FROM: enhanced_crypto_prophet_builder.py crypto feature logic
        
        Args:
            data: OHLCV crypto data
            asset: Specific crypto asset (ETH, BTC, etc.)
            
        Returns:
            DataFrame with crypto-specific features
        """
        feature_data = data.copy()
        
        # Price-based features
        price_col = 'Close'
        prices = feature_data[price_col]
        
        # Crypto-specific return periods (optimized for crypto volatility)
        for lag in [1, 2, 3, 6, 12, 24, 48, 168]:  # 1h to 1w lags
            if len(prices) > lag:
                feature_data[f'return_{lag}h'] = prices.pct_change(lag)
        
        # Crypto-specific price changes (absolute values matter for crypto)
        for lag in [1, 6, 24, 168]:  # 1h, 6h, 1d, 1w
            if len(prices) > lag:
                feature_data[f'price_change_{lag}h'] = prices.diff(lag)
        
        # Crypto volatility measures (multiple timeframes)
        for window in [6, 24, 168, 720]:  # 6h, 1d, 1w, 1m
            if len(prices) > window:
                returns = prices.pct_change()
                feature_data[f'volatility_{window}h'] = returns.rolling(window).std()
        
        # Volume-based features (if volume available)
        if 'Volume' in feature_data.columns and not feature_data['Volume'].isna().all():
            volume = feature_data['Volume']
            
            # Volume patterns for crypto
            for lag in [1, 6, 24, 168]:  # Lagged volume
                if len(volume) > lag:
                    feature_data[f'volume_lag_{lag}h'] = volume.shift(lag)
            
            # Volume moving averages and ratios
            for window in [24, 168, 720]:  # 1d, 1w, 1m
                if len(volume) > window:
                    feature_data[f'volume_ma_{window}h'] = volume.rolling(window).mean()
                    feature_data[f'volume_ratio_{window}h'] = volume / volume.rolling(window).mean()
        
        # Asset-specific features
        if asset == 'ETH':
            # Ethereum-specific patterns (if any special handling needed)
            feature_data['eth_network_hour'] = (data.index.hour % 4 == 0).astype(int)
        elif asset == 'BTC':
            # Bitcoin-specific patterns
            feature_data['btc_halving_cycle'] = (data.index.dayofyear % 365 < 100).astype(int)
        
        print(f"⚡ Added {len(feature_data.columns) - len(data.columns)} crypto-specific features for {asset}")
        
        return feature_data
    
    def generate_sample_data(self, asset: str = 'ETH', timeframe: str = '1H', 
                           periods: int = 1000) -> pd.DataFrame:
        """
        Generate realistic sample crypto data for testing.
        
        PRESERVED FROM: enhanced_crypto_prophet_builder.py _generate_sample_crypto_data()
        Maintains all legacy data generation logic.
        
        Args:
            asset: Crypto asset symbol
            timeframe: Data frequency  
            periods: Number of data points
            
        Returns:
            Realistic sample crypto OHLCV data
        """
        # Get base price for asset
        base_price = self.base_prices.get(asset, 2000)
        
        # Generate datetime index based on timeframe
        if timeframe == '1H':
            freq = 'H'
            start_date = datetime.now() - timedelta(hours=periods)
        elif timeframe == '1D':
            freq = 'D'
            start_date = datetime.now() - timedelta(days=periods)
        else:
            freq = 'H'  # Default to hourly
            start_date = datetime.now() - timedelta(hours=periods)
        
        dates = pd.date_range(start=start_date, periods=periods, freq=freq)
        
        # Generate realistic price movements
        np.random.seed(hash(asset) % 2**32)  # Asset-specific but reproducible
        
        # Crypto-specific volatility
        volatility = self.volatility_params['hourly_volatility']
        if timeframe == '1D':
            volatility = self.volatility_params['daily_volatility']
        
        returns = np.random.normal(0, volatility, periods)
        
        # Add crypto-specific patterns
        trend = np.sin(np.arange(periods) * 2 * np.pi / 168) * 0.005  # Weekly cycle
        seasonal = np.sin(np.arange(periods) * 2 * np.pi / 24) * 0.002  # Daily cycle
        returns += trend + seasonal
        
        # Calculate prices
        prices = [base_price]
        for i in range(1, periods):
            prices.append(prices[-1] * (1 + returns[i]))
        
        # Generate OHLC with realistic spreads
        opens = [prices[0]] + prices[:-1]
        closes = prices
        
        # Realistic high/low spreads for crypto (wider than traditional assets)
        highs = [p * (1 + abs(np.random.normal(0, 0.01))) for p in prices]
        lows = [p * (1 - abs(np.random.normal(0, 0.01))) for p in prices]
        
        # Generate volume with crypto characteristics (log-normal distribution)
        volumes = np.random.lognormal(16, 1.5, periods)
        
        sample_data = pd.DataFrame({
            'Open': opens,
            'High': highs,
            'Low': lows,
            'Close': closes,
            'Volume': volumes
        }, index=dates)
        
        print(f"🎲 Generated {asset} sample data: {len(sample_data)} {timeframe} candles")
        print(f"   💰 Price range: ${min(closes):,.2f} - ${max(closes):,.2f}")
        print(f"   📊 Avg volume: {np.mean(volumes):,.0f}")
        
        return sample_data
    
    def get_asset_config(self, asset: str) -> Dict[str, Any]:
        """
        Get asset-specific configuration for crypto.
        
        EXTRACTED FROM: enhanced_crypto_prophet_builder.py Prophet config logic
        
        Args:
            asset: Crypto asset symbol
            
        Returns:
            Asset-specific configuration parameters
        """
        base_config = {
            'market_type': 'crypto',
            'trading_hours': '24/7',
            'timezone': 'UTC',
            'volatility_regime': 'high',
            'liquidity': 'high'
        }
        
        # Asset-specific parameters
        if asset == 'ETH':
            asset_config = {
                'base_price': self.base_prices['ETH'],
                'volatility_adjustment': 1.0,
                'volume_multiplier': 1.2,
                'seasonality_strength': 'medium',
                'trend_sensitivity': 'high'
            }
        elif asset == 'BTC':
            asset_config = {
                'base_price': self.base_prices['BTC'],
                'volatility_adjustment': 0.8,  # BTC slightly less volatile than ETH
                'volume_multiplier': 1.5,      # Higher volume
                'seasonality_strength': 'strong',
                'trend_sensitivity': 'medium'
            }
        else:
            # Default altcoin configuration
            asset_config = {
                'base_price': self.base_prices.get(asset, 10),
                'volatility_adjustment': 1.3,  # Altcoins more volatile
                'volume_multiplier': 0.8,      # Lower volume
                'seasonality_strength': 'weak',
                'trend_sensitivity': 'high'
            }
        
        base_config.update(asset_config)
        
        print(f"⚙️ Asset config for {asset}: base_price=${base_config['base_price']:,.2f}")
        
        return base_config
    
    def validate_data_quality(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Validate crypto data quality and completeness.
        
        Args:
            data: Crypto OHLCV data
            
        Returns:
            Data quality assessment
        """
        quality_report = {
            'total_rows': len(data),
            'missing_values': data.isnull().sum().to_dict(),
            'date_range': {
                'start': data.index[0] if len(data) > 0 else None,
                'end': data.index[-1] if len(data) > 0 else None,
                'duration_hours': len(data) if len(data) > 0 else 0
            },
            'data_issues': [],
            'quality_score': 0.0
        }
        
        # Check for data issues
        if len(data) == 0:
            quality_report['data_issues'].append("Empty dataset")
            return quality_report
        
        # Check for negative prices (impossible for crypto)
        price_columns = ['Open', 'High', 'Low', 'Close']
        for col in price_columns:
            if col in data.columns and (data[col] <= 0).any():
                quality_report['data_issues'].append(f"Negative or zero prices in {col}")
        
        # Check for negative volume (impossible)
        if 'Volume' in data.columns and (data['Volume'] < 0).any():
            quality_report['data_issues'].append("Negative volume detected")
        
        # Check OHLC consistency
        if all(col in data.columns for col in price_columns):
            inconsistent = (data['High'] < data['Low']) | (data['High'] < data['Open']) | \
                          (data['High'] < data['Close']) | (data['Low'] > data['Open']) | \
                          (data['Low'] > data['Close'])
            if inconsistent.any():
                quality_report['data_issues'].append(f"OHLC inconsistencies: {inconsistent.sum()} rows")
        
        # Calculate quality score
        total_possible_issues = 5
        actual_issues = len(quality_report['data_issues'])
        quality_report['quality_score'] = max(0, (total_possible_issues - actual_issues) / total_possible_issues)
        
        # Add completeness metrics
        if len(data) > 0:
            quality_report['completeness'] = 1 - (data.isnull().sum().sum() / (len(data) * len(data.columns)))
        
        print(f"📋 Data quality report: {quality_report['quality_score']:.2f} score, {len(quality_report['data_issues'])} issues")
        
        return quality_report


# Helper functions for crypto-specific operations

def get_crypto_trading_calendar(start_date: datetime, end_date: datetime) -> pd.DatetimeIndex:
    """
    Get crypto trading calendar (24/7 trading).
    
    Args:
        start_date: Start date
        end_date: End date
        
    Returns:
        DatetimeIndex for crypto trading (continuous)
    """
    # Crypto trades 24/7, so just return hourly index
    return pd.date_range(start=start_date, end=end_date, freq='H')


def calculate_crypto_metrics(data: pd.DataFrame) -> Dict[str, float]:
    """
    Calculate crypto-specific performance metrics.
    
    Args:
        data: Crypto OHLCV data
        
    Returns:
        Dictionary of crypto-specific metrics
    """
    if 'Close' not in data.columns or len(data) == 0:
        return {}
    
    prices = data['Close']
    returns = prices.pct_change().dropna()
    
    metrics = {
        'total_return': (prices.iloc[-1] / prices.iloc[0] - 1) * 100,
        'annualized_volatility': returns.std() * np.sqrt(8760) * 100,  # 24/7 trading
        'max_drawdown': ((prices / prices.expanding().max() - 1).min()) * 100,
        'sharpe_ratio': returns.mean() / returns.std() * np.sqrt(8760) if returns.std() > 0 else 0,
        'sortino_ratio': returns.mean() / returns[returns < 0].std() * np.sqrt(8760) if len(returns[returns < 0]) > 0 else 0
    }
    
    # Crypto-specific metrics
    if 'Volume' in data.columns:
        volume = data['Volume']
        metrics['avg_volume'] = volume.mean()
        metrics['volume_volatility'] = volume.std() / volume.mean() if volume.mean() > 0 else 0
    
    return metrics


# Legacy compatibility - preserve original class name for compatibility
CryptoAssetAdapter = CryptoAdapter
