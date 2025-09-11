#!/usr/bin/env python3
"""
Forex Asset Processor

Processes forex currency pair data from Yahoo Finance raw data into
standardized bronze layer datasets with technical indicators and features.
"""

import os
import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import glob

class ForexAssetProcessor:
    """
    Processes forex currency pairs with technical indicators.
    """
    
    FOREX_ASSETS = ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
    
    # Major currency pairs characteristics
    CURRENCY_PAIRS = {
        'EURUSD': {'base': 'EUR', 'quote': 'USD', 'major': True, 'decimal_places': 5},
        'USDJPY': {'base': 'USD', 'quote': 'JPY', 'major': True, 'decimal_places': 3},
        'GBPUSD': {'base': 'GBP', 'quote': 'USD', 'major': True, 'decimal_places': 5},
        'AUDUSD': {'base': 'AUD', 'quote': 'USD', 'major': True, 'decimal_places': 5},
        'USDCAD': {'base': 'USD', 'quote': 'CAD', 'major': True, 'decimal_places': 5},
        'USDCHF': {'base': 'USD', 'quote': 'CHF', 'major': True, 'decimal_places': 5},
        'NZDUSD': {'base': 'NZD', 'quote': 'USD', 'major': True, 'decimal_places': 5}
    }
    
    def __init__(self, raw_data_path: str, output_path: str):
        """
        Initialize forex asset processor.
        
        Args:
            raw_data_path: Path to raw Yahoo Finance data
            output_path: Path for processed output files
        """
        self.raw_data_path = raw_data_path
        self.output_path = output_path
        self.forex_path = os.path.join(raw_data_path, 'forex')
        self.forex_output_path = os.path.join(output_path, 'forex')
        
        # Create output directory
        os.makedirs(self.forex_output_path, exist_ok=True)
        
        self.logger = logging.getLogger(f'{__name__}.ForexAssetProcessor')
        
        self.logger.info(f"Initialized ForexAssetProcessor")
        self.logger.info(f"Raw data path: {self.forex_path}")
        self.logger.info(f"Output path: {self.forex_output_path}")
    
    def process(self, intervals: List[str], assets: Optional[List[str]] = None) -> Dict:
        """
        Process forex assets for specified intervals.
        
        Args:
            intervals: List of intervals to process (1m, 1h, 1d)
            assets: List of specific assets to process (None = all)
            
        Returns:
            Dictionary with processing results
        """
        if assets is None:
            assets = self.FOREX_ASSETS
        else:
            # Filter to supported assets
            assets = [a for a in assets if a in self.FOREX_ASSETS]
        
        if not assets:
            self.logger.warning("No valid forex assets specified")
            return {}
        
        results = {}
        
        self.logger.info(f"Processing forex assets: {assets}")
        self.logger.info(f"Processing intervals: {intervals}")
        
        for interval in intervals:
            results[interval] = {}
            
            for asset in assets:
                asset_result = self._process_asset_interval(asset, interval)
                if asset_result:
                    results[interval][asset] = asset_result
                    self.logger.info(f"✅ Processed {asset} {interval}: {asset_result['records']} records")
                else:
                    self.logger.warning(f"⚠️  No data processed for {asset} {interval}")
        
        return results
    
    def _process_asset_interval(self, asset: str, interval: str) -> Optional[Dict]:
        """
        Process a single forex pair for a specific interval.
        
        Args:
            asset: Forex pair code (EURUSD, etc.)
            interval: Time interval (1m, 1h, 1d)
            
        Returns:
            Processing result dictionary or None if failed
        """
        self.logger.debug(f"Processing {asset} {interval}")
        
        # Find latest data file
        asset_interval_path = os.path.join(self.forex_path, asset, interval)
        latest_file = os.path.join(asset_interval_path, 'latest.csv')
        
        if not os.path.exists(latest_file):
            self.logger.warning(f"No latest.csv found for {asset} {interval}: {latest_file}")
            return None
        
        try:
            # Load raw data
            df = pd.read_csv(latest_file, index_col=0, parse_dates=True)
            
            if df.empty:
                self.logger.warning(f"Empty data file for {asset} {interval}")
                return None
            
            # Process the data
            processed_df = self._add_technical_indicators(df, asset, interval)
            
            # Generate output filename
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            output_filename = f"{asset}_bronze_{interval}_{timestamp}.csv"
            output_path = os.path.join(self.forex_output_path, output_filename)
            
            # Save processed data
            processed_df.to_csv(output_path)
            
            # Also save as latest
            latest_output_path = os.path.join(self.forex_output_path, f"{asset}_bronze_{interval}_latest.csv")
            processed_df.to_csv(latest_output_path)
            
            # Generate processing statistics
            stats = self._generate_statistics(processed_df, asset, interval)
            
            self.logger.info(f"💾 Saved {asset} {interval} processed data: {output_path}")
            
            return {
                'asset': asset,
                'interval': interval,
                'records': len(processed_df),
                'output_file': output_path,
                'latest_file': latest_output_path,
                'date_range': f"{processed_df.index.min()} to {processed_df.index.max()}",
                'statistics': stats
            }
            
        except Exception as e:
            self.logger.error(f"Error processing {asset} {interval}: {e}")
            return None
    
    def _add_technical_indicators(self, df: pd.DataFrame, asset: str, interval: str) -> pd.DataFrame:
        """
        Add technical indicators to the raw OHLCV data with forex-specific features.
        
        Args:
            df: Raw OHLCV DataFrame
            asset: Forex pair code
            interval: Time interval
            
        Returns:
            DataFrame with added technical indicators
        """
        processed_df = df.copy()
        
        # Ensure we have required columns (lowercase from unified collector)
        required_columns = ['open', 'high', 'low', 'close', 'volume']
        missing_columns = [col for col in required_columns if col not in processed_df.columns]
        
        if missing_columns:
            self.logger.error(f"Missing required columns for {asset}: {missing_columns}")
            return processed_df
        
        try:
            # 1. Basic Price Indicators
            processed_df['price_change'] = processed_df['close'].pct_change()
            processed_df['price_change_abs'] = processed_df['close'].diff()
            processed_df['hl_range'] = processed_df['high'] - processed_df['low']
            processed_df['oc_range'] = abs(processed_df['close'] - processed_df['open'])
            
            # 2. Forex-specific pip calculation
            pair_info = self.CURRENCY_PAIRS.get(asset, {'decimal_places': 5})
            decimal_places = pair_info['decimal_places']
            pip_multiplier = 10 ** (decimal_places - 1)  # Standard pip calculation
            
            processed_df['pips_change'] = processed_df['price_change_abs'] * pip_multiplier
            processed_df['pips_range'] = processed_df['hl_range'] * pip_multiplier
            
            # 3. Moving Averages (adaptive to data size)
            max_records = len(processed_df)
            
            # Forex-standard MA periods (adapted for data size)
            ma_short = min(21, max_records // 10) if max_records >= 42 else 5   # ~1 month daily
            ma_medium = min(50, max_records // 5) if max_records >= 100 else 10  # ~2.5 months daily  
            ma_long = min(200, max_records // 3) if max_records >= 400 else 20   # ~10 months daily
            
            if ma_short > 0:
                processed_df[f'ma_{ma_short}'] = processed_df['close'].rolling(window=ma_short).mean()
            if ma_medium > 0:
                processed_df[f'ma_{ma_medium}'] = processed_df['close'].rolling(window=ma_medium).mean()
            if ma_long > 0:
                processed_df[f'ma_{ma_long}'] = processed_df['close'].rolling(window=ma_long).mean()
            
            # 4. Volatility Indicators (forex-focused)
            vol_window = min(20, max_records // 10) if max_records >= 40 else 7
            if vol_window > 0:
                processed_df[f'volatility_{vol_window}'] = processed_df['price_change'].rolling(window=vol_window).std()
                processed_df[f'volatility_annualized'] = processed_df[f'volatility_{vol_window}'] * np.sqrt(
                    252 if interval == '1d' else (252 * 24 if interval == '1h' else 252 * 24 * 60)
                )  # 252 trading days for forex
                
                # Average True Range (ATR) - important for forex
                processed_df = self._calculate_atr(processed_df, period=vol_window)
            
            # 5. Momentum Indicators
            if max_records >= 14:
                # RSI (Relative Strength Index)
                processed_df = self._calculate_rsi(processed_df, period=min(14, max_records // 4))
                
                # Stochastic Oscillator (popular in forex)
                if max_records >= 14:
                    processed_df = self._calculate_stochastic(processed_df, period=min(14, max_records // 4))
            
            # 6. Trend Indicators
            if max_records >= 26:
                # MACD (Moving Average Convergence Divergence)
                fast_period = min(12, max_records // 8)
                slow_period = min(26, max_records // 4)
                signal_period = min(9, max_records // 10)
                processed_df = self._calculate_macd(processed_df, fast_period, slow_period, signal_period)
            
            # 7. Support/Resistance Levels
            if max_records >= 20:
                high_period = min(50, max_records // 3)
                low_period = min(50, max_records // 3)
                processed_df[f'resistance_{high_period}'] = processed_df['high'].rolling(window=high_period).max()
                processed_df[f'support_{low_period}'] = processed_df['low'].rolling(window=low_period).min()
                
                # Distance to support/resistance
                processed_df['distance_to_resistance'] = processed_df[f'resistance_{high_period}'] - processed_df['close']
                processed_df['distance_to_support'] = processed_df['close'] - processed_df[f'support_{low_period}']
                
                # Price position within range
                range_size = processed_df[f'resistance_{high_period}'] - processed_df[f'support_{low_period}']
                processed_df['price_position'] = (processed_df['close'] - processed_df[f'support_{low_period}']) / range_size
            
            # 8. Volume Analysis (forex volume is indicative, not actual)
            if processed_df['volume'].sum() > 0:
                processed_df['volume_change'] = processed_df['volume'].pct_change()
                if max_records >= 20:
                    vol_ma_period = min(20, max_records // 3)
                    processed_df[f'volume_ma_{vol_ma_period}'] = processed_df['volume'].rolling(window=vol_ma_period).mean()
                    processed_df['volume_ratio'] = processed_df['volume'] / processed_df[f'volume_ma_{vol_ma_period}']
            
            # 9. Time-based Features (important for forex trading sessions)
            processed_df['hour'] = processed_df.index.hour
            processed_df['day_of_week'] = processed_df.index.dayofweek
            processed_df['day_of_month'] = processed_df.index.day
            processed_df['month'] = processed_df.index.month
            
            # Forex trading session identification (UTC assumed)
            processed_df['trading_session'] = processed_df.apply(self._get_trading_session, axis=1)
            
            # 10. Currency-specific Features
            processed_df['base_currency'] = pair_info.get('base', asset[:3])
            processed_df['quote_currency'] = pair_info.get('quote', asset[3:])
            processed_df['is_usd_base'] = pair_info.get('base') == 'USD'
            processed_df['is_usd_quote'] = pair_info.get('quote') == 'USD'
            processed_df['decimal_places'] = decimal_places
            
            # 11. Asset and Interval Metadata
            processed_df['asset'] = asset
            processed_df['interval'] = interval
            processed_df['processing_timestamp'] = datetime.now()
            
            self.logger.debug(f"Added technical indicators for {asset} {interval}: {len(processed_df.columns)} total columns")
            
            return processed_df
            
        except Exception as e:
            self.logger.error(f"Error adding technical indicators for {asset} {interval}: {e}")
            return processed_df
    
    def _calculate_rsi(self, df: pd.DataFrame, period: int = 14) -> pd.DataFrame:
        """Calculate Relative Strength Index (RSI)."""
        delta = df['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        return df
    
    def _calculate_atr(self, df: pd.DataFrame, period: int = 14) -> pd.DataFrame:
        """Calculate Average True Range (ATR)."""
        high_low = df['high'] - df['low']
        high_close_prev = abs(df['high'] - df['close'].shift(1))
        low_close_prev = abs(df['low'] - df['close'].shift(1))
        
        true_range = pd.concat([high_low, high_close_prev, low_close_prev], axis=1).max(axis=1)
        df['atr'] = true_range.rolling(window=period).mean()
        
        return df
    
    def _calculate_stochastic(self, df: pd.DataFrame, period: int = 14, smooth_k: int = 3) -> pd.DataFrame:
        """Calculate Stochastic Oscillator."""
        lowest_low = df['low'].rolling(window=period).min()
        highest_high = df['high'].rolling(window=period).max()
        
        k_percent = 100 * (df['close'] - lowest_low) / (highest_high - lowest_low)
        df['stoch_k'] = k_percent.rolling(window=smooth_k).mean()
        df['stoch_d'] = df['stoch_k'].rolling(window=3).mean()
        
        return df
    
    def _calculate_macd(self, df: pd.DataFrame, fast_period: int = 12, slow_period: int = 26, signal_period: int = 9) -> pd.DataFrame:
        """Calculate MACD (Moving Average Convergence Divergence)."""
        ema_fast = df['close'].ewm(span=fast_period).mean()
        ema_slow = df['close'].ewm(span=slow_period).mean()
        
        df['macd'] = ema_fast - ema_slow
        df['macd_signal'] = df['macd'].ewm(span=signal_period).mean()
        df['macd_histogram'] = df['macd'] - df['macd_signal']
        
        return df
    
    def _get_trading_session(self, row) -> str:
        """Identify forex trading session based on UTC hour."""
        hour = row['hour']
        
        # Trading sessions in UTC
        if 21 <= hour or hour < 3:
            return 'Sydney'  # 21:00-05:00 UTC
        elif 0 <= hour < 9:
            return 'Tokyo'   # 00:00-09:00 UTC  
        elif 7 <= hour < 16:
            return 'London'  # 07:00-16:00 UTC
        elif 13 <= hour < 22:
            return 'NewYork' # 13:00-22:00 UTC
        else:
            return 'Overlap' # Session overlaps
    
    def _generate_statistics(self, df: pd.DataFrame, asset: str, interval: str) -> Dict:
        """Generate processing statistics with forex-specific metrics."""
        try:
            pair_info = self.CURRENCY_PAIRS.get(asset, {})
            
            return {
                'total_records': len(df),
                'date_range': {
                    'start': df.index.min().isoformat() if not df.empty else None,
                    'end': df.index.max().isoformat() if not df.empty else None
                },
                'price_stats': {
                    'min_close': float(df['close'].min()) if 'close' in df.columns else None,
                    'max_close': float(df['close'].max()) if 'close' in df.columns else None,
                    'mean_close': float(df['close'].mean()) if 'close' in df.columns else None,
                    'std_close': float(df['close'].std()) if 'close' in df.columns else None
                },
                'pip_stats': {
                    'total_pip_movement': float(df['pips_change'].abs().sum()) if 'pips_change' in df.columns else None,
                    'avg_pip_movement': float(df['pips_change'].abs().mean()) if 'pips_change' in df.columns else None,
                    'max_pip_range': float(df['pips_range'].max()) if 'pips_range' in df.columns else None
                },
                'volatility_stats': {
                    'atr_available': 'atr' in df.columns,
                    'mean_atr': float(df['atr'].mean()) if 'atr' in df.columns else None,
                    'annualized_volatility': float(df['volatility_annualized'].mean()) if 'volatility_annualized' in df.columns else None
                },
                'technical_indicators': {
                    'rsi_available': 'rsi' in df.columns,
                    'stochastic_available': 'stoch_k' in df.columns,
                    'macd_available': 'macd' in df.columns,
                    'atr_available': 'atr' in df.columns,
                    'moving_averages': len([col for col in df.columns if col.startswith('ma_')]),
                    'support_resistance': 'price_position' in df.columns
                },
                'currency_pair_info': pair_info,
                'trading_sessions': df['trading_session'].value_counts().to_dict() if 'trading_session' in df.columns else {},
                'columns_total': len(df.columns),
                'processing_timestamp': datetime.now().isoformat()
            }
        except Exception as e:
            self.logger.error(f"Error generating statistics: {e}")
            return {'error': str(e)}
