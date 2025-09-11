#!/usr/bin/env python3
"""
Crypto Asset Processor

Processes cryptocurrency data (ETH, BTC) from Yahoo Finance raw data into
standardized bronze layer datasets with technical indicators and features.
"""

import os
import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import glob

class CryptoAssetProcessor:
    """
    Processes cryptocurrency assets (ETH, BTC) with technical indicators.
    """
    
    CRYPTO_ASSETS = ['ETH', 'BTC']
    
    def __init__(self, raw_data_path: str, output_path: str):
        """
        Initialize crypto asset processor.
        
        Args:
            raw_data_path: Path to raw Yahoo Finance data
            output_path: Path for processed output files
        """
        self.raw_data_path = raw_data_path
        self.output_path = output_path
        self.crypto_path = os.path.join(raw_data_path, 'crypto')
        self.crypto_output_path = os.path.join(output_path, 'crypto')
        
        # Create output directory
        os.makedirs(self.crypto_output_path, exist_ok=True)
        
        self.logger = logging.getLogger(f'{__name__}.CryptoAssetProcessor')
        
        self.logger.info(f"Initialized CryptoAssetProcessor")
        self.logger.info(f"Raw data path: {self.crypto_path}")
        self.logger.info(f"Output path: {self.crypto_output_path}")
    
    def process(self, intervals: List[str], assets: Optional[List[str]] = None) -> Dict:
        """
        Process crypto assets for specified intervals.
        
        Args:
            intervals: List of intervals to process (1m, 1h, 1d)
            assets: List of specific assets to process (None = all)
            
        Returns:
            Dictionary with processing results
        """
        if assets is None:
            assets = self.CRYPTO_ASSETS
        else:
            # Filter to supported assets
            assets = [a for a in assets if a in self.CRYPTO_ASSETS]
        
        if not assets:
            self.logger.warning("No valid crypto assets specified")
            return {}
        
        results = {}
        
        self.logger.info(f"Processing crypto assets: {assets}")
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
        Process a single asset for a specific interval.
        
        Args:
            asset: Asset code (ETH, BTC)
            interval: Time interval (1m, 1h, 1d)
            
        Returns:
            Processing result dictionary or None if failed
        """
        self.logger.debug(f"Processing {asset} {interval}")
        
        # Find latest data file
        asset_interval_path = os.path.join(self.crypto_path, asset, interval)
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
            output_path = os.path.join(self.crypto_output_path, output_filename)
            
            # Save processed data
            processed_df.to_csv(output_path)
            
            # Also save as latest
            latest_output_path = os.path.join(self.crypto_output_path, f"{asset}_bronze_{interval}_latest.csv")
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
        Add technical indicators to the raw OHLCV data.
        
        Args:
            df: Raw OHLCV DataFrame
            asset: Asset code
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
            
            # 2. Moving Averages (adaptive to data size)
            max_records = len(processed_df)
            
            # Short-term MA (adaptive)
            ma_short = min(10, max_records // 10) if max_records >= 20 else 3
            ma_medium = min(20, max_records // 5) if max_records >= 40 else 7
            ma_long = min(50, max_records // 3) if max_records >= 100 else 15
            
            if ma_short > 0:
                processed_df[f'ma_{ma_short}'] = processed_df['close'].rolling(window=ma_short).mean()
            if ma_medium > 0:
                processed_df[f'ma_{ma_medium}'] = processed_df['close'].rolling(window=ma_medium).mean()
            if ma_long > 0:
                processed_df[f'ma_{ma_long}'] = processed_df['close'].rolling(window=ma_long).mean()
            
            # 3. Volatility Indicators
            vol_window = min(14, max_records // 10) if max_records >= 28 else 5
            if vol_window > 0:
                processed_df[f'volatility_{vol_window}'] = processed_df['price_change'].rolling(window=vol_window).std()
                processed_df[f'volatility_annualized'] = processed_df[f'volatility_{vol_window}'] * np.sqrt(
                    365 if interval == '1d' else (365 * 24 if interval == '1h' else 365 * 24 * 60)
                )
            
            # 4. Momentum Indicators
            if max_records >= 14:
                # RSI (Relative Strength Index)
                processed_df = self._calculate_rsi(processed_df, period=min(14, max_records // 4))
            
            # 5. Volume Indicators (if volume available)
            if processed_df['volume'].sum() > 0:
                processed_df['volume_change'] = processed_df['volume'].pct_change()
                if max_records >= 20:
                    vol_ma_period = min(20, max_records // 3)
                    processed_df[f'volume_ma_{vol_ma_period}'] = processed_df['volume'].rolling(window=vol_ma_period).mean()
                    processed_df['volume_ratio'] = processed_df['volume'] / processed_df[f'volume_ma_{vol_ma_period}']
            
            # 6. Price Position Indicators
            if max_records >= 20:
                high_period = min(20, max_records // 3)
                low_period = min(20, max_records // 3)
                processed_df[f'high_{high_period}'] = processed_df['high'].rolling(window=high_period).max()
                processed_df[f'low_{low_period}'] = processed_df['low'].rolling(window=low_period).min()
                processed_df[f'price_position'] = (processed_df['close'] - processed_df[f'low_{low_period}']) / \
                                                 (processed_df[f'high_{high_period}'] - processed_df[f'low_{low_period}'])
            
            # 7. Time-based Features
            processed_df['hour'] = processed_df.index.hour
            processed_df['day_of_week'] = processed_df.index.dayofweek
            processed_df['day_of_month'] = processed_df.index.day
            processed_df['month'] = processed_df.index.month
            
            # 8. Asset and Interval Metadata
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
    
    def _generate_statistics(self, df: pd.DataFrame, asset: str, interval: str) -> Dict:
        """Generate processing statistics."""
        try:
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
                'volume_stats': {
                    'total_volume': float(df['volume'].sum()) if 'volume' in df.columns else None,
                    'mean_volume': float(df['volume'].mean()) if 'volume' in df.columns else None
                },
                'technical_indicators': {
                    'rsi_available': 'rsi' in df.columns,
                    'moving_averages': len([col for col in df.columns if col.startswith('ma_')]),
                    'volatility_calculated': any(col.startswith('volatility_') for col in df.columns)
                },
                'columns_total': len(df.columns),
                'processing_timestamp': datetime.now().isoformat()
            }
        except Exception as e:
            self.logger.error(f"Error generating statistics: {e}")
            return {'error': str(e)}
