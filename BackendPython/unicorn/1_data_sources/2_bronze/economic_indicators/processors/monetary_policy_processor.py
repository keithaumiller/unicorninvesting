#!/usr/bin/env python3
"""
Monetary Policy Processor

Processes monetary policy indicators from FRED sources including:
- Federal Funds Rate
- Money Supply (M1, M2)
- Treasury Yield Curves
- Interest Rate Spreads
- Quantitative Easing indicators

Transforms raw monetary policy data into standardized features for XGBoost alpha models
at 1-minute, 1-hour, and 1-day trading intervals.
"""

import os
import pandas as pd
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional
from .base_processor import BaseEconomicProcessor

class MonetaryPolicyProcessor(BaseEconomicProcessor):
    """
    Processor for Federal Reserve monetary policy indicators.
    
    Handles interest rates, money supply, and yield curve data from FRED to create
    standardized features for crypto trading alpha models.
    """
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.category = "monetary_policy"
        self.logger.info("Initialized Monetary Policy Processor")
    
    def process_fred_interest_rates(self) -> pd.DataFrame:
        """
        Process FRED interest rate data from collected CSV files.
        
        Returns:
            DataFrame with processed interest rate indicators
        """
        # Load FRED data files
        fred_data = self.load_raw_data('fred')
        
        if not fred_data:
            self.logger.warning("No FRED data files found")
            return pd.DataFrame()
        
        interest_data = pd.DataFrame()
        
        # Process Federal Funds Rate and other interest rate files
        for filename, df in fred_data.items():
            if any(keyword in filename.lower() for keyword in ['fed', 'rate', 'yield', 'treasury']):
                self.logger.info(f"Processing interest rate data from {filename}")
                
                # Standardize timestamps
                df_processed = self.standardize_timestamps(df)
                
                if df_processed.empty:
                    continue
                
                # Clean and process interest rate data
                numeric_columns = df_processed.select_dtypes(include=[np.number]).columns
                
                for col in numeric_columns:
                    # Create meaningful column names based on file content
                    if 'fed' in filename.lower() or 'ffr' in filename.lower():
                        new_col_name = f"fed_funds_rate_{col.lower()}"
                    elif 'treasury' in filename.lower() or 'yield' in filename.lower():
                        new_col_name = f"treasury_yield_{col.lower()}"
                    else:
                        new_col_name = f"interest_rate_{col.lower()}"
                    
                    interest_data[new_col_name] = df_processed[col]
                
                # Preserve the datetime index
                if interest_data.empty:
                    interest_data.index = df_processed.index
        
        # Add derived interest rate features
        if not interest_data.empty:
            interest_data = self._add_interest_rate_features(interest_data)
        
        return interest_data
    
    def _add_interest_rate_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Add derived features for monetary policy analysis.
        
        Args:
            df: DataFrame with basic interest rate data
            
        Returns:
            DataFrame with additional monetary policy features
        """
        df_features = df.copy()
        
        # Find Federal Funds Rate column
        fed_rate_col = None
        for col in df.columns:
            if 'fed_funds_rate' in col.lower():
                fed_rate_col = col
                break
        
        if fed_rate_col is not None and df[fed_rate_col].notna().any():
            # Fed Rate Change (basis points)
            df_features['fed_rate_change_bp'] = df[fed_rate_col].diff() * 100
            
            # Fed Rate Direction (1 = rising, 0 = falling/unchanged)
            df_features['fed_rate_direction'] = np.where(df_features['fed_rate_change_bp'] > 0, 1, 0)
            
            # Fed Rate Trend (6-month moving average)
            df_features['fed_rate_trend_6m'] = df[fed_rate_col].rolling(window=126, min_periods=20).mean()  # ~6 months daily
            
            # Fed Rate Cycle Position (relative to recent range)
            rolling_min = df[fed_rate_col].rolling(window=252, min_periods=50).min()  # 1 year lookback
            rolling_max = df[fed_rate_col].rolling(window=252, min_periods=50).max()
            df_features['fed_rate_cycle_position'] = (
                (df[fed_rate_col] - rolling_min) / (rolling_max - rolling_min) * 100
            ).fillna(50)
            
            # Fed Rate Volatility (standard deviation of changes)
            df_features['fed_rate_volatility'] = df_features['fed_rate_change_bp'].rolling(
                window=60, min_periods=10).std()
            
            # Fed Rate Real vs Nominal (would need inflation data)
            # Placeholder for future inflation adjustment
            df_features['fed_rate_real_proxy'] = df[fed_rate_col] - 2.0  # Assuming 2% inflation target
            
            self.logger.info("Added Federal Funds Rate-specific features")
        
        # Process Treasury yield data if available
        treasury_cols = [col for col in df.columns if 'treasury_yield' in col.lower()]
        if treasury_cols:
            df_features = self._add_yield_curve_features(df_features, treasury_cols)
        
        return df_features
    
    def _add_yield_curve_features(self, df: pd.DataFrame, treasury_cols: List[str]) -> pd.DataFrame:
        """
        Add yield curve-specific features.
        
        Args:
            df: DataFrame with existing features
            treasury_cols: List of treasury yield column names
            
        Returns:
            DataFrame with yield curve features
        """
        df_features = df.copy()
        
        # If we have multiple treasury yields, calculate spreads
        if len(treasury_cols) >= 2:
            # Sort columns to identify short and long term rates
            sorted_cols = sorted(treasury_cols)
            
            # Calculate yield spread (long - short term)
            if len(sorted_cols) >= 2:
                short_col = sorted_cols[0]  # Assuming first is shortest maturity
                long_col = sorted_cols[-1]   # Assuming last is longest maturity
                
                df_features['yield_spread'] = df[long_col] - df[short_col]
                
                # Yield curve slope indicator
                df_features['yield_curve_slope'] = np.where(df_features['yield_spread'] > 0, 1, 0)
                
                # Yield spread momentum
                df_features['yield_spread_momentum'] = df_features['yield_spread'].diff()
                
                self.logger.info("Added yield curve spread features")
        
        return df_features
    
    def process_money_supply_data(self) -> pd.DataFrame:
        """
        Process money supply data from FRED (M1, M2, etc.).
        
        Returns:
            DataFrame with processed money supply indicators
        """
        # Load FRED data files
        fred_data = self.load_raw_data('fred')
        
        money_supply_data = pd.DataFrame()
        
        # Look for money supply files
        for filename, df in fred_data.items():
            if any(keyword in filename.lower() for keyword in ['m1', 'm2', 'money', 'supply']):
                self.logger.info(f"Processing money supply data from {filename}")
                
                # Standardize timestamps
                df_processed = self.standardize_timestamps(df)
                
                if df_processed.empty:
                    continue
                
                # Process money supply data
                numeric_columns = df_processed.select_dtypes(include=[np.number]).columns
                
                for col in numeric_columns:
                    if 'm1' in filename.lower():
                        new_col_name = f"m1_money_supply_{col.lower()}"
                    elif 'm2' in filename.lower():
                        new_col_name = f"m2_money_supply_{col.lower()}"
                    else:
                        new_col_name = f"money_supply_{col.lower()}"
                    
                    money_supply_data[new_col_name] = df_processed[col]
                
                if money_supply_data.empty:
                    money_supply_data.index = df_processed.index
        
        # Add money supply growth features
        if not money_supply_data.empty:
            money_supply_data = self._add_money_supply_features(money_supply_data)
        
        return money_supply_data
    
    def _add_money_supply_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Add money supply growth and trend features.
        
        Args:
            df: DataFrame with basic money supply data
            
        Returns:
            DataFrame with additional money supply features
        """
        df_features = df.copy()
        
        for col in df.columns:
            if 'money_supply' in col.lower() and df[col].notna().any():
                # Money supply growth rate (year-over-year)
                df_features[f"{col}_growth_yoy"] = df[col].pct_change(252) * 100  # ~1 year daily
                
                # Money supply growth rate (month-over-month)
                df_features[f"{col}_growth_mom"] = df[col].pct_change(21) * 100  # ~1 month daily
                
                # Money supply trend
                df_features[f"{col}_trend"] = df[col].rolling(window=63, min_periods=20).mean()  # ~3 months
                
                # Money supply acceleration
                df_features[f"{col}_acceleration"] = df_features[f"{col}_growth_mom"].diff()
        
        return df_features
    
    def process(self, intervals: List[str] = None) -> Dict[str, pd.DataFrame]:
        """
        Main processing method for monetary policy indicators.
        
        Args:
            intervals: List of intervals to process ('1_minute', '1_hour', '1_day')
            
        Returns:
            Dictionary of processed DataFrames keyed by interval
        """
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
        
        self.logger.info("Starting monetary policy data processing")
        
        # Process interest rate data from FRED
        interest_data = self.process_fred_interest_rates()
        
        # Process money supply data
        money_supply_data = self.process_money_supply_data()
        
        # Combine monetary policy data
        combined_data = pd.DataFrame()
        
        if not interest_data.empty:
            combined_data = interest_data
            
        if not money_supply_data.empty:
            if combined_data.empty:
                combined_data = money_supply_data
            else:
                # Align indexes and combine
                combined_data = pd.concat([combined_data, money_supply_data], axis=1, join='outer')
        
        if combined_data.empty:
            self.logger.warning("No monetary policy data available for processing")
            return {}
        
        # Validate data quality
        is_valid, issues = self.validate_data(combined_data)
        if not is_valid:
            self.logger.warning(f"Data validation issues: {issues}")
        
        # Calculate additional features
        feature_config = {
            'lags': [1, 5, 21, 63],  # 1 day, 1 week, 1 month, 3 months for daily data
            'differences': [1, 5, 21, 63],  # Various change periods
            'rolling_stats': [5, 21, 63, 126],  # 1 week to 6 months rolling windows
            'momentum': True,
            'volatility': True
        }
        
        monetary_features = self.calculate_features(combined_data, feature_config)
        
        # Resample to different time intervals
        resampled_data = self.resample_to_intervals(monetary_features, intervals)
        
        # Save processed data
        self.save_processed_data(resampled_data, "monetary_policy")
        
        # Log processing results
        for interval, df in resampled_data.items():
            self.logger.info(f"Monetary policy data - {interval}: {df.shape[0]} observations, {df.shape[1]} features")
        
        return resampled_data
    
    def get_current_fed_rate(self) -> Optional[float]:
        """
        Get the current Federal Funds Rate.
        
        Returns:
            Current Fed rate or None if not available
        """
        interest_data = self.process_fred_interest_rates()
        
        if interest_data.empty:
            return None
        
        # Find Fed funds rate column
        for col in interest_data.columns:
            if 'fed_funds_rate' in col.lower():
                current_rate = interest_data[col].dropna().iloc[-1]
                self.logger.info(f"Current Fed Funds Rate: {current_rate:.2f}%")
                return float(current_rate)
        
        return None
    
    def get_yield_curve_status(self) -> Optional[str]:
        """
        Get current yield curve status (normal/inverted/flat).
        
        Returns:
            Yield curve status or None if not available
        """
        interest_data = self.process_fred_interest_rates()
        
        if interest_data.empty:
            return None
        
        for col in interest_data.columns:
            if 'yield_spread' in col:
                latest_spread = interest_data[col].dropna().iloc[-1]
                if latest_spread > 0.5:
                    status = "normal"
                elif latest_spread < -0.5:
                    status = "inverted"
                else:
                    status = "flat"
                
                self.logger.info(f"Current yield curve status: {status} (spread: {latest_spread:.2f}bp)")
                return status
        
        return None
