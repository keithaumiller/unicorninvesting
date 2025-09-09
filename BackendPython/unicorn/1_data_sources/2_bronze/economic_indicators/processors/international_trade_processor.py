#!/usr/bin/env python3
"""
International Trade Processor

Processes international trade indicators from BEA and FRED sources including:
- Trade in Services Balance
- Current Account Balance
- Export/Import flows
- Exchange rate impacts

Transforms raw economic data into standardized features for XGBoost alpha models
at 1-minute, 1-hour, and 1-day trading intervals.
"""

import os
import pandas as pd
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional
from .base_processor import BaseEconomicProcessor

class InternationalTradeProcessor(BaseEconomicProcessor):
    """
    Processor for international trade and balance of payments indicators.
    
    Handles trade balance data from BEA and exchange rate impacts to create
    standardized features for crypto trading alpha models.
    """
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.category = "international_trade"
        self.logger.info("Initialized International Trade Processor")
    
    def process_bea_trade_data(self) -> pd.DataFrame:
        """
        Process BEA international trade data from collected CSV files.
        
        Returns:
            DataFrame with processed trade balance indicators
        """
        # Load BEA data files
        bea_data = self.load_raw_data('bea')
        
        if not bea_data:
            self.logger.warning("No BEA data files found")
            return pd.DataFrame()
        
        trade_data = pd.DataFrame()
        
        # Process trade balance files
        for filename, df in bea_data.items():
            if 'international_trade' in filename.lower() or 'trade' in filename.lower():
                self.logger.info(f"Processing trade data from {filename}")
                
                # Standardize timestamps
                df_processed = self.standardize_timestamps(df)
                
                if df_processed.empty:
                    continue
                
                # Clean and process trade data
                numeric_columns = df_processed.select_dtypes(include=[np.number]).columns
                string_columns = df_processed.select_dtypes(include=['object']).columns
                
                for col in numeric_columns:
                    # Create meaningful column names
                    if 'DataValue' in col:
                        new_col_name = f"trade_balance_{col.lower()}"
                    else:
                        new_col_name = f"trade_{col.lower()}"
                    
                    # Add to combined dataset
                    trade_data[new_col_name] = df_processed[col]
                
                # Handle non-numeric columns if they contain trade information
                for col in string_columns:
                    if col != 'Date':  # Skip date column
                        # Convert string data to categorical if meaningful
                        unique_values = df_processed[col].nunique()
                        if unique_values < 20:  # Reasonable number for categories
                            trade_data[f"trade_category_{col.lower()}"] = pd.Categorical(df_processed[col]).codes
                
                # Preserve the datetime index
                if trade_data.empty:
                    trade_data.index = df_processed.index
        
        # Add derived trade balance features
        if not trade_data.empty:
            trade_data = self._add_trade_features(trade_data)
        
        return trade_data
    
    def _add_trade_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Add derived features for international trade analysis.
        
        Args:
            df: DataFrame with basic trade data
            
        Returns:
            DataFrame with additional trade features
        """
        df_features = df.copy()
        
        # Find the main trade balance column
        trade_col = None
        for col in df.columns:
            if 'trade_balance_datavalue' in col.lower() or 'datavalue' in col.lower():
                trade_col = col
                break
        
        if trade_col is not None and df[trade_col].notna().any():
            # Trade Balance Growth Rate (month-over-month)
            df_features['trade_growth_mom'] = df[trade_col].pct_change() * 100
            
            # Trade Balance Growth Rate (year-over-year)
            df_features['trade_growth_yoy'] = df[trade_col].pct_change(12) * 100  # 12 months
            
            # Trade Balance Trend (12-month moving average)
            df_features['trade_trend_12m'] = df[trade_col].rolling(window=12, min_periods=1).mean()
            
            # Trade Balance Trend (6-month moving average)
            df_features['trade_trend_6m'] = df[trade_col].rolling(window=6, min_periods=1).mean()
            
            # Trade Balance Deviation from Trend
            df_features['trade_deviation_from_trend'] = ((df[trade_col] - df_features['trade_trend_12m']) / 
                                                        abs(df_features['trade_trend_12m']) * 100).fillna(0)
            
            # Trade Balance Volatility
            df_features['trade_volatility_3m'] = df_features['trade_growth_mom'].rolling(
                window=3, min_periods=1).std()
            df_features['trade_volatility_6m'] = df_features['trade_growth_mom'].rolling(
                window=6, min_periods=1).std()
            
            # Trade Balance Momentum
            df_features['trade_momentum_3m'] = df_features['trade_growth_mom'].rolling(
                window=3, min_periods=1).mean()
            
            # Trade Balance Direction (improving/deteriorating)
            df_features['trade_direction'] = np.where(
                df_features['trade_momentum_3m'] > 0, 1, 0  # 1 = improving, 0 = deteriorating
            )
            
            # Trade Balance Regime (surplus/deficit with strength indicator)
            trade_mean = df[trade_col].mean()
            df_features['trade_regime'] = np.where(
                df[trade_col] > trade_mean, 1, 0  # 1 = above average, 0 = below average
            )
            
            # Trade Balance Acceleration
            df_features['trade_acceleration'] = df_features['trade_momentum_3m'].diff()
            
            # Trade Balance Seasonality (monthly pattern)
            if len(df) > 24:  # Need at least 2 years of data
                df_features['trade_seasonal_component'] = (
                    df[trade_col].groupby(df.index.month).transform('mean') / 
                    df[trade_col].mean()
                ).fillna(1.0)
            
            # Trade Balance Z-Score (standardized measure)
            rolling_mean = df[trade_col].rolling(window=24, min_periods=12).mean()
            rolling_std = df[trade_col].rolling(window=24, min_periods=12).std()
            df_features['trade_zscore'] = ((df[trade_col] - rolling_mean) / rolling_std).fillna(0)
            
            # Trade Strength Index (0-100 scale)
            # Based on position within historical range
            rolling_min = df[trade_col].rolling(window=60, min_periods=12).min()
            rolling_max = df[trade_col].rolling(window=60, min_periods=12).max()
            df_features['trade_strength_index'] = (
                (df[trade_col] - rolling_min) / (rolling_max - rolling_min) * 100
            ).fillna(50)  # Default to neutral 50
            
            self.logger.info("Added international trade-specific features")
        
        return df_features
    
    def process_exchange_rate_impacts(self) -> pd.DataFrame:
        """
        Process exchange rate impact indicators (placeholder for future FRED integration).
        
        Returns:
            DataFrame with exchange rate impact indicators
        """
        # This would process USD strength indicators from FRED
        # For now, return empty DataFrame as we focus on BEA trade data
        self.logger.info("Exchange rate impact processing not implemented yet")
        return pd.DataFrame()
    
    def process(self, intervals: List[str] = None) -> Dict[str, pd.DataFrame]:
        """
        Main processing method for international trade indicators.
        
        Args:
            intervals: List of intervals to process ('1_minute', '1_hour', '1_day')
            
        Returns:
            Dictionary of processed DataFrames keyed by interval
        """
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
        
        self.logger.info("Starting international trade data processing")
        
        # Process trade balance data from BEA
        trade_data = self.process_bea_trade_data()
        
        if trade_data.empty:
            self.logger.warning("No trade data available for processing")
            return {}
        
        # Validate data quality
        is_valid, issues = self.validate_data(trade_data)
        if not is_valid:
            self.logger.warning(f"Data validation issues: {issues}")
        
        # Calculate additional features
        feature_config = {
            'lags': [1, 3, 6, 12],  # 1, 3, 6 months, 1 year for monthly trade data
            'differences': [1, 3, 6, 12],  # Various change periods
            'rolling_stats': [3, 6, 12, 24],  # 3 months to 2 years rolling windows
            'momentum': True,
            'volatility': True
        }
        
        trade_features = self.calculate_features(trade_data, feature_config)
        
        # Resample to different time intervals
        resampled_data = self.resample_to_intervals(trade_features, intervals)
        
        # Save processed data
        self.save_processed_data(resampled_data, "international_trade")
        
        # Log processing results
        for interval, df in resampled_data.items():
            self.logger.info(f"International trade data - {interval}: {df.shape[0]} observations, {df.shape[1]} features")
        
        return resampled_data
    
    def get_latest_trade_balance(self) -> Optional[float]:
        """
        Get the most recent trade balance value.
        
        Returns:
            Latest trade balance or None if not available
        """
        trade_data = self.process_bea_trade_data()
        
        if trade_data.empty:
            return None
        
        # Find trade balance column
        for col in trade_data.columns:
            if 'trade_balance_datavalue' in col.lower():
                latest_value = trade_data[col].dropna().iloc[-1]
                self.logger.info(f"Latest trade balance: {latest_value:.2f}")
                return float(latest_value)
        
        return None
    
    def get_trade_regime(self) -> Optional[str]:
        """
        Get current trade balance regime.
        
        Returns:
            'surplus' or 'deficit' based on latest trade data relative to historical average
        """
        trade_data = self.process_bea_trade_data()
        
        if trade_data.empty:
            return None
        
        for col in trade_data.columns:
            if 'trade_regime' in col:
                latest_regime = trade_data[col].dropna().iloc[-1]
                regime_name = "above_average" if latest_regime == 1 else "below_average"
                self.logger.info(f"Current trade regime: {regime_name}")
                return regime_name
        
        return None
    
    def get_trade_momentum(self) -> Optional[float]:
        """
        Get current trade balance momentum indicator.
        
        Returns:
            Latest 3-month trade momentum or None if not available
        """
        trade_data = self.process_bea_trade_data()
        
        if trade_data.empty:
            return None
        
        for col in trade_data.columns:
            if 'trade_momentum_3m' in col:
                latest_momentum = trade_data[col].dropna().iloc[-1]
                self.logger.info(f"Current trade momentum: {latest_momentum:.2f}")
                return float(latest_momentum)
        
        return None
