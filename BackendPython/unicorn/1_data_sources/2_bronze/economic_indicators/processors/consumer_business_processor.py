#!/usr/bin/env python3
"""
Consumer & Business Activity Processor

Processes consumer spending and business investment indicators from BEA and FRED sources including:
- Personal Consumption Expenditures (PCE)
- Consumer spending patterns
- Business fixed investment
- Retail sales indicators

Transforms raw economic data into standardized features for XGBoost alpha models
at 1-minute, 1-hour, and 1-day trading intervals.
"""

import os
import pandas as pd
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional
from .base_processor import BaseEconomicProcessor

class ConsumerBusinessProcessor(BaseEconomicProcessor):
    """
    Processor for consumer spending and business investment indicators.
    
    Handles PCE data from BEA, retail sales from FRED, and business investment indicators
    to create standardized features for crypto trading alpha models.
    """
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.category = "consumer_business"
        self.logger.info("Initialized Consumer & Business Activity Processor")
    
    def process_bea_consumer_data(self) -> pd.DataFrame:
        """
        Process BEA consumer spending data from collected CSV files.
        
        Returns:
            DataFrame with processed consumer spending indicators
        """
        # Load BEA data files
        bea_data = self.load_raw_data('bea')
        
        if not bea_data:
            self.logger.warning("No BEA data files found")
            return pd.DataFrame()
        
        consumer_data = pd.DataFrame()
        
        # Process consumer spending files
        for filename, df in bea_data.items():
            if 'consumer_spending' in filename.lower() or 'pce' in filename.lower():
                self.logger.info(f"Processing consumer data from {filename}")
                
                # Standardize timestamps
                df_processed = self.standardize_timestamps(df)
                
                if df_processed.empty:
                    continue
                
                # Clean and process consumer data
                numeric_columns = df_processed.select_dtypes(include=[np.number]).columns
                
                for col in numeric_columns:
                    # Create meaningful column names
                    if 'DataValue' in col:
                        new_col_name = f"pce_{col.lower()}"
                    elif 'LineNumber' in col:
                        new_col_name = f"pce_line_{col.lower()}"
                    else:
                        new_col_name = f"consumer_{col.lower()}"
                    
                    # Add to combined dataset
                    consumer_data[new_col_name] = df_processed[col]
                
                # Preserve the datetime index
                if consumer_data.empty:
                    consumer_data.index = df_processed.index
        
        # Add derived consumer spending features
        if not consumer_data.empty:
            consumer_data = self._add_consumer_features(consumer_data)
        
        return consumer_data
    
    def _add_consumer_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Add derived features for consumer spending analysis.
        
        Args:
            df: DataFrame with basic consumer spending data
            
        Returns:
            DataFrame with additional consumer features
        """
        df_features = df.copy()
        
        # Find the main PCE column
        pce_col = None
        for col in df.columns:
            if 'pce_datavalue' in col.lower():
                pce_col = col
                break
        
        if pce_col is not None:
            # Consumer Spending Growth Rate (month-over-month)
            df_features['pce_growth_mom'] = df[pce_col].pct_change() * 100
            
            # Consumer Spending Growth Rate (year-over-year)
            df_features['pce_growth_yoy'] = df[pce_col].pct_change(12) * 100  # 12 months
            
            # Consumer Spending Trend (12-month moving average)
            df_features['pce_trend_12m'] = df[pce_col].rolling(window=12, min_periods=1).mean()
            
            # Consumer Spending Trend (6-month moving average for shorter-term trends)
            df_features['pce_trend_6m'] = df[pce_col].rolling(window=6, min_periods=1).mean()
            
            # Consumer Spending Deviation from Trend
            df_features['pce_deviation_from_trend'] = ((df[pce_col] - df_features['pce_trend_12m']) / 
                                                      df_features['pce_trend_12m']) * 100
            
            # Consumer Spending Volatility
            df_features['pce_volatility_3m'] = df_features['pce_growth_mom'].rolling(
                window=3, min_periods=1).std()
            df_features['pce_volatility_6m'] = df_features['pce_growth_mom'].rolling(
                window=6, min_periods=1).std()
            
            # Consumer Spending Momentum (3-month average growth)
            df_features['pce_momentum_3m'] = df_features['pce_growth_mom'].rolling(
                window=3, min_periods=1).mean()
            
            # Consumer Spending Acceleration (change in momentum)
            df_features['pce_acceleration'] = df_features['pce_momentum_3m'].diff()
            
            # Consumer Confidence Proxy (based on spending consistency)
            # Higher values indicate more consistent spending patterns
            df_features['consumer_confidence_proxy'] = 1 / (1 + df_features['pce_volatility_3m'].fillna(0))
            
            # Consumer Spending Regime (strong/weak based on growth vs trend)
            df_features['pce_regime'] = np.where(
                df_features['pce_growth_yoy'] > df_features['pce_growth_yoy'].rolling(24).mean(),
                1, 0  # 1 = strong spending, 0 = weak spending
            )
            
            # Seasonal Adjustment (basic month-over-month seasonality)
            df_features['pce_seasonal_component'] = (
                df[pce_col].groupby(df.index.month).transform('mean') / df[pce_col].mean()
            )
            
            self.logger.info("Added consumer spending-specific features")
        
        return df_features
    
    def process_business_investment_data(self) -> pd.DataFrame:
        """
        Process business investment data (if available from BEA).
        
        Returns:
            DataFrame with processed business investment indicators
        """
        # Load BEA data files
        bea_data = self.load_raw_data('bea')
        
        investment_data = pd.DataFrame()
        
        # Look for business investment files
        for filename, df in bea_data.items():
            if 'business' in filename.lower() or 'investment' in filename.lower():
                self.logger.info(f"Processing business investment data from {filename}")
                
                # Standardize timestamps
                df_processed = self.standardize_timestamps(df)
                
                if df_processed.empty:
                    continue
                
                # Process investment data
                numeric_columns = df_processed.select_dtypes(include=[np.number]).columns
                
                for col in numeric_columns:
                    new_col_name = f"business_investment_{col.lower()}"
                    investment_data[new_col_name] = df_processed[col]
                
                if investment_data.empty:
                    investment_data.index = df_processed.index
        
        return investment_data
    
    def process(self, intervals: List[str] = None) -> Dict[str, pd.DataFrame]:
        """
        Main processing method for consumer & business activity indicators.
        
        Args:
            intervals: List of intervals to process ('1_minute', '1_hour', '1_day')
            
        Returns:
            Dictionary of processed DataFrames keyed by interval
        """
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
        
        self.logger.info("Starting consumer & business activity data processing")
        
        # Process consumer spending data from BEA
        consumer_data = self.process_bea_consumer_data()
        
        # Process business investment data
        investment_data = self.process_business_investment_data()
        
        # Combine consumer and business data
        combined_data = pd.DataFrame()
        
        if not consumer_data.empty:
            combined_data = consumer_data
            
        if not investment_data.empty:
            if combined_data.empty:
                combined_data = investment_data
            else:
                # Align indexes and combine
                combined_data = pd.concat([combined_data, investment_data], axis=1, join='outer')
        
        if combined_data.empty:
            self.logger.warning("No consumer/business data available for processing")
            return {}
        
        # Validate data quality
        is_valid, issues = self.validate_data(combined_data)
        if not is_valid:
            self.logger.warning(f"Data validation issues: {issues}")
        
        # Calculate additional features
        feature_config = {
            'lags': [1, 3, 6, 12],  # 1, 3, 6 months, 1 year for monthly data
            'differences': [1, 3, 6, 12],  # Various change periods
            'rolling_stats': [3, 6, 12, 24],  # 3 months to 2 years rolling windows
            'momentum': True,
            'volatility': True
        }
        
        combined_features = self.calculate_features(combined_data, feature_config)
        
        # Resample to different time intervals
        resampled_data = self.resample_to_intervals(combined_features, intervals)
        
        # Save processed data
        self.save_processed_data(resampled_data, "consumer_business")
        
        # Log processing results
        for interval, df in resampled_data.items():
            self.logger.info(f"Consumer & business data - {interval}: {df.shape[0]} observations, {df.shape[1]} features")
        
        return resampled_data
    
    def get_latest_consumer_sentiment(self) -> Optional[float]:
        """
        Get the most recent consumer sentiment proxy based on spending patterns.
        
        Returns:
            Latest consumer confidence proxy or None if not available
        """
        consumer_data = self.process_bea_consumer_data()
        
        if consumer_data.empty:
            return None
        
        # Find consumer confidence proxy column
        for col in consumer_data.columns:
            if 'consumer_confidence_proxy' in col:
                latest_value = consumer_data[col].dropna().iloc[-1]
                self.logger.info(f"Latest consumer sentiment proxy: {latest_value:.3f}")
                return float(latest_value)
        
        return None
    
    def get_consumer_spending_growth(self) -> Optional[float]:
        """
        Get current consumer spending growth rate.
        
        Returns:
            Latest year-over-year consumer spending growth rate
        """
        consumer_data = self.process_bea_consumer_data()
        
        if consumer_data.empty:
            return None
        
        for col in consumer_data.columns:
            if 'pce_growth_yoy' in col:
                latest_growth = consumer_data[col].dropna().iloc[-1]
                self.logger.info(f"Current consumer spending growth: {latest_growth:.2f}%")
                return float(latest_growth)
        
        return None
