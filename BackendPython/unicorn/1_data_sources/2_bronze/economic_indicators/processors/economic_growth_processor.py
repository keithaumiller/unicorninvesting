#!/usr/bin/env python3
"""
Economic Growth Processor

Processes economic growth indicators from BEA and FRED sources including:
- Real GDP (annual and quarterly)
- Industrial production
- Employment indicators
- Business activity measures

Transforms raw economic data into standardized features for XGBoost alpha models
at 1-minute, 1-hour, and 1-day trading intervals.
"""

import os
import pandas as pd
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional
from .base_processor import BaseEconomicProcessor

class EconomicGrowthProcessor(BaseEconomicProcessor):
    """
    Processor for economic growth and employment indicators.
    
    Handles GDP data from BEA, employment data from FRED, and other growth indicators
    to create standardized features for crypto trading alpha models.
    """
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.category = "economic_growth"
        self.logger.info("Initialized Economic Growth Processor")
    
    def process_bea_gdp_data(self) -> pd.DataFrame:
        """
        Process BEA GDP data from collected CSV files.
        
        Returns:
            DataFrame with processed GDP indicators
        """
        # Load BEA data files
        bea_data = self.load_raw_data('bea')
        
        if not bea_data:
            self.logger.warning("No BEA data files found")
            return pd.DataFrame()
        
        gdp_data = pd.DataFrame()
        
        # Process GDP files
        for filename, df in bea_data.items():
            if 'economic_growth' in filename.lower() or 'gdp' in filename.lower():
                self.logger.info(f"Processing GDP data from {filename}")
                
                # Standardize timestamps
                df_processed = self.standardize_timestamps(df)
                
                if df_processed.empty:
                    continue
                
                # Clean and process GDP data
                numeric_columns = df_processed.select_dtypes(include=[np.number]).columns
                
                for col in numeric_columns:
                    # Create meaningful column names
                    if 'DataValue' in col:
                        new_col_name = f"gdp_real_{col.lower()}"
                    else:
                        new_col_name = f"gdp_{col.lower()}"
                    
                    # Add to combined dataset
                    gdp_data[new_col_name] = df_processed[col]
                
                # Preserve the datetime index
                if gdp_data.empty:
                    gdp_data.index = df_processed.index
        
        # Add derived GDP features
        if not gdp_data.empty:
            gdp_data = self._add_gdp_features(gdp_data)
        
        return gdp_data
    
    def _add_gdp_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Add derived features for GDP analysis.
        
        Args:
            df: DataFrame with basic GDP data
            
        Returns:
            DataFrame with additional GDP features
        """
        df_features = df.copy()
        
        # Find the main GDP column
        gdp_col = None
        for col in df.columns:
            if 'datavalue' in col.lower() and 'gdp' in col.lower():
                gdp_col = col
                break
        
        if gdp_col is not None:
            # GDP Growth Rate (year-over-year)
            df_features['gdp_growth_yoy'] = df[gdp_col].pct_change(4)  # Quarterly data, 4 periods = 1 year
            
            # GDP Growth Rate (quarter-over-quarter annualized)
            df_features['gdp_growth_qoq_annualized'] = (df[gdp_col].pct_change() * 4) * 100
            
            # GDP Trend (12-period moving average)
            df_features['gdp_trend'] = df[gdp_col].rolling(window=12, min_periods=1).mean()
            
            # GDP Deviation from Trend
            df_features['gdp_deviation_from_trend'] = ((df[gdp_col] - df_features['gdp_trend']) / 
                                                      df_features['gdp_trend']) * 100
            
            # GDP Volatility (rolling standard deviation of growth rates)
            df_features['gdp_growth_volatility'] = df_features['gdp_growth_qoq_annualized'].rolling(
                window=8, min_periods=1).std()
            
            # GDP Momentum Indicator
            df_features['gdp_momentum'] = df_features['gdp_growth_qoq_annualized'].rolling(
                window=4, min_periods=1).mean()
            
            # GDP Regime Classification (expansion/contraction)
            df_features['gdp_regime'] = np.where(
                df_features['gdp_growth_qoq_annualized'] > 0, 1, 0)  # 1 = expansion, 0 = contraction
            
            self.logger.info("Added GDP-specific features")
        
        return df_features
    
    def process_fred_employment_data(self) -> pd.DataFrame:
        """
        Process FRED employment data (if available).
        
        Returns:
            DataFrame with processed employment indicators
        """
        # This would process FRED employment data
        # For now, return empty DataFrame as we focus on BEA data
        self.logger.info("FRED employment processing not implemented yet")
        return pd.DataFrame()
    
    def process(self, intervals: List[str] = None) -> Dict[str, pd.DataFrame]:
        """
        Main processing method for economic growth indicators.
        
        Args:
            intervals: List of intervals to process ('1_minute', '1_hour', '1_day')
            
        Returns:
            Dictionary of processed DataFrames keyed by interval
        """
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
        
        self.logger.info("Starting economic growth data processing")
        
        # Process GDP data from BEA
        gdp_data = self.process_bea_gdp_data()
        
        if gdp_data.empty:
            self.logger.warning("No GDP data available for processing")
            return {}
        
        # Validate data quality
        is_valid, issues = self.validate_data(gdp_data)
        if not is_valid:
            self.logger.warning(f"Data validation issues: {issues}")
        
        # Calculate additional features
        feature_config = {
            'lags': [1, 4, 8, 12],  # 1 quarter, 1 year, 2 years, 3 years for quarterly data
            'differences': [1, 4, 8],  # Quarter-over-quarter, year-over-year, 2-year changes
            'rolling_stats': [4, 8, 12, 20],  # 1-5 year rolling windows
            'momentum': True,
            'volatility': True
        }
        
        gdp_features = self.calculate_features(gdp_data, feature_config)
        
        # Resample to different time intervals
        resampled_data = self.resample_to_intervals(gdp_features, intervals)
        
        # Save processed data
        self.save_processed_data(resampled_data, "economic_growth")
        
        # Log processing results
        for interval, df in resampled_data.items():
            self.logger.info(f"Economic growth data - {interval}: {df.shape[0]} observations, {df.shape[1]} features")
        
        return resampled_data
    
    def get_latest_gdp_growth(self) -> Optional[float]:
        """
        Get the most recent GDP growth rate.
        
        Returns:
            Latest GDP growth rate or None if not available
        """
        gdp_data = self.process_bea_gdp_data()
        
        if gdp_data.empty:
            return None
        
        # Find GDP growth column
        for col in gdp_data.columns:
            if 'gdp_growth_qoq_annualized' in col:
                latest_value = gdp_data[col].dropna().iloc[-1]
                self.logger.info(f"Latest GDP growth rate: {latest_value:.2f}%")
                return float(latest_value)
        
        return None
    
    def get_gdp_regime(self) -> Optional[str]:
        """
        Get current GDP economic regime.
        
        Returns:
            'expansion' or 'contraction' based on latest GDP data
        """
        gdp_data = self.process_bea_gdp_data()
        
        if gdp_data.empty:
            return None
        
        for col in gdp_data.columns:
            if 'gdp_regime' in col:
                latest_regime = gdp_data[col].dropna().iloc[-1]
                regime_name = "expansion" if latest_regime == 1 else "contraction"
                self.logger.info(f"Current GDP regime: {regime_name}")
                return regime_name
        
        return None
