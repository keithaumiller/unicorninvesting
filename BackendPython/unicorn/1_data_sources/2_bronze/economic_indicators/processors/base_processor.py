#!/usr/bin/env python3
"""
Base Economic Indicator Processor

This module provides the foundation for processing raw economic data from FRED and BEA
sources into standardized formats suitable for XGBoost alpha models.

All processors inherit from BaseEconomicProcessor to ensure consistent data handling,
timestamp alignment, and feature engineering across different economic indicator categories.
"""

import os
import sys
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Union
from abc import ABC, abstractmethod
import warnings
import logging

# Suppress pandas warnings for cleaner output
warnings.filterwarnings('ignore', category=pd.errors.PerformanceWarning)
warnings.filterwarnings('ignore', category=FutureWarning)

class BaseEconomicProcessor(ABC):
    """
    Base class for all economic indicator processors.
    
    Provides common functionality for:
    - Data loading and validation
    - Timestamp alignment and resampling
    - Missing data handling
    - Feature engineering utilities
    - Export to standardized formats
    """
    
    def __init__(self, raw_data_path: str = None, output_path: str = None):
        """
        Initialize the base processor.
        
        Args:
            raw_data_path: Path to raw economic data directory
            output_path: Path for processed data output
        """
        # Set up logging
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        self.logger = logging.getLogger(self.__class__.__name__)
        
        # Set default paths
        if raw_data_path is None:
            current_dir = os.path.dirname(os.path.abspath(__file__))
            self.raw_data_path = os.path.join(current_dir, '..', '..', '1_raw', 'data', 'economic_indicators')
        else:
            self.raw_data_path = raw_data_path
            
        if output_path is None:
            current_dir = os.path.dirname(os.path.abspath(__file__))
            self.output_path = os.path.join(current_dir, '..', 'processed_data')
        else:
            self.output_path = output_path
            
        # Ensure output directories exist
        os.makedirs(os.path.join(self.output_path, '1_minute'), exist_ok=True)
        os.makedirs(os.path.join(self.output_path, '1_hour'), exist_ok=True)
        os.makedirs(os.path.join(self.output_path, '1_day'), exist_ok=True)
        
        # Standard trading time intervals
        self.intervals = {
            '1_minute': '1T',
            '1_hour': '1H', 
            '1_day': '1D'
        }
        
        self.logger.info(f"Initialized {self.__class__.__name__}")
        self.logger.info(f"Raw data path: {self.raw_data_path}")
        self.logger.info(f"Output path: {self.output_path}")
    
    def load_raw_data(self, source: str, pattern: str = "*.csv") -> Dict[str, pd.DataFrame]:
        """
        Load raw economic data files from specified source directory.
        
        Args:
            source: Source directory name ('fred', 'bea', etc.)
            pattern: File pattern to match (default: '*.csv')
            
        Returns:
            Dictionary of DataFrames keyed by filename
        """
        source_path = os.path.join(self.raw_data_path, source)
        if not os.path.exists(source_path):
            self.logger.warning(f"Source path does not exist: {source_path}")
            return {}
            
        import glob
        files = glob.glob(os.path.join(source_path, pattern))
        data = {}
        
        for file_path in files:
            try:
                filename = os.path.basename(file_path)
                df = pd.read_csv(file_path)
                data[filename] = df
                self.logger.info(f"Loaded {filename}: {df.shape[0]} rows, {df.shape[1]} columns")
            except Exception as e:
                self.logger.error(f"Failed to load {file_path}: {str(e)}")
                
        return data
    
    def standardize_timestamps(self, df: pd.DataFrame, date_column: str = 'Date') -> pd.DataFrame:
        """
        Standardize timestamp formats and set as index.
        
        Args:
            df: Input DataFrame
            date_column: Name of date column
            
        Returns:
            DataFrame with standardized datetime index
        """
        df = df.copy()
        
        # Convert date column to datetime
        try:
            df[date_column] = pd.to_datetime(df[date_column])
        except Exception as e:
            self.logger.warning(f"Date conversion failed: {str(e)}")
            return df
            
        # Set as index and sort
        df.set_index(date_column, inplace=True)
        df.sort_index(inplace=True)
        
        # Remove any duplicate timestamps
        df = df[~df.index.duplicated(keep='first')]
        
        return df
    
    def resample_to_intervals(self, df: pd.DataFrame, intervals: List[str] = None) -> Dict[str, pd.DataFrame]:
        """
        Resample data to multiple time intervals with forward-fill.
        
        Args:
            df: Input DataFrame with datetime index
            intervals: List of intervals to resample to
            
        Returns:
            Dictionary of resampled DataFrames keyed by interval
        """
        if intervals is None:
            intervals = ['1_day', '1_hour', '1_minute']
            
        resampled = {}
        
        for interval_name in intervals:
            if interval_name not in self.intervals:
                self.logger.warning(f"Unknown interval: {interval_name}")
                continue
                
            try:
                pandas_freq = self.intervals[interval_name]
                
                # For economic data, we typically want to forward-fill values
                # as economic indicators don't change every minute/hour
                resampled_df = df.resample(pandas_freq).ffill()
                
                # Ensure we don't have gaps in the data
                resampled_df = resampled_df.fillna(method='ffill')
                
                resampled[interval_name] = resampled_df
                self.logger.info(f"Resampled to {interval_name}: {resampled_df.shape[0]} observations")
                
            except Exception as e:
                self.logger.error(f"Failed to resample to {interval_name}: {str(e)}")
                
        return resampled
    
    def calculate_features(self, df: pd.DataFrame, feature_config: Dict = None) -> pd.DataFrame:
        """
        Calculate standard features for economic indicators.
        
        Args:
            df: Input DataFrame with economic data
            feature_config: Configuration for feature calculation
            
        Returns:
            DataFrame with additional feature columns
        """
        if feature_config is None:
            feature_config = {
                'lags': [1, 5, 15, 60],  # Standard lag periods
                'differences': [1, 5, 20],  # Difference periods
                'rolling_stats': [5, 20, 60],  # Rolling window sizes
                'momentum': True,
                'volatility': True
            }
            
        df_features = df.copy()
        
        # Calculate lagged features
        if 'lags' in feature_config:
            for lag in feature_config['lags']:
                for col in df.columns:
                    if df[col].dtype in ['float64', 'int64']:
                        df_features[f"{col}_lag_{lag}"] = df[col].shift(lag)
        
        # Calculate difference features (rate of change)
        if 'differences' in feature_config:
            for period in feature_config['differences']:
                for col in df.columns:
                    if df[col].dtype in ['float64', 'int64']:
                        df_features[f"{col}_diff_{period}"] = df[col].diff(period)
                        df_features[f"{col}_pct_change_{period}"] = df[col].pct_change(period)
        
        # Calculate rolling statistics
        if 'rolling_stats' in feature_config:
            for window in feature_config['rolling_stats']:
                for col in df.columns:
                    if df[col].dtype in ['float64', 'int64']:
                        df_features[f"{col}_mean_{window}"] = df[col].rolling(window).mean()
                        df_features[f"{col}_std_{window}"] = df[col].rolling(window).std()
        
        # Calculate momentum indicators
        if feature_config.get('momentum', False):
            for col in df.columns:
                if df[col].dtype in ['float64', 'int64']:
                    # Simple momentum
                    df_features[f"{col}_momentum_10"] = df[col] / df[col].shift(10) - 1
                    df_features[f"{col}_momentum_20"] = df[col] / df[col].shift(20) - 1
        
        # Calculate volatility measures
        if feature_config.get('volatility', False):
            for col in df.columns:
                if df[col].dtype in ['float64', 'int64']:
                    # Rolling volatility
                    returns = df[col].pct_change()
                    df_features[f"{col}_volatility_10"] = returns.rolling(10).std()
                    df_features[f"{col}_volatility_20"] = returns.rolling(20).std()
        
        return df_features
    
    def save_processed_data(self, data: Dict[str, pd.DataFrame], filename_prefix: str):
        """
        Save processed data to interval-specific directories.
        
        Args:
            data: Dictionary of DataFrames keyed by interval
            filename_prefix: Prefix for output filenames
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        for interval, df in data.items():
            output_dir = os.path.join(self.output_path, interval)
            filename = f"{filename_prefix}_{interval}_{timestamp}.csv"
            file_path = os.path.join(output_dir, filename)
            
            try:
                df.to_csv(file_path)
                self.logger.info(f"Saved {filename}: {df.shape[0]} rows, {df.shape[1]} columns")
            except Exception as e:
                self.logger.error(f"Failed to save {file_path}: {str(e)}")
    
    def validate_data(self, df: pd.DataFrame) -> Tuple[bool, List[str]]:
        """
        Validate data quality and completeness.
        
        Args:
            df: DataFrame to validate
            
        Returns:
            Tuple of (is_valid, list_of_issues)
        """
        issues = []
        
        # Check for completely empty DataFrame
        if df.empty:
            issues.append("DataFrame is empty")
            return False, issues
        
        # Check for missing timestamps
        if not isinstance(df.index, pd.DatetimeIndex):
            issues.append("Index is not datetime")
        
        # Check for missing data
        missing_pct = (df.isnull().sum() / len(df) * 100)
        high_missing = missing_pct[missing_pct > 50]
        if not high_missing.empty:
            issues.extend([f"Column {col} has {pct:.1f}% missing data" 
                          for col, pct in high_missing.items()])
        
        # Check for duplicate timestamps
        if df.index.duplicated().any():
            issues.append("Duplicate timestamps found")
        
        # Check for unrealistic values (basic outlier detection)
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        for col in numeric_cols:
            q1 = df[col].quantile(0.01)
            q99 = df[col].quantile(0.99)
            outliers = ((df[col] < q1 - 3 * (q99 - q1)) | 
                       (df[col] > q99 + 3 * (q99 - q1))).sum()
            if outliers > len(df) * 0.05:  # More than 5% outliers
                issues.append(f"Column {col} has {outliers} potential outliers")
        
        is_valid = len(issues) == 0
        return is_valid, issues
    
    @abstractmethod
    def process(self) -> Dict[str, pd.DataFrame]:
        """
        Main processing method to be implemented by subclasses.
        
        Returns:
            Dictionary of processed DataFrames keyed by interval
        """
        pass
