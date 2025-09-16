#!/usr/bin/env python3
"""
Silver Layer Economic Data Processor

Aggregates and normalizes bronze layer economic data from all 4 categories 
(economic growth, consumer business, international trade, monetary policy)
into a unified dataset optimized for alpha model consumption.

Features:
- Cross-category data integration
- Temporal alignment across different frequencies
- Feature selection and dimensionality reduction
- Data quality validation and cleaning
- Export to standardized formats for alpha models
"""

import os
import sys
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
import warnings
import logging
from pathlib import Path
import json

# Add processors to path
current_dir = os.path.dirname(os.path.abspath(__file__))
processors_dir = os.path.join(current_dir, '..', 'processors')
sys.path.append(processors_dir)

try:
    from sklearn.preprocessing import StandardScaler, MinMaxScaler
    from sklearn.feature_selection import SelectKBest, f_regression, mutual_info_regression
    from sklearn.decomposition import PCA
    SKLEARN_AVAILABLE = True
except ImportError:
    print("Warning: sklearn not available. Feature selection and scaling will be limited.")
    SKLEARN_AVAILABLE = False

warnings.filterwarnings('ignore')

class SilverLayerEconomicProcessor:
    """
    Silver layer processor for unified economic data integration.
    
    Combines bronze layer outputs from all economic categories into
    a single, clean, feature-engineered dataset for alpha models.
    """
    
    def __init__(self, bronze_path: str = None, output_path: str = None):
        """
        Initialize the silver layer processor.
        
        Args:
            bronze_path: Path to bronze layer processed data
            output_path: Path for silver layer output
        """
        # Set up logging
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        self.logger = logging.getLogger(self.__class__.__name__)
        
        # Set paths
        if bronze_path is None:
            self.bronze_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/economic_indicators/processed_data"
        else:
            self.bronze_path = bronze_path
            
        if output_path is None:
            current_dir = os.path.dirname(os.path.abspath(__file__))
            self.output_path = os.path.join(current_dir, '..', 'processed_data')
        else:
            self.output_path = output_path
            
        # Create output directories
        for interval in ['1_minute', '1_hour', '1_day']:
            os.makedirs(os.path.join(self.output_path, interval), exist_ok=True)
            
        # Economic categories
        self.categories = [
            'economic_growth',
            'consumer_business', 
            'international_trade',
            'monetary_policy'
        ]
        
        # Processing configuration
        self.config = {
            'max_features_per_category': 50,  # Limit features per category to avoid noise
            'min_data_coverage': 0.7,  # Minimum data coverage for inclusion
            'correlation_threshold': 0.95,  # Remove highly correlated features
            'variance_threshold': 0.01,  # Remove low-variance features
            'missing_data_threshold': 0.3  # Maximum missing data allowed
        }
        
        self.logger.info(f"Initialized Silver Layer Economic Processor")
        self.logger.info(f"Bronze path: {self.bronze_path}")
        self.logger.info(f"Output path: {self.output_path}")
        
    def load_bronze_data(self, interval: str = '1_day') -> Dict[str, pd.DataFrame]:
        """
        Load bronze layer data from all economic categories.
        
        Args:
            interval: Time interval ('1_minute', '1_hour', '1_day')
            
        Returns:
            Dictionary of DataFrames by category
        """
        bronze_data = {}
        interval_path = os.path.join(self.bronze_path, interval)
        
        if not os.path.exists(interval_path):
            self.logger.warning(f"Bronze path does not exist: {interval_path}")
            return {}
            
        # Load latest file for each category
        for category in self.categories:
            category_files = []
            
            # Find files matching category pattern
            if os.path.exists(interval_path):
                import glob
                pattern = os.path.join(interval_path, f"{category}_*.csv")
                category_files = glob.glob(pattern)
                
            if category_files:
                # Load most recent file
                latest_file = max(category_files, key=os.path.getmtime)
                try:
                    # Read CSV and properly handle the Date column
                    df = pd.read_csv(latest_file)
                    self.logger.info(f"  Raw CSV shape: {df.shape}")
                    
                    # Handle different possible date column names
                    date_columns = ['index', 'Date', 'date', 'timestamp']
                    date_col = None
                    for col in date_columns:
                        if col in df.columns:
                            date_col = col
                            break
                    
                    if date_col:
                        self.logger.info(f"  Found date column: {date_col}")
                        # Convert date column to datetime and set as index
                        df[date_col] = pd.to_datetime(df[date_col], errors='coerce')
                        na_count = df[date_col].isna().sum()
                        self.logger.info(f"  NaT values after conversion: {na_count}")
                        
                        df = df.set_index(date_col)
                        # Only drop rows where the index (date) is NaT, not all NaN values
                        df = df[df.index.notna()]
                        
                        # Additional validation - check if we actually have valid dates
                        if not df.empty:
                            self.logger.info(f"  Index dtype after set_index: {df.index.dtype}")
                            if df.index.dtype == 'datetime64[ns]':
                                bronze_data[category] = df
                                self.logger.info(f"Loaded {category}: {df.shape[0]} rows, {df.shape[1]} features from {os.path.basename(latest_file)}")
                                self.logger.info(f"  Date range: {df.index.min()} to {df.index.max()}")
                            else:
                                self.logger.warning(f"Index dtype is not datetime64[ns]: {df.index.dtype}")
                        else:
                            self.logger.warning(f"DataFrame is empty after date filtering for {category}")
                    else:
                        self.logger.warning(f"No date column found in {category} data (columns: {df.columns.tolist()[:5]}...)")
                        
                except Exception as e:
                    self.logger.error(f"Failed to load {category} data: {str(e)}")
                    import traceback
                    self.logger.error(f"Error details: {traceback.format_exc()}")
            else:
                self.logger.warning(f"No {category} data files found for {interval}")
                
        return bronze_data
        
    def align_temporal_data(self, data_dict: Dict[str, pd.DataFrame]) -> pd.DataFrame:
        """
        Align data from different categories temporally.
        
        Args:
            data_dict: Dictionary of DataFrames by category
            
        Returns:
            Combined DataFrame with aligned timestamps
        """
        if not data_dict:
            return pd.DataFrame()
            
        # Find common date range
        start_dates = []
        end_dates = []
        
        for category, df in data_dict.items():
            if not df.empty:
                start_dates.append(df.index.min())
                end_dates.append(df.index.max())
                
        if not start_dates:
            return pd.DataFrame()
            
        common_start = max(start_dates)
        common_end = min(end_dates)
        
        # Validate that common_start <= common_end
        if common_start > common_end:
            self.logger.warning(f"Common start date ({common_start}) is after end date ({common_end})")
            self.logger.info("Using the largest dataset's date range instead")
            
            # Find the dataset with most data and use its range
            largest_category = max(data_dict.keys(), key=lambda k: len(data_dict[k]))
            largest_df = data_dict[largest_category]
            common_start = largest_df.index.min()
            common_end = largest_df.index.max()
            self.logger.info(f"Using {largest_category} date range: {common_start} to {common_end}")
        
        self.logger.info(f"Final common date range: {common_start} to {common_end}")
        
        # Align all dataframes to common date range
        aligned_data = {}
        for category, df in data_dict.items():
            if not df.empty:
                # Filter to common date range
                mask = (df.index >= common_start) & (df.index <= common_end)
                df_aligned = df.loc[mask].copy()
                
                # Rename columns to include category prefix
                df_aligned.columns = [f"{category}_{col}" for col in df_aligned.columns]
                aligned_data[category] = df_aligned
                
        # Combine all aligned data
        if aligned_data:
            combined_df = pd.concat(aligned_data.values(), axis=1, join='outer')
            combined_df = combined_df.sort_index()
            
            self.logger.info(f"Combined data shape: {combined_df.shape[0]} rows, {combined_df.shape[1]} features")
            return combined_df
        else:
            return pd.DataFrame()
            
    def clean_and_validate_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Clean and validate the combined economic data.
        
        Args:
            df: Combined DataFrame
            
        Returns:
            Cleaned DataFrame
        """
        if df.empty:
            return df
            
        initial_shape = df.shape
        
        # Remove columns with too much missing data
        missing_threshold = self.config['missing_data_threshold']
        missing_ratios = df.isnull().sum() / len(df)
        cols_to_keep = missing_ratios[missing_ratios <= missing_threshold].index
        df = df[cols_to_keep]
        
        # Remove columns with zero variance
        if SKLEARN_AVAILABLE:
            from sklearn.feature_selection import VarianceThreshold
            variance_threshold = self.config['variance_threshold']
            
            # Only apply to numeric columns
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            if len(numeric_cols) > 0:
                selector = VarianceThreshold(threshold=variance_threshold)
                numeric_data = df[numeric_cols].fillna(df[numeric_cols].median())
                
                try:
                    selected_mask = selector.fit(numeric_data).get_support()
                    selected_numeric_cols = numeric_cols[selected_mask]
                    
                    # Keep selected numeric columns and all non-numeric columns
                    non_numeric_cols = df.select_dtypes(exclude=[np.number]).columns
                    final_cols = list(selected_numeric_cols) + list(non_numeric_cols)
                    df = df[final_cols]
                except:
                    self.logger.warning("Variance threshold filtering failed, keeping all features")
        
        # Forward fill missing values for economic data (common practice)
        df = df.fillna(method='ffill').fillna(method='bfill')
        
        # Remove any remaining rows with all NaN values
        df = df.dropna(how='all')
        
        self.logger.info(f"Data cleaning: {initial_shape} -> {df.shape}")
        self.logger.info(f"Removed {initial_shape[1] - df.shape[1]} features due to quality issues")
        
        return df
        
    def select_key_features(self, df: pd.DataFrame, target_features: int = 200) -> pd.DataFrame:
        """
        Select most important features using multiple criteria.
        
        Args:
            df: Input DataFrame
            target_features: Target number of features to select
            
        Returns:
            DataFrame with selected features
        """
        if df.empty or not SKLEARN_AVAILABLE:
            return df
            
        numeric_df = df.select_dtypes(include=[np.number])
        if numeric_df.empty:
            return df
            
        # Fill any remaining missing values
        numeric_df = numeric_df.fillna(numeric_df.median())
        
        # Remove highly correlated features
        corr_threshold = self.config['correlation_threshold']
        correlation_matrix = numeric_df.corr().abs()
        
        # Find pairs of highly correlated features
        upper_triangle = correlation_matrix.where(
            np.triu(np.ones(correlation_matrix.shape), k=1).astype(bool)
        )
        
        # Select features to drop
        features_to_drop = [column for column in upper_triangle.columns 
                          if any(upper_triangle[column] > corr_threshold)]
        
        numeric_df = numeric_df.drop(columns=features_to_drop)
        self.logger.info(f"Removed {len(features_to_drop)} highly correlated features")
        
        # Feature selection by category
        selected_features = []
        max_features_per_category = self.config['max_features_per_category']
        
        for category in self.categories:
            category_cols = [col for col in numeric_df.columns if col.startswith(f"{category}_")]
            
            if len(category_cols) > max_features_per_category:
                # Use variance for feature selection within category
                category_data = numeric_df[category_cols]
                variances = category_data.var()
                top_features = variances.nlargest(max_features_per_category).index.tolist()
                selected_features.extend(top_features)
            else:
                selected_features.extend(category_cols)
                
        # Ensure we don't exceed target
        if len(selected_features) > target_features:
            # Use overall variance to trim down
            variance_scores = numeric_df[selected_features].var()
            selected_features = variance_scores.nlargest(target_features).index.tolist()
            
        final_df = df[selected_features]
        self.logger.info(f"Feature selection: {len(numeric_df.columns)} -> {len(selected_features)} features")
        
        return final_df
        
    def create_feature_metadata(self, df: pd.DataFrame) -> Dict[str, Any]:
        """
        Create metadata about the processed features.
        
        Args:
            df: Processed DataFrame
            
        Returns:
            Feature metadata dictionary
        """
        metadata = {
            'processing_timestamp': datetime.now().isoformat(),
            'total_features': len(df.columns),
            'total_observations': len(df),
            'date_range': {
                'start': df.index.min().isoformat() if not df.empty else None,
                'end': df.index.max().isoformat() if not df.empty else None
            },
            'categories': {},
            'data_quality': {
                'missing_data_percentage': float(df.isnull().sum().sum() / (df.shape[0] * df.shape[1]) * 100),
                'numeric_features': len(df.select_dtypes(include=[np.number]).columns),
                'categorical_features': len(df.select_dtypes(exclude=[np.number]).columns)
            }
        }
        
        # Category breakdown
        for category in self.categories:
            category_cols = [col for col in df.columns if col.startswith(f"{category}_")]
            metadata['categories'][category] = {
                'feature_count': len(category_cols),
                'features': category_cols[:10]  # Sample of features
            }
            
        return metadata
        
    def process_interval(self, interval: str = '1_day') -> Tuple[pd.DataFrame, Dict[str, Any]]:
        """
        Process a specific time interval.
        
        Args:
            interval: Time interval to process
            
        Returns:
            Tuple of (processed DataFrame, metadata)
        """
        self.logger.info(f"Processing {interval} economic data...")
        
        # Load bronze data
        bronze_data = self.load_bronze_data(interval)
        if not bronze_data:
            self.logger.warning(f"No bronze data found for {interval}")
            return pd.DataFrame(), {}
            
        # Align temporal data
        combined_df = self.align_temporal_data(bronze_data)
        if combined_df.empty:
            self.logger.warning(f"No aligned data for {interval}")
            return pd.DataFrame(), {}
            
        # Clean and validate
        cleaned_df = self.clean_and_validate_data(combined_df)
        
        # Select key features
        final_df = self.select_key_features(cleaned_df)
        
        # Create metadata
        metadata = self.create_feature_metadata(final_df)
        
        return final_df, metadata
        
    def save_silver_data(self, df: pd.DataFrame, metadata: Dict[str, Any], interval: str = '1_day'):
        """
        Save processed silver layer data.
        
        Args:
            df: Processed DataFrame
            metadata: Feature metadata
            interval: Time interval
        """
        if df.empty:
            self.logger.warning(f"No data to save for {interval}")
            return
            
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save CSV data
        csv_path = os.path.join(self.output_path, interval, f"economic_silver_{interval}_{timestamp}.csv")
        df.to_csv(csv_path)
        self.logger.info(f"Saved silver data: {csv_path}")
        
        # Save metadata
        metadata_path = os.path.join(self.output_path, interval, f"economic_silver_metadata_{interval}_{timestamp}.json")
        with open(metadata_path, 'w') as f:
            json.dump(metadata, f, indent=2)
        self.logger.info(f"Saved metadata: {metadata_path}")
        
        # Save latest links
        latest_csv = os.path.join(self.output_path, interval, f"economic_silver_latest.csv")
        latest_metadata = os.path.join(self.output_path, interval, f"economic_silver_metadata_latest.json")
        
        # Create symbolic links or copies for latest files
        try:
            if os.path.exists(latest_csv):
                os.remove(latest_csv)
            if os.path.exists(latest_metadata):
                os.remove(latest_metadata)
                
            import shutil
            shutil.copy2(csv_path, latest_csv)
            shutil.copy2(metadata_path, latest_metadata)
            
            self.logger.info(f"Updated latest files for {interval}")
        except Exception as e:
            self.logger.warning(f"Failed to create latest links: {str(e)}")
            
    def process_all_intervals(self) -> Dict[str, Tuple[pd.DataFrame, Dict[str, Any]]]:
        """
        Process all time intervals.
        
        Returns:
            Dictionary of results by interval
        """
        results = {}
        intervals = ['1_day', '1_hour']  # Skip 1_minute due to memory constraints
        
        for interval in intervals:
            try:
                df, metadata = self.process_interval(interval)
                results[interval] = (df, metadata)
                
                if not df.empty:
                    self.save_silver_data(df, metadata, interval)
                    
            except Exception as e:
                self.logger.error(f"Failed to process {interval}: {str(e)}")
                results[interval] = (pd.DataFrame(), {})
                
        return results


def main():
    """Main function for testing silver layer processing."""
    print("🥈 Silver Layer Economic Data Processor")
    print("=" * 50)
    
    processor = SilverLayerEconomicProcessor()
    
    # Process all intervals
    results = processor.process_all_intervals()
    
    # Summary
    print("\n📊 Processing Summary:")
    for interval, (df, metadata) in results.items():
        if not df.empty:
            print(f"  {interval}: {df.shape[0]} observations, {df.shape[1]} features")
            print(f"    Date range: {df.index.min()} to {df.index.max()}")
            print(f"    Data quality: {metadata['data_quality']['missing_data_percentage']:.1f}% missing")
        else:
            print(f"  {interval}: No data processed")
            
    print("\n✅ Silver layer processing completed!")


if __name__ == "__main__":
    main()