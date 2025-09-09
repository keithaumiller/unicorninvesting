"""
Silver Layer Economic Data Processing Pipeline

This module implements the silver layer transformation for economic indicators,
creating cleaned, standardized, and enriched economic data that feeds into
all alpha models consistently.

Silver Layer Features:
- Data quality validation and cleaning
- Temporal standardization and gap filling
- Multi-source economic data integration
- Feature engineering and derived metrics
- Standardized schema for downstream consumption
- Data lineage and metadata tracking
"""

import os
import sys
import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
from typing import Dict, Any, List, Optional, Tuple, Union
import json
import sqlite3
from pathlib import Path
import warnings
import logging

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

warnings.filterwarnings('ignore')

class SilverEconomicProcessor:
    """
    Silver layer processor for economic indicators data.
    
    Transforms bronze layer economic data into silver layer standardized format:
    - Cleaned and validated data
    - Temporal alignment and gap filling
    - Standardized schema
    - Quality metrics and metadata
    """
    
    def __init__(self, bronze_path: Optional[str] = None, silver_path: Optional[str] = None):
        """Initialize silver layer economic processor."""
        
        # Set paths
        current_dir = Path(__file__).parent
        data_sources_dir = current_dir.parent.parent.parent / '1_data_sources'
        
        self.bronze_path = Path(bronze_path) if bronze_path else data_sources_dir / '2_bronze' / 'economic_indicators'
        self.silver_path = Path(silver_path) if silver_path else data_sources_dir / '3_silver' / 'economic_indicators'
        
        # Create silver directory if it doesn't exist
        self.silver_path.mkdir(parents=True, exist_ok=True)
        
        # Silver layer schema
        self.silver_schema = {
            'timestamp': 'datetime64[ns]',
            'indicator_category': 'string',
            'indicator_name': 'string',
            'indicator_value': 'float64',
            'unit': 'string',
            'frequency': 'string',
            'source': 'string',
            'quality_score': 'float64',
            'is_interpolated': 'bool',
            'is_seasonally_adjusted': 'bool',
            'metadata': 'string'
        }
        
        # Economic indicator categories
        self.indicator_categories = {
            'economic_growth': ['gdp_growth', 'industrial_production', 'productivity'],
            'consumer_business': ['consumer_confidence', 'retail_sales', 'housing_starts', 
                                'business_investment', 'consumer_spending'],
            'monetary_policy': ['interest_rates', 'money_supply', 'bank_lending', 
                              'yield_curve', 'pmi_manufacturing', 'corporate_earnings'],
            'international_trade': ['trade_balance', 'exports', 'imports', 
                                  'current_account', 'currency_exchange_rates', 
                                  'treasury_yields', 'commodity_prices']
        }
        
        # Quality thresholds
        self.quality_thresholds = {
            'missing_data_max': 0.1,  # Max 10% missing data
            'outlier_zscore_max': 4.0,  # Max 4 standard deviations
            'freshness_days_max': 90,  # Max 90 days old
            'frequency_consistency_min': 0.9  # Min 90% consistent frequency
        }
        
        logger.info(f"Initialized SilverEconomicProcessor")
        logger.info(f"Bronze path: {self.bronze_path}")
        logger.info(f"Silver path: {self.silver_path}")
    
    def process_all_economic_indicators(self) -> Dict[str, Any]:
        """
        Process all economic indicators from bronze to silver layer.
        
        Returns:
            Dict containing processing results and metadata
        """
        
        logger.info("🥈 Starting Silver Layer Economic Data Processing")
        
        results = {
            'processed_categories': [],
            'total_indicators': 0,
            'quality_summary': {},
            'processing_time': None,
            'errors': []
        }
        
        start_time = datetime.now()
        
        try:
            # Process each category
            for category, indicators in self.indicator_categories.items():
                logger.info(f"📊 Processing category: {category}")
                
                category_result = self._process_category(category, indicators)
                
                if category_result['success']:
                    results['processed_categories'].append(category)
                    results['total_indicators'] += category_result['indicator_count']
                    results['quality_summary'][category] = category_result['quality_metrics']
                else:
                    results['errors'].extend(category_result['errors'])
            
            # Create consolidated silver dataset
            self._create_consolidated_silver_dataset()
            
            # Generate silver layer metadata
            self._generate_silver_metadata(results)
            
            results['processing_time'] = (datetime.now() - start_time).total_seconds()
            
            logger.info(f"✅ Silver layer processing completed")
            logger.info(f"📊 Processed {results['total_indicators']} indicators across {len(results['processed_categories'])} categories")
            
        except Exception as e:
            logger.error(f"❌ Silver layer processing failed: {e}")
            results['errors'].append(str(e))
        
        return results
    
    def _process_category(self, category: str, indicators: List[str]) -> Dict[str, Any]:
        """Process a single category of economic indicators."""
        
        result = {
            'success': False,
            'indicator_count': 0,
            'quality_metrics': {},
            'errors': []
        }
        
        try:
            # Load bronze data for this category
            bronze_data = self._load_bronze_category_data(category)
            
            if bronze_data is None or bronze_data.empty:
                result['errors'].append(f"No bronze data found for category: {category}")
                return result
            
            # Clean and validate data
            cleaned_data = self._clean_and_validate_data(bronze_data, category)
            
            # Apply silver layer transformations
            silver_data = self._apply_silver_transformations(cleaned_data, category)
            
            # Calculate quality metrics
            quality_metrics = self._calculate_quality_metrics(silver_data, category)
            
            # Save silver data
            silver_file_path = self.silver_path / f"{category}_silver.parquet"
            silver_data.to_parquet(silver_file_path, index=False)
            
            result['success'] = True
            result['indicator_count'] = len(silver_data['indicator_name'].unique())
            result['quality_metrics'] = quality_metrics
            
            logger.info(f"✅ Processed {result['indicator_count']} indicators for {category}")
            
        except Exception as e:
            logger.error(f"❌ Error processing category {category}: {e}")
            result['errors'].append(str(e))
        
        return result
    
    def _load_bronze_category_data(self, category: str) -> Optional[pd.DataFrame]:
        """Load bronze layer data for a specific category."""
        
        try:
            # Look for bronze data files
            bronze_files = list(self.bronze_path.glob(f"*{category}*.csv")) + \
                          list(self.bronze_path.glob(f"*{category}*.parquet"))
            
            if not bronze_files:
                # Look in processed_data subdirectory
                processed_path = self.bronze_path / 'processed_data'
                if processed_path.exists():
                    bronze_files = list(processed_path.glob(f"*{category}*.csv")) + \
                                  list(processed_path.glob(f"*{category}*.parquet"))
            
            if not bronze_files:
                logger.warning(f"No bronze files found for category: {category}")
                return None
            
            # Combine all files for this category
            dataframes = []
            
            for file_path in bronze_files:
                if file_path.suffix == '.csv':
                    df = pd.read_csv(file_path)
                elif file_path.suffix == '.parquet':
                    df = pd.read_parquet(file_path)
                else:
                    continue
                
                # Add source metadata
                df['data_source_file'] = file_path.name
                dataframes.append(df)
            
            if not dataframes:
                return None
            
            # Combine all dataframes
            combined_df = pd.concat(dataframes, ignore_index=True)
            
            logger.info(f"📁 Loaded {len(combined_df)} records from {len(bronze_files)} files for {category}")
            
            return combined_df
            
        except Exception as e:
            logger.error(f"❌ Error loading bronze data for {category}: {e}")
            return None
    
    def _clean_and_validate_data(self, data: pd.DataFrame, category: str) -> pd.DataFrame:
        """Clean and validate bronze data according to silver layer standards."""
        
        logger.info(f"🧹 Cleaning and validating {category} data")
        
        # Create a copy to avoid modifying original
        df = data.copy()
        
        # Standardize column names (if they exist)
        column_mapping = {
            'date': 'timestamp',
            'datetime': 'timestamp',
            'time': 'timestamp',
            'value': 'indicator_value',
            'val': 'indicator_value',
            'indicator': 'indicator_name',
            'name': 'indicator_name',
            'category': 'indicator_category'
        }
        
        df.rename(columns=column_mapping, inplace=True)
        
        # Ensure required columns exist
        if 'timestamp' not in df.columns:
            # Try to create timestamp from other date columns
            date_cols = [col for col in df.columns if 'date' in col.lower() or 'time' in col.lower()]
            if date_cols:
                df['timestamp'] = pd.to_datetime(df[date_cols[0]], errors='coerce')
            else:
                # Create a dummy timestamp if none exists
                df['timestamp'] = pd.date_range(start='2020-01-01', periods=len(df), freq='D')
        
        # Convert timestamp to datetime
        df['timestamp'] = pd.to_datetime(df['timestamp'], errors='coerce')
        
        # Remove rows with invalid timestamps
        df = df.dropna(subset=['timestamp'])
        
        # Add missing required columns with defaults
        if 'indicator_category' not in df.columns:
            df['indicator_category'] = category
        
        if 'indicator_name' not in df.columns:
            # Try to infer from other columns or use category
            if 'variable' in df.columns:
                df['indicator_name'] = df['variable']
            else:
                df['indicator_name'] = f"{category}_indicator"
        
        if 'indicator_value' not in df.columns:
            # Try to find numeric columns
            numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
            if numeric_cols:
                df['indicator_value'] = df[numeric_cols[0]]
            else:
                logger.warning(f"No numeric columns found for {category}")
                return pd.DataFrame()  # Return empty if no values
        
        # Convert indicator_value to numeric
        df['indicator_value'] = pd.to_numeric(df['indicator_value'], errors='coerce')
        
        # Remove rows with invalid values
        df = df.dropna(subset=['indicator_value'])
        
        # Add default values for other silver schema columns
        df['unit'] = df.get('unit', 'unknown')
        df['frequency'] = df.get('frequency', 'daily')
        df['source'] = df.get('source', 'bronze_layer')
        df['is_interpolated'] = False
        df['is_seasonally_adjusted'] = False
        
        # Sort by timestamp
        df = df.sort_values('timestamp').reset_index(drop=True)
        
        logger.info(f"✅ Cleaned data: {len(df)} records remaining")
        
        return df
    
    def _apply_silver_transformations(self, data: pd.DataFrame, category: str) -> pd.DataFrame:
        """Apply silver layer transformations including gap filling and feature engineering."""
        
        logger.info(f"🔄 Applying silver transformations for {category}")
        
        df = data.copy()
        
        # Group by indicator for individual processing
        transformed_dfs = []
        
        for indicator_name in df['indicator_name'].unique():
            indicator_df = df[df['indicator_name'] == indicator_name].copy()
            
            # Sort by timestamp
            indicator_df = indicator_df.sort_values('timestamp')
            
            # Handle missing values and gaps
            indicator_df = self._handle_missing_values(indicator_df)
            
            # Detect and handle outliers
            indicator_df = self._handle_outliers(indicator_df)
            
            # Add derived features
            indicator_df = self._add_derived_features(indicator_df)
            
            # Calculate quality score
            indicator_df['quality_score'] = self._calculate_indicator_quality_score(indicator_df)
            
            transformed_dfs.append(indicator_df)
        
        # Combine all transformed indicators
        result_df = pd.concat(transformed_dfs, ignore_index=True)
        
        # Ensure silver schema compliance
        result_df = self._ensure_silver_schema_compliance(result_df)
        
        logger.info(f"✅ Applied transformations: {len(result_df)} records")
        
        return result_df
    
    def _handle_missing_values(self, df: pd.DataFrame) -> pd.DataFrame:
        """Handle missing values through interpolation and forward/backward filling."""
        
        df = df.copy()
        
        # Calculate missing data percentage
        missing_pct = df['indicator_value'].isna().mean()
        
        if missing_pct > 0:
            logger.info(f"📊 Handling {missing_pct:.1%} missing values")
            
            # For time series data, use interpolation
            if missing_pct < self.quality_thresholds['missing_data_max']:
                # Linear interpolation for short gaps
                df['indicator_value'] = df['indicator_value'].interpolate(method='linear')
                df['is_interpolated'] = df['indicator_value'].isna().shift(1, fill_value=False)
                
                # Forward fill remaining values
                df['indicator_value'] = df['indicator_value'].fillna(method='ffill')
                
                # Backward fill if still missing
                df['indicator_value'] = df['indicator_value'].fillna(method='bfill')
            else:
                logger.warning(f"⚠️ High missing data percentage: {missing_pct:.1%}")
        
        return df
    
    def _handle_outliers(self, df: pd.DataFrame) -> pd.DataFrame:
        """Detect and handle statistical outliers."""
        
        df = df.copy()
        
        # Calculate z-scores
        values = df['indicator_value'].dropna()
        if len(values) > 10:  # Need sufficient data
            z_scores = np.abs((values - values.mean()) / values.std())
            outlier_threshold = self.quality_thresholds['outlier_zscore_max']
            
            outliers = z_scores > outlier_threshold
            outlier_count = outliers.sum()
            
            if outlier_count > 0:
                logger.info(f"🎯 Detected {outlier_count} outliers")
                
                # Cap outliers at threshold percentiles
                lower_cap = values.quantile(0.01)
                upper_cap = values.quantile(0.99)
                
                df.loc[df['indicator_value'] < lower_cap, 'indicator_value'] = lower_cap
                df.loc[df['indicator_value'] > upper_cap, 'indicator_value'] = upper_cap
        
        return df
    
    def _add_derived_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add derived features like moving averages, rate of change, etc."""
        
        df = df.copy()
        
        if len(df) > 1:
            # Sort by timestamp to ensure proper ordering
            df = df.sort_values('timestamp')
            
            # Add rate of change
            df['value_pct_change'] = df['indicator_value'].pct_change()
            
            # Add moving averages if enough data
            if len(df) >= 7:
                df['value_ma_7d'] = df['indicator_value'].rolling(window=7, min_periods=1).mean()
            
            if len(df) >= 30:
                df['value_ma_30d'] = df['indicator_value'].rolling(window=30, min_periods=1).mean()
            
            # Add volatility measure
            if len(df) >= 10:
                df['value_volatility'] = df['indicator_value'].rolling(window=10, min_periods=1).std()
            
            # Add trend indicator
            if len(df) >= 5:
                # Simple trend: compare current value to 5-period average
                ma5 = df['indicator_value'].rolling(window=5, min_periods=1).mean()
                df['trend_direction'] = np.where(df['indicator_value'] > ma5, 1, 
                                               np.where(df['indicator_value'] < ma5, -1, 0))
        
        return df
    
    def _calculate_indicator_quality_score(self, df: pd.DataFrame) -> float:
        """Calculate quality score for an indicator based on multiple factors."""
        
        quality_factors = []
        
        # Data completeness (0-1)
        completeness = 1 - df['indicator_value'].isna().mean()
        quality_factors.append(completeness * 0.3)
        
        # Data freshness (0-1)
        if not df.empty and 'timestamp' in df.columns:
            latest_date = df['timestamp'].max()
            days_old = (datetime.now() - latest_date.to_pydatetime()).days
            freshness = max(0, 1 - days_old / self.quality_thresholds['freshness_days_max'])
            quality_factors.append(freshness * 0.2)
        
        # Data consistency (0-1) - based on regular intervals
        if len(df) > 5:
            time_diffs = df['timestamp'].diff().dt.days.dropna()
            if len(time_diffs) > 0:
                most_common_diff = time_diffs.mode().iloc[0] if len(time_diffs.mode()) > 0 else 1
                consistency = (time_diffs == most_common_diff).mean()
                quality_factors.append(consistency * 0.2)
        
        # Value reasonableness (0-1) - no extreme outliers
        if len(df) > 10:
            z_scores = np.abs((df['indicator_value'] - df['indicator_value'].mean()) / 
                             df['indicator_value'].std())
            extreme_outliers = (z_scores > 5).mean()
            reasonableness = max(0, 1 - extreme_outliers)
            quality_factors.append(reasonableness * 0.3)
        
        # Calculate overall quality score
        total_score = sum(quality_factors) if quality_factors else 0.5
        
        return min(1.0, max(0.0, total_score))
    
    def _ensure_silver_schema_compliance(self, df: pd.DataFrame) -> pd.DataFrame:
        """Ensure dataframe complies with silver layer schema."""
        
        result_df = df.copy()
        
        # Add missing schema columns with defaults
        schema_defaults = {
            'timestamp': pd.Timestamp.now(),
            'indicator_category': 'unknown',
            'indicator_name': 'unknown',
            'indicator_value': 0.0,
            'unit': 'unknown',
            'frequency': 'daily',
            'source': 'bronze_layer',
            'quality_score': 0.5,
            'is_interpolated': False,
            'is_seasonally_adjusted': False,
            'metadata': '{}'
        }
        
        for column, default_value in schema_defaults.items():
            if column not in result_df.columns:
                result_df[column] = default_value
        
        # Ensure data types match schema
        try:
            result_df['timestamp'] = pd.to_datetime(result_df['timestamp'])
            result_df['indicator_value'] = pd.to_numeric(result_df['indicator_value'], errors='coerce')
            result_df['quality_score'] = pd.to_numeric(result_df['quality_score'], errors='coerce')
            result_df['is_interpolated'] = result_df['is_interpolated'].astype(bool)
            result_df['is_seasonally_adjusted'] = result_df['is_seasonally_adjusted'].astype(bool)
            
            # Convert string columns
            string_cols = ['indicator_category', 'indicator_name', 'unit', 'frequency', 'source']
            for col in string_cols:
                result_df[col] = result_df[col].astype(str)
            
            # Ensure metadata is JSON string
            if 'metadata' in result_df.columns:
                result_df['metadata'] = result_df['metadata'].apply(
                    lambda x: x if isinstance(x, str) else json.dumps(x if x is not None else {})
                )
            else:
                result_df['metadata'] = '{}'
                
        except Exception as e:
            logger.error(f"❌ Error ensuring schema compliance: {e}")
        
        # Select only schema columns in correct order
        schema_columns = list(self.silver_schema.keys())
        available_columns = [col for col in schema_columns if col in result_df.columns]
        result_df = result_df[available_columns]
        
        return result_df
    
    def _calculate_quality_metrics(self, df: pd.DataFrame, category: str) -> Dict[str, Any]:
        """Calculate quality metrics for processed silver data."""
        
        metrics = {
            'total_records': len(df),
            'unique_indicators': df['indicator_name'].nunique(),
            'date_range': {
                'start': df['timestamp'].min().isoformat() if not df.empty else None,
                'end': df['timestamp'].max().isoformat() if not df.empty else None,
                'days_covered': (df['timestamp'].max() - df['timestamp'].min()).days if not df.empty else 0
            },
            'data_quality': {
                'avg_quality_score': df['quality_score'].mean(),
                'high_quality_records': (df['quality_score'] >= 0.8).sum(),
                'low_quality_records': (df['quality_score'] < 0.5).sum(),
                'interpolated_records': df['is_interpolated'].sum(),
            },
            'completeness': 1 - df['indicator_value'].isna().mean(),
            'category': category
        }
        
        return metrics
    
    def _create_consolidated_silver_dataset(self) -> None:
        """Create a consolidated silver dataset combining all categories."""
        
        logger.info("📊 Creating consolidated silver dataset")
        
        try:
            # Load all silver category files
            silver_files = list(self.silver_path.glob("*_silver.parquet"))
            
            if not silver_files:
                logger.warning("⚠️ No silver files found to consolidate")
                return
            
            # Combine all silver data
            dataframes = []
            for file_path in silver_files:
                df = pd.read_parquet(file_path)
                dataframes.append(df)
            
            consolidated_df = pd.concat(dataframes, ignore_index=True)
            
            # Sort by category and timestamp
            consolidated_df = consolidated_df.sort_values(['indicator_category', 'timestamp'])
            
            # Save consolidated dataset
            consolidated_path = self.silver_path / "consolidated_economic_indicators_silver.parquet"
            consolidated_df.to_parquet(consolidated_path, index=False)
            
            # Also create CSV for easier inspection
            csv_path = self.silver_path / "consolidated_economic_indicators_silver.csv"
            consolidated_df.to_csv(csv_path, index=False)
            
            logger.info(f"✅ Created consolidated dataset with {len(consolidated_df)} records")
            
        except Exception as e:
            logger.error(f"❌ Error creating consolidated dataset: {e}")
    
    def _generate_silver_metadata(self, processing_results: Dict[str, Any]) -> None:
        """Generate metadata file documenting silver layer processing."""
        
        logger.info("📋 Generating silver layer metadata")
        
        metadata = {
            'silver_layer_metadata': {
                'creation_timestamp': datetime.now().isoformat(),
                'data_sources_path': str(self.bronze_path),
                'silver_output_path': str(self.silver_path),
                'schema_version': '1.0',
                'processing_results': processing_results,
                'schema_definition': self.silver_schema,
                'quality_thresholds': self.quality_thresholds,
                'indicator_categories': self.indicator_categories
            }
        }
        
        # Save metadata
        metadata_path = self.silver_path / "silver_layer_metadata.json"
        with open(metadata_path, 'w') as f:
            json.dump(metadata, f, indent=2, default=str)
        
        logger.info(f"✅ Generated metadata: {metadata_path}")
    
    def get_silver_data(self, category: Optional[str] = None, 
                       indicator_name: Optional[str] = None,
                       start_date: Optional[str] = None,
                       end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Retrieve silver layer economic data with optional filters.
        
        Args:
            category: Filter by indicator category
            indicator_name: Filter by specific indicator name
            start_date: Filter data from this date (YYYY-MM-DD)
            end_date: Filter data until this date (YYYY-MM-DD)
            
        Returns:
            Filtered silver layer economic data
        """
        
        try:
            # Load consolidated dataset
            consolidated_path = self.silver_path / "consolidated_economic_indicators_silver.parquet"
            
            if not consolidated_path.exists():
                logger.warning("⚠️ Consolidated silver dataset not found. Run processing first.")
                return pd.DataFrame()
            
            df = pd.read_parquet(consolidated_path)
            
            # Apply filters
            if category:
                df = df[df['indicator_category'] == category]
            
            if indicator_name:
                df = df[df['indicator_name'] == indicator_name]
            
            if start_date:
                start_dt = pd.to_datetime(start_date)
                df = df[df['timestamp'] >= start_dt]
            
            if end_date:
                end_dt = pd.to_datetime(end_date)
                df = df[df['timestamp'] <= end_dt]
            
            logger.info(f"📊 Retrieved {len(df)} silver records")
            
            return df
            
        except Exception as e:
            logger.error(f"❌ Error retrieving silver data: {e}")
            return pd.DataFrame()


def main():
    """Main function to run silver layer economic processing."""
    
    print("🥈 SILVER LAYER ECONOMIC DATA PROCESSING")
    print("=" * 60)
    
    # Initialize processor
    processor = SilverEconomicProcessor()
    
    # Process all economic indicators
    results = processor.process_all_economic_indicators()
    
    # Display results
    print(f"\n📊 PROCESSING RESULTS")
    print(f"Processed Categories: {len(results['processed_categories'])}")
    print(f"Total Indicators: {results['total_indicators']}")
    print(f"Processing Time: {results['processing_time']:.2f} seconds")
    
    if results['errors']:
        print(f"\n⚠️ ERRORS ENCOUNTERED:")
        for error in results['errors']:
            print(f"  - {error}")
    
    # Show quality summary
    print(f"\n🎯 QUALITY SUMMARY BY CATEGORY:")
    for category, quality in results['quality_summary'].items():
        print(f"  {category}:")
        print(f"    Records: {quality['total_records']}")
        print(f"    Indicators: {quality['unique_indicators']}")
        print(f"    Avg Quality Score: {quality['data_quality']['avg_quality_score']:.3f}")
        print(f"    Data Completeness: {quality['completeness']:.1%}")
    
    print(f"\n✅ Silver layer processing completed!")
    print(f"Silver data location: {processor.silver_path}")


if __name__ == "__main__":
    main()
