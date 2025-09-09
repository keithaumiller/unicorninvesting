"""
Silver Layer Economic Data Utilities

Utility functions for silver layer economic data processing,
including data quality assessment, feature engineering, and
integration with alpha models.
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
import warnings
import logging

logger = logging.getLogger(__name__)
warnings.filterwarnings('ignore')

class EconomicDataQualityAssessor:
    """
    Quality assessment utilities for economic indicator data.
    """
    
    @staticmethod
    def assess_data_quality(df: pd.DataFrame) -> Dict[str, Any]:
        """
        Comprehensive data quality assessment.
        
        Args:
            df: Economic indicators dataframe
            
        Returns:
            Quality assessment report
        """
        
        assessment = {
            'overall_quality_score': 0.0,
            'completeness': {},
            'consistency': {},
            'validity': {},
            'timeliness': {},
            'recommendations': []
        }
        
        try:
            # Completeness assessment
            assessment['completeness'] = {
                'total_records': len(df),
                'missing_values': df.isnull().sum().to_dict(),
                'missing_percentage': (df.isnull().sum() / len(df)).to_dict(),
                'complete_records': len(df.dropna())
            }
            
            # Consistency assessment
            if 'timestamp' in df.columns:
                time_diffs = df['timestamp'].diff().dt.days.dropna()
                assessment['consistency'] = {
                    'time_intervals': {
                        'mean_days': time_diffs.mean(),
                        'std_days': time_diffs.std(),
                        'min_days': time_diffs.min(),
                        'max_days': time_diffs.max()
                    },
                    'regular_intervals': (time_diffs.std() < 2.0) if len(time_diffs) > 0 else False
                }
            
            # Validity assessment (for numeric columns)
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            validity_metrics = {}
            
            for col in numeric_cols:
                values = df[col].dropna()
                if len(values) > 0:
                    validity_metrics[col] = {
                        'outliers_count': len(values[np.abs(values - values.mean()) > 3 * values.std()]),
                        'outliers_percentage': len(values[np.abs(values - values.mean()) > 3 * values.std()]) / len(values),
                        'zero_values': (values == 0).sum(),
                        'negative_values': (values < 0).sum(),
                        'range': {'min': values.min(), 'max': values.max()}
                    }
            
            assessment['validity'] = validity_metrics
            
            # Timeliness assessment
            if 'timestamp' in df.columns and not df.empty:
                latest_date = df['timestamp'].max()
                days_since_latest = (datetime.now() - latest_date.to_pydatetime()).days
                
                assessment['timeliness'] = {
                    'latest_date': latest_date.isoformat(),
                    'days_since_latest': days_since_latest,
                    'is_current': days_since_latest <= 7,  # Within a week
                    'date_range': {
                        'start': df['timestamp'].min().isoformat(),
                        'end': df['timestamp'].max().isoformat(),
                        'span_days': (df['timestamp'].max() - df['timestamp'].min()).days
                    }
                }
            
            # Calculate overall quality score
            scores = []
            
            # Completeness score (0-1)
            completeness_score = assessment['completeness']['complete_records'] / assessment['completeness']['total_records'] if assessment['completeness']['total_records'] > 0 else 0
            scores.append(completeness_score * 0.3)
            
            # Consistency score (0-1)
            consistency_score = 1.0 if assessment['consistency'].get('regular_intervals', False) else 0.7
            scores.append(consistency_score * 0.2)
            
            # Validity score (0-1) - based on outlier percentage
            if validity_metrics:
                avg_outlier_pct = np.mean([metrics['outliers_percentage'] for metrics in validity_metrics.values()])
                validity_score = max(0, 1 - avg_outlier_pct * 2)  # Penalize high outlier rates
                scores.append(validity_score * 0.3)
            
            # Timeliness score (0-1)
            if assessment['timeliness']:
                timeliness_score = 1.0 if assessment['timeliness']['is_current'] else max(0, 1 - assessment['timeliness']['days_since_latest'] / 30)
                scores.append(timeliness_score * 0.2)
            
            assessment['overall_quality_score'] = sum(scores)
            
            # Generate recommendations
            recommendations = []
            
            if completeness_score < 0.8:
                recommendations.append("Consider data imputation or gap-filling strategies for missing values")
            
            if not assessment['consistency'].get('regular_intervals', True):
                recommendations.append("Irregular time intervals detected - consider resampling to regular frequency")
            
            if assessment['timeliness'] and assessment['timeliness']['days_since_latest'] > 14:
                recommendations.append("Data is not current - consider updating data sources")
            
            if validity_metrics:
                for col, metrics in validity_metrics.items():
                    if metrics['outliers_percentage'] > 0.05:  # More than 5% outliers
                        recommendations.append(f"High outlier rate in {col} - consider outlier treatment")
            
            assessment['recommendations'] = recommendations
            
        except Exception as e:
            logger.error(f"Error in quality assessment: {e}")
            assessment['error'] = str(e)
        
        return assessment


class EconomicFeatureEngineer:
    """
    Feature engineering utilities for economic indicators.
    """
    
    @staticmethod
    def create_economic_features(df: pd.DataFrame, 
                                lookback_periods: List[int] = [5, 10, 20, 60]) -> pd.DataFrame:
        """
        Create derived economic features from raw indicators.
        
        Args:
            df: Economic indicators dataframe
            lookback_periods: Periods for moving averages and other features
            
        Returns:
            Enhanced dataframe with engineered features
        """
        
        result_df = df.copy()
        
        try:
            # Sort by timestamp to ensure proper ordering
            if 'timestamp' in result_df.columns:
                result_df = result_df.sort_values('timestamp')
            
            # Process each indicator separately
            if 'indicator_name' in result_df.columns:
                enhanced_dfs = []
                
                for indicator in result_df['indicator_name'].unique():
                    indicator_df = result_df[result_df['indicator_name'] == indicator].copy()
                    
                    if 'indicator_value' in indicator_df.columns:
                        # Moving averages
                        for period in lookback_periods:
                            if len(indicator_df) >= period:
                                col_name = f"{indicator}_ma_{period}"
                                indicator_df[col_name] = indicator_df['indicator_value'].rolling(
                                    window=period, min_periods=max(1, period//2)
                                ).mean()
                        
                        # Rate of change features
                        indicator_df[f"{indicator}_pct_change_1"] = indicator_df['indicator_value'].pct_change(periods=1)
                        
                        if len(indicator_df) >= 5:
                            indicator_df[f"{indicator}_pct_change_5"] = indicator_df['indicator_value'].pct_change(periods=5)
                        
                        # Volatility features
                        for period in [10, 20]:
                            if len(indicator_df) >= period:
                                col_name = f"{indicator}_volatility_{period}"
                                indicator_df[col_name] = indicator_df['indicator_value'].rolling(
                                    window=period, min_periods=max(1, period//2)
                                ).std()
                        
                        # Momentum features
                        if len(indicator_df) >= 10:
                            # Price momentum (current vs 10-period average)
                            ma_10 = indicator_df['indicator_value'].rolling(window=10, min_periods=5).mean()
                            indicator_df[f"{indicator}_momentum"] = (indicator_df['indicator_value'] - ma_10) / ma_10
                        
                        # Trend features
                        if len(indicator_df) >= 20:
                            # Linear trend over 20 periods
                            indicator_df[f"{indicator}_trend_20"] = indicator_df['indicator_value'].rolling(
                                window=20, min_periods=10
                            ).apply(lambda x: np.polyfit(range(len(x)), x, 1)[0] if len(x) > 1 else 0)
                        
                        # Relative strength features
                        if len(indicator_df) >= 14:
                            # Simple RSI-like indicator
                            delta = indicator_df['indicator_value'].diff()
                            gain = (delta.where(delta > 0, 0)).rolling(window=14, min_periods=7).mean()
                            loss = (-delta.where(delta < 0, 0)).rolling(window=14, min_periods=7).mean()
                            rs = gain / loss.replace(0, np.nan)
                            indicator_df[f"{indicator}_rsi"] = 100 - (100 / (1 + rs))
                        
                        # Seasonal decomposition proxy (if monthly or daily data)
                        if 'timestamp' in indicator_df.columns and len(indicator_df) >= 12:
                            indicator_df[f"{indicator}_month"] = indicator_df['timestamp'].dt.month
                            indicator_df[f"{indicator}_quarter"] = indicator_df['timestamp'].dt.quarter
                            indicator_df[f"{indicator}_day_of_week"] = indicator_df['timestamp'].dt.dayofweek
                    
                    enhanced_dfs.append(indicator_df)
                
                # Combine all enhanced indicators
                if enhanced_dfs:
                    result_df = pd.concat(enhanced_dfs, ignore_index=True)
            
            logger.info(f"✅ Created {result_df.shape[1] - df.shape[1]} new features")
            
        except Exception as e:
            logger.error(f"❌ Error in feature engineering: {e}")
            result_df = df.copy()  # Return original if error
        
        return result_df
    
    @staticmethod
    def create_cross_indicator_features(df: pd.DataFrame) -> pd.DataFrame:
        """
        Create features that combine multiple economic indicators.
        
        Args:
            df: Economic indicators dataframe with multiple indicators
            
        Returns:
            DataFrame with cross-indicator features
        """
        
        result_df = df.copy()
        
        try:
            if 'indicator_name' not in df.columns or 'indicator_value' not in df.columns:
                return result_df
            
            # Pivot to have indicators as columns
            pivot_df = df.pivot_table(
                index='timestamp' if 'timestamp' in df.columns else df.index,
                columns='indicator_name',
                values='indicator_value',
                aggfunc='first'
            ).reset_index()
            
            numeric_cols = pivot_df.select_dtypes(include=[np.number]).columns.tolist()
            
            if len(numeric_cols) >= 2:
                # Correlation-based features
                for i, col1 in enumerate(numeric_cols):
                    for col2 in numeric_cols[i+1:]:
                        if len(pivot_df[[col1, col2]].dropna()) >= 10:
                            # Ratio feature
                            ratio_name = f"{col1}_to_{col2}_ratio"
                            pivot_df[ratio_name] = pivot_df[col1] / (pivot_df[col2].replace(0, np.nan))
                            
                            # Difference feature
                            diff_name = f"{col1}_minus_{col2}"
                            pivot_df[diff_name] = pivot_df[col1] - pivot_df[col2]
                
                # Economic composite indicators
                if len(numeric_cols) >= 3:
                    # Simple average of all indicators (normalized)
                    normalized_cols = []
                    for col in numeric_cols:
                        values = pivot_df[col].dropna()
                        if len(values) > 1 and values.std() > 0:
                            normalized = (pivot_df[col] - values.mean()) / values.std()
                            normalized_cols.append(normalized)
                    
                    if normalized_cols:
                        pivot_df['economic_composite_index'] = pd.concat(normalized_cols, axis=1).mean(axis=1)
                
                # Convert back to long format
                id_vars = ['timestamp'] if 'timestamp' in pivot_df.columns else []
                value_vars = [col for col in pivot_df.columns if col not in id_vars]
                
                melted_df = pivot_df.melt(
                    id_vars=id_vars,
                    value_vars=value_vars,
                    var_name='indicator_name',
                    value_name='indicator_value'
                )
                
                # Merge with original data to preserve other columns
                if 'timestamp' in df.columns:
                    result_df = df.merge(
                        melted_df,
                        on=['timestamp', 'indicator_name'],
                        how='left',
                        suffixes=('', '_enhanced')
                    )
                    # Use enhanced values where available
                    result_df['indicator_value'] = result_df['indicator_value_enhanced'].fillna(result_df['indicator_value'])
                    result_df = result_df.drop('indicator_value_enhanced', axis=1)
            
            logger.info(f"✅ Created cross-indicator features")
            
        except Exception as e:
            logger.error(f"❌ Error creating cross-indicator features: {e}")
            result_df = df.copy()
        
        return result_df


class SilverDataValidator:
    """
    Validation utilities for silver layer data.
    """
    
    @staticmethod
    def validate_silver_schema(df: pd.DataFrame, required_columns: List[str] = None) -> Dict[str, Any]:
        """
        Validate that dataframe conforms to silver layer schema.
        
        Args:
            df: DataFrame to validate
            required_columns: List of required column names
            
        Returns:
            Validation results
        """
        
        if required_columns is None:
            required_columns = [
                'timestamp', 'indicator_category', 'indicator_name', 
                'indicator_value', 'quality_score'
            ]
        
        validation = {
            'is_valid': False,
            'schema_compliance': {},
            'data_types': {},
            'issues': []
        }
        
        try:
            # Check required columns
            missing_cols = set(required_columns) - set(df.columns)
            extra_cols = set(df.columns) - set(required_columns)
            
            validation['schema_compliance'] = {
                'required_columns_present': len(missing_cols) == 0,
                'missing_columns': list(missing_cols),
                'extra_columns': list(extra_cols),
                'total_columns': len(df.columns)
            }
            
            # Check data types
            expected_types = {
                'timestamp': 'datetime64[ns]',
                'indicator_value': 'float64',
                'quality_score': 'float64'
            }
            
            type_issues = []
            for col, expected_type in expected_types.items():
                if col in df.columns:
                    actual_type = str(df[col].dtype)
                    validation['data_types'][col] = {
                        'expected': expected_type,
                        'actual': actual_type,
                        'matches': actual_type == expected_type
                    }
                    
                    if not validation['data_types'][col]['matches']:
                        type_issues.append(f"{col}: expected {expected_type}, got {actual_type}")
            
            # Check data quality
            if 'quality_score' in df.columns:
                quality_scores = df['quality_score'].dropna()
                if len(quality_scores) > 0:
                    if (quality_scores < 0).any() or (quality_scores > 1).any():
                        validation['issues'].append("Quality scores should be between 0 and 1")
            
            if 'timestamp' in df.columns:
                if df['timestamp'].isnull().any():
                    validation['issues'].append("Null timestamps found")
            
            # Overall validation
            validation['is_valid'] = (
                len(missing_cols) == 0 and
                len(type_issues) == 0 and
                len(validation['issues']) == 0
            )
            
            if type_issues:
                validation['issues'].extend(type_issues)
            
        except Exception as e:
            validation['issues'].append(f"Validation error: {str(e)}")
        
        return validation


def create_economic_data_pipeline(bronze_path: str, silver_path: str) -> Dict[str, Any]:
    """
    Create a complete economic data pipeline from bronze to silver layer.
    
    Args:
        bronze_path: Path to bronze layer data
        silver_path: Path to silver layer output
        
    Returns:
        Pipeline execution results
    """
    
    logger.info("🔄 Creating economic data pipeline")
    
    try:
        from .economic_indicators_processor import SilverEconomicProcessor
        
        # Initialize processor
        processor = SilverEconomicProcessor(bronze_path, silver_path)
        
        # Run processing pipeline
        results = processor.process_all_economic_indicators()
        
        logger.info("✅ Economic data pipeline completed")
        
        return results
        
    except Exception as e:
        logger.error(f"❌ Pipeline error: {e}")
        return {'error': str(e), 'success': False}


if __name__ == "__main__":
    # Example usage
    print("🥈 Silver Layer Economic Data Utilities")
    print("This module provides utilities for silver layer economic data processing.")
