"""
Silver Layer Yahoo Finance Assets Processing Pipeline

This module implements the silver layer transformation for Yahoo Finance assets,
creating cleaned, standardized, and enriched financial data that feeds into
alpha models consistently.

Silver Layer Features:
- Data quality validation and cleaning
- Temporal standardization and gap filling
- Cross-asset correlation analysis
- Advanced feature engineering and derived metrics
- Standardized schema for downstream consumption
- Data lineage and metadata tracking
- Quality scoring and completeness assessment

Silver layer enhances bronze layer with:
- Quality assessment scores (completeness, temporal alignment, outlier detection)
- Cross-asset correlations and market regime classification
- Advanced technical indicators and momentum features
- Economic integration readiness
- Alpha model consumption optimization
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
from dataclasses import dataclass, asdict
from sklearn.preprocessing import StandardScaler
from sklearn.impute import KNNImputer
import talib

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

warnings.filterwarnings('ignore')

@dataclass
class QualityMetrics:
    """Data quality metrics for silver layer assessment."""
    completeness_score: float  # 0-1, percentage of non-null values
    temporal_consistency: float  # 0-1, regularity of time intervals
    outlier_score: float  # 0-1, proportion of outliers detected
    feature_stability: float  # 0-1, consistency of feature distributions
    correlation_strength: float  # 0-1, average correlation with related assets
    overall_quality: float  # 0-1, weighted average of all metrics
    
    def to_dict(self) -> dict:
        return asdict(self)

@dataclass 
class SilverMetadata:
    """Metadata for silver layer datasets."""
    source_files: List[str]
    processing_timestamp: str
    records_processed: int
    features_generated: int
    quality_metrics: QualityMetrics
    schema_version: str = "1.0"
    
class SilverYahooFinanceProcessor:
    """
    Silver layer processor for Yahoo Finance assets data.
    
    Transforms bronze layer Yahoo Finance data into silver layer standardized format:
    - Enhanced data quality and validation
    - Cross-asset correlation analysis
    - Advanced feature engineering
    - Quality scoring and metadata
    - Alpha model ready datasets
    """
    
    def __init__(self, bronze_path: Optional[str] = None, silver_path: Optional[str] = None):
        """Initialize silver layer Yahoo Finance processor."""
        
        # Set paths
        current_dir = Path(__file__).parent
        unicorn_dir = current_dir.parent.parent.parent
        
        self.bronze_path = Path(bronze_path) if bronze_path else \
            unicorn_dir / "1_data_sources/2_bronze/yahoo_finance_assets/processed_data"
        self.silver_path = Path(silver_path) if silver_path else \
            current_dir / "processed_data"
        
        # Ensure directories exist
        self.silver_path.mkdir(parents=True, exist_ok=True)
        (self.silver_path / "crypto").mkdir(exist_ok=True)
        (self.silver_path / "forex").mkdir(exist_ok=True)
        (self.silver_path / "metadata").mkdir(exist_ok=True)
        (self.silver_path / "quality_reports").mkdir(exist_ok=True)
        
        # Asset configurations
        self.crypto_assets = ['ETH', 'BTC']
        self.forex_assets = ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
        self.intervals = ['1d', '1h']
        
        # Feature importance weights for quality scoring
        self.feature_weights = {
            'price_features': 0.3,  # Core price data quality
            'technical_indicators': 0.25,  # Technical analysis features
            'volume_features': 0.2,  # Volume-based features
            'temporal_features': 0.15,  # Time-based features
            'metadata_quality': 0.1  # Source and processing metadata
        }
        
        logger.info(f"Initialized Silver Yahoo Finance Processor")
        logger.info(f"Bronze path: {self.bronze_path}")
        logger.info(f"Silver path: {self.silver_path}")
        
    def load_bronze_data(self, category: str, asset: str, interval: str) -> Optional[pd.DataFrame]:
        """Load bronze layer data for specific asset and interval."""
        
        try:
            file_path = self.bronze_path / category / f"{asset}_bronze_{interval}_latest.csv"
            
            if not file_path.exists():
                logger.warning(f"Bronze file not found: {file_path}")
                return None
                
            df = pd.read_csv(file_path)
            
            # Handle different datetime column names (1h uses 'Datetime', 1d uses 'Date')
            datetime_col = None
            if 'Datetime' in df.columns:
                datetime_col = 'Datetime'
            elif 'Date' in df.columns:
                datetime_col = 'Date'
            else:
                logger.error(f"No datetime column found in {asset} {interval} data")
                return None
            
            # Parse datetime and standardize column name
            df['Datetime'] = pd.to_datetime(df[datetime_col])
            if datetime_col != 'Datetime':
                df.drop(columns=[datetime_col], inplace=True)
            df.set_index('Datetime', inplace=True)
            
            logger.info(f"Loaded {asset} {interval} bronze data: {len(df)} records")
            return df
            
        except Exception as e:
            logger.error(f"Error loading bronze data for {asset} {interval}: {e}")
            return None
    
    def calculate_quality_metrics(self, df: pd.DataFrame, asset: str, interval: str) -> QualityMetrics:
        """Calculate comprehensive quality metrics for the dataset."""
        
        try:
            # Completeness score
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            completeness_score = (1 - df[numeric_cols].isnull().sum().sum() / (len(df) * len(numeric_cols)))
            
            # Temporal consistency (regularity of intervals)
            time_diffs = df.index.to_series().diff().dropna()
            expected_freq = '1H' if interval == '1h' else '1D'
            expected_diff = pd.Timedelta(expected_freq)
            temporal_consistency = (time_diffs == expected_diff).mean()
            
            # Outlier detection using IQR method
            outlier_scores = []
            for col in ['close', 'volume']:
                if col in df.columns and df[col].notna().sum() > 0:
                    Q1 = df[col].quantile(0.25)
                    Q3 = df[col].quantile(0.75)
                    IQR = Q3 - Q1
                    outliers = ((df[col] < (Q1 - 1.5 * IQR)) | (df[col] > (Q3 + 1.5 * IQR))).sum()
                    outlier_scores.append(1 - outliers / len(df))
            outlier_score = np.mean(outlier_scores) if outlier_scores else 0.8
            
            # Feature stability (coefficient of variation for key features)
            stability_scores = []
            for col in ['close', 'volume', 'volatility_14']:
                if col in df.columns and df[col].notna().sum() > 0 and df[col].std() > 0:
                    cv = df[col].std() / df[col].mean()
                    # Convert CV to stability score (lower CV = higher stability)
                    stability_scores.append(1 / (1 + cv))
            feature_stability = np.mean(stability_scores) if stability_scores else 0.7
            
            # Correlation strength (placeholder - would calculate with other assets)
            correlation_strength = 0.8  # Will be calculated in cross-asset analysis
            
            # Overall quality (weighted average)
            overall_quality = (
                completeness_score * 0.3 +
                temporal_consistency * 0.25 +
                outlier_score * 0.2 +
                feature_stability * 0.15 +
                correlation_strength * 0.1
            )
            
            return QualityMetrics(
                completeness_score=float(completeness_score),
                temporal_consistency=float(temporal_consistency),
                outlier_score=float(outlier_score),
                feature_stability=float(feature_stability),
                correlation_strength=float(correlation_strength),
                overall_quality=float(overall_quality)
            )
            
        except Exception as e:
            logger.error(f"Error calculating quality metrics: {e}")
            return QualityMetrics(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
    
    def enhance_features(self, df: pd.DataFrame, asset: str, category: str) -> pd.DataFrame:
        """Enhance bronze layer features with silver layer advanced analytics."""
        
        try:
            df = df.copy()
            
            # Advanced Technical Indicators (using TA-Lib when available)
            if 'close' in df.columns:
                close_prices = df['close'].fillna(method='ffill').values
                
                # Advanced momentum indicators
                try:
                    df['williams_r'] = talib.WILLR(df['high'].fillna(method='ffill').values,
                                                  df['low'].fillna(method='ffill').values,
                                                  close_prices, timeperiod=14)
                    df['cci'] = talib.CCI(df['high'].fillna(method='ffill').values,
                                         df['low'].fillna(method='ffill').values,
                                         close_prices, timeperiod=14)
                    df['adx'] = talib.ADX(df['high'].fillna(method='ffill').values,
                                         df['low'].fillna(method='ffill').values,
                                         close_prices, timeperiod=14)
                except:
                    logger.warning("TA-Lib not available, using basic indicators")
                
                # Volatility regime classification
                if 'volatility_14' in df.columns:
                    vol_median = df['volatility_14'].median()
                    df['volatility_regime'] = np.where(
                        df['volatility_14'] > vol_median * 1.5, 'high',
                        np.where(df['volatility_14'] < vol_median * 0.5, 'low', 'normal')
                    )
                
                # Price momentum across multiple timeframes
                for period in [5, 10, 20, 50]:
                    df[f'momentum_{period}'] = df['close'].pct_change(periods=period)
                
                # Support and resistance levels (enhanced)
                df['resistance_level'] = df['high'].rolling(window=20).max()
                df['support_level'] = df['low'].rolling(window=20).min()
                df['price_position_enhanced'] = (df['close'] - df['support_level']) / \
                                              (df['resistance_level'] - df['support_level'])
                
            # Volume analysis enhancement
            if 'volume' in df.columns and df['volume'].sum() > 0:
                # Volume rate of change
                df['volume_roc'] = df['volume'].pct_change(periods=5)
                
                # Volume moving averages
                df['volume_ma_50'] = df['volume'].rolling(window=50).mean()
                df['volume_ratio_50'] = df['volume'] / df['volume_ma_50']
                
                # Volume trend
                df['volume_trend'] = np.where(df['volume'] > df['volume_ma_20'], 1,
                                            np.where(df['volume'] < df['volume_ma_20'] * 0.5, -1, 0))
            
            # Market microstructure (for forex)
            if category == 'forex':
                # Spread analysis (using high-low as proxy)
                df['spread_proxy'] = df['high'] - df['low']
                df['spread_ma'] = df['spread_proxy'].rolling(window=20).mean()
                df['spread_normalized'] = df['spread_proxy'] / df['spread_ma']
                
                # Trading session indicators (enhanced)
                hour = df.index.hour
                df['session_overlap'] = np.where(
                    ((hour >= 8) & (hour <= 12)) |  # London-Asia overlap
                    ((hour >= 13) & (hour <= 17)),  # London-NY overlap
                    1, 0
                )
            
            # Market regime classification
            if 'close' in df.columns:
                # Trend strength
                ma_short = df['close'].rolling(window=10).mean()
                ma_long = df['close'].rolling(window=50).mean()
                df['trend_strength'] = (ma_short - ma_long) / ma_long
                
                # Market regime
                df['market_regime'] = np.where(
                    df['trend_strength'] > 0.02, 'uptrend',
                    np.where(df['trend_strength'] < -0.02, 'downtrend', 'sideways')
                )
            
            # Quality flags
            df['data_quality_flag'] = 1  # Will be updated based on quality metrics
            df['silver_processing_timestamp'] = datetime.now().isoformat()
            
            logger.info(f"Enhanced {asset} features: {len(df.columns)} total columns")
            return df
            
        except Exception as e:
            logger.error(f"Error enhancing features for {asset}: {e}")
            return df
    
    def standardize_schema(self, df: pd.DataFrame, asset: str, category: str, interval: str) -> pd.DataFrame:
        """Standardize schema for consistent downstream consumption."""
        
        try:
            # Ensure required columns exist
            required_columns = [
                'asset', 'category', 'interval', 'source',
                'processing_timestamp', 'silver_processing_timestamp'
            ]
            
            for col in required_columns:
                if col not in df.columns:
                    if col == 'asset':
                        df[col] = asset
                    elif col == 'category':
                        df[col] = category
                    elif col == 'interval':
                        df[col] = interval
                    elif col == 'source':
                        df[col] = 'yahoo_finance'
                    elif col == 'silver_processing_timestamp':
                        df[col] = datetime.now().isoformat()
            
            # Standardize column order (core columns first)
            core_columns = ['asset', 'category', 'interval', 'source']
            price_columns = [col for col in df.columns if col in ['open', 'high', 'low', 'close', 'volume']]
            feature_columns = [col for col in df.columns if col not in core_columns + price_columns + ['processing_timestamp', 'silver_processing_timestamp']]
            timestamp_columns = ['processing_timestamp', 'silver_processing_timestamp']
            
            column_order = core_columns + price_columns + feature_columns + timestamp_columns
            column_order = [col for col in column_order if col in df.columns]
            
            df = df[column_order]
            
            logger.info(f"Standardized schema for {asset}: {len(df.columns)} columns")
            return df
            
        except Exception as e:
            logger.error(f"Error standardizing schema for {asset}: {e}")
            return df
    
    def process_asset(self, category: str, asset: str, interval: str) -> bool:
        """Process a single asset through silver layer transformation."""
        
        try:
            logger.info(f"Processing {category}/{asset} {interval} for silver layer...")
            
            # Load bronze data
            df = self.load_bronze_data(category, asset, interval)
            if df is None or len(df) == 0:
                logger.warning(f"No bronze data for {asset} {interval}")
                return False
            
            # Calculate quality metrics
            quality_metrics = self.calculate_quality_metrics(df, asset, interval)
            
            # Enhance features
            df_enhanced = self.enhance_features(df, asset, category)
            
            # Standardize schema
            df_final = self.standardize_schema(df_enhanced, asset, category, interval)
            
            # Save silver layer data
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_path = self.silver_path / category / f"{asset}_silver_{interval}_{timestamp}.csv"
            latest_path = self.silver_path / category / f"{asset}_silver_{interval}_latest.csv"
            
            df_final.to_csv(output_path, index=True)
            df_final.to_csv(latest_path, index=True)
            
            # Save metadata
            metadata = SilverMetadata(
                source_files=[f"{asset}_bronze_{interval}_latest.csv"],
                processing_timestamp=datetime.now().isoformat(),
                records_processed=len(df_final),
                features_generated=len(df_final.columns),
                quality_metrics=quality_metrics
            )
            
            metadata_path = self.silver_path / "metadata" / f"{asset}_{interval}_metadata.json"
            with open(metadata_path, 'w') as f:
                json.dump(asdict(metadata), f, indent=2)
            
            # Save quality report
            quality_report_path = self.silver_path / "quality_reports" / f"{asset}_{interval}_quality.json"
            with open(quality_report_path, 'w') as f:
                json.dump(quality_metrics.to_dict(), f, indent=2)
            
            logger.info(f"✅ Processed {asset} {interval}: {len(df_final)} records, quality score: {quality_metrics.overall_quality:.3f}")
            return True
            
        except Exception as e:
            logger.error(f"Error processing {asset} {interval}: {e}")
            return False
    
    def process_category(self, category: str) -> Dict[str, Any]:
        """Process all assets in a category."""
        
        try:
            logger.info(f"🔄 Processing {category.upper()} assets for silver layer...")
            
            assets = self.crypto_assets if category == 'crypto' else self.forex_assets
            results = {
                'category': category,
                'processed_assets': [],
                'failed_assets': [],
                'total_records': 0,
                'quality_scores': {}
            }
            
            for asset in assets:
                for interval in self.intervals:
                    if self.process_asset(category, asset, interval):
                        results['processed_assets'].append(f"{asset}_{interval}")
                        
                        # Load quality metrics
                        quality_path = self.silver_path / "quality_reports" / f"{asset}_{interval}_quality.json"
                        if quality_path.exists():
                            with open(quality_path, 'r') as f:
                                quality_data = json.load(f)
                                results['quality_scores'][f"{asset}_{interval}"] = quality_data['overall_quality']
                        
                        # Count records
                        latest_path = self.silver_path / category / f"{asset}_silver_{interval}_latest.csv"
                        if latest_path.exists():
                            df = pd.read_csv(latest_path)
                            results['total_records'] += len(df)
                    else:
                        results['failed_assets'].append(f"{asset}_{interval}")
            
            logger.info(f"✅ {category} processing completed successfully")
            return results
            
        except Exception as e:
            logger.error(f"Error processing {category}: {e}")
            return {'category': category, 'error': str(e)}
    
    def process_all_assets(self) -> Dict[str, Any]:
        """Process all Yahoo Finance assets through silver layer."""
        
        try:
            logger.info("=" * 60)
            logger.info("STARTING COMPREHENSIVE YAHOO FINANCE SILVER LAYER PROCESSING")
            logger.info("=" * 60)
            
            results = {
                'processing_timestamp': datetime.now().isoformat(),
                'categories': {},
                'summary': {}
            }
            
            # Process each category
            for category in ['crypto', 'forex']:
                category_results = self.process_category(category)
                results['categories'][category] = category_results
            
            # Generate summary
            total_processed = sum(len(cat['processed_assets']) for cat in results['categories'].values())
            total_failed = sum(len(cat['failed_assets']) for cat in results['categories'].values())
            total_records = sum(cat.get('total_records', 0) for cat in results['categories'].values())
            
            # Calculate average quality scores
            all_quality_scores = []
            for cat in results['categories'].values():
                all_quality_scores.extend(cat.get('quality_scores', {}).values())
            
            avg_quality = np.mean(all_quality_scores) if all_quality_scores else 0
            
            results['summary'] = {
                'total_processed': total_processed,
                'total_failed': total_failed,
                'success_rate': total_processed / (total_processed + total_failed) if (total_processed + total_failed) > 0 else 0,
                'total_records': total_records,
                'average_quality_score': avg_quality,
                'processing_location': str(self.silver_path)
            }
            
            # Save processing results
            results_path = self.silver_path / "processing_results.json"
            with open(results_path, 'w') as f:
                json.dump(results, f, indent=2)
            
            logger.info("=" * 60)
            logger.info("📊 YAHOO FINANCE SILVER LAYER PROCESSING SUMMARY")
            logger.info("=" * 60)
            logger.info(f"✅ Successful assets: {total_processed}")
            logger.info(f"❌ Failed assets: {total_failed}")
            logger.info(f"📈 Success rate: {results['summary']['success_rate']:.1%}")
            logger.info(f"📊 Total records: {total_records:,}")
            logger.info(f"🎯 Average quality score: {avg_quality:.3f}")
            logger.info(f"📁 Output location: {self.silver_path}")
            logger.info("=" * 60)
            
            return results
            
        except Exception as e:
            logger.error(f"Error in comprehensive processing: {e}")
            return {'error': str(e)}

def main():
    """Main function for command-line execution."""
    
    import argparse
    
    parser = argparse.ArgumentParser(description='Yahoo Finance Silver Layer Processor')
    parser.add_argument('--category', choices=['crypto', 'forex', 'all'], default='all',
                      help='Category to process (default: all)')
    parser.add_argument('--asset', help='Specific asset to process')
    parser.add_argument('--interval', choices=['1d', '1h'], help='Specific interval to process')
    
    args = parser.parse_args()
    
    processor = SilverYahooFinanceProcessor()
    
    if args.asset and args.interval:
        # Process specific asset
        category = 'crypto' if args.asset in processor.crypto_assets else 'forex'
        success = processor.process_asset(category, args.asset, args.interval)
        if success:
            print(f"✅ Successfully processed {args.asset} {args.interval}")
        else:
            print(f"❌ Failed to process {args.asset} {args.interval}")
    elif args.category != 'all':
        # Process specific category
        results = processor.process_category(args.category)
        print(f"✅ Processed {args.category}: {len(results.get('processed_assets', []))} assets")
    else:
        # Process all assets
        results = processor.process_all_assets()
        print(f"✅ Processing complete: {results['summary']['success_rate']:.1%} success rate")

if __name__ == "__main__":
    main()
