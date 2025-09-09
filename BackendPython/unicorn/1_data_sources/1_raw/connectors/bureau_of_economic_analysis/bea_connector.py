#!/usr/bin/env python3
"""
Bureau of Economic Analysis (BEA) API Connector

Production-ready economic data connector for comprehensive GDP, national accounts,
and economic activity data from the U.S. Bureau of Economic Analysis.

This connector provides essential macroeconomic context for crypto trading strategies
by collecting GDP, consumer spending, business investment, and trade data that
influences market cycles and capital allocation decisions.

Author: Unicorn Investing Platform
Version: 1.0.0
Status: Production Ready
"""

import os
import sys
import time
import logging
import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import asdict
import warnings

# Import BEA API library
try:
    import beaapi
except ImportError:
    raise ImportError("beaapi library required. Install with: pip install beaapi")

# Import local configuration
try:
    from config import (
        BEA_ECONOMIC_DATASETS, CRITICAL_DATASETS, IMPORTANT_DATASETS, ALL_DATASETS,
        CRYPTO_RELEVANCE_MAP, FEATURE_CONFIG, REGIME_THRESHOLDS,
        BEA_API_CONFIG, DATA_QUALITY_CONFIG, get_datasets_by_priority
    )
except ImportError:
    raise ImportError("Could not import BEA configuration. Ensure config.py exists.")

# Suppress pandas warnings for cleaner output
warnings.filterwarnings('ignore', category=pd.errors.PerformanceWarning)

class BEAConnector:
    """
    Production BEA API connector for economic data collection and processing.
    
    Features:
    - Automated data collection with smart throttling
    - GDP, consumer spending, business investment, and trade data
    - Feature engineering for crypto alpha models
    - Comprehensive error handling and logging
    - Data quality validation and cleaning
    """
    
    def __init__(self, api_key: Optional[str] = None, data_dir: Optional[str] = None):
        """
        Initialize BEA connector.
        
        Args:
            api_key: BEA API key (or set BEA_API_KEY environment variable)
            data_dir: Directory for data storage (defaults to ./data/)
        """
        # Set up logging
        self.logger = logging.getLogger(__name__)
        if not self.logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
            handler.setFormatter(formatter)
            self.logger.addHandler(handler)
            self.logger.setLevel(logging.INFO)
        
        # API key setup
        if api_key is None:
            try:
                # Use secure configuration manager
                sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent.parent))
                from config.config_manager import get_api_key
                self.api_key = get_api_key('bea')
            except Exception as e:
                # Fallback to environment variable
                self.api_key = os.getenv('BEA_API_KEY')
                if not self.api_key:
                    raise ValueError(
                        f"BEA API key required. Configure in config/secrets.json or set BEA_API_KEY environment variable.\n"
                        f"Configuration error: {e}\n"
                        "Get free API key at: https://apps.bea.gov/API/signup/"
                    )
        else:
            self.api_key = api_key
        
        # Data directory setup
        if data_dir:
            self.data_dir = data_dir
        else:
            # Default to economic_indicators directory structure
            current_dir = os.path.dirname(os.path.abspath(__file__))
            self.data_dir = os.path.join(current_dir, '..', '..', 'data', 'economic_indicators', 'bea')
        
        os.makedirs(self.data_dir, exist_ok=True)
        
        # Store API key for beaapi functions (no client initialization needed)
        self.logger.info("BEA Connector initialized successfully")
        
        # Load dataset configurations
        self.economic_datasets = BEA_ECONOMIC_DATASETS
        self.critical_datasets = CRITICAL_DATASETS
        self.important_datasets = IMPORTANT_DATASETS
        self.all_datasets = ALL_DATASETS
        
        self.logger.info(f"Loaded {len(self.all_datasets)} BEA economic datasets")
        self.logger.info(f"Data directory: {self.data_dir}")
    
    def get_dataset_data(self, dataset_info, start_year: Optional[int] = None, 
                        end_year: Optional[int] = None) -> pd.DataFrame:
        """
        Retrieve data for a specific BEA dataset.
        
        Args:
            dataset_info: BEADatasetInfo object with dataset configuration
            start_year: Start year for data collection (default: 2010)
            end_year: End year for data collection (default: current year)
            
        Returns:
            DataFrame with economic data indexed by date
        """
        if start_year is None:
            start_year = 2010
        if end_year is None:
            end_year = datetime.now().year
            
        self.logger.info(f"Fetching data for {dataset_info.description} ({start_year}-{end_year})")
        
        try:
            # Build API request parameters
            params = {
                'datasetname': dataset_info.dataset_name,
                'tablename': dataset_info.table_name,
                'frequency': dataset_info.frequency,
                'year': f"{start_year},{end_year}",
                'linecode': ','.join(dataset_info.line_codes),
                'resultsformat': 'json'
            }
            
            # Make API request with retry logic
            for attempt in range(BEA_API_CONFIG['retry_attempts']):
                try:
                    df = beaapi.get_data(
                        userid=self.api_key,
                        datasetname=dataset_info.dataset_name,
                        tablename=dataset_info.table_name,
                        frequency=dataset_info.frequency,
                        year=f"{start_year},{end_year}",
                        linecode=','.join(dataset_info.line_codes)
                    )
                    break
                except Exception as e:
                    if attempt < BEA_API_CONFIG['retry_attempts'] - 1:
                        self.logger.warning(f"Request attempt {attempt + 1} failed: {e}. Retrying...")
                        time.sleep(BEA_API_CONFIG['retry_delay'] * (attempt + 1))
                    else:
                        raise e
            
            # beaapi returns a DataFrame directly
            if df is None or df.empty:
                self.logger.warning(f"No data returned for {dataset_info.description}")
                return pd.DataFrame()
            
            # Process the BEA DataFrame
            processed_df = self._process_bea_data(df, dataset_info)
            
            self.logger.info(f"Retrieved {len(processed_df)} observations for {dataset_info.description}")
            return processed_df
            
        except Exception as e:
            self.logger.error(f"Error fetching {dataset_info.description}: {str(e)}")
            return pd.DataFrame()
    
    def _process_bea_data(self, df: pd.DataFrame, dataset_info) -> pd.DataFrame:
        """
        Process BEA DataFrame from beaapi library into standardized format.
        
        Args:
            df: BEA data DataFrame from beaapi.get_data()
            dataset_info: BEADatasetInfo object
            
        Returns:
            Processed DataFrame with date index and numeric values
        """
        if df.empty:
            return df
            
        try:
            # beaapi returns DataFrames with standard BEA column names
            # Common columns: TimePeriod, DataValue, possibly others
            
            # Create a copy to avoid modifying original
            processed_df = df.copy()
            
            # Handle date conversion based on frequency
            if 'TimePeriod' in processed_df.columns:
                if dataset_info.frequency == 'Q':
                    # Quarterly data: "2023Q1" format
                    processed_df['Date'] = pd.to_datetime(
                        processed_df['TimePeriod'].astype(str) + '-01', 
                        format='%YQ%q-%d', 
                        errors='coerce'
                    )
                elif dataset_info.frequency == 'A':
                    # Annual data: "2023" format  
                    processed_df['Date'] = pd.to_datetime(
                        processed_df['TimePeriod'].astype(str) + '-01-01'
                    )
                elif dataset_info.frequency == 'M':
                    # Monthly data: "2023M01" format
                    processed_df['Date'] = pd.to_datetime(
                        processed_df['TimePeriod'].astype(str), 
                        format='%YM%m', 
                        errors='coerce'
                    )
                else:
                    # Fallback: try to parse as-is
                    processed_df['Date'] = pd.to_datetime(
                        processed_df['TimePeriod'], 
                        errors='coerce'
                    )
            
            # Handle missing date creation
            if 'Date' not in processed_df.columns or processed_df['Date'].isna().all():
                self.logger.warning(f"Could not parse dates for {dataset_info.description}, using index")
                # Create sequential dates based on frequency
                start_date = datetime(2010, 1, 1)
                if dataset_info.frequency == 'Q':
                    dates = pd.date_range(start=start_date, periods=len(processed_df), freq='QS')
                elif dataset_info.frequency == 'A':
                    dates = pd.date_range(start=start_date, periods=len(processed_df), freq='YS')
                else:
                    dates = pd.date_range(start=start_date, periods=len(processed_df), freq='MS')
                processed_df['Date'] = dates
            
            # Set date as index
            if 'Date' in processed_df.columns:
                processed_df.set_index('Date', inplace=True)
                processed_df.sort_index(inplace=True)
            
            # Convert DataValue to numeric if present
            if 'DataValue' in processed_df.columns:
                processed_df['DataValue'] = pd.to_numeric(
                    processed_df['DataValue'], 
                    errors='coerce'
                )
            
            # Keep only numeric columns for analysis
            numeric_cols = processed_df.select_dtypes(include=[np.number]).columns
            if len(numeric_cols) > 0:
                processed_df = processed_df[numeric_cols]
            else:
                # If no numeric columns, try to convert DataValue
                if 'DataValue' in processed_df.columns:
                    processed_df = processed_df[['DataValue']]
                else:
                    self.logger.warning(f"No numeric data found for {dataset_info.description}")
                    return pd.DataFrame()
            
            # Add dataset metadata
            processed_df.attrs = {
                'dataset_name': dataset_info.dataset_name,
                'table_name': dataset_info.table_name,
                'description': dataset_info.description,
                'frequency': dataset_info.frequency,
                'crypto_relevance': dataset_info.crypto_relevance,
                'category': dataset_info.category
            }
            
            return processed_df
            
        except Exception as e:
            self.logger.error(f"Error processing data for {dataset_info.description}: {e}")
            return pd.DataFrame()
    
    def get_critical_indicators(self, start_year: Optional[int] = None,
                               end_year: Optional[int] = None) -> pd.DataFrame:
        """
        Get critical economic indicators for crypto trading models.
        
        Args:
            start_year: Start year (default: 5 years ago)
            end_year: End year (default: current year)
            
        Returns:
            Combined DataFrame with critical economic indicators
        """
        if start_year is None:
            start_year = datetime.now().year - 5
        if end_year is None:
            end_year = datetime.now().year
            
        self.logger.info(f"Collecting critical BEA indicators ({start_year}-{end_year})")
        
        combined_data = pd.DataFrame()
        
        for i, dataset_info in enumerate(self.critical_datasets):
            self.logger.info(f"[{i+1}/{len(self.critical_datasets)}] {dataset_info.description}")
            
            # Get dataset data
            data = self.get_dataset_data(dataset_info, start_year, end_year)
            
            if not data.empty:
                # Add to combined dataset
                if combined_data.empty:
                    combined_data = data.copy()
                else:
                    combined_data = combined_data.join(data, how='outer')
            
            # Throttle requests
            time.sleep(BEA_API_CONFIG['rate_limit_delay'])
        
        # Forward fill missing values for economic data
        combined_data = combined_data.fillna(method='ffill')
        
        self.logger.info(f"Critical indicators collection completed: {len(combined_data)} observations")
        return combined_data
    
    def delta_update(self, save_to_data_dir: bool = True) -> Optional[Dict[str, str]]:
        """
        Quick update of critical indicators (last 2 years).
        Optimized for frequent automated updates.
        
        Args:
            save_to_data_dir: Whether to save files to data directory
            
        Returns:
            Dictionary mapping file types to saved file paths
        """
        self.logger.info("⚡ Starting delta BEA data update (critical indicators, last 2 years)")
        
        # Get recent critical data (2 years for economic data context)
        end_year = datetime.now().year
        start_year = end_year - 2
        
        critical_data = self.get_critical_indicators(start_year=start_year, end_year=end_year)
        
        if critical_data.empty:
            self.logger.warning("No data collected in delta update")
            return None
        
        files_saved = {}
        
        if save_to_data_dir:
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            
            # Save timestamped delta update
            delta_file = os.path.join(self.data_dir, f'bea_delta_update_{timestamp}.csv')
            critical_data.to_csv(delta_file)
            files_saved['delta_update'] = delta_file
            
            # Save as latest critical indicators
            latest_file = os.path.join(self.data_dir, 'bea_critical_latest.csv')
            critical_data.to_csv(latest_file)
            files_saved['critical_latest'] = latest_file
            
            self.logger.info(f"Delta update saved: {len(critical_data)} observations")
        
        self.logger.info("⚡ Delta update completed")
        return files_saved
    
    def daily_update(self, save_to_data_dir: bool = True) -> Optional[Dict[str, str]]:
        """
        Comprehensive daily update of all important economic indicators.
        
        Args:
            save_to_data_dir: Whether to save files to data directory
            
        Returns:
            Dictionary mapping file types to saved file paths
        """
        self.logger.info("📅 Starting daily BEA data update (last 5 years)")
        
        # Get last 5 years of data for important indicators
        end_year = datetime.now().year
        start_year = end_year - 5
        
        all_data = pd.DataFrame()
        
        # Collect both critical and important datasets
        datasets_to_collect = self.critical_datasets + self.important_datasets
        
        self.logger.info(f"Collecting {len(datasets_to_collect)} datasets from {start_year} to {end_year}")
        
        for i, dataset_info in enumerate(datasets_to_collect):
            self.logger.info(f"   [{i+1}/{len(datasets_to_collect)}] ({(i+1)*100/len(datasets_to_collect):.1f}%) Updating {dataset_info.description}...")
            
            # Get data for this dataset
            data = self.get_dataset_data(dataset_info, start_year, end_year)
            
            if not data.empty:
                self.logger.info(f"      ✅ {len(data)} observations")
                
                # Combine with main dataset
                if all_data.empty:
                    all_data = data.copy()
                else:
                    all_data = all_data.join(data, how='outer')
            else:
                self.logger.warning(f"      ❌ No data retrieved for {dataset_info.description}")
            
            # Throttle requests (1 second for daily updates)
            time.sleep(1.0)
        
        if all_data.empty:
            self.logger.warning("No data collected in daily update")
            return None
        
        # Forward fill missing values
        all_data = all_data.fillna(method='ffill')
        
        files_saved = {}
        
        if save_to_data_dir:
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            
            # Save timestamped daily update
            daily_file = os.path.join(self.data_dir, f'bea_daily_update_{timestamp}.csv')
            all_data.to_csv(daily_file)
            files_saved['daily_update'] = daily_file
            
            # Save as comprehensive latest
            comprehensive_file = os.path.join(self.data_dir, 'bea_comprehensive_latest.csv')
            all_data.to_csv(comprehensive_file)
            files_saved['comprehensive_latest'] = comprehensive_file
            
            self.logger.info(f"💾 Updated comprehensive dataset: {len(all_data):,} total observations")
        
        self.logger.info("✅ Daily update completed")
        return files_saved
    
    def collect_comprehensive_historical_data(self, start_year: int = 2000, 
                                            throttle_delay: float = 2.0,
                                            save_to_data_dir: bool = True) -> Dict[str, pd.DataFrame]:
        """
        Collect comprehensive historical data across all BEA datasets.
        
        Args:
            start_year: Starting year for data collection
            throttle_delay: Delay between API requests (seconds)
            save_to_data_dir: Whether to save data files
            
        Returns:
            Dictionary of collected data by category
        """
        end_year = datetime.now().year
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        
        self.logger.info(f"🏛️ Starting comprehensive BEA historical data collection")
        self.logger.info(f"📅 Date range: {start_year} to {end_year}")
        self.logger.info(f"📊 Datasets: {len(self.all_datasets)} economic series")
        self.logger.info(f"⏱️  Throttling: {throttle_delay}s between requests")
        
        collected_data = {}
        
        # Group datasets by category for organized collection
        categories = {}
        for dataset_info in self.all_datasets:
            category = dataset_info.category
            if category not in categories:
                categories[category] = []
            categories[category].append(dataset_info)
        
        # Create main data directory for historical collection
        if save_to_data_dir:
            main_data_dir = self.data_dir
            os.makedirs(main_data_dir, exist_ok=True)
        
        # Collect data by category
        total_datasets = len(self.all_datasets)
        current_dataset = 0
        
        for category, datasets in categories.items():
            self.logger.info(f"\n📂 {category.upper()} ({len(datasets)} datasets)")
            category_data = {}
            
            for dataset_info in datasets:
                current_dataset += 1
                progress = current_dataset * 100 / total_datasets
                
                self.logger.info(f"   [{current_dataset}/{total_datasets}] ({progress:.1f}%) {dataset_info.description}")
                
                try:
                    # Get dataset data
                    data = self.get_dataset_data(dataset_info, start_year, end_year)
                    
                    if not data.empty:
                        # Store in category collection
                        series_key = f"{dataset_info.dataset_name}_{dataset_info.table_name}"
                        category_data[series_key] = data
                        
                        obs_count = len(data)
                        if obs_count > 0:
                            date_range = f"{data.index.min()} to {data.index.max()}"
                            self.logger.info(f"      ✅ {obs_count:,} observations ({date_range})")
                        
                        # Save individual dataset file if requested
                        if save_to_data_dir:
                            dataset_file = os.path.join(main_data_dir, 
                                                       f'{series_key}_historical_{timestamp}.csv')
                            data.to_csv(dataset_file)
                    else:
                        self.logger.warning(f"      ❌ No data retrieved for {dataset_info.description}")
                    
                    # Throttle to respect API limits
                    time.sleep(throttle_delay)
                    
                except Exception as e:
                    self.logger.error(f"      ❌ Error collecting {dataset_info.description}: {str(e)}")
                    time.sleep(throttle_delay * 2)  # Longer delay on error
            
            # Combine category data
            if category_data:
                category_df = pd.DataFrame()
                for series_key, data in category_data.items():
                    if category_df.empty:
                        category_df = data.copy()
                    else:
                        category_df = category_df.join(data, how='outer')
                
                collected_data[category] = category_df
                
                # Save category file
                if save_to_data_dir:
                    category_file = os.path.join(main_data_dir, f'bea_{category}_{timestamp}.csv')
                    category_df.to_csv(category_file)
                    self.logger.info(f"   💾 Saved {category} data: {len(category_df)} rows, {len(category_df.columns)} series")
        
        # Create comprehensive combined dataset
        if collected_data:
            self.logger.info("🔄 Creating comprehensive combined dataset...")
            
            combined_data = pd.DataFrame()
            for category, data in collected_data.items():
                if combined_data.empty:
                    combined_data = data.copy()
                else:
                    combined_data = combined_data.join(data, how='outer')
            
            # Save comprehensive dataset
            if save_to_data_dir:
                comprehensive_file = os.path.join(main_data_dir, f'bea_comprehensive_historical_{timestamp}.csv')
                combined_data.to_csv(comprehensive_file)
                
                # Also save as "latest" for easy access
                latest_file = os.path.join(main_data_dir, 'bea_comprehensive_latest.csv')
                combined_data.to_csv(latest_file)
                
                self.logger.info(f"💾 Comprehensive dataset saved:")
                self.logger.info(f"   📁 File: {os.path.basename(comprehensive_file)}")
                self.logger.info(f"   📊 Data: {len(combined_data):,} observations, {len(combined_data.columns)} series")
                self.logger.info(f"   📅 Range: {combined_data.index.min()} to {combined_data.index.max()}")
            
            # Create and save metadata
            metadata = {
                'collection_info': {
                    'timestamp': timestamp,
                    'start_year': start_year,
                    'end_year': end_year,
                    'throttle_delay': throttle_delay,
                    'total_datasets_requested': total_datasets,
                    'successful_collections': len([d for data in collected_data.values() for d in data.columns]),
                    'total_observations': len(combined_data)
                },
                'data_summary': {
                    category: {
                        'series_count': len(data.columns),
                        'observation_count': len(data),
                        'date_range': {
                            'start': data.index.min().strftime('%Y-%m-%d'),
                            'end': data.index.max().strftime('%Y-%m-%d')
                        } if not data.empty else None
                    } for category, data in collected_data.items()
                },
                'dataset_details': {
                    category: {
                        series_name: {
                            'description': getattr(data, 'attrs', {}).get('description', 'Unknown'),
                            'frequency': getattr(data, 'attrs', {}).get('frequency', 'Unknown'),
                            'crypto_relevance': getattr(data, 'attrs', {}).get('crypto_relevance', 3),
                            'observations': len(data)
                        } for series_name, data in [
                            (col, collected_data[category][col]) for col in collected_data[category].columns
                        ] if hasattr(data, 'dropna') and not data.dropna().empty
                    } for category in collected_data.keys()
                }
            }
            
            if save_to_data_dir:
                metadata_file = os.path.join(main_data_dir, f'bea_comprehensive_metadata_{timestamp}.json')
                with open(metadata_file, 'w') as f:
                    json.dump(metadata, f, indent=2, default=str)
                
                # Also save latest metadata
                latest_metadata_file = os.path.join(main_data_dir, 'bea_comprehensive_metadata_latest.json')
                with open(latest_metadata_file, 'w') as f:
                    json.dump(metadata, f, indent=2, default=str)
        
        success_rate = len([d for data in collected_data.values() for d in data.columns]) / total_datasets * 100
        
        self.logger.info(f"\n✅ BEA historical data collection completed!")
        self.logger.info(f"📊 Success rate: {success_rate:.1f}% ({len([d for data in collected_data.values() for d in data.columns])}/{total_datasets} datasets)")
        
        return collected_data
    
    def create_alpha_features(self, economic_data: pd.DataFrame) -> pd.DataFrame:
        """
        Create enhanced features for crypto alpha models from BEA economic data.
        
        Args:
            economic_data: DataFrame with BEA economic indicators
            
        Returns:
            DataFrame with engineered features for alpha models
        """
        if economic_data.empty:
            return pd.DataFrame()
        
        self.logger.info("🔧 Creating alpha model features from BEA economic data...")
        
        features = economic_data.copy()
        
        # Economic growth features
        if any('GDP' in col for col in features.columns):
            gdp_cols = [col for col in features.columns if 'GDP' in col]
            for col in gdp_cols:
                if col in features.columns:
                    # GDP growth rates
                    features[f'{col}_qoq'] = features[col].pct_change()  # Quarter-over-quarter
                    features[f'{col}_yoy'] = features[col].pct_change(4)  # Year-over-year (quarterly)
                    features[f'{col}_ma4'] = features[col].rolling(4).mean()  # 1-year moving average
                    features[f'{col}_volatility'] = features[col].pct_change().rolling(4).std()
        
        # Consumer spending features
        pce_cols = [col for col in features.columns if 'PCE' in col or 'Personal' in col]
        for col in pce_cols:
            if col in features.columns:
                features[f'{col}_growth'] = features[col].pct_change(4)  # Annual growth
                features[f'{col}_trend'] = features[col].rolling(8).mean()  # 2-year trend
        
        # Business investment features
        investment_cols = [col for col in features.columns if 'Investment' in col or 'CAPEX' in col]
        for col in investment_cols:
            if col in features.columns:
                features[f'{col}_growth'] = features[col].pct_change(4)
                features[f'{col}_acceleration'] = features[f'{col}_growth'].diff()  # Growth acceleration
        
        # Economic regime indicators
        if any('GDP' in col for col in features.columns):
            gdp_growth_col = next((col for col in features.columns if 'GDP' in col and '_qoq' in col), None)
            if gdp_growth_col:
                # Recession indicator (2 consecutive quarters of negative growth)
                features['recession_risk'] = (
                    (features[gdp_growth_col] < 0) & 
                    (features[gdp_growth_col].shift(1) < 0)
                ).astype(int)
                
                # Growth regime classification
                features['growth_regime'] = np.select([
                    features[gdp_growth_col] < REGIME_THRESHOLDS['recession_gdp_threshold'],
                    features[gdp_growth_col] > REGIME_THRESHOLDS['high_growth_threshold'],
                ], ['recession', 'high_growth'], 'moderate')
        
        # Trade and international features
        trade_cols = [col for col in features.columns if 'Trade' in col or 'Current' in col]
        for col in trade_cols:
            if col in features.columns:
                features[f'{col}_trend'] = features[col].rolling(4).mean()
                features[f'{col}_deviation'] = features[col] - features[f'{col}_trend']
        
        # Economic cycle features (using multiple indicators)
        numeric_cols = features.select_dtypes(include=[np.number]).columns
        if len(numeric_cols) > 1:
            # Create composite economic strength index
            # Normalize each series and average (simple approach)
            normalized_data = features[numeric_cols].apply(lambda x: (x - x.mean()) / x.std())
            features['economic_strength_index'] = normalized_data.mean(axis=1)
            features['economic_momentum'] = features['economic_strength_index'].diff()
        
        # Remove original columns that are now represented by features
        original_cols = economic_data.columns
        feature_cols = [col for col in features.columns if col not in original_cols or 
                       any(suffix in col for suffix in ['_growth', '_yoy', '_qoq', '_ma', '_trend', 
                                                       '_volatility', '_acceleration', '_deviation'])]
        
        alpha_features = features[feature_cols + ['recession_risk', 'growth_regime', 
                                                'economic_strength_index', 'economic_momentum']].copy()
        
        # Forward fill missing values
        alpha_features = alpha_features.fillna(method='ffill')
        
        self.logger.info(f"Created {len(alpha_features.columns)} alpha model features")
        return alpha_features
    
    def save_data_for_alpha_models(self, start_year: Optional[int] = None) -> Dict[str, str]:
        """
        Collect and save BEA economic data formatted for alpha model integration.
        
        Args:
            start_year: Starting year for data collection (default: 10 years ago)
            
        Returns:
            Dictionary mapping file types to saved file paths
        """
        if start_year is None:
            start_year = datetime.now().year - 10
        
        self.logger.info("💾 Preparing BEA data for alpha model integration...")
        
        # Get comprehensive economic data
        economic_data = pd.DataFrame()
        
        # Collect both critical and important datasets for alpha models
        datasets_to_collect = self.critical_datasets + self.important_datasets
        
        for dataset_info in datasets_to_collect:
            data = self.get_dataset_data(dataset_info, start_year=start_year)
            if not data.empty:
                if economic_data.empty:
                    economic_data = data.copy()
                else:
                    economic_data = economic_data.join(data, how='outer')
            time.sleep(BEA_API_CONFIG['rate_limit_delay'])
        
        if economic_data.empty:
            self.logger.warning("No data collected for alpha models")
            return {}
        
        # Create alpha model features
        alpha_features = self.create_alpha_features(economic_data)
        
        # Save files
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        files_saved = {}
        
        # Raw economic data
        raw_data_file = os.path.join(self.data_dir, f'bea_raw_data_{timestamp}.csv')
        economic_data.to_csv(raw_data_file)
        files_saved['raw_data'] = raw_data_file
        
        # Alpha model features
        features_file = os.path.join(self.data_dir, f'bea_features_{timestamp}.csv')
        alpha_features.to_csv(features_file)
        files_saved['features'] = features_file
        
        # Create metadata
        metadata = {
            'collection_timestamp': timestamp,
            'data_range': {
                'start': economic_data.index.min().strftime('%Y-%m-%d') if not economic_data.empty else None,
                'end': economic_data.index.max().strftime('%Y-%m-%d') if not economic_data.empty else None,
                'observations': len(economic_data)
            },
            'raw_data_series': len(economic_data.columns),
            'alpha_features': len(alpha_features.columns),
            'datasets_collected': [
                {
                    'description': dataset_info.description,
                    'category': dataset_info.category,
                    'crypto_relevance': dataset_info.crypto_relevance,
                    'frequency': dataset_info.frequency
                } for dataset_info in datasets_to_collect
            ]
        }
        
        # Save metadata
        metadata_file = os.path.join(self.data_dir, f'bea_metadata_{timestamp}.json')
        with open(metadata_file, 'w') as f:
            json.dump(metadata, f, indent=2, default=str)
        files_saved['metadata'] = metadata_file
        
        # Save "latest" versions for easy access
        latest_raw_file = os.path.join(self.data_dir, 'bea_raw_latest.csv')
        economic_data.to_csv(latest_raw_file)
        files_saved['raw_latest'] = latest_raw_file
        
        latest_features_file = os.path.join(self.data_dir, 'bea_features_latest.csv')
        alpha_features.to_csv(latest_features_file)
        files_saved['features_latest'] = latest_features_file
        
        self.logger.info(f"Alpha model data preparation completed")
        self.logger.info(f"Raw data: {len(economic_data)} observations, {len(economic_data.columns)} series")
        self.logger.info(f"Alpha features: {len(alpha_features)} observations, {len(alpha_features.columns)} features")
        
        return files_saved


def main():
    """Main function with support for command-line arguments."""
    import sys
    import argparse
    
    # Set up command line argument parsing
    parser = argparse.ArgumentParser(description='BEA Economic Data Connector')
    parser.add_argument('--comprehensive', action='store_true', 
                       help='Collect comprehensive historical data (20+ years)')
    parser.add_argument('--daily-update', action='store_true',
                       help='Collect daily update for all series (last 5 years)')
    parser.add_argument('--delta-update', action='store_true',
                       help='Collect delta update for critical series only (last 2 years)')
    parser.add_argument('--test', action='store_true',
                       help='Run basic test of BEA connector')
    
    args = parser.parse_args()
    
    try:
        # Initialize connector
        print("🏛️ Initializing BEA Connector...")
        bea = BEAConnector()
        
        # Handle comprehensive data collection
        if args.comprehensive:
            print("\n📊 Starting comprehensive historical data collection...")
            print("⏱️  This will collect 20+ years of data with 2-second throttling between calls")
            print("🕐 Estimated time: 5-10 minutes")
            
            # Ask for confirmation in interactive mode
            if sys.stdin.isatty():
                response = input("\nProceed with comprehensive data collection? (y/N): ")
                if response.lower() not in ['y', 'yes']:
                    print("❌ Comprehensive data collection cancelled.")
                    return
            
            collected_data = bea.collect_comprehensive_historical_data(
                start_year=2000,
                throttle_delay=2.0,
                save_to_data_dir=True
            )
            
            print(f"\n✅ Historical data collection completed!")
            print(f"📁 Data saved to: /workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/data/economic_indicators/bea/")
            return
        
        # Handle daily update
        elif args.daily_update:
            print(f"\n📅 Starting daily BEA data update - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
            print("⏱️  Collecting all important series for the last 5 years with 1-second throttling")
            print("🕐 Estimated time: 2-3 minutes")
            
            files_saved = bea.daily_update(save_to_data_dir=True)
            
            if files_saved:
                print(f"\n✅ Daily update completed at {datetime.now().strftime('%H:%M:%S')}")
                for file_type, file_path in files_saved.items():
                    print(f"   📁 {file_type}: {os.path.basename(file_path)}")
            else:
                print("❌ Daily update failed - no data collected")
            return
        
        # Handle delta update
        elif args.delta_update:
            print(f"\n⚡ Starting delta BEA data update - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
            print("⏱️  Collecting critical series for the last 2 years with 0.5-second throttling")
            print("🕐 Estimated time: 30-60 seconds")
            
            files_saved = bea.delta_update(save_to_data_dir=True)
            
            if files_saved:
                print(f"\n⚡ Delta update completed at {datetime.now().strftime('%H:%M:%S')}")
                for file_type, file_path in files_saved.items():
                    print(f"   📁 {file_type}: {os.path.basename(file_path)}")
                
                # Show latest critical values
                if 'critical_latest' in files_saved:
                    try:
                        latest_data = pd.read_csv(files_saved['critical_latest'], index_col=0, parse_dates=True)
                        if not latest_data.empty:
                            print("\n💹 Latest Critical Economic Indicators:")
                            latest_values = latest_data.iloc[-1]
                            for series_id, value in latest_values.items():
                                if not pd.isna(value):
                                    print(f"   {series_id}: {value:.2f}")
                    except Exception as e:
                        print(f"   ⚠️  Could not display latest values: {e}")
            else:
                print("❌ Delta update failed - no data collected")
            return
        
        # Handle test mode or default behavior
        elif args.test or len(sys.argv) == 1:
            # Test critical indicators collection
            print("\n📈 Testing BEA critical indicators collection...")
            critical_data = bea.get_critical_indicators(start_year=2020)
            
            if not critical_data.empty:
                print(f"   Retrieved {len(critical_data.columns)} series")
                print(f"   Date range: {critical_data.index.min()} to {critical_data.index.max()}")
                print(f"   Total observations: {len(critical_data)}")
                
                # Show latest values if available
                if not critical_data.empty:
                    print("\n💹 Latest BEA Economic Indicators:")
                    latest = critical_data.iloc[-1]
                    for series_id, value in latest.items():
                        if not pd.isna(value):
                            print(f"   {series_id}: {value:.2f}")
            
            # Create and save alpha model features
            print("\n🔧 Creating alpha model features...")
            files_saved = bea.save_data_for_alpha_models()
            
            if files_saved:
                print("\n💾 Files saved for alpha model integration:")
                for file_type, file_path in files_saved.items():
                    print(f"   {file_type}: {os.path.basename(file_path)}")
            
            print("\n✅ BEA Connector test completed successfully!")
            print("\n💡 Available commands:")
            print("   python bea_connector.py --comprehensive    # Full historical data collection")
            print("   python bea_connector.py --daily-update     # Daily update (all series)")
            print("   python bea_connector.py --delta-update     # Delta update (critical series)")
            print("   python bea_connector.py --test             # Run basic test")
        
        else:
            parser.print_help()
        
    except ValueError as e:
        print(f"❌ Configuration Error: {e}")
        print("\n💡 Setup Instructions:")
        print("1. Get free BEA API key: https://apps.bea.gov/API/signup/")
        print("2. Set environment variable: export BEA_API_KEY='your_key_here'")
        print("3. Or pass API key to BEAConnector(api_key='your_key_here')")
        
    except ImportError as e:
        print(f"❌ Library Error: {e}")
        print("💡 Install required library: pip install beaapi pandas")
        
    except Exception as e:
        print(f"❌ Unexpected Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
