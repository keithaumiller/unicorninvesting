"""
Economic Indicators Integration Module for Alpha Models

This module provides functionality to integrate bronze layer economic indicators 
into crypto alpha models, specifically for ETH and BTC predictions.

The module handles:
- Loading and preprocessing bronze layer economic datasets
- Temporal alignment with crypto price data
- Feature selection and dimensionality reduction
- Real-time indicator updates for live trading
"""

import os
import sys
import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple, Union
import warnings
from datetime import datetime, timedelta
from pathlib import Path
import glob

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
alpha_models_dir = os.path.dirname(os.path.dirname(current_dir))
sys.path.append(alpha_models_dir)

try:
    from sklearn.feature_selection import SelectKBest, f_regression, mutual_info_regression
    from sklearn.decomposition import PCA
    from sklearn.preprocessing import StandardScaler, MinMaxScaler
    SKLEARN_AVAILABLE = True
except ImportError:
    print("Warning: scikit-learn not available. Install with: pip install scikit-learn")
    SKLEARN_AVAILABLE = False

warnings.filterwarnings('ignore')

class EconomicIndicatorsIntegrator:
    """
    Integration layer for bronze layer economic indicators in crypto alpha models.
    
    Features:
    - Multi-category economic indicator loading (growth, consumer, trade, monetary)
    - Temporal alignment with crypto price data
    - Feature selection and dimensionality reduction
    - Real-time indicator updates
    - Caching for performance optimization
    """
    
    def __init__(self, bronze_layer_path: str = None):
        """
        Initialize the economic indicators integrator.
        
        Args:
            bronze_layer_path: Path to bronze layer processed data
        """
        self.bronze_layer_path = bronze_layer_path or "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/economic_indicators/processed_data"
        self.cache = {}
        self.scaler = StandardScaler()
        self.feature_selector = None
        
        # Economic indicator categories
        self.categories = {
            'economic_growth': 'macroeconomic indicators',
            'consumer_business': 'consumer spending and business activity',
            'international_trade': 'trade balance and international commerce',
            'monetary_policy': 'interest rates and monetary policy'
        }
        
    def load_latest_indicators(self, timeframe: str = '1_day') -> Dict[str, pd.DataFrame]:
        """
        Load the latest bronze layer economic indicators.
        
        Args:
            timeframe: Time interval ('1_day', '1_hour', '1_minute')
            
        Returns:
            Dictionary of DataFrames by category
        """
        print(f"📊 Loading economic indicators for {timeframe} timeframe...")
        
        indicators = {}
        data_path = os.path.join(self.bronze_layer_path, timeframe)
        
        if not os.path.exists(data_path):
            print(f"⚠️  Warning: Bronze layer path does not exist: {data_path}")
            return indicators
        
        for category in self.categories.keys():
            # Find the latest file for each category
            pattern = os.path.join(data_path, f"{category}_{timeframe}_*.csv")
            files = glob.glob(pattern)
            
            if files:
                # Get the most recent file
                latest_file = max(files, key=os.path.getctime)
                try:
                    df = pd.read_csv(latest_file)
                    
                    # Convert Date column to datetime if it exists
                    if 'Date' in df.columns:
                        df['Date'] = pd.to_datetime(df['Date'])
                        df = df.set_index('Date')
                    elif 'timestamp' in df.columns:
                        df['timestamp'] = pd.to_datetime(df['timestamp'])
                        df = df.set_index('timestamp')
                    
                    indicators[category] = df
                    print(f"✅ Loaded {category}: {df.shape[0]} observations, {df.shape[1]} features")
                    
                except Exception as e:
                    print(f"❌ Error loading {category}: {e}")
                    
            else:
                print(f"⚠️  No files found for {category}")
        
        return indicators
    
    def select_key_features(self, indicators: Dict[str, pd.DataFrame], 
                          n_features_per_category: int = 20,
                          method: str = 'variance') -> Dict[str, List[str]]:
        """
        Select key economic features for alpha model integration.
        
        Args:
            indicators: Dictionary of indicator DataFrames
            n_features_per_category: Number of features to select per category
            method: Feature selection method ('variance', 'mutual_info', 'correlation')
            
        Returns:
            Dictionary of selected feature names by category
        """
        print(f"🎯 Selecting key features using {method} method...")
        
        selected_features = {}
        
        for category, df in indicators.items():
            if df.empty:
                selected_features[category] = []
                continue
            
            # Remove non-numeric columns
            numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
            
            if len(numeric_cols) == 0:
                selected_features[category] = []
                continue
                
            df_numeric = df[numeric_cols]
            
            if method == 'variance':
                # Select features with highest variance (most dynamic)
                variances = df_numeric.var().sort_values(ascending=False)
                top_features = variances.head(n_features_per_category).index.tolist()
                
            elif method == 'mutual_info' and len(df_numeric) > 1:
                # Use mutual information (requires target variable approximation)
                # For now, use the first numeric column as proxy target
                if len(numeric_cols) > 1:
                    target_col = numeric_cols[0]
                    feature_cols = numeric_cols[1:]
                    
                    X = df_numeric[feature_cols].fillna(0)
                    y = df_numeric[target_col].fillna(0)
                    
                    if len(X.columns) > 0 and not X.empty and not y.empty:
                        mi_scores = mutual_info_regression(X, y)
                        feature_scores = pd.Series(mi_scores, index=feature_cols)
                        top_features = feature_scores.nlargest(n_features_per_category).index.tolist()
                    else:
                        top_features = numeric_cols[:n_features_per_category]
                else:
                    top_features = numeric_cols[:n_features_per_category]
                    
            elif method == 'correlation':
                # Select features with low correlation to each other (diverse information)
                corr_matrix = df_numeric.corr().abs()
                upper_tri = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(bool))
                
                # Find features with low average correlation
                avg_corr = upper_tri.mean(axis=1, skipna=True)
                top_features = avg_corr.nsmallest(n_features_per_category).index.tolist()
                
            else:
                # Default to first n features
                top_features = numeric_cols[:n_features_per_category]
            
            selected_features[category] = top_features
            print(f"✅ {category}: Selected {len(top_features)} features")
        
        return selected_features
    
    def align_with_crypto_data(self, crypto_df: pd.DataFrame, 
                              indicators: Dict[str, pd.DataFrame],
                              selected_features: Dict[str, List[str]] = None) -> pd.DataFrame:
        """
        Align economic indicators with crypto price data temporally.
        
        Args:
            crypto_df: Crypto price DataFrame with datetime index
            indicators: Dictionary of indicator DataFrames
            selected_features: Selected features per category
            
        Returns:
            Combined DataFrame with crypto data and aligned economic indicators
        """
        print("🔄 Aligning economic indicators with crypto data...")
        
        # Ensure crypto_df has datetime index
        if not isinstance(crypto_df.index, pd.DatetimeIndex):
            if 'date' in crypto_df.columns:
                crypto_df['date'] = pd.to_datetime(crypto_df['date'])
                crypto_df = crypto_df.set_index('date')
            else:
                print("⚠️  Warning: Could not convert crypto data index to datetime")
                return crypto_df
        
        combined_df = crypto_df.copy()
        
        for category, df in indicators.items():
            if df.empty:
                continue
                
            # Use selected features if provided, otherwise use all numeric features
            if selected_features and category in selected_features:
                feature_cols = selected_features[category]
            else:
                feature_cols = df.select_dtypes(include=[np.number]).columns.tolist()
            
            if not feature_cols:
                continue
            
            # Select only the feature columns
            df_features = df[feature_cols].copy()
            
            # Add category prefix to column names to avoid conflicts
            df_features.columns = [f"{category}_{col}" for col in df_features.columns]
            
        # Align timestamps using forward fill for economic data (since it's typically lower frequency)
        if isinstance(df_features.index, pd.DatetimeIndex):
            # Resample economic data to match crypto frequency, forward filling
            aligned_indicators = df_features.reindex(combined_df.index, method='ffill')
            
            # Merge with combined dataframe
            combined_df = pd.concat([combined_df, aligned_indicators], axis=1)
            
            print(f"✅ Aligned {category}: {len(feature_cols)} features")
        else:
            print(f"⚠️  Warning: Could not align {category} - no datetime index")
    
        # Fill any remaining NaN values with forward fill, then backward fill, then zero
        combined_df = combined_df.fillna(method='ffill').fillna(method='bfill').fillna(0)
        
        print(f"📊 Final combined dataset: {combined_df.shape[0]} observations, {combined_df.shape[1]} features")
        
        return combined_df
    
    def create_economic_features_summary(self, combined_df: pd.DataFrame) -> Dict[str, Any]:
        """
        Create a summary of economic features added to the model.
        
        Args:
            combined_df: Combined DataFrame with economic features
            
        Returns:
            Dictionary with feature summary statistics
        """
        economic_cols = [col for col in combined_df.columns 
                        if any(category in col for category in self.categories.keys())]
        
        if not economic_cols:
            return {"message": "No economic features found"}
        
        summary = {
            "total_economic_features": len(economic_cols),
            "features_by_category": {},
            "feature_statistics": {},
            "data_coverage": {}
        }
        
        for category in self.categories.keys():
            category_cols = [col for col in economic_cols if col.startswith(f"{category}_")]
            summary["features_by_category"][category] = {
                "count": len(category_cols),
                "features": category_cols
            }
            
            if category_cols:
                category_data = combined_df[category_cols]
                summary["feature_statistics"][category] = {
                    "mean_correlation": category_data.corr().abs().mean().mean(),
                    "coverage_percentage": (1 - category_data.isnull().sum().sum() / (len(category_data) * len(category_cols))) * 100
                }
        
        return summary
    
    def get_latest_indicators_for_prediction(self, timeframe: str = '1_day', 
                                           n_features_per_category: int = 15) -> Tuple[pd.DataFrame, Dict[str, Any]]:
        """
        Get the latest economic indicators formatted for model prediction.
        
        Args:
            timeframe: Time interval for indicators
            n_features_per_category: Number of features to select per category
            
        Returns:
            Tuple of (latest_indicators_df, metadata)
        """
        print("🔄 Preparing latest economic indicators for prediction...")
        
        # Load latest indicators
        indicators = self.load_latest_indicators(timeframe)
        
        if not indicators:
            return pd.DataFrame(), {"error": "No indicators loaded"}
        
        # Select key features
        selected_features = self.select_key_features(indicators, n_features_per_category, method='variance')
        
        # Combine indicators into a single row (latest values)
        latest_row = {}
        metadata = {"categories": {}, "timestamp": datetime.now().isoformat()}
        
        for category, df in indicators.items():
            if df.empty:
                continue
                
            feature_cols = selected_features.get(category, [])
            if not feature_cols:
                continue
            
            # Get the latest row of data
            latest_data = df[feature_cols].iloc[-1] if len(df) > 0 else pd.Series()
            
            # Add category prefix
            for col in feature_cols:
                if col in latest_data.index:
                    latest_row[f"{category}_{col}"] = latest_data[col]
            
            metadata["categories"][category] = {
                "features_count": len(feature_cols),
                "latest_date": df.index[-1].isoformat() if hasattr(df.index[-1], 'isoformat') else str(df.index[-1]),
                "data_shape": df.shape
            }
        
        # Convert to DataFrame
        if latest_row:
            latest_df = pd.DataFrame([latest_row])
            latest_df.index = [datetime.now()]
        else:
            latest_df = pd.DataFrame()
        
        print(f"✅ Prepared {len(latest_row)} economic features for prediction")
        
        return latest_df, metadata

# Convenience functions for easy integration

def integrate_economic_indicators_into_eth_model(crypto_df: pd.DataFrame, 
                                               n_features_per_category: int = 20) -> Tuple[pd.DataFrame, Dict[str, Any]]:
    """
    Convenience function to integrate economic indicators into ETH model data.
    
    Args:
        crypto_df: ETH price DataFrame
        n_features_per_category: Number of features to select per category
        
    Returns:
        Tuple of (enhanced_dataframe, integration_summary)
    """
    integrator = EconomicIndicatorsIntegrator()
    
    # Load indicators
    indicators = integrator.load_latest_indicators('1_day')
    
    # Select key features
    selected_features = integrator.select_key_features(indicators, n_features_per_category)
    
    # Align with crypto data
    enhanced_df = integrator.align_with_crypto_data(crypto_df, indicators, selected_features)
    
    # Create summary
    summary = integrator.create_economic_features_summary(enhanced_df)
    
    return enhanced_df, summary

def get_economic_indicators_for_live_trading() -> Tuple[pd.DataFrame, Dict[str, Any]]:
    """
    Get the latest economic indicators for live trading scenarios.
    
    Returns:
        Tuple of (latest_indicators_df, metadata)
    """
    integrator = EconomicIndicatorsIntegrator()
    return integrator.get_latest_indicators_for_prediction('1_day', n_features_per_category=10)
