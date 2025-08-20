"""
Feature list generation and management for machine learning models.

This module handles:
- Loading and managing feature lists for different portfolios
- Dynamic feature selection based on data availability
- Master feature list management
- Feature correlation analysis

Migrated from: BackendPython/datasetcreation/Generatefeatureslist.R
"""

import os
import logging
from typing import List, Dict, Set, Tuple, Optional
import pandas as pd
import numpy as np
from dataclasses import dataclass
import json
from pathlib import Path

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class FeatureConfig:
    """Configuration for feature list generation."""
    master_feature_file: str = "data/exchangedata/master_featurelist.csv"
    correlation_threshold: float = 0.8
    min_data_availability: float = 0.7
    max_features: int = 100
    exclude_currencies: bool = False

class FeatureListManager:
    """Manages feature lists for different portfolios and use cases."""
    
    def __init__(self, config: FeatureConfig = None):
        """Initialize with configuration."""
        self.config = config or FeatureConfig()
        self.master_features = None
        self.available_data_symbols = set()
        self.correlation_matrix = None
    
    def load_master_feature_list(self) -> List[str]:
        """
        Load the master feature list from CSV file.
        
        Returns:
            List of feature symbols
        """
        try:
            if not os.path.exists(self.config.master_feature_file):
                logger.warning(f"Master feature file not found: {self.config.master_feature_file}")
                return self._create_default_feature_list()
            
            df = pd.read_csv(self.config.master_feature_file)
            
            # Assuming the CSV has a column with feature symbols
            if 'symbol' in df.columns:
                features = df['symbol'].dropna().unique().tolist()
            elif 'Symbol' in df.columns:
                features = df['Symbol'].dropna().unique().tolist()
            else:
                # Take the first column
                features = df.iloc[:, 0].dropna().unique().tolist()
            
            # Filter out currencies if configured
            if self.config.exclude_currencies:
                features = [f for f in features if not self._is_currency_symbol(f)]
            
            self.master_features = features
            logger.info(f"Loaded {len(features)} features from master list")
            
            return features
            
        except Exception as e:
            logger.error(f"Error loading master feature list: {e}")
            return self._create_default_feature_list()
    
    def _create_default_feature_list(self) -> List[str]:
        """Create a default feature list if master list is not available."""
        default_features = [
            # Major tech stocks
            'AAPL', 'GOOGL', 'MSFT', 'AMZN', 'TSLA', 'META', 'NVDA', 'NFLX',
            # Financial sector
            'JPM', 'BAC', 'WFC', 'GS', 'MS',
            # Healthcare
            'JNJ', 'PFE', 'UNH', 'ABBV', 'MRK',
            # Energy
            'XOM', 'CVX', 'COP', 'SLB',
            # Consumer
            'KO', 'PEP', 'WMT', 'HD', 'DIS',
            # Industrial
            'BA', 'CAT', 'GE', 'HON',
            # ETFs
            'SPY', 'QQQ', 'IWM', 'VTI', 'VEA',
            # Indices (if available)
            '^GSPC', '^IXIC', '^DJI', '^RUT'
        ]
        
        logger.info(f"Using default feature list with {len(default_features)} symbols")
        return default_features
    
    def _is_currency_symbol(self, symbol: str) -> bool:
        """Check if a symbol represents a currency pair."""
        currency_indicators = ['=X', 'USD', 'EUR', 'GBP', 'JPY', 'CHF', 'CAD', 'AUD']
        return any(indicator in symbol.upper() for indicator in currency_indicators)
    
    def scan_available_data(self, data_dir: str = "data/stockdata") -> Set[str]:
        """
        Scan directory to find available stock data files.
        
        Args:
            data_dir: Directory containing stock data
            
        Returns:
            Set of symbols with available data
        """
        try:
            available_symbols = set()
            
            if not os.path.exists(data_dir):
                logger.warning(f"Data directory not found: {data_dir}")
                return available_symbols
            
            # Look for subdirectories (each symbol has its own folder)
            for item in os.listdir(data_dir):
                item_path = os.path.join(data_dir, item)
                
                if os.path.isdir(item_path):
                    # Check if stockdata.csv exists in this directory
                    csv_file = os.path.join(item_path, "stockdata.csv")
                    if os.path.exists(csv_file):
                        # Verify the file has data
                        try:
                            df = pd.read_csv(csv_file)
                            if len(df) > 0:
                                available_symbols.add(item)
                        except Exception:
                            logger.debug(f"Could not read data file for {item}")
            
            # Also look for CSV files directly in the data directory
            csv_files = [f for f in os.listdir(data_dir) if f.endswith('.csv')]
            for csv_file in csv_files:
                symbol = csv_file.replace('.csv', '')
                available_symbols.add(symbol)
            
            self.available_data_symbols = available_symbols
            logger.info(f"Found data for {len(available_symbols)} symbols")
            
            return available_symbols
            
        except Exception as e:
            logger.error(f"Error scanning available data: {e}")
            return set()
    
    def generate_feature_list(self, 
                            portfolio_name: str,
                            user_id: int = 1,
                            max_features: int = None,
                            data_dir: str = "data/stockdata") -> List[str]:
        """
        Generate optimal feature list for a specific portfolio.
        
        Args:
            portfolio_name: Name of the portfolio
            user_id: User ID for portfolio-specific features
            max_features: Maximum number of features to return
            data_dir: Directory with stock data
            
        Returns:
            List of selected feature symbols
        """
        try:
            # Load master feature list if not already loaded
            if self.master_features is None:
                self.load_master_feature_list()
            
            # Scan for available data
            self.scan_available_data(data_dir)
            
            # Filter features based on data availability
            available_features = [f for f in self.master_features 
                                if f in self.available_data_symbols]
            
            logger.info(f"Available features after data filtering: {len(available_features)}")
            
            # Apply additional filtering based on portfolio requirements
            selected_features = self._apply_portfolio_specific_filtering(
                available_features, portfolio_name, user_id
            )
            
            # Limit number of features if specified
            max_feat = max_features or self.config.max_features
            if len(selected_features) > max_feat:
                selected_features = selected_features[:max_feat]
                logger.info(f"Limited features to {max_feat}")
            
            logger.info(f"Final feature list contains {len(selected_features)} symbols")
            
            return selected_features
            
        except Exception as e:
            logger.error(f"Error generating feature list: {e}")
            return []
    
    def _apply_portfolio_specific_filtering(self, 
                                          features: List[str], 
                                          portfolio_name: str, 
                                          user_id: int) -> List[str]:
        """
        Apply portfolio-specific filtering logic.
        
        Args:
            features: List of available features
            portfolio_name: Portfolio name
            user_id: User ID
            
        Returns:
            Filtered feature list
        """
        # Different filtering strategies based on portfolio type
        if "tech" in portfolio_name.lower():
            # For tech portfolios, prioritize tech stocks
            tech_symbols = [f for f in features if f in [
                'AAPL', 'GOOGL', 'MSFT', 'AMZN', 'TSLA', 'META', 'NVDA', 'NFLX',
                'ADBE', 'CRM', 'ORCL', 'INTC', 'AMD', 'CSCO'
            ]]
            # Add market indices and other relevant symbols
            other_symbols = [f for f in features if f in [
                'SPY', 'QQQ', '^GSPC', '^IXIC', 'VTI'
            ]]
            return tech_symbols + other_symbols + [f for f in features 
                                                   if f not in tech_symbols + other_symbols][:20]
        
        elif "dividend" in portfolio_name.lower():
            # For dividend portfolios, prioritize dividend-paying stocks
            dividend_symbols = [f for f in features if f in [
                'JNJ', 'KO', 'PEP', 'PG', 'T', 'VZ', 'XOM', 'CVX',
                'JPM', 'BAC', 'WMT', 'HD', 'MCD', 'IBM'
            ]]
            return dividend_symbols + [f for f in features 
                                      if f not in dividend_symbols][:30]
        
        elif "growth" in portfolio_name.lower():
            # For growth portfolios, prioritize growth stocks
            growth_symbols = [f for f in features if f in [
                'AMZN', 'TSLA', 'NVDA', 'NFLX', 'GOOGL', 'META', 'ADBE',
                'CRM', 'SHOP', 'ROKU', 'TWLO', 'ZM', 'SNAP'
            ]]
            return growth_symbols + [f for f in features 
                                    if f not in growth_symbols][:25]
        
        else:
            # Default: balanced approach
            # Prioritize liquid, well-known stocks
            priority_symbols = [f for f in features if f in [
                'AAPL', 'MSFT', 'GOOGL', 'AMZN', 'TSLA', 'META', 'NVDA',
                'JPM', 'JNJ', 'KO', 'SPY', 'QQQ', '^GSPC'
            ]]
            return priority_symbols + [f for f in features 
                                      if f not in priority_symbols][:40]
    
    def analyze_feature_correlation(self, 
                                   features: List[str], 
                                   data_dir: str = "data/stockdata") -> pd.DataFrame:
        """
        Analyze correlation between features to identify redundant ones.
        
        Args:
            features: List of feature symbols
            data_dir: Directory with stock data
            
        Returns:
            Correlation matrix DataFrame
        """
        try:
            # Load data for all features
            feature_data = {}
            
            for symbol in features:
                try:
                    file_path = os.path.join(data_dir, symbol, "stockdata.csv")
                    if os.path.exists(file_path):
                        df = pd.read_csv(file_path, index_col=0, parse_dates=True)
                        
                        # Use adjusted close or close price
                        if f'{symbol}.Adjusted' in df.columns:
                            feature_data[symbol] = df[f'{symbol}.Adjusted']
                        elif 'Adj Close' in df.columns:
                            feature_data[symbol] = df['Adj Close']
                        elif 'Close' in df.columns:
                            feature_data[symbol] = df['Close']
                            
                except Exception:
                    logger.debug(f"Could not load correlation data for {symbol}")
            
            if not feature_data:
                logger.warning("No data available for correlation analysis")
                return pd.DataFrame()
            
            # Combine into DataFrame and calculate percentage changes
            combined_df = pd.DataFrame(feature_data)
            pct_changes = combined_df.pct_change().dropna()
            
            # Calculate correlation matrix
            correlation_matrix = pct_changes.corr()
            self.correlation_matrix = correlation_matrix
            
            logger.info(f"Calculated correlation matrix for {len(features)} features")
            
            return correlation_matrix
            
        except Exception as e:
            logger.error(f"Error analyzing feature correlation: {e}")
            return pd.DataFrame()
    
    def remove_highly_correlated_features(self, 
                                        features: List[str],
                                        threshold: float = None) -> List[str]:
        """
        Remove highly correlated features to reduce redundancy.
        
        Args:
            features: List of feature symbols
            threshold: Correlation threshold (default from config)
            
        Returns:
            List of features with high correlations removed
        """
        try:
            threshold = threshold or self.config.correlation_threshold
            
            # Analyze correlations if not already done
            if self.correlation_matrix is None:
                self.analyze_feature_correlation(features)
            
            if self.correlation_matrix is None or self.correlation_matrix.empty:
                logger.warning("No correlation data available")
                return features
            
            # Find highly correlated pairs
            corr_matrix = self.correlation_matrix
            upper_triangle = corr_matrix.where(
                np.triu(np.ones(corr_matrix.shape), k=1).astype(bool)
            )
            
            # Find features to remove
            to_remove = set()
            for column in upper_triangle.columns:
                correlated_features = upper_triangle.index[
                    abs(upper_triangle[column]) > threshold
                ].tolist()
                
                if correlated_features:
                    # Keep the first feature, remove others
                    to_remove.update(correlated_features)
            
            # Filter out highly correlated features
            filtered_features = [f for f in features if f not in to_remove]
            
            logger.info(f"Removed {len(to_remove)} highly correlated features")
            logger.info(f"Remaining features: {len(filtered_features)}")
            
            return filtered_features
            
        except Exception as e:
            logger.error(f"Error removing correlated features: {e}")
            return features
    
    def save_feature_list(self, 
                         features: List[str], 
                         portfolio_name: str,
                         user_id: int,
                         output_dir: str = "config") -> str:
        """
        Save feature list to file for later use.
        
        Args:
            features: List of feature symbols
            portfolio_name: Portfolio name
            user_id: User ID
            output_dir: Output directory
            
        Returns:
            Path to saved file
        """
        try:
            os.makedirs(output_dir, exist_ok=True)
            
            filename = f"features_{user_id}_{portfolio_name}.json"
            filepath = os.path.join(output_dir, filename)
            
            feature_config = {
                'user_id': user_id,
                'portfolio_name': portfolio_name,
                'features': features,
                'created_date': pd.Timestamp.now().isoformat(),
                'feature_count': len(features)
            }
            
            with open(filepath, 'w') as f:
                json.dump(feature_config, f, indent=2)
            
            logger.info(f"Saved feature list to {filepath}")
            
            return filepath
            
        except Exception as e:
            logger.error(f"Error saving feature list: {e}")
            return ""
    
    def load_feature_list(self, 
                         portfolio_name: str,
                         user_id: int,
                         config_dir: str = "config") -> List[str]:
        """
        Load previously saved feature list.
        
        Args:
            portfolio_name: Portfolio name
            user_id: User ID
            config_dir: Configuration directory
            
        Returns:
            List of feature symbols
        """
        try:
            filename = f"features_{user_id}_{portfolio_name}.json"
            filepath = os.path.join(config_dir, filename)
            
            if not os.path.exists(filepath):
                logger.info(f"No saved feature list found for {portfolio_name}")
                return []
            
            with open(filepath, 'r') as f:
                feature_config = json.load(f)
            
            features = feature_config.get('features', [])
            logger.info(f"Loaded {len(features)} features for {portfolio_name}")
            
            return features
            
        except Exception as e:
            logger.error(f"Error loading feature list: {e}")
            return []

# Example usage
if __name__ == "__main__":
    manager = FeatureListManager()
    
    # Generate feature list for a tech portfolio
    features = manager.generate_feature_list("tech_portfolio", user_id=1, max_features=50)
    print(f"Generated {len(features)} features for tech portfolio")
    
    # Analyze correlations
    if features:
        corr_matrix = manager.analyze_feature_correlation(features[:10])  # Sample
        print(f"Correlation matrix shape: {corr_matrix.shape}")
        
        # Remove highly correlated features
        filtered_features = manager.remove_highly_correlated_features(features[:10])
        print(f"Features after correlation filtering: {len(filtered_features)}")
        
        # Save feature list
        saved_path = manager.save_feature_list(filtered_features, "tech_portfolio", 1)
        print(f"Saved to: {saved_path}")
