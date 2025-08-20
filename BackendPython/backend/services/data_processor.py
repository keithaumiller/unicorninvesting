"""
Data preprocessing and feature engineering for machine learning models.

This module handles:
- Combining multiple stock datasets into unified training matrices
- Percentage change calculations and normalization
- Training/evaluation dataset splitting
- Data cleaning and missing value handling

Migrated from: BackendPython/datasetcreation/Combinestocks.R
"""

import os
import logging
from typing import List, Dict, Tuple, Optional
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import glob
from dataclasses import dataclass

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class DatasetConfig:
    """Configuration for dataset creation."""
    train_split_ratio: float = 0.75
    min_data_points: int = 252  # Minimum trading days required
    fill_na_method: str = 'forward'  # 'forward', 'backward', 'zero', 'drop'
    normalize_data: bool = True

class StockDataProcessor:
    """Handles stock data combination and preprocessing for ML models."""
    
    def __init__(self, config: DatasetConfig = None):
        """Initialize with configuration."""
        self.config = config or DatasetConfig()
        self.combined_data = None
        self.portfolio_columns = None
        self.feature_columns = None
    
    def load_stock_data(self, symbol: str, data_dir: str = "data/stockdata") -> pd.DataFrame:
        """
        Load individual stock data from CSV file.
        
        Args:
            symbol: Stock symbol
            data_dir: Base directory for stock data
            
        Returns:
            DataFrame with stock data, empty if file not found
        """
        try:
            file_path = os.path.join(data_dir, symbol, "stockdata.csv")
            
            if not os.path.exists(file_path):
                logger.warning(f"Data file not found for {symbol}: {file_path}")
                return pd.DataFrame()
            
            # Load data with date as index
            df = pd.read_csv(file_path, index_col=0, parse_dates=True)
            
            # Ensure we have the expected columns
            expected_cols = [f'{symbol}.Open', f'{symbol}.High', f'{symbol}.Low', 
                           f'{symbol}.Close', f'{symbol}.Volume', f'{symbol}.Adjusted']
            
            # If columns don't match expected format, try to standardize
            if not any(col in df.columns for col in expected_cols):
                if 'Adj Close' in df.columns:
                    df[f'{symbol}.Adjusted'] = df['Adj Close']
                elif 'Close' in df.columns:
                    df[f'{symbol}.Adjusted'] = df['Close']
                else:
                    logger.error(f"Unable to find price data for {symbol}")
                    return pd.DataFrame()
            
            logger.debug(f"Loaded {len(df)} records for {symbol}")
            return df
            
        except Exception as e:
            logger.error(f"Error loading data for {symbol}: {e}")
            return pd.DataFrame()
    
    def calculate_percentage_changes(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate percentage changes for price data.
        
        Args:
            df: DataFrame with price data
            
        Returns:
            DataFrame with percentage changes
        """
        try:
            # Calculate percentage change (current / previous - 1)
            pct_change_df = df.pct_change()
            
            # Replace infinite values with 0
            pct_change_df = pct_change_df.replace([np.inf, -np.inf], 0)
            
            # Handle NaN values based on configuration
            if self.config.fill_na_method == 'zero':
                pct_change_df = pct_change_df.fillna(0)
            elif self.config.fill_na_method == 'forward':
                pct_change_df = pct_change_df.fillna(method='ffill')
            elif self.config.fill_na_method == 'backward':
                pct_change_df = pct_change_df.fillna(method='bfill')
            elif self.config.fill_na_method == 'drop':
                pct_change_df = pct_change_df.dropna()
            
            return pct_change_df
            
        except Exception as e:
            logger.error(f"Error calculating percentage changes: {e}")
            return pd.DataFrame()
    
    def combine_stocks_data(self, 
                           feature_list: List[str], 
                           portfolio_list: List[str],
                           max_stocks: int = 0,
                           data_dir: str = "data/stockdata") -> Tuple[pd.DataFrame, List[str]]:
        """
        Combine multiple stock datasets into unified training matrix.
        
        Args:
            feature_list: List of symbols to use as features
            portfolio_list: List of symbols in the portfolio
            max_stocks: Maximum number of stocks to include (0 = no limit)
            data_dir: Directory containing stock data
            
        Returns:
            Tuple of (combined_dataframe, successfully_loaded_symbols)
        """
        try:
            all_symbols = list(set(feature_list + portfolio_list))
            
            if max_stocks > 0:
                all_symbols = all_symbols[:max_stocks]
            
            logger.info(f"Attempting to combine data for {len(all_symbols)} symbols")
            
            combined_data = {}
            successful_symbols = []
            
            for symbol in all_symbols:
                df = self.load_stock_data(symbol, data_dir)
                
                if not df.empty:
                    # Use adjusted close price for percentage change calculation
                    adj_col = f'{symbol}.Adjusted'
                    if adj_col in df.columns:
                        combined_data[symbol] = df[adj_col]
                        successful_symbols.append(symbol)
                    else:
                        logger.warning(f"No adjusted close data for {symbol}")
                else:
                    logger.warning(f"Skipping {symbol} - no valid data")
            
            if not combined_data:
                logger.error("No valid stock data found")
                return pd.DataFrame(), []
            
            # Combine all data into single DataFrame
            combined_df = pd.DataFrame(combined_data)
            
            # Remove rows where all values are NaN
            combined_df = combined_df.dropna(how='all')
            
            # Forward fill missing values
            combined_df = combined_df.fillna(method='ffill')
            
            # Calculate percentage changes
            pct_change_df = self.calculate_percentage_changes(combined_df)
            
            # Store for later use
            self.combined_data = pct_change_df
            self.feature_columns = [col for col in successful_symbols if col in feature_list]
            self.portfolio_columns = [col for col in successful_symbols if col in portfolio_list]
            
            logger.info(f"Successfully combined data for {len(successful_symbols)} symbols")
            logger.info(f"Data shape: {pct_change_df.shape}")
            logger.info(f"Date range: {pct_change_df.index.min()} to {pct_change_df.index.max()}")
            
            return pct_change_df, successful_symbols
            
        except Exception as e:
            logger.error(f"Error combining stock data: {e}")
            return pd.DataFrame(), []
    
    def generate_training_targets(self, portfolio_data: pd.DataFrame, method: str = "top_movers") -> pd.DataFrame:
        """
        Generate training targets for neural network.
        
        Args:
            portfolio_data: DataFrame with portfolio stock percentage changes
            method: Method for generating targets ('top_movers', 'momentum', 'mean_reversion')
            
        Returns:
            DataFrame with target allocations
        """
        try:
            if method == "top_movers":
                return self._generate_top_movers_targets(portfolio_data)
            elif method == "momentum":
                return self._generate_momentum_targets(portfolio_data)
            elif method == "mean_reversion":
                return self._generate_mean_reversion_targets(portfolio_data)
            else:
                logger.error(f"Unknown target generation method: {method}")
                return pd.DataFrame()
                
        except Exception as e:
            logger.error(f"Error generating training targets: {e}")
            return pd.DataFrame()
    
    def _generate_top_movers_targets(self, portfolio_data: pd.DataFrame) -> pd.DataFrame:
        """
        Generate targets based on top positive movers strategy.
        
        Args:
            portfolio_data: DataFrame with portfolio percentage changes
            
        Returns:
            DataFrame with allocation targets
        """
        # Allocation weights for top performers
        allocation_weights = [0.25, 0.15, 0.10, 0.10, 0.10, 0.10, 0.10, 0.05, 0.03, 0.02]
        
        training_targets = pd.DataFrame(
            np.zeros_like(portfolio_data), 
            index=portfolio_data.index, 
            columns=portfolio_data.columns
        )
        
        for date_idx in portfolio_data.index:
            daily_returns = portfolio_data.loc[date_idx]
            
            # Sort by returns (highest first)
            sorted_returns = daily_returns.sort_values(ascending=False)
            
            # Allocate weights to top performers
            for i, (symbol, _) in enumerate(sorted_returns.items()):
                if i < len(allocation_weights):
                    training_targets.loc[date_idx, symbol] = allocation_weights[i]
                else:
                    break
        
        return training_targets
    
    def _generate_momentum_targets(self, portfolio_data: pd.DataFrame) -> pd.DataFrame:
        """Generate targets based on momentum strategy."""
        # Simple momentum: allocate based on recent performance
        window = 5  # 5-day momentum
        
        momentum = portfolio_data.rolling(window=window).mean()
        
        # Normalize to sum to 1 for each day
        targets = momentum.div(momentum.sum(axis=1), axis=0)
        targets = targets.fillna(0)
        
        return targets
    
    def _generate_mean_reversion_targets(self, portfolio_data: pd.DataFrame) -> pd.DataFrame:
        """Generate targets based on mean reversion strategy."""
        # Mean reversion: allocate more to recent underperformers
        window = 10  # 10-day average
        
        rolling_mean = portfolio_data.rolling(window=window).mean()
        current_vs_mean = portfolio_data - rolling_mean
        
        # Invert: allocate more to stocks below their mean
        mean_reversion_signal = -current_vs_mean
        
        # Normalize to sum to 1
        targets = mean_reversion_signal.div(mean_reversion_signal.sum(axis=1), axis=0)
        targets = targets.fillna(0)
        
        # Ensure no negative allocations
        targets = targets.clip(lower=0)
        
        # Renormalize after clipping
        targets = targets.div(targets.sum(axis=1), axis=0)
        targets = targets.fillna(0)
        
        return targets
    
    def split_train_test(self, data: pd.DataFrame) -> Tuple[pd.DataFrame, pd.DataFrame]:
        """
        Split data into training and testing sets based on temporal order.
        
        Args:
            data: Input DataFrame with time series data
            
        Returns:
            Tuple of (training_data, testing_data)
        """
        try:
            split_point = int(len(data) * self.config.train_split_ratio)
            
            train_data = data.iloc[:split_point]
            test_data = data.iloc[split_point:]
            
            logger.info(f"Train set: {len(train_data)} samples, Test set: {len(test_data)} samples")
            
            return train_data, test_data
            
        except Exception as e:
            logger.error(f"Error splitting data: {e}")
            return pd.DataFrame(), pd.DataFrame()
    
    def create_ml_datasets(self, 
                          feature_list: List[str], 
                          portfolio_list: List[str],
                          max_stocks: int = 0,
                          target_method: str = "top_movers") -> Dict[str, pd.DataFrame]:
        """
        Create complete ML datasets ready for training.
        
        Args:
            feature_list: List of feature symbols
            portfolio_list: List of portfolio symbols
            max_stocks: Maximum stocks to include
            target_method: Method for generating training targets
            
        Returns:
            Dictionary with train/test features and targets
        """
        try:
            # Combine stock data
            combined_data, successful_symbols = self.combine_stocks_data(
                feature_list, portfolio_list, max_stocks
            )
            
            if combined_data.empty:
                logger.error("No data available for ML dataset creation")
                return {}
            
            # Extract features (all available columns)
            feature_data = combined_data[self.feature_columns]
            
            # Extract portfolio data for target generation
            portfolio_data = combined_data[self.portfolio_columns]
            
            # Generate training targets
            target_data = self.generate_training_targets(portfolio_data, target_method)
            
            # Split into train/test sets
            X_train, X_test = self.split_train_test(feature_data)
            y_train, y_test = self.split_train_test(target_data)
            
            datasets = {
                'X_train': X_train,
                'X_test': X_test,
                'y_train': y_train,
                'y_test': y_test,
                'feature_columns': self.feature_columns,
                'portfolio_columns': self.portfolio_columns,
                'successful_symbols': successful_symbols
            }
            
            logger.info("ML datasets created successfully")
            logger.info(f"Features: {len(self.feature_columns)}, Portfolio: {len(self.portfolio_columns)}")
            
            return datasets
            
        except Exception as e:
            logger.error(f"Error creating ML datasets: {e}")
            return {}

# Example usage
if __name__ == "__main__":
    processor = StockDataProcessor()
    
    # Example feature and portfolio lists
    feature_list = ['AAPL', 'GOOGL', 'MSFT', 'AMZN', 'TSLA']
    portfolio_list = ['AAPL', 'GOOGL', 'MSFT']
    
    # Create ML datasets
    datasets = processor.create_ml_datasets(feature_list, portfolio_list, max_stocks=10)
    
    if datasets:
        print(f"Training features shape: {datasets['X_train'].shape}")
        print(f"Training targets shape: {datasets['y_train'].shape}")
        print(f"Test features shape: {datasets['X_test'].shape}")
        print(f"Test targets shape: {datasets['y_test'].shape}")
