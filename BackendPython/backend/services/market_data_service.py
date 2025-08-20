"""
Market data collection and database interface for Unicorn Investing platform.

This module handles:
- Stock and forex data downloads using yfinance
- Database connectivity and operations using SQLAlchemy
- Portfolio and feature list management
- Data persistence and retrieval

Migrated from: BackendPython/datagathering/downloadstockdata.R
"""

import os
import time
import logging
from typing import List, Dict, Optional, Union
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
import yfinance as yf
from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker
from sqlalchemy.exc import SQLAlchemyError
import concurrent.futures
from dataclasses import dataclass

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class DataDownloadConfig:
    """Configuration for data download operations."""
    default_period: str = "2y"
    max_concurrent_downloads: int = 10
    retry_attempts: int = 3
    retry_delay: float = 1.0
    request_timeout: int = 30
    
@dataclass
class DatabaseConfig:
    """Database connection configuration."""
    host: str = "ec2-54-85-232-216.compute-1.amazonaws.com"
    user: str = "unicorn"
    password: str = "n7gtRLHi"
    database: str = "unicorn"
    port: int = 3306
    pool_recycle: int = 3600
    pool_size: int = 10
    max_overflow: int = 20

class MarketDataCollector:
    """Handles market data collection and database operations."""
    
    def __init__(self, db_config: DatabaseConfig = None, download_config: DataDownloadConfig = None):
        """Initialize with database and download configuration."""
        self.db_config = db_config or DatabaseConfig()
        self.download_config = download_config or DataDownloadConfig()
        self.engine = None
        self.session = None
        self._connect_to_database()
    
    def _connect_to_database(self):
        """Establish database connection using SQLAlchemy."""
        try:
            connection_string = (
                f"mysql+pymysql://{self.db_config.user}:{self.db_config.password}@"
                f"{self.db_config.host}:{self.db_config.port}/{self.db_config.database}"
            )
            self.engine = create_engine(
                connection_string, 
                pool_recycle=self.db_config.pool_recycle,
                pool_size=self.db_config.pool_size,
                max_overflow=self.db_config.max_overflow
            )
            Session = sessionmaker(bind=self.engine)
            self.session = Session()
            logger.info("Database connection established successfully")
        except SQLAlchemyError as e:
            logger.error(f"Database connection failed: {e}")
            raise
    
    def download_stock_data(self, symbol: str, period: str = None) -> pd.DataFrame:
        """
        Download stock data for a given symbol with retry logic.
        
        Args:
            symbol: Stock symbol (e.g., 'AAPL', 'GOOGL')
            period: Time period (default from config)
            
        Returns:
            DataFrame with OHLCV data and Adjusted Close
        """
        period = period or self.download_config.default_period
        
        for attempt in range(self.download_config.retry_attempts):
            try:
                ticker = yf.Ticker(symbol)
                data = ticker.history(period=period, timeout=self.download_config.request_timeout)
                
                if data.empty:
                    logger.warning(f"No data found for symbol: {symbol}")
                    return pd.DataFrame()
                
                # Validate data quality
                if len(data) < 10:  # Minimum data points
                    logger.warning(f"Insufficient data for symbol: {symbol} (only {len(data)} records)")
                    return pd.DataFrame()
                
                # Add symbol prefix to columns for consistency with R format
                data.columns = [f'{symbol}.{col}' for col in data.columns]
                
                # Ensure we have an Adjusted column
                if f'{symbol}.Adj Close' in data.columns:
                    data[f'{symbol}.Adjusted'] = data[f'{symbol}.Adj Close']
                elif f'{symbol}.Close' in data.columns:
                    data[f'{symbol}.Adjusted'] = data[f'{symbol}.Close']
                
                logger.debug(f"Downloaded {len(data)} records for {symbol}")
                return data
                
            except Exception as e:
                logger.warning(f"Attempt {attempt + 1} failed for {symbol}: {e}")
                if attempt < self.download_config.retry_attempts - 1:
                    time.sleep(self.download_config.retry_delay * (2 ** attempt))  # Exponential backoff
                else:
                    logger.error(f"Failed to download data for {symbol} after {self.download_config.retry_attempts} attempts")
                    return pd.DataFrame()
            
            # Calculate adjusted OHLCV based on adjustment factor
            adj_factor = data['Close'] / data['Adj Close']
            
            adjusted_data = pd.DataFrame({
                f'{symbol}.Open': data['Open'] / adj_factor,
                f'{symbol}.High': data['High'] / adj_factor,
                f'{symbol}.Low': data['Low'] / adj_factor,
                f'{symbol}.Close': data['Close'] / adj_factor,
                f'{symbol}.Volume': data['Volume'],
                f'{symbol}.Adjusted': data['Adj Close']
            })
            
            logger.info(f"Downloaded {len(adjusted_data)} records for {symbol}")
            return adjusted_data
            
        except Exception as e:
            logger.error(f"Error downloading data for {symbol}: {e}")
            return pd.DataFrame()
    
    def download_forex_data(self, currency_pair: str, period: str = "2y") -> pd.DataFrame:
        """
        Download forex data for a currency pair.
        
        Args:
            currency_pair: Currency pair (e.g., 'EURUSD=X', 'GBPUSD=X')
            period: Time period for data collection
            
        Returns:
            DataFrame with forex exchange rate data
        """
        try:
            # Convert ZARTWD format to EUR/USD format for yfinance
            if len(currency_pair) == 6:
                base = currency_pair[:3]
                quote = currency_pair[3:]
                yf_symbol = f"{base}{quote}=X"
            else:
                yf_symbol = currency_pair
            
            ticker = yf.Ticker(yf_symbol)
            data = ticker.history(period=period)
            
            if data.empty:
                logger.warning(f"No forex data found for: {currency_pair}")
                return pd.DataFrame()
            
            # Format for compatibility with existing system
            forex_data = pd.DataFrame({
                f'{currency_pair}.Adjusted': data['Close']
            })
            
            logger.info(f"Downloaded {len(forex_data)} forex records for {currency_pair}")
            return forex_data
            
        except Exception as e:
            logger.error(f"Error downloading forex data for {currency_pair}: {e}")
            return pd.DataFrame()
    
    def save_market_data(self, symbol: str, data: pd.DataFrame, data_type: str = "stock"):
        """
        Save market data to file system in CSV format.
        
        Args:
            symbol: Symbol identifier
            data: Market data DataFrame
            data_type: Type of data ('stock' or 'forex')
        """
        try:
            # Create directory structure
            data_dir = f"data/stockdata/{symbol}"
            os.makedirs(data_dir, exist_ok=True)
            
            # Save to CSV file
            file_path = os.path.join(data_dir, "stockdata.csv")
            data.to_csv(file_path)
            
            logger.info(f"Saved {data_type} data for {symbol} to {file_path}")
            
        except Exception as e:
            logger.error(f"Error saving data for {symbol}: {e}")
    
    def pull_stocks_list(self, stock_list: List[str], max_workers: int = 4):
        """
        Download data for multiple stocks in parallel.
        
        Args:
            stock_list: List of stock symbols to download
            max_workers: Maximum number of concurrent downloads
        """
        # Load currency list to separate stocks from forex
        currency_list = self.load_currency_list()
        
        # Separate stocks from forex pairs
        stocks_only = [symbol for symbol in stock_list if symbol not in currency_list]
        forex_only = [symbol for symbol in stock_list if symbol in currency_list]
        
        def download_and_save_stock(symbol):
            try:
                data = self.download_stock_data(symbol)
                if not data.empty:
                    self.save_market_data(symbol, data, "stock")
                return symbol, "success"
            except Exception as e:
                logger.error(f"Failed to download {symbol}: {e}")
                return symbol, "failed"
        
        def download_and_save_forex(symbol):
            try:
                data = self.download_forex_data(symbol)
                if not data.empty:
                    self.save_market_data(symbol, data, "forex")
                return symbol, "success"
            except Exception as e:
                logger.error(f"Failed to download forex {symbol}: {e}")
                return symbol, "failed"
        
        # Download stocks in parallel
        if stocks_only:
            with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
                stock_results = list(executor.map(download_and_save_stock, stocks_only))
            
            logger.info(f"Stock downloads completed: {len([r for r in stock_results if r[1] == 'success'])} successful")
        
        # Download forex in parallel
        if forex_only:
            with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
                forex_results = list(executor.map(download_and_save_forex, forex_only))
            
            logger.info(f"Forex downloads completed: {len([r for r in forex_results if r[1] == 'success'])} successful")
    
    def load_currency_list(self) -> List[str]:
        """Load list of currency pairs from database or file."""
        try:
            # Try to load from CSV file first
            currency_file = "data/exchangedata/FOREX.csv"
            if os.path.exists(currency_file):
                df = pd.read_csv(currency_file)
                return df.iloc[:, 0].tolist()
            else:
                logger.warning(f"Currency file not found: {currency_file}")
                return []
        except Exception as e:
            logger.error(f"Error loading currency list: {e}")
            return []
    
    def load_portfolio_list(self, user_id: int, portfolio_id: int) -> List[str]:
        """
        Load portfolio composition from database.
        
        Args:
            user_id: User identifier
            portfolio_id: Portfolio identifier
            
        Returns:
            List of symbols in the portfolio
        """
        try:
            query = text("""
                SELECT symbol FROM unicorn_portfolios 
                WHERE userid = :user_id AND portfolioid = :portfolio_id
            """)
            
            result = self.session.execute(query, {
                'user_id': user_id, 
                'portfolio_id': portfolio_id
            })
            
            symbols = [row[0] for row in result.fetchall()]
            return sorted(symbols)
            
        except SQLAlchemyError as e:
            logger.error(f"Error loading portfolio list: {e}")
            return []
    
    def load_feature_list(self, user_id: int, portfolio_name: int, max_features: int = 0) -> List[str]:
        """
        Load feature list for machine learning models.
        
        Args:
            user_id: User identifier
            portfolio_name: Portfolio identifier
            max_features: Maximum number of features (0 = no limit)
            
        Returns:
            List of feature symbols
        """
        try:
            # Load portfolio symbols
            portfolio_list = self.load_portfolio_list(user_id, portfolio_name)
            
            # Load universal feature list from database
            query = text("SELECT * FROM unicorn_universalfeaturelist_daily")
            result = self.session.execute(query)
            
            # Extract second column (assuming it contains symbols)
            feature_df = pd.DataFrame(result.fetchall())
            if len(feature_df.columns) > 1:
                feature_list = sorted(feature_df.iloc[:, 1].tolist())
            else:
                feature_list = []
            
            # Limit features if specified
            if max_features > 0:
                feature_list = feature_list[:max_features] + portfolio_list
            
            # Combine and sort unique features
            all_features = sorted(list(set(feature_list + portfolio_list)))
            
            # Clean whitespace
            all_features = [symbol.strip() for symbol in all_features]
            
            return all_features
            
        except SQLAlchemyError as e:
            logger.error(f"Error loading feature list: {e}")
            return []
    
    def insert_portfolio_data(self, user_id: int, portfolio_id: int, symbols: List[str]):
        """
        Insert portfolio composition into database.
        
        Args:
            user_id: User identifier
            portfolio_id: Portfolio identifier
            symbols: List of symbols in portfolio
        """
        try:
            # Create DataFrame for bulk insert
            portfolio_data = pd.DataFrame({
                'userid': [user_id] * len(symbols),
                'portfolioid': [portfolio_id] * len(symbols),
                'symbol': symbols
            })
            
            # Insert into database
            portfolio_data.to_sql(
                'unicorn_portfolios', 
                con=self.engine, 
                if_exists='append', 
                index=False
            )
            
            logger.info(f"Inserted {len(symbols)} symbols for portfolio {portfolio_id}")
            
        except SQLAlchemyError as e:
            logger.error(f"Error inserting portfolio data: {e}")
    
    def insert_best_feature_list(self, user_id: int, portfolio_id: int, symbols: List[str]):
        """
        Insert GA-optimized feature list into database.
        
        Args:
            user_id: User identifier
            portfolio_id: Portfolio identifier
            symbols: List of optimized feature symbols
        """
        try:
            # Remove old best feature list
            delete_query = text("""
                DELETE FROM unicorn_best_featurelist 
                WHERE userid = :user_id AND portfolioid = :portfolio_id
            """)
            self.session.execute(delete_query, {
                'user_id': user_id,
                'portfolio_id': portfolio_id
            })
            
            # Insert new feature list
            feature_data = pd.DataFrame({
                'userid': [user_id] * len(symbols),
                'portfolioid': [portfolio_id] * len(symbols),
                'symbol': symbols
            })
            
            feature_data.to_sql(
                'unicorn_best_featurelist',
                con=self.engine,
                if_exists='append',
                index=False
            )
            
            self.session.commit()
            logger.info(f"Updated best feature list with {len(symbols)} features")
            
        except SQLAlchemyError as e:
            logger.error(f"Error updating best feature list: {e}")
            self.session.rollback()
    
    def close_connection(self):
        """Close database connection."""
        if self.session:
            self.session.close()
        if self.engine:
            self.engine.dispose()
        logger.info("Database connection closed")

# Example usage
if __name__ == "__main__":
    collector = MarketDataCollector()
    
    # Example: Download data for a few stocks
    test_symbols = ['AAPL', 'GOOGL', 'MSFT']
    collector.pull_stocks_list(test_symbols)
    
    # Example: Load portfolio for user 1, portfolio 1
    portfolio = collector.load_portfolio_list(1, 1)
    print(f"Portfolio symbols: {portfolio}")
    
    collector.close_connection()
