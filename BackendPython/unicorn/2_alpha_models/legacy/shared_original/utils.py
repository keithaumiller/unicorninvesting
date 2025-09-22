"""
Shared Utilities for Alpha Models

Common functions used across all asset classes and model types.
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class DataValidator:
    """Data validation utilities for alpha models."""
    
    @staticmethod
    def validate_ohlcv_data(data: pd.DataFrame) -> bool:
        """
        Validate OHLCV data format.
        
        Args:
            data: DataFrame with OHLCV data
            
        Returns:
            bool: True if valid
            
        Raises:
            ValueError: If data is invalid
        """
        required_columns = ['Open', 'High', 'Low', 'Close', 'Volume']
        missing_columns = [col for col in required_columns if col not in data.columns]
        
        if missing_columns:
            raise ValueError(f"Missing required OHLCV columns: {missing_columns}")
            
        # Check for logical consistency
        if not all(data['High'] >= data['Low']):
            raise ValueError("High prices must be >= Low prices")
            
        if not all(data['High'] >= data['Open']) or not all(data['High'] >= data['Close']):
            raise ValueError("High prices must be >= Open and Close prices")
            
        if not all(data['Low'] <= data['Open']) or not all(data['Low'] <= data['Close']):
            raise ValueError("Low prices must be <= Open and Close prices")
            
        if not all(data['Volume'] >= 0):
            raise ValueError("Volume must be non-negative")
            
        return True
    
    @staticmethod
    def check_data_completeness(data: pd.DataFrame, min_rows: int = 100) -> Dict[str, Any]:
        """
        Check data completeness and quality.
        
        Args:
            data: Input DataFrame
            min_rows: Minimum required rows
            
        Returns:
            Dict with completeness metrics
        """
        total_rows = len(data)
        missing_data = data.isnull().sum()
        
        return {
            'total_rows': total_rows,
            'sufficient_data': total_rows >= min_rows,
            'missing_data': missing_data.to_dict(),
            'completeness_ratio': (total_rows - missing_data.sum()) / (total_rows * len(data.columns)) if total_rows > 0 else 0,
            'data_quality_score': 1.0 - (missing_data.sum() / (total_rows * len(data.columns))) if total_rows > 0 else 0
        }

class SignalProcessor:
    """Signal processing and filtering utilities."""
    
    @staticmethod
    def normalize_signal(signal: float, method: str = 'tanh') -> float:
        """
        Normalize signal to [-1, 1] range.
        
        Args:
            signal: Raw signal value
            method: Normalization method ('tanh', 'clip', 'sigmoid')
            
        Returns:
            Normalized signal
        """
        if method == 'tanh':
            return np.tanh(signal)
        elif method == 'clip':
            return np.clip(signal, -1, 1)
        elif method == 'sigmoid':
            return 2 * (1 / (1 + np.exp(-signal))) - 1
        else:
            raise ValueError(f"Unknown normalization method: {method}")
    
    @staticmethod
    def apply_signal_filter(signals: pd.Series, filter_type: str = 'ema', window: int = 5) -> pd.Series:
        """
        Apply smoothing filter to signals.
        
        Args:
            signals: Time series of signals
            filter_type: Type of filter ('ema', 'sma', 'median')
            window: Filter window size
            
        Returns:
            Filtered signals
        """
        if filter_type == 'ema':
            return signals.ewm(span=window).mean()
        elif filter_type == 'sma':
            return signals.rolling(window=window).mean()
        elif filter_type == 'median':
            return signals.rolling(window=window).median()
        else:
            raise ValueError(f"Unknown filter type: {filter_type}")
    
    @staticmethod
    def calculate_signal_confidence(signal: float, volatility: float, volume: float) -> float:
        """
        Calculate signal confidence based on market conditions.
        
        Args:
            signal: Raw signal strength
            volatility: Market volatility
            volume: Trading volume
            
        Returns:
            Confidence score [0, 1]
        """
        # Base confidence from signal strength
        base_confidence = abs(signal)
        
        # Adjust for volatility (lower confidence in high volatility)
        volatility_factor = 1 / (1 + volatility * 2)
        
        # Adjust for volume (higher confidence with higher volume)
        volume_factor = min(1.0, volume / np.mean([volume]) if volume > 0 else 0.5)
        
        return base_confidence * volatility_factor * volume_factor

class PerformanceAnalyzer:
    """Performance analysis utilities."""
    
    @staticmethod
    def calculate_sharpe_ratio(returns: pd.Series, risk_free_rate: float = 0.02) -> float:
        """
        Calculate Sharpe ratio.
        
        Args:
            returns: Time series of returns
            risk_free_rate: Annual risk-free rate
            
        Returns:
            Sharpe ratio
        """
        if len(returns) < 2:
            return 0.0
            
        excess_returns = returns - risk_free_rate / 252  # Daily risk-free rate
        return excess_returns.mean() / excess_returns.std() * np.sqrt(252) if excess_returns.std() > 0 else 0.0
    
    @staticmethod
    def calculate_max_drawdown(cumulative_returns: pd.Series) -> float:
        """
        Calculate maximum drawdown.
        
        Args:
            cumulative_returns: Cumulative returns series
            
        Returns:
            Maximum drawdown percentage
        """
        running_max = cumulative_returns.expanding().max()
        drawdown = (cumulative_returns - running_max) / running_max
        return drawdown.min()
    
    @staticmethod
    def calculate_win_rate(signals: pd.Series, returns: pd.Series) -> float:
        """
        Calculate win rate of signals.
        
        Args:
            signals: Trading signals
            returns: Corresponding returns
            
        Returns:
            Win rate percentage
        """
        if len(signals) != len(returns):
            raise ValueError("Signals and returns must have same length")
            
        signal_returns = signals * returns
        winning_trades = (signal_returns > 0).sum()
        total_trades = (signals != 0).sum()
        
        return winning_trades / total_trades if total_trades > 0 else 0.0

class RiskManager:
    """Risk management utilities."""
    
    @staticmethod
    def calculate_position_size(signal: float, confidence: float, risk_budget: float = 0.02, volatility: float = None) -> float:
        """
        Calculate position size based on signal and risk parameters.
        
        Args:
            signal: Trading signal [-1, 1]
            confidence: Signal confidence [0, 1]
            risk_budget: Maximum risk per trade
            volatility: Asset volatility (optional)
            
        Returns:
            Position size as fraction of portfolio
        """
        # Base position size from signal strength and confidence
        base_size = abs(signal) * confidence
        
        # Apply risk budget constraint
        risk_adjusted_size = base_size * risk_budget
        
        # Adjust for volatility if provided
        if volatility is not None and volatility > 0:
            volatility_adjustment = min(1.0, 0.1 / volatility)  # Target 10% volatility
            risk_adjusted_size *= volatility_adjustment
        
        return np.clip(risk_adjusted_size, 0, risk_budget)
    
    @staticmethod
    def apply_risk_limits(positions: Dict[str, float], max_portfolio_risk: float = 0.20) -> Dict[str, float]:
        """
        Apply portfolio-level risk limits.
        
        Args:
            positions: Dictionary of asset positions
            max_portfolio_risk: Maximum total portfolio risk
            
        Returns:
            Risk-adjusted positions
        """
        total_risk = sum(abs(pos) for pos in positions.values())
        
        if total_risk > max_portfolio_risk:
            # Scale down all positions proportionally
            scale_factor = max_portfolio_risk / total_risk
            positions = {asset: pos * scale_factor for asset, pos in positions.items()}
            
        return positions

def setup_logging(name: str, level: str = 'INFO') -> logging.Logger:
    """
    Setup logging for alpha model.
    
    Args:
        name: Logger name
        level: Logging level
        
    Returns:
        Configured logger
    """
    logger = logging.getLogger(name)
    logger.setLevel(getattr(logging, level.upper()))
    
    if not logger.handlers:
        handler = logging.StreamHandler()
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        handler.setFormatter(formatter)
        logger.addHandler(handler)
    
    return logger

def load_market_data(symbol: str, start_date: str, end_date: str) -> pd.DataFrame:
    """
    Load market data for backtesting.
    
    Note: This is a placeholder function. In production, this would
    connect to actual data sources.
    
    Args:
        symbol: Asset symbol
        start_date: Start date (YYYY-MM-DD)
        end_date: End date (YYYY-MM-DD)
        
    Returns:
        OHLCV DataFrame
    """
    logger.warning("Using placeholder data loader. Implement actual data source connection.")
    
    # Generate sample data for testing
    dates = pd.date_range(start_date, end_date, freq='D')
    np.random.seed(42)  # For reproducible testing
    
    data = pd.DataFrame({
        'Date': dates,
        'Open': 100 + np.cumsum(np.random.randn(len(dates)) * 0.02),
        'High': np.nan,
        'Low': np.nan,
        'Close': np.nan,
        'Volume': np.random.randint(1000000, 10000000, len(dates))
    })
    
    # Generate OHLC from close prices
    data['Close'] = data['Open'] + np.random.randn(len(dates)) * 0.01
    data['High'] = np.maximum(data['Open'], data['Close']) + np.abs(np.random.randn(len(dates)) * 0.005)
    data['Low'] = np.minimum(data['Open'], data['Close']) - np.abs(np.random.randn(len(dates)) * 0.005)
    
    return data.set_index('Date')
