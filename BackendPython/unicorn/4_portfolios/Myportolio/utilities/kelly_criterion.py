"""
Kelly Criterion Position Sizing for ETH Portfolio
Advanced position sizing algorithm using Kelly formula with risk constraints
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple
import logging
from datetime import datetime, timedelta

# Import our risk management
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

logger = logging.getLogger(__name__)

class KellyCriterionCalculator:
    """
    Kelly Criterion position sizing calculator for optimal portfolio allocation
    
    Uses Kelly formula: f* = (bp - q) / b
    Where:
    - f* = fraction of capital to wager
    - b = odds received on the wager (expected return / downside risk)
    - p = probability of winning
    - q = probability of losing (1 - p)
    """
    
    def __init__(self, 
                 lookback_window: int = 30,
                 max_kelly_fraction: float = 0.25,
                 min_win_rate: float = 0.35,
                 risk_adjustment: float = 0.5):
        """
        Initialize Kelly Criterion calculator
        
        Args:
            lookback_window: Number of historical signals to analyze
            max_kelly_fraction: Maximum Kelly fraction allowed (risk control)
            min_win_rate: Minimum win rate required for position
            risk_adjustment: Risk adjustment factor (0.5 = half Kelly)
        """
        self.lookback_window = lookback_window
        self.max_kelly_fraction = max_kelly_fraction
        self.min_win_rate = min_win_rate
        self.risk_adjustment = risk_adjustment
        
        # Track historical signals and outcomes
        self.signal_history = []
        self.performance_history = []
        
        logging.info(f"Kelly Criterion Calculator initialized: window={lookback_window}, max_fraction={max_kelly_fraction}")
    
    def update_signal_history(self, signal_data: Dict, outcome_return: float = None):
        """
        Update signal history with new signal and optionally outcome
        
        Args:
            signal_data: Signal information from trading algorithm
            outcome_return: Actual return from the signal (if available)
        """
        
        timestamp = datetime.now()
        
        signal_record = {
            'timestamp': timestamp,
            'signal': signal_data.get('signal', 'HOLD'),
            'confidence': signal_data.get('confidence', 0.0),
            'target_position': signal_data.get('target_position', 0.0),
            'outcome_return': outcome_return
        }
        
        self.signal_history.append(signal_record)
        
        # Keep only recent history
        if len(self.signal_history) > self.lookback_window * 2:
            self.signal_history = self.signal_history[-self.lookback_window:]
        
        logging.debug(f"Updated signal history: {len(self.signal_history)} records")
    
    def calculate_win_loss_probabilities(self, signal_type: str = 'BUY') -> Tuple[float, float, float]:
        """
        Calculate win probability, average win, and average loss
        
        Args:
            signal_type: Type of signal to analyze ('BUY', 'SELL')
            
        Returns:
            Tuple of (win_probability, avg_win, avg_loss)
        """
        
        # Filter signals by type with outcomes
        relevant_signals = [
            s for s in self.signal_history 
            if s['signal'] == signal_type and s['outcome_return'] is not None
        ]
        
        if len(relevant_signals) < 5:
            # Default conservative estimates for insufficient data
            return 0.45, 0.03, 0.02  # 45% win rate, 3% avg win, 2% avg loss
        
        returns = [s['outcome_return'] for s in relevant_signals]
        wins = [r for r in returns if r > 0]
        losses = [r for r in returns if r < 0]
        
        win_probability = len(wins) / len(returns)
        avg_win = np.mean(wins) if wins else 0.03
        avg_loss = abs(np.mean(losses)) if losses else 0.02
        
        return win_probability, avg_win, avg_loss
    
    def calculate_kelly_fraction(self, 
                                signal_confidence: float,
                                signal_type: str = 'BUY') -> Dict:
        """
        Calculate optimal Kelly fraction for position sizing
        
        Args:
            signal_confidence: Confidence level from trading algorithm (0-1)
            signal_type: Type of signal ('BUY', 'SELL')
            
        Returns:
            Dict with Kelly calculation details
        """
        
        # Get historical probabilities
        win_prob, avg_win, avg_loss = self.calculate_win_loss_probabilities(signal_type)
        
        # Adjust win probability based on signal confidence
        # Higher confidence signals should have higher win probability
        adjusted_win_prob = win_prob + (signal_confidence - 0.5) * 0.2
        adjusted_win_prob = max(0.1, min(0.9, adjusted_win_prob))  # Clamp between 10-90%
        
        lose_prob = 1 - adjusted_win_prob
        
        # Calculate Kelly fraction: f* = (bp - q) / b
        # Where b = avg_win / avg_loss (odds ratio)
        if avg_loss <= 0:
            avg_loss = 0.02  # Default 2% loss
            
        odds_ratio = avg_win / avg_loss
        
        # Kelly formula
        kelly_fraction = (odds_ratio * adjusted_win_prob - lose_prob) / odds_ratio
        
        # Apply risk controls
        if adjusted_win_prob < self.min_win_rate:
            kelly_fraction = 0.0  # No position if win rate too low
        
        # Apply maximum fraction limit
        kelly_fraction = max(0, min(kelly_fraction, self.max_kelly_fraction))
        
        # Apply risk adjustment (fractional Kelly)
        adjusted_kelly = kelly_fraction * self.risk_adjustment
        
        return {
            'kelly_fraction': kelly_fraction,
            'adjusted_kelly': adjusted_kelly,
            'win_probability': adjusted_win_prob,
            'lose_probability': lose_prob,
            'avg_win': avg_win,
            'avg_loss': avg_loss,
            'odds_ratio': odds_ratio,
            'signal_confidence': signal_confidence,
            'sample_size': len([s for s in self.signal_history if s['signal'] == signal_type])
        }
    
    def calculate_position_size(self, 
                              signal_data: Dict,
                              portfolio_value: float,
                              current_price: float) -> Dict:
        """
        Calculate optimal position size using Kelly Criterion
        
        Args:
            signal_data: Signal from trading algorithm
            portfolio_value: Total portfolio value
            current_price: Current asset price
            
        Returns:
            Dict with position sizing recommendation
        """
        
        signal_type = signal_data.get('signal', 'HOLD')
        confidence = signal_data.get('confidence', 0.0)
        
        if signal_type == 'HOLD' or confidence <= 0:
            return {
                'position_size': 0.0,
                'position_value': 0.0,
                'kelly_fraction': 0.0,
                'reason': 'No signal or zero confidence'
            }
        
        # Calculate Kelly fraction
        kelly_result = self.calculate_kelly_fraction(confidence, signal_type)
        
        if kelly_result['adjusted_kelly'] <= 0:
            return {
                'position_size': 0.0,
                'position_value': 0.0,
                'kelly_fraction': kelly_result['adjusted_kelly'],
                'reason': f"Kelly fraction too low: {kelly_result['adjusted_kelly']:.3f}",
                'kelly_details': kelly_result
            }
        
        # Calculate position value and size
        position_value = portfolio_value * kelly_result['adjusted_kelly']
        position_size = position_value / current_price if current_price > 0 else 0
        
        return {
            'position_size': position_size,
            'position_value': position_value,
            'kelly_fraction': kelly_result['adjusted_kelly'],
            'position_pct': kelly_result['adjusted_kelly'],
            'reason': f"Kelly optimal: {kelly_result['adjusted_kelly']:.1%} of portfolio",
            'kelly_details': kelly_result,
            'risk_metrics': {
                'win_probability': kelly_result['win_probability'],
                'expected_return': kelly_result['win_probability'] * kelly_result['avg_win'] - 
                                kelly_result['lose_probability'] * kelly_result['avg_loss']
            }
        }
    
    def get_performance_summary(self) -> Dict:
        """Get performance summary of Kelly Criterion implementation"""
        
        signals_with_outcomes = [s for s in self.signal_history if s['outcome_return'] is not None]
        
        if len(signals_with_outcomes) < 2:
            return {
                'total_signals': len(self.signal_history),
                'signals_with_outcomes': len(signals_with_outcomes),
                'insufficient_data': True
            }
        
        returns = [s['outcome_return'] for s in signals_with_outcomes]
        
        return {
            'total_signals': len(self.signal_history),
            'signals_with_outcomes': len(signals_with_outcomes),
            'win_rate': len([r for r in returns if r > 0]) / len(returns),
            'avg_return': np.mean(returns),
            'avg_win': np.mean([r for r in returns if r > 0]),
            'avg_loss': np.mean([r for r in returns if r < 0]),
            'sharpe_ratio': np.mean(returns) / np.std(returns) if np.std(returns) > 0 else 0,
            'max_return': max(returns),
            'min_return': min(returns),
            'recent_performance': returns[-10:] if len(returns) >= 10 else returns
        }


class ETHKellyPortfolioManager:
    """
    Enhanced ETH Portfolio Manager with Kelly Criterion position sizing
    Integrates with existing ETH momentum strategy and risk management
    """
    
    def __init__(self, 
                 kelly_config: Dict = None,
                 risk_config: Dict = None):
        """
        Initialize ETH Kelly Portfolio Manager
        
        Args:
            kelly_config: Kelly Criterion configuration
            risk_config: Risk management configuration
        """
        
        # Kelly Criterion configuration
        kelly_config = kelly_config or {}
        self.kelly_calculator = KellyCriterionCalculator(
            lookback_window=kelly_config.get('lookback_window', 30),
            max_kelly_fraction=kelly_config.get('max_kelly_fraction', 0.25),
            min_win_rate=kelly_config.get('min_win_rate', 0.35),
            risk_adjustment=kelly_config.get('risk_adjustment', 0.5)
        )
        
        # Portfolio state
        self.current_position_size = 0.0
        self.current_position_value = 0.0
        self.entry_price = 0.0
        self.entry_timestamp = None
        
        logging.info("ETH Kelly Portfolio Manager initialized")
    
    def process_signal(self, 
                      signal_data: Dict,
                      market_data: Dict,
                      portfolio_data: Dict) -> Dict:
        """
        Process trading signal with Kelly Criterion position sizing
        
        Args:
            signal_data: Signal from ETH momentum strategy
            market_data: Current market data
            portfolio_data: Current portfolio state
            
        Returns:
            Dict with position sizing recommendation
        """
        
        current_price = market_data.get('price', 0)
        portfolio_value = portfolio_data.get('total_value', 0)
        
        # Calculate Kelly optimal position size
        kelly_result = self.kelly_calculator.calculate_position_size(
            signal_data, portfolio_value, current_price
        )
        
        # Add current position context
        kelly_result['current_position_size'] = self.current_position_size
        kelly_result['current_position_value'] = self.current_position_value
        kelly_result['position_change'] = kelly_result['position_size'] - self.current_position_size
        
        return kelly_result
    
    def update_position(self, 
                       new_position_size: float,
                       entry_price: float,
                       timestamp: datetime = None):
        """Update current position tracking"""
        
        self.current_position_size = new_position_size
        self.current_position_value = new_position_size * entry_price
        self.entry_price = entry_price
        self.entry_timestamp = timestamp or datetime.now()
        
        logging.info(f"Position updated: size={new_position_size:.4f}, value=${self.current_position_value:.2f}")
    
    def calculate_position_return(self, current_price: float) -> float:
        """Calculate current position return"""
        
        if self.current_position_size <= 0 or self.entry_price <= 0:
            return 0.0
        
        return (current_price - self.entry_price) / self.entry_price
    
    def get_portfolio_summary(self) -> Dict:
        """Get comprehensive portfolio summary"""
        
        return {
            'kelly_performance': self.kelly_calculator.get_performance_summary(),
            'current_position': {
                'size': self.current_position_size,
                'value': self.current_position_value,
                'entry_price': self.entry_price,
                'entry_timestamp': self.entry_timestamp
            },
            'signal_history_count': len(self.kelly_calculator.signal_history)
        }

# Aliases for backward compatibility
KellyCriterion = KellyCriterionCalculator
