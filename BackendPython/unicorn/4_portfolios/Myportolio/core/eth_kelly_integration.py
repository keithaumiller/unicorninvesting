"""
Enhanced ETH Portfolio Integration with Kelly Criterion
Combines ETH momentum strategy, risk management, and Kelly position sizing
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Tuple, Any
import logging
from datetime import datetime, timedelta
import sys
import os

# Add paths for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)
sys.path.append(os.path.join(current_dir, '..', '..'))
sys.path.append(os.path.join(current_dir, 'utilities'))

from trading_algorithms.eth_momentum_strategy import ETHMomentumStrategy
from risk_algorithms.eth_basic_risk import ETHBasicRisk
from utilities.kelly_criterion import ETHKellyPortfolioManager

logger = logging.getLogger(__name__)

class ETHKellyIntegratedPortfolio:
    """
    Complete ETH Portfolio Management with Kelly Criterion Integration
    
    Integrates:
    - ETH Momentum Strategy (signal generation)
    - ETH Risk Management (risk controls)  
    - Kelly Criterion (optimal position sizing)
    """
    
    def __init__(self, config: Dict):
        """
        Initialize integrated ETH portfolio management system
        
        Args:
            config: Complete configuration dictionary
        """
        self.config = config
        
        # Initialize components
        trading_config = config.get('trading', {})
        risk_config = config.get('risk', {})
        kelly_config = config.get('kelly', {})
        
        # Core components
        self.momentum_strategy = ETHMomentumStrategy(trading_config)
        self.risk_manager = ETHBasicRisk(
            max_drawdown=risk_config.get('max_drawdown', 0.15),
            max_position_pct=risk_config.get('max_position_pct', 0.8),
            var_confidence=risk_config.get('var_confidence', 0.05)
        )
        self.kelly_manager = ETHKellyPortfolioManager(kelly_config, risk_config)
        
        # Portfolio state
        self.portfolio_value = config.get('initial_capital', 100000.0)
        self.cash = self.portfolio_value
        self.current_position = 0.0
        self.current_price = 0.0
        
        # Performance tracking
        self.trade_history = []
        self.portfolio_history = []
        self.signal_history = []
        
        logging.info(f"ETH Kelly Integrated Portfolio initialized with ${self.portfolio_value:,.2f}")
    
    def process_market_data(self, market_data: pd.DataFrame) -> Dict:
        """
        Process new market data and generate complete portfolio decision
        
        Args:
            market_data: DataFrame with OHLCV data
            
        Returns:
            Dict with complete portfolio decision and analysis
        """
        
        if market_data.empty:
            return {'error': 'No market data provided'}
        
        # Get current price
        self.current_price = market_data['close'].iloc[-1]
        
        # Update portfolio value
        position_value = self.current_position * self.current_price
        self.portfolio_value = self.cash + position_value
        
        # 1. Generate momentum signal
        momentum_signal = self.momentum_strategy.generate_signal(market_data)
        
        # 2. Update risk management with current portfolio value
        self.risk_manager.update_portfolio_metrics(self.portfolio_value)
        
        # 3. Calculate Kelly optimal position size
        portfolio_data = {
            'total_value': self.portfolio_value,
            'cash': self.cash,
            'positions': {'ETHUSD': self.current_position},
            'current_prices': {'ETHUSD': self.current_price}
        }
        
        market_context = {
            'price': self.current_price,
            'timestamp': datetime.now()
        }
        
        kelly_recommendation = self.kelly_manager.process_signal(
            momentum_signal, market_context, portfolio_data
        )
        
        # 4. Risk validation
        proposed_position_value = kelly_recommendation.get('position_value', 0)
        
        position_validation = self.risk_manager.validate_position_size(
            proposed_position_value, self.portfolio_value
        )
        
        drawdown_validation = self.risk_manager.validate_drawdown(
            self.portfolio_value - abs(proposed_position_value - position_value)
        )
        
        # 5. Make final decision
        final_decision = self._make_final_decision(
            momentum_signal, kelly_recommendation, 
            position_validation, drawdown_validation
        )
        
        # 6. Record decision
        decision_record = {
            'timestamp': datetime.now(),
            'price': self.current_price,
            'portfolio_value': self.portfolio_value,
            'momentum_signal': momentum_signal,
            'kelly_recommendation': kelly_recommendation,
            'risk_validation': {
                'position': position_validation,
                'drawdown': drawdown_validation
            },
            'final_decision': final_decision
        }
        
        self.signal_history.append(decision_record)
        
        return decision_record
    
    def _make_final_decision(self, 
                           momentum_signal: Dict,
                           kelly_recommendation: Dict,
                           position_validation: Dict,
                           drawdown_validation: Dict) -> Dict:
        """
        Make final portfolio decision based on all inputs
        
        Returns:
            Dict with final decision and reasoning
        """
        
        # Check if any risk controls reject the position
        if not position_validation.get('approved', False):
            return {
                'action': 'REJECT',
                'reason': f"Position risk: {position_validation['reason']}",
                'target_position': self.current_position,
                'position_change': 0.0
            }
        
        if not drawdown_validation.get('approved', False):
            return {
                'action': 'REJECT', 
                'reason': f"Drawdown risk: {drawdown_validation['reason']}",
                'target_position': self.current_position,
                'position_change': 0.0
            }
        
        # Get Kelly recommendation
        kelly_position_size = kelly_recommendation.get('position_size', 0.0)
        kelly_fraction = kelly_recommendation.get('kelly_fraction', 0.0)
        
        # Check momentum signal alignment
        momentum_signal_type = momentum_signal.get('signal', 'HOLD')
        momentum_confidence = momentum_signal.get('confidence', 0.0)
        
        if momentum_signal_type == 'HOLD' or momentum_confidence <= 0.1:
            return {
                'action': 'HOLD',
                'reason': f"Momentum signal: {momentum_signal_type} (confidence: {momentum_confidence:.1%})",
                'target_position': self.current_position,
                'position_change': 0.0
            }
        
        # Calculate final position size
        if momentum_signal_type == 'BUY' and kelly_position_size > 0:
            target_position = kelly_position_size
            action = 'BUY' if target_position > self.current_position else 'REDUCE'
        elif momentum_signal_type == 'SELL':
            target_position = 0.0
            action = 'SELL'
        else:
            target_position = self.current_position
            action = 'HOLD'
        
        position_change = target_position - self.current_position
        
        return {
            'action': action,
            'target_position': target_position,
            'position_change': position_change,
            'kelly_fraction': kelly_fraction,
            'momentum_confidence': momentum_confidence,
            'reason': f"{action}: Kelly={kelly_fraction:.1%}, Momentum={momentum_confidence:.1%}",
            'execution_details': {
                'current_position': self.current_position,
                'target_position': target_position,
                'position_value': target_position * self.current_price,
                'trade_value': abs(position_change) * self.current_price
            }
        }
    
    def execute_decision(self, decision: Dict) -> Dict:
        """
        Execute portfolio decision and update state
        
        Args:
            decision: Final decision from process_market_data
            
        Returns:
            Dict with execution results
        """
        
        if 'final_decision' not in decision:
            return {'error': 'Invalid decision format'}
        
        final_decision = decision['final_decision']
        action = final_decision.get('action', 'HOLD')
        target_position = final_decision.get('target_position', self.current_position)
        position_change = final_decision.get('position_change', 0.0)
        
        if action == 'HOLD' or abs(position_change) < 0.001:
            return {
                'executed': False,
                'reason': 'No position change required',
                'current_position': self.current_position
            }
        
        # Calculate trade details
        trade_value = abs(position_change) * self.current_price
        
        # Check if we have enough cash for buying
        if position_change > 0 and trade_value > self.cash:
            return {
                'executed': False,
                'reason': f'Insufficient cash: need ${trade_value:.2f}, have ${self.cash:.2f}',
                'current_position': self.current_position
            }
        
        # Execute the trade
        previous_position = self.current_position
        self.current_position = target_position
        
        # Update cash
        if position_change > 0:  # Buying
            self.cash -= trade_value
        else:  # Selling
            self.cash += trade_value
        
        # Update Kelly manager position tracking
        self.kelly_manager.update_position(
            self.current_position, 
            self.current_price,
            datetime.now()
        )
        
        # Update momentum strategy position
        self.momentum_strategy.update_position(self.current_position)
        
        # Record trade
        trade_record = {
            'timestamp': datetime.now(),
            'action': action,
            'price': self.current_price,
            'position_change': position_change,
            'previous_position': previous_position,
            'new_position': self.current_position,
            'trade_value': trade_value,
            'cash_after': self.cash,
            'portfolio_value': self.cash + (self.current_position * self.current_price)
        }
        
        self.trade_history.append(trade_record)
        
        logging.info(f"Trade executed: {action} {abs(position_change):.4f} ETH at ${self.current_price:.2f}")
        
        return {
            'executed': True,
            'trade_details': trade_record,
            'portfolio_summary': self.get_portfolio_summary()
        }
    
    def get_portfolio_summary(self) -> Dict:
        """Get comprehensive portfolio summary"""
        
        position_value = self.current_position * self.current_price
        total_value = self.cash + position_value
        
        # Calculate returns
        initial_capital = self.config.get('initial_capital', 100000.0)
        total_return = (total_value - initial_capital) / initial_capital
        
        return {
            'portfolio_value': total_value,
            'cash': self.cash,
            'position': {
                'size': self.current_position,
                'value': position_value,
                'percentage': position_value / total_value if total_value > 0 else 0
            },
            'performance': {
                'total_return': total_return,
                'total_return_pct': total_return * 100,
                'current_price': self.current_price
            },
            'risk_metrics': {
                'current_drawdown': self.risk_manager.current_drawdown,
                'portfolio_volatility': self.risk_manager.portfolio_volatility,
                'var_5pct': self.risk_manager.current_var
            },
            'kelly_performance': self.kelly_manager.get_portfolio_summary(),
            'trade_count': len(self.trade_history),
            'signal_count': len(self.signal_history)
        }
    
    def get_recent_performance(self, days: int = 7) -> Dict:
        """Get recent performance summary"""
        
        cutoff_date = datetime.now() - timedelta(days=days)
        
        recent_trades = [
            t for t in self.trade_history 
            if t['timestamp'] >= cutoff_date
        ]
        
        recent_signals = [
            s for s in self.signal_history
            if s['timestamp'] >= cutoff_date
        ]
        
        if not recent_trades:
            return {
                'period_days': days,
                'no_trades': True,
                'recent_signals': len(recent_signals)
            }
        
        trade_returns = []
        for trade in recent_trades:
            if trade['action'] in ['SELL', 'REDUCE']:
                # Calculate return for closing trades
                # Simplified: assume average cost basis
                trade_return = (trade['price'] - self.kelly_manager.entry_price) / self.kelly_manager.entry_price
                trade_returns.append(trade_return)
        
        return {
            'period_days': days,
            'trade_count': len(recent_trades),
            'signal_count': len(recent_signals),
            'recent_trades': recent_trades[-5:],  # Last 5 trades
            'avg_trade_return': np.mean(trade_returns) if trade_returns else 0,
            'win_rate': len([r for r in trade_returns if r > 0]) / len(trade_returns) if trade_returns else 0
        }
