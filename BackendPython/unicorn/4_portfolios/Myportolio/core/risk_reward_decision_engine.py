#!/usr/bin/env python3
"""
Risk/Reward Trading Decision Engine
Integrates with existing Kelly Criterion framework to make trading decisions
based on alpha and risk model calculations every 5 minutes.
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any
import logging
import json

# Import existing components if available
try:
    sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
    from utilities.EnhancedPortfolioManager import EnhancedPortfolioManager
except ImportError:
    # Create a simple placeholder if not available
    class EnhancedPortfolioManager:
        pass

class RiskRewardDecisionEngine:
    """
    Trading decision engine that evaluates risk/reward opportunities
    every 5 minutes using alpha and risk models with Kelly Criterion optimization.
    """
    
    def __init__(self, config_path: str = None):
        """
        Initialize the risk/reward decision engine
        
        Args:
            config_path: Path to execution settings configuration
        """
        self.config = self._load_config(config_path)
        self.minimum_reward_risk_ratio = self.config.get('minimum_reward_risk_ratio', 1.5)
        self.alpha_model_weight = self.config.get('alpha_model_weight', 0.6)
        self.risk_model_weight = self.config.get('risk_model_weight', 0.4)
        self.confidence_threshold = self.config.get('confidence_threshold', 0.65)
        self.skip_threshold = self.config.get('skip_threshold', 0.3)
        
        # Initialize logger
        self.logger = logging.getLogger(__name__)
        
        # Decision history for tracking
        self.decision_history = []
        
    def _load_config(self, config_path: str = None) -> Dict:
        """Load configuration from execution settings"""
        if config_path is None:
            config_path = os.path.join(
                os.path.dirname(__file__), '..', 'config', 'execution_settings.json'
            )
        
        try:
            with open(config_path, 'r') as f:
                config = json.load(f)
                return config.get('risk_reward_evaluation', {})
        except Exception as e:
            self.logger.warning(f"Could not load config: {e}. Using defaults.")
            return {}
    
    def evaluate_trading_opportunity(self, 
                                   asset: str,
                                   alpha_signal: Dict[str, Any],
                                   risk_metrics: Dict[str, Any],
                                   current_position: float = 0.0,
                                   market_data: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Evaluate whether a trading opportunity meets risk/reward criteria
        
        Args:
            asset: Asset symbol (e.g., 'ETH', 'BTC')
            alpha_signal: Signal from alpha model with prediction and confidence
            risk_metrics: Risk metrics including volatility, VaR, etc.
            current_position: Current position size (-1 to 1)
            market_data: Current market data for the asset
            
        Returns:
            Dictionary with trading decision and rationale
        """
        decision = {
            'asset': asset,
            'timestamp': datetime.now(),
            'should_trade': False,
            'action': 'hold',
            'size': 0.0,
            'confidence': 0.0,
            'reward_risk_ratio': 0.0,
            'rationale': '',
            'alpha_contribution': 0.0,
            'risk_contribution': 0.0
        }
        
        try:
            # Extract alpha signal components
            alpha_prediction = alpha_signal.get('prediction', 0.0)
            alpha_confidence = alpha_signal.get('confidence', 0.0)
            expected_return = alpha_signal.get('expected_return', 0.0)
            
            # Extract risk metrics
            volatility = risk_metrics.get('volatility', 0.0)
            var_1day = risk_metrics.get('var_1day', 0.0)
            sharpe_estimate = risk_metrics.get('sharpe_estimate', 0.0)
            
            # Calculate potential reward
            potential_reward = abs(expected_return)
            
            # Calculate potential risk (using VaR as risk measure)
            potential_risk = max(var_1day, volatility * 0.5)  # Fallback to half volatility
            
            # Calculate reward/risk ratio
            reward_risk_ratio = potential_reward / potential_risk if potential_risk > 0 else 0.0
            
            # Calculate weighted confidence score
            alpha_contribution = alpha_confidence * self.alpha_model_weight
            risk_contribution = min(reward_risk_ratio / self.minimum_reward_risk_ratio, 1.0) * self.risk_model_weight
            combined_confidence = alpha_contribution + risk_contribution
            
            # Update decision metrics
            decision.update({
                'confidence': combined_confidence,
                'reward_risk_ratio': reward_risk_ratio,
                'alpha_contribution': alpha_contribution,
                'risk_contribution': risk_contribution
            })
            
            # Decision logic
            if combined_confidence < self.skip_threshold:
                decision['rationale'] = f"Low confidence ({combined_confidence:.3f} < {self.skip_threshold})"
                
            elif reward_risk_ratio < self.minimum_reward_risk_ratio:
                decision['rationale'] = f"Poor risk/reward ratio ({reward_risk_ratio:.3f} < {self.minimum_reward_risk_ratio})"
                
            elif combined_confidence < self.confidence_threshold:
                decision['rationale'] = f"Insufficient confidence ({combined_confidence:.3f} < {self.confidence_threshold})"
                
            else:
                # Trading opportunity identified
                decision['should_trade'] = True
                
                # Determine action based on alpha prediction
                if alpha_prediction > 0.05:  # Bullish threshold
                    decision['action'] = 'buy' if current_position < 0.5 else 'hold'
                elif alpha_prediction < -0.05:  # Bearish threshold
                    decision['action'] = 'sell' if current_position > -0.5 else 'hold'
                else:
                    decision['action'] = 'hold'
                
                # Calculate position size using Kelly-like sizing
                kelly_fraction = self._calculate_kelly_sizing(
                    expected_return, volatility, combined_confidence
                )
                
                if decision['action'] in ['buy', 'sell']:
                    decision['size'] = kelly_fraction * (1 if decision['action'] == 'buy' else -1)
                    decision['rationale'] = f"Strong opportunity: R/R={reward_risk_ratio:.2f}, Conf={combined_confidence:.3f}"
                
            # Store decision in history
            self.decision_history.append(decision.copy())
            
            return decision
            
        except Exception as e:
            self.logger.error(f"Error evaluating trading opportunity for {asset}: {e}")
            decision['rationale'] = f"Evaluation error: {str(e)}"
            return decision
    
    def _calculate_kelly_sizing(self, 
                               expected_return: float, 
                               volatility: float, 
                               confidence: float) -> float:
        """
        Calculate Kelly-style position sizing
        
        Args:
            expected_return: Expected return for the trade
            volatility: Asset volatility
            confidence: Combined confidence score
            
        Returns:
            Position size fraction (0-0.25)
        """
        if volatility <= 0 or expected_return == 0:
            return 0.0
        
        # Simplified Kelly calculation: f = μ / σ²
        # where μ is expected return and σ² is variance
        variance = volatility ** 2
        raw_kelly = expected_return / variance
        
        # Apply confidence scaling and caps
        confidence_adjusted_kelly = raw_kelly * confidence
        
        # Cap at 25% maximum position
        max_position = 0.25
        return max(min(abs(confidence_adjusted_kelly), max_position), 0.0)
    
    def evaluate_portfolio_opportunities(self, 
                                       portfolio_data: Dict[str, Dict],
                                       current_positions: Dict[str, float]) -> Dict[str, Dict]:
        """
        Evaluate trading opportunities for all assets in the portfolio
        
        Args:
            portfolio_data: Dictionary with asset data including alpha signals and risk metrics
            current_positions: Current position sizes for each asset
            
        Returns:
            Dictionary of trading decisions for each asset
        """
        decisions = {}
        
        for asset, data in portfolio_data.items():
            alpha_signal = data.get('alpha_signal', {})
            risk_metrics = data.get('risk_metrics', {})
            market_data = data.get('market_data', {})
            current_position = current_positions.get(asset, 0.0)
            
            decision = self.evaluate_trading_opportunity(
                asset=asset,
                alpha_signal=alpha_signal,
                risk_metrics=risk_metrics,
                current_position=current_position,
                market_data=market_data
            )
            
            decisions[asset] = decision
        
        return decisions
    
    def get_decision_summary(self, decisions: Dict[str, Dict]) -> Dict[str, Any]:
        """
        Generate summary of trading decisions
        
        Args:
            decisions: Dictionary of asset trading decisions
            
        Returns:
            Summary statistics and recommendations
        """
        total_assets = len(decisions)
        trading_recommended = sum(1 for d in decisions.values() if d['should_trade'])
        buy_signals = sum(1 for d in decisions.values() if d['action'] == 'buy')
        sell_signals = sum(1 for d in decisions.values() if d['action'] == 'sell')
        
        avg_confidence = np.mean([d['confidence'] for d in decisions.values()])
        avg_reward_risk = np.mean([d['reward_risk_ratio'] for d in decisions.values()])
        
        summary = {
            'timestamp': datetime.now(),
            'total_assets_evaluated': total_assets,
            'trading_opportunities': trading_recommended,
            'buy_signals': buy_signals,
            'sell_signals': sell_signals,
            'hold_recommendations': total_assets - trading_recommended,
            'average_confidence': avg_confidence,
            'average_reward_risk_ratio': avg_reward_risk,
            'portfolio_action_recommended': trading_recommended > 0
        }
        
        return summary
    
    def get_recent_performance(self, lookback_periods: int = 288) -> Dict[str, Any]:
        """
        Get performance metrics for recent decisions (288 = 24 hours of 5-min intervals)
        
        Args:
            lookback_periods: Number of recent decision periods to analyze
            
        Returns:
            Performance metrics
        """
        if len(self.decision_history) < 2:
            return {'status': 'insufficient_data'}
        
        recent_decisions = self.decision_history[-lookback_periods:]
        
        total_decisions = len(recent_decisions)
        trading_decisions = [d for d in recent_decisions if d['should_trade']]
        trading_rate = len(trading_decisions) / total_decisions if total_decisions > 0 else 0
        
        avg_confidence = np.mean([d['confidence'] for d in recent_decisions])
        avg_reward_risk = np.mean([d['reward_risk_ratio'] for d in recent_decisions])
        
        performance = {
            'total_decision_periods': total_decisions,
            'trading_decisions': len(trading_decisions),
            'trading_rate': trading_rate,
            'average_confidence': avg_confidence,
            'average_reward_risk_ratio': avg_reward_risk,
            'timespan_hours': total_decisions * 5 / 60,  # 5-minute intervals
        }
        
        return performance