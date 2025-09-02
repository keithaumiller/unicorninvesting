"""
Comprehensive Risk Mitigation Strategy for Myportolio
Advanced risk management system integrating multiple risk controls
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Union
import logging
import json
import sys
import os

# Import existing risk components
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from eth_basic_risk import ETHBasicRisk

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from utilities.kelly_criterion import KellyCriterionCalculator

logger = logging.getLogger(__name__)

class ComprehensiveRiskManager:
    """
    Comprehensive Risk Mitigation Strategy
    
    Integrates multiple risk management approaches:
    1. VaR (Value at Risk) monitoring
    2. Maximum Drawdown controls
    3. Kelly Criterion position sizing
    4. Correlation monitoring
    5. Volatility tracking
    6. Emergency stop mechanisms
    7. Real-time risk alerts
    """
    
    def __init__(self, config_path: str = None):
        """Initialize comprehensive risk management system"""
        
        # Load risk parameters
        if config_path is None:
            config_path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'risk_parameters.json')
        
        with open(config_path, 'r') as f:
            self.risk_config = json.load(f)
        
        # Initialize component risk managers
        self.basic_risk = ETHBasicRisk(
            max_drawdown=self.risk_config['max_drawdown'],
            max_position_pct=self.risk_config['position_limits']['max_position_size'],
            var_confidence=0.05
        )
        
        self.kelly_calculator = KellyCriterionCalculator(
            lookback_window=30,
            max_kelly_fraction=0.25,
            min_win_rate=0.35,
            risk_adjustment=0.5
        )
        
        # Risk monitoring state
        self.risk_state = {
            'current_var_1day': 0.0,
            'current_var_1week': 0.0,
            'portfolio_volatility': 0.0,
            'current_drawdown': 0.0,
            'correlation_matrix': {},
            'last_risk_check': None,
            'emergency_stop_triggered': False,
            'risk_alerts': []
        }
        
        # Performance tracking
        self.portfolio_history = []
        self.risk_metrics_history = []
        
        logger.info("Comprehensive Risk Manager initialized")
    
    def add_risk_check(self, portfolio_data: Dict) -> Dict:
        """
        Main risk assessment function - performs comprehensive risk evaluation
        
        Args:
            portfolio_data: Complete portfolio information
            
        Returns:
            Risk assessment with recommendations and controls
        """
        
        timestamp = datetime.now()
        
        # 1. Update portfolio history
        self._update_portfolio_history(portfolio_data, timestamp)
        
        # 2. Calculate current risk metrics
        risk_metrics = self._calculate_risk_metrics(portfolio_data)
        
        # 3. Validate against all risk limits
        risk_violations = self._check_risk_violations(risk_metrics)
        
        # 4. Calculate position sizing recommendations
        position_recommendations = self._calculate_position_sizing(portfolio_data, risk_metrics)
        
        # 5. Emergency stop assessment
        emergency_assessment = self._assess_emergency_conditions(risk_metrics)
        
        # 6. Generate risk alerts
        alerts = self._generate_risk_alerts(risk_violations, emergency_assessment)
        
        # 7. Compile comprehensive assessment
        risk_assessment = {
            'timestamp': timestamp,
            'risk_metrics': risk_metrics,
            'risk_violations': risk_violations,
            'position_recommendations': position_recommendations,
            'emergency_assessment': emergency_assessment,
            'risk_alerts': alerts,
            'overall_risk_score': self._calculate_overall_risk_score(risk_metrics, risk_violations),
            'recommended_actions': self._generate_recommendations(risk_violations, emergency_assessment)
        }
        
        # Update risk state
        self.risk_state.update({
            'current_var_1day': risk_metrics.get('var_1day', 0.0),
            'current_var_1week': risk_metrics.get('var_1week', 0.0),
            'portfolio_volatility': risk_metrics.get('portfolio_volatility', 0.0),
            'current_drawdown': risk_metrics.get('current_drawdown', 0.0),
            'last_risk_check': timestamp,
            'risk_alerts': alerts
        })
        
        # Store risk metrics history
        self.risk_metrics_history.append(risk_assessment)
        
        # Keep only last 100 assessments
        if len(self.risk_metrics_history) > 100:
            self.risk_metrics_history = self.risk_metrics_history[-100:]
        
        logger.info(f"Risk assessment completed: Overall risk score {risk_assessment['overall_risk_score']:.2f}")
        
        return risk_assessment
    
    def _update_portfolio_history(self, portfolio_data: Dict, timestamp: datetime):
        """Update portfolio performance history"""
        
        portfolio_record = {
            'timestamp': timestamp,
            'total_value': portfolio_data.get('total_value', 0),
            'cash': portfolio_data.get('cash', 0),
            'positions': portfolio_data.get('positions', {}),
            'market_values': portfolio_data.get('market_values', {})
        }
        
        self.portfolio_history.append(portfolio_record)
        
        # Keep only last 252 days (1 year)
        if len(self.portfolio_history) > 252:
            self.portfolio_history = self.portfolio_history[-252:]
        
        # Update basic risk manager
        self.basic_risk.update_portfolio_metrics(portfolio_data.get('total_value', 0))
    
    def _calculate_risk_metrics(self, portfolio_data: Dict) -> Dict:
        """Calculate comprehensive risk metrics"""
        
        metrics = {}
        
        # 1. VaR calculations
        if len(self.portfolio_history) >= 5:
            returns = self._calculate_portfolio_returns()
            metrics['var_1day'] = self._calculate_var(returns, 0.05)
            metrics['var_1week'] = self._calculate_var(returns, 0.05) * np.sqrt(7)
        else:
            metrics['var_1day'] = 0.0
            metrics['var_1week'] = 0.0
        
        # 2. Volatility
        if len(self.portfolio_history) >= 10:
            returns = self._calculate_portfolio_returns()
            metrics['portfolio_volatility'] = np.std(returns) * np.sqrt(252)  # Annualized
        else:
            metrics['portfolio_volatility'] = 0.0
        
        # 3. Drawdown
        metrics['current_drawdown'] = self.basic_risk.current_drawdown
        
        # 4. Position concentrations
        total_value = portfolio_data.get('total_value', 1)
        positions = portfolio_data.get('positions', {})
        market_values = portfolio_data.get('market_values', {})
        
        position_weights = {}
        for symbol, quantity in positions.items():
            market_value = market_values.get(symbol, 0)
            position_weights[symbol] = market_value / total_value if total_value > 0 else 0
        
        metrics['position_weights'] = position_weights
        metrics['max_position_weight'] = max(position_weights.values()) if position_weights else 0
        
        # 5. Correlation (simplified - would need price data for full calculation)
        metrics['estimated_correlation'] = 0.7  # ETH-BTC typical correlation
        
        # 6. Sharpe ratio (if sufficient history)
        if len(self.portfolio_history) >= 30:
            returns = self._calculate_portfolio_returns()
            if len(returns) > 0 and np.std(returns) > 0:
                metrics['sharpe_ratio'] = np.mean(returns) / np.std(returns) * np.sqrt(252)
            else:
                metrics['sharpe_ratio'] = 0.0
        else:
            metrics['sharpe_ratio'] = 0.0
        
        return metrics
    
    def _calculate_portfolio_returns(self) -> List[float]:
        """Calculate portfolio returns from history"""
        
        if len(self.portfolio_history) < 2:
            return []
        
        returns = []
        for i in range(1, len(self.portfolio_history)):
            prev_value = self.portfolio_history[i-1]['total_value']
            curr_value = self.portfolio_history[i]['total_value']
            
            if prev_value > 0:
                ret = (curr_value / prev_value) - 1
                returns.append(ret)
        
        return returns
    
    def _calculate_var(self, returns: List[float], confidence_level: float) -> float:
        """Calculate Value at Risk"""
        
        if len(returns) < 5:
            return 0.05  # Default 5% VaR
        
        return abs(np.percentile(returns, confidence_level * 100))
    
    def _check_risk_violations(self, risk_metrics: Dict) -> List[Dict]:
        """Check for risk limit violations"""
        
        violations = []
        
        # 1. VaR limits
        if risk_metrics['var_1day'] > self.risk_config['var_limit_1day']:
            violations.append({
                'type': 'var_1day_violation',
                'severity': 'high',
                'current': risk_metrics['var_1day'],
                'limit': self.risk_config['var_limit_1day'],
                'description': f"1-day VaR {risk_metrics['var_1day']:.1%} exceeds limit {self.risk_config['var_limit_1day']:.1%}"
            })
        
        if risk_metrics['var_1week'] > self.risk_config['var_limit_1week']:
            violations.append({
                'type': 'var_1week_violation',
                'severity': 'high',
                'current': risk_metrics['var_1week'],
                'limit': self.risk_config['var_limit_1week'],
                'description': f"1-week VaR {risk_metrics['var_1week']:.1%} exceeds limit {self.risk_config['var_limit_1week']:.1%}"
            })
        
        # 2. Volatility limits
        if risk_metrics['portfolio_volatility'] > self.risk_config['max_portfolio_volatility']:
            violations.append({
                'type': 'volatility_violation',
                'severity': 'medium',
                'current': risk_metrics['portfolio_volatility'],
                'limit': self.risk_config['max_portfolio_volatility'],
                'description': f"Portfolio volatility {risk_metrics['portfolio_volatility']:.1%} exceeds limit {self.risk_config['max_portfolio_volatility']:.1%}"
            })
        
        # 3. Drawdown limits
        if risk_metrics['current_drawdown'] > self.risk_config['max_drawdown']:
            violations.append({
                'type': 'drawdown_violation',
                'severity': 'critical',
                'current': risk_metrics['current_drawdown'],
                'limit': self.risk_config['max_drawdown'],
                'description': f"Current drawdown {risk_metrics['current_drawdown']:.1%} exceeds limit {self.risk_config['max_drawdown']:.1%}"
            })
        
        # 4. Position concentration
        if risk_metrics['max_position_weight'] > self.risk_config['max_single_asset_weight']:
            violations.append({
                'type': 'concentration_violation',
                'severity': 'medium',
                'current': risk_metrics['max_position_weight'],
                'limit': self.risk_config['max_single_asset_weight'],
                'description': f"Maximum position weight {risk_metrics['max_position_weight']:.1%} exceeds limit {self.risk_config['max_single_asset_weight']:.1%}"
            })
        
        return violations
    
    def _calculate_position_sizing(self, portfolio_data: Dict, risk_metrics: Dict) -> Dict:
        """Calculate optimal position sizing using Kelly Criterion and risk constraints"""
        
        total_value = portfolio_data.get('total_value', 0)
        current_positions = portfolio_data.get('positions', {})
        
        recommendations = {}
        
        # Apply Kelly Criterion constraints
        max_kelly_position = total_value * self.kelly_calculator.max_kelly_fraction
        
        # Apply risk-based position limits
        max_risk_position = total_value * self.risk_config['position_limits']['max_position_size']
        
        # Conservative approach: use the more restrictive limit
        max_position_value = min(max_kelly_position, max_risk_position)
        
        # Calculate recommendations for each asset
        for asset in ['ETH', 'BTC']:
            target_allocation = self.risk_config['risk_budget_allocation'][asset]
            target_value = total_value * target_allocation
            
            # Apply risk constraints
            recommended_value = min(target_value, max_position_value)
            
            recommendations[asset] = {
                'target_allocation': target_allocation,
                'target_value': target_value,
                'risk_adjusted_value': recommended_value,
                'current_value': portfolio_data.get('market_values', {}).get(asset, 0),
                'recommended_action': self._determine_action(
                    portfolio_data.get('market_values', {}).get(asset, 0),
                    recommended_value
                )
            }
        
        return recommendations
    
    def _determine_action(self, current_value: float, target_value: float) -> str:
        """Determine recommended action based on current vs target position"""
        
        tolerance = 0.05  # 5% tolerance
        
        if current_value == 0 and target_value > 0:
            return 'BUY'
        elif current_value > 0 and target_value == 0:
            return 'SELL'
        elif abs(current_value - target_value) / max(current_value, target_value, 1) > tolerance:
            if current_value < target_value:
                return 'BUY'
            else:
                return 'SELL'
        else:
            return 'HOLD'
    
    def _assess_emergency_conditions(self, risk_metrics: Dict) -> Dict:
        """Assess if emergency stop conditions are met"""
        
        emergency_triggers = []
        emergency_score = 0
        
        # Critical drawdown
        if risk_metrics['current_drawdown'] > self.risk_config['max_drawdown'] * 1.2:
            emergency_triggers.append('critical_drawdown')
            emergency_score += 3
        
        # Extreme VaR
        if risk_metrics['var_1day'] > self.risk_config['var_limit_1day'] * 2:
            emergency_triggers.append('extreme_var')
            emergency_score += 2
        
        # Extreme volatility
        if risk_metrics['portfolio_volatility'] > self.risk_config['max_portfolio_volatility'] * 1.5:
            emergency_triggers.append('extreme_volatility')
            emergency_score += 2
        
        # Multiple violations
        if len(emergency_triggers) >= 2:
            emergency_score += 1
        
        emergency_stop_recommended = emergency_score >= 3
        
        return {
            'emergency_triggers': emergency_triggers,
            'emergency_score': emergency_score,
            'emergency_stop_recommended': emergency_stop_recommended,
            'severity': 'critical' if emergency_stop_recommended else ('high' if emergency_score >= 2 else 'low')
        }
    
    def _generate_risk_alerts(self, violations: List[Dict], emergency_assessment: Dict) -> List[Dict]:
        """Generate risk alerts and notifications"""
        
        alerts = []
        
        # Emergency alerts
        if emergency_assessment['emergency_stop_recommended']:
            alerts.append({
                'type': 'emergency_stop',
                'severity': 'critical',
                'message': 'EMERGENCY STOP RECOMMENDED - Multiple critical risk thresholds exceeded',
                'timestamp': datetime.now(),
                'triggers': emergency_assessment['emergency_triggers']
            })
        
        # Violation alerts
        for violation in violations:
            alerts.append({
                'type': f"risk_violation_{violation['type']}",
                'severity': violation['severity'],
                'message': violation['description'],
                'timestamp': datetime.now(),
                'recommendation': self._get_violation_recommendation(violation)
            })
        
        return alerts
    
    def _get_violation_recommendation(self, violation: Dict) -> str:
        """Get specific recommendation for risk violation"""
        
        if violation['type'] == 'drawdown_violation':
            return "Consider reducing position sizes and implementing stop-losses"
        elif violation['type'] == 'var_1day_violation':
            return "Reduce portfolio risk exposure immediately"
        elif violation['type'] == 'volatility_violation':
            return "Diversify positions or reduce leverage"
        elif violation['type'] == 'concentration_violation':
            return "Rebalance portfolio to reduce single asset concentration"
        else:
            return "Review and adjust risk parameters"
    
    def _calculate_overall_risk_score(self, risk_metrics: Dict, violations: List[Dict]) -> float:
        """Calculate overall risk score (0-10, where 10 is highest risk)"""
        
        score = 0
        
        # Base score from violations
        for violation in violations:
            if violation['severity'] == 'critical':
                score += 3
            elif violation['severity'] == 'high':
                score += 2
            elif violation['severity'] == 'medium':
                score += 1
        
        # Additional score from metrics
        if risk_metrics['current_drawdown'] > 0.05:
            score += 1
        if risk_metrics['portfolio_volatility'] > 0.15:
            score += 1
        if risk_metrics['var_1day'] > 0.03:
            score += 1
        
        return min(score, 10)  # Cap at 10
    
    def _generate_recommendations(self, violations: List[Dict], emergency_assessment: Dict) -> List[str]:
        """Generate actionable recommendations"""
        
        recommendations = []
        
        if emergency_assessment['emergency_stop_recommended']:
            recommendations.append("IMMEDIATE ACTION: Implement emergency stop procedures")
            recommendations.append("Liquidate high-risk positions to preserve capital")
            recommendations.append("Suspend automated trading until risk conditions improve")
        
        if violations:
            recommendations.append("Review and adjust position sizes to comply with risk limits")
            recommendations.append("Consider implementing additional stop-loss mechanisms")
            recommendations.append("Monitor risk metrics more frequently during volatile periods")
        
        if not violations and not emergency_assessment['emergency_stop_recommended']:
            recommendations.append("Risk levels are within acceptable parameters")
            recommendations.append("Continue monitoring and maintain current risk controls")
        
        return recommendations
    
    def get_risk_summary(self) -> Dict:
        """Get current risk summary for monitoring"""
        
        return {
            'timestamp': datetime.now(),
            'risk_state': self.risk_state,
            'emergency_stop_triggered': self.risk_state.get('emergency_stop_triggered', False),
            'active_alerts': len(self.risk_state.get('risk_alerts', [])),
            'last_assessment': self.risk_state.get('last_risk_check'),
            'system_status': 'operational' if not self.risk_state.get('emergency_stop_triggered') else 'emergency_stop'
        }
