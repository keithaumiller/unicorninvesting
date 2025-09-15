"""
ETH Risk Management Algorithm - Enhanced with Performance Logging
Basic risk management for ETH trading with drawdown and VaR controls
Now includes comprehensive logging for risk decision attribution analysis
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging
import sys
from pathlib import Path

# Performance logging integration
try:
    sys.path.append(str(Path(__file__).parent.parent / "simulations"))
    from performance_logger import PerformanceLogger
    PERFORMANCE_LOGGING_AVAILABLE = True
except ImportError:
    PERFORMANCE_LOGGING_AVAILABLE = False
    logging.warning("Performance logging not available")

class ETHBasicRisk:
    """
    Enhanced ETH Risk Management Algorithm with comprehensive performance logging
    
    Implements risk controls with detailed decision tracking:
    - Maximum drawdown limits with impact analysis
    - Position size limits with reasoning
    - Value-at-Risk (VaR) estimation and validation
    - Portfolio volatility monitoring with alerts
    - Risk decision attribution and performance impact tracking
    """
    
    def __init__(self, max_drawdown=0.15, max_position_pct=0.8, var_confidence=0.05, performance_logger: 'PerformanceLogger' = None):
        """
        Initialize ETH risk management with performance logging
        
        Args:
            max_drawdown: Maximum allowed portfolio drawdown (15% default)
            max_position_pct: Maximum position size as % of portfolio (80% default)
            var_confidence: VaR confidence level (5% default = 95% VaR)
            performance_logger: Performance logger instance for detailed tracking
        """
        self.max_drawdown = max_drawdown
        self.max_position_pct = max_position_pct
        self.var_confidence = var_confidence
        
        # Performance logging
        self.performance_logger = performance_logger
        self.log_enabled = performance_logger is not None
        
        # Risk monitoring state
        self.portfolio_high_water_mark = 0.0
        self.current_drawdown = 0.0
        self.daily_returns = []
        
        # Risk metrics
        self.current_var = 0.0
        self.portfolio_volatility = 0.0
        
        # Decision tracking
        self.total_risk_checks = 0
        self.decisions_rejected = 0
        self.decisions_approved = 0
        self.decision_history = []
        
        logging.info(f"ETH Risk Algorithm initialized: max_dd={max_drawdown}, max_pos={max_position_pct}")
        logging.info(f"Performance logging: {'ENABLED' if self.log_enabled else 'DISABLED'}")
    
    def update_portfolio_metrics(self, portfolio_value: float):
        """Update portfolio risk metrics with detailed logging"""
        
        previous_drawdown = self.current_drawdown
        previous_hwm = self.portfolio_high_water_mark
        
        # Update high water mark
        if portfolio_value > self.portfolio_high_water_mark:
            self.portfolio_high_water_mark = portfolio_value
            old_drawdown = self.current_drawdown
            self.current_drawdown = 0.0
            
            if self.log_enabled and old_drawdown > 0:
                self.performance_logger.logger.info(f"New high water mark: ${portfolio_value:,.2f} (recovered from {old_drawdown:.2%} drawdown)")
        else:
            # Calculate current drawdown
            self.current_drawdown = (self.portfolio_high_water_mark - portfolio_value) / self.portfolio_high_water_mark
            
            if self.log_enabled and self.current_drawdown != previous_drawdown:
                self.performance_logger.logger.info(f"Drawdown update: {self.current_drawdown:.2%} (was {previous_drawdown:.2%})")
        
        # Update daily returns (simplified)
        if len(self.daily_returns) > 0:
            last_value = self.daily_returns[-1] if self.daily_returns else portfolio_value
            if last_value != 0:
                daily_return = (portfolio_value / last_value) - 1
            else:
                daily_return = 0.0
            self.daily_returns.append(daily_return)
            
            # Keep only last 30 days
            if len(self.daily_returns) > 30:
                self.daily_returns = self.daily_returns[-30:]
                
            # Log significant daily moves
            if self.log_enabled and abs(daily_return) > 0.05:  # 5% daily move
                self.performance_logger.logger.warning(f"Large daily move: {daily_return:+.2%} (portfolio: ${portfolio_value:,.2f})")
        else:
            self.daily_returns.append(portfolio_value)  # Store initial value, not return
    
    def calculate_var(self, returns: List[float] = None) -> float:
        """Calculate Value-at-Risk (VaR) with detailed logging"""
        
        if returns is None:
            returns = self.daily_returns
            
        if len(returns) < 5:
            default_var = 0.05
            if self.log_enabled:
                self.performance_logger.logger.debug(f"Using default VaR {default_var:.3f} due to insufficient return data ({len(returns)} points)")
            return default_var
            
        # Calculate historical VaR
        returns_array = np.array(returns)
        var = np.percentile(returns_array, self.var_confidence * 100)
        
        self.current_var = abs(var)  # Store as positive value
        
        if self.log_enabled:
            percentile = self.var_confidence * 100
            self.performance_logger.logger.debug(f"VaR calculated: {self.current_var:.3f} ({percentile:.1f}th percentile of {len(returns)} returns)")
        
        return self.current_var
    
    def validate_position_size(self, proposed_position_value: float, total_portfolio_value: float, asset: str = "ETHUSD") -> Dict:
        """Validate position size with comprehensive logging and reasoning"""
        
        self.total_risk_checks += 1
        
        position_pct = proposed_position_value / total_portfolio_value if total_portfolio_value > 0 else 0
        
        risk_metrics = {
            'position_pct': position_pct,
            'position_value': proposed_position_value,
            'portfolio_value': total_portfolio_value,
            'max_allowed_pct': self.max_position_pct,
            'max_allowed_value': total_portfolio_value * self.max_position_pct
        }
        
        if position_pct > self.max_position_pct:
            # Position rejected
            self.decisions_rejected += 1
            
            reason = f'Position size {position_pct:.1%} exceeds limit {self.max_position_pct:.1%}'
            max_allowed_value = total_portfolio_value * self.max_position_pct
            impact_on_position = proposed_position_value - max_allowed_value
            
            decision_result = {
                'approved': False,
                'reason': reason,
                'max_allowed_value': max_allowed_value,
                'position_adjustment': impact_on_position,
                'risk_metrics': risk_metrics
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="POSITION_LIMIT",
                    proposed_action=f"Set position to ${proposed_position_value:,.2f}",
                    approved=False,
                    reason=reason,
                    risk_metrics=risk_metrics,
                    impact_on_position=-impact_on_position
                )
        else:
            # Position approved
            self.decisions_approved += 1
            
            reason = f'Position size {position_pct:.1%} within limit {self.max_position_pct:.1%}'
            
            decision_result = {
                'approved': True,
                'reason': reason,
                'position_pct': position_pct,
                'max_allowed_value': total_portfolio_value * self.max_position_pct,
                'risk_metrics': risk_metrics
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="POSITION_LIMIT",
                    proposed_action=f"Set position to ${proposed_position_value:,.2f}",
                    approved=True,
                    reason=reason,
                    risk_metrics=risk_metrics,
                    impact_on_position=0.0
                )
        
        # Store decision in history
        decision_data = {
            'timestamp': datetime.now().isoformat(),
            'type': 'position_limit',
            'approved': decision_result['approved'],
            'position_pct': position_pct,
            'reason': decision_result['reason']
        }
        self.decision_history.append(decision_data)
        
        return decision_result
    
    def validate_drawdown(self, proposed_portfolio_value: float, asset: str = "ETHUSD") -> Dict:
        """Validate drawdown limits with comprehensive logging and impact analysis"""
        
        self.total_risk_checks += 1
        
        if self.portfolio_high_water_mark == 0:
            # No baseline established yet
            self.decisions_approved += 1
            
            decision_result = {
                'approved': True, 
                'drawdown': 0.0,
                'reason': 'No high water mark established yet'
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="DRAWDOWN_LIMIT",
                    proposed_action=f"Portfolio value ${proposed_portfolio_value:,.2f}",
                    approved=True,
                    reason="Initial portfolio setup - no drawdown baseline",
                    risk_metrics={'portfolio_value': proposed_portfolio_value, 'high_water_mark': 0},
                    impact_on_position=0.0
                )
            
            return decision_result
            
        proposed_drawdown = (self.portfolio_high_water_mark - proposed_portfolio_value) / self.portfolio_high_water_mark
        
        risk_metrics = {
            'proposed_drawdown': proposed_drawdown,
            'max_allowed_drawdown': self.max_drawdown,
            'proposed_value': proposed_portfolio_value,
            'high_water_mark': self.portfolio_high_water_mark,
            'current_drawdown': self.current_drawdown
        }
        
        if proposed_drawdown > self.max_drawdown:
            # Drawdown limit exceeded
            self.decisions_rejected += 1
            
            reason = f'Proposed drawdown {proposed_drawdown:.1%} exceeds limit {self.max_drawdown:.1%}'
            
            # Calculate minimum portfolio value to stay within limits
            min_allowed_value = self.portfolio_high_water_mark * (1 - self.max_drawdown)
            value_adjustment = proposed_portfolio_value - min_allowed_value
            
            decision_result = {
                'approved': False,
                'reason': reason,
                'proposed_drawdown': proposed_drawdown,
                'max_allowed_drawdown': self.max_drawdown,
                'min_allowed_value': min_allowed_value,
                'value_adjustment_required': -value_adjustment,
                'risk_metrics': risk_metrics
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="DRAWDOWN_LIMIT",
                    proposed_action=f"Portfolio value ${proposed_portfolio_value:,.2f}",
                    approved=False,
                    reason=reason,
                    risk_metrics=risk_metrics,
                    impact_on_position=value_adjustment
                )
        else:
            # Drawdown within limits
            self.decisions_approved += 1
            
            reason = f'Proposed drawdown {proposed_drawdown:.1%} within limit {self.max_drawdown:.1%}'
            
            decision_result = {
                'approved': True,
                'drawdown': proposed_drawdown,
                'reason': reason,
                'risk_metrics': risk_metrics
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="DRAWDOWN_LIMIT", 
                    proposed_action=f"Portfolio value ${proposed_portfolio_value:,.2f}",
                    approved=True,
                    reason=reason,
                    risk_metrics=risk_metrics,
                    impact_on_position=0.0
                )
        
        # Store decision in history
        decision_data = {
            'timestamp': datetime.now().isoformat(),
            'type': 'drawdown_limit',
            'approved': decision_result['approved'],
            'proposed_drawdown': proposed_drawdown,
            'reason': decision_result['reason']
        }
        self.decision_history.append(decision_data)
        
        return decision_result

    def validate_var_limit(self, portfolio_value: float, asset: str = "ETHUSD") -> Dict:
        """Validate VaR limits with detailed logging"""
        
        self.total_risk_checks += 1
        
        # Calculate current VaR
        current_var = self.calculate_var()
        
        # Define VaR limit (could be configurable)
        var_limit = 0.06  # 6% daily VaR limit
        
        risk_metrics = {
            'current_var': current_var,
            'var_limit': var_limit,
            'portfolio_value': portfolio_value,
            'returns_sample_size': len(self.daily_returns)
        }
        
        if current_var > var_limit:
            # VaR limit exceeded
            self.decisions_rejected += 1
            
            reason = f'Current VaR {current_var:.3f} exceeds limit {var_limit:.3f}'
            
            decision_result = {
                'approved': False,
                'reason': reason,
                'current_var': current_var,
                'var_limit': var_limit,
                'risk_metrics': risk_metrics
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="VAR_LIMIT",
                    proposed_action="Continue current risk exposure",
                    approved=False,
                    reason=reason,
                    risk_metrics=risk_metrics,
                    impact_on_position=0.0
                )
        else:
            # VaR within limits
            self.decisions_approved += 1
            
            reason = f'Current VaR {current_var:.3f} within limit {var_limit:.3f}'
            
            decision_result = {
                'approved': True,
                'current_var': current_var,
                'var_limit': var_limit,
                'reason': reason,
                'risk_metrics': risk_metrics
            }
            
            if self.log_enabled:
                self.performance_logger.log_risk_decision(
                    asset=asset,
                    decision_type="VAR_LIMIT",
                    proposed_action="Continue current risk exposure",
                    approved=True,
                    reason=reason,
                    risk_metrics=risk_metrics,
                    impact_on_position=0.0
                )
        
        return decision_result

    def get_risk_summary(self) -> Dict:
        """Get comprehensive risk decision summary for performance analysis"""
        
        approval_rate = (self.decisions_approved / self.total_risk_checks) if self.total_risk_checks > 0 else 0
        
        return {
            'total_decisions': self.total_risk_checks,
            'decisions_approved': self.decisions_approved,
            'decisions_rejected': self.decisions_rejected,
            'approval_rate': approval_rate,
            'current_drawdown': self.current_drawdown,
            'current_var': self.current_var,
            'portfolio_hwm': self.portfolio_high_water_mark,
            'decision_types': {
                'position_limits': len([d for d in self.decision_history if d['type'] == 'position_limit']),
                'drawdown_limits': len([d for d in self.decision_history if d['type'] == 'drawdown_limit'])
            }
        }
    
    def validate_portfolio_risk(self, portfolio_data: Dict) -> Dict:
        """
        Comprehensive portfolio risk validation
        
        Args:
            portfolio_data: Dict containing:
                - total_value: Current portfolio value
                - positions: Dict of positions {'symbol': quantity}
                - current_prices: Dict of current prices {'symbol': price}
                - cash: Available cash
        
        Returns:
            Dict with validation result and risk metrics
        """
        
        total_value = portfolio_data.get('total_value', 0)
        positions = portfolio_data.get('positions', {})
        current_prices = portfolio_data.get('current_prices', {})
        cash = portfolio_data.get('cash', 0)
        
        # Update portfolio metrics
        self.update_portfolio_metrics(total_value)
        
        # Calculate position values
        position_values = {}
        total_position_value = 0
        
        for symbol, quantity in positions.items():
            if symbol in current_prices:
                value = quantity * current_prices[symbol]
                position_values[symbol] = value
                total_position_value += value
        
        # Validate position sizes
        for symbol, value in position_values.items():
            size_check = self.validate_position_size(value, total_value)
            if not size_check['approved']:
                return {
                    'approved': False,
                    'reason': f'{symbol}: {size_check["reason"]}',
                    'risk_metrics': self.get_risk_metrics()
                }
        
        # Validate drawdown
        drawdown_check = self.validate_drawdown(total_value)
        if not drawdown_check['approved']:
            return {
                'approved': False,
                'reason': drawdown_check['reason'],
                'risk_metrics': self.get_risk_metrics()
            }
        
        # Calculate VaR
        current_var = self.calculate_var()
        
        return {
            'approved': True,
            'total_position_pct': total_position_value / total_value if total_value > 0 else 0,
            'current_drawdown': self.current_drawdown,
            'var_estimate': current_var,
            'risk_metrics': self.get_risk_metrics()
        }
    
    def get_risk_metrics(self) -> Dict:
        """Get current risk metrics summary"""
        
        return {
            'max_drawdown_limit': self.max_drawdown,
            'current_drawdown': self.current_drawdown,
            'max_position_limit': self.max_position_pct,
            'current_var': self.current_var,
            'portfolio_hwm': self.portfolio_high_water_mark,
            'returns_history_length': len(self.daily_returns)
        }
    
    def reset_risk_state(self):
        """Reset risk management state (for testing/new strategies)"""
        
        self.portfolio_high_water_mark = 0.0
        self.current_drawdown = 0.0
        self.daily_returns = []
        self.current_var = 0.0
        
        logging.info("Risk management state reset")

def main():
    """Test ETH risk algorithm"""
    print("=" * 50)
    print("ETH Risk Management Algorithm Test")
    print(f"Timestamp: {datetime.now()}")
    print("=" * 50)
    
    # Initialize risk algorithm
    risk_algo = ETHBasicRisk(max_drawdown=0.15, max_position_pct=0.8)
    
    # Test portfolio scenarios
    test_portfolios = [
        {
            'name': 'Conservative Portfolio',
            'total_value': 100000,
            'positions': {'ETH': 20},  # 20 ETH
            'current_prices': {'ETH': 3500},
            'cash': 30000
        },
        {
            'name': 'Aggressive Portfolio', 
            'total_value': 100000,
            'positions': {'ETH': 30},  # 30 ETH
            'current_prices': {'ETH': 3500},
            'cash': -5000  # Leveraged
        },
        {
            'name': 'High Drawdown Portfolio',
            'total_value': 80000,  # Down from 100k
            'positions': {'ETH': 25},
            'current_prices': {'ETH': 3200},
            'cash': 0
        }
    ]
    
    for i, portfolio in enumerate(test_portfolios):
        print(f"\n{i+1}. Testing {portfolio['name']}:")
        print(f"   Total Value: ${portfolio['total_value']:,}")
        print(f"   ETH Position: {portfolio['positions']['ETH']} @ ${portfolio['current_prices']['ETH']}")
        
        result = risk_algo.validate_portfolio_risk(portfolio)
        
        if result['approved']:
            print(f"   ✅ Risk Check: APPROVED")
            print(f"   Position %: {result['total_position_pct']:.1%}")
            print(f"   Drawdown: {result['current_drawdown']:.1%}")
        else:
            print(f"   ❌ Risk Check: REJECTED - {result['reason']}")
        
        print(f"   Risk Metrics: {result['risk_metrics']}")
    
    print("\n✅ ETH Risk Algorithm testing complete!")

if __name__ == "__main__":
    main()
