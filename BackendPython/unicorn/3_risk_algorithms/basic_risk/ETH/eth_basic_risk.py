"""
ETH Risk Management Algorithm - Hello World Implementation
Basic risk management for ETH trading with drawdown and VaR controls
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging

class ETHBasicRisk:
    """
    Basic ETH Risk Management Algorithm
    
    Implements simple risk controls:
    - Maximum drawdown limits
    - Position size limits
    - Value-at-Risk (VaR) estimation
    - Portfolio volatility monitoring
    
    This is a "Hello World" implementation for the LEAN framework integration.
    """
    
    def __init__(self, max_drawdown=0.15, max_position_pct=0.8, var_confidence=0.05):
        """
        Initialize ETH risk management
        
        Args:
            max_drawdown: Maximum allowed portfolio drawdown (15% default)
            max_position_pct: Maximum position size as % of portfolio (80% default)
            var_confidence: VaR confidence level (5% default = 95% VaR)
        """
        self.max_drawdown = max_drawdown
        self.max_position_pct = max_position_pct
        self.var_confidence = var_confidence
        
        # Risk monitoring state
        self.portfolio_high_water_mark = 0.0
        self.current_drawdown = 0.0
        self.daily_returns = []
        
        # Risk metrics
        self.current_var = 0.0
        self.portfolio_volatility = 0.0
        
        logging.info(f"ETH Risk Algorithm initialized: max_dd={max_drawdown}, max_pos={max_position_pct}")
    
    def update_portfolio_metrics(self, portfolio_value: float):
        """Update portfolio risk metrics"""
        
        # Update high water mark
        if portfolio_value > self.portfolio_high_water_mark:
            self.portfolio_high_water_mark = portfolio_value
            self.current_drawdown = 0.0
        else:
            # Calculate current drawdown
            self.current_drawdown = (self.portfolio_high_water_mark - portfolio_value) / self.portfolio_high_water_mark
        
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
        else:
            self.daily_returns.append(portfolio_value)  # Store initial value, not return
    
    def calculate_var(self, returns: List[float] = None) -> float:
        """Calculate Value-at-Risk (VaR)"""
        
        if returns is None:
            returns = self.daily_returns
            
        if len(returns) < 5:
            return 0.05  # Default 5% VaR if insufficient data
            
        # Calculate historical VaR
        returns_array = np.array(returns)
        var = np.percentile(returns_array, self.var_confidence * 100)
        
        self.current_var = abs(var)  # Store as positive value
        return self.current_var
    
    def validate_position_size(self, proposed_position_value: float, total_portfolio_value: float) -> Dict:
        """Validate if proposed position size is within risk limits"""
        
        position_pct = proposed_position_value / total_portfolio_value if total_portfolio_value > 0 else 0
        
        if position_pct > self.max_position_pct:
            return {
                'approved': False,
                'reason': f'Position size {position_pct:.1%} exceeds limit {self.max_position_pct:.1%}',
                'max_allowed_value': total_portfolio_value * self.max_position_pct
            }
        
        return {
            'approved': True,
            'position_pct': position_pct,
            'max_allowed_value': total_portfolio_value * self.max_position_pct
        }
    
    def validate_drawdown(self, proposed_portfolio_value: float) -> Dict:
        """Validate if proposed portfolio value would exceed drawdown limits"""
        
        if self.portfolio_high_water_mark == 0:
            return {'approved': True, 'drawdown': 0.0}
            
        proposed_drawdown = (self.portfolio_high_water_mark - proposed_portfolio_value) / self.portfolio_high_water_mark
        
        if proposed_drawdown > self.max_drawdown:
            return {
                'approved': False,
                'reason': f'Proposed drawdown {proposed_drawdown:.1%} exceeds limit {self.max_drawdown:.1%}',
                'current_drawdown': self.current_drawdown,
                'proposed_drawdown': proposed_drawdown
            }
        
        return {
            'approved': True,
            'current_drawdown': self.current_drawdown,
            'proposed_drawdown': proposed_drawdown
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
