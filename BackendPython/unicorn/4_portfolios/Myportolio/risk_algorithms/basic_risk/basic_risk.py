#!/usr/bin/env python3
"""
Basic Risk Management
Simple risk controls for portfolio management
"""

class BasicRiskManager:
    """Basic risk management implementation"""
    
    def __init__(self):
        self.max_position_size = 0.20
        self.max_portfolio_exposure = 0.95
        
    def validate_position(self, allocation: float) -> bool:
        """Validate individual position size"""
        return allocation <= self.max_position_size
        
    def validate_portfolio(self, total_exposure: float) -> bool:
        """Validate total portfolio exposure"""
        return total_exposure <= self.max_portfolio_exposure
