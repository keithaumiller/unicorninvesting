#!/usr/bin/env python3
"""
Kelly Criterion Implementation
Optimal position sizing based on Kelly Criterion formula
"""

import numpy as np

class KellyCriterion:
    """Kelly Criterion position sizing"""
    
    def __init__(self):
        self.max_kelly_fraction = 0.25  # Cap at 25% of Kelly
        
    def calculate_kelly_fraction(self, win_rate: float, avg_win: float, avg_loss: float) -> float:
        """Calculate Kelly fraction: f = (bp - q) / b"""
        if avg_loss <= 0:
            return 0.0
            
        b = avg_win / abs(avg_loss)  # Win/loss ratio
        p = win_rate  # Probability of win
        q = 1 - p     # Probability of loss
        
        kelly_fraction = (b * p - q) / b
        return min(max(kelly_fraction, 0.0), self.max_kelly_fraction)
