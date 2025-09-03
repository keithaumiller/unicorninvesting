#!/usr/bin/env python3
"""
Value at Risk (VaR) Models
Calculate portfolio Value at Risk using different methodologies
"""

import numpy as np
import pandas as pd

class VaRCalculator:
    """Value at Risk calculation methods"""
    
    def __init__(self):
        self.confidence_levels = [0.95, 0.99]
        
    def historical_var(self, returns: np.array, confidence: float = 0.95) -> float:
        """Calculate Historical VaR"""
        if len(returns) == 0:
            return 0.0
        return np.percentile(returns, (1 - confidence) * 100)
        
    def parametric_var(self, returns: np.array, confidence: float = 0.95) -> float:
        """Calculate Parametric VaR assuming normal distribution"""
        if len(returns) == 0:
            return 0.0
        mean = np.mean(returns)
        std = np.std(returns)
        z_score = -1.645 if confidence == 0.95 else -2.33  # 95% or 99%
        return mean + z_score * std
