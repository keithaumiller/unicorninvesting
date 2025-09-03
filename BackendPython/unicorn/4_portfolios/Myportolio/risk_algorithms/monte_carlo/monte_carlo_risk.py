#!/usr/bin/env python3
"""
Monte Carlo Risk Simulation
Monte Carlo methods for portfolio risk assessment
"""

import numpy as np
import pandas as pd

class MonteCarloRisk:
    """Monte Carlo risk simulation methods"""
    
    def __init__(self, num_simulations: int = 10000):
        self.num_simulations = num_simulations
        
    def simulate_portfolio_returns(self, expected_return: float, volatility: float, days: int = 252) -> np.array:
        """Simulate portfolio returns using geometric Brownian motion"""
        dt = 1 / days
        random_shocks = np.random.standard_normal((self.num_simulations, days))
        
        returns = np.zeros((self.num_simulations, days))
        for i in range(days):
            returns[:, i] = expected_return * dt + volatility * np.sqrt(dt) * random_shocks[:, i]
            
        return returns
        
    def calculate_risk_metrics(self, simulated_returns: np.array) -> dict:
        """Calculate risk metrics from simulated returns"""
        portfolio_values = np.cumprod(1 + simulated_returns, axis=1)
        final_values = portfolio_values[:, -1]
        
        return {
            'var_95': np.percentile(final_values, 5),
            'var_99': np.percentile(final_values, 1),
            'expected_shortfall': np.mean(final_values[final_values <= np.percentile(final_values, 5)]),
            'max_drawdown': np.mean([self._calculate_max_drawdown(path) for path in portfolio_values])
        }
        
    def _calculate_max_drawdown(self, values: np.array) -> float:
        """Calculate maximum drawdown for a single path"""
        peak = np.maximum.accumulate(values)
        drawdown = (values - peak) / peak
        return np.min(drawdown)
