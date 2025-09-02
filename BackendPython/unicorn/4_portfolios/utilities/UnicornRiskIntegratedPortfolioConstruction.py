#!/usr/bin/env python3
"""
🦄 Unicorn Risk-Integrated Portfolio Construction Framework
Demonstrates proper integration of risk management INTO portfolio construction
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from abc import ABC, abstractmethod
import logging
from datetime import datetime, timedelta

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class RiskBudget:
    """Risk budget allocation across different risk factors"""
    total_risk_budget: float = 0.15  # 15% max portfolio volatility
    asset_class_limit: float = 0.60   # 60% of risk to any asset class
    sector_limit: float = 0.30        # 30% of risk to any sector
    currency_limit: float = 0.20      # 20% of risk to any currency
    concentration_limit: float = 0.25  # 25% of risk to any single position
    liquidity_requirement: float = 0.90  # 90% of portfolio must be liquid

@dataclass
class RiskMetrics:
    """Real-time risk metrics for portfolio"""
    portfolio_var_95: float
    expected_shortfall: float
    maximum_drawdown: float
    correlation_risk: float
    concentration_risk: float
    liquidity_risk: float
    
    def exceeds_budget(self, budget: RiskBudget) -> bool:
        """Check if current risk exceeds allocated budget"""
        return (
            self.portfolio_var_95 > budget.total_risk_budget or
            self.concentration_risk > budget.concentration_limit or
            self.liquidity_risk < budget.liquidity_requirement
        )

@dataclass 
class PortfolioTarget:
    """Risk-adjusted portfolio target"""
    symbol: str
    target_weight: float
    risk_contribution: float
    confidence: float
    liquidity_score: float
    reason: str

class RiskAssessmentEngine:
    """
    🛡️ Risk assessment engine that calculates risk metrics
    for use IN portfolio construction (not after)
    """
    
    def __init__(self):
        self.lookback_periods = 252  # 1 year
        
    def calculate_var_95(self, returns: pd.Series) -> float:
        """Calculate 95% Value at Risk"""
        if len(returns) < 30:
            return 0.02  # Conservative default
        return np.percentile(returns, 5) * -1
    
    def calculate_expected_shortfall(self, returns: pd.Series) -> float:
        """Calculate Expected Shortfall (Conditional VaR)"""
        if len(returns) < 30:
            return 0.03  # Conservative default
        var_95 = self.calculate_var_95(returns)
        tail_losses = returns[returns <= -var_95]
        return tail_losses.mean() * -1 if len(tail_losses) > 0 else var_95
    
    def calculate_concentration_risk(self, weights: Dict[str, float]) -> float:
        """Calculate concentration risk using Herfindahl index"""
        weight_values = list(weights.values())
        return sum(w**2 for w in weight_values)
    
    def calculate_correlation_risk(self, correlation_matrix: pd.DataFrame) -> float:
        """Calculate average correlation as proxy for diversification"""
        if correlation_matrix.empty:
            return 0.5  # Moderate correlation default
        
        # Average absolute correlation excluding diagonal
        mask = ~np.eye(correlation_matrix.shape[0], dtype=bool)
        avg_correlation = np.abs(correlation_matrix.values[mask]).mean()
        return avg_correlation
    
    def assess_portfolio_risk(self, 
                            positions: Dict[str, float],
                            price_data: pd.DataFrame,
                            correlation_matrix: pd.DataFrame) -> RiskMetrics:
        """
        Comprehensive portfolio risk assessment
        """
        # Calculate portfolio returns
        portfolio_returns = self._calculate_portfolio_returns(positions, price_data)
        
        # Risk metrics
        var_95 = self.calculate_var_95(portfolio_returns)
        expected_shortfall = self.calculate_expected_shortfall(portfolio_returns)
        concentration_risk = self.calculate_concentration_risk(positions)
        correlation_risk = self.calculate_correlation_risk(correlation_matrix)
        
        # Maximum drawdown
        cumulative_returns = (1 + portfolio_returns).cumprod()
        rolling_max = cumulative_returns.expanding().max()
        drawdowns = (cumulative_returns - rolling_max) / rolling_max
        max_drawdown = drawdowns.min()
        
        # Liquidity risk (simplified - could use bid-ask spreads, volume data)
        liquidity_risk = 0.95  # Assume good liquidity for now
        
        return RiskMetrics(
            portfolio_var_95=var_95,
            expected_shortfall=expected_shortfall,
            maximum_drawdown=abs(max_drawdown),
            correlation_risk=correlation_risk,
            concentration_risk=concentration_risk,
            liquidity_risk=liquidity_risk
        )
    
    def _calculate_portfolio_returns(self, 
                                   positions: Dict[str, float], 
                                   price_data: pd.DataFrame) -> pd.Series:
        """Calculate portfolio returns from positions and price data"""
        # Simplified implementation
        returns_data = price_data.pct_change().dropna()
        
        # Weight returns by position sizes
        portfolio_returns = pd.Series(index=returns_data.index, data=0.0)
        
        for symbol, weight in positions.items():
            if symbol in returns_data.columns:
                portfolio_returns += weight * returns_data[symbol]
        
        return portfolio_returns.dropna()

class RiskBudgetingFramework:
    """
    💰 Risk budgeting framework - foundation of portfolio construction
    """
    
    def __init__(self, risk_budget: RiskBudget):
        self.risk_budget = risk_budget
        self.risk_assessor = RiskAssessmentEngine()
    
    def allocate_risk_to_assets(self, 
                               insights: Dict[str, float],
                               price_data: pd.DataFrame) -> Dict[str, float]:
        """
        Allocate risk budget to assets based on insights and risk characteristics
        
        This is the KEY method - position sizes determined by RISK ALLOCATION
        """
        logger.info("🎯 Allocating risk budget to assets...")
        
        # Step 1: Assess individual asset risks
        asset_risks = self._assess_individual_asset_risks(insights, price_data)
        
        # Step 2: Calculate risk-adjusted position sizes
        risk_adjusted_weights = self._calculate_risk_adjusted_weights(
            insights, asset_risks
        )
        
        # Step 3: Apply risk budget constraints
        final_weights = self._apply_risk_constraints(risk_adjusted_weights)
        
        logger.info(f"✅ Risk budget allocated across {len(final_weights)} assets")
        return final_weights
    
    def _assess_individual_asset_risks(self, 
                                     insights: Dict[str, float],
                                     price_data: pd.DataFrame) -> Dict[str, float]:
        """Assess risk characteristics of individual assets"""
        asset_risks = {}
        
        for symbol in insights.keys():
            if symbol in price_data.columns:
                returns = price_data[symbol].pct_change().dropna()
                asset_var = self.risk_assessor.calculate_var_95(returns)
                asset_risks[symbol] = asset_var
            else:
                asset_risks[symbol] = 0.02  # Default risk
        
        return asset_risks
    
    def _calculate_risk_adjusted_weights(self, 
                                       insights: Dict[str, float],
                                       asset_risks: Dict[str, float]) -> Dict[str, float]:
        """Calculate position weights based on risk-adjusted expected returns"""
        weights = {}
        
        for symbol in insights.keys():
            insight_strength = insights[symbol]
            asset_risk = asset_risks.get(symbol, 0.02)
            
            # Risk-adjusted weight using simplified Kelly criterion
            # weight = (expected_return - risk_free_rate) / variance
            risk_adjusted_weight = max(0, insight_strength / (asset_risk ** 2))
            weights[symbol] = risk_adjusted_weight
        
        # Normalize to sum to 1
        total_weight = sum(weights.values())
        if total_weight > 0:
            weights = {k: v/total_weight for k, v in weights.items()}
        
        return weights
    
    def _apply_risk_constraints(self, weights: Dict[str, float]) -> Dict[str, float]:
        """Apply risk budget constraints to position weights"""
        constrained_weights = weights.copy()
        
        # Apply concentration limits
        for symbol, weight in constrained_weights.items():
            if weight > self.risk_budget.concentration_limit:
                logger.warning(f"⚠️ Reducing {symbol} from {weight:.3f} to {self.risk_budget.concentration_limit:.3f}")
                constrained_weights[symbol] = self.risk_budget.concentration_limit
        
        # Re-normalize after constraints
        total_weight = sum(constrained_weights.values())
        if total_weight > 0:
            constrained_weights = {k: v/total_weight for k, v in constrained_weights.items()}
        
        return constrained_weights

class UnicornRiskIntegratedPortfolioConstruction:
    """
    🦄 Main Portfolio Construction Engine with INTEGRATED Risk Management
    
    This is the correct architecture - risk management IS portfolio construction
    """
    
    def __init__(self, risk_budget: Optional[RiskBudget] = None):
        self.risk_budget = risk_budget or RiskBudget()
        self.risk_budgeting = RiskBudgetingFramework(self.risk_budget)
        self.risk_assessor = RiskAssessmentEngine()
        
        logger.info("🦄 Risk-Integrated Portfolio Construction initialized")
        logger.info(f"📊 Risk Budget: {self.risk_budget.total_risk_budget:.1%} total volatility")
    
    def construct_portfolio(self, 
                          insights: Dict[str, float],
                          current_positions: Dict[str, float],
                          price_data: pd.DataFrame,
                          correlation_matrix: pd.DataFrame) -> List[PortfolioTarget]:
        """
        Main portfolio construction with integrated risk management
        
        Args:
            insights: Asset insights {symbol: expected_return}
            current_positions: Current portfolio {symbol: weight}
            price_data: Historical price data for risk assessment
            correlation_matrix: Asset correlation matrix
            
        Returns:
            List of risk-adjusted portfolio targets
        """
        logger.info("🎯 Starting risk-integrated portfolio construction...")
        
        # Step 1: RISK BUDGETING (Foundation)
        logger.info("1️⃣ Allocating risk budget...")
        target_weights = self.risk_budgeting.allocate_risk_to_assets(
            insights, price_data
        )
        
        # Step 2: RISK VALIDATION
        logger.info("2️⃣ Validating portfolio risk...")
        portfolio_risk = self.risk_assessor.assess_portfolio_risk(
            target_weights, price_data, correlation_matrix
        )
        
        # Step 3: RISK ADJUSTMENT (if needed)
        if portfolio_risk.exceeds_budget(self.risk_budget):
            logger.warning("⚠️ Portfolio exceeds risk budget - adjusting...")
            target_weights = self._adjust_for_risk_violations(
                target_weights, portfolio_risk
            )
        
        # Step 4: CREATE PORTFOLIO TARGETS
        logger.info("3️⃣ Creating portfolio targets...")
        targets = self._create_portfolio_targets(
            target_weights, insights, portfolio_risk
        )
        
        logger.info(f"✅ Portfolio construction complete - {len(targets)} targets created")
        self._log_risk_metrics(portfolio_risk)
        
        return targets
    
    def _adjust_for_risk_violations(self, 
                                  weights: Dict[str, float],
                                  risk_metrics: RiskMetrics) -> Dict[str, float]:
        """Adjust portfolio weights when risk budget is exceeded"""
        adjusted_weights = weights.copy()
        
        if risk_metrics.portfolio_var_95 > self.risk_budget.total_risk_budget:
            # Scale down all positions proportionally
            scale_factor = self.risk_budget.total_risk_budget / risk_metrics.portfolio_var_95
            adjusted_weights = {k: v * scale_factor for k, v in adjusted_weights.items()}
            logger.info(f"📉 Scaled portfolio by {scale_factor:.3f} to meet risk budget")
        
        if risk_metrics.concentration_risk > self.risk_budget.concentration_limit:
            # Cap largest positions
            max_weight = self.risk_budget.concentration_limit * 0.8  # 80% of limit
            for symbol, weight in adjusted_weights.items():
                if weight > max_weight:
                    adjusted_weights[symbol] = max_weight
            
            # Re-normalize
            total = sum(adjusted_weights.values())
            if total > 0:
                adjusted_weights = {k: v/total for k, v in adjusted_weights.items()}
        
        return adjusted_weights
    
    def _create_portfolio_targets(self, 
                                weights: Dict[str, float],
                                insights: Dict[str, float],
                                risk_metrics: RiskMetrics) -> List[PortfolioTarget]:
        """Create portfolio targets with risk attribution"""
        targets = []
        
        for symbol, weight in weights.items():
            if weight > 0.001:  # Minimum position size threshold
                # Calculate individual risk contribution
                risk_contribution = weight * risk_metrics.portfolio_var_95
                
                target = PortfolioTarget(
                    symbol=symbol,
                    target_weight=weight,
                    risk_contribution=risk_contribution,
                    confidence=insights.get(symbol, 0.0),
                    liquidity_score=risk_metrics.liquidity_risk,
                    reason=f"Risk-adjusted allocation based on {insights.get(symbol, 0.0):.1%} expected return"
                )
                targets.append(target)
        
        return sorted(targets, key=lambda x: x.target_weight, reverse=True)
    
    def _log_risk_metrics(self, risk_metrics: RiskMetrics):
        """Log comprehensive risk metrics"""
        logger.info("📊 PORTFOLIO RISK METRICS:")
        logger.info(f"   VaR 95%: {risk_metrics.portfolio_var_95:.2%}")
        logger.info(f"   Expected Shortfall: {risk_metrics.expected_shortfall:.2%}")
        logger.info(f"   Max Drawdown: {risk_metrics.maximum_drawdown:.2%}")
        logger.info(f"   Concentration Risk: {risk_metrics.concentration_risk:.3f}")
        logger.info(f"   Correlation Risk: {risk_metrics.correlation_risk:.3f}")
        logger.info(f"   Liquidity Score: {risk_metrics.liquidity_risk:.2%}")
    
    def monitor_risk_budget_utilization(self, 
                                      current_positions: Dict[str, float],
                                      price_data: pd.DataFrame,
                                      correlation_matrix: pd.DataFrame) -> str:
        """
        Monitor current risk budget utilization for dynamic adjustment
        
        Returns:
            Action recommendation: 'INCREASE', 'DECREASE', 'MAINTAIN'
        """
        current_risk = self.risk_assessor.assess_portfolio_risk(
            current_positions, price_data, correlation_matrix
        )
        
        risk_utilization = current_risk.portfolio_var_95 / self.risk_budget.total_risk_budget
        
        logger.info(f"📊 Risk Budget Utilization: {risk_utilization:.1%}")
        
        if risk_utilization < 0.70:
            return "INCREASE"  # Under-utilizing risk budget
        elif risk_utilization > 1.0:
            return "DECREASE"  # Exceeding risk budget
        else:
            return "MAINTAIN"  # Optimal utilization

# Example Usage and Testing
def demo_risk_integrated_portfolio():
    """Demonstrate the risk-integrated portfolio construction"""
    
    print("🦄 UNICORN RISK-INTEGRATED PORTFOLIO CONSTRUCTION DEMO")
    print("=" * 60)
    
    # Mock data
    insights = {
        'BTC': 0.15,   # 15% expected return
        'ETH': 0.12,   # 12% expected return  
        'SPY': 0.08,   # 8% expected return
        'GLD': 0.05    # 5% expected return
    }
    
    # Create mock price data
    dates = pd.date_range('2023-01-01', '2024-01-01', freq='D')
    np.random.seed(42)
    
    price_data = pd.DataFrame({
        'BTC': np.cumsum(np.random.normal(0.001, 0.04, len(dates))) + 100,
        'ETH': np.cumsum(np.random.normal(0.0008, 0.035, len(dates))) + 50,
        'SPY': np.cumsum(np.random.normal(0.0003, 0.015, len(dates))) + 400,
        'GLD': np.cumsum(np.random.normal(0.0001, 0.01, len(dates))) + 150
    }, index=dates)
    
    # Mock correlation matrix
    correlation_matrix = pd.DataFrame([
        [1.0, 0.7, 0.3, -0.1],
        [0.7, 1.0, 0.4, 0.0],
        [0.3, 0.4, 1.0, 0.2],
        [-0.1, 0.0, 0.2, 1.0]
    ], index=['BTC', 'ETH', 'SPY', 'GLD'], columns=['BTC', 'ETH', 'SPY', 'GLD'])
    
    # Current positions
    current_positions = {'BTC': 0.3, 'ETH': 0.2, 'SPY': 0.4, 'GLD': 0.1}
    
    # Initialize portfolio construction
    risk_budget = RiskBudget(
        total_risk_budget=0.12,  # 12% max volatility
        concentration_limit=0.30  # 30% max position
    )
    
    portfolio_engine = UnicornRiskIntegratedPortfolioConstruction(risk_budget)
    
    # Construct portfolio
    targets = portfolio_engine.construct_portfolio(
        insights=insights,
        current_positions=current_positions,
        price_data=price_data,
        correlation_matrix=correlation_matrix
    )
    
    # Display results
    print("\n🎯 PORTFOLIO TARGETS:")
    print("-" * 40)
    for target in targets:
        print(f"{target.symbol:4s}: {target.target_weight:6.1%} "
              f"(Risk: {target.risk_contribution:5.2%}, "
              f"Confidence: {target.confidence:5.1%})")
    
    # Monitor risk budget
    recommendation = portfolio_engine.monitor_risk_budget_utilization(
        current_positions, price_data, correlation_matrix
    )
    print(f"\n💡 Risk Budget Recommendation: {recommendation}")

if __name__ == "__main__":
    demo_risk_integrated_portfolio()
