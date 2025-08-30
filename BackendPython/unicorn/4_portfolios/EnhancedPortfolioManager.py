#!/usr/bin/env python3
"""
🦄 Enhanced Portfolio Manager
Integrates portfolio configuration management with risk-integrated portfolio construction
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple, Any
import logging
from datetime import datetime, timedelta
from pathlib import Path
import sys

# Add the current directory to the path for imports
sys.path.append(str(Path(__file__).parent))

from PortfolioConfigManager import PortfolioConfigManager, PortfolioConfig, RiskParameters, ExecutionSettings
from UnicornRiskIntegratedPortfolioConstruction import (
    UnicornRiskIntegratedPortfolioConstruction, 
    RiskBudget, 
    PortfolioTarget,
    RiskMetrics
)

logger = logging.getLogger(__name__)

class EnhancedPortfolioManager:
    """
    🦄 Enhanced Portfolio Manager
    
    Combines portfolio configuration management with risk-integrated construction
    for complete portfolio lifecycle management
    """
    
    def __init__(self, portfolio_name: str, base_path: str = None):
        """
        Initialize enhanced portfolio manager
        
        Args:
            portfolio_name: Name of the portfolio to manage
            base_path: Base path to portfolios directory
        """
        self.portfolio_name = portfolio_name
        self.config_manager = PortfolioConfigManager(base_path)
        
        # Load complete portfolio configuration
        try:
            self.portfolio_data = self.config_manager.load_complete_portfolio(portfolio_name)
            self.config = self.portfolio_data['config']
            self.risk_params = self.portfolio_data['risk_parameters']
            self.execution_settings = self.portfolio_data['execution_settings']
            
            # Create risk budget from configuration
            self.risk_budget = self._create_risk_budget_from_config()
            
            # Initialize portfolio construction engine
            self.portfolio_engine = UnicornRiskIntegratedPortfolioConstruction(self.risk_budget)
            
            logger.info(f"🦄 Enhanced Portfolio Manager initialized: {portfolio_name}")
            
        except Exception as e:
            logger.error(f"❌ Failed to initialize portfolio manager: {e}")
            raise
    
    def _create_risk_budget_from_config(self) -> RiskBudget:
        """Create RiskBudget object from portfolio configuration"""
        return RiskBudget(
            total_risk_budget=self.risk_params.max_portfolio_volatility,
            asset_class_limit=self.risk_params.max_single_asset_weight,
            concentration_limit=self.risk_params.max_single_asset_weight,
            liquidity_requirement=self.risk_params.position_limits.get('cash_reserve', 0.05)
        )
    
    def get_target_allocations(self) -> Dict[str, float]:
        """Get target allocations from portfolio configuration"""
        allocations = {}
        for symbol, asset_config in self.config.assets.items():
            allocations[symbol] = asset_config.allocation_percent / 100.0
        
        return allocations
    
    def construct_portfolio_from_insights(self, 
                                        insights: Dict[str, float],
                                        current_positions: Optional[Dict[str, float]] = None,
                                        market_data: Optional[pd.DataFrame] = None) -> List[PortfolioTarget]:
        """
        Construct portfolio using insights and configuration
        
        Args:
            insights: Asset insights {symbol: expected_return}
            current_positions: Current portfolio positions
            market_data: Historical market data for risk assessment
            
        Returns:
            List of portfolio targets
        """
        logger.info(f"🎯 Constructing portfolio for {self.portfolio_name}...")
        
        # Use target allocations if no current positions provided
        if current_positions is None:
            current_positions = self.get_target_allocations()
        
        # Create mock market data if not provided (for demonstration)
        if market_data is None:
            market_data = self._create_mock_market_data()
        
        # Create correlation matrix
        correlation_matrix = self._create_correlation_matrix(market_data)
        
        # Filter insights to only include assets in portfolio
        portfolio_assets = set(self.config.assets.keys())
        filtered_insights = {k: v for k, v in insights.items() if k in portfolio_assets}
        
        # Construct portfolio using risk-integrated framework
        targets = self.portfolio_engine.construct_portfolio(
            insights=filtered_insights,
            current_positions=current_positions,
            price_data=market_data,
            correlation_matrix=correlation_matrix
        )
        
        return targets
    
    def rebalance_portfolio(self, 
                          current_positions: Dict[str, float],
                          market_data: pd.DataFrame,
                          alpha_insights: Dict[str, float]) -> Dict[str, Any]:
        """
        Execute portfolio rebalancing based on current market conditions
        
        Returns:
            Rebalancing recommendations and metrics
        """
        logger.info(f"🔄 Rebalancing portfolio {self.portfolio_name}...")
        
        # Get new portfolio targets
        targets = self.construct_portfolio_from_insights(
            insights=alpha_insights,
            current_positions=current_positions,
            market_data=market_data
        )
        
        # Calculate rebalancing trades
        trades = self._calculate_rebalancing_trades(current_positions, targets)
        
        # Assess risk impact
        correlation_matrix = self._create_correlation_matrix(market_data)
        new_positions = {target.symbol: target.target_weight for target in targets}
        
        risk_metrics = self.portfolio_engine.risk_assessor.assess_portfolio_risk(
            new_positions, market_data, correlation_matrix
        )
        
        # Check if rebalancing is needed based on thresholds
        rebalancing_needed = self._should_rebalance(current_positions, targets)
        
        return {
            'rebalancing_needed': rebalancing_needed,
            'targets': targets,
            'trades': trades,
            'risk_metrics': risk_metrics,
            'execution_settings': self.execution_settings,
            'rebalancing_threshold': self.risk_params.position_limits.get('minimum_rebalancing_threshold', 0.05)
        }
    
    def _calculate_rebalancing_trades(self, 
                                    current_positions: Dict[str, float],
                                    targets: List[PortfolioTarget]) -> List[Dict[str, Any]]:
        """Calculate required trades for rebalancing"""
        trades = []
        target_positions = {target.symbol: target.target_weight for target in targets}
        
        # Include all assets (current and target)
        all_assets = set(current_positions.keys()) | set(target_positions.keys())
        
        for asset in all_assets:
            current_weight = current_positions.get(asset, 0.0)
            target_weight = target_positions.get(asset, 0.0)
            
            weight_change = target_weight - current_weight
            
            if abs(weight_change) > 0.001:  # Minimum trade threshold
                trade = {
                    'symbol': asset,
                    'current_weight': current_weight,
                    'target_weight': target_weight,
                    'weight_change': weight_change,
                    'action': 'BUY' if weight_change > 0 else 'SELL',
                    'urgency': 'HIGH' if abs(weight_change) > 0.10 else 'NORMAL'
                }
                trades.append(trade)
        
        return sorted(trades, key=lambda x: abs(x['weight_change']), reverse=True)
    
    def _should_rebalance(self, 
                         current_positions: Dict[str, float],
                         targets: List[PortfolioTarget]) -> bool:
        """Determine if rebalancing is needed based on drift thresholds"""
        target_positions = {target.symbol: target.target_weight for target in targets}
        
        # Calculate maximum drift from target
        max_drift = 0.0
        for asset in set(current_positions.keys()) | set(target_positions.keys()):
            current = current_positions.get(asset, 0.0)
            target = target_positions.get(asset, 0.0)
            drift = abs(current - target)
            max_drift = max(max_drift, drift)
        
        # Get rebalancing threshold from risk parameters
        threshold = 0.05  # Default 5% drift threshold
        if hasattr(self.risk_params, 'rebalancing_triggers'):
            threshold = self.risk_params.rebalancing_triggers.get('allocation_drift_threshold', 0.05)
        
        return max_drift > threshold
    
    def get_portfolio_status(self) -> Dict[str, Any]:
        """Get comprehensive portfolio status"""
        validation_results = self.config_manager.validate_portfolio_config(self.portfolio_name)
        
        return {
            'portfolio_name': self.portfolio_name,
            'status': self.config.status,
            'description': self.config.description,
            'strategy_type': self.config.strategy_type,
            'assets': list(self.config.assets.keys()),
            'target_volatility': self.config.target_volatility,
            'rebalancing_frequency': self.config.rebalancing_frequency,
            'risk_profile': self.risk_params.risk_profile,
            'max_drawdown_limit': self.risk_params.max_drawdown,
            'validation_passed': all(validation_results.values()),
            'validation_details': validation_results,
            'last_updated': self.config.last_updated
        }
    
    def _create_mock_market_data(self) -> pd.DataFrame:
        """Create mock market data for demonstration purposes"""
        # This would be replaced with real market data in production
        assets = list(self.config.assets.keys())
        dates = pd.date_range(end=datetime.now(), periods=252, freq='D')
        
        np.random.seed(42)  # For reproducible results
        
        data = {}
        for asset in assets:
            # Create realistic price movements based on asset type
            if 'BTC' in asset or 'ETH' in asset:
                volatility = 0.04  # Higher volatility for crypto
                drift = 0.001
            else:
                volatility = 0.015  # Lower volatility for traditional assets
                drift = 0.0003
                
            returns = np.random.normal(drift, volatility, len(dates))
            prices = 100 * np.exp(np.cumsum(returns))
            data[asset] = prices
        
        return pd.DataFrame(data, index=dates)
    
    def _create_correlation_matrix(self, market_data: pd.DataFrame) -> pd.DataFrame:
        """Create correlation matrix from market data"""
        returns = market_data.pct_change().dropna()
        correlation_matrix = returns.corr()
        
        # Fill any NaN values with neutral correlation
        correlation_matrix = correlation_matrix.fillna(0.0)
        
        return correlation_matrix
    
    def generate_performance_report(self, 
                                  current_positions: Dict[str, float],
                                  market_data: pd.DataFrame,
                                  benchmark_returns: Optional[pd.Series] = None) -> Dict[str, Any]:
        """Generate comprehensive performance report"""
        
        # Calculate portfolio returns
        returns = market_data.pct_change().dropna()
        portfolio_returns = pd.Series(index=returns.index, data=0.0)
        
        for asset, weight in current_positions.items():
            if asset in returns.columns:
                portfolio_returns += weight * returns[asset]
        
        # Performance metrics
        total_return = (1 + portfolio_returns).prod() - 1
        annualized_return = (1 + portfolio_returns.mean()) ** 252 - 1
        volatility = portfolio_returns.std() * np.sqrt(252)
        sharpe_ratio = annualized_return / volatility if volatility > 0 else 0
        
        # Maximum drawdown
        cumulative_returns = (1 + portfolio_returns).cumprod()
        rolling_max = cumulative_returns.expanding().max()
        drawdowns = (cumulative_returns - rolling_max) / rolling_max
        max_drawdown = drawdowns.min()
        
        # Risk metrics
        correlation_matrix = self._create_correlation_matrix(market_data)
        risk_metrics = self.portfolio_engine.risk_assessor.assess_portfolio_risk(
            current_positions, market_data, correlation_matrix
        )
        
        report = {
            'performance_metrics': {
                'total_return': total_return,
                'annualized_return': annualized_return,
                'volatility': volatility,
                'sharpe_ratio': sharpe_ratio,
                'max_drawdown': abs(max_drawdown)
            },
            'risk_metrics': {
                'var_95': risk_metrics.portfolio_var_95,
                'expected_shortfall': risk_metrics.expected_shortfall,
                'concentration_risk': risk_metrics.concentration_risk,
                'correlation_risk': risk_metrics.correlation_risk
            },
            'portfolio_composition': current_positions,
            'risk_budget_utilization': risk_metrics.portfolio_var_95 / self.risk_budget.total_risk_budget,
            'meets_risk_targets': not risk_metrics.exceeds_budget(self.risk_budget),
            'report_date': datetime.now().isoformat()
        }
        
        return report

# Example usage and testing
def demo_enhanced_portfolio_manager():
    """Demonstrate enhanced portfolio management"""
    print("🦄 ENHANCED PORTFOLIO MANAGER DEMO")
    print("=" * 50)
    
    try:
        # Initialize manager for ETH_Only portfolio
        manager = EnhancedPortfolioManager("ETH_Only")
        
        # Get portfolio status
        status = manager.get_portfolio_status()
        print(f"\n📊 Portfolio Status: {status['portfolio_name']}")
        print(f"   Description: {status['description']}")
        print(f"   Assets: {status['assets']}")
        print(f"   Target Volatility: {status['target_volatility']:.1%}")
        print(f"   Validation Passed: {'✅' if status['validation_passed'] else '❌'}")
        
        # Mock alpha insights
        insights = {'ETH': 0.15}  # 15% expected return for ETH
        
        # Construct portfolio
        targets = manager.construct_portfolio_from_insights(insights)
        print(f"\n🎯 Portfolio Targets:")
        for target in targets:
            print(f"   {target.symbol}: {target.target_weight:.1%} "
                  f"(Risk: {target.risk_contribution:.2%})")
        
        # Simulate current positions for rebalancing demo
        current_positions = {'ETH': 0.95, 'CASH': 0.05}
        
        # Create mock market data
        market_data = manager._create_mock_market_data()
        
        # Generate performance report
        report = manager.generate_performance_report(current_positions, market_data)
        print(f"\n📈 Performance Report:")
        print(f"   Annualized Return: {report['performance_metrics']['annualized_return']:.1%}")
        print(f"   Volatility: {report['performance_metrics']['volatility']:.1%}")
        print(f"   Sharpe Ratio: {report['performance_metrics']['sharpe_ratio']:.2f}")
        print(f"   Max Drawdown: {report['performance_metrics']['max_drawdown']:.1%}")
        print(f"   Risk Budget Utilization: {report['risk_budget_utilization']:.1%}")
        
    except Exception as e:
        print(f"❌ Demo failed: {e}")
        print("💡 Make sure the ETH_Only portfolio configuration exists")

if __name__ == "__main__":
    demo_enhanced_portfolio_manager()
