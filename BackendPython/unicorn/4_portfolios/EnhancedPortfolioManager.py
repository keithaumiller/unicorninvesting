#!/usr/bin/env python3
"""
🦄 Enhanced Portfolio Manager
Core portfolio management capabilities with integrated risk management and execution coordination.
Foundation component for the Enhanced Portfolio Orchestration system.
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from pathlib import Path
import logging
import json
import sys

# Add utilities path for imports
utilities_path = Path(__file__).parent / "utilities"
sys.path.insert(0, str(utilities_path))

try:
    from PortfolioConfigManager import PortfolioConfig, AssetConfig, RiskParameters, ExecutionSettings
    from UnicornRiskIntegratedPortfolioConstruction import (
        RiskBudget, RiskMetrics, PortfolioTarget, 
        UnicornRiskIntegratedPortfolioConstructor
    )
    DEPENDENCIES_AVAILABLE = True
except ImportError as e:
    logging.warning(f"Import warning in EnhancedPortfolioManager: {e}")
    DEPENDENCIES_AVAILABLE = False
    
    # Create placeholder classes for development
    class PortfolioConfig:
        def __init__(self, **kwargs):
            self.__dict__.update(kwargs)
        
        @classmethod
        def from_dict(cls, data):
            return cls(**data)
    
    class RiskBudget:
        def __init__(self, **kwargs):
            self.__dict__.update(kwargs)
    
    class RiskMetrics:
        def __init__(self, **kwargs):
            self.__dict__.update(kwargs)
    
    class PortfolioTarget:
        def __init__(self, symbol: str, target_weight: float, confidence_score: float = 1.0, risk_contribution: float = 0.0, **kwargs):
            self.symbol = symbol
            self.target_weight = target_weight
            self.confidence_score = confidence_score
            self.risk_contribution = risk_contribution
            self.__dict__.update(kwargs)

logger = logging.getLogger(__name__)

@dataclass
class PortfolioState:
    """Current state of the portfolio"""
    timestamp: datetime
    total_value: float
    positions: Dict[str, float]  # symbol -> quantity
    allocations: Dict[str, float]  # symbol -> percentage
    cash: float
    unrealized_pnl: float
    realized_pnl: float
    risk_metrics: Optional['RiskMetrics'] = None
    performance_metrics: Dict[str, float] = field(default_factory=dict)

@dataclass
class ExecutionResult:
    """Result of a portfolio execution operation"""
    success: bool
    timestamp: datetime
    operation: str
    details: Dict[str, Any]
    executed_positions: Dict[str, float] = field(default_factory=dict)
    execution_cost: float = 0.0
    execution_time_ms: float = 0.0
    error_message: Optional[str] = None

@dataclass
class PortfolioSignal:
    """Trading signal with portfolio context"""
    symbol: str
    signal_type: str  # 'BUY', 'SELL', 'HOLD'
    confidence: float  # 0.0 to 1.0
    target_allocation: float  # desired portfolio percentage
    risk_adjusted_size: float  # risk-adjusted position size
    alpha_score: float  # alpha model score
    risk_score: float  # risk assessment score
    timestamp: datetime
    source: str  # which alpha model generated this

class EnhancedPortfolioManager:
    """
    Enhanced Portfolio Manager with integrated risk management and execution coordination
    
    Core capabilities:
    - Portfolio state management and tracking
    - Risk-integrated position sizing
    - Multi-asset allocation optimization
    - Execution planning and coordination
    - Performance monitoring and analytics
    """
    
    def __init__(self, portfolio_name: str, config_path: Optional[str] = None):
        """
        Initialize Enhanced Portfolio Manager
        
        Args:
            portfolio_name: Name of the portfolio to manage
            config_path: Optional path to portfolio configuration
        """
        self.portfolio_name = portfolio_name
        self.config_path = config_path or f"config/{portfolio_name}_config.json"
        
        # Load configuration
        self.config = self._load_configuration()
        
        # Initialize components
        if DEPENDENCIES_AVAILABLE:
            try:
                self.risk_constructor = UnicornRiskIntegratedPortfolioConstructor()
            except Exception as e:
                logger.warning(f"Could not initialize risk constructor: {e}")
                self.risk_constructor = None
        else:
            self.risk_constructor = None
        
        # Portfolio state
        self.current_state = PortfolioState(
            timestamp=datetime.now(),
            total_value=self.config.initial_capital,
            positions={},
            allocations={},
            cash=self.config.initial_capital,
            unrealized_pnl=0.0,
            realized_pnl=0.0
        )
        
        # Execution tracking
        self.execution_history: List[ExecutionResult] = []
        self.pending_signals: List[PortfolioSignal] = []
        
        # Performance tracking
        self.performance_history: List[Dict] = []
        
        logger.info(f"✅ Enhanced Portfolio Manager initialized for {portfolio_name}")
        logger.info(f"   Initial Capital: ${self.config.initial_capital:,.2f}")
        logger.info(f"   Assets: {len(self.config.assets)} configured")
    
    def _load_configuration(self) -> PortfolioConfig:
        """Load portfolio configuration from file or create default"""
        try:
            if Path(self.config_path).exists():
                with open(self.config_path, 'r') as f:
                    config_data = json.load(f)
                return PortfolioConfig.from_dict(config_data)
            else:
                logger.info(f"Configuration file not found, creating default for {self.portfolio_name}")
                return self._create_default_config()
        except Exception as e:
            logger.warning(f"Error loading configuration: {e}, using default")
            return self._create_default_config()
    
    def _create_default_config(self) -> PortfolioConfig:
        """Create default portfolio configuration"""
        if DEPENDENCIES_AVAILABLE:
            from PortfolioConfigManager import AssetConfig, RiskParameters, ExecutionSettings
            
            # Default ETH-focused configuration
            assets = [
                AssetConfig(
                    symbol="ETHUSD",
                    allocation_percent=80.0,
                    asset_type="CRYPTO",
                    data_source="polygon",
                    model_type="eth_momentum"
                ),
                AssetConfig(
                    symbol="BTCUSD", 
                    allocation_percent=20.0,
                    asset_type="CRYPTO",
                    data_source="polygon",
                    model_type="btc_momentum"
                )
            ]
            
            risk_params = RiskParameters(
                max_daily_var=0.05,
                max_drawdown=0.15,
                position_size_limit=0.50,
                correlation_limit=0.75
            )
            
            execution_settings = ExecutionSettings(
                execution_frequency=300,  # 5 minutes
                max_execution_time=120,   # 2 minutes
                slippage_tolerance=0.001,
                min_trade_size=100.0
            )
            
            return PortfolioConfig(
                portfolio_name=self.portfolio_name,
                initial_capital=100000.0,
                assets=assets,
                risk_parameters=risk_params,
                execution_settings=execution_settings
            )
        else:
            # Simplified config when dependencies not available
            config = PortfolioConfig(
                portfolio_name=self.portfolio_name,
                initial_capital=100000.0,
                assets=[],
                risk_parameters=type('RiskParams', (), {
                    'max_daily_var': 0.05,
                    'position_size_limit': 0.50
                })(),
                execution_settings=type('ExecSettings', (), {
                    'execution_frequency': 300
                })()
            )
            return config
    
    def get_current_state(self) -> PortfolioState:
        """Get current portfolio state"""
        # Update state with latest data
        self._update_portfolio_state()
        return self.current_state
    
    def _update_portfolio_state(self):
        """Update portfolio state with latest market data and positions"""
        try:
            # Update timestamp
            self.current_state.timestamp = datetime.now()
            
            # Calculate current allocations
            if self.current_state.total_value > 0:
                for symbol, quantity in self.current_state.positions.items():
                    # This would connect to real market data
                    # For now, using placeholder logic
                    market_value = quantity * self._get_current_price(symbol)
                    self.current_state.allocations[symbol] = market_value / self.current_state.total_value
            
            # Update risk metrics
            self.current_state.risk_metrics = self._calculate_risk_metrics()
            
            # Update performance metrics
            self._update_performance_metrics()
            
        except Exception as e:
            logger.error(f"Error updating portfolio state: {e}")
    
    def _get_current_price(self, symbol: str) -> float:
        """Get current market price for symbol (placeholder implementation)"""
        # This would connect to real market data feed
        # For development purposes, return placeholder price
        price_map = {
            "ETHUSD": 2500.0,
            "BTCUSD": 45000.0
        }
        return price_map.get(symbol, 100.0)
    
    def _calculate_risk_metrics(self) -> RiskMetrics:
        """Calculate current portfolio risk metrics"""
        try:
            # Use risk constructor for integrated risk calculation if available
            if self.risk_constructor:
                risk_metrics = self.risk_constructor.calculate_portfolio_risk(
                    positions=self.current_state.positions,
                    allocations=self.current_state.allocations
                )
                return risk_metrics
            else:
                # Fallback to simplified risk calculation
                return self._calculate_simple_risk_metrics()
        except Exception as e:
            logger.warning(f"Error calculating risk metrics: {e}")
            # Return placeholder risk metrics
            return RiskMetrics(
                portfolio_var_95=0.02,
                expected_shortfall=0.03,
                maximum_drawdown=0.05,
                correlation_risk=0.40,
                concentration_risk=0.30,
                liquidity_risk=0.95
            )
    
    def _calculate_simple_risk_metrics(self) -> RiskMetrics:
        """Simple risk metrics calculation for fallback"""
        # Calculate basic concentration risk
        max_allocation = max(self.current_state.allocations.values()) if self.current_state.allocations else 0.0
        concentration_risk = max_allocation
        
        # Simple portfolio VaR estimation
        portfolio_var = len(self.current_state.positions) * 0.02 if self.current_state.positions else 0.01
        
        return RiskMetrics(
            portfolio_var_95=portfolio_var,
            expected_shortfall=portfolio_var * 1.5,
            maximum_drawdown=0.05,
            correlation_risk=0.40,
            concentration_risk=concentration_risk,
            liquidity_risk=0.95
        )
    
    def _update_performance_metrics(self):
        """Update portfolio performance metrics"""
        try:
            # Calculate basic performance metrics
            initial_value = self.config.initial_capital
            current_value = self.current_state.total_value
            total_return = (current_value - initial_value) / initial_value
            
            # Calculate time-based returns
            performance = {
                'total_return': total_return,
                'total_value': current_value,
                'unrealized_pnl': self.current_state.unrealized_pnl,
                'realized_pnl': self.current_state.realized_pnl,
                'timestamp': self.current_state.timestamp.isoformat()
            }
            
            self.current_state.performance_metrics = performance
            
            # Add to history
            self.performance_history.append(performance.copy())
            
        except Exception as e:
            logger.error(f"Error updating performance metrics: {e}")
    
    def process_alpha_signals(self, signals: List[PortfolioSignal]) -> List[PortfolioTarget]:
        """
        Process alpha signals into risk-adjusted portfolio targets
        
        Args:
            signals: List of trading signals from alpha models
            
        Returns:
            List of risk-adjusted portfolio targets
        """
        try:
            logger.info(f"Processing {len(signals)} alpha signals")
            
            # Add signals to pending queue
            self.pending_signals.extend(signals)
            
            # Use risk constructor for portfolio optimization if available
            if self.risk_constructor:
                targets = self.risk_constructor.optimize_portfolio(
                    alpha_signals=signals,
                    current_positions=self.current_state.positions,
                    risk_budget=self._get_risk_budget()
                )
            else:
                # Fallback to simple target generation
                targets = self._generate_simple_targets(signals)
            
            logger.info(f"Generated {len(targets)} portfolio targets")
            return targets
            
        except Exception as e:
            logger.error(f"Error processing alpha signals: {e}")
            return []
    
    def _generate_simple_targets(self, signals: List[PortfolioSignal]) -> List[PortfolioTarget]:
        """Simple target generation for fallback when risk constructor unavailable"""
        targets = []
        
        for signal in signals:
            # Simple allocation based on signal confidence and current allocation limits
            base_allocation = signal.target_allocation
            confidence_adjustment = signal.confidence
            
            # Apply position size limits
            max_position = self.config.risk_parameters.position_size_limit if hasattr(self.config, 'risk_parameters') else 0.5
            final_allocation = min(base_allocation * confidence_adjustment, max_position)
            
            target = PortfolioTarget(
                symbol=signal.symbol,
                target_weight=final_allocation,
                confidence_score=signal.confidence,
                risk_contribution=final_allocation * 0.02  # Simple risk estimate
            )
            targets.append(target)
        
        return targets
    
    def _get_risk_budget(self) -> RiskBudget:
        """Get current risk budget from configuration"""
        return RiskBudget(
            total_risk_budget=self.config.risk_parameters.max_daily_var,
            asset_class_limit=0.60,
            sector_limit=0.30,
            currency_limit=0.20,
            concentration_limit=self.config.risk_parameters.position_size_limit,
            liquidity_requirement=0.90
        )
    
    def execute_portfolio_targets(self, targets: List[PortfolioTarget]) -> ExecutionResult:
        """
        Execute portfolio targets with risk management
        
        Args:
            targets: List of portfolio targets to execute
            
        Returns:
            ExecutionResult with execution details
        """
        start_time = datetime.now()
        
        try:
            logger.info(f"Executing {len(targets)} portfolio targets")
            
            # Validate targets against risk limits
            if not self._validate_targets(targets):
                return ExecutionResult(
                    success=False,
                    timestamp=start_time,
                    operation="execute_targets",
                    details={"error": "Risk validation failed"},
                    error_message="Portfolio targets exceed risk limits"
                )
            
            # Calculate execution plan
            execution_plan = self._create_execution_plan(targets)
            
            # Execute trades (placeholder implementation)
            executed_positions = self._execute_trades(execution_plan)
            
            # Update portfolio state
            self._update_positions(executed_positions)
            
            execution_time = (datetime.now() - start_time).total_seconds() * 1000
            
            result = ExecutionResult(
                success=True,
                timestamp=start_time,
                operation="execute_targets",
                details={
                    "targets_count": len(targets),
                    "executed_count": len(executed_positions),
                    "execution_plan": execution_plan
                },
                executed_positions=executed_positions,
                execution_cost=self._calculate_execution_cost(executed_positions),
                execution_time_ms=execution_time
            )
            
            # Add to execution history
            self.execution_history.append(result)
            
            logger.info(f"✅ Portfolio execution completed in {execution_time:.1f}ms")
            return result
            
        except Exception as e:
            logger.error(f"Portfolio execution failed: {e}")
            return ExecutionResult(
                success=False,
                timestamp=start_time,
                operation="execute_targets",
                details={"error": str(e)},
                error_message=f"Execution error: {e}"
            )
    
    def _validate_targets(self, targets: List[PortfolioTarget]) -> bool:
        """Validate portfolio targets against risk limits"""
        try:
            # Check total allocation
            total_allocation = sum(target.target_weight for target in targets)
            if total_allocation > 1.05:  # Allow 5% tolerance
                logger.warning(f"Total allocation exceeds 100%: {total_allocation:.2%}")
                return False
            
            # Check individual position limits
            for target in targets:
                if target.target_weight > self.config.risk_parameters.position_size_limit:
                    logger.warning(f"Position {target.symbol} exceeds size limit: {target.target_weight:.2%}")
                    return False
            
            return True
            
        except Exception as e:
            logger.error(f"Error validating targets: {e}")
            return False
    
    def _create_execution_plan(self, targets: List[PortfolioTarget]) -> Dict[str, Any]:
        """Create detailed execution plan for portfolio targets"""
        plan = {
            'timestamp': datetime.now().isoformat(),
            'trades': [],
            'total_value': self.current_state.total_value
        }
        
        for target in targets:
            current_allocation = self.current_state.allocations.get(target.symbol, 0.0)
            allocation_change = target.target_weight - current_allocation
            
            if abs(allocation_change) > 0.01:  # 1% threshold for trading
                trade = {
                    'symbol': target.symbol,
                    'action': 'BUY' if allocation_change > 0 else 'SELL',
                    'current_allocation': current_allocation,
                    'target_allocation': target.target_weight,
                    'allocation_change': allocation_change,
                    'dollar_amount': allocation_change * self.current_state.total_value
                }
                plan['trades'].append(trade)
        
        return plan
    
    def _execute_trades(self, execution_plan: Dict[str, Any]) -> Dict[str, float]:
        """Execute trades according to execution plan (placeholder implementation)"""
        executed_positions = {}
        
        for trade in execution_plan.get('trades', []):
            symbol = trade['symbol']
            dollar_amount = trade['dollar_amount']
            
            # Convert dollar amount to quantity (simplified)
            price = self._get_current_price(symbol)
            quantity_change = dollar_amount / price
            
            current_quantity = self.current_state.positions.get(symbol, 0.0)
            new_quantity = current_quantity + quantity_change
            
            executed_positions[symbol] = new_quantity
            
            logger.info(f"Executed: {symbol} {trade['action']} ${dollar_amount:.2f} "
                       f"({quantity_change:.4f} shares)")
        
        return executed_positions
    
    def _update_positions(self, executed_positions: Dict[str, float]):
        """Update portfolio positions after execution"""
        for symbol, quantity in executed_positions.items():
            self.current_state.positions[symbol] = quantity
        
        # Update cash position
        self._recalculate_cash_position()
        
        # Update total value and allocations
        self._update_portfolio_state()
    
    def _recalculate_cash_position(self):
        """Recalculate cash position based on current holdings"""
        total_invested = 0.0
        for symbol, quantity in self.current_state.positions.items():
            price = self._get_current_price(symbol)
            total_invested += quantity * price
        
        self.current_state.cash = self.current_state.total_value - total_invested
        
        if self.current_state.cash < 0:
            logger.warning(f"Negative cash position: ${self.current_state.cash:.2f}")
    
    def _calculate_execution_cost(self, executed_positions: Dict[str, float]) -> float:
        """Calculate execution costs (commissions, slippage, etc.)"""
        # Simplified cost calculation
        total_cost = 0.0
        for symbol, quantity in executed_positions.items():
            price = self._get_current_price(symbol)
            trade_value = abs(quantity) * price
            # Assume 0.1% execution cost
            total_cost += trade_value * 0.001
        
        return total_cost
    
    def get_portfolio_metrics(self) -> Dict[str, Any]:
        """Get comprehensive portfolio metrics for monitoring"""
        state = self.get_current_state()
        
        metrics = {
            'portfolio_name': self.portfolio_name,
            'timestamp': state.timestamp.isoformat(),
            'total_value': state.total_value,
            'total_return': (state.total_value - self.config.initial_capital) / self.config.initial_capital,
            'cash': state.cash,
            'positions': state.positions,
            'allocations': state.allocations,
            'unrealized_pnl': state.unrealized_pnl,
            'realized_pnl': state.realized_pnl,
            'performance_metrics': state.performance_metrics,
            'execution_history_count': len(self.execution_history),
            'pending_signals_count': len(self.pending_signals)
        }
        
        if state.risk_metrics:
            metrics['risk_metrics'] = {
                'portfolio_var_95': state.risk_metrics.portfolio_var_95,
                'expected_shortfall': state.risk_metrics.expected_shortfall,
                'maximum_drawdown': state.risk_metrics.maximum_drawdown,
                'concentration_risk': state.risk_metrics.concentration_risk,
                'liquidity_risk': state.risk_metrics.liquidity_risk
            }
        
        return metrics
    
    def clear_pending_signals(self):
        """Clear pending signals queue"""
        cleared_count = len(self.pending_signals)
        self.pending_signals.clear()
        logger.info(f"Cleared {cleared_count} pending signals")
    
    def get_execution_history(self, limit: Optional[int] = None) -> List[ExecutionResult]:
        """Get execution history with optional limit"""
        if limit:
            return self.execution_history[-limit:]
        return self.execution_history.copy()
    
    def save_state(self, filepath: Optional[str] = None):
        """Save current portfolio state to file"""
        if not filepath:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filepath = f"portfolio_state_{self.portfolio_name}_{timestamp}.json"
        
        state_data = {
            'portfolio_name': self.portfolio_name,
            'config': self.config.__dict__,
            'current_state': {
                'timestamp': self.current_state.timestamp.isoformat(),
                'total_value': self.current_state.total_value,
                'positions': self.current_state.positions,
                'allocations': self.current_state.allocations,
                'cash': self.current_state.cash,
                'unrealized_pnl': self.current_state.unrealized_pnl,
                'realized_pnl': self.current_state.realized_pnl,
                'performance_metrics': self.current_state.performance_metrics
            },
            'execution_history_count': len(self.execution_history),
            'performance_history_count': len(self.performance_history)
        }
        
        try:
            with open(filepath, 'w') as f:
                json.dump(state_data, f, indent=2)
            logger.info(f"Portfolio state saved to {filepath}")
        except Exception as e:
            logger.error(f"Error saving portfolio state: {e}")

# Example usage and testing
if __name__ == "__main__":
    # Initialize Enhanced Portfolio Manager
    manager = EnhancedPortfolioManager("Myportolio")
    
    # Get current state
    state = manager.get_current_state()
    print(f"Portfolio: {manager.portfolio_name}")
    print(f"Total Value: ${state.total_value:,.2f}")
    print(f"Cash: ${state.cash:,.2f}")
    
    # Get portfolio metrics
    metrics = manager.get_portfolio_metrics()
    print(f"Portfolio Metrics: {json.dumps(metrics, indent=2, default=str)}")
