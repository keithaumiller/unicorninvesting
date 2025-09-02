"""
ETH Portfolio to Execution Bridge
Connects ETH portfolio decisions to order execution via IBKR
"""

import sys
import os
import logging
from datetime import datetime
from typing import Dict, List, Optional

# Add paths for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
portfolio_dir = os.path.join(current_dir, '..', '4_portfolios', 'Myportolio')
sys.path.append(portfolio_dir)

from eth_execution_engine import ETHExecutionEngine, Order, OrderSide, OrderType, ExecutionResult

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ETHPortfolioExecutor:
    """
    Bridge between ETH portfolio management and order execution
    Integrates Kelly Criterion decisions with IBKR order placement
    """
    
    def __init__(self, paper_trading: bool = True, 
                 execution_style: str = "market",
                 min_trade_size: float = 0.01):
        self.execution_engine = ETHExecutionEngine(paper_trading=paper_trading)
        self.execution_style = execution_style
        self.min_trade_size = min_trade_size
        
        # Portfolio state tracking
        self.current_position = 0.0
        self.target_position = 0.0
        self.cash_balance = 10000.0  # Starting cash for simulation
        self.trade_history = []
        
        logger.info(f"ETH Portfolio Executor initialized - Paper: {paper_trading}")
    
    def update_portfolio_state(self, current_eth: float, cash: float):
        """Update current portfolio state"""
        self.current_position = current_eth
        self.cash_balance = cash
        logger.info(f"Portfolio state: {current_eth:.4f} ETH, ${cash:,.2f} cash")
    
    def execute_kelly_decision(self, kelly_decision: Dict) -> Dict:
        """
        Execute a Kelly Criterion portfolio decision
        
        Args:
            kelly_decision: Dict with keys like 'target_allocation', 'current_allocation', 'action', etc.
        
        Returns:
            Dict with execution results and updated portfolio state
        """
        try:
            logger.info("🎯 Executing Kelly Criterion Decision")
            
            # Extract decision parameters
            target_allocation = kelly_decision.get('target_allocation', 0.0)
            current_allocation = kelly_decision.get('current_allocation', 0.0)
            total_portfolio_value = kelly_decision.get('portfolio_value', self.cash_balance)
            
            # Calculate target ETH position
            target_eth_value = total_portfolio_value * target_allocation
            current_market_price = self.execution_engine.get_market_price()
            
            if not current_market_price:
                return {
                    "success": False,
                    "message": "Could not get market price",
                    "execution_results": []
                }
            
            target_eth_quantity = target_eth_value / current_market_price
            position_change = target_eth_quantity - self.current_position
            
            logger.info(f"Target allocation: {target_allocation:.1%}")
            logger.info(f"Current allocation: {current_allocation:.1%}")
            logger.info(f"Position change: {position_change:+.4f} ETH")
            
            # Check if trade is significant enough
            if abs(position_change) < self.min_trade_size:
                return {
                    "success": True,
                    "message": f"Position change too small ({abs(position_change):.4f} < {self.min_trade_size})",
                    "execution_results": [],
                    "portfolio_state": self._get_portfolio_state()
                }
            
            # Execute the rebalancing
            execution_results = self.execution_engine.execute_portfolio_decision(
                current_eth_position=self.current_position,
                target_eth_position=target_eth_quantity,
                execution_style=self.execution_style
            )
            
            # Update portfolio state based on execution results
            successful_executions = [r for r in execution_results if r.success and r.filled_quantity > 0]
            
            for result in successful_executions:
                self._update_state_from_execution(result)
            
            # Record trade in history
            trade_record = {
                "timestamp": datetime.now(),
                "decision_type": "kelly_criterion",
                "target_allocation": target_allocation,
                "position_change": position_change,
                "execution_results": execution_results,
                "portfolio_value_after": self.cash_balance + (self.current_position * current_market_price)
            }
            self.trade_history.append(trade_record)
            
            return {
                "success": True,
                "message": f"Kelly decision executed: {len(successful_executions)} orders filled",
                "execution_results": execution_results,
                "portfolio_state": self._get_portfolio_state(),
                "trade_record": trade_record
            }
            
        except Exception as e:
            logger.error(f"Kelly decision execution error: {e}")
            return {
                "success": False,
                "message": f"Execution failed: {e}",
                "execution_results": []
            }
    
    def execute_momentum_signal(self, momentum_signal: Dict) -> Dict:
        """
        Execute a momentum strategy signal
        
        Args:
            momentum_signal: Dict with signal type, confidence, target_position, etc.
        
        Returns:
            Dict with execution results
        """
        try:
            logger.info("📈 Executing Momentum Signal")
            
            signal_type = momentum_signal.get('signal', 'hold')
            target_position_pct = momentum_signal.get('target_position', 0.0)
            confidence = momentum_signal.get('confidence', 0.0)
            
            # Convert percentage to actual position size
            current_market_price = self.execution_engine.get_market_price()
            if not current_market_price:
                return {"success": False, "message": "Could not get market price"}
            
            total_value = self.cash_balance + (self.current_position * current_market_price)
            target_eth_quantity = (total_value * target_position_pct) / current_market_price
            
            logger.info(f"Signal: {signal_type}, Confidence: {confidence:.3f}")
            logger.info(f"Target position: {target_position_pct:.1%} = {target_eth_quantity:.4f} ETH")
            
            # Execute based on signal confidence
            if confidence < 0.5:  # Low confidence - smaller position
                target_eth_quantity *= 0.5
                logger.info(f"Low confidence, reducing position to {target_eth_quantity:.4f} ETH")
            
            # Execute the position change
            execution_results = self.execution_engine.execute_portfolio_decision(
                current_eth_position=self.current_position,
                target_eth_position=target_eth_quantity,
                execution_style=self.execution_style
            )
            
            # Update portfolio state
            successful_executions = [r for r in execution_results if r.success and r.filled_quantity > 0]
            for result in successful_executions:
                self._update_state_from_execution(result)
            
            return {
                "success": True,
                "message": f"Momentum signal executed: {signal_type}",
                "execution_results": execution_results,
                "portfolio_state": self._get_portfolio_state()
            }
            
        except Exception as e:
            logger.error(f"Momentum signal execution error: {e}")
            return {
                "success": False,
                "message": f"Execution failed: {e}",
                "execution_results": []
            }
    
    def execute_risk_management_action(self, risk_action: Dict) -> Dict:
        """
        Execute risk management actions (stop loss, position reduction, etc.)
        
        Args:
            risk_action: Dict with action type, urgency, target_reduction, etc.
        
        Returns:
            Dict with execution results
        """
        try:
            logger.info("🛡️ Executing Risk Management Action")
            
            action_type = risk_action.get('action', 'hold')
            urgency = risk_action.get('urgency', 'normal')
            reduction_pct = risk_action.get('reduction_percentage', 0.0)
            
            if action_type == 'reduce_position':
                # Calculate new target position
                new_target = self.current_position * (1 - reduction_pct)
                
                # Use market orders for urgent risk management
                execution_style = "market" if urgency == "urgent" else self.execution_style
                
                execution_results = self.execution_engine.execute_portfolio_decision(
                    current_eth_position=self.current_position,
                    target_eth_position=new_target,
                    execution_style=execution_style
                )
                
                logger.info(f"Risk action: Reduce position by {reduction_pct:.1%}")
                
            elif action_type == 'emergency_exit':
                # Emergency liquidation
                execution_results = self.execution_engine.execute_portfolio_decision(
                    current_eth_position=self.current_position,
                    target_eth_position=0.0,
                    execution_style="market"  # Always use market orders for emergency
                )
                
                logger.info("Risk action: Emergency exit - liquidating all positions")
                
            else:
                return {
                    "success": True,
                    "message": f"No action required for {action_type}",
                    "execution_results": []
                }
            
            # Update portfolio state
            successful_executions = [r for r in execution_results if r.success and r.filled_quantity > 0]
            for result in successful_executions:
                self._update_state_from_execution(result)
            
            return {
                "success": True,
                "message": f"Risk management executed: {action_type}",
                "execution_results": execution_results,
                "portfolio_state": self._get_portfolio_state()
            }
            
        except Exception as e:
            logger.error(f"Risk management execution error: {e}")
            return {
                "success": False,
                "message": f"Risk execution failed: {e}",
                "execution_results": []
            }
    
    def _update_state_from_execution(self, result: ExecutionResult):
        """Update portfolio state based on execution result"""
        if not result.success or result.filled_quantity == 0:
            return
        
        # Update ETH position and cash balance
        order = self.execution_engine.get_order_status(result.order_id)
        if order:
            if order.side == OrderSide.BUY:
                self.current_position += result.filled_quantity
                self.cash_balance -= (result.filled_quantity * result.avg_fill_price + result.commission)
            else:  # SELL
                self.current_position -= result.filled_quantity
                self.cash_balance += (result.filled_quantity * result.avg_fill_price - result.commission)
        
        logger.info(f"Updated position: {self.current_position:.4f} ETH, ${self.cash_balance:,.2f} cash")
    
    def _get_portfolio_state(self) -> Dict:
        """Get current portfolio state"""
        current_price = self.execution_engine.get_market_price() or 0
        eth_value = self.current_position * current_price
        total_value = self.cash_balance + eth_value
        eth_allocation = eth_value / total_value if total_value > 0 else 0
        
        return {
            "eth_quantity": self.current_position,
            "cash_balance": self.cash_balance,
            "eth_value": eth_value,
            "total_portfolio_value": total_value,
            "eth_allocation": eth_allocation,
            "current_eth_price": current_price,
            "last_updated": datetime.now()
        }
    
    def get_performance_summary(self) -> Dict:
        """Get portfolio performance summary"""
        if not self.trade_history:
            return {"trades": 0, "message": "No trades executed yet"}
        
        initial_value = 10000.0  # Starting value
        current_state = self._get_portfolio_state()
        current_value = current_state["total_portfolio_value"]
        
        total_return = ((current_value - initial_value) / initial_value) * 100
        total_trades = len(self.trade_history)
        
        # Calculate execution statistics
        execution_summary = self.execution_engine.get_execution_summary()
        
        return {
            "initial_value": initial_value,
            "current_value": current_value,
            "total_return_pct": total_return,
            "total_trades": total_trades,
            "execution_stats": execution_summary,
            "current_portfolio": current_state,
            "trade_history": self.trade_history[-5:]  # Last 5 trades
        }

# Demo function
def demo_portfolio_executor():
    """Demonstrate the ETH Portfolio Executor"""
    print("🎯 ETH Portfolio Executor Demo")
    print("=" * 50)
    
    # Initialize executor
    executor = ETHPortfolioExecutor(paper_trading=True)
    
    # Set initial portfolio state
    executor.update_portfolio_state(current_eth=2.0, cash=8000.0)
    
    print("\n1. Testing Kelly Criterion Decision:")
    kelly_decision = {
        "target_allocation": 0.7,    # 70% ETH
        "current_allocation": 0.5,   # 50% ETH currently
        "portfolio_value": 10000.0,
        "confidence": 0.8
    }
    
    result = executor.execute_kelly_decision(kelly_decision)
    print(f"   Result: {result['message']}")
    if result['success'] and result['execution_results']:
        for exec_result in result['execution_results']:
            if exec_result.success:
                print(f"   Executed: {exec_result.filled_quantity:.4f} ETH @ ${exec_result.avg_fill_price:,.2f}")
    
    print("\n2. Testing Momentum Signal:")
    momentum_signal = {
        "signal": "buy",
        "confidence": 0.75,
        "target_position": 0.8,  # 80% ETH
        "reason": "Strong bullish momentum"
    }
    
    result = executor.execute_momentum_signal(momentum_signal)
    print(f"   Result: {result['message']}")
    
    print("\n3. Testing Risk Management:")
    risk_action = {
        "action": "reduce_position",
        "reduction_percentage": 0.2,  # Reduce by 20%
        "urgency": "normal",
        "reason": "VaR limit exceeded"
    }
    
    result = executor.execute_risk_management_action(risk_action)
    print(f"   Result: {result['message']}")
    
    print("\n4. Portfolio Performance Summary:")
    summary = executor.get_performance_summary()
    print(f"   Current Value: ${summary['current_value']:,.2f}")
    print(f"   Total Return: {summary['total_return_pct']:+.2f}%")
    print(f"   Total Trades: {summary['total_trades']}")
    print(f"   ETH Position: {summary['current_portfolio']['eth_quantity']:.4f} ETH")
    print(f"   ETH Allocation: {summary['current_portfolio']['eth_allocation']:.1%}")
    
    print("\n✅ ETH Portfolio Executor Demo Complete!")

if __name__ == "__main__":
    demo_portfolio_executor()
