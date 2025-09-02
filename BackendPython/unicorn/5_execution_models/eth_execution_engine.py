"""
LEAN Layer 5: ETH Order Execution Models
Basic order execution engine for ETH trading via IBKR
"""

import logging
import requests
import json
from datetime import datetime
from typing import Dict, List, Optional, Union
from dataclasses import dataclass
from enum import Enum

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class OrderType(Enum):
    MARKET = "MKT"
    LIMIT = "LMT"
    STOP = "STP"
    STOP_LIMIT = "STP LMT"

class OrderSide(Enum):
    BUY = "BUY"
    SELL = "SELL"

class OrderStatus(Enum):
    PENDING = "PendingSubmit"
    SUBMITTED = "Submitted"
    FILLED = "Filled"
    CANCELLED = "Cancelled"
    REJECTED = "Rejected"

@dataclass
class Order:
    """Order object for ETH trading"""
    symbol: str
    quantity: float
    side: OrderSide
    order_type: OrderType
    price: Optional[float] = None
    stop_price: Optional[float] = None
    time_in_force: str = "DAY"
    order_id: Optional[str] = None
    status: OrderStatus = OrderStatus.PENDING
    created_at: datetime = None
    
    def __post_init__(self):
        if self.created_at is None:
            self.created_at = datetime.now()

@dataclass
class ExecutionResult:
    """Result of order execution"""
    order_id: str
    success: bool
    message: str
    filled_quantity: float = 0.0
    avg_fill_price: float = 0.0
    commission: float = 0.0
    timestamp: datetime = None
    
    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = datetime.now()

class ETHExecutionEngine:
    """
    Basic ETH order execution engine for IBKR integration
    Handles order placement, monitoring, and execution logic
    """
    
    def __init__(self, base_url: str = "http://localhost:5000/v1/api", 
                 eth_contract_id: int = 541686654, paper_trading: bool = True):
        self.base_url = base_url
        self.eth_contract_id = eth_contract_id
        self.paper_trading = paper_trading
        self.session = requests.Session()
        self.orders = {}  # Store orders by order_id
        
        logger.info(f"ETH Execution Engine initialized - Paper Trading: {paper_trading}")
    
    def validate_order(self, order: Order) -> Dict[str, Union[bool, str]]:
        """Validate order before submission"""
        try:
            # Basic validation
            if order.quantity <= 0:
                return {"valid": False, "reason": "Quantity must be positive"}
            
            if order.order_type == OrderType.LIMIT and order.price is None:
                return {"valid": False, "reason": "Limit orders require price"}
            
            if order.order_type in [OrderType.STOP, OrderType.STOP_LIMIT] and order.stop_price is None:
                return {"valid": False, "reason": "Stop orders require stop price"}
            
            # ETH-specific validation
            if order.quantity < 0.001:  # Minimum ETH order size
                return {"valid": False, "reason": "Minimum ETH order size is 0.001"}
            
            if order.quantity > 100:  # Maximum single order size
                return {"valid": False, "reason": "Maximum single order size is 100 ETH"}
            
            logger.info(f"Order validation passed: {order.side.value} {order.quantity} ETH")
            return {"valid": True, "reason": "Order validated successfully"}
            
        except Exception as e:
            logger.error(f"Order validation error: {e}")
            return {"valid": False, "reason": f"Validation error: {e}"}
    
    def get_market_price(self) -> Optional[float]:
        """Get current ETH market price for order execution"""
        try:
            response = self.session.get(
                f"{self.base_url}/iserver/marketdata/snapshot",
                params={
                    'conids': self.eth_contract_id,
                    'fields': '31,84,86'  # Last, Bid, Ask
                },
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                if data and len(data) > 0:
                    market_data = data[0]
                    last_price = market_data.get('31')
                    
                    if last_price:
                        price = float(last_price)
                        logger.info(f"Current ETH market price: ${price:,.2f}")
                        return price
            
            logger.warning("Could not retrieve market price")
            return None
            
        except Exception as e:
            logger.error(f"Market price error: {e}")
            return None
    
    def place_order(self, order: Order) -> ExecutionResult:
        """Place order with IBKR (or simulate for paper trading)"""
        try:
            # Validate order first
            validation = self.validate_order(order)
            if not validation["valid"]:
                return ExecutionResult(
                    order_id="INVALID",
                    success=False,
                    message=validation["reason"]
                )
            
            # Generate order ID
            order_id = f"ETH_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{order.side.value}"
            order.order_id = order_id
            
            if self.paper_trading:
                # Simulate order execution for paper trading
                return self._simulate_order_execution(order)
            else:
                # Real IBKR order placement
                return self._place_real_order(order)
                
        except Exception as e:
            logger.error(f"Order placement error: {e}")
            return ExecutionResult(
                order_id="ERROR",
                success=False,
                message=f"Order placement failed: {e}"
            )
    
    def _simulate_order_execution(self, order: Order) -> ExecutionResult:
        """Simulate order execution for paper trading"""
        try:
            # Get current market price
            market_price = self.get_market_price()
            if not market_price:
                return ExecutionResult(
                    order_id=order.order_id,
                    success=False,
                    message="Could not get market price for simulation"
                )
            
            # Simulate execution based on order type
            if order.order_type == OrderType.MARKET:
                # Market orders execute immediately at market price
                fill_price = market_price
                # Add small slippage simulation
                slippage = 0.001 if order.side == OrderSide.BUY else -0.001
                fill_price = market_price * (1 + slippage)
                
            elif order.order_type == OrderType.LIMIT:
                # Limit orders execute at limit price if market allows
                if order.side == OrderSide.BUY and order.price >= market_price:
                    fill_price = min(order.price, market_price)
                elif order.side == OrderSide.SELL and order.price <= market_price:
                    fill_price = max(order.price, market_price)
                else:
                    # Order would not fill immediately
                    order.status = OrderStatus.SUBMITTED
                    self.orders[order.order_id] = order
                    return ExecutionResult(
                        order_id=order.order_id,
                        success=True,
                        message="Limit order submitted (not filled)",
                        filled_quantity=0.0
                    )
            else:
                fill_price = market_price
            
            # Calculate simulated commission (0.05% for crypto)
            commission = order.quantity * fill_price * 0.0005
            
            # Mark order as filled
            order.status = OrderStatus.FILLED
            self.orders[order.order_id] = order
            
            result = ExecutionResult(
                order_id=order.order_id,
                success=True,
                message="Order executed successfully (simulated)",
                filled_quantity=order.quantity,
                avg_fill_price=fill_price,
                commission=commission
            )
            
            logger.info(f"✅ Simulated execution: {order.side.value} {order.quantity} ETH @ ${fill_price:,.2f}")
            return result
            
        except Exception as e:
            logger.error(f"Simulation error: {e}")
            return ExecutionResult(
                order_id=order.order_id,
                success=False,
                message=f"Simulation failed: {e}"
            )
    
    def _place_real_order(self, order: Order) -> ExecutionResult:
        """Place real order with IBKR Gateway"""
        try:
            # Construct IBKR order payload
            order_payload = {
                "conid": self.eth_contract_id,
                "orderType": order.order_type.value,
                "side": order.side.value,
                "quantity": order.quantity,
                "tif": order.time_in_force
            }
            
            # Add price parameters based on order type
            if order.order_type == OrderType.LIMIT:
                order_payload["price"] = order.price
            elif order.order_type == OrderType.STOP:
                order_payload["auxPrice"] = order.stop_price
            elif order.order_type == OrderType.STOP_LIMIT:
                order_payload["price"] = order.price
                order_payload["auxPrice"] = order.stop_price
            
            # Submit order to IBKR
            response = self.session.post(
                f"{self.base_url}/iserver/account/orders",
                json={"orders": [order_payload]},
                timeout=10
            )
            
            if response.status_code == 200:
                result_data = response.json()
                order.status = OrderStatus.SUBMITTED
                self.orders[order.order_id] = order
                
                return ExecutionResult(
                    order_id=order.order_id,
                    success=True,
                    message="Order submitted to IBKR",
                    filled_quantity=0.0  # Will be updated when filled
                )
            else:
                return ExecutionResult(
                    order_id=order.order_id,
                    success=False,
                    message=f"IBKR order submission failed: {response.status_code}"
                )
                
        except Exception as e:
            logger.error(f"Real order placement error: {e}")
            return ExecutionResult(
                order_id=order.order_id,
                success=False,
                message=f"Real order placement failed: {e}"
            )
    
    def get_order_status(self, order_id: str) -> Optional[Order]:
        """Get current status of an order"""
        return self.orders.get(order_id)
    
    def cancel_order(self, order_id: str) -> ExecutionResult:
        """Cancel a pending order"""
        try:
            order = self.orders.get(order_id)
            if not order:
                return ExecutionResult(
                    order_id=order_id,
                    success=False,
                    message="Order not found"
                )
            
            if order.status in [OrderStatus.FILLED, OrderStatus.CANCELLED]:
                return ExecutionResult(
                    order_id=order_id,
                    success=False,
                    message=f"Cannot cancel order in {order.status.value} status"
                )
            
            # Mark as cancelled
            order.status = OrderStatus.CANCELLED
            
            logger.info(f"Order {order_id} cancelled")
            return ExecutionResult(
                order_id=order_id,
                success=True,
                message="Order cancelled successfully"
            )
            
        except Exception as e:
            logger.error(f"Order cancellation error: {e}")
            return ExecutionResult(
                order_id=order_id,
                success=False,
                message=f"Cancellation failed: {e}"
            )
    
    def execute_portfolio_decision(self, current_eth_position: float, 
                                 target_eth_position: float, 
                                 execution_style: str = "market") -> List[ExecutionResult]:
        """
        Execute portfolio rebalancing decision
        Convert portfolio target into actual orders
        """
        try:
            results = []
            position_change = target_eth_position - current_eth_position
            
            if abs(position_change) < 0.001:  # No significant change
                logger.info("No significant position change required")
                return results
            
            # Determine order side and quantity
            if position_change > 0:
                side = OrderSide.BUY
                quantity = abs(position_change)
            else:
                side = OrderSide.SELL
                quantity = abs(position_change)
            
            # Create order based on execution style
            if execution_style == "market":
                order = Order(
                    symbol="ETH",
                    quantity=quantity,
                    side=side,
                    order_type=OrderType.MARKET
                )
            elif execution_style == "limit":
                market_price = self.get_market_price()
                if not market_price:
                    raise ValueError("Cannot get market price for limit order")
                
                # Set limit price with small buffer
                if side == OrderSide.BUY:
                    limit_price = market_price * 1.001  # 0.1% above market
                else:
                    limit_price = market_price * 0.999  # 0.1% below market
                
                order = Order(
                    symbol="ETH",
                    quantity=quantity,
                    side=side,
                    order_type=OrderType.LIMIT,
                    price=limit_price
                )
            else:
                raise ValueError(f"Unknown execution style: {execution_style}")
            
            # Execute the order
            result = self.place_order(order)
            results.append(result)
            
            logger.info(f"Portfolio rebalancing: {side.value} {quantity:.4f} ETH")
            return results
            
        except Exception as e:
            logger.error(f"Portfolio execution error: {e}")
            error_result = ExecutionResult(
                order_id="PORTFOLIO_ERROR",
                success=False,
                message=f"Portfolio execution failed: {e}"
            )
            return [error_result]
    
    def get_execution_summary(self) -> Dict:
        """Get summary of all executions"""
        total_orders = len(self.orders)
        filled_orders = sum(1 for o in self.orders.values() if o.status == OrderStatus.FILLED)
        cancelled_orders = sum(1 for o in self.orders.values() if o.status == OrderStatus.CANCELLED)
        pending_orders = sum(1 for o in self.orders.values() if o.status in [OrderStatus.PENDING, OrderStatus.SUBMITTED])
        
        return {
            "total_orders": total_orders,
            "filled_orders": filled_orders,
            "cancelled_orders": cancelled_orders,
            "pending_orders": pending_orders,
            "fill_rate": filled_orders / total_orders if total_orders > 0 else 0,
            "orders": list(self.orders.values())
        }

# Demo function for testing
def demo_execution_engine():
    """Demonstrate the ETH execution engine"""
    print("🚀 ETH Execution Engine Demo")
    print("=" * 40)
    
    # Initialize execution engine in paper trading mode
    engine = ETHExecutionEngine(paper_trading=True)
    
    # Test market buy order
    print("\n1. Testing Market Buy Order:")
    buy_order = Order(
        symbol="ETH",
        quantity=1.5,
        side=OrderSide.BUY,
        order_type=OrderType.MARKET
    )
    
    result = engine.place_order(buy_order)
    print(f"   Result: {result.message}")
    if result.success:
        print(f"   Filled: {result.filled_quantity} ETH @ ${result.avg_fill_price:,.2f}")
        print(f"   Commission: ${result.commission:.2f}")
    
    # Test limit sell order
    print("\n2. Testing Limit Sell Order:")
    sell_order = Order(
        symbol="ETH",
        quantity=0.8,
        side=OrderSide.SELL,
        order_type=OrderType.LIMIT,
        price=4400.00  # Limit price
    )
    
    result = engine.place_order(sell_order)
    print(f"   Result: {result.message}")
    
    # Test portfolio rebalancing
    print("\n3. Testing Portfolio Rebalancing:")
    current_position = 2.5  # ETH
    target_position = 3.2   # ETH
    
    results = engine.execute_portfolio_decision(current_position, target_position)
    for result in results:
        print(f"   Result: {result.message}")
        if result.success and result.filled_quantity > 0:
            print(f"   Executed: {result.filled_quantity} ETH @ ${result.avg_fill_price:,.2f}")
    
    # Show execution summary
    print("\n4. Execution Summary:")
    summary = engine.get_execution_summary()
    print(f"   Total Orders: {summary['total_orders']}")
    print(f"   Filled Orders: {summary['filled_orders']}")
    print(f"   Fill Rate: {summary['fill_rate']:.1%}")
    
    print("\n✅ ETH Execution Engine Demo Complete!")

if __name__ == "__main__":
    demo_execution_engine()
