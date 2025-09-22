#!/usr/bin/env python3
"""
Backtesting Integration Framework
Integrates our live data pipeline with multiple backtesting frameworks
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import json
from typing import Dict, List, Tuple, Optional
import logging

class BacktestingEngine:
    """
    Multi-Framework Backtesting Integration
    Converts our live trading signals into framework-compatible formats
    """
    
    def __init__(self, initial_capital: float = 100000.0):
        """
        Initialize backtesting engine
        
        Args:
            initial_capital: Starting capital for backtesting
        """
        self.initial_capital = initial_capital
        self.current_capital = initial_capital
        self.positions = {}
        self.trades = []
        self.performance_history = []
        
        self.logger = logging.getLogger(__name__)
        
    def process_data_point(self, 
                          timestamp: datetime,
                          symbol: str,
                          price: float,
                          signal: float,
                          position_size: float,
                          features: Dict) -> Dict:
        """
        Process a single data point for backtesting
        
        Args:
            timestamp: Data point timestamp
            symbol: Asset symbol
            price: Current price
            signal: Trading signal (-1 to 1)
            position_size: Recommended position size (0 to 1)
            features: Technical features dictionary
            
        Returns:
            Backtest result for this data point
        """
        
        # Create framework-compatible data structure
        backtest_data = {
            'Time': timestamp.isoformat(),
            'Symbol': symbol,
            'Price': price,
            'Volume': 1000000,  # Default volume
            'Signal': signal,
            'PositionSize': position_size
        }
        
        # Add technical features
        backtest_data.update(features)
        
        # Execute trade if position size is significant
        trade_executed = False
        if abs(position_size) > 0.01:  # 1% minimum position
            trade_result = self._execute_backtest_trade(
                symbol=symbol,
                timestamp=timestamp,
                price=price,
                target_position=position_size
            )
            
            if trade_result:
                backtest_data['TradeExecuted'] = True
                backtest_data['TradeDetails'] = trade_result
                trade_executed = True
        
        # Calculate portfolio metrics
        portfolio_value = self._calculate_portfolio_value(price, symbol)
        
        backtest_data.update({
            'PortfolioValue': portfolio_value,
            'Cash': self.current_capital,
            'TotalReturn': (portfolio_value - self.initial_capital) / self.initial_capital,
            'TradeExecuted': trade_executed
        })
        
        # Store performance
        self.performance_history.append(backtest_data.copy())
        
        return backtest_data
    
    def _execute_backtest_trade(self, 
                               symbol: str,
                               timestamp: datetime,
                               price: float,
                               target_position: float) -> Optional[Dict]:
        """Execute a trade in the backtest"""
        
        try:
            current_position = self.positions.get(symbol, 0.0)
            portfolio_value = self._calculate_portfolio_value(price, symbol)
            
            # Calculate target dollar amount
            target_value = target_position * portfolio_value
            current_value = current_position * price
            
            # Calculate trade size
            trade_value = target_value - current_value
            trade_quantity = trade_value / price if price > 0 else 0
            
            if abs(trade_value) > portfolio_value * 0.001:  # Minimum trade threshold
                # Execute trade
                new_position = current_position + trade_quantity
                
                # Update positions
                self.positions[symbol] = new_position
                
                # Update cash
                self.current_capital -= trade_value
                
                # Record trade
                trade_record = {
                    'Timestamp': timestamp.isoformat(),
                    'Symbol': symbol,
                    'Action': 'BUY' if trade_quantity > 0 else 'SELL',
                    'Quantity': abs(trade_quantity),
                    'Price': price,
                    'Value': abs(trade_value),
                    'NewPosition': new_position,
                    'CashRemaining': self.current_capital
                }
                
                self.trades.append(trade_record)
                
                self.logger.info(f"🔄 Backtest Trade: {trade_record['Action']} {trade_record['Quantity']:.6f} {symbol} @ ${price:.2f}")
                
                return trade_record
                
        except Exception as e:
            self.logger.error(f"❌ Backtest trade failed: {e}")
            
        return None
    
    def _calculate_portfolio_value(self, current_price: float, symbol: str) -> float:
        """Calculate total portfolio value"""
        try:
            total_value = self.current_capital
            
            # Add position values (simplified - assumes only one asset for this trace)
            if symbol in self.positions:
                position_value = self.positions[symbol] * current_price
                total_value += position_value
                
            return total_value
            
        except Exception as e:
            self.logger.error(f"❌ Portfolio value calculation failed: {e}")
            return self.current_capital
    
    def get_performance_summary(self) -> Dict:
        """Get backtesting performance summary"""
        
        if not self.performance_history:
            return {'error': 'No performance history available'}
        
        # Calculate metrics from performance history
        returns = [p['TotalReturn'] for p in self.performance_history]
        portfolio_values = [p['PortfolioValue'] for p in self.performance_history]
        
        summary = {
            'initial_capital': self.initial_capital,
            'final_portfolio_value': portfolio_values[-1] if portfolio_values else self.initial_capital,
            'total_return': returns[-1] if returns else 0.0,
            'total_trades': len(self.trades),
            'data_points_processed': len(self.performance_history),
            'max_portfolio_value': max(portfolio_values) if portfolio_values else self.initial_capital,
            'min_portfolio_value': min(portfolio_values) if portfolio_values else self.initial_capital,
            'trades_summary': self.trades[-5:] if self.trades else [],  # Last 5 trades
            'performance_trend': 'UP' if returns[-1] > 0 else 'DOWN' if returns[-1] < 0 else 'FLAT'
        }
        
        return summary
    
    def export_standard_format(self, output_path: str) -> bool:
        """Export results in standard format"""
        
        try:
            # Create standard output format
            standard_output = {
                'BacktestId': f"unicorn_backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
                'StartDate': self.performance_history[0]['Time'] if self.performance_history else datetime.now().isoformat(),
                'EndDate': self.performance_history[-1]['Time'] if self.performance_history else datetime.now().isoformat(),
                'InitialCapital': self.initial_capital,
                'FinalValue': self.performance_history[-1]['PortfolioValue'] if self.performance_history else self.initial_capital,
                'TotalReturn': self.performance_history[-1]['TotalReturn'] if self.performance_history else 0.0,
                'Trades': self.trades,
                'PerformanceHistory': self.performance_history,
                'Summary': self.get_performance_summary()
            }
            
            # Write to file
            with open(output_path, 'w') as f:
                json.dump(standard_output, f, indent=2, default=str)
            
            self.logger.info(f"✅ Backtest results exported to {output_path}")
            return True
            
        except Exception as e:
            self.logger.error(f"❌ Failed to export results: {e}")
            return False

# Demo function for basic usage examples
def demo_backtesting_integration():
    """Demonstrate basic backtesting integration functionality"""
    
    print("🔬 BACKTESTING INTEGRATION DEMO")
    print("=" * 50)
    
    # Initialize backtesting engine
    backtest_engine = BacktestingEngine(initial_capital=100000.0)
    
    print("✅ Backtesting engine initialized successfully")
    print("� For full testing, run the test suite in /tests/unicorn/test_backtesting_integration.py")
    
    return backtest_engine

if __name__ == "__main__":
    demo_backtesting_integration()