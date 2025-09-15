#!/usr/bin/env python3
"""
ETH Algorithm Integration System
Real-time integration of ETH momentum and risk algorithms with REAL historical data
Connects silver layer data warehouse to ETH momentum strategy and risk management
🚫 NO SIMULATED DATA - REAL HISTORICAL DATA ONLY
"""

import sys
import os
import pandas as pd
from datetime import datetime
import logging

# Add portfolio path to system path
portfolio_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
data_path = "/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources"
sys.path.append(portfolio_path)
sys.path.append(data_path)

# Import our algorithms and data connector
from trading_algorithms.eth_momentum_strategy import ETHMomentumStrategy
from risk_algorithms.eth_basic_risk import ETHBasicRisk
# Import silver layer data connector for REAL historical data
from silver_layer_data_connector import SilverLayerDataConnector

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ETHAlgorithmRunner:
    """Integrates ETH data feed with trading and risk algorithms"""
    
    def __init__(self, use_real_data=True):
        """Initialize with REAL historical data from silver layer by default"""
        self.use_real_data = use_real_data
        
        # Initialize data connector with REAL data from silver layer
        self.data_connector = SilverLayerDataConnector()
        logger.info("🎯 Using REAL historical data from silver layer data warehouse")
            
        # Initialize algorithms
        trading_config = {
            'short_ma_period': 10,
            'long_ma_period': 20,
            'max_position_size': 0.8,  # 80% max position
            'volatility_window': 14
        }
        self.trading_algorithm = ETHMomentumStrategy(trading_config)
        self.risk_algorithm = ETHBasicRisk()
        
        # Portfolio state
        self.portfolio = {
            'cash': 100000.0,  # Starting with $100k
            'eth_position': 0.0,
            'total_value': 100000.0,
            'max_drawdown': 0.0,
            'last_price': 0.0
        }
        
        logger.info("ETH Algorithm Runner initialized")
        
    def get_market_data(self):
        """Get current market data feed from silver layer"""
        try:
            # Get latest price
            latest_price = self.data_connector.get_live_price('ETH')
            
            # Get historical data for analysis
            historical_data = self.data_connector.get_historical_data(
                asset='ETH',
                interval='1h',
                periods=100
            )
            
            return {
                'current': {
                    'last_price': latest_price,
                    'timestamp': historical_data['Datetime'].iloc[-1] if not historical_data.empty else None
                },
                'historical': historical_data
            }
        except Exception as e:
            logger.error(f"Error getting market data: {e}")
            return {
                'current': {'last_price': 0, 'timestamp': None},
                'historical': pd.DataFrame()
            }
    
    def run_trading_algorithm(self, market_data):
        """Run the momentum trading algorithm"""
        
        current_price = market_data['current']['last_price']
        historical_df = market_data['historical']
        
        # Generate trading signal using historical data
        if not historical_df.empty:
            signal_result = self.trading_algorithm.generate_signal(historical_df)
            signal_strength = signal_result.get('signal_strength', 0)
            
            logger.info(f"Trading signal result: {signal_result}")
            
            # Convert signal strength to simple buy/sell/hold signal
            if signal_strength > 0.1:
                signal = 1  # Buy
            elif signal_strength < -0.1:
                signal = -1  # Sell
            else:
                signal = 0  # Hold
                
            return signal
        else:
            logger.warning("No historical data available for trading signal")
            return 0
    
    def run_risk_algorithm(self, market_data, proposed_position):
        """Run risk management checks"""
        
        current_price = market_data['current']['last_price']
        
        # Calculate proposed portfolio value
        proposed_eth_value = proposed_position * current_price
        proposed_total = self.portfolio['cash'] + proposed_eth_value
        
        # Create portfolio data for risk assessment
        portfolio_data = {
            'total_value': proposed_total,
            'positions': {'ETH': proposed_position},
            'cash': self.portfolio['cash'],
            'current_prices': {'ETH': current_price}
        }
        
        # Check risk constraints
        risk_check = self.risk_algorithm.validate_portfolio_risk(portfolio_data)
        
        logger.info(f"Risk check: {risk_check}")
        
        return risk_check
    
    def execute_trade(self, signal, current_price):
        """Execute trading decision based on signal and risk checks"""
        
        if signal == 0:
            return None  # No trade
            
        # Calculate position size (simplified)
        max_position_value = self.portfolio['total_value'] * 0.8  # Max 80% allocation
        max_eth_shares = max_position_value / current_price
        
        if signal > 0:  # Buy signal
            proposed_position = min(max_eth_shares, self.portfolio['eth_position'] + 10)  # Add 10 ETH
        else:  # Sell signal
            proposed_position = max(0, self.portfolio['eth_position'] - 5)  # Sell 5 ETH
            
        # Get current market data for risk check
        market_data = self.get_market_data()
        
        # Run risk check
        risk_check = self.run_risk_algorithm(market_data, proposed_position)
        
        if risk_check['approved']:
            # Execute trade
            eth_change = proposed_position - self.portfolio['eth_position']
            cash_change = -eth_change * current_price
            
            self.portfolio['eth_position'] = proposed_position
            self.portfolio['cash'] += cash_change
            self.portfolio['total_value'] = self.portfolio['cash'] + (self.portfolio['eth_position'] * current_price)
            self.portfolio['last_price'] = current_price
            
            trade_info = {
                'action': 'BUY' if eth_change > 0 else 'SELL',
                'quantity': abs(eth_change),
                'price': current_price,
                'value': abs(cash_change),
                'new_position': self.portfolio['eth_position'],
                'new_cash': self.portfolio['cash'],
                'total_value': self.portfolio['total_value']
            }
            
            logger.info(f"Trade executed: {trade_info}")
            return trade_info
        else:
            logger.warning(f"Trade rejected by risk management: {risk_check['reason']}")
            return None
    
    def run_single_cycle(self):
        """Run a single algorithm cycle"""
        
        logger.info("=" * 50)
        logger.info(f"Running ETH Algorithm Cycle - {datetime.now()}")
        
        # Get market data
        market_data = self.get_market_data()
        current_price = market_data['current']['last_price']
        
        logger.info(f"Current ETH Price: ${current_price}")
        logger.info(f"Portfolio: {self.portfolio['eth_position']:.2f} ETH, "
                   f"${self.portfolio['cash']:.2f} Cash, "
                   f"Total: ${self.portfolio['total_value']:.2f}")
        
        # Run trading algorithm
        signal = self.run_trading_algorithm(market_data)
        
        # Execute trade if signal present
        if signal != 0:
            trade_result = self.execute_trade(signal, current_price)
            if trade_result:
                logger.info(f"✅ Trade completed: {trade_result['action']} {trade_result['quantity']} ETH @ ${trade_result['price']}")
            else:
                logger.info("❌ Trade rejected by risk management")
        else:
            logger.info("No trading signal generated")
        
        return {
            'timestamp': datetime.now(),
            'price': current_price,
            'signal': signal,
            'portfolio': self.portfolio.copy(),
            'market_data': market_data
        }
    
    def run_backtest(self, cycles=10):
        """Run multiple algorithm cycles for testing"""
        
        logger.info(f"Starting ETH algorithm backtest with {cycles} cycles")
        results = []
        
        for i in range(cycles):
            cycle_result = self.run_single_cycle()
            results.append(cycle_result)
            
            # Brief pause between cycles
            import time
            time.sleep(1)
        
        # Calculate performance metrics
        final_value = self.portfolio['total_value']
        initial_value = 100000.0
        total_return = (final_value / initial_value) - 1
        
        logger.info("=" * 50)
        logger.info("BACKTEST RESULTS")
        logger.info(f"Initial Value: ${initial_value:,.2f}")
        logger.info(f"Final Value: ${final_value:,.2f}")
        logger.info(f"Total Return: {total_return*100:.2f}%")
        logger.info(f"Final ETH Position: {self.portfolio['eth_position']:.2f}")
        logger.info(f"Final Cash: ${self.portfolio['cash']:,.2f}")
        
        return results

def main():
    """Test the integrated ETH algorithm system"""
    print("=" * 60)
    print("ETH Algorithm Integration Test")
    print(f"Timestamp: {datetime.now()}")
    print("=" * 60)
    
    # Initialize algorithm runner with REAL data
    runner = ETHAlgorithmRunner(use_real_data=True)
    
    # Run a few test cycles
    print("\nRunning algorithm test cycles...")
    results = runner.run_backtest(cycles=5)
    
    print("\n✅ ETH Algorithm Integration Complete!")
    print("🎯 Ready for live trading with IBKR data when bridge is configured")

if __name__ == "__main__":
    main()
