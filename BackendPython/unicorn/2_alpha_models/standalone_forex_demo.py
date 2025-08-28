#!/usr/bin/env python3
"""
Standalone Forex Trading Demo
=============================

A simple standalone forex trading simulation that demonstrates
the basic concepts without requiring the full LEAN framework.

Currency Pairs: EURUSD, USDJPY, USDCNH, ETHUSD
Strategy: Simple Moving Average Crossover
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import json
from typing import Dict, List, Tuple, Optional

class StandaloneForexDemo:
    """
    Standalone forex trading demo for educational purposes.
    Simulates basic forex trading with moving average crossover strategy.
    """
    
    def __init__(self, initial_capital: float = 10000):
        self.initial_capital = initial_capital
        self.current_capital = initial_capital
        self.positions = {}
        self.trade_history = []
        self.price_data = {}
        
        # Currency pairs to trade
        self.symbols = ['EURUSD', 'USDJPY', 'USDCNH', 'ETHUSD']
        
        # Moving average periods
        self.fast_period = 10
        self.slow_period = 20
        
        # Risk management
        self.max_position_size = 0.1  # 10% of portfolio per position
        
        print(f"🦄 Unicorn Forex Demo Initialized")
        print(f"💰 Initial Capital: ${self.initial_capital:,.2f}")
        print(f"📈 Symbols: {', '.join(self.symbols)}")
        print(f"⚡ Fast MA: {self.fast_period}, Slow MA: {self.slow_period}")
    
    def generate_sample_data(self, symbol: str, days: int = 100) -> pd.DataFrame:
        """Generate realistic-looking sample forex data for demonstration."""
        
        # Base prices for different symbols
        base_prices = {
            'EURUSD': 1.1000,
            'USDJPY': 150.00,
            'USDCNH': 7.2000,
            'ETHUSD': 2000.00
        }
        
        # Generate dates
        end_date = datetime.now()
        start_date = end_date - timedelta(days=days)
        dates = pd.date_range(start=start_date, end=end_date, freq='h')[:-1]  # Hourly data
        
        # Generate random walk with some trending periods to ensure signals
        np.random.seed(42)  # For reproducible results
        base_price = base_prices.get(symbol, 1.0)
        
        # Create alternating trending and sideways periods for more interesting signals
        prices = [base_price]
        trend_length = 50  # periods per trend
        
        for i in range(1, len(dates)):
            # Create alternating uptrend/downtrend/sideways periods
            period = (i // trend_length) % 3
            if period == 0:  # Uptrend
                drift = 0.0008
                volatility = 0.003
            elif period == 1:  # Downtrend
                drift = -0.0008
                volatility = 0.003
            else:  # Sideways
                drift = 0.0001
                volatility = 0.004
            
            return_val = np.random.normal(drift, volatility)
            new_price = prices[-1] * (1 + return_val)
            prices.append(max(new_price, 0.001))  # Prevent negative prices
        
        df = pd.DataFrame({
            'timestamp': dates,
            'open': prices,
            'high': [p * (1 + abs(np.random.normal(0, 0.002))) for p in prices],
            'low': [p * (1 - abs(np.random.normal(0, 0.002))) for p in prices],
            'close': prices,
            'volume': np.random.randint(1000, 10000, len(dates))
        })
        
        df.set_index('timestamp', inplace=True)
        return df
    
    def calculate_moving_averages(self, data: pd.DataFrame) -> pd.DataFrame:
        """Calculate fast and slow moving averages."""
        data['fast_ma'] = data['close'].rolling(window=self.fast_period).mean()
        data['slow_ma'] = data['close'].rolling(window=self.slow_period).mean()
        
        # Calculate crossover signals
        data['signal'] = 0
        data.loc[data['fast_ma'] > data['slow_ma'], 'signal'] = 1  # Buy signal
        data.loc[data['fast_ma'] < data['slow_ma'], 'signal'] = -1  # Sell signal
        
        # Detect signal changes
        data['signal_change'] = data['signal'].diff()
        
        return data
    
    def calculate_position_size(self, symbol: str, price: float) -> float:
        """Calculate position size based on risk management rules."""
        max_investment = self.current_capital * self.max_position_size
        
        if symbol in ['USDJPY', 'USDCNH', 'ETHUSD']:
            # For these pairs, we buy the base currency with USD
            position_size = max_investment / price
        else:  # EURUSD
            # For EURUSD, we buy EUR with USD
            position_size = max_investment / price
            
        return position_size
    
    def execute_trade(self, symbol: str, signal: int, price: float, timestamp: datetime):
        """Execute a trade based on the signal."""
        
        if signal == 1:  # Buy signal
            if symbol not in self.positions or self.positions[symbol] <= 0:
                position_size = self.calculate_position_size(symbol, price)
                cost = position_size * price
                
                if cost <= self.current_capital:
                    self.positions[symbol] = position_size
                    self.current_capital -= cost
                    
                    trade = {
                        'timestamp': timestamp,
                        'symbol': symbol,
                        'action': 'BUY',
                        'size': position_size,
                        'price': price,
                        'cost': cost
                    }
                    self.trade_history.append(trade)
                    
                    print(f"🟢 BUY {symbol}: {position_size:.4f} @ {price:.4f} = ${cost:.2f}")
        
        elif signal == -1:  # Sell signal
            if symbol in self.positions and self.positions[symbol] > 0:
                position_size = self.positions[symbol]
                proceeds = position_size * price
                
                self.current_capital += proceeds
                self.positions[symbol] = 0
                
                trade = {
                    'timestamp': timestamp,
                    'symbol': symbol,
                    'action': 'SELL',
                    'size': position_size,
                    'price': price,
                    'proceeds': proceeds
                }
                self.trade_history.append(trade)
                
                print(f"🔴 SELL {symbol}: {position_size:.4f} @ {price:.4f} = ${proceeds:.2f}")
    
    def run_backtest(self):
        """Run the complete backtest simulation."""
        print(f"\n🚀 Starting Forex Trading Simulation...")
        print("=" * 50)
        
        # Generate data for all symbols
        for symbol in self.symbols:
            print(f"📊 Generating data for {symbol}...")
            self.price_data[symbol] = self.generate_sample_data(symbol)
        
        # Find common date range (use first symbol as base since we have same timeframe)
        common_dates = list(self.price_data[self.symbols[0]].index)
        common_dates = sorted(common_dates)
        
        if len(common_dates) == 0:
            print("❌ No common trading dates found!")
            return
        
        print(f"📅 Trading period: {common_dates[0]} to {common_dates[-1]}")
        print(f"📈 Total data points: {len(common_dates)}")
        print("\n🔄 Processing signals and executing trades...")
        
        # Process each time period
        for i, timestamp in enumerate(common_dates[self.slow_period:], self.slow_period):
            
            for symbol in self.symbols:
                # Get data up to current timestamp
                current_data = self.price_data[symbol].loc[:timestamp].tail(self.slow_period + 1)
                
                if len(current_data) >= self.slow_period:
                    # Calculate moving averages
                    current_data = self.calculate_moving_averages(current_data)
                    
                    # Get current signal and check for signal changes
                    if len(current_data) >= 2:  # Need at least 2 points to detect change
                        latest_signal = current_data['signal_change'].iloc[-1]
                        current_price = current_data['close'].iloc[-1]
                        
                        # Execute trade if signal changed and is significant
                        if abs(latest_signal) == 2 and not np.isnan(latest_signal):  # Signal flip from -1 to 1 or vice versa
                            signal_direction = 1 if latest_signal > 0 else -1
                            self.execute_trade(symbol, signal_direction, current_price, timestamp)
        
        self.print_results()
    
    def calculate_portfolio_value(self) -> float:
        """Calculate current portfolio value including open positions."""
        portfolio_value = self.current_capital
        
        for symbol, position in self.positions.items():
            if position > 0:
                # Use last known price
                last_price = self.price_data[symbol]['close'].iloc[-1]
                portfolio_value += position * last_price
                
        return portfolio_value
    
    def print_results(self):
        """Print comprehensive trading results."""
        print("\n" + "=" * 50)
        print("📊 TRADING RESULTS")
        print("=" * 50)
        
        # Portfolio performance
        final_portfolio_value = self.calculate_portfolio_value()
        total_return = final_portfolio_value - self.initial_capital
        return_percentage = (total_return / self.initial_capital) * 100
        
        print(f"💰 Initial Capital: ${self.initial_capital:,.2f}")
        print(f"💰 Final Portfolio Value: ${final_portfolio_value:,.2f}")
        print(f"📈 Total Return: ${total_return:,.2f} ({return_percentage:+.2f}%)")
        print(f"💵 Cash Available: ${self.current_capital:,.2f}")
        
        # Position summary
        print(f"\n📋 Current Positions:")
        for symbol, position in self.positions.items():
            if position > 0:
                last_price = self.price_data[symbol]['close'].iloc[-1]
                position_value = position * last_price
                print(f"  {symbol}: {position:.4f} units @ {last_price:.4f} = ${position_value:.2f}")
        
        # Trade statistics
        print(f"\n📊 Trade Statistics:")
        print(f"  Total Trades: {len(self.trade_history)}")
        
        buy_trades = [t for t in self.trade_history if t['action'] == 'BUY']
        sell_trades = [t for t in self.trade_history if t['action'] == 'SELL']
        
        print(f"  Buy Orders: {len(buy_trades)}")
        print(f"  Sell Orders: {len(sell_trades)}")
        
        if self.trade_history:
            # Calculate some basic metrics
            symbol_trades = {}
            for trade in self.trade_history:
                symbol = trade['symbol']
                if symbol not in symbol_trades:
                    symbol_trades[symbol] = []
                symbol_trades[symbol].append(trade)
            
            print(f"\n📈 Trades by Symbol:")
            for symbol, trades in symbol_trades.items():
                print(f"  {symbol}: {len(trades)} trades")
        
        print("\n🎯 Strategy: Simple Moving Average Crossover")
        print(f"⚡ Fast MA: {self.fast_period} periods, Slow MA: {self.slow_period} periods")
        print(f"🛡️  Max position size: {self.max_position_size*100:.0f}% of portfolio")
        print("\n" + "=" * 50)

def main():
    """Main function to run the forex trading demo."""
    print("🦄 Unicorn Investing - Forex Trading Demo")
    print("=" * 50)
    print("This is a standalone educational demonstration of basic forex trading concepts.")
    print("It simulates a simple moving average crossover strategy on major currency pairs.")
    print("\n⚠️  This is for educational purposes only - not real trading advice!")
    print("=" * 50)
    
    # Create and run the demo
    demo = StandaloneForexDemo(initial_capital=10000)
    demo.run_backtest()
    
    print(f"\n✅ Demo completed successfully!")
    print(f"📖 Check FOREX_HELLO_WORLD.md for detailed explanations.")

if __name__ == "__main__":
    main()
