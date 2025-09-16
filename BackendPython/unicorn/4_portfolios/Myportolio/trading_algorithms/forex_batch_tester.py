#!/usr/bin/env python3
"""
Forex Momentum Strategy Batch Tester
===================================

Complete the remaining 3 forex pairs (USDCAD, USDCHF, NZDUSD) with streamlined testing.
This allows us to quickly evaluate all 7 major forex pairs and compare results.

Author: Unicorn Investing Platform
Date: September 2025
"""

import sys
import os
import pandas as pd
import numpy as np
import yfinance as yf
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import warnings
warnings.filterwarnings('ignore')

class SimplifiedForexMomentum:
    """Simplified forex momentum strategy for quick testing"""
    
    def __init__(self, symbol: str, 
                 lookback_fast: int = 7,
                 lookback_slow: int = 20,
                 momentum_threshold: float = 0.006,
                 stop_loss: float = 0.018,
                 take_profit: float = 0.030):
        self.symbol = symbol
        self.asset_name = symbol.replace("=X", "")
        self.lookback_fast = lookback_fast
        self.lookback_slow = lookback_slow
        self.momentum_threshold = momentum_threshold
        self.stop_loss = stop_loss
        self.take_profit = take_profit
        self.initial_capital = 10000.0
        
        # Strategy state
        self.portfolio_value = self.initial_capital
        self.position = 0.0
        self.entry_price = 0.0
        self.trades = []
        self.daily_returns = []
    
    def get_data(self, start_date: str, end_date: str) -> Optional[pd.DataFrame]:
        """Get forex data for the specified period"""
        try:
            ticker = yf.Ticker(self.symbol)
            data = ticker.history(start=start_date, end=end_date)
            return data if len(data) > 30 else None
        except:
            return None
    
    def calculate_signals(self, data: pd.DataFrame) -> pd.DataFrame:
        """Calculate basic momentum signals"""
        df = data.copy()
        
        # Basic momentum
        df['price_change_fast'] = df['Close'].pct_change(self.lookback_fast)
        df['ema_fast'] = df['Close'].ewm(span=self.lookback_fast).mean()
        df['ema_slow'] = df['Close'].ewm(span=self.lookback_slow).mean()
        
        # RSI
        delta = df['Close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # Signals
        df['buy_signal'] = ((df['price_change_fast'] > self.momentum_threshold) & 
                           (df['ema_fast'] > df['ema_slow']) &
                           (df['rsi'].between(30, 75))).astype(int)
        
        df['sell_signal'] = ((df['price_change_fast'] < -self.momentum_threshold) |
                            (df['rsi'] < 25)).astype(int)
        
        return df
    
    def simulate(self, start_date: str = "2025-03-01", end_date: str = "2025-09-16") -> Dict:
        """Run simplified backtest"""
        data = self.get_data(start_date, end_date)
        if data is None:
            return {'error': 'No data'}
        
        data_with_signals = self.calculate_signals(data)
        
        # Reset state
        self.portfolio_value = self.initial_capital
        self.position = 0.0
        self.trades = []
        self.daily_returns = []
        
        portfolio_history = []
        
        for i, (date, row) in enumerate(data_with_signals.iterrows()):
            current_price = row['Close']
            
            # Risk management
            if self.position > 0:
                price_change = (current_price - self.entry_price) / self.entry_price
                if price_change <= -self.stop_loss or price_change >= self.take_profit:
                    pnl = self.position * (current_price - self.entry_price)
                    self.portfolio_value += pnl
                    self.trades.append({
                        'type': 'close',
                        'date': date.strftime('%Y-%m-%d'),
                        'pnl': pnl
                    })
                    self.position = 0
            
            # Calculate portfolio value
            if self.position != 0:
                unrealized_pnl = self.position * (current_price - self.entry_price)
                current_portfolio_value = self.portfolio_value + unrealized_pnl
            else:
                current_portfolio_value = self.portfolio_value
            
            # Daily returns
            if i > 0:
                daily_return = (current_portfolio_value - portfolio_history[-1]) / portfolio_history[-1]
                self.daily_returns.append(daily_return)
            
            portfolio_history.append(current_portfolio_value)
            
            # Trading signals
            if row['buy_signal'] == 1 and self.position <= 0:
                position_size = 0.15  # Fixed 15% position
                position_value = self.portfolio_value * position_size
                self.position = position_value / current_price
                self.entry_price = current_price
                self.trades.append({
                    'type': 'buy',
                    'date': date.strftime('%Y-%m-%d'),
                    'price': current_price
                })
            elif row['sell_signal'] == 1 and self.position > 0:
                pnl = self.position * (current_price - self.entry_price)
                self.portfolio_value += pnl
                self.trades.append({
                    'type': 'sell',
                    'date': date.strftime('%Y-%m-%d'),
                    'pnl': pnl
                })
                self.position = 0
        
        # Final portfolio value
        if self.position != 0:
            final_price = data_with_signals['Close'].iloc[-1]
            final_pnl = self.position * (final_price - self.entry_price)
            self.portfolio_value += final_pnl
        
        # Calculate metrics
        total_return = (self.portfolio_value - self.initial_capital) / self.initial_capital
        total_days = len(data_with_signals)
        annualized_return = (1 + total_return) ** (365 / total_days) - 1
        
        trades_with_pnl = [t for t in self.trades if 'pnl' in t]
        winning_trades = [t for t in trades_with_pnl if t['pnl'] > 0]
        win_rate = len(winning_trades) / len(trades_with_pnl) if trades_with_pnl else 0
        
        daily_returns_array = np.array(self.daily_returns)
        volatility = np.std(daily_returns_array) * np.sqrt(252) if len(daily_returns_array) > 0 else 0
        sharpe_ratio = annualized_return / volatility if volatility > 0 else 0
        
        # Max drawdown
        portfolio_series = pd.Series(portfolio_history)
        rolling_max = portfolio_series.expanding().max()
        drawdown = (portfolio_series - rolling_max) / rolling_max
        max_drawdown = drawdown.min()
        
        return {
            'success': True,
            'asset': self.asset_name,
            'total_return': total_return,
            'annualized_return': annualized_return,
            'total_trades': len(self.trades),
            'pnl_trades': len(trades_with_pnl),
            'win_rate': win_rate,
            'sharpe_ratio': sharpe_ratio,
            'max_drawdown': max_drawdown,
            'final_value': self.portfolio_value
        }

def test_remaining_forex_pairs():
    """Test the remaining 3 forex pairs"""
    
    pairs = [
        ("USDCAD=X", "Canadian Dollar"),
        ("USDCHF=X", "Swiss Franc"), 
        ("NZDUSD=X", "New Zealand Dollar")
    ]
    
    results = []
    
    print("🚀 Testing Remaining Forex Pairs - Quick Validation")
    print("=" * 55)
    
    for symbol, name in pairs:
        print(f"\n📊 Testing {name} ({symbol.replace('=X', '')})")
        print("-" * 40)
        
        strategy = SimplifiedForexMomentum(symbol)
        result = strategy.simulate()
        
        if result.get('success'):
            results.append(result)
            
            print(f"✅ {result['asset']} Results:")
            print(f"   Total Return: {result['total_return']:.2%}")
            print(f"   Annual Return: {result['annualized_return']:.2%}")
            print(f"   Trades: {result['pnl_trades']} P&L trades")
            print(f"   Win Rate: {result['win_rate']:.1%}")
            print(f"   Sharpe Ratio: {result['sharpe_ratio']:.2f}")
            print(f"   Max Drawdown: {result['max_drawdown']:.2%}")
            
            # Get benchmark
            try:
                ticker = yf.Ticker(symbol)
                benchmark_data = ticker.history(start="2025-03-01", end="2025-09-16")
                if len(benchmark_data) > 0:
                    benchmark_return = (benchmark_data['Close'].iloc[-1] - benchmark_data['Close'].iloc[0]) / benchmark_data['Close'].iloc[0]
                    excess_return = result['total_return'] - benchmark_return
                    print(f"   vs Buy&Hold: {excess_return:.2%} excess return")
            except:
                pass
                
        else:
            print(f"❌ {symbol} failed: {result.get('error', 'Unknown error')}")
    
    return results

def main():
    """Run batch forex testing"""
    results = test_remaining_forex_pairs()
    
    print(f"\n🏆 Quick Forex Testing Complete")
    print("=" * 35)
    print(f"📊 Tested {len(results)} additional pairs")
    print("📋 Use these results for final forex comparison")

if __name__ == "__main__":
    main()