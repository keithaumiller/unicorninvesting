#!/usr/bin/env python3
"""
EURUSD Momentum Strategy V2 - Optimized for Forex
===============================================

Enhanced momentum trading strategy for EURUSD with forex-specific optimizations.
Based on lessons learned from the initial version and adapted for forex market characteristics.

Key Improvements:
- Lower momentum thresholds for forex volatility
- Multi-signal confirmation system
- Forex-specific risk management
- Trend filtering with currency correlation
- Dynamic position sizing

Expected Performance Range:
- Target Annual Return: 8-15%
- Expected Sharpe Ratio: 0.6-1.0
- Maximum Drawdown: <8%
- Win Rate: 50-60%

Author: Unicorn Investing Platform
Date: September 2025
"""

import sys
import os
import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta
import yfinance as yf
from typing import Dict, List, Tuple, Optional
import warnings
warnings.filterwarnings('ignore')

# Add project paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')

class EURUSDMomentumV2:
    """Enhanced EURUSD Momentum Strategy optimized for forex characteristics"""
    
    def __init__(self, 
                 initial_capital: float = 10000.0,
                 lookback_fast: int = 5,
                 lookback_slow: int = 20,
                 momentum_threshold: float = 0.008,  # Lower threshold for forex
                 stop_loss: float = 0.012,          # Tighter stop loss
                 take_profit: float = 0.020,        # Lower take profit
                 max_position_size: float = 0.20):  # Conservative sizing
        """
        Initialize enhanced EURUSD momentum strategy
        
        Args:
            initial_capital: Starting capital for simulation
            lookback_fast: Fast momentum lookback period
            lookback_slow: Slow momentum lookback period
            momentum_threshold: Minimum momentum signal strength (lower for forex)
            stop_loss: Stop loss as fraction of price
            take_profit: Take profit as fraction of price
            max_position_size: Maximum position size as fraction of portfolio
        """
        self.symbol = "EURUSD=X"
        self.asset_name = "EURUSD"
        self.initial_capital = initial_capital
        self.lookback_fast = lookback_fast
        self.lookback_slow = lookback_slow
        self.momentum_threshold = momentum_threshold
        self.stop_loss = stop_loss
        self.take_profit = take_profit
        self.max_position_size = max_position_size
        
        # Strategy state
        self.portfolio_value = initial_capital
        self.position = 0.0
        self.entry_price = 0.0
        self.trades = []
        self.daily_returns = []
        
        # Forex-specific parameters
        self.confirmation_period = 3  # Require signal confirmation
        self.volatility_lookback = 10
        
        # Setup logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
        
        print(f"🚀 EURUSD Momentum Strategy V2 Initialized")
        print(f"📊 Parameters: fast={lookback_fast}, slow={lookback_slow}, threshold={momentum_threshold:.1%}")
        print(f"💰 Initial Capital: ${initial_capital:,.2f}")
        print(f"⚖️  Risk Management: stop={stop_loss:.1%}, profit={take_profit:.1%}")
    
    def get_market_data(self, days: int = 200) -> Optional[pd.DataFrame]:
        """
        Get EURUSD market data with forex-optimized timeframe
        
        Args:
            days: Number of days of historical data
            
        Returns:
            DataFrame with OHLCV data or None
        """
        try:
            end_date = datetime.now()
            start_date = end_date - timedelta(days=days)
            
            print(f"📡 Fetching EURUSD data: {start_date.date()} to {end_date.date()}")
            ticker = yf.Ticker(self.symbol)
            data = ticker.history(start=start_date, end=end_date, interval='1d')
            
            if data is not None and len(data) > 50:
                print(f"✅ Market data: {len(data)} records from {data.index[0].date()} to {data.index[-1].date()}")
                return data
            else:
                print("❌ Insufficient market data")
                return None
                
        except Exception as e:
            print(f"❌ Market data fetch failed: {e}")
            return None
    
    def calculate_forex_signals(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate forex-optimized momentum signals for EURUSD
        
        Args:
            data: OHLCV DataFrame
            
        Returns:
            DataFrame with forex momentum indicators
        """
        df = data.copy()
        
        # Forex-specific price changes
        df['price_change_fast'] = df['Close'].pct_change(self.lookback_fast)
        df['price_change_slow'] = df['Close'].pct_change(self.lookback_slow)
        
        # Exponential moving averages (better for forex)
        df['ema_fast'] = df['Close'].ewm(span=self.lookback_fast).mean()
        df['ema_slow'] = df['Close'].ewm(span=self.lookback_slow).mean()
        
        # Trend strength
        df['trend_strength'] = (df['ema_fast'] - df['ema_slow']) / df['ema_slow']
        
        # Forex volatility (using True Range)
        df['high_low'] = df['High'] - df['Low']
        df['high_close'] = np.abs(df['High'] - df['Close'].shift(1))
        df['low_close'] = np.abs(df['Low'] - df['Close'].shift(1))
        df['true_range'] = np.maximum(df['high_low'], np.maximum(df['high_close'], df['low_close']))
        df['atr'] = df['true_range'].rolling(window=self.volatility_lookback).mean()
        
        # Volatility-adjusted momentum
        df['vol_adj_momentum'] = df['price_change_fast'] / (df['atr'] / df['Close'] + 1e-8)
        
        # Forex RSI (shorter period for forex)
        delta = df['Close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=9).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=9).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # MACD for forex
        ema_12 = df['Close'].ewm(span=12).mean()
        ema_26 = df['Close'].ewm(span=26).mean()
        df['macd'] = ema_12 - ema_26
        df['macd_signal'] = df['macd'].ewm(span=9).mean()
        df['macd_histogram'] = df['macd'] - df['macd_signal']
        
        # Forex-specific buy signals (multiple confirmations)
        buy_conditions = [
            df['price_change_fast'] > self.momentum_threshold,    # Fast momentum
            df['trend_strength'] > 0.002,                        # Uptrend strength
            df['ema_fast'] > df['ema_slow'],                      # Fast EMA above slow
            df['vol_adj_momentum'] > 0.3,                        # Vol-adjusted momentum
            df['rsi'].between(40, 75),                           # RSI in range
            df['macd_histogram'] > 0,                            # MACD bullish
        ]
        
        # Require at least 4 out of 6 conditions
        df['buy_score'] = np.sum(buy_conditions, axis=0)
        df['buy_signal'] = (df['buy_score'] >= 4).astype(int)
        
        # Forex-specific sell signals
        sell_conditions = [
            df['price_change_fast'] < -self.momentum_threshold,   # Fast downward momentum
            df['trend_strength'] < -0.002,                       # Downtrend strength
            df['ema_fast'] < df['ema_slow'],                      # Fast EMA below slow
            df['vol_adj_momentum'] < -0.3,                       # Negative vol momentum
            df['rsi'] < 25,                                       # RSI oversold
            df['macd_histogram'] < 0,                            # MACD bearish
        ]
        
        # Require at least 3 out of 6 conditions for sell
        df['sell_score'] = np.sum(sell_conditions, axis=0)
        df['sell_signal'] = (df['sell_score'] >= 3).astype(int)
        
        # Signal confirmation (reduce noise)
        df['buy_confirmed'] = (df['buy_signal'].rolling(window=self.confirmation_period).sum() >= 2).astype(int)
        df['sell_confirmed'] = (df['sell_signal'].rolling(window=self.confirmation_period).sum() >= 2).astype(int)
        
        return df
    
    def calculate_dynamic_kelly(self, returns: List[float], current_volatility: float) -> float:
        """
        Calculate dynamic Kelly fraction adjusted for current market volatility
        
        Args:
            returns: List of historical returns
            current_volatility: Current market volatility
            
        Returns:
            Dynamic Kelly fraction
        """
        if len(returns) < 10:
            return 0.08  # Conservative default for forex
        
        returns_array = np.array(returns)
        
        # Calculate recent performance
        recent_returns = returns_array[-20:] if len(returns_array) >= 20 else returns_array
        
        wins = recent_returns[recent_returns > 0]
        losses = recent_returns[recent_returns < 0]
        
        if len(wins) == 0 or len(losses) == 0:
            return 0.05
        
        win_rate = len(wins) / len(recent_returns)
        avg_win = np.mean(wins)
        avg_loss = np.abs(np.mean(losses))
        
        # Basic Kelly calculation
        if avg_loss > 0:
            b = avg_win / avg_loss
            kelly_fraction = (b * win_rate - (1 - win_rate)) / b
        else:
            kelly_fraction = 0.05
        
        # Volatility adjustment (reduce size in high volatility)
        volatility_factor = min(1.0, 0.01 / (current_volatility + 1e-8))
        adjusted_kelly = kelly_fraction * volatility_factor
        
        # Apply forex-specific constraints
        adjusted_kelly = max(0.03, min(adjusted_kelly, self.max_position_size))
        
        return adjusted_kelly
    
    def execute_forex_trade(self, signal: str, price: float, date: str, 
                           position_size: float = 0.1) -> Dict:
        """
        Execute forex trade with enhanced position management
        
        Args:
            signal: 'buy', 'sell', or 'hold'
            price: Current price
            date: Date of trade
            position_size: Position sizing fraction
            
        Returns:
            Trade execution details
        """
        trade_info = {
            'date': date,
            'signal': signal,
            'price': price,
            'position_before': self.position,
            'portfolio_before': self.portfolio_value,
            'executed': False
        }
        
        if signal == 'buy' and self.position <= 0:
            # Close any short position
            if self.position < 0:
                pnl = -self.position * (self.entry_price - price)
                self.portfolio_value += pnl
                self.trades.append({
                    'type': 'close_short',
                    'date': date,
                    'price': price,
                    'position': -self.position,
                    'pnl': pnl,
                    'portfolio_value': self.portfolio_value
                })
            
            # Open long position
            position_value = self.portfolio_value * position_size
            new_position = position_value / price
            
            self.position = new_position
            self.entry_price = price
            trade_info['executed'] = True
            trade_info['new_position'] = new_position
            
            self.trades.append({
                'type': 'buy',
                'date': date,
                'price': price,
                'position': new_position,
                'portfolio_value': self.portfolio_value
            })
            
        elif signal == 'sell' and self.position > 0:
            # Close long position only (no short selling for now)
            pnl = self.position * (price - self.entry_price)
            self.portfolio_value += pnl
            
            self.trades.append({
                'type': 'close_long',
                'date': date,
                'price': price,
                'position': self.position,
                'pnl': pnl,
                'portfolio_value': self.portfolio_value
            })
            
            self.position = 0.0
            trade_info['executed'] = True
            trade_info['new_position'] = 0.0
        
        trade_info['position_after'] = self.position
        trade_info['portfolio_after'] = self.portfolio_value
        
        return trade_info
    
    def check_forex_risk_management(self, current_price: float, date: str) -> bool:
        """
        Check forex-specific risk management rules
        
        Args:
            current_price: Current market price
            date: Current date
            
        Returns:
            True if position was closed, False otherwise
        """
        if self.position == 0:
            return False
        
        price_change = (current_price - self.entry_price) / self.entry_price
        
        # For long positions only (no short positions in V2)
        if self.position > 0:
            if price_change <= -self.stop_loss:  # Stop loss
                pnl = self.position * (current_price - self.entry_price)
                self.portfolio_value += pnl
                self.trades.append({
                    'type': 'stop_loss',
                    'date': date,
                    'price': current_price,
                    'position': self.position,
                    'pnl': pnl,
                    'portfolio_value': self.portfolio_value
                })
                self.position = 0
                return True
            elif price_change >= self.take_profit:  # Take profit
                pnl = self.position * (current_price - self.entry_price)
                self.portfolio_value += pnl
                self.trades.append({
                    'type': 'take_profit',
                    'date': date,
                    'price': current_price,
                    'position': self.position,
                    'pnl': pnl,
                    'portfolio_value': self.portfolio_value
                })
                self.position = 0
                return True
        
        return False
    
    def run_enhanced_backtest(self, start_date: str = "2025-03-01", 
                             end_date: str = "2025-09-16") -> Dict:
        """
        Run enhanced backtest for EURUSD with forex optimizations
        
        Args:
            start_date: Start date for backtest
            end_date: End date for backtest
            
        Returns:
            Comprehensive backtest results
        """
        print(f"\n🔄 EURUSD Enhanced Backtest: {start_date} to {end_date}")
        print("=" * 60)
        
        # Get market data
        data = self.get_market_data(days=220)
        if data is None:
            print("❌ Failed to fetch market data")
            return {'error': 'No data available'}
        
        # Filter data for backtest period
        try:
            data.index = pd.to_datetime(data.index)
            mask = (data.index >= start_date) & (data.index <= end_date)
            data = data[mask]
            
            if len(data) < 30:
                print(f"❌ Insufficient data for backtest: {len(data)} days")
                return {'error': 'Insufficient data'}
            
            print(f"📊 Backtest data: {len(data)} days from {data.index[0].date()} to {data.index[-1].date()}")
            
        except Exception as e:
            print(f"❌ Date filtering error: {e}")
            return {'error': f'Date filtering failed: {e}'}
        
        # Calculate enhanced signals
        data_with_signals = self.calculate_forex_signals(data)
        
        # Reset strategy state
        self.portfolio_value = self.initial_capital
        self.position = 0.0
        self.entry_price = 0.0
        self.trades = []
        self.daily_returns = []
        
        portfolio_history = []
        recent_returns = []
        
        print(f"🎯 Processing {len(data_with_signals)} trading days...")
        
        # Enhanced simulation loop
        for i, (date, row) in enumerate(data_with_signals.iterrows()):
            current_price = row['Close']
            current_atr = row.get('atr', 0.01)
            
            # Check risk management first
            self.check_forex_risk_management(current_price, date.strftime('%Y-%m-%d'))
            
            # Calculate current portfolio value
            if self.position != 0:
                unrealized_pnl = self.position * (current_price - self.entry_price)
                current_portfolio_value = self.portfolio_value + unrealized_pnl
            else:
                current_portfolio_value = self.portfolio_value
            
            # Calculate daily returns
            if i > 0:
                daily_return = (current_portfolio_value - portfolio_history[-1]) / portfolio_history[-1]
                self.daily_returns.append(daily_return)
                recent_returns.append(daily_return)
                
                # Keep only recent returns for Kelly calculation
                if len(recent_returns) > 40:
                    recent_returns.pop(0)
            
            portfolio_history.append(current_portfolio_value)
            
            # Generate trading signals with confirmations
            if not np.isnan(row.get('buy_confirmed', np.nan)) and not np.isnan(row.get('sell_confirmed', np.nan)):
                current_volatility = current_atr / current_price
                kelly_fraction = self.calculate_dynamic_kelly(recent_returns, current_volatility)
                
                if row['buy_confirmed'] == 1 and self.position <= 0:
                    self.execute_forex_trade('buy', current_price, date.strftime('%Y-%m-%d'), kelly_fraction)
                elif row['sell_confirmed'] == 1 and self.position > 0:
                    self.execute_forex_trade('sell', current_price, date.strftime('%Y-%m-%d'), kelly_fraction)
        
        # Final portfolio value
        final_price = data_with_signals['Close'].iloc[-1]
        if self.position != 0:
            final_pnl = self.position * (final_price - self.entry_price)
            self.portfolio_value += final_pnl
        
        # Calculate enhanced performance metrics
        total_return = (self.portfolio_value - self.initial_capital) / self.initial_capital
        total_days = len(data_with_signals)
        annualized_return = (1 + total_return) ** (365 / total_days) - 1
        
        # Enhanced risk metrics
        daily_returns_array = np.array(self.daily_returns)
        volatility = np.std(daily_returns_array) * np.sqrt(252) if len(daily_returns_array) > 0 else 0
        sharpe_ratio = annualized_return / volatility if volatility > 0 else 0
        
        # Drawdown analysis
        portfolio_series = pd.Series(portfolio_history)
        rolling_max = portfolio_series.expanding().max()
        drawdown = (portfolio_series - rolling_max) / rolling_max
        max_drawdown = drawdown.min()
        
        # Enhanced trade analysis
        trades_with_pnl = [t for t in self.trades if 'pnl' in t]
        winning_trades = [t for t in trades_with_pnl if t['pnl'] > 0]
        losing_trades = [t for t in trades_with_pnl if t['pnl'] < 0]
        
        win_rate = len(winning_trades) / len(trades_with_pnl) if trades_with_pnl else 0
        avg_win = np.mean([t['pnl'] for t in winning_trades]) if winning_trades else 0
        avg_loss = np.mean([t['pnl'] for t in losing_trades]) if losing_trades else 0
        
        # Compile enhanced results
        results = {
            'success': True,
            'period': f"{start_date} to {end_date}",
            'total_days': total_days,
            'initial_capital': self.initial_capital,
            'final_value': self.portfolio_value,
            'total_return': total_return,
            'annualized_return': annualized_return,
            'total_trades': len(self.trades),
            'pnl_trades': len(trades_with_pnl),
            'winning_trades': len(winning_trades),
            'losing_trades': len(losing_trades),
            'win_rate': win_rate,
            'avg_win': avg_win,
            'avg_loss': avg_loss,
            'profit_factor': abs(avg_win / avg_loss) if avg_loss != 0 else float('inf'),
            'sharpe_ratio': sharpe_ratio,
            'volatility': volatility,
            'max_drawdown': max_drawdown,
            'portfolio_history': portfolio_history,
            'trades': self.trades,
            'daily_returns': self.daily_returns
        }
        
        # Enhanced results display
        print(f"\n🎉 EURUSD Enhanced Backtest Results")
        print("=" * 50)
        print(f"📅 Period: {start_date} to {end_date} ({total_days} days)")
        print(f"💰 Initial Capital: ${self.initial_capital:,.2f}")
        print(f"💰 Final Value: ${self.portfolio_value:,.2f}")
        print(f"📈 Total Return: {total_return:.2%}")
        print(f"📈 Annualized Return: {annualized_return:.2%}")
        print(f"🎯 Total Trades: {len(self.trades)} ({len(trades_with_pnl)} with P&L)")
        print(f"✅ Win Rate: {win_rate:.1%}")
        print(f"📊 Sharpe Ratio: {sharpe_ratio:.2f}")
        print(f"📉 Max Drawdown: {max_drawdown:.2%}")
        print(f"🔄 Volatility: {volatility:.2%}")
        
        if winning_trades and losing_trades:
            print(f"💵 Average Win: ${avg_win:.2f}")
            print(f"💸 Average Loss: ${avg_loss:.2f}")
            print(f"⚖️  Profit Factor: {results['profit_factor']:.2f}")
        
        return results

def main():
    """Run enhanced EURUSD momentum strategy backtest"""
    print("🚀 EURUSD Enhanced Momentum Strategy - 6 Month Validation")
    print("=======================================================")
    
    # Initialize enhanced strategy
    strategy = EURUSDMomentumV2(
        initial_capital=10000.0,
        lookback_fast=5,              # Faster signals for forex
        lookback_slow=20,             # Trend confirmation
        momentum_threshold=0.005,     # Lower threshold for forex
        stop_loss=0.015,              # Tight stop loss
        take_profit=0.025,            # Reasonable take profit
        max_position_size=0.15        # Conservative position sizing
    )
    
    # Run enhanced 6-month backtest
    results = strategy.run_enhanced_backtest(
        start_date="2025-03-01",
        end_date="2025-09-16"
    )
    
    if results.get('success'):
        print(f"\n🏆 EURUSD Enhanced Strategy Performance:")
        print(f"   Return: {results['annualized_return']:.1%} annually")
        print(f"   Sharpe: {results['sharpe_ratio']:.2f}")
        print(f"   Trades: {results['pnl_trades']} P&L trades ({results['win_rate']:.1%} win rate)")
        print(f"   Max DD: {results['max_drawdown']:.1%}")
        
        # Enhanced benchmark comparison
        try:
            ticker = yf.Ticker("EURUSD=X")
            benchmark_data = ticker.history(start="2025-03-01", end="2025-09-16")
            if len(benchmark_data) > 0:
                benchmark_return = (benchmark_data['Close'].iloc[-1] - benchmark_data['Close'].iloc[0]) / benchmark_data['Close'].iloc[0]
                excess_return = results['total_return'] - benchmark_return
                
                print(f"\n📊 vs Buy & Hold EURUSD:")
                print(f"   Strategy: {results['total_return']:.2%}")
                print(f"   Buy&Hold: {benchmark_return:.2%}")
                print(f"   Excess: {excess_return:.2%}")
        except:
            print(f"\n📊 Strategy Return: {results['total_return']:.2%}")
        
        # Enhanced risk assessment
        if results['sharpe_ratio'] > 0.8:
            print(f"✅ EXCELLENT: Sharpe ratio {results['sharpe_ratio']:.2f} shows strong risk-adjusted returns")
        elif results['sharpe_ratio'] > 0.4:
            print(f"✅ GOOD: Sharpe ratio {results['sharpe_ratio']:.2f} indicates decent performance")
        elif results['sharpe_ratio'] > 0:
            print(f"⚠️  FAIR: Sharpe ratio {results['sharpe_ratio']:.2f} suggests modest performance")
        else:
            print(f"⚠️  POOR: Negative Sharpe ratio {results['sharpe_ratio']:.2f} indicates strategy needs work")
            
    else:
        print(f"❌ Enhanced backtest failed: {results.get('error', 'Unknown error')}")

if __name__ == "__main__":
    main()