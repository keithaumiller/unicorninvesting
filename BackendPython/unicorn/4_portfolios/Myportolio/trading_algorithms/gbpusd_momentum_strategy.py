#!/usr/bin/env python3
"""
GBPUSD Momentum Strategy - Forex Validation
==========================================

Enhanced momentum trading strategy for GBPUSD with British Pound specific optimizations.
Testing the systematic approach on the third major forex pair following EURUSD and USDJPY.

Key Features:
- British Pound volatility considerations
- Brexit and UK economic impact awareness
- Currency pair specific momentum patterns
- Enhanced risk management for GBP volatility
- Comprehensive validation framework

Expected Performance Range:
- Target Annual Return: 10-18%
- Expected Sharpe Ratio: 0.7-1.3
- Maximum Drawdown: <10%
- Win Rate: 50-65%

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

class GBPUSDMomentumStrategy:
    """GBPUSD Momentum Strategy with British Pound specific optimizations"""
    
    def __init__(self, 
                 initial_capital: float = 10000.0,
                 lookback_fast: int = 6,           # Faster for GBP volatility
                 lookback_slow: int = 18,          # Shorter for GBP responsiveness
                 momentum_threshold: float = 0.007, # Higher threshold for GBP
                 stop_loss: float = 0.020,         # Wider stops for GBP volatility
                 take_profit: float = 0.035,       # Higher profits for GBP moves
                 max_position_size: float = 0.18): # Conservative for volatile GBP
        """
        Initialize GBPUSD momentum strategy with currency-specific parameters
        
        Args:
            initial_capital: Starting capital for simulation
            lookback_fast: Fast momentum lookback period (6 for GBP)
            lookback_slow: Slow momentum lookback period (18 for GBP)
            momentum_threshold: Minimum momentum signal strength (0.7% for GBP)
            stop_loss: Stop loss as fraction of price (2.0% for GBP)
            take_profit: Take profit as fraction of price (3.5% for GBP)
            max_position_size: Maximum position size (18% for volatile pair)
        """
        self.symbol = "GBPUSD=X"
        self.asset_name = "GBPUSD"
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
        
        # GBP-specific parameters
        self.confirmation_period = 2  # Quick confirmation for volatile GBP
        self.volatility_lookback = 12 # Shorter for responsive adjustment
        self.rsi_period = 10          # Faster RSI for GBP
        self.volume_factor = True     # Consider volume for GBP
        
        # Setup logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
        
        print(f"🚀 GBPUSD Momentum Strategy Initialized")
        print(f"📊 Parameters: fast={lookback_fast}, slow={lookback_slow}, threshold={momentum_threshold:.1%}")
        print(f"💰 Initial Capital: ${initial_capital:,.2f}")
        print(f"⚖️  Risk Management: stop={stop_loss:.1%}, profit={take_profit:.1%}")
        print(f"🇬🇧 Currency Pair: {self.asset_name} (GBP volatility optimized)")
    
    def get_gbpusd_data(self, days: int = 180) -> Optional[pd.DataFrame]:
        """
        Get GBPUSD market data with optimized lookback for trend analysis
        
        Args:
            days: Number of days of historical data
            
        Returns:
            DataFrame with OHLCV data or None
        """
        try:
            end_date = datetime.now()
            start_date = end_date - timedelta(days=days)
            
            print(f"📡 Fetching GBPUSD data: {start_date.date()} to {end_date.date()}")
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
    
    def calculate_gbp_signals(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate GBPUSD-specific momentum signals with British Pound considerations
        
        Args:
            data: OHLCV DataFrame
            
        Returns:
            DataFrame with GBPUSD momentum indicators
        """
        df = data.copy()
        
        # GBP-specific price changes
        df['price_change_fast'] = df['Close'].pct_change(self.lookback_fast)
        df['price_change_slow'] = df['Close'].pct_change(self.lookback_slow)
        
        # Adaptive Moving Averages for GBP volatility
        def adaptive_ma(series, period):
            # Kaufman's Adaptive Moving Average
            change = np.abs(series.diff(period))
            volatility = np.abs(series.diff()).rolling(window=period).sum()
            efficiency_ratio = change / volatility
            # Smooth constant for fast and slow EMA
            fast_sc = 2 / (2 + 1)
            slow_sc = 2 / (30 + 1)
            sc = (efficiency_ratio * (fast_sc - slow_sc) + slow_sc) ** 2
            
            ama = np.zeros_like(series)
            ama[period-1] = series.iloc[period-1]
            
            for i in range(period, len(series)):
                ama[i] = ama[i-1] + sc.iloc[i] * (series.iloc[i] - ama[i-1])
            
            return pd.Series(ama, index=series.index)
        
        df['ama_fast'] = adaptive_ma(df['Close'], self.lookback_fast)
        df['ama_slow'] = adaptive_ma(df['Close'], self.lookback_slow)
        
        # GBP trend strength
        df['trend_strength'] = (df['ama_fast'] - df['ama_slow']) / df['ama_slow']
        
        # Volatility indicators for GBP
        df['high_low'] = df['High'] - df['Low']
        df['high_close'] = np.abs(df['High'] - df['Close'].shift(1))
        df['low_close'] = np.abs(df['Low'] - df['Close'].shift(1))
        df['true_range'] = np.maximum(df['high_low'], np.maximum(df['high_close'], df['low_close']))
        df['atr'] = df['true_range'].rolling(window=self.volatility_lookback).mean()
        
        # Volatility-adjusted momentum
        df['vol_adj_momentum'] = df['price_change_fast'] / (df['atr'] / df['Close'] + 1e-8)
        
        # GBP-optimized RSI (faster period)
        delta = df['Close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=self.rsi_period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=self.rsi_period).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # Williams %R for GBP momentum
        high_14 = df['High'].rolling(window=14).max()
        low_14 = df['Low'].rolling(window=14).min()
        df['williams_r'] = -100 * (high_14 - df['Close']) / (high_14 - low_14)
        
        # Commodity Channel Index for GBP
        typical_price = (df['High'] + df['Low'] + df['Close']) / 3
        cci_period = 14
        tp_sma = typical_price.rolling(window=cci_period).mean()
        mean_deviation = typical_price.rolling(window=cci_period).apply(lambda x: np.mean(np.abs(x - x.mean())))
        df['cci'] = (typical_price - tp_sma) / (0.015 * mean_deviation)
        
        # Volume-adjusted momentum (if volume data available)
        if 'Volume' in df.columns and df['Volume'].sum() > 0:
            df['volume_ma'] = df['Volume'].rolling(window=10).mean()
            df['volume_ratio'] = df['Volume'] / df['volume_ma']
            df['volume_momentum'] = df['price_change_fast'] * np.log1p(df['volume_ratio'])
        else:
            df['volume_momentum'] = df['price_change_fast']
        
        # Bollinger Bands for GBP
        bb_period = 16  # Shorter for GBP responsiveness
        df['bb_middle'] = df['Close'].rolling(window=bb_period).mean()
        bb_std = df['Close'].rolling(window=bb_period).std()
        df['bb_upper'] = df['bb_middle'] + (bb_std * 2.1)  # Slightly wider for GBP
        df['bb_lower'] = df['bb_middle'] - (bb_std * 2.1)
        df['bb_position'] = (df['Close'] - df['bb_lower']) / (df['bb_upper'] - df['bb_lower'])
        df['bb_width'] = (df['bb_upper'] - df['bb_lower']) / df['bb_middle']
        
        # GBPUSD-specific buy signals
        buy_conditions = [
            df['price_change_fast'] > self.momentum_threshold,     # Fast momentum
            df['trend_strength'] > 0.004,                         # Strong trend
            df['ama_fast'] > df['ama_slow'],                       # AMA bullish
            df['vol_adj_momentum'] > 0.5,                         # Vol-adjusted momentum
            df['rsi'].between(35, 75),                            # RSI in range
            df['williams_r'] > -75,                               # Williams %R bullish
            df['cci'] > -100,                                     # CCI bullish
            df['volume_momentum'] > 0,                            # Volume-adjusted positive
            df['bb_position'] > 0.25,                             # Above BB lower
            df['bb_width'] > 0.02,                                # Sufficient volatility
        ]
        
        # Require 6 out of 10 conditions for GBP
        df['buy_score'] = np.sum(buy_conditions, axis=0)
        df['buy_signal'] = (df['buy_score'] >= 6).astype(int)
        
        # GBPUSD-specific sell signals
        sell_conditions = [
            df['price_change_fast'] < -self.momentum_threshold,    # Fast downward momentum
            df['trend_strength'] < -0.004,                        # Downtrend strength
            df['ama_fast'] < df['ama_slow'],                       # AMA bearish
            df['vol_adj_momentum'] < -0.5,                        # Negative vol momentum
            df['rsi'] < 25,                                        # RSI oversold
            df['williams_r'] < -80,                               # Williams %R bearish
            df['cci'] < -150,                                     # CCI bearish
            df['bb_position'] < 0.15,                             # Near BB lower
        ]
        
        # Require 5 out of 8 conditions for sell
        df['sell_score'] = np.sum(sell_conditions, axis=0)
        df['sell_signal'] = (df['sell_score'] >= 5).astype(int)
        
        # Signal confirmation
        df['buy_confirmed'] = (df['buy_signal'].rolling(window=self.confirmation_period).sum() >= 1).astype(int)
        df['sell_confirmed'] = (df['sell_signal'].rolling(window=self.confirmation_period).sum() >= 1).astype(int)
        
        return df
    
    def calculate_gbp_kelly(self, returns: List[float], current_volatility: float) -> float:
        """
        Calculate Kelly fraction adjusted for GBP characteristics
        
        Args:
            returns: List of historical returns
            current_volatility: Current market volatility
            
        Returns:
            GBP-optimized Kelly fraction
        """
        if len(returns) < 6:
            return 0.08  # Conservative default for volatile GBP
        
        returns_array = np.array(returns)
        
        # Focus on very recent performance for volatile GBP
        recent_returns = returns_array[-10:] if len(returns_array) >= 10 else returns_array
        
        wins = recent_returns[recent_returns > 0]
        losses = recent_returns[recent_returns < 0]
        
        if len(wins) == 0 or len(losses) == 0:
            return 0.06
        
        win_rate = len(wins) / len(recent_returns)
        avg_win = np.mean(wins)
        avg_loss = np.abs(np.mean(losses))
        
        # Kelly calculation
        if avg_loss > 0:
            b = avg_win / avg_loss
            kelly_fraction = (b * win_rate - (1 - win_rate)) / b
        else:
            kelly_fraction = 0.06
        
        # GBP volatility adjustment (more conservative)
        volatility_factor = min(1.0, 0.012 / (current_volatility + 1e-8))
        adjusted_kelly = kelly_fraction * volatility_factor
        
        # GBP-specific constraints (more conservative due to volatility)
        adjusted_kelly = max(0.04, min(adjusted_kelly, self.max_position_size))
        
        return adjusted_kelly
    
    def execute_gbp_trade(self, signal: str, price: float, date: str, 
                         position_size: float = 0.12) -> Dict:
        """
        Execute GBP trade with currency-specific position management
        
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
            # Close long position
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
    
    def check_gbp_risk_management(self, current_price: float, date: str) -> bool:
        """
        Check GBP-specific risk management rules
        
        Args:
            current_price: Current market price
            date: Current date
            
        Returns:
            True if position was closed, False otherwise
        """
        if self.position == 0:
            return False
        
        price_change = (current_price - self.entry_price) / self.entry_price
        
        # For long positions
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
    
    def run_gbpusd_backtest(self, start_date: str = "2025-03-01", 
                           end_date: str = "2025-09-16") -> Dict:
        """
        Run comprehensive GBPUSD backtest with currency-specific optimizations
        
        Args:
            start_date: Start date for backtest
            end_date: End date for backtest
            
        Returns:
            Comprehensive backtest results
        """
        print(f"\n🔄 GBPUSD Momentum Backtest: {start_date} to {end_date}")
        print("=" * 60)
        
        # Get market data
        data = self.get_gbpusd_data(days=220)
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
        
        # Calculate GBP-specific signals
        data_with_signals = self.calculate_gbp_signals(data)
        
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
            current_atr = row.get('atr', 0.008)  # GBP typical volatility
            
            # Check risk management first
            self.check_gbp_risk_management(current_price, date.strftime('%Y-%m-%d'))
            
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
                
                # Keep recent returns for Kelly calculation
                if len(recent_returns) > 25:
                    recent_returns.pop(0)
            
            portfolio_history.append(current_portfolio_value)
            
            # Generate trading signals
            if not np.isnan(row.get('buy_confirmed', np.nan)) and not np.isnan(row.get('sell_confirmed', np.nan)):
                current_volatility = current_atr / current_price
                kelly_fraction = self.calculate_gbp_kelly(recent_returns, current_volatility)
                
                if row['buy_confirmed'] == 1 and self.position <= 0:
                    self.execute_gbp_trade('buy', current_price, date.strftime('%Y-%m-%d'), kelly_fraction)
                elif row['sell_confirmed'] == 1 and self.position > 0:
                    self.execute_gbp_trade('sell', current_price, date.strftime('%Y-%m-%d'), kelly_fraction)
        
        # Final portfolio value
        final_price = data_with_signals['Close'].iloc[-1]
        if self.position != 0:
            final_pnl = self.position * (final_price - self.entry_price)
            self.portfolio_value += final_pnl
        
        # Calculate performance metrics
        total_return = (self.portfolio_value - self.initial_capital) / self.initial_capital
        total_days = len(data_with_signals)
        annualized_return = (1 + total_return) ** (365 / total_days) - 1
        
        # Risk metrics
        daily_returns_array = np.array(self.daily_returns)
        volatility = np.std(daily_returns_array) * np.sqrt(252) if len(daily_returns_array) > 0 else 0
        sharpe_ratio = annualized_return / volatility if volatility > 0 else 0
        
        # Drawdown analysis
        portfolio_series = pd.Series(portfolio_history)
        rolling_max = portfolio_series.expanding().max()
        drawdown = (portfolio_series - rolling_max) / rolling_max
        max_drawdown = drawdown.min()
        
        # Trade analysis
        trades_with_pnl = [t for t in self.trades if 'pnl' in t]
        winning_trades = [t for t in trades_with_pnl if t['pnl'] > 0]
        losing_trades = [t for t in trades_with_pnl if t['pnl'] < 0]
        
        win_rate = len(winning_trades) / len(trades_with_pnl) if trades_with_pnl else 0
        avg_win = np.mean([t['pnl'] for t in winning_trades]) if winning_trades else 0
        avg_loss = np.mean([t['pnl'] for t in losing_trades]) if losing_trades else 0
        
        # Compile results
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
        
        # Results display
        print(f"\n🎉 GBPUSD Momentum Backtest Results")
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
    """Run GBPUSD momentum strategy backtest"""
    print("🚀 GBPUSD Momentum Strategy - 6 Month Validation")
    print("================================================")
    
    # Initialize GBP-optimized strategy
    strategy = GBPUSDMomentumStrategy(
        initial_capital=10000.0,
        lookback_fast=6,              # Fast for GBP volatility
        lookback_slow=18,             # Responsive trend confirmation
        momentum_threshold=0.007,     # Higher threshold for GBP
        stop_loss=0.020,              # Wider stops for GBP volatility
        take_profit=0.035,            # Higher profit targets
        max_position_size=0.15        # Conservative for volatile pair
    )
    
    # Run comprehensive 6-month backtest
    results = strategy.run_gbpusd_backtest(
        start_date="2025-03-01",
        end_date="2025-09-16"
    )
    
    if results.get('success'):
        print(f"\n🏆 GBPUSD Strategy Performance:")
        print(f"   Return: {results['annualized_return']:.1%} annually")
        print(f"   Sharpe: {results['sharpe_ratio']:.2f}")
        print(f"   Trades: {results['pnl_trades']} P&L trades ({results['win_rate']:.1%} win rate)")
        print(f"   Max DD: {results['max_drawdown']:.1%}")
        
        # Benchmark comparison
        try:
            ticker = yf.Ticker("GBPUSD=X")
            benchmark_data = ticker.history(start="2025-03-01", end="2025-09-16")
            if len(benchmark_data) > 0:
                benchmark_return = (benchmark_data['Close'].iloc[-1] - benchmark_data['Close'].iloc[0]) / benchmark_data['Close'].iloc[0]
                excess_return = results['total_return'] - benchmark_return
                
                print(f"\n📊 vs Buy & Hold GBPUSD:")
                print(f"   Strategy: {results['total_return']:.2%}")
                print(f"   Buy&Hold: {benchmark_return:.2%}")
                print(f"   Excess: {excess_return:.2%}")
        except:
            print(f"\n📊 Strategy Return: {results['total_return']:.2%}")
        
        # Risk assessment
        if results['sharpe_ratio'] > 1.0:
            print(f"✅ EXCELLENT: Sharpe ratio {results['sharpe_ratio']:.2f} shows strong risk-adjusted returns")
        elif results['sharpe_ratio'] > 0.6:
            print(f"✅ GOOD: Sharpe ratio {results['sharpe_ratio']:.2f} indicates solid performance")
        elif results['sharpe_ratio'] > 0.2:
            print(f"⚠️  FAIR: Sharpe ratio {results['sharpe_ratio']:.2f} suggests room for improvement")
        else:
            print(f"⚠️  POOR: Sharpe ratio {results['sharpe_ratio']:.2f} indicates strategy needs optimization")
            
    else:
        print(f"❌ Backtest failed: {results.get('error', 'Unknown error')}")

if __name__ == "__main__":
    main()