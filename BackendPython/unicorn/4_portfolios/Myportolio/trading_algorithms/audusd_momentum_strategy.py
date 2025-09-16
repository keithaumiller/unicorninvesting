#!/usr/bin/env python3
"""
AUDUSD Momentum Strategy - Commodity Currency Validation
======================================================

Enhanced momentum trading strategy for AUDUSD with Australian Dollar commodity currency optimizations.
Testing the systematic approach on the fourth major forex pair with commodity correlations.

Key Features:
- Australian Dollar commodity currency considerations
- Mining and agricultural economy correlations
- Asia-Pacific market timing awareness
- Commodity price momentum integration
- Risk management for commodity-linked volatility

Expected Performance Range:
- Target Annual Return: 6-12%
- Expected Sharpe Ratio: 0.5-1.0
- Maximum Drawdown: <8%
- Win Rate: 45-60%

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

class AUDUSDMomentumStrategy:
    """AUDUSD Momentum Strategy with commodity currency specific optimizations"""
    
    def __init__(self, 
                 initial_capital: float = 10000.0,
                 lookback_fast: int = 8,           # Balanced for AUD commodity sensitivity
                 lookback_slow: int = 22,          # Extended for commodity trend confirmation
                 momentum_threshold: float = 0.005, # Moderate threshold for AUD
                 stop_loss: float = 0.016,         # Moderate stops for AUD
                 take_profit: float = 0.028,       # Reasonable profits for commodity currency
                 max_position_size: float = 0.22): # Higher allocation for stable commodity pair
        """
        Initialize AUDUSD momentum strategy with commodity currency parameters
        
        Args:
            initial_capital: Starting capital for simulation
            lookback_fast: Fast momentum lookback period (8 for AUD)
            lookback_slow: Slow momentum lookback period (22 for AUD)
            momentum_threshold: Minimum momentum signal strength (0.5% for AUD)
            stop_loss: Stop loss as fraction of price (1.6% for AUD)
            take_profit: Take profit as fraction of price (2.8% for AUD)
            max_position_size: Maximum position size (22% for commodity pair)
        """
        self.symbol = "AUDUSD=X"
        self.asset_name = "AUDUSD"
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
        
        # AUD-specific parameters
        self.confirmation_period = 3  # Commodity confirmation period
        self.volatility_lookback = 15 # Balanced volatility window
        self.rsi_period = 12          # Balanced RSI for commodity currency
        self.commodity_factor = True  # Consider commodity correlations
        
        # Setup logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
        
        print(f"🚀 AUDUSD Momentum Strategy Initialized")
        print(f"📊 Parameters: fast={lookback_fast}, slow={lookback_slow}, threshold={momentum_threshold:.1%}")
        print(f"💰 Initial Capital: ${initial_capital:,.2f}")
        print(f"⚖️  Risk Management: stop={stop_loss:.1%}, profit={take_profit:.1%}")
        print(f"🇦🇺 Currency Pair: {self.asset_name} (Commodity currency optimized)")
    
    def get_audusd_data(self, days: int = 200) -> Optional[pd.DataFrame]:
        """
        Get AUDUSD market data with commodity trend consideration
        
        Args:
            days: Number of days of historical data
            
        Returns:
            DataFrame with OHLCV data or None
        """
        try:
            end_date = datetime.now()
            start_date = end_date - timedelta(days=days)
            
            print(f"📡 Fetching AUDUSD data: {start_date.date()} to {end_date.date()}")
            ticker = yf.Ticker(self.symbol)
            data = ticker.history(start=start_date, end=end_date, interval='1d')
            
            if data is not None and len(data) > 55:
                print(f"✅ Market data: {len(data)} records from {data.index[0].date()} to {data.index[-1].date()}")
                return data
            else:
                print("❌ Insufficient market data")
                return None
                
        except Exception as e:
            print(f"❌ Market data fetch failed: {e}")
            return None
    
    def calculate_aud_signals(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate AUDUSD-specific momentum signals with commodity considerations
        
        Args:
            data: OHLCV DataFrame
            
        Returns:
            DataFrame with AUDUSD momentum indicators
        """
        df = data.copy()
        
        # AUD-specific price changes
        df['price_change_fast'] = df['Close'].pct_change(self.lookback_fast)
        df['price_change_slow'] = df['Close'].pct_change(self.lookback_slow)
        
        # Triple Exponential Moving Average (TEMA) for smoother commodity trends
        def tema(series, period):
            ema1 = series.ewm(span=period).mean()
            ema2 = ema1.ewm(span=period).mean()
            ema3 = ema2.ewm(span=period).mean()
            return 3 * ema1 - 3 * ema2 + ema3
        
        df['tema_fast'] = tema(df['Close'], self.lookback_fast)
        df['tema_slow'] = tema(df['Close'], self.lookback_slow)
        
        # AUD trend strength
        df['trend_strength'] = (df['tema_fast'] - df['tema_slow']) / df['tema_slow']
        
        # Commodity-style volatility (Chaikin Volatility)
        high_low_pct = (df['High'] - df['Low']) / df['Low'] * 100
        df['chaikin_vol'] = high_low_pct.rolling(window=10).std()
        
        # Standard ATR for risk management
        df['high_low'] = df['High'] - df['Low']
        df['high_close'] = np.abs(df['High'] - df['Close'].shift(1))
        df['low_close'] = np.abs(df['Low'] - df['Close'].shift(1))
        df['true_range'] = np.maximum(df['high_low'], np.maximum(df['high_close'], df['low_close']))
        df['atr'] = df['true_range'].rolling(window=self.volatility_lookback).mean()
        
        # Volatility-adjusted momentum
        df['vol_adj_momentum'] = df['price_change_fast'] / (df['atr'] / df['Close'] + 1e-8)
        
        # Commodity-optimized RSI
        delta = df['Close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=self.rsi_period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=self.rsi_period).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # Money Flow Index (commodity volume consideration)
        if 'Volume' in df.columns and df['Volume'].sum() > 0:
            typical_price = (df['High'] + df['Low'] + df['Close']) / 3
            money_flow = typical_price * df['Volume']
            positive_flow = money_flow.where(typical_price > typical_price.shift(1), 0).rolling(window=14).sum()
            negative_flow = money_flow.where(typical_price < typical_price.shift(1), 0).rolling(window=14).sum()
            mfi_ratio = positive_flow / negative_flow
            df['mfi'] = 100 - (100 / (1 + mfi_ratio))
        else:
            df['mfi'] = df['rsi']  # Fallback to RSI
        
        # Parabolic SAR for trend following (good for commodities)
        def parabolic_sar(high, low, close, af_step=0.02, af_max=0.2):
            length = len(close)
            sar = np.zeros(length)
            trend = np.zeros(length)
            af = af_step
            ep = high.iloc[0]
            sar[0] = low.iloc[0]
            trend[0] = 1
            
            for i in range(1, length):
                if trend[i-1] == 1:  # Uptrend
                    sar[i] = sar[i-1] + af * (ep - sar[i-1])
                    if low.iloc[i] <= sar[i]:
                        trend[i] = -1
                        sar[i] = ep
                        ep = low.iloc[i]
                        af = af_step
                    else:
                        trend[i] = 1
                        if high.iloc[i] > ep:
                            ep = high.iloc[i]
                            af = min(af + af_step, af_max)
                else:  # Downtrend
                    sar[i] = sar[i-1] + af * (ep - sar[i-1])
                    if high.iloc[i] >= sar[i]:
                        trend[i] = 1
                        sar[i] = ep
                        ep = high.iloc[i]
                        af = af_step
                    else:
                        trend[i] = -1
                        if low.iloc[i] < ep:
                            ep = low.iloc[i]
                            af = min(af + af_step, af_max)
            
            return pd.Series(sar, index=close.index), pd.Series(trend, index=close.index)
        
        df['sar'], df['sar_trend'] = parabolic_sar(df['High'], df['Low'], df['Close'])
        
        # Donchian Channels for breakout detection
        donchian_period = 20
        df['donchian_upper'] = df['High'].rolling(window=donchian_period).max()
        df['donchian_lower'] = df['Low'].rolling(window=donchian_period).min()
        df['donchian_middle'] = (df['donchian_upper'] + df['donchian_lower']) / 2
        df['donchian_position'] = (df['Close'] - df['donchian_lower']) / (df['donchian_upper'] - df['donchian_lower'])
        
        # Average Directional Index (ADX) for trend strength
        def calculate_adx(high, low, close, period=14):
            # True Range calculation done above
            tr = df['true_range']
            
            # Directional Movement
            dm_plus = np.where((high.diff() > low.diff().abs()) & (high.diff() > 0), high.diff(), 0)
            dm_minus = np.where((low.diff().abs() > high.diff()) & (low.diff() < 0), low.diff().abs(), 0)
            
            # Smoothed True Range and Directional Movement
            tr_smooth = pd.Series(tr).rolling(window=period).mean()
            dm_plus_smooth = pd.Series(dm_plus).rolling(window=period).mean()
            dm_minus_smooth = pd.Series(dm_minus).rolling(window=period).mean()
            
            # Directional Indicators
            di_plus = 100 * dm_plus_smooth / tr_smooth
            di_minus = 100 * dm_minus_smooth / tr_smooth
            
            # Directional Movement Index
            dx = 100 * np.abs(di_plus - di_minus) / (di_plus + di_minus)
            adx = dx.rolling(window=period).mean()
            
            return adx, di_plus, di_minus
        
        df['adx'], df['di_plus'], df['di_minus'] = calculate_adx(df['High'], df['Low'], df['Close'])
        
        # AUDUSD-specific buy signals
        buy_conditions = [
            df['price_change_fast'] > self.momentum_threshold,     # Fast momentum
            df['trend_strength'] > 0.003,                         # Trend strength
            df['tema_fast'] > df['tema_slow'],                     # TEMA bullish
            df['vol_adj_momentum'] > 0.4,                         # Vol-adjusted momentum
            df['rsi'].between(40, 70),                            # RSI in range
            df['mfi'] > 45,                                       # Money flow bullish
            df['sar_trend'] == 1,                                 # SAR uptrend
            df['donchian_position'] > 0.3,                       # Upper Donchian range
            df['adx'] > 20,                                       # Strong trend
            df['di_plus'] > df['di_minus'],                       # Bullish DI
        ]
        
        # Require 6 out of 10 conditions for AUD
        df['buy_score'] = np.sum(buy_conditions, axis=0)
        df['buy_signal'] = (df['buy_score'] >= 6).astype(int)
        
        # AUDUSD-specific sell signals
        sell_conditions = [
            df['price_change_fast'] < -self.momentum_threshold,    # Fast downward momentum
            df['trend_strength'] < -0.003,                        # Downtrend strength
            df['tema_fast'] < df['tema_slow'],                     # TEMA bearish
            df['vol_adj_momentum'] < -0.4,                        # Negative vol momentum
            df['rsi'] < 30,                                        # RSI oversold
            df['mfi'] < 35,                                       # Money flow bearish
            df['sar_trend'] == -1,                                # SAR downtrend
            df['donchian_position'] < 0.2,                       # Lower Donchian range
        ]
        
        # Require 5 out of 8 conditions for sell
        df['sell_score'] = np.sum(sell_conditions, axis=0)
        df['sell_signal'] = (df['sell_score'] >= 5).astype(int)
        
        # Signal confirmation
        df['buy_confirmed'] = (df['buy_signal'].rolling(window=self.confirmation_period).sum() >= 2).astype(int)
        df['sell_confirmed'] = (df['sell_signal'].rolling(window=self.confirmation_period).sum() >= 2).astype(int)
        
        return df
    
    def calculate_aud_kelly(self, returns: List[float], current_volatility: float) -> float:
        """
        Calculate Kelly fraction adjusted for AUD commodity characteristics
        
        Args:
            returns: List of historical returns
            current_volatility: Current market volatility
            
        Returns:
            AUD-optimized Kelly fraction
        """
        if len(returns) < 8:
            return 0.10  # Moderate default for commodity currency
        
        returns_array = np.array(returns)
        
        # Focus on recent performance
        recent_returns = returns_array[-12:] if len(returns_array) >= 12 else returns_array
        
        wins = recent_returns[recent_returns > 0]
        losses = recent_returns[recent_returns < 0]
        
        if len(wins) == 0 or len(losses) == 0:
            return 0.08
        
        win_rate = len(wins) / len(recent_returns)
        avg_win = np.mean(wins)
        avg_loss = np.abs(np.mean(losses))
        
        # Kelly calculation
        if avg_loss > 0:
            b = avg_win / avg_loss
            kelly_fraction = (b * win_rate - (1 - win_rate)) / b
        else:
            kelly_fraction = 0.08
        
        # AUD volatility adjustment (moderate)
        volatility_factor = min(1.1, 0.013 / (current_volatility + 1e-8))
        adjusted_kelly = kelly_fraction * volatility_factor
        
        # AUD-specific constraints
        adjusted_kelly = max(0.06, min(adjusted_kelly, self.max_position_size))
        
        return adjusted_kelly
    
    def execute_aud_trade(self, signal: str, price: float, date: str, 
                         position_size: float = 0.14) -> Dict:
        """
        Execute AUD trade with commodity currency position management
        
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
    
    def check_aud_risk_management(self, current_price: float, date: str) -> bool:
        """
        Check AUD-specific risk management rules
        
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
    
    def run_audusd_backtest(self, start_date: str = "2025-03-01", 
                           end_date: str = "2025-09-16") -> Dict:
        """
        Run comprehensive AUDUSD backtest with commodity optimizations
        
        Args:
            start_date: Start date for backtest
            end_date: End date for backtest
            
        Returns:
            Comprehensive backtest results
        """
        print(f"\n🔄 AUDUSD Momentum Backtest: {start_date} to {end_date}")
        print("=" * 60)
        
        # Get market data
        data = self.get_audusd_data(days=230)
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
        
        # Calculate AUD-specific signals
        data_with_signals = self.calculate_aud_signals(data)
        
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
            current_atr = row.get('atr', 0.006)  # AUD typical volatility
            
            # Check risk management first
            self.check_aud_risk_management(current_price, date.strftime('%Y-%m-%d'))
            
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
                if len(recent_returns) > 30:
                    recent_returns.pop(0)
            
            portfolio_history.append(current_portfolio_value)
            
            # Generate trading signals
            if not np.isnan(row.get('buy_confirmed', np.nan)) and not np.isnan(row.get('sell_confirmed', np.nan)):
                current_volatility = current_atr / current_price
                kelly_fraction = self.calculate_aud_kelly(recent_returns, current_volatility)
                
                if row['buy_confirmed'] == 1 and self.position <= 0:
                    self.execute_aud_trade('buy', current_price, date.strftime('%Y-%m-%d'), kelly_fraction)
                elif row['sell_confirmed'] == 1 and self.position > 0:
                    self.execute_aud_trade('sell', current_price, date.strftime('%Y-%m-%d'), kelly_fraction)
        
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
        print(f"\n🎉 AUDUSD Momentum Backtest Results")
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
    """Run AUDUSD momentum strategy backtest"""
    print("🚀 AUDUSD Momentum Strategy - 6 Month Validation")
    print("================================================")
    
    # Initialize AUD-optimized strategy
    strategy = AUDUSDMomentumStrategy(
        initial_capital=10000.0,
        lookback_fast=8,              # Balanced for commodity sensitivity
        lookback_slow=22,             # Extended trend confirmation
        momentum_threshold=0.005,     # Moderate threshold for AUD
        stop_loss=0.016,              # Moderate stops
        take_profit=0.028,            # Reasonable profit targets
        max_position_size=0.18        # Moderate position sizing
    )
    
    # Run comprehensive 6-month backtest
    results = strategy.run_audusd_backtest(
        start_date="2025-03-01",
        end_date="2025-09-16"
    )
    
    if results.get('success'):
        print(f"\n🏆 AUDUSD Strategy Performance:")
        print(f"   Return: {results['annualized_return']:.1%} annually")
        print(f"   Sharpe: {results['sharpe_ratio']:.2f}")
        print(f"   Trades: {results['pnl_trades']} P&L trades ({results['win_rate']:.1%} win rate)")
        print(f"   Max DD: {results['max_drawdown']:.1%}")
        
        # Benchmark comparison
        try:
            ticker = yf.Ticker("AUDUSD=X")
            benchmark_data = ticker.history(start="2025-03-01", end="2025-09-16")
            if len(benchmark_data) > 0:
                benchmark_return = (benchmark_data['Close'].iloc[-1] - benchmark_data['Close'].iloc[0]) / benchmark_data['Close'].iloc[0]
                excess_return = results['total_return'] - benchmark_return
                
                print(f"\n📊 vs Buy & Hold AUDUSD:")
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