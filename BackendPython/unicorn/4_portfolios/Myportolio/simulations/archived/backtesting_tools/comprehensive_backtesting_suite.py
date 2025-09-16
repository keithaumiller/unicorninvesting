#!/usr/bin/env python3
"""
Comprehensive Backtesting Suite with Live Market Data
Tests multiple trading strategies using real market data from APIs
"""

import sys
import os
import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings('ignore')

class ComprehensiveBacktestingSuite:
    """Advanced backtesting system using live market data"""
    
    def __init__(self, initial_capital=100000.0):
        self.initial_capital = initial_capital
        self.current_capital = initial_capital
        
        # Load live market data components
        sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
        from live_market_data_feed import LiveMarketDataFeed
        from simplified_ensemble_portfolio import EnsembleMultiAssetPortfolio
        from lean_backtesting_integration import LEANBacktestingEngine
        
        self.market_feed = LiveMarketDataFeed()
        self.portfolio = EnsembleMultiAssetPortfolio(initial_capital)
        self.lean_engine = LEANBacktestingEngine(initial_capital)
        
        # Strategy configurations
        self.strategies = {
            'momentum_short': {'lookback': 5, 'rsi_threshold': 30},
            'momentum_medium': {'lookback': 14, 'rsi_threshold': 35},
            'momentum_long': {'lookback': 21, 'rsi_threshold': 40},
            'mean_reversion': {'lookback': 20, 'rsi_oversold': 20, 'rsi_overbought': 80},
            'trend_following': {'sma_short': 5, 'sma_long': 20}
        }
        
        # Results storage
        self.backtest_results = {}
        self.performance_metrics = {}
        
    def generate_extended_market_data(self, symbol='ETH', periods=100):
        """Generate extended historical market data for backtesting"""
        print(f"📊 Generating extended market data for {symbol}...")
        
        # Get current live price
        current_price = self.market_feed.get_crypto_price(symbol)
        print(f"   🔗 Live {symbol} Price: ${current_price:,.2f}")
        
        # Generate comprehensive historical data
        market_data = self.market_feed.generate_realistic_market_data(
            symbol, current_price, periods=periods
        )
        
        # Add more sophisticated features
        self._add_advanced_features(market_data)
        
        print(f"   📈 Generated {len(market_data)} data points")
        print(f"   📊 Price Range: ${market_data['close'].min():.2f} - ${market_data['close'].max():.2f}")
        print(f"   📊 Avg Volume: {market_data['volume'].mean():,.0f}")
        
        return market_data
    
    def _add_advanced_features(self, data):
        """Add advanced technical indicators"""
        prices = data['close']
        
        # Moving averages
        data['sma_5'] = prices.rolling(5).mean()
        data['sma_10'] = prices.rolling(10).mean()
        data['sma_20'] = prices.rolling(20).mean()
        data['sma_50'] = prices.rolling(50).mean()
        
        # Exponential moving averages
        data['ema_12'] = prices.ewm(span=12).mean()
        data['ema_26'] = prices.ewm(span=26).mean()
        
        # MACD
        data['macd'] = data['ema_12'] - data['ema_26']
        data['macd_signal'] = data['macd'].ewm(span=9).mean()
        data['macd_histogram'] = data['macd'] - data['macd_signal']
        
        # RSI
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
        rs = gain / loss
        data['rsi'] = 100 - (100 / (1 + rs))
        
        # Bollinger Bands
        data['bb_middle'] = prices.rolling(20).mean()
        bb_std = prices.rolling(20).std()
        data['bb_upper'] = data['bb_middle'] + (bb_std * 2)
        data['bb_lower'] = data['bb_middle'] - (bb_std * 2)
        data['bb_position'] = (prices - data['bb_lower']) / (data['bb_upper'] - data['bb_lower'])
        
        # Volatility measures
        data['volatility_10'] = prices.pct_change().rolling(10).std()
        data['volatility_20'] = prices.pct_change().rolling(20).std()
        
        # Price momentum
        data['momentum_5'] = prices.pct_change(5)
        data['momentum_10'] = prices.pct_change(10)
        data['momentum_20'] = prices.pct_change(20)
        
        # Volume indicators
        data['volume_sma'] = data['volume'].rolling(20).mean()
        data['volume_ratio'] = data['volume'] / data['volume_sma']
        
        # Clean data
        data.fillna(method='backfill', inplace=True)
        data.fillna(0, inplace=True)
    
    def strategy_momentum_short(self, data, config):
        """Short-term momentum strategy"""
        signals = []
        lookback = config['lookback']
        rsi_threshold = config['rsi_threshold']
        
        for i in range(len(data)):
            if i < lookback:
                signals.append(0.0)
                continue
                
            # Current values
            current_rsi = data.iloc[i]['rsi']
            price_momentum = data.iloc[i]['momentum_5']
            macd_signal = data.iloc[i]['macd_histogram']
            
            # Generate signal
            signal = 0.0
            
            # Bullish conditions
            if (current_rsi < rsi_threshold and 
                price_momentum > 0.02 and 
                macd_signal > 0):
                signal = min(0.15, price_momentum * 3)  # Cap at 15%
            
            # Bearish conditions  
            elif (current_rsi > (100 - rsi_threshold) and 
                  price_momentum < -0.02 and 
                  macd_signal < 0):
                signal = max(-0.15, price_momentum * 3)  # Cap at -15%
            
            signals.append(signal)
        
        return signals
    
    def strategy_momentum_medium(self, data, config):
        """Medium-term momentum strategy"""
        signals = []
        lookback = config['lookback']
        rsi_threshold = config['rsi_threshold']
        
        for i in range(len(data)):
            if i < lookback:
                signals.append(0.0)
                continue
                
            # Current values
            current_rsi = data.iloc[i]['rsi']
            sma_5 = data.iloc[i]['sma_5']
            sma_20 = data.iloc[i]['sma_20']
            bb_position = data.iloc[i]['bb_position']
            
            # Generate signal
            signal = 0.0
            
            # Bullish momentum
            if (current_rsi > rsi_threshold and current_rsi < 70 and
                sma_5 > sma_20 and bb_position > 0.2 and bb_position < 0.8):
                signal = min(0.20, (sma_5 - sma_20) / sma_20 * 10)
            
            # Bearish momentum
            elif (current_rsi < (100 - rsi_threshold) and current_rsi > 30 and
                  sma_5 < sma_20 and bb_position < 0.8 and bb_position > 0.2):
                signal = max(-0.20, (sma_5 - sma_20) / sma_20 * 10)
            
            signals.append(signal)
        
        return signals
    
    def strategy_momentum_long(self, data, config):
        """Long-term momentum strategy"""
        signals = []
        lookback = config['lookback']
        rsi_threshold = config['rsi_threshold']
        
        for i in range(len(data)):
            if i < lookback:
                signals.append(0.0)
                continue
                
            # Current values
            current_rsi = data.iloc[i]['rsi']
            momentum_20 = data.iloc[i]['momentum_20']
            sma_20 = data.iloc[i]['sma_20']
            sma_50 = data.iloc[i]['sma_50']
            volatility = data.iloc[i]['volatility_20']
            
            # Generate signal
            signal = 0.0
            
            # Strong bullish trend
            if (current_rsi > rsi_threshold and 
                momentum_20 > 0.05 and 
                sma_20 > sma_50 and 
                volatility < 0.15):
                signal = min(0.25, momentum_20 * 2)
            
            # Strong bearish trend
            elif (current_rsi < (100 - rsi_threshold) and 
                  momentum_20 < -0.05 and 
                  sma_20 < sma_50 and 
                  volatility < 0.15):
                signal = max(-0.25, momentum_20 * 2)
            
            signals.append(signal)
        
        return signals
    
    def strategy_mean_reversion(self, data, config):
        """Mean reversion strategy"""
        signals = []
        lookback = config['lookback']
        rsi_oversold = config['rsi_oversold']
        rsi_overbought = config['rsi_overbought']
        
        for i in range(len(data)):
            if i < lookback:
                signals.append(0.0)
                continue
                
            # Current values
            current_rsi = data.iloc[i]['rsi']
            bb_position = data.iloc[i]['bb_position']
            volatility = data.iloc[i]['volatility_10']
            
            # Generate signal
            signal = 0.0
            
            # Oversold conditions (buy)
            if (current_rsi < rsi_oversold and 
                bb_position < 0.1 and 
                volatility > 0.02):
                signal = min(0.18, (rsi_oversold - current_rsi) / 20)
            
            # Overbought conditions (sell)
            elif (current_rsi > rsi_overbought and 
                  bb_position > 0.9 and 
                  volatility > 0.02):
                signal = max(-0.18, (rsi_overbought - current_rsi) / 20)
            
            signals.append(signal)
        
        return signals
    
    def strategy_trend_following(self, data, config):
        """Trend following strategy"""
        signals = []
        sma_short = config['sma_short']
        sma_long = config['sma_long']
        
        for i in range(len(data)):
            if i < sma_long:
                signals.append(0.0)
                continue
                
            # Current values
            short_ma = data.iloc[i]['sma_5']
            long_ma = data.iloc[i]['sma_20']
            macd = data.iloc[i]['macd']
            volume_ratio = data.iloc[i]['volume_ratio']
            
            # Generate signal
            signal = 0.0
            
            # Bullish trend
            if (short_ma > long_ma and 
                macd > 0 and 
                volume_ratio > 1.2):
                signal = min(0.22, (short_ma - long_ma) / long_ma * 5)
            
            # Bearish trend
            elif (short_ma < long_ma and 
                  macd < 0 and 
                  volume_ratio > 1.2):
                signal = max(-0.22, (short_ma - long_ma) / long_ma * 5)
            
            signals.append(signal)
        
        return signals
    
    def run_backtest(self, strategy_name, data, signals):
        """Run backtest for a specific strategy"""
        print(f"\n🎯 Running backtest for {strategy_name}...")
        
        portfolio_value = self.initial_capital
        cash = self.initial_capital
        position = 0.0
        trades = []
        portfolio_history = []
        
        for i in range(len(data)):
            current_price = data.iloc[i]['close']
            current_signal = signals[i]
            current_time = data.index[i]
            
            # Calculate target position
            if abs(current_signal) > 0.01:  # Minimum signal threshold
                target_value = portfolio_value * current_signal
                target_shares = target_value / current_price
                
                # Execute trade
                if target_shares != position:
                    trade_shares = target_shares - position
                    trade_value = trade_shares * current_price
                    
                    if abs(trade_value) > 100:  # Minimum trade size
                        # Apply transaction costs (0.1%)
                        transaction_cost = abs(trade_value) * 0.001
                        
                        if trade_shares > 0:  # Buy
                            if cash >= (trade_value + transaction_cost):
                                cash -= (trade_value + transaction_cost)
                                position += trade_shares
                                trades.append({
                                    'time': current_time,
                                    'type': 'BUY',
                                    'shares': trade_shares,
                                    'price': current_price,
                                    'value': trade_value,
                                    'cost': transaction_cost
                                })
                        else:  # Sell
                            cash += (abs(trade_value) - transaction_cost)
                            position += trade_shares
                            trades.append({
                                'time': current_time,
                                'type': 'SELL',
                                'shares': abs(trade_shares),
                                'price': current_price,
                                'value': abs(trade_value),
                                'cost': transaction_cost
                            })
            
            # Calculate portfolio value
            portfolio_value = cash + (position * current_price)
            
            portfolio_history.append({
                'time': current_time,
                'price': current_price,
                'signal': current_signal,
                'position': position,
                'cash': cash,
                'portfolio_value': portfolio_value,
                'returns': (portfolio_value - self.initial_capital) / self.initial_capital
            })
        
        # Calculate performance metrics
        final_value = portfolio_history[-1]['portfolio_value']
        total_return = (final_value - self.initial_capital) / self.initial_capital
        
        returns_series = pd.Series([h['returns'] for h in portfolio_history])
        daily_returns = returns_series.diff().dropna()
        
        volatility = daily_returns.std() * np.sqrt(252) if len(daily_returns) > 1 else 0
        sharpe_ratio = (total_return / volatility) if volatility > 0 else 0
        
        max_drawdown = 0
        peak = self.initial_capital
        for h in portfolio_history:
            if h['portfolio_value'] > peak:
                peak = h['portfolio_value']
            drawdown = (peak - h['portfolio_value']) / peak
            max_drawdown = max(max_drawdown, drawdown)
        
        win_trades = [t for t in trades[1:] if trades.index(t) % 2 == 1]
        total_trades = len(trades) // 2 if len(trades) > 1 else 0
        
        print(f"   📊 Final Value: ${final_value:,.2f}")
        print(f"   📊 Total Return: {total_return:.2%}")
        print(f"   📊 Total Trades: {total_trades}")
        print(f"   📊 Sharpe Ratio: {sharpe_ratio:.2f}")
        print(f"   📊 Max Drawdown: {max_drawdown:.2%}")
        
        return {
            'strategy': strategy_name,
            'final_value': final_value,
            'total_return': total_return,
            'volatility': volatility,
            'sharpe_ratio': sharpe_ratio,
            'max_drawdown': max_drawdown,
            'total_trades': total_trades,
            'trades': trades,
            'portfolio_history': portfolio_history
        }
    
    def run_comprehensive_backtests(self, symbols=['ETH'], periods=200):
        """Run comprehensive backtests across all strategies"""
        print("🚀 COMPREHENSIVE BACKTESTING SUITE")
        print("=" * 60)
        
        results_summary = []
        
        for symbol in symbols:
            print(f"\n📈 BACKTESTING {symbol}")
            print("-" * 40)
            
            # Generate market data
            market_data = self.generate_extended_market_data(symbol, periods)
            
            # Test each strategy
            for strategy_name, config in self.strategies.items():
                print(f"\n🎯 Strategy: {strategy_name.upper()}")
                
                # Generate signals
                if strategy_name == 'momentum_short':
                    signals = self.strategy_momentum_short(market_data, config)
                elif strategy_name == 'momentum_medium':
                    signals = self.strategy_momentum_medium(market_data, config)
                elif strategy_name == 'momentum_long':
                    signals = self.strategy_momentum_long(market_data, config)
                elif strategy_name == 'mean_reversion':
                    signals = self.strategy_mean_reversion(market_data, config)
                elif strategy_name == 'trend_following':
                    signals = self.strategy_trend_following(market_data, config)
                
                # Run backtest
                result = self.run_backtest(f"{symbol}_{strategy_name}", market_data, signals)
                result['symbol'] = symbol
                result['config'] = config
                
                self.backtest_results[f"{symbol}_{strategy_name}"] = result
                results_summary.append(result)
        
        return results_summary
    
    def generate_performance_report(self, results_summary):
        """Generate comprehensive performance report"""
        print("\n📊 PERFORMANCE ANALYSIS REPORT")
        print("=" * 60)
        
        # Sort by total return
        sorted_results = sorted(results_summary, key=lambda x: x['total_return'], reverse=True)
        
        print("\n🏆 STRATEGY RANKINGS (by Total Return):")
        print("-" * 50)
        
        for i, result in enumerate(sorted_results):
            rank = i + 1
            strategy = result['strategy']
            total_return = result['total_return']
            sharpe = result['sharpe_ratio']
            max_dd = result['max_drawdown']
            trades = result['total_trades']
            
            status = "🟢" if total_return > 0 else "🔴"
            
            print(f"{rank:2d}. {status} {strategy:<25} "
                  f"Return: {total_return:>7.2%} | "
                  f"Sharpe: {sharpe:>5.2f} | "
                  f"MaxDD: {max_dd:>6.2%} | "
                  f"Trades: {trades:>3d}")
        
        # Best performers analysis
        print(f"\n🎯 TOP PERFORMER: {sorted_results[0]['strategy']}")
        best = sorted_results[0]
        print(f"   💰 Total Return: {best['total_return']:.2%}")
        print(f"   📊 Final Value: ${best['final_value']:,.2f}")
        print(f"   ⚡ Sharpe Ratio: {best['sharpe_ratio']:.2f}")
        print(f"   🛡️ Max Drawdown: {best['max_drawdown']:.2%}")
        print(f"   🔄 Total Trades: {best['total_trades']}")
        
        # Risk-adjusted performance
        print(f"\n🛡️ BEST RISK-ADJUSTED (Highest Sharpe):")
        best_sharpe = max(sorted_results, key=lambda x: x['sharpe_ratio'])
        print(f"   🎯 Strategy: {best_sharpe['strategy']}")
        print(f"   ⚡ Sharpe Ratio: {best_sharpe['sharpe_ratio']:.2f}")
        print(f"   💰 Total Return: {best_sharpe['total_return']:.2%}")
        print(f"   🛡️ Max Drawdown: {best_sharpe['max_drawdown']:.2%}")
        
        # Summary statistics
        total_returns = [r['total_return'] for r in results_summary]
        sharpe_ratios = [r['sharpe_ratio'] for r in results_summary]
        max_drawdowns = [r['max_drawdown'] for r in results_summary]
        
        print(f"\n📈 PORTFOLIO STATISTICS:")
        print(f"   📊 Average Return: {np.mean(total_returns):.2%}")
        print(f"   📊 Best Return: {max(total_returns):.2%}")
        print(f"   📊 Worst Return: {min(total_returns):.2%}")
        print(f"   📊 Average Sharpe: {np.mean(sharpe_ratios):.2f}")
        print(f"   📊 Average MaxDD: {np.mean(max_drawdowns):.2%}")
        
        profitable_strategies = len([r for r in results_summary if r['total_return'] > 0])
        print(f"   💰 Profitable Strategies: {profitable_strategies}/{len(results_summary)} ({profitable_strategies/len(results_summary):.1%})")
        
        return {
            'best_performer': sorted_results[0],
            'best_risk_adjusted': best_sharpe,
            'summary_stats': {
                'avg_return': np.mean(total_returns),
                'best_return': max(total_returns),
                'worst_return': min(total_returns),
                'avg_sharpe': np.mean(sharpe_ratios),
                'avg_max_dd': np.mean(max_drawdowns),
                'profitable_pct': profitable_strategies/len(results_summary)
            }
        }
    
    def export_results(self, results_summary, performance_report):
        """Export results to JSON for further analysis"""
        export_data = {
            'backtest_run': {
                'timestamp': datetime.now().isoformat(),
                'initial_capital': self.initial_capital,
                'total_strategies': len(results_summary)
            },
            'results_summary': results_summary,
            'performance_report': performance_report,
            'strategy_configs': self.strategies
        }
        
        output_file = f'/tmp/backtesting_results_{datetime.now().strftime("%Y%m%d_%H%M%S")}.json'
        
        # Convert datetime objects to strings for JSON serialization
        def convert_datetime(obj):
            if hasattr(obj, 'isoformat'):
                return obj.isoformat()
            elif isinstance(obj, dict):
                return {k: convert_datetime(v) for k, v in obj.items()}
            elif isinstance(obj, list):
                return [convert_datetime(v) for v in obj]
            else:
                return obj
        
        export_data = convert_datetime(export_data)
        
        with open(output_file, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        print(f"\n📁 RESULTS EXPORTED: {output_file}")
        return output_file

def main():
    """Run comprehensive backtesting suite"""
    import logging
    logging.getLogger().setLevel(logging.WARNING)
    
    print("🚀 STARTING COMPREHENSIVE BACKTESTING WITH LIVE DATA")
    print("=" * 60)
    
    # Initialize backtesting suite
    suite = ComprehensiveBacktestingSuite(initial_capital=100000.0)
    
    # Run comprehensive backtests
    results = suite.run_comprehensive_backtests(symbols=['ETH'], periods=150)
    
    # Generate performance report
    performance_report = suite.generate_performance_report(results)
    
    # Export results
    output_file = suite.export_results(results, performance_report)
    
    print(f"\n🎉 BACKTESTING COMPLETE!")
    print("=" * 60)
    print(f"✅ Tested {len(suite.strategies)} strategies")
    print(f"✅ Generated {len(results)} backtest results")
    print(f"✅ Used live market data from APIs")
    print(f"✅ Results exported to: {output_file}")
    
    return results, performance_report

if __name__ == "__main__":
    main()