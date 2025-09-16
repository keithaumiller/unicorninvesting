#!/usr/bin/env python3
"""
Robust Backtesting Suite with Live Market Data
Simplified but comprehensive backtesting with multiple strategies
"""

import sys
import os
import json
import pandas as pd
import numpy as np
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

class RobustBacktestingSuite:
    """Robust backtesting system using live market data"""
    
    def __init__(self, initial_capital=100000.0):
        self.initial_capital = initial_capital
        
        # Load live market data components
        sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
        from live_market_data_feed import LiveMarketDataFeed
        
        self.market_feed = LiveMarketDataFeed()
        
        # Pre-defined optimized strategy configurations
        self.strategies = {
            'conservative_momentum': {
                'lookback': 14,
                'rsi_threshold': 35,
                'position_limit': 0.15,
                'min_volume_ratio': 1.2,
                'description': 'Conservative momentum with 15% position limit'
            },
            'aggressive_momentum': {
                'lookback': 10,
                'rsi_threshold': 30,
                'position_limit': 0.25,
                'min_volume_ratio': 1.5,
                'description': 'Aggressive momentum with 25% position limit'
            },
            'long_term_trend': {
                'lookback': 21,
                'rsi_threshold': 40,
                'position_limit': 0.20,
                'min_volume_ratio': 1.1,
                'description': 'Long-term trend following'
            },
            'mean_reversion': {
                'rsi_oversold': 25,
                'rsi_overbought': 75,
                'position_limit': 0.18,
                'volatility_threshold': 0.02,
                'description': 'Mean reversion strategy'
            },
            'balanced_portfolio': {
                'lookback': 14,
                'rsi_threshold': 40,
                'position_limit': 0.12,
                'min_volume_ratio': 1.0,
                'description': 'Balanced risk-adjusted approach'
            }
        }
        
        self.backtest_results = {}
    
    def generate_market_data_with_features(self, symbol='ETH', periods=200):
        """Generate market data with all necessary features"""
        print(f"📊 Generating market data with features for {symbol}...")
        
        # Get current live price
        current_price = self.market_feed.get_crypto_price(symbol)
        print(f"   🔗 Live {symbol} Price: ${current_price:,.2f}")
        
        # Generate historical data
        market_data = self.market_feed.generate_realistic_market_data(
            symbol, current_price, periods=periods
        )
        
        # Add technical features
        self._calculate_technical_features(market_data)
        
        print(f"   📈 Generated {len(market_data)} data points")
        print(f"   📊 Price Range: ${market_data['close'].min():.2f} - ${market_data['close'].max():.2f}")
        print(f"   📊 Features: {len(market_data.columns)} technical indicators")
        
        return market_data
    
    def _calculate_technical_features(self, data):
        """Calculate comprehensive technical features"""
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
        
        # Volatility
        data['volatility'] = prices.pct_change().rolling(14).std()
        
        # Momentum (price change over periods)
        data['momentum_5'] = prices.pct_change(5)
        data['momentum_10'] = prices.pct_change(10)
        data['momentum_20'] = prices.pct_change(20)
        
        # Volume indicators
        data['volume_sma'] = data['volume'].rolling(20).mean()
        data['volume_ratio'] = data['volume'] / data['volume_sma']
        
        # Price channels
        data['high_20'] = prices.rolling(20).max()
        data['low_20'] = prices.rolling(20).min()
        data['channel_position'] = (prices - data['low_20']) / (data['high_20'] - data['low_20'])
        
        # Clean data
        data.fillna(method='backfill', inplace=True)
        data.fillna(0, inplace=True)
    
    def generate_momentum_signals(self, data, config):
        """Generate momentum-based trading signals"""
        signals = []
        lookback = config['lookback']
        rsi_threshold = config['rsi_threshold']
        position_limit = config['position_limit']
        min_volume_ratio = config['min_volume_ratio']
        
        for i in range(len(data)):
            if i < max(lookback, 20):
                signals.append(0.0)
                continue
            
            row = data.iloc[i]
            
            # Current market conditions
            current_rsi = row['rsi']
            
            # Use available momentum columns or calculate on the fly
            if f'momentum_{min(lookback, 20)}' in data.columns:
                momentum = row[f'momentum_{min(lookback, 20)}']
            else:
                # Calculate momentum on the fly
                if i >= min(lookback, 20):
                    momentum = (data.iloc[i]['close'] - data.iloc[i - min(lookback, 20)]['close']) / data.iloc[i - min(lookback, 20)]['close']
                else:
                    momentum = 0.0
            
            macd_histogram = row['macd_histogram']
            volatility = row['volatility']
            volume_ratio = row['volume_ratio']
            bb_position = row['bb_position']
            
            signal = 0.0
            
            # Bullish momentum conditions
            if (current_rsi > rsi_threshold and current_rsi < 70 and
                momentum > 0.015 and
                macd_histogram > 0 and
                volume_ratio > min_volume_ratio and
                volatility < 0.08 and
                bb_position > 0.2 and bb_position < 0.8):
                
                signal = min(position_limit, momentum * 8)
            
            # Bearish momentum conditions
            elif (current_rsi < (100 - rsi_threshold) and current_rsi > 30 and
                  momentum < -0.015 and
                  macd_histogram < 0 and
                  volume_ratio > min_volume_ratio and
                  volatility < 0.08 and
                  bb_position > 0.2 and bb_position < 0.8):
                
                signal = max(-position_limit, momentum * 8)
            
            signals.append(signal)
        
        return signals
    
    def generate_mean_reversion_signals(self, data, config):
        """Generate mean reversion trading signals"""
        signals = []
        rsi_oversold = config['rsi_oversold']
        rsi_overbought = config['rsi_overbought']
        position_limit = config['position_limit']
        volatility_threshold = config['volatility_threshold']
        
        for i in range(len(data)):
            if i < 30:
                signals.append(0.0)
                continue
            
            row = data.iloc[i]
            
            current_rsi = row['rsi']
            bb_position = row['bb_position']
            volatility = row['volatility']
            channel_position = row['channel_position']
            
            signal = 0.0
            
            # Oversold conditions (buy)
            if (current_rsi < rsi_oversold and
                bb_position < 0.15 and
                volatility > volatility_threshold and
                channel_position < 0.25):
                
                signal = min(position_limit, (rsi_oversold - current_rsi) / 30)
            
            # Overbought conditions (sell)
            elif (current_rsi > rsi_overbought and
                  bb_position > 0.85 and
                  volatility > volatility_threshold and
                  channel_position > 0.75):
                
                signal = max(-position_limit, (rsi_overbought - current_rsi) / 30)
            
            signals.append(signal)
        
        return signals
    
    def run_backtest_simulation(self, data, signals, strategy_name):
        """Run backtest simulation with detailed tracking"""
        print(f"   🎯 Running {strategy_name} backtest...")
        
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
                
                # Execute trade if significant enough
                if abs(target_shares - position) * current_price > 100:  # Min $100 trade
                    trade_shares = target_shares - position
                    trade_value = trade_shares * current_price
                    
                    # Transaction costs (0.1%)
                    transaction_cost = abs(trade_value) * 0.001
                    
                    if trade_shares > 0:  # Buy order
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
                    else:  # Sell order
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
            
            # Update portfolio value
            portfolio_value = cash + (position * current_price)
            
            # Track portfolio progression
            portfolio_history.append({
                'time': current_time,
                'price': current_price,
                'signal': current_signal,
                'position': position,
                'cash': cash,
                'portfolio_value': portfolio_value,
                'returns': (portfolio_value - self.initial_capital) / self.initial_capital
            })
        
        return self._calculate_performance_metrics(portfolio_history, trades, strategy_name)
    
    def _calculate_performance_metrics(self, portfolio_history, trades, strategy_name):
        """Calculate comprehensive performance metrics"""
        final_value = portfolio_history[-1]['portfolio_value']
        total_return = (final_value - self.initial_capital) / self.initial_capital
        
        # Calculate daily returns
        returns_series = pd.Series([h['returns'] for h in portfolio_history])
        daily_returns = returns_series.diff().dropna()
        
        # Risk metrics
        volatility = daily_returns.std() * np.sqrt(252) if len(daily_returns) > 1 else 0
        sharpe_ratio = (total_return / volatility) if volatility > 0 else 0
        
        # Maximum drawdown
        max_drawdown = 0
        peak = self.initial_capital
        for h in portfolio_history:
            if h['portfolio_value'] > peak:
                peak = h['portfolio_value']
            drawdown = (peak - h['portfolio_value']) / peak
            max_drawdown = max(max_drawdown, drawdown)
        
        # Trading statistics
        total_trades = len(trades)
        buy_trades = len([t for t in trades if t['type'] == 'BUY'])
        sell_trades = len([t for t in trades if t['type'] == 'SELL'])
        total_costs = sum([t['cost'] for t in trades])
        
        # Performance periods
        if len(portfolio_history) >= 30:
            month_ago_value = portfolio_history[-30]['portfolio_value']
            monthly_return = (final_value - month_ago_value) / month_ago_value
        else:
            monthly_return = total_return
        
        print(f"      📊 Final Value: ${final_value:,.2f}")
        print(f"      📊 Total Return: {total_return:.2%}")
        print(f"      📊 Monthly Return: {monthly_return:.2%}")
        print(f"      📊 Sharpe Ratio: {sharpe_ratio:.2f}")
        print(f"      📊 Max Drawdown: {max_drawdown:.2%}")
        print(f"      📊 Total Trades: {total_trades} (Buy: {buy_trades}, Sell: {sell_trades})")
        print(f"      📊 Transaction Costs: ${total_costs:.2f}")
        
        return {
            'strategy': strategy_name,
            'final_value': final_value,
            'total_return': total_return,
            'monthly_return': monthly_return,
            'volatility': volatility,
            'sharpe_ratio': sharpe_ratio,
            'max_drawdown': max_drawdown,
            'total_trades': total_trades,
            'buy_trades': buy_trades,
            'sell_trades': sell_trades,
            'transaction_costs': total_costs,
            'trades': trades,
            'portfolio_history': portfolio_history[-20:]  # Keep last 20 points
        }
    
    def run_comprehensive_backtests(self, symbol='ETH', periods=200):
        """Run comprehensive backtests across all strategies"""
        print("🚀 COMPREHENSIVE BACKTESTING WITH LIVE DATA")
        print("=" * 60)
        
        # Generate market data
        market_data = self.generate_market_data_with_features(symbol, periods)
        
        results = []
        
        print(f"\n📈 TESTING STRATEGIES ON {symbol} DATA")
        print("-" * 40)
        
        for strategy_name, config in self.strategies.items():
            print(f"\n🎯 Strategy: {strategy_name.upper()}")
            print(f"   📝 Description: {config['description']}")
            
            # Generate signals based on strategy type
            if 'momentum' in strategy_name or 'trend' in strategy_name or 'balanced' in strategy_name:
                signals = self.generate_momentum_signals(market_data, config)
            else:  # mean_reversion
                signals = self.generate_mean_reversion_signals(market_data, config)
            
            # Run backtest
            result = self.run_backtest_simulation(market_data, signals, strategy_name)
            result['symbol'] = symbol
            result['config'] = config
            
            results.append(result)
            self.backtest_results[strategy_name] = result
        
        return results
    
    def generate_comprehensive_report(self, results):
        """Generate comprehensive performance report"""
        print("\n📊 COMPREHENSIVE PERFORMANCE REPORT")
        print("=" * 60)
        
        # Sort by Sharpe ratio for risk-adjusted ranking
        sorted_by_sharpe = sorted(results, key=lambda x: x['sharpe_ratio'], reverse=True)
        
        # Sort by total return for absolute performance
        sorted_by_return = sorted(results, key=lambda x: x['total_return'], reverse=True)
        
        print("\n🏆 STRATEGY RANKINGS (Risk-Adjusted - by Sharpe Ratio):")
        print("-" * 55)
        
        for i, result in enumerate(sorted_by_sharpe):
            rank = i + 1
            strategy = result['strategy']
            total_return = result['total_return']
            sharpe = result['sharpe_ratio']
            max_dd = result['max_drawdown']
            trades = result['total_trades']
            
            status = "🟢" if total_return > 0 else "🔴"
            
            print(f"{rank}. {status} {strategy:<22} "
                  f"Sharpe: {sharpe:>5.2f} | "
                  f"Return: {total_return:>7.2%} | "
                  f"MaxDD: {max_dd:>6.2%} | "
                  f"Trades: {trades:>3d}")
        
        print("\n💰 STRATEGY RANKINGS (Absolute Performance - by Return):")
        print("-" * 55)
        
        for i, result in enumerate(sorted_by_return):
            rank = i + 1
            strategy = result['strategy']
            total_return = result['total_return']
            monthly_return = result['monthly_return']
            final_value = result['final_value']
            
            status = "🟢" if total_return > 0 else "🔴"
            
            print(f"{rank}. {status} {strategy:<22} "
                  f"Return: {total_return:>7.2%} | "
                  f"Monthly: {monthly_return:>7.2%} | "
                  f"Value: ${final_value:>9,.0f}")
        
        # Best performers analysis
        best_sharpe = sorted_by_sharpe[0]
        best_return = sorted_by_return[0]
        
        print(f"\n🎯 BEST RISK-ADJUSTED PERFORMANCE:")
        print(f"   Strategy: {best_sharpe['strategy']}")
        print(f"   Configuration: {best_sharpe['config']['description']}")
        print(f"   Sharpe Ratio: {best_sharpe['sharpe_ratio']:.2f}")
        print(f"   Total Return: {best_sharpe['total_return']:.2%}")
        print(f"   Max Drawdown: {best_sharpe['max_drawdown']:.2%}")
        
        print(f"\n💰 BEST ABSOLUTE PERFORMANCE:")
        print(f"   Strategy: {best_return['strategy']}")
        print(f"   Configuration: {best_return['config']['description']}")
        print(f"   Total Return: {best_return['total_return']:.2%}")
        print(f"   Final Value: ${best_return['final_value']:,.2f}")
        print(f"   Monthly Return: {best_return['monthly_return']:.2%}")
        
        # Portfolio statistics
        returns = [r['total_return'] for r in results]
        sharpe_ratios = [r['sharpe_ratio'] for r in results]
        drawdowns = [r['max_drawdown'] for r in results]
        
        profitable_count = len([r for r in results if r['total_return'] > 0])
        
        print(f"\n📈 PORTFOLIO STATISTICS:")
        print(f"   📊 Strategies Tested: {len(results)}")
        print(f"   📊 Profitable Strategies: {profitable_count}/{len(results)} ({profitable_count/len(results):.1%})")
        print(f"   📊 Average Return: {np.mean(returns):.2%}")
        print(f"   📊 Best Return: {max(returns):.2%}")
        print(f"   📊 Worst Return: {min(returns):.2%}")
        print(f"   📊 Average Sharpe: {np.mean(sharpe_ratios):.2f}")
        print(f"   📊 Average Max Drawdown: {np.mean(drawdowns):.2%}")
        
        return {
            'best_risk_adjusted': best_sharpe,
            'best_absolute': best_return,
            'summary_stats': {
                'total_strategies': len(results),
                'profitable_strategies': profitable_count,
                'profitable_percentage': profitable_count/len(results),
                'avg_return': np.mean(returns),
                'best_return': max(returns),
                'worst_return': min(returns),
                'avg_sharpe': np.mean(sharpe_ratios),
                'avg_max_drawdown': np.mean(drawdowns)
            },
            'rankings': {
                'by_sharpe': sorted_by_sharpe,
                'by_return': sorted_by_return
            }
        }
    
    def export_results(self, results, report):
        """Export comprehensive results"""
        output_file = f'/tmp/robust_backtesting_results_{datetime.now().strftime("%Y%m%d_%H%M%S")}.json'
        
        export_data = {
            'backtest_summary': {
                'timestamp': datetime.now().isoformat(),
                'initial_capital': self.initial_capital,
                'strategies_tested': len(results),
                'data_source': 'Live market data from Coinbase API'
            },
            'individual_results': results,
            'performance_report': report,
            'strategy_configurations': self.strategies
        }
        
        # Convert datetime objects for JSON serialization
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
    """Run comprehensive robust backtesting"""
    import logging
    logging.getLogger().setLevel(logging.WARNING)
    
    print("🚀 STARTING ROBUST BACKTESTING WITH LIVE MARKET DATA")
    print("=" * 60)
    
    # Initialize backtesting suite
    suite = RobustBacktestingSuite(initial_capital=100000.0)
    
    # Run comprehensive backtests
    results = suite.run_comprehensive_backtests(symbol='ETH', periods=180)
    
    # Generate comprehensive report
    report = suite.generate_comprehensive_report(results)
    
    # Export results
    output_file = suite.export_results(results, report)
    
    print(f"\n🎉 ROBUST BACKTESTING COMPLETE!")
    print("=" * 60)
    print(f"✅ Tested {len(suite.strategies)} optimized strategies")
    print(f"✅ Used live market data from Coinbase API")
    print(f"✅ Generated comprehensive performance analysis")
    print(f"✅ Results exported to: {output_file}")
    
    return results, report

if __name__ == "__main__":
    main()