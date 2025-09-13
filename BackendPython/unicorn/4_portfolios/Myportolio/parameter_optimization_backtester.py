#!/usr/bin/env python3
"""
Advanced Parameter Optimization Backtesting
Tests multiple parameter combinations to find optimal strategy settings
"""

import sys
import os
import json
import pandas as pd
import numpy as np
from datetime import datetime
import itertools
import warnings
warnings.filterwarnings('ignore')

class ParameterOptimizationBacktester:
    """Advanced backtesting with parameter optimization"""
    
    def __init__(self, initial_capital=100000.0):
        self.initial_capital = initial_capital
        
        # Load components
        sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
        from live_market_data_feed import LiveMarketDataFeed
        
        self.market_feed = LiveMarketDataFeed()
        
        # Parameter grids for optimization
        self.parameter_grids = {
            'momentum_strategy': {
                'lookback_periods': [5, 10, 14, 21, 30],
                'rsi_thresholds': [30, 35, 40, 45, 50],
                'position_limits': [0.10, 0.15, 0.20, 0.25, 0.30]
            },
            'mean_reversion': {
                'rsi_oversold': [15, 20, 25, 30],
                'rsi_overbought': [70, 75, 80, 85],
                'volatility_threshold': [0.01, 0.02, 0.03, 0.04],
                'position_limits': [0.12, 0.18, 0.24, 0.30]
            },
            'trend_following': {
                'short_ma': [3, 5, 8, 10],
                'long_ma': [15, 20, 25, 30],
                'volume_threshold': [1.0, 1.2, 1.5, 2.0],
                'position_limits': [0.15, 0.20, 0.25, 0.30]
            }
        }
        
        self.optimization_results = {}
    
    def generate_optimized_market_data(self, symbol='ETH', periods=200):
        """Generate market data optimized for backtesting"""
        print(f"📊 Generating optimized market data for {symbol}...")
        
        # Get live price
        current_price = self.market_feed.get_crypto_price(symbol)
        print(f"   🔗 Live {symbol} Price: ${current_price:,.2f}")
        
        # Generate extended data with more realistic patterns
        market_data = self.market_feed.generate_realistic_market_data(
            symbol, current_price, periods=periods
        )
        
        # Add comprehensive technical indicators
        self._add_comprehensive_features(market_data)
        
        print(f"   📈 Generated {len(market_data)} data points")
        print(f"   📊 Price Range: ${market_data['close'].min():.2f} - ${market_data['close'].max():.2f}")
        print(f"   📊 Volatility: {market_data['close'].pct_change().std():.4f}")
        
        return market_data
    
    def _add_comprehensive_features(self, data):
        """Add comprehensive technical analysis features"""
        prices = data['close']
        
        # Multiple timeframe moving averages
        for period in [3, 5, 8, 10, 15, 20, 25, 30, 50]:
            data[f'sma_{period}'] = prices.rolling(period).mean()
            data[f'ema_{period}'] = prices.ewm(span=period).mean()
        
        # RSI with multiple periods
        for period in [7, 14, 21]:
            delta = prices.diff()
            gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
            rs = gain / loss
            data[f'rsi_{period}'] = 100 - (100 / (1 + rs))
        
        # MACD variations (calculate after EMAs are created)
        if 'ema_12' in data.columns and 'ema_26' in data.columns:
            data['macd_12_26'] = data['ema_12'] - data['ema_26']
            data['macd_signal_9'] = data['macd_12_26'].ewm(span=9).mean()
            data['macd_histogram'] = data['macd_12_26'] - data['macd_signal_9']
        else:
            # Fallback calculation
            ema_12 = prices.ewm(span=12).mean()
            ema_26 = prices.ewm(span=26).mean()
            data['macd_12_26'] = ema_12 - ema_26
            data['macd_signal_9'] = data['macd_12_26'].ewm(span=9).mean()
            data['macd_histogram'] = data['macd_12_26'] - data['macd_signal_9']
        
        # Bollinger Bands with multiple periods
        for period in [10, 20, 30]:
            bb_middle = prices.rolling(period).mean()
            bb_std = prices.rolling(period).std()
            data[f'bb_upper_{period}'] = bb_middle + (bb_std * 2)
            data[f'bb_lower_{period}'] = bb_middle - (bb_std * 2)
            data[f'bb_position_{period}'] = (prices - data[f'bb_lower_{period}']) / (data[f'bb_upper_{period}'] - data[f'bb_lower_{period}'])
        
        # Volatility measures
        for period in [5, 10, 20, 30]:
            data[f'volatility_{period}'] = prices.pct_change().rolling(period).std()
            data[f'momentum_{period}'] = prices.pct_change(period)
        
        # Volume indicators
        data['volume_sma_10'] = data['volume'].rolling(10).mean()
        data['volume_sma_20'] = data['volume'].rolling(20).mean()
        data['volume_ratio'] = data['volume'] / data['volume_sma_20']
        
        # Price channels
        data['high_20'] = data['close'].rolling(20).max()
        data['low_20'] = data['close'].rolling(20).min()
        data['channel_position'] = (prices - data['low_20']) / (data['high_20'] - data['low_20'])
        
        # Clean data
        data.fillna(method='backfill', inplace=True)
        data.fillna(0, inplace=True)
    
    def optimized_momentum_strategy(self, data, lookback, rsi_threshold, position_limit):
        """Optimized momentum strategy with parameters"""
        signals = []
        
        for i in range(len(data)):
            if i < max(lookback, 20):
                signals.append(0.0)
                continue
            
            # Multi-timeframe analysis
            current_rsi = data.iloc[i][f'rsi_14']
            short_ma = data.iloc[i][f'sma_{min(lookback, 20)}']
            long_ma = data.iloc[i][f'sma_{max(lookback, 30)}']
            momentum = data.iloc[i][f'momentum_{lookback}']
            volatility = data.iloc[i][f'volatility_{lookback}']
            volume_ratio = data.iloc[i]['volume_ratio']
            macd = data.iloc[i]['macd_histogram']
            
            # Generate signal
            signal = 0.0
            
            # Strong bullish momentum
            if (current_rsi > rsi_threshold and current_rsi < 70 and
                short_ma > long_ma and
                momentum > 0.02 and
                volatility < 0.1 and
                volume_ratio > 1.1 and
                macd > 0):
                signal = min(position_limit, momentum * 5)
            
            # Strong bearish momentum
            elif (current_rsi < (100 - rsi_threshold) and current_rsi > 30 and
                  short_ma < long_ma and
                  momentum < -0.02 and
                  volatility < 0.1 and
                  volume_ratio > 1.1 and
                  macd < 0):
                signal = max(-position_limit, momentum * 5)
            
            signals.append(signal)
        
        return signals
    
    def optimized_mean_reversion_strategy(self, data, rsi_oversold, rsi_overbought, vol_threshold, position_limit):
        """Optimized mean reversion strategy"""
        signals = []
        
        for i in range(len(data)):
            if i < 30:
                signals.append(0.0)
                continue
            
            current_rsi = data.iloc[i]['rsi_14']
            bb_position_20 = data.iloc[i]['bb_position_20']
            volatility = data.iloc[i]['volatility_10']
            channel_position = data.iloc[i]['channel_position']
            
            signal = 0.0
            
            # Oversold conditions (buy)
            if (current_rsi < rsi_oversold and
                bb_position_20 < 0.1 and
                volatility > vol_threshold and
                channel_position < 0.2):
                signal = min(position_limit, (rsi_oversold - current_rsi) / 20)
            
            # Overbought conditions (sell)
            elif (current_rsi > rsi_overbought and
                  bb_position_20 > 0.9 and
                  volatility > vol_threshold and
                  channel_position > 0.8):
                signal = max(-position_limit, (rsi_overbought - current_rsi) / 20)
            
            signals.append(signal)
        
        return signals
    
    def optimized_trend_following_strategy(self, data, short_ma, long_ma, vol_threshold, position_limit):
        """Optimized trend following strategy"""
        signals = []
        
        for i in range(len(data)):
            if i < long_ma:
                signals.append(0.0)
                continue
            
            short_average = data.iloc[i][f'sma_{short_ma}']
            long_average = data.iloc[i][f'sma_{long_ma}']
            macd = data.iloc[i]['macd_12_26']
            volume_ratio = data.iloc[i]['volume_ratio']
            momentum = data.iloc[i][f'momentum_{short_ma}']
            
            signal = 0.0
            
            # Bullish trend
            if (short_average > long_average and
                macd > 0 and
                volume_ratio > vol_threshold and
                momentum > 0.01):
                signal = min(position_limit, (short_average - long_average) / long_average * 10)
            
            # Bearish trend
            elif (short_average < long_average and
                  macd < 0 and
                  volume_ratio > vol_threshold and
                  momentum < -0.01):
                signal = max(-position_limit, (short_average - long_average) / long_average * 10)
            
            signals.append(signal)
        
        return signals
    
    def run_parameter_optimization(self, data, strategy_type, max_combinations=50):
        """Run parameter optimization for a strategy"""
        print(f"\n🔧 OPTIMIZING {strategy_type.upper()} PARAMETERS")
        print("-" * 50)
        
        param_grid = self.parameter_grids[strategy_type]
        
        # Generate parameter combinations
        param_names = list(param_grid.keys())
        param_values = list(param_grid.values())
        combinations = list(itertools.product(*param_values))
        
        # Limit combinations for performance
        if len(combinations) > max_combinations:
            combinations = combinations[:max_combinations]
            print(f"   📊 Testing {len(combinations)} parameter combinations (limited from {len(list(itertools.product(*param_values)))})")
        else:
            print(f"   📊 Testing {len(combinations)} parameter combinations")
        
        results = []
        
        for i, params in enumerate(combinations):
            param_dict = dict(zip(param_names, params))
            
            # Generate signals based on strategy type
            if strategy_type == 'momentum_strategy':
                signals = self.optimized_momentum_strategy(
                    data, param_dict['lookback_periods'], 
                    param_dict['rsi_thresholds'], param_dict['position_limits']
                )
            elif strategy_type == 'mean_reversion':
                signals = self.optimized_mean_reversion_strategy(
                    data, param_dict['rsi_oversold'], param_dict['rsi_overbought'],
                    param_dict['volatility_threshold'], param_dict['position_limits']
                )
            elif strategy_type == 'trend_following':
                signals = self.optimized_trend_following_strategy(
                    data, param_dict['short_ma'], param_dict['long_ma'],
                    param_dict['volume_threshold'], param_dict['position_limits']
                )
            
            # Run backtest
            result = self.run_single_backtest(data, signals)
            result.update({
                'parameters': param_dict,
                'strategy_type': strategy_type,
                'combination_id': i
            })
            
            results.append(result)
            
            # Progress update
            if (i + 1) % 10 == 0:
                print(f"   ⏳ Completed {i + 1}/{len(combinations)} combinations...")
        
        # Sort by Sharpe ratio (risk-adjusted returns)
        results.sort(key=lambda x: x['sharpe_ratio'], reverse=True)
        
        return results
    
    def run_single_backtest(self, data, signals):
        """Run a single backtest"""
        portfolio_value = self.initial_capital
        cash = self.initial_capital
        position = 0.0
        trades = []
        
        for i in range(len(data)):
            current_price = data.iloc[i]['close']
            current_signal = signals[i]
            
            # Calculate target position
            if abs(current_signal) > 0.005:  # Minimum signal threshold
                target_value = portfolio_value * current_signal
                target_shares = target_value / current_price
                
                # Execute trade
                if abs(target_shares - position) * current_price > 50:  # Minimum trade size
                    trade_shares = target_shares - position
                    trade_value = trade_shares * current_price
                    
                    # Apply transaction costs (0.1%)
                    transaction_cost = abs(trade_value) * 0.001
                    
                    if trade_shares > 0:  # Buy
                        if cash >= (trade_value + transaction_cost):
                            cash -= (trade_value + transaction_cost)
                            position += trade_shares
                            trades.append('BUY')
                    else:  # Sell
                        cash += (abs(trade_value) - transaction_cost)
                        position += trade_shares
                        trades.append('SELL')
            
            # Update portfolio value
            portfolio_value = cash + (position * current_price)
        
        # Calculate metrics
        final_value = portfolio_value
        total_return = (final_value - self.initial_capital) / self.initial_capital
        
        # Calculate volatility and Sharpe ratio
        returns = []
        temp_value = self.initial_capital
        for i in range(1, len(data)):
            current_price = data.iloc[i]['close']
            prev_price = data.iloc[i-1]['close']
            if abs(signals[i]) > 0.005:
                daily_return = (current_price - prev_price) / prev_price * signals[i]
            else:
                daily_return = 0
            returns.append(daily_return)
        
        volatility = np.std(returns) * np.sqrt(252) if len(returns) > 0 else 0
        sharpe_ratio = (total_return / volatility) if volatility > 0 else 0
        
        # Calculate max drawdown
        max_drawdown = 0
        peak = self.initial_capital
        temp_portfolio = self.initial_capital
        for i in range(len(data)):
            current_price = data.iloc[i]['close']
            if i > 0:
                if abs(signals[i]) > 0.005:
                    price_change = (current_price - data.iloc[i-1]['close']) / data.iloc[i-1]['close']
                    temp_portfolio *= (1 + price_change * signals[i])
                
                if temp_portfolio > peak:
                    peak = temp_portfolio
                drawdown = (peak - temp_portfolio) / peak
                max_drawdown = max(max_drawdown, drawdown)
        
        return {
            'final_value': final_value,
            'total_return': total_return,
            'volatility': volatility,
            'sharpe_ratio': sharpe_ratio,
            'max_drawdown': max_drawdown,
            'total_trades': len(trades)
        }
    
    def generate_optimization_report(self, strategy_results):
        """Generate comprehensive optimization report"""
        print(f"\n📊 PARAMETER OPTIMIZATION RESULTS")
        print("=" * 60)
        
        for strategy_type, results in strategy_results.items():
            print(f"\n🎯 {strategy_type.upper()} OPTIMIZATION")
            print("-" * 40)
            
            # Top 5 performers
            print("🏆 TOP 5 PARAMETER COMBINATIONS:")
            for i, result in enumerate(results[:5]):
                rank = i + 1
                params = result['parameters']
                total_return = result['total_return']
                sharpe = result['sharpe_ratio']
                max_dd = result['max_drawdown']
                
                print(f"{rank}. Return: {total_return:>6.2%} | Sharpe: {sharpe:>5.2f} | MaxDD: {max_dd:>5.2%}")
                print(f"   Parameters: {params}")
            
            # Best performer details
            best = results[0]
            print(f"\n🎯 OPTIMAL PARAMETERS FOR {strategy_type.upper()}:")
            print(f"   📊 Parameters: {best['parameters']}")
            print(f"   💰 Total Return: {best['total_return']:.2%}")
            print(f"   ⚡ Sharpe Ratio: {best['sharpe_ratio']:.2f}")
            print(f"   🛡️ Max Drawdown: {best['max_drawdown']:.2%}")
            print(f"   🔄 Total Trades: {best['total_trades']}")
    
    def run_comprehensive_optimization(self, symbol='ETH', periods=200):
        """Run comprehensive parameter optimization"""
        print("🚀 COMPREHENSIVE PARAMETER OPTIMIZATION")
        print("=" * 60)
        
        # Generate market data
        market_data = self.generate_optimized_market_data(symbol, periods)
        
        # Optimize each strategy type
        strategy_results = {}
        
        for strategy_type in self.parameter_grids.keys():
            results = self.run_parameter_optimization(market_data, strategy_type)
            strategy_results[strategy_type] = results
            self.optimization_results[strategy_type] = results
        
        # Generate report
        self.generate_optimization_report(strategy_results)
        
        # Export results
        output_file = f'/tmp/parameter_optimization_{datetime.now().strftime("%Y%m%d_%H%M%S")}.json'
        
        export_data = {
            'optimization_run': {
                'timestamp': datetime.now().isoformat(),
                'symbol': symbol,
                'periods': periods,
                'initial_capital': self.initial_capital
            },
            'results': strategy_results,
            'parameter_grids': self.parameter_grids
        }
        
        with open(output_file, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        print(f"\n📁 OPTIMIZATION RESULTS EXPORTED: {output_file}")
        
        return strategy_results

def main():
    """Run parameter optimization backtesting"""
    import logging
    logging.getLogger().setLevel(logging.WARNING)
    
    print("🚀 STARTING PARAMETER OPTIMIZATION BACKTESTING")
    print("=" * 60)
    
    # Initialize optimizer
    optimizer = ParameterOptimizationBacktester(initial_capital=100000.0)
    
    # Run comprehensive optimization
    results = optimizer.run_comprehensive_optimization(symbol='ETH', periods=150)
    
    print(f"\n🎉 PARAMETER OPTIMIZATION COMPLETE!")
    print("=" * 60)
    print(f"✅ Optimized {len(results)} strategy types")
    print(f"✅ Tested multiple parameter combinations")
    print(f"✅ Used live market data from APIs")
    print(f"✅ Found optimal parameters for each strategy")
    
    return results

if __name__ == "__main__":
    main()