#!/usr/bin/env python3
"""
Advanced Multi-Asset Multi-Timeframe Trading Strategy
Manages risk across 6 positions: ETH (1min, 1hour, 1day) + BTC (1min, 1hour, 1day)
Implements long, mid, and short holding strategies with portfolio-level risk management
"""

import os
import sys
import json
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from enum import Enum
import logging

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Add paths
current_dir = os.path.dirname(os.path.abspath(__file__))
portfolio_dir = os.path.dirname(current_dir)
sys.path.extend([current_dir, portfolio_dir])

try:
    from dual_crypto_portfolio_manager import DualCryptoPortfolioManager
    from btc_model_manager import BTCModelManager
except ImportError as e:
    logger.warning(f"Import warning: {e}")

class HoldingStrategy(Enum):
    """Enumeration of holding strategies based on timeframe"""
    SHORT = "short"    # 1min - Scalping (minutes to hours)
    MID = "mid"        # 1hour - Swing trading (hours to days)  
    LONG = "long"      # 1day - Position trading (days to weeks)

class AssetTimeframe(Enum):
    """Asset-Timeframe combinations"""
    ETH_1MIN = "ETH_1min"
    ETH_1HOUR = "ETH_1hour"
    ETH_1DAY = "ETH_1day"
    BTC_1MIN = "BTC_1min"
    BTC_1HOUR = "BTC_1hour"
    BTC_1DAY = "BTC_1day"

@dataclass
class PositionSignal:
    """Signal for a specific asset-timeframe position"""
    asset: str
    timeframe: str
    strategy: HoldingStrategy
    signal: str  # BUY, SELL, HOLD
    confidence: float
    target_allocation: float
    risk_score: float
    expected_return: float
    expected_volatility: float
    holding_period: str
    stop_loss: float
    take_profit: float
    timestamp: datetime

@dataclass
class PortfolioRisk:
    """Portfolio-level risk metrics"""
    total_exposure: float
    concentration_risk: float
    correlation_risk: float
    volatility_risk: float
    drawdown_risk: float
    leverage_ratio: float
    diversification_score: float
    risk_budget_utilization: float

class AdvancedMultiAssetStrategy:
    """
    Advanced strategy managing 6 positions with sophisticated risk controls
    """
    
    def __init__(self):
        """Initialize advanced multi-asset strategy"""
        
        # Initialize managers
        try:
            self.dual_manager = DualCryptoPortfolioManager()
            self.btc_manager = BTCModelManager()
        except:
            self.dual_manager = None
            self.btc_manager = None
            logger.warning("Portfolio managers not available - using simulation mode")
        
        # Strategy configuration
        self.config = {
            'max_total_exposure': 0.95,      # Max 95% invested
            'max_single_asset': 0.70,        # Max 70% in one asset
            'max_single_timeframe': 0.40,    # Max 40% in one timeframe
            'min_diversification': 0.20,     # Min 20% diversification score
            'correlation_threshold': 0.85,   # Max correlation between positions
            'volatility_limit': 0.35,        # Max portfolio volatility
            'drawdown_limit': 0.20,          # Max portfolio drawdown
            'risk_budget': 1.0,              # Total risk budget
            'rebalance_threshold': 0.05      # 5% drift triggers rebalance
        }
        
        # Holding strategy parameters
        self.holding_strategies = {
            HoldingStrategy.SHORT: {
                'timeframes': ['1min'],
                'holding_period': '5min-2hour',
                'max_allocation': 0.15,      # Max 15% in scalping
                'stop_loss': 0.02,           # 2% stop loss
                'take_profit': 0.03,         # 3% take profit
                'confidence_threshold': 0.75,
                'risk_multiplier': 0.5       # Conservative for high frequency
            },
            HoldingStrategy.MID: {
                'timeframes': ['1hour'],
                'holding_period': '1hour-3day',
                'max_allocation': 0.50,      # Max 50% in swing trading
                'stop_loss': 0.05,           # 5% stop loss
                'take_profit': 0.08,         # 8% take profit
                'confidence_threshold': 0.65,
                'risk_multiplier': 0.75      # Moderate risk
            },
            HoldingStrategy.LONG: {
                'timeframes': ['1day'],
                'holding_period': '3day-4week',
                'max_allocation': 0.70,      # Max 70% in position trading
                'stop_loss': 0.15,           # 15% stop loss
                'take_profit': 0.25,         # 25% take profit
                'confidence_threshold': 0.60,
                'risk_multiplier': 1.0       # Full Kelly allocation
            }
        }
        
        # Position tracking
        self.current_positions = {
            AssetTimeframe.ETH_1MIN: 0.0,
            AssetTimeframe.ETH_1HOUR: 0.0,
            AssetTimeframe.ETH_1DAY: 0.0,
            AssetTimeframe.BTC_1MIN: 0.0,
            AssetTimeframe.BTC_1HOUR: 0.0,
            AssetTimeframe.BTC_1DAY: 0.0
        }
        
        self.signals_history = []
        self.performance_history = []
        
        logger.info("Advanced Multi-Asset Strategy initialized")
    
    def get_eth_signals_all_timeframes(self) -> Dict[str, PositionSignal]:
        """Get ETH signals for all timeframes"""
        
        signals = {}
        
        # Simulate ETH signals (in production, integrate with actual ETH models)
        timeframes = ['1min', '1hour', '1day']
        
        for timeframe in timeframes:
            try:
                # Simulate ETH signal generation
                strategy = self._get_holding_strategy(timeframe)
                
                # Mock signal based on timeframe characteristics
                if timeframe == '1min':
                    signal_strength = np.random.uniform(0.6, 0.85)
                    signal_direction = np.random.choice(['BUY', 'SELL', 'HOLD'], p=[0.4, 0.3, 0.3])
                    volatility = 0.08
                elif timeframe == '1hour':
                    signal_strength = np.random.uniform(0.65, 0.80)
                    signal_direction = np.random.choice(['BUY', 'SELL', 'HOLD'], p=[0.45, 0.25, 0.3])
                    volatility = 0.12
                else:  # 1day
                    signal_strength = np.random.uniform(0.60, 0.75)
                    signal_direction = np.random.choice(['BUY', 'SELL', 'HOLD'], p=[0.5, 0.2, 0.3])
                    volatility = 0.18
                
                signal = PositionSignal(
                    asset='ETH',
                    timeframe=timeframe,
                    strategy=strategy,
                    signal=signal_direction,
                    confidence=signal_strength,
                    target_allocation=self._calculate_target_allocation('ETH', timeframe, signal_strength),
                    risk_score=self._calculate_risk_score(volatility, signal_strength),
                    expected_return=0.12 if signal_direction == 'BUY' else -0.08,
                    expected_volatility=volatility,
                    holding_period=self.holding_strategies[strategy]['holding_period'],
                    stop_loss=self.holding_strategies[strategy]['stop_loss'],
                    take_profit=self.holding_strategies[strategy]['take_profit'],
                    timestamp=datetime.now()
                )
                
                signals[f'ETH_{timeframe}'] = signal
                
            except Exception as e:
                logger.error(f"Error generating ETH signal for {timeframe}: {e}")
        
        return signals
    
    def get_btc_signals_all_timeframes(self) -> Dict[str, PositionSignal]:
        """Get Bitcoin signals for all timeframes"""
        
        signals = {}
        
        # Get actual Bitcoin signals if manager available
        if self.btc_manager is not None:
            try:
                btc_recommendation = self.btc_manager.get_portfolio_recommendation()
                
                if 'error' not in btc_recommendation:
                    # Extract multi-timeframe analysis
                    multi_tf = btc_recommendation.get('multi_timeframe_analysis', [])
                    
                    for tf_rec in multi_tf:
                        timeframe = tf_rec['timeframe']
                        strategy = self._get_holding_strategy(timeframe)
                        
                        signal = PositionSignal(
                            asset='BTC',
                            timeframe=timeframe,
                            strategy=strategy,
                            signal=tf_rec['signal'],
                            confidence=tf_rec['confidence'],
                            target_allocation=tf_rec['target_allocation'],
                            risk_score=self._calculate_risk_score(0.15, tf_rec['confidence']),
                            expected_return=0.10 if tf_rec['signal'] == 'BUY' else -0.06,
                            expected_volatility=0.20,
                            holding_period=self.holding_strategies[strategy]['holding_period'],
                            stop_loss=self.holding_strategies[strategy]['stop_loss'],
                            take_profit=self.holding_strategies[strategy]['take_profit'],
                            timestamp=datetime.now()
                        )
                        
                        signals[f'BTC_{timeframe}'] = signal
                
            except Exception as e:
                logger.error(f"Error getting Bitcoin signals: {e}")
        
        # Fallback: simulate Bitcoin signals
        if not signals:
            timeframes = ['1hour', '1day']  # Bitcoin models available for these
            
            for timeframe in timeframes:
                strategy = self._get_holding_strategy(timeframe)
                
                signal = PositionSignal(
                    asset='BTC',
                    timeframe=timeframe,
                    strategy=strategy,
                    signal=np.random.choice(['BUY', 'SELL', 'HOLD'], p=[0.4, 0.3, 0.3]),
                    confidence=np.random.uniform(0.60, 0.75),
                    target_allocation=self._calculate_target_allocation('BTC', timeframe, 0.65),
                    risk_score=self._calculate_risk_score(0.20, 0.65),
                    expected_return=0.08,
                    expected_volatility=0.22,
                    holding_period=self.holding_strategies[strategy]['holding_period'],
                    stop_loss=self.holding_strategies[strategy]['stop_loss'],
                    take_profit=self.holding_strategies[strategy]['take_profit'],
                    timestamp=datetime.now()
                )
                
                signals[f'BTC_{timeframe}'] = signal
        
        return signals
    
    def _get_holding_strategy(self, timeframe: str) -> HoldingStrategy:
        """Map timeframe to holding strategy"""
        
        if timeframe == '1min':
            return HoldingStrategy.SHORT
        elif timeframe == '1hour':
            return HoldingStrategy.MID
        elif timeframe == '1day':
            return HoldingStrategy.LONG
        else:
            return HoldingStrategy.MID  # Default
    
    def _calculate_target_allocation(self, asset: str, timeframe: str, confidence: float) -> float:
        """Calculate target allocation for asset-timeframe combination"""
        
        strategy = self._get_holding_strategy(timeframe)
        max_allocation = self.holding_strategies[strategy]['max_allocation']
        base_allocation = 0.6 if asset == 'ETH' else 0.4  # Portfolio weights
        
        # Adjust by timeframe
        if timeframe == '1min':
            timeframe_weight = 0.2
        elif timeframe == '1hour':
            timeframe_weight = 0.4
        else:  # 1day
            timeframe_weight = 0.6
        
        # Calculate allocation
        allocation = base_allocation * timeframe_weight * confidence
        allocation = min(allocation, max_allocation)
        
        return allocation
    
    def _calculate_risk_score(self, volatility: float, confidence: float) -> float:
        """Calculate risk score for position"""
        
        # Higher volatility = higher risk
        # Lower confidence = higher risk
        risk_score = volatility * (2 - confidence)
        return min(1.0, max(0.0, risk_score))
    
    def calculate_portfolio_risk(self, signals: Dict[str, PositionSignal]) -> PortfolioRisk:
        """Calculate comprehensive portfolio risk metrics"""
        
        total_exposure = sum(signal.target_allocation for signal in signals.values())
        
        # Concentration risk (how concentrated in single asset/timeframe)
        eth_exposure = sum(s.target_allocation for s in signals.values() if s.asset == 'ETH')
        btc_exposure = sum(s.target_allocation for s in signals.values() if s.asset == 'BTC')
        max_asset_exposure = max(eth_exposure, btc_exposure)
        concentration_risk = max_asset_exposure / max(total_exposure, 0.01)
        
        # Timeframe concentration
        short_exposure = sum(s.target_allocation for s in signals.values() if s.strategy == HoldingStrategy.SHORT)
        mid_exposure = sum(s.target_allocation for s in signals.values() if s.strategy == HoldingStrategy.MID)
        long_exposure = sum(s.target_allocation for s in signals.values() if s.strategy == HoldingStrategy.LONG)
        max_tf_exposure = max(short_exposure, mid_exposure, long_exposure)
        
        # Correlation risk (simplified)
        correlation_risk = 0.65  # Typical BTC-ETH correlation
        
        # Volatility risk (portfolio weighted volatility)
        portfolio_volatility = 0
        if total_exposure > 0:
            for signal in signals.values():
                weight = signal.target_allocation / total_exposure
                portfolio_volatility += weight * signal.expected_volatility
        
        # Diversification score
        num_positions = len([s for s in signals.values() if s.target_allocation > 0.01])
        diversification_score = min(1.0, num_positions / 6)  # Max 6 positions
        
        # Risk budget utilization
        risk_budget_used = sum(s.risk_score * s.target_allocation for s in signals.values())
        
        return PortfolioRisk(
            total_exposure=total_exposure,
            concentration_risk=concentration_risk,
            correlation_risk=correlation_risk,
            volatility_risk=portfolio_volatility,
            drawdown_risk=portfolio_volatility * 2,  # Estimate
            leverage_ratio=total_exposure,
            diversification_score=diversification_score,
            risk_budget_utilization=risk_budget_used
        )
    
    def optimize_allocations(self, signals: Dict[str, PositionSignal]) -> Dict[str, float]:
        """Optimize allocations across all positions considering risk constraints"""
        
        # Calculate portfolio risk
        portfolio_risk = self.calculate_portfolio_risk(signals)
        
        # Check risk constraints
        risk_adjustments = {}
        
        # Total exposure constraint
        if portfolio_risk.total_exposure > self.config['max_total_exposure']:
            scale_factor = self.config['max_total_exposure'] / portfolio_risk.total_exposure
            for key, signal in signals.items():
                risk_adjustments[key] = signal.target_allocation * scale_factor
        else:
            for key, signal in signals.items():
                risk_adjustments[key] = signal.target_allocation
        
        # Concentration constraints
        eth_total = sum(adj for key, adj in risk_adjustments.items() if 'ETH' in key)
        btc_total = sum(adj for key, adj in risk_adjustments.items() if 'BTC' in key)
        
        if eth_total > self.config['max_single_asset']:
            eth_scale = self.config['max_single_asset'] / eth_total
            for key in risk_adjustments:
                if 'ETH' in key:
                    risk_adjustments[key] *= eth_scale
        
        if btc_total > self.config['max_single_asset']:
            btc_scale = self.config['max_single_asset'] / btc_total
            for key in risk_adjustments:
                if 'BTC' in key:
                    risk_adjustments[key] *= btc_scale
        
        # Timeframe constraints
        for strategy in HoldingStrategy:
            strategy_total = sum(
                adj for key, adj in risk_adjustments.items() 
                if signals[key].strategy == strategy
            )
            max_strategy = self.holding_strategies[strategy]['max_allocation']
            
            if strategy_total > max_strategy:
                strategy_scale = max_strategy / strategy_total
                for key in risk_adjustments:
                    if signals[key].strategy == strategy:
                        risk_adjustments[key] *= strategy_scale
        
        return risk_adjustments
    
    def generate_portfolio_recommendation(self) -> Dict:
        """Generate comprehensive portfolio recommendation across all positions"""
        
        try:
            # Get signals for all positions
            eth_signals = self.get_eth_signals_all_timeframes()
            btc_signals = self.get_btc_signals_all_timeframes()
            
            all_signals = {**eth_signals, **btc_signals}
            
            # Calculate portfolio risk
            portfolio_risk = self.calculate_portfolio_risk(all_signals)
            
            # Optimize allocations
            optimized_allocations = self.optimize_allocations(all_signals)
            
            # Calculate changes from current positions
            allocation_changes = {}
            actions = {}
            
            for key, new_allocation in optimized_allocations.items():
                # Map key to AssetTimeframe enum
                try:
                    asset_tf = AssetTimeframe(key.replace('_', '_'))
                    current = self.current_positions.get(asset_tf, 0.0)
                except:
                    current = 0.0
                
                change = new_allocation - current
                allocation_changes[key] = change
                
                if abs(change) > self.config['rebalance_threshold']:
                    actions[key] = 'INCREASE' if change > 0 else 'DECREASE'
                else:
                    actions[key] = 'HOLD'
            
            # Overall portfolio action
            significant_changes = [k for k, v in actions.items() if v != 'HOLD']
            overall_action = 'REBALANCE' if significant_changes else 'HOLD'
            
            # Risk assessment
            risk_within_limits = (
                portfolio_risk.total_exposure <= self.config['max_total_exposure'] and
                portfolio_risk.concentration_risk <= 0.8 and
                portfolio_risk.volatility_risk <= self.config['volatility_limit'] and
                portfolio_risk.diversification_score >= self.config['min_diversification']
            )
            
            return {
                'timestamp': datetime.now().isoformat(),
                'signals': {key: {
                    'asset': signal.asset,
                    'timeframe': signal.timeframe,
                    'strategy': signal.strategy.value,
                    'signal': signal.signal,
                    'confidence': signal.confidence,
                    'holding_period': signal.holding_period
                } for key, signal in all_signals.items()},
                'current_allocations': dict(self.current_positions),
                'optimized_allocations': optimized_allocations,
                'allocation_changes': allocation_changes,
                'actions': actions,
                'overall_action': overall_action,
                'portfolio_risk': {
                    'total_exposure': portfolio_risk.total_exposure,
                    'concentration_risk': portfolio_risk.concentration_risk,
                    'correlation_risk': portfolio_risk.correlation_risk,
                    'volatility_risk': portfolio_risk.volatility_risk,
                    'diversification_score': portfolio_risk.diversification_score,
                    'risk_within_limits': risk_within_limits
                },
                'strategy_breakdown': {
                    'short_term': sum(opt for key, opt in optimized_allocations.items() 
                                    if all_signals[key].strategy == HoldingStrategy.SHORT),
                    'mid_term': sum(opt for key, opt in optimized_allocations.items() 
                                  if all_signals[key].strategy == HoldingStrategy.MID),
                    'long_term': sum(opt for key, opt in optimized_allocations.items() 
                                   if all_signals[key].strategy == HoldingStrategy.LONG)
                },
                'performance_metrics': {
                    'expected_return': sum(signal.expected_return * optimized_allocations[key] 
                                         for key, signal in all_signals.items()),
                    'expected_volatility': portfolio_risk.volatility_risk,
                    'risk_adjusted_return': 0.0  # To be calculated
                }
            }
            
        except Exception as e:
            logger.error(f"Error generating portfolio recommendation: {e}")
            return {'error': str(e)}
    
    def update_positions(self, new_allocations: Dict[str, float]):
        """Update current positions"""
        
        for key, allocation in new_allocations.items():
            try:
                # Map key to AssetTimeframe enum
                asset_tf = AssetTimeframe(key.replace('_', '_'))
                self.current_positions[asset_tf] = allocation
            except:
                logger.warning(f"Could not map position key: {key}")
        
        logger.info("Positions updated")

def main():
    """Main function for advanced multi-asset strategy"""
    
    strategy = AdvancedMultiAssetStrategy()
    
    print("🎯 ADVANCED MULTI-ASSET MULTI-TIMEFRAME STRATEGY")
    print("=" * 70)
    
    # Generate portfolio recommendation
    print("Analyzing 6-position portfolio (ETH + BTC across 3 timeframes)...")
    recommendation = strategy.generate_portfolio_recommendation()
    
    if 'error' not in recommendation:
        print(f"\n📊 Portfolio Recommendation:")
        print(f"Overall Action: {recommendation['overall_action']}")
        print(f"Risk Within Limits: {recommendation['portfolio_risk']['risk_within_limits']}")
        
        # Show strategy breakdown
        strategy_breakdown = recommendation['strategy_breakdown']
        print(f"\n⏱️ Strategy Breakdown:")
        print(f"  Short-term (1min): {strategy_breakdown['short_term']:.1%}")
        print(f"  Mid-term (1hour): {strategy_breakdown['mid_term']:.1%}")
        print(f"  Long-term (1day): {strategy_breakdown['long_term']:.1%}")
        
        # Show optimized allocations
        print(f"\n🎯 Optimized Allocations:")
        for position, allocation in recommendation['optimized_allocations'].items():
            change = recommendation['allocation_changes'][position]
            action = recommendation['actions'][position]
            signal_info = recommendation['signals'][position]
            print(f"  {position}: {allocation:.1%} ({change:+.1%}) - {action}")
            print(f"    Signal: {signal_info['signal']} (confidence: {signal_info['confidence']:.2f})")
            print(f"    Strategy: {signal_info['strategy']} ({signal_info['holding_period']})")
        
        # Risk metrics
        risk = recommendation['portfolio_risk']
        print(f"\n🛡️ Portfolio Risk Metrics:")
        print(f"  Total Exposure: {risk['total_exposure']:.1%}")
        print(f"  Concentration Risk: {risk['concentration_risk']:.1%}")
        print(f"  Volatility Risk: {risk['volatility_risk']:.1%}")
        print(f"  Diversification Score: {risk['diversification_score']:.2f}")
        
        # Performance metrics
        perf = recommendation['performance_metrics']
        print(f"\n📈 Expected Performance:")
        print(f"  Expected Return: {perf['expected_return']:.1%}")
        print(f"  Expected Volatility: {perf['expected_volatility']:.1%}")
        
    else:
        print(f"❌ Error: {recommendation['error']}")
    
    print("\n🎯 Advanced multi-asset strategy analysis complete!")

if __name__ == "__main__":
    main()
