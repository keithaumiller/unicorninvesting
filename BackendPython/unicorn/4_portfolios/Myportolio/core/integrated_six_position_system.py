#!/usr/bin/env python3
"""
Integrated Six Position Trading System
Combines advanced multi-asset strategy with comprehensive risk management
Manages ETH + BTC across 1min, 1hour, 1day timeframes with risk controls
"""

import os
import sys
import json
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
import logging

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Add paths
current_dir = os.path.dirname(os.path.abspath(__file__))
portfolio_dir = os.path.dirname(current_dir)
trading_dir = os.path.join(portfolio_dir, 'trading_algorithms')
risk_dir = os.path.join(portfolio_dir, 'risk_algorithms')

sys.path.extend([current_dir, portfolio_dir, trading_dir, risk_dir])

try:
    from advanced_multi_asset_strategy import AdvancedMultiAssetStrategy
    from six_position_risk_manager import SixPositionRiskManager
except ImportError as e:
    logger.warning(f"Import warning: {e}")

class IntegratedSixPositionSystem:
    """
    Integrated trading system managing 6 positions with comprehensive risk controls
    """
    
    def __init__(self):
        """Initialize integrated system"""
        
        # Initialize components
        try:
            self.strategy = AdvancedMultiAssetStrategy()
            self.risk_manager = SixPositionRiskManager()
        except:
            self.strategy = None
            self.risk_manager = None
            logger.warning("Could not initialize components - using simulation mode")
        
        # System configuration
        self.system_config = {
            'max_iterations': 5,        # Max risk adjustment iterations
            'risk_tolerance': 'moderate', # conservative, moderate, aggressive
            'auto_rebalance': True,     # Automatic rebalancing
            'emergency_stop_loss': 0.25, # Emergency portfolio stop loss
            'position_limits': {
                'min_position': 0.01,   # Minimum position size (1%)
                'max_position': 0.30,   # Maximum single position (30%)
                'min_total_crypto': 0.20, # Minimum crypto exposure
                'max_total_crypto': 0.95  # Maximum crypto exposure
            }
        }
        
        # Performance tracking
        self.performance_history = []
        self.rebalance_history = []
        self.risk_events = []
        
        # Current state
        self.last_update = None
        self.current_positions = {}
        self.system_status = 'initialized'
        
        logger.info("Integrated Six Position System initialized")
    
    def generate_signals_with_risk_control(self) -> Dict:
        """Generate signals with integrated risk control"""
        
        try:
            if self.strategy is None or self.risk_manager is None:
                return {'error': 'Strategy or risk manager not available'}
            
            # Step 1: Generate initial portfolio recommendation
            initial_recommendation = self.strategy.generate_portfolio_recommendation()
            
            if 'error' in initial_recommendation:
                return initial_recommendation
            
            # Step 2: Extract allocations for risk analysis
            initial_allocations = initial_recommendation['optimized_allocations']
            
            # Step 3: Risk validation and adjustment
            risk_adjusted_result = self._adjust_for_risk_limits(initial_allocations)
            
            # Step 4: Final portfolio construction
            final_recommendation = self._build_final_recommendation(
                initial_recommendation, 
                risk_adjusted_result
            )
            
            return final_recommendation
            
        except Exception as e:
            logger.error(f"Error in integrated signal generation: {e}")
            return {'error': str(e)}
    
    def _adjust_for_risk_limits(self, allocations: Dict[str, float]) -> Dict:
        """Adjust allocations to meet risk limits through iterative process"""
        
        adjusted_allocations = allocations.copy()
        iterations = 0
        adjustment_history = []
        
        while iterations < self.system_config['max_iterations']:
            # Validate current allocations
            risk_validation = self.risk_manager.validate_risk_limits(adjusted_allocations)
            
            if risk_validation['risk_valid']:
                # Risk limits satisfied
                break
            
            # Get risk adjustments
            adjustment_result = self.risk_manager.suggest_risk_adjustments(adjusted_allocations)
            
            if not adjustment_result['adjustments_needed']:
                break
            
            # Apply adjustments
            adjusted_allocations = adjustment_result['adjusted_allocations']
            adjustment_history.extend(adjustment_result['adjustments_made'])
            
            iterations += 1
        
        # Generate final risk report
        final_risk_report = self.risk_manager.generate_risk_report(adjusted_allocations)
        
        return {
            'original_allocations': allocations,
            'adjusted_allocations': adjusted_allocations,
            'iterations_required': iterations,
            'adjustment_history': adjustment_history,
            'final_risk_report': final_risk_report,
            'risk_adjusted': iterations > 0
        }
    
    def _build_final_recommendation(self, initial_rec: Dict, risk_result: Dict) -> Dict:
        """Build final recommendation combining strategy and risk outputs"""
        
        # Base recommendation from strategy
        final_rec = initial_rec.copy()
        
        # Update with risk-adjusted allocations
        final_rec['risk_adjusted_allocations'] = risk_result['adjusted_allocations']
        final_rec['risk_adjustment_made'] = risk_result['risk_adjusted']
        final_rec['risk_iterations'] = risk_result['iterations_required']
        final_rec['risk_adjustments'] = risk_result['adjustment_history']
        
        # Update allocation changes based on risk-adjusted allocations
        current_positions = self.current_positions or {}
        final_changes = {}
        final_actions = {}
        
        for position, new_allocation in risk_result['adjusted_allocations'].items():
            current = current_positions.get(position, 0.0)
            change = new_allocation - current
            final_changes[position] = change
            
            if abs(change) > 0.05:  # 5% threshold
                final_actions[position] = 'INCREASE' if change > 0 else 'DECREASE'
            else:
                final_actions[position] = 'HOLD'
        
        final_rec['final_allocation_changes'] = final_changes
        final_rec['final_actions'] = final_actions
        
        # Overall action
        significant_changes = [k for k, v in final_actions.items() if v != 'HOLD']
        final_rec['final_overall_action'] = 'REBALANCE' if significant_changes else 'HOLD'
        
        # Risk metrics from final risk report
        risk_report = risk_result['final_risk_report']
        final_rec['final_risk_metrics'] = {
            'risk_status': risk_report['risk_dashboard']['overall_risk_status'],
            'risk_score': risk_report['risk_dashboard']['risk_score'],
            'total_exposure': risk_report['portfolio_metrics']['total_exposure'],
            'portfolio_volatility': risk_report['portfolio_metrics']['portfolio_volatility'],
            'portfolio_var': risk_report['portfolio_metrics']['portfolio_var'],
            'diversification_score': risk_report['portfolio_metrics']['diversification_score'],
            'violations': risk_report['risk_validation']['violations'],
            'warnings': risk_report['risk_validation']['warnings']
        }
        
        # Performance estimates (risk-adjusted)
        expected_return = sum(
            initial_rec['signals'][pos]['signal'] == 'BUY' and 0.08 or 
            initial_rec['signals'][pos]['signal'] == 'SELL' and -0.04 or 0.02
            for pos in risk_result['adjusted_allocations']
            if pos in initial_rec['signals']
        ) / len(risk_result['adjusted_allocations'])
        
        final_rec['risk_adjusted_performance'] = {
            'expected_return': expected_return,
            'expected_volatility': risk_report['portfolio_metrics']['portfolio_volatility'],
            'risk_adjusted_return': expected_return / max(0.01, risk_report['portfolio_metrics']['portfolio_volatility']),
            'sharpe_estimate': expected_return / max(0.01, risk_report['portfolio_metrics']['portfolio_volatility'])
        }
        
        return final_rec
    
    def execute_rebalancing(self, recommendation: Dict) -> Dict:
        """Execute portfolio rebalancing based on recommendation"""
        
        try:
            if recommendation['final_overall_action'] == 'HOLD':
                return {
                    'rebalancing_executed': False,
                    'reason': 'No significant changes required',
                    'current_positions': self.current_positions
                }
            
            # Check emergency conditions
            emergency_check = self._check_emergency_conditions(recommendation)
            if emergency_check['emergency_detected']:
                return {
                    'rebalancing_executed': False,
                    'emergency_stop': True,
                    'reason': emergency_check['reason'],
                    'emergency_actions': emergency_check['actions']
                }
            
            # Execute position changes
            new_positions = recommendation['risk_adjusted_allocations'].copy()
            executed_trades = []
            
            for position, target_allocation in new_positions.items():
                current_allocation = self.current_positions.get(position, 0.0)
                change = target_allocation - current_allocation
                
                if abs(change) > 0.01:  # Only execute meaningful changes
                    trade = {
                        'position': position,
                        'action': 'BUY' if change > 0 else 'SELL',
                        'from_allocation': current_allocation,
                        'to_allocation': target_allocation,
                        'change': change,
                        'timestamp': datetime.now().isoformat()
                    }
                    executed_trades.append(trade)
            
            # Update positions
            self.current_positions = new_positions
            self.last_update = datetime.now()
            
            # Record rebalancing event
            rebalance_event = {
                'timestamp': datetime.now().isoformat(),
                'trades_executed': len(executed_trades),
                'total_turnover': sum(abs(trade['change']) for trade in executed_trades),
                'new_total_exposure': sum(new_positions.values()),
                'risk_status': recommendation['final_risk_metrics']['risk_status']
            }
            self.rebalance_history.append(rebalance_event)
            
            return {
                'rebalancing_executed': True,
                'trades_executed': executed_trades,
                'new_positions': new_positions,
                'rebalance_summary': rebalance_event,
                'risk_status': recommendation['final_risk_metrics']['risk_status']
            }
            
        except Exception as e:
            logger.error(f"Error executing rebalancing: {e}")
            return {'error': str(e)}
    
    def _check_emergency_conditions(self, recommendation: Dict) -> Dict:
        """Check for emergency stop conditions"""
        
        emergency_conditions = []
        
        # Check portfolio VaR
        portfolio_var = recommendation['final_risk_metrics']['portfolio_var']
        if portfolio_var > self.system_config['emergency_stop_loss']:
            emergency_conditions.append(f"Portfolio VaR {portfolio_var:.1%} exceeds emergency limit {self.system_config['emergency_stop_loss']:.1%}")
        
        # Check risk violations
        violations = recommendation['final_risk_metrics']['violations']
        if len(violations) > 2:
            emergency_conditions.append(f"Multiple risk violations detected: {len(violations)}")
        
        # Check individual position sizes
        for position, allocation in recommendation['risk_adjusted_allocations'].items():
            if allocation > self.system_config['position_limits']['max_position']:
                emergency_conditions.append(f"Position {position} exceeds maximum limit: {allocation:.1%}")
        
        emergency_actions = []
        if emergency_conditions:
            emergency_actions = [
                'Halt all new position increases',
                'Review risk parameters',
                'Consider reducing exposure',
                'Manual intervention required'
            ]
        
        return {
            'emergency_detected': len(emergency_conditions) > 0,
            'conditions': emergency_conditions,
            'reason': '; '.join(emergency_conditions) if emergency_conditions else None,
            'actions': emergency_actions
        }
    
    def get_system_status(self) -> Dict:
        """Get comprehensive system status"""
        
        try:
            # Get latest recommendation
            latest_rec = self.generate_signals_with_risk_control()
            
            # System health check
            health_status = 'healthy'
            if 'error' in latest_rec:
                health_status = 'error'
            elif latest_rec.get('final_risk_metrics', {}).get('risk_status') == 'RED':
                health_status = 'warning'
            
            # Performance summary
            total_exposure = sum(self.current_positions.values()) if self.current_positions else 0.0
            
            return {
                'timestamp': datetime.now().isoformat(),
                'system_health': health_status,
                'last_update': self.last_update.isoformat() if self.last_update else None,
                'current_positions': self.current_positions,
                'total_exposure': total_exposure,
                'num_active_positions': len([p for p in self.current_positions.values() if p > 0.01]),
                'latest_recommendation': latest_rec,
                'rebalance_history_count': len(self.rebalance_history),
                'risk_events_count': len(self.risk_events),
                'system_configuration': self.system_config
            }
            
        except Exception as e:
            logger.error(f"Error getting system status: {e}")
            return {'error': str(e)}

def main():
    """Main function for integrated system testing"""
    
    system = IntegratedSixPositionSystem()
    
    print("🎯 INTEGRATED SIX POSITION TRADING SYSTEM")
    print("=" * 70)
    
    # Get system status
    print("Initializing integrated trading system...")
    status = system.get_system_status()
    
    if 'error' not in status:
        print(f"\n📊 System Status:")
        print(f"  Health: {status['system_health'].upper()}")
        print(f"  Total Exposure: {status['total_exposure']:.1%}")
        print(f"  Active Positions: {status['num_active_positions']}")
        
        # Latest recommendation
        if 'error' not in status['latest_recommendation']:
            rec = status['latest_recommendation']
            
            print(f"\n🎯 Latest Recommendation:")
            print(f"  Overall Action: {rec['final_overall_action']}")
            print(f"  Risk Adjusted: {rec['risk_adjustment_made']}")
            print(f"  Risk Status: {rec['final_risk_metrics']['risk_status']}")
            
            # Show risk-adjusted allocations
            print(f"\n📈 Risk-Adjusted Allocations:")
            risk_allocations = rec['risk_adjusted_allocations']
            
            # Group by asset
            eth_positions = {k: v for k, v in risk_allocations.items() if 'ETH' in k}
            btc_positions = {k: v for k, v in risk_allocations.items() if 'BTC' in k}
            
            print(f"\n  ETH Positions:")
            for pos, alloc in eth_positions.items():
                action = rec['final_actions'].get(pos, 'HOLD')
                print(f"    {pos}: {alloc:.1%} - {action}")
            
            print(f"\n  BTC Positions:")
            for pos, alloc in btc_positions.items():
                action = rec['final_actions'].get(pos, 'HOLD')
                print(f"    {pos}: {alloc:.1%} - {action}")
            
            # Risk metrics
            risk_metrics = rec['final_risk_metrics']
            print(f"\n🛡️ Risk Metrics:")
            print(f"  Risk Score: {risk_metrics['risk_score']:.1f}")
            print(f"  Portfolio Volatility: {risk_metrics['portfolio_volatility']:.1%}")
            print(f"  Portfolio VaR: {risk_metrics['portfolio_var']:.1%}")
            print(f"  Diversification: {risk_metrics['diversification_score']:.2f}")
            
            if risk_metrics['violations']:
                print(f"\n❌ Risk Violations:")
                for violation in risk_metrics['violations']:
                    print(f"    • {violation}")
            
            if risk_metrics['warnings']:
                print(f"\n⚠️ Risk Warnings:")
                for warning in risk_metrics['warnings']:
                    print(f"    • {warning}")
            
            # Performance estimates
            if 'risk_adjusted_performance' in rec:
                perf = rec['risk_adjusted_performance']
                print(f"\n📊 Performance Estimates:")
                print(f"  Expected Return: {perf['expected_return']:.1%}")
                print(f"  Expected Volatility: {perf['expected_volatility']:.1%}")
                print(f"  Sharpe Estimate: {perf['sharpe_estimate']:.2f}")
            
            # Test rebalancing execution
            print(f"\n🔄 Testing Rebalancing Execution...")
            rebalance_result = system.execute_rebalancing(rec)
            
            if 'error' not in rebalance_result:
                if rebalance_result['rebalancing_executed']:
                    print(f"  ✅ Rebalancing executed: {len(rebalance_result['trades_executed'])} trades")
                    print(f"  Risk Status: {rebalance_result['risk_status']}")
                elif rebalance_result.get('emergency_stop'):
                    print(f"  🚨 Emergency stop triggered: {rebalance_result['reason']}")
                else:
                    print(f"  ⏸️ No rebalancing needed: {rebalance_result['reason']}")
            else:
                print(f"  ❌ Rebalancing error: {rebalance_result['error']}")
        
        else:
            print(f"❌ Recommendation error: {status['latest_recommendation']['error']}")
    
    else:
        print(f"❌ System error: {status['error']}")
    
    print("\n🎯 Integrated system analysis complete!")
    print("\n📋 System Features:")
    print("  ✅ 6-position management (ETH + BTC × 3 timeframes)")
    print("  ✅ Integrated risk controls with automatic adjustment")
    print("  ✅ Multi-timeframe signal generation")
    print("  ✅ Position sizing with correlation awareness")
    print("  ✅ Emergency stop conditions")
    print("  ✅ Automated rebalancing execution")

if __name__ == "__main__":
    main()
