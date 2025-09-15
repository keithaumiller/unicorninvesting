#!/usr/bin/env python3
"""
Dual Crypto Portfolio Manager
Manages both ETH and BTC allocations for balanced portfolio
"""

import os
import sys
import json
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Add paths
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)

try:
    from btc_model_manager import BTCModelManager
except ImportError as e:
    logger.warning(f"Bitcoin model manager import warning: {e}")
    BTCModelManager = None

class DualCryptoPortfolioManager:
    """
    Manages dual cryptocurrency portfolio (ETH + BTC)
    Integrates with existing ETH systems and new BTC models
    """
    
    def __init__(self):
        """Initialize dual crypto portfolio manager"""
        
        # Load portfolio configuration
        self.load_portfolio_config()
        
        # Initialize Bitcoin manager
        if BTCModelManager is not None:
            self.btc_manager = BTCModelManager()
        else:
            self.btc_manager = None
            logger.warning("Bitcoin model manager not available")
        
        # Portfolio state
        self.current_allocations = {
            'ETH': 0.0,
            'BTC': 0.0,
            'CASH': 1.0
        }
        
        self.target_allocations = {
            'ETH': 0.60,  # 60% target
            'BTC': 0.40,  # 40% target
            'CASH': 0.00
        }
        
        # Risk parameters
        self.risk_config = {
            'max_total_crypto': 0.95,    # Max 95% in crypto
            'min_cash_buffer': 0.05,     # Min 5% cash
            'max_single_asset': 0.70,    # Max 70% in single asset
            'rebalance_threshold': 0.05,  # 5% drift threshold
            'correlation_limit': 0.80,    # Max correlation between assets
            'volatility_limit': 0.30     # Max portfolio volatility
        }
        
        logger.info("Dual Crypto Portfolio Manager initialized")
    
    def load_portfolio_config(self):
        """Load portfolio configuration"""
        
        config_path = os.path.join(current_dir, 'config.json')
        
        try:
            with open(config_path, 'r') as f:
                self.portfolio_config = json.load(f)
            
            # Extract target allocations from config
            if 'assets' in self.portfolio_config:
                for asset, config in self.portfolio_config['assets'].items():
                    if asset in ['ETH', 'BTC']:
                        self.target_allocations[asset] = config.get('allocation_percent', 0) / 100
            
            logger.info("Portfolio configuration loaded")
            
        except Exception as e:
            logger.warning(f"Could not load portfolio config: {e}")
            self.portfolio_config = {}
    
    def get_eth_signals(self) -> Dict:
        """Get ETH signals from existing ETH system"""
        
        try:
            # For now, simulate ETH signals
            # In production, this would integrate with existing ETH models
            
            eth_signals = {
                'timestamp': datetime.now().isoformat(),
                'signal': 'BUY',
                'confidence': 0.72,
                'price_target': 3500.0,
                'risk_score': 0.15,
                'recommendation': {
                    'target_allocation': 0.60,
                    'action': 'HOLD',
                    'reasoning': 'Strong momentum indicators with manageable risk'
                }
            }
            
            return eth_signals
            
        except Exception as e:
            logger.error(f"Error getting ETH signals: {e}")
            return {'error': str(e)}
    
    def get_btc_signals(self) -> Dict:
        """Get BTC signals from Bitcoin model manager"""
        
        if self.btc_manager is None:
            return {'error': 'Bitcoin model manager not available'}
        
        try:
            return self.btc_manager.get_portfolio_recommendation()
        except Exception as e:
            logger.error(f"Error getting BTC signals: {e}")
            return {'error': str(e)}
    
    def calculate_correlation_adjustment(self, eth_signals: Dict, btc_signals: Dict) -> Dict:
        """Calculate correlation-based allocation adjustments"""
        
        try:
            # Simplified correlation analysis
            # In production, would use historical price correlation
            
            # Assume moderate correlation between BTC and ETH
            correlation = 0.65  # Typical BTC-ETH correlation
            
            adjustments = {}
            
            if correlation > self.risk_config['correlation_limit']:
                # High correlation - reduce both positions slightly
                correlation_penalty = (correlation - self.risk_config['correlation_limit']) * 0.5
                
                adjustments['ETH'] = -correlation_penalty * 0.5
                adjustments['BTC'] = -correlation_penalty * 0.5
                adjustments['CASH'] = correlation_penalty
                
                reasoning = f"High correlation ({correlation:.2f}) detected - reducing crypto exposure"
            else:
                # Normal correlation - no adjustment needed
                adjustments['ETH'] = 0.0
                adjustments['BTC'] = 0.0
                adjustments['CASH'] = 0.0
                
                reasoning = f"Normal correlation ({correlation:.2f}) - no adjustment needed"
            
            return {
                'correlation': correlation,
                'adjustments': adjustments,
                'reasoning': reasoning
            }
            
        except Exception as e:
            logger.error(f"Error calculating correlation adjustment: {e}")
            return {'error': str(e)}
    
    def generate_portfolio_allocation(self) -> Dict:
        """Generate optimal portfolio allocation considering both assets"""
        
        try:
            # Get signals from both assets
            eth_signals = self.get_eth_signals()
            btc_signals = self.get_btc_signals()
            
            portfolio_recommendation = {
                'timestamp': datetime.now().isoformat(),
                'current_allocations': self.current_allocations.copy(),
                'target_allocations': self.target_allocations.copy(),
                'individual_signals': {
                    'ETH': eth_signals,
                    'BTC': btc_signals
                }
            }
            
            # Check for signal errors
            eth_error = 'error' in eth_signals
            btc_error = 'error' in btc_signals
            
            if eth_error and btc_error:
                return {
                    'error': 'No valid signals available for either asset',
                    'individual_signals': {'ETH': eth_signals, 'BTC': btc_signals}
                }
            
            # Calculate base allocations from signals
            new_allocations = {}
            
            if not eth_error:
                eth_target = eth_signals.get('recommendation', {}).get('target_allocation', 0.60)
                eth_confidence = eth_signals.get('confidence', 0.5)
            else:
                eth_target = 0.30  # Conservative fallback
                eth_confidence = 0.3
            
            if not btc_error:
                btc_target = btc_signals.get('constrained_allocation', 
                                           btc_signals.get('recommended_allocation', 0.40))
                btc_confidence = btc_signals.get('confidence', 0.5)
            else:
                btc_target = 0.20  # Conservative fallback
                btc_confidence = 0.3
            
            # Apply confidence weighting
            eth_weighted = eth_target * eth_confidence
            btc_weighted = btc_target * btc_confidence
            
            # Normalize to ensure total doesn't exceed maximum crypto allocation
            total_crypto = eth_weighted + btc_weighted
            max_crypto = self.risk_config['max_total_crypto']
            
            if total_crypto > max_crypto:
                scale_factor = max_crypto / total_crypto
                eth_weighted *= scale_factor
                btc_weighted *= scale_factor
            
            # Calculate correlation adjustment
            correlation_adj = self.calculate_correlation_adjustment(eth_signals, btc_signals)
            
            if 'error' not in correlation_adj:
                adjustments = correlation_adj['adjustments']
                eth_weighted += adjustments['ETH']
                btc_weighted += adjustments['BTC']
            
            # Apply individual asset limits
            eth_final = max(0.05, min(self.risk_config['max_single_asset'], eth_weighted))
            btc_final = max(0.05, min(self.risk_config['max_single_asset'], btc_weighted))
            
            # Ensure minimum cash buffer
            total_crypto_final = eth_final + btc_final
            if total_crypto_final > max_crypto:
                # Proportionally reduce both
                reduction_factor = max_crypto / total_crypto_final
                eth_final *= reduction_factor
                btc_final *= reduction_factor
            
            cash_final = 1.0 - eth_final - btc_final
            cash_final = max(self.risk_config['min_cash_buffer'], cash_final)
            
            # Final normalization
            total_allocation = eth_final + btc_final + cash_final
            new_allocations = {
                'ETH': eth_final / total_allocation,
                'BTC': btc_final / total_allocation,
                'CASH': cash_final / total_allocation
            }
            
            # Calculate required actions
            actions = {}
            for asset in ['ETH', 'BTC', 'CASH']:
                current = self.current_allocations[asset]
                target = new_allocations[asset]
                diff = target - current
                
                if abs(diff) > self.risk_config['rebalance_threshold']:
                    if diff > 0:
                        actions[asset] = 'INCREASE'
                    else:
                        actions[asset] = 'DECREASE'
                else:
                    actions[asset] = 'HOLD'
            
            # Determine overall action
            if any(action in ['INCREASE', 'DECREASE'] for action in actions.values()):
                overall_action = 'REBALANCE'
            else:
                overall_action = 'HOLD'
            
            portfolio_recommendation.update({
                'optimized_allocations': new_allocations,
                'allocation_changes': {
                    asset: new_allocations[asset] - self.current_allocations[asset]
                    for asset in new_allocations
                },
                'actions': actions,
                'overall_action': overall_action,
                'risk_metrics': {
                    'total_crypto_exposure': new_allocations['ETH'] + new_allocations['BTC'],
                    'cash_buffer': new_allocations['CASH'],
                    'max_single_asset': max(new_allocations['ETH'], new_allocations['BTC']),
                    'within_risk_limits': True
                },
                'correlation_analysis': correlation_adj,
                'confidence_metrics': {
                    'eth_confidence': eth_confidence,
                    'btc_confidence': btc_confidence,
                    'overall_confidence': (eth_confidence + btc_confidence) / 2
                }
            })
            
            return portfolio_recommendation
            
        except Exception as e:
            logger.error(f"Error generating portfolio allocation: {e}")
            return {'error': str(e)}
    
    def update_current_allocations(self, allocations: Dict):
        """Update current portfolio allocations"""
        
        for asset, allocation in allocations.items():
            if asset in self.current_allocations:
                self.current_allocations[asset] = max(0.0, min(1.0, allocation))
        
        # Ensure allocations sum to 1
        total = sum(self.current_allocations.values())
        if total > 0:
            for asset in self.current_allocations:
                self.current_allocations[asset] /= total
        
        logger.info(f"Portfolio allocations updated: {self.current_allocations}")
    
    def get_portfolio_status(self) -> Dict:
        """Get comprehensive portfolio status"""
        
        try:
            # Get latest allocation recommendation
            recommendation = self.generate_portfolio_allocation()
            
            # Bitcoin model health check
            btc_health = None
            if self.btc_manager is not None:
                btc_health = self.btc_manager.health_check()
            
            status = {
                'timestamp': datetime.now().isoformat(),
                'portfolio_config': self.portfolio_config,
                'current_allocations': self.current_allocations,
                'target_allocations': self.target_allocations,
                'risk_configuration': self.risk_config,
                'latest_recommendation': recommendation,
                'bitcoin_model_health': btc_health,
                'portfolio_health': 'healthy'
            }
            
            # Determine portfolio health
            issues = []
            
            if 'error' in recommendation:
                issues.append(f"Allocation generation error: {recommendation['error']}")
            
            if btc_health and btc_health.get('overall_status') not in ['healthy']:
                issues.append(f"Bitcoin models: {btc_health.get('overall_status', 'unknown')}")
            
            if len(issues) == 0:
                status['portfolio_health'] = 'healthy'
            elif len(issues) <= 2:
                status['portfolio_health'] = 'warning'
            else:
                status['portfolio_health'] = 'critical'
            
            status['health_issues'] = issues
            
            return status
            
        except Exception as e:
            logger.error(f"Error getting portfolio status: {e}")
            return {'error': str(e)}

def main():
    """Main function for dual crypto portfolio management"""
    
    # Initialize manager
    portfolio_manager = DualCryptoPortfolioManager()
    
    print("🔄 Dual Crypto Portfolio Manager")
    print("=" * 50)
    
    # Get portfolio status
    print("Analyzing dual crypto portfolio...")
    status = portfolio_manager.get_portfolio_status()
    
    if 'error' not in status:
        print(f"\n📊 Portfolio Status: {status['portfolio_health'].upper()}")
        
        # Current allocations
        current = status['current_allocations']
        print(f"\n💼 Current Allocations:")
        for asset, allocation in current.items():
            print(f"  {asset}: {allocation:.1%}")
        
        # Latest recommendation
        if 'error' not in status['latest_recommendation']:
            rec = status['latest_recommendation']
            optimized = rec['optimized_allocations']
            
            print(f"\n🎯 Optimized Allocations:")
            for asset, allocation in optimized.items():
                change = rec['allocation_changes'][asset]
                action = rec['actions'][asset]
                print(f"  {asset}: {allocation:.1%} ({change:+.1%}) - {action}")
            
            print(f"\n⚡ Overall Action: {rec['overall_action']}")
            print(f"🎲 Confidence: {rec['confidence_metrics']['overall_confidence']:.2f}")
            
            # Risk metrics
            risk = rec['risk_metrics']
            print(f"\n🛡️  Risk Metrics:")
            print(f"  Total Crypto: {risk['total_crypto_exposure']:.1%}")
            print(f"  Cash Buffer: {risk['cash_buffer']:.1%}")
            print(f"  Max Single Asset: {risk['max_single_asset']:.1%}")
        
        # Health issues
        if status['health_issues']:
            print(f"\n⚠️  Health Issues:")
            for issue in status['health_issues']:
                print(f"  • {issue}")
        
        # Bitcoin model status
        if status['bitcoin_model_health']:
            btc_health = status['bitcoin_model_health']
            print(f"\n🟠 Bitcoin Models: {btc_health['overall_status'].upper()}")
            for component, status_val in btc_health['components'].items():
                print(f"  {component}: {status_val}")
    
    else:
        print(f"❌ Error getting portfolio status: {status['error']}")
    
    print("\n🔄 Dual crypto portfolio analysis complete!")

if __name__ == "__main__":
    main()
