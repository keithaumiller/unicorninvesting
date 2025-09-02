#!/usr/bin/env python3
"""
Bitcoin Model Manager for Myportolio
Manages Bitcoin models for portfolio integration
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
btc_dir = os.path.abspath(os.path.join(current_dir, '../../2_alpha_models/CRYPTO/BTC'))
sys.path.append(btc_dir)

try:
    from btc_production_framework import BTCProductionModelFramework
    from btc_portfolio_integration import BTCPortfolioIntegration
except ImportError as e:
    logger.error(f"Cannot import Bitcoin modules: {e}")
    BTCProductionModelFramework = None
    BTCPortfolioIntegration = None

class BTCModelManager:
    """
    Bitcoin Model Manager for Myportolio integration
    """
    
    def __init__(self):
        """Initialize Bitcoin Model Manager"""
        
        # Initialize Bitcoin components if available
        if BTCProductionModelFramework is not None:
            self.production_framework = BTCProductionModelFramework()
        else:
            self.production_framework = None
            logger.warning("Bitcoin production framework not available")
        
        if BTCPortfolioIntegration is not None:
            self.portfolio_integration = BTCPortfolioIntegration()
        else:
            self.portfolio_integration = None
            logger.warning("Bitcoin portfolio integration not available")
        
        # Configuration
        self.config = {
            'target_allocation': 0.40,  # 40% target allocation
            'min_allocation': 0.05,     # 5% minimum
            'max_allocation': 0.50,     # 50% maximum
            'rebalance_threshold': 0.05, # 5% drift threshold
            'risk_limit': 0.15,         # 15% max drawdown
            'confidence_threshold': 0.6  # Minimum signal confidence
        }
        
        # Model status
        self.model_status = {
            'initialized': False,
            'last_update': None,
            'active_models': 0,
            'performance_score': 0.0
        }
        
        logger.info("Bitcoin Model Manager initialized")
    
    def initialize_models(self) -> Dict:
        """Initialize Bitcoin models if not already done"""
        
        if self.production_framework is None:
            return {'error': 'Bitcoin production framework not available'}
        
        try:
            # Train models if needed
            print("🟠 Initializing Bitcoin models...")
            results = self.production_framework.train_all_models(['1hour', '1day'])
            
            # Count successful models
            successful_models = 0
            for timeframe, timeframe_results in results.items():
                for model_type, result in timeframe_results.items():
                    if 'error' not in result:
                        successful_models += 1
            
            self.model_status = {
                'initialized': True,
                'last_update': datetime.now().isoformat(),
                'active_models': successful_models,
                'performance_score': 0.85  # Initial score
            }
            
            return {
                'success': True,
                'active_models': successful_models,
                'training_results': results
            }
            
        except Exception as e:
            logger.error(f"Error initializing Bitcoin models: {e}")
            return {'error': str(e)}
    
    def get_btc_signals(self, timeframe: str = '1hour') -> Dict:
        """Get Bitcoin trading signals"""
        
        if self.portfolio_integration is None:
            return {'error': 'Bitcoin portfolio integration not available'}
        
        try:
            signals = self.portfolio_integration.get_btc_signals(timeframe)
            return signals
        except Exception as e:
            logger.error(f"Error getting BTC signals: {e}")
            return {'error': str(e)}
    
    def get_portfolio_recommendation(self) -> Dict:
        """Get Bitcoin portfolio allocation recommendation"""
        
        if self.portfolio_integration is None:
            return {'error': 'Bitcoin portfolio integration not available'}
        
        try:
            recommendation = self.portfolio_integration.generate_portfolio_recommendation()
            
            # Apply configuration constraints
            if 'error' not in recommendation:
                rec_allocation = recommendation.get('recommended_allocation', 0.05)
                
                # Apply limits
                rec_allocation = max(self.config['min_allocation'], 
                                   min(self.config['max_allocation'], rec_allocation))
                
                # Check confidence threshold
                confidence = recommendation.get('confidence', 0.0)
                if confidence < self.config['confidence_threshold']:
                    rec_allocation = self.config['min_allocation']  # Conservative default
                
                recommendation['constrained_allocation'] = rec_allocation
                recommendation['applied_constraints'] = self.config
            
            return recommendation
            
        except Exception as e:
            logger.error(f"Error getting portfolio recommendation: {e}")
            return {'error': str(e)}
    
    def update_position(self, new_allocation: float) -> Dict:
        """Update Bitcoin position"""
        
        if self.portfolio_integration is None:
            return {'error': 'Bitcoin portfolio integration not available'}
        
        try:
            # Validate allocation
            if new_allocation < 0 or new_allocation > 1:
                return {'error': 'Allocation must be between 0 and 1'}
            
            # Apply constraints
            constrained_allocation = max(self.config['min_allocation'],
                                       min(self.config['max_allocation'], new_allocation))
            
            self.portfolio_integration.update_position(constrained_allocation)
            
            return {
                'success': True,
                'requested_allocation': new_allocation,
                'actual_allocation': constrained_allocation,
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"Error updating Bitcoin position: {e}")
            return {'error': str(e)}
    
    def get_model_performance(self) -> Dict:
        """Get Bitcoin model performance summary"""
        
        if self.production_framework is None:
            return {'error': 'Bitcoin production framework not available'}
        
        try:
            summary = self.production_framework.get_production_models_summary()
            
            # Add performance details
            performance = {
                'model_summary': summary,
                'status': self.model_status,
                'configuration': self.config,
                'last_signals': None,
                'health_check': 'healthy' if summary['total_models'] > 0 else 'warning'
            }
            
            # Get latest signals for health check
            try:
                latest_signals = self.get_btc_signals('1hour')
                if 'error' not in latest_signals:
                    performance['last_signals'] = latest_signals
                    performance['signals_available'] = True
                else:
                    performance['signals_available'] = False
            except:
                performance['signals_available'] = False
            
            return performance
            
        except Exception as e:
            logger.error(f"Error getting model performance: {e}")
            return {'error': str(e)}
    
    def health_check(self) -> Dict:
        """Perform health check on Bitcoin models"""
        
        health = {
            'timestamp': datetime.now().isoformat(),
            'components': {},
            'overall_status': 'healthy',
            'issues': []
        }
        
        # Check production framework
        if self.production_framework is not None:
            health['components']['production_framework'] = 'available'
        else:
            health['components']['production_framework'] = 'unavailable'
            health['issues'].append('Bitcoin production framework not available')
        
        # Check portfolio integration
        if self.portfolio_integration is not None:
            health['components']['portfolio_integration'] = 'available'
        else:
            health['components']['portfolio_integration'] = 'unavailable'
            health['issues'].append('Bitcoin portfolio integration not available')
        
        # Check model status
        if self.model_status['initialized']:
            health['components']['models'] = f"{self.model_status['active_models']} active"
        else:
            health['components']['models'] = 'not initialized'
            health['issues'].append('Bitcoin models not initialized')
        
        # Test signal generation
        try:
            if self.portfolio_integration is not None:
                test_signals = self.get_btc_signals('1hour')
                if 'error' not in test_signals:
                    health['components']['signal_generation'] = 'working'
                else:
                    health['components']['signal_generation'] = 'error'
                    health['issues'].append(f"Signal generation error: {test_signals['error']}")
            else:
                health['components']['signal_generation'] = 'unavailable'
        except Exception as e:
            health['components']['signal_generation'] = 'error'
            health['issues'].append(f"Signal generation exception: {e}")
        
        # Determine overall status
        if len(health['issues']) == 0:
            health['overall_status'] = 'healthy'
        elif len(health['issues']) <= 2:
            health['overall_status'] = 'warning'
        else:
            health['overall_status'] = 'critical'
        
        return health

def main():
    """Main function for Bitcoin model management"""
    
    # Initialize manager
    btc_manager = BTCModelManager()
    
    print("🟠 Bitcoin Model Manager for Myportolio")
    print("=" * 50)
    
    # Health check
    print("Performing Bitcoin model health check...")
    health = btc_manager.health_check()
    print(f"Overall Status: {health['overall_status'].upper()}")
    
    for component, status in health['components'].items():
        print(f"  {component}: {status}")
    
    if health['issues']:
        print("\nIssues found:")
        for issue in health['issues']:
            print(f"  ⚠️  {issue}")
    
    # Initialize models if needed
    if not btc_manager.model_status['initialized']:
        print("\nInitializing Bitcoin models...")
        init_result = btc_manager.initialize_models()
        
        if 'error' not in init_result:
            print(f"✅ Successfully initialized {init_result['active_models']} Bitcoin models")
        else:
            print(f"❌ Error initializing models: {init_result['error']}")
            return
    
    # Get portfolio recommendation
    print("\nGenerating Bitcoin portfolio recommendation...")
    recommendation = btc_manager.get_portfolio_recommendation()
    
    if 'error' not in recommendation:
        print(f"📊 Portfolio Recommendation:")
        print(f"  Current Allocation: {recommendation.get('current_allocation', 0):.1%}")
        print(f"  Recommended: {recommendation.get('recommended_allocation', 0):.1%}")
        print(f"  Constrained: {recommendation.get('constrained_allocation', 0):.1%}")
        print(f"  Action: {recommendation.get('action', 'UNKNOWN')}")
        print(f"  Confidence: {recommendation.get('confidence', 0):.2f}")
    else:
        print(f"❌ Error getting recommendation: {recommendation['error']}")
    
    # Performance summary
    print("\nBitcoin model performance:")
    performance = btc_manager.get_model_performance()
    
    if 'error' not in performance:
        model_summary = performance.get('model_summary', {})
        print(f"  Total Models: {model_summary.get('total_models', 0)}")
        print(f"  Health Check: {performance.get('health_check', 'unknown').upper()}")
        print(f"  Signals Available: {performance.get('signals_available', False)}")
    else:
        print(f"❌ Error getting performance: {performance['error']}")
    
    print("\n🟠 Bitcoin model management complete!")

if __name__ == "__main__":
    main()
