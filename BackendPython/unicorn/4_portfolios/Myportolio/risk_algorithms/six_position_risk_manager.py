#!/usr/bin/env python3
"""
Six Position Risk Manager
Advanced risk management for ETH + BTC across 3 timeframes each
Manages correlation, concentration, and leverage risks
"""

import os
import sys
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
import logging

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class PositionRisk:
    """Risk metrics for individual position"""
    position_id: str
    asset: str
    timeframe: str
    allocation: float
    volatility: float
    var_1day: float
    var_1week: float
    max_drawdown: float
    correlation_eth: float
    correlation_btc: float
    leverage_contribution: float
    risk_contribution: float

@dataclass
class PortfolioRiskLimits:
    """Portfolio-level risk limits"""
    max_total_exposure: float = 0.95
    max_single_asset: float = 0.70
    max_single_timeframe: float = 0.40
    max_correlation: float = 0.85
    max_portfolio_volatility: float = 0.30
    max_portfolio_var: float = 0.15
    max_drawdown: float = 0.20
    min_diversification: float = 0.25
    max_leverage: float = 1.0

class SixPositionRiskManager:
    """
    Advanced risk management for 6-position dual crypto strategy
    """
    
    def __init__(self):
        """Initialize risk manager"""
        
        self.risk_limits = PortfolioRiskLimits()
        
        # Position definitions
        self.positions = {
            'ETH_1min': {'asset': 'ETH', 'timeframe': '1min', 'strategy': 'short'},
            'ETH_1hour': {'asset': 'ETH', 'timeframe': '1hour', 'strategy': 'mid'},
            'ETH_1day': {'asset': 'ETH', 'timeframe': '1day', 'strategy': 'long'},
            'BTC_1min': {'asset': 'BTC', 'timeframe': '1min', 'strategy': 'short'},
            'BTC_1hour': {'asset': 'BTC', 'timeframe': '1hour', 'strategy': 'mid'},
            'BTC_1day': {'asset': 'BTC', 'timeframe': '1day', 'strategy': 'long'}
        }
        
        # Historical volatilities by asset-timeframe
        self.volatility_estimates = {
            'ETH_1min': 0.08,
            'ETH_1hour': 0.12,
            'ETH_1day': 0.18,
            'BTC_1min': 0.10,
            'BTC_1hour': 0.15,
            'BTC_1day': 0.22
        }
        
        # Correlation matrix (simplified)
        self.correlation_matrix = pd.DataFrame({
            'ETH_1min': [1.0, 0.75, 0.60, 0.65, 0.50, 0.45],
            'ETH_1hour': [0.75, 1.0, 0.85, 0.55, 0.65, 0.55],
            'ETH_1day': [0.60, 0.85, 1.0, 0.45, 0.55, 0.65],
            'BTC_1min': [0.65, 0.55, 0.45, 1.0, 0.75, 0.60],
            'BTC_1hour': [0.50, 0.65, 0.55, 0.75, 1.0, 0.85],
            'BTC_1day': [0.45, 0.55, 0.65, 0.60, 0.85, 1.0]
        }, index=['ETH_1min', 'ETH_1hour', 'ETH_1day', 'BTC_1min', 'BTC_1hour', 'BTC_1day'])
        
        logger.info("Six Position Risk Manager initialized")
    
    def calculate_position_risk(self, position_id: str, allocation: float) -> PositionRisk:
        """Calculate risk metrics for individual position"""
        
        position_info = self.positions[position_id]
        asset = position_info['asset']
        timeframe = position_info['timeframe']
        
        # Volatility
        volatility = self.volatility_estimates[position_id]
        
        # VaR calculations (95% confidence)
        var_1day = allocation * volatility * 1.645  # 95% VaR
        var_1week = allocation * volatility * np.sqrt(7) * 1.645
        
        # Maximum drawdown estimate
        max_drawdown = allocation * volatility * 2.5  # Rough estimate
        
        # Correlations
        corr_eth = np.mean([self.correlation_matrix.loc[position_id, pos] 
                           for pos in self.correlation_matrix.columns if 'ETH' in pos and pos != position_id])
        corr_btc = np.mean([self.correlation_matrix.loc[position_id, pos] 
                           for pos in self.correlation_matrix.columns if 'BTC' in pos and pos != position_id])
        
        # Risk contribution (simplified)
        risk_contribution = allocation * volatility
        
        return PositionRisk(
            position_id=position_id,
            asset=asset,
            timeframe=timeframe,
            allocation=allocation,
            volatility=volatility,
            var_1day=var_1day,
            var_1week=var_1week,
            max_drawdown=max_drawdown,
            correlation_eth=corr_eth,
            correlation_btc=corr_btc,
            leverage_contribution=allocation,
            risk_contribution=risk_contribution
        )
    
    def calculate_portfolio_risk(self, allocations: Dict[str, float]) -> Dict:
        """Calculate comprehensive portfolio risk metrics"""
        
        # Calculate individual position risks
        position_risks = {}
        for position_id, allocation in allocations.items():
            if allocation > 0.001:  # Only calculate for meaningful positions
                position_risks[position_id] = self.calculate_position_risk(position_id, allocation)
        
        # Portfolio-level calculations
        total_exposure = sum(allocations.values())
        
        # Asset concentration
        eth_exposure = sum(alloc for pos, alloc in allocations.items() if 'ETH' in pos)
        btc_exposure = sum(alloc for pos, alloc in allocations.items() if 'BTC' in pos)
        max_asset_concentration = max(eth_exposure, btc_exposure)
        
        # Timeframe concentration
        short_exposure = sum(alloc for pos, alloc in allocations.items() if '1min' in pos)
        mid_exposure = sum(alloc for pos, alloc in allocations.items() if '1hour' in pos)
        long_exposure = sum(alloc for pos, alloc in allocations.items() if '1day' in pos)
        max_timeframe_concentration = max(short_exposure, mid_exposure, long_exposure)
        
        # Portfolio volatility calculation
        portfolio_volatility = self._calculate_portfolio_volatility(allocations)
        
        # Portfolio VaR
        portfolio_var = portfolio_volatility * 1.645  # 95% VaR
        
        # Diversification score
        num_positions = len([alloc for alloc in allocations.values() if alloc > 0.01])
        diversification_score = min(1.0, num_positions / 6)
        
        # Calculate Herfindahl index for concentration
        herfindahl_index = sum(alloc**2 for alloc in allocations.values())
        
        # Risk budget utilization
        total_risk_contribution = sum(pr.risk_contribution for pr in position_risks.values())
        
        # Correlation risk
        avg_correlation = self._calculate_average_correlation(allocations)
        
        return {
            'total_exposure': total_exposure,
            'eth_exposure': eth_exposure,
            'btc_exposure': btc_exposure,
            'max_asset_concentration': max_asset_concentration,
            'max_timeframe_concentration': max_timeframe_concentration,
            'short_term_exposure': short_exposure,
            'mid_term_exposure': mid_exposure,
            'long_term_exposure': long_exposure,
            'portfolio_volatility': portfolio_volatility,
            'portfolio_var_1day': portfolio_var,
            'portfolio_var_1week': portfolio_var * np.sqrt(7),
            'diversification_score': diversification_score,
            'herfindahl_index': herfindahl_index,
            'total_risk_contribution': total_risk_contribution,
            'average_correlation': avg_correlation,
            'position_risks': position_risks,
            'num_active_positions': num_positions
        }
    
    def _calculate_portfolio_volatility(self, allocations: Dict[str, float]) -> float:
        """Calculate portfolio volatility using correlation matrix"""
        
        # Create weight vector
        positions = list(self.correlation_matrix.index)
        weights = np.array([allocations.get(pos, 0.0) for pos in positions])
        
        # Create volatility vector
        volatilities = np.array([self.volatility_estimates[pos] for pos in positions])
        
        # Calculate portfolio variance using correlation matrix
        corr_matrix = self.correlation_matrix.values
        
        # Portfolio variance = w' * Σ * w where Σ = diag(σ) * Corr * diag(σ)
        sigma_matrix = np.outer(volatilities, volatilities) * corr_matrix
        portfolio_variance = np.dot(weights, np.dot(sigma_matrix, weights))
        
        return np.sqrt(portfolio_variance)
    
    def _calculate_average_correlation(self, allocations: Dict[str, float]) -> float:
        """Calculate weighted average correlation between positions"""
        
        active_positions = [pos for pos, alloc in allocations.items() if alloc > 0.01]
        
        if len(active_positions) < 2:
            return 0.0
        
        total_correlation = 0.0
        pair_count = 0
        total_weight = 0.0
        
        for i, pos1 in enumerate(active_positions):
            for pos2 in active_positions[i+1:]:
                correlation = self.correlation_matrix.loc[pos1, pos2]
                weight = allocations[pos1] * allocations[pos2]
                total_correlation += correlation * weight
                total_weight += weight
                pair_count += 1
        
        return total_correlation / max(total_weight, 0.001)
    
    def validate_risk_limits(self, allocations: Dict[str, float]) -> Dict:
        """Validate portfolio against risk limits"""
        
        portfolio_risk = self.calculate_portfolio_risk(allocations)
        violations = []
        warnings = []
        
        # Check total exposure
        if portfolio_risk['total_exposure'] > self.risk_limits.max_total_exposure:
            violations.append(f"Total exposure {portfolio_risk['total_exposure']:.1%} exceeds limit {self.risk_limits.max_total_exposure:.1%}")
        
        # Check asset concentration
        if portfolio_risk['max_asset_concentration'] > self.risk_limits.max_single_asset:
            violations.append(f"Max asset concentration {portfolio_risk['max_asset_concentration']:.1%} exceeds limit {self.risk_limits.max_single_asset:.1%}")
        
        # Check timeframe concentration
        if portfolio_risk['max_timeframe_concentration'] > self.risk_limits.max_single_timeframe:
            violations.append(f"Max timeframe concentration {portfolio_risk['max_timeframe_concentration']:.1%} exceeds limit {self.risk_limits.max_single_timeframe:.1%}")
        
        # Check portfolio volatility
        if portfolio_risk['portfolio_volatility'] > self.risk_limits.max_portfolio_volatility:
            violations.append(f"Portfolio volatility {portfolio_risk['portfolio_volatility']:.1%} exceeds limit {self.risk_limits.max_portfolio_volatility:.1%}")
        
        # Check portfolio VaR
        if portfolio_risk['portfolio_var_1day'] > self.risk_limits.max_portfolio_var:
            violations.append(f"Portfolio VaR {portfolio_risk['portfolio_var_1day']:.1%} exceeds limit {self.risk_limits.max_portfolio_var:.1%}")
        
        # Check diversification
        if portfolio_risk['diversification_score'] < self.risk_limits.min_diversification:
            warnings.append(f"Diversification score {portfolio_risk['diversification_score']:.2f} below target {self.risk_limits.min_diversification:.2f}")
        
        # Check correlation
        if portfolio_risk['average_correlation'] > self.risk_limits.max_correlation:
            warnings.append(f"Average correlation {portfolio_risk['average_correlation']:.2f} above limit {self.risk_limits.max_correlation:.2f}")
        
        return {
            'risk_valid': len(violations) == 0,
            'violations': violations,
            'warnings': warnings,
            'risk_score': len(violations) + len(warnings) * 0.5,
            'portfolio_risk': portfolio_risk
        }
    
    def suggest_risk_adjustments(self, allocations: Dict[str, float]) -> Dict:
        """Suggest adjustments to meet risk limits"""
        
        validation = self.validate_risk_limits(allocations)
        
        if validation['risk_valid']:
            return {'adjustments_needed': False, 'current_allocations': allocations}
        
        adjusted_allocations = allocations.copy()
        adjustments_made = []
        
        portfolio_risk = validation['portfolio_risk']
        
        # Adjust total exposure
        if portfolio_risk['total_exposure'] > self.risk_limits.max_total_exposure:
            scale_factor = self.risk_limits.max_total_exposure / portfolio_risk['total_exposure']
            for pos in adjusted_allocations:
                adjusted_allocations[pos] *= scale_factor
            adjustments_made.append(f"Scaled all positions by {scale_factor:.3f} to meet exposure limit")
        
        # Adjust asset concentration
        if portfolio_risk['max_asset_concentration'] > self.risk_limits.max_single_asset:
            # Find which asset is over-concentrated
            if portfolio_risk['eth_exposure'] > self.risk_limits.max_single_asset:
                # Scale down ETH positions
                eth_scale = self.risk_limits.max_single_asset / portfolio_risk['eth_exposure']
                for pos in adjusted_allocations:
                    if 'ETH' in pos:
                        adjusted_allocations[pos] *= eth_scale
                adjustments_made.append(f"Scaled ETH positions by {eth_scale:.3f}")
            
            if portfolio_risk['btc_exposure'] > self.risk_limits.max_single_asset:
                # Scale down BTC positions
                btc_scale = self.risk_limits.max_single_asset / portfolio_risk['btc_exposure']
                for pos in adjusted_allocations:
                    if 'BTC' in pos:
                        adjusted_allocations[pos] *= btc_scale
                adjustments_made.append(f"Scaled BTC positions by {btc_scale:.3f}")
        
        # Adjust timeframe concentration
        if portfolio_risk['max_timeframe_concentration'] > self.risk_limits.max_single_timeframe:
            # Find over-concentrated timeframe and reduce
            timeframes = {'1min': portfolio_risk['short_term_exposure'],
                         '1hour': portfolio_risk['mid_term_exposure'], 
                         '1day': portfolio_risk['long_term_exposure']}
            
            for tf, exposure in timeframes.items():
                if exposure > self.risk_limits.max_single_timeframe:
                    tf_scale = self.risk_limits.max_single_timeframe / exposure
                    for pos in adjusted_allocations:
                        if tf in pos:
                            adjusted_allocations[pos] *= tf_scale
                    adjustments_made.append(f"Scaled {tf} positions by {tf_scale:.3f}")
        
        return {
            'adjustments_needed': True,
            'original_allocations': allocations,
            'adjusted_allocations': adjusted_allocations,
            'adjustments_made': adjustments_made,
            'risk_improvement': 'Adjusted to meet risk limits'
        }
    
    def generate_risk_report(self, allocations: Dict[str, float]) -> Dict:
        """Generate comprehensive risk report"""
        
        portfolio_risk = self.calculate_portfolio_risk(allocations)
        validation = self.validate_risk_limits(allocations)
        
        # Risk dashboard
        risk_dashboard = {
            'overall_risk_status': 'GREEN' if validation['risk_valid'] else 'RED',
            'risk_score': validation['risk_score'],
            'total_violations': len(validation['violations']),
            'total_warnings': len(validation['warnings'])
        }
        
        # Position breakdown
        position_breakdown = {}
        for pos_id, pos_risk in portfolio_risk['position_risks'].items():
            position_breakdown[pos_id] = {
                'allocation': pos_risk.allocation,
                'volatility': pos_risk.volatility,
                'var_1day': pos_risk.var_1day,
                'risk_contribution': pos_risk.risk_contribution,
                'risk_rank': 0  # To be calculated
            }
        
        # Rank positions by risk contribution
        sorted_positions = sorted(position_breakdown.items(), 
                                key=lambda x: x[1]['risk_contribution'], reverse=True)
        for i, (pos_id, pos_data) in enumerate(sorted_positions):
            position_breakdown[pos_id]['risk_rank'] = i + 1
        
        return {
            'timestamp': datetime.now().isoformat(),
            'risk_dashboard': risk_dashboard,
            'portfolio_metrics': {
                'total_exposure': portfolio_risk['total_exposure'],
                'portfolio_volatility': portfolio_risk['portfolio_volatility'],
                'portfolio_var': portfolio_risk['portfolio_var_1day'],
                'diversification_score': portfolio_risk['diversification_score'],
                'average_correlation': portfolio_risk['average_correlation']
            },
            'concentration_analysis': {
                'eth_exposure': portfolio_risk['eth_exposure'],
                'btc_exposure': portfolio_risk['btc_exposure'],
                'short_term_exposure': portfolio_risk['short_term_exposure'],
                'mid_term_exposure': portfolio_risk['mid_term_exposure'],
                'long_term_exposure': portfolio_risk['long_term_exposure']
            },
            'position_breakdown': position_breakdown,
            'risk_validation': validation,
            'risk_limits': {
                'max_total_exposure': self.risk_limits.max_total_exposure,
                'max_single_asset': self.risk_limits.max_single_asset,
                'max_single_timeframe': self.risk_limits.max_single_timeframe,
                'max_portfolio_volatility': self.risk_limits.max_portfolio_volatility,
                'max_portfolio_var': self.risk_limits.max_portfolio_var
            }
        }
    
    def suggest_risk_adjustments(self, allocations: Dict[str, float]) -> Dict:
        """Suggest risk adjustments for allocations that violate limits"""
        
        try:
            # Validate current allocations
            validation = self.validate_risk_limits(allocations)
            
            if validation['risk_valid']:
                return {
                    'adjustments_needed': False,
                    'adjusted_allocations': allocations,
                    'adjustments_made': []
                }
            
            adjusted_allocations = allocations.copy()
            adjustments_made = []
            
            # Calculate portfolio metrics for reference
            portfolio_metrics = self._calculate_portfolio_metrics(adjusted_allocations)
            
            # Adjustment 1: Scale down if total exposure exceeds limit
            total_exposure = sum(adjusted_allocations.values())
            if total_exposure > self.risk_limits.max_total_exposure:
                scale_factor = self.risk_limits.max_total_exposure / total_exposure
                for position in adjusted_allocations:
                    old_value = adjusted_allocations[position]
                    adjusted_allocations[position] *= scale_factor
                    adjustments_made.append(f"Scaled {position} from {old_value:.1%} to {adjusted_allocations[position]:.1%}")
            
            # Adjustment 2: Cap individual positions
            for position, allocation in adjusted_allocations.items():
                if allocation > self.risk_limits.max_position_size:
                    old_value = allocation
                    adjusted_allocations[position] = self.risk_limits.max_position_size
                    adjustments_made.append(f"Capped {position} from {old_value:.1%} to {self.risk_limits.max_position_size:.1%}")
            
            # Adjustment 3: VaR-based scaling
            portfolio_var = portfolio_metrics['portfolio_var']
            if portfolio_var > self.risk_limits.max_portfolio_var:
                # Scale down all positions proportionally to reduce VaR
                var_scale_factor = self.risk_limits.max_portfolio_var / portfolio_var * 0.9  # 10% buffer
                for position in adjusted_allocations:
                    old_value = adjusted_allocations[position]
                    adjusted_allocations[position] *= var_scale_factor
                    if old_value != adjusted_allocations[position]:
                        adjustments_made.append(f"VaR scaling {position} from {old_value:.1%} to {adjusted_allocations[position]:.1%}")
            
            # Adjustment 4: Diversification enhancement
            # Reduce concentration in over-allocated assets
            eth_total = sum(v for k, v in adjusted_allocations.items() if 'ETH' in k)
            btc_total = sum(v for k, v in adjusted_allocations.items() if 'BTC' in k)
            
            # If one asset class is > max_single_asset, reduce it
            if eth_total > self.risk_limits.max_single_asset:
                eth_scale = self.risk_limits.max_single_asset / eth_total
                for position in adjusted_allocations:
                    if 'ETH' in position:
                        old_value = adjusted_allocations[position]
                        adjusted_allocations[position] *= eth_scale
                        adjustments_made.append(f"ETH diversification: {position} from {old_value:.1%} to {adjusted_allocations[position]:.1%}")
            
            if btc_total > self.risk_limits.max_single_asset:
                btc_scale = self.risk_limits.max_single_asset / btc_total
                for position in adjusted_allocations:
                    if 'BTC' in position:
                        old_value = adjusted_allocations[position]
                        adjusted_allocations[position] *= btc_scale
                        adjustments_made.append(f"BTC diversification: {position} from {old_value:.1%} to {adjusted_allocations[position]:.1%}")
            
            # Adjustment 5: Minimum position enforcement
            for position in list(adjusted_allocations.keys()):
                if adjusted_allocations[position] < self.risk_limits.min_position_size:
                    old_value = adjusted_allocations[position]
                    adjusted_allocations[position] = 0.0
                    adjustments_made.append(f"Eliminated small position {position}: {old_value:.1%} → 0.0%")
            
            # Remove zero positions
            adjusted_allocations = {k: v for k, v in adjusted_allocations.items() if v > 0.001}
            
            return {
                'adjustments_needed': len(adjustments_made) > 0,
                'adjusted_allocations': adjusted_allocations,
                'adjustments_made': adjustments_made,
                'original_total_exposure': sum(allocations.values()),
                'adjusted_total_exposure': sum(adjusted_allocations.values()),
                'risk_reduction_achieved': portfolio_metrics['portfolio_var'] > self._calculate_portfolio_metrics(adjusted_allocations)['portfolio_var']
            }
            
        except Exception as e:
            logger.error(f"Error suggesting risk adjustments: {e}")
            return {
                'adjustments_needed': False,
                'adjusted_allocations': allocations,
                'adjustments_made': [],
                'error': str(e)
            }

def main():
    """Main function for risk manager testing"""
    
    risk_manager = SixPositionRiskManager()
    
    print("🛡️ SIX POSITION RISK MANAGER")
    print("=" * 50)
    
    # Test allocations
    test_allocations = {
        'ETH_1min': 0.10,
        'ETH_1hour': 0.25,
        'ETH_1day': 0.25,
        'BTC_1min': 0.05,
        'BTC_1hour': 0.15,
        'BTC_1day': 0.15
    }
    
    print("Testing portfolio allocation:")
    for pos, alloc in test_allocations.items():
        print(f"  {pos}: {alloc:.1%}")
    
    # Generate risk report
    print("\nGenerating comprehensive risk report...")
    risk_report = risk_manager.generate_risk_report(test_allocations)
    
    # Display results
    dashboard = risk_report['risk_dashboard']
    print(f"\n🎯 Risk Dashboard:")
    print(f"  Overall Status: {dashboard['overall_risk_status']}")
    print(f"  Risk Score: {dashboard['risk_score']:.1f}")
    print(f"  Violations: {dashboard['total_violations']}")
    print(f"  Warnings: {dashboard['total_warnings']}")
    
    # Portfolio metrics
    metrics = risk_report['portfolio_metrics']
    print(f"\n📊 Portfolio Metrics:")
    print(f"  Total Exposure: {metrics['total_exposure']:.1%}")
    print(f"  Portfolio Volatility: {metrics['portfolio_volatility']:.1%}")
    print(f"  Portfolio VaR (1-day): {metrics['portfolio_var']:.1%}")
    print(f"  Diversification Score: {metrics['diversification_score']:.2f}")
    print(f"  Average Correlation: {metrics['average_correlation']:.2f}")
    
    # Concentration analysis
    concentration = risk_report['concentration_analysis']
    print(f"\n🎯 Concentration Analysis:")
    print(f"  ETH Exposure: {concentration['eth_exposure']:.1%}")
    print(f"  BTC Exposure: {concentration['btc_exposure']:.1%}")
    print(f"  Short-term (1min): {concentration['short_term_exposure']:.1%}")
    print(f"  Mid-term (1hour): {concentration['mid_term_exposure']:.1%}")
    print(f"  Long-term (1day): {concentration['long_term_exposure']:.1%}")
    
    # Position ranking
    print(f"\n🏆 Position Risk Ranking:")
    position_breakdown = risk_report['position_breakdown']
    sorted_positions = sorted(position_breakdown.items(), 
                            key=lambda x: x[1]['risk_rank'])
    
    for pos_id, pos_data in sorted_positions:
        print(f"  #{pos_data['risk_rank']} {pos_id}: {pos_data['allocation']:.1%} "
              f"(VaR: {pos_data['var_1day']:.2%})")
    
    # Risk validation
    validation = risk_report['risk_validation']
    if validation['violations']:
        print(f"\n❌ Risk Violations:")
        for violation in validation['violations']:
            print(f"  • {violation}")
    
    if validation['warnings']:
        print(f"\n⚠️ Risk Warnings:")
        for warning in validation['warnings']:
            print(f"  • {warning}")
    
    print("\n🛡️ Risk analysis complete!")

if __name__ == "__main__":
    main()
