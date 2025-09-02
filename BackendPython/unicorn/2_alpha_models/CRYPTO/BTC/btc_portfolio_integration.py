#!/usr/bin/env python3
"""
Bitcoin Portfolio Integration Manager
Integrates Bitcoin models with portfolio management system
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

# Add paths for portfolio integration
current_dir = os.path.dirname(os.path.abspath(__file__))
portfolio_dir = os.path.abspath(os.path.join(current_dir, '../../../4_portfolios/Myportolio'))
utilities_dir = os.path.join(portfolio_dir, 'utilities')
risk_dir = os.path.join(portfolio_dir, 'risk_algorithms')

sys.path.extend([current_dir, portfolio_dir, utilities_dir, risk_dir])

try:
    from btc_production_framework import BTCProductionModelFramework
except ImportError as e:
    logger.warning(f"BTC framework import warning: {e}")

try:
    from EnhancedPortfolioManager import EnhancedPortfolioManager
except ImportError:
    logger.warning("EnhancedPortfolioManager not available")
    EnhancedPortfolioManager = None

try:
    from Kelly_criterion import KellyCriterionCalculator
except ImportError:
    logger.warning("KellyCriterionCalculator not available")
    KellyCriterionCalculator = None

try:
    from eth_basic_risk import ETHBasicRisk
except ImportError:
    logger.warning("ETHBasicRisk not available")
    ETHBasicRisk = None

class BTCPortfolioIntegration:
    """
    Bitcoin portfolio integration with risk management and position sizing
    """
    
    def __init__(self):
        """Initialize Bitcoin portfolio integration"""
        
        self.btc_framework = BTCProductionModelFramework()
        
        # Initialize Kelly calculator if available
        if KellyCriterionCalculator is not None:
            self.kelly_calculator = KellyCriterionCalculator()
        else:
            self.kelly_calculator = None
            logger.warning("Kelly Criterion calculator not available - using simplified position sizing")
        
        # Bitcoin-specific portfolio parameters
        self.btc_config = {
            'symbol': 'BTC-USD',
            'max_position_size': 0.40,  # 40% max allocation to BTC
            'min_position_size': 0.05,  # 5% minimum allocation
            'rebalance_threshold': 0.05,  # 5% drift threshold
            'risk_multiplier': 0.75,  # Conservative multiplier for Kelly
            'stop_loss_threshold': 0.15,  # 15% stop loss
            'profit_taking_threshold': 0.25,  # 25% profit taking
        }
        
        # Portfolio state
        self.current_position = 0.0
        self.target_position = 0.0
        self.last_rebalance = None
        self.signals_history = []
        
        logger.info("Bitcoin Portfolio Integration initialized")
    
    def get_btc_signals(self, timeframe: str = '1hour') -> Dict:
        """Get Bitcoin trading signals from production models"""
        
        try:
            # Fetch latest Bitcoin data
            data = self.btc_framework.fetch_btc_data(timeframe='1h', period='30d')
            
            if data.empty:
                return {'error': 'No data available'}
            
            # Get signals from available models
            signals = {}
            
            # Prophet signal (trend-based)
            prophet_signal = self._get_prophet_signal(data)
            signals['prophet'] = prophet_signal
            
            # XGBoost signal (ML-based)
            xgboost_signal = self._get_xgboost_signal(data)
            signals['xgboost'] = xgboost_signal
            
            # Momentum signal (technical)
            momentum_signal = self._get_momentum_signal(data)
            signals['momentum'] = momentum_signal
            
            # Risk-adjusted signal
            risk_signal = self._get_risk_adjusted_signal(data)
            signals['risk_adjusted'] = risk_signal
            
            # Ensemble signal (weighted combination)
            ensemble_signal = self._get_ensemble_signal(signals)
            signals['ensemble'] = ensemble_signal
            
            # Calculate confidence and position sizing
            position_recommendation = self._calculate_position_sizing(signals, data)
            
            return {
                'timestamp': datetime.now().isoformat(),
                'timeframe': timeframe,
                'signals': signals,
                'position_recommendation': position_recommendation,
                'current_price': data['Close'].iloc[-1],
                'data_points': len(data)
            }
            
        except Exception as e:
            logger.error(f"Error getting BTC signals: {e}")
            return {'error': str(e)}
    
    def _get_prophet_signal(self, data: pd.DataFrame) -> Dict:
        """Generate Prophet-based trend signal"""
        
        try:
            # Simple trend analysis (replace with actual Prophet model)
            short_ma = data['Close'].rolling(window=12).mean()
            long_ma = data['Close'].rolling(window=26).mean()
            
            current_price = data['Close'].iloc[-1]
            short_trend = short_ma.iloc[-1]
            long_trend = long_ma.iloc[-1]
            
            # Trend strength
            trend_strength = abs(short_trend - long_trend) / long_trend
            
            if short_trend > long_trend:
                signal = 'BUY'
                confidence = min(0.9, trend_strength * 10)
            elif short_trend < long_trend:
                signal = 'SELL'
                confidence = min(0.9, trend_strength * 10)
            else:
                signal = 'HOLD'
                confidence = 0.5
            
            return {
                'signal': signal,
                'confidence': confidence,
                'trend_strength': trend_strength,
                'price_target': short_trend * (1 + trend_strength * (1 if signal == 'BUY' else -1))
            }
            
        except Exception as e:
            return {'signal': 'HOLD', 'confidence': 0.0, 'error': str(e)}
    
    def _get_xgboost_signal(self, data: pd.DataFrame) -> Dict:
        """Generate XGBoost ML-based signal"""
        
        try:
            # Feature engineering for ML signal
            features = self.btc_framework.create_features_for_prediction(data)
            
            if features.empty:
                return {'signal': 'HOLD', 'confidence': 0.0, 'error': 'No features available'}
            
            # Simplified ML-based signal (replace with actual XGBoost model)
            latest_features = features.iloc[-1]
            
            # Simple scoring based on technical indicators
            score = 0
            
            # RSI signal
            if 'rsi' in latest_features:
                rsi = latest_features['rsi']
                if rsi < 30:
                    score += 0.3  # Oversold
                elif rsi > 70:
                    score -= 0.3  # Overbought
            
            # MACD signal
            if 'macd' in latest_features:
                macd = latest_features['macd']
                if macd > 0:
                    score += 0.2
                else:
                    score -= 0.2
            
            # Volume signal
            if 'volume_ratio' in latest_features:
                vol_ratio = latest_features['volume_ratio']
                if vol_ratio > 1.2:
                    score += 0.2  # High volume confirmation
            
            # Momentum signals
            momentum_features = [col for col in latest_features.index if 'momentum' in col]
            for momentum_col in momentum_features:
                momentum_val = latest_features[momentum_col]
                if momentum_val > 0.02:
                    score += 0.1
                elif momentum_val < -0.02:
                    score -= 0.1
            
            # Generate signal
            if score > 0.3:
                signal = 'BUY'
                confidence = min(0.9, abs(score))
            elif score < -0.3:
                signal = 'SELL' 
                confidence = min(0.9, abs(score))
            else:
                signal = 'HOLD'
                confidence = 0.5
            
            return {
                'signal': signal,
                'confidence': confidence,
                'ml_score': score,
                'feature_count': len(latest_features)
            }
            
        except Exception as e:
            return {'signal': 'HOLD', 'confidence': 0.0, 'error': str(e)}
    
    def _get_momentum_signal(self, data: pd.DataFrame) -> Dict:
        """Generate momentum-based signal"""
        
        try:
            # Calculate momentum indicators
            returns_1d = data['Close'].pct_change(1).iloc[-1]
            returns_7d = data['Close'].pct_change(7).iloc[-1] if len(data) > 7 else 0
            returns_30d = data['Close'].pct_change(30).iloc[-1] if len(data) > 30 else 0
            
            # Volatility-adjusted momentum
            volatility = data['Close'].pct_change().rolling(window=20).std().iloc[-1]
            
            if volatility > 0:
                momentum_score = (returns_1d * 0.5 + returns_7d * 0.3 + returns_30d * 0.2) / volatility
            else:
                momentum_score = 0
            
            # Generate signal
            if momentum_score > 0.5:
                signal = 'BUY'
                confidence = min(0.8, abs(momentum_score))
            elif momentum_score < -0.5:
                signal = 'SELL'
                confidence = min(0.8, abs(momentum_score))
            else:
                signal = 'HOLD'
                confidence = 0.4
            
            return {
                'signal': signal,
                'confidence': confidence,
                'momentum_score': momentum_score,
                'returns_1d': returns_1d,
                'returns_7d': returns_7d,
                'volatility': volatility
            }
            
        except Exception as e:
            return {'signal': 'HOLD', 'confidence': 0.0, 'error': str(e)}
    
    def _get_risk_adjusted_signal(self, data: pd.DataFrame) -> Dict:
        """Generate risk-adjusted signal"""
        
        try:
            # Calculate risk metrics
            returns = data['Close'].pct_change().dropna()
            
            if len(returns) < 20:
                return {'signal': 'HOLD', 'confidence': 0.0, 'error': 'Insufficient data'}
            
            # VaR calculation (95% confidence)
            var_95 = np.percentile(returns, 5)
            
            # Maximum drawdown
            cumulative = (1 + returns).cumprod()
            running_max = cumulative.expanding().max()
            drawdown = (cumulative - running_max) / running_max
            max_drawdown = drawdown.min()
            
            # Sharpe ratio (annualized)
            sharpe_ratio = (returns.mean() * 252) / (returns.std() * np.sqrt(252))
            
            # Risk score (higher is better)
            risk_score = 0
            
            # VaR check
            if var_95 > -0.05:  # VaR better than -5%
                risk_score += 0.3
            elif var_95 < -0.10:  # VaR worse than -10%
                risk_score -= 0.4
            
            # Drawdown check
            if max_drawdown > -0.10:  # Max drawdown better than -10%
                risk_score += 0.3
            elif max_drawdown < -0.20:  # Max drawdown worse than -20%
                risk_score -= 0.4
            
            # Sharpe ratio check
            if sharpe_ratio > 1.0:
                risk_score += 0.3
            elif sharpe_ratio < 0:
                risk_score -= 0.3
            
            # Recent volatility check
            recent_vol = returns.tail(10).std()
            historical_vol = returns.std()
            
            if recent_vol < historical_vol * 1.2:  # Volatility not elevated
                risk_score += 0.2
            elif recent_vol > historical_vol * 2.0:  # High volatility
                risk_score -= 0.3
            
            # Generate signal based on risk assessment
            if risk_score > 0.4:
                signal = 'BUY'
                confidence = min(0.8, risk_score)
            elif risk_score < -0.4:
                signal = 'SELL'
                confidence = min(0.8, abs(risk_score))
            else:
                signal = 'HOLD'
                confidence = 0.5
            
            return {
                'signal': signal,
                'confidence': confidence,
                'risk_score': risk_score,
                'var_95': var_95,
                'max_drawdown': max_drawdown,
                'sharpe_ratio': sharpe_ratio,
                'recent_volatility': recent_vol
            }
            
        except Exception as e:
            return {'signal': 'HOLD', 'confidence': 0.0, 'error': str(e)}
    
    def _get_ensemble_signal(self, signals: Dict) -> Dict:
        """Generate ensemble signal from all component signals"""
        
        try:
            # Weight different signal types
            weights = {
                'prophet': 0.25,
                'xgboost': 0.30,
                'momentum': 0.25,
                'risk_adjusted': 0.20
            }
            
            # Convert signals to numeric scores
            signal_scores = {}
            total_confidence = 0
            
            for signal_type, weight in weights.items():
                if signal_type in signals and 'error' not in signals[signal_type]:
                    signal_data = signals[signal_type]
                    
                    # Convert signal to score
                    if signal_data['signal'] == 'BUY':
                        score = 1.0
                    elif signal_data['signal'] == 'SELL':
                        score = -1.0
                    else:
                        score = 0.0
                    
                    # Weight by confidence
                    confidence = signal_data.get('confidence', 0.5)
                    weighted_score = score * confidence * weight
                    
                    signal_scores[signal_type] = weighted_score
                    total_confidence += confidence * weight
            
            # Calculate ensemble score
            ensemble_score = sum(signal_scores.values())
            
            # Generate ensemble signal
            if ensemble_score > 0.2:
                signal = 'BUY'
                confidence = min(0.9, total_confidence + abs(ensemble_score) * 0.5)
            elif ensemble_score < -0.2:
                signal = 'SELL'
                confidence = min(0.9, total_confidence + abs(ensemble_score) * 0.5)
            else:
                signal = 'HOLD'
                confidence = total_confidence * 0.5
            
            return {
                'signal': signal,
                'confidence': confidence,
                'ensemble_score': ensemble_score,
                'component_scores': signal_scores,
                'total_confidence': total_confidence
            }
            
        except Exception as e:
            return {'signal': 'HOLD', 'confidence': 0.0, 'error': str(e)}
    
    def _calculate_position_sizing(self, signals: Dict, data: pd.DataFrame) -> Dict:
        """Calculate optimal position sizing using Kelly Criterion and risk management"""
        
        try:
            ensemble_signal = signals.get('ensemble', {})
            
            if 'error' in ensemble_signal:
                return {'target_allocation': 0.0, 'error': 'No valid ensemble signal'}
            
            signal = ensemble_signal.get('signal', 'HOLD')
            confidence = ensemble_signal.get('confidence', 0.0)
            
            if signal == 'HOLD':
                return {
                    'target_allocation': 0.05,  # Minimum allocation
                    'action': 'HOLD',
                    'confidence': confidence,
                    'reasoning': 'Neutral signal - maintain minimum allocation'
                }
            
            # Calculate Kelly position size
            returns = data['Close'].pct_change().dropna()
            
            if len(returns) < 20:
                return {'target_allocation': 0.05, 'error': 'Insufficient data for Kelly calculation'}
            
            # Simplified Kelly calculation
            win_rate = len(returns[returns > 0]) / len(returns)
            avg_win = returns[returns > 0].mean() if len(returns[returns > 0]) > 0 else 0
            avg_loss = abs(returns[returns < 0].mean()) if len(returns[returns < 0]) > 0 else 0.01
            
            if avg_loss > 0:
                kelly_fraction = (win_rate * avg_win - (1 - win_rate) * avg_loss) / avg_win
            else:
                kelly_fraction = 0
            
            # Apply risk multiplier and confidence adjustment
            kelly_fraction *= self.btc_config['risk_multiplier']
            kelly_fraction *= confidence
            
            # Determine target allocation based on signal
            if signal == 'BUY':
                target_allocation = max(
                    self.btc_config['min_position_size'],
                    min(self.btc_config['max_position_size'], kelly_fraction)
                )
                action = 'BUY'
            else:  # SELL
                target_allocation = max(0.0, kelly_fraction)  # Can go to zero on sell
                action = 'REDUCE' if target_allocation > 0 else 'SELL_ALL'
            
            return {
                'target_allocation': target_allocation,
                'action': action,
                'confidence': confidence,
                'kelly_fraction': kelly_fraction,
                'win_rate': win_rate,
                'avg_win': avg_win,
                'avg_loss': avg_loss,
                'reasoning': f'{signal} signal with {confidence:.2f} confidence, Kelly suggests {kelly_fraction:.3f}'
            }
            
        except Exception as e:
            return {'target_allocation': 0.05, 'error': str(e)}
    
    def generate_portfolio_recommendation(self) -> Dict:
        """Generate comprehensive portfolio recommendation for Bitcoin"""
        
        try:
            # Get signals across timeframes
            signals_1hour = self.get_btc_signals('1hour')
            signals_1day = self.get_btc_signals('1day')
            
            if 'error' in signals_1hour and 'error' in signals_1day:
                return {'error': 'No valid signals available'}
            
            # Combine multi-timeframe analysis
            recommendations = []
            
            if 'error' not in signals_1hour:
                recommendations.append({
                    'timeframe': '1hour',
                    'signal': signals_1hour['signals']['ensemble']['signal'],
                    'confidence': signals_1hour['signals']['ensemble']['confidence'],
                    'target_allocation': signals_1hour['position_recommendation']['target_allocation']
                })
            
            if 'error' not in signals_1day:
                recommendations.append({
                    'timeframe': '1day',
                    'signal': signals_1day['signals']['ensemble']['signal'],
                    'confidence': signals_1day['signals']['ensemble']['confidence'],
                    'target_allocation': signals_1day['position_recommendation']['target_allocation']
                })
            
            # Weight recommendations (daily carries more weight)
            if len(recommendations) == 2:
                final_allocation = (
                    recommendations[0]['target_allocation'] * 0.3 +  # 1hour weight
                    recommendations[1]['target_allocation'] * 0.7    # 1day weight
                )
                final_confidence = (
                    recommendations[0]['confidence'] * 0.3 +
                    recommendations[1]['confidence'] * 0.7
                )
            elif len(recommendations) == 1:
                final_allocation = recommendations[0]['target_allocation']
                final_confidence = recommendations[0]['confidence']
            else:
                final_allocation = 0.05  # Default minimum
                final_confidence = 0.0
            
            # Determine action
            current_allocation = self.current_position
            allocation_diff = abs(final_allocation - current_allocation)
            
            if allocation_diff > self.btc_config['rebalance_threshold']:
                if final_allocation > current_allocation:
                    action = 'INCREASE_POSITION'
                else:
                    action = 'DECREASE_POSITION'
            else:
                action = 'HOLD_CURRENT'
            
            return {
                'timestamp': datetime.now().isoformat(),
                'current_allocation': current_allocation,
                'recommended_allocation': final_allocation,
                'allocation_change': final_allocation - current_allocation,
                'action': action,
                'confidence': final_confidence,
                'rebalance_needed': allocation_diff > self.btc_config['rebalance_threshold'],
                'multi_timeframe_analysis': recommendations,
                'risk_parameters': self.btc_config
            }
            
        except Exception as e:
            logger.error(f"Error generating portfolio recommendation: {e}")
            return {'error': str(e)}
    
    def update_position(self, new_allocation: float):
        """Update current BTC position"""
        
        self.current_position = max(0.0, min(1.0, new_allocation))
        self.last_rebalance = datetime.now()
        
        logger.info(f"BTC position updated to {self.current_position:.3f}")

def main():
    """Main function for Bitcoin portfolio integration"""
    
    # Initialize integration
    btc_integration = BTCPortfolioIntegration()
    
    print("🟠 Bitcoin Portfolio Integration")
    print("=" * 50)
    
    # Generate recommendation
    print("Generating Bitcoin portfolio recommendation...")
    recommendation = btc_integration.generate_portfolio_recommendation()
    
    if 'error' not in recommendation:
        print(f"\n📊 Portfolio Recommendation:")
        print(f"  Current Allocation: {recommendation['current_allocation']:.1%}")
        print(f"  Recommended Allocation: {recommendation['recommended_allocation']:.1%}")
        print(f"  Action: {recommendation['action']}")
        print(f"  Confidence: {recommendation['confidence']:.2f}")
        print(f"  Rebalance Needed: {recommendation['rebalance_needed']}")
        
        print(f"\n📈 Multi-Timeframe Analysis:")
        for rec in recommendation['multi_timeframe_analysis']:
            print(f"  {rec['timeframe']}: {rec['signal']} (confidence: {rec['confidence']:.2f})")
    else:
        print(f"❌ Error: {recommendation['error']}")
    
    print("\n🟠 Bitcoin portfolio integration complete!")

if __name__ == "__main__":
    main()
