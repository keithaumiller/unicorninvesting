#!/usr/bin/env python3
"""
🦄 Unicorn Alpha-to-Portfolio Integration Example
Demonstrates how risk-integrated portfolio construction connects to existing alpha models
"""

import sys
import os
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn')

import numpy as np
import pandas as pd
from typing import Dict, List
import logging

# Import our integrated framework
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolio_construction')
from UnicornRiskIntegratedPortfolioConstruction import (
    UnicornRiskIntegratedPortfolioConstruction,
    RiskBudget,
    PortfolioTarget
)

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AlphaModelOutput:
    """Simulates output from your alpha models (BTC, ETH, etc.)"""
    
    def __init__(self):
        self.model_predictions = {}
    
    def get_crypto_insights(self) -> Dict[str, float]:
        """
        Simulate alpha model outputs from your trained models
        (BTC Prophet, ETH XGBoost, etc.)
        """
        # These would come from your actual alpha models in 2_alpha_models/
        crypto_insights = {
            'BTC': 0.18,   # 18% expected return (from BTC ensemble model)
            'ETH': 0.15,   # 15% expected return (from ETH ensemble model) 
            'ADA': 0.12,   # 12% expected return (from Cardano model)
            'SOL': 0.10,   # 10% expected return (from Solana model)
        }
        
        logger.info("📈 Alpha Model Insights Generated:")
        for symbol, expected_return in crypto_insights.items():
            logger.info(f"   {symbol}: {expected_return:.1%} expected return")
        
        return crypto_insights
    
    def get_traditional_insights(self) -> Dict[str, float]:
        """Simulate traditional asset insights"""
        traditional_insights = {
            'SPY': 0.08,   # 8% expected return (S&P 500)
            'QQQ': 0.10,   # 10% expected return (NASDAQ)
            'GLD': 0.04,   # 4% expected return (Gold)
            'TLT': 0.03,   # 3% expected return (Treasury bonds)
        }
        
        logger.info("📊 Traditional Asset Insights:")
        for symbol, expected_return in traditional_insights.items():
            logger.info(f"   {symbol}: {expected_return:.1%} expected return")
        
        return traditional_insights

class UnicornDataProvider:
    """Simulates data provision for portfolio construction"""
    
    def get_price_data(self, symbols: List[str]) -> pd.DataFrame:
        """Generate realistic price data for risk assessment"""
        
        # Generate 1 year of daily data
        dates = pd.date_range('2023-01-01', '2024-01-01', freq='D')
        np.random.seed(42)  # Reproducible results
        
        price_data = {}
        
        for symbol in symbols:
            # Asset-specific volatility and drift
            if symbol in ['BTC', 'ETH', 'ADA', 'SOL']:
                # Crypto: higher volatility
                daily_vol = 0.04  # 4% daily volatility
                annual_drift = 0.10  # 10% annual drift
            elif symbol in ['SPY', 'QQQ']:
                # Equities: moderate volatility
                daily_vol = 0.015  # 1.5% daily volatility
                annual_drift = 0.08  # 8% annual drift
            else:
                # Bonds/Gold: lower volatility
                daily_vol = 0.008  # 0.8% daily volatility
                annual_drift = 0.03  # 3% annual drift
            
            # Generate price series
            daily_returns = np.random.normal(annual_drift/252, daily_vol, len(dates))
            prices = 100 * np.exp(np.cumsum(daily_returns))
            price_data[symbol] = prices
        
        return pd.DataFrame(price_data, index=dates)
    
    def get_correlation_matrix(self, symbols: List[str]) -> pd.DataFrame:
        """Generate realistic correlation matrix"""
        
        # Define correlation structure
        correlations = {
            # Crypto correlations
            ('BTC', 'ETH'): 0.75,
            ('BTC', 'ADA'): 0.65,
            ('BTC', 'SOL'): 0.70,
            ('ETH', 'ADA'): 0.68,
            ('ETH', 'SOL'): 0.72,
            ('ADA', 'SOL'): 0.60,
            
            # Equity correlations
            ('SPY', 'QQQ'): 0.85,
            
            # Cross-asset correlations
            ('BTC', 'SPY'): 0.30,
            ('BTC', 'QQQ'): 0.35,
            ('ETH', 'SPY'): 0.32,
            ('ETH', 'QQQ'): 0.38,
            
            # Safe haven assets
            ('GLD', 'SPY'): -0.10,
            ('GLD', 'BTC'): 0.15,
            ('TLT', 'SPY'): -0.20,
            ('TLT', 'BTC'): -0.05,
        }
        
        # Build correlation matrix
        n = len(symbols)
        corr_matrix = np.eye(n)
        
        for i, symbol1 in enumerate(symbols):
            for j, symbol2 in enumerate(symbols):
                if i != j:
                    key = (symbol1, symbol2)
                    reverse_key = (symbol2, symbol1)
                    
                    if key in correlations:
                        corr_matrix[i, j] = correlations[key]
                    elif reverse_key in correlations:
                        corr_matrix[i, j] = correlations[reverse_key]
                    else:
                        corr_matrix[i, j] = 0.1  # Default low correlation
        
        return pd.DataFrame(corr_matrix, index=symbols, columns=symbols)

class UnicornPortfolioManager:
    """
    Main portfolio management system integrating alpha models with risk-aware construction
    """
    
    def __init__(self):
        # Initialize components
        self.alpha_models = AlphaModelOutput()
        self.data_provider = UnicornDataProvider()
        
        # Risk budget configuration
        self.risk_budget = RiskBudget(
            total_risk_budget=0.15,      # 15% max portfolio volatility
            concentration_limit=0.25,    # 25% max single position
            asset_class_limit=0.60,      # 60% max to any asset class
            liquidity_requirement=0.90   # 90% liquidity requirement
        )
        
        # Portfolio construction engine
        self.portfolio_engine = UnicornRiskIntegratedPortfolioConstruction(
            self.risk_budget
        )
        
        logger.info("🦄 Unicorn Portfolio Manager initialized")
        logger.info(f"📊 Risk Budget: {self.risk_budget.total_risk_budget:.0%} max volatility")
    
    def generate_full_portfolio(self) -> List[PortfolioTarget]:
        """
        Complete portfolio generation workflow:
        Alpha Models → Risk Integration → Portfolio Targets
        """
        
        print("\n🦄 UNICORN PORTFOLIO GENERATION WORKFLOW")
        print("=" * 50)
        
        # Step 1: Gather alpha insights
        logger.info("1️⃣ Gathering Alpha Model Insights...")
        crypto_insights = self.alpha_models.get_crypto_insights()
        traditional_insights = self.alpha_models.get_traditional_insights()
        
        # Combine all insights
        all_insights = {**crypto_insights, **traditional_insights}
        symbols = list(all_insights.keys())
        
        # Step 2: Gather market data for risk assessment
        logger.info("2️⃣ Collecting Market Data for Risk Assessment...")
        price_data = self.data_provider.get_price_data(symbols)
        correlation_matrix = self.data_provider.get_correlation_matrix(symbols)
        
        logger.info(f"   📊 Price data: {len(price_data)} days for {len(symbols)} assets")
        logger.info(f"   🔗 Correlation range: {correlation_matrix.values[correlation_matrix.values != 1].min():.2f} to {correlation_matrix.values[correlation_matrix.values != 1].max():.2f}")
        
        # Step 3: Current portfolio state (assume equal weight starting point)
        current_positions = {symbol: 1.0/len(symbols) for symbol in symbols}
        
        # Step 4: Risk-integrated portfolio construction
        logger.info("3️⃣ Risk-Integrated Portfolio Construction...")
        targets = self.portfolio_engine.construct_portfolio(
            insights=all_insights,
            current_positions=current_positions,
            price_data=price_data,
            correlation_matrix=correlation_matrix
        )
        
        return targets
    
    def analyze_portfolio_composition(self, targets: List[PortfolioTarget]):
        """Analyze the final portfolio composition"""
        
        print("\n📊 PORTFOLIO COMPOSITION ANALYSIS")
        print("=" * 40)
        
        # Group by asset class
        crypto_allocation = sum(t.target_weight for t in targets if t.symbol in ['BTC', 'ETH', 'ADA', 'SOL'])
        equity_allocation = sum(t.target_weight for t in targets if t.symbol in ['SPY', 'QQQ'])
        safe_haven_allocation = sum(t.target_weight for t in targets if t.symbol in ['GLD', 'TLT'])
        
        print(f"Asset Class Allocation:")
        print(f"  🪙 Cryptocurrency: {crypto_allocation:.1%}")
        print(f"  📈 Equities:       {equity_allocation:.1%}")
        print(f"  🛡️ Safe Haven:     {safe_haven_allocation:.1%}")
        
        print(f"\nTop 5 Positions:")
        for i, target in enumerate(targets[:5], 1):
            print(f"  {i}. {target.symbol}: {target.target_weight:.1%} "
                  f"(Risk: {target.risk_contribution:.2%}, "
                  f"Expected: {target.confidence:.1%})")
        
        # Risk metrics
        total_risk_contribution = sum(t.risk_contribution for t in targets)
        print(f"\nRisk Analysis:")
        print(f"  📊 Total Risk Contribution: {total_risk_contribution:.2%}")
        print(f"  🎯 Risk Budget Utilization: {total_risk_contribution/self.risk_budget.total_risk_budget:.1%}")
        
        # Check risk constraints
        max_position = max(t.target_weight for t in targets)
        print(f"  ⚖️ Maximum Position: {max_position:.1%} (Limit: {self.risk_budget.concentration_limit:.1%})")
        
    def monitor_ongoing_risk(self, targets: List[PortfolioTarget]):
        """Demonstrate ongoing risk monitoring"""
        
        print("\n🔍 ONGOING RISK MONITORING")
        print("=" * 30)
        
        # Simulate current positions based on targets
        current_positions = {t.symbol: t.target_weight for t in targets}
        
        # Get fresh data for monitoring
        symbols = list(current_positions.keys())
        price_data = self.data_provider.get_price_data(symbols)
        correlation_matrix = self.data_provider.get_correlation_matrix(symbols)
        
        # Check risk budget utilization
        recommendation = self.portfolio_engine.monitor_risk_budget_utilization(
            current_positions, price_data, correlation_matrix
        )
        
        print(f"💡 Risk Management Recommendation: {recommendation}")
        
        if recommendation == "INCREASE":
            print("   → Portfolio is under-utilizing risk budget")
            print("   → Consider increasing position sizes for better returns")
        elif recommendation == "DECREASE":
            print("   → Portfolio is exceeding risk budget")
            print("   → Must reduce position sizes to manage risk")
        else:
            print("   → Portfolio risk utilization is optimal")

def main():
    """Demonstrate the complete alpha-to-portfolio integration"""
    
    try:
        # Initialize portfolio manager
        portfolio_manager = UnicornPortfolioManager()
        
        # Generate portfolio
        targets = portfolio_manager.generate_full_portfolio()
        
        # Analyze results
        portfolio_manager.analyze_portfolio_composition(targets)
        
        # Demonstrate ongoing monitoring
        portfolio_manager.monitor_ongoing_risk(targets)
        
        print("\n✅ INTEGRATION DEMONSTRATION COMPLETE")
        print("=" * 45)
        print("This shows how your alpha models (BTC, ETH, etc.) integrate")
        print("with risk-aware portfolio construction for optimal allocation.")
        
    except Exception as e:
        logger.error(f"❌ Error in portfolio generation: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
