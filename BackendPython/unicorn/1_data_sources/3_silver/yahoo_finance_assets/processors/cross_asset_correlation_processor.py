"""
Cross-Asset Correlation Processor for Silver Layer

This module analyzes correlations and relationships between different Yahoo Finance assets
to enhance trading signals and market regime detection.

Features:
- Crypto-forex correlation analysis
- Market regime detection based on cross-asset behavior
- Lead-lag relationship identification
- Risk factor decomposition
- Dynamic correlation tracking
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional
import logging
from pathlib import Path
from datetime import datetime
import json
from scipy.stats import pearsonr, spearmanr
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import warnings

warnings.filterwarnings('ignore')
logger = logging.getLogger(__name__)

class CrossAssetCorrelationProcessor:
    """Analyzes correlations and relationships between Yahoo Finance assets."""
    
    def __init__(self, silver_path: Optional[str] = None):
        """Initialize cross-asset correlation processor."""
        
        current_dir = Path(__file__).parent
        self.silver_path = Path(silver_path) if silver_path else current_dir / "processed_data"
        
        # Asset configurations
        self.crypto_assets = ['ETH', 'BTC']
        self.forex_assets = ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
        self.intervals = ['1d', '1h']
        
        # Correlation analysis settings
        self.correlation_window = 30  # Days for rolling correlation
        self.min_periods = 10  # Minimum periods for correlation calculation
        
        logger.info("Initialized Cross-Asset Correlation Processor")
    
    def load_silver_data(self, category: str, asset: str, interval: str) -> Optional[pd.DataFrame]:
        """Load silver layer data for analysis."""
        
        try:
            file_path = self.silver_path / category / f"{asset}_silver_{interval}_latest.csv"
            
            if not file_path.exists():
                logger.warning(f"Silver file not found: {file_path}")
                return None
            
            df = pd.read_csv(file_path, index_col=0, parse_dates=True)
            logger.info(f"Loaded {asset} {interval} silver data: {len(df)} records")
            return df
            
        except Exception as e:
            logger.error(f"Error loading silver data for {asset} {interval}: {e}")
            return None
    
    def calculate_asset_correlations(self, interval: str = '1d') -> Dict[str, any]:
        """Calculate correlations between all assets for given interval."""
        
        try:
            logger.info(f"Calculating cross-asset correlations for {interval} interval...")
            
            # Load all asset data
            price_data = {}
            return_data = {}
            
            # Load crypto assets
            for asset in self.crypto_assets:
                df = self.load_silver_data('crypto', asset, interval)
                if df is not None and 'close' in df.columns:
                    price_data[f"{asset}_crypto"] = df['close']
                    return_data[f"{asset}_crypto"] = df['close'].pct_change()
            
            # Load forex assets
            for asset in self.forex_assets:
                df = self.load_silver_data('forex', asset, interval)
                if df is not None and 'close' in df.columns:
                    price_data[f"{asset}_forex"] = df['close']
                    return_data[f"{asset}_forex"] = df['close'].pct_change()
            
            if len(price_data) < 2:
                logger.warning("Insufficient data for correlation analysis")
                return {}
            
            # Combine data
            prices_df = pd.DataFrame(price_data).dropna()
            returns_df = pd.DataFrame(return_data).dropna()
            
            # Calculate static correlations
            price_correlations = prices_df.corr()
            return_correlations = returns_df.corr()
            
            # Calculate rolling correlations for key pairs
            rolling_correlations = {}
            
            # Crypto-crypto correlations
            if 'ETH_crypto' in returns_df.columns and 'BTC_crypto' in returns_df.columns:
                rolling_correlations['ETH_BTC'] = returns_df['ETH_crypto'].rolling(
                    window=self.correlation_window, min_periods=self.min_periods
                ).corr(returns_df['BTC_crypto'])
            
            # Crypto-forex correlations (focus on USD pairs)
            for crypto in ['ETH_crypto', 'BTC_crypto']:
                if crypto in returns_df.columns:
                    for forex in ['EURUSD_forex', 'USDJPY_forex', 'GBPUSD_forex']:
                        if forex in returns_df.columns:
                            pair_name = f"{crypto.split('_')[0]}_{forex.split('_')[0]}"
                            rolling_correlations[pair_name] = returns_df[crypto].rolling(
                                window=self.correlation_window, min_periods=self.min_periods
                            ).corr(returns_df[forex])
            
            # Calculate correlation strength summary
            correlation_summary = {
                'timestamp': datetime.now().isoformat(),
                'interval': interval,
                'n_assets': len(price_data),
                'correlation_stats': {
                    'mean_correlation': float(return_correlations.mean().mean()),
                    'max_correlation': float(return_correlations.max().max()),
                    'min_correlation': float(return_correlations.min().min()),
                    'correlation_std': float(return_correlations.std().std())
                }
            }
            
            return {
                'price_correlations': price_correlations,
                'return_correlations': return_correlations,
                'rolling_correlations': rolling_correlations,
                'summary': correlation_summary
            }
            
        except Exception as e:
            logger.error(f"Error calculating correlations: {e}")
            return {}
    
    def detect_market_regimes(self, interval: str = '1d') -> Dict[str, any]:
        """Detect market regimes based on cross-asset behavior."""
        
        try:
            logger.info(f"Detecting market regimes for {interval} interval...")
            
            # Load volatility data from all assets
            volatility_data = {}
            
            # Crypto volatilities
            for asset in self.crypto_assets:
                df = self.load_silver_data('crypto', asset, interval)
                if df is not None and 'volatility_14' in df.columns:
                    volatility_data[f"{asset}_vol"] = df['volatility_14']
            
            # Forex volatilities
            for asset in self.forex_assets:
                df = self.load_silver_data('forex', asset, interval)
                if df is not None and 'volatility_20' in df.columns:
                    volatility_data[f"{asset}_vol"] = df['volatility_20']
            
            if len(volatility_data) < 2:
                logger.warning("Insufficient volatility data for regime detection")
                return {}
            
            # Combine volatility data
            vol_df = pd.DataFrame(volatility_data).dropna()
            
            # Calculate market stress indicator (average volatility)
            vol_df['market_stress'] = vol_df.mean(axis=1)
            vol_df['stress_percentile'] = vol_df['market_stress'].rolling(
                window=252 if interval == '1d' else 252*24
            ).rank(pct=True)
            
            # Define market regimes based on stress percentile
            vol_df['market_regime'] = np.where(
                vol_df['stress_percentile'] > 0.8, 'high_stress',
                np.where(vol_df['stress_percentile'] < 0.2, 'low_stress', 'normal')
            )
            
            # Calculate regime statistics
            regime_stats = vol_df['market_regime'].value_counts(normalize=True).to_dict()
            
            # Recent regime (last 30 observations)
            recent_regime = vol_df['market_regime'].tail(30).mode().iloc[0] if len(vol_df) >= 30 else 'unknown'
            
            return {
                'regime_data': vol_df,
                'regime_statistics': regime_stats,
                'current_regime': recent_regime,
                'market_stress_level': float(vol_df['stress_percentile'].iloc[-1]) if len(vol_df) > 0 else 0
            }
            
        except Exception as e:
            logger.error(f"Error detecting market regimes: {e}")
            return {}
    
    def perform_risk_factor_analysis(self, interval: str = '1d') -> Dict[str, any]:
        """Perform PCA-based risk factor analysis on asset returns."""
        
        try:
            logger.info(f"Performing risk factor analysis for {interval} interval...")
            
            # Load return data
            return_data = {}
            
            # Crypto returns
            for asset in self.crypto_assets:
                df = self.load_silver_data('crypto', asset, interval)
                if df is not None and 'close' in df.columns:
                    return_data[f"{asset}"] = df['close'].pct_change()
            
            # Forex returns
            for asset in self.forex_assets:
                df = self.load_silver_data('forex', asset, interval)
                if df is not None and 'close' in df.columns:
                    return_data[f"{asset}"] = df['close'].pct_change()
            
            if len(return_data) < 3:
                logger.warning("Insufficient data for PCA analysis")
                return {}
            
            # Combine and clean data
            returns_df = pd.DataFrame(return_data).dropna()
            
            # Standardize returns
            scaler = StandardScaler()
            returns_scaled = scaler.fit_transform(returns_df)
            
            # Perform PCA
            pca = PCA()
            pca_result = pca.fit_transform(returns_scaled)
            
            # Extract factor loadings
            factor_loadings = pd.DataFrame(
                pca.components_[:3].T,  # First 3 factors
                columns=['Factor_1', 'Factor_2', 'Factor_3'],
                index=returns_df.columns
            )
            
            # Calculate explained variance
            explained_variance = pca.explained_variance_ratio_[:3]
            
            # Interpret factors based on loadings
            factor_interpretation = {}
            for i, factor in enumerate(['Factor_1', 'Factor_2', 'Factor_3']):
                # Find assets with highest absolute loadings
                top_assets = factor_loadings[factor].abs().nlargest(3)
                factor_interpretation[factor] = {
                    'explained_variance': float(explained_variance[i]),
                    'top_assets': top_assets.to_dict(),
                    'interpretation': self._interpret_factor(factor_loadings[factor])
                }
            
            return {
                'factor_loadings': factor_loadings,
                'explained_variance': explained_variance.tolist(),
                'factor_interpretation': factor_interpretation,
                'total_variance_explained': float(explained_variance.sum())
            }
            
        except Exception as e:
            logger.error(f"Error in risk factor analysis: {e}")
            return {}
    
    def _interpret_factor(self, loadings: pd.Series) -> str:
        """Interpret PCA factor based on asset loadings."""
        
        try:
            crypto_loading = loadings[[col for col in loadings.index if col in ['ETH', 'BTC']]].abs().mean()
            forex_loading = loadings[[col for col in loadings.index if col not in ['ETH', 'BTC']]].abs().mean()
            
            if crypto_loading > forex_loading * 1.5:
                return "Crypto-dominated factor"
            elif forex_loading > crypto_loading * 1.5:
                return "Forex-dominated factor"
            else:
                return "Mixed asset factor"
                
        except:
            return "Unknown factor"
    
    def generate_correlation_report(self) -> Dict[str, any]:
        """Generate comprehensive cross-asset correlation report."""
        
        try:
            logger.info("Generating comprehensive cross-asset correlation report...")
            
            report = {
                'generation_timestamp': datetime.now().isoformat(),
                'analysis_intervals': {}
            }
            
            # Analyze both intervals
            for interval in self.intervals:
                logger.info(f"Processing {interval} interval...")
                
                interval_analysis = {
                    'correlations': self.calculate_asset_correlations(interval),
                    'market_regimes': self.detect_market_regimes(interval),
                    'risk_factors': self.perform_risk_factor_analysis(interval)
                }
                
                report['analysis_intervals'][interval] = interval_analysis
            
            # Save report
            report_path = self.silver_path / "cross_asset_correlation_report.json"
            with open(report_path, 'w') as f:
                # Convert numpy arrays and DataFrames to JSON-serializable format
                json_report = self._make_json_serializable(report)
                json.dump(json_report, f, indent=2)
            
            logger.info(f"✅ Cross-asset correlation report saved to {report_path}")
            return report
            
        except Exception as e:
            logger.error(f"Error generating correlation report: {e}")
            return {}
    
    def _make_json_serializable(self, obj):
        """Convert complex objects to JSON-serializable format."""
        
        if isinstance(obj, dict):
            return {k: self._make_json_serializable(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [self._make_json_serializable(item) for item in obj]
        elif isinstance(obj, pd.DataFrame):
            return obj.to_dict()
        elif isinstance(obj, pd.Series):
            return obj.to_dict()
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        elif isinstance(obj, (np.integer, np.floating)):
            return float(obj)
        else:
            return obj

def main():
    """Main function for cross-asset correlation analysis."""
    
    processor = CrossAssetCorrelationProcessor()
    report = processor.generate_correlation_report()
    
    if report:
        print("✅ Cross-asset correlation analysis completed successfully")
        
        # Print summary
        for interval in report.get('analysis_intervals', {}):
            interval_data = report['analysis_intervals'][interval]
            
            print(f"\n📊 {interval.upper()} Interval Summary:")
            
            # Correlation summary
            corr_summary = interval_data.get('correlations', {}).get('summary', {})
            if corr_summary:
                print(f"   • Mean correlation: {corr_summary.get('correlation_stats', {}).get('mean_correlation', 0):.3f}")
                print(f"   • Assets analyzed: {corr_summary.get('n_assets', 0)}")
            
            # Market regime
            regime_data = interval_data.get('market_regimes', {})
            if regime_data:
                print(f"   • Current regime: {regime_data.get('current_regime', 'unknown')}")
                print(f"   • Stress level: {regime_data.get('market_stress_level', 0):.1%}")
            
            # Risk factors
            risk_data = interval_data.get('risk_factors', {})
            if risk_data:
                print(f"   • Variance explained (3 factors): {risk_data.get('total_variance_explained', 0):.1%}")
    else:
        print("❌ Cross-asset correlation analysis failed")

if __name__ == "__main__":
    main()
