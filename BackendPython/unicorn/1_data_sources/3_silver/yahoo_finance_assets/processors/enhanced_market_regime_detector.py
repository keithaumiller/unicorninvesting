"""
Enhanced Market Regime Detector for Silver Layer

This module provides advanced market regime detection using multiple indicators
and machine learning techniques to classify market conditions.

Features:
- Multi-indicator regime detection
- Volatility regime classification
- Trend regime identification
- Risk-on/risk-off detection
- Machine learning-based regime prediction
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Tuple
import logging
from pathlib import Path
from datetime import datetime, timedelta
import json
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestClassifier
from scipy.stats import zscore
import warnings

warnings.filterwarnings('ignore')
logger = logging.getLogger(__name__)

class EnhancedMarketRegimeDetector:
    """Advanced market regime detection using multiple indicators and ML."""
    
    def __init__(self, silver_path: Optional[str] = None):
        """Initialize enhanced market regime detector."""
        
        current_dir = Path(__file__).parent
        self.silver_path = Path(silver_path) if silver_path else current_dir / "processed_data"
        
        # Asset configurations
        self.crypto_assets = ['ETH', 'BTC']
        self.forex_assets = ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCAD', 'USDCHF', 'NZDUSD']
        self.intervals = ['1d', '1h']
        
        # Regime detection parameters
        self.lookback_window = 60  # Days for regime classification
        self.min_regime_duration = 5  # Minimum days for regime persistence
        self.n_regimes = 4  # Number of market regimes to detect
        
        logger.info("Initialized Enhanced Market Regime Detector")
    
    def load_silver_data(self, category: str, asset: str, interval: str) -> Optional[pd.DataFrame]:
        """Load silver layer data for regime analysis."""
        
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
    
    def calculate_regime_indicators(self, interval: str = '1d') -> pd.DataFrame:
        """Calculate comprehensive regime indicators from all assets."""
        
        try:
            logger.info(f"Calculating regime indicators for {interval} interval...")
            
            regime_indicators = {}
            
            # Load and process crypto assets
            for asset in self.crypto_assets:
                df = self.load_silver_data('crypto', asset, interval)
                if df is not None:
                    # Volatility indicators
                    if 'volatility_14' in df.columns:
                        regime_indicators[f'{asset}_volatility'] = df['volatility_14']
                    
                    # Price momentum
                    if 'close' in df.columns:
                        returns = df['close'].pct_change()
                        regime_indicators[f'{asset}_returns'] = returns
                        regime_indicators[f'{asset}_momentum_20'] = returns.rolling(20).mean()
                        regime_indicators[f'{asset}_volatility_raw'] = returns.rolling(20).std()
                    
                    # Technical indicators
                    if 'rsi_14' in df.columns:
                        regime_indicators[f'{asset}_rsi'] = df['rsi_14']
                    
                    # Volume indicators (if available)
                    if 'volume_sma_20' in df.columns:
                        regime_indicators[f'{asset}_volume_trend'] = df['volume_sma_20'].pct_change()
            
            # Load and process forex assets
            for asset in self.forex_assets:
                df = self.load_silver_data('forex', asset, interval)
                if df is not None:
                    # Volatility indicators
                    if 'volatility_20' in df.columns:
                        regime_indicators[f'{asset}_volatility'] = df['volatility_20']
                    
                    # Price momentum
                    if 'close' in df.columns:
                        returns = df['close'].pct_change()
                        regime_indicators[f'{asset}_returns'] = returns
                        regime_indicators[f'{asset}_momentum_20'] = returns.rolling(20).mean()
                    
                    # Technical indicators
                    if 'rsi_14' in df.columns:
                        regime_indicators[f'{asset}_rsi'] = df['rsi_14']
            
            if not regime_indicators:
                logger.warning("No regime indicators calculated")
                return pd.DataFrame()
            
            # Combine all indicators
            regime_df = pd.DataFrame(regime_indicators).dropna()
            
            # Calculate aggregate indicators
            regime_df['market_volatility'] = regime_df[[col for col in regime_df.columns if 'volatility' in col]].mean(axis=1)
            regime_df['market_momentum'] = regime_df[[col for col in regime_df.columns if 'momentum' in col]].mean(axis=1)
            regime_df['market_rsi'] = regime_df[[col for col in regime_df.columns if 'rsi' in col]].mean(axis=1)
            
            # Calculate crypto-specific indicators
            crypto_cols = [col for col in regime_df.columns if any(crypto in col for crypto in self.crypto_assets)]
            if crypto_cols:
                regime_df['crypto_sentiment'] = regime_df[crypto_cols].mean(axis=1)
            
            # Calculate forex-specific indicators
            forex_cols = [col for col in regime_df.columns if any(forex in col for forex in self.forex_assets)]
            if forex_cols:
                regime_df['forex_sentiment'] = regime_df[forex_cols].mean(axis=1)
            
            logger.info(f"Calculated {len(regime_df.columns)} regime indicators")
            return regime_df
            
        except Exception as e:
            logger.error(f"Error calculating regime indicators: {e}")
            return pd.DataFrame()
    
    def detect_volatility_regimes(self, regime_df: pd.DataFrame) -> pd.DataFrame:
        """Detect volatility-based market regimes."""
        
        try:
            logger.info("Detecting volatility regimes...")
            
            if 'market_volatility' not in regime_df.columns:
                logger.warning("Market volatility not available for regime detection")
                return regime_df
            
            # Calculate volatility percentiles
            vol_data = regime_df['market_volatility'].dropna()
            
            # Define volatility thresholds
            low_vol_threshold = vol_data.quantile(0.25)
            high_vol_threshold = vol_data.quantile(0.75)
            extreme_vol_threshold = vol_data.quantile(0.90)
            
            # Classify volatility regimes
            regime_df['volatility_regime'] = np.where(
                regime_df['market_volatility'] > extreme_vol_threshold, 'extreme_volatility',
                np.where(
                    regime_df['market_volatility'] > high_vol_threshold, 'high_volatility',
                    np.where(
                        regime_df['market_volatility'] < low_vol_threshold, 'low_volatility',
                        'normal_volatility'
                    )
                )
            )
            
            # Add volatility persistence (regime duration)
            regime_df['vol_regime_duration'] = regime_df.groupby(
                (regime_df['volatility_regime'] != regime_df['volatility_regime'].shift()).cumsum()
            ).cumcount() + 1
            
            logger.info("✅ Volatility regimes detected")
            return regime_df
            
        except Exception as e:
            logger.error(f"Error detecting volatility regimes: {e}")
            return regime_df
    
    def detect_trend_regimes(self, regime_df: pd.DataFrame) -> pd.DataFrame:
        """Detect trend-based market regimes."""
        
        try:
            logger.info("Detecting trend regimes...")
            
            if 'market_momentum' not in regime_df.columns:
                logger.warning("Market momentum not available for trend regime detection")
                return regime_df
            
            # Calculate momentum thresholds
            momentum_data = regime_df['market_momentum'].dropna()
            
            # Define trend thresholds
            strong_up_threshold = momentum_data.quantile(0.80)
            weak_up_threshold = momentum_data.quantile(0.60)
            weak_down_threshold = momentum_data.quantile(0.40)
            strong_down_threshold = momentum_data.quantile(0.20)
            
            # Classify trend regimes
            regime_df['trend_regime'] = np.where(
                regime_df['market_momentum'] > strong_up_threshold, 'strong_uptrend',
                np.where(
                    regime_df['market_momentum'] > weak_up_threshold, 'weak_uptrend',
                    np.where(
                        regime_df['market_momentum'] < strong_down_threshold, 'strong_downtrend',
                        np.where(
                            regime_df['market_momentum'] < weak_down_threshold, 'weak_downtrend',
                            'sideways'
                        )
                    )
                )
            )
            
            # Add trend persistence
            regime_df['trend_regime_duration'] = regime_df.groupby(
                (regime_df['trend_regime'] != regime_df['trend_regime'].shift()).cumsum()
            ).cumcount() + 1
            
            logger.info("✅ Trend regimes detected")
            return regime_df
            
        except Exception as e:
            logger.error(f"Error detecting trend regimes: {e}")
            return regime_df
    
    def detect_risk_sentiment_regimes(self, regime_df: pd.DataFrame) -> pd.DataFrame:
        """Detect risk-on/risk-off regimes."""
        
        try:
            logger.info("Detecting risk sentiment regimes...")
            
            # Use crypto sentiment as risk-on indicator
            if 'crypto_sentiment' in regime_df.columns and 'forex_sentiment' in regime_df.columns:
                # Risk-on: crypto outperforming, risk currencies strong
                risk_on_score = regime_df['crypto_sentiment'] * 0.6 + regime_df['forex_sentiment'] * 0.4
                
                # Calculate risk sentiment thresholds
                risk_threshold_high = risk_on_score.quantile(0.70)
                risk_threshold_low = risk_on_score.quantile(0.30)
                
                regime_df['risk_sentiment'] = np.where(
                    risk_on_score > risk_threshold_high, 'risk_on',
                    np.where(
                        risk_on_score < risk_threshold_low, 'risk_off',
                        'neutral'
                    )
                )
                
                # Add sentiment persistence
                regime_df['risk_sentiment_duration'] = regime_df.groupby(
                    (regime_df['risk_sentiment'] != regime_df['risk_sentiment'].shift()).cumsum()
                ).cumcount() + 1
                
                logger.info("✅ Risk sentiment regimes detected")
            
            return regime_df
            
        except Exception as e:
            logger.error(f"Error detecting risk sentiment regimes: {e}")
            return regime_df
    
    def ml_regime_classification(self, regime_df: pd.DataFrame) -> pd.DataFrame:
        """Use machine learning for comprehensive regime classification."""
        
        try:
            logger.info("Performing ML-based regime classification...")
            
            # Select features for clustering
            feature_cols = [
                'market_volatility', 'market_momentum', 'market_rsi'
            ]
            
            # Add crypto and forex sentiment if available
            if 'crypto_sentiment' in regime_df.columns:
                feature_cols.append('crypto_sentiment')
            if 'forex_sentiment' in regime_df.columns:
                feature_cols.append('forex_sentiment')
            
            # Filter available features
            available_features = [col for col in feature_cols if col in regime_df.columns]
            
            if len(available_features) < 2:
                logger.warning("Insufficient features for ML regime classification")
                return regime_df
            
            # Prepare data
            feature_data = regime_df[available_features].dropna()
            
            if len(feature_data) < 50:
                logger.warning("Insufficient data for ML regime classification")
                return regime_df
            
            # Standardize features
            scaler = StandardScaler()
            features_scaled = scaler.fit_transform(feature_data)
            
            # Perform K-means clustering
            kmeans = KMeans(n_clusters=self.n_regimes, random_state=42, n_init=10)
            clusters = kmeans.fit_predict(features_scaled)
            
            # Assign clusters back to dataframe
            regime_df.loc[feature_data.index, 'ml_regime'] = clusters
            
            # Interpret clusters based on characteristics
            cluster_interpretation = {}
            for cluster_id in range(self.n_regimes):
                cluster_mask = clusters == cluster_id
                cluster_features = feature_data[cluster_mask]
                
                # Calculate cluster characteristics
                avg_vol = cluster_features['market_volatility'].mean()
                avg_momentum = cluster_features['market_momentum'].mean()
                
                # Interpret cluster
                if avg_vol > feature_data['market_volatility'].quantile(0.75):
                    if avg_momentum > feature_data['market_momentum'].quantile(0.60):
                        interpretation = 'high_vol_bullish'
                    else:
                        interpretation = 'high_vol_bearish'
                elif avg_vol < feature_data['market_volatility'].quantile(0.25):
                    interpretation = 'low_vol_calm'
                else:
                    if avg_momentum > feature_data['market_momentum'].quantile(0.60):
                        interpretation = 'normal_vol_bullish'
                    else:
                        interpretation = 'normal_vol_bearish'
                
                cluster_interpretation[cluster_id] = interpretation
            
            # Map cluster IDs to interpretations
            regime_df['ml_regime_label'] = regime_df['ml_regime'].map(cluster_interpretation)
            
            # Calculate regime transition probabilities
            regime_transitions = self._calculate_regime_transitions(regime_df['ml_regime'].dropna())
            regime_df.attrs['regime_transitions'] = regime_transitions
            
            logger.info("✅ ML regime classification completed")
            return regime_df
            
        except Exception as e:
            logger.error(f"Error in ML regime classification: {e}")
            return regime_df
    
    def _calculate_regime_transitions(self, regime_series: pd.Series) -> Dict:
        """Calculate regime transition probabilities."""
        
        try:
            # Create transition matrix
            transitions = {}
            for i in range(len(regime_series) - 1):
                current_regime = regime_series.iloc[i]
                next_regime = regime_series.iloc[i + 1]
                
                if current_regime not in transitions:
                    transitions[current_regime] = {}
                
                if next_regime not in transitions[current_regime]:
                    transitions[current_regime][next_regime] = 0
                
                transitions[current_regime][next_regime] += 1
            
            # Convert to probabilities
            transition_probs = {}
            for from_regime, to_regimes in transitions.items():
                total_transitions = sum(to_regimes.values())
                transition_probs[from_regime] = {
                    to_regime: count / total_transitions
                    for to_regime, count in to_regimes.items()
                }
            
            return transition_probs
            
        except Exception as e:
            logger.error(f"Error calculating regime transitions: {e}")
            return {}
    
    def generate_regime_report(self) -> Dict[str, any]:
        """Generate comprehensive market regime analysis report."""
        
        try:
            logger.info("Generating comprehensive market regime report...")
            
            report = {
                'generation_timestamp': datetime.now().isoformat(),
                'analysis_intervals': {}
            }
            
            # Analyze both intervals
            for interval in self.intervals:
                logger.info(f"Processing {interval} interval regime analysis...")
                
                # Calculate regime indicators
                regime_df = self.calculate_regime_indicators(interval)
                
                if regime_df.empty:
                    logger.warning(f"No regime indicators for {interval}")
                    continue
                
                # Detect different types of regimes
                regime_df = self.detect_volatility_regimes(regime_df)
                regime_df = self.detect_trend_regimes(regime_df)
                regime_df = self.detect_risk_sentiment_regimes(regime_df)
                regime_df = self.ml_regime_classification(regime_df)
                
                # Calculate regime statistics
                regime_stats = {}
                
                # Volatility regime distribution
                if 'volatility_regime' in regime_df.columns:
                    regime_stats['volatility_regimes'] = regime_df['volatility_regime'].value_counts(normalize=True).to_dict()
                    regime_stats['current_volatility_regime'] = regime_df['volatility_regime'].iloc[-1] if len(regime_df) > 0 else 'unknown'
                
                # Trend regime distribution
                if 'trend_regime' in regime_df.columns:
                    regime_stats['trend_regimes'] = regime_df['trend_regime'].value_counts(normalize=True).to_dict()
                    regime_stats['current_trend_regime'] = regime_df['trend_regime'].iloc[-1] if len(regime_df) > 0 else 'unknown'
                
                # Risk sentiment distribution
                if 'risk_sentiment' in regime_df.columns:
                    regime_stats['risk_sentiment_regimes'] = regime_df['risk_sentiment'].value_counts(normalize=True).to_dict()
                    regime_stats['current_risk_sentiment'] = regime_df['risk_sentiment'].iloc[-1] if len(regime_df) > 0 else 'unknown'
                
                # ML regime distribution
                if 'ml_regime_label' in regime_df.columns:
                    regime_stats['ml_regimes'] = regime_df['ml_regime_label'].value_counts(normalize=True).to_dict()
                    regime_stats['current_ml_regime'] = regime_df['ml_regime_label'].iloc[-1] if len(regime_df) > 0 else 'unknown'
                
                # Save processed regime data
                regime_file = self.silver_path / f"market_regimes_{interval}.csv"
                regime_df.to_csv(regime_file)
                
                report['analysis_intervals'][interval] = {
                    'regime_statistics': regime_stats,
                    'data_file': str(regime_file),
                    'total_observations': len(regime_df),
                    'feature_count': len(regime_df.columns)
                }
            
            # Save comprehensive report
            report_path = self.silver_path / "market_regime_analysis_report.json"
            with open(report_path, 'w') as f:
                json.dump(report, f, indent=2)
            
            logger.info(f"✅ Market regime analysis report saved to {report_path}")
            return report
            
        except Exception as e:
            logger.error(f"Error generating regime report: {e}")
            return {}

def main():
    """Main function for market regime analysis."""
    
    detector = EnhancedMarketRegimeDetector()
    report = detector.generate_regime_report()
    
    if report:
        print("✅ Market regime analysis completed successfully")
        
        # Print summary
        for interval in report.get('analysis_intervals', {}):
            interval_data = report['analysis_intervals'][interval]
            
            print(f"\n📊 {interval.upper()} Interval Regime Summary:")
            print(f"   • Total observations: {interval_data.get('total_observations', 0):,}")
            print(f"   • Features analyzed: {interval_data.get('feature_count', 0)}")
            
            regime_stats = interval_data.get('regime_statistics', {})
            
            # Current regimes
            print(f"   • Current volatility regime: {regime_stats.get('current_volatility_regime', 'unknown')}")
            print(f"   • Current trend regime: {regime_stats.get('current_trend_regime', 'unknown')}")
            print(f"   • Current risk sentiment: {regime_stats.get('current_risk_sentiment', 'unknown')}")
            print(f"   • Current ML regime: {regime_stats.get('current_ml_regime', 'unknown')}")
    else:
        print("❌ Market regime analysis failed")

if __name__ == "__main__":
    main()
