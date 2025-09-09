"""
Silver Layer Economic Data Integration for Alpha Models

This module integrates silver layer economic indicators into our alpha models,
providing a standardized interface for economic data consumption across
BTC and ETH prediction models.
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
import sqlite3
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

class SilverEconomicDataConnector:
    """
    Connector for integrating silver layer economic data into alpha models.
    
    Features:
    - Standardized economic feature extraction
    - Temporal alignment with crypto price data
    - Feature importance tracking
    - Model-ready data preparation
    """
    
    def __init__(self, silver_data_path: Optional[str] = None):
        """Initialize connector with silver layer data path."""
        
        if silver_data_path is None:
            current_dir = Path(__file__).parent
            data_sources_dir = current_dir.parent.parent
            self.silver_path = data_sources_dir / '1_data_sources' / '3_silver' / 'economic_indicators'
        else:
            self.silver_path = Path(silver_data_path)
        
        self.consolidated_file = self.silver_path / 'consolidated_economic_indicators_silver.csv'
        
        # Economic feature categories with weights for alpha models
        self.economic_feature_weights = {
            'economic_growth': {
                'gdp_growth': 0.25,
                'industrial_production': 0.20,
                'unemployment_rate': 0.15
            },
            'consumer_business': {
                'consumer_confidence': 0.20,
                'retail_sales': 0.18,
                'housing_starts': 0.12
            },
            'monetary_policy': {
                'interest_rates': 0.30,
                'pmi_manufacturing': 0.15,
                'corporate_earnings': 0.10
            },
            'international_trade': {
                'trade_balance': 0.08,
                'treasury_yields': 0.12
            }
        }
        
        logger.info(f"Initialized SilverEconomicDataConnector")
        logger.info(f"Silver data path: {self.silver_path}")
        
    def load_silver_economic_data(self) -> pd.DataFrame:
        """Load consolidated silver layer economic data."""
        
        try:
            if not self.consolidated_file.exists():
                logger.warning(f"Consolidated file not found: {self.consolidated_file}")
                return pd.DataFrame()
            
            df = pd.read_csv(self.consolidated_file)
            
            # Ensure timestamp is datetime
            df['timestamp'] = pd.to_datetime(df['timestamp'])
            
            # Sort by timestamp for consistent processing
            df = df.sort_values('timestamp').reset_index(drop=True)
            
            logger.info(f"Loaded {len(df)} silver economic records")
            logger.info(f"Date range: {df['timestamp'].min()} to {df['timestamp'].max()}")
            logger.info(f"Categories: {df['indicator_category'].unique().tolist()}")
            
            return df
            
        except Exception as e:
            logger.error(f"Error loading silver economic data: {e}")
            return pd.DataFrame()
    
    def prepare_economic_features_for_crypto(self, 
                                           crypto_data: pd.DataFrame,
                                           crypto_symbol: str = "ETH") -> pd.DataFrame:
        """
        Prepare economic features aligned with crypto price data.
        
        Args:
            crypto_data: DataFrame with crypto price data (must have 'timestamp' column)
            crypto_symbol: Crypto symbol for feature naming
            
        Returns:
            Enhanced crypto DataFrame with economic features
        """
        
        logger.info(f"Preparing economic features for {crypto_symbol}")
        
        try:
            # Load silver economic data
            economic_df = self.load_silver_economic_data()
            
            if economic_df.empty:
                logger.warning("No economic data available")
                return crypto_data.copy()
            
            # Create pivot table for easier feature engineering
            pivot_df = economic_df.pivot_table(
                index='timestamp',
                columns='indicator_name',
                values='indicator_value',
                aggfunc='first'
            ).reset_index()
            
            # Ensure timestamp columns are compatible
            crypto_data['timestamp'] = pd.to_datetime(crypto_data['timestamp'])
            pivot_df['timestamp'] = pd.to_datetime(pivot_df['timestamp'])
            
            # Merge with crypto data using nearest temporal match
            merged_df = pd.merge_asof(
                crypto_data.sort_values('timestamp'),
                pivot_df.sort_values('timestamp'),
                on='timestamp',
                direction='backward',  # Use most recent economic data
                tolerance=pd.Timedelta(days=30)  # Max 30 days lookback
            )
            
            # Create enhanced economic features
            enhanced_df = self._create_enhanced_economic_features(merged_df, crypto_symbol)
            
            # Add feature importance metadata
            enhanced_df = self._add_feature_importance_metadata(enhanced_df)
            
            logger.info(f"Enhanced {crypto_symbol} data with {enhanced_df.shape[1] - crypto_data.shape[1]} economic features")
            
            return enhanced_df
            
        except Exception as e:
            logger.error(f"Error preparing economic features: {e}")
            return crypto_data.copy()
    
    def _create_enhanced_economic_features(self, 
                                         merged_df: pd.DataFrame, 
                                         crypto_symbol: str) -> pd.DataFrame:
        """Create enhanced economic features optimized for crypto prediction."""
        
        df = merged_df.copy()
        economic_cols = [col for col in df.columns if col not in ['timestamp', 'price', 'volume', 'open', 'high', 'low', 'close']]
        
        try:
            # 1. Normalized economic indicators (z-score)
            for col in economic_cols:
                if pd.api.types.is_numeric_dtype(df[col]):
                    values = df[col].dropna()
                    if len(values) > 1 and values.std() > 0:
                        df[f"economic_{col}_normalized"] = (df[col] - values.mean()) / values.std()
            
            # 2. Economic momentum features
            for col in economic_cols:
                if pd.api.types.is_numeric_dtype(df[col]):
                    # 1-month momentum
                    df[f"economic_{col}_momentum_1m"] = df[col].pct_change(periods=1)
                    
                    # 3-month momentum  
                    if len(df) >= 3:
                        df[f"economic_{col}_momentum_3m"] = df[col].pct_change(periods=min(3, len(df)-1))
            
            # 3. Economic composite indicators by category
            growth_indicators = ['gdp_growth', 'industrial_production', 'unemployment_rate']
            consumer_indicators = ['consumer_confidence', 'retail_sales', 'housing_starts']
            monetary_indicators = ['interest_rates', 'pmi_manufacturing', 'corporate_earnings']
            trade_indicators = ['trade_balance', 'treasury_yields']
            
            # Create category composites
            self._create_category_composite(df, growth_indicators, 'economic_growth_composite')
            self._create_category_composite(df, consumer_indicators, 'consumer_business_composite')  
            self._create_category_composite(df, monetary_indicators, 'monetary_policy_composite')
            self._create_category_composite(df, trade_indicators, 'international_trade_composite')
            
            # 4. Crypto-specific economic features
            if crypto_symbol in ['BTC', 'ETH']:
                # Risk sentiment composite (inverse of volatility indicators)
                risk_cols = ['interest_rates', 'treasury_yields']
                available_risk_cols = [col for col in risk_cols if col in df.columns]
                
                if available_risk_cols:
                    risk_values = df[available_risk_cols].mean(axis=1, skipna=True)
                    df[f'{crypto_symbol.lower()}_risk_sentiment'] = -risk_values  # Inverse for risk-on sentiment
                
                # Liquidity proxy (inverse of interest rates)
                if 'interest_rates' in df.columns:
                    df[f'{crypto_symbol.lower()}_liquidity_proxy'] = -df['interest_rates']
                
                # Economic uncertainty (volatility of economic indicators)
                uncertainty_cols = ['gdp_growth', 'consumer_confidence', 'pmi_manufacturing']
                available_uncertainty_cols = [col for col in uncertainty_cols if col in df.columns]
                
                if available_uncertainty_cols and len(df) >= 5:
                    for col in available_uncertainty_cols:
                        rolling_std = df[col].rolling(window=5, min_periods=2).std()
                        df[f'{crypto_symbol.lower()}_economic_uncertainty'] = rolling_std.mean()
            
            logger.info(f"Created {df.shape[1] - merged_df.shape[1]} enhanced economic features")
            
        except Exception as e:
            logger.error(f"Error creating enhanced features: {e}")
        
        return df
    
    def _create_category_composite(self, 
                                 df: pd.DataFrame, 
                                 indicators: List[str], 
                                 composite_name: str) -> None:
        """Create a composite indicator for a category of economic indicators."""
        
        try:
            available_indicators = [ind for ind in indicators if ind in df.columns]
            
            if available_indicators:
                # Normalize each indicator first
                normalized_values = []
                
                for ind in available_indicators:
                    values = df[ind].dropna()
                    if len(values) > 1 and values.std() > 0:
                        normalized = (df[ind] - values.mean()) / values.std()
                        normalized_values.append(normalized)
                
                if normalized_values:
                    # Average of normalized indicators
                    composite = pd.concat(normalized_values, axis=1).mean(axis=1, skipna=True)
                    df[composite_name] = composite
                    
                    logger.info(f"Created {composite_name} from {len(available_indicators)} indicators")
        
        except Exception as e:
            logger.error(f"Error creating composite {composite_name}: {e}")
    
    def _add_feature_importance_metadata(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add metadata about feature importance for model interpretation."""
        
        # Calculate feature importance based on predefined weights
        feature_importance = {}
        
        for category, indicators in self.economic_feature_weights.items():
            for indicator, weight in indicators.items():
                if indicator in df.columns:
                    feature_importance[indicator] = weight
                
                # Also add importance for derived features
                derived_features = [col for col in df.columns if col.startswith(f'economic_{indicator}')]
                for derived in derived_features:
                    feature_importance[derived] = weight * 0.8  # Slightly lower for derived
        
        # Store as DataFrame attribute for later access
        df.attrs['economic_feature_importance'] = feature_importance
        
        return df
    
    def get_economic_feature_summary(self, enhanced_df: pd.DataFrame) -> Dict[str, Any]:
        """Get summary of economic features added to the dataset."""
        
        economic_features = [col for col in enhanced_df.columns if col.startswith('economic_')]
        
        summary = {
            'total_economic_features': len(economic_features),
            'feature_categories': {
                'growth': len([f for f in economic_features if any(x in f for x in ['gdp', 'industrial', 'unemployment'])]),
                'consumer_business': len([f for f in economic_features if any(x in f for x in ['consumer', 'retail', 'housing'])]),
                'monetary_policy': len([f for f in economic_features if any(x in f for x in ['interest', 'pmi', 'corporate'])]),
                'international_trade': len([f for f in economic_features if any(x in f for x in ['trade', 'treasury'])])
            },
            'composite_indicators': len([f for f in economic_features if 'composite' in f]),
            'momentum_features': len([f for f in economic_features if 'momentum' in f]),
            'normalized_features': len([f for f in economic_features if 'normalized' in f]),
            'crypto_specific': len([f for f in economic_features if any(x in f for x in ['risk_sentiment', 'liquidity', 'uncertainty'])]),
            'feature_importance_available': hasattr(enhanced_df, 'attrs') and 'economic_feature_importance' in enhanced_df.attrs
        }
        
        return summary
    
    def export_for_alpha_model(self, 
                              enhanced_df: pd.DataFrame, 
                              model_type: str = "xgboost",
                              output_path: Optional[str] = None) -> str:
        """
        Export enhanced dataset in format optimized for alpha models.
        
        Args:
            enhanced_df: Enhanced DataFrame with economic features
            model_type: Type of model (xgboost, ensemble, etc.)
            output_path: Optional output file path
            
        Returns:
            Path to exported file
        """
        
        try:
            if output_path is None:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                filename = f"alpha_model_economic_features_{model_type}_{timestamp}.csv"
                output_path = self.silver_path / filename
            
            # Select relevant columns for alpha models
            feature_cols = []
            
            # Basic price/volume features
            price_cols = [col for col in enhanced_df.columns if col in ['timestamp', 'price', 'volume', 'open', 'high', 'low', 'close']]
            feature_cols.extend(price_cols)
            
            # All economic features
            economic_cols = [col for col in enhanced_df.columns if col.startswith('economic_')]
            feature_cols.extend(economic_cols)
            
            # Export dataset
            export_df = enhanced_df[feature_cols].copy()
            export_df.to_csv(output_path, index=False)
            
            logger.info(f"Exported alpha model dataset: {output_path}")
            logger.info(f"Features: {len(feature_cols)} ({len(economic_cols)} economic)")
            
            return str(output_path)
            
        except Exception as e:
            logger.error(f"Error exporting alpha model dataset: {e}")
            return ""


def demonstrate_silver_integration():
    """Demonstrate silver layer economic data integration with crypto data."""
    
    print("🔗 SILVER LAYER ECONOMIC DATA INTEGRATION")
    print("=" * 60)
    
    # Initialize connector
    connector = SilverEconomicDataConnector()
    
    # Create sample crypto data
    print("📊 Creating sample crypto data...")
    dates = pd.date_range(start='2023-01-01', end='2025-09-01', freq='D')
    sample_crypto = pd.DataFrame({
        'timestamp': dates,
        'price': 2000 + np.cumsum(np.random.normal(0, 50, len(dates))),
        'volume': np.random.normal(1000000, 200000, len(dates))
    })
    print(f"   Created {len(sample_crypto)} crypto records")
    
    # Integrate economic features
    print("🏦 Integrating economic features...")
    enhanced_crypto = connector.prepare_economic_features_for_crypto(sample_crypto, "ETH")
    print(f"   Enhanced with {enhanced_crypto.shape[1] - sample_crypto.shape[1]} economic features")
    
    # Get feature summary
    summary = connector.get_economic_feature_summary(enhanced_crypto)
    print(f"\n📈 FEATURE SUMMARY:")
    print(f"   Total Economic Features: {summary['total_economic_features']}")
    print(f"   Growth Features: {summary['feature_categories']['growth']}")
    print(f"   Consumer/Business: {summary['feature_categories']['consumer_business']}")
    print(f"   Monetary Policy: {summary['feature_categories']['monetary_policy']}")
    print(f"   International Trade: {summary['feature_categories']['international_trade']}")
    print(f"   Composite Indicators: {summary['composite_indicators']}")
    print(f"   Momentum Features: {summary['momentum_features']}")
    print(f"   Crypto-Specific: {summary['crypto_specific']}")
    
    # Export for alpha models
    export_path = connector.export_for_alpha_model(enhanced_crypto, "demonstration")
    print(f"\n💾 Exported enhanced dataset: {Path(export_path).name}")
    
    print(f"\n✅ Silver layer integration complete!")
    print(f"🎯 Ready for alpha model consumption!")


if __name__ == "__main__":
    demonstrate_silver_integration()
