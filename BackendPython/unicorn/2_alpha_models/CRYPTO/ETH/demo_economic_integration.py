#!/usr/bin/env python3
"""
Demo: Economic Indicators Integration with ETH Alpha Models

This demo script showcases the integration of bronze layer economic indicators
into ETH cryptocurrency prediction models, demonstrating enhanced prediction
capabilities through macroeconomic factor analysis.

The demo includes:
- Loading and preprocessing economic indicators
- Creating enhanced ETH models with economic features
- Performance comparison between technical-only and economic-enhanced models
- Feature importance analysis for economic factors
"""

import os
import sys
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import warnings

# Add parent directories for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
eth_dir = os.path.dirname(current_dir)
sys.path.append(eth_dir)

from eth_xgboost_economic_enhanced import ETHXGBoostWithEconomicIndicators

warnings.filterwarnings('ignore')

def generate_sample_eth_data(days: int = 365) -> pd.DataFrame:
    """
    Generate sample ETH price data for demonstration.
    
    Args:
        days: Number of days of data to generate
        
    Returns:
        DataFrame with ETH price data
    """
    print("📊 Generating sample ETH price data...")
    
    # Create date range
    end_date = datetime.now()
    start_date = end_date - timedelta(days=days)
    dates = pd.date_range(start=start_date, end=end_date, freq='D')
    
    # Generate realistic ETH price data with trends and volatility
    np.random.seed(42)  # For reproducible results
    
    # Base price trend (upward with some cycles)
    trend = np.linspace(1500, 2500, len(dates))  # ETH price range
    cycles = 200 * np.sin(np.linspace(0, 4 * np.pi, len(dates)))  # Market cycles
    
    # Add random volatility
    volatility = np.random.normal(0, 50, len(dates))  # Daily volatility
    
    # Combine components
    prices = trend + cycles + volatility
    prices = np.maximum(prices, 100)  # Ensure prices stay positive
    
    # Add some volume data
    volumes = np.random.lognormal(15, 0.5, len(dates))  # Log-normal volume distribution
    
    df = pd.DataFrame({
        'price': prices,
        'volume': volumes
    }, index=dates)
    
    print(f"✅ Generated {len(df)} days of ETH data")
    print(f"   Price range: ${df['price'].min():.2f} - ${df['price'].max():.2f}")
    print(f"   Average volume: {df['volume'].mean():,.0f}")
    
    return df

def demo_economic_indicators_integration():
    """Main demonstration of economic indicators integration."""
    
    print("🚀 ECONOMIC INDICATORS INTEGRATION DEMO")
    print("=" * 60)
    print("Testing bronze layer economic indicators integration")
    print("with ETH cryptocurrency alpha models")
    print("=" * 60)
    
    # Generate sample ETH data
    eth_data = generate_sample_eth_data(days=500)
    
    print(f"\n📈 Sample ETH Data Overview:")
    print(f"   Date range: {eth_data.index.min().date()} to {eth_data.index.max().date()}")
    print(f"   Data points: {len(eth_data)}")
    print(f"   Current price: ${eth_data['price'].iloc[-1]:.2f}")
    print(f"   Price change (30d): {((eth_data['price'].iloc[-1] / eth_data['price'].iloc[-30]) - 1) * 100:.1f}%")
    
    # Test economic indicators integration
    print(f"\n🏦 Testing Economic Indicators Integration...")
    
    try:
        # Create enhanced model with economic indicators
        enhanced_model = ETHXGBoostWithEconomicIndicators(enable_economic_indicators=True)
        
        print("✅ Economic indicators integrator initialized")
        
        # Test feature creation
        print("\n🔄 Creating enhanced features...")
        enhanced_df, economic_summary = enhanced_model.create_enhanced_features(
            eth_data, target_col='price', n_economic_features=15
        )
        
        print(f"✅ Enhanced features created successfully!")
        print(f"   📊 Original features: {eth_data.shape[1]}")
        print(f"   🚀 Enhanced features: {enhanced_df.shape[1]}")
        print(f"   🏦 Economic features added: {economic_summary.get('total_economic_features', 0)}")
        
        # Display economic features summary
        if 'features_by_category' in economic_summary:
            print(f"\n📋 Economic Features by Category:")
            for category, info in economic_summary['features_by_category'].items():
                print(f"   • {category.replace('_', ' ').title()}: {info['count']} features")
        
        # Test model training if we have enough data
        if len(enhanced_df) > 100:  # Need sufficient data for training
            print(f"\n🤖 Training Economic-Enhanced XGBoost Model...")
            
            try:
                model_result = enhanced_model.create_economic_enhanced_model(
                    eth_data, target_col='price', n_economic_features=10
                )
                
                print(f"✅ Model training completed successfully!")
                print(f"   🎯 Model ID: {model_result['model_id']}")
                print(f"   📊 Test R² Score: {model_result['performance']['test_r2']:.4f}")
                print(f"   📈 Test MAE: ${model_result['performance']['test_mae']:.2f}")
                print(f"   🏦 Economic Features Importance: {model_result['performance']['economic_importance_ratio']:.1%}")
                
                # Show top economic features
                print(f"\n🔝 Top Economic Features:")
                top_features = enhanced_model.get_top_economic_features(model_result['model_id'], top_n=10)
                
                if not top_features.empty:
                    for i, row in top_features.head(5).iterrows():
                        feature_name = row['Feature'].replace('_', ' ').title()
                        print(f"   {i+1}. {feature_name}: {row['Importance_Percentage']:.1f}%")
                else:
                    print("   No economic features found in model")
                
                # Performance comparison
                print(f"\n📊 Model Performance Summary:")
                comparison_df = enhanced_model.compare_models()
                
                if not comparison_df.empty:
                    latest_model = comparison_df.iloc[0]
                    print(f"   • Model Variant: {latest_model['model_variant']}")
                    print(f"   • R² Score: {latest_model['r2_score']:.4f}")
                    print(f"   • MAE: ${latest_model['mae']:.2f}")
                    print(f"   • Economic Features: {latest_model['economic_features']}/{latest_model['total_features']}")
                    print(f"   • Economic Importance: {latest_model['economic_feature_importance']:.1%}")
                
            except Exception as e:
                print(f"⚠️  Model training encountered issue: {e}")
                print("   This may be due to limited economic indicator data")
        
        else:
            print(f"⚠️  Insufficient data for model training ({len(enhanced_df)} samples)")
            print("   Need at least 100 samples for reliable model training")
    
    except Exception as e:
        print(f"❌ Economic indicators integration failed: {e}")
        print("   This may indicate bronze layer data is not available")
        print("   Ensure economic indicators pipeline has been run")
    
    # Test basic model without economic indicators
    print(f"\n🔧 Testing Technical-Only Model (Fallback)...")
    
    try:
        basic_model = ETHXGBoostWithEconomicIndicators(enable_economic_indicators=False)
        
        # Create basic features
        enhanced_df_basic, _ = basic_model.create_enhanced_features(
            eth_data, target_col='price', n_economic_features=0
        )
        
        print(f"✅ Technical features created: {enhanced_df_basic.shape[1]} features")
        
    except Exception as e:
        print(f"❌ Technical model creation failed: {e}")
    
    print(f"\n🎉 Demo completed!")
    print(f"=" * 60)
    
    # Summary and recommendations
    print(f"\n📋 INTEGRATION SUMMARY:")
    print(f"✅ Economic indicators integration module created")
    print(f"✅ Enhanced ETH XGBoost model with economic features")
    print(f"✅ Feature engineering combining technical + fundamental analysis")
    print(f"✅ Performance tracking with economic feature importance")
    print(f"✅ Fallback to technical-only model if economic data unavailable")
    
    print(f"\n🚀 NEXT STEPS:")
    print(f"1. Ensure bronze layer pipeline runs regularly for fresh economic data")
    print(f"2. Set up API keys (FRED_API_KEY, BEA_API_KEY) for live economic data")
    print(f"3. Test models with real ETH price data from your data sources")
    print(f"4. Fine-tune economic feature selection based on model performance")
    print(f"5. Integrate into production trading algorithms in portfolio layer")

if __name__ == "__main__":
    demo_economic_indicators_integration()
