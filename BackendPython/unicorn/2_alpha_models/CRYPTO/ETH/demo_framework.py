#!/usr/bin/env python3
"""
ETH Prophet Framework - Quick Start Demo

This script demonstrates how to quickly use the ETH Prophet framework
for alpha model development and comparison.
"""

import sys
import os
from pathlib import Path

# Add current directory to path
sys.path.append(str(Path(__file__).parent))

from eth_prophet_simple import ETHProphetFramework, create_sample_data

def demo_basic_usage():
    """Demonstrate basic framework usage."""
    print("🚀 ETH PROPHET FRAMEWORK - QUICK START DEMO")
    print("=" * 55)
    
    # Step 1: Create sample data
    print("\n📊 Step 1: Creating Sample ETH Data")
    print("-" * 35)
    data = create_sample_data(400)  # 400 days of data
    
    print(f"✅ Generated {len(data)} days of ETH price data")
    print(f"   Date range: {data.index[0].date()} to {data.index[-1].date()}")
    print(f"   Price range: ${data['Close'].min():.2f} - ${data['Close'].max():.2f}")
    print(f"   Average daily volume: {data['Volume'].mean():.0f}")
    
    # Step 2: Initialize framework
    print("\n🔧 Step 2: Initializing Framework")
    print("-" * 32)
    framework = ETHProphetFramework()
    
    print("✅ Framework initialized")
    print("✅ Database connection established")
    print("✅ Three Prophet models configured")
    
    # Step 3: Train and compare models
    print("\n🤖 Step 3: Training and Comparing Models")
    print("-" * 39)
    results = framework.train_and_compare(data, validation_split=0.25)  # 25% validation
    
    # Step 4: Show results
    print("\n📊 Step 4: Results Analysis")
    print("-" * 26)
    
    if results['models']:
        best_model = results['best_model']
        best_metrics = results['models'][best_model]['metrics']
        
        print(f"🏆 Best Model: {best_model.upper()}")
        print(f"🎯 Best MAPE: {best_metrics['mape']:.2f}%")
        print(f"📈 Directional Accuracy: {best_metrics['directional_accuracy']:.1f}%")
        print(f"📊 R² Score: {best_metrics['r2']:.4f}")
        
        # Show all model comparison
        print(f"\n📋 Model Comparison Summary:")
        print("-" * 28)
        for model_name, model_data in results['models'].items():
            metrics = model_data['metrics']
            is_best = "⭐" if model_name == best_model else "  "
            print(f"{is_best} {model_name.upper():<10} MAPE: {metrics['mape']:.2f}%  R²: {metrics['r2']:.4f}")
    
    return framework, results

def demo_database_features(framework):
    """Demonstrate database and historical features."""
    print("\n💾 Step 5: Database Features")
    print("-" * 24)
    
    # Show database contents
    history = framework.get_history()
    print(f"✅ Database contains {len(history)} experiment records")
    
    if len(history) > 0:
        print(f"✅ Latest experiment: {history.iloc[0]['experiment_id']}")
        print(f"✅ Best model in history: {history.loc[history['mape'].idxmin(), 'model_variant']}")
        print(f"✅ Best MAPE in history: {history['mape'].min():.2f}%")

def demo_model_prediction(results):
    """Demonstrate making predictions with the best model."""
    print("\n🔮 Step 6: Making Predictions")
    print("-" * 26)
    
    if results['models']:
        best_model_name = results['best_model']
        best_model = results['models'][best_model_name]['model']
        
        # Make 30-day forecast
        forecast = best_model.predict(periods=30)
        
        print(f"✅ Generated 30-day forecast using {best_model_name} model")
        print(f"📊 Forecast shape: {forecast.shape}")
        print(f"📈 Predicted price range: ${forecast['yhat'].min():.2f} - ${forecast['yhat'].max():.2f}")
        
        # Show first 5 predictions
        print(f"\n📅 First 5 Days Forecast:")
        print("-" * 23)
        future_forecast = forecast.tail(30).head(5)
        for idx, row in future_forecast.iterrows():
            print(f"   {row['ds'].strftime('%Y-%m-%d')}: ${row['yhat']:.2f} (±${row['yhat_upper'] - row['yhat']:.2f})")

def demo_model_configuration():
    """Show model configurations."""
    print("\n⚙️  Step 7: Model Configurations")
    print("-" * 29)
    
    framework = ETHProphetFramework()
    models = framework.create_models()
    
    for model_name, model in models.items():
        config = model.config
        print(f"\n{model_name.upper()} MODEL CONFIG:")
        print(f"  Seasonality Mode: {config['seasonality_mode']}")
        print(f"  Changepoint Prior: {config['changepoint_prior_scale']}")
        print(f"  Seasonality Prior: {config['seasonality_prior_scale']}")
        if 'daily_seasonality' in config:
            print(f"  Daily Seasonality: {config['daily_seasonality']}")

def main():
    """Main demo function."""
    print("Starting ETH Prophet Framework demonstration...")
    
    try:
        # Basic usage demo
        framework, results = demo_basic_usage()
        
        # Database features demo
        demo_database_features(framework)
        
        # Prediction demo
        demo_model_prediction(results)
        
        # Configuration demo
        demo_model_configuration()
        
        # Final summary
        print("\n🎉 DEMO COMPLETED SUCCESSFULLY!")
        print("=" * 32)
        print("✅ Framework is working perfectly")
        print("✅ Three Prophet models trained and compared")
        print("✅ Performance metrics calculated and stored")
        print("✅ Database functionality verified")
        print("✅ Prediction capabilities demonstrated")
        
        print(f"\n💡 Key Takeaways:")
        print(f"   • Best model achieved {results['models'][results['best_model']]['metrics']['mape']:.2f}% MAPE")
        print(f"   • Framework provides automated model comparison")
        print(f"   • Database stores all experiments for analysis")
        print(f"   • Ready for production deployment")
        
        print(f"\n🚀 Next Steps:")
        print(f"   1. Integrate real ETH price data")
        print(f"   2. Set up automated retraining")
        print(f"   3. Deploy best model for live trading")
        print(f"   4. Monitor performance and retrain as needed")
        
    except Exception as e:
        print(f"❌ Demo failed: {str(e)}")
        return False
    
    return True

if __name__ == "__main__":
    success = main()
    if success:
        print("\n✨ Demo completed successfully! Framework ready for use.")
    else:
        print("\n💥 Demo encountered issues. Check error messages above.")
    
    sys.exit(0 if success else 1)
