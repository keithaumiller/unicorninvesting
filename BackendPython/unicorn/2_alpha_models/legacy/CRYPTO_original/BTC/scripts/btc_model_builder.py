"""
BTC Model Builder Script

Build and train all BTC models with performance tracking.
"""

import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta
import json

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.performance_tracker import ModelPerformanceTracker
from models.btc_prophet import create_and_train_btc_prophet
from models.btc_xgboost import create_and_train_btc_xgboost
from models.btc_ensemble import create_and_train_btc_ensemble

def generate_training_data() -> pd.DataFrame:
    """Generate training data for BTC models."""
    # This would connect to real data sources in production
    # For now, generate realistic synthetic data
    
    np.random.seed(42)
    base_price = 50000 if 'BTC' == 'BTC' else 3000 if 'BTC' == 'ETH' else 100
    volatility = 0.04 if 'BTC' == 'BTC' else 0.05 if 'BTC' == 'ETH' else 0.03
    
    periods = 1000  # ~3 years of data
    dates = pd.date_range(
        start=datetime.now() - timedelta(days=periods),
        periods=periods,
        freq='D'
    )
    
    # Generate price series with realistic patterns
    returns = np.random.normal(0.0005, volatility, periods)  # Slight positive drift
    
    # Add some trend and cycle patterns
    trend = np.linspace(0, 0.5, periods)  # Long-term uptrend
    cycle = 0.1 * np.sin(2 * np.pi * np.arange(periods) / 365)  # Annual cycle
    
    returns += trend/periods + cycle/periods
    
    # Generate prices
    log_prices = np.log(base_price) + np.cumsum(returns)
    prices = np.exp(log_prices)
    
    # Generate OHLC
    close_prices = prices
    open_prices = np.roll(close_prices, 1)
    open_prices[0] = close_prices[0]
    
    # Add realistic intraday variation
    daily_range = volatility * 0.5
    high_prices = np.maximum(open_prices, close_prices) * (1 + np.abs(np.random.normal(0, daily_range, periods)))
    low_prices = np.minimum(open_prices, close_prices) * (1 - np.abs(np.random.normal(0, daily_range, periods)))
    
    # Generate volume with correlation to price movements
    price_changes = np.abs(np.diff(np.append(close_prices[0], close_prices)))
    base_volume = 1000000 if 'BTC' in ['BTC', 'ETH'] else 100000
    volume_multiplier = 1 + price_changes * 20  # Higher volume on big moves
    volume = base_volume * volume_multiplier * np.random.lognormal(0, 0.4, periods)
    
    return pd.DataFrame({
        'Open': open_prices,
        'High': high_prices,
        'Low': low_prices,
        'Close': close_prices,
        'Volume': volume
    }, index=dates)

def build_all_models() -> Dict[str, Any]:
    """Build and train all BTC models."""
    print(f"🏗️ Building All BTC Models")
    print("=" * 50)
    
    # Generate training data
    print("📊 Generating training data...")
    training_data = generate_training_data()
    print(f"✅ Generated {len(training_data)} days of training data")
    
    results = {
        'asset_name': 'BTC',
        'build_timestamp': datetime.now().isoformat(),
        'training_samples': len(training_data),
        'models': {}
    }
    
    # Build Prophet model
    try:
        print("\n🔮 Building Prophet Model...")
        prophet_model = create_and_train_btc_prophet(training_data)
        results['models']['prophet'] = {
            'success': prophet_model.is_trained,
            'model_type': 'Prophet',
            'training_metrics': prophet_model.performance_metrics
        }
        
        # Save model
        prophet_model.save_model(f"btc_prophet_model.pkl")
        print(f"💾 Prophet model saved to btc_prophet_model.pkl")
        
    except Exception as e:
        print(f"❌ Prophet model building failed: {e}")
        results['models']['prophet'] = {'success': False, 'error': str(e)}
    
    # Build XGBoost model
    try:
        print("\n🌲 Building XGBoost Model...")
        xgboost_model = create_and_train_btc_xgboost(training_data)
        results['models']['xgboost'] = {
            'success': xgboost_model.is_trained,
            'model_type': 'XGBoost',
            'training_metrics': xgboost_model.performance_metrics
        }
        
        # Save model
        xgboost_model.save_model(f"btc_xgboost_model.pkl")
        print(f"💾 XGBoost model saved to btc_xgboost_model.pkl")
        
    except Exception as e:
        print(f"❌ XGBoost model building failed: {e}")
        results['models']['xgboost'] = {'success': False, 'error': str(e)}
    
    # Build Ensemble model
    try:
        print("\n🎯 Building Ensemble Model...")
        ensemble_model = create_and_train_btc_ensemble(training_data)
        results['models']['ensemble'] = {
            'success': ensemble_model.is_trained,
            'model_type': 'Ensemble',
            'training_metrics': ensemble_model.performance_metrics
        }
        
        # Save model
        ensemble_model.save_model(f"btc_ensemble_model.pkl")
        print(f"💾 Ensemble model saved to btc_ensemble_model.pkl")
        
    except Exception as e:
        print(f"❌ Ensemble model building failed: {e}")
        results['models']['ensemble'] = {'success': False, 'error': str(e)}
    
    # Save results
    results_file = f"btc_model_build_results.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    # Summary
    print("\n" + "=" * 50)
    print(f"📈 BTC Model Building Summary")
    print("=" * 50)
    
    successful_models = [name for name, result in results['models'].items() if result.get('success', False)]
    failed_models = [name for name, result in results['models'].items() if not result.get('success', False)]
    
    print(f"✅ Successful Models: {', '.join(successful_models) if successful_models else 'None'}")
    print(f"❌ Failed Models: {', '.join(failed_models) if failed_models else 'None'}")
    print(f"📁 Results saved to {results_file}")
    
    return results

if __name__ == "__main__":
    results = build_all_models()
    print(f"\n🏁 BTC Model Building Complete!")
