"""
ETH Model Builder Script

Build and train all ETH models (Prophet, XGBoost, Ensemble) with performance tracking.
"""

import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta
from typing import Dict, Any
import json
import pickle

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))

from shared.performance_tracker import ModelPerformanceTracker

# Import ETH models from the models directory
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'models'))
from eth_prophet import create_and_train_eth_prophet
from eth_xgboost import create_and_train_eth_xgboost
from eth_ensemble import create_and_train_eth_ensemble

def generate_eth_training_data() -> pd.DataFrame:
    """Generate realistic training data for ETH models."""
    np.random.seed(42)
    
    # ETH-specific parameters
    base_price = 3000  # ETH typical price range
    volatility = 0.05  # Higher volatility than BTC
    
    periods = 1000  # ~3 years of data
    dates = pd.date_range(
        start=datetime.now() - timedelta(days=periods),
        periods=periods,
        freq='D'
    )
    
    # Generate price series with ETH-specific patterns
    returns = np.random.normal(0.0008, volatility, periods)  # Slightly higher drift than BTC
    
    # Add ETH-specific patterns
    trend = np.linspace(0, 0.8, periods)  # Stronger uptrend than BTC historically
    cycle = 0.15 * np.sin(2 * np.pi * np.arange(periods) / 365)  # Stronger annual cycle
    
    # Add DeFi summer and upgrade cycles (ETH-specific events)
    defi_boost = np.where((np.arange(periods) % 365) < 60, 0.05, 0)  # Q1 boost pattern
    upgrade_volatility = np.random.choice([0, 0.02], periods, p=[0.9, 0.1])  # Random upgrade events
    
    returns += trend/periods + cycle/periods + defi_boost/periods + upgrade_volatility
    
    # Generate prices
    log_prices = np.log(base_price) + np.cumsum(returns)
    prices = np.exp(log_prices)
    
    # Generate OHLC with ETH characteristics
    close_prices = prices
    open_prices = np.roll(close_prices, 1)
    open_prices[0] = close_prices[0]
    
    # ETH has higher intraday volatility
    daily_range = volatility * 0.6
    high_prices = np.maximum(open_prices, close_prices) * (1 + np.abs(np.random.normal(0, daily_range, periods)))
    low_prices = np.minimum(open_prices, close_prices) * (1 - np.abs(np.random.normal(0, daily_range, periods)))
    
    # Generate volume with ETH-specific patterns
    price_changes = np.abs(np.diff(np.append(close_prices[0], close_prices)))
    base_volume = 15000000  # ETH has higher volume than BTC typically
    volume_multiplier = 1 + price_changes * 25  # Higher volume sensitivity
    volume = base_volume * volume_multiplier * np.random.lognormal(0, 0.5, periods)
    
    return pd.DataFrame({
        'Open': open_prices,
        'High': high_prices,
        'Low': low_prices,
        'Close': close_prices,
        'Volume': volume
    }, index=dates)

def save_model(model, filename: str, model_dir: str = None) -> bool:
    """
    Save trained model to file.
    
    Args:
        model: Trained model object
        filename: Name for the saved model file
        model_dir: Directory to save model (default: current directory)
        
    Returns:
        Success status
    """
    try:
        if model_dir is None:
            model_dir = os.path.dirname(os.path.abspath(__file__))
            model_dir = os.path.join(model_dir, '..', 'models')
        
        os.makedirs(model_dir, exist_ok=True)
        filepath = os.path.join(model_dir, filename)
        
        with open(filepath, 'wb') as f:
            pickle.dump(model, f)
        
        print(f"✅ Model saved: {filepath}")
        return True
        
    except Exception as e:
        print(f"❌ Failed to save model {filename}: {str(e)}")
        return False

def build_all_eth_models() -> Dict[str, Any]:
    """Build and train all ETH models."""
    print(f"🏗️ Building All ETH Models")
    print("=" * 50)
    
    # Generate training data
    print("📊 Generating ETH training data...")
    training_data = generate_eth_training_data()
    print(f"✅ Generated {len(training_data)} days of ETH training data")
    print(f"   Price range: ${training_data['Close'].min():.2f} - ${training_data['Close'].max():.2f}")
    print(f"   Average daily volume: {training_data['Volume'].mean():,.0f}")
    
    results = {
        'asset_name': 'ETH',
        'build_timestamp': datetime.now().isoformat(),
        'training_samples': len(training_data),
        'price_statistics': {
            'min_price': float(training_data['Close'].min()),
            'max_price': float(training_data['Close'].max()),
            'mean_price': float(training_data['Close'].mean()),
            'volatility': float(training_data['Close'].pct_change().std())
        },
        'models': {}
    }
    
    # Build Prophet model
    try:
        print("\n🔮 Building ETH Prophet Model...")
        prophet_model = create_and_train_eth_prophet(training_data)
        
        # Save Prophet model
        save_success = save_model(prophet_model, 'eth_prophet_model.pkl')
        
        results['models']['prophet'] = {
            'success': prophet_model.is_trained,
            'model_type': 'Prophet',
            'saved': save_success,
            'training_metrics': getattr(prophet_model, 'performance_metrics', {})
        }
        print(f"   Prophet model status: {'✅ Trained' if prophet_model.is_trained else '❌ Failed'}")
        
    except Exception as e:
        print(f"❌ Prophet model failed: {str(e)}")
        results['models']['prophet'] = {'success': False, 'error': str(e)}
    
    # Build XGBoost model
    try:
        print("\n🚀 Building ETH XGBoost Model...")
        xgboost_model = create_and_train_eth_xgboost(training_data)
        
        # Save XGBoost model
        save_success = save_model(xgboost_model, 'eth_xgboost_model.pkl')
        
        results['models']['xgboost'] = {
            'success': xgboost_model.is_trained,
            'model_type': 'XGBoost',
            'saved': save_success,
            'training_metrics': getattr(xgboost_model, 'performance_metrics', {})
        }
        print(f"   XGBoost model status: {'✅ Trained' if xgboost_model.is_trained else '❌ Failed'}")
        
    except Exception as e:
        print(f"❌ XGBoost model failed: {str(e)}")
        results['models']['xgboost'] = {'success': False, 'error': str(e)}
    
    # Build Ensemble model
    try:
        print("\n🎯 Building ETH Ensemble Model...")
        ensemble_model = create_and_train_eth_ensemble(training_data)
        
        # Save Ensemble model
        save_success = save_model(ensemble_model, 'eth_ensemble_model.pkl')
        
        results['models']['ensemble'] = {
            'success': ensemble_model.is_trained,
            'model_type': 'Ensemble',
            'saved': save_success,
            'prophet_weight': ensemble_model.prophet_weight,
            'xgboost_weight': ensemble_model.xgboost_weight,
            'training_metrics': getattr(ensemble_model, 'performance_metrics', {})
        }
        print(f"   Ensemble model status: {'✅ Trained' if ensemble_model.is_trained else '❌ Failed'}")
        print(f"   Weights - Prophet: {ensemble_model.prophet_weight:.3f}, XGBoost: {ensemble_model.xgboost_weight:.3f}")
        
    except Exception as e:
        print(f"❌ Ensemble model failed: {str(e)}")
        results['models']['ensemble'] = {'success': False, 'error': str(e)}
    
    # Summary
    successful_models = sum(1 for model in results['models'].values() if model.get('success', False))
    total_models = len(results['models'])
    
    print(f"\n📊 ETH Model Building Summary")
    print("=" * 30)
    print(f"Successful models: {successful_models}/{total_models}")
    print(f"Training data: {results['training_samples']} samples")
    print(f"ETH price volatility: {results['price_statistics']['volatility']:.4f}")
    
    if successful_models == total_models:
        print("🎉 All ETH models built successfully!")
        results['overall_success'] = True
    else:
        print("⚠️ Some ETH models failed to build")
        results['overall_success'] = False
    
    # Save results summary
    try:
        summary_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'models', 'eth_build_summary.json')
        os.makedirs(os.path.dirname(summary_path), exist_ok=True)
        with open(summary_path, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        print(f"📝 Build summary saved: {summary_path}")
    except Exception as e:
        print(f"⚠️ Could not save build summary: {str(e)}")
    
    return results

def test_model_predictions(data: pd.DataFrame) -> None:
    """Test predictions from all built models."""
    print(f"\n🧪 Testing ETH Model Predictions")
    print("=" * 35)
    
    model_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'models')
    
    # Test Prophet
    prophet_path = os.path.join(model_dir, 'eth_prophet_model.pkl')
    if os.path.exists(prophet_path):
        try:
            with open(prophet_path, 'rb') as f:
                prophet_model = pickle.load(f)
            pred = prophet_model.predict(data.tail(100), periods=7)
            print(f"✅ Prophet prediction: 7 days, range ${pred['yhat'].min():.2f}-${pred['yhat'].max():.2f}")
        except Exception as e:
            print(f"❌ Prophet prediction failed: {str(e)}")
    
    # Test XGBoost
    xgboost_path = os.path.join(model_dir, 'eth_xgboost_model.pkl')
    if os.path.exists(xgboost_path):
        try:
            with open(xgboost_path, 'rb') as f:
                xgboost_model = pickle.load(f)
            pred = xgboost_model.predict(data.tail(1), periods=7)
            print(f"✅ XGBoost prediction: 7 days, range ${pred.min():.2f}-${pred.max():.2f}")
        except Exception as e:
            print(f"❌ XGBoost prediction failed: {str(e)}")
    
    # Test Ensemble
    ensemble_path = os.path.join(model_dir, 'eth_ensemble_model.pkl')
    if os.path.exists(ensemble_path):
        try:
            with open(ensemble_path, 'rb') as f:
                ensemble_model = pickle.load(f)
            pred = ensemble_model.ensemble_predict(data.tail(100), periods=7)
            print(f"✅ Ensemble prediction: 7 days, range ${pred['ensemble_prediction'].min():.2f}-${pred['ensemble_prediction'].max():.2f}")
        except Exception as e:
            print(f"❌ Ensemble prediction failed: {str(e)}")

if __name__ == "__main__":
    # Build all models
    results = build_all_eth_models()
    
    # Test predictions if models were built successfully
    if results['overall_success']:
        training_data = generate_eth_training_data()
        test_model_predictions(training_data)
    
    print(f"\n🦄 ETH Model Building Complete!")
    print(f"Check the models/ directory for saved model files.")
