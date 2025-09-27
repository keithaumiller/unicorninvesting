#!/usr/bin/env python3
"""
Simple XGBoost Test for ETH Forecasting

This script tests the XGBoost implementation with mock data.
"""

import pandas as pd
import numpy as np
from datetime import datetime
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def test_xgboost_forecast():
    """Test XGBoost forecasting capability"""
    
    print("🚀 TESTING XGBOOST FOR ETH FORECASTING")
    print("=" * 50)
    
    try:
        import xgboost as xgb
        from sklearn.metrics import mean_absolute_percentage_error
        
        print("✅ XGBoost library available")
        
        # Create test data
        dates = pd.date_range(start='2024-03-01', end='2024-09-22', freq='D')
        np.random.seed(42)
        
        # Generate realistic ETH price data
        base_price = 3500
        returns = np.random.normal(0.001, 0.03, len(dates))
        prices = [base_price]
        
        for ret in returns[1:]:
            prices.append(prices[-1] * (1 + ret))
        
        eth_data = pd.DataFrame({
            'close': prices,
            'volume': np.random.lognormal(15, 1, len(dates))
        }, index=dates)
        
        print(f"📊 Created test data: {len(eth_data)} days")
        print(f"💰 Price range: ${eth_data['close'].min():.2f} - ${eth_data['close'].max():.2f}")
        
        # Simple feature engineering
        eth_data['returns'] = eth_data['close'].pct_change()
        eth_data['sma_5'] = eth_data['close'].rolling(5).mean()
        eth_data['sma_10'] = eth_data['close'].rolling(10).mean()
        eth_data['volatility'] = eth_data['returns'].rolling(10).std()
        eth_data = eth_data.dropna()
        
        # Prepare training data
        features = ['returns', 'sma_5', 'sma_10', 'volatility']
        X = eth_data[features].iloc[:-10]
        y = eth_data['close'].shift(-1).iloc[:-10]
        
        # Remove NaN values
        mask = ~(X.isna().any(axis=1) | y.isna())
        X = X[mask]
        y = y[mask]
        
        print(f"🎯 Training on {len(X)} samples")
        
        # Train model
        model = xgb.XGBRegressor(
            n_estimators=50,
            max_depth=4,
            learning_rate=0.1,
            random_state=42
        )
        
        model.fit(X, y)
        print("✅ XGBoost model trained")
        
        # Make prediction
        current_price = eth_data['close'].iloc[-1]
        last_features = eth_data[features].iloc[-1:].values
        next_price = model.predict(last_features)[0]
        
        price_change = (next_price - current_price) / current_price * 100
        
        print(f"\n🔮 XGBoost Test Forecast:")
        print(f"   Current Price: ${current_price:.2f}")
        print(f"   Next Day Forecast: ${next_price:.2f}")
        print(f"   Expected Change: {price_change:+.2f}%")
        
        print(f"\n🎉 XGBoost test completed successfully!")
        return True
        
    except ImportError:
        print("❌ XGBoost not available. Install with: pip install xgboost")
        return False
    except Exception as e:
        print(f"❌ Test failed: {e}")
        return False

if __name__ == "__main__":
    test_xgboost_forecast()