#!/usr/bin/env python3
"""
Prophet Configuration Test
==========================

This script tests Prophet installation and basic functionality
for forex time series forecasting.
"""

import sys
import warnings
warnings.filterwarnings('ignore')

def test_prophet_installation():
    """Test Prophet installation and basic functionality."""
    print("🔮 Testing Prophet Installation and Configuration")
    print("=" * 50)
    
    try:
        # Test Prophet import
        print("📦 Testing Prophet import...")
        from prophet import Prophet
        print("✅ Prophet imported successfully")
        
        # Test pandas integration
        print("📊 Testing pandas integration...")
        import pandas as pd
        import numpy as np
        from datetime import datetime, timedelta
        print("✅ Dependencies imported successfully")
        
        # Create sample forex-like data
        print("💱 Creating sample forex data...")
        dates = pd.date_range(start='2023-01-01', end='2023-03-31', freq='H')
        np.random.seed(42)
        
        # Simulate EURUSD-like price movement
        base_price = 1.1000
        returns = np.random.normal(0, 0.001, len(dates))  # 0.1% hourly volatility
        prices = [base_price]
        
        for i in range(1, len(dates)):
            # Add some hourly seasonality (simplified)
            hour = dates[i].hour
            session_effect = 0
            if 8 <= hour <= 16:  # European session
                session_effect = 0.0002
            elif 13 <= hour <= 21:  # US session overlap
                session_effect = 0.0003
            
            new_price = prices[-1] * (1 + returns[i] + session_effect)
            prices.append(new_price)
        
        print(f"✅ Generated {len(prices)} data points")
        print(f"📊 Price range: {min(prices):.5f} - {max(prices):.5f}")
        
        # Prepare Prophet data
        print("🔮 Preparing Prophet dataset...")
        prophet_data = pd.DataFrame({
            'ds': dates,
            'y': prices
        })
        
        # Split data for validation
        train_size = int(len(prophet_data) * 0.8)
        train_data = prophet_data[:train_size]
        test_data = prophet_data[train_size:]
        
        print(f"📈 Training data: {len(train_data)} points")
        print(f"📊 Test data: {len(test_data)} points")
        
        # Create and configure Prophet model for forex
        print("⚙️ Configuring Prophet model for forex...")
        model = Prophet(
            daily_seasonality=True,      # Trading sessions
            weekly_seasonality=True,     # Weekday patterns
            yearly_seasonality=False,    # Not enough data
            changepoint_prior_scale=0.05,
            seasonality_prior_scale=10.0,
            seasonality_mode='multiplicative',
            interval_width=0.8
        )
        
        print("✅ Prophet model configured")
        
        # Train the model
        print("🏋️ Training Prophet model...")
        model.fit(train_data)
        print("✅ Prophet model trained successfully")
        
        # Make future predictions
        print("🔮 Generating forecasts...")
        future = model.make_future_dataframe(periods=24, freq='H')  # 24 hours ahead
        forecast = model.predict(future)
        print("✅ Forecasts generated")
        
        # Evaluate on test data
        print("📊 Evaluating forecast accuracy...")
        test_forecast = forecast.tail(len(test_data))
        actual_prices = test_data['y'].values
        predicted_prices = test_forecast['yhat'].values[:len(actual_prices)]
        
        # Calculate metrics
        mae = np.mean(np.abs(predicted_prices - actual_prices))
        mape = np.mean(np.abs((predicted_prices - actual_prices) / actual_prices)) * 100
        
        print(f"📊 Mean Absolute Error: {mae:.6f}")
        print(f"📊 Mean Absolute Percentage Error: {mape:.2f}%")
        
        # Show sample predictions
        print("\n🔮 Sample Forecast Results:")
        print("-" * 40)
        print("Time\t\t\tActual\t\tPredicted\tLower\t\tUpper")
        print("-" * 40)
        
        for i in range(min(5, len(test_data))):
            time_str = test_data.iloc[i]['ds'].strftime('%m-%d %H:%M')
            actual = actual_prices[i]
            pred = predicted_prices[i]
            lower = test_forecast.iloc[i]['yhat_lower']
            upper = test_forecast.iloc[i]['yhat_upper']
            
            print(f"{time_str}\t{actual:.5f}\t{pred:.5f}\t{lower:.5f}\t{upper:.5f}")
        
        print("-" * 40)
        
        # Test Prophet components
        print("\n🔍 Analyzing Prophet components...")
        components = model.predict(train_data)
        
        if 'trend' in components.columns:
            print(f"📈 Trend component: Available")
        if 'daily' in components.columns:
            print(f"🕐 Daily seasonality: Available")
        if 'weekly' in components.columns:
            print(f"📅 Weekly seasonality: Available")
        
        # Future forecast
        print("\n🚀 Next 6 hours forecast:")
        future_24h = model.make_future_dataframe(periods=6, freq='H', include_history=False)
        future_forecast = model.predict(future_24h)
        
        current_price = prices[-1]
        for i, row in future_forecast.iterrows():
            time_str = row['ds'].strftime('%m-%d %H:%M')
            pred_price = row['yhat']
            change_pct = (pred_price - current_price) / current_price * 100
            print(f"   {time_str}: {pred_price:.5f} ({change_pct:+.2f}%)")
        
        print("\n" + "=" * 50)
        print("✅ Prophet Configuration Test PASSED")
        print("🔮 Prophet is ready for forex forecasting!")
        print("=" * 50)
        
        return True
        
    except ImportError as e:
        print(f"❌ Import Error: {e}")
        print("💡 Try: pip install prophet")
        return False
        
    except Exception as e:
        print(f"❌ Prophet Test Failed: {e}")
        return False

def test_prophet_performance():
    """Test Prophet performance characteristics."""
    print("\n⚡ Testing Prophet Performance...")
    
    try:
        import time
        from prophet import Prophet
        import pandas as pd
        import numpy as np
        
        # Generate larger dataset
        dates = pd.date_range(start='2023-01-01', end='2023-12-31', freq='H')
        np.random.seed(42)
        prices = np.cumsum(np.random.randn(len(dates)) * 0.001) + 1.1000
        
        data = pd.DataFrame({'ds': dates, 'y': prices})
        
        # Test training time
        start_time = time.time()
        model = Prophet(daily_seasonality=True, weekly_seasonality=True)
        model.fit(data)
        training_time = time.time() - start_time
        
        # Test prediction time
        start_time = time.time()
        future = model.make_future_dataframe(periods=24, freq='H')
        forecast = model.predict(future)
        prediction_time = time.time() - start_time
        
        print(f"📊 Dataset size: {len(data)} points")
        print(f"⏱️ Training time: {training_time:.2f} seconds")
        print(f"⚡ Prediction time: {prediction_time:.2f} seconds")
        print(f"🚀 Performance: {'Good' if training_time < 30 else 'Slow'}")
        
        return True
        
    except Exception as e:
        print(f"❌ Performance test failed: {e}")
        return False

if __name__ == "__main__":
    print("🦄 Unicorn Investing - Prophet Configuration Test")
    print("=" * 60)
    
    # Run basic test
    basic_test = test_prophet_installation()
    
    if basic_test:
        # Run performance test
        perf_test = test_prophet_performance()
        
        if perf_test:
            print("\n🎉 All Prophet tests passed!")
            print("✅ Prophet is ready for LEAN integration")
        else:
            print("\n⚠️ Basic functionality works, performance may be slow")
    else:
        print("\n❌ Prophet configuration failed")
        print("Please install Prophet: pip install prophet")
        sys.exit(1)
