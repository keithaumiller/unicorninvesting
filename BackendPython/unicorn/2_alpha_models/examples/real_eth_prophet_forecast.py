#!/usr/bin/env python3
"""
Real ETH Prophet Forecast using Silver Layer Data

This script demonstrates creating a production-ready Prophet forecast
for ETH using real market data from our silver layer data pipeline.

Features:
- Real historical ETH data from silver layer
- Prophet methodology with crypto-specific parameters
- Production model training and forecasting
- Performance validation and model registry storage
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import logging
import warnings
warnings.filterwarnings('ignore')

# Setup paths
current_dir = Path(__file__).parent.parent
sys.path.append(str(current_dir))
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/core')

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def create_real_eth_prophet_forecast():
    """Create real ETH Prophet forecast using silver layer data"""
    
    print("🔮 REAL ETH PROPHET FORECAST USING SILVER LAYER DATA")
    print("=" * 65)
    
    # Step 1: Load real ETH data from silver layer
    print("\n📊 Step 1: Loading Real ETH Data from Silver Layer")
    try:
        from silver_layer_data_connector import SilverLayerDataConnector
        connector = SilverLayerDataConnector()
        
        # Get 180 days of real ETH data for robust training
        eth_data = connector.get_historical_data(
            asset='ETH',
            interval='1d',  # Daily data for Prophet
            periods=180     # 6 months of data
        )
        
        if eth_data.empty:
            raise ValueError("No ETH data available from silver layer")
            
        print(f"✅ Retrieved {len(eth_data)} days of real ETH data")
        print(f"📅 Date range: {eth_data.index.min()} to {eth_data.index.max()}")
        print(f"💰 Price range: ${eth_data['close'].min():.2f} - ${eth_data['close'].max():.2f}")
        print(f"📊 Latest price: ${eth_data['close'].iloc[-1]:.2f}")
        
    except Exception as e:
        logger.error(f"Failed to load ETH data: {e}")
        return
    
    # Step 2: Prepare data for Prophet
    print("\n🔧 Step 2: Preparing Data for Prophet Model")
    try:
        # Prophet requires 'ds' (datestamp) and 'y' (value) columns
        # Remove timezone from the index to avoid Prophet compatibility issues
        prophet_data = pd.DataFrame({
            'ds': eth_data.index.tz_localize(None),  # Remove timezone
            'y': eth_data['close'].values
        })
        
        # Add additional regressors from silver layer features
        prophet_data['volume'] = eth_data['volume'].values
        prophet_data['volatility'] = eth_data['volatility_14'].values
        prophet_data['rsi'] = eth_data['rsi'].values
        
        # Handle any missing values
        prophet_data = prophet_data.fillna(method='ffill').fillna(method='bfill')
        
        print(f"✅ Prepared Prophet data: {len(prophet_data)} records")
        print(f"📈 Features: ds, y, volume, volatility, rsi")
        
    except Exception as e:
        logger.error(f"Failed to prepare Prophet data: {e}")
        return
    
    # Step 3: Create and configure Prophet model
    print("\n🤖 Step 3: Creating Prophet Model with Crypto-Specific Configuration")
    try:
        # Direct Prophet import since methodology import has path issues
        from prophet import Prophet
        
        # Create Prophet model with crypto-optimized parameters
        model = Prophet(
            # Crypto markets are 24/7, so no weekly/yearly seasonality patterns like stocks
            daily_seasonality=True,     # Capture intraday patterns
            weekly_seasonality=False,   # Crypto doesn't follow traditional weekly patterns
            yearly_seasonality=False,   # Not enough historical data for yearly patterns
            
            # Crypto is highly volatile - be more conservative with trend changes
            changepoint_prior_scale=0.01,  # Lower = more conservative trend changes
            seasonality_prior_scale=0.1,   # Lower = more conservative seasonality
            
            # Wider uncertainty intervals for crypto volatility
            interval_width=0.95,        # 95% confidence intervals
            
            # Growth model
            growth='linear',            # Linear growth assumption
            
            # Mcmc sampling for better uncertainty estimation
            mcmc_samples=100           # Bayesian sampling for uncertainty
        )
        
        # Add additional regressors for market features
        model.add_regressor('volume', standardize=True)
        model.add_regressor('volatility', standardize=True) 
        model.add_regressor('rsi', standardize=True)
        
        print("✅ Prophet model configured with crypto-specific parameters")
        print("   - 24/7 market patterns (no weekly seasonality)")
        print("   - Conservative trend change detection")
        print("   - Wide uncertainty intervals for crypto volatility")
        print("   - Additional regressors: volume, volatility, RSI")
        
    except Exception as e:
        logger.error(f"Failed to create Prophet model: {e}")
        return
    
    # Step 4: Train the model
    print("\n🎯 Step 4: Training Prophet Model on Real ETH Data")
    try:
        # Fit the model
        start_time = datetime.now()
        model.fit(prophet_data)
        training_time = (datetime.now() - start_time).total_seconds()
        
        print(f"✅ Model training completed in {training_time:.2f} seconds")
        print(f"📊 Trained on {len(prophet_data)} real ETH data points")
        
    except Exception as e:
        logger.error(f"Failed to train Prophet model: {e}")
        return
    
    # Step 5: Generate forecasts
    print("\n🔮 Step 5: Generating ETH Price Forecasts")
    try:
        # Create future dataframe for next 30 days
        future_periods = 30
        future = model.make_future_dataframe(periods=future_periods, freq='D')
        
        # Add regressor values for future periods
        # For future periods, use last known values (conservative approach)
        last_volume = prophet_data['volume'].iloc[-1]
        last_volatility = prophet_data['volatility'].iloc[-1]
        last_rsi = prophet_data['rsi'].iloc[-1]
        
        # Set regressor values for ALL rows (historical + future)
        future['volume'] = last_volume  # Set all rows to the last known value
        future['volatility'] = last_volatility
        future['rsi'] = last_rsi
        
        # For historical data, use actual values where available
        historical_len = len(prophet_data)
        future.loc[:historical_len-1, 'volume'] = prophet_data['volume'].values
        future.loc[:historical_len-1, 'volatility'] = prophet_data['volatility'].values
        future.loc[:historical_len-1, 'rsi'] = prophet_data['rsi'].values
        
        # Generate forecast
        forecast = model.predict(future)
        
        # Extract future predictions
        forecast_future = forecast.tail(future_periods)
        
        print(f"✅ Generated {future_periods}-day ETH price forecast")
        print(f"📅 Forecast period: {forecast_future['ds'].min().date()} to {forecast_future['ds'].max().date()}")
        
        # Display forecast summary
        current_price = prophet_data['y'].iloc[-1]
        forecast_prices = forecast_future['yhat'].values
        price_change = ((forecast_prices[-1] - current_price) / current_price) * 100
        
        print(f"\n💰 FORECAST SUMMARY:")
        print(f"   Current ETH Price: ${current_price:.2f}")
        print(f"   30-day Forecast: ${forecast_prices[-1]:.2f}")
        print(f"   Expected Change: {price_change:+.2f}%")
        print(f"   Confidence Interval: ${forecast_future['yhat_lower'].iloc[-1]:.2f} - ${forecast_future['yhat_upper'].iloc[-1]:.2f}")
        
        # Show weekly forecasts
        print(f"\n📈 WEEKLY FORECAST BREAKDOWN:")
        for week in range(0, future_periods, 7):
            if week < len(forecast_future):
                week_data = forecast_future.iloc[week]
                days_out = week + 1
                weekly_change = ((week_data['yhat'] - current_price) / current_price) * 100
                print(f"   Day {days_out:2d}: ${week_data['yhat']:.2f} ({weekly_change:+.1f}%) [${week_data['yhat_lower']:.2f}-${week_data['yhat_upper']:.2f}]")
        
    except Exception as e:
        logger.error(f"Failed to generate forecasts: {e}")
        return
    
    # Step 6: Model performance validation
    print("\n✅ Step 6: Model Performance Validation")
    try:
        # Cross-validation on recent data
        from prophet.diagnostics import cross_validation, performance_metrics
        
        # Use last 60 days for cross-validation
        cv_results = cross_validation(
            model, 
            initial='120 days',    # Use 120 days for initial training
            period='7 days',       # Make forecast every 7 days  
            horizon='14 days',     # Forecast 14 days ahead
            parallel='processes'   # Parallel processing
        )
        
        # Calculate performance metrics
        perf_metrics = performance_metrics(cv_results)
        
        # Display key metrics
        mape = perf_metrics['mape'].mean() * 100  # Convert to percentage
        rmse = perf_metrics['rmse'].mean()
        mae = perf_metrics['mae'].mean()
        
        print(f"✅ Cross-validation completed on recent data")
        print(f"📊 PERFORMANCE METRICS:")
        print(f"   Mean Absolute Percentage Error (MAPE): {mape:.2f}%")
        print(f"   Root Mean Square Error (RMSE): ${rmse:.2f}")
        print(f"   Mean Absolute Error (MAE): ${mae:.2f}")
        
        # Validate model quality
        if mape < 15:  # Less than 15% error is good for crypto
            print(f"✅ Model performance: EXCELLENT (MAPE < 15%)")
        elif mape < 25:
            print(f"⚠️  Model performance: ACCEPTABLE (MAPE < 25%)")
        else:
            print(f"❌ Model performance: POOR (MAPE > 25%)")
            
    except Exception as e:
        logger.warning(f"Performance validation failed: {e}")
        print("⚠️  Continuing without detailed performance metrics")
    
    # Step 7: Save model and results
    print("\n💾 Step 7: Saving Model and Forecast Results")
    try:
        # Create output directory
        output_dir = current_dir / "examples" / "results"
        output_dir.mkdir(exist_ok=True)
        
        # Save forecast results
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        forecast_file = output_dir / f"eth_prophet_forecast_{timestamp}.csv"
        
        # Prepare forecast output
        forecast_output = forecast_future[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].copy()
        forecast_output.columns = ['date', 'forecast_price', 'lower_bound', 'upper_bound']
        forecast_output['current_price'] = current_price
        forecast_output['days_ahead'] = range(1, len(forecast_output) + 1)
        forecast_output['price_change_pct'] = ((forecast_output['forecast_price'] - current_price) / current_price) * 100
        
        # Save to CSV
        forecast_output.to_csv(forecast_file, index=False)
        print(f"✅ Forecast results saved: {forecast_file}")
        
        # Save model metadata
        metadata = {
            'model_type': 'Prophet',
            'asset': 'ETH',
            'training_data_points': len(prophet_data),
            'training_period': f"{prophet_data['ds'].min()} to {prophet_data['ds'].max()}",
            'forecast_period': f"{forecast_future['ds'].min().date()} to {forecast_future['ds'].max().date()}",
            'current_price': current_price,
            'forecast_price_30d': forecast_prices[-1],
            'expected_change_30d_pct': price_change,
            'confidence_interval_30d': f"${forecast_future['yhat_lower'].iloc[-1]:.2f} - ${forecast_future['yhat_upper'].iloc[-1]:.2f}",
            'created_timestamp': datetime.now().isoformat(),
            'data_source': 'silver_layer'
        }
        
        import json
        metadata_file = output_dir / f"eth_prophet_metadata_{timestamp}.json"
        with open(metadata_file, 'w') as f:
            json.dump(metadata, f, indent=2)
        print(f"✅ Model metadata saved: {metadata_file}")
        
    except Exception as e:
        logger.error(f"Failed to save results: {e}")
    
    print(f"\n🎉 REAL ETH PROPHET FORECAST COMPLETED SUCCESSFULLY!")
    print(f"📊 Used {len(prophet_data)} days of real silver layer data")
    print(f"🔮 Generated 30-day ETH price forecast")
    print(f"💰 Current: ${current_price:.2f} → 30-day: ${forecast_prices[-1]:.2f} ({price_change:+.2f}%)")
    
    return {
        'model': model,
        'forecast': forecast_future,
        'current_price': current_price,
        'metadata': metadata
    }

if __name__ == "__main__":
    # Execute the real ETH Prophet forecast
    result = create_real_eth_prophet_forecast()