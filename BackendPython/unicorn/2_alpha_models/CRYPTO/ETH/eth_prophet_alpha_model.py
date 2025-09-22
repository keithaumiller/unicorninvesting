"""
ETH Prophet Alpha Model

A clean, focused implementation of Facebook Prophet for ETH price forecasting.
This model provides alpha signals for trading algorithms by predicting future ETH price movements.

Key Features:
- Simple Prophet implementation with optimal parameters for crypto markets
- Alpha signal generation (BUY/SELL/HOLD)
- Performance tracking and validation
- Real-time forecast capability
- Leak-free feature engineering following Unicorn Investing standards

Created: January 2025
Status: Production Ready
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional, Tuple
import sys
import os
from datetime import datetime, timedelta
import pickle
import warnings
import sqlite3
import json
from pathlib import Path

# Add parent directories to path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, '../../../../../'))
sys.path.append(project_root)

try:
    from prophet import Prophet
    from prophet.diagnostics import cross_validation, performance_metrics
except ImportError:
    print("⚠️  Prophet not installed. Install with: pip install prophet")
    print("Run: pip install prophet")
    sys.exit(1)

warnings.filterwarnings('ignore', category=RuntimeWarning)

class ETHProphetAlphaModel:
    """
    ETH Prophet Alpha Model for generating trading signals.
    
    This model uses Facebook Prophet to forecast ETH prices and generates
    alpha signals for trading algorithms.
    """
    
    def __init__(self, timeframe: str = '1hour', lookback_days: int = 30):
        """
        Initialize the ETH Prophet Alpha Model.
        
        Args:
            timeframe: Data timeframe ('1min', '1hour', '1day')
            lookback_days: Number of days of historical data to use
        """
        self.timeframe = timeframe
        self.lookback_days = lookback_days
        self.model = None
        self.last_training_time = None
        self.performance_metrics = {}
        
        # Alpha signal thresholds
        self.signal_thresholds = {
            'strong_buy': 0.03,    # 3% expected return
            'buy': 0.01,           # 1% expected return
            'hold': 0.01,          # Within ±1%
            'sell': -0.01,         # -1% expected return
            'strong_sell': -0.03   # -3% expected return
        }
        
        # Prophet model parameters optimized for crypto
        self.prophet_params = {
            'growth': 'linear',
            'changepoint_prior_scale': 0.1,  # More flexible for crypto volatility
            'seasonality_prior_scale': 0.1,   # Moderate seasonality
            'holidays_prior_scale': 0.1,      # Account for market events
            'seasonality_mode': 'multiplicative',  # Better for financial data
            'interval_width': 0.8,             # 80% confidence intervals
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': timeframe in ['1min', '1hour']  # Only for intraday
        }
    
    def get_eth_data(self) -> pd.DataFrame:
        """
        Fetch ETH price data from the silver layer.
        
        Returns:
            DataFrame with ETH price data (timestamp, close, volume, etc.)
        """
        try:
            # Import silver layer data connector
            silver_layer_path = os.path.join(project_root, 'BackendPython/unicorn/4_portfolios/Myportolio/core')
            sys.path.append(silver_layer_path)
            
            from silver_layer_data_connector import SilverLayerDataConnector
            
            connector = SilverLayerDataConnector()
            
            # Calculate start date
            end_date = datetime.now()
            start_date = end_date - timedelta(days=self.lookback_days)
            
            # Map our timeframe to silver layer interval format
            interval_map = {
                '1min': '1m',
                '1hour': '1h', 
                '1day': '1d'
            }
            interval = interval_map.get(self.timeframe, '1h')
            
            # Fetch ETH data using correct method name and parameters
            data = connector.get_historical_data(
                asset='ETH',
                interval=interval,
                start_date=start_date.strftime('%Y-%m-%d'),
                end_date=end_date.strftime('%Y-%m-%d')
            )
            
            if data is None or data.empty:
                print(f"❌ No ETH data found for interval {interval}")
                return pd.DataFrame()
            
            print(f"✅ Retrieved {len(data)} ETH data points from silver layer")
            return data
            
        except Exception as e:
            print(f"❌ Error fetching ETH data: {e}")
            # Print more detailed error info for debugging
            import traceback
            print(f"📋 Detailed error: {traceback.format_exc()}")
            return pd.DataFrame()
    
    def prepare_prophet_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Prepare data for Prophet training.
        
        Args:
            data: Raw ETH price data
            
        Returns:
            DataFrame formatted for Prophet (ds, y columns)
        """
        if data.empty:
            return pd.DataFrame()
        
        print(f"📊 Data columns available: {data.columns.tolist()}")
        
        # Determine timestamp and price columns based on available columns
        timestamp_col = None
        price_col = None
        
        # Look for timestamp column
        for col in ['timestamp', 'datetime', 'date', 'ds']:
            if col in data.columns:
                timestamp_col = col
                break
        
        # Look for price column (prefer close, then others)
        for col in ['close', 'Close', 'price', 'Price', 'y']:
            if col in data.columns:
                price_col = col
                break
        
        if timestamp_col is None:
            # If no timestamp column found, use index if it's datetime
            if isinstance(data.index, pd.DatetimeIndex):
                print("📅 Using datetime index as timestamp")
                data = data.reset_index()
                timestamp_col = data.columns[0]  # First column should be the index
            else:
                print(f"❌ No timestamp column found. Available columns: {data.columns.tolist()}")
                return pd.DataFrame()
        
        if price_col is None:
            print(f"❌ No price column found. Available columns: {data.columns.tolist()}")
            return pd.DataFrame()
        
        print(f"✅ Using timestamp column: '{timestamp_col}', price column: '{price_col}'")
        
        # Prepare Prophet format
        prophet_data = pd.DataFrame({
            'ds': pd.to_datetime(data[timestamp_col]),
            'y': pd.to_numeric(data[price_col], errors='coerce')
        })
        
        # Remove any NaN values
        prophet_data = prophet_data.dropna()
        
        # Sort by timestamp
        prophet_data = prophet_data.sort_values('ds').reset_index(drop=True)
        
        print(f"✅ Prepared {len(prophet_data)} data points for Prophet training")
        print(f"📊 Price range: ${prophet_data['y'].min():.2f} - ${prophet_data['y'].max():.2f}")
        return prophet_data
    
    def train_model(self, data: pd.DataFrame = None) -> bool:
        """
        Train the Prophet model.
        
        Args:
            data: Optional pre-loaded data. If None, will fetch fresh data.
            
        Returns:
            True if training successful, False otherwise
        """
        try:
            # Get data if not provided
            if data is None:
                print(f"📊 Fetching ETH data for {self.timeframe} timeframe...")
                data = self.get_eth_data()
            
            if data.empty:
                print("❌ No data available for training")
                return False
            
            # Prepare data for Prophet
            prophet_data = self.prepare_prophet_data(data)
            if prophet_data.empty:
                print("❌ Failed to prepare data for Prophet")
                return False
            
            # Initialize and train Prophet model
            print(f"🤖 Training Prophet model with {len(prophet_data)} data points...")
            self.model = Prophet(**self.prophet_params)
            
            # Fit the model
            self.model.fit(prophet_data)
            self.last_training_time = datetime.now()
            
            # Calculate performance metrics on training data (for monitoring only)
            self._calculate_performance_metrics(prophet_data)
            
            print(f"✅ Prophet model trained successfully at {self.last_training_time}")
            return True
            
        except Exception as e:
            print(f"❌ Error training Prophet model: {e}")
            return False
    
    def _calculate_performance_metrics(self, data: pd.DataFrame):
        """
        Calculate performance metrics for the trained model.
        
        Args:
            data: Training data used for the model
        """
        try:
            if self.model is None:
                return
            
            # Create forecast for the training period (for basic validation)
            forecast = self.model.predict(data[['ds']])
            
            # Calculate basic metrics
            actual = data['y'].values
            predicted = forecast['yhat'].values
            
            # Ensure same length
            min_len = min(len(actual), len(predicted))
            actual = actual[:min_len]
            predicted = predicted[:min_len]
            
            # Calculate metrics
            mse = np.mean((actual - predicted) ** 2)
            rmse = np.sqrt(mse)
            mae = np.mean(np.abs(actual - predicted))
            mape = np.mean(np.abs((actual - predicted) / actual)) * 100
            
            # R-squared (coefficient of determination)
            ss_res = np.sum((actual - predicted) ** 2)
            ss_tot = np.sum((actual - np.mean(actual)) ** 2)
            r2 = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0
            
            self.performance_metrics = {
                'mse': float(mse),
                'rmse': float(rmse),
                'mae': float(mae),
                'mape': float(mape),
                'r2': float(r2),
                'training_samples': len(actual),
                'last_updated': datetime.now().isoformat()
            }
            
            print(f"📊 Performance Metrics - RMSE: {rmse:.4f}, MAE: {mae:.4f}, MAPE: {mape:.2f}%, R²: {r2:.4f}")
            
        except Exception as e:
            print(f"⚠️  Error calculating performance metrics: {e}")
    
    def generate_forecast(self, periods: int = 24) -> pd.DataFrame:
        """
        Generate future price forecasts.
        
        Args:
            periods: Number of periods to forecast ahead
            
        Returns:
            DataFrame with forecast results
        """
        if self.model is None:
            print("❌ Model not trained. Call train_model() first.")
            return pd.DataFrame()
        
        try:
            # Create future dataframe
            future = self.model.make_future_dataframe(periods=periods, freq='H' if self.timeframe == '1hour' else 'D')
            
            # Generate forecast
            forecast = self.model.predict(future)
            
            # Return relevant columns
            result = forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].copy()
            result.columns = ['timestamp', 'predicted_price', 'lower_bound', 'upper_bound']
            
            # Add confidence score
            result['confidence'] = (result['upper_bound'] - result['lower_bound']) / result['predicted_price']
            
            print(f"🔮 Generated forecast for {periods} periods ahead")
            return result
            
        except Exception as e:
            print(f"❌ Error generating forecast: {e}")
            return pd.DataFrame()
    
    def generate_alpha_signal(self, current_price: float = None) -> Dict[str, Any]:
        """
        Generate alpha trading signal based on Prophet forecast.
        
        Args:
            current_price: Current ETH price. If None, will use latest forecast.
            
        Returns:
            Dictionary with signal, strength, and metadata
        """
        if self.model is None:
            return {
                'signal': 'HOLD',
                'strength': 0.0,
                'confidence': 0.0,
                'expected_return': 0.0,
                'error': 'Model not trained'
            }
        
        try:
            # Generate forecast for next period
            forecast = self.generate_forecast(periods=1)
            
            if forecast.empty:
                return {
                    'signal': 'HOLD',
                    'strength': 0.0,
                    'confidence': 0.0,
                    'expected_return': 0.0,
                    'error': 'No forecast generated'
                }
            
            # Get next predicted price
            next_price = forecast.iloc[-1]['predicted_price']
            confidence = 1.0 / (1.0 + forecast.iloc[-1]['confidence'])  # Higher confidence = lower uncertainty
            
            # Use current price or latest available price
            if current_price is None:
                # Fetch current price from data
                current_data = self.get_eth_data()
                if not current_data.empty:
                    # Look for price column
                    price_col = None
                    for col in ['close', 'Close', 'price', 'Price']:
                        if col in current_data.columns:
                            price_col = col
                            break
                    
                    if price_col:
                        current_price = float(current_data.iloc[-1][price_col])
                    else:
                        return {
                            'signal': 'HOLD',
                            'strength': 0.0,
                            'confidence': 0.0,
                            'expected_return': 0.0,
                            'error': 'Cannot find price column in data'
                        }
                else:
                    return {
                        'signal': 'HOLD',
                        'strength': 0.0,
                        'confidence': 0.0,
                        'expected_return': 0.0,
                        'error': 'Cannot determine current price'
                    }
            
            # Calculate expected return
            expected_return = (next_price - current_price) / current_price
            
            # Generate signal based on thresholds
            if expected_return >= self.signal_thresholds['strong_buy']:
                signal = 'STRONG_BUY'
                strength = min(1.0, expected_return / self.signal_thresholds['strong_buy'])
            elif expected_return >= self.signal_thresholds['buy']:
                signal = 'BUY'
                strength = expected_return / self.signal_thresholds['buy']
            elif expected_return <= self.signal_thresholds['strong_sell']:
                signal = 'STRONG_SELL'
                strength = min(1.0, abs(expected_return) / abs(self.signal_thresholds['strong_sell']))
            elif expected_return <= self.signal_thresholds['sell']:
                signal = 'SELL'
                strength = abs(expected_return) / abs(self.signal_thresholds['sell'])
            else:
                signal = 'HOLD'
                strength = 0.0
            
            return {
                'signal': signal,
                'strength': float(strength),
                'confidence': float(confidence),
                'expected_return': float(expected_return),
                'current_price': float(current_price),
                'predicted_price': float(next_price),
                'timeframe': self.timeframe,
                'timestamp': datetime.now().isoformat(),
                'model_last_trained': self.last_training_time.isoformat() if self.last_training_time else None
            }
            
        except Exception as e:
            return {
                'signal': 'HOLD',
                'strength': 0.0,
                'confidence': 0.0,
                'expected_return': 0.0,
                'error': f'Signal generation error: {str(e)}'
            }
    
    def save_model(self, filepath: str = None) -> bool:
        """
        Save the trained model to disk.
        
        Args:
            filepath: Path to save the model. If None, uses default path.
            
        Returns:
            True if save successful, False otherwise
        """
        if self.model is None:
            print("❌ No model to save")
            return False
        
        try:
            if filepath is None:
                # Create default path
                model_dir = os.path.join(current_dir, 'models', 'prophet')
                os.makedirs(model_dir, exist_ok=True)
                timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
                filepath = os.path.join(model_dir, f'eth_prophet_{self.timeframe}_{timestamp}.pkl')
            
            # Save model and metadata
            model_data = {
                'model': self.model,
                'timeframe': self.timeframe,
                'lookback_days': self.lookback_days,
                'signal_thresholds': self.signal_thresholds,
                'prophet_params': self.prophet_params,
                'performance_metrics': self.performance_metrics,
                'last_training_time': self.last_training_time,
                'created_time': datetime.now()
            }
            
            with open(filepath, 'wb') as f:
                pickle.dump(model_data, f)
            
            print(f"✅ Model saved to: {filepath}")
            return True
            
        except Exception as e:
            print(f"❌ Error saving model: {e}")
            return False
    
    def load_model(self, filepath: str) -> bool:
        """
        Load a trained model from disk.
        
        Args:
            filepath: Path to the saved model file
            
        Returns:
            True if load successful, False otherwise
        """
        try:
            if not os.path.exists(filepath):
                print(f"❌ Model file not found: {filepath}")
                return False
            
            with open(filepath, 'rb') as f:
                model_data = pickle.load(f)
            
            # Restore model and parameters
            self.model = model_data['model']
            self.timeframe = model_data['timeframe']
            self.lookback_days = model_data['lookback_days']
            self.signal_thresholds = model_data['signal_thresholds']
            self.prophet_params = model_data['prophet_params']
            self.performance_metrics = model_data['performance_metrics']
            self.last_training_time = model_data['last_training_time']
            
            print(f"✅ Model loaded from: {filepath}")
            print(f"📊 Model trained on: {self.last_training_time}")
            return True
            
        except Exception as e:
            print(f"❌ Error loading model: {e}")
            return False
    
    def get_model_info(self) -> Dict[str, Any]:
        """
        Get comprehensive information about the current model.
        
        Returns:
            Dictionary with model information and performance
        """
        return {
            'model_type': 'Prophet',
            'asset': 'ETH',
            'timeframe': self.timeframe,
            'lookback_days': self.lookback_days,
            'is_trained': self.model is not None,
            'last_training_time': self.last_training_time.isoformat() if self.last_training_time else None,
            'performance_metrics': self.performance_metrics,
            'signal_thresholds': self.signal_thresholds,
            'prophet_parameters': self.prophet_params
        }


def demo_eth_prophet_alpha():
    """
    Demonstration of ETH Prophet Alpha Model usage.
    """
    print("🦄 ETH Prophet Alpha Model Demo")
    print("=" * 50)
    
    # Initialize model
    print("📊 Initializing ETH Prophet Alpha Model...")
    model = ETHProphetAlphaModel(timeframe='1hour', lookback_days=7)
    
    # Train model
    print("\n🤖 Training Prophet model...")
    success = model.train_model()
    
    if not success:
        print("❌ Training failed")
        return
    
    # Generate alpha signal
    print("\n🔮 Generating alpha signal...")
    signal = model.generate_alpha_signal()
    
    print(f"\n📊 Alpha Signal Results:")
    print(f"Signal: {signal['signal']}")
    print(f"Strength: {signal['strength']:.3f}")
    print(f"Confidence: {signal['confidence']:.3f}")
    print(f"Expected Return: {signal['expected_return']:.4f} ({signal['expected_return']*100:.2f}%)")
    
    # Model information
    print(f"\n📋 Model Information:")
    info = model.get_model_info()
    print(f"Performance R²: {info['performance_metrics'].get('r2', 'N/A'):.4f}")
    print(f"MAPE: {info['performance_metrics'].get('mape', 'N/A'):.2f}%")
    print(f"Training Samples: {info['performance_metrics'].get('training_samples', 'N/A')}")
    
    # Save model
    print(f"\n💾 Saving model...")
    model.save_model()
    
    print("\n✅ Demo completed successfully!")


if __name__ == "__main__":
    demo_eth_prophet_alpha()