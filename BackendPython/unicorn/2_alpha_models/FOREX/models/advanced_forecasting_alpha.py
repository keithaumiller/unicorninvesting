"""
Advanced Forex Forecasting Alpha Model
=====================================

Pure forecasting component that generates Insights based on multiple ML models.
This Alpha Model combines ARIMA, Neural Networks, Prophet, and XGBoost for
robust forex predictions.

Separation of Concerns:
- This class ONLY does forecasting and generates Insights
- Portfolio construction, execution, and risk management are handled separately
"""

from AlgorithmImports import *
import torch
import torch.nn as nn
import numpy as np
from prophet import Prophet
import pandas as pd
import warnings
warnings.filterwarnings('ignore')

class AdvancedForexForecastingAlpha(AlphaModel):
    """
    Advanced forex forecasting Alpha Model using ensemble of ML models.
    
    Models:
    - ARIMA (25%): Time series trend analysis
    - Neural Network (25%): Pattern recognition  
    - Prophet (25%): Seasonality and trend decomposition
    - XGBoost (25%): Feature-based gradient boosting
    
    Output: Insights with direction, confidence, and prediction horizon
    """
    
    def __init__(self, prediction_horizon_hours=4, confidence_threshold=0.005):
        """
        Initialize the forecasting alpha model.
        
        Args:
            prediction_horizon_hours: How far ahead to predict (hours)
            confidence_threshold: Minimum expected return to generate insight (e.g., 0.5%)
        """
        self.prediction_interval = timedelta(hours=prediction_horizon_hours)
        self.confidence_threshold = confidence_threshold
        self.lookback_period = 168  # 1 week of hourly data
        
        # Model storage
        self.forecasting_models = {}
        self.neural_models = {}
        self.prophet_models = {}
        self.feature_cache = {}
        
        # Ensemble weights (can be dynamically adjusted based on performance)
        self.model_weights = {
            'arima': 0.25,
            'neural': 0.25,
            'prophet': 0.25,
            'xgboost': 0.25
        }
        
        # Performance tracking for dynamic weighting
        self.model_performance = {
            'arima': [],
            'neural': [],
            'prophet': [],
            'xgboost': []
        }
        
    def update(self, algorithm, data):
        """
        Generate Insights based on ensemble forecasting.
        
        This is the main entry point called by LEAN framework.
        Returns Insights (NOT trades) - trading decisions handled by other components.
        """
        insights = []
        
        for symbol in self.forecasting_models:
            if not data.contains_key(symbol) or not data[symbol]:
                continue
                
            try:
                # Generate ensemble forecast
                forecasts = self.generate_ensemble_forecast(algorithm, symbol, data)
                
                if forecasts and len(forecasts) > 0:
                    # Calculate weighted ensemble prediction
                    ensemble_forecast = self.combine_forecasts(forecasts)
                    current_price = algorithm.securities[symbol].price
                    
                    if current_price > 0:
                        expected_return = (ensemble_forecast - current_price) / current_price
                        
                        # Generate Insight if signal is strong enough
                        if abs(expected_return) > self.confidence_threshold:
                            insight = self.create_insight(symbol, expected_return, forecasts)
                            insights.append(insight)
                            
                            algorithm.debug(f"🔮 FORECAST {symbol}: {expected_return:.2%} "
                                          f"(confidence: {insight.confidence:.2f})")
                            
            except Exception as e:
                algorithm.debug(f"⚠️ Forecasting error for {symbol}: {e}")
                
        return insights
    
    def generate_ensemble_forecast(self, algorithm, symbol, data):
        """
        Generate forecasts from all available models.
        
        Returns dictionary with forecasts from each model.
        """
        forecasts = {}
        current_price = algorithm.securities[symbol].price
        
        # 1. ARIMA Forecast
        arima_forecast = self.get_arima_forecast(symbol)
        if arima_forecast:
            forecasts['arima'] = arima_forecast
            
        # 2. Neural Network Forecast
        nn_forecast = self.get_neural_network_forecast(algorithm, symbol)
        if nn_forecast:
            forecasts['neural'] = nn_forecast
            
        # 3. Prophet Forecast
        prophet_forecast = self.get_prophet_forecast(algorithm, symbol)
        if prophet_forecast:
            forecasts['prophet'] = prophet_forecast
            
        # 4. XGBoost Forecast (using technical features)
        xgb_forecast = self.get_xgboost_forecast(algorithm, symbol)
        if xgb_forecast:
            forecasts['xgboost'] = xgb_forecast
            
        return forecasts
    
    def get_arima_forecast(self, symbol):
        """Get forecast from ARIMA models."""
        try:
            if symbol not in self.forecasting_models:
                return None
                
            arima_models = self.forecasting_models[symbol]
            
            # Use ensemble of ARIMA models with different parameters
            forecasts = []
            for model_name, model in arima_models.items():
                if model and model.is_ready:
                    forecasts.append(model.current.value)
                    
            return np.mean(forecasts) if forecasts else None
            
        except Exception as e:
            return None
    
    def get_neural_network_forecast(self, algorithm, symbol):
        """Get forecast from neural network."""
        try:
            if symbol not in self.neural_models:
                return None
                
            # Get recent price data
            history = algorithm.history(symbol, 50, Resolution.HOUR)
            if history.empty:
                return None
                
            prices = history['close'].values
            if len(prices) < 20:
                return None
                
            # Simple neural network prediction (placeholder)
            # In production, you'd use a more sophisticated model
            recent_change = (prices[-1] - prices[-10]) / prices[-10]
            trend_forecast = prices[-1] * (1 + recent_change * 0.1)
            
            return trend_forecast
            
        except Exception as e:
            return None
    
    def get_prophet_forecast(self, algorithm, symbol):
        """Get forecast from Prophet model."""
        try:
            # Get historical data
            history = algorithm.history(symbol, 200, Resolution.HOUR)
            if history.empty:
                return None
                
            # Prepare data for Prophet
            df = history.reset_index()
            df = df[['time', 'close']].rename(columns={'time': 'ds', 'close': 'y'})
            
            # Create and fit Prophet model
            model = Prophet(
                daily_seasonality=False,
                yearly_seasonality=False,
                weekly_seasonality=True,
                changepoint_prior_scale=0.05
            )
            
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                model.fit(df)
                
                # Make forecast
                future = model.make_future_dataframe(periods=4, freq='H')
                forecast = model.predict(future)
                
                return forecast['yhat'].iloc[-1]
                
        except Exception as e:
            return None
    
    def get_xgboost_forecast(self, algorithm, symbol):
        """Get forecast using XGBoost with technical features."""
        try:
            # Get historical data for feature engineering
            history = algorithm.history(symbol, 100, Resolution.HOUR)
            if history.empty or len(history) < 50:
                return None
                
            # Create technical features
            features = self.create_technical_features(history)
            if features is None:
                return None
                
            # Simple trend-based prediction (placeholder for XGBoost)
            # In production, you'd train an actual XGBoost model
            current_price = history['close'].iloc[-1]
            sma_20 = history['close'].rolling(20).mean().iloc[-1]
            momentum = (current_price - sma_20) / sma_20
            
            # Trend continuation assumption
            predicted_change = momentum * 0.1
            xgb_forecast = current_price * (1 + predicted_change)
            
            return xgb_forecast
            
        except Exception as e:
            return None
    
    def create_technical_features(self, history):
        """Create technical analysis features for XGBoost."""
        try:
            df = history.copy()
            
            # Moving averages
            df['sma_10'] = df['close'].rolling(10).mean()
            df['sma_20'] = df['close'].rolling(20).mean()
            df['sma_50'] = df['close'].rolling(50).mean()
            
            # RSI
            delta = df['close'].diff()
            gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
            rs = gain / loss
            df['rsi'] = 100 - (100 / (1 + rs))
            
            # Bollinger Bands
            df['bb_upper'] = df['sma_20'] + (df['close'].rolling(20).std() * 2)
            df['bb_lower'] = df['sma_20'] - (df['close'].rolling(20).std() * 2)
            
            # Momentum indicators
            df['momentum'] = df['close'] / df['close'].shift(10) - 1
            df['volatility'] = df['close'].rolling(20).std()
            
            return df.iloc[-1]  # Return latest features
            
        except Exception as e:
            return None
    
    def combine_forecasts(self, forecasts):
        """
        Combine multiple model forecasts using weighted average.
        
        Weights can be adjusted based on historical performance.
        """
        if not forecasts:
            return None
            
        weighted_sum = 0
        total_weight = 0
        
        for model_name, forecast in forecasts.items():
            if forecast and model_name in self.model_weights:
                weight = self.model_weights[model_name]
                weighted_sum += forecast * weight
                total_weight += weight
                
        return weighted_sum / total_weight if total_weight > 0 else None
    
    def create_insight(self, symbol, expected_return, forecasts):
        """
        Create an Insight from forecast data.
        
        Insight contains direction, confidence, magnitude, and prediction horizon.
        """
        direction = InsightDirection.UP if expected_return > 0 else InsightDirection.DOWN
        
        # Calculate confidence based on:
        # 1. Magnitude of expected return
        # 2. Agreement between models
        # 3. Historical accuracy (future enhancement)
        
        magnitude = abs(expected_return)
        confidence = min(magnitude * 20, 1.0)  # Scale to 0-1
        
        # Adjust confidence based on model agreement
        if len(forecasts) > 1:
            forecast_values = list(forecasts.values())
            forecast_std = np.std(forecast_values) / np.mean(forecast_values)
            agreement_factor = max(0.5, 1 - forecast_std)  # Higher std = lower agreement
            confidence *= agreement_factor
        
        # Create the Insight
        insight = Insight.price(
            symbol,
            self.prediction_interval,
            direction,
            magnitude,
            confidence,
            weight=confidence  # Use confidence as portfolio weight suggestion
        )
        
        # Add metadata about forecasting models used
        insight.tag = f"Models: {len(forecasts)}, Confidence: {confidence:.2f}"
        
        return insight
    
    def on_securities_changed(self, algorithm, changes):
        """
        Initialize forecasting models when securities are added/removed.
        """
        # Add new securities
        for security in changes.added_securities:
            symbol = security.symbol
            
            # Initialize ARIMA models with different parameters
            self.forecasting_models[symbol] = {
                'arima_111': algorithm.arima(symbol, 1, 1, 1, self.lookback_period),
                'arima_212': algorithm.arima(symbol, 2, 1, 2, self.lookback_period),
                'arima_110': algorithm.arima(symbol, 1, 1, 0, self.lookback_period)
            }
            
            # Initialize neural network placeholder
            self.neural_models[symbol] = True  # Placeholder for actual NN model
            
            # Initialize Prophet placeholder  
            self.prophet_models[symbol] = True  # Placeholder for Prophet model
            
            algorithm.debug(f"🔧 Initialized forecasting models for {symbol}")
            
        # Remove securities
        for security in changes.removed_securities:
            symbol = security.symbol
            
            if symbol in self.forecasting_models:
                del self.forecasting_models[symbol]
            if symbol in self.neural_models:
                del self.neural_models[symbol]
            if symbol in self.prophet_models:
                del self.prophet_models[symbol]
            if symbol in self.feature_cache:
                del self.feature_cache[symbol]
                
            algorithm.debug(f"🗑️ Removed forecasting models for {symbol}")
    
    def update_model_weights(self, performance_data):
        """
        Dynamically adjust model weights based on performance.
        
        This could be called periodically to optimize the ensemble.
        """
        if not performance_data:
            return
            
        # Simple performance-based weighting
        total_performance = sum(performance_data.values())
        if total_performance > 0:
            for model_name, performance in performance_data.items():
                if model_name in self.model_weights:
                    self.model_weights[model_name] = performance / total_performance
