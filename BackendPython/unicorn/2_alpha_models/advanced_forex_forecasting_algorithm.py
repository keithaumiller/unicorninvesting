"""
Advanced Forex Forecasting Algorithm
====================================

This algorithm demonstrates LEAN's built-in forecasting capabilities for forex trading.
It combines multiple forecasting approaches:

1. ARIMA models for time series forecasting
2. Neural networks for pattern recognition
3. Prophet for trend and seasonality analysis
4. Ensemble methods for robust predictions

Currency Pairs: EURUSD, USDJPY, USDCNH, ETHUSD
Forecasting Methods: ARIMA, Neural Networks, Prophet
Framework: LEAN Algorithm Framework with Insights
"""

from AlgorithmImports import *
import torch
import torch.nn as nn
import numpy as np
from prophet import Prophet
import pandas as pd

class AdvancedForexForecastingAlgorithm(QCAlgorithm):
    """
    Advanced forex forecasting algorithm using LEAN's built-in capabilities.
    Demonstrates multiple forecasting approaches integrated with the backtesting framework.
    """
    
    def initialize(self):
        # Set algorithm parameters
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(100000)
        
        # Add forex pairs - note: we need to use the correct LEAN symbols
        self.forex_symbols = {
            "EURUSD": self.add_forex("EURUSD", Resolution.HOUR).symbol,
            "USDJPY": self.add_forex("USDJPY", Resolution.HOUR).symbol,
            "USDCNH": self.add_forex("USDCNH", Resolution.HOUR).symbol,
            # Note: ETHUSD might need to be added as crypto
            # "ETHUSD": self.add_crypto("ETHUSD", Resolution.HOUR).symbol,
        }
        
        # Initialize forecasting models
        self.forecasting_models = {}
        self.predictions = {}
        self.lookback_period = 168  # 1 week of hourly data
        
        # Initialize ARIMA models for each symbol
        for name, symbol in self.forex_symbols.items():
            # Create ARIMA models with different configurations
            self.forecasting_models[f"{name}_ARIMA"] = {
                'arima_111': self.arima(symbol, 1, 1, 1, self.lookback_period),
                'arima_212': self.arima(symbol, 2, 1, 2, self.lookback_period),
                'arima_110': self.arima(symbol, 1, 1, 0, self.lookback_period)
            }
        
        # Neural network models will be created after we have some data
        self.neural_models = {}
        self.prophet_models = {}
        
        # Schedule forecasting updates
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=6)),
            self.update_forecasts
        )
        
        # Schedule trading decisions
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=1)),
            self.make_trading_decisions
        )
        
        # Risk management
        self.max_position_size = 0.2  # 20% per position
        self.stop_loss_pct = 0.02     # 2% stop loss
        self.take_profit_pct = 0.04   # 4% take profit
        
        # Performance tracking
        self.prediction_accuracy = {}
        self.trades_executed = 0
        
        self.debug("🦄 Advanced Forex Forecasting Algorithm Initialized")
        self.debug(f"📊 Symbols: {list(self.forex_symbols.keys())}")
        self.debug(f"📈 Forecasting Methods: ARIMA, Neural Networks, Prophet")
    
    def update_forecasts(self):
        """Update all forecasting models with latest data."""
        try:
            # Get historical data for all symbols
            for name, symbol in self.forex_symbols.items():
                # Get recent historical data
                history = self.history(symbol, self.lookback_period, Resolution.HOUR)
                
                if not history.empty and len(history) >= 50:  # Minimum data required
                    close_prices = history['close'].values
                    
                    # Update ARIMA forecasts
                    arima_forecasts = self.get_arima_forecasts(name)
                    
                    # Update neural network forecasts
                    nn_forecast = self.get_neural_network_forecast(name, close_prices)
                    
                    # Update Prophet forecasts (if we have enough data)
                    prophet_forecast = self.get_prophet_forecast(name, history)
                    
                    # Combine forecasts using ensemble method
                    ensemble_forecast = self.combine_forecasts(
                        arima_forecasts, nn_forecast, prophet_forecast
                    )
                    
                    # Store predictions
                    self.predictions[name] = {
                        'arima': arima_forecasts,
                        'neural_network': nn_forecast,
                        'prophet': prophet_forecast,
                        'ensemble': ensemble_forecast,
                        'timestamp': self.time,
                        'current_price': close_prices[-1]
                    }
                    
                    self.debug(f"📊 Updated forecasts for {name}: Ensemble={ensemble_forecast:.4f}")
        
        except Exception as e:
            self.error(f"❌ Error updating forecasts: {str(e)}")
    
    def get_arima_forecasts(self, symbol_name):
        """Get forecasts from ARIMA models."""
        forecasts = {}
        
        try:
            models = self.forecasting_models.get(f"{symbol_name}_ARIMA", {})
            
            for model_name, model in models.items():
                if model.is_ready:
                    # LEAN ARIMA models provide current value
                    forecasts[model_name] = model.current.value
                else:
                    forecasts[model_name] = None
            
            return forecasts
        
        except Exception as e:
            self.error(f"❌ Error in ARIMA forecasting for {symbol_name}: {str(e)}")
            return {}
    
    def get_neural_network_forecast(self, symbol_name, price_data):
        """Get forecast from neural network model."""
        try:
            # Create or update neural network model
            if symbol_name not in self.neural_models:
                self.neural_models[symbol_name] = self.create_neural_network()
            
            # Prepare data for neural network
            if len(price_data) >= 50:
                # Normalize data
                prices_normalized = (price_data - np.mean(price_data)) / np.std(price_data)
                
                # Create sequences for training
                sequence_length = 20
                if len(prices_normalized) >= sequence_length + 1:
                    X, y = self.create_sequences(prices_normalized, sequence_length)
                    
                    # Train the model (simple online learning)
                    model = self.neural_models[symbol_name]
                    prediction = self.train_and_predict_nn(model, X, y, price_data[-1])
                    
                    return prediction
            
            return None
        
        except Exception as e:
            self.error(f"❌ Error in neural network forecasting for {symbol_name}: {str(e)}")
            return None
    
    def create_neural_network(self):
        """Create a simple neural network for price prediction."""
        class ForexPredictor(nn.Module):
            def __init__(self, input_size=20, hidden_size=50, output_size=1):
                super(ForexPredictor, self).__init__()
                self.lstm = nn.LSTM(1, hidden_size, batch_first=True)
                self.fc = nn.Linear(hidden_size, output_size)
                
            def forward(self, x):
                lstm_out, _ = self.lstm(x)
                prediction = self.fc(lstm_out[:, -1, :])
                return prediction
        
        return ForexPredictor()
    
    def create_sequences(self, data, seq_length):
        """Create sequences for neural network training."""
        X, y = [], []
        for i in range(len(data) - seq_length):
            X.append(data[i:(i + seq_length)])
            y.append(data[i + seq_length])
        return np.array(X), np.array(y)
    
    def train_and_predict_nn(self, model, X, y, current_price):
        """Train neural network and make prediction."""
        try:
            # Convert to tensors
            X_tensor = torch.FloatTensor(X).unsqueeze(-1)
            y_tensor = torch.FloatTensor(y)
            
            # Simple training (in practice, you'd want more sophisticated training)
            criterion = nn.MSELoss()
            optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
            
            # Quick training loop (just a few epochs for online learning)
            model.train()
            for epoch in range(5):
                optimizer.zero_grad()
                output = model(X_tensor)
                loss = criterion(output.squeeze(), y_tensor)
                loss.backward()
                optimizer.step()
            
            # Make prediction
            model.eval()
            with torch.no_grad():
                last_sequence = X[-1:].reshape(1, -1, 1)
                last_sequence_tensor = torch.FloatTensor(last_sequence)
                prediction_normalized = model(last_sequence_tensor).item()
                
                # Denormalize prediction
                # This is a simplified denormalization - in practice you'd want to be more careful
                prediction = current_price * (1 + prediction_normalized * 0.01)
                
                return prediction
        
        except Exception as e:
            self.error(f"❌ Error in neural network training/prediction: {str(e)}")
            return None
    
    def get_prophet_forecast(self, symbol_name, history_df):
        """Get forecast from Prophet model."""
        try:
            # Prophet requires specific column names
            if len(history_df) >= 100:  # Prophet needs sufficient data
                # Prepare data for Prophet
                prophet_data = pd.DataFrame({
                    'ds': history_df.index,
                    'y': history_df['close'].values
                })
                
                # Create or update Prophet model
                model = Prophet(daily_seasonality=True, weekly_seasonality=True)
                model.fit(prophet_data)
                
                # Make future prediction (next hour)
                future = model.make_future_dataframe(periods=1, freq='H')
                forecast = model.predict(future)
                
                # Return the next period prediction
                return forecast['yhat'].iloc[-1]
            
            return None
        
        except Exception as e:
            self.error(f"❌ Error in Prophet forecasting for {symbol_name}: {str(e)}")
            return None
    
    def combine_forecasts(self, arima_forecasts, nn_forecast, prophet_forecast):
        """Combine multiple forecasts using ensemble method."""
        forecasts = []
        weights = []
        
        # Add ARIMA forecasts
        for model_name, forecast in arima_forecasts.items():
            if forecast is not None:
                forecasts.append(forecast)
                weights.append(0.3)  # 30% weight for ARIMA models
        
        # Add neural network forecast
        if nn_forecast is not None:
            forecasts.append(nn_forecast)
            weights.append(0.4)  # 40% weight for neural network
        
        # Add Prophet forecast
        if prophet_forecast is not None:
            forecasts.append(prophet_forecast)
            weights.append(0.3)  # 30% weight for Prophet
        
        if forecasts:
            # Weighted average
            weights = np.array(weights)
            weights = weights / weights.sum()  # Normalize weights
            ensemble_forecast = np.average(forecasts, weights=weights)
            return ensemble_forecast
        
        return None
    
    def make_trading_decisions(self):
        """Make trading decisions based on forecasts."""
        for name, symbol in self.forex_symbols.items():
            if name in self.predictions:
                prediction_data = self.predictions[name]
                ensemble_forecast = prediction_data.get('ensemble')
                current_price = prediction_data.get('current_price')
                
                if ensemble_forecast is not None and current_price is not None:
                    # Calculate expected return
                    expected_return = (ensemble_forecast - current_price) / current_price
                    
                    # Trading logic
                    if abs(expected_return) > 0.005:  # 0.5% threshold
                        
                        if expected_return > 0.01:  # Strong buy signal (>1% expected return)
                            if not self.portfolio[symbol].invested:
                                quantity = self.calculate_position_size(symbol)
                                self.market_order(symbol, quantity)
                                self.trades_executed += 1
                                self.debug(f"🟢 BUY {name}: Expected return {expected_return:.2%}")
                        
                        elif expected_return < -0.01:  # Strong sell signal (<-1% expected return)
                            if self.portfolio[symbol].invested and self.portfolio[symbol].is_long:
                                self.liquidate(symbol)
                                self.trades_executed += 1
                                self.debug(f"🔴 SELL {name}: Expected return {expected_return:.2%}")
    
    def calculate_position_size(self, symbol):
        """Calculate position size based on risk management rules."""
        available_cash = self.portfolio.cash
        max_investment = available_cash * self.max_position_size
        current_price = self.securities[symbol].price
        
        if current_price > 0:
            return int(max_investment / current_price)
        return 0
    
    def on_data(self, data):
        """Main data event handler."""
        # This is called for each data point
        # Main trading logic is handled in scheduled functions
        pass
    
    def on_end_of_algorithm(self):
        """Called at the end of the algorithm."""
        self.debug("🎯 Algorithm Summary:")
        self.debug(f"📊 Total trades executed: {self.trades_executed}")
        self.debug(f"💰 Final portfolio value: ${self.portfolio.total_portfolio_value:,.2f}")
        
        # Log final predictions
        for name, prediction_data in self.predictions.items():
            if prediction_data:
                self.debug(f"📈 Final forecast for {name}: {prediction_data.get('ensemble', 'N/A')}")

class ForexForecastingAlphaModel(AlphaModel):
    """
    Alpha model that generates Insights based on forex forecasting.
    This demonstrates how to integrate forecasting with LEAN's Algorithm Framework.
    """
    
    def __init__(self):
        self.symbol_data = {}
        self.prediction_interval = timedelta(hours=4)  # 4-hour prediction horizon
    
    def update(self, algorithm, data):
        """Generate insights based on forecasting models."""
        insights = []
        
        # This would integrate with the forecasting algorithm above
        # For brevity, showing the structure
        
        for symbol in self.symbol_data:
            # Get current forecast
            forecast = self.get_forecast(symbol, algorithm)
            
            if forecast:
                direction = InsightDirection.UP if forecast > 0 else InsightDirection.DOWN
                magnitude = abs(forecast)
                
                insight = Insight.price(
                    symbol, 
                    self.prediction_interval, 
                    direction, 
                    magnitude, 
                    None
                )
                insights.append(insight)
        
        return insights
    
    def get_forecast(self, symbol, algorithm):
        """Get forecast for a specific symbol."""
        # This would call the forecasting methods from the main algorithm
        # Return expected price change as percentage
        return 0.01  # Placeholder
    
    def on_securities_changed(self, algorithm, changes):
        """Handle universe changes."""
        for security in changes.added_securities:
            self.symbol_data[security.symbol] = {}
        
        for security in changes.removed_securities:
            if security.symbol in self.symbol_data:
                del self.symbol_data[security.symbol]
