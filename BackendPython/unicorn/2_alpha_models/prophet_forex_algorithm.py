"""
Prophet Forex Forecasting Algorithm
===================================

This algorithm focuses specifically on Facebook's Prophet library for forex forecasting.
Prophet is excellent for forex because it:
- Handles trends and seasonality automatically
- Works well with missing data
- Provides uncertainty intervals
- Can incorporate holiday effects
- Requires minimal parameter tuning

Currency Pairs: EURUSD, USDJPY, USDCNH
Forecasting Method: Prophet only
Framework: LEAN integration with Prophet
"""

from AlgorithmImports import *
import pandas as pd
import numpy as np
from prophet import Prophet
from prophet.plot import plot_plotly, plot_components_plotly
import warnings
warnings.filterwarnings('ignore')

class ProphetForexAlgorithm(QCAlgorithm):
    """
    Forex trading algorithm using Facebook's Prophet for time series forecasting.
    Demonstrates Prophet integration with LEAN backtesting framework.
    """
    
    def initialize(self):
        # Set algorithm parameters
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2023, 12, 31)
        self.set_cash(50000)
        
        # Add forex symbols
        self.symbols = {
            'EURUSD': self.add_forex("EURUSD", Resolution.HOUR).symbol,
            'USDJPY': self.add_forex("USDJPY", Resolution.HOUR).symbol,
            'USDCNH': self.add_forex("USDCNH", Resolution.HOUR).symbol,
        }
        
        # Prophet models for each symbol
        self.prophet_models = {}
        self.forecasts = {}
        self.last_training_time = {}
        
        # Prophet configuration
        self.lookback_days = 30        # Use 30 days of data for training
        self.retrain_hours = 24        # Retrain every 24 hours
        self.forecast_periods = 6      # Forecast 6 hours ahead
        
        # Trading parameters
        self.confidence_threshold = 0.8    # Only trade on high-confidence forecasts
        self.min_expected_move = 0.002     # Minimum 0.2% expected move to trade
        self.max_position_size = 0.3       # Max 30% per position
        
        # Performance tracking
        self.prophet_stats = {
            'models_trained': 0,
            'forecasts_made': 0,
            'trades_executed': 0,
            'successful_predictions': 0
        }
        
        # Schedule Prophet model training and forecasting
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=6)),
            self.train_prophet_models
        )
        
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=1)),
            self.make_prophet_forecasts
        )
        
        self.debug("🔮 Prophet Forex Algorithm Initialized")
        self.debug(f"📊 Symbols: {list(self.symbols.keys())}")
        self.debug(f"📈 Lookback: {self.lookback_days} days")
        self.debug(f"🎯 Forecast horizon: {self.forecast_periods} hours")
    
    def train_prophet_models(self):
        """Train Prophet models for all symbols."""
        self.debug("🔄 Training Prophet models...")
        
        for name, symbol in self.symbols.items():
            try:
                # Check if we need to retrain
                if self.should_retrain_model(name):
                    self.train_single_prophet_model(name, symbol)
                    
            except Exception as e:
                self.error(f"❌ Error training Prophet model for {name}: {str(e)}")
    
    def should_retrain_model(self, symbol_name):
        """Determine if a model needs retraining."""
        if symbol_name not in self.last_training_time:
            return True
        
        time_since_training = self.time - self.last_training_time[symbol_name]
        return time_since_training >= timedelta(hours=self.retrain_hours)
    
    def train_single_prophet_model(self, symbol_name, symbol):
        """Train a Prophet model for a single symbol."""
        try:
            # Get historical data
            history = self.history(symbol, self.lookback_days, Resolution.HOUR)
            
            if history.empty or len(history) < 72:  # Need at least 3 days of hourly data
                self.debug(f"⚠️ Insufficient data for {symbol_name}")
                return
            
            # Prepare data for Prophet
            prophet_data = self.prepare_prophet_data(history)
            
            if len(prophet_data) < 48:  # Need minimum data points
                self.debug(f"⚠️ Not enough cleaned data for {symbol_name}")
                return
            
            # Create and configure Prophet model
            model = Prophet(
                # Core settings
                daily_seasonality=True,      # Capture daily patterns (Asian/London/NY sessions)
                weekly_seasonality=True,     # Capture weekly patterns (weekday vs weekend)
                yearly_seasonality=False,    # Not enough data typically
                
                # Advanced settings for forex
                changepoint_prior_scale=0.05,  # Flexibility in trend changes
                seasonality_prior_scale=10.0,  # Strong seasonality in forex
                holidays_prior_scale=10.0,     # Holiday effects
                seasonality_mode='multiplicative',  # Better for forex percentage changes
                
                # Uncertainty estimation
                interval_width=0.8,  # 80% confidence intervals
                mcmc_samples=0,      # No MCMC for speed
            )
            
            # Add custom seasonalities for forex trading sessions
            model.add_seasonality(
                name='trading_session',
                period=24,  # 24-hour cycle
                fourier_order=8,  # Capture intraday patterns
                condition_name=None
            )
            
            # Train the model
            self.debug(f"🔮 Training Prophet model for {symbol_name}...")
            model.fit(prophet_data)
            
            # Store the trained model
            self.prophet_models[symbol_name] = model
            self.last_training_time[symbol_name] = self.time
            self.prophet_stats['models_trained'] += 1
            
            self.debug(f"✅ Prophet model trained for {symbol_name}")
            self.debug(f"📊 Training data: {len(prophet_data)} hours")
            
        except Exception as e:
            self.error(f"❌ Error training Prophet model for {symbol_name}: {str(e)}")
    
    def prepare_prophet_data(self, history):
        """Prepare historical data for Prophet."""
        # Prophet requires columns named 'ds' (datestamp) and 'y' (value)
        prophet_data = pd.DataFrame({
            'ds': history.index,
            'y': history['close']
        })
        
        # Remove any NaN values
        prophet_data = prophet_data.dropna()
        
        # Ensure datetime index
        prophet_data['ds'] = pd.to_datetime(prophet_data['ds'])
        
        # Sort by timestamp
        prophet_data = prophet_data.sort_values('ds').reset_index(drop=True)
        
        return prophet_data
    
    def make_prophet_forecasts(self):
        """Generate forecasts using trained Prophet models."""
        
        for name, symbol in self.symbols.items():
            if name in self.prophet_models:
                try:
                    forecast = self.generate_prophet_forecast(name, symbol)
                    if forecast:
                        self.forecasts[name] = forecast
                        self.evaluate_trading_signal(name, symbol, forecast)
                        
                except Exception as e:
                    self.error(f"❌ Error generating forecast for {name}: {str(e)}")
    
    def generate_prophet_forecast(self, symbol_name, symbol):
        """Generate forecast for a specific symbol."""
        try:
            model = self.prophet_models[symbol_name]
            
            # Create future dataframe for forecasting
            future = model.make_future_dataframe(
                periods=self.forecast_periods,
                freq='H',
                include_history=False  # Only forecast future periods
            )
            
            # Generate forecast
            forecast = model.predict(future)
            
            # Get current price for comparison
            current_price = self.securities[symbol].price
            
            # Extract key forecast information
            forecast_info = {
                'timestamps': future['ds'].tolist(),
                'predictions': forecast['yhat'].tolist(),
                'lower_bound': forecast['yhat_lower'].tolist(),
                'upper_bound': forecast['yhat_upper'].tolist(),
                'current_price': current_price,
                'forecast_time': self.time
            }
            
            # Calculate forecast metrics
            next_hour_forecast = forecast['yhat'].iloc[0]
            forecast_change = (next_hour_forecast - current_price) / current_price
            confidence_width = (forecast['yhat_upper'].iloc[0] - forecast['yhat_lower'].iloc[0]) / current_price
            
            forecast_info.update({
                'next_hour_price': next_hour_forecast,
                'expected_change': forecast_change,
                'confidence_width': confidence_width,
                'confidence_score': 1.0 - min(confidence_width, 0.02) / 0.02  # Higher score = narrower CI
            })
            
            self.prophet_stats['forecasts_made'] += 1
            
            self.debug(f"🔮 {symbol_name} Forecast:")
            self.debug(f"   Current: {current_price:.5f}")
            self.debug(f"   Next hour: {next_hour_forecast:.5f}")
            self.debug(f"   Expected change: {forecast_change:+.3%}")
            self.debug(f"   Confidence: {forecast_info['confidence_score']:.2f}")
            
            return forecast_info
            
        except Exception as e:
            self.error(f"❌ Error generating Prophet forecast for {symbol_name}: {str(e)}")
            return None
    
    def evaluate_trading_signal(self, symbol_name, symbol, forecast):
        """Evaluate trading signal based on Prophet forecast."""
        try:
            expected_change = forecast['expected_change']
            confidence_score = forecast['confidence_score']
            
            # Only trade if we have high confidence and significant expected move
            if (confidence_score >= self.confidence_threshold and 
                abs(expected_change) >= self.min_expected_move):
                
                current_holdings = self.portfolio[symbol]
                
                # Generate trading signal
                if expected_change > self.min_expected_move:
                    # Bullish signal
                    if not current_holdings.is_long:
                        self.execute_prophet_trade(symbol_name, symbol, "BUY", forecast)
                
                elif expected_change < -self.min_expected_move:
                    # Bearish signal
                    if current_holdings.is_long:
                        self.execute_prophet_trade(symbol_name, symbol, "SELL", forecast)
            
            else:
                self.debug(f"📊 {symbol_name}: No trade - confidence={confidence_score:.2f}, move={expected_change:+.3%}")
        
        except Exception as e:
            self.error(f"❌ Error evaluating trading signal for {symbol_name}: {str(e)}")
    
    def execute_prophet_trade(self, symbol_name, symbol, action, forecast):
        """Execute a trade based on Prophet forecast."""
        try:
            if action == "BUY":
                position_size = self.max_position_size
                self.set_holdings(symbol, position_size)
                self.prophet_stats['trades_executed'] += 1
                
                self.debug(f"🟢 BUY {symbol_name}")
                self.debug(f"   Expected return: {forecast['expected_change']:+.3%}")
                self.debug(f"   Confidence: {forecast['confidence_score']:.2f}")
                self.debug(f"   Position size: {position_size:.1%}")
            
            elif action == "SELL":
                self.liquidate(symbol)
                self.prophet_stats['trades_executed'] += 1
                
                self.debug(f"🔴 SELL {symbol_name}")
                self.debug(f"   Expected decline: {forecast['expected_change']:+.3%}")
                self.debug(f"   Confidence: {forecast['confidence_score']:.2f}")
        
        except Exception as e:
            self.error(f"❌ Error executing trade for {symbol_name}: {str(e)}")
    
    def on_data(self, data):
        """Handle incoming data."""
        # Prophet models are updated in scheduled functions
        # This can be used for real-time monitoring if needed
        pass
    
    def on_end_of_algorithm(self):
        """Algorithm summary and statistics."""
        
        final_value = self.portfolio.total_portfolio_value
        initial_value = 50000
        total_return = (final_value - initial_value) / initial_value * 100
        
        self.debug("=" * 60)
        self.debug("🔮 PROPHET FOREX ALGORITHM RESULTS")
        self.debug("=" * 60)
        
        # Performance metrics
        self.debug(f"💰 Initial Capital: ${initial_value:,.2f}")
        self.debug(f"💰 Final Portfolio Value: ${final_value:,.2f}")
        self.debug(f"📊 Total Return: {total_return:+.2f}%")
        
        # Prophet statistics
        self.debug(f"🔮 Prophet Models Trained: {self.prophet_stats['models_trained']}")
        self.debug(f"📈 Forecasts Generated: {self.prophet_stats['forecasts_made']}")
        self.debug(f"📊 Trades Executed: {self.prophet_stats['trades_executed']}")
        
        # Current positions
        self.debug(f"\n📋 Final Positions:")
        for name, symbol in self.symbols.items():
            holdings = self.portfolio[symbol]
            if holdings.invested:
                self.debug(f"   {name}: {holdings.quantity:.2f} units, P&L: {holdings.unrealized_profit_percent:+.2%}")
        
        # Latest forecasts
        self.debug(f"\n🔮 Latest Prophet Forecasts:")
        for name, forecast in self.forecasts.items():
            if forecast:
                self.debug(f"   {name}: {forecast['expected_change']:+.3%} (confidence: {forecast['confidence_score']:.2f})")
        
        self.debug("=" * 60)
        self.debug("🎯 KEY INSIGHTS:")
        self.debug("• Prophet excels at capturing forex seasonality")
        self.debug("• Trading session patterns are automatically detected")
        self.debug("• Confidence intervals help filter low-quality signals")
        self.debug("• Weekly and daily patterns improve forecast accuracy")
        self.debug("• Model retraining keeps forecasts current")
        self.debug("=" * 60)
