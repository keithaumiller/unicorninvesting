"""
Pure Forecasting Example - LEAN Algorithm Framework
==================================================

This example demonstrates how to separate forecasting from trading strategies
using LEAN's Algorithm Framework architecture.

Forecasting: AlphaModel (generates Insights)
Trading: PortfolioConstructionModel + ExecutionModel
"""

from AlgorithmImports import *
import numpy as np
from prophet import Prophet
import pandas as pd

class PureForecastingExample(QCAlgorithm):
    """
    Example showing clean separation between forecasting and trading
    using LEAN's Algorithm Framework.
    """
    
    def initialize(self):
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        self.set_cash(100000)
        
        # Add forex symbols
        symbols = [Symbol.create("EURUSD", SecurityType.FOREX, Market.OANDA)]
        self.set_universe_selection(ManualUniverseSelectionModel(symbols))
        
        # PURE SEPARATION: Forecasting vs Trading
        self.set_alpha(MultiModelForecastingAlpha())           # FORECASTING ONLY
        self.set_portfolio_construction(EqualWeightingPortfolioConstructionModel())  # POSITION SIZING
        self.set_execution(ImmediateExecutionModel())         # TRADE EXECUTION
        self.set_risk_management(NullRiskManagementModel())   # RISK CONTROLS


class MultiModelForecastingAlpha(AlphaModel):
    """
    Pure forecasting AlphaModel - NO TRADING LOGIC
    Only generates forecasts and converts to Insights
    """
    
    def __init__(self):
        self.forecasting_models = {}
        self.prediction_interval = timedelta(hours=4)
        self.lookback_period = 100
        
    def update(self, algorithm, data):
        """
        Pure forecasting - only generates Insights based on predictions
        NO TRADING DECISIONS HERE
        """
        insights = []
        
        for symbol in self.forecasting_models:
            if not data.contains_key(symbol):
                continue
                
            # PURE FORECASTING - Multiple models
            forecasts = self.generate_forecasts(algorithm, symbol)
            
            if forecasts:
                # Ensemble forecast
                ensemble_forecast = np.mean(list(forecasts.values()))
                current_price = algorithm.securities[symbol].price
                
                # Convert forecast to expected return
                expected_return = (ensemble_forecast - current_price) / current_price
                
                # Generate Insight (NOT a trade)
                if abs(expected_return) > 0.005:  # 0.5% threshold
                    direction = InsightDirection.UP if expected_return > 0 else InsightDirection.DOWN
                    confidence = min(abs(expected_return) * 10, 1.0)  # Scale confidence
                    
                    insight = Insight.price(
                        symbol,
                        self.prediction_interval,
                        direction,
                        confidence,
                        None,
                        weight=confidence
                    )
                    insights.append(insight)
                    
                    algorithm.debug(f"🔮 FORECAST {symbol}: {expected_return:.2%} confidence={confidence:.2f}")
        
        return insights
    
    def generate_forecasts(self, algorithm, symbol):
        """
        Pure forecasting logic - multiple models
        Returns dictionary of forecasts from different models
        """
        forecasts = {}
        
        try:
            # 1. ARIMA Forecast
            if 'arima' in self.forecasting_models[symbol]:
                arima_model = self.forecasting_models[symbol]['arima']
                if arima_model.is_ready:
                    forecasts['arima'] = arima_model.current.value
            
            # 2. Prophet Forecast
            forecasts['prophet'] = self.get_prophet_forecast(algorithm, symbol)
            
            # 3. Simple Moving Average Forecast (as baseline)
            history = algorithm.history(symbol, 20, Resolution.HOUR)
            if not history.empty:
                sma_forecast = history['close'].mean()
                forecasts['sma'] = sma_forecast
                
        except Exception as e:
            algorithm.debug(f"⚠️ Forecasting error for {symbol}: {e}")
            
        return forecasts
    
    def get_prophet_forecast(self, algorithm, symbol):
        """
        Prophet-based forecasting
        """
        try:
            # Get historical data
            history = algorithm.history(symbol, 200, Resolution.HOUR)
            if history.empty:
                return None
                
            # Prepare data for Prophet
            df = history.reset_index()
            df = df[['time', 'close']].rename(columns={'time': 'ds', 'close': 'y'})
            
            # Simple Prophet model
            model = Prophet(daily_seasonality=False, yearly_seasonality=False)
            model.fit(df)
            
            # Make forecast
            future = model.make_future_dataframe(periods=4, freq='H')
            forecast = model.predict(future)
            
            # Return the last forecast value
            return forecast['yhat'].iloc[-1]
            
        except Exception as e:
            algorithm.debug(f"Prophet forecast error: {e}")
            return None
    
    def on_securities_changed(self, algorithm, changes):
        """
        Initialize forecasting models when securities are added
        """
        for security in changes.added_securities:
            symbol = security.symbol
            
            # Initialize ARIMA models
            self.forecasting_models[symbol] = {
                'arima': algorithm.arima(symbol, 1, 1, 1, self.lookback_period)
            }
            
            algorithm.debug(f"🔧 Initialized forecasting models for {symbol}")
            
        # Clean up removed securities
        for security in changes.removed_securities:
            if security.symbol in self.forecasting_models:
                del self.forecasting_models[security.symbol]


class ForecastOnlyResearchAlgorithm(QCAlgorithm):
    """
    Alternative: Pure research algorithm that ONLY does forecasting
    No trading at all - just evaluates forecast accuracy
    """
    
    def initialize(self):
        self.set_start_date(2023, 1, 1)
        self.set_end_date(2024, 1, 1)
        
        # Add symbols but don't trade
        self.eurusd = self.add_forex("EURUSD", Resolution.HOUR).symbol
        
        # Initialize forecasting models
        self.arima_model = self.arima(self.eurusd, 1, 1, 1, 100)
        
        # Track forecasting performance
        self.forecasts = []
        self.actuals = []
        
        # Schedule forecast evaluation
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=4)),
            self.evaluate_forecasts_only
        )
    
    def evaluate_forecasts_only(self):
        """
        PURE FORECASTING EVALUATION - No trading
        """
        if not self.arima_model.is_ready:
            return
            
        current_price = self.securities[self.eurusd].price
        forecast = self.arima_model.current.value
        
        # Store for accuracy analysis
        self.forecasts.append(forecast)
        self.actuals.append(current_price)
        
        # Calculate forecast error
        if len(self.forecasts) > 1:
            error = abs(self.forecasts[-1] - self.actuals[-1]) / self.actuals[-1]
            self.debug(f"📊 Forecast Error: {error:.2%}")
        
        self.debug(f"🔮 Current: {current_price:.5f}, Forecast: {forecast:.5f}")
    
    def on_end_of_algorithm(self):
        """
        Analyze forecasting performance at the end
        """
        if len(self.forecasts) > 10:
            errors = [abs(f - a) / a for f, a in zip(self.forecasts[1:], self.actuals[1:])]
            mae = np.mean(errors)
            self.debug(f"📈 Final MAE: {mae:.2%}")
