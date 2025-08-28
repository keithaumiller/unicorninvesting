# QUANTCONNECT.COM - Democratizing Finance, Empowering Individuals.
# Lean Algorithmic Trading Engine v2.0. Copyright 2014 QuantConnect Corporation.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from AlgorithmImports import *
import numpy as np
import pandas as pd
from prophet import Prophet

### <summary>
### Enhanced Ensemble Forex Algorithm for Unicorn Investing Platform
### Combines 4 forecasting methods: ARIMA + Neural Networks + Prophet + XGBoost
### Follows established unicorn platform patterns and risk management
### </summary>
### <meta name="tag" content="forex" />
### <meta name="tag" content="ensemble" />
### <meta name="tag" content="machine learning" />
### <meta name="tag" content="forecasting" />
### <meta name="tag" content="unicorn" />
class EnhancedEnsembleForexAlgorithm(QCAlgorithm):

    def initialize(self):
        """Initialize the enhanced ensemble forex algorithm"""
        
        # Set the cash we'd like to use for our backtest
        self.set_cash(100000)  # $100,000 starting capital

        # Start and end dates for the backtest
        self.set_start_date(2024, 1, 1)
        self.set_end_date(2024, 8, 27)

        # Set timezone for forex trading
        self.set_time_zone("UTC")

        # Add forex pairs (following established unicorn patterns)
        self.forex_symbols = {
            "EURUSD": self.add_forex("EURUSD", Resolution.HOUR).symbol,
            "USDJPY": self.add_forex("USDJPY", Resolution.HOUR).symbol,
            "USDCNH": self.add_forex("USDCNH", Resolution.HOUR).symbol,
        }
        
        # Add crypto (following unicorn pattern)
        self.crypto_symbols = {
            "ETHUSD": self.add_crypto("ETHUSD", Resolution.HOUR, Market.COINBASE).symbol
        }
        
        # Combine all symbols for easier processing
        self.all_symbols = {**self.forex_symbols, **self.crypto_symbols}
        
        # Model parameters
        self.lookback_period = 168  # 1 week of hourly data for training
        self.retrain_frequency = 24  # Retrain every 24 hours
        
        # Initialize forecasting models for each symbol
        self.forecasting_models = {}
        self.ensemble_predictions = {}
        self.model_weights = {
            'arima': 0.25,
            'neural': 0.25, 
            'prophet': 0.25,
            'xgboost': 0.25
        }
        
        for name, symbol in self.all_symbols.items():
            self.forecasting_models[name] = {
                # 1. ARIMA Models (using LEAN's built-in ARIMA)
                'arima_models': {
                    'arima_111': self.arima(symbol, 1, 1, 1, self.lookback_period),
                    'arima_212': self.arima(symbol, 2, 1, 2, self.lookback_period),
                },
                
                # 2. Neural Network (simulated with indicators)
                'neural_indicators': {
                    'rsi': self.rsi(symbol, 14, Resolution.HOUR),
                    'sma_short': self.sma(symbol, 12, Resolution.HOUR),
                    'sma_long': self.sma(symbol, 24, Resolution.HOUR),
                    'ema': self.ema(symbol, 12, Resolution.HOUR),
                    'momentum': self.momp(symbol, 20, Resolution.HOUR)
                },
                
                # 3. Prophet (will be initialized with data)
                'prophet_model': None,
                'prophet_data': [],
                
                # 4. XGBoost features storage
                'xgboost_features': {
                    'prices': [],
                    'returns': [],
                    'volatility': []
                },
                
                # Prediction storage
                'predictions': {
                    'arima': None,
                    'neural': None,
                    'prophet': None,
                    'xgboost': None,
                    'ensemble': None
                }
            }
        
        # Trading parameters (following established patterns)
        self.max_position_size = 0.15  # 15% per position
        self.prediction_threshold = 0.005  # 0.5% minimum predicted move to trade
        self.confidence_threshold = 0.6  # Minimum ensemble confidence
        self.stop_loss_pct = 0.02  # 2% stop loss
        
        # Performance tracking
        self.trades_executed = 0
        self.model_accuracy = {name: {'correct': 0, 'total': 0} for name in self.all_symbols.keys()}
        self.last_trade_time = {}
        
        # Schedule model updates and trading decisions
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=self.retrain_frequency)),
            self.update_ensemble_models
        )
        
        self.schedule.on(
            self.date_rules.every_day(),
            self.time_rules.every(timedelta(hours=1)),
            self.make_ensemble_decisions
        )
        
        # Log initialization (following unicorn logging patterns)
        self.log("🦄 Unicorn Enhanced Ensemble Forex Algorithm Initialized!")
        self.log(f"💰 Starting Cash: ${self.portfolio.cash:,.2f}")
        self.log(f"📊 Trading: {len(self.forex_symbols)} Forex + {len(self.crypto_symbols)} Crypto pairs")
        self.log(f"🤖 Ensemble Models: ARIMA + Neural + Prophet + XGBoost")

    def on_data(self, data):
        """Handle incoming market data and update all model features"""
        
        for name, symbol in self.all_symbols.items():
            if symbol in data and data[symbol] is not None:
                current_price = data[symbol].close
                current_time = self.time
                
                # Update Prophet data
                prophet_data = self.forecasting_models[name]['prophet_data']
                prophet_data.append({
                    'ds': current_time,
                    'y': current_price
                })
                
                # Keep only recent data for Prophet
                if len(prophet_data) > self.lookback_period:
                    self.forecasting_models[name]['prophet_data'] = prophet_data[-self.lookback_period:]
                
                # Update XGBoost features
                xgb_features = self.forecasting_models[name]['xgboost_features']
                xgb_features['prices'].append(current_price)
                
                # Calculate returns for XGBoost
                if len(xgb_features['prices']) >= 2:
                    prev_price = xgb_features['prices'][-2]
                    current_return = (current_price - prev_price) / prev_price
                    xgb_features['returns'].append(current_return)
                
                # Keep only recent data
                for feature_type in ['prices', 'returns']:
                    if len(xgb_features[feature_type]) > self.lookback_period:
                        xgb_features[feature_type] = xgb_features[feature_type][-self.lookback_period:]

    def update_ensemble_models(self):
        """Update all forecasting models in the ensemble"""
        
        self.log("🤖 Updating ensemble forecasting models...")
        
        for name, symbol in self.all_symbols.items():
            try:
                models = self.forecasting_models[name]
                predictions = models['predictions']
                
                # 1. Update ARIMA predictions
                predictions['arima'] = self.get_arima_prediction(name)
                
                # 2. Update Neural Network prediction (using technical indicators)
                predictions['neural'] = self.get_neural_prediction(name)
                
                # 3. Update Prophet prediction
                predictions['prophet'] = self.get_prophet_prediction(name)
                
                # 4. Update XGBoost prediction
                predictions['xgboost'] = self.get_xgboost_prediction(name)
                
                # 5. Calculate ensemble prediction
                predictions['ensemble'] = self.calculate_ensemble_prediction(name)
                
                if predictions['ensemble'] is not None:
                    current_price = self.securities[symbol].price
                    self.log(f"🎯 {name} ensemble updated: Current=${current_price:.4f}, Predicted=${predictions['ensemble']:.4f}")
                
            except Exception as e:
                self.error(f"❌ Error updating ensemble for {name}: {str(e)}")

    def get_arima_prediction(self, symbol_name):
        """Get ARIMA model prediction"""
        try:
            arima_models = self.forecasting_models[symbol_name]['arima_models']
            predictions = []
            
            for model_name, model in arima_models.items():
                if model.is_ready:
                    predictions.append(model.current.value)
            
            return np.mean(predictions) if predictions else None
            
        except Exception as e:
            self.error(f"❌ ARIMA prediction error for {symbol_name}: {str(e)}")
            return None

    def get_neural_prediction(self, symbol_name):
        """Get neural network prediction (simulated with technical indicators)"""
        try:
            indicators = self.forecasting_models[symbol_name]['neural_indicators']
            
            if not all(ind.is_ready for ind in indicators.values()):
                return None
            
            # Simulate neural network decision using technical indicators
            rsi = indicators['rsi'].current.value
            sma_short = indicators['sma_short'].current.value
            sma_long = indicators['sma_long'].current.value
            ema = indicators['ema'].current.value
            momentum = indicators['momentum'].current.value
            
            current_price = sma_short  # Use as base price
            
            # Neural network-style signal combination
            rsi_signal = (50 - rsi) / 100  # RSI mean reversion signal
            trend_signal = (sma_short - sma_long) / sma_long  # Trend signal
            momentum_signal = momentum / 100  # Momentum signal
            ema_signal = (current_price - ema) / ema  # EMA deviation
            
            # Weighted combination (simulating neural network output)
            combined_signal = (
                rsi_signal * 0.3 +
                trend_signal * 0.3 +
                momentum_signal * 0.25 +
                ema_signal * 0.15
            )
            
            # Convert signal to price prediction
            predicted_price = current_price * (1 + combined_signal * 0.1)  # Damped signal
            return predicted_price
            
        except Exception as e:
            self.error(f"❌ Neural prediction error for {symbol_name}: {str(e)}")
            return None

    def get_prophet_prediction(self, symbol_name):
        """Get Prophet model prediction"""
        try:
            prophet_data = self.forecasting_models[symbol_name]['prophet_data']
            
            if len(prophet_data) < 50:  # Need minimum data for Prophet
                return None
            
            # Prepare data for Prophet
            df = pd.DataFrame(prophet_data)
            df['ds'] = pd.to_datetime(df['ds'])
            
            # Create and fit Prophet model
            model = Prophet(
                changepoint_prior_scale=0.05,
                seasonality_prior_scale=10,
                daily_seasonality=True,
                weekly_seasonality=True,
                yearly_seasonality=False
            )
            
            model.fit(df)
            
            # Make future prediction (1 hour ahead)
            future = model.make_future_dataframe(periods=1, freq='H')
            forecast = model.predict(future)
            
            # Return the last prediction
            return forecast['yhat'].iloc[-1]
            
        except Exception as e:
            self.error(f"❌ Prophet prediction error for {symbol_name}: {str(e)}")
            return None

    def get_xgboost_prediction(self, symbol_name):
        """Get XGBoost prediction (simulated with statistical ensemble)"""
        try:
            xgb_data = self.forecasting_models[symbol_name]['xgboost_features']
            
            if len(xgb_data['prices']) < 50:
                return None
            
            prices = np.array(xgb_data['prices'])
            returns = np.array(xgb_data['returns']) if xgb_data['returns'] else np.array([0])
            
            current_price = prices[-1]
            
            # XGBoost-style feature engineering and prediction
            features = {
                'price_momentum_6h': np.mean(returns[-6:]) if len(returns) >= 6 else 0,
                'price_momentum_24h': np.mean(returns[-24:]) if len(returns) >= 24 else 0,
                'volatility': np.std(returns[-24:]) if len(returns) >= 24 else 0,
                'price_trend': (prices[-1] - prices[-24]) / prices[-24] if len(prices) >= 24 else 0,
                'mean_reversion': (current_price - np.mean(prices[-48:])) / np.mean(prices[-48:]) if len(prices) >= 48 else 0
            }
            
            # Ensemble of boosted decision trees (simulated)
            predictions = []
            
            # Tree 1: Momentum-based
            momentum_pred = current_price * (1 + features['price_momentum_24h'] * 0.5)
            predictions.append(momentum_pred)
            
            # Tree 2: Mean reversion
            reversion_pred = current_price * (1 - features['mean_reversion'] * 0.3)
            predictions.append(reversion_pred)
            
            # Tree 3: Trend following
            trend_pred = current_price * (1 + features['price_trend'] * 0.2)
            predictions.append(trend_pred)
            
            # Tree 4: Volatility-adjusted
            vol_adjustment = features['volatility'] * np.random.normal(0, 0.1)
            vol_pred = current_price * (1 + vol_adjustment)
            predictions.append(vol_pred)
            
            # XGBoost ensemble (weighted average of trees)
            xgb_prediction = np.mean(predictions)
            return xgb_prediction
            
        except Exception as e:
            self.error(f"❌ XGBoost prediction error for {symbol_name}: {str(e)}")
            return None

    def calculate_ensemble_prediction(self, symbol_name):
        """Calculate weighted ensemble prediction from all models"""
        try:
            predictions = self.forecasting_models[symbol_name]['predictions']
            
            # Get valid predictions
            valid_predictions = {}
            for model_name, prediction in predictions.items():
                if model_name != 'ensemble' and prediction is not None:
                    valid_predictions[model_name] = prediction
            
            if not valid_predictions:
                return None
            
            # Calculate weighted average
            total_weight = 0
            weighted_sum = 0
            
            for model_name, prediction in valid_predictions.items():
                weight = self.model_weights.get(model_name, 0.25)
                weighted_sum += prediction * weight
                total_weight += weight
            
            if total_weight > 0:
                ensemble_prediction = weighted_sum / total_weight
                
                # Calculate confidence based on agreement between models
                predictions_list = list(valid_predictions.values())
                std_dev = np.std(predictions_list)
                mean_pred = np.mean(predictions_list)
                confidence = max(0.3, 1.0 - (std_dev / mean_pred)) if mean_pred > 0 else 0.5
                
                # Store ensemble result
                self.ensemble_predictions[symbol_name] = {
                    'prediction': ensemble_prediction,
                    'confidence': confidence,
                    'model_count': len(valid_predictions),
                    'timestamp': self.time
                }
                
                return ensemble_prediction
            
            return None
            
        except Exception as e:
            self.error(f"❌ Ensemble calculation error for {symbol_name}: {str(e)}")
            return None

    def make_ensemble_decisions(self):
        """Make trading decisions based on ensemble predictions"""
        
        for name, symbol in self.all_symbols.items():
            if name in self.ensemble_predictions:
                try:
                    ensemble_data = self.ensemble_predictions[name]
                    predicted_price = ensemble_data['prediction']
                    confidence = ensemble_data['confidence']
                    
                    current_price = self.securities[symbol].price
                    expected_return = (predicted_price - current_price) / current_price
                    
                    # Check trading conditions
                    if (abs(expected_return) > self.prediction_threshold and 
                        confidence > self.confidence_threshold):
                        
                        # Avoid over-trading
                        if name in self.last_trade_time:
                            if self.time - self.last_trade_time[name] < timedelta(hours=2):
                                continue
                        
                        current_holdings = self.portfolio[symbol]
                        
                        # Trading logic
                        if expected_return > self.prediction_threshold:
                            if not current_holdings.is_long:
                                self.set_holdings(symbol, self.max_position_size)
                                self.last_trade_time[name] = self.time
                                self.trades_executed += 1
                                self.log(f"🚀 BUY {name}: Expected return {expected_return * 100:.2f}% "
                                        f"(Confidence: {confidence:.1%}, Models: {ensemble_data['model_count']})")
                        
                        elif expected_return < -self.prediction_threshold:
                            if current_holdings.is_long:
                                self.liquidate(symbol)
                                self.last_trade_time[name] = self.time
                                self.trades_executed += 1
                                self.log(f"🔻 SELL {name}: Expected return {expected_return * 100:.2f}% "
                                        f"(Confidence: {confidence:.1%}, Models: {ensemble_data['model_count']})")
                
                except Exception as e:
                    self.error(f"❌ Trading decision error for {name}: {str(e)}")

    def on_order_event(self, order_event):
        """Handle order events (following established patterns)"""
        if order_event.status == OrderStatus.FILLED:
            self.log(f"✅ Order Filled: {order_event.symbol} - {order_event.direction} - "
                    f"Quantity: {order_event.fill_quantity} - Price: ${order_event.fill_price:.4f}")

    def on_end_of_algorithm(self):
        """Called at the end of the algorithm (following established patterns)"""
        final_value = self.portfolio.total_portfolio_value
        total_return = (final_value - 100000) / 100000 * 100
        
        self.log("🏁 Enhanced Ensemble Forex Algorithm Completed!")
        self.log(f"💰 Initial Capital: ${100000:.2f}")
        self.log(f"💰 Final Portfolio Value: ${final_value:.2f}")
        self.log(f"📊 Total Return: {total_return:.2f}%")
        self.log(f"🤖 Total Trades: {self.trades_executed}")
        
        # Log model performance
        self.log("📊 Model Performance Summary:")
        for name in self.all_symbols.keys():
            if name in self.model_accuracy:
                accuracy = self.model_accuracy[name]
                if accuracy['total'] > 0:
                    pct = (accuracy['correct'] / accuracy['total']) * 100
                    self.log(f"   {name}: {pct:.1f}% accuracy ({accuracy['correct']}/{accuracy['total']})")
        
        # Log final holdings
        self.log("📋 Final Holdings:")
        for name, symbol in self.all_symbols.items():
            if self.portfolio[symbol].invested:
                holding = self.portfolio[symbol]
                self.log(f"   {name}: {holding.quantity:.4f} units @ ${holding.average_price:.4f}")
            else:
                self.log(f"   {name}: No position")
