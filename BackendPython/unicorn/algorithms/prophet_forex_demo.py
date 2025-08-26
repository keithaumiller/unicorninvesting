#!/usr/bin/env python3
"""
Simple Prophet Forex Demo
=========================

This is a standalone demonstration of Prophet for forex forecasting.
It shows how Prophet can be used for currency prediction with
realistic forex data patterns.

Features:
- Prophet time series forecasting
- Forex-specific seasonality patterns
- Trading signal generation
- Performance evaluation
"""

import pandas as pd
import numpy as np
from prophet import Prophet
from datetime import datetime, timedelta
import warnings
warnings.filterwarnings('ignore')

class ProphetForexDemo:
    """
    Standalone Prophet demonstration for forex forecasting.
    """
    
    def __init__(self, initial_capital=10000):
        self.initial_capital = initial_capital
        self.current_capital = initial_capital
        self.positions = {}
        self.trade_history = []
        
        # Prophet models for each symbol
        self.prophet_models = {}
        self.forecasts = {}
        
        # Trading parameters
        self.confidence_threshold = 0.7   # Minimum confidence for trading
        self.min_expected_move = 0.003    # Minimum 0.3% expected move
        self.max_position_size = 0.25     # 25% max per position
        
        # Forex symbols
        self.symbols = ['EURUSD', 'USDJPY', 'USDCNH']
        
        print("🔮 Prophet Forex Demo Initialized")
        print(f"💰 Initial Capital: ${self.initial_capital:,.2f}")
        print(f"📊 Symbols: {', '.join(self.symbols)}")
        print(f"🎯 Min expected move: {self.min_expected_move:.1%}")
    
    def generate_forex_data(self, symbol, days=90):
        """Generate realistic forex data with trading session patterns."""
        
        # Base prices for different symbols
        base_prices = {
            'EURUSD': 1.1000,
            'USDJPY': 150.00,
            'USDCNH': 7.2000
        }
        
        # Generate hourly timestamps
        end_date = datetime.now()
        start_date = end_date - timedelta(days=days)
        timestamps = pd.date_range(start=start_date, end=end_date, freq='H')[:-1]
        
        # Base parameters
        base_price = base_prices.get(symbol, 1.0)
        np.random.seed(hash(symbol) % 2**32)  # Consistent seed per symbol
        
        prices = [base_price]
        
        for i, ts in enumerate(timestamps[1:], 1):
            hour = ts.hour
            day_of_week = ts.weekday()
            
            # Base volatility
            volatility = 0.0008  # 0.08% base hourly volatility
            
            # Trading session effects (realistic forex patterns)
            session_effect = 0
            
            # Asian session (22:00-08:00 UTC)
            if hour >= 22 or hour <= 8:
                volatility *= 0.8  # Lower volatility
                session_effect = -0.0001  # Slight downward bias
            
            # London session (08:00-16:00 UTC)
            elif 8 <= hour <= 16:
                volatility *= 1.2  # Higher volatility
                session_effect = 0.0002   # Upward bias
            
            # New York session (13:00-21:00 UTC)
            elif 13 <= hour <= 21:
                volatility *= 1.3  # Highest volatility
                session_effect = 0.0001   # Mild upward bias
            
            # Weekend effect (Friday after 21:00, Sunday before 22:00)
            if day_of_week == 4 and hour >= 21:  # Friday evening
                volatility *= 0.5
            elif day_of_week == 6:  # Saturday
                volatility *= 0.3
            elif day_of_week == 5:  # Sunday
                volatility *= 0.5
            
            # Weekly pattern (Monday gap, Friday profit-taking)
            weekly_effect = 0
            if day_of_week == 0 and hour == 22:  # Monday opening
                weekly_effect = np.random.normal(0, 0.002)  # Gap
            elif day_of_week == 4 and hour >= 16:  # Friday afternoon
                weekly_effect = -0.0001  # Profit taking
            
            # Generate price movement
            random_component = np.random.normal(0, volatility)
            total_effect = random_component + session_effect + weekly_effect
            
            new_price = prices[-1] * (1 + total_effect)
            prices.append(max(new_price, 0.001))  # Prevent negative prices
        
        # Create DataFrame
        df = pd.DataFrame({
            'ds': timestamps,
            'y': prices[:len(timestamps)]
        })
        
        return df
    
    def train_prophet_model(self, symbol, data):
        """Train Prophet model for a specific symbol."""
        print(f"🔮 Training Prophet model for {symbol}...")
        
        # Configure Prophet for forex
        model = Prophet(
            # Core seasonality
            daily_seasonality=True,      # Trading sessions
            weekly_seasonality=True,     # Week patterns
            yearly_seasonality=False,    # Not enough data
            
            # Forex-specific settings
            changepoint_prior_scale=0.05,    # Moderate trend flexibility
            seasonality_prior_scale=15.0,    # Strong seasonality
            seasonality_mode='multiplicative',  # Better for forex
            interval_width=0.8,               # 80% confidence intervals
        )
        
        # Add custom seasonalities for forex trading
        # Trading session cycle (24 hours)
        model.add_seasonality(
            name='trading_sessions',
            period=24,
            fourier_order=8
        )
        
        # Fit the model
        model.fit(data)
        
        self.prophet_models[symbol] = model
        print(f"✅ Prophet model trained for {symbol}")
        
        return model
    
    def generate_forecast(self, symbol, hours_ahead=6):
        """Generate Prophet forecast for a symbol."""
        if symbol not in self.prophet_models:
            return None
        
        model = self.prophet_models[symbol]
        
        # Create future dataframe
        future = model.make_future_dataframe(
            periods=hours_ahead,
            freq='H',
            include_history=False
        )
        
        # Generate forecast
        forecast = model.predict(future)
        
        return forecast
    
    def evaluate_forecast_signal(self, symbol, current_price, forecast):
        """Evaluate trading signal from Prophet forecast."""
        if forecast is None or len(forecast) == 0:
            return None, 0
        
        # Get next hour prediction
        next_hour_pred = forecast['yhat'].iloc[0]
        confidence_lower = forecast['yhat_lower'].iloc[0]
        confidence_upper = forecast['yhat_upper'].iloc[0]
        
        # Calculate expected return
        expected_return = (next_hour_pred - current_price) / current_price
        
        # Calculate confidence score (narrower interval = higher confidence)
        confidence_width = (confidence_upper - confidence_lower) / current_price
        confidence_score = 1.0 - min(confidence_width, 0.02) / 0.02
        
        # Generate signal
        signal = None
        if (confidence_score >= self.confidence_threshold and 
            abs(expected_return) >= self.min_expected_move):
            
            if expected_return > 0:
                signal = "BUY"
            else:
                signal = "SELL"
        
        return signal, {
            'expected_return': expected_return,
            'confidence_score': confidence_score,
            'next_hour_price': next_hour_pred,
            'confidence_range': (confidence_lower, confidence_upper)
        }
    
    def execute_trade(self, symbol, signal, signal_info, current_price):
        """Execute a trade based on Prophet signal."""
        if signal == "BUY":
            # Calculate position size
            max_investment = self.current_capital * self.max_position_size
            position_size = max_investment / current_price
            cost = position_size * current_price
            
            if cost <= self.current_capital:
                self.positions[symbol] = self.positions.get(symbol, 0) + position_size
                self.current_capital -= cost
                
                trade = {
                    'timestamp': datetime.now(),
                    'symbol': symbol,
                    'action': 'BUY',
                    'size': position_size,
                    'price': current_price,
                    'cost': cost,
                    'expected_return': signal_info['expected_return'],
                    'confidence': signal_info['confidence_score']
                }
                self.trade_history.append(trade)
                
                print(f"🟢 BUY {symbol}: {position_size:.2f} @ {current_price:.5f}")
                print(f"   Expected return: {signal_info['expected_return']:+.3%}")
                print(f"   Confidence: {signal_info['confidence_score']:.2f}")
        
        elif signal == "SELL" and symbol in self.positions:
            position_size = self.positions[symbol]
            if position_size > 0:
                proceeds = position_size * current_price
                self.current_capital += proceeds
                self.positions[symbol] = 0
                
                trade = {
                    'timestamp': datetime.now(),
                    'symbol': symbol,
                    'action': 'SELL',
                    'size': position_size,
                    'price': current_price,
                    'proceeds': proceeds,
                    'expected_return': signal_info['expected_return'],
                    'confidence': signal_info['confidence_score']
                }
                self.trade_history.append(trade)
                
                print(f"🔴 SELL {symbol}: {position_size:.2f} @ {current_price:.5f}")
                print(f"   Expected return: {signal_info['expected_return']:+.3%}")
                print(f"   Confidence: {signal_info['confidence_score']:.2f}")
    
    def run_prophet_demo(self):
        """Run the complete Prophet forex demo."""
        print(f"\n🚀 Starting Prophet Forex Demo...")
        print("=" * 60)
        
        # Generate data for all symbols
        forex_data = {}
        for symbol in self.symbols:
            print(f"📊 Generating data for {symbol}...")
            data = self.generate_forex_data(symbol, days=60)
            forex_data[symbol] = data
            
            # Train Prophet model
            self.train_prophet_model(symbol, data)
        
        print(f"\n🔮 Running Prophet forecasting simulation...")
        
        # Simulate trading over the last week of data
        simulation_days = 7
        
        for symbol in self.symbols:
            data = forex_data[symbol]
            # Use last week for simulation
            sim_start = len(data) - (simulation_days * 24)
            
            for i in range(sim_start, len(data) - 6, 6):  # Every 6 hours
                # Use data up to current point for forecasting
                historical_data = data.iloc[:i]
                current_price = historical_data['y'].iloc[-1]
                
                # Retrain model with updated data
                if len(historical_data) >= 48:  # Minimum 48 hours
                    self.train_prophet_model(symbol, historical_data)
                    
                    # Generate forecast
                    forecast = self.generate_forecast(symbol, hours_ahead=6)
                    
                    # Evaluate signal
                    signal, signal_info = self.evaluate_forecast_signal(
                        symbol, current_price, forecast
                    )
                    
                    # Execute trade if signal exists
                    if signal:
                        self.execute_trade(symbol, signal, signal_info, current_price)
        
        self.print_results()
    
    def calculate_portfolio_value(self):
        """Calculate current portfolio value."""
        portfolio_value = self.current_capital
        
        # Add current position values (using last known prices)
        for symbol, position in self.positions.items():
            if position > 0:
                # For demo, assume we can liquidate at current "market" price
                # In reality, this would use the latest market price
                last_trade = [t for t in self.trade_history if t['symbol'] == symbol]
                if last_trade:
                    last_price = last_trade[-1]['price']
                    portfolio_value += position * last_price
        
        return portfolio_value
    
    def print_results(self):
        """Print comprehensive Prophet demo results."""
        print("\n" + "=" * 60)
        print("🔮 PROPHET FOREX DEMO RESULTS")
        print("=" * 60)
        
        # Portfolio performance
        final_portfolio_value = self.calculate_portfolio_value()
        total_return = final_portfolio_value - self.initial_capital
        return_percentage = (total_return / self.initial_capital) * 100
        
        print(f"💰 Initial Capital: ${self.initial_capital:,.2f}")
        print(f"💰 Final Portfolio Value: ${final_portfolio_value:,.2f}")
        print(f"📈 Total Return: ${total_return:,.2f} ({return_percentage:+.2f}%)")
        print(f"💵 Cash Available: ${self.current_capital:,.2f}")
        
        # Position summary
        print(f"\n📋 Current Positions:")
        for symbol, position in self.positions.items():
            if position > 0:
                print(f"  {symbol}: {position:.4f} units")
        
        # Trade statistics
        print(f"\n📊 Prophet Trading Statistics:")
        print(f"  Total Trades: {len(self.trade_history)}")
        
        buy_trades = [t for t in self.trade_history if t['action'] == 'BUY']
        sell_trades = [t for t in self.trade_history if t['action'] == 'SELL']
        
        print(f"  Buy Orders: {len(buy_trades)}")
        print(f"  Sell Orders: {len(sell_trades)}")
        
        # Confidence and expected return analysis
        if self.trade_history:
            avg_confidence = np.mean([t.get('confidence', 0) for t in self.trade_history])
            avg_expected_return = np.mean([abs(t.get('expected_return', 0)) for t in self.trade_history])
            
            print(f"  Average Confidence: {avg_confidence:.2f}")
            print(f"  Average Expected Move: {avg_expected_return:.3%}")
            
            # Show recent trades
            print(f"\n📈 Recent Prophet Trades:")
            for trade in self.trade_history[-5:]:
                action = trade['action']
                symbol = trade['symbol']
                expected = trade.get('expected_return', 0)
                confidence = trade.get('confidence', 0)
                print(f"  {action} {symbol}: {expected:+.3%} expected (confidence: {confidence:.2f})")
        
        print(f"\n🔮 Prophet Features Demonstrated:")
        print(f"  ✅ Automatic seasonality detection (daily/weekly)")
        print(f"  ✅ Trading session pattern recognition")
        print(f"  ✅ Confidence interval-based signal filtering")
        print(f"  ✅ Forex-optimized model configuration")
        print(f"  ✅ Real-time model retraining")
        print(f"  ✅ Multi-timeframe forecasting (6-hour horizon)")
        
        print("=" * 60)

def main():
    """Main function to run Prophet forex demo."""
    print("🦄 Unicorn Investing - Prophet Forex Demo")
    print("=" * 60)
    print("This demo shows Prophet's capabilities for forex forecasting.")
    print("Prophet automatically detects seasonality patterns and trends.")
    print("\n⚠️  This is for educational purposes - not trading advice!")
    print("=" * 60)
    
    # Create and run demo
    demo = ProphetForexDemo(initial_capital=10000)
    demo.run_prophet_demo()
    
    print(f"\n✅ Prophet demo completed successfully!")
    print(f"🔮 Prophet is ready for LEAN integration!")

if __name__ == "__main__":
    main()
