# 🏗️ ETH LEAN Framework Integration Guide
## Implementing ETH Trading System in QuantConnect LEAN

---

## 🎯 **INTEGRATION OVERVIEW**

This guide shows how to implement our three ETH use cases within the existing LEAN Algorithm Framework located at `/workspaces/unicorninvesting/BackendPython/Lean/`.

### **LEAN Architecture Mapping**
```
ETH Use Cases → LEAN Framework Components
├── Forecasting Model Development → Research + Algorithm.Python
├── Live Trading → Algorithm.Python + Engine + Data
└── Risk Analysis → Algorithm.Python + Portfolio + Risk Management
```

---

## 📊 **USE CASE 1: FORECASTING MODEL DEVELOPMENT**
### **LEAN Research Environment Implementation**

#### **1. Research Notebook Setup**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Research/ETH_Forecasting_Research.ipynb
# This leverages LEAN's research environment for model development

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/Lean')

from Research import QuantBookManager
from Data import Market
from Algorithm.Python import QCAlgorithm
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot as plt

class ETHForecastingResearch:
    def __init__(self):
        self.qb = QuantBookManager()
        self.symbol = self.qb.AddCrypto("ETHUSD")
        
    def collect_training_data(self):
        """Collect historical data for model training using LEAN data APIs"""
        # Get 2 years of data at multiple timeframes
        timeframes = [
            Resolution.Minute,      # 1-minute bars
            Resolution.Hour,        # 1-hour bars  
            Resolution.Daily        # Daily bars
        ]
        
        end_date = datetime.now()
        start_date = end_date - timedelta(days=730)  # 2 years
        
        datasets = {}
        for resolution in timeframes:
            print(f"Downloading {resolution} data...")
            history = self.qb.History(
                self.symbol, 
                start_date, 
                end_date, 
                resolution
            )
            datasets[str(resolution)] = history
            
        return datasets
        
    def engineer_features(self, data):
        """Create features using LEAN's built-in indicators"""
        from Indicators import SimpleMovingAverage, RelativeStrengthIndex, MovingAverageConvergenceDivergence
        
        # Initialize LEAN indicators
        sma_10 = SimpleMovingAverage(10)
        sma_20 = SimpleMovingAverage(20) 
        sma_50 = SimpleMovingAverage(50)
        rsi = RelativeStrengthIndex(14)
        macd = MovingAverageConvergenceDivergence(12, 26, 9)
        
        features = []
        for bar in data:
            # Update indicators with new bar
            sma_10.Update(bar.Close)
            sma_20.Update(bar.Close)
            sma_50.Update(bar.Close)
            rsi.Update(bar.Close)
            macd.Update(bar.Close)
            
            # Collect features once indicators are ready
            if sma_50.IsReady and rsi.IsReady and macd.IsReady:
                feature_row = {
                    'timestamp': bar.Time,
                    'close': bar.Close,
                    'volume': bar.Volume,
                    'sma_10': sma_10.Current.Value,
                    'sma_20': sma_20.Current.Value,
                    'sma_50': sma_50.Current.Value,
                    'rsi': rsi.Current.Value,
                    'macd': macd.Current.Value,
                    'macd_signal': macd.Signal.Current.Value,
                    'price_change': (bar.Close - bar.Open) / bar.Open,
                    'volatility': self.calculate_volatility(data, 20),
                    # Target: predict next period return
                    'target': self.calculate_future_return(data, bar.Time)
                }
                features.append(feature_row)
                
        return pd.DataFrame(features)
        
    def train_forecasting_model(self, features_df):
        """Train ML model using scikit-learn"""
        # Prepare training data
        feature_columns = ['sma_10', 'sma_20', 'sma_50', 'rsi', 'macd', 
                          'macd_signal', 'price_change', 'volatility']
        X = features_df[feature_columns].dropna()
        y = features_df['target'].dropna()
        
        # Align X and y
        min_len = min(len(X), len(y))
        X = X.iloc[:min_len]
        y = y.iloc[:min_len]
        
        # Train Random Forest model
        model = RandomForestRegressor(
            n_estimators=100,
            max_depth=10,
            random_state=42
        )
        
        # Time series split for training/validation
        split_idx = int(len(X) * 0.8)
        X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
        y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
        
        model.fit(X_train, y_train)
        
        # Evaluate model
        train_score = model.score(X_train, y_train)
        test_score = model.score(X_test, y_test)
        
        print(f"Training R²: {train_score:.4f}")
        print(f"Testing R²: {test_score:.4f}")
        
        # Feature importance
        feature_importance = pd.DataFrame({
            'feature': feature_columns,
            'importance': model.feature_importances_
        }).sort_values('importance', ascending=False)
        
        print("Feature Importance:")
        print(feature_importance)
        
        return model, feature_importance
```

#### **2. Model Storage Integration**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Algorithm/Python/ETHModelStorage.py

import pickle
import os
from datetime import datetime

class ETHModelManager:
    def __init__(self):
        self.model_path = "/workspaces/unicorninvesting/BackendPython/Lean/Data/ETH_Models"
        os.makedirs(self.model_path, exist_ok=True)
        
    def save_model(self, model, model_name, metadata=None):
        """Save trained model with metadata"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{model_name}_{timestamp}.pkl"
        filepath = os.path.join(self.model_path, filename)
        
        model_package = {
            'model': model,
            'timestamp': timestamp,
            'metadata': metadata or {}
        }
        
        with open(filepath, 'wb') as f:
            pickle.dump(model_package, f)
            
        print(f"Model saved: {filepath}")
        return filepath
        
    def load_latest_model(self, model_name):
        """Load most recent model"""
        # Find latest model file
        model_files = [f for f in os.listdir(self.model_path) 
                      if f.startswith(model_name) and f.endswith('.pkl')]
        
        if not model_files:
            raise FileNotFoundError(f"No models found for {model_name}")
            
        latest_file = sorted(model_files)[-1]
        filepath = os.path.join(self.model_path, latest_file)
        
        with open(filepath, 'rb') as f:
            model_package = pickle.load(f)
            
        return model_package['model'], model_package['metadata']
```

---

## 🔴 **USE CASE 2: LIVE TRADING IMPLEMENTATION**
### **LEAN Algorithm Framework Integration**

#### **1. Main Algorithm Class**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Algorithm/Python/ETHLiveTradingAlgorithm.py

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/Lean')

from AlgorithmImports import *
from ETHModelStorage import ETHModelManager
import pickle
import numpy as np

class ETHLiveTradingAlgorithm(QCAlgorithm):
    
    def Initialize(self):
        """Initialize algorithm with ETH trading setup"""
        # Set date range and cash
        self.SetStartDate(2024, 1, 1)
        self.SetCash(10000)
        
        # Add ETH cryptocurrency
        self.symbol = self.AddCrypto("ETHUSD", Resolution.Minute).Symbol
        
        # Load trained forecasting model
        self.model_manager = ETHModelManager()
        try:
            self.forecasting_model, self.model_metadata = self.model_manager.load_latest_model("eth_forecasting")
            self.Log(f"Loaded forecasting model: {self.model_metadata}")
        except FileNotFoundError:
            self.Log("No forecasting model found. Running in passive mode.")
            self.forecasting_model = None
            
        # Initialize indicators for real-time feature calculation
        self.sma_10 = self.SMA(self.symbol, 10, Resolution.Minute)
        self.sma_20 = self.SMA(self.symbol, 20, Resolution.Minute)
        self.sma_50 = self.SMA(self.symbol, 50, Resolution.Minute)
        self.rsi = self.RSI(self.symbol, 14, Resolution.Minute)
        self.macd = self.MACD(self.symbol, 12, 26, 9, Resolution.Minute)
        
        # Risk management parameters
        self.max_position_size = 0.5  # 50% of portfolio
        self.stop_loss_pct = 0.05     # 5% stop loss
        self.take_profit_pct = 0.10   # 10% take profit
        
        # Trading state
        self.last_prediction = None
        self.entry_price = None
        self.position_size = 0
        
        # Schedule rebalancing every minute when we have new data
        self.Schedule.On(
            self.DateRules.EveryDay(),
            self.TimeRules.Every(TimeSpan.FromMinutes(1)),
            self.Rebalance
        )
        
    def OnData(self, data):
        """Handle new market data"""
        if not data.ContainsKey(self.symbol):
            return
            
        current_price = data[self.symbol].Close
        
        # Update our internal tracking
        self.UpdateRiskManagement(current_price)
        
    def Rebalance(self):
        """Main trading logic called every minute"""
        if not self.forecasting_model:
            return
            
        # Calculate current features for prediction
        features = self.CalculateCurrentFeatures()
        if features is None:
            return
            
        # Get model prediction
        prediction = self.forecasting_model.predict([features])[0]
        self.last_prediction = prediction
        
        # Generate trading signal
        signal = self.GenerateTradingSignal(prediction)
        
        # Execute trades based on signal
        self.ExecuteTrade(signal)
        
        # Log current state
        current_price = self.Securities[self.symbol].Price
        self.Log(f"Price: ${current_price:.2f}, Prediction: {prediction:.4f}, Signal: {signal}, Position: {self.position_size:.2f}")
        
    def CalculateCurrentFeatures(self):
        """Calculate features for current market state"""
        # Ensure all indicators are ready
        if not (self.sma_50.IsReady and self.rsi.IsReady and self.macd.IsReady):
            return None
            
        current_price = self.Securities[self.symbol].Price
        previous_price = self.History(self.symbol, 2, Resolution.Minute)
        
        if len(previous_price) < 2:
            return None
            
        price_change = (current_price - previous_price.iloc[-2]['close']) / previous_price.iloc[-2]['close']
        
        # Calculate volatility using recent history
        recent_history = self.History(self.symbol, 20, Resolution.Minute)
        if len(recent_history) < 20:
            return None
            
        returns = recent_history['close'].pct_change().dropna()
        volatility = returns.std()
        
        features = [
            self.sma_10.Current.Value,
            self.sma_20.Current.Value, 
            self.sma_50.Current.Value,
            self.rsi.Current.Value,
            self.macd.Current.Value,
            self.macd.Signal.Current.Value,
            price_change,
            volatility
        ]
        
        return features
        
    def GenerateTradingSignal(self, prediction):
        """Convert model prediction to trading signal"""
        # Signal thresholds (tune based on backtesting)
        buy_threshold = 0.02   # Predict >2% return
        sell_threshold = -0.01 # Predict <-1% return
        
        current_position = self.position_size
        
        if prediction > buy_threshold and current_position <= 0:
            return "BUY"
        elif prediction < sell_threshold and current_position >= 0:
            return "SELL"
        elif abs(prediction) < 0.005:  # Very low prediction, close position
            if current_position != 0:
                return "CLOSE"
        
        return "HOLD"
        
    def ExecuteTrade(self, signal):
        """Execute trades based on signal"""
        current_price = self.Securities[self.symbol].Price
        
        if signal == "BUY":
            # Calculate position size based on Kelly criterion or fixed fraction
            target_position = self.max_position_size
            current_position = self.Portfolio[self.symbol].Quantity * current_price / self.Portfolio.TotalPortfolioValue
            
            if current_position < target_position:
                order_value = (target_position - current_position) * self.Portfolio.TotalPortfolioValue
                quantity = order_value / current_price
                
                self.MarketOrder(self.symbol, quantity)
                self.entry_price = current_price
                self.position_size = target_position
                self.Log(f"BUY: {quantity:.4f} ETH at ${current_price:.2f}")
                
        elif signal == "SELL":
            # Close long position and go short (if allowed)
            if self.Portfolio[self.symbol].Quantity > 0:
                self.Liquidate(self.symbol)
                self.Log(f"SELL: Liquidated ETH position at ${current_price:.2f}")
                
            # Optional: Go short
            # target_position = -self.max_position_size
            # ... implement short logic
            
        elif signal == "CLOSE":
            if self.Portfolio[self.symbol].Quantity != 0:
                self.Liquidate(self.symbol)
                self.position_size = 0
                self.entry_price = None
                self.Log(f"CLOSE: Liquidated position at ${current_price:.2f}")
                
    def UpdateRiskManagement(self, current_price):
        """Check stop loss and take profit levels"""
        if self.entry_price is None or self.position_size == 0:
            return
            
        current_position = self.Portfolio[self.symbol].Quantity
        if current_position == 0:
            return
            
        # Calculate unrealized P&L percentage
        if current_position > 0:  # Long position
            pnl_pct = (current_price - self.entry_price) / self.entry_price
        else:  # Short position
            pnl_pct = (self.entry_price - current_price) / self.entry_price
            
        # Check stop loss
        if pnl_pct <= -self.stop_loss_pct:
            self.Liquidate(self.symbol)
            self.Log(f"STOP LOSS: Position closed at ${current_price:.2f}, Loss: {pnl_pct:.2%}")
            self.position_size = 0
            self.entry_price = None
            
        # Check take profit
        elif pnl_pct >= self.take_profit_pct:
            self.Liquidate(self.symbol)
            self.Log(f"TAKE PROFIT: Position closed at ${current_price:.2f}, Profit: {pnl_pct:.2%}")
            self.position_size = 0
            self.entry_price = None
```

#### **2. Custom Universe Selection**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Algorithm/Python/ETHUniverse.py

from AlgorithmImports import *

class ETHUniverseSelection(UniverseSelectionModel):
    """Custom universe selection for ETH-focused trading"""
    
    def __init__(self):
        self.lastMonth = -1
        
    def CreateUniverses(self, algorithm):
        """Create universe with ETH and related cryptocurrencies"""
        return [
            ManualUniverseSelectionModel([
                Symbol.Create("ETHUSD", SecurityType.Crypto, Market.GDAX),
                Symbol.Create("BTCUSD", SecurityType.Crypto, Market.GDAX),  # For correlation analysis
            ])
        ]
        
    def SelectCoarse(self, algorithm, coarse):
        """Select ETH as primary trading symbol"""
        return [x.Symbol for x in coarse if x.Symbol.Value in ["ETHUSD", "BTCUSD"]]
```

---

## 🛡️ **USE CASE 3: RISK ANALYSIS IMPLEMENTATION**
### **LEAN Risk Management Integration**

#### **1. Custom Risk Management Model**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Algorithm/Python/ETHRiskManagement.py

from AlgorithmImports import *
import numpy as np
import pandas as pd

class ETHRiskManagementModel(RiskManagementModel):
    """Advanced risk management for ETH trading"""
    
    def __init__(self, algorithm):
        self.algorithm = algorithm
        self.lookback_period = 252  # 1 year of daily data
        self.var_confidence = 0.05  # 95% VaR
        self.max_portfolio_var = 0.10  # 10% max portfolio VaR
        self.max_drawdown = 0.15  # 15% max drawdown
        
        # Risk metrics tracking
        self.historical_returns = []
        self.peak_portfolio_value = 0
        self.current_drawdown = 0
        
    def ManageRisk(self, algorithm, targets):
        """Main risk management logic"""
        risk_adjusted_targets = []
        
        # Update risk metrics
        self.UpdateRiskMetrics(algorithm)
        
        # Calculate current portfolio risk
        portfolio_var = self.CalculatePortfolioVaR(algorithm)
        current_drawdown = self.CalculateCurrentDrawdown(algorithm)
        
        # Risk-based position sizing
        for target in targets:
            if target.Quantity == 0:
                risk_adjusted_targets.append(target)
                continue
                
            # Adjust position size based on risk limits
            risk_adjusted_quantity = self.AdjustPositionForRisk(
                algorithm, target, portfolio_var, current_drawdown
            )
            
            if risk_adjusted_quantity != target.Quantity:
                algorithm.Log(f"Risk adjustment: {target.Symbol} quantity changed from {target.Quantity} to {risk_adjusted_quantity}")
                
            risk_adjusted_targets.append(
                PortfolioTarget(target.Symbol, risk_adjusted_quantity)
            )
            
        return risk_adjusted_targets
        
    def UpdateRiskMetrics(self, algorithm):
        """Update historical returns and risk metrics"""
        current_value = algorithm.Portfolio.TotalPortfolioValue
        
        # Track peak portfolio value for drawdown calculation
        if current_value > self.peak_portfolio_value:
            self.peak_portfolio_value = current_value
            
        # Calculate current drawdown
        self.current_drawdown = (self.peak_portfolio_value - current_value) / self.peak_portfolio_value
        
        # Update historical returns (daily)
        if algorithm.Time.hour == 0 and algorithm.Time.minute == 0:  # Daily update
            if len(self.historical_returns) > 0:
                previous_value = self.historical_returns[-1]['portfolio_value']
                daily_return = (current_value - previous_value) / previous_value
            else:
                daily_return = 0.0
                
            self.historical_returns.append({
                'date': algorithm.Time,
                'portfolio_value': current_value,
                'daily_return': daily_return
            })
            
            # Keep only lookback period
            if len(self.historical_returns) > self.lookback_period:
                self.historical_returns = self.historical_returns[-self.lookback_period:]
                
    def CalculatePortfolioVaR(self, algorithm):
        """Calculate portfolio Value at Risk"""
        if len(self.historical_returns) < 30:  # Need minimum historical data
            return 0.0
            
        returns = [x['daily_return'] for x in self.historical_returns]
        returns_array = np.array(returns)
        
        # Calculate VaR using historical simulation
        var_95 = np.percentile(returns_array, self.var_confidence * 100)
        
        return abs(var_95)  # Return as positive value
        
    def CalculateCurrentDrawdown(self, algorithm):
        """Calculate current drawdown percentage"""
        return self.current_drawdown
        
    def AdjustPositionForRisk(self, algorithm, target, portfolio_var, current_drawdown):
        """Adjust position size based on risk limits"""
        original_quantity = target.Quantity
        
        # Check drawdown limit
        if current_drawdown >= self.max_drawdown:
            algorithm.Log(f"Maximum drawdown reached: {current_drawdown:.2%}. Reducing position size.")
            return 0  # Close all positions
            
        # Check VaR limit
        if portfolio_var >= self.max_portfolio_var:
            # Reduce position size proportionally
            reduction_factor = self.max_portfolio_var / portfolio_var
            adjusted_quantity = original_quantity * reduction_factor
            algorithm.Log(f"Portfolio VaR too high: {portfolio_var:.2%}. Reducing position by {(1-reduction_factor):.1%}")
            return adjusted_quantity
            
        # Volatility-based position sizing
        symbol_volatility = self.CalculateSymbolVolatility(algorithm, target.Symbol)
        if symbol_volatility > 0:
            # Inverse volatility scaling: higher volatility = smaller position
            vol_adjustment = min(1.0, 0.20 / symbol_volatility)  # 20% target volatility
            adjusted_quantity = original_quantity * vol_adjustment
            
            if vol_adjustment < 0.8:  # Log significant adjustments
                algorithm.Log(f"High volatility detected for {target.Symbol}: {symbol_volatility:.2%}. Position reduced by {(1-vol_adjustment):.1%}")
                
            return adjusted_quantity
            
        return original_quantity
        
    def CalculateSymbolVolatility(self, algorithm, symbol):
        """Calculate rolling volatility for a symbol"""
        try:
            # Get recent price history
            history = algorithm.History(symbol, 20, Resolution.Daily)
            if len(history) < 10:
                return 0.0
                
            # Calculate daily returns
            returns = history['close'].pct_change().dropna()
            
            # Annualized volatility
            volatility = returns.std() * np.sqrt(252)
            
            return volatility
            
        except Exception as e:
            algorithm.Log(f"Error calculating volatility for {symbol}: {e}")
            return 0.0
```

#### **2. Risk Reporting and Monitoring**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Algorithm/Python/ETHRiskReporting.py

from AlgorithmImports import *
import json

class ETHRiskReporter:
    """Risk reporting and monitoring for ETH trading"""
    
    def __init__(self, algorithm):
        self.algorithm = algorithm
        self.risk_log = []
        
    def GenerateRiskReport(self, risk_model):
        """Generate comprehensive risk report"""
        current_time = self.algorithm.Time
        portfolio_value = self.algorithm.Portfolio.TotalPortfolioValue
        
        # Collect risk metrics
        portfolio_var = risk_model.CalculatePortfolioVaR(self.algorithm)
        current_drawdown = risk_model.CalculateCurrentDrawdown(self.algorithm)
        
        # Position-level risk
        positions_risk = {}
        for symbol, holding in self.algorithm.Portfolio.items():
            if holding.Quantity != 0:
                symbol_vol = risk_model.CalculateSymbolVolatility(self.algorithm, symbol)
                position_value = holding.HoldingsValue
                position_weight = position_value / portfolio_value
                
                positions_risk[str(symbol)] = {
                    'quantity': holding.Quantity,
                    'value': position_value,
                    'weight': position_weight,
                    'volatility': symbol_vol,
                    'unrealized_pnl': holding.UnrealizedProfit,
                    'unrealized_pnl_pct': holding.UnrealizedProfitPercent
                }
        
        # Create risk report
        risk_report = {
            'timestamp': current_time.strftime('%Y-%m-%d %H:%M:%S'),
            'portfolio_value': portfolio_value,
            'portfolio_var_95': portfolio_var,
            'current_drawdown': current_drawdown,
            'max_drawdown_limit': risk_model.max_drawdown,
            'var_limit': risk_model.max_portfolio_var,
            'positions': positions_risk,
            'risk_alerts': self.CheckRiskAlerts(risk_model, portfolio_var, current_drawdown)
        }
        
        # Store report
        self.risk_log.append(risk_report)
        
        # Log critical alerts
        if risk_report['risk_alerts']:
            for alert in risk_report['risk_alerts']:
                self.algorithm.Log(f"RISK ALERT: {alert}")
                
        return risk_report
        
    def CheckRiskAlerts(self, risk_model, portfolio_var, current_drawdown):
        """Check for risk limit violations"""
        alerts = []
        
        # VaR limit check
        if portfolio_var >= risk_model.max_portfolio_var * 0.8:  # 80% of limit
            alerts.append(f"Portfolio VaR approaching limit: {portfolio_var:.2%} / {risk_model.max_portfolio_var:.2%}")
            
        # Drawdown limit check
        if current_drawdown >= risk_model.max_drawdown * 0.7:  # 70% of limit
            alerts.append(f"Drawdown approaching limit: {current_drawdown:.2%} / {risk_model.max_drawdown:.2%}")
            
        # Position concentration check
        for symbol, holding in self.algorithm.Portfolio.items():
            if holding.Quantity != 0:
                position_weight = abs(holding.HoldingsValue) / self.algorithm.Portfolio.TotalPortfolioValue
                if position_weight > 0.6:  # 60% concentration threshold
                    alerts.append(f"High concentration in {symbol}: {position_weight:.1%}")
                    
        return alerts
        
    def SaveRiskReports(self):
        """Save risk reports to file"""
        filename = f"/workspaces/unicorninvesting/BackendPython/Lean/Data/ETH_Risk_Reports/risk_report_{self.algorithm.Time.strftime('%Y%m%d')}.json"
        
        os.makedirs(os.path.dirname(filename), exist_ok=True)
        
        with open(filename, 'w') as f:
            json.dump(self.risk_log, f, indent=2)
            
        self.algorithm.Log(f"Risk report saved: {filename}")
```

---

## 🚀 **DEPLOYMENT INTEGRATION**

### **Main Integration Script**
```python
# Location: /workspaces/unicorninvesting/BackendPython/Lean/Algorithm/Python/ETHMasterAlgorithm.py

from AlgorithmImports import *
from ETHLiveTradingAlgorithm import ETHLiveTradingAlgorithm
from ETHRiskManagement import ETHRiskManagementModel, ETHRiskReporter
from ETHUniverse import ETHUniverseSelection

class ETHMasterAlgorithm(ETHLiveTradingAlgorithm):
    """Master algorithm integrating all ETH use cases"""
    
    def Initialize(self):
        # Call parent initialization
        super().Initialize()
        
        # Add risk management
        self.risk_model = ETHRiskManagementModel(self)
        self.SetRiskManagement(self.risk_model)
        
        # Add universe selection
        self.SetUniverseSelection(ETHUniverseSelection())
        
        # Add risk reporting
        self.risk_reporter = ETHRiskReporter(self)
        
        # Schedule risk reporting
        self.Schedule.On(
            self.DateRules.EveryDay(),
            self.TimeRules.At(23, 59),  # End of day
            self.GenerateRiskReport
        )
        
    def GenerateRiskReport(self):
        """Generate daily risk report"""
        report = self.risk_reporter.GenerateRiskReport(self.risk_model)
        
        # Log summary
        self.Log(f"Daily Risk Summary - VaR: {report['portfolio_var_95']:.2%}, Drawdown: {report['current_drawdown']:.2%}")
        
    def OnEndOfAlgorithm(self):
        """Save final reports when algorithm ends"""
        self.risk_reporter.SaveRiskReports()
        self.Log("ETH Trading Algorithm completed. Risk reports saved.")
```

This integration guide shows how to implement all three ETH use cases within the existing LEAN framework, leveraging its built-in capabilities for data handling, backtesting, and live trading while adding our custom forecasting and risk management components.
