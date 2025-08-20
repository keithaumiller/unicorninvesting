# QuantConnect Deployment Guide

## 🚀 Deployment Strategy

This guide provides step-by-step instructions for deploying Unicorn Investing algorithms to QuantConnect's cloud platform, from development to live trading.

## 📋 Table of Contents

1. [Pre-Deployment Checklist](#pre-deployment-checklist)
2. [Development Environment Setup](#development-environment-setup)
3. [Algorithm Packaging](#algorithm-packaging)
4. [Testing & Validation](#testing--validation)
5. [Deployment Process](#deployment-process)
6. [Live Trading Setup](#live-trading-setup)
7. [Monitoring & Maintenance](#monitoring--maintenance)
8. [Troubleshooting](#troubleshooting)

## ✅ Pre-Deployment Checklist

### Technical Requirements
- [ ] QuantConnect account with sufficient credits
- [ ] Verified identity for live trading
- [ ] Brokerage account setup (Interactive Brokers recommended)
- [ ] Python 3.8+ algorithms tested locally
- [ ] All dependencies available in QuantConnect environment
- [ ] Strategy performance validated through backtesting
- [ ] Risk management parameters configured
- [ ] Compliance review completed

### Documentation Requirements  
- [ ] Algorithm documentation complete
- [ ] Strategy description and rationale documented
- [ ] Risk parameters and constraints defined
- [ ] Expected performance metrics established
- [ ] Monitoring and alerting procedures defined
- [ ] Rollback procedures documented

### Regulatory Requirements
- [ ] Trading strategy registered with appropriate authorities
- [ ] Risk disclosures prepared
- [ ] Compliance monitoring procedures in place
- [ ] Audit trail capabilities verified
- [ ] Data retention policies implemented

## 🛠️ Development Environment Setup

### Local Development Environment

```bash
# Create project directory
mkdir unicorn-quantconnect-deploy
cd unicorn-quantconnect-deploy

# Setup Python virtual environment
python3.8 -m venv venv
source venv/bin/activate  # Linux/Mac
# venv\Scripts\activate  # Windows

# Install QuantConnect LEAN CLI
pip install lean

# Install Unicorn dependencies
pip install -r requirements.txt

# Initialize LEAN project
lean init
```

### QuantConnect Cloud IDE Setup

1. **Create New Project**
   ```python
   # Access QuantConnect.com
   # Navigate to Algorithm Lab
   # Click "New Project"
   # Select "Python" language
   # Name: "Unicorn_GA_Strategy_v1"
   ```

2. **Configure Project Settings**
   ```json
   {
     "name": "Unicorn_GA_Strategy_v1",
     "description": "Genetic Algorithm + Neural Network Portfolio Optimization",
     "language": "Python",
     "cloudId": 12345678,
     "created": "2025-08-20T10:00:00Z"
   }
   ```

### Version Control Integration

```bash
# Connect to Git repository
git clone https://github.com/unicorninvesting/quantconnect-algorithms.git
cd quantconnect-algorithms

# Create deployment branch
git checkout -b deploy/unicorn-ga-v1.0

# Setup deployment structure
mkdir -p algorithms/unicorn-ga-v1
mkdir -p tests/
mkdir -p configs/
mkdir -p docs/
```

## 📦 Algorithm Packaging

### Directory Structure

```
unicorn-ga-algorithm/
├── main.py                    # Main algorithm file
├── unicorn/                   # Unicorn modules
│   ├── __init__.py
│   ├── genetic_algorithm.py
│   ├── neural_networks.py
│   ├── feature_engine.py
│   ├── risk_manager.py
│   └── portfolio_optimizer.py
├── data/                      # Data files
│   ├── features_config.json
│   ├── model_weights.pkl
│   └── universe_symbols.json
├── tests/                     # Test files
│   ├── test_algorithm.py
│   ├── test_genetic_algorithm.py
│   └── test_neural_networks.py
├── config.json               # Algorithm configuration
├── requirements.txt          # Dependencies
└── README.md                # Documentation
```

### Main Algorithm File (`main.py`)

```python
#region imports
from AlgorithmImports import *
from unicorn.genetic_algorithm import GeneticAlgorithm
from unicorn.neural_networks import NeuralNetwork
from unicorn.feature_engine import FeatureEngine
from unicorn.risk_manager import RiskManager
from unicorn.portfolio_optimizer import PortfolioOptimizer
import json
import numpy as np
import pandas as pd
#endregion

class UnicornGeneticAlgorithm(QCAlgorithm):
    """
    Unicorn Investing Genetic Algorithm + Neural Network Strategy
    
    This algorithm combines genetic algorithms for parameter optimization
    with neural networks for pattern recognition to create optimal
    portfolio allocations for unicorn and high-growth stocks.
    """
    
    def Initialize(self):
        """Initialize the algorithm with configuration and components"""
        
        # Set algorithm parameters
        self.SetStartDate(2024, 1, 1)
        self.SetEndDate(2025, 12, 31)
        self.SetCash(1000000)  # $1M starting capital
        self.SetBenchmark("QQQ")  # NASDAQ benchmark
        
        # Load configuration
        self.config = self.LoadConfiguration()
        
        # Initialize components
        self.genetic_algorithm = GeneticAlgorithm(self.config['ga_params'])
        self.neural_network = NeuralNetwork(self.config['nn_params'])
        self.feature_engine = FeatureEngine(self.config['features'])
        self.risk_manager = RiskManager(self.config['risk_params'])
        self.portfolio_optimizer = PortfolioOptimizer(self.config['optimization'])
        
        # Setup universe
        self.universe_symbols = []
        self.LoadUniverse()
        
        # Schedule rebalancing
        self.Schedule.On(
            self.DateRules.WeekStartEnd(Calendar.Weekly),
            self.TimeRules.AfterMarketOpen("QQQ", 30),
            self.Rebalance
        )
        
        # Setup warm-up period
        self.SetWarmUp(timedelta(days=252))  # 1 year warm-up
        
        # Initialize tracking variables
        self.last_rebalance_time = self.Time
        self.rebalance_count = 0
        self.performance_metrics = {}
        
        # Setup custom charts
        self.SetupCharts()
        
        self.Log("Unicorn Genetic Algorithm initialized successfully")
    
    def LoadConfiguration(self):
        """Load algorithm configuration from JSON file"""
        try:
            config_data = self.Download("config.json")
            return json.loads(config_data)
        except Exception as e:
            self.Error(f"Failed to load configuration: {e}")
            return self.GetDefaultConfiguration()
    
    def LoadUniverse(self):
        """Load trading universe from configuration"""
        universe_symbols = self.config.get('universe', [
            "AAPL", "GOOGL", "MSFT", "AMZN", "TSLA", "META", "NVDA", "NFLX"
        ])
        
        for symbol in universe_symbols:
            try:
                equity = self.AddEquity(symbol, Resolution.Daily)
                equity.SetDataNormalizationMode(DataNormalizationMode.Adjusted)
                self.universe_symbols.append(equity.Symbol)
                self.Log(f"Added {symbol} to universe")
            except Exception as e:
                self.Error(f"Failed to add {symbol}: {e}")
    
    def OnData(self, data):
        """Process incoming data and update features"""
        if self.IsWarmingUp:
            return
        
        # Update feature engine with new data
        self.feature_engine.UpdateFeatures(data, self.universe_symbols)
        
        # Update risk metrics
        self.risk_manager.UpdateRiskMetrics(self.Portfolio, data)
    
    def Rebalance(self):
        """Main rebalancing logic"""
        try:
            self.Log(f"Starting rebalance #{self.rebalance_count + 1}")
            
            # Check if we have sufficient data
            if not self.feature_engine.HasSufficientData():
                self.Log("Insufficient data for rebalancing, skipping...")
                return
            
            # Extract features for all symbols
            features = self.feature_engine.GetCurrentFeatures(self.universe_symbols)
            if features is None or len(features) == 0:
                self.Log("No features available, skipping rebalancing")
                return
            
            # Run genetic algorithm optimization
            ga_weights = self.genetic_algorithm.Optimize(
                features, 
                self.GetHistoricalReturns()
            )
            
            # Generate neural network predictions
            nn_predictions = self.neural_network.Predict(features)
            
            # Combine GA and NN results
            combined_weights = self.portfolio_optimizer.CombineAllocations(
                ga_weights, 
                nn_predictions,
                self.config['combination_method']
            )
            
            # Apply risk management constraints
            final_weights = self.risk_manager.ApplyConstraints(
                combined_weights,
                self.Portfolio.TotalPortfolioValue
            )
            
            # Execute portfolio rebalancing
            self.ExecuteRebalance(final_weights)
            
            # Update tracking
            self.rebalance_count += 1
            self.last_rebalance_time = self.Time
            
            # Log performance metrics
            self.LogPerformanceMetrics(final_weights)
            
        except Exception as e:
            self.Error(f"Rebalancing failed: {e}")
    
    def ExecuteRebalance(self, target_weights):
        """Execute portfolio rebalancing with target weights"""
        portfolio_value = self.Portfolio.TotalPortfolioValue
        
        for symbol in self.universe_symbols:
            symbol_str = str(symbol)
            target_weight = target_weights.get(symbol_str, 0.0)
            target_value = portfolio_value * target_weight
            
            current_holdings = self.Portfolio[symbol]
            current_value = current_holdings.HoldingsValue
            
            # Calculate required trade
            value_difference = target_value - current_value
            
            if abs(value_difference) > 1000:  # Minimum trade threshold
                current_price = self.Securities[symbol].Price
                if current_price > 0:
                    shares_to_trade = int(value_difference / current_price)
                    
                    if shares_to_trade != 0:
                        # Execute market order
                        order_ticket = self.MarketOrder(
                            symbol, 
                            shares_to_trade,
                            tag=f"Rebalance_{self.rebalance_count}"
                        )
                        
                        self.Log(f"Ordered {shares_to_trade} shares of {symbol} "
                                f"(target: {target_weight:.2%})")
    
    def GetHistoricalReturns(self, lookback_days=252):
        """Get historical returns for optimization"""
        history = self.History(self.universe_symbols, lookback_days, Resolution.Daily)
        
        if history.empty:
            return None
        
        # Calculate returns
        returns = {}
        for symbol in self.universe_symbols:
            if symbol in history.index.get_level_values(0):
                symbol_data = history.loc[symbol]['close']
                symbol_returns = symbol_data.pct_change().dropna()
                returns[str(symbol)] = symbol_returns.values
        
        return returns
    
    def SetupCharts(self):
        """Setup custom performance charts"""
        
        # Portfolio composition chart
        composition_chart = Chart("Portfolio Composition")
        composition_chart.AddSeries(Series("Cash", SeriesType.Line, "%"))
        for symbol in self.universe_symbols:
            composition_chart.AddSeries(
                Series(f"{symbol} Weight", SeriesType.Line, "%")
            )
        self.AddChart(composition_chart)
        
        # Risk metrics chart
        risk_chart = Chart("Risk Metrics")
        risk_chart.AddSeries(Series("Portfolio Volatility", SeriesType.Line, ""))
        risk_chart.AddSeries(Series("Max Drawdown", SeriesType.Line, "%"))
        risk_chart.AddSeries(Series("VaR 95%", SeriesType.Line, "%"))
        self.AddChart(risk_chart)
        
        # Algorithm performance chart
        algo_chart = Chart("Algorithm Performance")
        algo_chart.AddSeries(Series("GA Fitness", SeriesType.Line, ""))
        algo_chart.AddSeries(Series("NN Confidence", SeriesType.Line, ""))
        algo_chart.AddSeries(Series("Rebalance Count", SeriesType.Line, "#"))
        self.AddChart(algo_chart)
    
    def LogPerformanceMetrics(self, weights):
        """Log and plot performance metrics"""
        
        # Calculate current metrics
        portfolio_return = self.Portfolio.TotalReturn
        benchmark_return = self.Benchmark.Evaluate(self.Time)
        
        # Plot portfolio composition
        cash_percentage = self.Portfolio.Cash / self.Portfolio.TotalPortfolioValue * 100
        self.Plot("Portfolio Composition", "Cash", cash_percentage)
        
        for symbol in self.universe_symbols:
            weight = weights.get(str(symbol), 0.0) * 100
            self.Plot("Portfolio Composition", f"{symbol} Weight", weight)
        
        # Plot risk metrics
        if hasattr(self.risk_manager, 'current_volatility'):
            self.Plot("Risk Metrics", "Portfolio Volatility", 
                     self.risk_manager.current_volatility)
        
        # Log to console
        self.Log(f"Portfolio Return: {portfolio_return:.2%}, "
                f"Benchmark Return: {benchmark_return:.2%}")
    
    def OnOrderEvent(self, orderEvent):
        """Handle order events"""
        if orderEvent.Status == OrderStatus.Filled:
            self.Log(f"Order filled: {orderEvent.Symbol} - "
                    f"{orderEvent.FillQuantity} shares at ${orderEvent.FillPrice}")
    
    def OnEndOfAlgorithm(self):
        """Algorithm termination cleanup"""
        self.Log(f"Algorithm completed. Total rebalances: {self.rebalance_count}")
        self.Log(f"Final portfolio value: ${self.Portfolio.TotalPortfolioValue:,.2f}")
        self.Log(f"Total return: {self.Portfolio.TotalReturn:.2%}")
```

### Configuration File (`config.json`)

```json
{
  "version": "1.0.0",
  "algorithm": {
    "name": "Unicorn Genetic Algorithm",
    "description": "GA + NN optimization for unicorn stock portfolios",
    "author": "Unicorn Investing Team",
    "created": "2025-08-20"
  },
  
  "universe": [
    "AAPL", "GOOGL", "MSFT", "AMZN", "TSLA", "META", "NVDA", "NFLX",
    "CRM", "ADBE", "PYPL", "INTC", "AMD", "QCOM", "UBER", "ABNB"
  ],
  
  "ga_params": {
    "population_size": 100,
    "generations": 200,
    "mutation_rate": 0.1,
    "crossover_rate": 0.8,
    "elite_size": 10,
    "fitness_function": "sharpe_ratio",
    "convergence_threshold": 0.001,
    "max_stagnation_generations": 50
  },
  
  "nn_params": {
    "architecture": [64, 32, 16, 8],
    "activation": "relu",
    "learning_rate": 0.001,
    "epochs": 100,
    "batch_size": 32,
    "dropout_rate": 0.2,
    "regularization": 0.001
  },
  
  "features": {
    "technical_indicators": [
      "rsi", "macd", "bollinger_bands", "moving_averages",
      "stochastic", "williams_r", "momentum"
    ],
    "fundamental_ratios": [
      "pe_ratio", "price_to_book", "debt_to_equity", "roe", "roa"
    ],
    "market_indicators": [
      "volume_profile", "price_action", "volatility", "correlation"
    ],
    "lookback_periods": [5, 10, 20, 50, 200],
    "feature_engineering": {
      "normalization": "z_score",
      "missing_value_strategy": "forward_fill",
      "outlier_detection": "iqr_method"
    }
  },
  
  "risk_params": {
    "max_position_size": 0.15,
    "min_position_size": 0.01,
    "max_portfolio_volatility": 0.20,
    "max_drawdown_threshold": 0.10,
    "var_confidence_level": 0.95,
    "stress_test_scenarios": ["market_crash", "sector_rotation", "interest_rate_shock"],
    "rebalance_threshold": 0.05
  },
  
  "optimization": {
    "combination_method": "weighted_average",
    "ga_weight": 0.6,
    "nn_weight": 0.4,
    "objective_function": "risk_adjusted_return",
    "constraints": {
      "long_only": true,
      "sector_diversification": true,
      "turnover_limit": 0.5
    }
  },
  
  "execution": {
    "rebalance_frequency": "weekly",
    "market_timing": "after_open_30min",
    "order_type": "market",
    "trade_size_limit": 1000,
    "slippage_model": "volume_share"
  },
  
  "monitoring": {
    "performance_alerts": {
      "drawdown_threshold": 0.05,
      "underperformance_threshold": 0.10,
      "volatility_threshold": 0.25
    },
    "logging_level": "INFO",
    "save_intermediate_results": true
  }
}
```

### Requirements File (`requirements.txt`)

```txt
# Core scientific computing
numpy>=1.21.0
pandas>=1.3.0
scipy>=1.7.0

# Machine learning
scikit-learn>=1.0.0
tensorflow>=2.6.0

# Financial libraries  
quantlib>=1.24
TA-Lib>=0.4.0

# Optimization
DEAP>=1.3.0
cvxpy>=1.1.0

# Utilities
joblib>=1.1.0
tqdm>=4.62.0
python-dateutil>=2.8.0
```

## 🧪 Testing & Validation

### Backtesting Setup

```python
# Create backtest configuration
backtest_config = {
    "start_date": "2020-01-01",
    "end_date": "2024-12-31", 
    "initial_capital": 1000000,
    "benchmark": "QQQ",
    "resolution": "Daily"
}

# Run local backtest using LEAN CLI
lean backtest "Unicorn GA Algorithm" \
    --start 20200101 \
    --end 20241231 \
    --cash 1000000
```

### Unit Testing

```python
# tests/test_genetic_algorithm.py
import unittest
from unicorn.genetic_algorithm import GeneticAlgorithm

class TestGeneticAlgorithm(unittest.TestCase):
    def setUp(self):
        self.ga_config = {
            "population_size": 50,
            "generations": 100,
            "mutation_rate": 0.1,
            "crossover_rate": 0.8
        }
        self.ga = GeneticAlgorithm(self.ga_config)
    
    def test_initialization(self):
        self.assertEqual(self.ga.population_size, 50)
        self.assertEqual(self.ga.generations, 100)
    
    def test_optimization(self):
        # Mock feature data
        features = np.random.randn(10, 20)
        returns = np.random.randn(10, 252)
        
        # Run optimization
        weights = self.ga.optimize(features, returns)
        
        # Validate results
        self.assertIsInstance(weights, dict)
        self.assertAlmostEqual(sum(weights.values()), 1.0, places=3)
    
    def test_fitness_calculation(self):
        weights = {"AAPL": 0.5, "GOOGL": 0.5}
        returns = np.random.randn(252)
        
        fitness = self.ga.calculate_fitness(weights, returns)
        self.assertIsInstance(fitness, float)

if __name__ == '__main__':
    unittest.main()
```

### Performance Validation

```python
# Validate algorithm performance
def validate_algorithm_performance(backtest_results):
    """Validate algorithm meets performance criteria"""
    
    required_metrics = {
        'total_return': 0.10,      # Minimum 10% annual return
        'sharpe_ratio': 1.0,       # Minimum Sharpe ratio of 1.0  
        'max_drawdown': -0.15,     # Maximum 15% drawdown
        'win_rate': 0.55,          # Minimum 55% win rate
        'volatility': 0.25         # Maximum 25% volatility
    }
    
    validation_results = {}
    
    for metric, threshold in required_metrics.items():
        actual_value = backtest_results.get(metric, 0)
        
        if metric == 'max_drawdown':
            passed = actual_value >= threshold  # Less negative is better
        else:
            passed = actual_value >= threshold
        
        validation_results[metric] = {
            'actual': actual_value,
            'threshold': threshold,
            'passed': passed
        }
    
    return validation_results
```

## 🚀 Deployment Process

### Step 1: Upload to QuantConnect

```python
from quantconnect_api import QuantConnectAPI

# Initialize API client
qc_api = QuantConnectAPI(api_token="your_api_token")

# Create new project
project = qc_api.create_project(
    name="Unicorn_GA_Algorithm_v1",
    language="Python"
)

# Upload algorithm files
files_to_upload = {
    "main.py": open("main.py", "r").read(),
    "config.json": open("config.json", "r").read(),
    "unicorn/genetic_algorithm.py": open("unicorn/genetic_algorithm.py", "r").read(),
    "unicorn/neural_networks.py": open("unicorn/neural_networks.py", "r").read(),
    # ... upload all necessary files
}

for filename, content in files_to_upload.items():
    qc_api.upload_file(project.id, filename, content)

print(f"Project created successfully: {project.id}")
```

### Step 2: Compile and Test

```python
# Compile the project
compile_result = qc_api.compile_project(project.id)

if compile_result.success:
    print("Compilation successful!")
    
    # Run cloud backtest
    backtest = qc_api.create_backtest(
        project_id=project.id,
        compile_id=compile_result.compile_id,
        name="Production_Backtest_v1"
    )
    
    # Wait for backtest completion
    while backtest.status == "Running":
        time.sleep(30)
        backtest = qc_api.get_backtest(backtest.id)
    
    if backtest.status == "Completed":
        print("Backtest completed successfully!")
        print(f"Total Return: {backtest.statistics['Total Return']}")
        print(f"Sharpe Ratio: {backtest.statistics['Sharpe Ratio']}")
    
else:
    print("Compilation failed:")
    for error in compile_result.errors:
        print(f"  - {error}")
```

### Step 3: Deploy to Paper Trading

```python
# Deploy to paper trading first
paper_deployment = qc_api.create_live_algorithm(
    project_id=project.id,
    compile_id=compile_result.compile_id,
    brokerage="PaperTradingBrokerage",
    node_type="O1-8",  # Optimization node
    environment="paper"
)

print(f"Paper trading deployment: {paper_deployment.id}")

# Monitor paper trading for validation period
validation_period_days = 30
```

### Step 4: Deploy to Live Trading

```python
# After successful paper trading validation
if paper_trading_validated:
    live_deployment = qc_api.create_live_algorithm(
        project_id=project.id,
        compile_id=compile_result.compile_id,
        brokerage="InteractiveBrokersBrokerage",
        node_type="O1-8",
        environment="live",
        base_live_algorithm_settings={
            "id": "InteractiveBrokersBrokerage",
            "user": ib_username,
            "password": ib_password,
            "account": ib_account,
            "host": "LIVE",  # Use "TESTBED" for paper
            "port": "7497",
            "agentDescription": "Individual"
        }
    )
    
    print(f"Live trading deployment: {live_deployment.id}")
```

## 📊 Live Trading Setup

### Interactive Brokers Configuration

```python
# IB TWS/Gateway settings for live trading
ib_config = {
    "account_type": "individual",  # or "institutional"
    "trading_permissions": [
        "stocks", "options", "futures", "forex"
    ],
    "api_settings": {
        "enable_api": True,
        "socket_port": 7497,  # Live: 7497, Paper: 7498
        "master_api_id": 1,
        "read_only_api": False,
        "download_open_orders": True
    },
    "risk_settings": {
        "max_daily_loss": 50000,
        "max_position_size": 150000,
        "allowed_instruments": ["STK"],  # Stocks only
        "restricted_symbols": []
    }
}
```

### Risk Management Setup

```python
class LiveTradingRiskManager:
    def __init__(self, config):
        self.max_daily_loss = config['max_daily_loss']
        self.max_position_size = config['max_position_size']
        self.daily_pnl = 0.0
        self.start_of_day_equity = 0.0
    
    def check_risk_limits(self, portfolio_value, proposed_order):
        """Check if proposed order violates risk limits"""
        
        # Check daily loss limit
        if self.daily_pnl <= -self.max_daily_loss:
            return False, "Daily loss limit exceeded"
        
        # Check position size limit
        position_value = abs(proposed_order.quantity * proposed_order.price)
        if position_value > self.max_position_size:
            return False, "Position size limit exceeded"
        
        # Check portfolio heat
        portfolio_heat = self.calculate_portfolio_heat(portfolio_value)
        if portfolio_heat > 0.8:  # 80% max heat
            return False, "Portfolio heat too high"
        
        return True, "Risk check passed"
    
    def update_daily_pnl(self, current_equity):
        """Update daily P&L tracking"""
        if self.start_of_day_equity == 0:
            self.start_of_day_equity = current_equity
        
        self.daily_pnl = current_equity - self.start_of_day_equity
```

### Order Management

```python
class SmartOrderManager:
    def __init__(self, algorithm):
        self.algorithm = algorithm
        self.order_queue = []
        self.execution_tracker = {}
    
    def smart_market_order(self, symbol, quantity, tag=""):
        """Execute market order with smart execution logic"""
        
        # Check market conditions
        if not self.is_market_suitable_for_trading(symbol):
            self.algorithm.Log(f"Market conditions not suitable for {symbol}")
            return None
        
        # Split large orders
        if abs(quantity) > 1000:
            return self.execute_split_order(symbol, quantity, tag)
        
        # Execute regular market order
        return self.algorithm.MarketOrder(symbol, quantity, tag=tag)
    
    def is_market_suitable_for_trading(self, symbol):
        """Check if market conditions are suitable for trading"""
        
        security = self.algorithm.Securities[symbol]
        
        # Check if market is open
        if not security.Exchange.DateTimeIsOpen(self.algorithm.Time):
            return False
        
        # Check liquidity (volume)
        if hasattr(security, 'Volume') and security.Volume < 10000:
            return False
        
        # Check volatility
        volatility = self.calculate_recent_volatility(symbol)
        if volatility > 0.05:  # 5% volatility threshold
            return False
        
        return True
    
    def execute_split_order(self, symbol, total_quantity, tag):
        """Split large order into smaller chunks"""
        
        chunk_size = 500  # Share chunks
        remaining_quantity = abs(total_quantity)
        direction = 1 if total_quantity > 0 else -1
        
        orders = []
        
        while remaining_quantity > 0:
            current_chunk = min(chunk_size, remaining_quantity)
            order_quantity = current_chunk * direction
            
            order = self.algorithm.MarketOrder(
                symbol, 
                order_quantity,
                tag=f"{tag}_chunk_{len(orders)+1}"
            )
            orders.append(order)
            
            remaining_quantity -= current_chunk
            
            # Wait between chunks to avoid market impact
            if remaining_quantity > 0:
                self.algorithm.Schedule.On(
                    self.algorithm.DateRules.Today,
                    self.algorithm.TimeRules.At(
                        self.algorithm.Time + timedelta(minutes=2)
                    ),
                    lambda: None  # Small delay
                )
        
        return orders
```

## 📈 Monitoring & Maintenance

### Performance Monitoring Dashboard

```python
class PerformanceMonitor:
    def __init__(self, deployment_id):
        self.deployment_id = deployment_id
        self.qc_api = QuantConnectAPI()
        self.alert_thresholds = {
            'max_drawdown': -0.10,
            'daily_loss': -0.05,
            'sharpe_ratio': 1.0,
            'tracking_error': 0.05
        }
    
    def monitor_performance(self):
        """Continuous performance monitoring"""
        
        while True:
            try:
                # Get live results
                results = self.qc_api.get_live_results(self.deployment_id)
                
                # Calculate metrics
                metrics = self.calculate_performance_metrics(results)
                
                # Check alerts
                alerts = self.check_alert_conditions(metrics)
                
                if alerts:
                    self.send_alerts(alerts)
                
                # Update dashboard
                self.update_dashboard(metrics)
                
                # Log status
                self.log_status(metrics)
                
                time.sleep(60)  # Check every minute
                
            except Exception as e:
                print(f"Monitoring error: {e}")
                time.sleep(300)  # Wait 5 minutes on error
    
    def check_alert_conditions(self, metrics):
        """Check if any alert conditions are triggered"""
        
        alerts = []
        
        for metric, threshold in self.alert_thresholds.items():
            if metric in metrics:
                value = metrics[metric]
                
                if metric == 'max_drawdown' and value < threshold:
                    alerts.append({
                        'type': 'CRITICAL',
                        'message': f"Maximum drawdown exceeded: {value:.2%}",
                        'metric': metric,
                        'value': value,
                        'threshold': threshold
                    })
                
                elif metric == 'daily_loss' and value < threshold:
                    alerts.append({
                        'type': 'WARNING',
                        'message': f"Daily loss threshold exceeded: {value:.2%}",
                        'metric': metric,
                        'value': value,
                        'threshold': threshold
                    })
        
        return alerts
    
    def send_alerts(self, alerts):
        """Send alerts via email/SMS/Slack"""
        
        for alert in alerts:
            # Email notification
            self.send_email_alert(alert)
            
            # Slack notification
            self.send_slack_alert(alert)
            
            # SMS for critical alerts
            if alert['type'] == 'CRITICAL':
                self.send_sms_alert(alert)
```

### Automated Maintenance Tasks

```python
class MaintenanceManager:
    def __init__(self, deployment_id):
        self.deployment_id = deployment_id
    
    def daily_maintenance(self):
        """Daily maintenance tasks"""
        
        # 1. Check algorithm health
        health_status = self.check_algorithm_health()
        
        # 2. Validate data feeds
        data_validation = self.validate_data_feeds()
        
        # 3. Check resource usage
        resource_usage = self.check_resource_usage()
        
        # 4. Backup important data
        self.backup_algorithm_state()
        
        # 5. Generate daily report
        self.generate_daily_report(health_status, data_validation, resource_usage)
    
    def weekly_maintenance(self):
        """Weekly maintenance tasks"""
        
        # 1. Performance review
        self.conduct_performance_review()
        
        # 2. Strategy parameter review
        self.review_strategy_parameters()
        
        # 3. Risk model validation
        self.validate_risk_models()
        
        # 4. Update universe if needed
        self.review_trading_universe()
    
    def monthly_maintenance(self):
        """Monthly maintenance tasks"""
        
        # 1. Comprehensive performance analysis
        self.comprehensive_performance_analysis()
        
        # 2. Model retraining
        self.retrain_models()
        
        # 3. Strategy optimization
        self.optimize_strategy_parameters()
        
        # 4. Compliance review
        self.conduct_compliance_review()
```

### Logging and Audit Trail

```python
class AuditLogger:
    def __init__(self, algorithm_id):
        self.algorithm_id = algorithm_id
        self.log_file = f"audit_log_{algorithm_id}.json"
    
    def log_trade(self, order_event):
        """Log trade execution details"""
        
        trade_record = {
            'timestamp': datetime.utcnow().isoformat(),
            'algorithm_id': self.algorithm_id,
            'order_id': order_event.OrderId,
            'symbol': str(order_event.Symbol),
            'quantity': order_event.FillQuantity,
            'fill_price': order_event.FillPrice,
            'direction': 'BUY' if order_event.FillQuantity > 0 else 'SELL',
            'order_type': order_event.OrderType,
            'tag': order_event.Tag,
            'commission': order_event.Commission,
            'message': order_event.Message
        }
        
        self.write_log_record(trade_record)
    
    def log_rebalance(self, rebalance_data):
        """Log portfolio rebalancing details"""
        
        rebalance_record = {
            'timestamp': datetime.utcnow().isoformat(),
            'algorithm_id': self.algorithm_id,
            'event_type': 'REBALANCE',
            'portfolio_value': rebalance_data['portfolio_value'],
            'target_weights': rebalance_data['target_weights'],
            'actual_weights': rebalance_data['actual_weights'],
            'trades_executed': rebalance_data['trades_executed'],
            'rebalance_reason': rebalance_data['reason']
        }
        
        self.write_log_record(rebalance_record)
    
    def write_log_record(self, record):
        """Write log record to file and database"""
        
        # Write to file
        with open(self.log_file, 'a') as f:
            f.write(json.dumps(record) + '\n')
        
        # Store in database for compliance
        self.store_in_database(record)
```

## 🔧 Troubleshooting

### Common Deployment Issues

#### 1. Compilation Errors

```python
# Common compilation error fixes

# Error: Module not found
# Solution: Check imports and dependencies
#endregion imports should include all necessary modules

# Error: API call limit exceeded  
# Solution: Implement rate limiting
class RateLimitedAPI:
    def __init__(self, max_calls_per_minute=60):
        self.max_calls = max_calls_per_minute
        self.calls_made = []
    
    def make_api_call(self, func, *args, **kwargs):
        now = datetime.now()
        # Remove calls older than 1 minute
        self.calls_made = [call_time for call_time in self.calls_made 
                          if (now - call_time).seconds < 60]
        
        if len(self.calls_made) >= self.max_calls:
            sleep_time = 60 - (now - self.calls_made[0]).seconds
            time.sleep(sleep_time)
        
        self.calls_made.append(now)
        return func(*args, **kwargs)
```

#### 2. Runtime Errors

```python
# Error handling for runtime issues
class RobustAlgorithm(QCAlgorithm):
    def OnData(self, data):
        try:
            # Your algorithm logic here
            self.process_data(data)
        except Exception as e:
            self.Error(f"OnData error: {e}")
            self.emergency_risk_management()
    
    def emergency_risk_management(self):
        """Emergency procedures for algorithm errors"""
        
        # Close all positions if critical error
        for symbol in self.Portfolio.Keys:
            if self.Portfolio[symbol].Invested:
                self.Liquidate(symbol, "Emergency liquidation")
        
        # Send alert
        self.send_emergency_alert("Critical algorithm error - positions liquidated")
        
        # Stop algorithm
        self.Quit("Algorithm stopped due to critical error")
```

#### 3. Data Feed Issues

```python
class DataFeedValidator:
    def __init__(self, algorithm):
        self.algorithm = algorithm
        self.missing_data_count = {}
    
    def validate_data_feed(self, data, symbols):
        """Validate incoming data feed quality"""
        
        for symbol in symbols:
            if symbol not in data or data[symbol] is None:
                self.missing_data_count[symbol] = self.missing_data_count.get(symbol, 0) + 1
                
                if self.missing_data_count[symbol] > 10:
                    self.algorithm.Log(f"Warning: Missing data for {symbol} "
                                     f"({self.missing_data_count[symbol]} times)")
                    
                    # Remove symbol from universe temporarily
                    if symbol in self.algorithm.universe_symbols:
                        self.algorithm.universe_symbols.remove(symbol)
                        self.algorithm.Log(f"Temporarily removed {symbol} from universe")
```

### Performance Optimization

```python
# Algorithm performance optimization tips

class OptimizedAlgorithm(QCAlgorithm):
    def __init__(self):
        # Cache frequently used calculations
        self.feature_cache = {}
        self.calculation_cache = {}
        
        # Batch operations
        self.pending_orders = []
        self.batch_size = 10
    
    def calculate_features_optimized(self, symbol, data):
        """Optimized feature calculation with caching"""
        
        cache_key = f"{symbol}_{data.Time.date()}"
        
        if cache_key in self.feature_cache:
            return self.feature_cache[cache_key]
        
        # Calculate features
        features = self.calculate_features(symbol, data)
        
        # Cache results
        self.feature_cache[cache_key] = features
        
        # Limit cache size
        if len(self.feature_cache) > 1000:
            # Remove oldest entries
            oldest_keys = sorted(self.feature_cache.keys())[:100]
            for key in oldest_keys:
                del self.feature_cache[key]
        
        return features
    
    def batch_order_execution(self):
        """Execute orders in batches for efficiency"""
        
        if len(self.pending_orders) >= self.batch_size:
            for order_instruction in self.pending_orders:
                self.MarketOrder(
                    order_instruction['symbol'],
                    order_instruction['quantity'],
                    tag=order_instruction['tag']
                )
            
            self.pending_orders.clear()
```

### Monitoring Checklist

- [ ] Algorithm running status
- [ ] Data feed connectivity  
- [ ] Order execution latency
- [ ] Portfolio performance metrics
- [ ] Risk limit compliance
- [ ] Resource usage (CPU, memory)
- [ ] Network connectivity
- [ ] Brokerage account status
- [ ] Regulatory compliance
- [ ] Backup system status

This comprehensive deployment guide provides all the necessary steps and considerations for successfully deploying Unicorn Investing algorithms to QuantConnect's platform, from initial setup through live trading and ongoing maintenance.
