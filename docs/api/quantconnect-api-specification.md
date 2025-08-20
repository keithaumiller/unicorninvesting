# QuantConnect API Specification

## 🔌 API Integration Overview

This document specifies the API interfaces for integrating Unicorn Investing with QuantConnect's platform, including REST endpoints, WebSocket streams, and data schemas.

## 📋 Table of Contents

1. [Authentication](#authentication)
2. [REST API Endpoints](#rest-api-endpoints)
3. [WebSocket Streams](#websocket-streams)
4. [Data Schemas](#data-schemas)
5. [Error Handling](#error-handling)
6. [Rate Limiting](#rate-limiting)
7. [Examples](#examples)

## 🔐 Authentication

### OAuth 2.0 Authentication Flow

```http
POST /oauth/token
Content-Type: application/x-www-form-urlencoded

grant_type=client_credentials
&client_id={client_id}
&client_secret={client_secret}
&scope=read write
```

**Response:**
```json
{
  "access_token": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "Bearer",
  "expires_in": 3600,
  "scope": "read write"
}
```

### API Key Authentication

```http
GET /api/v2/projects
Authorization: Bearer {access_token}
```

## 🌐 REST API Endpoints

### Project Management

#### Create Project
```http
POST /api/v2/projects/create
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "name": "Unicorn_GA_Strategy_001",
  "description": "Genetic Algorithm Portfolio Optimization",
  "language": "Python"
}
```

**Response:**
```json
{
  "projectId": 12345678,
  "name": "Unicorn_GA_Strategy_001",
  "created": "2025-08-20T10:30:00Z",
  "modified": "2025-08-20T10:30:00Z"
}
```

#### Upload Algorithm
```http
POST /api/v2/projects/{projectId}/files
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "name": "main.py",
  "content": "# Unicorn GA+NN Algorithm\nclass UnicornAlgorithm(QCAlgorithm):\n    def Initialize(self):\n        # Algorithm initialization\n        pass"
}
```

### Live Trading Management

#### Deploy Live Algorithm
```http
POST /api/v2/live/create
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "projectId": 12345678,
  "compileId": "5f4d2c1b8e9a3f7e6d8c9b2a",
  "brokerage": "InteractiveBrokersBrokerage",
  "dataFeed": "InteractiveBrokersDataFeed",
  "environment": "paper",
  "nodeType": "O1-8",
  "baseLiveAlgorithmSettings": {
    "id": "InteractiveBrokersBrokerage",
    "user": "",
    "password": "",
    "account": "",
    "host": "TESTBED",
    "port": "4001",
    "agentDescription": "Individual",
    "tradingMode": "paper"
  }
}
```

**Response:**
```json
{
  "projectId": 12345678,
  "deployId": "L-abc123def456",
  "status": "DeploymentInProgress",
  "launched": "2025-08-20T11:00:00Z"
}
```

#### Get Live Results
```http
GET /api/v2/live/{projectId}/read
Authorization: Bearer {access_token}
```

**Response:**
```json
{
  "projectId": 12345678,
  "deployId": "L-abc123def456",
  "status": "Running",
  "launched": "2025-08-20T11:00:00Z",
  "stopped": null,
  "holdings": [
    {
      "symbol": {
        "value": "AAPL",
        "id": "AAPL R735QTJ8XC9X",
        "permtick": "AAPL"
      },
      "quantity": 100,
      "averagePrice": 150.25,
      "marketPrice": 152.30,
      "marketValue": 15230.00,
      "unrealizedPnL": 205.00
    }
  ],
  "cash": 84770.00,
  "equity": 100000.00,
  "performance": {
    "totalReturn": 0.0023,
    "sharpeRatio": 1.45,
    "maxDrawdown": -0.0045,
    "winRate": 0.62
  }
}
```

### Portfolio Management

#### Get Portfolio Holdings
```http
GET /api/v2/live/{projectId}/holdings
Authorization: Bearer {access_token}
```

#### Place Orders
```http
POST /api/v2/live/{projectId}/orders
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "orders": [
    {
      "symbol": "AAPL",
      "quantity": 50,
      "orderType": "Market",
      "direction": "Buy",
      "tag": "Unicorn_Rebalance_20250820"
    },
    {
      "symbol": "GOOGL",
      "quantity": -25,
      "orderType": "Market", 
      "direction": "Sell",
      "tag": "Unicorn_Rebalance_20250820"
    }
  ]
}
```

### Data Access

#### Get Historical Data
```http
GET /api/v2/data/equity/AAPL/history
Authorization: Bearer {access_token}
?start=2024-01-01&end=2025-01-01&resolution=daily
```

#### Get Fundamental Data
```http
GET /api/v2/data/equity/AAPL/fundamental
Authorization: Bearer {access_token}
```

## 🔄 WebSocket Streams

### Real-time Data Streaming

#### Connection
```javascript
const ws = new WebSocket('wss://www.quantconnect.com/api/v2/live/{projectId}/stream');

ws.on('open', () => {
  // Subscribe to real-time data
  ws.send(JSON.stringify({
    action: 'subscribe',
    symbols: ['AAPL', 'GOOGL', 'MSFT'],
    dataTypes: ['quotes', 'trades', 'bars']
  }));
});
```

#### Market Data Messages
```json
{
  "type": "quote",
  "symbol": "AAPL",
  "timestamp": "2025-08-20T14:30:15.123Z",
  "bid": 151.85,
  "ask": 151.87,
  "bidSize": 500,
  "askSize": 300
}
```

```json
{
  "type": "trade",
  "symbol": "AAPL", 
  "timestamp": "2025-08-20T14:30:15.456Z",
  "price": 151.86,
  "volume": 100
}
```

```json
{
  "type": "bar",
  "symbol": "AAPL",
  "timestamp": "2025-08-20T14:30:00.000Z",
  "open": 151.50,
  "high": 152.10,
  "low": 151.40,
  "close": 151.86,
  "volume": 15420
}
```

### Portfolio Updates

#### Position Changes
```json
{
  "type": "position_update",
  "symbol": "AAPL",
  "timestamp": "2025-08-20T14:30:16.789Z",
  "quantity": 150,
  "averagePrice": 151.20,
  "marketPrice": 151.86,
  "unrealizedPnL": 99.00
}
```

#### Order Fills
```json
{
  "type": "order_fill",
  "orderId": "order_12345",
  "symbol": "AAPL",
  "timestamp": "2025-08-20T14:30:15.500Z",
  "fillQuantity": 50,
  "fillPrice": 151.86,
  "direction": "Buy",
  "tag": "Unicorn_Rebalance_20250820"
}
```

## 📊 Data Schemas

### UnicornStrategy Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "UnicornStrategy",
  "properties": {
    "strategyId": {
      "type": "string",
      "description": "Unique strategy identifier"
    },
    "name": {
      "type": "string",
      "description": "Human-readable strategy name"
    },
    "type": {
      "type": "string",
      "enum": ["GA_NN", "GA_ONLY", "NN_ONLY"],
      "description": "Strategy optimization type"
    },
    "configuration": {
      "type": "object",
      "properties": {
        "geneticAlgorithm": {
          "$ref": "#/definitions/GAConfig"
        },
        "neuralNetwork": {
          "$ref": "#/definitions/NNConfig"
        },
        "riskManagement": {
          "$ref": "#/definitions/RiskConfig"
        },
        "rebalanceFrequency": {
          "type": "string",
          "enum": ["daily", "weekly", "monthly"],
          "description": "Portfolio rebalancing frequency"
        }
      }
    },
    "universe": {
      "type": "array",
      "items": {
        "type": "string"
      },
      "description": "List of tradeable symbols"
    },
    "constraints": {
      "type": "object",
      "properties": {
        "maxPositionSize": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Maximum position size as portfolio percentage"
        },
        "minPositionSize": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Minimum position size as portfolio percentage"
        },
        "maxTurnover": {
          "type": "number",
          "minimum": 0,
          "description": "Maximum daily turnover rate"
        }
      }
    }
  },
  "required": ["strategyId", "name", "type", "configuration", "universe"],
  
  "definitions": {
    "GAConfig": {
      "type": "object",
      "properties": {
        "populationSize": {
          "type": "integer",
          "minimum": 10,
          "maximum": 1000,
          "default": 100
        },
        "generations": {
          "type": "integer",
          "minimum": 10,
          "maximum": 1000,
          "default": 200
        },
        "mutationRate": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "default": 0.1
        },
        "crossoverRate": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "default": 0.8
        },
        "fitnessFunction": {
          "type": "string",
          "enum": ["sharpe_ratio", "sortino_ratio", "calmar_ratio"],
          "default": "sharpe_ratio"
        }
      }
    },
    
    "NNConfig": {
      "type": "object",
      "properties": {
        "architecture": {
          "type": "array",
          "items": {
            "type": "integer",
            "minimum": 1
          },
          "description": "Neural network layer sizes"
        },
        "activation": {
          "type": "string",
          "enum": ["relu", "tanh", "sigmoid"],
          "default": "relu"
        },
        "learningRate": {
          "type": "number",
          "minimum": 0.0001,
          "maximum": 1,
          "default": 0.001
        },
        "epochs": {
          "type": "integer",
          "minimum": 10,
          "maximum": 1000,
          "default": 100
        },
        "batchSize": {
          "type": "integer",
          "minimum": 1,
          "maximum": 1000,
          "default": 32
        }
      }
    },
    
    "RiskConfig": {
      "type": "object",
      "properties": {
        "maxDrawdown": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Maximum allowable drawdown"
        },
        "volatilityTarget": {
          "type": "number",
          "minimum": 0,
          "description": "Target portfolio volatility"
        },
        "riskBudget": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Maximum risk budget allocation"
        }
      }
    }
  }
}
```

### Portfolio Performance Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "PortfolioPerformance",
  "properties": {
    "strategyId": {
      "type": "string"
    },
    "timestamp": {
      "type": "string",
      "format": "date-time"
    },
    "equity": {
      "type": "number",
      "description": "Total portfolio equity"
    },
    "cash": {
      "type": "number",
      "description": "Available cash"
    },
    "totalReturn": {
      "type": "number",
      "description": "Total return since inception"
    },
    "dailyReturn": {
      "type": "number",
      "description": "Daily return"
    },
    "sharpeRatio": {
      "type": "number",
      "description": "Sharpe ratio"
    },
    "maxDrawdown": {
      "type": "number",
      "description": "Maximum drawdown"
    },
    "volatility": {
      "type": "number",
      "description": "Annualized volatility"
    },
    "beta": {
      "type": "number",
      "description": "Beta vs benchmark"
    },
    "alpha": {
      "type": "number",
      "description": "Alpha vs benchmark"
    },
    "winRate": {
      "type": "number",
      "description": "Percentage of profitable periods"
    },
    "positions": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/Position"
      }
    }
  },
  
  "definitions": {
    "Position": {
      "type": "object",
      "properties": {
        "symbol": {
          "type": "string"
        },
        "quantity": {
          "type": "number"
        },
        "averagePrice": {
          "type": "number"
        },
        "marketPrice": {
          "type": "number"
        },
        "marketValue": {
          "type": "number"
        },
        "unrealizedPnL": {
          "type": "number"
        },
        "weight": {
          "type": "number",
          "description": "Position weight in portfolio"
        }
      }
    }
  }
}
```

## ❌ Error Handling

### Error Response Format

```json
{
  "error": {
    "code": "INVALID_REQUEST",
    "message": "The request contains invalid parameters",
    "details": {
      "field": "projectId",
      "value": "invalid_id",
      "expected": "Valid project identifier"
    },
    "timestamp": "2025-08-20T14:30:00Z",
    "requestId": "req_abc123def456"
  }
}
```

### Error Codes

| Code | Description | HTTP Status |
|------|-------------|-------------|
| `AUTHENTICATION_FAILED` | Invalid or expired authentication credentials | 401 |
| `AUTHORIZATION_DENIED` | Insufficient permissions for requested operation | 403 |
| `INVALID_REQUEST` | Request contains invalid parameters or format | 400 |
| `RESOURCE_NOT_FOUND` | Requested resource does not exist | 404 |
| `RATE_LIMIT_EXCEEDED` | API rate limit exceeded | 429 |
| `INTERNAL_ERROR` | Internal server error occurred | 500 |
| `SERVICE_UNAVAILABLE` | Service temporarily unavailable | 503 |
| `DEPLOYMENT_FAILED` | Algorithm deployment failed | 422 |
| `INSUFFICIENT_FUNDS` | Insufficient funds for requested operation | 402 |
| `MARKET_CLOSED` | Operation not allowed when market is closed | 409 |

### Retry Logic

```python
import asyncio
from typing import Dict, Optional

class QuantConnectAPIClient:
    async def make_request_with_retry(self, 
                                    endpoint: str, 
                                    data: Dict,
                                    max_retries: int = 3) -> Optional[Dict]:
        """Make API request with exponential backoff retry logic"""
        
        for attempt in range(max_retries + 1):
            try:
                response = await self.make_request(endpoint, data)
                return response
                
            except APIError as e:
                if e.code in ['RATE_LIMIT_EXCEEDED', 'SERVICE_UNAVAILABLE']:
                    if attempt < max_retries:
                        wait_time = 2 ** attempt  # Exponential backoff
                        await asyncio.sleep(wait_time)
                        continue
                raise e
                
        raise APIError("MAX_RETRIES_EXCEEDED", "Failed after maximum retry attempts")
```

## 🚦 Rate Limiting

### Rate Limits

| Endpoint Category | Requests per Minute | Requests per Hour |
|------------------|-------------------|------------------|
| Authentication | 10 | 100 |
| Project Management | 60 | 1000 |
| Live Trading | 120 | 2000 |
| Data Access | 600 | 10000 |
| WebSocket Connections | 5 | 50 |

### Rate Limit Headers

```http
HTTP/1.1 200 OK
X-RateLimit-Limit: 60
X-RateLimit-Remaining: 45
X-RateLimit-Reset: 1692537600
```

## 💡 Examples

### Complete Strategy Deployment Example

```python
import asyncio
from typing import Dict
from quantconnect_client import QuantConnectClient
from unicorn_strategy import UnicornGANeuralNetworkStrategy

class UnicornQuantConnectIntegration:
    def __init__(self, api_key: str, user_id: str):
        self.qc_client = QuantConnectClient(api_key, user_id)
        
    async def deploy_unicorn_strategy(self, strategy: UnicornGANeuralNetworkStrategy) -> str:
        """Complete example of deploying Unicorn strategy to QuantConnect"""
        
        # 1. Create project
        project = await self.qc_client.create_project(
            name=f"Unicorn_{strategy.name}",
            description=f"GA+NN Strategy: {strategy.description}"
        )
        
        # 2. Package algorithm
        algorithm_code = self.package_strategy_for_quantconnect(strategy)
        
        # 3. Upload algorithm files
        await self.qc_client.upload_files(project.id, {
            "main.py": algorithm_code,
            "config.json": strategy.to_json(),
            "requirements.txt": self.get_requirements()
        })
        
        # 4. Compile project
        compile_result = await self.qc_client.compile_project(project.id)
        if not compile_result.success:
            raise CompilationError(f"Compilation failed: {compile_result.errors}")
        
        # 5. Deploy to live trading
        deployment = await self.qc_client.deploy_live(
            project_id=project.id,
            compile_id=compile_result.compile_id,
            brokerage="InteractiveBrokersBrokerage",
            environment="paper"  # Start with paper trading
        )
        
        # 6. Monitor deployment
        while deployment.status == "DeploymentInProgress":
            await asyncio.sleep(5)
            deployment = await self.qc_client.get_deployment_status(deployment.id)
        
        if deployment.status != "Running":
            raise DeploymentError(f"Deployment failed: {deployment.error}")
        
        return deployment.id
    
    def package_strategy_for_quantconnect(self, strategy: UnicornGANeuralNetworkStrategy) -> str:
        """Convert Unicorn strategy to QuantConnect algorithm"""
        
        template = """
from AlgorithmImports import *
import numpy as np
import pandas as pd
from typing import Dict, List
import json
import base64

class UnicornAlgorithm(QCAlgorithm):
    def Initialize(self):
        # Set algorithm parameters
        self.SetStartDate(2024, 1, 1)
        self.SetCash(1000000)
        self.SetBenchmark("SPY")
        
        # Load strategy configuration
        self.config = self.load_strategy_config()
        
        # Initialize Unicorn components
        self.genetic_algorithm = self.initialize_genetic_algorithm()
        self.neural_network = self.initialize_neural_network()
        self.feature_calculator = FeatureCalculator(self.config['features'])
        
        # Add universe
        self.symbols = []
        for symbol in self.config['universe']:
            equity = self.AddEquity(symbol, Resolution.Minute)
            self.symbols.append(equity.Symbol)
        
        # Schedule rebalancing
        self.Schedule.On(
            self.DateRules.WeeklyOnDay(DayOfWeek.Monday),
            self.TimeRules.At(9, 31),
            self.Rebalance
        )
        
        # Initialize tracking
        self.last_rebalance = self.Time
        self.performance_tracker = PerformanceTracker()
    
    def load_strategy_config(self) -> Dict:
        # Load configuration from uploaded file
        config_content = self.Download("config.json")
        return json.loads(config_content)
    
    def OnData(self, data):
        # Update features with new data
        for symbol in self.symbols:
            if data.ContainsKey(symbol) and data[symbol] is not None:
                self.feature_calculator.update(symbol, data[symbol])
    
    def Rebalance(self):
        try:
            # Calculate features
            features = self.calculate_current_features()
            
            # Run optimization
            ga_result = self.genetic_algorithm.optimize(features)
            nn_allocation = self.neural_network.predict(features)
            
            # Combine results
            target_allocation = self.combine_optimizations(ga_result, nn_allocation)
            
            # Execute trades
            self.execute_portfolio_rebalance(target_allocation)
            
            # Log performance
            self.performance_tracker.log_rebalance(target_allocation)
            
        except Exception as e:
            self.Error(f"Rebalancing failed: {str(e)}")
    
    def calculate_current_features(self) -> np.ndarray:
        # Feature calculation logic
        pass
    
    def execute_portfolio_rebalance(self, target_allocation: Dict[str, float]):
        portfolio_value = self.Portfolio.TotalPortfolioValue
        
        for symbol_str, weight in target_allocation.items():
            symbol = Symbol(symbol_str)
            target_value = portfolio_value * weight
            current_value = self.Portfolio[symbol].HoldingsValue
            
            difference = target_value - current_value
            if abs(difference) > 1000:  # Minimum trade size
                price = self.Securities[symbol].Price
                quantity = int(difference / price)
                if quantity != 0:
                    self.MarketOrder(symbol, quantity, tag="Rebalance")
"""
        
        return template
    
    async def monitor_strategy_performance(self, deployment_id: str):
        """Monitor deployed strategy performance"""
        
        while True:
            try:
                # Get live results
                results = await self.qc_client.get_live_results(deployment_id)
                
                # Calculate performance metrics
                performance = self.calculate_performance_metrics(results)
                
                # Check for alerts
                alerts = self.check_performance_alerts(performance)
                if alerts:
                    await self.send_alerts(alerts)
                
                # Update dashboard
                await self.update_performance_dashboard(deployment_id, performance)
                
                # Wait for next update
                await asyncio.sleep(60)  # Update every minute
                
            except Exception as e:
                self.logger.error(f"Performance monitoring error: {e}")
                await asyncio.sleep(300)  # Wait 5 minutes before retry

# Usage example
async def main():
    # Initialize client
    client = UnicornQuantConnectIntegration(
        api_key="your_api_key",
        user_id="your_user_id"
    )
    
    # Create strategy
    strategy = UnicornGANeuralNetworkStrategy(
        name="TechStockGA",
        universe=["AAPL", "GOOGL", "MSFT", "AMZN", "TSLA"],
        ga_config=GAConfig(population_size=50, generations=100),
        nn_config=NNConfig(hidden_layers=[64, 32, 16])
    )
    
    # Deploy strategy
    deployment_id = await client.deploy_unicorn_strategy(strategy)
    print(f"Strategy deployed successfully: {deployment_id}")
    
    # Monitor performance
    await client.monitor_strategy_performance(deployment_id)

if __name__ == "__main__":
    asyncio.run(main())
```

This API specification provides a complete interface for integrating Unicorn Investing's optimization algorithms with QuantConnect's trading platform, enabling sophisticated algorithmic trading strategies with institutional-grade execution capabilities.
