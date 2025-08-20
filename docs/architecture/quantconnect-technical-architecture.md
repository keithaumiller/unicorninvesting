# QuantConnect Technical Architecture

## 🏗️ Integration Architecture Overview

This document details the technical architecture for integrating Unicorn Investing's genetic algorithm + neural network optimization with QuantConnect's algorithmic trading platform.

## 📋 Architecture Components

### 1. Hybrid Platform Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                 Unicorn Investing Platform                  │
│                                                             │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────┐  │
│  │   Web Frontend  │  │  Backend APIs   │  │  ML Engine  │  │
│  │   (Drupal 11)   │◄─┤   (FastAPI)    │◄─┤   (Python)  │  │
│  └─────────────────┘  └─────────────────┘  └─────────────┘  │
│                              │                             │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │        QuantConnect Integration Layer                   │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │  │
│  │  │ Algorithm   │  │    Data     │  │   Portfolio     │  │  │
│  │  │ Deployment  │  │ Sync Bridge │  │  Sync Service   │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────────┘  │  │
│  └─────────────────────────────────────────────────────────┘  │
└─────────────────────────┬───────────────────────────────────┘
                          │ HTTPS/WebSocket
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                  QuantConnect Platform                      │
│                                                             │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────┐  │
│  │ LEAN Algorithm  │  │  Data Services  │  │  Execution  │  │
│  │    Framework    │◄─┤   (Market +     │◄─┤   Engine    │  │
│  │                 │  │  Fundamental)   │  │             │  │
│  └─────────────────┘  └─────────────────┘  └─────────────┘  │
│                                                             │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │                Live Trading                             │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │  │
│  │  │  Interactive│  │   Alpaca    │  │     Other       │  │  │
│  │  │   Brokers   │  │             │  │   Brokerages    │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────────┘  │  │
│  └─────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### 2. Data Flow Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Data Flow Pipeline                       │
└─────────────────────────────────────────────────────────────┘

Market Data Sources
        │
        ▼
┌─────────────────┐    Real-time     ┌─────────────────┐
│  QuantConnect   │◄─────────────────┤   Unicorn Data  │
│  Data Feeds     │     WebSocket    │   Processor     │
│  • Equities     │                  │  • Feature Eng  │
│  • Options      │                  │  • Normalization│
│  • Futures      │                  │  • ML Prep      │
│  • Forex        │                  └─────────────────┘
│  • Crypto       │                            │
│  • Fundamental  │                            ▼
└─────────────────┘                  ┌─────────────────┐
        │                            │  ML Optimization│
        ▼                            │  • Genetic Alg  │
┌─────────────────┐                  │  • Neural Net   │
│ LEAN Algorithm  │◄─────────────────┤  • Risk Mgmt    │
│ • Data Handler  │   Allocations    │  • Performance  │
│ • Order Mgmt    │                  └─────────────────┘
│ • Risk Controls │
│ • Execution     │
└─────────────────┘
        │
        ▼
┌─────────────────┐
│   Brokerage     │
│   Execution     │
│  • Order Routing│
│  • Settlement   │
│  • Reporting    │
└─────────────────┘
```

## 🔧 Technical Components

### 1. QuantConnect Integration Service

```python
# Main integration service
class QuantConnectIntegrationService:
    """
    Central service for QuantConnect platform integration.
    Handles algorithm deployment, data synchronization, and portfolio management.
    """
    
    def __init__(self, config: QuantConnectConfig):
        self.config = config
        self.api_client = QuantConnectAPIClient(config.api_key, config.user_id)
        self.algorithm_manager = AlgorithmManager()
        self.data_bridge = DataSynchronizationBridge()
        self.portfolio_sync = PortfolioSynchronizer()
        
    async def deploy_strategy(self, strategy: UnicornStrategy) -> str:
        """Deploy Unicorn strategy to QuantConnect platform"""
        algorithm_code = self.algorithm_manager.package_strategy(strategy)
        project_id = await self.api_client.create_project(
            name=f"Unicorn_{strategy.name}",
            language="Python"
        )
        
        await self.api_client.upload_algorithm(project_id, algorithm_code)
        deployment_id = await self.api_client.deploy_live(project_id, strategy.brokerage)
        
        return deployment_id
        
    async def sync_portfolio_state(self, strategy_id: str) -> PortfolioState:
        """Synchronize portfolio state between platforms"""
        qc_positions = await self.api_client.get_live_positions(strategy_id)
        unicorn_positions = await self.get_unicorn_positions(strategy_id)
        
        return await self.portfolio_sync.reconcile(qc_positions, unicorn_positions)
```

### 2. Algorithm Packaging System

```python
# Algorithm packaging for QuantConnect deployment
class AlgorithmPackager:
    """
    Converts Unicorn optimization strategies into QuantConnect algorithms.
    Handles serialization of ML models and optimization parameters.
    """
    
    def __init__(self):
        self.template_engine = Jinja2Environment()
        self.model_serializer = ModelSerializer()
        
    def package_ga_nn_strategy(self, strategy: GANeuralNetworkStrategy) -> str:
        """Package GA+NN strategy as QuantConnect algorithm"""
        
        # Serialize ML models
        ga_config = self.serialize_ga_config(strategy.genetic_algorithm)
        nn_model = self.serialize_neural_network(strategy.neural_network)
        feature_config = self.serialize_feature_config(strategy.feature_manager)
        
        # Generate algorithm code
        template = self.template_engine.get_template('ga_nn_algorithm.py.j2')
        algorithm_code = template.render(
            ga_config=ga_config,
            nn_model=nn_model,
            feature_config=feature_config,
            rebalance_frequency=strategy.rebalance_frequency,
            risk_management=strategy.risk_management
        )
        
        return algorithm_code
    
    def serialize_neural_network(self, nn_model: PortfolioNeuralNetwork) -> Dict:
        """Serialize TensorFlow model for deployment"""
        # Convert to TensorFlow Lite for efficiency
        converter = tf.lite.TFLiteConverter.from_keras_model(nn_model.model)
        tflite_model = converter.convert()
        
        return {
            'model_bytes': base64.b64encode(tflite_model).decode('utf-8'),
            'input_shape': nn_model.input_shape,
            'output_shape': nn_model.output_shape,
            'feature_names': nn_model.feature_names,
            'preprocessing_config': nn_model.preprocessing_config
        }
```

### 3. Real-time Data Synchronization

```python
# Real-time data bridge between platforms
class DataSynchronizationBridge:
    """
    Manages real-time data flow between Unicorn and QuantConnect.
    Handles data normalization, validation, and streaming.
    """
    
    def __init__(self):
        self.websocket_manager = WebSocketManager()
        self.data_validator = DataValidator()
        self.feature_processor = FeatureProcessor()
        
    async def start_data_stream(self, symbols: List[str], strategy_id: str):
        """Start real-time data streaming from QuantConnect"""
        
        # Establish WebSocket connection
        ws_url = f"wss://quantconnect.com/api/v2/live/{strategy_id}/data"
        await self.websocket_manager.connect(ws_url)
        
        # Subscribe to symbols
        subscription = {
            "action": "subscribe",
            "symbols": symbols,
            "resolution": "minute",
            "data_types": ["quotes", "trades", "bars"]
        }
        await self.websocket_manager.send(subscription)
        
        # Process incoming data
        async for message in self.websocket_manager.receive():
            processed_data = await self.process_market_data(message)
            await self.notify_optimization_engine(processed_data)
    
    async def process_market_data(self, raw_data: Dict) -> ProcessedMarketData:
        """Process and validate incoming market data"""
        
        # Validate data quality
        if not self.data_validator.validate(raw_data):
            raise DataValidationError(f"Invalid data: {raw_data}")
        
        # Convert to Unicorn format
        processed_data = ProcessedMarketData(
            symbol=raw_data['symbol'],
            timestamp=pd.to_datetime(raw_data['time']),
            price=raw_data['price'],
            volume=raw_data['volume'],
            bid=raw_data.get('bid'),
            ask=raw_data.get('ask')
        )
        
        # Calculate features
        features = await self.feature_processor.calculate_features(processed_data)
        processed_data.features = features
        
        return processed_data
```

### 4. Live Trading Algorithm Template

```python
# QuantConnect algorithm template for Unicorn strategies
class UnicornGANeuralNetworkAlgorithm(QCAlgorithm):
    """
    QuantConnect algorithm implementation of Unicorn's GA+NN optimization.
    This template is generated dynamically for each strategy deployment.
    """
    
    def Initialize(self):
        """Initialize algorithm with Unicorn strategy configuration"""
        
        # Set algorithm parameters
        self.SetStartDate({{ start_date }})
        self.SetCash({{ initial_cash }})
        self.SetBenchmark("SPY")
        
        # Initialize Unicorn components
        self.genetic_algorithm = self.load_genetic_algorithm()
        self.neural_network = self.load_neural_network()
        self.feature_calculator = FeatureCalculator({{ feature_config }})
        self.risk_manager = RiskManager({{ risk_config }})
        
        # Set up universe and data
        self.symbols = [self.AddEquity(symbol, Resolution.Minute).Symbol 
                       for symbol in {{ symbol_list }}]
        
        # Schedule rebalancing
        self.Schedule.On(
            self.DateRules.{{ rebalance_schedule }},
            self.TimeRules.At({{ rebalance_time }}),
            self.Rebalance
        )
        
        # Initialize tracking variables
        self.last_rebalance = self.Time
        self.current_allocations = {}
        self.performance_tracker = PerformanceTracker()
    
    def OnData(self, data):
        """Process real-time data and update features"""
        
        # Update feature calculations
        for symbol in self.symbols:
            if data.ContainsKey(symbol):
                price_data = data[symbol]
                self.feature_calculator.update(symbol, price_data)
        
        # Check if optimization should be triggered
        if self.should_reoptimize():
            self.TriggerOptimization()
    
    def Rebalance(self):
        """Execute portfolio rebalancing based on optimization"""
        
        try:
            # Calculate current features
            current_features = self.calculate_portfolio_features()
            
            # Run genetic algorithm optimization
            ga_result = self.genetic_algorithm.optimize(
                features=current_features,
                current_positions=self.get_current_positions(),
                market_conditions=self.assess_market_conditions()
            )
            
            # Neural network prediction
            nn_allocations = self.neural_network.predict(current_features)
            
            # Combine GA and NN results
            optimal_allocations = self.combine_optimization_results(
                ga_result, nn_allocations
            )
            
            # Apply risk management
            risk_adjusted_allocations = self.risk_manager.apply_constraints(
                optimal_allocations, self.Portfolio
            )
            
            # Execute trades
            self.execute_rebalancing(risk_adjusted_allocations)
            
            # Log performance
            self.log_rebalancing_event(risk_adjusted_allocations)
            
        except Exception as e:
            self.Error(f"Rebalancing failed: {str(e)}")
            # Implement fallback strategy
            self.execute_fallback_strategy()
    
    def execute_rebalancing(self, target_allocations: Dict[str, float]):
        """Execute portfolio rebalancing orders"""
        
        current_value = self.Portfolio.TotalPortfolioValue
        
        for symbol_str, target_weight in target_allocations.items():
            symbol = Symbol(symbol_str)
            target_value = current_value * target_weight
            
            # Calculate required position change
            current_holdings = self.Portfolio[symbol]
            current_value = current_holdings.HoldingsValue
            position_change = target_value - current_value
            
            if abs(position_change) > self.minimum_trade_size:
                # Place market order
                quantity = int(position_change / self.Securities[symbol].Price)
                if quantity != 0:
                    self.MarketOrder(symbol, quantity, 
                                   tag=f"Rebalance_{self.Time}")
    
    def load_genetic_algorithm(self) -> GeneticAlgorithm:
        """Load and initialize genetic algorithm"""
        config = GAConfig(**{{ ga_config }})
        return GeneticAlgorithm(config)
    
    def load_neural_network(self) -> NeuralNetworkPredictor:
        """Load pre-trained neural network model"""
        model_data = base64.b64decode("{{ nn_model_bytes }}")
        return NeuralNetworkPredictor.from_bytes(model_data)
```

### 5. Portfolio Synchronization System

```python
# Portfolio state synchronization between platforms
class PortfolioSynchronizer:
    """
    Manages portfolio state consistency between Unicorn and QuantConnect.
    Handles position reconciliation, trade synchronization, and state updates.
    """
    
    def __init__(self):
        self.position_tracker = PositionTracker()
        self.trade_reconciler = TradeReconciler()
        self.state_validator = StateValidator()
        
    async def reconcile_positions(self, 
                                qc_positions: Dict[str, Position],
                                unicorn_positions: Dict[str, Position]) -> ReconciliationResult:
        """Reconcile position differences between platforms"""
        
        differences = []
        
        # Find position discrepancies
        all_symbols = set(qc_positions.keys()) | set(unicorn_positions.keys())
        
        for symbol in all_symbols:
            qc_pos = qc_positions.get(symbol, Position.empty())
            unicorn_pos = unicorn_positions.get(symbol, Position.empty())
            
            if not self.positions_match(qc_pos, unicorn_pos):
                difference = PositionDifference(
                    symbol=symbol,
                    qc_quantity=qc_pos.quantity,
                    unicorn_quantity=unicorn_pos.quantity,
                    qc_value=qc_pos.market_value,
                    unicorn_value=unicorn_pos.market_value,
                    discrepancy_type=self.classify_discrepancy(qc_pos, unicorn_pos)
                )
                differences.append(difference)
        
        # Generate reconciliation actions
        actions = self.generate_reconciliation_actions(differences)
        
        return ReconciliationResult(
            differences=differences,
            actions=actions,
            is_reconciled=len(differences) == 0
        )
    
    def positions_match(self, pos1: Position, pos2: Position, 
                       tolerance: float = 0.01) -> bool:
        """Check if two positions match within tolerance"""
        quantity_diff = abs(pos1.quantity - pos2.quantity)
        value_diff = abs(pos1.market_value - pos2.market_value)
        
        return (quantity_diff < tolerance and 
                value_diff < tolerance * max(pos1.market_value, pos2.market_value))
```

### 6. Performance Monitoring System

```python
# Real-time performance monitoring
class PerformanceMonitor:
    """
    Monitors strategy performance across both platforms.
    Provides real-time analytics and alerting.
    """
    
    def __init__(self):
        self.metrics_calculator = MetricsCalculator()
        self.alert_manager = AlertManager()
        self.dashboard_updater = DashboardUpdater()
        
    async def monitor_strategy_performance(self, strategy_id: str):
        """Continuously monitor strategy performance"""
        
        while True:
            try:
                # Collect performance data
                qc_performance = await self.get_qc_performance(strategy_id)
                unicorn_performance = await self.get_unicorn_performance(strategy_id)
                
                # Calculate metrics
                metrics = self.metrics_calculator.calculate_comprehensive_metrics(
                    qc_performance, unicorn_performance
                )
                
                # Check for alerts
                alerts = self.check_performance_alerts(metrics)
                if alerts:
                    await self.alert_manager.send_alerts(alerts)
                
                # Update dashboard
                await self.dashboard_updater.update_real_time_metrics(
                    strategy_id, metrics
                )
                
                # Wait for next update cycle
                await asyncio.sleep(self.monitoring_interval)
                
            except Exception as e:
                self.logger.error(f"Performance monitoring error: {e}")
                await asyncio.sleep(self.error_retry_interval)
    
    def calculate_comprehensive_metrics(self, 
                                      qc_data: PerformanceData,
                                      unicorn_data: PerformanceData) -> Dict[str, float]:
        """Calculate comprehensive performance metrics"""
        
        return {
            'total_return': self.calculate_total_return(qc_data),
            'sharpe_ratio': self.calculate_sharpe_ratio(qc_data),
            'max_drawdown': self.calculate_max_drawdown(qc_data),
            'volatility': self.calculate_volatility(qc_data),
            'alpha': self.calculate_alpha(qc_data),
            'beta': self.calculate_beta(qc_data),
            'information_ratio': self.calculate_information_ratio(qc_data),
            'win_rate': self.calculate_win_rate(qc_data),
            'avg_trade_return': self.calculate_avg_trade_return(qc_data),
            'platform_sync_score': self.calculate_sync_score(qc_data, unicorn_data)
        }
```

## 🔒 Security Architecture

### Authentication & Authorization

```python
# Secure API integration
class SecureQuantConnectClient:
    """
    Secure client for QuantConnect API interactions.
    Implements OAuth 2.0, API key management, and request signing.
    """
    
    def __init__(self, config: SecurityConfig):
        self.config = config
        self.token_manager = TokenManager()
        self.request_signer = RequestSigner()
        
    async def authenticate(self) -> AuthToken:
        """Authenticate with QuantConnect using OAuth 2.0"""
        
        auth_url = f"{self.config.auth_endpoint}/oauth/authorize"
        token_response = await self.oauth_client.get_token(
            auth_url,
            client_id=self.config.client_id,
            client_secret=self.config.client_secret,
            scope="read write"
        )
        
        return AuthToken(
            access_token=token_response['access_token'],
            refresh_token=token_response['refresh_token'],
            expires_in=token_response['expires_in']
        )
    
    async def make_secure_request(self, endpoint: str, data: Dict) -> Dict:
        """Make authenticated and signed API request"""
        
        # Ensure valid token
        token = await self.token_manager.get_valid_token()
        
        # Sign request
        signed_request = self.request_signer.sign_request(
            method="POST",
            url=f"{self.config.api_base_url}/{endpoint}",
            data=data,
            timestamp=int(time.time()),
            nonce=self.generate_nonce()
        )
        
        # Add authentication headers
        headers = {
            'Authorization': f'Bearer {token.access_token}',
            'X-Signature': signed_request.signature,
            'X-Timestamp': str(signed_request.timestamp),
            'X-Nonce': signed_request.nonce
        }
        
        return await self.http_client.post(
            signed_request.url,
            data=signed_request.data,
            headers=headers
        )
```

### Data Encryption

```python
# Encryption for sensitive data
class DataEncryption:
    """
    Handles encryption of sensitive strategy data and model parameters.
    Uses AES-256 encryption with secure key management.
    """
    
    def __init__(self, key_manager: KeyManager):
        self.key_manager = key_manager
        self.cipher_suite = Fernet(key_manager.get_encryption_key())
        
    def encrypt_strategy_data(self, strategy: UnicornStrategy) -> EncryptedStrategy:
        """Encrypt sensitive strategy components"""
        
        # Serialize strategy
        strategy_data = pickle.dumps(strategy)
        
        # Encrypt data
        encrypted_data = self.cipher_suite.encrypt(strategy_data)
        
        return EncryptedStrategy(
            strategy_id=strategy.id,
            encrypted_data=encrypted_data,
            encryption_version=self.key_manager.version,
            created_at=datetime.utcnow()
        )
    
    def decrypt_strategy_data(self, encrypted_strategy: EncryptedStrategy) -> UnicornStrategy:
        """Decrypt strategy data"""
        
        # Get appropriate decryption key
        key = self.key_manager.get_key_by_version(encrypted_strategy.encryption_version)
        cipher_suite = Fernet(key)
        
        # Decrypt and deserialize
        decrypted_data = cipher_suite.decrypt(encrypted_strategy.encrypted_data)
        strategy = pickle.loads(decrypted_data)
        
        return strategy
```

## 📊 Monitoring & Observability

### Real-time Dashboards

```python
# Real-time monitoring dashboard
class RealTimeMonitoringDashboard:
    """
    Provides real-time monitoring and alerting for QuantConnect integration.
    Displays key metrics, performance data, and system health.
    """
    
    def __init__(self):
        self.metrics_collector = MetricsCollector()
        self.alert_engine = AlertEngine()
        self.visualization_engine = VisualizationEngine()
        
    async def start_monitoring(self):
        """Start real-time monitoring dashboard"""
        
        # Start metrics collection
        await self.metrics_collector.start_collection()
        
        # Initialize dashboard
        dashboard = self.create_dashboard_layout()
        
        # Start real-time updates
        while True:
            # Collect latest metrics
            current_metrics = await self.metrics_collector.get_latest_metrics()
            
            # Update visualizations
            await self.update_dashboard_visualizations(dashboard, current_metrics)
            
            # Check for alerts
            alerts = self.alert_engine.check_alerts(current_metrics)
            if alerts:
                await self.handle_alerts(alerts)
            
            # Wait for next update
            await asyncio.sleep(1)  # 1-second refresh rate
    
    def create_dashboard_layout(self) -> Dashboard:
        """Create comprehensive monitoring dashboard"""
        
        return Dashboard([
            PerformanceMetricsPanel(),
            PortfolioAllocationPanel(),
            RiskMetricsPanel(),
            TradingActivityPanel(),
            SystemHealthPanel(),
            AlertsPanel()
        ])
```

This technical architecture provides a comprehensive foundation for integrating Unicorn Investing with QuantConnect, ensuring scalable, secure, and high-performance algorithmic trading capabilities.
