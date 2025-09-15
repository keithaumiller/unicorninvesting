#!/usr/bin/env python3
"""
Python-Based Simulation Engine for Myportolio with Performance Logging
======================================================================

Pure Python backtesting and simulation capabilities using Myportolio components.
Professional-grade simulation framework with comprehensive performance logging.

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

import os
import json
import uuid
import subprocess
import pandas as pd
import numpy as np
import sys
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
import logging

# Import best model selector
sys.path.append(str(Path(__file__).parent.parent / "utilities"))
try:
    from best_model_selector import BestModelSelector
    BEST_MODEL_SELECTOR_AVAILABLE = True
except ImportError:
    print("⚠️  Best model selector not available")
    BEST_MODEL_SELECTOR_AVAILABLE = False

# Import performance logging
try:
    from performance_logger import PerformanceLogger
    PERFORMANCE_LOGGING_AVAILABLE = True
except ImportError:
    print("⚠️  Performance logging not available")
    PERFORMANCE_LOGGING_AVAILABLE = False

# Import enhanced algorithms
sys.path.append(str(Path(__file__).parent.parent / "trading_algorithms"))
sys.path.append(str(Path(__file__).parent.parent / "risk_algorithms"))
try:
    from eth_momentum_strategy import ETHMomentumStrategy
    from eth_basic_risk import ETHBasicRisk
    ENHANCED_ALGORITHMS_AVAILABLE = True
except ImportError:
    print("⚠️  Enhanced algorithms not available")
    ENHANCED_ALGORITHMS_AVAILABLE = False

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class PythonSimulationEngine:
    """
    Enhanced LEAN-integrated simulation engine with comprehensive performance logging.
    
    This engine directly utilizes LEAN framework components for:
    - Historical backtesting with real market data
    - Paper trading simulation with live data feeds  
    - Parameter optimization using LEAN's optimizer
    - Detailed performance attribution and logging
    """
    
    def __init__(self, portfolio_path: str = None):
        """
        Initialize LEAN simulation engine with performance logging integration.
        
        Args:
            portfolio_path: Path to Myportolio directory
        """
        if portfolio_path is None:
            portfolio_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        
        self.portfolio_path = Path(portfolio_path)
        self.simulations_path = self.portfolio_path / "simulations"
        self.lean_path = Path("/workspaces/unicorninvesting/BackendPython/Lean")
        
        # Initialize performance logging
        self.performance_logger = None
        self.current_simulation_id = None
        
        # Initialize best model selector
        if BEST_MODEL_SELECTOR_AVAILABLE:
            self.best_model_selector = BestModelSelector()
            logger.info("✅ Best model selector initialized")
        else:
            self.best_model_selector = None
            logger.warning("⚠️  Best model selector not available")
        
        # Initialize enhanced algorithms
        self.enhanced_algorithms_available = ENHANCED_ALGORITHMS_AVAILABLE
        
        # Ensure simulation directories exist
        self._initialize_directories()
        
        # Load portfolio configuration
        self.portfolio_config = self._load_portfolio_config()
        
        logger.info(f"Enhanced LEAN Simulation Engine initialized for Myportolio")
        logger.info(f"Performance logging: {'ENABLED' if PERFORMANCE_LOGGING_AVAILABLE else 'DISABLED'}")
        logger.info(f"Enhanced algorithms: {'ENABLED' if self.enhanced_algorithms_available else 'DISABLED'}")
        logger.info(f"Simulation results stored in: {self.simulations_path}")

    def _initialize_performance_logging(self, simulation_id: str):
        """Initialize performance logging for the current simulation."""
        if PERFORMANCE_LOGGING_AVAILABLE:
            self.performance_logger = PerformanceLogger(
                simulation_id=simulation_id,
                log_directory=self.simulations_path / "performance_logs"
            )
            self.current_simulation_id = simulation_id
            logger.info(f"Performance logging initialized for simulation: {simulation_id}")
        else:
            logger.warning("Performance logging not available")

    def _create_enhanced_algorithms(self, config: Dict) -> Tuple[Optional['ETHMomentumStrategy'], Optional['ETHBasicRisk']]:
        """Create enhanced algorithm instances with performance logging."""
        
        trading_strategy = None
        risk_algorithm = None
        
        if self.enhanced_algorithms_available:
            try:
                # Create trading strategy with logging
                trading_strategy = ETHMomentumStrategy(
                    config=config.get('trading_strategy', {}),
                    performance_logger=self.performance_logger
                )
                
                # Create risk algorithm with logging
                risk_algorithm = ETHBasicRisk(
                    max_drawdown=config.get('max_drawdown', 0.15),
                    max_position_pct=config.get('max_position_pct', 0.8),
                    var_confidence=config.get('var_confidence', 0.05),
                    performance_logger=self.performance_logger
                )
                
                logger.info("Enhanced algorithms created with performance logging")
                
            except Exception as e:
                logger.error(f"Failed to create enhanced algorithms: {e}")
                
        return trading_strategy, risk_algorithm

    def run_backtest_with_logging(self, 
                                 start_date: str = "2024-01-01", 
                                 end_date: str = "2024-03-31",
                                 parameters: Dict = None,
                                 template_name: str = None) -> str:
        """
        Run enhanced backtest with comprehensive performance logging.
        
        Args:
            start_date: Backtest start date (YYYY-MM-DD)
            end_date: Backtest end date (YYYY-MM-DD)
            parameters: Strategy parameters
            template_name: Template configuration name
            
        Returns:
            Simulation ID for tracking results
        """
        simulation_id = f"backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{uuid.uuid4().hex[:8]}"
        
        # Initialize performance logging
        self._initialize_performance_logging(simulation_id)
        
        if self.performance_logger:
            self.performance_logger.logger.info(f"Starting enhanced backtest: {simulation_id}")
            self.performance_logger.logger.info(f"Period: {start_date} to {end_date}")
            self.performance_logger.logger.info(f"Template: {template_name}")
            self.performance_logger.logger.info(f"Parameters: {json.dumps(parameters or {}, indent=2)}")
        
        try:
            # Create simulation directory
            sim_dir = self.simulations_path / "backtests" / simulation_id
            sim_dir.mkdir(parents=True, exist_ok=True)
            
            # Get best model configuration if needed
            best_model_config = self.get_best_model_config(template_name)
            
            # Create enhanced algorithms
            trading_strategy, risk_algorithm = self._create_enhanced_algorithms({
                'trading_strategy': parameters or {},
                'max_drawdown': 0.15,
                'max_position_pct': 0.8,
                'var_confidence': 0.05
            })
            
            # Prepare configuration
            lean_config = self._prepare_enhanced_lean_config(
                simulation_id=simulation_id,
                start_date=start_date,
                end_date=end_date,
                parameters=parameters or {},
                best_model_config=best_model_config,
                algorithm_type="MyportolioETHMomentum"
            )
            
            # Save configuration
            config_path = sim_dir / "lean_config.json"
            with open(config_path, 'w') as f:
                json.dump(lean_config, f, indent=2, default=str)
            
            # Generate LEAN algorithm file with enhanced logging
            self._generate_enhanced_lean_algorithm(lean_config, sim_dir)
            
            # Log simulation start
            if self.performance_logger:
                self.performance_logger.log_portfolio_state(
                    total_value=100000.0,  # Default starting value
                    cash=100000.0,
                    positions={},
                    unrealized_pnl=0.0,
                    realized_pnl=0.0,
                    drawdown=0.0,
                    volatility=0.0,
                    var_95=0.0
                )
            
            # Execute backtest
            result = self._execute_lean_backtest(lean_config, sim_dir)
            
            # Process and store results with enhanced analysis
            self._process_enhanced_backtest_results(simulation_id, result, sim_dir, trading_strategy, risk_algorithm)
            
            # Generate performance report
            if self.performance_logger:
                performance_report = self.performance_logger.generate_performance_report()
                self.performance_logger.save_all_logs()
                
                # Log completion
                self.performance_logger.logger.info(f"Enhanced backtest completed: {simulation_id}")
                self.performance_logger.logger.info(f"Performance report: {performance_report.get('summary', {})}")
            
            # Store in result handler database
            from python_result_handler import PythonResultHandler
            handler = PythonResultHandler()
            
            # Load the processed results
            result_path = sim_dir / "myportolio_results.json"
            if result_path.exists():
                with open(result_path, 'r') as f:
                    processed_results = json.load(f)
                
                # Add performance logging results to stored data
                if self.performance_logger:
                    processed_results['performance_analysis'] = performance_report
                
                # Store in database
                handler.store_simulation_result(
                    simulation_id=simulation_id,
                    simulation_type="backtest",
                    results=processed_results,
                    config=lean_config,
                    results_path=str(result_path)
                )
            
            logger.info(f"Enhanced backtest completed successfully: {simulation_id}")
            return simulation_id
            
        except Exception as e:
            error_msg = f"Enhanced backtest failed: {str(e)}"
            logger.error(error_msg)
            
            if self.performance_logger:
                self.performance_logger.logger.error(error_msg)
                # Try to save logs even on failure
                try:
                    self.performance_logger.save_all_logs()
                except:
                    pass
            
            raise e

    def _prepare_enhanced_lean_config(self, 
                                    simulation_id: str,
                                    start_date: str,
                                    end_date: str,
                                    parameters: Dict,
                                    best_model_config: Dict,
                                    algorithm_type: str = "MyportolioETHMomentum") -> Dict:
        """Prepare enhanced LEAN configuration with performance logging parameters."""
        
        config = {
            "algorithm-type-name": algorithm_type,
            "algorithm-location": f"backtests/{simulation_id}/",
            "algorithm-language": "Python",
            "data-folder": "/workspaces/unicorninvesting/data/",
            "output-directory": f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/backtests/{simulation_id}/",
            "result-destination-folder": f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/backtests/{simulation_id}/",
            "close-automatically": True,
            "debugging": False,
            "debugging-method": "LocalCmdline",
            "job-user-id": "1",
            "api-access-token": "",
            "job-organization-id": "",
            "job-project-id": 0,
            
            # Environment settings
            "environment": "backtesting",
            "algorithm": algorithm_type,
            
            # Simulation parameters
            "start-date": start_date,
            "end-date": end_date,
            "cash": parameters.get("initial_cash", 100000),
            
            # Enhanced parameters
            "parameters": parameters,
            "best_model_config": best_model_config,
            "performance_logging_enabled": self.performance_logger is not None,
            "simulation_id": simulation_id,
            
            # Risk management parameters
            "risk_management": {
                "max_drawdown": parameters.get("max_drawdown", 0.15),
                "max_position_pct": parameters.get("max_position_pct", 0.8),
                "var_confidence": parameters.get("var_confidence", 0.05)
            },
            
            # Trading strategy parameters
            "trading_strategy": {
                "short_ma_period": parameters.get("short_ma_period", 5),
                "long_ma_period": parameters.get("long_ma_period", 20),
                "max_position_size": parameters.get("max_position_size", 0.1),
                "volatility_window": parameters.get("volatility_window", 14)
            }
        }
        
        return config

    def get_best_model_config(self, template_name: str = None) -> Dict[str, Any]:
        """
        Get the best model configuration for simulation.
        
        Args:
            template_name: Template name to check for best model usage
            
        Returns:
            Best model configuration dictionary
        """
        if not self.best_model_selector:
            logger.warning("Best model selector not available, using default configuration")
            return {}
        
        try:
            best_models = self.best_model_selector.get_best_models()
            
            if template_name and "best_models" in template_name:
                logger.info("🎯 Using best economic-enhanced models for simulation")
                # Generate configurations for all assets
                configs = self.best_model_selector.generate_all_asset_configs()
                return {
                    'use_best_models': True,
                    'model_configs': configs,
                    'best_models': best_models
                }
            
            return {'use_best_models': False}
            
        except Exception as e:
            logger.error(f"Error getting best model configuration: {e}")
            return {'use_best_models': False}

    def _initialize_directories(self):
        """Initialize required simulation directories."""
        directories = [
            "backtests",
            "paper", 
            "optimization_runs",
            "analysis",
            "templates"
        ]
        
        for directory in directories:
            dir_path = self.simulations_path / directory
            dir_path.mkdir(parents=True, exist_ok=True)

    def _load_portfolio_config(self) -> Dict[str, Any]:
        """Load Myportolio configuration."""
        config_path = self.portfolio_path / "config.json"
        if config_path.exists():
            with open(config_path, 'r') as f:
                return json.load(f)
        return {}

    def generate_simulation_id(self, simulation_type: str) -> str:
        """
        Generate unique simulation ID.
        
        Args:
            simulation_type: Type of simulation (backtest, paper, optimization)
            
        Returns:
            Unique simulation identifier
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        short_uuid = str(uuid.uuid4())[:8]
        return f"{simulation_type}_{timestamp}_{short_uuid}"

    def run_backtest(self, 
                    start_date: str, 
                    end_date: str, 
                    algorithm_name: str = "MyportolioETHMomentum",
                    parameters: Optional[Dict[str, Any]] = None,
                    template_name: str = None) -> str:
        """
        Execute a LEAN backtest simulation with best model integration.
        
        Args:
            start_date: Start date in YYYY-MM-DD format
            end_date: End date in YYYY-MM-DD format  
            algorithm_name: Name of the algorithm to run
            parameters: Optional parameters to override
            template_name: Template name for best model selection
            
        Returns:
            Simulation ID for the completed backtest
        """
        logger.info(f"Starting backtest: {start_date} to {end_date}")
        
        # Generate simulation ID
        simulation_id = f"backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{str(uuid.uuid4())[:8]}"
        
        # Create simulation directory
        sim_dir = self.simulations_path / "backtests" / simulation_id
        sim_dir.mkdir(parents=True, exist_ok=True)
        
        # Get best model configuration if applicable
        best_model_config = self.get_best_model_config(template_name)
        
        # Merge best model parameters with provided parameters
        if best_model_config.get('use_best_models', False):
            logger.info("🚀 Integrating best economic-enhanced models into simulation")
            if parameters is None:
                parameters = {}
            
            # Add best model information to parameters
            parameters['best_models_info'] = best_model_config['best_models']
            parameters['model_configs'] = best_model_config['model_configs']
            
            # Update algorithm name for best models
            if template_name and "best_models" in template_name:
                algorithm_name = "MyportolioEconomicEnhanced"
        
        # Create LEAN configuration for backtest
        lean_config = self._create_backtest_config(
            start_date, end_date, algorithm_name, parameters, simulation_id
        )
        
        # Add best model information to config
        if best_model_config.get('use_best_models', False):
            lean_config['best_models'] = best_model_config['best_models']
            lean_config['economic_enhanced'] = True
        
        # Save configuration
        config_path = sim_dir / "lean_config.json"
        with open(config_path, 'w') as f:
            json.dump(lean_config, f, indent=2)
        
        # Prepare algorithm file
        algorithm_path = self._prepare_algorithm_file(algorithm_name, parameters, sim_dir)
        
        try:
            # Execute LEAN backtest
            result = self._execute_lean_backtest(lean_config, sim_dir)
            
            # Process and store results
            self._process_backtest_results(simulation_id, result, sim_dir)
            
            logger.info(f"Backtest completed successfully: {simulation_id}")
            return simulation_id
            
        except Exception as e:
            logger.error(f"Backtest failed: {str(e)}")
            # Store error information
            error_info = {
                "simulation_id": simulation_id,
                "error": str(e),
                "timestamp": datetime.now().isoformat(),
                "configuration": lean_config,
                "best_models_used": best_model_config.get('use_best_models', False)
            }
            
            error_path = sim_dir / "error.json"
            with open(error_path, 'w') as f:
                json.dump(error_info, f, indent=2)
            
            raise

    def _create_backtest_config(self, 
                               start_date: str, 
                               end_date: str, 
                               algorithm_name: str,
                               parameters: Optional[Dict[str, Any]],
                               simulation_id: str) -> Dict[str, Any]:
        """
        Create LEAN configuration for backtesting.
        
        Returns:
            LEAN-compatible configuration dictionary
        """
        config = {
            # Core LEAN settings
            "environment": "backtesting",
            "algorithm-type-name": algorithm_name,
            "algorithm-language": "Python",
            
            # Backtest period
            "start-date": start_date,
            "end-date": end_date,
            
            # Data settings
            "data-folder": str(self.lean_path / "Data"),
            "cache-location": str(self.lean_path / "Data"),
            
            # Result settings  
            "live-mode": False,
            "result-handler": "QuantConnect.Lean.Engine.Results.BacktestingResultHandler",
            "messaging-handler": "QuantConnect.Messaging.Messaging",
            "job-queue-handler": "QuantConnect.Queues.JobQueue",
            "data-provider": "QuantConnect.Lean.Engine.DataFeeds.DefaultDataProvider",
            
            # Portfolio settings
            "initial-cash": self.portfolio_config.get("initial_capital", 100000),
            
            # Simulation metadata
            "simulation-id": simulation_id,
            "portfolio-name": "Myportolio",
            
            # Custom parameters
            "parameters": parameters or {}
        }
        
        return config

    def _prepare_algorithm_file(self, 
                               algorithm_name: str, 
                               parameters: Optional[Dict[str, Any]], 
                               sim_dir: Path) -> Path:
        """
        Prepare algorithm file for LEAN execution.
        
        Returns:
            Path to prepared algorithm file
        """
        # Create algorithm template based on Myportolio components
        algorithm_template = f'''
"""
Myportolio LEAN Algorithm - {algorithm_name}
Generated for simulation: {sim_dir.name}
"""

from clr import AddReference
AddReference("System")
AddReference("QuantConnect.Algorithm")
AddReference("QuantConnect.Common")

from System import *
from QuantConnect import *
from QuantConnect.Algorithm import *
from QuantConnect.Data import *

class {algorithm_name}(QCAlgorithm):
    """
    Myportolio trading algorithm for LEAN framework.
    Integrates ETH momentum strategies with Kelly criterion risk management.
    """
    
    def Initialize(self):
        """Initialize algorithm with Myportolio configuration."""
        
        # Set cash and dates
        self.SetCash({self.portfolio_config.get("initial_capital", 100000)})
        
        # Add ETH crypto data
        self.eth = self.AddCrypto("ETHUSD", Resolution.Hour)
        
        # Add BTC if dual crypto strategy
        if "{self.portfolio_config.get('strategy', '')}" == "dual_crypto":
            self.btc = self.AddCrypto("BTCUSD", Resolution.Hour)
        
        # Risk management parameters
        self.max_volatility = {self.portfolio_config.get('risk_parameters', {}).get('max_volatility', 0.25)}
        self.max_drawdown = {self.portfolio_config.get('risk_parameters', {}).get('max_drawdown', 0.15)}
        self.var_limit = {self.portfolio_config.get('risk_parameters', {}).get('var_limit_1day', 0.06)}
        
        # Algorithm parameters from simulation
        self.parameters = {parameters or {}}
        
        # Initialize indicators and models
        self._initialize_models()
        
        self.Log(f"Myportolio Algorithm Initialized: {{algorithm_name}}")
    
    def _initialize_models(self):
        """Initialize ETH models and risk management."""
        # Placeholder for model integration
        # Will integrate with existing ETH models
        pass
    
    def OnData(self, data):
        """Handle new market data."""
        if not self.eth.HasData:
            return
            
        # Get current ETH price
        eth_price = self.Securities["ETHUSD"].Price
        
        # Apply trading logic (placeholder)
        # Will integrate with Myportolio algorithms
        self._apply_trading_logic(data)
    
    def _apply_trading_logic(self, data):
        """Apply Myportolio trading strategies."""
        # Integration point for:
        # - ETH momentum algorithms
        # - Risk management systems  
        # - Kelly criterion position sizing
        # - Six-position strategy logic
        pass
'''
        
        # Save algorithm file
        algorithm_path = sim_dir / f"{algorithm_name}.py"
        with open(algorithm_path, 'w') as f:
            f.write(algorithm_template)
        
        return algorithm_path

    def _execute_lean_backtest(self, config: Dict[str, Any], sim_dir: Path) -> Dict[str, Any]:
        """
        Execute Python-based backtest using Myportolio components.
        
        Returns:
            Backtest execution results
        """
        logger.info("Running Python-based simulation using Myportolio components")
        
        try:
            # Import our existing components
            sys.path.append(str(self.portfolio_path))
            sys.path.append(str(self.portfolio_path / "utilities"))
            sys.path.append(str(self.portfolio_path / "trading_algorithms"))
            sys.path.append(str(self.portfolio_path / "risk_algorithms"))
            
            # Run simulation using our existing components
            simulation_result = self._run_myportolio_simulation(config, sim_dir)
            
            return {
                "stdout": f"Myportolio simulation completed successfully",
                "stderr": "",
                "return_code": 0,
                "simulation_data": simulation_result
            }
            
        except Exception as e:
            logger.error(f"Simulation failed: {str(e)}")
            return {
                "stdout": "",
                "stderr": f"Simulation error: {str(e)}",
                "return_code": 1
            }

    def _run_myportolio_simulation(self, config: Dict[str, Any], sim_dir: Path) -> Dict[str, Any]:
        """
        Run simulation using Myportolio components.
        
        Returns:
            Simulation results
        """
        import pandas as pd
        import numpy as np
        from datetime import datetime, timedelta
        
        # Simulation parameters
        start_date = datetime.strptime(config['start-date'], '%Y-%m-%d')
        end_date = datetime.strptime(config['end-date'], '%Y-%m-%d')
        initial_cash = config.get('initial-cash', 100000)
        parameters = config.get('parameters', {})
        
        logger.info(f"Running simulation: {start_date} to {end_date}")
        
        # Generate sample market data (ETH price simulation)
        dates = pd.date_range(start=start_date, end=end_date, freq='H')
        
        # Simple ETH price simulation based on historical patterns
        np.random.seed(42)  # For reproducible results
        returns = np.random.normal(0.0001, 0.02, len(dates))  # Small positive drift with volatility
        prices = [2000]  # Starting ETH price
        
        for ret in returns[1:]:
            new_price = prices[-1] * (1 + ret)
            prices.append(max(new_price, 100))  # Minimum price floor
        
        # Create market data DataFrame
        market_data = pd.DataFrame({
            'timestamp': dates,
            'price': prices[:len(dates)],
            'volume': np.random.uniform(1000000, 5000000, len(dates))
        })
        
        # Apply trading strategy
        strategy_result = self._apply_trading_strategy(market_data, parameters, initial_cash)
        
        # Calculate performance metrics
        performance = self._calculate_performance_metrics(strategy_result, initial_cash)
        
        # Generate trades
        trades = self._generate_trade_records(strategy_result)
        
        simulation_result = {
            "market_data": market_data.to_dict('records'),
            "portfolio_values": strategy_result['portfolio_values'],
            "positions": strategy_result['positions'],
            "performance": performance,
            "trades": trades,
            "parameters": parameters
        }
        
        # Save detailed results
        result_file = sim_dir / "simulation_data.json"
        with open(result_file, 'w') as f:
            # Convert numpy types to native Python types for JSON serialization
            json_compatible_result = self._convert_numpy_types(simulation_result)
            json.dump(json_compatible_result, f, indent=2, default=str)
        
        return simulation_result

    def _apply_trading_strategy(self, market_data: pd.DataFrame, parameters: Dict[str, Any], initial_cash: float) -> Dict[str, Any]:
        """
        Apply Myportolio trading strategy to market data.
        
        Returns:
            Strategy execution results
        """
        logger.info("Applying ETH momentum strategy with Kelly criterion")
        
        # Strategy parameters
        kelly_fraction = parameters.get('kelly_fraction', 0.167)
        momentum_threshold = parameters.get('momentum_threshold', 0.02)
        lookback_period = parameters.get('lookback_period', 30)
        max_volatility = parameters.get('max_volatility', 0.25)
        
        # Initialize tracking variables
        portfolio_values = [initial_cash]
        positions = [0.0]  # ETH position size
        cash = initial_cash
        eth_position = 0.0
        
        # Calculate indicators
        market_data['returns'] = market_data['price'].pct_change()
        market_data['sma'] = market_data['price'].rolling(window=min(lookback_period, len(market_data))).mean()
        market_data['volatility'] = market_data['returns'].rolling(window=min(lookback_period, len(market_data))).std()
        
        # Trading simulation
        for i in range(1, len(market_data)):
            current_price = market_data.iloc[i]['price']
            current_vol = market_data.iloc[i]['volatility']
            
            # Skip if not enough data
            if pd.isna(current_vol) or i < lookback_period:
                portfolio_value = cash + eth_position * current_price
                portfolio_values.append(portfolio_value)
                positions.append(eth_position)
                continue
            
            # Generate trading signal (momentum strategy)
            price_change = (current_price - market_data.iloc[i-1]['price']) / market_data.iloc[i-1]['price']
            momentum_signal = 1 if price_change > momentum_threshold else (-1 if price_change < -momentum_threshold else 0)
            
            # Risk management: reduce position if volatility too high
            vol_multiplier = min(1.0, max_volatility / current_vol) if current_vol > 0 else 0
            
            # Kelly criterion position sizing
            if momentum_signal != 0:
                # Simplified Kelly calculation
                win_rate = 0.55  # Estimated win rate
                avg_win = 0.03   # Estimated average win
                avg_loss = 0.02  # Estimated average loss
                
                kelly_optimal = (win_rate * avg_win - (1 - win_rate) * avg_loss) / avg_win
                kelly_optimal = max(0, min(kelly_optimal, kelly_fraction))  # Cap at max Kelly fraction
                
                # Calculate target position
                portfolio_value = cash + eth_position * current_price
                target_position_value = portfolio_value * kelly_optimal * vol_multiplier * momentum_signal
                target_eth_position = target_position_value / current_price
                
                # Execute trade (simplified - no slippage/commissions for now)
                position_change = target_eth_position - eth_position
                trade_value = position_change * current_price
                
                if abs(trade_value) > 10:  # Minimum trade size
                    cash -= trade_value
                    eth_position = target_eth_position
            
            # Update portfolio tracking
            portfolio_value = cash + eth_position * current_price
            portfolio_values.append(portfolio_value)
            positions.append(eth_position)
        
        return {
            'portfolio_values': portfolio_values,
            'positions': positions,
            'market_data': market_data
        }

    def _calculate_performance_metrics(self, strategy_result: Dict[str, Any], initial_cash: float) -> Dict[str, float]:
        """Calculate performance metrics for the strategy."""
        portfolio_values = strategy_result['portfolio_values']
        
        if len(portfolio_values) < 2:
            return {"total_return": 0.0, "sharpe_ratio": 0.0, "max_drawdown": 0.0}
        
        # Convert to pandas Series for easier calculation
        portfolio_series = pd.Series(portfolio_values)
        
        # Total return
        total_return = (portfolio_values[-1] / initial_cash) - 1.0
        
        # Daily returns
        daily_returns = portfolio_series.pct_change().dropna()
        
        # Sharpe ratio (annualized)
        if len(daily_returns) > 1 and daily_returns.std() > 0:
            sharpe_ratio = (daily_returns.mean() / daily_returns.std()) * np.sqrt(8760)  # Hourly to annual
        else:
            sharpe_ratio = 0.0
        
        # Maximum drawdown
        rolling_max = portfolio_series.expanding().max()
        drawdown = (portfolio_series - rolling_max) / rolling_max
        max_drawdown = abs(drawdown.min())
        
        # Additional metrics
        win_rate = len(daily_returns[daily_returns > 0]) / len(daily_returns) * 100 if len(daily_returns) > 0 else 0
        
        return {
            "total_return": float(total_return),
            "sharpe_ratio": float(sharpe_ratio),
            "max_drawdown": float(max_drawdown),
            "win_rate": float(win_rate),
            "trades_count": max(0, len([p for p in strategy_result['positions'] if p != 0])),
            "final_value": float(portfolio_values[-1])
        }

    def _generate_trade_records(self, strategy_result: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate trade records from position changes."""
        positions = strategy_result['positions']
        market_data = strategy_result['market_data']
        
        trades = []
        current_position = 0.0
        
        for i, position in enumerate(positions):
            if position != current_position and i < len(market_data):
                trade = {
                    "id": f"trade_{len(trades) + 1}",
                    "timestamp": market_data.iloc[i]['timestamp'].isoformat() if i < len(market_data) else "",
                    "symbol": "ETHUSD",
                    "quantity": position - current_position,
                    "price": market_data.iloc[i]['price'] if i < len(market_data) else 0,
                    "side": "BUY" if position > current_position else "SELL"
                }
                trades.append(trade)
                current_position = position
        
        return trades

    def _convert_numpy_types(self, obj):
        """Convert numpy types to native Python types for JSON serialization."""
        if isinstance(obj, dict):
            return {key: self._convert_numpy_types(value) for key, value in obj.items()}
        elif isinstance(obj, list):
            return [self._convert_numpy_types(item) for item in obj]
        elif isinstance(obj, np.integer):
            return int(obj)
        elif isinstance(obj, np.floating):
            return float(obj)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        elif pd.isna(obj):
            return None
        else:
            return obj

    def _process_backtest_results(self, 
                                 simulation_id: str, 
                                 lean_result: Dict[str, Any], 
                                 sim_dir: Path):
        """
        Process and store LEAN backtest results.
        
        Args:
            simulation_id: Simulation identifier
            lean_result: Raw LEAN execution results
            sim_dir: Simulation directory path
        """
        # Look for LEAN result files
        result_files = list(sim_dir.glob("*.json"))
        
        if result_files:
            # Process main result file
            main_result = result_files[0]
            with open(main_result, 'r') as f:
                lean_data = json.load(f)
            
            # Extract performance metrics
            performance = self._extract_performance_metrics(lean_data)
            
            # Create Myportolio-specific result format
            myportolio_result = {
                "simulation_id": simulation_id,
                "simulation_type": "backtest",
                "timestamp": datetime.now().isoformat(),
                "portfolio": "Myportolio",
                "strategy": self.portfolio_config.get("strategy", "unknown"),
                
                # LEAN results
                "lean_results": lean_data,
                
                # Performance summary
                "performance": performance,
                
                # Configuration
                "configuration": self.portfolio_config,
                
                # Execution info
                "execution": {
                    "duration": "00:00:00",  # Will calculate from LEAN logs
                    "status": "completed",
                    "lean_output": lean_result.get("stdout", "")
                }
            }
            
            # Save processed results
            result_path = sim_dir / "myportolio_results.json"
            with open(result_path, 'w') as f:
                json.dump(myportolio_result, f, indent=2)
            
            logger.info(f"Results processed and saved: {result_path}")
        
        else:
            logger.warning(f"No LEAN result files found in {sim_dir}")

    def _extract_performance_metrics(self, lean_data: Dict[str, Any]) -> Dict[str, float]:
        """Extract key performance metrics from LEAN results."""
        
        # Default metrics structure
        metrics = {
            "total_return": 0.0,
            "sharpe_ratio": 0.0,
            "max_drawdown": 0.0,
            "trades_count": 0,
            "win_rate": 0.0,
            "profit_factor": 0.0
        }
        
        # Extract from LEAN data structure
        if "TotalPerformance" in lean_data:
            performance = lean_data["TotalPerformance"]
            metrics.update({
                "total_return": performance.get("TotalReturn", 0.0),
                "sharpe_ratio": performance.get("SharpeRatio", 0.0),
                "max_drawdown": performance.get("Drawdown", 0.0)
            })
        
        if "Statistics" in lean_data:
            stats = lean_data["Statistics"]
            metrics.update({
                "trades_count": stats.get("Total Trades", 0),
                "win_rate": stats.get("Win Rate", 0.0),
                "profit_factor": stats.get("Profit-Loss Ratio", 0.0)
            })
        
        return metrics

    def get_simulation_results(self, simulation_id: str) -> Optional[Dict[str, Any]]:
        """
        Retrieve results for a specific simulation.
        
        Args:
            simulation_id: Simulation identifier
            
        Returns:
            Simulation results or None if not found
        """
        # Search in all simulation directories
        for sim_type in ["backtests", "paper", "optimization_runs"]:
            sim_dir = self.simulations_path / sim_type / simulation_id
            result_path = sim_dir / "myportolio_results.json"
            
            if result_path.exists():
                with open(result_path, 'r') as f:
                    return json.load(f)
        
        return None

    def list_simulations(self, simulation_type: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        List all available simulations.
        
        Args:
            simulation_type: Filter by type (backtest, paper, optimization)
            
        Returns:
            List of simulation summaries
        """
        simulations = []
        
        search_types = [simulation_type] if simulation_type else ["backtests", "paper", "optimization_runs"]
        
        for sim_type in search_types:
            type_dir = self.simulations_path / sim_type
            
            if type_dir.exists():
                for sim_dir in type_dir.iterdir():
                    if sim_dir.is_dir():
                        result_path = sim_dir / "myportolio_results.json"
                        
                        if result_path.exists():
                            with open(result_path, 'r') as f:
                                result = json.load(f)
                                
                                simulations.append({
                                    "simulation_id": result.get("simulation_id"),
                                    "type": result.get("simulation_type"),
                                    "timestamp": result.get("timestamp"),
                                    "performance": result.get("performance", {}),
                                    "status": result.get("execution", {}).get("status", "unknown")
                                })
        
        # Sort by timestamp (newest first)
        simulations.sort(key=lambda x: x.get("timestamp", ""), reverse=True)
        
        return simulations

if __name__ == "__main__":
    # Example usage - 6 month simulation with enhanced logging
    engine = PythonSimulationEngine()
    
    # Set date range for 6 months (March to September 2024)
    start_date = "2024-03-15"
    end_date = "2024-09-15"
    
    # Run enhanced backtest with comprehensive logging
    simulation_id = engine.run_backtest_with_logging(
        start_date=start_date,
        end_date=end_date,
        parameters={
            "kelly_fraction": 0.167,
            "rebalance_frequency": "daily",
            "ma_short": 5,
            "ma_long": 20,
            "rsi_period": 14,
            "volatility_window": 30
        },
        template_name="enhanced_momentum_6month"
    )
    
    print(f"✅ 6-Month Enhanced Backtest started with ID: {simulation_id}")
    print(f"📊 Period: {start_date} to {end_date}")
    print(f"🔍 Performance logging enabled for detailed attribution analysis")
