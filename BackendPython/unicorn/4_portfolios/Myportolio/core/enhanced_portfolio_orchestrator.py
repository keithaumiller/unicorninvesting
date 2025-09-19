#!/usr/bin/env python3
"""
🎯 Enhanced Portfolio-Level Orchestration Engine

Extends the proven 5-minute scheduler with formal workflow coordination
across all LEAN framework layers (Data → Alpha → Risk → Portfolio → Execution)
while maintaining the operational foundation that currently works.

Architecture: Portfolio-Level Orchestration (Layer 4) with LEAN Algorithm Templates (Layer 6)
"""

import asyncio
import time
import threading
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Union, Callable
import logging
import json
from dataclasses import dataclass, field
from enum import Enum
import traceback

# Import existing proven components
from five_minute_trading_scheduler import FiveMinuteTradingScheduler
from risk_reward_decision_engine import RiskRewardDecisionEngine
from simplified_ensemble_portfolio import EnsembleMultiAssetPortfolio
from live_market_data_feed import LiveMarketDataFeed

# Framework utilities
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'utilities'))
from EnhancedPortfolioManager import EnhancedPortfolioManager


class OrchestrationState(Enum):
    """Orchestration workflow states"""
    IDLE = "idle"
    DATA_COLLECTION = "data_collection"
    ALPHA_GENERATION = "alpha_generation"
    RISK_EVALUATION = "risk_evaluation"
    PORTFOLIO_OPTIMIZATION = "portfolio_optimization"
    EXECUTION_PLANNING = "execution_planning"
    EXECUTION = "execution"
    POST_EXECUTION = "post_execution"
    ERROR = "error"


@dataclass
class WorkflowContext:
    """Context object passed through workflow stages"""
    timestamp: datetime
    assets: List[str]
    timeframes: List[str]
    market_data: Optional[Dict] = None
    alpha_signals: Optional[Dict] = None
    risk_metrics: Optional[Dict] = None
    portfolio_targets: Optional[Dict] = None
    execution_plan: Optional[Dict] = None
    execution_results: Optional[Dict] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    errors: List[str] = field(default_factory=list)


@dataclass
class OrchestrationMetrics:
    """Performance metrics for orchestration monitoring"""
    total_cycles: int = 0
    successful_cycles: int = 0
    failed_cycles: int = 0
    average_cycle_duration: float = 0.0
    last_successful_cycle: Optional[datetime] = None
    stage_durations: Dict[str, float] = field(default_factory=dict)
    error_counts: Dict[str, int] = field(default_factory=dict)


class EnhancedPortfolioOrchestrator:
    """
    🎯 Enhanced Portfolio-Level Orchestration Engine
    
    Coordinates Data → Alpha → Risk → Portfolio → Execution workflow
    while building upon the proven 5-minute scheduler foundation.
    """
    
    def __init__(self, 
                 portfolio_name: str = "Myportolio",
                 config_path: str = None,
                 enable_async: bool = True,
                 enable_monitoring: bool = True):
        """
        Initialize enhanced portfolio orchestrator
        
        Args:
            portfolio_name: Portfolio to orchestrate
            config_path: Configuration file path
            enable_async: Enable asynchronous processing
            enable_monitoring: Enable performance monitoring
        """
        self.portfolio_name = portfolio_name
        self.config = self._load_orchestration_config(config_path)
        self.enable_async = enable_async
        self.enable_monitoring = enable_monitoring
        
        # Current operational state
        self.state = OrchestrationState.IDLE
        self.current_context = None
        self.is_running = False
        
        # Performance monitoring
        self.metrics = OrchestrationMetrics()
        self.workflow_history = []
        
        # Initialize core components (proven existing systems)
        self._initialize_core_components()
        
        # Workflow stage handlers
        self._setup_workflow_handlers()
        
        # Initialize logging
        self.logger = logging.getLogger(__name__)
        self.logger.info(f"Enhanced Portfolio Orchestrator initialized for {portfolio_name}")
    
    def _load_orchestration_config(self, config_path: str = None) -> Dict:
        """Load orchestration configuration"""
        default_config = {
            "orchestration": {
                "cycle_interval_seconds": 300,  # 5 minutes
                "enable_multi_timeframe": True,
                "timeframes": ["1min", "5min", "1hour", "1day"],
                "assets": ["ETH", "BTC", "EURUSD", "USDJPY", "GBPUSD", "AUDUSD", "USDCAD", "USDCHF", "NZDUSD"],
                "workflow_timeout_seconds": 240,
                "stage_timeouts": {
                    "data_collection": 30,
                    "alpha_generation": 60,
                    "risk_evaluation": 30,
                    "portfolio_optimization": 45,
                    "execution_planning": 15,
                    "execution": 30
                },
                "error_handling": {
                    "max_retries": 3,
                    "retry_delay_seconds": 5,
                    "fail_fast_on_critical_errors": True
                }
            }
        }
        
        if config_path:
            try:
                with open(config_path, 'r') as f:
                    config = json.load(f)
                    # Merge with defaults
                    default_config.update(config)
            except Exception as e:
                self.logger.warning(f"Could not load config from {config_path}, using defaults: {e}")
        
        return default_config["orchestration"]
    
    def _initialize_core_components(self):
        """Initialize proven core components"""
        try:
            # Initialize portfolio (existing proven system)
            self.portfolio = EnsembleMultiAssetPortfolio(
                initial_capital=100000,
                risk_tolerance=0.15,
                equal_value_allocation=True
            )
            
            # Initialize scheduler (existing proven system)
            self.scheduler = FiveMinuteTradingScheduler(
                portfolio=self.portfolio,
                enable_logging=True
            )
            
            # Initialize decision engine (existing proven system) 
            self.decision_engine = RiskRewardDecisionEngine()
            
            # Initialize market data feed (existing proven system)
            self.market_data_feed = LiveMarketDataFeed()
            
            # Initialize enhanced portfolio manager
            self.portfolio_manager = EnhancedPortfolioManager(self.portfolio_name)
            
            self.logger.info("Core components initialized successfully")
            
        except Exception as e:
            self.logger.error(f"Failed to initialize core components: {e}")
            raise
    
    def _setup_workflow_handlers(self):
        """Setup workflow stage handlers"""
        self.workflow_handlers = {
            OrchestrationState.DATA_COLLECTION: self._handle_data_collection,
            OrchestrationState.ALPHA_GENERATION: self._handle_alpha_generation,
            OrchestrationState.RISK_EVALUATION: self._handle_risk_evaluation,
            OrchestrationState.PORTFOLIO_OPTIMIZATION: self._handle_portfolio_optimization,
            OrchestrationState.EXECUTION_PLANNING: self._handle_execution_planning,
            OrchestrationState.EXECUTION: self._handle_execution,
            OrchestrationState.POST_EXECUTION: self._handle_post_execution
        }
    
    # =============================================================================
    # PUBLIC ORCHESTRATION INTERFACE
    # =============================================================================
    
    def start_orchestration(self) -> bool:
        """
        Start the enhanced orchestration engine
        
        Returns:
            bool: True if started successfully
        """
        try:
            if self.is_running:
                self.logger.warning("Orchestration already running")
                return False
            
            self.is_running = True
            self.state = OrchestrationState.IDLE
            
            self.logger.info("Starting Enhanced Portfolio Orchestration")
            
            if self.enable_async:
                # Start async orchestration loop
                self.orchestration_task = asyncio.create_task(self._async_orchestration_loop())
            else:
                # Start synchronous orchestration loop
                self.orchestration_thread = threading.Thread(target=self._sync_orchestration_loop)
                self.orchestration_thread.start()
            
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to start orchestration: {e}")
            self.is_running = False
            return False
    
    def stop_orchestration(self) -> bool:
        """
        Stop the orchestration engine gracefully
        
        Returns:
            bool: True if stopped successfully
        """
        try:
            self.logger.info("Stopping Enhanced Portfolio Orchestration")
            self.is_running = False
            
            # Wait for current workflow to complete
            if self.current_context:
                self.logger.info("Waiting for current workflow to complete...")
                # Add timeout and cleanup logic here
            
            self.state = OrchestrationState.IDLE
            self.logger.info("Orchestration stopped successfully")
            return True
            
        except Exception as e:
            self.logger.error(f"Error stopping orchestration: {e}")
            return False
    
    def execute_single_workflow_cycle(self, 
                                     assets: List[str] = None, 
                                     timeframes: List[str] = None) -> Dict[str, Any]:
        """
        Execute a single orchestration workflow cycle
        
        Args:
            assets: List of assets to process (uses config default if None)
            timeframes: List of timeframes to process (uses config default if None)
            
        Returns:
            Dict containing workflow results and metrics
        """
        start_time = datetime.now()
        
        # Create workflow context
        context = WorkflowContext(
            timestamp=start_time,
            assets=assets or self.config["assets"],
            timeframes=timeframes or self.config["timeframes"]
        )
        
        try:
            # Execute workflow stages sequentially
            result = self._execute_workflow_stages(context)
            
            # Update metrics
            duration = (datetime.now() - start_time).total_seconds()
            self._update_metrics(success=True, duration=duration)
            
            return {
                "success": True,
                "context": context,
                "duration_seconds": duration,
                "timestamp": start_time
            }
            
        except Exception as e:
            duration = (datetime.now() - start_time).total_seconds()
            self._update_metrics(success=False, duration=duration, error=str(e))
            
            self.logger.error(f"Workflow cycle failed: {e}")
            return {
                "success": False,
                "error": str(e),
                "context": context,
                "duration_seconds": duration,
                "timestamp": start_time
            }
    
    # =============================================================================
    # WORKFLOW STAGE HANDLERS
    # =============================================================================
    
    async def _handle_data_collection(self, context: WorkflowContext) -> bool:
        """
        Layer 1: Data Sources - Collect market data across all timeframes and assets
        """
        self.logger.info(f"Data collection for {len(context.assets)} assets, {len(context.timeframes)} timeframes")
        
        try:
            # Use existing proven live market data feed
            market_data = {}
            
            for asset in context.assets:
                asset_data = {}
                for timeframe in context.timeframes:
                    # Collect data for each asset/timeframe combination
                    data = await self._collect_asset_timeframe_data(asset, timeframe)
                    asset_data[timeframe] = data
                market_data[asset] = asset_data
            
            context.market_data = market_data
            context.metadata["data_collection_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Data collection failed: {e}")
            self.logger.error(f"Data collection failed: {e}")
            return False
    
    async def _handle_alpha_generation(self, context: WorkflowContext) -> bool:
        """
        Layer 2: Alpha Models - Generate trading signals using ensemble models
        """
        self.logger.info("Generating alpha signals using ensemble models")
        
        try:
            # Use existing alpha model infrastructure
            alpha_signals = {}
            
            for asset in context.assets:
                asset_signals = {}
                for timeframe in context.timeframes:
                    # Generate signals for each asset/timeframe
                    signals = await self._generate_asset_alpha_signals(asset, timeframe, context.market_data)
                    asset_signals[timeframe] = signals
                alpha_signals[asset] = asset_signals
            
            context.alpha_signals = alpha_signals
            context.metadata["alpha_generation_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Alpha generation failed: {e}")
            self.logger.error(f"Alpha generation failed: {e}")
            return False
    
    async def _handle_risk_evaluation(self, context: WorkflowContext) -> bool:
        """
        Layer 3: Risk Management - Evaluate risk metrics and constraints
        """
        self.logger.info("Evaluating risk metrics and constraints")
        
        try:
            # Use existing risk/reward decision engine
            risk_metrics = await self._evaluate_portfolio_risk(context)
            
            context.risk_metrics = risk_metrics
            context.metadata["risk_evaluation_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Risk evaluation failed: {e}")
            self.logger.error(f"Risk evaluation failed: {e}")
            return False
    
    async def _handle_portfolio_optimization(self, context: WorkflowContext) -> bool:
        """
        Layer 4: Portfolio Construction - Optimize portfolio allocation
        """
        self.logger.info("Optimizing portfolio allocation")
        
        try:
            # Use existing enhanced portfolio manager
            portfolio_targets = await self._optimize_portfolio_allocation(context)
            
            context.portfolio_targets = portfolio_targets
            context.metadata["portfolio_optimization_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Portfolio optimization failed: {e}")
            self.logger.error(f"Portfolio optimization failed: {e}")
            return False
    
    async def _handle_execution_planning(self, context: WorkflowContext) -> bool:
        """
        Layer 5: Execution Models - Plan trade execution
        """
        self.logger.info("Planning trade execution")
        
        try:
            execution_plan = await self._plan_trade_execution(context)
            
            context.execution_plan = execution_plan
            context.metadata["execution_planning_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Execution planning failed: {e}")
            self.logger.error(f"Execution planning failed: {e}")
            return False
    
    async def _handle_execution(self, context: WorkflowContext) -> bool:
        """
        Execute planned trades
        """
        self.logger.info("Executing planned trades")
        
        try:
            execution_results = await self._execute_planned_trades(context)
            
            context.execution_results = execution_results
            context.metadata["execution_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Trade execution failed: {e}")
            self.logger.error(f"Trade execution failed: {e}")
            return False
    
    async def _handle_post_execution(self, context: WorkflowContext) -> bool:
        """
        Post-execution processing and logging
        """
        self.logger.info("Processing post-execution tasks")
        
        try:
            # Log performance, update metrics, store results
            await self._process_execution_results(context)
            
            context.metadata["post_execution_completed"] = datetime.now()
            
            return True
            
        except Exception as e:
            context.errors.append(f"Post-execution processing failed: {e}")
            self.logger.error(f"Post-execution processing failed: {e}")
            return False
    
    # =============================================================================
    # WORKFLOW EXECUTION ENGINE
    # =============================================================================
    
    def _execute_workflow_stages(self, context: WorkflowContext) -> Dict[str, Any]:
        """Execute all workflow stages sequentially"""
        
        workflow_stages = [
            (OrchestrationState.DATA_COLLECTION, "data_collection"),
            (OrchestrationState.ALPHA_GENERATION, "alpha_generation"),
            (OrchestrationState.RISK_EVALUATION, "risk_evaluation"),
            (OrchestrationState.PORTFOLIO_OPTIMIZATION, "portfolio_optimization"),
            (OrchestrationState.EXECUTION_PLANNING, "execution_planning"),
            (OrchestrationState.EXECUTION, "execution"),
            (OrchestrationState.POST_EXECUTION, "post_execution")
        ]
        
        for stage_state, stage_name in workflow_stages:
            stage_start = time.time()
            self.state = stage_state
            
            try:
                if self.enable_async:
                    # Run async handler
                    success = asyncio.run(self.workflow_handlers[stage_state](context))
                else:
                    # Convert to sync execution
                    success = asyncio.run(self.workflow_handlers[stage_state](context))
                
                stage_duration = time.time() - stage_start
                
                if not success:
                    raise Exception(f"Stage {stage_name} failed")
                
                # Record stage performance
                if stage_name not in self.metrics.stage_durations:
                    self.metrics.stage_durations[stage_name] = []
                self.metrics.stage_durations[stage_name] = stage_duration
                
                self.logger.info(f"Stage {stage_name} completed in {stage_duration:.2f}s")
                
            except Exception as e:
                self.state = OrchestrationState.ERROR
                raise Exception(f"Workflow failed at stage {stage_name}: {e}")
        
        self.state = OrchestrationState.IDLE
        return {"success": True, "stages_completed": len(workflow_stages)}
    
    # =============================================================================
    # ORCHESTRATION LOOPS
    # =============================================================================
    
    async def _async_orchestration_loop(self):
        """Asynchronous orchestration loop"""
        self.logger.info("Starting async orchestration loop")
        
        while self.is_running:
            try:
                cycle_start = datetime.now()
                
                # Execute workflow cycle
                result = self.execute_single_workflow_cycle()
                
                # Wait for next cycle
                await asyncio.sleep(self.config["cycle_interval_seconds"])
                
            except Exception as e:
                self.logger.error(f"Orchestration loop error: {e}")
                await asyncio.sleep(10)  # Brief pause before retry
    
    def _sync_orchestration_loop(self):
        """Synchronous orchestration loop"""
        self.logger.info("Starting sync orchestration loop")
        
        while self.is_running:
            try:
                cycle_start = datetime.now()
                
                # Execute workflow cycle
                result = self.execute_single_workflow_cycle()
                
                # Wait for next cycle
                time.sleep(self.config["cycle_interval_seconds"])
                
            except Exception as e:
                self.logger.error(f"Orchestration loop error: {e}")
                time.sleep(10)  # Brief pause before retry
    
    # =============================================================================
    # MONITORING AND METRICS
    # =============================================================================
    
    def _update_metrics(self, success: bool, duration: float, error: str = None):
        """Update performance metrics"""
        self.metrics.total_cycles += 1
        
        if success:
            self.metrics.successful_cycles += 1
            self.metrics.last_successful_cycle = datetime.now()
        else:
            self.metrics.failed_cycles += 1
            if error:
                error_type = type(error).__name__
                self.metrics.error_counts[error_type] = self.metrics.error_counts.get(error_type, 0) + 1
        
        # Update average duration
        total_successful = self.metrics.successful_cycles
        if total_successful > 0:
            self.metrics.average_cycle_duration = (
                (self.metrics.average_cycle_duration * (total_successful - 1) + duration) / total_successful
            )
    
    def get_orchestration_status(self) -> Dict[str, Any]:
        """Get current orchestration status and metrics"""
        return {
            "is_running": self.is_running,
            "current_state": self.state.value,
            "portfolio_name": self.portfolio_name,
            "metrics": {
                "total_cycles": self.metrics.total_cycles,
                "successful_cycles": self.metrics.successful_cycles,
                "failed_cycles": self.metrics.failed_cycles,
                "success_rate": self.metrics.successful_cycles / max(1, self.metrics.total_cycles),
                "average_cycle_duration": self.metrics.average_cycle_duration,
                "last_successful_cycle": self.metrics.last_successful_cycle
            },
            "configuration": self.config,
            "timestamp": datetime.now()
        }
    
    # =============================================================================
    # HELPER METHODS (To be implemented with existing components)
    # =============================================================================
    
    async def _collect_asset_timeframe_data(self, asset: str, timeframe: str) -> Dict:
        """Collect market data for specific asset and timeframe"""
        # Implement using existing LiveMarketDataFeed
        return {"placeholder": "data_collection_implementation"}
    
    async def _generate_asset_alpha_signals(self, asset: str, timeframe: str, market_data: Dict) -> Dict:
        """Generate alpha signals for specific asset and timeframe"""
        # Implement using existing alpha models infrastructure
        return {"placeholder": "alpha_generation_implementation"}
    
    async def _evaluate_portfolio_risk(self, context: WorkflowContext) -> Dict:
        """Evaluate portfolio risk metrics"""
        # Implement using existing RiskRewardDecisionEngine
        return {"placeholder": "risk_evaluation_implementation"}
    
    async def _optimize_portfolio_allocation(self, context: WorkflowContext) -> Dict:
        """Optimize portfolio allocation"""
        # Implement using existing EnhancedPortfolioManager
        return {"placeholder": "portfolio_optimization_implementation"}
    
    async def _plan_trade_execution(self, context: WorkflowContext) -> Dict:
        """Plan trade execution"""
        # Implement execution planning logic
        return {"placeholder": "execution_planning_implementation"}
    
    async def _execute_planned_trades(self, context: WorkflowContext) -> Dict:
        """Execute planned trades"""
        # Implement using existing portfolio execution methods
        return {"placeholder": "trade_execution_implementation"}
    
    async def _process_execution_results(self, context: WorkflowContext) -> Dict:
        """Process execution results and update performance tracking"""
        # Implement result processing and logging
        return {"placeholder": "post_execution_implementation"}


# =============================================================================
# LEAN ALGORITHM TEMPLATE INTEGRATION
# =============================================================================

class LEANAlgorithmTemplate:
    """
    LEAN Framework compliant algorithm template that serves as a wrapper
    around the Enhanced Portfolio Orchestrator for framework compliance
    """
    
    def __init__(self):
        """Initialize LEAN algorithm template"""
        self.orchestrator = EnhancedPortfolioOrchestrator()
        self.is_initialized = False
    
    def Initialize(self):
        """LEAN Initialize method"""
        self.orchestrator.start_orchestration()
        self.is_initialized = True
    
    def OnData(self, data):
        """LEAN OnData method - called on new market data"""
        if not self.is_initialized:
            return
        
        # Execute single workflow cycle with current data
        result = self.orchestrator.execute_single_workflow_cycle()
        
        return result
    
    def OnEndOfAlgorithm(self):
        """LEAN OnEndOfAlgorithm method"""
        self.orchestrator.stop_orchestration()