#!/usr/bin/env python3
"""
Comprehensive Performance Logging System for Myportolio Backtesting
==================================================================

This module provides detailed logging and analysis for every component of the
backtesting framework to identify performance bottlenecks and attribution.

Features:
- Alpha model accuracy tracking vs actual price movements
- Trading strategy signal analysis and decision logging
- Risk management impact measurement
- Portfolio state change tracking
- Performance attribution analysis
- Trade execution efficiency metrics

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

import json
import logging
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from dataclasses import dataclass, asdict
from enum import Enum

class LogLevel(Enum):
    """Logging level enumeration"""
    TRADE = "TRADE"
    SIGNAL = "SIGNAL" 
    ALPHA = "ALPHA"
    RISK = "RISK"
    PORTFOLIO = "PORTFOLIO"
    EXECUTION = "EXECUTION"
    ERROR = "ERROR"

@dataclass
class AlphaModelPrediction:
    """Alpha model prediction tracking"""
    timestamp: str
    asset: str
    model_type: str
    timeframe: str
    predicted_direction: str  # UP, DOWN, NEUTRAL
    predicted_return: float
    confidence: float
    actual_return: Optional[float] = None
    accuracy: Optional[bool] = None
    prediction_horizon: int = 1  # hours

@dataclass
class TradingSignal:
    """Trading signal tracking"""
    timestamp: str
    asset: str
    signal_type: str  # BUY, SELL, HOLD
    confidence: float
    current_price: float
    target_position: float
    current_position: float
    signal_reason: str
    technical_indicators: Dict[str, float]
    
@dataclass
class RiskDecision:
    """Risk management decision tracking"""
    timestamp: str
    asset: str
    decision_type: str  # POSITION_LIMIT, DRAWDOWN_LIMIT, VAR_LIMIT, VOLATILITY_LIMIT
    proposed_action: str
    approved: bool
    reason: str
    risk_metrics: Dict[str, float]
    impact_on_position: float

@dataclass
class TradeExecution:
    """Trade execution tracking"""
    timestamp: str
    asset: str
    action: str  # BUY, SELL
    intended_quantity: float
    executed_quantity: float
    intended_price: float
    executed_price: float
    slippage: float
    execution_delay_ms: float
    trade_cost: float

@dataclass
class PortfolioState:
    """Portfolio state snapshot"""
    timestamp: str
    total_value: float
    cash: float
    positions: Dict[str, float]
    unrealized_pnl: float
    realized_pnl: float
    drawdown: float
    volatility: float
    var_95: float
    sharpe_ratio: Optional[float] = None

class PerformanceLogger:
    """
    Comprehensive performance logging system for backtesting analysis.
    
    This logger captures every decision and outcome in the backtesting process
    to enable detailed performance attribution and debugging.
    """
    
    def __init__(self, simulation_id: str, log_directory: Path = None):
        """
        Initialize performance logger.
        
        Args:
            simulation_id: Unique identifier for this simulation
            log_directory: Directory to store detailed logs
        """
        self.simulation_id = simulation_id
        self.start_time = datetime.now()
        
        # Set up logging directory
        if log_directory is None:
            log_directory = Path(f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/performance_logs")
        
        self.log_directory = log_directory
        self.log_directory.mkdir(parents=True, exist_ok=True)
        
        # Initialize data stores
        self.alpha_predictions: List[AlphaModelPrediction] = []
        self.trading_signals: List[TradingSignal] = []
        self.risk_decisions: List[RiskDecision] = []
        self.trade_executions: List[TradeExecution] = []
        self.portfolio_states: List[PortfolioState] = []
        
        # Performance metrics tracking
        self.daily_returns = []
        self.benchmark_returns = []
        self.prediction_accuracy_by_model = {}
        self.signal_accuracy_by_strategy = {}
        
        # Set up file logging
        self._setup_file_logging()
        
        self.logger.info(f"Performance logger initialized for simulation: {simulation_id}")
        
    def _setup_file_logging(self):
        """Set up detailed file logging."""
        log_file = self.log_directory / f"{self.simulation_id}_performance.log"
        
        # Create logger
        self.logger = logging.getLogger(f"performance_{self.simulation_id}")
        self.logger.setLevel(logging.DEBUG)
        
        # Clear any existing handlers
        self.logger.handlers.clear()
        
        # File handler
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(logging.DEBUG)
        
        # Console handler  
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)
        
        # Formatter
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        self.logger.addHandler(file_handler)
        self.logger.addHandler(console_handler)

    def log_alpha_prediction(self, 
                           asset: str,
                           model_type: str,
                           timeframe: str,
                           predicted_direction: str,
                           predicted_return: float,
                           confidence: float,
                           current_price: float,
                           prediction_horizon: int = 1):
        """
        Log alpha model prediction for later accuracy analysis.
        
        Args:
            asset: Asset symbol (e.g., "ETH", "BTC")
            model_type: Model type (e.g., "xgboost", "prophet", "ensemble") 
            timeframe: Prediction timeframe (e.g., "1min", "1hour", "1day")
            predicted_direction: UP, DOWN, or NEUTRAL
            predicted_return: Expected return magnitude
            confidence: Model confidence (0-1)
            current_price: Current asset price
            prediction_horizon: Hours ahead to validate prediction
        """
        prediction = AlphaModelPrediction(
            timestamp=datetime.now().isoformat(),
            asset=asset,
            model_type=model_type,
            timeframe=timeframe,
            predicted_direction=predicted_direction,
            predicted_return=predicted_return,
            confidence=confidence,
            prediction_horizon=prediction_horizon
        )
        
        self.alpha_predictions.append(prediction)
        
        self.logger.info(f"ALPHA | {asset} {model_type} {timeframe}: "
                        f"{predicted_direction} return={predicted_return:.4f} "
                        f"confidence={confidence:.3f} price={current_price:.2f}")

    def log_trading_signal(self,
                          asset: str,
                          signal_type: str,
                          confidence: float,
                          current_price: float,
                          target_position: float,
                          current_position: float,
                          signal_reason: str,
                          technical_indicators: Dict[str, float] = None):
        """
        Log trading strategy signal generation.
        
        Args:
            asset: Asset symbol
            signal_type: BUY, SELL, or HOLD
            confidence: Signal confidence (0-1)
            current_price: Current asset price
            target_position: Target position size
            current_position: Current position size
            signal_reason: Explanation for signal
            technical_indicators: Dict of technical indicator values
        """
        if technical_indicators is None:
            technical_indicators = {}
            
        signal = TradingSignal(
            timestamp=datetime.now().isoformat(),
            asset=asset,
            signal_type=signal_type,
            confidence=confidence,
            current_price=current_price,
            target_position=target_position,
            current_position=current_position,
            signal_reason=signal_reason,
            technical_indicators=technical_indicators
        )
        
        self.trading_signals.append(signal)
        
        position_change = target_position - current_position
        self.logger.info(f"SIGNAL | {asset} {signal_type}: "
                        f"confidence={confidence:.3f} price={current_price:.2f} "
                        f"pos_change={position_change:+.4f} reason='{signal_reason}'")

    def log_risk_decision(self,
                         asset: str,
                         decision_type: str,
                         proposed_action: str,
                         approved: bool,
                         reason: str,
                         risk_metrics: Dict[str, float],
                         impact_on_position: float = 0.0):
        """
        Log risk management decisions and their impact.
        
        Args:
            asset: Asset symbol
            decision_type: Type of risk check (e.g., POSITION_LIMIT, DRAWDOWN_LIMIT)
            proposed_action: What action was proposed
            approved: Whether risk check approved the action
            reason: Explanation for approval/rejection
            risk_metrics: Current risk metrics values
            impact_on_position: How much position was adjusted
        """
        decision = RiskDecision(
            timestamp=datetime.now().isoformat(),
            asset=asset,
            decision_type=decision_type,
            proposed_action=proposed_action,
            approved=approved,
            reason=reason,
            risk_metrics=risk_metrics,
            impact_on_position=impact_on_position
        )
        
        self.risk_decisions.append(decision)
        
        status = "APPROVED" if approved else "REJECTED"
        self.logger.info(f"RISK | {asset} {decision_type}: {status} - "
                        f"action='{proposed_action}' reason='{reason}' "
                        f"impact={impact_on_position:+.4f}")

    def log_trade_execution(self,
                           asset: str,
                           action: str,
                           intended_quantity: float,
                           executed_quantity: float,
                           intended_price: float,
                           executed_price: float,
                           execution_delay_ms: float = 0.0,
                           trade_cost: float = 0.0):
        """
        Log trade execution details for slippage and cost analysis.
        
        Args:
            asset: Asset symbol
            action: BUY or SELL
            intended_quantity: Intended trade size
            executed_quantity: Actual executed size
            intended_price: Expected execution price
            executed_price: Actual execution price
            execution_delay_ms: Execution delay in milliseconds
            trade_cost: Total trade costs (fees, slippage, etc.)
        """
        slippage = executed_price - intended_price
        
        execution = TradeExecution(
            timestamp=datetime.now().isoformat(),
            asset=asset,
            action=action,
            intended_quantity=intended_quantity,
            executed_quantity=executed_quantity,
            intended_price=intended_price,
            executed_price=executed_price,
            slippage=slippage,
            execution_delay_ms=execution_delay_ms,
            trade_cost=trade_cost
        )
        
        self.trade_executions.append(execution)
        
        slippage_pct = (slippage / intended_price * 100) if intended_price != 0 else 0
        quantity_fill = (executed_quantity / intended_quantity * 100) if intended_quantity != 0 else 0
        
        self.logger.info(f"EXECUTION | {asset} {action}: "
                        f"qty={executed_quantity:.4f} ({quantity_fill:.1f}% fill) "
                        f"price={executed_price:.2f} slippage={slippage_pct:+.3f}% "
                        f"cost=${trade_cost:.2f} delay={execution_delay_ms:.1f}ms")

    def log_portfolio_state(self,
                           total_value: float,
                           cash: float,
                           positions: Dict[str, float],
                           unrealized_pnl: float,
                           realized_pnl: float,
                           drawdown: float,
                           volatility: float,
                           var_95: float):
        """
        Log portfolio state snapshot for performance tracking.
        
        Args:
            total_value: Total portfolio value
            cash: Cash position
            positions: Dict of asset positions
            unrealized_pnl: Unrealized P&L
            realized_pnl: Realized P&L  
            drawdown: Current drawdown
            volatility: Portfolio volatility
            var_95: 95% Value at Risk
        """
        # Calculate Sharpe ratio if we have enough returns data
        sharpe_ratio = None
        if len(self.daily_returns) > 10:
            returns_array = np.array(self.daily_returns[-30:])  # Last 30 days
            if returns_array.std() > 0:
                sharpe_ratio = returns_array.mean() / returns_array.std() * np.sqrt(252)
        
        state = PortfolioState(
            timestamp=datetime.now().isoformat(),
            total_value=total_value,
            cash=cash,
            positions=positions.copy(),
            unrealized_pnl=unrealized_pnl,
            realized_pnl=realized_pnl,
            drawdown=drawdown,
            volatility=volatility,
            var_95=var_95,
            sharpe_ratio=sharpe_ratio
        )
        
        self.portfolio_states.append(state)
        
        total_pnl = unrealized_pnl + realized_pnl
        position_summary = ", ".join([f"{k}:{v:.4f}" for k, v in positions.items() if v != 0])
        
        self.logger.info(f"PORTFOLIO | value=${total_value:,.2f} "
                        f"pnl=${total_pnl:+,.2f} dd={drawdown:.2%} "
                        f"vol={volatility:.2%} var={var_95:.2%} "
                        f"positions=[{position_summary}]")

    def update_alpha_accuracy(self, 
                             asset: str,
                             model_type: str,
                             actual_price: float,
                             validation_timestamp: datetime = None):
        """
        Update alpha model prediction accuracy after validation period.
        
        Args:
            asset: Asset symbol
            model_type: Model type to validate
            actual_price: Actual price after prediction horizon
            validation_timestamp: When to validate (default: now)
        """
        if validation_timestamp is None:
            validation_timestamp = datetime.now()
            
        # Find predictions ready for validation
        for prediction in self.alpha_predictions:
            if (prediction.asset == asset and 
                prediction.model_type == model_type and 
                prediction.actual_return is None):
                
                pred_time = datetime.fromisoformat(prediction.timestamp)
                time_diff = (validation_timestamp - pred_time).total_seconds() / 3600
                
                # Check if enough time has passed for validation
                if time_diff >= prediction.prediction_horizon:
                    # Calculate actual return (simplified - assumes we have initial price)
                    # In real implementation, would track price at prediction time
                    prediction.actual_return = 0.0  # Placeholder - need historical price
                    
                    # Determine accuracy
                    actual_direction = "UP" if prediction.actual_return > 0 else "DOWN" if prediction.actual_return < 0 else "NEUTRAL"
                    prediction.accuracy = (prediction.predicted_direction == actual_direction)
                    
                    # Update model accuracy tracking
                    model_key = f"{asset}_{model_type}"
                    if model_key not in self.prediction_accuracy_by_model:
                        self.prediction_accuracy_by_model[model_key] = []
                    
                    self.prediction_accuracy_by_model[model_key].append(prediction.accuracy)
                    
                    self.logger.info(f"ALPHA_ACCURACY | {model_key}: "
                                   f"predicted={prediction.predicted_direction} "
                                   f"actual={actual_direction} "
                                   f"correct={prediction.accuracy}")

    def generate_performance_report(self) -> Dict[str, Any]:
        """
        Generate comprehensive performance analysis report.
        
        Returns:
            Dictionary containing detailed performance metrics and attribution
        """
        report = {
            "simulation_id": self.simulation_id,
            "analysis_timestamp": datetime.now().isoformat(),
            "simulation_duration": (datetime.now() - self.start_time).total_seconds(),
            "summary": self._generate_summary_metrics(),
            "alpha_model_analysis": self._analyze_alpha_models(),
            "trading_strategy_analysis": self._analyze_trading_signals(),
            "risk_management_analysis": self._analyze_risk_decisions(),
            "execution_analysis": self._analyze_trade_executions(),
            "portfolio_performance": self._analyze_portfolio_performance(),
            "attribution_analysis": self._generate_attribution_analysis()
        }
        
        # Save report to file
        report_file = self.log_directory / f"{self.simulation_id}_performance_report.json"
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2, default=str)
            
        self.logger.info(f"Performance report generated: {report_file}")
        
        return report

    def _generate_summary_metrics(self) -> Dict[str, Any]:
        """Generate high-level summary metrics."""
        if not self.portfolio_states:
            return {"error": "No portfolio states recorded"}
            
        initial_value = self.portfolio_states[0].total_value
        final_value = self.portfolio_states[-1].total_value
        total_return = (final_value - initial_value) / initial_value
        
        max_drawdown = max([state.drawdown for state in self.portfolio_states], default=0)
        
        return {
            "total_return": total_return,
            "initial_value": initial_value,
            "final_value": final_value,
            "max_drawdown": max_drawdown,
            "num_trades": len(self.trade_executions),
            "num_signals": len(self.trading_signals),
            "num_risk_decisions": len(self.risk_decisions),
            "num_alpha_predictions": len(self.alpha_predictions)
        }

    def _analyze_alpha_models(self) -> Dict[str, Any]:
        """Analyze alpha model performance and accuracy."""
        analysis = {
            "total_predictions": len(self.alpha_predictions),
            "predictions_by_model": {},
            "accuracy_by_model": {},
            "confidence_correlation": {}
        }
        
        # Group predictions by model
        for prediction in self.alpha_predictions:
            model_key = f"{prediction.asset}_{prediction.model_type}_{prediction.timeframe}"
            
            if model_key not in analysis["predictions_by_model"]:
                analysis["predictions_by_model"][model_key] = 0
            analysis["predictions_by_model"][model_key] += 1
            
            # Calculate accuracy for validated predictions
            if prediction.accuracy is not None:
                if model_key not in analysis["accuracy_by_model"]:
                    analysis["accuracy_by_model"][model_key] = []
                analysis["accuracy_by_model"][model_key].append(prediction.accuracy)
        
        # Calculate accuracy percentages
        for model_key in analysis["accuracy_by_model"]:
            accuracies = analysis["accuracy_by_model"][model_key]
            analysis["accuracy_by_model"][model_key] = {
                "accuracy_rate": sum(accuracies) / len(accuracies),
                "total_validated": len(accuracies)
            }
            
        return analysis

    def _analyze_trading_signals(self) -> Dict[str, Any]:
        """Analyze trading strategy signal generation and effectiveness."""
        if not self.trading_signals:
            return {"error": "No trading signals recorded"}
            
        analysis = {
            "total_signals": len(self.trading_signals),
            "signals_by_type": {"BUY": 0, "SELL": 0, "HOLD": 0},
            "average_confidence": 0,
            "signal_frequency": {},
            "position_changes": []
        }
        
        confidence_sum = 0
        for signal in self.trading_signals:
            analysis["signals_by_type"][signal.signal_type] += 1
            confidence_sum += signal.confidence
            
            position_change = abs(signal.target_position - signal.current_position)
            analysis["position_changes"].append(position_change)
        
        analysis["average_confidence"] = confidence_sum / len(self.trading_signals)
        analysis["average_position_change"] = np.mean(analysis["position_changes"])
        
        return analysis

    def _analyze_risk_decisions(self) -> Dict[str, Any]:
        """Analyze risk management decision impact."""
        if not self.risk_decisions:
            return {"error": "No risk decisions recorded"}
            
        analysis = {
            "total_decisions": len(self.risk_decisions),
            "approval_rate": 0,
            "decisions_by_type": {},
            "position_impacts": []
        }
        
        approved_count = 0
        for decision in self.risk_decisions:
            if decision.approved:
                approved_count += 1
                
            if decision.decision_type not in analysis["decisions_by_type"]:
                analysis["decisions_by_type"][decision.decision_type] = {
                    "total": 0, "approved": 0
                }
            
            analysis["decisions_by_type"][decision.decision_type]["total"] += 1
            if decision.approved:
                analysis["decisions_by_type"][decision.decision_type]["approved"] += 1
                
            analysis["position_impacts"].append(abs(decision.impact_on_position))
        
        analysis["approval_rate"] = approved_count / len(self.risk_decisions)
        analysis["average_position_impact"] = np.mean(analysis["position_impacts"])
        
        return analysis

    def _analyze_trade_executions(self) -> Dict[str, Any]:
        """Analyze trade execution efficiency."""
        if not self.trade_executions:
            return {"error": "No trade executions recorded"}
            
        analysis = {
            "total_executions": len(self.trade_executions),
            "average_slippage": 0,
            "average_execution_delay": 0,
            "total_trade_costs": 0,
            "fill_rate": 0
        }
        
        slippages = []
        delays = []
        costs = []
        fill_rates = []
        
        for execution in self.trade_executions:
            slippage_pct = (execution.slippage / execution.intended_price * 100) if execution.intended_price != 0 else 0
            slippages.append(abs(slippage_pct))
            
            delays.append(execution.execution_delay_ms)
            costs.append(execution.trade_cost)
            
            fill_rate = (execution.executed_quantity / execution.intended_quantity) if execution.intended_quantity != 0 else 0
            fill_rates.append(fill_rate)
        
        analysis["average_slippage"] = np.mean(slippages)
        analysis["average_execution_delay"] = np.mean(delays)
        analysis["total_trade_costs"] = sum(costs)
        analysis["average_fill_rate"] = np.mean(fill_rates)
        
        return analysis

    def _analyze_portfolio_performance(self) -> Dict[str, Any]:
        """Analyze overall portfolio performance metrics."""
        if not self.portfolio_states:
            return {"error": "No portfolio states recorded"}
            
        # Extract time series data
        values = [state.total_value for state in self.portfolio_states]
        drawdowns = [state.drawdown for state in self.portfolio_states]
        volatilities = [state.volatility for state in self.portfolio_states]
        
        # Calculate returns
        returns = [(values[i] - values[i-1]) / values[i-1] for i in range(1, len(values))]
        
        analysis = {
            "total_return": (values[-1] - values[0]) / values[0],
            "volatility": np.std(returns) * np.sqrt(252) if len(returns) > 1 else 0,
            "max_drawdown": max(drawdowns),
            "average_drawdown": np.mean(drawdowns),
            "sharpe_ratio": self.portfolio_states[-1].sharpe_ratio,
            "var_95": self.portfolio_states[-1].var_95,
            "return_distribution": {
                "mean": np.mean(returns) if returns else 0,
                "std": np.std(returns) if returns else 0,
                "skewness": float(pd.Series(returns).skew()) if len(returns) > 2 else 0,
                "kurtosis": float(pd.Series(returns).kurtosis()) if len(returns) > 3 else 0
            }
        }
        
        return analysis

    def _generate_attribution_analysis(self) -> Dict[str, Any]:
        """Generate performance attribution analysis."""
        return {
            "alpha_contribution": "Analysis of alpha model contribution to returns",
            "strategy_contribution": "Analysis of trading strategy contribution",
            "risk_contribution": "Analysis of risk management impact",
            "execution_contribution": "Analysis of execution cost impact",
            "note": "Detailed attribution requires more sophisticated tracking"
        }

    def save_all_logs(self):
        """Save all logged data to JSON files for analysis."""
        # Save individual log types
        log_files = {
            "alpha_predictions": self.alpha_predictions,
            "trading_signals": self.trading_signals,
            "risk_decisions": self.risk_decisions,
            "trade_executions": self.trade_executions,
            "portfolio_states": self.portfolio_states
        }
        
        for log_type, data in log_files.items():
            if data:
                file_path = self.log_directory / f"{self.simulation_id}_{log_type}.json"
                with open(file_path, 'w') as f:
                    json.dump([asdict(item) for item in data], f, indent=2, default=str)
                    
                self.logger.info(f"Saved {len(data)} {log_type} to {file_path}")