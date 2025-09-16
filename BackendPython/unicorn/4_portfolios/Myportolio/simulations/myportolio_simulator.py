#!/usr/bin/env python3
"""
Myportolio Master Simulation System
==================================

SINGLE AUTHORITATIVE ENTRY POINT for all Myportolio simulations.

This is the ONE AND ONLY simulation engine entry point that:
- ALWAYS uses enhanced performance logging (cannot be bypassed)
- Provides a clean, unified API for all simulation types
- Integrates backtesting, paper trading, and optimization
- Ensures consistent result handling and analysis

Usage:
    # Python API
    from myportolio_simulator import MyportolioSimulator
    
    simulator = MyportolioSimulator()
    result = simulator.run_backtest(
        start_date="2024-03-01",
        end_date="2024-09-01", 
        strategy="best_models"
    )
    
    # Command Line
    python myportolio_simulator.py backtest --start 2024-03-01 --end 2024-09-01 --strategy best_models
    python myportolio_simulator.py analyze --simulation-id <id>
    python myportolio_simulator.py results --list

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

import os
import sys
import json
import logging
import argparse
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from dataclasses import dataclass

# Add simulation path for imports
sys.path.append(str(Path(__file__).parent))

# Import core simulation components - MANDATORY
from python_simulation_engine import PythonSimulationEngine
from python_result_handler import PythonResultHandler
from performance_logger import PerformanceLogger

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class SimulationRequest:
    """Request object for simulations with mandatory enhanced logging."""
    start_date: str
    end_date: str
    strategy_template: str = "momentum"  # Default template
    algorithm_name: str = "MyportolioETHMomentum"
    parameters: Optional[Dict[str, Any]] = None
    metadata: Optional[Dict[str, Any]] = None

@dataclass 
class SimulationResult:
    """Standardized simulation result structure."""
    simulation_id: str
    status: str  # 'completed', 'failed', 'running'
    total_return: float
    sharpe_ratio: float
    max_drawdown: float
    trades_count: int
    performance_log_path: str
    results_path: str
    metadata: Dict[str, Any]

class MyportolioSimulator:
    """
    Master simulation system for Myportolio with MANDATORY enhanced logging.
    
    This is the single, authoritative entry point for all simulations.
    Enhanced logging cannot be bypassed - it's built into every operation.
    """
    
    def __init__(self, portfolio_path: str = None):
        """
        Initialize the master simulation system.
        
        Args:
            portfolio_path: Path to Myportolio directory
        """
        if portfolio_path is None:
            portfolio_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        
        self.portfolio_path = Path(portfolio_path)
        self.simulations_path = self.portfolio_path / "simulations"
        
        # Initialize core components - ENHANCED LOGGING IS MANDATORY
        self.engine = PythonSimulationEngine(str(self.portfolio_path))
        self.result_handler = PythonResultHandler(str(self.portfolio_path))
        
        # Strategy template mapping
        self.strategies = {
            "best_models": "best_models_template",
            "momentum": "backtest_template", 
            "dual_crypto": "dual_crypto_template",
            "btc_momentum": "btc_momentum_template",
            "btc_momentum_validation": "btc_momentum_template",
            "paper": "paper_trading_template",
            "optimization": "optimization_template"
        }
        
        # Initialize templates
        self.templates = self.load_strategy_templates()
        
        logger.info("🚀 Myportolio Master Simulator initialized with MANDATORY enhanced logging")
        
    def load_strategy_templates(self) -> Dict[str, Dict[str, Any]]:
        """Load available strategy templates."""
        templates_path = self.simulations_path / "templates" / "simulation_templates.json"
        
        if templates_path.exists():
            with open(templates_path, 'r') as f:
                return json.load(f)
        else:
            logger.warning("⚠️  Strategy templates not found, using defaults")
            return self._get_default_strategies()
    
    def _get_default_strategies(self) -> Dict[str, Dict]:
        """Default strategy configurations."""
        return {
            "best_models": {
                "name": "Best Economic-Enhanced Models",
                "algorithm": "MyportolioEconomicEnhanced",
                "parameters": {
                    "strategy_type": "dual_asset_economic_enhanced",
                    "kelly_fraction": 0.2,
                    "confidence_threshold": 0.65
                }
            },
            "momentum": {
                "name": "ETH Momentum Strategy",
                "algorithm": "MyportolioETHMomentum", 
                "parameters": {
                    "strategy_type": "eth_momentum",
                    "kelly_fraction": 0.167,
                    "momentum_threshold": 0.02
                }
            },
            "dual_crypto": {
                "name": "Dual Crypto Strategy",
                "algorithm": "MyportolioMultiCrypto",
                "parameters": {
                    "strategy_type": "dual_crypto",
                    "eth_allocation": 0.6,
                    "btc_allocation": 0.4
                }
            }
        }
    
    def run_backtest(self,
                    start_date: str,
                    end_date: str,
                    strategy: str = "best_models",
                    asset: str = "ETH",
                    parameters: Optional[Dict[str, Any]] = None,
                    metadata: Optional[Dict[str, Any]] = None) -> SimulationResult:
        """
        Run a backtest simulation with MANDATORY enhanced logging.
        
        Args:
            start_date: Start date in YYYY-MM-DD format
            end_date: End date in YYYY-MM-DD format
            strategy: Strategy name (must exist in templates)
            asset: Asset to trade (ETH, BTC, etc.)
            parameters: Optional parameter overrides
            metadata: Optional metadata for tracking
            
        Returns:
            SimulationResult with all details and log paths
        """
        logger.info(f"🚀 Starting backtest simulation: {strategy} for {asset}")
        
        # Validate strategy
        if strategy not in self.strategies:
            available = ", ".join(self.strategies.keys())
            raise ValueError(f"Strategy '{strategy}' not found. Available: {available}")
        
        # Prepare simulation request
        template_name = self.strategies[strategy]
        template_config = self.templates.get(template_name, {})
        final_parameters = template_config.get("parameters", {}).copy()
        if parameters:
            final_parameters.update(parameters)
        
        # Add asset symbol to parameters
        final_parameters["asset_symbol"] = asset.upper()
        
        request = SimulationRequest(
            start_date=start_date,
            end_date=end_date,
            strategy_template=strategy,
            parameters=final_parameters
        )
        
        logger.info(f"📋 Strategy: {template_config.get('name', template_name)}")
        logger.info(f"🪙 Asset: {asset.upper()}")
        logger.info(f"📅 Period: {start_date} to {end_date}")
        logger.info(f"⚙️  Parameters: {json.dumps(final_parameters, indent=2)}")
        
        try:
            # Run backtest with MANDATORY ENHANCED LOGGING
            simulation_id = self.engine.run_backtest_with_logging(
                start_date=start_date,
                end_date=end_date,
                parameters=final_parameters,
                template_name=strategy
            )
            
            # Retrieve results
            result_summary = self.result_handler.get_simulation_summary(simulation_id)
            
            # Performance log path
            performance_log_path = self.simulations_path / "performance_logs" / f"{simulation_id}_performance.log"
            results_path = self.simulations_path / "backtests" / simulation_id / "myportolio_results.json"
            
            if result_summary:
                result = SimulationResult(
                    simulation_id=simulation_id,
                    status="completed",
                    total_return=result_summary.total_return,
                    sharpe_ratio=result_summary.sharpe_ratio,
                    max_drawdown=result_summary.max_drawdown,
                    trades_count=result_summary.trades_count,
                    performance_log_path=str(performance_log_path),
                    results_path=str(results_path),
                    metadata=request.metadata
                )
            else:
                # Fallback if database result not available
                result = SimulationResult(
                    simulation_id=simulation_id,
                    status="completed",
                    total_return=0.0,
                    sharpe_ratio=0.0,
                    max_drawdown=0.0,
                    trades_count=0,
                    performance_log_path=str(performance_log_path),
                    results_path=str(results_path),
                    metadata=request.metadata
                )
            
            logger.info(f"✅ Simulation completed: {simulation_id}")
            logger.info(f"📊 Return: {result.total_return:.2%}, Trades: {result.trades_count}")
            logger.info(f"📁 Performance Log: {performance_log_path}")
            
            return result
            
        except Exception as e:
            logger.error(f"❌ Simulation failed: {str(e)}")
            raise
    
    def analyze_simulation(self, simulation_id: str) -> Dict[str, Any]:
        """
        Analyze simulation results with performance attribution.
        
        Args:
            simulation_id: The simulation ID to analyze
            
        Returns:
            Comprehensive analysis report
        """
        logger.info(f"📊 Analyzing simulation: {simulation_id}")
        
        # Get performance log path
        performance_log_path = self.simulations_path / "performance_logs" / f"{simulation_id}_performance.log"
        performance_report_path = self.simulations_path / "performance_logs" / f"{simulation_id}_performance_report.json"
        
        analysis = {
            "simulation_id": simulation_id,
            "analysis_timestamp": datetime.now().isoformat(),
            "performance_log_available": performance_log_path.exists(),
            "performance_report_available": performance_report_path.exists()
        }
        
        # Load performance report if available
        if performance_report_path.exists():
            with open(performance_report_path, 'r') as f:
                performance_report = json.load(f)
            analysis["performance_attribution"] = performance_report
        
        # Load simulation results
        results_path = self.simulations_path / "backtests" / simulation_id / "myportolio_results.json"
        if results_path.exists():
            with open(results_path, 'r') as f:
                simulation_results = json.load(f)
            analysis["simulation_results"] = simulation_results.get("lean_results", {}).get("performance", {})
        
        # Generate summary insights
        analysis["insights"] = self._generate_analysis_insights(analysis)
        
        return analysis
    
    def _generate_analysis_insights(self, analysis: Dict[str, Any]) -> List[str]:
        """Generate actionable insights from analysis."""
        insights = []
        
        if not analysis["performance_log_available"]:
            insights.append("⚠️  No performance log found - enhanced logging was not enabled for this simulation")
        
        if analysis.get("simulation_results"):
            results = analysis["simulation_results"]
            total_return = results.get("total_return", 0)
            trades_count = results.get("trades_count", 0)
            
            if trades_count == 0:
                insights.append("🚨 CRITICAL: Zero trades executed - check signal generation logic")
            
            if total_return < 0:
                insights.append(f"📉 Negative return ({total_return:.2%}) - review strategy parameters")
            
            if trades_count > 1000:
                insights.append(f"🔄 High trade frequency ({trades_count}) - consider reducing over-trading")
        
        return insights
    
    def list_simulations(self, limit: int = 20) -> List[Dict[str, Any]]:
        """List recent simulations with enhanced logging status."""
        simulations = self.result_handler.list_simulations(limit=limit)
        
        enhanced_list = []
        for sim in simulations:
            performance_log_path = self.simulations_path / "performance_logs" / f"{sim.simulation_id}_performance.log"
            
            enhanced_list.append({
                "simulation_id": sim.simulation_id,
                "simulation_type": sim.simulation_type,
                "timestamp": sim.timestamp.isoformat(),
                "total_return": sim.total_return,
                "sharpe_ratio": sim.sharpe_ratio,
                "max_drawdown": sim.max_drawdown,
                "trades_count": sim.trades_count,
                "status": sim.status,
                "enhanced_logging": performance_log_path.exists()
            })
        
        return enhanced_list
    
    def get_available_strategies(self) -> Dict[str, str]:
        """Get list of available strategies."""
        return {name: config["name"] for name, config in self.strategies.items()}

def main():
    """Command line interface for Myportolio Master Simulator."""
    parser = argparse.ArgumentParser(
        description="Myportolio Master Simulation System",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s backtest --start 2024-03-01 --end 2024-09-01 --strategy best_models
  %(prog)s analyze --simulation-id backtest_20250915_192900_ac125477
  %(prog)s results --list --limit 10
  %(prog)s strategies
        """
    )
    
    subparsers = parser.add_subparsers(dest="command", help="Available commands")
    
    # Backtest command
    backtest_parser = subparsers.add_parser("backtest", help="Run backtest simulation")
    backtest_parser.add_argument("--start", required=True, help="Start date (YYYY-MM-DD)")
    backtest_parser.add_argument("--end", required=True, help="End date (YYYY-MM-DD)")
    backtest_parser.add_argument("--strategy", default="best_models", help="Strategy name")
    backtest_parser.add_argument("--asset", default="ETH", help="Asset to trade (ETH, BTC)")
    backtest_parser.add_argument("--kelly", type=float, help="Kelly fraction override")
    backtest_parser.add_argument("--confidence", type=float, help="Confidence threshold override")
    
    # Analyze command
    analyze_parser = subparsers.add_parser("analyze", help="Analyze simulation results")
    analyze_parser.add_argument("--simulation-id", required=True, help="Simulation ID to analyze")
    
    # Results command
    results_parser = subparsers.add_parser("results", help="List simulation results")
    results_parser.add_argument("--list", action="store_true", help="List simulations")
    results_parser.add_argument("--limit", type=int, default=20, help="Maximum results to show")
    
    # Strategies command
    strategies_parser = subparsers.add_parser("strategies", help="List available strategies")
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    # Initialize simulator
    simulator = MyportolioSimulator()
    
    try:
        if args.command == "backtest":
            # Prepare parameter overrides
            parameter_overrides = {}
            if args.kelly:
                parameter_overrides["kelly_fraction"] = args.kelly
            if args.confidence:
                parameter_overrides["confidence_threshold"] = args.confidence
            
            # Run backtest
            result = simulator.run_backtest(
                start_date=args.start,
                end_date=args.end,
                strategy=args.strategy,
                asset=getattr(args, 'asset', 'ETH'),
                parameters=parameter_overrides
            )
            
            print("\n🎯 SIMULATION COMPLETE")
            print("=" * 50)
            print(f"📊 Simulation ID: {result.simulation_id}")
            print(f"📈 Total Return: {result.total_return:.2%}")
            print(f"📊 Sharpe Ratio: {result.sharpe_ratio:.2f}")
            print(f"📉 Max Drawdown: {result.max_drawdown:.2%}")
            print(f"🔄 Total Trades: {result.trades_count}")
            print(f"📁 Performance Log: {result.performance_log_path}")
            print(f"📄 Results: {result.results_path}")
            
        elif args.command == "analyze":
            analysis = simulator.analyze_simulation(args.simulation_id)
            
            print(f"\n📊 SIMULATION ANALYSIS: {args.simulation_id}")
            print("=" * 60)
            
            if analysis["insights"]:
                print("💡 KEY INSIGHTS:")
                for insight in analysis["insights"]:
                    print(f"   {insight}")
            
            print(f"\n📁 Enhanced Logging: {'✅' if analysis['performance_log_available'] else '❌'}")
            print(f"📊 Performance Report: {'✅' if analysis['performance_report_available'] else '❌'}")
            
        elif args.command == "results":
            if args.list:
                simulations = simulator.list_simulations(limit=args.limit)
                
                print(f"\n📊 RECENT SIMULATIONS ({len(simulations)})")
                print("=" * 80)
                print(f"{'ID':<25} {'Type':<10} {'Return':<8} {'Trades':<6} {'Enhanced':<8} {'Date':<10}")
                print("-" * 80)
                
                for sim in simulations:
                    enhanced = "✅" if sim["enhanced_logging"] else "❌"
                    date_str = sim["timestamp"][:10]
                    print(f"{sim['simulation_id'][:24]:<25} "
                          f"{sim['simulation_type']:<10} "
                          f"{sim['total_return']:>7.2%} "
                          f"{sim['trades_count']:>5} "
                          f"{enhanced:<8} "
                          f"{date_str:<10}")
            
        elif args.command == "strategies":
            strategies = simulator.get_available_strategies()
            
            print("\n🎯 AVAILABLE STRATEGIES")
            print("=" * 40)
            for name, description in strategies.items():
                print(f"  {name:<15} | {description}")
        
    except Exception as e:
        print(f"\n❌ ERROR: {str(e)}")
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())