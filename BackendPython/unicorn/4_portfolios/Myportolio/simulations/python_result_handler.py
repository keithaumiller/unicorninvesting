#!/usr/bin/env python3
"""
Python Result Handler for Myportolio Simulations
================================================

Handles storage, retrieval, and analysis of simulation results using
professional-grade result handling patterns.

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

import os
import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
import logging
import sqlite3
from dataclasses import dataclass

logger = logging.getLogger(__name__)

@dataclass
class SimulationSummary:
    """Summary of simulation results."""
    simulation_id: str
    simulation_type: str
    timestamp: datetime
    total_return: float
    sharpe_ratio: float
    max_drawdown: float
    trades_count: int
    status: str

class PythonResultHandler:
    """
    Python result handler for Myportolio simulations.
    
    Provides comprehensive result storage, analysis, and comparison
    capabilities using professional result handling patterns.
    """
    
    def __init__(self, portfolio_path: str = None):
        """
        Initialize result handler.
        
        Args:
            portfolio_path: Path to Myportolio directory
        """
        if portfolio_path is None:
            portfolio_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio"
        
        self.portfolio_path = Path(portfolio_path)
        self.simulations_path = self.portfolio_path / "simulations"
        
        # Initialize results database
        self.db_path = self.simulations_path / "simulation_results.db"
        self._initialize_database()

    def _initialize_database(self):
        """Initialize SQLite database for simulation tracking."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            # Simulations table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS simulations (
                    simulation_id TEXT PRIMARY KEY,
                    simulation_type TEXT NOT NULL,
                    timestamp TEXT NOT NULL,
                    strategy TEXT,
                    start_date TEXT,
                    end_date TEXT,
                    status TEXT DEFAULT 'running',
                    total_return REAL,
                    sharpe_ratio REAL,
                    max_drawdown REAL,
                    trades_count INTEGER,
                    win_rate REAL,
                    profit_factor REAL,
                    config_json TEXT,
                    results_path TEXT
                )
            ''')
            
            # Performance metrics table for detailed tracking
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS performance_metrics (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    simulation_id TEXT,
                    metric_name TEXT,
                    metric_value REAL,
                    metric_type TEXT,
                    timestamp TEXT,
                    FOREIGN KEY (simulation_id) REFERENCES simulations (simulation_id)
                )
            ''')
            
            # Trades table for detailed trade analysis
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS trades (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    simulation_id TEXT,
                    trade_id TEXT,
                    symbol TEXT,
                    entry_time TEXT,
                    exit_time TEXT,
                    quantity REAL,
                    entry_price REAL,
                    exit_price REAL,
                    pnl REAL,
                    pnl_percent REAL,
                    duration_hours REAL,
                    FOREIGN KEY (simulation_id) REFERENCES simulations (simulation_id)
                )
            ''')
            
            conn.commit()
            logger.info("Simulation results database initialized")

    def store_simulation_result(self, 
                               simulation_id: str,
                               simulation_type: str,
                               results: Dict[str, Any],
                               config: Dict[str, Any],
                               results_path: str) -> bool:
        """
        Store simulation results in database and file system.
        
        Args:
            simulation_id: Unique simulation identifier
            simulation_type: Type of simulation (backtest, paper, optimization)
            results: Complete simulation results
            config: Simulation configuration
            results_path: Path to detailed results file
            
        Returns:
            Success status
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.cursor()
                
                # Extract performance metrics
                performance = results.get("performance", {})
                execution = results.get("execution", {})
                
                # Insert main simulation record
                cursor.execute('''
                    INSERT OR REPLACE INTO simulations 
                    (simulation_id, simulation_type, timestamp, strategy, start_date, end_date,
                     status, total_return, sharpe_ratio, max_drawdown, trades_count, 
                     win_rate, profit_factor, config_json, results_path)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    simulation_id,
                    simulation_type,
                    results.get("timestamp", datetime.now().isoformat()),
                    config.get("strategy", "unknown"),
                    config.get("start-date", ""),
                    config.get("end-date", ""),
                    execution.get("status", "completed"),
                    performance.get("total_return", 0.0),
                    performance.get("sharpe_ratio", 0.0),
                    performance.get("max_drawdown", 0.0),
                    performance.get("trades_count", 0),
                    performance.get("win_rate", 0.0),
                    performance.get("profit_factor", 0.0),
                    json.dumps(config),
                    results_path
                ))
                
                # Store detailed performance metrics
                self._store_performance_metrics(cursor, simulation_id, performance)
                
                # Store trades if available
                if "trades" in results:
                    self._store_trades(cursor, simulation_id, results["trades"])
                
                conn.commit()
                logger.info(f"Simulation results stored: {simulation_id}")
                return True
                
        except Exception as e:
            logger.error(f"Failed to store simulation results: {str(e)}")
            return False

    def _store_performance_metrics(self, cursor, simulation_id: str, performance: Dict[str, Any]):
        """Store detailed performance metrics."""
        timestamp = datetime.now().isoformat()
        
        for metric_name, metric_value in performance.items():
            if isinstance(metric_value, (int, float)):
                cursor.execute('''
                    INSERT INTO performance_metrics 
                    (simulation_id, metric_name, metric_value, metric_type, timestamp)
                    VALUES (?, ?, ?, ?, ?)
                ''', (simulation_id, metric_name, float(metric_value), "performance", timestamp))

    def _store_trades(self, cursor, simulation_id: str, trades: List[Dict[str, Any]]):
        """Store individual trade records."""
        for trade in trades:
            cursor.execute('''
                INSERT INTO trades 
                (simulation_id, trade_id, symbol, entry_time, exit_time, quantity,
                 entry_price, exit_price, pnl, pnl_percent, duration_hours)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                simulation_id,
                trade.get("id", ""),
                trade.get("symbol", ""),
                trade.get("entry_time", ""),
                trade.get("exit_time", ""), 
                trade.get("quantity", 0.0),
                trade.get("entry_price", 0.0),
                trade.get("exit_price", 0.0),
                trade.get("pnl", 0.0),
                trade.get("pnl_percent", 0.0),
                trade.get("duration_hours", 0.0)
            ))

    def get_simulation_summary(self, simulation_id: str) -> Optional[SimulationSummary]:
        """
        Get summary of a specific simulation.
        
        Args:
            simulation_id: Simulation identifier
            
        Returns:
            Simulation summary or None if not found
        """
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute('''
                SELECT simulation_id, simulation_type, timestamp, total_return,
                       sharpe_ratio, max_drawdown, trades_count, status
                FROM simulations 
                WHERE simulation_id = ?
            ''', (simulation_id,))
            
            result = cursor.fetchone()
            
            if result:
                return SimulationSummary(
                    simulation_id=result[0],
                    simulation_type=result[1],
                    timestamp=datetime.fromisoformat(result[2]),
                    total_return=result[3] or 0.0,
                    sharpe_ratio=result[4] or 0.0,
                    max_drawdown=result[5] or 0.0,
                    trades_count=result[6] or 0,
                    status=result[7] or "unknown"
                )
        
        return None

    def list_simulations(self, 
                        simulation_type: Optional[str] = None,
                        limit: int = 50) -> List[SimulationSummary]:
        """
        List simulations with optional filtering.
        
        Args:
            simulation_type: Filter by simulation type
            limit: Maximum number of results
            
        Returns:
            List of simulation summaries
        """
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            if simulation_type:
                cursor.execute('''
                    SELECT simulation_id, simulation_type, timestamp, total_return,
                           sharpe_ratio, max_drawdown, trades_count, status
                    FROM simulations 
                    WHERE simulation_type = ?
                    ORDER BY timestamp DESC
                    LIMIT ?
                ''', (simulation_type, limit))
            else:
                cursor.execute('''
                    SELECT simulation_id, simulation_type, timestamp, total_return,
                           sharpe_ratio, max_drawdown, trades_count, status
                    FROM simulations 
                    ORDER BY timestamp DESC
                    LIMIT ?
                ''', (limit,))
            
            results = []
            for row in cursor.fetchall():
                results.append(SimulationSummary(
                    simulation_id=row[0],
                    simulation_type=row[1],
                    timestamp=datetime.fromisoformat(row[2]),
                    total_return=row[3] or 0.0,
                    sharpe_ratio=row[4] or 0.0,
                    max_drawdown=row[5] or 0.0,
                    trades_count=row[6] or 0,
                    status=row[7] or "unknown"
                ))
            
            return results

    def compare_simulations(self, simulation_ids: List[str]) -> Dict[str, Any]:
        """
        Compare multiple simulations.
        
        Args:
            simulation_ids: List of simulation IDs to compare
            
        Returns:
            Comparison analysis
        """
        simulations = []
        
        for sim_id in simulation_ids:
            summary = self.get_simulation_summary(sim_id)
            if summary:
                simulations.append(summary)
        
        if not simulations:
            return {"error": "No valid simulations found"}
        
        # Create comparison DataFrame
        data = []
        for sim in simulations:
            data.append({
                "simulation_id": sim.simulation_id,
                "type": sim.simulation_type,
                "total_return": sim.total_return,
                "sharpe_ratio": sim.sharpe_ratio,
                "max_drawdown": sim.max_drawdown,
                "trades_count": sim.trades_count
            })
        
        df = pd.DataFrame(data)
        
        # Calculate comparison metrics
        comparison = {
            "simulations_compared": len(simulations),
            "best_return": {
                "simulation_id": df.loc[df["total_return"].idxmax(), "simulation_id"],
                "value": df["total_return"].max()
            },
            "best_sharpe": {
                "simulation_id": df.loc[df["sharpe_ratio"].idxmax(), "simulation_id"],
                "value": df["sharpe_ratio"].max()
            },
            "lowest_drawdown": {
                "simulation_id": df.loc[df["max_drawdown"].idxmin(), "simulation_id"],
                "value": df["max_drawdown"].min()
            },
            "summary_statistics": {
                "avg_return": df["total_return"].mean(),
                "avg_sharpe": df["sharpe_ratio"].mean(),
                "avg_drawdown": df["max_drawdown"].mean(),
                "total_trades": df["trades_count"].sum()
            },
            "detailed_data": data
        }
        
        return comparison

    def generate_performance_report(self, simulation_id: str) -> Dict[str, Any]:
        """
        Generate comprehensive performance report for a simulation.
        
        Args:
            simulation_id: Simulation identifier
            
        Returns:
            Detailed performance report
        """
        summary = self.get_simulation_summary(simulation_id)
        
        if not summary:
            return {"error": f"Simulation {simulation_id} not found"}
        
        # Get detailed metrics
        with sqlite3.connect(self.db_path) as conn:
            # Performance metrics
            metrics_df = pd.read_sql_query('''
                SELECT metric_name, metric_value, timestamp
                FROM performance_metrics 
                WHERE simulation_id = ?
                ORDER BY timestamp
            ''', conn, params=(simulation_id,))
            
            # Trades analysis
            trades_df = pd.read_sql_query('''
                SELECT * FROM trades 
                WHERE simulation_id = ?
                ORDER BY entry_time
            ''', conn, params=(simulation_id,))
        
        report = {
            "simulation_id": simulation_id,
            "summary": {
                "type": summary.simulation_type,
                "timestamp": summary.timestamp.isoformat(),
                "total_return": summary.total_return,
                "sharpe_ratio": summary.sharpe_ratio,
                "max_drawdown": summary.max_drawdown,
                "trades_count": summary.trades_count,
                "status": summary.status
            },
            "detailed_metrics": metrics_df.to_dict('records') if not metrics_df.empty else [],
            "trade_analysis": self._analyze_trades(trades_df) if not trades_df.empty else {},
            "risk_analysis": self._analyze_risk_metrics(summary),
            "recommendations": self._generate_recommendations(summary)
        }
        
        return report

    def _analyze_trades(self, trades_df: pd.DataFrame) -> Dict[str, Any]:
        """Analyze trade performance."""
        if trades_df.empty:
            return {}
        
        analysis = {
            "total_trades": len(trades_df),
            "winning_trades": len(trades_df[trades_df["pnl"] > 0]),
            "losing_trades": len(trades_df[trades_df["pnl"] < 0]),
            "win_rate": len(trades_df[trades_df["pnl"] > 0]) / len(trades_df) * 100,
            "avg_win": trades_df[trades_df["pnl"] > 0]["pnl"].mean() if len(trades_df[trades_df["pnl"] > 0]) > 0 else 0,
            "avg_loss": trades_df[trades_df["pnl"] < 0]["pnl"].mean() if len(trades_df[trades_df["pnl"] < 0]) > 0 else 0,
            "largest_win": trades_df["pnl"].max(),
            "largest_loss": trades_df["pnl"].min(),
            "avg_trade_duration": trades_df["duration_hours"].mean(),
            "profit_factor": abs(trades_df[trades_df["pnl"] > 0]["pnl"].sum() / trades_df[trades_df["pnl"] < 0]["pnl"].sum()) if trades_df[trades_df["pnl"] < 0]["pnl"].sum() != 0 else float('inf')
        }
        
        return analysis

    def _analyze_risk_metrics(self, summary: SimulationSummary) -> Dict[str, Any]:
        """Analyze risk characteristics."""
        return {
            "risk_score": self._calculate_risk_score(summary),
            "return_to_risk_ratio": summary.total_return / abs(summary.max_drawdown) if summary.max_drawdown != 0 else float('inf'),
            "risk_category": self._categorize_risk(summary.max_drawdown),
            "sharpe_category": self._categorize_sharpe(summary.sharpe_ratio)
        }

    def _calculate_risk_score(self, summary: SimulationSummary) -> float:
        """Calculate composite risk score (0-100, lower is better)."""
        drawdown_score = min(abs(summary.max_drawdown) * 100, 50)
        sharpe_penalty = max(0, (1.0 - summary.sharpe_ratio) * 25) if summary.sharpe_ratio > 0 else 50
        
        return drawdown_score + sharpe_penalty

    def _categorize_risk(self, max_drawdown: float) -> str:
        """Categorize risk level based on drawdown."""
        abs_drawdown = abs(max_drawdown)
        
        if abs_drawdown <= 0.05:
            return "Low Risk"
        elif abs_drawdown <= 0.15:
            return "Moderate Risk"
        elif abs_drawdown <= 0.25:
            return "High Risk"
        else:
            return "Very High Risk"

    def _categorize_sharpe(self, sharpe_ratio: float) -> str:
        """Categorize performance based on Sharpe ratio."""
        if sharpe_ratio >= 2.0:
            return "Excellent"
        elif sharpe_ratio >= 1.5:
            return "Very Good"
        elif sharpe_ratio >= 1.0:
            return "Good"
        elif sharpe_ratio >= 0.5:
            return "Acceptable"
        else:
            return "Poor"

    def _generate_recommendations(self, summary: SimulationSummary) -> List[str]:
        """Generate actionable recommendations."""
        recommendations = []
        
        if summary.sharpe_ratio < 1.0:
            recommendations.append("Consider improving risk-adjusted returns by optimizing position sizing")
        
        if abs(summary.max_drawdown) > 0.15:
            recommendations.append("Implement stricter risk controls to reduce maximum drawdown")
        
        if summary.trades_count < 10:
            recommendations.append("Extend simulation period or adjust strategy frequency for more robust results")
        
        if summary.total_return < 0:
            recommendations.append("Review strategy parameters and consider paper trading before live deployment")
        
        if summary.sharpe_ratio > 2.0 and abs(summary.max_drawdown) < 0.05:
            recommendations.append("Strategy shows excellent risk-adjusted performance - consider live deployment")
        
        return recommendations

if __name__ == "__main__":
    # Example usage
    handler = PythonResultHandler()
    
    # List recent simulations
    simulations = handler.list_simulations(limit=10)
    print(f"Found {len(simulations)} simulations")
    
    for sim in simulations:
        print(f"  {sim.simulation_id}: {sim.total_return:.2%} return, {sim.sharpe_ratio:.2f} Sharpe")
