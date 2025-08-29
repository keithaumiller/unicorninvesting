"""
Model Performance Tracking System

Comprehensive tracking of model performance across training, validation, and live periods.
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta
import json
import sqlite3
from pathlib import Path
import logging
from dataclasses import dataclass, asdict
from enum import Enum

logger = logging.getLogger(__name__)

class ModelStage(Enum):
    """Model performance tracking stages."""
    TRAINING = "training"
    VALIDATION = "validation"
    LIVE = "live"

class MetricType(Enum):
    """Types of performance metrics."""
    ACCURACY = "accuracy"
    RETURN = "return"
    RISK = "risk"
    SIGNAL = "signal"

@dataclass
class PerformanceMetric:
    """Individual performance metric."""
    metric_name: str
    metric_type: MetricType
    value: float
    timestamp: datetime
    stage: ModelStage
    period: str  # e.g., "daily", "weekly", "monthly"
    metadata: Dict[str, Any] = None

@dataclass
class ModelPerformanceRecord:
    """Complete performance record for a model."""
    model_id: str
    asset_name: str
    model_type: str
    model_version: str
    created_at: datetime
    updated_at: datetime
    stage: ModelStage
    metrics: List[PerformanceMetric]
    summary_stats: Dict[str, Any] = None

class PerformanceCalculator:
    """Calculate various performance metrics."""
    
    @staticmethod
    def calculate_returns_metrics(predictions: pd.Series, actuals: pd.Series, signals: pd.Series = None) -> Dict[str, float]:
        """Calculate return-based performance metrics."""
        if len(predictions) != len(actuals):
            raise ValueError("Predictions and actuals must have same length")
        
        # Basic error metrics
        mse = float(np.mean((predictions - actuals) ** 2))
        mae = float(np.mean(np.abs(predictions - actuals)))
        rmse = float(np.sqrt(mse))
        
        # Direction accuracy (if we have signals)
        direction_accuracy = 0.0
        if signals is not None and len(signals) == len(predictions):
            actual_direction = np.sign(actuals.diff().fillna(0))
            predicted_direction = np.sign(signals)
            direction_accuracy = float(np.mean(actual_direction == predicted_direction))
        
        # R-squared
        ss_res = np.sum((actuals - predictions) ** 2)
        ss_tot = np.sum((actuals - np.mean(actuals)) ** 2)
        r2 = float(1 - (ss_res / ss_tot)) if ss_tot != 0 else 0.0
        
        return {
            'mse': mse,
            'mae': mae,
            'rmse': rmse,
            'r2': r2,
            'direction_accuracy': direction_accuracy
        }
    
    @staticmethod
    def calculate_signal_metrics(signals: pd.Series, returns: pd.Series) -> Dict[str, float]:
        """Calculate signal-based performance metrics."""
        if len(signals) != len(returns):
            raise ValueError("Signals and returns must have same length")
        
        # Signal statistics
        total_signals = int(np.sum(signals != 0))
        buy_signals = int(np.sum(signals > 0))
        sell_signals = int(np.sum(signals < 0))
        
        # Signal returns
        signal_returns = signals * returns
        win_rate = float(np.mean(signal_returns > 0)) if total_signals > 0 else 0.0
        
        # Average returns
        avg_return = float(np.mean(signal_returns[signal_returns != 0])) if total_signals > 0 else 0.0
        avg_win = float(np.mean(signal_returns[signal_returns > 0])) if np.sum(signal_returns > 0) > 0 else 0.0
        avg_loss = float(np.mean(signal_returns[signal_returns < 0])) if np.sum(signal_returns < 0) > 0 else 0.0
        
        # Risk metrics
        volatility = float(np.std(signal_returns[signal_returns != 0])) if total_signals > 0 else 0.0
        sharpe_ratio = float(avg_return / volatility) if volatility > 0 else 0.0
        
        # Maximum drawdown
        cumulative_returns = np.cumsum(signal_returns)
        running_max = np.maximum.accumulate(cumulative_returns)
        drawdown = cumulative_returns - running_max
        max_drawdown = float(np.min(drawdown))
        
        return {
            'total_signals': total_signals,
            'buy_signals': buy_signals,
            'sell_signals': sell_signals,
            'win_rate': win_rate,
            'avg_return': avg_return,
            'avg_win': avg_win,
            'avg_loss': avg_loss,
            'volatility': volatility,
            'sharpe_ratio': sharpe_ratio,
            'max_drawdown': max_drawdown
        }
    
    @staticmethod
    def calculate_risk_metrics(returns: pd.Series, benchmark_returns: pd.Series = None) -> Dict[str, float]:
        """Calculate risk-based performance metrics."""
        # Basic risk metrics
        volatility = float(returns.std())
        downside_volatility = float(returns[returns < 0].std())
        
        # VaR calculations
        var_95 = float(np.percentile(returns, 5))
        var_99 = float(np.percentile(returns, 1))
        
        # Expected shortfall (CVaR)
        cvar_95 = float(returns[returns <= var_95].mean())
        cvar_99 = float(returns[returns <= var_99].mean())
        
        # Skewness and kurtosis
        skewness = float(returns.skew())
        kurtosis = float(returns.kurtosis())
        
        metrics = {
            'volatility': volatility,
            'downside_volatility': downside_volatility,
            'var_95': var_95,
            'var_99': var_99,
            'cvar_95': cvar_95,
            'cvar_99': cvar_99,
            'skewness': skewness,
            'kurtosis': kurtosis
        }
        
        # Beta calculation if benchmark provided
        if benchmark_returns is not None and len(benchmark_returns) == len(returns):
            covariance = float(np.cov(returns, benchmark_returns)[0, 1])
            benchmark_variance = float(np.var(benchmark_returns))
            beta = covariance / benchmark_variance if benchmark_variance != 0 else 0.0
            metrics['beta'] = beta
        
        return metrics

class PerformanceDatabase:
    """SQLite database for storing performance metrics."""
    
    def __init__(self, db_path: str = "model_performance.db"):
        """Initialize performance database."""
        self.db_path = db_path
        self.init_database()
    
    def init_database(self):
        """Initialize database tables."""
        with sqlite3.connect(self.db_path) as conn:
            # Models table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS models (
                    model_id TEXT PRIMARY KEY,
                    asset_name TEXT NOT NULL,
                    model_type TEXT NOT NULL,
                    model_version TEXT NOT NULL,
                    created_at TIMESTAMP NOT NULL,
                    updated_at TIMESTAMP NOT NULL,
                    stage TEXT NOT NULL,
                    summary_stats TEXT
                )
            """)
            
            # Metrics table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS metrics (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    model_id TEXT NOT NULL,
                    metric_name TEXT NOT NULL,
                    metric_type TEXT NOT NULL,
                    value REAL NOT NULL,
                    timestamp TIMESTAMP NOT NULL,
                    stage TEXT NOT NULL,
                    period TEXT NOT NULL,
                    metadata TEXT,
                    FOREIGN KEY (model_id) REFERENCES models (model_id)
                )
            """)
            
            # Create indexes
            conn.execute("CREATE INDEX IF NOT EXISTS idx_model_asset ON models (asset_name)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_model_type ON models (model_type)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_metrics_model ON metrics (model_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_metrics_timestamp ON metrics (timestamp)")
    
    def save_model_record(self, record: ModelPerformanceRecord):
        """Save model performance record."""
        with sqlite3.connect(self.db_path) as conn:
            # Save model record
            conn.execute("""
                INSERT OR REPLACE INTO models 
                (model_id, asset_name, model_type, model_version, created_at, updated_at, stage, summary_stats)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                record.model_id,
                record.asset_name,
                record.model_type,
                record.model_version,
                record.created_at,
                record.updated_at,
                record.stage.value,
                json.dumps(record.summary_stats) if record.summary_stats else None
            ))
            
            # Save metrics
            for metric in record.metrics:
                conn.execute("""
                    INSERT INTO metrics 
                    (model_id, metric_name, metric_type, value, timestamp, stage, period, metadata)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    record.model_id,
                    metric.metric_name,
                    metric.metric_type.value,
                    metric.value,
                    metric.timestamp,
                    metric.stage.value,
                    metric.period,
                    json.dumps(metric.metadata) if metric.metadata else None
                ))
    
    def get_model_performance(self, model_id: str) -> Optional[ModelPerformanceRecord]:
        """Get model performance record."""
        with sqlite3.connect(self.db_path) as conn:
            # Get model record
            model_row = conn.execute("""
                SELECT asset_name, model_type, model_version, created_at, updated_at, stage, summary_stats
                FROM models WHERE model_id = ?
            """, (model_id,)).fetchone()
            
            if not model_row:
                return None
            
            # Get metrics
            metrics_rows = conn.execute("""
                SELECT metric_name, metric_type, value, timestamp, stage, period, metadata
                FROM metrics WHERE model_id = ? ORDER BY timestamp
            """, (model_id,)).fetchall()
            
            # Build performance record
            metrics = []
            for row in metrics_rows:
                metric = PerformanceMetric(
                    metric_name=row[0],
                    metric_type=MetricType(row[1]),
                    value=row[2],
                    timestamp=datetime.fromisoformat(row[3]),
                    stage=ModelStage(row[4]),
                    period=row[5],
                    metadata=json.loads(row[6]) if row[6] else None
                )
                metrics.append(metric)
            
            return ModelPerformanceRecord(
                model_id=model_id,
                asset_name=model_row[0],
                model_type=model_row[1],
                model_version=model_row[2],
                created_at=datetime.fromisoformat(model_row[3]),
                updated_at=datetime.fromisoformat(model_row[4]),
                stage=ModelStage(model_row[5]),
                metrics=metrics,
                summary_stats=json.loads(model_row[6]) if model_row[6] else None
            )
    
    def get_asset_models(self, asset_name: str) -> List[str]:
        """Get all model IDs for an asset."""
        with sqlite3.connect(self.db_path) as conn:
            rows = conn.execute("""
                SELECT model_id FROM models WHERE asset_name = ? ORDER BY updated_at DESC
            """, (asset_name,)).fetchall()
            return [row[0] for row in rows]
    
    def get_latest_metrics(self, model_id: str, metric_type: MetricType = None, limit: int = 10) -> List[PerformanceMetric]:
        """Get latest metrics for a model."""
        with sqlite3.connect(self.db_path) as conn:
            query = """
                SELECT metric_name, metric_type, value, timestamp, stage, period, metadata
                FROM metrics WHERE model_id = ?
            """
            params = [model_id]
            
            if metric_type:
                query += " AND metric_type = ?"
                params.append(metric_type.value)
            
            query += " ORDER BY timestamp DESC LIMIT ?"
            params.append(limit)
            
            rows = conn.execute(query, params).fetchall()
            
            metrics = []
            for row in rows:
                metric = PerformanceMetric(
                    metric_name=row[0],
                    metric_type=MetricType(row[1]),
                    value=row[2],
                    timestamp=datetime.fromisoformat(row[3]),
                    stage=ModelStage(row[4]),
                    period=row[5],
                    metadata=json.loads(row[6]) if row[6] else None
                )
                metrics.append(metric)
            
            return metrics

class ModelPerformanceTracker:
    """Main interface for tracking model performance."""
    
    def __init__(self, db_path: str = "model_performance.db"):
        """Initialize performance tracker."""
        self.db = PerformanceDatabase(db_path)
        self.calculator = PerformanceCalculator()
    
    def create_model_record(self, model_id: str, asset_name: str, model_type: str, 
                          model_version: str = "1.0", stage: ModelStage = ModelStage.TRAINING) -> ModelPerformanceRecord:
        """Create new model performance record."""
        now = datetime.now()
        
        return ModelPerformanceRecord(
            model_id=model_id,
            asset_name=asset_name,
            model_type=model_type,
            model_version=model_version,
            created_at=now,
            updated_at=now,
            stage=stage,
            metrics=[]
        )
    
    def track_training_performance(self, model_id: str, predictions: pd.Series, actuals: pd.Series, 
                                 signals: pd.Series = None) -> Dict[str, Any]:
        """Track training performance metrics."""
        
        # Calculate metrics
        return_metrics = self.calculator.calculate_returns_metrics(predictions, actuals, signals)
        
        # Create performance metrics
        metrics = []
        timestamp = datetime.now()
        
        for metric_name, value in return_metrics.items():
            metric = PerformanceMetric(
                metric_name=metric_name,
                metric_type=MetricType.ACCURACY,
                value=value,
                timestamp=timestamp,
                stage=ModelStage.TRAINING,
                period="training_period"
            )
            metrics.append(metric)
        
        # Add signal metrics if available
        if signals is not None:
            signal_metrics = self.calculator.calculate_signal_metrics(signals, actuals.pct_change().fillna(0))
            
            for metric_name, value in signal_metrics.items():
                metric = PerformanceMetric(
                    metric_name=metric_name,
                    metric_type=MetricType.SIGNAL,
                    value=value,
                    timestamp=timestamp,
                    stage=ModelStage.TRAINING,
                    period="training_period"
                )
                metrics.append(metric)
        
        # Update model record
        record = self.db.get_model_performance(model_id)
        if record:
            record.metrics.extend(metrics)
            record.updated_at = timestamp
            record.stage = ModelStage.TRAINING
        else:
            # Create new record (extract from model_id)
            parts = model_id.split('_')
            asset_name = parts[0] if len(parts) > 0 else "unknown"
            model_type = parts[1] if len(parts) > 1 else "unknown"
            
            record = self.create_model_record(model_id, asset_name, model_type, stage=ModelStage.TRAINING)
            record.metrics = metrics
        
        self.db.save_model_record(record)
        
        return {
            'model_id': model_id,
            'timestamp': timestamp,
            'metrics_count': len(metrics),
            'return_metrics': return_metrics,
            'signal_metrics': signal_metrics if signals is not None else {}
        }
    
    def track_validation_performance(self, model_id: str, predictions: pd.Series, actuals: pd.Series, 
                                   signals: pd.Series = None) -> Dict[str, Any]:
        """Track validation performance metrics."""
        
        # Calculate metrics (similar to training but with VALIDATION stage)
        return_metrics = self.calculator.calculate_returns_metrics(predictions, actuals, signals)
        
        metrics = []
        timestamp = datetime.now()
        
        for metric_name, value in return_metrics.items():
            metric = PerformanceMetric(
                metric_name=metric_name,
                metric_type=MetricType.ACCURACY,
                value=value,
                timestamp=timestamp,
                stage=ModelStage.VALIDATION,
                period="validation_period"
            )
            metrics.append(metric)
        
        # Add signal metrics if available
        signal_metrics = {}
        if signals is not None:
            signal_metrics = self.calculator.calculate_signal_metrics(signals, actuals.pct_change().fillna(0))
            
            for metric_name, value in signal_metrics.items():
                metric = PerformanceMetric(
                    metric_name=metric_name,
                    metric_type=MetricType.SIGNAL,
                    value=value,
                    timestamp=timestamp,
                    stage=ModelStage.VALIDATION,
                    period="validation_period"
                )
                metrics.append(metric)
        
        # Update model record
        record = self.db.get_model_performance(model_id)
        if record:
            record.metrics.extend(metrics)
            record.updated_at = timestamp
            record.stage = ModelStage.VALIDATION
            self.db.save_model_record(record)
        
        return {
            'model_id': model_id,
            'timestamp': timestamp,
            'metrics_count': len(metrics),
            'return_metrics': return_metrics,
            'signal_metrics': signal_metrics
        }
    
    def track_live_performance(self, model_id: str, signal: float, actual_return: float, 
                             confidence: float = None) -> Dict[str, Any]:
        """Track single live performance data point."""
        
        timestamp = datetime.now()
        
        # Create metrics for this live performance point
        metrics = [
            PerformanceMetric(
                metric_name="signal_return",
                metric_type=MetricType.RETURN,
                value=signal * actual_return,
                timestamp=timestamp,
                stage=ModelStage.LIVE,
                period="daily",
                metadata={'signal': signal, 'actual_return': actual_return}
            ),
            PerformanceMetric(
                metric_name="signal_accuracy",
                metric_type=MetricType.SIGNAL,
                value=1.0 if np.sign(signal) == np.sign(actual_return) else 0.0,
                timestamp=timestamp,
                stage=ModelStage.LIVE,
                period="daily",
                metadata={'signal': signal, 'actual_return': actual_return}
            )
        ]
        
        if confidence is not None:
            metrics.append(PerformanceMetric(
                metric_name="confidence",
                metric_type=MetricType.SIGNAL,
                value=confidence,
                timestamp=timestamp,
                stage=ModelStage.LIVE,
                period="daily"
            ))
        
        # Update model record
        record = self.db.get_model_performance(model_id)
        if record:
            record.metrics.extend(metrics)
            record.updated_at = timestamp
            self.db.save_model_record(record)
        
        return {
            'model_id': model_id,
            'timestamp': timestamp,
            'signal_return': signal * actual_return,
            'signal_accuracy': 1.0 if np.sign(signal) == np.sign(actual_return) else 0.0
        }
    
    def get_performance_summary(self, model_id: str, stage: ModelStage = None) -> Dict[str, Any]:
        """Get performance summary for a model."""
        record = self.db.get_model_performance(model_id)
        if not record:
            return {'error': f'Model {model_id} not found'}
        
        # Filter metrics by stage if specified
        metrics = record.metrics
        if stage:
            metrics = [m for m in metrics if m.stage == stage]
        
        if not metrics:
            return {'error': f'No metrics found for model {model_id}' + (f' in stage {stage.value}' if stage else '')}
        
        # Group metrics by type
        metric_groups = {}
        for metric in metrics:
            metric_type = metric.metric_type.value
            if metric_type not in metric_groups:
                metric_groups[metric_type] = {}
            
            metric_name = metric.metric_name
            if metric_name not in metric_groups[metric_type]:
                metric_groups[metric_type][metric_name] = []
            
            metric_groups[metric_type][metric_name].append(metric.value)
        
        # Calculate summary statistics
        summary = {
            'model_id': model_id,
            'asset_name': record.asset_name,
            'model_type': record.model_type,
            'stage': stage.value if stage else 'all',
            'total_metrics': len(metrics),
            'last_updated': record.updated_at.isoformat(),
            'metric_summary': {}
        }
        
        for metric_type, type_metrics in metric_groups.items():
            summary['metric_summary'][metric_type] = {}
            for metric_name, values in type_metrics.items():
                summary['metric_summary'][metric_type][metric_name] = {
                    'latest': values[-1],
                    'mean': float(np.mean(values)),
                    'std': float(np.std(values)),
                    'min': float(np.min(values)),
                    'max': float(np.max(values)),
                    'count': len(values)
                }
        
        return summary
    
    def compare_models(self, model_ids: List[str], metric_name: str, stage: ModelStage = None) -> Dict[str, Any]:
        """Compare multiple models on a specific metric."""
        comparison = {
            'metric_name': metric_name,
            'stage': stage.value if stage else 'latest',
            'models': {}
        }
        
        for model_id in model_ids:
            record = self.db.get_model_performance(model_id)
            if not record:
                comparison['models'][model_id] = {'error': 'Model not found'}
                continue
            
            # Find matching metrics
            matching_metrics = [
                m for m in record.metrics 
                if m.metric_name == metric_name and (stage is None or m.stage == stage)
            ]
            
            if matching_metrics:
                values = [m.value for m in matching_metrics]
                comparison['models'][model_id] = {
                    'asset_name': record.asset_name,
                    'model_type': record.model_type,
                    'latest_value': values[-1],
                    'mean_value': float(np.mean(values)),
                    'count': len(values)
                }
            else:
                comparison['models'][model_id] = {'error': f'No {metric_name} metrics found'}
        
        return comparison

# Example usage
if __name__ == "__main__":
    print("✅ Model Performance Tracking System Loaded Successfully")
    print("Available Classes:")
    print("- ModelPerformanceTracker")
    print("- PerformanceDatabase")
    print("- PerformanceCalculator")
    print("Available Enums:")
    print("- ModelStage (TRAINING, VALIDATION, LIVE)")
    print("- MetricType (ACCURACY, RETURN, RISK, SIGNAL)")
