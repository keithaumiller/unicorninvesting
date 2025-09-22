"""
Performance tracking for alpha models.

Provides comprehensive performance monitoring, benchmarking, and analysis.
"""

from typing import Dict, Any, List, Optional, Union, Tuple
from pathlib import Path
import sqlite3
import json
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging

from ...core.interfaces.data_interfaces import PerformanceMetrics, ForecastResult

logger = logging.getLogger(__name__)

class PerformanceTracker:
    """
    Comprehensive performance tracking for alpha models.
    
    Tracks model performance over time, provides benchmarking,
    and generates performance analytics and reports.
    """
    
    def __init__(self, tracker_path: Optional[Union[str, Path]] = None):
        """
        Initialize performance tracker.
        
        Args:
            tracker_path: Path to performance database (default: auto-detect)
        """
        if tracker_path is None:
            current_dir = Path(__file__).parent
            self.tracker_path = current_dir / "performance.db"
        else:
            self.tracker_path = Path(tracker_path)
        
        # Create directory
        self.tracker_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Initialize database
        self._init_database()
        
        logger.info(f"PerformanceTracker initialized: {self.tracker_path}")
    
    def _init_database(self):
        """Initialize SQLite database with performance tracking tables"""
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Real-time performance tracking
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS realtime_performance (
                    tracking_id TEXT PRIMARY KEY,
                    model_id TEXT NOT NULL,
                    methodology TEXT NOT NULL,
                    asset_class TEXT NOT NULL,
                    symbol TEXT NOT NULL,
                    prediction_time TIMESTAMP NOT NULL,
                    forecast_horizon INTEGER,
                    predicted_value REAL,
                    actual_value REAL,
                    confidence REAL,
                    direction_predicted TEXT,
                    direction_actual TEXT,
                    accuracy_points REAL,
                    absolute_error REAL,
                    percentage_error REAL,
                    hit BOOLEAN,
                    metadata_json TEXT
                )
            """)
            
            # Daily performance summaries
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS daily_performance (
                    summary_id TEXT PRIMARY KEY,
                    model_id TEXT NOT NULL,
                    date DATE NOT NULL,
                    predictions_count INTEGER,
                    hits_count INTEGER,
                    hit_rate REAL,
                    avg_confidence REAL,
                    avg_absolute_error REAL,
                    avg_percentage_error REAL,
                    total_return REAL,
                    daily_sharpe REAL,
                    max_drawdown REAL,
                    volatility REAL,
                    best_prediction REAL,
                    worst_prediction REAL,
                    metadata_json TEXT
                )
            """)
            
            # Benchmark comparisons
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS benchmark_performance (
                    benchmark_id TEXT PRIMARY KEY,
                    model_id TEXT NOT NULL,
                    benchmark_name TEXT NOT NULL,
                    comparison_date DATE NOT NULL,
                    model_return REAL,
                    benchmark_return REAL,
                    excess_return REAL,
                    model_sharpe REAL,
                    benchmark_sharpe REAL,
                    model_volatility REAL,
                    benchmark_volatility REAL,
                    correlation REAL,
                    beta REAL,
                    alpha REAL,
                    information_ratio REAL,
                    tracking_error REAL,
                    metadata_json TEXT
                )
            """)
            
            # Model degradation tracking
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS model_degradation (
                    degradation_id TEXT PRIMARY KEY,
                    model_id TEXT NOT NULL,
                    check_date TIMESTAMP NOT NULL,
                    performance_window_days INTEGER,
                    current_performance REAL,
                    baseline_performance REAL,
                    degradation_pct REAL,
                    degradation_status TEXT,
                    alert_triggered BOOLEAN,
                    recommended_action TEXT,
                    metadata_json TEXT
                )
            """)
            
            # Create indexes
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_realtime_model ON realtime_performance (model_id)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_realtime_time ON realtime_performance (prediction_time)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_daily_model ON daily_performance (model_id)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_daily_date ON daily_performance (date)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_benchmark_model ON benchmark_performance (model_id)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_degradation_model ON model_degradation (model_id)")
            
            conn.commit()
    
    def track_prediction(self, model_id: str, methodology: str, asset_class: str,
                        symbol: str, forecast: ForecastResult, actual_value: float = None,
                        metadata: Dict[str, Any] = None) -> str:
        """
        Track a single prediction for performance analysis.
        
        Args:
            model_id: Model identifier
            methodology: Methodology name
            asset_class: Asset class
            symbol: Asset symbol
            forecast: Forecast result
            actual_value: Actual observed value (if available)
            metadata: Additional metadata
            
        Returns:
            Tracking ID
        """
        tracking_id = f"{model_id}_{forecast.timestamp.strftime('%Y%m%d_%H%M%S')}"
        
        # Calculate performance metrics if actual value is available
        accuracy_points = None
        absolute_error = None
        percentage_error = None
        hit = None
        direction_actual = None
        
        if actual_value is not None:
            absolute_error = abs(forecast.prediction - actual_value)
            percentage_error = (absolute_error / abs(actual_value)) * 100 if actual_value != 0 else 0
            
            # Determine actual direction
            if actual_value > forecast.prediction:
                direction_actual = 'up'
            elif actual_value < forecast.prediction:
                direction_actual = 'down'
            else:
                direction_actual = 'neutral'
            
            # Calculate hit (directional accuracy)
            hit = (forecast.direction == direction_actual)
            
            # Accuracy points (higher is better)
            accuracy_points = max(0, 100 - percentage_error)
        
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                INSERT OR REPLACE INTO realtime_performance (
                    tracking_id, model_id, methodology, asset_class, symbol,
                    prediction_time, forecast_horizon, predicted_value, actual_value,
                    confidence, direction_predicted, direction_actual,
                    accuracy_points, absolute_error, percentage_error, hit, metadata_json
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                tracking_id, model_id, methodology, asset_class, symbol,
                forecast.timestamp, forecast.forecast_horizon, forecast.prediction, actual_value,
                forecast.confidence, forecast.direction, direction_actual,
                accuracy_points, absolute_error, percentage_error, hit,
                json.dumps(metadata) if metadata else None
            ))
            
            conn.commit()
        
        logger.debug(f"Tracked prediction {tracking_id}")
        return tracking_id
    
    def update_prediction_actual(self, tracking_id: str, actual_value: float):
        """
        Update prediction with actual observed value.
        
        Args:
            tracking_id: Tracking identifier
            actual_value: Actual observed value
        """
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Get existing prediction
            cursor.execute("""
                SELECT predicted_value, direction_predicted 
                FROM realtime_performance 
                WHERE tracking_id = ?
            """, (tracking_id,))
            
            result = cursor.fetchone()
            if not result:
                logger.warning(f"Tracking ID not found: {tracking_id}")
                return
            
            predicted_value, direction_predicted = result
            
            # Calculate performance metrics
            absolute_error = abs(predicted_value - actual_value)
            percentage_error = (absolute_error / abs(actual_value)) * 100 if actual_value != 0 else 0
            
            # Determine actual direction
            if actual_value > predicted_value:
                direction_actual = 'up'
            elif actual_value < predicted_value:
                direction_actual = 'down'
            else:
                direction_actual = 'neutral'
            
            # Calculate hit and accuracy points
            hit = (direction_predicted == direction_actual)
            accuracy_points = max(0, 100 - percentage_error)
            
            # Update record
            cursor.execute("""
                UPDATE realtime_performance 
                SET actual_value = ?, direction_actual = ?, accuracy_points = ?,
                    absolute_error = ?, percentage_error = ?, hit = ?
                WHERE tracking_id = ?
            """, (
                actual_value, direction_actual, accuracy_points,
                absolute_error, percentage_error, hit, tracking_id
            ))
            
            conn.commit()
        
        logger.debug(f"Updated prediction {tracking_id} with actual value {actual_value}")
    
    def generate_daily_summary(self, model_id: str, date: datetime.date = None) -> Dict[str, Any]:
        """
        Generate daily performance summary for a model.
        
        Args:
            model_id: Model identifier
            date: Date for summary (default: today)
            
        Returns:
            Daily performance summary
        """
        if date is None:
            date = datetime.now().date()
        
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Get daily predictions
            cursor.execute("""
                SELECT COUNT(*), AVG(confidence), AVG(absolute_error), 
                       AVG(percentage_error), SUM(CASE WHEN hit = 1 THEN 1 ELSE 0 END),
                       MIN(accuracy_points), MAX(accuracy_points)
                FROM realtime_performance 
                WHERE model_id = ? AND DATE(prediction_time) = ?
                AND actual_value IS NOT NULL
            """, (model_id, date))
            
            result = cursor.fetchone()
            
            if not result or result[0] == 0:
                logger.info(f"No predictions found for model {model_id} on {date}")
                return {}
            
            predictions_count, avg_confidence, avg_absolute_error, avg_percentage_error, hits_count, worst_prediction, best_prediction = result
            
            hit_rate = (hits_count / predictions_count) * 100 if predictions_count > 0 else 0
            
            # Calculate daily return and risk metrics
            daily_return, daily_sharpe, max_drawdown, volatility = self._calculate_daily_metrics(
                model_id, date
            )
            
            summary = {
                'model_id': model_id,
                'date': date.isoformat(),
                'predictions_count': predictions_count,
                'hits_count': hits_count,
                'hit_rate': hit_rate,
                'avg_confidence': avg_confidence,
                'avg_absolute_error': avg_absolute_error,
                'avg_percentage_error': avg_percentage_error,
                'total_return': daily_return,
                'daily_sharpe': daily_sharpe,
                'max_drawdown': max_drawdown,
                'volatility': volatility,
                'best_prediction': best_prediction,
                'worst_prediction': worst_prediction
            }
            
            # Store summary in database
            summary_id = f"{model_id}_{date.strftime('%Y%m%d')}"
            
            cursor.execute("""
                INSERT OR REPLACE INTO daily_performance (
                    summary_id, model_id, date, predictions_count, hits_count,
                    hit_rate, avg_confidence, avg_absolute_error, avg_percentage_error,
                    total_return, daily_sharpe, max_drawdown, volatility,
                    best_prediction, worst_prediction, metadata_json
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                summary_id, model_id, date, predictions_count, hits_count,
                hit_rate, avg_confidence, avg_absolute_error, avg_percentage_error,
                daily_return, daily_sharpe, max_drawdown, volatility,
                best_prediction, worst_prediction, json.dumps(summary)
            ))
            
            conn.commit()
        
        return summary
    
    def _calculate_daily_metrics(self, model_id: str, date: datetime.date) -> Tuple[float, float, float, float]:
        """Calculate daily return and risk metrics"""
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Get predictions with actual values for the day
            cursor.execute("""
                SELECT predicted_value, actual_value, confidence
                FROM realtime_performance 
                WHERE model_id = ? AND DATE(prediction_time) = ?
                AND actual_value IS NOT NULL
                ORDER BY prediction_time
            """, (model_id, date))
            
            predictions = cursor.fetchall()
            
            if not predictions:
                return 0.0, 0.0, 0.0, 0.0
            
            # Calculate returns based on prediction accuracy
            returns = []
            for predicted, actual, confidence in predictions:
                # Return is proportional to directional accuracy and confidence
                direction_correct = (predicted - actual) * (actual - predicted) >= 0
                accuracy = confidence if direction_correct else -confidence
                returns.append(accuracy / 100.0)  # Normalize to [-1, 1]
            
            returns = np.array(returns)
            
            # Calculate metrics
            total_return = np.sum(returns)
            volatility = np.std(returns) if len(returns) > 1 else 0.0
            
            # Sharpe ratio (assuming risk-free rate of 0)
            daily_sharpe = np.mean(returns) / volatility if volatility > 0 else 0.0
            
            # Max drawdown
            cumulative_returns = np.cumsum(returns)
            running_max = np.maximum.accumulate(cumulative_returns)
            drawdown = (cumulative_returns - running_max)
            max_drawdown = np.min(drawdown)
            
            return float(total_return), float(daily_sharpe), float(max_drawdown), float(volatility)
    
    def compare_to_benchmark(self, model_id: str, benchmark_name: str,
                           benchmark_returns: List[float], 
                           comparison_date: datetime.date = None) -> Dict[str, Any]:
        """
        Compare model performance to benchmark.
        
        Args:
            model_id: Model identifier
            benchmark_name: Name of benchmark
            benchmark_returns: Benchmark return series
            comparison_date: Date for comparison
            
        Returns:
            Benchmark comparison results
        """
        if comparison_date is None:
            comparison_date = datetime.now().date()
        
        # Get model returns for the same period
        model_returns = self._get_model_returns(model_id, len(benchmark_returns))
        
        if len(model_returns) != len(benchmark_returns):
            logger.warning(f"Mismatched return series lengths: model={len(model_returns)}, benchmark={len(benchmark_returns)}")
            return {}
        
        model_returns = np.array(model_returns)
        benchmark_returns = np.array(benchmark_returns)
        
        # Calculate comparison metrics
        model_return = np.sum(model_returns)
        benchmark_return = np.sum(benchmark_returns)
        excess_return = model_return - benchmark_return
        
        model_vol = np.std(model_returns)
        benchmark_vol = np.std(benchmark_returns)
        
        model_sharpe = np.mean(model_returns) / model_vol if model_vol > 0 else 0
        benchmark_sharpe = np.mean(benchmark_returns) / benchmark_vol if benchmark_vol > 0 else 0
        
        correlation = np.corrcoef(model_returns, benchmark_returns)[0, 1] if len(model_returns) > 1 else 0
        
        # Beta and Alpha
        if benchmark_vol > 0:
            beta = correlation * (model_vol / benchmark_vol)
            alpha = model_return - beta * benchmark_return
        else:
            beta = 0
            alpha = model_return
        
        # Information ratio and tracking error
        excess_returns = model_returns - benchmark_returns
        tracking_error = np.std(excess_returns) if len(excess_returns) > 1 else 0
        information_ratio = np.mean(excess_returns) / tracking_error if tracking_error > 0 else 0
        
        comparison = {
            'model_id': model_id,
            'benchmark_name': benchmark_name,
            'comparison_date': comparison_date.isoformat(),
            'model_return': float(model_return),
            'benchmark_return': float(benchmark_return),
            'excess_return': float(excess_return),
            'model_sharpe': float(model_sharpe),
            'benchmark_sharpe': float(benchmark_sharpe),
            'model_volatility': float(model_vol),
            'benchmark_volatility': float(benchmark_vol),
            'correlation': float(correlation),
            'beta': float(beta),
            'alpha': float(alpha),
            'information_ratio': float(information_ratio),
            'tracking_error': float(tracking_error)
        }
        
        # Store in database
        benchmark_id = f"{model_id}_{benchmark_name}_{comparison_date.strftime('%Y%m%d')}"
        
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                INSERT OR REPLACE INTO benchmark_performance (
                    benchmark_id, model_id, benchmark_name, comparison_date,
                    model_return, benchmark_return, excess_return,
                    model_sharpe, benchmark_sharpe, model_volatility, benchmark_volatility,
                    correlation, beta, alpha, information_ratio, tracking_error, metadata_json
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                benchmark_id, model_id, benchmark_name, comparison_date,
                model_return, benchmark_return, excess_return,
                model_sharpe, benchmark_sharpe, model_vol, benchmark_vol,
                correlation, beta, alpha, information_ratio, tracking_error,
                json.dumps(comparison)
            ))
            
            conn.commit()
        
        return comparison
    
    def _get_model_returns(self, model_id: str, periods: int) -> List[float]:
        """Get model returns for specified number of periods"""
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                SELECT predicted_value, actual_value, confidence
                FROM realtime_performance 
                WHERE model_id = ? AND actual_value IS NOT NULL
                ORDER BY prediction_time DESC
                LIMIT ?
            """, (model_id, periods))
            
            predictions = cursor.fetchall()
            
            returns = []
            for predicted, actual, confidence in reversed(predictions):
                # Calculate return based on prediction accuracy
                direction_correct = (predicted - actual) * (actual - predicted) >= 0
                accuracy = confidence if direction_correct else -confidence
                returns.append(accuracy / 100.0)
        
        return returns
    
    def check_model_degradation(self, model_id: str, window_days: int = 30,
                               threshold_pct: float = 20.0) -> Dict[str, Any]:
        """
        Check for model performance degradation.
        
        Args:
            model_id: Model identifier
            window_days: Window for performance comparison
            threshold_pct: Degradation threshold percentage
            
        Returns:
            Degradation analysis results
        """
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Get baseline performance (first month of data)
            cursor.execute("""
                SELECT AVG(accuracy_points)
                FROM realtime_performance 
                WHERE model_id = ? AND actual_value IS NOT NULL
                ORDER BY prediction_time
                LIMIT 1000
            """, (model_id,))
            
            baseline_result = cursor.fetchone()
            baseline_performance = baseline_result[0] if baseline_result and baseline_result[0] else 0
            
            # Get recent performance
            cutoff_date = datetime.now() - timedelta(days=window_days)
            cursor.execute("""
                SELECT AVG(accuracy_points)
                FROM realtime_performance 
                WHERE model_id = ? AND actual_value IS NOT NULL
                AND prediction_time >= ?
            """, (model_id, cutoff_date))
            
            recent_result = cursor.fetchone()
            current_performance = recent_result[0] if recent_result and recent_result[0] else 0
            
            # Calculate degradation
            if baseline_performance > 0:
                degradation_pct = ((baseline_performance - current_performance) / baseline_performance) * 100
            else:
                degradation_pct = 0
            
            # Determine status and recommendations
            if degradation_pct >= threshold_pct:
                status = "DEGRADED"
                alert_triggered = True
                recommended_action = "Retrain model with recent data"
            elif degradation_pct >= threshold_pct * 0.5:
                status = "WARNING"
                alert_triggered = False
                recommended_action = "Monitor closely, consider retraining"
            else:
                status = "HEALTHY"
                alert_triggered = False
                recommended_action = "Continue monitoring"
            
            degradation_analysis = {
                'model_id': model_id,
                'check_date': datetime.now().isoformat(),
                'window_days': window_days,
                'current_performance': current_performance,
                'baseline_performance': baseline_performance,
                'degradation_pct': degradation_pct,
                'degradation_status': status,
                'alert_triggered': alert_triggered,
                'recommended_action': recommended_action
            }
            
            # Store in database
            degradation_id = f"{model_id}_deg_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            
            cursor.execute("""
                INSERT INTO model_degradation (
                    degradation_id, model_id, check_date, performance_window_days,
                    current_performance, baseline_performance, degradation_pct,
                    degradation_status, alert_triggered, recommended_action, metadata_json
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                degradation_id, model_id, datetime.now(), window_days,
                current_performance, baseline_performance, degradation_pct,
                status, alert_triggered, recommended_action,
                json.dumps(degradation_analysis)
            ))
            
            conn.commit()
        
        return degradation_analysis
    
    def get_performance_report(self, model_id: str, days: int = 30) -> Dict[str, Any]:
        """
        Generate comprehensive performance report.
        
        Args:
            model_id: Model identifier
            days: Number of days to include in report
            
        Returns:
            Comprehensive performance report
        """
        cutoff_date = datetime.now() - timedelta(days=days)
        
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Overall statistics
            cursor.execute("""
                SELECT COUNT(*), AVG(accuracy_points), AVG(hit), AVG(confidence),
                       AVG(absolute_error), AVG(percentage_error),
                       MIN(accuracy_points), MAX(accuracy_points)
                FROM realtime_performance 
                WHERE model_id = ? AND prediction_time >= ?
                AND actual_value IS NOT NULL
            """, (model_id, cutoff_date))
            
            overall_stats = cursor.fetchone()
            
            # Daily performance trend
            cursor.execute("""
                SELECT date, hit_rate, avg_confidence, total_return
                FROM daily_performance 
                WHERE model_id = ? AND date >= DATE(?)
                ORDER BY date
            """, (model_id, cutoff_date))
            
            daily_trends = cursor.fetchall()
            
            # Recent degradation checks
            cursor.execute("""
                SELECT degradation_status, degradation_pct, recommended_action
                FROM model_degradation 
                WHERE model_id = ? 
                ORDER BY check_date DESC
                LIMIT 1
            """, (model_id,))
            
            degradation_info = cursor.fetchone()
            
            report = {
                'model_id': model_id,
                'report_period_days': days,
                'generated_at': datetime.now().isoformat(),
                'overall_performance': {
                    'total_predictions': overall_stats[0] if overall_stats else 0,
                    'avg_accuracy': overall_stats[1] if overall_stats else 0,
                    'hit_rate': (overall_stats[2] * 100) if overall_stats and overall_stats[2] else 0,
                    'avg_confidence': overall_stats[3] if overall_stats else 0,
                    'avg_absolute_error': overall_stats[4] if overall_stats else 0,
                    'avg_percentage_error': overall_stats[5] if overall_stats else 0,
                    'worst_accuracy': overall_stats[6] if overall_stats else 0,
                    'best_accuracy': overall_stats[7] if overall_stats else 0
                },
                'daily_trends': [
                    {
                        'date': trend[0],
                        'hit_rate': trend[1],
                        'avg_confidence': trend[2],
                        'total_return': trend[3]
                    }
                    for trend in daily_trends
                ],
                'model_health': {
                    'status': degradation_info[0] if degradation_info else 'UNKNOWN',
                    'degradation_pct': degradation_info[1] if degradation_info else 0,
                    'recommendation': degradation_info[2] if degradation_info else 'No recent checks'
                }
            }
        
        return report
    
    def get_tracker_stats(self) -> Dict[str, Any]:
        """
        Get performance tracker statistics.
        
        Returns:
            Tracker statistics
        """
        with sqlite3.connect(self.tracker_path) as conn:
            cursor = conn.cursor()
            
            # Count records
            cursor.execute("SELECT COUNT(*) FROM realtime_performance")
            total_predictions = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(*) FROM realtime_performance WHERE actual_value IS NOT NULL")
            completed_predictions = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(DISTINCT model_id) FROM realtime_performance")
            tracked_models = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(*) FROM daily_performance")
            daily_summaries = cursor.fetchone()[0]
            
            # Database size
            db_size = self.tracker_path.stat().st_size / (1024 * 1024)
            
            return {
                'total_predictions': total_predictions,
                'completed_predictions': completed_predictions,
                'tracked_models': tracked_models,
                'daily_summaries': daily_summaries,
                'completion_rate': (completed_predictions / total_predictions * 100) if total_predictions > 0 else 0,
                'database_size_mb': db_size,
                'tracker_path': str(self.tracker_path)
            }
    
    def __str__(self) -> str:
        """String representation of performance tracker"""
        return f"PerformanceTracker({self.tracker_path})"
    
    def __repr__(self) -> str:
        """Detailed representation of performance tracker"""
        stats = self.get_tracker_stats()
        return (f"PerformanceTracker(path='{self.tracker_path}', "
                f"tracked_models={stats['tracked_models']}, "
                f"total_predictions={stats['total_predictions']})")