#!/usr/bin/env python3
"""
ETH Forecast Scheduler - Production System

This script manages the automated generation of ETH forecasts across all timeframes
with the 10-iteration retraining rule and production model management.

Features:
- Automated forecast generation scheduling
- 10-iteration retraining management
- Production model selection and deployment
- Performance monitoring and alerting
- Error handling and recovery
- Comprehensive logging

Usage:
    python eth_forecast_scheduler.py [--daemon] [--timeframes 1min,1hour,1day]
"""

import schedule
import time
import sys
import os
import argparse
import logging
import signal
import threading
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Any
import json
import sqlite3

# Add parent directories to path
sys.path.append(str(Path(__file__).parent))

from eth_forecast_generator import ETHForecastGenerator
from eth_forecast_reader import ETHForecastReader

class ForecastScheduler:
    """
    Manages automated forecast generation with intelligent scheduling.
    """
    
    def __init__(self, asset: str = "ETH"):
        self.asset = asset
        self.generator = ETHForecastGenerator(asset)
        self.reader = ETHForecastReader(asset)
        
        # Scheduling configuration
        self.timeframe_schedules = {
            '1min': {
                'interval_minutes': 1,
                'retrain_every_iterations': 10,
                'max_consecutive_errors': 5,
                'priority': 1  # High priority
            },
            '1hour': {
                'interval_minutes': 60, 
                'retrain_every_iterations': 10,
                'max_consecutive_errors': 3,
                'priority': 2  # Medium priority
            },
            '1day': {
                'interval_minutes': 1440,  # 24 hours
                'retrain_every_iterations': 10,
                'max_consecutive_errors': 2,
                'priority': 3  # Lower priority
            }
        }
        
        # State tracking
        self.error_counts = {tf: 0 for tf in self.timeframe_schedules.keys()}
        self.last_success = {tf: None for tf in self.timeframe_schedules.keys()}
        self.is_running = False
        self.shutdown_requested = False
        
        # Setup logging
        self.logger = self._setup_logging()
        
        # Performance tracking
        self.performance_db = Path(__file__).parent / "scheduler_performance.db"
        self._init_performance_db()
        
    def _setup_logging(self) -> logging.Logger:
        """Setup comprehensive logging."""
        logger = logging.getLogger('ETHForecastScheduler')
        logger.setLevel(logging.INFO)
        
        # Create formatters
        detailed_formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(funcName)s:%(lineno)d - %(message)s'
        )
        simple_formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - %(message)s'
        )
        
        # File handler for detailed logs
        log_dir = Path(__file__).parent / "logs"
        log_dir.mkdir(exist_ok=True)
        
        file_handler = logging.FileHandler(log_dir / f"forecast_scheduler_{datetime.now().strftime('%Y%m%d')}.log")
        file_handler.setLevel(logging.DEBUG)
        file_handler.setFormatter(detailed_formatter)
        
        # Console handler for important messages
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)
        console_handler.setFormatter(simple_formatter)
        
        # Add handlers if not already added
        if not logger.handlers:
            logger.addHandler(file_handler)
            logger.addHandler(console_handler)
        
        return logger
    
    def _init_performance_db(self):
        """Initialize scheduler performance database."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS scheduler_runs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timeframe TEXT NOT NULL,
                start_time DATETIME NOT NULL,
                end_time DATETIME,
                success BOOLEAN DEFAULT FALSE,
                error_message TEXT,
                forecast_generated BOOLEAN DEFAULT FALSE,
                retrain_triggered BOOLEAN DEFAULT FALSE,
                execution_time_seconds REAL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS scheduler_alerts (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timeframe TEXT NOT NULL,
                alert_type TEXT NOT NULL,
                message TEXT NOT NULL,
                severity TEXT NOT NULL,
                acknowledged BOOLEAN DEFAULT FALSE,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def setup_schedules(self, timeframes: List[str] = None):
        """Setup forecast generation schedules."""
        if timeframes is None:
            timeframes = list(self.timeframe_schedules.keys())
        
        # Clear existing schedules
        schedule.clear()
        
        for timeframe in timeframes:
            config = self.timeframe_schedules[timeframe]
            interval_minutes = config['interval_minutes']
            
            if timeframe == '1min':
                # Schedule every minute for 1-minute forecasts
                schedule.every().minute.do(self._run_forecast_job, timeframe)
            elif timeframe == '1hour':
                # Schedule every hour for hourly forecasts
                schedule.every().hour.do(self._run_forecast_job, timeframe)
            elif timeframe == '1day':
                # Schedule daily at 00:05 for daily forecasts
                schedule.every().day.at("00:05").do(self._run_forecast_job, timeframe)
            
            self.logger.info(f"Scheduled {timeframe} forecasts every {interval_minutes} minutes")
        
        # Additional maintenance schedules
        schedule.every().hour.do(self._health_check)
        schedule.every().day.at("01:00").do(self._daily_maintenance)
        schedule.every().week.do(self._weekly_cleanup)
        
        self.logger.info("All schedules configured successfully")
    
    def _run_forecast_job(self, timeframe: str):
        """Execute forecast generation job for specific timeframe."""
        job_start = datetime.now()
        run_id = self._log_run_start(timeframe, job_start)
        
        try:
            self.logger.info(f"Starting {timeframe} forecast generation...")
            
            # Generate sample market data (replace with real data source)
            market_data = self._get_market_data(timeframe)
            
            if market_data is None or len(market_data) < 10:
                raise ValueError(f"Insufficient market data for {timeframe}")
            
            # Generate forecast
            forecast = self.generator.generate_forecast(
                timeframe=timeframe,
                market_data=market_data,
                force_retrain=False
            )
            
            # Validate forecast quality
            if not self._validate_forecast(forecast):
                raise ValueError("Generated forecast failed quality validation")
            
            # Success - reset error count
            self.error_counts[timeframe] = 0
            self.last_success[timeframe] = datetime.now()
            
            # Log successful run
            execution_time = (datetime.now() - job_start).total_seconds()
            self._log_run_success(run_id, execution_time, forecast)
            
            self.logger.info(f"✅ {timeframe} forecast generated successfully "
                           f"(model: {forecast.metadata.model_type}, "
                           f"confidence: {forecast.metadata.confidence_score:.3f})")
            
        except Exception as e:
            # Handle error
            self.error_counts[timeframe] += 1
            error_message = str(e)
            
            # Log failed run
            execution_time = (datetime.now() - job_start).total_seconds()
            self._log_run_failure(run_id, execution_time, error_message)
            
            self.logger.error(f"❌ {timeframe} forecast generation failed: {error_message}")
            
            # Check if alert is needed
            self._check_alert_conditions(timeframe)
    
    def _get_market_data(self, timeframe: str) -> Optional[pd.DataFrame]:
        """Get market data for forecast generation (replace with real data source)."""
        try:
            import pandas as pd
            import numpy as np
            
            # Generate sample data based on timeframe
            if timeframe == '1min':
                periods = 2880  # 48 hours
                freq = '1min'
            elif timeframe == '1hour':
                periods = 720   # 30 days
                freq = '1H'
            else:  # 1day
                periods = 365   # 1 year
                freq = '1D'
            
            dates = pd.date_range(end=pd.Timestamp.now(), periods=periods, freq=freq)
            np.random.seed(int(datetime.now().timestamp()) % 1000)  # Varying seed
            
            # Generate realistic ETH-like data
            base_price = 3000
            noise_scale = 30 if timeframe == '1day' else 10 if timeframe == '1hour' else 3
            
            close_prices = base_price + np.cumsum(np.random.randn(len(dates)) * noise_scale * 0.01)
            
            market_data = pd.DataFrame({
                'open': close_prices + np.random.randn(len(dates)) * noise_scale * 0.005,
                'high': close_prices + np.abs(np.random.randn(len(dates)) * noise_scale * 0.01),
                'low': close_prices - np.abs(np.random.randn(len(dates)) * noise_scale * 0.01),
                'close': close_prices,
                'volume': np.random.randint(1000, 50000, len(dates))
            }, index=dates)
            
            # Ensure OHLC consistency
            market_data['high'] = np.maximum.reduce([
                market_data['open'], market_data['high'], 
                market_data['low'], market_data['close']
            ])
            market_data['low'] = np.minimum.reduce([
                market_data['open'], market_data['high'], 
                market_data['low'], market_data['close']
            ])
            
            return market_data
            
        except Exception as e:
            self.logger.error(f"Error generating market data for {timeframe}: {e}")
            return None
    
    def _validate_forecast(self, forecast) -> bool:
        """Validate forecast quality."""
        try:
            # Basic validation checks
            if forecast.metadata.confidence_score < 0.1:
                return False
            
            if len(forecast.predictions) == 0:
                return False
            
            # Check for reasonable price predictions
            for pred_data in forecast.predictions.values():
                price = pred_data.get('predicted_price', 0)
                if price <= 0 or price > 100000:  # Sanity check
                    return False
            
            return True
            
        except Exception:
            return False
    
    def _check_alert_conditions(self, timeframe: str):
        """Check if alerts should be generated."""
        config = self.timeframe_schedules[timeframe]
        error_count = self.error_counts[timeframe]
        max_errors = config['max_consecutive_errors']
        
        if error_count >= max_errors:
            self._generate_alert(
                timeframe=timeframe,
                alert_type='consecutive_errors',
                message=f"{error_count} consecutive errors in {timeframe} forecast generation",
                severity='high'
            )
        
        # Check for stale forecasts
        last_success = self.last_success[timeframe]
        if last_success:
            hours_since_success = (datetime.now() - last_success).total_seconds() / 3600
            max_stale_hours = config['interval_minutes'] / 60 * 3  # 3 intervals
            
            if hours_since_success > max_stale_hours:
                self._generate_alert(
                    timeframe=timeframe,
                    alert_type='stale_forecast',
                    message=f"No successful {timeframe} forecast for {hours_since_success:.1f} hours",
                    severity='medium'
                )
    
    def _generate_alert(self, timeframe: str, alert_type: str, message: str, severity: str):
        """Generate system alert."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT INTO scheduler_alerts (timeframe, alert_type, message, severity)
            VALUES (?, ?, ?, ?)
        ''', (timeframe, alert_type, message, severity))
        
        conn.commit()
        conn.close()
        
        # Log alert
        if severity == 'high':
            self.logger.error(f"🚨 HIGH ALERT - {timeframe}: {message}")
        elif severity == 'medium':
            self.logger.warning(f"⚠️  MEDIUM ALERT - {timeframe}: {message}")
        else:
            self.logger.info(f"ℹ️  INFO ALERT - {timeframe}: {message}")
    
    def _health_check(self):
        """Perform system health check."""
        self.logger.info("🏥 Performing system health check...")
        
        health_status = {}
        
        # Check forecast availability
        for timeframe in self.timeframe_schedules.keys():
            try:
                forecast = self.reader.get_forecast(timeframe)
                health_status[timeframe] = {
                    'available': forecast.quality.value != 'unavailable',
                    'quality': forecast.quality.value,
                    'age_minutes': forecast.forecast_age_minutes,
                    'confidence': forecast.confidence_score
                }
            except Exception as e:
                health_status[timeframe] = {'error': str(e)}
        
        # Check model states
        model_summary = self.generator.get_forecast_summary()
        
        # Generate health report
        healthy_timeframes = sum(1 for tf, status in health_status.items() 
                               if status.get('available', False))
        total_timeframes = len(health_status)
        
        if healthy_timeframes < total_timeframes:
            self._generate_alert(
                timeframe='system',
                alert_type='health_check',
                message=f"Only {healthy_timeframes}/{total_timeframes} timeframes healthy",
                severity='medium' if healthy_timeframes > 0 else 'high'
            )
        
        self.logger.info(f"Health check complete: {healthy_timeframes}/{total_timeframes} timeframes healthy")
    
    def _daily_maintenance(self):
        """Perform daily maintenance tasks."""
        self.logger.info("🧹 Performing daily maintenance...")
        
        try:
            # Clean up old log files
            log_dir = Path(__file__).parent / "logs"
            if log_dir.exists():
                cutoff_date = datetime.now() - timedelta(days=7)
                for log_file in log_dir.glob("*.log"):
                    if datetime.fromtimestamp(log_file.stat().st_mtime) < cutoff_date:
                        log_file.unlink()
                        self.logger.info(f"Removed old log file: {log_file.name}")
            
            # Acknowledge old alerts
            conn = sqlite3.connect(self.performance_db)
            cursor = conn.cursor()
            
            old_alert_date = datetime.now() - timedelta(days=3)
            cursor.execute('''
                UPDATE scheduler_alerts 
                SET acknowledged = TRUE 
                WHERE created_at < ? AND acknowledged = FALSE
            ''', (old_alert_date,))
            
            acknowledged_count = cursor.rowcount
            conn.commit()
            conn.close()
            
            if acknowledged_count > 0:
                self.logger.info(f"Auto-acknowledged {acknowledged_count} old alerts")
            
            # Reset daily error counts
            for timeframe in self.error_counts:
                self.error_counts[timeframe] = 0
            
            self.logger.info("Daily maintenance completed successfully")
            
        except Exception as e:
            self.logger.error(f"Error during daily maintenance: {e}")
    
    def _weekly_cleanup(self):
        """Perform weekly cleanup tasks."""
        self.logger.info("🗑️  Performing weekly cleanup...")
        
        try:
            # Clean up old performance data
            conn = sqlite3.connect(self.performance_db)
            cursor = conn.cursor()
            
            old_data_date = datetime.now() - timedelta(days=30)
            
            cursor.execute('DELETE FROM scheduler_runs WHERE created_at < ?', (old_data_date,))
            runs_deleted = cursor.rowcount
            
            cursor.execute('DELETE FROM scheduler_alerts WHERE created_at < ? AND acknowledged = TRUE', (old_data_date,))
            alerts_deleted = cursor.rowcount
            
            conn.commit()
            conn.close()
            
            self.logger.info(f"Weekly cleanup: {runs_deleted} old runs, {alerts_deleted} old alerts removed")
            
        except Exception as e:
            self.logger.error(f"Error during weekly cleanup: {e}")
    
    def _log_run_start(self, timeframe: str, start_time: datetime) -> int:
        """Log the start of a forecast run."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT INTO scheduler_runs (timeframe, start_time)
            VALUES (?, ?)
        ''', (timeframe, start_time))
        
        run_id = cursor.lastrowid
        conn.commit()
        conn.close()
        
        return run_id
    
    def _log_run_success(self, run_id: int, execution_time: float, forecast):
        """Log successful forecast run."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        # Check if retraining was triggered
        retrain_triggered = getattr(forecast.metadata, 'retrain_triggered', False)
        
        cursor.execute('''
            UPDATE scheduler_runs 
            SET end_time = ?, success = TRUE, forecast_generated = TRUE,
                retrain_triggered = ?, execution_time_seconds = ?
            WHERE id = ?
        ''', (datetime.now(), retrain_triggered, execution_time, run_id))
        
        conn.commit()
        conn.close()
    
    def _log_run_failure(self, run_id: int, execution_time: float, error_message: str):
        """Log failed forecast run."""
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            UPDATE scheduler_runs 
            SET end_time = ?, success = FALSE, error_message = ?, execution_time_seconds = ?
            WHERE id = ?
        ''', (datetime.now(), error_message, execution_time, run_id))
        
        conn.commit()
        conn.close()
    
    def start_daemon(self, timeframes: List[str] = None):
        """Start the scheduler daemon."""
        self.logger.info("🚀 Starting ETH Forecast Scheduler daemon...")
        
        # Setup signal handlers for graceful shutdown
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
        
        # Setup schedules
        self.setup_schedules(timeframes)
        
        self.is_running = True
        
        # Run initial health check
        self._health_check()
        
        self.logger.info("Scheduler daemon started successfully")
        
        # Main scheduler loop
        while self.is_running and not self.shutdown_requested:
            try:
                schedule.run_pending()
                time.sleep(1)  # Check every second
            except Exception as e:
                self.logger.error(f"Error in scheduler loop: {e}")
                time.sleep(5)  # Wait before retrying
        
        self.logger.info("Scheduler daemon stopped")
    
    def stop_daemon(self):
        """Stop the scheduler daemon."""
        self.logger.info("🛑 Stopping scheduler daemon...")
        self.shutdown_requested = True
        self.is_running = False
    
    def _signal_handler(self, signum, frame):
        """Handle shutdown signals."""
        self.logger.info(f"Received signal {signum}, initiating shutdown...")
        self.stop_daemon()
    
    def get_status_report(self) -> Dict[str, Any]:
        """Get comprehensive status report."""
        # Query recent runs
        conn = sqlite3.connect(self.performance_db)
        cursor = conn.cursor()
        
        cursor.execute('''
            SELECT timeframe, COUNT(*) as total_runs,
                   SUM(CASE WHEN success = 1 THEN 1 ELSE 0 END) as successful_runs,
                   AVG(execution_time_seconds) as avg_execution_time
            FROM scheduler_runs 
            WHERE created_at > datetime('now', '-24 hours')
            GROUP BY timeframe
        ''')
        
        recent_stats = {row[0]: {
            'total_runs': row[1],
            'successful_runs': row[2], 
            'success_rate': row[2] / row[1] if row[1] > 0 else 0,
            'avg_execution_time': row[3]
        } for row in cursor.fetchall()}
        
        cursor.execute('''
            SELECT timeframe, alert_type, COUNT(*) as count
            FROM scheduler_alerts 
            WHERE created_at > datetime('now', '-24 hours') AND acknowledged = FALSE
            GROUP BY timeframe, alert_type
        ''')
        
        active_alerts = cursor.fetchall()
        conn.close()
        
        # Get forecast status
        forecast_status = {}
        for timeframe in self.timeframe_schedules.keys():
            try:
                forecast = self.reader.get_forecast(timeframe)
                forecast_status[timeframe] = {
                    'available': forecast.quality.value != 'unavailable',
                    'quality': forecast.quality.value,
                    'age_minutes': forecast.forecast_age_minutes,
                    'model_type': forecast.model_type
                }
            except Exception as e:
                forecast_status[timeframe] = {'error': str(e)}
        
        return {
            'timestamp': datetime.now(),
            'is_running': self.is_running,
            'recent_performance': recent_stats,
            'active_alerts': len(active_alerts),
            'alert_details': active_alerts,
            'forecast_status': forecast_status,
            'error_counts': self.error_counts.copy(),
            'last_success': self.last_success.copy()
        }


def main():
    parser = argparse.ArgumentParser(description='ETH Forecast Scheduler')
    parser.add_argument('--daemon', action='store_true',
                       help='Run as daemon (continuous mode)')
    parser.add_argument('--timeframes', default='1min,1hour,1day',
                       help='Comma-separated timeframes to schedule')
    parser.add_argument('--asset', default='ETH',
                       help='Asset symbol to generate forecasts for')
    parser.add_argument('--status', action='store_true',
                       help='Show status report and exit')
    
    args = parser.parse_args()
    
    # Parse timeframes
    timeframes = [tf.strip() for tf in args.timeframes.split(',')]
    
    # Initialize scheduler
    scheduler = ForecastScheduler(args.asset)
    
    if args.status:
        # Show status report
        status = scheduler.get_status_report()
        print("\n📊 ETH Forecast Scheduler Status Report")
        print("=" * 50)
        print(f"Timestamp: {status['timestamp'].strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"Running: {'✅ Yes' if status['is_running'] else '❌ No'}")
        print(f"Active Alerts: {status['active_alerts']}")
        
        print(f"\n📈 Recent Performance (24h):")
        for tf, stats in status['recent_performance'].items():
            print(f"  {tf:>5}: {stats['successful_runs']}/{stats['total_runs']} "
                  f"({stats['success_rate']:.1%}) avg: {stats['avg_execution_time']:.1f}s")
        
        print(f"\n🔮 Forecast Status:")
        for tf, fstatus in status['forecast_status'].items():
            if 'error' in fstatus:
                print(f"  {tf:>5}: ❌ Error - {fstatus['error']}")
            else:
                availability = "✅" if fstatus['available'] else "❌"
                print(f"  {tf:>5}: {availability} {fstatus['quality']} "
                      f"(age: {fstatus['age_minutes']:.1f}m)")
        
        return 0
    
    print(f"🤖 ETH Forecast Scheduler")
    print(f"Asset: {args.asset}")
    print(f"Timeframes: {', '.join(timeframes)}")
    print(f"Mode: {'Daemon' if args.daemon else 'One-time'}")
    
    if args.daemon:
        # Run as daemon
        try:
            scheduler.start_daemon(timeframes)
        except KeyboardInterrupt:
            print("\n🛑 Shutdown requested...")
            scheduler.stop_daemon()
        except Exception as e:
            print(f"\n❌ Scheduler error: {e}")
            return 1
    else:
        # Run once for each timeframe
        for timeframe in timeframes:
            print(f"\n🔄 Running {timeframe} forecast generation...")
            scheduler._run_forecast_job(timeframe)
    
    return 0


if __name__ == "__main__":
    exit(main())
