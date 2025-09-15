#!/usr/bin/env python3
"""
5-Minute Trading Scheduler
Coordinates trading across multiple assets on 5-minute intervals
with equal value allocation strategy.
"""

import time
import threading
import schedule
from datetime import datetime, timedelta
from typing import Dict, List, Callable, Any, Optional
import logging
import json
import signal
import sys

from simplified_ensemble_portfolio import EnsembleMultiAssetPortfolio


class FiveMinuteTradingScheduler:
    """
    Scheduler for coordinating 5-minute interval trading across multiple assets
    with equal value allocation and risk/reward evaluation.
    """
    
    def __init__(self, 
                 portfolio: EnsembleMultiAssetPortfolio,
                 config_path: str = None,
                 enable_logging: bool = True):
        """
        Initialize the 5-minute trading scheduler
        
        Args:
            portfolio: The portfolio instance to execute trades on
            config_path: Path to scheduler configuration
            enable_logging: Whether to enable detailed logging
        """
        self.portfolio = portfolio
        self.config = self._load_config(config_path)
        self.is_running = False
        self.scheduler_thread = None
        self.stop_event = threading.Event()
        
        # Scheduling configuration
        self.interval_seconds = self.config.get('interval_seconds', 300)  # 5 minutes
        self.coordination_mode = self.config.get('coordination', 'simultaneous')
        self.decision_window_seconds = self.config.get('decision_window_seconds', 30)
        self.execution_window_seconds = self.config.get('execution_window_seconds', 120)
        
        # Performance tracking
        self.cycle_history = []
        self.performance_stats = {
            'cycles_completed': 0,
            'cycles_failed': 0,
            'average_cycle_duration': 0.0,
            'trades_executed': 0,
            'last_successful_cycle': None
        }
        
        # Initialize logging
        if enable_logging:
            self._setup_logging()
        self.logger = logging.getLogger(__name__)
        
        # Setup signal handlers for graceful shutdown
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
        
    def _load_config(self, config_path: str = None) -> Dict:
        """Load scheduler configuration"""
        if config_path is None:
            config_path = '../config/execution_settings.json'
        
        try:
            with open(config_path, 'r') as f:
                config = json.load(f)
                return config.get('trading_schedule', {})
        except Exception as e:
            # Return default configuration
            return {
                'interval_seconds': 300,
                'coordination': 'simultaneous',
                'decision_window_seconds': 30,
                'execution_window_seconds': 120
            }
    
    def _setup_logging(self):
        """Setup comprehensive logging for scheduler"""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler('trading_scheduler.log'),
                logging.StreamHandler()
            ]
        )
    
    def _signal_handler(self, signum, frame):
        """Handle shutdown signals gracefully"""
        self.logger.info(f"Received signal {signum}, shutting down gracefully...")
        self.stop_scheduler()
        sys.exit(0)
    
    def start_scheduler(self) -> bool:
        """
        Start the 5-minute trading scheduler
        
        Returns:
            True if started successfully, False otherwise
        """
        if self.is_running:
            self.logger.warning("Scheduler is already running")
            return False
        
        try:
            self.logger.info("🚀 Starting 5-minute trading scheduler...")
            
            # Schedule the trading cycle to run every 5 minutes
            schedule.every(self.interval_seconds).seconds.do(self._execute_trading_cycle)
            
            # Start scheduler in separate thread
            self.scheduler_thread = threading.Thread(target=self._run_scheduler_loop, daemon=True)
            self.is_running = True
            self.stop_event.clear()
            self.scheduler_thread.start()
            
            # Execute initial cycle immediately
            self._execute_trading_cycle()
            
            self.logger.info(f"✅ Scheduler started - trading every {self.interval_seconds} seconds")
            return True
            
        except Exception as e:
            self.logger.error(f"❌ Failed to start scheduler: {e}")
            self.is_running = False
            return False
    
    def stop_scheduler(self) -> bool:
        """
        Stop the trading scheduler gracefully
        
        Returns:
            True if stopped successfully, False otherwise
        """
        if not self.is_running:
            self.logger.warning("Scheduler is not running")
            return False
        
        try:
            self.logger.info("🛑 Stopping trading scheduler...")
            
            self.is_running = False
            self.stop_event.set()
            
            # Clear scheduled jobs
            schedule.clear()
            
            # Wait for scheduler thread to finish
            if self.scheduler_thread and self.scheduler_thread.is_alive():
                self.scheduler_thread.join(timeout=10)
            
            self.logger.info("✅ Scheduler stopped successfully")
            return True
            
        except Exception as e:
            self.logger.error(f"❌ Error stopping scheduler: {e}")
            return False
    
    def _run_scheduler_loop(self):
        """Main scheduler loop running in separate thread"""
        self.logger.info("📅 Scheduler loop started")
        
        while self.is_running and not self.stop_event.is_set():
            try:
                schedule.run_pending()
                time.sleep(1)  # Check every second
                
            except Exception as e:
                self.logger.error(f"Error in scheduler loop: {e}")
                
        self.logger.info("📅 Scheduler loop ended")
    
    def _execute_trading_cycle(self):
        """Execute a single 5-minute trading cycle"""
        cycle_start = datetime.now()
        cycle_id = f"cycle_{cycle_start.strftime('%Y%m%d_%H%M%S')}"
        
        self.logger.info(f"🔄 Starting trading cycle: {cycle_id}")
        
        try:
            # Execute the portfolio trading cycle
            cycle_results = self.portfolio.run_five_minute_trading_cycle()
            
            # Process results
            if 'error' not in cycle_results:
                self._process_successful_cycle(cycle_id, cycle_results)
            else:
                self._process_failed_cycle(cycle_id, cycle_results)
                
        except Exception as e:
            self.logger.error(f"❌ Trading cycle {cycle_id} failed with exception: {e}")
            self.performance_stats['cycles_failed'] += 1
    
    def _process_successful_cycle(self, cycle_id: str, results: Dict[str, Any]):
        """Process results from a successful trading cycle"""
        cycle_duration = results.get('cycle_duration_seconds', 0)
        trades_executed = len(results.get('trades_executed', {}).get('execution_details', {}))
        
        # Update performance stats
        self.performance_stats['cycles_completed'] += 1
        self.performance_stats['trades_executed'] += trades_executed
        self.performance_stats['last_successful_cycle'] = datetime.now()
        
        # Update average cycle duration
        total_cycles = self.performance_stats['cycles_completed']
        current_avg = self.performance_stats['average_cycle_duration']
        self.performance_stats['average_cycle_duration'] = (
            (current_avg * (total_cycles - 1) + cycle_duration) / total_cycles
        )
        
        # Store cycle history
        cycle_summary = {
            'cycle_id': cycle_id,
            'timestamp': results.get('timestamp'),
            'duration_seconds': cycle_duration,
            'trades_executed': trades_executed,
            'status': 'success',
            'decision_summary': results.get('decision_summary', {}),
            'portfolio_status': results.get('portfolio_status', {})
        }
        
        self.cycle_history.append(cycle_summary)
        
        # Keep only last 100 cycles
        if len(self.cycle_history) > 100:
            self.cycle_history = self.cycle_history[-100:]
        
        # Log cycle summary
        decision_summary = results.get('decision_summary', {})
        trading_opportunities = decision_summary.get('trading_opportunities', 0)
        avg_confidence = decision_summary.get('average_confidence', 0)
        
        self.logger.info(f"✅ Cycle {cycle_id} completed successfully:")
        self.logger.info(f"   Duration: {cycle_duration:.2f}s")
        self.logger.info(f"   Trading opportunities: {trading_opportunities}")
        self.logger.info(f"   Trades executed: {trades_executed}")
        self.logger.info(f"   Average confidence: {avg_confidence:.3f}")
        
        # Log portfolio status
        portfolio_status = results.get('portfolio_status', {})
        if portfolio_status:
            total_allocation = portfolio_status.get('total_allocation', 0)
            max_deviation = portfolio_status.get('max_deviation', 0)
            self.logger.info(f"   Portfolio allocation: {total_allocation:.1%}")
            self.logger.info(f"   Max deviation from equal allocation: {max_deviation:.3f}")
    
    def _process_failed_cycle(self, cycle_id: str, results: Dict[str, Any]):
        """Process results from a failed trading cycle"""
        error = results.get('error', 'Unknown error')
        
        self.performance_stats['cycles_failed'] += 1
        
        # Store failed cycle
        cycle_summary = {
            'cycle_id': cycle_id,
            'timestamp': results.get('timestamp'),
            'status': 'failed',
            'error': error
        }
        
        self.cycle_history.append(cycle_summary)
        
        self.logger.error(f"❌ Cycle {cycle_id} failed: {error}")
    
    def get_scheduler_status(self) -> Dict[str, Any]:
        """Get comprehensive scheduler status"""
        status = {
            'is_running': self.is_running,
            'interval_seconds': self.interval_seconds,
            'coordination_mode': self.coordination_mode,
            'performance_stats': self.performance_stats.copy(),
            'next_cycle_time': None,
            'uptime_seconds': 0
        }
        
        # Calculate next cycle time
        if self.is_running and self.performance_stats['last_successful_cycle']:
            last_cycle = self.performance_stats['last_successful_cycle']
            next_cycle = last_cycle + timedelta(seconds=self.interval_seconds)
            status['next_cycle_time'] = next_cycle
            
            # Calculate uptime
            if self.cycle_history:
                first_cycle = self.cycle_history[0]['timestamp']
                status['uptime_seconds'] = (datetime.now() - first_cycle).total_seconds()
        
        return status
    
    def get_recent_performance(self, lookback_cycles: int = 20) -> Dict[str, Any]:
        """Get performance metrics for recent cycles"""
        recent_cycles = self.cycle_history[-lookback_cycles:] if self.cycle_history else []
        
        if not recent_cycles:
            return {'status': 'no_data'}
        
        successful_cycles = [c for c in recent_cycles if c['status'] == 'success']
        failed_cycles = [c for c in recent_cycles if c['status'] == 'failed']
        
        success_rate = len(successful_cycles) / len(recent_cycles) if recent_cycles else 0
        
        avg_duration = 0
        total_trades = 0
        
        if successful_cycles:
            durations = [c.get('duration_seconds', 0) for c in successful_cycles]
            avg_duration = sum(durations) / len(durations)
            total_trades = sum(c.get('trades_executed', 0) for c in successful_cycles)
        
        performance = {
            'lookback_cycles': len(recent_cycles),
            'successful_cycles': len(successful_cycles),
            'failed_cycles': len(failed_cycles),
            'success_rate': success_rate,
            'average_duration_seconds': avg_duration,
            'total_trades_executed': total_trades,
            'trades_per_cycle': total_trades / len(successful_cycles) if successful_cycles else 0
        }
        
        return performance
    
    def force_trading_cycle(self) -> Dict[str, Any]:
        """Force execution of a trading cycle immediately"""
        self.logger.info("🔧 Forcing immediate trading cycle execution...")
        
        try:
            results = self.portfolio.run_five_minute_trading_cycle()
            self.logger.info("✅ Forced trading cycle completed")
            return results
        except Exception as e:
            self.logger.error(f"❌ Forced trading cycle failed: {e}")
            return {'error': str(e)}


def main():
    """Demo/test function for the trading scheduler"""
    print("🚀 5-Minute Trading Scheduler Demo")
    print("=" * 50)
    
    # Initialize portfolio
    portfolio = EnsembleMultiAssetPortfolio(
        initial_capital=100000,
        equal_value_allocation=True
    )
    
    # Initialize scheduler
    scheduler = FiveMinuteTradingScheduler(portfolio)
    
    try:
        # Start scheduler
        if scheduler.start_scheduler():
            print("✅ Scheduler started successfully")
            print("⏱️  Trading cycles will execute every 5 minutes")
            print("Press Ctrl+C to stop...")
            
            # Keep running until interrupted
            while scheduler.is_running:
                time.sleep(10)
                
                # Show status every 30 seconds
                status = scheduler.get_scheduler_status()
                performance = scheduler.get_recent_performance()
                
                print(f"\n📊 Scheduler Status:")
                print(f"   Cycles completed: {status['performance_stats']['cycles_completed']}")
                print(f"   Success rate: {performance.get('success_rate', 0):.1%}")
                print(f"   Average cycle duration: {performance.get('average_duration_seconds', 0):.2f}s")
                
        else:
            print("❌ Failed to start scheduler")
            
    except KeyboardInterrupt:
        print("\n🛑 Received interrupt signal")
        
    finally:
        scheduler.stop_scheduler()
        print("👋 Scheduler stopped")


if __name__ == "__main__":
    main()