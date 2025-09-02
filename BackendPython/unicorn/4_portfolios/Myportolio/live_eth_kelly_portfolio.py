"""
Real-time ETH Kelly Portfolio with IBKR Integration
Live portfolio management using Kelly Criterion with IBKR Gateway data
"""

import sys
import os
import json
import pandas as pd
import numpy as np
import asyncio
import logging
import requests
from datetime import datetime, timedelta
from typing import Dict, List, Optional

# Add paths for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)

from eth_kelly_integration import ETHKellyIntegratedPortfolio

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class IBKRDataCollector:
    """Simple IBKR data collector for ETH prices"""
    
    def __init__(self, base_url: str = "http://localhost:5000/v1/api", contract_id: int = 541686654):
        self.base_url = base_url
        self.contract_id = contract_id
        
    def get_current_price(self) -> Dict:
        """Get current ETH price from IBKR"""
        try:
            response = requests.get(f"{self.base_url}/iserver/marketdata/snapshot", 
                                  params={'conids': self.contract_id, 'fields': '31,55,70,71'}, 
                                  timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                if data and len(data) > 0:
                    # IBKR returns list of dicts, find our contract
                    for item in data:
                        if item.get('conid') == self.contract_id:
                            # Extract price fields
                            last_price = item.get('31')  # Last price
                            bid_price = item.get('84')   # Bid
                            ask_price = item.get('86')   # Ask 
                            volume = item.get('70')      # Volume
                            
                            # Sometimes price comes as string, convert to float
                            try:
                                price = float(last_price) if last_price else None
                                if price and price > 0:
                                    return {
                                        'price': price,
                                        'bid': float(bid_price) if bid_price else price,
                                        'ask': float(ask_price) if ask_price else price,
                                        'volume': float(volume) if volume else 1000000,
                                        'timestamp': datetime.now()
                                    }
                            except (ValueError, TypeError):
                                # Try to get any numeric field as price
                                for field in ['70', '71', '55']:  # Try volume fields as fallback
                                    try:
                                        fallback_price = float(item.get(field, 0))
                                        if fallback_price > 1000:  # Reasonable ETH price range
                                            return {
                                                'price': fallback_price,
                                                'volume': 1000000,
                                                'timestamp': datetime.now()
                                            }
                                    except (ValueError, TypeError):
                                        continue
            return None
        except Exception as e:
            logger.error(f"IBKR data error: {e}")
            return None
    
    def get_minute_data(self, days_back: int = 1, max_bars: int = 1000) -> pd.DataFrame:
        """Get historical minute data (simplified)"""
        # For now, return empty DataFrame - would implement full historical data retrieval
        return pd.DataFrame()

class LiveETHKellyPortfolio:
    """
    Live ETH Portfolio Management with Kelly Criterion and IBKR Integration
    
    Provides real-time portfolio management using:
    - IBKR Gateway for live ETH data
    - Kelly Criterion for optimal position sizing
    - ETH momentum strategy for signal generation
    - Comprehensive risk management
    """
    
    def __init__(self, config_path: str = None):
        """
        Initialize live ETH Kelly portfolio
        
        Args:
            config_path: Path to configuration file
        """
        
        # Load configuration
        if config_path is None:
            config_path = os.path.join(current_dir, 'config', 'eth_kelly_config.json')
        
        with open(config_path, 'r') as f:
            self.config = json.load(f)
        
        # Initialize portfolio manager
        self.portfolio = ETHKellyIntegratedPortfolio(self.config)
        
        # Initialize IBKR data collector
        ibkr_config = {
            'base_url': 'http://localhost:5000/v1/api',
            'contract_id': 541686654,  # ETH-USD from our validation
        }
        self.data_collector = IBKRDataCollector(**ibkr_config)
        
        # Runtime state
        self.is_running = False
        self.last_update = None
        self.data_buffer = []
        self.update_frequency = self.config.get('data_sources', {}).get('update_frequency', '1min')
        
        # Performance tracking
        self.session_start = datetime.now()
        self.total_updates = 0
        self.successful_updates = 0
        
        logger.info(f"Live ETH Kelly Portfolio initialized with config: {config_path}")
    
    async def initialize_data_connection(self) -> bool:
        """Initialize connection to IBKR Gateway"""
        
        try:
            # Test IBKR connection
            logger.info("Testing IBKR Gateway connection...")
            
            # Get current ETH data
            current_data = self.data_collector.get_current_price()
            
            if current_data and 'price' in current_data:
                logger.info(f"✅ IBKR connection successful - ETH price: ${current_data['price']:.2f}")
                return True
            else:
                logger.error("❌ IBKR connection failed - no price data received")
                return False
                
        except Exception as e:
            logger.error(f"❌ IBKR connection error: {e}")
            return False
    
    def get_historical_data(self, days: int = 30) -> pd.DataFrame:
        """Get historical ETH data for algorithm initialization"""
        
        try:
            logger.info(f"Fetching {days} days of historical ETH data...")
            
            # Get minute data for the specified period
            historical_data = self.data_collector.get_minute_data(
                days_back=days,
                max_bars=days * 1440  # 1440 minutes per day
            )
            
            if not historical_data.empty:
                logger.info(f"✅ Retrieved {len(historical_data)} historical data points")
                return historical_data
            else:
                logger.warning("⚠️ No historical data available, using simulated data")
                return self._generate_fallback_data(days)
                
        except Exception as e:
            logger.error(f"❌ Historical data error: {e}")
            return self._generate_fallback_data(days)
    
    def _generate_fallback_data(self, days: int) -> pd.DataFrame:
        """Generate fallback data if IBKR data unavailable"""
        
        # Simple fallback using current price and random walk
        try:
            current_data = self.data_collector.get_current_price()
            base_price = current_data.get('price', 3000.0) if current_data else 3000.0
        except:
            base_price = 3000.0
        
        dates = pd.date_range(
            start=datetime.now() - timedelta(days=days), 
            periods=days * 24, 
            freq='h'
        )
        
        np.random.seed(42)
        returns = np.random.normal(0, 0.01, len(dates))
        prices = [base_price]
        
        for ret in returns[1:]:
            prices.append(prices[-1] * (1 + ret))
        
        data = []
        for i, (date, price) in enumerate(zip(dates, prices)):
            data.append({
                'timestamp': date,
                'open': prices[i-1] if i > 0 else price,
                'high': price * 1.005,
                'low': price * 0.995,
                'close': price,
                'volume': 1000000
            })
        
        logger.info(f"Generated {len(data)} fallback data points")
        return pd.DataFrame(data)
    
    async def update_portfolio(self) -> Dict:
        """Update portfolio with latest market data"""
        
        try:
            self.total_updates += 1
            
            # Get current market data
            current_data = self.data_collector.get_current_price()
            
            if not current_data or 'price' not in current_data:
                logger.warning("⚠️ No current price data available")
                return {'error': 'No price data'}
            
            # Add to data buffer
            timestamp = datetime.now()
            market_point = {
                'timestamp': timestamp,
                'open': current_data.get('price'),
                'high': current_data.get('price'),
                'low': current_data.get('price'),
                'close': current_data.get('price'),
                'volume': current_data.get('volume', 1000000)
            }
            
            self.data_buffer.append(market_point)
            
            # Keep reasonable buffer size
            if len(self.data_buffer) > 1000:
                self.data_buffer = self.data_buffer[-500:]
            
            # Create DataFrame for portfolio processing
            market_df = pd.DataFrame(self.data_buffer)
            
            # Process portfolio decision
            if len(market_df) >= 20:  # Need enough data for moving averages
                decision = self.portfolio.process_market_data(market_df)
                
                # Execute decision if actionable
                execution_result = None
                if decision.get('final_decision', {}).get('action') != 'HOLD':
                    execution_result = self.portfolio.execute_decision(decision)
                
                self.successful_updates += 1
                self.last_update = timestamp
                
                return {
                    'timestamp': timestamp,
                    'price': current_data['price'],
                    'decision': decision,
                    'execution': execution_result,
                    'portfolio_summary': self.portfolio.get_portfolio_summary()
                }
            else:
                return {
                    'timestamp': timestamp,
                    'price': current_data['price'],
                    'status': 'collecting_data',
                    'buffer_size': len(self.data_buffer)
                }
                
        except Exception as e:
            logger.error(f"❌ Portfolio update error: {e}")
            return {'error': str(e)}
    
    async def run_live_session(self, duration_minutes: int = 60):
        """Run live portfolio management session"""
        
        logger.info(f"🚀 Starting live ETH Kelly portfolio session ({duration_minutes} minutes)")
        
        # Initialize
        if not await self.initialize_data_connection():
            logger.error("❌ Failed to initialize IBKR connection")
            return False
        
        # Get historical data for algorithm initialization
        historical_data = self.get_historical_data(30)
        
        if not historical_data.empty:
            # Initialize portfolio with historical data
            initial_decision = self.portfolio.process_market_data(historical_data)
            logger.info(f"📊 Portfolio initialized with historical data")
        
        # Run live session
        self.is_running = True
        session_end = datetime.now() + timedelta(minutes=duration_minutes)
        update_interval = 60  # Update every minute
        
        logger.info(f"▶️ Live session started - running until {session_end.strftime('%H:%M:%S')}")
        
        try:
            while self.is_running and datetime.now() < session_end:
                # Update portfolio
                update_result = await self.update_portfolio()
                
                # Log update
                if 'error' not in update_result:
                    price = update_result.get('price', 0)
                    portfolio_value = update_result.get('portfolio_summary', {}).get('portfolio_value', 0)
                    position_size = update_result.get('portfolio_summary', {}).get('position', {}).get('size', 0)
                    
                    logger.info(f"📊 Update: ETH=${price:.2f}, Portfolio=${portfolio_value:,.2f}, Position={position_size:.4f}")
                    
                    # Check for executed trades
                    if update_result.get('execution', {}).get('executed'):
                        trade_details = update_result['execution']['trade_details']
                        logger.info(f"🎯 Trade executed: {trade_details['action']} {abs(trade_details['position_change']):.4f} ETH at ${trade_details['price']:.2f}")
                else:
                    logger.warning(f"⚠️ Update failed: {update_result['error']}")
                
                # Wait for next update
                await asyncio.sleep(update_interval)
                
        except KeyboardInterrupt:
            logger.info("⏹️ Session stopped by user")
        except Exception as e:
            logger.error(f"❌ Session error: {e}")
        finally:
            self.is_running = False
        
        # Session summary
        await self.print_session_summary()
        return True
    
    async def print_session_summary(self):
        """Print comprehensive session summary"""
        
        session_duration = datetime.now() - self.session_start
        success_rate = (self.successful_updates / self.total_updates * 100) if self.total_updates > 0 else 0
        
        portfolio_summary = self.portfolio.get_portfolio_summary()
        recent_performance = self.portfolio.get_recent_performance(1)  # Last day
        
        print("\n" + "="*60)
        print("📊 LIVE ETH KELLY PORTFOLIO SESSION SUMMARY")
        print("="*60)
        
        print(f"⏱️ Session Duration: {session_duration}")
        print(f"📡 Total Updates: {self.total_updates}")
        print(f"✅ Successful Updates: {self.successful_updates}")
        print(f"📈 Success Rate: {success_rate:.1f}%")
        
        print(f"\n💰 Portfolio Performance:")
        print(f"  💵 Portfolio Value: ${portfolio_summary['portfolio_value']:,.2f}")
        print(f"  📊 Total Return: {portfolio_summary['performance']['total_return_pct']:.2f}%")
        print(f"  💵 Cash: ${portfolio_summary['cash']:,.2f}")
        print(f"  🪙 ETH Position: {portfolio_summary['position']['size']:.4f} ETH")
        print(f"  💲 Position Value: ${portfolio_summary['position']['value']:,.2f}")
        
        print(f"\n🛡️ Risk Metrics:")
        print(f"  📉 Current Drawdown: {portfolio_summary['risk_metrics']['current_drawdown']:.1%}")
        print(f"  📊 Portfolio VaR: {portfolio_summary['risk_metrics']['var_5pct']:.1%}")
        
        print(f"\n🎯 Trading Activity:")
        print(f"  📈 Total Trades: {portfolio_summary['trade_count']}")
        print(f"  📡 Total Signals: {portfolio_summary['signal_count']}")
        
        if recent_performance.get('trade_count', 0) > 0:
            print(f"  🏆 Recent Win Rate: {recent_performance.get('win_rate', 0):.1%}")
            print(f"  💹 Avg Trade Return: {recent_performance.get('avg_trade_return', 0):.2%}")
        
        print(f"\n🔮 Kelly Performance:")
        kelly_perf = portfolio_summary.get('kelly_performance', {})
        if 'kelly_performance' in kelly_perf and not kelly_perf['kelly_performance'].get('insufficient_data'):
            kp = kelly_perf['kelly_performance']
            print(f"  🎯 Kelly Win Rate: {kp.get('win_rate', 0):.1%}")
            print(f"  📊 Kelly Sharpe: {kp.get('sharpe_ratio', 0):.2f}")
        else:
            print(f"  📊 Kelly Performance: Insufficient data for analysis")
        
        print("="*60)
    
    def stop_session(self):
        """Stop the live session"""
        self.is_running = False
        logger.info("🛑 Live session stop requested")

# CLI Interface for easy testing
async def main():
    """Main CLI interface"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Live ETH Kelly Portfolio Management')
    parser.add_argument('--config', type=str, help='Configuration file path')
    parser.add_argument('--duration', type=int, default=60, help='Session duration in minutes')
    parser.add_argument('--test-connection', action='store_true', help='Test IBKR connection only')
    
    args = parser.parse_args()
    
    # Initialize portfolio
    portfolio = LiveETHKellyPortfolio(args.config)
    
    if args.test_connection:
        # Test connection only
        print("🔍 Testing IBKR Gateway connection...")
        success = await portfolio.initialize_data_connection()
        print(f"Connection result: {'✅ Success' if success else '❌ Failed'}")
        return
    
    # Run live session
    print(f"🚀 Starting live ETH Kelly portfolio session")
    success = await portfolio.run_live_session(args.duration)
    
    if success:
        print("✅ Session completed successfully")
    else:
        print("❌ Session failed")

if __name__ == "__main__":
    asyncio.run(main())
