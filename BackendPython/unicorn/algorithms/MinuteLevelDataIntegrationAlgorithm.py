"""
Minute-Level Data Integration Algorithm
======================================

This algorithm demonstrates how to integrate custom minute-level data sources
into the LEAN framework using Alpha Vantage as an example.

Shows:
1. Adding custom minute data sources
2. Using multiple data providers in one algorithm  
3. Handling different asset types (stocks, forex, crypto)
4. Framework integration with custom data
"""

import sys
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/data_sources')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/framework')

from AlgorithmImports import *
from AlphaVantageMinuteData import AlphaVantageMinuteData, AlphaVantageForexData, AlphaVantageCryptoData
from alphas.EthFocusedAlpha import EthFocusedAlpha
from portfolio.UnicornPortfolioConstruction import UnicornEqualWeightPortfolioConstruction
from risk.UnicornRiskManagement import UnicornRiskManagementModel


class MinuteLevelDataIntegrationAlgorithm(QCAlgorithm):
    """
    Minute-Level Data Integration Algorithm
    
    Demonstrates how to:
    1. Integrate custom minute-level data sources (Alpha Vantage)
    2. Combine free external data with LEAN's native data
    3. Use Framework components with custom data
    4. Handle multiple asset types and data sources
    
    Data Sources Demonstrated:
    - Alpha Vantage: Free minute-level stocks/forex/crypto
    - LEAN Native: GDAX crypto data (ETH)
    - Mixed resolution: Custom minute + native hourly
    """
    
    def initialize(self):
        """Initialize algorithm with multiple data sources."""
        
        # Basic setup
        self.set_start_date(2024, 8, 1)   # Recent date for minute data availability
        self.set_end_date(2024, 8, 27)    # Current date
        self.set_cash(10000)              # $10K for testing
        
        # ===========================================
        # CUSTOM DATA SOURCE INTEGRATION
        # ===========================================
        
        # 1. Alpha Vantage Stocks (Minute Resolution)
        # Note: Alpha Vantage free tier provides ~30 days of minute data
        try:
            self.add_data(AlphaVantageMinuteData, "AAPL", Resolution.MINUTE)
            self.add_data(AlphaVantageMinuteData, "MSFT", Resolution.MINUTE)
            self.debug("✅ Added Alpha Vantage minute stock data")
        except Exception as e:
            self.debug(f"⚠️ Alpha Vantage stocks failed: {e}")
        
        # 2. Alpha Vantage Forex (Minute Resolution)
        try:
            self.add_data(AlphaVantageForexData, "EURUSD", Resolution.MINUTE)
            self.debug("✅ Added Alpha Vantage minute forex data")
        except Exception as e:
            self.debug(f"⚠️ Alpha Vantage forex failed: {e}")
        
        # 3. Alpha Vantage Crypto (Minute Resolution)
        try:
            self.add_data(AlphaVantageCryptoData, "BTCUSD", Resolution.MINUTE)
            self.debug("✅ Added Alpha Vantage minute crypto data")
        except Exception as e:
            self.debug(f"⚠️ Alpha Vantage crypto failed: {e}")
        
        # ===========================================
        # NATIVE LEAN DATA (FOR COMPARISON)
        # ===========================================
        
        # 4. Native LEAN Crypto Data (Hourly for framework)
        try:
            # This will use GDAX/Coinbase data at hourly resolution
            eth_symbol = Symbol.create("ETHUSD", SecurityType.CRYPTO, Market.GDAX)
            self.debug("✅ Added native LEAN ETH data (hourly)")
        except Exception as e:
            try:
                # Fallback to forex market
                eth_symbol = Symbol.create("ETHUSD", SecurityType.FOREX, Market.OANDA)
                self.debug("✅ Added ETH as forex (hourly)")
            except Exception as e2:
                self.debug(f"⚠️ ETH data failed: {e2}")
                eth_symbol = None
        
        # ===========================================
        # FRAMEWORK INTEGRATION WITH CUSTOM DATA
        # ===========================================
        
        # Create mixed universe: Custom minute data + Native hourly data
        universe_symbols = []
        if eth_symbol:
            universe_symbols.append(eth_symbol)
        
        if universe_symbols:
            # Use framework with native data (hourly ETH)
            self.set_universe_selection(ManualUniverseSelectionModel(universe_symbols))
            
            # Alpha Model: ETH technical analysis (works with hourly data)
            self.set_alpha(EthFocusedAlpha(
                prediction_horizon_hours=1,      # Shorter horizon for minute data
                confidence_threshold=0.005       # Lower threshold for testing
            ))
            
            # Portfolio Construction: Equal weight
            self.set_portfolio_construction(UnicornEqualWeightPortfolioConstruction(
                rebalance_frequency=Resolution.MINUTE,  # More frequent rebalancing
                max_position_size=0.3                   # Smaller positions for testing
            ))
            
            # Risk Management: Conservative for testing
            self.set_risk_management(UnicornRiskManagementModel(
                max_position_size=0.3,
                stop_loss_percentage=0.01,      # 1% stop loss
                max_portfolio_drawdown=0.05,    # 5% max drawdown
                use_volatility_stops=False,     # Disable for minute data
                volatility_multiplier=1.0
            ))
            
            self.debug("🎯 Framework configured with mixed data sources")
        
        # ===========================================
        # CUSTOM DATA MONITORING
        # ===========================================
        
        # Track custom data reception
        self.custom_data_counts = {
            "alpha_vantage_stocks": 0,
            "alpha_vantage_forex": 0, 
            "alpha_vantage_crypto": 0,
            "native_eth": 0
        }
        
        # Performance tracking
        self.last_update_time = None
        self.data_source_performance = {}
        
        self.debug("🚀 MINUTE-LEVEL DATA INTEGRATION ALGORITHM INITIALIZED")
        self.debug("📊 Data Sources: Alpha Vantage (minute) + LEAN (hourly)")
        self.debug("⚡ Resolution: Mixed (minute for custom, hourly for framework)")
        self.debug("💰 Capital: $10,000")
    
    def on_data(self, data: Slice):
        """
        Process incoming data from multiple sources.
        
        Demonstrates handling both custom minute data and native framework data.
        """
        current_time = self.time
        
        # ===========================================
        # CUSTOM DATA PROCESSING
        # ===========================================
        
        # Process Alpha Vantage minute data
        for symbol_str in ["AAPL", "MSFT"]:
            if symbol_str in data and data[symbol_str] is not None:
                custom_data = data[symbol_str]
                self.custom_data_counts["alpha_vantage_stocks"] += 1
                
                self.debug(f"📈 {symbol_str} Minute Data: "
                          f"Close={custom_data.close:.2f}, "
                          f"Volume={custom_data.volume}, "
                          f"Time={custom_data.time}")
                
                # Example: Simple minute-level trading logic
                if not self.portfolio[symbol_str].invested:
                    if custom_data.close > custom_data.open:  # Green minute bar
                        # Small position for testing
                        self.set_holdings(symbol_str, 0.1)
                        self.debug(f"🟢 Bought {symbol_str} on green minute bar")
        
        # Process Alpha Vantage forex data
        if "EURUSD" in data and data["EURUSD"] is not None:
            forex_data = data["EURUSD"]
            self.custom_data_counts["alpha_vantage_forex"] += 1
            
            self.debug(f"💱 EURUSD Minute: Close={forex_data.close:.5f}")
        
        # Process Alpha Vantage crypto data
        if "BTCUSD" in data and data["BTCUSD"] is not None:
            crypto_data = data["BTCUSD"]
            self.custom_data_counts["alpha_vantage_crypto"] += 1
            
            self.debug(f"₿ BTC Minute: Close=${crypto_data.close:.2f}")
        
        # Track ETH from framework (if using native data)
        if data.contains_key("ETHUSD"):
            self.custom_data_counts["native_eth"] += 1
        
        # ===========================================
        # DATA SOURCE PERFORMANCE MONITORING
        # ===========================================
        
        # Log data reception statistics every 30 minutes
        if (self.last_update_time is None or 
            (current_time - self.last_update_time).total_seconds() >= 1800):
            
            self.last_update_time = current_time
            
            self.debug("📊 DATA SOURCE PERFORMANCE SUMMARY:")
            for source, count in self.custom_data_counts.items():
                self.debug(f"   {source}: {count} data points")
            
            # Check for data gaps
            total_custom_data = sum(self.custom_data_counts.values())
            if total_custom_data == 0:
                self.debug("⚠️ WARNING: No custom data received - check API keys and limits")
            else:
                self.debug(f"✅ Total custom data points: {total_custom_data}")
    
    def on_order_event(self, order_event: OrderEvent):
        """Monitor order execution from custom data signals."""
        if order_event.status == OrderStatus.FILLED:
            self.debug(f"🎯 Order Filled: {order_event.symbol} "
                      f"({order_event.direction}) "
                      f"@ ${order_event.fill_price:.2f}")
    
    def on_end_of_algorithm(self):
        """Print final statistics about data sources."""
        self.debug("🏁 ALGORITHM COMPLETED - DATA SOURCE SUMMARY:")
        self.debug("=" * 50)
        
        total_data_points = sum(self.custom_data_counts.values())
        
        for source, count in self.custom_data_counts.items():
            percentage = (count / total_data_points * 100) if total_data_points > 0 else 0
            self.debug(f"📊 {source}: {count} points ({percentage:.1f}%)")
        
        self.debug(f"📈 Total Portfolio Value: ${self.portfolio.total_portfolio_value:.2f}")
        self.debug(f"💰 Total Return: {(self.portfolio.total_portfolio_value / 10000 - 1) * 100:.2f}%")
        
        if total_data_points == 0:
            self.debug("❌ NO CUSTOM DATA RECEIVED")
            self.debug("💡 Check Alpha Vantage API key and rate limits")
            self.debug("💡 Free tier: 500 calls/month, 5 calls/minute")
        else:
            self.debug("✅ Custom data integration successful!")
