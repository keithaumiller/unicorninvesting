"""
Unicorn Risk Management Models
=============================

Custom risk management models for the Unicorn Investing platform.
These models provide risk controls including stop losses, position limits,
drawdown protection, and volatility-based risk management.

Separation of Concerns:
- Alpha Models generate Insights (forecasts)
- Portfolio Construction Models decide position sizes
- Risk Models provide risk controls and limits
- Execution Models handle order placement
"""

from AlgorithmImports import *
import numpy as np

class UnicornRiskManagementModel(RiskManagementModel):
    """
    Comprehensive risk management model for Unicorn platform.
    
    Features:
    - Stop loss management (fixed % and volatility-based)
    - Position size limits
    - Maximum drawdown protection
    - Correlation-based position limits
    - Volatility-adjusted risk controls
    """
    
    def __init__(self, 
                 max_position_size=0.25,           # 25% max per position
                 stop_loss_percentage=0.02,        # 2% stop loss
                 max_portfolio_drawdown=0.10,      # 10% max drawdown
                 use_volatility_stops=True,        # Use ATR-based stops
                 volatility_multiplier=2.0):       # ATR multiplier for stops
        """
        Initialize Unicorn risk management model.
        
        Args:
            max_position_size: Maximum allocation per position
            stop_loss_percentage: Fixed stop loss percentage
            max_portfolio_drawdown: Maximum portfolio drawdown before liquidation
            use_volatility_stops: Whether to use volatility-based stops
            volatility_multiplier: Multiplier for ATR-based stops
        """
        self.max_position_size = max_position_size
        self.stop_loss_percentage = stop_loss_percentage
        self.max_portfolio_drawdown = max_portfolio_drawdown
        self.use_volatility_stops = use_volatility_stops
        self.volatility_multiplier = volatility_multiplier
        
        # Track risk metrics
        self.portfolio_high_water_mark = 0
        self.position_entry_prices = {}
        self.volatility_indicators = {}
        self.correlation_matrix = {}
        
        # Risk event tracking
        self.risk_events = []
        self.last_risk_check = datetime.min
        
    def manage_risk(self, algorithm, targets):
        """
        Main risk management function called by LEAN framework.
        
        Evaluates current portfolio against risk limits and returns
        risk management targets (typically liquidation orders).
        """
        risk_targets = []
        
        try:
            # Update portfolio high water mark
            self.update_high_water_mark(algorithm)
            
            # 1. Portfolio-level risk checks
            portfolio_risk_targets = self.check_portfolio_risk(algorithm)
            risk_targets.extend(portfolio_risk_targets)
            
            # 2. Position-level risk checks
            position_risk_targets = self.check_position_risk(algorithm, targets)
            risk_targets.extend(position_risk_targets)
            
            # 3. Volatility-based risk adjustments
            if self.use_volatility_stops:
                volatility_risk_targets = self.check_volatility_risk(algorithm)
                risk_targets.extend(volatility_risk_targets)
            
            # 4. Correlation risk checks
            correlation_risk_targets = self.check_correlation_risk(algorithm)
            risk_targets.extend(correlation_risk_targets)
            
            # Log risk events
            if risk_targets:
                self.log_risk_events(algorithm, risk_targets)
                
        except Exception as e:
            algorithm.debug(f"⚠️ Risk management error: {e}")
        
        return risk_targets
    
    def update_high_water_mark(self, algorithm):
        """Update portfolio high water mark for drawdown calculation."""
        current_value = algorithm.portfolio.total_portfolio_value
        if current_value > self.portfolio_high_water_mark:
            self.portfolio_high_water_mark = current_value
    
    def check_portfolio_risk(self, algorithm):
        """
        Check portfolio-level risk limits.
        
        Returns liquidation targets if portfolio risk limits are exceeded.
        """
        risk_targets = []
        
        # 1. Maximum drawdown check
        current_value = algorithm.portfolio.total_portfolio_value
        if self.portfolio_high_water_mark > 0:
            drawdown = (self.portfolio_high_water_mark - current_value) / self.portfolio_high_water_mark
            
            if drawdown > self.max_portfolio_drawdown:
                # Liquidate all positions due to excessive drawdown
                for security in algorithm.securities.values():
                    if security.invested:
                        risk_targets.append(PortfolioTarget(security.symbol, 0))
                        
                algorithm.debug(f"🚨 PORTFOLIO DRAWDOWN EXCEEDED: {drawdown:.2%} > {self.max_portfolio_drawdown:.2%}")
                self.risk_events.append({
                    'time': algorithm.utc_time,
                    'type': 'portfolio_drawdown',
                    'value': drawdown,
                    'action': 'liquidate_all'
                })
        
        # 2. Maximum leverage check
        total_leverage = sum(abs(security.holdings.holdings_value / algorithm.portfolio.total_portfolio_value) 
                           for security in algorithm.securities.values() 
                           if security.invested and algorithm.portfolio.total_portfolio_value > 0)
        
        if total_leverage > 1.5:  # 150% max leverage
            # Reduce positions proportionally
            scale_factor = 1.0 / total_leverage
            for security in algorithm.securities.values():
                if security.invested:
                    current_weight = security.holdings.holdings_value / algorithm.portfolio.total_portfolio_value
                    new_weight = current_weight * scale_factor
                    risk_targets.append(PortfolioTarget(security.symbol, new_weight))
                    
            algorithm.debug(f"🚨 LEVERAGE EXCEEDED: {total_leverage:.2%} > 150%")
        
        return risk_targets
    
    def check_position_risk(self, algorithm, targets):
        """
        Check individual position risk limits.
        
        Includes stop losses and position size limits.
        """
        risk_targets = []
        
        for security in algorithm.securities.values():
            if not security.invested:
                continue
                
            symbol = security.symbol
            current_price = security.price
            holdings = security.holdings
            
            # 1. Position size limit check
            portfolio_value = algorithm.portfolio.total_portfolio_value
            if portfolio_value > 0:
                position_weight = abs(holdings.holdings_value / portfolio_value)
                
                if position_weight > self.max_position_size:
                    # Reduce position to max allowed size
                    direction = 1 if holdings.is_long else -1
                    new_weight = self.max_position_size * direction
                    risk_targets.append(PortfolioTarget(symbol, new_weight))
                    
                    algorithm.debug(f"🚨 POSITION SIZE EXCEEDED {symbol}: {position_weight:.2%} > {self.max_position_size:.2%}")
            
            # 2. Stop loss check
            if symbol in self.position_entry_prices:
                entry_price = self.position_entry_prices[symbol]
                
                # Calculate stop loss level
                if holdings.is_long:
                    stop_loss_price = entry_price * (1 - self.stop_loss_percentage)
                    if current_price <= stop_loss_price:
                        risk_targets.append(PortfolioTarget(symbol, 0))
                        algorithm.debug(f"🛑 STOP LOSS TRIGGERED {symbol}: {current_price:.5f} <= {stop_loss_price:.5f}")
                        
                elif holdings.is_short:
                    stop_loss_price = entry_price * (1 + self.stop_loss_percentage)
                    if current_price >= stop_loss_price:
                        risk_targets.append(PortfolioTarget(symbol, 0))
                        algorithm.debug(f"🛑 STOP LOSS TRIGGERED {symbol}: {current_price:.5f} >= {stop_loss_price:.5f}")
            
            # 3. Time-based position limits (optional)
            # Could add logic to close positions held too long
        
        return risk_targets
    
    def check_volatility_risk(self, algorithm):
        """
        Check volatility-based risk using ATR indicators.
        
        Implements dynamic stop losses based on market volatility.
        """
        risk_targets = []
        
        for security in algorithm.securities.values():
            if not security.invested:
                continue
                
            symbol = security.symbol
            
            # Initialize ATR indicator if not exists
            if symbol not in self.volatility_indicators:
                self.volatility_indicators[symbol] = algorithm.atr(symbol, 14, Resolution.HOUR)
                continue  # Skip this iteration if indicator just created
            
            atr_indicator = self.volatility_indicators[symbol]
            if not atr_indicator.is_ready:
                continue
                
            current_price = security.price
            atr_value = atr_indicator.current.value
            holdings = security.holdings
            
            # Calculate volatility-based stop loss
            if symbol in self.position_entry_prices and atr_value > 0:
                entry_price = self.position_entry_prices[symbol]
                volatility_stop_distance = atr_value * self.volatility_multiplier
                
                if holdings.is_long:
                    volatility_stop = entry_price - volatility_stop_distance
                    if current_price <= volatility_stop:
                        risk_targets.append(PortfolioTarget(symbol, 0))
                        algorithm.debug(f"🛑 VOLATILITY STOP {symbol}: Price {current_price:.5f} <= Stop {volatility_stop:.5f}")
                        
                elif holdings.is_short:
                    volatility_stop = entry_price + volatility_stop_distance
                    if current_price >= volatility_stop:
                        risk_targets.append(PortfolioTarget(symbol, 0))
                        algorithm.debug(f"🛑 VOLATILITY STOP {symbol}: Price {current_price:.5f} >= Stop {volatility_stop:.5f}")
        
        return risk_targets
    
    def check_correlation_risk(self, algorithm):
        """
        Check for excessive correlation between positions.
        
        Reduces positions if portfolio becomes too concentrated in correlated assets.
        """
        risk_targets = []
        
        # This is a simplified correlation check
        # In production, you'd calculate actual correlations using historical returns
        
        invested_symbols = [security.symbol for security in algorithm.securities.values() if security.invested]
        
        if len(invested_symbols) < 2:
            return risk_targets
        
        # Check for currency concentration (simplified)
        currency_exposure = {}
        for security in algorithm.securities.values():
            if not security.invested:
                continue
                
            symbol_str = str(security.symbol)
            
            # Extract currency information (simplified for forex pairs)
            if 'USD' in symbol_str:
                currency_exposure['USD'] = currency_exposure.get('USD', 0) + abs(security.holdings.holdings_value)
            if 'EUR' in symbol_str:
                currency_exposure['EUR'] = currency_exposure.get('EUR', 0) + abs(security.holdings.holdings_value)
            if 'ETH' in symbol_str:
                currency_exposure['ETH'] = currency_exposure.get('ETH', 0) + abs(security.holdings.holdings_value)
        
        # Check for excessive concentration
        total_exposure = sum(currency_exposure.values())
        if total_exposure > 0:
            for currency, exposure in currency_exposure.items():
                concentration = exposure / total_exposure
                
                if concentration > 0.7:  # 70% concentration limit
                    algorithm.debug(f"🚨 HIGH CORRELATION RISK: {currency} concentration {concentration:.2%}")
                    
                    # Reduce positions in this currency (simplified)
                    for security in algorithm.securities.values():
                        if security.invested and currency in str(security.symbol):
                            current_weight = security.holdings.holdings_value / algorithm.portfolio.total_portfolio_value
                            reduced_weight = current_weight * 0.7  # Reduce by 30%
                            risk_targets.append(PortfolioTarget(security.symbol, reduced_weight))
        
        return risk_targets
    
    def on_securities_changed(self, algorithm, changes):
        """
        Handle security changes - track entry prices and initialize indicators.
        """
        # Track entry prices for new positions
        for security in changes.added_securities:
            symbol = security.symbol
            
            # Initialize volatility indicator
            if self.use_volatility_stops:
                self.volatility_indicators[symbol] = algorithm.atr(symbol, 14, Resolution.HOUR)
            
            algorithm.debug(f"🔧 Risk management initialized for {symbol}")
        
        # Clean up removed securities
        for security in changes.removed_securities:
            symbol = security.symbol
            
            if symbol in self.position_entry_prices:
                del self.position_entry_prices[symbol]
            if symbol in self.volatility_indicators:
                del self.volatility_indicators[symbol]
                
            algorithm.debug(f"🗑️ Risk management cleaned up for {symbol}")
    
    def update_position_entry_price(self, algorithm, symbol, fill_price):
        """
        Update entry price when position is opened or modified.
        
        Should be called from the algorithm when orders are filled.
        """
        self.position_entry_prices[symbol] = fill_price
        algorithm.debug(f"📝 Entry price updated for {symbol}: {fill_price:.5f}")
    
    def log_risk_events(self, algorithm, risk_targets):
        """Log risk management events for monitoring and analysis."""
        for target in risk_targets:
            event = {
                'time': algorithm.utc_time,
                'symbol': target.symbol,
                'target_weight': target.quantity,
                'action': 'liquidate' if target.quantity == 0 else 'reduce'
            }
            self.risk_events.append(event)
            
        # Keep only recent events (last 100)
        if len(self.risk_events) > 100:
            self.risk_events = self.risk_events[-100:]
    
    def get_risk_summary(self, algorithm):
        """
        Get current risk summary for monitoring.
        
        Returns dictionary with key risk metrics.
        """
        current_value = algorithm.portfolio.total_portfolio_value
        drawdown = 0
        
        if self.portfolio_high_water_mark > 0:
            drawdown = (self.portfolio_high_water_mark - current_value) / self.portfolio_high_water_mark
        
        # Calculate total leverage
        total_leverage = 0
        if current_value > 0:
            total_leverage = sum(abs(security.holdings.holdings_value / current_value) 
                               for security in algorithm.securities.values() 
                               if security.invested)
        
        # Count positions
        active_positions = sum(1 for security in algorithm.securities.values() if security.invested)
        
        return {
            'portfolio_value': current_value,
            'high_water_mark': self.portfolio_high_water_mark,
            'current_drawdown': drawdown,
            'total_leverage': total_leverage,
            'active_positions': active_positions,
            'risk_events_count': len(self.risk_events)
        }


class UnicornForexRiskManagement(UnicornRiskManagementModel):
    """
    Forex-specific risk management model extending the base Unicorn model.
    
    Additional features for forex trading:
    - Currency exposure limits
    - Carry trade risk management
    - Economic event risk controls
    - Cross-currency correlation management
    """
    
    def __init__(self, max_currency_exposure=0.5, **kwargs):
        """
        Initialize forex-specific risk management.
        
        Args:
            max_currency_exposure: Maximum exposure to any single currency (50%)
            **kwargs: Arguments passed to base class
        """
        super().__init__(**kwargs)
        self.max_currency_exposure = max_currency_exposure
        self.currency_exposures = {}
        
    def check_currency_exposure(self, algorithm):
        """
        Check exposure limits for individual currencies.
        
        Specific to forex trading where currency exposure matters.
        """
        risk_targets = []
        
        # Calculate current currency exposures
        self.currency_exposures = self.calculate_currency_exposures(algorithm)
        
        total_portfolio_value = algorithm.portfolio.total_portfolio_value
        
        for currency, exposure in self.currency_exposures.items():
            if total_portfolio_value > 0:
                exposure_ratio = abs(exposure) / total_portfolio_value
                
                if exposure_ratio > self.max_currency_exposure:
                    algorithm.debug(f"🚨 CURRENCY EXPOSURE EXCEEDED {currency}: {exposure_ratio:.2%}")
                    
                    # Reduce positions involving this currency
                    risk_targets.extend(self.reduce_currency_exposure(algorithm, currency, exposure_ratio))
        
        return risk_targets
    
    def calculate_currency_exposures(self, algorithm):
        """Calculate net exposure to each currency."""
        exposures = {}
        
        for security in algorithm.securities.values():
            if not security.invested:
                continue
                
            symbol_str = str(security.symbol)
            holdings_value = security.holdings.holdings_value
            
            # Parse forex pair (simplified)
            if len(symbol_str) >= 6:
                base_currency = symbol_str[:3]
                quote_currency = symbol_str[3:6]
                
                # Add to base currency exposure
                exposures[base_currency] = exposures.get(base_currency, 0) + holdings_value
                
                # Subtract from quote currency exposure
                exposures[quote_currency] = exposures.get(quote_currency, 0) - holdings_value
        
        return exposures
    
    def reduce_currency_exposure(self, algorithm, currency, current_ratio):
        """Reduce exposure to a specific currency."""
        risk_targets = []
        target_ratio = self.max_currency_exposure * 0.9  # Reduce to 90% of limit
        reduction_factor = target_ratio / current_ratio
        
        for security in algorithm.securities.values():
            if security.invested and currency in str(security.symbol):
                current_weight = security.holdings.holdings_value / algorithm.portfolio.total_portfolio_value
                new_weight = current_weight * reduction_factor
                risk_targets.append(PortfolioTarget(security.symbol, new_weight))
                
        return risk_targets
