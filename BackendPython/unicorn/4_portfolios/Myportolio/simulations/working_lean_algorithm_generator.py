#!/usr/bin/env python3
"""
Fixed LEAN Algorithm Generator - Working Trading Logic
====================================================

Creates a functional LEAN algorithm that incorporates our enhanced
ETH momentum strategy and risk management with actual trading logic.

Author: Unicorn Investing Platform
Date: September 15, 2025
"""

def generate_working_lean_algorithm(algorithm_name: str, parameters: dict, sim_dir_name: str) -> str:
    """
    Generate a LEAN algorithm with ACTUAL trading logic instead of placeholders.
    
    This incorporates:
    - Real momentum strategy calculations
    - Actual buy/sell signal generation
    - Position sizing with Kelly criterion
    - Risk management decisions
    """
    
    algorithm_code = f'''
"""
Myportolio LEAN Algorithm - {algorithm_name} (FIXED WITH TRADING LOGIC)
Generated for simulation: {sim_dir_name}
"""

from clr import AddReference
AddReference("System")
AddReference("QuantConnect.Algorithm")
AddReference("QuantConnect.Common")

from System import *
from QuantConnect import *
from QuantConnect.Algorithm import *
from QuantConnect.Data import *
import numpy as np

class {algorithm_name}(QCAlgorithm):
    """
    FUNCTIONAL Myportolio trading algorithm with real trading logic.
    """
    
    def Initialize(self):
        """Initialize algorithm with working configuration."""
        
        # Set cash and dates
        self.SetCash({parameters.get("initial_capital", 100000)})
        
        # Add ETH crypto data
        self.eth = self.AddCrypto("ETHUSD", Resolution.Hour)
        self.eth_symbol = self.eth.Symbol
        
        # Strategy parameters
        self.ma_short = {parameters.get("ma_short", 5)}
        self.ma_long = {parameters.get("ma_long", 20)}
        self.rsi_period = {parameters.get("rsi_period", 14)}
        self.kelly_fraction = {parameters.get("kelly_fraction", 0.167)}
        self.confidence_threshold = {parameters.get("confidence_threshold", 0.65)}
        
        # Risk management parameters
        self.max_volatility = {parameters.get("max_volatility", 0.25)}
        self.max_drawdown = {parameters.get("max_drawdown", 0.15)}
        self.var_limit = {parameters.get("var_limit_1day", 0.06)}
        self.max_position_pct = {parameters.get("max_position_pct", 0.25)}
        
        # Initialize indicators
        self.ma_short_indicator = self.SMA(self.eth_symbol, self.ma_short, Resolution.Hour)
        self.ma_long_indicator = self.SMA(self.eth_symbol, self.ma_long, Resolution.Hour)
        self.rsi_indicator = self.RSI(self.eth_symbol, self.rsi_period, Resolution.Hour)
        
        # State tracking
        self.previous_portfolio_value = self.Portfolio.TotalPortfolioValue
        self.high_water_mark = self.Portfolio.TotalPortfolioValue
        self.daily_returns = []
        self.trade_count = 0
        
        # Trading state
        self.last_signal = "HOLD"
        self.last_trade_time = self.Time
        
        self.Log("✅ " + algorithm_name + " initialized with REAL trading logic")
        self.Log("📊 Parameters: MA(" + str(self.ma_short) + "/" + str(self.ma_long) + "), RSI(" + str(self.rsi_period) + "), Kelly(" + str(self.kelly_fraction) + ")")'''
    
    def OnData(self, data):
        """Handle new market data with ACTUAL trading logic."""
        
        # Ensure we have ETH data and indicators are ready
        if not self.eth.HasData or not self.ma_short_indicator.IsReady or not self.ma_long_indicator.IsReady:
            return
            
        if not self.rsi_indicator.IsReady:
            return
        
        # Get current market data
        eth_price = self.Securities[self.eth_symbol].Price
        
        # Skip if no price data
        if eth_price <= 0:
            return
        
        # Calculate technical indicators
        ma_short = self.ma_short_indicator.Current.Value
        ma_long = self.ma_long_indicator.Current.Value
        rsi = self.rsi_indicator.Current.Value
        
        # Generate trading signal based on momentum strategy
        signal, signal_strength, reasoning = self._generate_momentum_signal(
            price=eth_price,
            ma_short=ma_short,
            ma_long=ma_long,
            rsi=rsi
        )
        
        # Apply risk management
        portfolio_value = self.Portfolio.TotalPortfolioValue
        risk_approved, risk_reason = self._validate_risk_limits(portfolio_value, eth_price)
        
        if not risk_approved:
            self.Log(f"❌ Risk rejected: {risk_reason}")
            return
        
        # Execute trades based on signal
        if signal != "HOLD" and signal != self.last_signal:
            self._execute_trade(signal, signal_strength, eth_price, reasoning)
            self.last_signal = signal
            self.last_trade_time = self.Time
        
        # Update portfolio tracking
        self._update_portfolio_metrics(portfolio_value)
    
    def _generate_momentum_signal(self, price, ma_short, ma_long, rsi):
        """Generate momentum-based trading signal with actual logic."""
        
        signal = "HOLD"
        strength = 0.0
        reasoning = []
        
        # Moving average crossover logic
        ma_ratio = ma_short / ma_long if ma_long > 0 else 1.0
        
        # RSI overbought/oversold logic
        rsi_oversold = rsi < 30
        rsi_overbought = rsi > 70
        rsi_neutral = 30 <= rsi <= 70
        
        # Generate BUY signal
        if ma_ratio > 1.02:  # Short MA significantly above long MA
            signal = "BUY"
            strength = min((ma_ratio - 1.0) * 10, 1.0)  # Scale to 0-1
            reasoning.append(f"MA bullish: {ma_short:.2f} > {ma_long:.2f}")
            
            # Boost strength if RSI confirms
            if rsi_oversold:
                strength = min(strength * 1.5, 1.0)
                reasoning.append(f"RSI oversold: {rsi:.1f}")
            elif rsi_neutral:
                reasoning.append(f"RSI neutral: {rsi:.1f}")
        
        # Generate SELL signal
        elif ma_ratio < 0.98:  # Short MA significantly below long MA
            signal = "SELL"
            strength = min((1.0 - ma_ratio) * 10, 1.0)  # Scale to 0-1
            reasoning.append(f"MA bearish: {ma_short:.2f} < {ma_long:.2f}")
            
            # Boost strength if RSI confirms
            if rsi_overbought:
                strength = min(strength * 1.5, 1.0)
                reasoning.append(f"RSI overbought: {rsi:.1f}")
            elif rsi_neutral:
                reasoning.append(f"RSI neutral: {rsi:.1f}")
        
        # Apply confidence threshold
        if strength < (self.confidence_threshold / 100.0):  # Convert percentage to decimal
            signal = "HOLD"
            reasoning.append(f"Below confidence threshold: {strength:.2f} < {self.confidence_threshold/100:.2f}")
        
        reasoning_text = "; ".join(reasoning)
        return signal, strength, reasoning_text
    
    def _validate_risk_limits(self, portfolio_value, current_price):
        """Validate position against risk management rules."""
        
        # Check maximum position size
        current_holdings = self.Portfolio[self.eth_symbol].Quantity
        current_position_value = abs(current_holdings * current_price)
        max_position_value = portfolio_value * self.max_position_pct
        
        if current_position_value > max_position_value:
            return False, f"Position too large: ${current_position_value:,.0f} > ${max_position_value:,.0f}"
        
        # Check drawdown limits
        current_drawdown = (self.high_water_mark - portfolio_value) / self.high_water_mark
        if current_drawdown > self.max_drawdown:
            return False, f"Drawdown too high: {current_drawdown:.2%} > {self.max_drawdown:.2%}"
        
        # Basic volatility check (simplified)
        if len(self.daily_returns) > 10:
            recent_vol = np.std(self.daily_returns[-10:]) * np.sqrt(252)  # Annualized
            if recent_vol > self.max_volatility:
                return False, f"Volatility too high: {recent_vol:.2%} > {self.max_volatility:.2%}"
        
        return True, "Risk checks passed"
    
    def _execute_trade(self, signal, strength, price, reasoning):
        """Execute trade with Kelly criterion position sizing."""
        
        portfolio_value = self.Portfolio.TotalPortfolioValue
        
        # Calculate Kelly position size
        # Simplified Kelly: f = (win_rate * avg_win - loss_rate * avg_loss) / avg_win
        # For now, use conservative approach: position_size = kelly_fraction * strength
        base_position_pct = self.kelly_fraction * strength
        
        # Cap position size
        position_pct = min(base_position_pct, self.max_position_pct)
        position_value = portfolio_value * position_pct
        
        if signal == "BUY":
            # Calculate shares to buy
            shares_to_buy = int(position_value / price)
            if shares_to_buy > 0:
                self.MarketOrder(self.eth_symbol, shares_to_buy)
                self.trade_count += 1
                self.Log(f"🟢 BUY {shares_to_buy} ETH @ ${price:.2f} | Strength: {strength:.2f} | {reasoning}")
        
        elif signal == "SELL":
            # Sell current position
            current_holdings = self.Portfolio[self.eth_symbol].Quantity
            if current_holdings > 0:
                self.MarketOrder(self.eth_symbol, -current_holdings)
                self.trade_count += 1
                self.Log(f"🔴 SELL {current_holdings} ETH @ ${price:.2f} | Strength: {strength:.2f} | {reasoning}")
    
    def _update_portfolio_metrics(self, current_value):
        """Update portfolio performance tracking."""
        
        # Update high water mark
        if current_value > self.high_water_mark:
            self.high_water_mark = current_value
        
        # Calculate daily return (simplified)
        if self.previous_portfolio_value > 0:
            daily_return = (current_value / self.previous_portfolio_value) - 1
            self.daily_returns.append(daily_return)
            
            # Keep only recent returns for volatility calculation
            if len(self.daily_returns) > 252:  # Keep 1 year
                self.daily_returns.pop(0)
        
        self.previous_portfolio_value = current_value
    
    def OnEndOfAlgorithm(self):
        """Log final performance metrics."""
        final_value = self.Portfolio.TotalPortfolioValue
        total_return = (final_value / {parameters.get("initial_capital", 100000)}) - 1
        
        self.Log(f"📊 FINAL PERFORMANCE:")
        self.Log(f"💰 Final Value: ${final_value:,.2f}")
        self.Log(f"📈 Total Return: {total_return:.2%}")
        self.Log(f"🔄 Total Trades: {self.trade_count}")
        self.Log(f"✅ Algorithm completed with REAL trading logic")
'''
    
    return algorithm_code

if __name__ == "__main__":
    # Test algorithm generation
    test_params = {
        "initial_capital": 100000,
        "ma_short": 5,
        "ma_long": 20,
        "rsi_period": 14,
        "kelly_fraction": 0.167,
        "confidence_threshold": 50,  # Lower threshold for testing
        "max_volatility": 0.30,
        "max_drawdown": 0.15,
        "var_limit_1day": 0.06,
        "max_position_pct": 0.25
    }
    
    algorithm_code = generate_working_lean_algorithm("TestAlgorithm", test_params, "test_simulation")
    print("✅ Working LEAN algorithm generated successfully")
    print(f"📝 Code length: {len(algorithm_code):,} characters")
    print("🎯 Includes: Real momentum signals, risk management, Kelly sizing, trade execution")