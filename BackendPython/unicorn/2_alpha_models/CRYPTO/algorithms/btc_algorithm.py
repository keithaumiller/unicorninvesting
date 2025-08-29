"""
BTC LEAN Algorithm for CRYPTO

LEAN trading algorithm implementation for BTC.
"""

from AlgorithmImports import *
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from ..models.btc_alpha import BTCAlphaModel

class BTCAlgorithm(QCAlgorithm):
    """
    LEAN algorithm for BTC trading.
    """
    
    def Initialize(self):
        """Initialize algorithm."""
        # Set dates and cash
        self.SetStartDate(2020, 1, 1)
        self.SetEndDate(2023, 12, 31)
        self.SetCash(100000)
        
        # Add BTC data
        # TODO: Update symbol based on actual BTC ticker
        self.symbol = self.AddEquity("BTC", Resolution.Daily).Symbol
        
        # Set alpha model
        self.alpha_model = BTCAlphaModel()
        
        # Portfolio construction
        self.SetPortfolioConstruction(EqualWeightingPortfolioConstructionModel())
        
        # Execution model
        self.SetExecution(ImmediateExecutionModel())
        
        # Risk management
        self.SetRiskManagement(MaximumDrawdownPercentPerSecurity(0.05))
        
        # Universe selection
        self.SetUniverseSelection(ManualUniverseSelectionModel([self.symbol]))
        
        # Warm up for technical indicators
        self.SetWarmUp(100)
        
    def OnData(self, data):
        """Process new data."""
        if self.IsWarmingUp:
            return
            
        if not data.ContainsKey(self.symbol):
            return
            
        # Get recent data for alpha model
        history = self.History(self.symbol, 100, Resolution.Daily)
        
        if history.empty:
            return
            
        # Convert to expected format
        df = history.droplevel(0, axis=0)
        df.columns = ['Open', 'High', 'Low', 'Close', 'Volume']
        
        # Generate signal
        signal_result = self.alpha_model.generate_signal(df)
        
        # Execute trades based on signal
        signal = signal_result['signal']
        confidence = signal_result['confidence']
        
        if signal == 1 and confidence > 0.5:
            self.SetHoldings(self.symbol, 0.8 * confidence)
            self.Debug(f"Buy signal: confidence={confidence:.2f}")
        elif signal == -1 and confidence > 0.5:
            self.SetHoldings(self.symbol, -0.8 * confidence)
            self.Debug(f"Sell signal: confidence={confidence:.2f}")
        elif signal == 0:
            self.Liquidate(self.symbol)
            self.Debug("Hold/Exit signal")
            
    def OnEndOfAlgorithm(self):
        """Called at end of algorithm."""
        self.Debug(f"Total signals generated: {self.alpha_model.signals_generated}")

if __name__ == "__main__":
    print("✅ BTC LEAN Algorithm Template Ready")
