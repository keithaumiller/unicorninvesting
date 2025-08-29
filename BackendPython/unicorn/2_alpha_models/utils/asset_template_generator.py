"""
Asset Template Generator

Creates standardized directory structure and template files for new asset classes.
"""

import os
from typing import Dict, List

class AssetTemplateGenerator:
    """Generate template structure for new asset classes."""
    
    def __init__(self, base_path: str = "."):
        self.base_path = base_path
        
    def create_asset_structure(self, asset_name: str, asset_class: str = "CRYPTO"):
        ""# Example usage
if __name__ == "__main__":
    generator = AssetTemplateGenerator()
    
    # Create templates for common assets
    assets = [
        ("BTC", "CRYPTO"),
        ("AAPL", "EQUITIES"), 
        ("EURUSD", "FOREX"),
        ("SPY", "EQUITIES")
    ]
    
    for asset_name, asset_class in assets:
        print(f"Creating template for {asset_name} in {asset_class}")
        generator.create_asset_structure(asset_name, asset_class)
        
    print("✅ All asset templates created")

    def _create_prophet_model_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create Prophet model template."""
        template_path = os.path.join(asset_dir, 'models', f'{asset_name.lower()}_prophet.py')
        
        template_content = f'''"""
{asset_name} Prophet Model for {asset_class}

Time series forecasting model using Facebook Prophet.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.model_framework import ProphetModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class {asset_name}ProphetModel(ProphetModel):
    """
    Prophet-based forecasting model for {asset_name}.
    
    Uses Facebook Prophet for time series forecasting with {asset_name}-specific optimizations.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        # Asset-specific Prophet configuration
        default_config = {{
            'seasonality_mode': 'multiplicative',
            'yearly_seasonality': True,
            'weekly_seasonality': True,
            'daily_seasonality': False,
            'changepoint_prior_scale': 0.05 if '{asset_name}' == 'ETH' else 0.1,
            'seasonality_prior_scale': 10.0 if '{asset_name}' == 'ETH' else 15.0
        }}
        
        if config:
            default_config.update(config)
            
        super().__init__('{asset_name}', default_config)
        self.performance_tracker = ModelPerformanceTracker()

if __name__ == "__main__":
    print("✅ {asset_name} Prophet Model Template Ready")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)

    def _create_xgboost_model_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create XGBoost model template.""" 
        template_path = os.path.join(asset_dir, 'models', f'{asset_name.lower()}_xgboost.py')
        
        template_content = f'''"""
{asset_name} XGBoost Model for {asset_class}

Gradient boosting model for {asset_name} price prediction.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.model_framework import XGBoostModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class {asset_name}XGBoostModel(XGBoostModel):
    """
    XGBoost-based prediction model for {asset_name}.
    
    Uses gradient boosting with {asset_name}-specific feature engineering.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        # Asset-specific XGBoost configuration
        default_config = {{
            'n_estimators': 100 if '{asset_name}' == 'ETH' else 150,
            'max_depth': 6 if '{asset_name}' == 'ETH' else 8,
            'learning_rate': 0.1 if '{asset_name}' == 'ETH' else 0.08
        }}
        
        if config:
            default_config.update(config)
            
        super().__init__('{asset_name}', default_config)
        self.performance_tracker = ModelPerformanceTracker()

if __name__ == "__main__":
    print("✅ {asset_name} XGBoost Model Template Ready")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)

    def _create_ensemble_model_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create Ensemble model template."""
        template_path = os.path.join(asset_dir, 'models', f'{asset_name.lower()}_ensemble.py')
        
        template_content = f'''"""
{asset_name} Ensemble Model for {asset_class}

Ensemble model combining Prophet and XGBoost for {asset_name}.
"""

import pandas as pd
import numpy as np  
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.model_framework import EnsembleModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class {asset_name}EnsembleModel(EnsembleModel):
    """
    Ensemble model for {asset_name} combining Prophet and XGBoost.
    
    Uses weighted combination of time series forecasting and feature-based prediction.
    """
    
    def __init__(self, prophet_weight: float = 0.6, xgboost_weight: float = 0.4):
        super().__init__('{asset_name}', prophet_weight, xgboost_weight)
        self.performance_tracker = ModelPerformanceTracker()

if __name__ == "__main__":
    print("✅ {asset_name} Ensemble Model Template Ready")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)

    def _create_validation_script(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create comprehensive validation script."""
        template_path = os.path.join(asset_dir, 'scripts', f'{asset_name.lower()}_validation.py')
        
        template_content = f'''"""
{asset_name} Model Validation Script

Comprehensive validation of all {asset_name} models.
"""

import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.testing_framework import run_model_test_suite, generate_test_report

def run_comprehensive_validation():
    """Run comprehensive validation of all {asset_name} models."""
    print(f"🚀 Starting Comprehensive {asset_name} Model Validation")
    print("=" * 60)
    
    # Placeholder for comprehensive validation
    # TODO: Implement full validation suite
    
    print(f"🏁 {asset_name} Model Validation Complete!")

if __name__ == "__main__":
    run_comprehensive_validation()
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)complete directory structure for new asset.
        
        Args:
            asset_name: Name of the asset (e.g., 'BTC', 'AAPL')
            asset_class: Asset class category (ETH, FOREX, CRYPTO, EQUITIES)
        """
        asset_dir = os.path.join(self.base_path, asset_class)
        
        # Create directories
        directories = [
            'models',
            'algorithms', 
            'features',
            'research',
            'scripts',
            'tests'
        ]
        
        for directory in directories:
            dir_path = os.path.join(asset_dir, directory)
            os.makedirs(dir_path, exist_ok=True)
            
            # Create __init__.py
            init_file = os.path.join(dir_path, '__init__.py')
            with open(init_file, 'w') as f:
                f.write(f'"""\n{asset_class} {directory.title()} Package\n"""\n')
        
        # Create template files
        self._create_model_template(asset_dir, asset_name, asset_class)
        self._create_prophet_model_template(asset_dir, asset_name, asset_class)
        self._create_xgboost_model_template(asset_dir, asset_name, asset_class)
        self._create_ensemble_model_template(asset_dir, asset_name, asset_class)
        self._create_algorithm_template(asset_dir, asset_name, asset_class)
        self._create_test_template(asset_dir, asset_name, asset_class)
        self._create_validation_script(asset_dir, asset_name, asset_class)
        
    def _create_model_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create alpha model template."""
        template_path = os.path.join(asset_dir, 'models', f'{asset_name.lower()}_alpha.py')
        
        template_content = f'''"""
{asset_name} Alpha Model for {asset_class}

Template for implementing alpha models for {asset_name}.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List
from ...shared.base_alpha import TechnicalAlphaModel

class {asset_name}AlphaModel(TechnicalAlphaModel):
    """
    Alpha model for {asset_name} trading.
    
    Implements technical analysis based signals for {asset_name}.
    """
    
    def __init__(self, lookback_window: int = 100):
        super().__init__(
            name=f"{asset_name}AlphaModel",
            asset_class="{asset_class}",
            lookback_window=lookback_window
        )
        
        # Model-specific parameters
        self.short_window = 20
        self.long_window = 50
        self.rsi_window = 14
        self.rsi_overbought = 70
        self.rsi_oversold = 30
        
    def get_required_columns(self) -> List[str]:
        """Return required data columns."""
        return ['Open', 'High', 'Low', 'Close', 'Volume']
        
    def generate_signal(self, data: pd.DataFrame) -> Dict[str, Any]:
        """
        Generate trading signal for {asset_name}.
        
        Args:
            data: OHLCV data
            
        Returns:
            Signal dictionary with direction, confidence, and metadata
        """
        self.validate_data(data)
        
        if len(data) < self.lookback_window:
            return {{
                'signal': 0,
                'confidence': 0.0,
                'metadata': {{'reason': 'Insufficient data'}}
            }}
        
        # Calculate technical indicators
        close_prices = data['Close']
        
        # Moving averages
        sma_short = self.calculate_sma(close_prices, self.short_window)
        sma_long = self.calculate_sma(close_prices, self.long_window)
        
        # RSI
        rsi = self.calculate_rsi(close_prices, self.rsi_window)
        
        # Bollinger Bands
        bb = self.calculate_bollinger_bands(close_prices)
        
        # Get latest values
        current_price = close_prices.iloc[-1]
        current_sma_short = sma_short.iloc[-1]
        current_sma_long = sma_long.iloc[-1]
        current_rsi = rsi.iloc[-1]
        current_bb_upper = bb['upper'].iloc[-1]
        current_bb_lower = bb['lower'].iloc[-1]
        
        # Generate signal
        signal = 0
        confidence = 0.0
        metadata = {{}}
        
        # Moving average crossover
        if current_sma_short > current_sma_long:
            signal += 0.4
            metadata['ma_signal'] = 'bullish'
        else:
            signal -= 0.4
            metadata['ma_signal'] = 'bearish'
            
        # RSI oversold/overbought
        if current_rsi < self.rsi_oversold:
            signal += 0.3
            metadata['rsi_signal'] = 'oversold'
        elif current_rsi > self.rsi_overbought:
            signal -= 0.3
            metadata['rsi_signal'] = 'overbought'
        else:
            metadata['rsi_signal'] = 'neutral'
            
        # Bollinger Bands
        if current_price < current_bb_lower:
            signal += 0.3
            metadata['bb_signal'] = 'oversold'
        elif current_price > current_bb_upper:
            signal -= 0.3
            metadata['bb_signal'] = 'overbought'
        else:
            metadata['bb_signal'] = 'neutral'
            
        # Normalize signal and calculate confidence
        signal = np.clip(signal, -1, 1)
        confidence = abs(signal)
        
        # Convert to discrete signal
        if signal > 0.2:
            discrete_signal = 1
        elif signal < -0.2:
            discrete_signal = -1
        else:
            discrete_signal = 0
            
        self.signals_generated += 1
        self.last_signal_time = data.index[-1]
        
        return {{
            'signal': discrete_signal,
            'confidence': confidence,
            'metadata': {{
                'price': current_price,
                'sma_short': current_sma_short,
                'sma_long': current_sma_long,
                'rsi': current_rsi,
                'bb_position': (current_price - current_bb_lower) / (current_bb_upper - current_bb_lower),
                **metadata
            }}
        }}
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)
    
    def _create_algorithm_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create LEAN algorithm template."""
        template_path = os.path.join(asset_dir, 'algorithms', f'{asset_name.lower()}_algorithm.py')
        
        template_content = f'''"""
{asset_name} LEAN Algorithm for {asset_class}

LEAN trading algorithm implementation for {asset_name}.
"""

from AlgorithmImports import *
from ...{asset_class.lower()}.models.{asset_name.lower()}_alpha import {asset_name}AlphaModel

class {asset_name}Algorithm(QCAlgorithm):
    """
    LEAN algorithm for {asset_name} trading.
    """
    
    def Initialize(self):
        """Initialize algorithm."""
        # Set dates and cash
        self.SetStartDate(2020, 1, 1)
        self.SetEndDate(2023, 12, 31)
        self.SetCash(100000)
        
        # Add {asset_name} data
        # TODO: Update symbol based on actual {asset_name} ticker
        self.symbol = self.AddEquity("{asset_name}", Resolution.Daily).Symbol
        
        # Set alpha model
        self.alpha_model = {asset_name}AlphaModel()
        
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
            self.Debug(f"Buy signal: confidence={{confidence:.2f}}")
        elif signal == -1 and confidence > 0.5:
            self.SetHoldings(self.symbol, -0.8 * confidence)
            self.Debug(f"Sell signal: confidence={{confidence:.2f}}")
        elif signal == 0:
            self.Liquidate(self.symbol)
            self.Debug("Hold/Exit signal")
            
    def OnEndOfAlgorithm(self):
        """Called at end of algorithm."""
        self.Debug(f"Total signals generated: {{self.alpha_model.signals_generated}}")
        performance = self.alpha_model.get_performance_summary()
        self.Debug(f"Alpha model performance: {{performance}}")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)
    
    def _create_test_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create test template."""
        template_path = os.path.join(asset_dir, 'tests', f'test_{asset_name.lower()}_alpha.py')
        
        template_content = f'''"""
Tests for {asset_name} Alpha Model
"""

import pytest
import pandas as pd
import numpy as np
from unittest.mock import Mock, patch
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from models.{asset_name.lower()}_alpha import {asset_name}AlphaModel

class Test{asset_name}AlphaModel:
    """Test cases for {asset_name} alpha model."""
    
    def setup_method(self):
        """Setup test fixtures."""
        self.alpha_model = {asset_name}AlphaModel()
        
        # Create sample data
        np.random.seed(42)
        dates = pd.date_range('2023-01-01', periods=200, freq='D')
        
        self.sample_data = pd.DataFrame({{
            'Open': 100 + np.cumsum(np.random.randn(200) * 0.02),
            'High': np.nan,
            'Low': np.nan, 
            'Close': np.nan,
            'Volume': np.random.randint(1000000, 10000000, 200)
        }}, index=dates)
        
        # Generate OHLC
        self.sample_data['Close'] = self.sample_data['Open'] + np.random.randn(200) * 0.01
        self.sample_data['High'] = np.maximum(
            self.sample_data['Open'], 
            self.sample_data['Close']
        ) + np.abs(np.random.randn(200) * 0.005)
        self.sample_data['Low'] = np.minimum(
            self.sample_data['Open'],
            self.sample_data['Close'] 
        ) - np.abs(np.random.randn(200) * 0.005)
        
    def test_model_initialization(self):
        """Test model initializes correctly."""
        assert self.alpha_model.name == "{asset_name}AlphaModel"
        assert self.alpha_model.asset_class == "{asset_class}"
        assert self.alpha_model.signals_generated == 0
        
    def test_required_columns(self):
        """Test required columns are correctly specified."""
        required = self.alpha_model.get_required_columns()
        expected = ['Open', 'High', 'Low', 'Close', 'Volume']
        assert required == expected
        
    def test_data_validation(self):
        """Test data validation works."""
        # Valid data should pass
        assert self.alpha_model.validate_data(self.sample_data)
        
        # Missing columns should fail
        invalid_data = self.sample_data.drop('Close', axis=1)
        with pytest.raises(ValueError):
            self.alpha_model.validate_data(invalid_data)
            
    def test_signal_generation(self):
        """Test signal generation."""
        signal_result = self.alpha_model.generate_signal(self.sample_data)
        
        # Check return format
        assert isinstance(signal_result, dict)
        assert 'signal' in signal_result
        assert 'confidence' in signal_result
        assert 'metadata' in signal_result
        
        # Check signal values
        assert signal_result['signal'] in [-1, 0, 1]
        assert 0.0 <= signal_result['confidence'] <= 1.0
        
        # Check metadata
        metadata = signal_result['metadata']
        assert 'price' in metadata
        assert 'rsi' in metadata
        
    def test_insufficient_data(self):
        """Test behavior with insufficient data."""
        small_data = self.sample_data.head(10)
        signal_result = self.alpha_model.generate_signal(small_data)
        
        assert signal_result['signal'] == 0
        assert signal_result['confidence'] == 0.0
        assert 'Insufficient data' in signal_result['metadata']['reason']
        
    def test_signal_counter(self):
        """Test signal counter increments."""
        initial_count = self.alpha_model.signals_generated
        self.alpha_model.generate_signal(self.sample_data)
        assert self.alpha_model.signals_generated == initial_count + 1
        
    def test_performance_tracking(self):
        """Test performance tracking functionality."""
        signal_result = self.alpha_model.generate_signal(self.sample_data)
        
        # Update with mock return
        self.alpha_model.update_performance(signal_result, 0.02)
        
        # Check performance summary
        performance = self.alpha_model.get_performance_summary()
        assert 'signals_generated' in performance
        assert 'total_signals' in performance
        assert performance['total_signals'] == 1
        
    def test_multiple_signals(self):
        """Test multiple signal generation."""
        signals = []
        
        # Generate multiple signals with rolling windows
        for i in range(10):
            data_window = self.sample_data.iloc[i:i+100]
            if len(data_window) >= 100:
                signal_result = self.alpha_model.generate_signal(data_window)
                signals.append(signal_result['signal'])
                
        # Should have generated some signals
        assert len(signals) > 0
        assert self.alpha_model.signals_generated >= len(signals)
        
        # Signals should be in valid range
        for signal in signals:
            assert signal in [-1, 0, 1]

if __name__ == "__main__":
    # Run basic test
    test_model = Test{asset_name}AlphaModel()
    test_model.setup_method()
    test_model.test_signal_generation()
    print("✅ {asset_name} Alpha Model basic test passed")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)

# Example usage
if __name__ == "__main__":
    generator = AssetTemplateGenerator()
    
    # Create templates for common assets
    assets = [
        ("BTC", "CRYPTO"),
        ("AAPL", "EQUITIES"), 
        ("EURUSD", "FOREX"),
        ("SPY", "EQUITIES")
    ]
    
    for asset_name, asset_class in assets:
        print(f"Creating template for {{asset_name}} in {{asset_class}}")
        generator.create_asset_structure(asset_name, asset_class)
        
    print("✅ All asset templates created")
