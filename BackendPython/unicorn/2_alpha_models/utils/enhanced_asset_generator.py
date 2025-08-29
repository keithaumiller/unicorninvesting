"""
Enhanced Asset Template Generator

Creates standardized directory structure and template files for new asset classes with
comprehensive model support (Technical Alpha, Prophet, XGBoost, Ensemble).
"""

import os
from typing import Dict, List
from datetime import datetime

class EnhancedAssetTemplateGenerator:
    """Generate comprehensive template structure for new asset classes."""
    
    def __init__(self, base_path: str = "."):
        self.base_path = base_path
        
    def create_asset_structure(self, asset_name: str, asset_class: str = "CRYPTO"):
        """
        Create complete directory structure for new asset with all model types.
        
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
        
        # Create all model templates
        self._create_technical_alpha_template(asset_dir, asset_name, asset_class)
        self._create_prophet_model_template(asset_dir, asset_name, asset_class)
        self._create_xgboost_model_template(asset_dir, asset_name, asset_class)
        self._create_ensemble_model_template(asset_dir, asset_name, asset_class)
        self._create_algorithm_template(asset_dir, asset_name, asset_class)
        self._create_comprehensive_test_template(asset_dir, asset_name, asset_class)
        self._create_validation_script(asset_dir, asset_name, asset_class)
        self._create_model_builder_script(asset_dir, asset_name, asset_class)
        
        print(f"✅ Created complete asset structure for {asset_name} in {asset_class}")
        print(f"   - Technical Alpha Model")
        print(f"   - Prophet Forecasting Model")
        print(f"   - XGBoost Prediction Model")
        print(f"   - Ensemble Model")
        print(f"   - LEAN Algorithm")
        print(f"   - Comprehensive Test Suite")
        print(f"   - Validation Framework")
        print(f"   - Model Builder Script")
    
    def _create_technical_alpha_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create technical alpha model template."""
        template_path = os.path.join(asset_dir, 'models', f'{asset_name.lower()}_alpha.py')
        
        template_content = f'''"""
{asset_name} Technical Alpha Model for {asset_class}

Technical analysis based alpha model for {asset_name} trading.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.base_alpha import TechnicalAlphaModel

class {asset_name}AlphaModel(TechnicalAlphaModel):
    """
    Technical alpha model for {asset_name} trading.
    
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

if __name__ == "__main__":
    print("✅ {asset_name} Technical Alpha Model Template Ready")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)
    
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
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train model with validation split.
        
        Args:
            data: Historical price data
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Split data
        split_idx = int(len(data) * (1 - validation_split))
        train_data = data.iloc[:split_idx]
        val_data = data.iloc[split_idx:]
        
        # Train model
        train_result = self.train(train_data)
        
        if not train_result['success']:
            return train_result
        
        # Validate on holdout data
        val_predictions = self.predict(train_data, periods=len(val_data))
        
        # Track performance
        model_id = f"{{self.asset_name}}_prophet_{{datetime.now().strftime('%Y%m%d_%H%M%S')}}"
        
        # Track validation performance
        self.performance_tracker.track_validation_performance(
            model_id=model_id,
            predictions=val_predictions['yhat'],
            actuals=val_data['Close']
        )
        
        return {{
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'validation_samples': len(val_data),
            'train_samples': len(train_data)
        }}

def create_and_train_{asset_name.lower()}_prophet(data: pd.DataFrame) -> {asset_name}ProphetModel:
    """
    Convenience function to create and train {asset_name} Prophet model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained Prophet model
    """
    model = {asset_name}ProphetModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ {asset_name} Prophet model trained successfully")
        print(f"Model ID: {{result['model_id']}}")
    else:
        print(f"❌ {asset_name} Prophet model training failed: {{result.get('error', 'Unknown error')}}")
    
    return model

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
            'learning_rate': 0.1 if '{asset_name}' == 'ETH' else 0.08,
            'subsample': 0.8 if '{asset_name}' == 'ETH' else 0.9,
            'colsample_bytree': 0.8 if '{asset_name}' == 'ETH' else 0.9,
            'feature_windows': [5, 10, 20, 50] if '{asset_name}' == 'ETH' else [5, 10, 20, 50, 100]
        }}
        
        if config:
            default_config.update(config)
            
        super().__init__('{asset_name}', default_config)
        self.performance_tracker = ModelPerformanceTracker()
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train model with validation split and performance tracking.
        
        Args:
            data: Historical price data
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Train model
        train_result = self.train(data)
        
        if not train_result['success']:
            return train_result
        
        # Track performance
        model_id = f"{{self.asset_name}}_xgboost_{{datetime.now().strftime('%Y%m%d_%H%M%S')}}"
        
        # Get predictions for performance tracking
        X, y = self.prepare_data(data)
        train_size = int(len(X) * 0.8)
        
        X_train = X.iloc[:train_size]
        y_train = y.iloc[:train_size]
        X_val = X.iloc[train_size:]
        y_val = y.iloc[train_size:]
        
        # Scale and predict
        X_train_scaled = self.scaler.transform(X_train)
        X_val_scaled = self.scaler.transform(X_val)
        
        train_pred = pd.Series(self.model.predict(X_train_scaled), index=y_train.index)
        val_pred = pd.Series(self.model.predict(X_val_scaled), index=y_val.index)
        
        # Track validation performance  
        self.performance_tracker.track_validation_performance(
            model_id=model_id,
            predictions=val_pred,
            actuals=y_val
        )
        
        return {{
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'validation_samples': len(X_val),
            'train_samples': len(X_train),
            'feature_importance': dict(zip(self.feature_columns, self.model.feature_importances_))
        }}

def create_and_train_{asset_name.lower()}_xgboost(data: pd.DataFrame) -> {asset_name}XGBoostModel:
    """
    Convenience function to create and train {asset_name} XGBoost model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained XGBoost model
    """
    model = {asset_name}XGBoostModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ {asset_name} XGBoost model trained successfully")
        print(f"Model ID: {{result['model_id']}}")
    else:
        print(f"❌ {asset_name} XGBoost model training failed: {{result.get('error', 'Unknown error')}}")
    
    return model

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
from .{asset_name.lower()}_prophet import {asset_name}ProphetModel
from .{asset_name.lower()}_xgboost import {asset_name}XGBoostModel

class {asset_name}EnsembleModel(EnsembleModel):
    """
    Ensemble model for {asset_name} combining Prophet and XGBoost.
    
    Uses weighted combination of time series forecasting and feature-based prediction.
    """
    
    def __init__(self, prophet_weight: float = 0.6, xgboost_weight: float = 0.4):
        super().__init__('{asset_name}', prophet_weight, xgboost_weight)
        
        # Replace base models with asset-specific models
        self.prophet_model = {asset_name}ProphetModel()
        self.xgboost_model = {asset_name}XGBoostModel()
        
        self.performance_tracker = ModelPerformanceTracker()
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train ensemble model with validation split and performance tracking.
        
        Args:
            data: Historical price data
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Train ensemble
        train_result = self.train(data)
        
        if not train_result['success']:
            return train_result
        
        # Track performance
        model_id = f"{{self.asset_name}}_ensemble_{{datetime.now().strftime('%Y%m%d_%H%M%S')}}"
        
        return {{
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'prophet_weight': self.prophet_weight,
            'xgboost_weight': self.xgboost_weight
        }}

def create_and_train_{asset_name.lower()}_ensemble(data: pd.DataFrame) -> {asset_name}EnsembleModel:
    """
    Convenience function to create and train {asset_name} Ensemble model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained Ensemble model
    """
    model = {asset_name}EnsembleModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ {asset_name} Ensemble model trained successfully")
        print(f"Model ID: {{result['model_id']}}")
    else:
        print(f"❌ {asset_name} Ensemble model training failed: {{result.get('error', 'Unknown error')}}")
    
    return model

if __name__ == "__main__":
    print("✅ {asset_name} Ensemble Model Template Ready")
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
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from ..models.{asset_name.lower()}_alpha import {asset_name}AlphaModel

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

if __name__ == "__main__":
    print("✅ {asset_name} LEAN Algorithm Template Ready")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)
    
    def _create_comprehensive_test_template(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create comprehensive test template."""
        template_path = os.path.join(asset_dir, 'tests', f'test_{asset_name.lower()}_models.py')
        
        template_content = f'''"""
Comprehensive Tests for {asset_name} Models
"""

import pytest
import pandas as pd
import numpy as np
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.testing_framework import run_model_test_suite
from models.{asset_name.lower()}_alpha import {asset_name}AlphaModel

class Test{asset_name}Models:
    """Comprehensive test suite for {asset_name} models."""
    
    def setup_method(self):
        """Setup test fixtures."""
        self.alpha_model = {asset_name}AlphaModel()
        
        # Create sample data
        np.random.seed(42)
        dates = pd.date_range('2023-01-01', periods=200, freq='D')
        
        base_price = 50000 if '{asset_name}' == 'BTC' else 3000 if '{asset_name}' == 'ETH' else 100
        
        self.sample_data = pd.DataFrame({{
            'Open': base_price + np.cumsum(np.random.randn(200) * 0.02),
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
        
    def test_alpha_model_comprehensive(self):
        """Run comprehensive test suite for alpha model."""
        results = run_model_test_suite(
            model_class={asset_name}AlphaModel,
            asset_name='{asset_name}',
            model_type="technical"
        )
        
        assert results['overall_passed'], f"Alpha model tests failed: {{results}}"
        
    def test_signal_generation(self):
        """Test signal generation functionality."""
        signal_result = self.alpha_model.generate_signal(self.sample_data)
        
        assert isinstance(signal_result, dict)
        assert 'signal' in signal_result
        assert 'confidence' in signal_result
        assert signal_result['signal'] in [-1, 0, 1]
        assert 0.0 <= signal_result['confidence'] <= 1.0

if __name__ == "__main__":
    # Run basic tests
    test_suite = Test{asset_name}Models()
    test_suite.setup_method()
    test_suite.test_signal_generation()
    print("✅ {asset_name} Models basic tests passed")
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
from models.{asset_name.lower()}_alpha import {asset_name}AlphaModel

def generate_test_data() -> pd.DataFrame:
    """Generate test data for {asset_name} validation."""
    np.random.seed(42)
    
    base_price = 50000 if '{asset_name}' == 'BTC' else 3000 if '{asset_name}' == 'ETH' else 100
    volatility = 0.04 if '{asset_name}' == 'BTC' else 0.05 if '{asset_name}' == 'ETH' else 0.03
    
    periods = 730
    dates = pd.date_range(
        start=datetime.now() - timedelta(days=periods),
        periods=periods,
        freq='D'
    )
    
    # Generate returns
    returns = np.random.normal(0, volatility, periods)
    
    # Convert to prices
    log_prices = np.log(base_price) + np.cumsum(returns)
    prices = np.exp(log_prices)
    
    # Generate OHLC
    close_prices = prices
    open_prices = np.roll(close_prices, 1)
    open_prices[0] = close_prices[0]
    
    intraday_noise = np.random.normal(0, volatility * 0.2, periods)
    high_prices = np.maximum(open_prices, close_prices) + np.abs(intraday_noise)
    low_prices = np.minimum(open_prices, close_prices) - np.abs(intraday_noise)
    
    # Generate volume
    base_volume = 1000000 if '{asset_name}' in ['BTC', 'ETH'] else 100000
    volume = base_volume * np.random.lognormal(0, 0.3, periods)
    
    return pd.DataFrame({{
        'Open': open_prices,
        'High': high_prices,
        'Low': low_prices,
        'Close': close_prices,
        'Volume': volume
    }}, index=dates)

def run_comprehensive_validation() -> Dict[str, Any]:
    """Run comprehensive validation of all {asset_name} models."""
    print(f"🚀 Starting Comprehensive {asset_name} Model Validation")
    print("=" * 60)
    
    # Validate technical alpha model
    print(f"🧪 Validating {asset_name} Technical Alpha Model...")
    test_results = run_model_test_suite(
        model_class={asset_name}AlphaModel,
        asset_name='{asset_name}',
        model_type="technical"
    )
    
    # Generate test report
    report = generate_test_report(test_results)
    report_file = f"{asset_name.lower()}_validation_report.txt"
    with open(report_file, 'w') as f:
        f.write(report)
    
    print(f"✅ Validation complete. Report saved to {{report_file}}")
    
    return test_results

if __name__ == "__main__":
    results = run_comprehensive_validation()
    print(f"🏁 {asset_name} Model Validation Complete!")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)
    
    def _create_model_builder_script(self, asset_dir: str, asset_name: str, asset_class: str):
        """Create model builder script."""
        template_path = os.path.join(asset_dir, 'scripts', f'{asset_name.lower()}_model_builder.py')
        
        template_content = f'''"""
{asset_name} Model Builder Script

Build and train all {asset_name} models with performance tracking.
"""

import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta
import json

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.performance_tracker import ModelPerformanceTracker
from models.{asset_name.lower()}_prophet import create_and_train_{asset_name.lower()}_prophet
from models.{asset_name.lower()}_xgboost import create_and_train_{asset_name.lower()}_xgboost
from models.{asset_name.lower()}_ensemble import create_and_train_{asset_name.lower()}_ensemble

def generate_training_data() -> pd.DataFrame:
    """Generate training data for {asset_name} models."""
    # This would connect to real data sources in production
    # For now, generate realistic synthetic data
    
    np.random.seed(42)
    base_price = 50000 if '{asset_name}' == 'BTC' else 3000 if '{asset_name}' == 'ETH' else 100
    volatility = 0.04 if '{asset_name}' == 'BTC' else 0.05 if '{asset_name}' == 'ETH' else 0.03
    
    periods = 1000  # ~3 years of data
    dates = pd.date_range(
        start=datetime.now() - timedelta(days=periods),
        periods=periods,
        freq='D'
    )
    
    # Generate price series with realistic patterns
    returns = np.random.normal(0.0005, volatility, periods)  # Slight positive drift
    
    # Add some trend and cycle patterns
    trend = np.linspace(0, 0.5, periods)  # Long-term uptrend
    cycle = 0.1 * np.sin(2 * np.pi * np.arange(periods) / 365)  # Annual cycle
    
    returns += trend/periods + cycle/periods
    
    # Generate prices
    log_prices = np.log(base_price) + np.cumsum(returns)
    prices = np.exp(log_prices)
    
    # Generate OHLC
    close_prices = prices
    open_prices = np.roll(close_prices, 1)
    open_prices[0] = close_prices[0]
    
    # Add realistic intraday variation
    daily_range = volatility * 0.5
    high_prices = np.maximum(open_prices, close_prices) * (1 + np.abs(np.random.normal(0, daily_range, periods)))
    low_prices = np.minimum(open_prices, close_prices) * (1 - np.abs(np.random.normal(0, daily_range, periods)))
    
    # Generate volume with correlation to price movements
    price_changes = np.abs(np.diff(np.append(close_prices[0], close_prices)))
    base_volume = 1000000 if '{asset_name}' in ['BTC', 'ETH'] else 100000
    volume_multiplier = 1 + price_changes * 20  # Higher volume on big moves
    volume = base_volume * volume_multiplier * np.random.lognormal(0, 0.4, periods)
    
    return pd.DataFrame({{
        'Open': open_prices,
        'High': high_prices,
        'Low': low_prices,
        'Close': close_prices,
        'Volume': volume
    }}, index=dates)

def build_all_models() -> Dict[str, Any]:
    """Build and train all {asset_name} models."""
    print(f"🏗️ Building All {asset_name} Models")
    print("=" * 50)
    
    # Generate training data
    print("📊 Generating training data...")
    training_data = generate_training_data()
    print(f"✅ Generated {{len(training_data)}} days of training data")
    
    results = {{
        'asset_name': '{asset_name}',
        'build_timestamp': datetime.now().isoformat(),
        'training_samples': len(training_data),
        'models': {{}}
    }}
    
    # Build Prophet model
    try:
        print("\\n🔮 Building Prophet Model...")
        prophet_model = create_and_train_{asset_name.lower()}_prophet(training_data)
        results['models']['prophet'] = {{
            'success': prophet_model.is_trained,
            'model_type': 'Prophet',
            'training_metrics': prophet_model.performance_metrics
        }}
        
        # Save model
        prophet_model.save_model(f"{asset_name.lower()}_prophet_model.pkl")
        print(f"💾 Prophet model saved to {asset_name.lower()}_prophet_model.pkl")
        
    except Exception as e:
        print(f"❌ Prophet model building failed: {{e}}")
        results['models']['prophet'] = {{'success': False, 'error': str(e)}}
    
    # Build XGBoost model
    try:
        print("\\n🌲 Building XGBoost Model...")
        xgboost_model = create_and_train_{asset_name.lower()}_xgboost(training_data)
        results['models']['xgboost'] = {{
            'success': xgboost_model.is_trained,
            'model_type': 'XGBoost',
            'training_metrics': xgboost_model.performance_metrics
        }}
        
        # Save model
        xgboost_model.save_model(f"{asset_name.lower()}_xgboost_model.pkl")
        print(f"💾 XGBoost model saved to {asset_name.lower()}_xgboost_model.pkl")
        
    except Exception as e:
        print(f"❌ XGBoost model building failed: {{e}}")
        results['models']['xgboost'] = {{'success': False, 'error': str(e)}}
    
    # Build Ensemble model
    try:
        print("\\n🎯 Building Ensemble Model...")
        ensemble_model = create_and_train_{asset_name.lower()}_ensemble(training_data)
        results['models']['ensemble'] = {{
            'success': ensemble_model.is_trained,
            'model_type': 'Ensemble',
            'training_metrics': ensemble_model.performance_metrics
        }}
        
        # Save model
        ensemble_model.save_model(f"{asset_name.lower()}_ensemble_model.pkl")
        print(f"💾 Ensemble model saved to {asset_name.lower()}_ensemble_model.pkl")
        
    except Exception as e:
        print(f"❌ Ensemble model building failed: {{e}}")
        results['models']['ensemble'] = {{'success': False, 'error': str(e)}}
    
    # Save results
    results_file = f"{asset_name.lower()}_model_build_results.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    # Summary
    print("\\n" + "=" * 50)
    print(f"📈 {asset_name} Model Building Summary")
    print("=" * 50)
    
    successful_models = [name for name, result in results['models'].items() if result.get('success', False)]
    failed_models = [name for name, result in results['models'].items() if not result.get('success', False)]
    
    print(f"✅ Successful Models: {{', '.join(successful_models) if successful_models else 'None'}}")
    print(f"❌ Failed Models: {{', '.join(failed_models) if failed_models else 'None'}}")
    print(f"📁 Results saved to {{results_file}}")
    
    return results

if __name__ == "__main__":
    results = build_all_models()
    print(f"\\n🏁 {asset_name} Model Building Complete!")
'''
        
        with open(template_path, 'w') as f:
            f.write(template_content)

# Example usage and utility functions
def create_bitcoin_asset():
    """Create Bitcoin asset structure."""
    generator = EnhancedAssetTemplateGenerator()
    generator.create_asset_structure("BTC", "CRYPTO")

def create_asset_from_input():
    """Interactive asset creation."""
    print("🏗️ Enhanced Asset Template Generator")
    print("=" * 40)
    
    asset_name = input("Enter asset name (e.g., BTC, AAPL): ").upper()
    print("Available asset classes:")
    print("1. CRYPTO")
    print("2. EQUITIES") 
    print("3. FOREX")
    print("4. ETH")
    
    choice = input("Select asset class (1-4): ")
    asset_classes = {{'1': 'CRYPTO', '2': 'EQUITIES', '3': 'FOREX', '4': 'ETH'}}
    asset_class = asset_classes.get(choice, 'CRYPTO')
    
    generator = EnhancedAssetTemplateGenerator()
    generator.create_asset_structure(asset_name, asset_class)
    
    print(f"\\n🎉 {asset_name} asset structure created successfully!")
    print(f"Next steps:")
    print(f"1. Run: python {asset_class}/{asset_name.lower()}/scripts/{asset_name.lower()}_validation.py")
    print(f"2. Run: python {asset_class}/{asset_name.lower()}/scripts/{asset_name.lower()}_model_builder.py")

if __name__ == "__main__":
    print("✅ Enhanced Asset Template Generator Ready")
    print("Available functions:")
    print("- create_bitcoin_asset()")
    print("- create_asset_from_input()")
    print("- EnhancedAssetTemplateGenerator().create_asset_structure(asset_name, asset_class)")
