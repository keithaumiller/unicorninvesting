# Alpha Models Architecture - Methodology-First Design

## 🎯 **Architecture Philosophy**

This document defines the optimal methodology-first architecture for managing multiple assets and multiple methodologies in the Unicorn Investing alpha models framework. The design prioritizes methodology reusability, asset scalability, and maintainability through abstraction and adapter patterns.

**Design Principle**: **Methodology Abstraction with Asset Specialization**

## 🏗️ **Core Architecture Overview**

### **Methodology-First Structure**

```
/BackendPython/unicorn/2_alpha_models/
├── methodologies/                    # METHODOLOGY-FIRST approach
│   ├── prophet/                      # Prophet methodology framework
│   │   ├── core/                     # Core Prophet implementation
│   │   │   ├── __init__.py
│   │   │   ├── prophet_methodology.py    # Main methodology class
│   │   │   ├── feature_engineering.py   # Prophet-specific features
│   │   │   └── validation.py             # Prophet validation logic
│   │   ├── adapters/                 # Asset-specific adapters
│   │   │   ├── __init__.py
│   │   │   ├── crypto_adapter.py         # Crypto-specific Prophet logic
│   │   │   ├── forex_adapter.py          # Forex-specific Prophet logic
│   │   │   └── equity_adapter.py         # Equity-specific Prophet logic
│   │   ├── configs/                  # Methodology configurations
│   │   │   ├── default_config.json       # Base Prophet parameters
│   │   │   ├── crypto_overrides.json     # Crypto-specific overrides
│   │   │   └── forex_overrides.json      # Forex-specific overrides
│   │   └── models/                   # Trained Prophet model storage
│   │       ├── crypto/                   # Crypto Prophet models
│   │       ├── forex/                    # Forex Prophet models
│   │       └── metadata/                 # Model metadata
│   ├── xgboost/                      # XGBoost methodology framework
│   │   ├── core/                     # Core XGBoost implementation
│   │   │   ├── __init__.py
│   │   │   ├── xgboost_methodology.py   # Main methodology class
│   │   │   ├── feature_engineering.py   # XGBoost-specific features
│   │   │   └── validation.py             # XGBoost validation logic
│   │   ├── adapters/                 # Asset-specific adapters
│   │   │   ├── __init__.py
│   │   │   ├── crypto_adapter.py         # Crypto-specific XGBoost logic
│   │   │   ├── forex_adapter.py          # Forex-specific XGBoost logic
│   │   │   └── equity_adapter.py         # Equity-specific XGBoost logic
│   │   ├── configs/                  # Methodology configurations
│   │   │   ├── default_config.json       # Base XGBoost parameters
│   │   │   ├── crypto_overrides.json     # Crypto-specific overrides
│   │   │   └── forex_overrides.json      # Forex-specific overrides
│   │   └── models/                   # Trained XGBoost model storage
│   │       ├── crypto/                   # Crypto XGBoost models
│   │       ├── forex/                    # Forex XGBoost models
│   │       └── metadata/                 # Model metadata
│   ├── ensemble/                     # Ensemble methodology framework
│   │   ├── core/                     # Core ensemble logic
│   │   │   ├── __init__.py
│   │   │   ├── ensemble_methodology.py  # Main ensemble class
│   │   │   ├── combination_strategies.py # Ensemble combination logic
│   │   │   └── validation.py             # Ensemble validation logic
│   │   ├── adapters/                 # Asset-specific ensemble adapters
│   │   │   ├── __init__.py
│   │   │   ├── crypto_adapter.py         # Crypto-specific ensemble logic
│   │   │   ├── forex_adapter.py          # Forex-specific ensemble logic
│   │   │   └── equity_adapter.py         # Equity-specific ensemble logic
│   │   ├── configs/                  # Ensemble configurations
│   │   │   ├── default_config.json       # Base ensemble parameters
│   │   │   ├── crypto_overrides.json     # Crypto-specific combinations
│   │   │   └── forex_overrides.json      # Forex-specific combinations
│   │   └── models/                   # Ensemble model storage
│   │       ├── crypto/                   # Crypto ensemble models
│   │       ├── forex/                    # Forex ensemble models
│   │       └── metadata/                 # Ensemble metadata
│   └── lstm/                         # Future: LSTM methodology
│       ├── core/
│       ├── adapters/
│       ├── configs/
│       └── models/
├── assets/                           # ASSET ADAPTERS
│   ├── __init__.py
│   ├── base_adapter.py               # Abstract asset adapter base class
│   ├── crypto/                       # Cryptocurrency adapter
│   │   ├── __init__.py
│   │   ├── crypto_adapter.py             # Main crypto adapter
│   │   ├── eth_adapter.py                # ETH-specific logic
│   │   ├── btc_adapter.py                # BTC-specific logic
│   │   └── market_characteristics.py    # Crypto market behavior
│   ├── forex/                        # Forex adapter
│   │   ├── __init__.py
│   │   ├── forex_adapter.py              # Main forex adapter
│   │   ├── major_pairs_adapter.py        # Major pairs logic
│   │   ├── minor_pairs_adapter.py        # Minor pairs logic
│   │   └── market_characteristics.py    # Forex market behavior
│   ├── equities/                     # Equity adapter
│   │   ├── __init__.py
│   │   ├── equity_adapter.py             # Main equity adapter
│   │   ├── individual_stocks.py          # Individual stock logic
│   │   ├── sectors.py                    # Sector-based logic
│   │   └── market_characteristics.py    # Equity market behavior
│   └── commodities/                  # Future: Commodities adapter
│       ├── __init__.py
│       ├── commodity_adapter.py
│       └── market_characteristics.py
├── core/                             # SHARED FRAMEWORK
│   ├── __init__.py
│   ├── interfaces/                   # Abstract base classes
│   │   ├── __init__.py
│   │   ├── methodology_interface.py      # AlphaMethodology ABC
│   │   ├── asset_adapter_interface.py   # AssetAdapter ABC
│   │   ├── model_interface.py            # TrainedModel interface
│   │   └── data_interfaces.py            # Data structure interfaces
│   ├── data_pipeline/                # Data processing pipeline
│   │   ├── __init__.py
│   │   ├── data_loader.py                # Unified data loading
│   │   ├── data_normalizer.py            # Cross-asset normalization
│   │   └── feature_pipeline.py          # Feature engineering pipeline
│   ├── validation/                   # Model validation framework
│   │   ├── __init__.py
│   │   ├── performance_metrics.py        # Standardized metrics
│   │   ├── cross_validation.py           # Time-series CV
│   │   └── backtesting.py                # Backtesting framework
│   ├── orchestration/                # Multi-methodology coordination
│   │   ├── __init__.py
│   │   ├── model_orchestrator.py         # Main orchestration engine
│   │   ├── training_coordinator.py       # Training coordination
│   │   └── forecast_coordinator.py       # Forecast coordination
│   └── configuration/                # Configuration management
│       ├── __init__.py
│       ├── config_manager.py             # Configuration loading/merging
│       ├── methodology_config.py         # Methodology-specific configs
│       └── asset_config.py               # Asset-specific configs
└── storage/                          # UNIFIED MODEL STORAGE
    ├── __init__.py
    ├── metadata/                     # Model metadata database
    │   ├── model_registry.py             # Model registration system
    │   ├── performance_tracker.py        # Performance tracking
    │   └── model_metadata.db             # SQLite metadata database
    ├── performance/                  # Performance tracking
    │   ├── performance_database.py       # Performance data management
    │   ├── benchmark_tracker.py          # Benchmarking system
    │   └── model_comparison.py           # Cross-model comparison
    └── artifacts/                    # Model artifacts by methodology
        ├── prophet/                      # Prophet model files
        ├── xgboost/                      # XGBoost model files
        ├── ensemble/                     # Ensemble model files
        └── metadata/                     # Artifact metadata
```

## 🔧 **Core Framework Components**

### **1. Methodology Interface (Abstract Base Class)**

```python
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional
from core.interfaces.data_interfaces import AssetData, FeatureSet, ForecastResult, PerformanceMetrics
from core.configuration.methodology_config import MethodologyConfig

class AlphaMethodology(ABC):
    """Abstract base class for all alpha methodologies"""
    
    def __init__(self, methodology_name: str):
        self.methodology_name = methodology_name
        self.version = "1.0.0"
    
    @abstractmethod
    def prepare_features(self, asset_data: AssetData, asset_adapter: 'AssetAdapter') -> FeatureSet:
        """Methodology-specific feature preparation with asset adaptation"""
        pass
    
    @abstractmethod
    def train_model(self, features: FeatureSet, config: MethodologyConfig) -> 'TrainedModel':
        """Methodology-specific training logic"""
        pass
    
    @abstractmethod
    def generate_forecast(self, model: 'TrainedModel', current_data: AssetData) -> ForecastResult:
        """Methodology-specific forecasting"""
        pass
    
    @abstractmethod
    def validate_performance(self, model: 'TrainedModel', test_data: AssetData) -> PerformanceMetrics:
        """Methodology-specific validation"""
        pass
    
    @abstractmethod
    def get_feature_importance(self, model: 'TrainedModel') -> Dict[str, float]:
        """Get feature importance for this methodology"""
        pass
```

### **2. Asset Adapter Interface (Normalization Layer)**

```python
from abc import ABC, abstractmethod
from typing import Dict, Any, List
from core.interfaces.data_interfaces import RawAssetData, StandardizedData, MarketCharacteristics, ConstrainedForecast

class AssetAdapter(ABC):
    """Abstract adapter for different asset classes"""
    
    def __init__(self, asset_class: str, symbol: str):
        self.asset_class = asset_class
        self.symbol = symbol
        self.market_characteristics = self.get_market_characteristics()
    
    @abstractmethod
    def normalize_time_series(self, raw_data: RawAssetData) -> StandardizedData:
        """Convert asset-specific data to standardized format"""
        pass
    
    @abstractmethod
    def get_market_characteristics(self) -> MarketCharacteristics:
        """Return asset-specific market behavior patterns"""
        pass
    
    @abstractmethod
    def apply_asset_constraints(self, forecast: ForecastResult) -> ConstrainedForecast:
        """Apply asset-specific trading constraints"""
        pass
    
    @abstractmethod
    def get_feature_engineering_params(self, methodology: str) -> Dict[str, Any]:
        """Get methodology-specific feature engineering parameters for this asset"""
        pass
    
    @abstractmethod
    def validate_data_quality(self, data: StandardizedData) -> Dict[str, Any]:
        """Validate data quality for this asset class"""
        pass
```

### **3. Configuration Management System**

```python
from typing import Dict, Any, Optional
import json
from pathlib import Path

class MethodologyConfig:
    """Unified configuration for methodologies with asset-specific overrides"""
    
    def __init__(self, methodology: str, asset_class: str, symbol: str):
        self.methodology = methodology
        self.asset_class = asset_class
        self.symbol = symbol
        
        # Load configurations in priority order
        self.base_config = self._load_base_config(methodology)
        self.asset_class_overrides = self._load_asset_class_overrides(methodology, asset_class)
        self.symbol_overrides = self._load_symbol_overrides(methodology, asset_class, symbol)
        
        # Merge configurations
        self.merged_config = self._merge_configurations()
    
    def _load_base_config(self, methodology: str) -> Dict[str, Any]:
        """Load base methodology configuration"""
        config_path = Path(f"methodologies/{methodology}/configs/default_config.json")
        if config_path.exists():
            with open(config_path, 'r') as f:
                return json.load(f)
        return {}
    
    def _load_asset_class_overrides(self, methodology: str, asset_class: str) -> Dict[str, Any]:
        """Load asset class specific overrides"""
        config_path = Path(f"methodologies/{methodology}/configs/{asset_class.lower()}_overrides.json")
        if config_path.exists():
            with open(config_path, 'r') as f:
                return json.load(f)
        return {}
    
    def _load_symbol_overrides(self, methodology: str, asset_class: str, symbol: str) -> Dict[str, Any]:
        """Load symbol-specific overrides"""
        config_path = Path(f"methodologies/{methodology}/configs/{asset_class.lower()}_{symbol.lower()}_overrides.json")
        if config_path.exists():
            with open(config_path, 'r') as f:
                return json.load(f)
        return {}
    
    def _merge_configurations(self) -> Dict[str, Any]:
        """Merge configurations with priority: symbol > asset_class > base"""
        merged = self.base_config.copy()
        
        # Apply asset class overrides
        for key, value in self.asset_class_overrides.items():
            if isinstance(value, dict) and key in merged and isinstance(merged[key], dict):
                merged[key].update(value)
            else:
                merged[key] = value
        
        # Apply symbol overrides
        for key, value in self.symbol_overrides.items():
            if isinstance(value, dict) and key in merged and isinstance(merged[key], dict):
                merged[key].update(value)
            else:
                merged[key] = value
        
        return merged
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get configuration value"""
        return self.merged_config.get(key, default)
    
    def get_methodology_params(self) -> Dict[str, Any]:
        """Get methodology-specific parameters"""
        return self.merged_config.get('methodology_params', {})
    
    def get_training_params(self) -> Dict[str, Any]:
        """Get training-specific parameters"""
        return self.merged_config.get('training_params', {})
    
    def get_validation_params(self) -> Dict[str, Any]:
        """Get validation-specific parameters"""
        return self.merged_config.get('validation_params', {})
```

### **4. Multi-Methodology Orchestration Engine**

```python
from typing import Dict, List, Optional, Any
from core.interfaces.methodology_interface import AlphaMethodology
from assets.base_adapter import AssetAdapter
from storage.metadata.model_registry import ModelRegistry
from storage.performance.performance_tracker import PerformanceTracker
from core.configuration.methodology_config import MethodologyConfig

class AlphaModelOrchestrator:
    """Orchestrates multiple methodologies across multiple assets"""
    
    def __init__(self):
        self.methodologies: Dict[str, AlphaMethodology] = {}
        self.asset_adapters: Dict[str, AssetAdapter] = {}
        self.model_registry = ModelRegistry()
        self.performance_tracker = PerformanceTracker()
    
    def register_methodology(self, name: str, methodology: AlphaMethodology):
        """Register a new methodology implementation"""
        self.methodologies[name] = methodology
        print(f"✅ Registered methodology: {name}")
    
    def register_asset_adapter(self, asset_class: str, symbol: str, adapter: AssetAdapter):
        """Register an asset adapter"""
        key = f"{asset_class}_{symbol}"
        self.asset_adapters[key] = adapter
        print(f"✅ Registered asset adapter: {key}")
    
    def train_asset_models(self, asset_class: str, symbol: str, 
                          methodologies: Optional[List[str]] = None) -> Dict[str, Any]:
        """Train models for a specific asset using specified methodologies"""
        
        if methodologies is None:
            methodologies = list(self.methodologies.keys())
        
        # Get asset adapter and data
        adapter_key = f"{asset_class}_{symbol}"
        if adapter_key not in self.asset_adapters:
            raise ValueError(f"No asset adapter registered for {asset_class}_{symbol}")
        
        adapter = self.asset_adapters[adapter_key]
        asset_data = self._load_asset_data(asset_class, symbol)
        
        trained_models = {}
        
        for methodology_name in methodologies:
            if methodology_name not in self.methodologies:
                print(f"⚠️ Methodology {methodology_name} not registered, skipping")
                continue
                
            try:
                # Get methodology and configuration
                methodology = self.methodologies[methodology_name]
                config = MethodologyConfig(methodology_name, asset_class, symbol)
                
                print(f"🔄 Training {methodology_name} model for {asset_class}_{symbol}")
                
                # Prepare features using asset adapter
                features = methodology.prepare_features(asset_data, adapter)
                
                # Train model
                model = methodology.train_model(features, config)
                
                # Validate performance
                performance = methodology.validate_performance(model, asset_data)
                
                # Store model and metadata
                model_id = self.model_registry.store_model(
                    model=model,
                    methodology=methodology_name,
                    asset_class=asset_class,
                    symbol=symbol,
                    performance=performance,
                    config=config.merged_config
                )
                
                trained_models[methodology_name] = {
                    'model_id': model_id,
                    'model': model,
                    'performance': performance,
                    'config': config.merged_config
                }
                
                print(f"✅ Successfully trained {methodology_name} for {asset_class}_{symbol}")
                
            except Exception as e:
                print(f"❌ Failed to train {methodology_name} for {asset_class}_{symbol}: {e}")
                trained_models[methodology_name] = {
                    'error': str(e),
                    'status': 'failed'
                }
        
        return trained_models
    
    def generate_ensemble_forecast(self, asset_class: str, symbol: str) -> Dict[str, Any]:
        """Generate ensemble forecast combining all methodologies"""
        
        # Load trained models for this asset
        models = self.model_registry.get_models(asset_class, symbol)
        
        if not models:
            raise ValueError(f"No trained models found for {asset_class}_{symbol}")
        
        # Generate forecasts from each methodology
        individual_forecasts = {}
        adapter_key = f"{asset_class}_{symbol}"
        adapter = self.asset_adapters[adapter_key]
        current_data = self._load_current_data(asset_class, symbol)
        
        for methodology_name, model_info in models.items():
            if methodology_name in self.methodologies:
                try:
                    methodology = self.methodologies[methodology_name]
                    forecast = methodology.generate_forecast(model_info['model'], current_data)
                    individual_forecasts[methodology_name] = forecast
                except Exception as e:
                    print(f"⚠️ Failed to generate {methodology_name} forecast: {e}")
        
        # Combine forecasts using ensemble methodology
        if 'ensemble' in self.methodologies and len(individual_forecasts) > 1:
            ensemble_methodology = self.methodologies['ensemble']
            ensemble_forecast = ensemble_methodology.combine_forecasts(
                individual_forecasts, adapter
            )
            return ensemble_forecast
        else:
            # Return best performing individual forecast
            return self._select_best_forecast(individual_forecasts, asset_class, symbol)
    
    def _load_asset_data(self, asset_class: str, symbol: str) -> 'AssetData':
        """Load asset data from silver layer"""
        # Implementation to load from silver layer data sources
        pass
    
    def _load_current_data(self, asset_class: str, symbol: str) -> 'AssetData':
        """Load current data for forecasting"""
        # Implementation to load latest data
        pass
    
    def _select_best_forecast(self, forecasts: Dict[str, Any], 
                            asset_class: str, symbol: str) -> Dict[str, Any]:
        """Select best performing forecast when ensemble is not available"""
        # Implementation to select based on historical performance
        pass
```

## 🚀 **Implementation Advantages**

### **1. Methodology Reusability**
- **Single Implementation**: Each methodology implemented once, works across all assets
- **Reduced Duplication**: ~60% reduction in methodology-specific code
- **Faster Development**: 3-5x faster implementation of new assets or methodologies

### **2. Asset Scalability**
- **New Assets**: Add by implementing asset adapters only
- **Market Characteristics**: Asset-specific behavior captured in adapters
- **Trading Constraints**: Asset-specific limits applied through adapters

### **3. Configuration Flexibility**
- **Hierarchical Overrides**: Base → Asset Class → Symbol specific parameters
- **Methodology Isolation**: Each methodology manages its own configurations
- **Easy Tuning**: Parameter optimization per asset without code changes

### **4. Performance & Maintainability**
- **Clear Separation**: Methodology logic separated from asset concerns
- **Unified Interfaces**: Consistent patterns across all implementations
- **Parallel Processing**: Independent training across asset/methodology combinations
- **Comprehensive Testing**: Separate testing of methodologies and adapters

## 📋 **Implementation Roadmap**

### **Phase 1: Core Framework Foundation**
1. Implement abstract interfaces (`core/interfaces/`)
2. Create configuration management system (`core/configuration/`)
3. Build model registry and storage (`storage/`)
4. Implement data pipeline foundation (`core/data_pipeline/`)

### **Phase 2: Asset Adapters**
1. Implement crypto asset adapter with ETH/BTC support
2. Implement forex asset adapter for major pairs
3. Create equity adapter framework for future expansion
4. Validate adapter performance and data normalization

### **Phase 3: Methodology Migration**
1. Refactor existing Prophet implementation to new framework
2. Refactor existing XGBoost implementation to new framework
3. Implement ensemble methodology using new interfaces
4. Migrate existing trained models to new storage structure

### **Phase 4: Integration & Optimization**
1. Integrate with existing silver layer data pipeline
2. Implement orchestration engine
3. Performance optimization and caching
4. Comprehensive testing and validation framework

This methodology-first architecture provides the optimal foundation for managing multiple assets and methodologies with maximum reusability, scalability, and maintainability.
