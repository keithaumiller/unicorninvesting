# ETH Model Storage Structure

## Overview

This directory contains an organized, scalable model storage system for ETH algorithmic trading models. The structure is designed to handle multiple methodologies, versions, and variants while maintaining clean organization and easy retrieval.

## Directory Structure

```
model_storage/
├── prophet/                    # Facebook Prophet models
│   ├── v001_eth_prophet_basic_20250902_143025.pkl
│   ├── v002_eth_prophet_enhanced_20250902_143030.pkl
│   └── v003_eth_prophet_optimized_20250902_143035.pkl
├── xgboost/                   # XGBoost models
│   ├── v001_eth_xgboost_standard_20250902_143040.pkl
│   └── v002_eth_xgboost_tuned_20250902_143045.pkl
├── lstm/                      # LSTM neural networks
├── ensemble/                  # Ensemble models
├── arima/                     # ARIMA models
├── transformer/               # Transformer models
├── garch/                     # GARCH volatility models
└── model_metadata.db          # SQLite database for model metadata
```

## Naming Convention

Models follow a standardized naming pattern:
```
v{VERSION:03d}_{asset}_{methodology}_{variant}_{timestamp}.pkl
```

Examples:
- `v001_eth_prophet_basic_20250902_143025.pkl`
- `v002_eth_prophet_enhanced_20250902_143030.pkl`
- `v003_eth_xgboost_tuned_20250902_143045.pkl`

## Key Components

### 1. ModelStorageManager (`model_storage_manager.py`)

Central class for managing model storage and retrieval:

```python
from model_storage_manager import ModelStorageManager

# Initialize storage manager
storage = ModelStorageManager()

# Store a model
model_id = storage.store_model(
    model=trained_model,
    methodology='prophet',
    asset='ETH',
    model_config={...},
    performance_metrics={...},
    description="Enhanced Prophet model with regressors",
    variant="enhanced",
    tags=['production', 'validated']
)

# Load a model
model, metadata = storage.load_model(model_id)

# Load latest version
model, metadata = storage.load_latest_model('prophet', 'ETH')

# List models
models = storage.list_models(methodology='prophet', asset='ETH')
```

### 2. ETH Prophet Framework (`eth_prophet_organized.py`)

Enhanced Prophet framework with integrated storage:

```python
from eth_prophet_organized import ETHProphetFrameworkWithStorage

# Initialize framework
framework = ETHProphetFrameworkWithStorage()

# Train all three Prophet variants
model_ids = framework.train_all_variants(historical_data)

# Compare performance
comparison = framework.compare_model_performance()
```

## Model Variants

### Prophet Models
1. **Basic**: Standard Prophet configuration optimized for ETH
2. **Enhanced**: Includes external regressors (volume, volatility, momentum)
3. **Optimized**: Hyperparameter-tuned with multiple seasonalities

### XGBoost Models
1. **Standard**: Basic XGBoost with technical indicators
2. **Tuned**: Hyperparameter-optimized version

### Future Methodologies
- **LSTM**: Neural network for sequence modeling
- **Ensemble**: Combining multiple model predictions
- **ARIMA**: Traditional time series analysis
- **Transformer**: Attention-based models
- **GARCH**: Volatility modeling

## Metadata Database Schema

The `model_metadata.db` SQLite database stores comprehensive metadata:

```sql
CREATE TABLE model_metadata (
    model_id TEXT PRIMARY KEY,          -- Unique identifier
    methodology TEXT NOT NULL,          -- prophet, xgboost, etc.
    version INTEGER NOT NULL,           -- Version number
    asset TEXT NOT NULL,               -- ETH, BTC, etc.
    created_at TEXT NOT NULL,          -- ISO timestamp
    file_path TEXT NOT NULL,           -- Full path to model file
    file_size INTEGER NOT NULL,        -- File size in bytes
    model_config TEXT NOT NULL,        -- JSON config
    performance_metrics TEXT NOT NULL, -- JSON metrics
    description TEXT NOT NULL,         -- Human description
    tags TEXT NOT NULL                 -- JSON tag array
);
```

## Performance Tracking

Each model includes comprehensive performance metrics:

- **MAPE**: Mean Absolute Percentage Error
- **RMSE**: Root Mean Square Error
- **MAE**: Mean Absolute Error
- **Directional Accuracy**: Percentage of correct directional predictions
- **Sharpe Ratio**: Risk-adjusted returns (when applicable)
- **Maximum Drawdown**: Largest peak-to-trough decline

## Usage Examples

### Store a New Model

```python
from model_storage_manager import ModelStorageManager
import pickle

# Train your model
model = train_my_model(data)

# Initialize storage
storage = ModelStorageManager()

# Store with metadata
model_id = storage.store_model(
    model=model,
    methodology='xgboost',
    asset='ETH',
    model_config={
        'n_estimators': 100,
        'max_depth': 6,
        'learning_rate': 0.1
    },
    performance_metrics={
        'mape': 3.45,
        'rmse': 125.67,
        'directional_accuracy': 67.8
    },
    description="XGBoost model with technical indicators",
    variant="standard",
    tags=['production', 'backtested']
)

print(f"Model stored as: {model_id}")
```

### Load and Use a Model

```python
# Load specific model
model, metadata = storage.load_model('prophet_ETH_v001')

# Load latest Prophet model
model, metadata = storage.load_latest_model('prophet', 'ETH')

# Generate predictions
predictions = model.predict(future_data)

# View model information
print(f"Model: {metadata.description}")
print(f"Performance: MAPE {metadata.performance_metrics['mape']:.2f}%")
print(f"Created: {metadata.created_at}")
```

### Compare Model Performance

```python
# List all ETH Prophet models
prophet_models = storage.list_models(methodology='prophet', asset='ETH')

# Compare performance
for model_meta in prophet_models:
    metrics = model_meta.performance_metrics
    print(f"{model_meta.model_id}:")
    print(f"  MAPE: {metrics.get('mape', 'N/A')}%")
    print(f"  Directional Accuracy: {metrics.get('directional_accuracy', 'N/A')}%")
    print(f"  Created: {model_meta.created_at[:10]}")
```

### Storage Statistics

```python
# Get storage overview
stats = storage.get_storage_stats()
print(f"Total models: {stats['total_models']}")
print(f"Storage used: {stats['total_size_mb']:.1f} MB")

# Detailed summary
storage.print_storage_summary()
```

## Migration from Old Structure

To migrate existing models from the old flat structure:

```python
from model_storage_manager import migrate_existing_models

# Migrate all .pkl files from models/ directory
migrate_existing_models()
```

## Best Practices

### 1. Version Control
- Always increment versions when retraining with new data
- Use descriptive variant names (`basic`, `enhanced`, `optimized`)
- Include meaningful descriptions

### 2. Performance Tracking
- Store comprehensive metrics for comparison
- Include both accuracy and business metrics
- Track model degradation over time

### 3. Tagging Strategy
- Use tags for categorization: `['production', 'experimental', 'validated']`
- Include methodology-specific tags: `['high_volatility', 'trend_following']`
- Add temporal tags: `['q3_2025', 'bull_market']`

### 4. Storage Management
- Regular cleanup of underperforming models
- Archive old versions after validation
- Monitor storage usage and file sizes

### 5. Documentation
- Include detailed descriptions for each model
- Document preprocessing steps and feature engineering
- Maintain change logs for model updates

## Integration with LEAN Framework

This storage system integrates seamlessly with the LEAN algorithmic trading framework:

1. **Alpha Models**: Store trained models for signal generation
2. **Risk Management**: Load models for risk assessment
3. **Portfolio Construction**: Use ensemble models for allocation
4. **Backtesting**: Easy model comparison across historical periods

## Future Enhancements

Planned improvements:
- Model performance monitoring dashboard
- Automated retraining pipelines
- A/B testing framework for model comparison
- Integration with cloud storage (S3, Azure Blob)
- Model explainability and feature importance tracking
- Real-time performance monitoring and alerts

## File Organization

```
ETH/
├── model_storage/              # NEW: Organized model storage
│   ├── prophet/               # Prophet models by version
│   ├── xgboost/              # XGBoost models by version
│   ├── lstm/                 # Future: LSTM models
│   ├── ensemble/             # Future: Ensemble models
│   └── model_metadata.db     # Comprehensive metadata
├── model_storage_manager.py   # Storage management system
├── eth_prophet_organized.py   # Enhanced Prophet framework
├── models/                    # LEGACY: Old flat structure
├── algorithms/               # Algorithm implementations
├── features/                 # Feature engineering
└── tests/                   # Test suites
```

This structure provides a scalable foundation for managing machine learning models in production algorithmic trading systems.
