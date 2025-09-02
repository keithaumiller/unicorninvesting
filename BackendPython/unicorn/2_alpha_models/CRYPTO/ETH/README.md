# ETH Prophet Framework

A comprehensive framework for developing and comparing Prophet-based forecasting models for Ethereum (ETH) price prediction. This framework implements three distinct Prophet model variants with complete performance tracking and comparison capabilities.

## 🎯 Overview

The ETH Prophet Framework provides:
- **Three Prophet Model Variants**: Basic, Enhanced, and Optimized configurations
- **Comprehensive Performance Tracking**: 12+ metrics including MAPE, R², Sharpe ratio, directional accuracy
- **Database Storage**: SQLite database for historical experiments and model comparisons
- **Automated Deployment**: Complete deployment pipeline with monitoring setup
- **Visualization Tools**: Detailed charts and comparison reports

## 🏗️ Architecture

```
ETH Prophet Framework/
├── eth_prophet_framework.py     # Core framework implementation
├── test_prophet_framework.py    # Comprehensive testing suite
├── deploy_prophet_framework.py  # Deployment automation
├── prophet_config.py           # Configuration management
├── models/                     # Individual model implementations
│   ├── eth_prophet.py         # Base ETH Prophet model
│   ├── eth_prophet_model.pkl  # Trained model artifacts
│   └── production/            # Production-ready models
├── reports/                   # Generated reports and visualizations
└── eth_prophet_comparison.db  # Performance tracking database
```

## 🚀 Quick Start

### 1. Installation & Setup

```bash
# Clone the repository and navigate to ETH framework
cd BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/

# Install dependencies
pip install prophet yfinance matplotlib seaborn pandas numpy

# Run the deployment script
python deploy_prophet_framework.py
```

### 2. Basic Usage

```python
from eth_prophet_framework import ETHProphetFramework, create_sample_eth_data

# Create framework instance
framework = ETHProphetFramework()

# Load or create data
eth_data = create_sample_eth_data(500)  # 500 days of sample data

# Train all three models and compare
results = framework.train_all_models(eth_data, validation_split=0.2)

# Generate comparison report
print(framework.generate_comparison_report())
```

### 3. Using Real Data

```python
import yfinance as yf

# Download real ETH data
eth_data = yf.download('ETH-USD', start='2022-01-01', end='2024-01-01')
eth_data.columns = ['Open', 'High', 'Low', 'Close', 'Adj Close', 'Volume']
eth_data = eth_data.drop('Adj Close', axis=1)

# Train models on real data
results = framework.train_all_models(eth_data)
```

## 📊 Model Variants

### 1. Basic Prophet Model
- **Purpose**: Baseline forecasting with standard Prophet configuration
- **Configuration**: Additive seasonality, basic trend detection
- **Use Case**: Quick prototyping and baseline comparisons

### 2. Enhanced Prophet Model
- **Purpose**: Improved accuracy with external regressors
- **Features**: Volume indicators, volatility measures, crypto holidays
- **Configuration**: Multiplicative seasonality, external regressors
- **Use Case**: Production deployments requiring moderate complexity

### 3. Optimized Prophet Model
- **Purpose**: Maximum performance with hyperparameter tuning
- **Features**: Custom seasonalities, advanced regressors, optimized parameters
- **Configuration**: Fine-tuned for ETH-specific patterns
- **Use Case**: High-stakes trading applications

## 📈 Performance Metrics

The framework tracks comprehensive performance metrics:

### Accuracy Metrics
- **MAPE** (Mean Absolute Percentage Error)
- **MAE** (Mean Absolute Error)
- **RMSE** (Root Mean Square Error)
- **R²** (Coefficient of Determination)

### Trading Performance
- **Directional Accuracy**: Percentage of correct direction predictions
- **Sharpe Ratio**: Risk-adjusted returns
- **Maximum Drawdown**: Largest peak-to-trough decline
- **Volatility**: Annualized price volatility

### Model Quality
- **Information Ratio**: Excess return per unit of tracking error
- **Calmar Ratio**: Return relative to maximum drawdown

## 🗄️ Database Schema

### Models Table
```sql
CREATE TABLE models (
    model_id TEXT PRIMARY KEY,
    asset_name TEXT NOT NULL,
    model_type TEXT NOT NULL,
    model_version TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    stage TEXT NOT NULL
);
```

### Model Comparisons Table
```sql
CREATE TABLE model_comparisons (
    experiment_id TEXT NOT NULL,
    model_variant TEXT NOT NULL,
    validation_mape REAL,
    validation_r2 REAL,
    directional_accuracy REAL,
    config TEXT
);
```

## 🔧 Configuration

### Model Configuration Example
```python
config = {
    'seasonality_mode': 'multiplicative',
    'yearly_seasonality': True,
    'weekly_seasonality': True,
    'changepoint_prior_scale': 0.15,
    'seasonality_prior_scale': 20.0,
    'interval_width': 0.85
}

model = ETHProphetModel(config)
```

### Feature Engineering
The framework includes advanced feature engineering:
- Technical indicators (SMA, EMA, RSI, MACD)
- Volatility measures (rolling std, GARCH)
- Momentum indicators (rate of change, price momentum)
- Volume indicators (OBV, volume ratios)

## 📊 Visualization & Reporting

### Automated Reports
- **Comparison Reports**: Side-by-side model performance
- **Detailed Analysis**: In-depth metrics and recommendations
- **Visual Charts**: Prediction plots, error distributions, heatmaps

### Example Visualization
```python
from test_prophet_framework import plot_model_predictions

# Generate comparison plots
plot_model_predictions(framework, save_path='eth_comparison.png')
```

## 🚦 Production Deployment

### Deployment Pipeline
1. **Environment Setup**: Dependencies and directory structure
2. **Model Training**: All three variants with validation
3. **Performance Evaluation**: Comprehensive metrics calculation
4. **Best Model Selection**: Automated selection based on criteria
5. **Production Deployment**: Model serialization and configuration
6. **Monitoring Setup**: Performance tracking and alerting

### Monitoring
```python
# Production monitoring example
def monitor_model_performance():
    # Check model drift
    # Validate prediction accuracy
    # Trigger retraining if needed
    pass
```

## 📋 Example Results

### Sample Performance Comparison
```
ETH Prophet Models Comparison Report
====================================
Experiment ID: eth_prophet_exp_20241220_143052
Best Model: optimized

BASIC MODEL:
  MAPE: 8.45%
  R²: 0.6234
  Directional Accuracy: 62.3%
  Sharpe Ratio: 1.23

ENHANCED MODEL:
  MAPE: 6.78%
  R²: 0.7456
  Directional Accuracy: 67.8%
  Sharpe Ratio: 1.45

OPTIMIZED MODEL:
  MAPE: 5.23%
  R²: 0.8012
  Directional Accuracy: 71.2%
  Sharpe Ratio: 1.67
```

## 🛠️ Advanced Usage

### Custom Model Configuration
```python
# Create custom Prophet model
custom_config = {
    'seasonality_mode': 'multiplicative',
    'changepoint_prior_scale': 0.2,
    'seasonality_prior_scale': 25.0,
    'custom_seasonalities': [
        {
            'name': 'crypto_quarterly',
            'period': 91.25,
            'fourier_order': 8
        }
    ]
}

custom_model = ETHProphetModel(custom_config)
```

### Batch Processing
```python
# Process multiple experiments
experiments = []
for i in range(5):
    results = framework.train_all_models(data, validation_split=0.2)
    experiments.append(results)

# Analyze experiment stability
stability_metrics = analyze_experiment_stability(experiments)
```

## 🔍 Troubleshooting

### Common Issues

1. **Prophet Installation**: 
   ```bash
   pip install prophet
   # If issues with dependencies:
   conda install -c conda-forge prophet
   ```

2. **Data Format**: Ensure data has required columns:
   ```python
   required_columns = ['Open', 'High', 'Low', 'Close', 'Volume']
   ```

3. **Memory Issues**: For large datasets, consider:
   ```python
   # Reduce uncertainty samples
   config['uncertainty_samples'] = 500
   ```

### Performance Optimization
- Use smaller validation splits for faster training
- Disable daily seasonality for monthly+ data
- Reduce Fourier terms for custom seasonalities

## 📚 API Reference

### ETHProphetFramework Class
```python
class ETHProphetFramework:
    def __init__(self, data_path: Optional[str] = None)
    def train_all_models(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]
    def generate_comparison_report(self) -> str
    def get_historical_experiments(self) -> pd.DataFrame
```

### ETHProphetModel Class
```python
class ETHProphetModel(ProphetModel):
    def __init__(self, config: Optional[Dict[str, Any]] = None)
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]
    def predict(self, data: pd.DataFrame, periods: int) -> pd.DataFrame
```

## 🤝 Contributing

1. Fork the repository
2. Create feature branch (`git checkout -b feature/new-model`)
3. Commit changes (`git commit -am 'Add new model variant'`)
4. Push to branch (`git push origin feature/new-model`)
5. Create Pull Request

## 📄 License

This project is part of the Unicorn Investing platform. See main repository for license details.

## 🆘 Support

For issues and questions:
- Create GitHub issue with detailed description
- Include sample data and error messages
- Specify environment details (OS, Python version, Prophet version)

---

**🚀 Ready to predict ETH prices with confidence!**
