# Predictive Analytics

Model exploration, regression analysis, and neural network architecture experimentation.

## Purpose
- Neural network hyperparameter optimization and architecture search
- Regression analysis and time series forecasting
- Model performance evaluation and comparison
- Experimental model development and testing

## Files

### modelexploration.R
**Purpose**: Neural network architecture exploration and hyperparameter tuning
**Key Functions**:
- `modelexplorer(runid, featurelistforNN, outputdirectory)` - Main model exploration wrapper
- `trainmodel(runid, numberofstockstouse, minibatchsz, lambda, gamma, momentum, epochs, netdepth, layer2, layer3, layer4, tol_level, learn_rate, l2reg, outputdirectory)` - Neural network training with configurable parameters
- `combinestocksfunction(numberofstockstouse, featurelistforNN, outputdirectory)` - Dataset preparation

**Neural Network Parameters**:
- Network depth (3-5 layers supported)
- Layer sizes: configurable hidden layer neurons (layer2, layer3, layer4)
- Learning algorithms: SGD (Stochastic Gradient Descent), BP (Backpropagation), SA (Simulated Annealing)
- Optimization parameters: learning rate, L2 regularization, batch size, momentum
- Training parameters: epochs, tolerance level, report frequency

**Architecture Search**:
- Automated grid search over network architectures
- Layer size optimization (60-300 neurons per layer)
- Learning rate optimization (0.001-0.1 range)
- L2 regularization tuning (0.001-0.9 range)
- Feature count optimization (100-250 features)

**Dependencies**: 
- Sources from `recomendationsystems/1_modeltrainer_FCNN4R.R`
- Sources from `datasetcreation/Combinestocks.R`
- Sources from `recomendationsystems/modelperformance.R`

### regressionfuntions.R
**Purpose**: Time series forecasting and regression analysis
**Key Functions**:
- `regressionthists(thists)` - Time series regression using Holt-Winters methods
- Simple exponential smoothing (level only)
- Double exponential smoothing (level + trend)
- Triple exponential smoothing (level + trend + seasonal)

**Forecasting Methods**:
- Uses R's `forecast` package for time series analysis
- Holt-Winters exponential smoothing with configurable components
- Returns forecasted values for next trading day

**Dependencies**: forecast package for time series analysis

**Usage Notes**:
- `modelexploration.R` is the primary entry point for neural network experimentation
- Supports extensive hyperparameter grid search for optimization
- Integrates with genetic algorithm for automated feature selection
- Performance evaluation through portfolio simulation over historical data
