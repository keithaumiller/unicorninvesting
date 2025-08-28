# Recommendation Systems

Core machine learning algorithms combining Genetic Algorithms and Neural Networks for portfolio optimization.

## Purpose
- Genetic Algorithm (GA) optimization for feature selection and neural network hyperparameters
- Neural network training using FCNN4R (Fully Connected Neural Networks for R)
- Portfolio performance evaluation and backtesting
- Automated model selection and optimization

## Files

### GA_parameter_explorer.R
**Purpose**: Genetic Algorithm implementation for neural network optimization
**Key Functions**:
- `launchaGAportfolio(userid, portfolioid, outputdirectory)` - Main GA portfolio optimization entry point
- `fitnesfunction(x)` - GA fitness function that evaluates neural network performance
- `convertobjecttonetinputlist(XX)` - Converts GA binary chromosome to feature list
- `postFitness(theGA)` - Post-processing after each GA generation
- `plotmyNN(myfilesavelocation, performance)` - Plots neural network performance charts
- `updateportfolioGAplot(obj)` - Updates GA convergence plots
- `monitor(obj)` - GA generation monitoring and progress tracking

**GA Configuration**:
- Population size: 300 individuals
- Max iterations: 100 generations  
- Binary encoding for feature selection
- Mutation rate: 0.2
- Elite preservation and suggestion seeding
- Fitness based on portfolio performance over 252 trading days

**Optimization Process**:
1. GA generates binary feature selection vectors
2. Each chromosome represents which features to include in neural network
3. Neural network trained with selected features
4. Portfolio performance evaluated as fitness score
5. GA evolves toward better feature combinations
6. Best performing models saved for live trading

### 1_modeltrainer_FCNN4R.R
**Purpose**: Neural network training implementation using FCNN4R package
**Key Functions**:
- `trainmodel(runid, numberofstockstouse, minibatchszparam, lambdaparam, gammaparam, momentumparam, epocsparam, netdepthparam, layer2param, layer3param, layer4param, tol_levelparam, learn_rateparam, l2regparam)` - Main neural network training function
- `obj_func(net)` - Objective function for simulated annealing optimization
- `reportaction(net)` - Progress reporting during training

**Network Architecture**:
- Configurable depth (3-5 layers)
- Input layer size based on number of features
- Hidden layers with configurable neuron counts
- Output layer size matches portfolio size
- Activation functions: configurable for hidden and output layers

**Training Algorithms**:
- **BP**: Backpropagation with configurable learning rate and L2 regularization
- **SGD**: Stochastic Gradient Descent with mini-batch support
- **SA**: Simulated Annealing for global optimization

**Performance Evaluation**:
- Uses `modelperformance()` function to evaluate trained networks
- Portfolio simulation over historical data (252 trading days)
- Returns portfolio value change from $1000 initial investment

### modelperformance.R
**Purpose**: Portfolio performance evaluation and backtesting framework
**Key Functions**:
- `modelperformance(mlpeval_eval, adjustedinput, saveit)` - Main performance evaluation function
- `forexperformance(mlpeval_eval, adjustedinput, saveit)` - Specialized forex portfolio evaluation
- `generatetrainingmatrix(trainingmatrix)` - Legacy training target generation
- `convertNNoutputtoallocation(NNoutput)` - Converts neural network output to portfolio allocations

**Portfolio Simulation**:
- Starts with $1000 seed money
- Daily rebalancing based on neural network allocations
- Tracks running portfolio value over time period
- Normalizes allocations to sum to 100%
- Calculates daily returns based on actual market performance

**Performance Metrics**:
- Final portfolio value after simulation period
- Daily portfolio value progression (`NNperformancechart`)
- Risk-adjusted returns for forex portfolios
- Handles both stock and forex portfolio types

**Backtesting Framework**:
- Uses last 252 trading days (1 year) for evaluation
- Temporal train/test split to avoid look-ahead bias
- Portfolio weight calculation: `(allocation[day-1] * price_change[day])`
- Compounds returns over simulation period

### Fitnessfunction.R
**Purpose**: Alternative fitness calculation approaches (currently unused)
**Key Functions**:
- `fitnesscalc(todayschanges, outputnodes)` - Alternative fitness calculation method
- Portfolio performance attribution framework
- Stock-level contribution analysis

**Notes**: 
- This file contains experimental fitness functions not currently in use
- Main fitness evaluation handled by `modelperformance.R`
- Contains references to portfolio analysis packages and methodologies

## System Integration

**Data Flow**:
1. `GA_parameter_explorer.R` generates feature combinations
2. `1_modeltrainer_FCNN4R.R` trains neural networks with selected features
3. `modelperformance.R` evaluates trained networks through portfolio simulation
4. Performance feedback guides GA evolution toward better solutions

**Output Files**:
- Best neural networks saved to `/portfoliosbest/bestnetfile`
- GA state saved for resumption between runs
- Performance plots and charts for visualization
- Training results logged to CSV files

**Dependencies**:
- FCNN4R package for neural network implementation
- GA package for genetic algorithm optimization
- quantmod for financial data handling
- RMySQL for database connectivity
