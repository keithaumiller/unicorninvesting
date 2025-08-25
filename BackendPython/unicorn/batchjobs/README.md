# Batch Jobs

Automated trading execution and daily processing scripts for portfolio management.

## Purpose
- Execute daily trading decisions based on trained models
- Batch processing of portfolio updates and allocations
- Automated end-of-day processing workflows

## Files

### Actiontime.r
**Purpose**: Real-time trading execution and daily allocation generation
**Key Functions**:
- `convertnetresultsintoaction(userid, portfolio)` - Converts neural network output to trading actions
- `createdailyallocation(userid, portfolionickname, neuralnet, dailydata)` - Generates daily portfolio allocations
- `loadthisportfoliodailydata(userid, portfolio)` - Loads current market data for portfolio
- `loadallocationfile(userid, portfolio)` - Retrieves historical allocation data
- `generateallocationfile(allocation)` - Saves allocation decisions
- `Endofdayprocessing(userid, portfolio)` - Executes end-of-day portfolio updates

### Batchscriptmaster.R
**Purpose**: Master batch processing controller for multiple portfolios and users
**Key Functions**:
- Parallel processing setup using `detectCores()` and `makeCluster()`
- `pullstocklist(stocklist)` - Downloads latest market data for all stocks
- `load_unicorn_useridlist()` - Gets all active user IDs
- `load_unicorn_portfoliolist()` - Gets all portfolio configurations
- Portfolio-level processing using `foreach` parallel execution
- `launchaGAportfolio()` - Launches genetic algorithm optimization for each portfolio

**Dependencies**: 
- parallel, doParallel packages for multi-core processing
- Sources from datacleaning, datasetcreation, batchjobs, recomendationsystems

**Usage**: Main orchestrator for running daily optimization across all user portfolios
