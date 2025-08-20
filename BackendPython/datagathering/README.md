# Data Gathering

Market data collection, ingestion, and database connectivity for real-time trading data.

## Purpose
- Download and process stock and forex market data
- Maintain database connections and data persistence
- Feature list management and portfolio configuration

## Files

### downloadstockdata.R
**Purpose**: Primary market data collection and database interface
**Key Functions**:
- `downloaddata(symbol)` - Downloads stock data using quantmod/getSymbols
- `downloadcurrency(symbol)` - Downloads forex data using getFX
- `pullstocklist(stocklist)` - Parallel download of multiple stocks using foreach
- `loadfeaturelist(userid, portfolioname, maxfeaturestouse)` - Loads feature list from database
- `loadportfoliolist(userid, portfolio)` - Loads portfolio composition from database
- `convertportfoliolisttodbformat(userid, portfolio, symbollist)` - Formats data for database insert

**Database Functions**:
- `load_from_unicorn_portfolios_table(userid, portfolioname)` - Retrieves portfolio symbols
- `insert_into_unicorn_portfolios_table(data)` - Saves portfolio configurations  
- `load_unicorn_best_featurelist(userid, portfolioname)` - Gets optimized feature lists
- `insert_into_unicorn_best_featurelist(userid, portfolioname, symbollist)` - Saves GA-optimized features
- `insert_into_unicorn_allocationhistory(userid, portfolio, allocationtable)` - Records daily allocations
- `load_unicorn_allocationhistory(userid, portfolio, recorddate)` - Retrieves allocation history
- `insert_into_unicorn_portfolios_details(userid, portfolio, values)` - Saves performance metrics
- `load_unicorn_portfolios_details(userid, portfolio, recorddate)` - Gets portfolio performance data
- `load_unicorn_useridlist()` - Gets all active users
- `load_unicorn_portfoliolist()` - Gets all portfolio IDs
- `load_unicorn_usersportfolios(userid)` - Gets portfolios for specific user
- `portfolioisforex(userid, portfolioid)` - Checks if portfolio is forex-based

**Dependencies**: quantmod, RMySQL, parallel, multicore, foreach, doParallel

### mysqlconnector.R
**Purpose**: Database connection setup and basic MySQL operations
**Key Functions**:
- Database connection setup using RMySQL
- Basic table read/write operations
- Connection string: MySQL server on ec2-54-85-232-216.compute-1.amazonaws.com

### data/ subdirectory
**Purpose**: Contains downloaded stock data organized by symbol in CSV format
**Structure**: `/data/stockdata/[SYMBOL]/stockdata.csv` for each downloaded security
