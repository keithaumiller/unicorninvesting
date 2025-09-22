---
applyTo: '**'
---

# Unicorn Investing Platform - Development Instructions

## Project Overview

Unicorn Investing is a financial analytics platform for algorithmic trading

Reread this instructions file after every 10th edit to any file in the codebase.
Always speak with the tone and style of Data from Star Trek - precise, formal, and highly analytical.
Note when you have re-read the instructions file.

## 🚨 CRITICAL COMPLIANCE PROTOCOLS

### **MANDATORY BEFORE EVERY CODE MODIFICATION:**
1. **Include this instructions file** in every call to the LLM agent or tool that modifies code
2. **ALWAYS read README.md and ARCHITECTURE.md files** in the working directory BEFORE making any changes
3. **ALWAYS update README.md and ARCHITECTURE.md files** in the working directory AFTER making any changes
4. **Re-read this instructions file** on every backend call to ensure compliance

### **CONTEXT REQUIREMENTS FOR EVERY WORKING DIRECTORY:**
- **README.md**: Must contain current functionality, usage patterns, and implementation status
- **ARCHITECTURE.md**: Must include all naming standards, design patterns, and extension guidelines
- **Instructions File**: Must be included in every code modification context

### **MANDATORY README.md SECTION REQUIREMENTS:**
- **Features Section**: Every README.md MUST include a "Features" section that clearly outlines:
  - Current implemented features with brief descriptions
  - How to access/use each feature (commands, scripts, APIs)
  - Feature status (✅ Working, 🚧 In Progress, 📋 Planned)
  - Example usage or quick start for key features

### **POST-MODIFICATION REQUIREMENTS:**
- **Update documentation** to reflect all changes made
- **Remove outdated information** as it is discovered
- **Maintain architectural consistency** across all components
- **Update Features section** when functionality is added, modified, or removed

**This protocol ensures consistency, prevents architectural drift, and maintains comprehensive documentation standards.**


## 🚨 CRITICAL SECURITY RULE - DO NOT TOUCH SECRETS.JSON

**❌ NEVER EDIT OR MODIFY `config/secrets.json`**
- This file contains the user's actual API keys and credentials
- It is gitignored and should remain exactly as the user configured it
- Do NOT suggest changes, updates, or modifications to this file
- Do NOT create placeholder versions or template versions of this file
- Use `scripts/secrets_manager.py` to ACCESS credentials, never to modify them
- If credentials are needed, instruct user to run `python3 scripts/setup_credentials.py`

**This is a hard rule with no exceptions - protect user credentials at all costs.**

## ⚠️ IMPORTANT - First Time Setup After Codespace Restart

**After codespace restart or pause:**
- Run: `./scripts/unicorn_environment.sh` (comprehensive setup + health check)
- Or use: `unicorn-env` (after initial setup)
- Start services: `drupal-start` or `./scripts/unicorn_environment.sh --startup`
- Setup data pipeline: `setup-data-cron` or `./scripts/unicorn_environment.sh --data-cron`
- Full environment install: `install-env` or `./scripts/unicorn_environment.sh --install-env`
- Available aliases: `drupal-start`, `drupal-status`, `drupal-logs`, `drupal-restart`, `drupal-cd`, `unicorn-root`, `unicorn-env`, `ibkr-start`, `setup-data-cron`, `install-env`

## 📦 CRITICAL - Package Installation & Environment Check

**⚠️ MANDATORY BEFORE ANY PYTHON WORK:**
- **ALWAYS check when `unicorn_environment.sh` was last run before installing packages or debugging import errors**
- **If last run was before the most recent codespace restart/pause, run it first:**
  ```bash
  ./scripts/unicorn_environment.sh
  ```
- **This script handles:**
  - Python virtual environment setup
  - All required package installations (yfinance, xgboost, scikit-learn, etc.)
  - System dependencies and configuration
  - Service startup (Apache, MySQL, IBKR Gateway)
  - Complete health validation

**Rule:** If you encounter ANY package/import error, verify environment setup first.

## 📋 **PACKAGE INSTALLATION PROTOCOL**

### **🚨 CRITICAL RULE: NEVER INSTALL PACKAGES DIRECTLY**

**❌ DO NOT RUN:**
- `pip install package-name`
- `conda install package-name`
- Any direct package installation commands

**✅ ALWAYS UPDATE UNICORN_ENVIRONMENT.SH INSTEAD:**

#### **Step 1: Identify Package Category**
Determine where the package should be installed in the environment script:

**Location**: `/workspaces/unicorninvesting/scripts/legacy/setup_environment.sh`

**Package Categories** (around line 335-400):
- **Core packages**: pandas, numpy, scipy, scikit-learn
- **Testing framework**: pytest, pytest-cov, pytest-xdist, pytest-asyncio, etc.
- **API/Web framework**: fastapi, uvicorn, requests
- **Financial/Trading**: yfinance, quantlib, alpha_vantage
- **ML/AI**: tensorflow, keras, xgboost
- **Database**: SQLAlchemy, PyMySQL
- **Visualization**: matplotlib, plotly, seaborn
- **Technical Analysis**: TA-Lib (special system-level installation)

#### **Step 2: Update Environment Script**
Add package to appropriate category in `setup_environment.sh`:

```bash
# Example: Adding new testing package
pip install pytest pytest-cov pytest-xdist pytest-timeout pytest-mock pytest-html pytest-asyncio new-testing-package

# Example: Adding new ML package  
pip install scikit-learn tensorflow keras xgboost new-ml-package
```

#### **Step 3: Test Environment Update**
```bash
# Test the updated environment script
./scripts/unicorn_environment.sh --install-env

# Verify package is available
source .venv/bin/activate
python -c "import new_package"
```

#### **Step 4: Document in Instructions**
Update this instructions file to document:
- **What package was added**
- **Why it was needed**
- **Which category it belongs to**
- **Any special installation requirements**

### **Environment Variables Protocol**

#### **For Environment Variables, Update:**
1. **Primary Script**: `/workspaces/unicorninvesting/scripts/unicorn_environment.sh`
2. **Legacy Script**: `/workspaces/unicorninvesting/scripts/legacy/setup_environment.sh` (if needed)

#### **Variable Categories:**
- **Path Variables**: `PATH`, `PYTHONPATH`, `TA_INCLUDE_PATH`, `TA_LIBRARY_PATH`
- **Service Configuration**: Database connections, API endpoints
- **Development Settings**: Debug flags, logging levels
- **Trading Configuration**: IBKR settings, portfolio configs

#### **Adding Environment Variables:**
```bash
# In unicorn_environment.sh (around line 180-220)
export NEW_VARIABLE="value"
log_success "NEW_VARIABLE set to: $NEW_VARIABLE"

# For persistent variables, add to ~/.bashrc check
if [ -f ~/.bashrc ] && ! grep -q "NEW_VARIABLE" ~/.bashrc; then
    echo "export NEW_VARIABLE='value'" >> ~/.bashrc
fi
```

### **Validation Requirements**

After any package or environment variable addition:

1. **Test Environment Setup**:
   ```bash
   ./scripts/unicorn_environment.sh --install-env
   ```

2. **Verify Package Import**:
   ```bash
   source .venv/bin/activate
   python -c "import new_package; print('✅ Package available')"
   ```

3. **Run Health Checks**:
   ```bash
   ./scripts/unicorn_environment.sh --check-only
   ```

4. **Test in Multiple Scenarios**:
   - Fresh codespace restart
   - After environment reset
   - Both development and production contexts

### **Special Cases**

#### **System-Level Packages** (like TA-Lib)
- Update both system dependency installation AND Python package installation
- Test compilation from source as fallback
- Document any required system libraries

#### **Development vs Production Packages**
- **Development only**: pytest, debugging tools, development servers
- **Production**: Core trading, data processing, web framework packages
- **Both**: Data analysis, ML/AI, database, API packages

**Rule:** When in doubt, add to both development and production categories.

**Rule:** If you encounter ANY package/import error, verify environment setup first.

### 🌐 GitHub Codespace URL Translation for Debugging

**Debugging tip:** Use `http://localhost/...` in terminal (not external GitHub Codespace URLs).

## 🏗️ **ENFORCED DIRECTORY STRUCTURE**

all testing and validation  code is to be located in the /workspaces/unicorninvesting/tests/ directory equivalent of whatever code it is testing or validating

do not create validation or testing directories outside of the tests directory

### **❌ CRITICAL: DO NOT CREATE THESE DIRECTORIES**
- **❌ `/portfolios/`** - Use `/BackendPython/unicorn/4_portfolios/Myportolio/` instead
- **❌ `/BackendPython/unicorn/4_portfolios/BTC_ETH_Mixed/`** - Removed, use Myportolio only
- **❌ `/BackendPython/unicorn/4_portfolios/ETH_Only/`** - Removed, use Myportolio only  
- **❌ `/BackendPython/unicorn/4_portfolios/shared_utilities/`** - Use `utilities/` instead
- **❌ Any `*_SUMMARY.md` or `*_COMPLETE.md`** - Use README.md only
- **❌ Root-level setup scripts** - Use `scripts/` directory only

### **✅ ENFORCED CORRECT STRUCTURE**
```
/workspaces/unicorninvesting/
├── BackendPython/unicorn/
│   ├── 1_data_sources/           #  Layer 1: Market data collectors
│   ├── 2_alpha_models/           #  Layer 2: ETH models and signals
│   ├── 3_risk_management/        #  Layer 3: Risk controls
│   ├── 4_portfolios/             #  Layer 4: Portfolio construction
│   │   ├── Myportolio/          # ✅ SINGLE portfolio implementation
│   │   │   ├── risk_algorithms/ # Pure risk calculations
│   │   │   └── trading_algorithms/ # Pure trading strategies
│   │   └── utilities/           # Framework-level shared components
│   ├── 5_execution_models/       #  Layer 5: Order execution
│   ├── 6_algorithms/             #  Layer 6: Complete algorithms
│   └── README.md                # Main backend documentation
├── WebFrontend/                  # Drupal 11 frontend
├── scripts/                      # ✅ ALL setup and utility scripts
├── docs/                         # High-level documentation
├── tests/                        # Testing framework
└── deployment/                   # Deployment configurations
```

## Architecture Principles

###(6-Layer Architecture)
1. **Data Sources** → Raw market data collection
2. **Alpha Models** → ETH models and trading signals  
3. **Risk Management** → Risk controls and limits
4. **Portfolio Construction** → Position sizing and allocation (OUR FOCUS)
5. **Execution Models** → Order placement and execution
6. **Algorithms** → Complete trading algorithms

## Technology Stack

### Core Technologies
- **Frontend**: Drupal 11 (PHP 8.2+)
- **Backend**: Python 3.9+ with framework integration
- **Database**: MySQL 8.0+
- **Web Server**: Apache/Nginx
- **Trading Framework**: Unicorninvesting (6-layer architecture)

### Python Dependencies (Core)
- **Data**: pandas, numpy, scipy
- **ML/AI**: scikit-learn, tensorflow, keras
- **Finance**: quantlib, yfinance, alpha_vantage
- **Database**: SQLAlchemy, PyMySQL
- **API**: FastAPI, requests
- **Visualization**: matplotlib, plotly, seaborn

### Frontend Dependencies
- **CMS**: Drupal 11, PHP 8.2+
- **Frontend**: JavaScript (ES6+), SCSS/Sass, Bootstrap

## 📋 Script Usage Reference

### **Environment & Setup Scripts** (`scripts/`)
```bash
# Primary setup (comprehensive)
./scripts/unicorn_environment.sh              # Full environment setup + health check
./scripts/unicorn_environment.sh --setup-only # Setup only, no health check
./scripts/unicorn_environment.sh --check-only # Health check only

# Drupal-specific
./scripts/startup_drupal.sh                   # Start Drupal services
./scripts/drupalcachreset.sh                  # Reset Drupal cache (if exists)


### **Available Aliases** (after setup)
```bash
unicorn-env        # Run comprehensive unicorn_environment.sh
drupal-start       # Start Drupal services and run full platform validation
drupal-status      # Check Drupal status
drupal-logs        # View Drupal logs
drupal-restart     # Restart Drupal services
drupal-cd          # Change to Drupal directory
unicorn-root       # Change to root directory
ibkr-start         # Start IBKR Gateway only (critical for trading)
setup-data-cron    # Setup data pipeline automation
install-env        # Full comprehensive environment installation
```

### **Comprehensive Environment Script** (`unicorn_environment.sh`)
**Enhanced September 2025** - Now includes consolidated functionality:

```bash
# Full environment setup + health check (default)
./scripts/unicorn_environment.sh

# Environment variables and aliases only
./scripts/unicorn_environment.sh --setup-only

# Health checks only
./scripts/unicorn_environment.sh --check-only

# Full startup (Drupal + IBKR + validation)
./scripts/unicorn_environment.sh --startup

# IBKR Gateway only
./scripts/unicorn_environment.sh --ibkr-only

# Data pipeline automation setup
./scripts/unicorn_environment.sh --data-cron

# Comprehensive environment installation
./scripts/unicorn_environment.sh --install-env
```

**Replaces:**
- `setup_environment.sh` → Use `--install-env`
- `setup_data_cron.sh` → Use `--data-cron`


### Portfolio Algorithm Development

#### **Risk Algorithm Standards** (`Myportolio/risk_algorithms/`)
```python
# Risk algorithms: Pure risk calculations, NO trading decisions
class ETHBasicRisk:
    def __init__(self, max_drawdown=0.15, max_daily_var=0.06):
        # Risk-only parameters
        
    def calculate_risk_metrics(self, portfolio_data):
        # Pure risk calculations
        return risk_metrics
        
    def validate_risk_limits(self, positions):
        # Risk validation only
        return validation_result
```

#### **Trading Algorithm Standards** (`Myportolio/trading_algorithms/`)
```python
# Trading algorithms: Pure trading strategies, NO risk calculations
class ETHMomentumStrategy:
    def __init__(self, symbol="ETHUSD", lookback=10):
        # Trading strategy parameters only
        
    def generate_signals(self, market_data):
        # Pure trading signal generation
        return trading_signals
        
    def optimize_portfolio(self, signals):
        # Portfolio optimization without risk calculation
        return portfolio_targets
```

#### **Framework Integration** (`utilities/`)
```python
# Framework utilities integrate both algorithm types
from utilities.EnhancedPortfolioManager import EnhancedPortfolioManager
from Myportolio.risk_algorithms.eth_basic_risk import ETHBasicRisk
from Myportolio.trading_algorithms.eth_momentum_strategy import ETHMomentumStrategy

# Integration example
portfolio_manager = EnhancedPortfolioManager()
risk_constraints = risk_algorithm.calculate_constraints()
trading_signals = trading_algorithm.generate_signals()
portfolio_targets = portfolio_manager.integrate(trading_signals, risk_constraints)
```

### Drupal Development Best Practices

1. **Cache Management**
   - Always fix permissions before cache operations to avoid errors
   - Use proper user context for cache rebuilds
   - **IMPORTANT**: Run Drush cache operations as www-data user to prevent MySQL PDO errors
   ```bash
   # Fix permissions before cache operations
   
   # Remove problematic cached files if needed
   sudo rm -rf /workspaces/unicorninvesting/WebFrontend/web/sites/default/files/css/*
   sudo rm -rf /workspaces/unicorninvesting/WebFrontend/web/sites/default/files/js/*
   
   ```

2. **Drush Operations**
   - Always use system PHP for Drush operations: `/usr/bin/php8.3 ./vendor/bin/drush.php`
   - **CRITICAL**: For Drush commands with database operations, use sudo to avoid permission issues:
     ```bash
     # Simple method that works in this environment:
     cd /workspaces/unicorninvesting/WebFrontend
     sudo /usr/bin/php8.3 ./vendor/bin/drush.php cache:rebuild
     ```
   - Verify PHP extensions are available before troubleshooting
   - See `/workspaces/unicorninvesting/docs/DRUSH_DEPENDENCIES_CHECKLIST.md` for complete troubleshooting guide

3. **Module Development & Changes**
    - **MANDATORY**: After ANY change to Drupal modules (code, CSS, routing, etc.), ALWAYS clear the Drupal cache.
    - To clear cache:
       ```bash
       su root
       ./scripts/drupalcachreset.sh
       ```

### Python Standards
- Follow PEP 8
- Use type hints and dataclasses
- Use pandas for data manipulation
- Use scikit-learn pipelines, validate models
- Use FastAPI for APIs, Pydantic for validation

### Database Standards
- Normalize schema, use indexes and FKs
- Use snake_case naming
- Use parameterized queries, transactions
- Use SQLAlchemy ORM

## Migration Strategy

- Phase 1: Infra setup (LAMP, Drupal, MySQL, Python venv)
- Phase 2: Data migration (R→Python, files→DB, validation)
- Phase 3: Backend (Python analytics, REST APIs, ML migration)
- Phase 4: Frontend (Drupal content types, auth, dashboard)

## 📝 Documentation Standards

### **CRITICAL: No Summary Files After Actions**
- **❌ DO NOT** create separate summary files (e.g., `*_SUMMARY.md`, `*_COMPLETE.md`) after completing tasks
- **❌ DO NOT** create standalone documentation files for completed work

### **✅ Documentation Approach:**

#### **1. README.md Files (Primary Documentation)**
- **Update existing README.md files** in the relevant directory after any changes
- **Create README.md files** for new directories or components
- Include comprehensive information about functionality, usage, and architecture
- Keep documentation current and accurate
- Always read the readme file in any directory before making changes
- Always update the readme file in any directory after making changes

#### **2. Commit Messages (Action Reporting)**
- **Use detailed commit messages** for after-action reporting
- Include what was accomplished, why it was done, and any important notes
- Structure: `[Type]: Brief description`
- Body: Detailed explanation of changes and impact

#### **3. Inline Code Documentation**
- Document functions, classes, and complex logic directly in code
- Use appropriate comment styles for each language (docstrings, JSDoc, etc.)
- Include usage examples where helpful

### **Documentation Workflow:**
1. **Make changes** to code/configuration
2. **Update relevant README.md** with new information
3. **Commit with detailed message** explaining the changes
4. **NO separate summary files**

### **Example Good Commit Message:**
```
feat: Consolidate health_check.sh and setup_environment.sh scripts

- Created comprehensive unicorn_environment.sh script combining functionality
- Added modular execution options (--setup-only, --check-only, --help)
- Enhanced health checks with 23 system validations
- Moved legacy scripts to scripts/legacy/ directory
- Added deprecation wrappers for backward compatibility
- Updated documentation in scripts/README.md
- Success rate: 86% system validation with minor issues identified
```

## README.md Context Protocol

**CRITICAL REQUIREMENT - ENFORCED ON EVERY BACKEND CALL:**

When working on any part of this codebase, you MUST:

### **PRE-WORK CHECKLIST:**
1. ✅ **Read README.md** in the current working directory
2. ✅ **Read ARCHITECTURE.md** in the current working directory  
3. ✅ **Include this instructions file** in the modification context
4. ✅ **Understand existing patterns** before implementing changes

### **POST-WORK CHECKLIST:**
1. ✅ **Update README.md** with all new functionality and changes
2. ✅ **Update ARCHITECTURE.md** with any new patterns or standards
3. ✅ **Remove outdated information** discovered during work
4. ✅ **Ensure documentation accuracy** reflects current implementation

### **BACKEND CALL REQUIREMENTS:**
- **Instructions file re-read**: MANDATORY on every backend interaction
- **Context inclusion**: README.md + ARCHITECTURE.md + instructions file
- **Documentation updates**: Required after every modification session

**Failure to follow this protocol results in architectural inconsistency and technical debt.**

## 🎯 **GitHub Issues Management Strategy**

### **Issue Creation Policy**
- **Create GitHub Issues** for all documented but unimplemented features
- **Track architectural decisions** that need future implementation
- **Use epics** for high-level components and use cases
- **Link issues** to documentation and commits for traceability

### **Issue Categories & Labels**

#### **Epic Issues** (`epic` label)
- **Purpose**: Major architectural components requiring multiple sub-issues
- **Scope**: Multi-week initiatives (4+ weeks)
- **Examples**: "Epic: Integration", "Epic: Advanced ETH Algorithms"
- **When to Create**: High-level architecture documented but not implemented

#### **Feature Issues** (`feature` label)  
- **Purpose**: Specific functionality or components to be built
- **Scope**: 1-3 week development cycles
- **Examples**: "Feature: Real-time ETH Data Pipeline", "Feature: VaR Risk Calculator"
- **When to Create**: Detailed requirements exist but implementation pending

#### **Task Issues** (`task` label)
- **Purpose**: Specific coding tasks or configurations
- **Scope**: 1-5 day implementation items
- **Examples**: "Task: Implement Kelly Criterion", "Task: Add VaR calculations"
- **When to Create**: Specific technical requirements identified

## General Principles

- High accuracy and security (financial platform)
- Performance is critical for real-time trading
- Intuitive UX for financial professionals
- Maintainable, well-documented, auditable code
- **Use GitHub Issues** to track all planned but unimplemented work

## 📊 Current Implementation Status

### ✅ **Completed Architecture Components**
- **Clean Directory Structure**: Enforced single portfolio focus (Myportolio)
- **Algorithm Separation**: Risk and trading algorithms completely separated
- **Framework Utilities**: Portfolio management components organized in utilities/
- **Configuration System**: JSON-based portfolio configuration
- **Documentation Standards**: README.md only approach implemented
- **Script Organization**: All setup scripts properly organized in scripts/