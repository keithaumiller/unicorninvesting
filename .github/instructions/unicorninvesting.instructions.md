---
applyTo: '**'
---

# Unicorn Investing Platform - Development Instructions

## Project Overview

Unicorn Investing is a financial analytics platform for algorithmic trading with LEAN framework integration.

## ⚠️ IMPORTANT - First Time Setup After Codespace Restart

**After codespace restart or pause:**
- Run: `./scripts/unicorn_environment.sh` (comprehensive setup + health check)
- Or use: `unicorn-env` (after initial setup)
- Start services: `drupal-start` or `scripts/startup_drupal.sh`
- Available aliases: `drupal-start`, `drupal-status`, `drupal-logs`, `drupal-restart`, `drupal-cd`, `unicorn-root`, `unicorn-env`

### 🌐 GitHub Codespace URL Translation for Debugging

**Debugging tip:** Use `http://localhost/...` in terminal (not external GitHub Codespace URLs).

## 🏗️ **ENFORCED DIRECTORY STRUCTURE**

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
│   ├── 1_data_sources/           # LEAN Layer 1: Market data collectors
│   ├── 2_alpha_models/           # LEAN Layer 2: ETH models and signals
│   ├── 3_risk_management/        # LEAN Layer 3: Risk controls
│   ├── 4_portfolios/             # LEAN Layer 4: Portfolio construction
│   │   ├── Myportolio/          # ✅ SINGLE portfolio implementation
│   │   │   ├── risk_algorithms/ # Pure risk calculations
│   │   │   └── trading_algorithms/ # Pure trading strategies
│   │   └── utilities/           # Framework-level shared components
│   ├── 5_execution_models/       # LEAN Layer 5: Order execution
│   ├── 6_algorithms/             # LEAN Layer 6: Complete algorithms
│   └── README.md                # Main backend documentation
├── WebFrontend/                  # Drupal 11 frontend
├── scripts/                      # ✅ ALL setup and utility scripts
├── docs/                         # High-level documentation
├── tests/                        # Testing framework
└── deployment/                   # Deployment configurations
```

## Architecture Principles

### Clean Algorithm Separation
- **Risk Algorithms**: Pure risk calculations with NO trading decisions
- **Trading Algorithms**: Pure trading strategies with NO risk calculations
- **Framework Utilities**: Shared components for portfolio management
- **Single Portfolio Focus**: Myportolio as the ONLY portfolio implementation

### LEAN Framework Integration (6-Layer Architecture)
1. **Data Sources** → Raw market data collection
2. **Alpha Models** → ETH models and trading signals  
3. **Risk Management** → Risk controls and limits
4. **Portfolio Construction** → Position sizing and allocation (OUR FOCUS)
5. **Execution Models** → Order placement and execution
6. **Algorithms** → Complete trading algorithms

## Technology Stack

### Core Technologies
- **Frontend**: Drupal 11 (PHP 8.2+)
- **Backend**: Python 3.9+ with LEAN framework integration
- **Database**: MySQL 8.0+
- **Web Server**: Apache/Nginx
- **Trading Framework**: QuantConnect LEAN (6-layer architecture)

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

# Legacy (use unicorn_environment.sh instead)
./scripts/setup_environment.sh                # Basic environment setup
./scripts/health_check.sh                     # Basic health check
```

### **Available Aliases** (after setup)
```bash
unicorn-env        # Run unicorn_environment.sh
drupal-start       # Start Drupal services
drupal-status      # Check Drupal status
drupal-logs        # View Drupal logs
drupal-restart     # Restart Drupal services
drupal-cd          # Change to Drupal directory
unicorn-root       # Change to root directory
```

## Coding Standards

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

## README.md Context
When working on any part of this codebase, always consider:
- Always review and update README.md in the same directory before and after any file change.
- **Create GitHub Issues** for any new unimplemented features discovered in documentation.

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
- **Examples**: "Epic: LEAN Integration", "Epic: Advanced ETH Algorithms"
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
- **Basic ETH Algorithms**: Hello World implementations for momentum and risk
- **Configuration System**: JSON-based portfolio configuration
- **Documentation Standards**: README.md only approach implemented
- **Script Organization**: All setup scripts properly organized in scripts/

### 🚧 **Ready for Development**
- **LEAN Integration**: Architecture ready for backtesting framework connection
- **Advanced Algorithms**: Foundation ready for sophisticated algorithm development
- **Data Pipeline**: ETH model performance system with SQLite database available
- **Risk Management**: Basic risk framework ready for extension
- **Portfolio Construction**: Framework components ready for enhancement

### 🎯 **Development Focus Areas**
1. **Algorithm Implementation**: Expand ETH momentum and risk algorithms
2. **LEAN Framework Integration**: Connect portfolio construction to LEAN backtesting
3. **Data Integration**: Leverage existing ETH models and performance data
4. **Testing Framework**: Implement comprehensive testing for algorithm separation
5. **API Development**: Create REST APIs for portfolio management

---

**Architecture Status**: ✅ Complete  
**Implementation Status**: 🚧 Ready for Algorithm Development  
**LEAN Integration**: 🚧 Ready for Framework Connection  
**Success Rate**: 100% clean separation achieved  
**Next Phase**: Advanced ETH algorithm development and LEAN backtesting integration