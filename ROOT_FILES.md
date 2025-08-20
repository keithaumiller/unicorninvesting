# Root Level Files

Configuration and documentation files in the repository root.

## Files

### forexnotes.txt
**Purpose**: Documentation and research notes on forex trading optimization
**Contents**: 
- Fitness function formulations for forex trading
- Performance metrics including Net Profit (NP), Drawdown (DD), and Perfect Profit Correlation (PPC)
- Risk management strategies and optimization approaches
- Mathematical formulations: `Fitness = NP*NP*NP*Winners/(DD*DD)` with trade count penalties

### Architecture.md
**Purpose**: High-level system architecture overview
**Contents**:
- Current legacy architecture (R + WPF + MySQL)
- Target modern architecture (Python + Drupal + MySQL)
- Migration strategy and implementation phases
- Technology stack specifications

### LICENSE
**Purpose**: Software license and usage terms
**Contents**: Legal terms for software distribution and usage

### .gitignore
**Purpose**: Git version control exclusions
**Contents**: Files and directories to exclude from version control

## Directory Structure

### .github/
**Purpose**: GitHub-specific configuration and workflows
**Contents**:
- `instructions/unicorninvesting.instructions.md` - Development guidelines and coding standards

### BackendPython/
**Purpose**: All backend analytics, data processing, and machine learning code
**Contents**: Legacy R scripts being migrated to Python, database schemas, testing frameworks

### WebFrontend/
**Purpose**: Drupal 11 web interface and frontend components
**Contents**: Modern web UI replacing legacy WPF desktop applications

### datacleaning/
**Purpose**: Legacy data preprocessing and validation utilities
**Contents**: R debugging framework and data quality assurance tools
