# BackendPython

This directory contains the Python backend services for the Unicorn Investing Platform and the QuantConnect LEAN integration. The structure has been reorganized to separate proprietary unicorn code from the third-party LEAN framework.

## Directory Structure

```
BackendPython/
├── README.md                  # This file
├── Lean/                      # QuantConnect LEAN Framework (3rd party)
│   ├── Algorithm/            # LEAN algorithm framework
│   ├── Algorithm.CSharp/     # C# algorithm examples
│   ├── Algorithm.Python/     # Python algorithm examples
│   ├── Common/               # LEAN common libraries
│   ├── Data/                 # LEAN data handling
│   ├── Engine/               # LEAN execution engine
│   ├── Indicators/           # Technical indicators
│   ├── Tests/                # LEAN test suites
│   └── ...                   # Other LEAN components
└── unicorn/                  # Proprietary Unicorn Investing code
    ├── backtesting/          # Portfolio analytics and backtesting
    ├── batchjobs/            # Automated batch processing scripts
    ├── blotterscripts/       # Trade execution and portfolio management
    ├── data/                 # Data storage and processing
    ├── database/             # Database schemas and migrations
    ├── datagathering/        # Data collection and ingestion
    ├── datasetcreation/      # Feature engineering and dataset preparation
    ├── deployment/           # Deployment configurations
    ├── docs/                 # Documentation
    ├── predictiveanalytics/  # Advanced analytics and modeling
    ├── recomendationsystems/ # Recommendation algorithms
    ├── tests/                # Test suites
    ├── wpf-app/              # Legacy WPF applications (to be migrated)
    ├── quickstartGAportfolio.R   # R genetic algorithm portfolio
    ├── quickstartsingleNN.R      # R neural network implementation
    ├── unicorn.RData             # R workspace data
    └── unicorninvesting.Rproj    # R project file
```

## Architecture Overview

This directory now maintains clear separation between:

### LEAN Framework (`/Lean/`)
- **Purpose**: QuantConnect's open-source algorithmic trading engine
- **Repository**: https://github.com/QuantConnect/Lean
- **License**: Apache License 2.0
- **Components**: Complete LEAN framework with algorithms, data handling, and execution engine
- **Integration**: Used as foundation for algorithmic trading capabilities

### Unicorn Proprietary Code (`/unicorn/`)
- **Purpose**: Custom investment analysis, machine learning, and portfolio management
- **Components**: All proprietary algorithms, data processing, and business logic
- **Legacy**: Contains R scripts and data for migration to Python
- **Integration**: Will interface with LEAN for algorithmic trading execution

## Python Environment Setup

### Virtual Environment
Create and activate a Python virtual environment:

```bash
cd /workspaces/unicorninvesting/BackendPython
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

### Install Dependencies

#### LEAN Dependencies
```bash
cd Lean
# Follow LEAN installation guide for Python requirements
pip install -r requirements.txt  # If available in LEAN
```

#### Unicorn Dependencies
```bash
cd unicorn
pip install pandas numpy scipy scikit-learn tensorflow quantlib yfinance alpha_vantage SQLAlchemy PyMySQL fastapi uvicorn matplotlib plotly seaborn
```

## LEAN Framework Integration

### Installation and Setup
The LEAN framework is cloned from the official QuantConnect repository and provides:
- Algorithmic trading engine
- Data feeds and market data handling
- Backtesting infrastructure
- Live trading capabilities
- Research environment

### Key LEAN Components
- **Algorithm Framework**: Base classes for trading algorithms
- **Data Handling**: Market data ingestion and processing
- **Execution Engine**: Order management and trade execution
- **Indicators**: Technical analysis indicators
- **Brokerages**: Integration with various brokers

### LEAN Configuration
LEAN configuration files are located in:
- `Lean/Launcher/config.json` - Main configuration
- `Lean/Data/` - Data configuration and storage

## Unicorn Proprietary Components

### Core Services (Target Architecture)
The unicorn directory will be restructured to include:

```
unicorn/
├── backend/                   # Python backend services
│   ├── api/                  # FastAPI routes and endpoints
│   ├── ml/                   # Machine learning models and algorithms
│   ├── models/               # Database models and schemas
│   ├── services/             # Business logic and data processing
│   └── utils/                # Utility functions and helpers
├── algorithms/               # Custom trading algorithms for LEAN
├── data/                     # Data storage and processing
├── integrations/             # LEAN integration layer
└── legacy/                   # Legacy R code and data
```

### Migration Status

#### Completed Components
- ✅ **Directory Segregation**: LEAN and unicorn code separated
- ✅ **LEAN Framework**: Official repository cloned and available
- ✅ **Legacy Preservation**: All R code and data preserved in unicorn directory

#### Pending Components
- 🔄 **Python Backend Structure**: Reorganize unicorn directory for Python services
- 🔄 **LEAN Integration Layer**: Create interface between unicorn algorithms and LEAN
- 🔄 **R to Python Migration**: Convert R algorithms to Python
- 🔄 **Database Integration**: Connect unicorn services with LEAN data

## Running Components

### LEAN Framework
```bash
cd /workspaces/unicorninvesting/BackendPython/Lean
# Follow LEAN documentation for running algorithms
dotnet run --project Launcher/QuantConnect.Lean.Launcher.csproj
```

### Unicorn Services (Future)
```bash
cd /workspaces/unicorninvesting/BackendPython/unicorn
python -m uvicorn backend.api.main:app --reload --host 0.0.0.0 --port 8000
```

## Integration Strategy

### Data Flow
1. **Market Data**: LEAN handles real-time and historical market data
2. **Analysis**: Unicorn algorithms process data and generate signals
3. **Execution**: LEAN executes trades based on unicorn algorithm decisions
4. **Monitoring**: Both systems provide logging and performance metrics

### Development Workflow
1. Develop custom algorithms in `unicorn/algorithms/`
2. Test algorithms using LEAN's backtesting engine
3. Deploy algorithms to LEAN for live trading
4. Monitor performance through both LEAN and unicorn dashboards

## Database Configuration

### LEAN Database
LEAN uses its own data storage mechanisms for market data and algorithm state.

### Unicorn Database
Unicorn services connect to MySQL databases:
- **Development**: `unicorn_dev`
- **Production**: `unicorn_analytics`

## API Documentation

- **LEAN API**: Follow QuantConnect documentation
- **Unicorn API**: Available at `http://localhost:8000/docs` (when implemented)

## Testing

### LEAN Tests
```bash
cd Lean
# Follow LEAN testing procedures
dotnet test
```

### Unicorn Tests
```bash
cd unicorn
python -m pytest tests/ -v
```

## Security Considerations

- **LEAN**: Follows QuantConnect security practices
- **Unicorn**: Implements additional security for proprietary algorithms
- **Integration**: Secure communication between LEAN and unicorn services
- **Data Protection**: Encryption for sensitive financial algorithms

## Documentation References

- **LEAN Documentation**: https://www.quantconnect.com/docs/
- **LEAN GitHub**: https://github.com/QuantConnect/Lean
- **QuantConnect API**: https://www.quantconnect.com/docs/api-reference/
- **Unicorn Architecture**: See `/docs/` directory in main repository

## Contributing

1. **LEAN Changes**: Contribute to QuantConnect's official repository
2. **Unicorn Changes**: Follow internal development guidelines
3. **Integration**: Ensure compatibility between LEAN and unicorn components
4. **Testing**: Test both LEAN and unicorn components independently

## Contact

For questions about:
- **LEAN Framework**: Refer to QuantConnect documentation and community
- **Unicorn Integration**: Create issues in the internal repository
- **Architecture**: Consult the main project documentation
