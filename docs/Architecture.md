# Unicorn Investing Platform - Architecture Overview

## Current Architecture (Legacy)

- **Frontend**: WPF desktop applications (Windows-only)
- **Backend**: R scripts for data processing, analytics, and machine learning
- **Database**: MySQL (basic schema, direct access from R)
- **Data Storage**: File-based (CSV, RData, etc.) for market, portfolio, and results data
- **Batch Processing**: R scripts for scheduled jobs and backtesting
- **Directory Structure**:
  - `/backtesting/` - R-based backtesting and analytics
  - `/batchjobs/` - R batch scripts for automation
  - `/blotterscripts/` - R scripts for trade blotter management
  - `/data/` - Raw and processed market data (CSV, TXT)
  - `/database/` - MySQL dumps and schema files
  - `/datagathering/` - R scripts for data acquisition
  - `/datasetcreation/` - R scripts for feature and training set generation
  - `/predictiveanalytics/` - R scripts for regression and model exploration
  - `/recomendationsystems/` - R scripts for GA and neural network model training
  - `/wpf-app/` - Legacy WPF desktop application

## Key Characteristics
- Heavy use of R for all analytics and ML
- Direct database access from R
- Manual file-based data pipelines
- Limited web or API integration
- Windows-centric user interface

## Planned Future Architecture (Target)

- **Frontend**: Drupal 11 web application (PHP 8.2+, Twig, modern JS/CSS)
- **Backend**: Python 3.9+ for all analytics, data processing, and machine learning
- **Database**: MySQL 8.0+ with optimized, normalized schema
- **Infrastructure**: Standard LAMP stack (Linux, Apache/Nginx, MySQL, PHP)
- **API Layer**: FastAPI (Python) for RESTful services and integration
- **Data Pipelines**: Automated, database-driven, Python-based ETL
- **Machine Learning**: scikit-learn, TensorFlow, Keras for model development and deployment
- **Testing & CI/CD**: Automated testing (pytest, PHPUnit, Jest), CI/CD pipelines

## Migration Strategy
1. **Infrastructure Setup**: LAMP server, Drupal install, MySQL migration
2. **Data Migration**: Convert R scripts to Python, move file data to DB
3. **Backend Services**: Python analytics engine, REST APIs, ML model migration
4. **Frontend Development**: Drupal content types, views, dashboards, integration with backend

## Security & Performance
- Role-based access control (Drupal, OAuth2 for APIs)
- Data encryption and GDPR compliance
- Caching, connection pooling, and query optimization
- Real-time data updates via web APIs

## Directory Structure (Target)
```
unicorninvesting/
├── backend/        # Python backend services
│   ├── api/       # FastAPI routes
│   ├── models/    # SQLAlchemy models
│   ├── services/  # Business logic
│   ├── ml/        # Machine learning models
│   └── utils/     # Utility functions
├── frontend/      # Drupal frontend
│   ├── modules/   # Custom Drupal modules
│   ├── themes/    # Custom themes
│   └── config/    # Configuration files
├── database/      # Database schemas and migrations
├── tests/         # Test suites
├── docs/          # Documentation
└── deployment/    # Deployment scripts
```

## Summary
The Unicorn Investing platform is transitioning from a legacy R and desktop-based system to a modern, scalable, and secure web-based architecture. The new stack leverages Drupal for the frontend, Python for backend analytics and machine learning, and MySQL for robust data management, all deployed on a standard LAMP infrastructure. This will enable better performance, maintainability, and user experience for financial professionals.
