---
applyTo: '**'
---

# Unicorn Investing Platform - Development Instructions

## Project Overview

Unicorn Investing is a comprehensive investment platform focused on unicorn startups, high-growth companies, and financial market analysis. The platform provides investment analysis, portfolio management, algorithmic trading, and machine learning-driven recommendations.

## ⚠️ IMPORTANT - First Time Setup After Codespace Restart

**When working in this codebase for the first time after a codespace restart or pause, ALWAYS run the environment health check scripts first:**

1. **Set up environment aliases**: `source /workspaces/unicorninvesting/scripts/setup_environment.sh`
2. **Validate and start services**: `drupal-start` (or `/workspaces/unicorninvesting/scripts/startup_drupal.sh`)

These scripts will:
- ✅ Check and start Apache web server
- ✅ Check and start MySQL database server  
- ✅ Validate port availability (80, 3306)
- ✅ Clear Drupal cache to prevent module loading issues
- ✅ Test website accessibility at both homepage and dashboard
- ✅ Set up convenient aliases for development

**Available aliases after setup:**
- `drupal-start` - Start and validate Drupal system
- `drupal-status` - Check Apache and MySQL status
- `drupal-logs` - View recent Drupal error logs
- `drupal-restart` - Restart Apache and MySQL services
- `drupal-cd` - Change to Drupal root directory
- `unicorn-root` - Change to project root directory

### 🌐 GitHub Codespace URL Translation for Debugging

**IMPORTANT**: When debugging in the workspace environment, always translate external GitHub Codespace URLs to localhost:

- **External URL**: `https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/admin/metrics`
- **Terminal/Debugging URL**: `http://localhost/admin/metrics` or `http://127.0.0.1/admin/metrics`

**Why**: The workspace terminal cannot authenticate through GitHub's tunnel proxy (returns 401 Unauthorized), but localhost routes directly to Apache and works perfectly. Browser access uses your authenticated GitHub session, while terminal access needs local routing.

### Current State
- Legacy R-based analytics and machine learning models
- Basic MySQL database schema
- WPF desktop applications (legacy)
- File-based data storage and processing

### Future Architecture (Target State)
- **Frontend**: Drupal 11 web interface following modern web standards
- **Backend**: Python-based data processing, analysis, and machine learning
- **Database**: MySQL with optimized schema for high-performance analytics
- **Infrastructure**: Standard LAMP server architecture

## Technology Stack

### Primary Technologies
- **Frontend**: Drupal 11 with PHP 8.2+
- **Backend Processing**: Python 3.9+ with data science libraries
- **Database**: MySQL 8.0+
- **Web Server**: Apache/Nginx
- **Version Control**: Git

### Python Dependencies
- **Data Analysis**: pandas, numpy, scipy
- **Machine Learning**: scikit-learn, tensorflow, keras
- **Financial Data**: quantlib, yfinance, alpha_vantage
- **Database**: SQLAlchemy, PyMySQL
- **API**: FastAPI, requests
- **Visualization**: matplotlib, plotly, seaborn

### Frontend Dependencies
- **Drupal 11**: Latest stable release
- **PHP**: 8.2 or higher
- **JavaScript**: ES6+ with modern frameworks as needed
- **CSS**: SCSS/Sass with Bootstrap or similar framework

## Coding Standards

### Drupal 11 Standards
Follow official Drupal coding standards and best practices:

1. **PHP Code Style**
   - Use PSR-12 coding standard
   - Follow Drupal API documentation patterns
   - Implement proper dependency injection
   - Use typed properties and return types where possible

2. **Module Development**
   - Create custom modules for unicorn-specific functionality
   - Use proper hook implementations
   - Follow configuration management best practices
   - Implement proper caching strategies

3. **Theme Development**
   - Use Twig templating system
   - Implement responsive design patterns
   - Follow accessibility guidelines (WCAG 2.1)
   - Use Drupal's CSS and JavaScript libraries

4. **Database Integration**
   - Use Drupal's database abstraction layer
   - Implement proper entity relationships
   - Use Views for data display
   - Follow content type and field best practices

### Python Standards
Follow PEP 8 and modern Python best practices:

1. **Code Structure**
   ```python
   # Type hints for all functions
   def analyze_portfolio(portfolio_id: int, timeframe: str = '1Y') -> Dict[str, float]:
       """Analyze portfolio performance metrics."""
       pass

   # Use dataclasses for data structures
   @dataclass
   class InvestmentAnalysis:
       risk_score: float
       expected_return: float
       volatility: float
   ```

2. **Data Processing**
   - Use pandas for data manipulation
   - Implement proper error handling and logging
   - Use type hints consistently
   - Follow functional programming principles where applicable

3. **Machine Learning**
   - Use scikit-learn pipelines
   - Implement proper model validation
   - Save models using joblib or pickle
   - Document model parameters and performance metrics

4. **API Development**
   - Use FastAPI for REST APIs
   - Implement proper authentication
   - Use Pydantic models for data validation
   - Follow OpenAPI specification

### Database Standards

1. **Schema Design**
   - Use normalized database design
   - Implement proper indexing strategies
   - Use foreign key constraints
   - Follow naming conventions (snake_case)

2. **Queries**
   - Use parameterized queries
   - Implement proper transaction management
   - Use SQLAlchemy ORM for Python
   - Optimize for performance

## Migration Strategy

### Phase 1: Infrastructure Setup
1. Set up LAMP server environment
2. Install and configure Drupal 11
3. Migrate MySQL database schema
4. Set up Python virtual environment

### Phase 2: Data Migration
1. Convert R data processing scripts to Python
2. Migrate file-based data to database
3. Implement data validation and cleaning
4. Set up automated data pipelines

### Phase 3: Backend Services
1. Implement Python-based analytics engine
2. Create REST APIs for data access
3. Migrate machine learning models to Python
4. Implement real-time data processing

### Phase 4: Frontend Development
1. Design Drupal content types and views
2. Implement user authentication and authorization
3. Create dashboard and reporting interfaces
4. Integrate with Python backend services

## Development Guidelines

### Code Organization
```
unicorninvesting/
├── backend/                    # Python backend services
│   ├── api/                   # FastAPI routes
│   ├── models/                # SQLAlchemy models
│   ├── services/              # Business logic
│   ├── ml/                    # Machine learning models
│   └── utils/                 # Utility functions
├── frontend/                  # Drupal frontend
│   ├── modules/               # Custom Drupal modules
│   ├── themes/                # Custom themes
│   └── config/                # Configuration files
├── database/                  # Database schemas and migrations
├── tests/                     # Test suites
├── docs/                      # Documentation
└── deployment/                # Deployment scripts
```

### Testing Requirements
1. **Python Backend**
   - Unit tests with pytest
   - Integration tests for APIs
   - Performance tests for ML models
   - Minimum 80% code coverage

2. **Drupal Frontend**
   - PHPUnit for backend functionality
   - JavaScript testing with Jest
   - Functional testing with Behat
   - Accessibility testing

### Security Requirements
1. **Authentication & Authorization**
   - Implement OAuth 2.0 for API access
   - Use Drupal's user management system
   - Implement role-based access control
   - Secure API endpoints with proper authentication

2. **Data Security**
   - Encrypt sensitive financial data
   - Implement data backup strategies
   - Use HTTPS for all communications
   - Follow GDPR compliance requirements

### Performance Requirements
1. **Database Optimization**
   - Index frequently queried columns
   - Implement database connection pooling
   - Use read replicas for reporting queries
   - Monitor query performance

2. **Application Performance**
   - Implement caching strategies (Redis/Memcached)
   - Use asynchronous processing for heavy tasks
   - Optimize image and asset delivery
   - Monitor application performance metrics

## Legacy Code Migration

### R to Python Conversion Guidelines
1. **Data Processing Functions**
   ```r
   # R Code (Legacy)
   loadfeaturelist <- function(userid, portfolioname) {
     # R implementation
   }
   ```
   
   ```python
   # Python Code (Target)
   def load_feature_list(user_id: int, portfolio_name: str) -> List[str]:
       """Load feature list for given user and portfolio."""
       # Python implementation using pandas/SQLAlchemy
   ```

2. **Machine Learning Models**
   - Convert R neural networks to TensorFlow/Keras
   - Migrate statistical models to scikit-learn
   - Implement proper model versioning
   - Maintain backward compatibility during transition

### WPF Application Replacement
1. Replace desktop WPF applications with web-based Drupal interface
2. Migrate user workflows to web forms
3. Implement real-time data updates via WebSockets
4. Ensure feature parity with legacy applications

## Quality Assurance

### Code Review Process
1. All code changes require peer review
2. Automated testing must pass
3. Security scanning for vulnerabilities
4. Performance impact assessment

### Documentation Requirements
1. All functions must have docstrings
2. API endpoints must be documented
3. Database schema changes must be documented
4. User-facing features need user documentation

### Deployment Process
1. Use Git for version control
2. Implement CI/CD pipelines
3. Use staging environment for testing
4. Monitor production deployments

## README.md Context

When working on any part of this codebase, always consider:
- This is a financial platform requiring high accuracy and security
- Performance is critical for real-time trading decisions
- User experience should be intuitive for financial professionals
- Code must be maintainable and well-documented
- All financial calculations must be auditable and traceable

## Parent Directory Context

### /workspaces/unicorninvesting/
This is the main repository containing:
- Legacy R scripts for financial analysis and machine learning
- Data processing pipelines for stock and forex data
- Portfolio management and optimization algorithms
- Database integration for MySQL
- Batch processing jobs for automated trading
- Research and backtesting frameworks

The goal is to modernize this codebase while preserving the core financial logic and improving scalability, maintainability, and user experience.



When updating a file. always review the README.md for that file in the same directory for context.
After updating a file. Always update the README.md for that directory with the latest information.