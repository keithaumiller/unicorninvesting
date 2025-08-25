# WebFrontend

**Status**: ✅ Production-ready Drupal 11 installations with LAMP infrastructure

Drupal 11 web interface and frontend components for the Unicorn Investing platform, replacing legacy WPF desktop applications.

## Infrastructure Status (August 2025)

### ✅ Deployed Components
- **Web Server**: Apache 2.4.58 with virtual hosts configured
- **PHP**: Version 8.3 with required extensions
- **Database**: MySQL 8.0 with isolated Drupal databases
- **SSL Certificates**: Let's Encrypt with auto-renewal
- **Multi-Domain Setup**: 4 production domains configured

### 🌐 Live Domains
1. **thetruthperspective.org** - Primary domain with Cloudflare integration
2. **stlouisintegration.com** - Business integration services
3. **angelicafeliciano.com** - Professional portfolio site
4. **unicorninvesting.us** - Main trading platform domain

### 🗄️ Database Configuration
```bash
# Isolated Drupal databases with secure authentication
stlouisintegration_drupal    # Business site database
angelicafeliciano_drupal     # Portfolio site database  
unicorninvesting_drupal      # Main platform database
unicorn_analytics            # Financial data and ML models
```

## Directory Structure

### frontend/
**Status**: ✅ Drupal 11 core installed and configured
**Purpose**: Drupal 11 application structure
**Contents**: 
- `modules/` - Custom Drupal modules for financial functionality
- `themes/` - Responsive themes optimized for trading interfaces  
- `config/` - Drupal configuration for portfolio and trading content types

## Current Functionality ✅

### Infrastructure Services
- **HTTPS Security**: SSL certificates active on all domains
- **Database Isolation**: Separate databases for each application
- **User Authentication**: MySQL-based user management with strong passwords
- **File Permissions**: Secure file system permissions for web applications
- **Backup Strategy**: Database backup procedures in place

### Web Server Configuration
```apache
# Virtual hosts configured for all domains
# SSL certificates with auto-renewal
# Security headers and HTTPS redirects
# PHP 8.3 with required extensions
```

## Target Functionality 🔄

### User Interface Components (In Development)
- **Portfolio Dashboard** - Real-time portfolio performance and allocation displays
- **Trading Interface** - Order placement and trade management via LEAN
- **Analytics Charts** - Performance visualization and market analysis
- **Risk Management** - Portfolio risk metrics and analysis tools
- **Market Data Feeds** - Live stock and forex data integration

### API Integration (Ready for Development)
- **FastAPI Backend**: Python backend ready at `/BackendPython/unicorn/backend/`
- **REST Endpoints**: Communication layer for real-time data
- **Database Sync**: Shared data models between Drupal and Python
- **Authentication**: Unified user management across platforms

### Drupal Features (To Be Implemented)
- **Content Types** - Portfolio, Trade, Market Data, User Profile entities
- **Views** - Custom data displays and reporting interfaces
- **Custom Modules** - Unicorn-specific business logic and API integration
- **User Roles** - Trader, Admin, Analyst permission management
- **Real-time Updates** - WebSocket integration for live market data

### Mobile Responsiveness
- Bootstrap-based responsive design
- Mobile-optimized trading interface
- Touch-friendly chart interactions
- Progressive Web App (PWA) capabilities

## Development Workflow

### Current Setup
1. **Web Access**: All domains accessible via HTTPS
2. **Database Access**: MySQL credentials configured
3. **File System**: Proper permissions for Drupal development
4. **SSL Security**: Automated certificate management

### Next Steps
1. **Drupal Configuration**: Complete site setup through web interface
2. **API Integration**: Connect with Python FastAPI backend
3. **Custom Modules**: Develop financial trading functionality
4. **Theme Development**: Create responsive trading interfaces
5. **Testing**: Comprehensive testing across all domains

## Server Environment

### System Information
- **OS**: Ubuntu 24.04.2 LTS
- **Web Server**: Apache/2.4.58
- **Database**: MySQL 8.0.40-0ubuntu0.24.04.1
- **PHP**: 8.3.6 with required extensions
- **SSL**: Let's Encrypt certificates with auto-renewal

### Security Features
- **HTTPS Everywhere**: All traffic encrypted
- **Database Isolation**: Separate credentials per application
- **File Permissions**: Secure web directory permissions
- **Password Security**: Strong authentication requirements

## Migration from WPF
The web frontend will replace all WPF desktop application functionality:

1. **Portfolio Management** - Web-based portfolio creation and management
2. **Real-time Data** - WebSocket integration for live market feeds
3. **Trading Controls** - Web forms for order placement and management
4. **Analytics** - Interactive charts using D3.js or similar libraries
5. **User Management** - Drupal user authentication replacing Windows authentication

## Technology Stack
- **Drupal 11** - Content management and user authentication
- **PHP 8.2+** - Backend scripting following PSR-12 standards
- **Twig** - Templating engine for dynamic content
- **JavaScript ES6+** - Frontend interactivity and API integration
- **SCSS/Sass** - Responsive styling with Bootstrap framework
- **WebSockets** - Real-time data streaming from Python backend

## Development Standards
- Follow Drupal coding standards and best practices
- Implement proper accessibility (WCAG 2.1) guidelines
- Use responsive design patterns for all devices
- Integrate with Python FastAPI backend services
- Implement proper caching strategies for performance
