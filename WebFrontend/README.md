# WebFrontend

Drupal 11 web interface and frontend components for the Unicorn Investing platform.

## Purpose
- Modern web-based user interface replacing legacy WPF desktop applications
- Drupal 11 content management and user authentication
- Responsive dashboard for portfolio management and analytics
- Integration with Python backend APIs for real-time data

## Directory Structure

### frontend/
**Purpose**: Drupal 11 application structure
**Contents**: 
- `modules/` - Custom Drupal modules for unicorn-specific functionality
- `themes/` - Custom responsive themes and UI components  
- `config/` - Drupal configuration files for content types and views

## Target Functionality

### User Interface Components
- **Portfolio Dashboard** - Real-time portfolio performance and allocation displays
- **Trading Interface** - Order placement and trade management
- **Analytics Charts** - Performance visualization and market analysis
- **Risk Management** - Portfolio risk metrics and analysis tools
- **Market Data Feeds** - Live stock and forex data integration

### Drupal Integration
- **Content Types** - Portfolio, Trade, Market Data, User Profile entities
- **Views** - Custom data displays and reporting interfaces
- **Custom Modules** - Unicorn-specific business logic and API integration
- **User Roles** - Trader, Admin, Analyst permission management
- **API Integration** - RESTful communication with Python backend services

### Mobile Responsiveness
- Bootstrap-based responsive design
- Mobile-optimized trading interface
- Touch-friendly chart interactions
- Progressive Web App (PWA) capabilities

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
