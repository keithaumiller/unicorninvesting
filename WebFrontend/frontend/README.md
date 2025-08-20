# Frontend (Drupal 11)

Drupal 11 application structure for the Unicorn Investing web interface.

## Purpose
- Drupal-based content management system
- User authentication and role management
- Custom content types for financial data
- API integration with Python backend services

## Directory Structure

### modules/
**Purpose**: Custom Drupal modules for unicorn-specific functionality
**Target Modules**:
- `unicorn_portfolio` - Portfolio management and display
- `unicorn_trading` - Trade order placement and management
- `unicorn_analytics` - Performance charts and analytics integration
- `unicorn_market_data` - Real-time market data feeds
- `unicorn_api` - Backend API integration module
- `unicorn_user` - Extended user profile management
- `unicorn_reports` - Custom reporting and data export

### themes/
**Purpose**: Custom Drupal themes for responsive design
**Target Themes**:
- `unicorn_base` - Base theme with trading-specific styling
- `unicorn_responsive` - Mobile-optimized trading interface
- `unicorn_admin` - Administrative dashboard theme

### config/
**Purpose**: Drupal configuration management
**Configuration Files**:
- `content_types.yml` - Portfolio, Trade, MarketData content type definitions
- `views.yml` - Custom views for portfolio dashboards and reports
- `user_roles.yml` - Trader, Admin, Analyst role definitions
- `field_definitions.yml` - Custom fields for financial data
- `menu_configuration.yml` - Navigation structure for trading interface

## Content Types (Planned)

### Portfolio Content Type
**Fields**:
- Portfolio Name (Text)
- User Owner (Entity Reference)
- Description (Text Area)
- Risk Level (Select List)
- Creation Date (Date)
- Performance Metrics (Custom Field Group)

### Trade Content Type  
**Fields**:
- Symbol (Text)
- Action (Buy/Sell Select)
- Quantity (Number)
- Price (Decimal)
- Timestamp (DateTime)
- Portfolio (Entity Reference)
- Status (Select List)

### Market Data Content Type
**Fields**:
- Symbol (Text)
- Price (Decimal)
- Volume (Number)
- Timestamp (DateTime)
- Change (Decimal)
- Change Percentage (Decimal)

## Views (Planned)

### Portfolio Dashboard
- Real-time portfolio performance
- Asset allocation charts
- Recent trade activity
- Risk metrics display

### Trading Interface
- Live market data feeds
- Order entry forms
- Order book display
- Position management

### Analytics Reports
- Historical performance charts
- Risk analysis reports  
- Comparative performance views
- Export functionality

## API Integration
- RESTful endpoints for Python backend communication
- WebSocket integration for real-time data streaming
- Authentication token management
- Error handling and retry logic

## Development Standards
- Follow Drupal 11 coding standards
- Use Drupal's configuration management system
- Implement proper caching for performance
- Follow accessibility guidelines (WCAG 2.1)
- Use responsive design principles folder contains all Drupal 11 frontend code for Unicorn Investing.

## Purpose
- User interface, dashboards, and reporting

## Current Occupants
- [modules/]: Custom Drupal modules (stub)
- [themes/]: Custom themes (stub)
- [config/]: Configuration files (stub)

## Future Stub Files
- modules/unicorn_portfolio.module
- themes/unicorn_theme/
- config/content_types.yml
