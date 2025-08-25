# Frontend

**Status**: 🔄 Template structure for future component-based frontend architecture

This directory contains the template structure for frontend components that will be implemented as the platform evolves beyond the current Drupal-based architecture.

## Current Architecture (August 2025)

### Active Frontend Location
**Primary Frontend**: `/WebFrontend/` (Drupal 11 installations)
- ✅ **Production Ready**: 4 domains with SSL certificates
- ✅ **Drupal 11**: Modern CMS with PHP 8.3
- ✅ **Database Integration**: MySQL 8.0 with isolated databases
- ✅ **Infrastructure**: Apache 2.4.58 with security hardening

### This Directory Purpose
This `/frontend/` directory serves as a template for future frontend architectures when the platform requires:
- React/Vue.js single-page applications
- Progressive Web App (PWA) capabilities
- Real-time trading interfaces
- Mobile-first responsive design
- Modern JavaScript frameworks

## Planned Frontend Evolution

### Current State: Drupal 11 ✅
- **Advantages**: Rapid development, content management, user authentication
- **Use Case**: Initial platform launch and business user interfaces
- **Location**: `/WebFrontend/` with production domains

### Future State: Modern JavaScript Framework 📋
- **Technology**: React/Vue.js with TypeScript
- **Features**: Real-time trading, advanced charting, mobile optimization
- **Architecture**: Component-based with state management
- **Integration**: Direct API communication with Python backend

## Template Structure

### Planned Components
```
frontend/
├── config/        # Build configuration and environment settings
├── modules/       # Reusable UI components and business logic
└── themes/        # Design systems and styling frameworks
```

### Technology Considerations
- **Framework**: React with TypeScript for type safety
- **State Management**: Redux Toolkit for complex trading data
- **UI Library**: Material-UI or Chakra UI for consistent design
- **Charts**: TradingView widgets or D3.js for financial visualizations
- **Real-time**: WebSocket integration for live market data

## Migration Strategy

### Phase 1: Drupal Foundation ✅
- **Current**: Drupal 11 for initial platform functionality
- **Benefits**: Rapid deployment, user management, content workflows
- **Timeline**: Active development phase

### Phase 2: API-First Development 🔄
- **Backend**: FastAPI providing RESTful services
- **Frontend**: Drupal consuming backend APIs
- **Integration**: Gradual decoupling of frontend and backend

### Phase 3: Modern Frontend Migration 📋
- **Development**: Component-based frontend architecture
- **Testing**: Parallel development with Drupal fallback
- **Deployment**: Progressive migration of user interfaces

## Development Guidelines

### When to Use This Directory (Future)
- Advanced trading interfaces requiring real-time updates
- Mobile applications and Progressive Web Apps
- Custom charting and visualization requirements
- High-performance user interactions

### When to Use WebFrontend (Current) ✅
- **Current Development**: All current frontend work
- **Content Management**: User documentation and marketing
- **Administrative Interfaces**: User management and reporting
- **Rapid Prototyping**: Quick feature development and testing

## Integration Points

### Backend API Integration
```javascript
// Future API integration pattern
const API_BASE = 'http://localhost:8000';

// Market data endpoints
const marketData = await fetch(`${API_BASE}/market/data`);

// Portfolio management
const portfolio = await fetch(`${API_BASE}/portfolio/holdings`);

// Real-time updates
const websocket = new WebSocket(`ws://localhost:8000/ws`);
```

### Authentication Flow
```javascript
// Unified authentication with Drupal and Python backend
const authToken = await authenticateUser(credentials);
const apiClient = new APIClient(authToken);
```

## Current Development Focus

### Active Frontend Development
**Location**: `/WebFrontend/` ✅ **Use This**
**Status**: Production-ready Drupal 11 installations
**Features**: Multi-domain hosting, SSL security, database integration

### Template Maintenance
**Location**: This directory (`/frontend/`)
**Status**: Template structure for future development
**Purpose**: Modern frontend architecture planning

## Documentation Links

- **Current Frontend**: [WebFrontend README](../WebFrontend/README.md)
- **Backend API**: [Python Backend](../BackendPython/unicorn/backend/README.md)
- **Database**: [Database Configuration](../database/README.md)
- **Deployment**: [Infrastructure Setup](../deployment/README.md)