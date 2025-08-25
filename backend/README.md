# Backend Services

**Status**: 🔄 Template structure for future microservices architecture

This directory contains the template structure for backend services that will be implemented as the platform scales beyond the current integrated architecture.

## Current Architecture (August 2025)

### Active Backend Location
**Primary Backend**: `/BackendPython/unicorn/backend/`
- ✅ **Python 3.12**: Fully configured virtual environment
- ✅ **FastAPI**: Production-ready API application
- ✅ **LEAN Integration**: QuantConnect framework integrated
- ✅ **Database**: MySQL connections configured

### This Directory Purpose
This `/backend/` directory serves as a template for future microservices when the platform requires:
- Service decomposition for scalability
- Independent deployment of components
- Distributed architecture patterns
- Container orchestration (Kubernetes/Docker)

## Planned Microservices Architecture

### Service Decomposition (Future)
```
backend/
├── api/           # API Gateway service
├── ml/            # Machine Learning service
├── models/        # Data models service  
├── services/      # Business logic services
└── utils/         # Shared utility services
```

### Migration Path
1. **Current State**: Monolithic application in `/BackendPython/unicorn/backend/`
2. **Future State**: Microservices architecture in this directory
3. **Transition**: Gradual extraction of services as platform scales

## Development Guidelines

### When to Use This Directory
- Platform requires independent service scaling
- Team size grows beyond single development team
- Service-specific deployment needs arise
- Container orchestration becomes necessary

### When to Use BackendPython
- Current development and features ✅ **Use This**
- Single-team development
- Rapid prototyping and iteration
- Integrated development workflow

## Current Development Focus

### Active Backend Development
**Location**: `/BackendPython/unicorn/backend/`
**Status**: ✅ Ready for development
**Features**: FastAPI, LEAN integration, database connectivity

### Template Maintenance
**Location**: This directory (`/backend/`)
**Status**: Template structure maintained
**Purpose**: Future microservices planning

## Service Templates

### API Gateway Template
```python
# Future API gateway for microservices routing
# Load balancing and service discovery
# Authentication and authorization centralization
```

### ML Service Template  
```python
# Dedicated machine learning service
# Model training and inference isolation
# Scalable computation resources
```

### Data Service Template
```python
# Dedicated data processing service
# ETL pipeline management
# Real-time data streaming
```

## Migration Strategy

### Phase 1: Current Architecture ✅
- Monolithic FastAPI application
- Integrated LEAN framework
- Single database connection

### Phase 2: Service Preparation 📋
- Identify service boundaries
- Design API contracts
- Plan data architecture

### Phase 3: Microservices Transition 🔄
- Extract core services
- Implement service communication
- Deploy containerized services

## Documentation Links

- **Current Backend**: [BackendPython README](../BackendPython/README.md)
- **API Documentation**: [FastAPI Application](../BackendPython/unicorn/backend/api/README.md)
- **Database**: [Database Configuration](../database/README.md)
- **Infrastructure**: [WebFrontend README](../WebFrontend/README.md)