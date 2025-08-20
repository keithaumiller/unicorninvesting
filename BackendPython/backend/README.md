# Backend API Structure (Future)

Modern Python backend API structure for migration from legacy R scripts.

## Purpose
- Target structure for Python-based backend services
- FastAPI-based REST API endpoints
- SQLAlchemy ORM for database operations
- Modern Python architecture replacing R analytics

## Directory Structure

### api/
**Purpose**: FastAPI route definitions and endpoint handlers
**Future Contents**:
- `routes.py` - Main API route definitions
- `auth.py` - Authentication and authorization endpoints
- `portfolio.py` - Portfolio management endpoints
- `market_data.py` - Market data API endpoints
- `trading.py` - Trade execution and order management

### models/
**Purpose**: SQLAlchemy ORM models for database entities
**Future Contents**:
- `user.py` - User authentication and profile models
- `portfolio.py` - Portfolio and holding models
- `market_data.py` - Stock and forex data models
- `trade.py` - Trade execution and history models
- `ml_models.py` - Machine learning model metadata

### services/
**Purpose**: Business logic and service layer implementations
**Future Contents**:
- `portfolio_service.py` - Portfolio management business logic
- `trading_service.py` - Trade execution and order management
- `ml_service.py` - Machine learning model training and prediction
- `market_data_service.py` - Data collection and processing
- `risk_service.py` - Risk management and analytics

### ml/
**Purpose**: Machine learning models and training infrastructure
**Future Contents**:
- `model_trainer.py` - Neural network training (replacing R FCNN4R)
- `genetic_algorithm.py` - Feature selection optimization (replacing GA_parameter_explorer.R)
- `model_evaluator.py` - Performance evaluation and backtesting
- `feature_engineering.py` - Data preprocessing and feature creation

### utils/
**Purpose**: Utility functions and helper modules
**Future Contents**:
- `helpers.py` - Common utility functions
- `logger.py` - Logging configuration and utilities
- `config.py` - Application configuration management
- `database.py` - Database connection and session management

## Migration Strategy
1. **Phase 1**: Set up FastAPI application structure and basic endpoints
2. **Phase 2**: Migrate R data processing functions to Python/pandas
3. **Phase 3**: Convert FCNN4R neural networks to TensorFlow/scikit-learn
4. **Phase 4**: Implement GA optimization using DEAP or similar Python library
5. **Phase 5**: Create web APIs for frontend Drupal integration

## Technology Stack
- **FastAPI**: Modern Python web framework for APIs
- **SQLAlchemy**: Database ORM and migrations
- **pandas/numpy**: Data processing and analysis
- **scikit-learn/TensorFlow**: Machine learning and neural networks
- **Celery**: Asynchronous task processing for model training
- **Redis**: Caching and session management
