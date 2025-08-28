# Unicorn Backend Services

This directory contains the Python backend services for the Unicorn Investing Platform, separate from the LEAN framework.

## Directory Structure

```
backend/
├── README.md           # This file
├── api/               # FastAPI routes and endpoints
├── ml/                # Machine learning models and algorithms
├── models/            # Database models and schemas
├── services/          # Business logic and data processing
└── utils/             # Utility functions and helpers
```

## Purpose

This backend provides:
- RESTful API endpoints for frontend integration
- Machine learning algorithms for portfolio optimization
- Database models for financial data storage
- Business logic for investment analysis
- Utility functions for data processing

## Integration with LEAN

The unicorn backend integrates with the QuantConnect LEAN framework through:
- Custom algorithm interfaces in `/algorithms/`
- Integration layer in `/integrations/`
- Shared data models and services

## Development

All Python code follows the project coding standards and integrates with the virtual environment configured at the BackendPython level.
