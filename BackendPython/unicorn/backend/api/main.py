"""
Main FastAPI application for the Unicorn Investing Platform
"""
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import uvicorn
from datetime import datetime
import sys
import os

# Add the backend directory to the Python path
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from utils.config import config

# Create FastAPI application
app = FastAPI(
    title="Unicorn Investing Platform API",
    description="RESTful API for the Unicorn Investing Platform with LEAN integration",
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Configure appropriately for production
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "message": "Unicorn Investing Platform API",
        "version": "1.0.0",
        "timestamp": datetime.utcnow().isoformat(),
        "status": "operational"
    }


@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "timestamp": datetime.utcnow().isoformat(),
        "services": {
            "api": "operational",
            "database": "checking...",  # TODO: Add database health check
            "lean_integration": "checking..."  # TODO: Add LEAN health check
        }
    }


@app.get("/config")
async def get_config():
    """Get configuration information (non-sensitive data only)"""
    return {
        "api": {
            "host": config.api.host,
            "port": config.api.port,
            "debug": config.api.debug
        },
        "database": {
            "host": config.database.host,
            "port": config.database.port,
            "database": config.database.database
        },
        "lean": {
            "lean_path": config.lean.lean_path,
            "data_path": config.lean.data_path,
            "algorithm_path": config.lean.algorithm_path
        }
    }


# Error handlers
@app.exception_handler(HTTPException)
async def http_exception_handler(request, exc):
    return JSONResponse(
        status_code=exc.status_code,
        content={"message": exc.detail, "timestamp": datetime.utcnow().isoformat()}
    )


@app.exception_handler(Exception)
async def general_exception_handler(request, exc):
    return JSONResponse(
        status_code=500,
        content={
            "message": "Internal server error",
            "timestamp": datetime.utcnow().isoformat()
        }
    )


if __name__ == "__main__":
    uvicorn.run(
        "main:app",
        host=config.api.host,
        port=config.api.port,
        reload=config.api.debug
    )
