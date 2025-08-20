"""
SQLAlchemy ORM models for Unicorn Investing database schema.

This module defines the database structure for:
- User management and authentication
- Portfolio composition and metadata
- Market data and feature lists
- Allocation history and performance tracking

Migrated from: MySQL schema used by R scripts
"""

from sqlalchemy import Column, Integer, String, Float, DateTime, Text, Boolean, ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from datetime import datetime
from typing import List, Optional

Base = declarative_base()

class User(Base):
    """User authentication and profile information."""
    __tablename__ = 'unicorn_users'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    username = Column(String(50), unique=True, nullable=False)
    email = Column(String(320), unique=True, nullable=False)
    password_hash = Column(String(255), nullable=False)
    first_name = Column(String(100))
    last_name = Column(String(100))
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())
    is_active = Column(Boolean, default=True)
    
    # Relationships
    portfolios = relationship("Portfolio", back_populates="user")

class Portfolio(Base):
    """Portfolio definitions and metadata."""
    __tablename__ = 'unicorn_portfolios'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)  # User-defined portfolio ID
    symbol = Column(String(20), nullable=False)
    created_at = Column(DateTime, default=func.now())
    
    # Relationships
    user = relationship("User", back_populates="portfolios")
    
    def __repr__(self):
        return f"<Portfolio(userid={self.userid}, portfolioid={self.portfolioid}, symbol={self.symbol})>"

class PortfolioDetails(Base):
    """Portfolio performance metrics and metadata."""
    __tablename__ = 'unicorn_portfolios_details'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    best_performance = Column(Float)
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())
    
    def __repr__(self):
        return f"<PortfolioDetails(userid={self.userid}, portfolioid={self.portfolioid}, performance={self.best_performance})>"

class PortfolioAttributes(Base):
    """Portfolio configuration and settings."""
    __tablename__ = 'unicorn_portfolio_attributes'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    is_forex = Column(Boolean, default=False)
    risk_level = Column(String(20))
    description = Column(Text)
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

class BestFeatureList(Base):
    """GA-optimized feature selections for portfolios."""
    __tablename__ = 'unicorn_best_featurelist'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    symbol = Column(String(20), nullable=False)
    created_at = Column(DateTime, default=func.now())
    
    def __repr__(self):
        return f"<BestFeatureList(userid={self.userid}, portfolioid={self.portfolioid}, symbol={self.symbol})>"

class UniversalFeatureList(Base):
    """Master list of all available features for ML models."""
    __tablename__ = 'unicorn_universalfeaturelist_daily'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    symbol = Column(String(20), nullable=False, unique=True)
    description = Column(String(255))
    data_source = Column(String(50))
    is_active = Column(Boolean, default=True)
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

class AllocationHistory(Base):
    """Daily portfolio allocation decisions and history."""
    __tablename__ = 'unicorn_allocationhistory'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    symbol = Column(String(20), nullable=False)
    allocation = Column(Float, nullable=False)  # Percentage allocation (0.0 to 1.0)
    datetime = Column(DateTime, default=func.now())
    
    def __repr__(self):
        return f"<AllocationHistory(userid={self.userid}, portfolioid={self.portfolioid}, symbol={self.symbol}, allocation={self.allocation})>"

class MarketData(Base):
    """Historical market data for stocks and forex."""
    __tablename__ = 'market_data'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    symbol = Column(String(20), nullable=False)
    date = Column(DateTime, nullable=False)
    open_price = Column(Float)
    high_price = Column(Float)
    low_price = Column(Float)
    close_price = Column(Float)
    adjusted_close = Column(Float)
    volume = Column(Integer)
    data_source = Column(String(50), default='yfinance')
    created_at = Column(DateTime, default=func.now())
    
    def __repr__(self):
        return f"<MarketData(symbol={self.symbol}, date={self.date}, close={self.close_price})>"

class MLModel(Base):
    """Metadata for trained machine learning models."""
    __tablename__ = 'ml_models'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    model_type = Column(String(50), nullable=False)  # 'neural_network', 'genetic_algorithm'
    model_path = Column(String(500))  # File path to serialized model
    performance_score = Column(Float)
    hyperparameters = Column(Text)  # JSON string of model hyperparameters
    training_date = Column(DateTime, default=func.now())
    is_active = Column(Boolean, default=True)
    
    def __repr__(self):
        return f"<MLModel(userid={self.userid}, portfolioid={self.portfolioid}, type={self.model_type}, score={self.performance_score})>"

class TradeExecution(Base):
    """Trade execution records and audit trail."""
    __tablename__ = 'trade_executions'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    symbol = Column(String(20), nullable=False)
    action = Column(String(10), nullable=False)  # 'BUY', 'SELL'
    quantity = Column(Float, nullable=False)
    price = Column(Float, nullable=False)
    execution_time = Column(DateTime, default=func.now())
    model_recommendation = Column(Float)  # Original ML model recommendation
    actual_allocation = Column(Float)  # Actual executed allocation
    status = Column(String(20), default='PENDING')  # 'PENDING', 'EXECUTED', 'FAILED'
    
    def __repr__(self):
        return f"<TradeExecution(symbol={self.symbol}, action={self.action}, quantity={self.quantity}, price={self.price})>"

class PerformanceMetrics(Base):
    """Portfolio performance tracking over time."""
    __tablename__ = 'performance_metrics'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    userid = Column(Integer, ForeignKey('unicorn_users.id'), nullable=False)
    portfolioid = Column(Integer, nullable=False)
    date = Column(DateTime, nullable=False)
    portfolio_value = Column(Float, nullable=False)
    daily_return = Column(Float)
    cumulative_return = Column(Float)
    volatility = Column(Float)
    sharpe_ratio = Column(Float)
    max_drawdown = Column(Float)
    created_at = Column(DateTime, default=func.now())
    
    def __repr__(self):
        return f"<PerformanceMetrics(userid={self.userid}, portfolioid={self.portfolioid}, value={self.portfolio_value})>"

# Database utility functions
def create_all_tables(engine):
    """Create all tables in the database."""
    Base.metadata.create_all(engine)

def drop_all_tables(engine):
    """Drop all tables from the database."""
    Base.metadata.drop_all(engine)
