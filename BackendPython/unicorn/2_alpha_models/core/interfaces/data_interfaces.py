"""
Data structure interfaces for the alpha models framework.

Defines standardized data structures used across all methodologies and assets.
"""

from abc import ABC
from typing import Dict, Any, List, Optional, Union
from datetime import datetime
import pandas as pd
from dataclasses import dataclass

@dataclass
class RawAssetData:
    """Raw asset data from data sources"""
    timestamp: pd.Series
    open: pd.Series
    high: pd.Series
    low: pd.Series
    close: pd.Series
    volume: pd.Series
    asset_class: str
    symbol: str
    source: str
    metadata: Dict[str, Any]

@dataclass 
class StandardizedData:
    """Standardized asset data after normalization"""
    timestamp: pd.Series
    open_price: pd.Series
    high_price: pd.Series
    low_price: pd.Series
    close_price: pd.Series
    volume: pd.Series
    asset_type: str
    symbol: str
    market_characteristics: 'MarketCharacteristics'
    normalization_metadata: Dict[str, Any]

@dataclass
class AssetData:
    """Processed asset data ready for model consumption"""
    data: pd.DataFrame
    features: Optional[pd.DataFrame]
    target: Optional[pd.Series]
    timestamps: pd.Series
    asset_class: str
    symbol: str
    timeframe: str
    metadata: Dict[str, Any]

@dataclass
class FeatureSet:
    """Feature set prepared by methodology"""
    features: pd.DataFrame
    target: Optional[pd.Series]
    feature_names: List[str]
    methodology: str
    asset_class: str
    symbol: str
    preparation_metadata: Dict[str, Any]

@dataclass
class ForecastResult:
    """Forecast result from methodology"""
    prediction: float
    confidence: float
    direction: str  # 'up', 'down', 'neutral'
    forecast_horizon: int  # periods ahead
    methodology: str
    timestamp: datetime
    metadata: Dict[str, Any]

@dataclass
class PerformanceMetrics:
    """Performance metrics for model validation"""
    r2_score: float
    mape: float  # Mean Absolute Percentage Error
    mae: float   # Mean Absolute Error
    rmse: float  # Root Mean Square Error
    sharpe_ratio: Optional[float]
    max_drawdown: Optional[float]
    hit_rate: Optional[float]  # Directional accuracy
    methodology: str
    asset_class: str
    symbol: str
    evaluation_period: Dict[str, datetime]
    additional_metrics: Dict[str, float]

@dataclass
class MarketCharacteristics:
    """Market characteristics for asset class"""
    market_hours: str  # '24/7', 'business_hours', 'exchange_hours'
    volatility_regime: str  # 'low', 'medium', 'high', 'extreme'
    liquidity_level: str  # 'high', 'medium', 'low'
    tick_size: float
    minimum_trade_size: float
    trading_costs: Dict[str, float]  # spread, commission, etc.
    market_impact_factor: float
    seasonal_patterns: List[str]
    correlation_factors: List[str]

@dataclass
class ConstrainedForecast:
    """Forecast with asset-specific constraints applied"""
    original_forecast: ForecastResult
    constrained_prediction: float
    constrained_confidence: float
    applied_constraints: List[str]
    risk_adjustments: Dict[str, float]
    trading_feasibility: Dict[str, Any]
    recommended_position_size: Optional[float]