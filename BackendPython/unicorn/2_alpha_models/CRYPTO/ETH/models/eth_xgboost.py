"""
ETH XGBoost Model for CRYPTO

Gradient boosting model for ETH price prediction with ETH-specific optimizations.
"""

import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import sys
import os
from datetime import datetime

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))

from shared.model_framework import XGBoostModel
from shared.performance_tracker import ModelPerformanceTracker, ModelStage

class ETHXGBoostModel(XGBoostModel):
    """
    XGBoost-based prediction model for ETH.
    
    Uses gradient boosting with ETH-specific feature engineering.
    ETH typically requires different hyperparameters due to higher volatility.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        # ETH-specific XGBoost configuration
        default_config = {
            'n_estimators': 150,  # More trees for ETH complexity
            'max_depth': 8,  # Deeper trees for ETH patterns
            'learning_rate': 0.08,  # Slightly lower for better generalization
            'subsample': 0.9,  # Higher sampling for ETH
            'colsample_bytree': 0.9,  # Higher feature sampling
            'feature_windows': [5, 10, 20, 50, 100],  # More lookback windows
            'reg_alpha': 0.1,  # L1 regularization for ETH
            'reg_lambda': 1.0,  # L2 regularization
            'random_state': 42,
            'objective': 'reg:squarederror',
            'eval_metric': 'rmse'
        }
        
        if config:
            default_config.update(config)
            
        super().__init__('ETH', default_config)
        self.performance_tracker = ModelPerformanceTracker()
        
    def engineer_eth_features(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Engineer ETH-specific features for prediction.
        
        Args:
            data: OHLCV data
            
        Returns:
            Enhanced feature matrix
        """
        features = data.copy()
        
        # Price-based features
        features['price_change'] = features['Close'].pct_change()
        features['price_momentum'] = features['Close'].rolling(window=5).mean() / features['Close'].rolling(window=20).mean()
        
        # Volatility features (important for ETH)
        features['volatility_short'] = features['Close'].rolling(window=5).std()
        features['volatility_long'] = features['Close'].rolling(window=20).std()
        features['volatility_ratio'] = features['volatility_short'] / features['volatility_long']
        
        # Volume features (ETH volume patterns)
        features['volume_sma'] = features['Volume'].rolling(window=10).mean()
        features['volume_ratio'] = features['Volume'] / features['volume_sma']
        features['price_volume'] = features['Close'] * features['Volume']
        
        # Technical indicators for ETH
        features['rsi'] = self.calculate_rsi(features['Close'], period=14)
        features['macd'], features['macd_signal'] = self.calculate_macd(features['Close'])
        features['bb_upper'], features['bb_lower'] = self.calculate_bollinger_bands(features['Close'])
        
        # ETH-specific patterns
        features['intraday_return'] = (features['Close'] - features['Open']) / features['Open']
        features['overnight_return'] = (features['Open'] - features['Close'].shift(1)) / features['Close'].shift(1)
        features['true_range'] = np.maximum(
            features['High'] - features['Low'],
            np.maximum(
                np.abs(features['High'] - features['Close'].shift(1)),
                np.abs(features['Low'] - features['Close'].shift(1))
            )
        )
        
        # Time-based features (ETH shows different patterns)
        features['hour'] = features.index.hour if hasattr(features.index, 'hour') else 12
        features['day_of_week'] = features.index.dayofweek if hasattr(features.index, 'dayofweek') else 1
        features['month'] = features.index.month if hasattr(features.index, 'month') else 6
        
        return features
        
    def calculate_rsi(self, prices: pd.Series, period: int = 14) -> pd.Series:
        """Calculate Relative Strength Index."""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
        
    def calculate_macd(self, prices: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9):
        """Calculate MACD indicator."""
        exp1 = prices.ewm(span=fast).mean()
        exp2 = prices.ewm(span=slow).mean()
        macd = exp1 - exp2
        signal_line = macd.ewm(span=signal).mean()
        return macd, signal_line
        
    def calculate_bollinger_bands(self, prices: pd.Series, window: int = 20, num_std: float = 2):
        """Calculate Bollinger Bands."""
        rolling_mean = prices.rolling(window=window).mean()
        rolling_std = prices.rolling(window=window).std()
        upper_band = rolling_mean + (rolling_std * num_std)
        lower_band = rolling_mean - (rolling_std * num_std)
        return upper_band, lower_band
        
    def train_and_validate(self, data: pd.DataFrame, validation_split: float = 0.2) -> Dict[str, Any]:
        """
        Train model with validation split and performance tracking.
        
        Args:
            data: Historical price data
            validation_split: Fraction of data to use for validation
            
        Returns:
            Training and validation results
        """
        # Engineer ETH-specific features
        enhanced_data = self.engineer_eth_features(data)
        
        # Train model
        train_result = self.train(enhanced_data)
        
        if not train_result['success']:
            return train_result
        
        # Track performance
        model_id = f"{self.asset_name}_xgboost_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Get predictions for performance tracking
        X, y = self.prepare_data(enhanced_data)
        train_size = int(len(X) * 0.8)
        
        X_train = X.iloc[:train_size]
        y_train = y.iloc[:train_size]
        X_val = X.iloc[train_size:]
        y_val = y.iloc[train_size:]
        
        # Scale and predict
        X_train_scaled = self.scaler.transform(X_train)
        X_val_scaled = self.scaler.transform(X_val)
        
        train_pred = pd.Series(self.model.predict(X_train_scaled), index=y_train.index)
        val_pred = pd.Series(self.model.predict(X_val_scaled), index=y_val.index)
        
        # Track validation performance  
        self.performance_tracker.track_validation_performance(
            model_id=model_id,
            predictions=val_pred,
            actuals=y_val
        )
        
        # Feature importance analysis
        feature_importance = pd.Series(
            self.model.feature_importances_,
            index=X.columns
        ).sort_values(ascending=False)
        
        return {
            'success': True,
            'model_id': model_id,
            'train_result': train_result,
            'validation_predictions': val_pred,
            'validation_actuals': y_val,
            'feature_importance': feature_importance,
            'model_config': self.config
        }

def create_and_train_eth_xgboost(data: pd.DataFrame) -> ETHXGBoostModel:
    """
    Convenience function to create and train ETH XGBoost model.
    
    Args:
        data: Historical OHLCV data
        
    Returns:
        Trained XGBoost model
    """
    model = ETHXGBoostModel()
    result = model.train_and_validate(data)
    
    if result['success']:
        print(f"✅ ETH XGBoost model trained successfully")
        print(f"   Model ID: {result['model_id']}")
        if 'feature_importance' in result:
            print(f"   Top features: {list(result['feature_importance'].head(3).index)}")
    else:
        print(f"❌ ETH XGBoost model training failed: {result.get('error', 'Unknown error')}")
    
    return model

if __name__ == "__main__":
    # Example usage
    print("ETH XGBoost Model - Example Usage")
    print("=" * 40)
    
    # This would be replaced with real data loading
    dates = pd.date_range(start='2022-01-01', periods=365, freq='D')
    sample_data = pd.DataFrame({
        'Open': np.random.normal(3000, 150, 365),
        'High': np.random.normal(3100, 150, 365),
        'Low': np.random.normal(2900, 150, 365),
        'Close': np.random.normal(3000, 150, 365),
        'Volume': np.random.normal(1000000, 200000, 365)
    }, index=dates)
    
    model = create_and_train_eth_xgboost(sample_data)
    print(f"Model trained: {model.is_trained}")
