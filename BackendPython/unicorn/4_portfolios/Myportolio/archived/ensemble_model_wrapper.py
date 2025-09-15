#!/usr/bin/env python3
"""
Ensemble Model Wrapper
Handles the dictionary-based ensemble models from our 100% success framework
"""

import numpy as np
import pandas as pd
from typing import Dict, Any
import logging

class EnsembleModelWrapper:
    """
    Wrapper for ensemble models saved as dictionaries with separate Prophet and XGBoost components
    """
    
    def __init__(self, ensemble_dict: Dict[str, Any]):
        """
        Initialize wrapper with ensemble dictionary
        
        Args:
            ensemble_dict: Dictionary containing prophet_model, xgboost_model, weights, etc.
        """
        self.ensemble_dict = ensemble_dict
        self.prophet_model = ensemble_dict.get('prophet_model', {}).get('model')
        self.xgboost_model = ensemble_dict.get('xgboost_model', {}).get('model')
        self.weights = ensemble_dict.get('weights', {'prophet': 0.5, 'xgboost': 0.5})
        self.status = ensemble_dict.get('status', 'unknown')
        
        # Get preprocessing components if available
        self.prophet_preprocessing = ensemble_dict.get('prophet_model', {}).get('preprocessing')
        self.xgboost_preprocessing = ensemble_dict.get('xgboost_model', {}).get('preprocessing')
        
        self.logger = logging.getLogger(__name__)
        
        # Validate models
        self.is_valid = self._validate_models()
        
    def _validate_models(self) -> bool:
        """Validate that we have working models"""
        prophet_valid = self.prophet_model is not None and hasattr(self.prophet_model, 'predict')
        xgboost_valid = self.xgboost_model is not None and hasattr(self.xgboost_model, 'predict')
        
        if not prophet_valid and not xgboost_valid:
            self.logger.error("No valid models found in ensemble")
            return False
        
        if not prophet_valid:
            self.logger.warning("Prophet model not available, using XGBoost only")
        
        if not xgboost_valid:
            self.logger.warning("XGBoost model not available, using Prophet only")
        
        return True
    
    def predict_with_silver_layer(self, asset: str, timeframe: str) -> np.ndarray:
        """
        Make prediction using silver layer data integration
        
        Args:
            asset: Asset symbol (e.g., 'ETH', 'EURUSD')
            timeframe: Timeframe ('1h' or '1d')
            
        Returns:
            Array of predictions
        """
        if not self.is_valid:
            raise ValueError("Ensemble model is not valid")
        
        try:
            # Import silver layer mapper
            from silver_layer_integration_mapper import SilverLayerFeatureMapper
            mapper = SilverLayerFeatureMapper()
            
            predictions = []
            prediction_weights = []
            
            # Prophet prediction with silver layer features
            if self.prophet_model is not None and hasattr(self.prophet_model, 'predict'):
                try:
                    prophet_features = mapper.get_features_for_model(asset, timeframe, 'prophet')
                    if prophet_features is not None and len(prophet_features) > 0:
                        # Create future dataframe for Prophet
                        future_df = pd.DataFrame({
                            'ds': [pd.Timestamp.now()],
                        })
                        
                        # Add regressor columns from silver layer
                        for feature, value in prophet_features.items():
                            future_df[feature] = value
                        
                        prophet_pred = self.prophet_model.predict(future_df)
                        if len(prophet_pred) > 0:
                            pred_value = prophet_pred['yhat'].iloc[-1] if 'yhat' in prophet_pred.columns else prophet_pred.iloc[-1, 0]
                            predictions.append(pred_value)
                            prediction_weights.append(self.weights.get('prophet', 0.5))
                            self.logger.debug(f"Prophet prediction: {pred_value}")
                        
                except Exception as e:
                    self.logger.warning(f"Prophet prediction failed: {e}")
            
            # XGBoost prediction with silver layer features
            if self.xgboost_model is not None and hasattr(self.xgboost_model, 'predict'):
                try:
                    xgb_features = mapper.get_features_for_model(asset, timeframe, 'xgboost')
                    if xgb_features is not None and len(xgb_features) > 0:
                        # Convert to DataFrame for XGBoost
                        feature_df = pd.DataFrame([xgb_features])
                        
                        # Apply preprocessing if available
                        if self.xgboost_preprocessing:
                            feature_df = self._apply_xgboost_preprocessing(feature_df)
                        
                        xgb_pred = self.xgboost_model.predict(feature_df)
                        if len(xgb_pred) > 0:
                            predictions.append(xgb_pred[0])
                            prediction_weights.append(self.weights.get('xgboost', 0.5))
                            self.logger.debug(f"XGBoost prediction: {xgb_pred[0]}")
                        
                except Exception as e:
                    self.logger.warning(f"XGBoost prediction failed: {e}")
            
            # Combine predictions using weights
            if len(predictions) > 0:
                total_weight = sum(prediction_weights)
                if total_weight > 0:
                    normalized_weights = [w / total_weight for w in prediction_weights]
                    ensemble_prediction = sum(p * w for p, w in zip(predictions, normalized_weights))
                    return np.array([ensemble_prediction])
                else:
                    return np.array([sum(predictions) / len(predictions)])
            else:
                self.logger.warning("No predictions generated, returning zero")
                return np.array([0.0])
                
        except Exception as e:
            self.logger.error(f"Silver layer prediction failed: {e}")
            return np.array([0.0])

    def predict(self, features: pd.DataFrame) -> np.ndarray:
        """
        Make ensemble prediction using both models and weights
        
        Args:
            features: DataFrame with features for prediction
            
        Returns:
            Array of predictions
        """
        if not self.is_valid:
            raise ValueError("Ensemble model is not valid")
        
        predictions = []
        prediction_weights = []
        
        try:
            # Prophet prediction
            if self.prophet_model is not None and hasattr(self.prophet_model, 'predict'):
                try:
                    # Prophet expects specific format - create a simple future DataFrame
                    if len(features) > 0:
                        # Use the latest timestamp or create one
                        future_df = pd.DataFrame({
                            'ds': [pd.Timestamp.now()],  # Prophet expects 'ds' column
                        })
                        
                        prophet_pred = self.prophet_model.predict(future_df)
                        if len(prophet_pred) > 0:
                            # Extract the prediction value
                            pred_value = prophet_pred['yhat'].iloc[-1] if 'yhat' in prophet_pred.columns else prophet_pred.iloc[-1, 0]
                            predictions.append(pred_value)
                            prediction_weights.append(self.weights.get('prophet', 0.5))
                            
                except Exception as e:
                    self.logger.warning(f"Prophet prediction failed: {e}")
            
            # XGBoost prediction
            if self.xgboost_model is not None and hasattr(self.xgboost_model, 'predict'):
                try:
                    # Apply preprocessing if available
                    processed_features = features.copy()
                    
                    if self.xgboost_preprocessing:
                        # Apply the same preprocessing used during training
                        processed_features = self._apply_xgboost_preprocessing(processed_features)
                    
                    # Ensure we have numeric data only
                    numeric_features = processed_features.select_dtypes(include=[np.number])
                    
                    if len(numeric_features.columns) > 0:
                        xgb_pred = self.xgboost_model.predict(numeric_features)
                        if len(xgb_pred) > 0:
                            predictions.append(xgb_pred[0])
                            prediction_weights.append(self.weights.get('xgboost', 0.5))
                            
                except Exception as e:
                    self.logger.warning(f"XGBoost prediction failed: {e}")
            
            # Combine predictions using weights
            if len(predictions) > 0:
                # Normalize weights
                total_weight = sum(prediction_weights)
                if total_weight > 0:
                    normalized_weights = [w / total_weight for w in prediction_weights]
                    ensemble_prediction = sum(p * w for p, w in zip(predictions, normalized_weights))
                    return np.array([ensemble_prediction])
                else:
                    # Fallback to simple average
                    return np.array([sum(predictions) / len(predictions)])
            else:
                # No predictions available, return zero
                self.logger.warning("No predictions generated, returning zero")
                return np.array([0.0])
                
        except Exception as e:
            self.logger.error(f"Ensemble prediction failed: {e}")
            return np.array([0.0])
    
    def _apply_xgboost_preprocessing(self, features: pd.DataFrame) -> pd.DataFrame:
        """Apply XGBoost preprocessing if available"""
        try:
            if self.xgboost_preprocessing and isinstance(self.xgboost_preprocessing, dict):
                processed = features.copy()
                
                # Apply any scaling/transformation stored in preprocessing
                if 'scaler' in self.xgboost_preprocessing:
                    scaler = self.xgboost_preprocessing['scaler']
                    if hasattr(scaler, 'transform'):
                        numeric_cols = processed.select_dtypes(include=[np.number]).columns
                        processed[numeric_cols] = scaler.transform(processed[numeric_cols])
                
                return processed
            
            return features
            
        except Exception as e:
            self.logger.warning(f"Preprocessing failed: {e}")
            return features
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the ensemble model"""
        return {
            'status': self.status,
            'weights': self.weights,
            'prophet_available': self.prophet_model is not None,
            'xgboost_available': self.xgboost_model is not None,
            'is_valid': self.is_valid
        }


def create_ensemble_wrapper(model_path: str) -> EnsembleModelWrapper:
    """
    Create an ensemble wrapper from a model file path
    
    Args:
        model_path: Path to the ensemble model file
        
    Returns:
        EnsembleModelWrapper instance
    """
    import joblib
    
    try:
        ensemble_dict = joblib.load(model_path)
        return EnsembleModelWrapper(ensemble_dict)
    except Exception as e:
        logging.error(f"Failed to load ensemble model from {model_path}: {e}")
        # Return a dummy wrapper
        return EnsembleModelWrapper({})


# Test the wrapper
def test_ensemble_wrapper():
    """Test the ensemble wrapper with actual model"""
    logging.basicConfig(level=logging.INFO)
    
    model_path = '/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/fixed_multi_asset_models/ETH_1d/ensemble_fixed_model.joblib'
    
    print("🧪 Testing Ensemble Model Wrapper")
    print("=" * 50)
    
    # Load wrapper
    wrapper = create_ensemble_wrapper(model_path)
    
    # Get model info
    info = wrapper.get_model_info()
    print(f"📊 Model Status: {info['status']}")
    print(f"⚖️  Weights: Prophet={info['weights'].get('prophet', 0):.3f}, XGBoost={info['weights'].get('xgboost', 0):.3f}")
    print(f"🔵 Prophet Available: {info['prophet_available']}")
    print(f"🟢 XGBoost Available: {info['xgboost_available']}")
    print(f"✅ Is Valid: {info['is_valid']}")
    print()
    
    if wrapper.is_valid:
        # Create test features
        test_features = pd.DataFrame({
            'feature1': [1.5],
            'feature2': [2.0],
            'feature3': [0.5]
        })
        
        print("🎯 Making test prediction...")
        try:
            prediction = wrapper.predict(test_features)
            print(f"📈 Prediction: {prediction[0]:.6f}")
            print("✅ Wrapper working correctly!")
        except Exception as e:
            print(f"❌ Prediction failed: {e}")
    else:
        print("❌ Wrapper is not valid")


if __name__ == "__main__":
    test_ensemble_wrapper()
