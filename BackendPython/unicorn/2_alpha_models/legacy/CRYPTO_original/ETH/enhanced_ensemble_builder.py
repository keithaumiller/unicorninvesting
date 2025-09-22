#!/usr/bin/env python3
"""
Enhanced Ensemble Builder with Leak-Free Validation
==================================================

Production-ready ensemble model builder eliminating all overfitting patterns
identified in Prophet/XGBoost analysis. Implements proper validation methodology
for combining component models with realistic performance expectations.

Key Features:
1. Leak-free component validation before ensemble integration
2. Proper temporal train/validation/test splits for financial time series
3. Cross-validation based ensemble weight calculation  
4. Realistic performance expectations (R² -0.02 to +0.15)
5. Component overfitting screening and rejection
6. Independent ensemble validation on held-out data

Author: Unicorn Investing Platform
Date: January 2025
Purpose: Build production-ready ensemble models with honest performance assessment
"""

import pandas as pd
import numpy as np
import sqlite3
import logging
import json
import pickle
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any
import warnings
warnings.filterwarnings('ignore')

# Import validation frameworks
try:
    import sys
    sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH')
    from crypto_model_validator import CryptoModelValidator
    from enhanced_crypto_prophet_builder import EnhancedCryptoProphetBuilder
    from enhanced_xgboost_builder import EnhancedXGBoostBuilder
except ImportError as e:
    print(f"Warning: Could not import validation frameworks: {e}")

# ML imports with graceful fallback
try:
    from sklearn.metrics import mean_absolute_error, r2_score, mean_squared_error
    from sklearn.model_selection import TimeSeriesSplit
    sklearn_available = True
except ImportError:
    sklearn_available = False

try:
    import xgboost as xgb
    xgboost_available = True
except ImportError:
    xgboost_available = False

try:
    from prophet import Prophet
    prophet_available = True
except ImportError:
    prophet_available = False

class EnhancedEnsembleBuilder:
    """
    Enhanced ensemble builder with comprehensive overfitting elimination.
    
    Implements leak-free ensemble construction methodology:
    1. Independent component validation
    2. Proper temporal data splitting  
    3. Cross-validation based weight calculation
    4. Realistic performance assessment
    5. Component overfitting screening
    """
    
    def __init__(self, models_dir: str = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH"):
        """Initialize the enhanced ensemble builder."""
        self.models_dir = Path(models_dir)
        self.ensemble_dir = self.models_dir / "leak_free_ensembles"
        self.ensemble_dir.mkdir(exist_ok=True)
        
        # Setup logging
        log_file = self.ensemble_dir / f"ensemble_builder_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
        
        # Realistic performance thresholds for financial ensemble models
        self.performance_thresholds = {
            'max_realistic_ensemble_r2': 0.15,  # Maximum realistic ensemble R²
            'min_ensemble_improvement': 0.05,   # Minimum improvement over best component
            'max_component_r2': 0.10,           # Maximum acceptable component R²
            'min_component_r2': -0.05,          # Minimum acceptable component R²
            'overfitting_threshold': 0.20,      # Clear overfitting indicator
            'component_stability_threshold': 0.03,  # Component performance stability
            'ensemble_stability_threshold': 0.02    # Ensemble performance stability
        }
        
        # Data splitting configuration for financial time series
        self.data_split_config = {
            'training_ratio': 0.70,    # 70% for component training
            'validation_ratio': 0.20,  # 20% for component validation & weight calculation
            'test_ratio': 0.10,        # 10% for independent ensemble evaluation
            'min_observations': 100,    # Minimum observations per split
            'temporal_order': True      # Maintain chronological order
        }
        
        # Initialize component builders
        self.component_builders = {}
        if prophet_available:
            try:
                self.component_builders['prophet'] = EnhancedCryptoProphetBuilder()
                self.logger.info("Prophet component builder initialized")
            except Exception as e:
                self.logger.warning(f"Could not initialize Prophet builder: {e}")
        
        if xgboost_available and sklearn_available:
            try:
                self.component_builders['xgboost'] = EnhancedXGBoostBuilder()
                self.logger.info("XGBoost component builder initialized")
            except Exception as e:
                self.logger.warning(f"Could not initialize XGBoost builder: {e}")
        
        self.logger.info("Enhanced Ensemble Builder initialized")
        self.logger.info(f"Performance thresholds: {self.performance_thresholds}")
        self.logger.info(f"Available components: {list(self.component_builders.keys())}")

    def create_temporal_splits(self, data: pd.DataFrame) -> Dict[str, pd.DataFrame]:
        """
        Create proper temporal splits for financial time series.
        
        Returns:
            Dict with 'train', 'validation', 'test' DataFrames
        """
        if len(data) < self.data_split_config['min_observations']:
            raise ValueError(f"Insufficient data: {len(data)} < {self.data_split_config['min_observations']}")
        
        # Ensure data is sorted by timestamp
        data = data.sort_index()
        
        # Calculate split points
        n_total = len(data)
        n_train = int(n_total * self.data_split_config['training_ratio'])
        n_val = int(n_total * self.data_split_config['validation_ratio'])
        n_test = n_total - n_train - n_val
        
        # Ensure minimum observations per split
        min_obs = self.data_split_config['min_observations'] // 3
        if n_train < min_obs or n_val < min_obs or n_test < min_obs:
            raise ValueError(f"Insufficient data for proper splitting. Need at least {min_obs*3} observations")
        
        # Create splits maintaining temporal order
        train_data = data.iloc[:n_train].copy()
        val_data = data.iloc[n_train:n_train+n_val].copy()
        test_data = data.iloc[n_train+n_val:].copy()
        
        splits = {
            'train': train_data,
            'validation': val_data,
            'test': test_data
        }
        
        self.logger.info(f"Created temporal splits: Train={len(train_data)}, Val={len(val_data)}, Test={len(test_data)}")
        
        return splits

    def validate_component_model(self, model_type: str, model, data_splits: Dict, symbol: str) -> Dict:
        """
        Validate individual component model for overfitting and realistic performance.
        
        Returns:
            Validation metrics and overfitting assessment
        """
        validation_results = {
            'model_type': model_type,
            'symbol': symbol,
            'is_valid': False,
            'overfitting_detected': False,
            'performance_metrics': {},
            'validation_details': {},
            'rejection_reasons': []
        }
        
        try:
            train_data = data_splits['train']
            val_data = data_splits['validation']
            
            # Generate predictions for validation data
            if model_type == 'prophet' and model:
                # Prophet validation
                future_df = pd.DataFrame({'ds': val_data.index})
                val_predictions = model.predict(future_df)['yhat'].values
                val_actual = val_data['price'].values
                
            elif model_type == 'xgboost' and model:
                # XGBoost validation (using leak-free features)
                builder = self.component_builders['xgboost']
                val_features = builder.create_leak_free_features(val_data, symbol)
                feature_columns = [col for col in val_features.columns if col not in ['price', 'timestamp']]
                X_val = val_features[feature_columns].fillna(method='ffill').fillna(0)
                val_predictions = model.predict(X_val)
                val_actual = val_data['price'].values[:len(val_predictions)]
                
            else:
                validation_results['rejection_reasons'].append(f"Unknown model type: {model_type}")
                return validation_results
            
            # Align predictions and actuals
            min_len = min(len(val_predictions), len(val_actual))
            val_predictions = val_predictions[:min_len]
            val_actual = val_actual[:min_len]
            
            if min_len < 10:  # Need minimum observations for validation
                validation_results['rejection_reasons'].append("Insufficient validation predictions")
                return validation_results
            
            # Calculate validation metrics
            mape = np.mean(np.abs((val_actual - val_predictions) / val_actual)) * 100
            mae = np.mean(np.abs(val_actual - val_predictions))
            rmse = np.sqrt(np.mean((val_actual - val_predictions) ** 2))
            r2 = r2_score(val_actual, val_predictions) if sklearn_available else 0
            
            # Calculate additional stability metrics
            prediction_volatility = np.std(val_predictions) / np.mean(val_predictions)
            actual_volatility = np.std(val_actual) / np.mean(val_actual)
            volatility_ratio = prediction_volatility / actual_volatility if actual_volatility > 0 else 0
            
            validation_results['performance_metrics'] = {
                'validation_r2': r2,
                'validation_mape': mape,
                'validation_mae': mae,
                'validation_rmse': rmse,
                'prediction_volatility': prediction_volatility,
                'volatility_ratio': volatility_ratio,
                'validation_observations': min_len
            }
            
            # Overfitting detection
            overfitting_flags = []
            
            # Flag 1: Unrealistic R² performance
            if r2 > self.performance_thresholds['max_component_r2']:
                overfitting_flags.append(f"R² too high: {r2:.4f} > {self.performance_thresholds['max_component_r2']}")
                validation_results['overfitting_detected'] = True
            
            # Flag 2: Very low MAPE (suspicious for financial data)
            if mape < 1.0:
                overfitting_flags.append(f"MAPE suspiciously low: {mape:.2f}% < 1.0%")
                validation_results['overfitting_detected'] = True
            
            # Flag 3: Very high MAPE (poor performance)
            if mape > 50.0:
                overfitting_flags.append(f"MAPE too high: {mape:.2f}% > 50.0%")
            
            # Flag 4: Extreme volatility mismatch
            if volatility_ratio < 0.1 or volatility_ratio > 10.0:
                overfitting_flags.append(f"Volatility mismatch: {volatility_ratio:.2f}")
            
            # Flag 5: Negative R² (worse than naive prediction)
            if r2 < self.performance_thresholds['min_component_r2']:
                overfitting_flags.append(f"R² too low: {r2:.4f} < {self.performance_thresholds['min_component_r2']}")
            
            validation_results['validation_details']['overfitting_flags'] = overfitting_flags
            
            # Component acceptance criteria
            is_valid = (
                not validation_results['overfitting_detected'] and
                self.performance_thresholds['min_component_r2'] <= r2 <= self.performance_thresholds['max_component_r2'] and
                1.0 <= mape <= 50.0 and
                0.1 <= volatility_ratio <= 10.0
            )
            
            validation_results['is_valid'] = is_valid
            
            if not is_valid:
                if validation_results['overfitting_detected']:
                    validation_results['rejection_reasons'].append("Overfitting detected")
                if r2 > self.performance_thresholds['max_component_r2']:
                    validation_results['rejection_reasons'].append(f"R² too high: {r2:.4f}")
                if r2 < self.performance_thresholds['min_component_r2']:
                    validation_results['rejection_reasons'].append(f"R² too low: {r2:.4f}")
                if mape > 50.0:
                    validation_results['rejection_reasons'].append(f"MAPE too high: {mape:.2f}%")
                if mape < 1.0:
                    validation_results['rejection_reasons'].append(f"MAPE suspiciously low: {mape:.2f}%")
            
            self.logger.info(f"{model_type} validation: R²={r2:.4f}, MAPE={mape:.2f}%, Valid={is_valid}")
            
        except Exception as e:
            validation_results['rejection_reasons'].append(f"Validation error: {str(e)}")
            self.logger.error(f"Component validation failed for {model_type}: {e}")
        
        return validation_results

    def build_component_models(self, data_splits: Dict, symbol: str) -> Dict:
        """
        Build and validate component models using leak-free methodology.
        
        Returns:
            Dict of validated component models and their metrics
        """
        component_results = {
            'valid_components': {},
            'rejected_components': {},
            'validation_summary': {
                'total_attempted': 0,
                'total_valid': 0,
                'rejection_rate': 0
            }
        }
        
        train_data = data_splits['train']
        
        for model_type, builder in self.component_builders.items():
            self.logger.info(f"Building {model_type} component for {symbol}")
            component_results['validation_summary']['total_attempted'] += 1
            
            try:
                # Build component model using only training data
                if model_type == 'prophet':
                    model_result = builder.build_prophet_model(train_data, symbol)
                    if model_result and 'model' in model_result:
                        model = model_result['model']
                        build_metrics = model_result.get('metrics', {})
                    else:
                        model = None
                        build_metrics = {}
                
                elif model_type == 'xgboost':
                    model_result = builder.build_xgboost_model(train_data, symbol)
                    if model_result and 'model' in model_result:
                        model = model_result['model']
                        build_metrics = model_result.get('metrics', {})
                    else:
                        model = None
                        build_metrics = {}
                
                else:
                    self.logger.warning(f"Unknown model type: {model_type}")
                    continue
                
                if model is None:
                    component_results['rejected_components'][model_type] = {
                        'reason': 'Model building failed',
                        'build_metrics': build_metrics
                    }
                    continue
                
                # Validate component model on independent validation data
                validation_result = self.validate_component_model(model_type, model, data_splits, symbol)
                
                if validation_result['is_valid']:
                    # Component passed validation
                    component_results['valid_components'][model_type] = {
                        'model': model,
                        'build_metrics': build_metrics,
                        'validation_metrics': validation_result['performance_metrics'],
                        'validation_details': validation_result['validation_details']
                    }
                    component_results['validation_summary']['total_valid'] += 1
                    self.logger.info(f"✅ {model_type} component validated successfully")
                
                else:
                    # Component failed validation
                    component_results['rejected_components'][model_type] = {
                        'reason': '; '.join(validation_result['rejection_reasons']),
                        'build_metrics': build_metrics,
                        'validation_metrics': validation_result['performance_metrics'],
                        'overfitting_detected': validation_result['overfitting_detected']
                    }
                    self.logger.warning(f"❌ {model_type} component rejected: {'; '.join(validation_result['rejection_reasons'])}")
            
            except Exception as e:
                component_results['rejected_components'][model_type] = {
                    'reason': f'Component building error: {str(e)}',
                    'build_metrics': {},
                    'validation_metrics': {}
                }
                self.logger.error(f"❌ {model_type} component building failed: {e}")
        
        # Calculate summary statistics
        total_attempted = component_results['validation_summary']['total_attempted']
        total_valid = component_results['validation_summary']['total_valid']
        component_results['validation_summary']['rejection_rate'] = (
            100 * (total_attempted - total_valid) / total_attempted if total_attempted > 0 else 0
        )
        
        self.logger.info(f"Component building complete: {total_valid}/{total_attempted} valid ({100*total_valid/total_attempted:.1f}% success rate)")
        
        return component_results

    def calculate_ensemble_weights(self, valid_components: Dict, data_splits: Dict) -> Dict:
        """
        Calculate ensemble weights using cross-validation on validation data.
        
        Uses validation performance to determine optimal component weights.
        """
        if len(valid_components) < 2:
            return {"error": "Need at least 2 valid components for ensemble"}
        
        val_data = data_splits['validation']
        component_names = list(valid_components.keys())
        
        # Get validation predictions for each component
        validation_predictions = {}
        validation_errors = {}
        
        for comp_name, comp_data in valid_components.items():
            model = comp_data['model']
            
            try:
                if comp_name == 'prophet':
                    future_df = pd.DataFrame({'ds': val_data.index})
                    predictions = model.predict(future_df)['yhat'].values
                    
                elif comp_name == 'xgboost':
                    builder = self.component_builders['xgboost']
                    val_features = builder.create_leak_free_features(val_data, "ETH")  # Default symbol
                    feature_columns = [col for col in val_features.columns if col not in ['price', 'timestamp']]
                    X_val = val_features[feature_columns].fillna(method='ffill').fillna(0)
                    predictions = model.predict(X_val)
                
                else:
                    continue
                
                # Align with actual values
                actual_values = val_data['price'].values
                min_len = min(len(predictions), len(actual_values))
                predictions = predictions[:min_len]
                actual_values = actual_values[:min_len]
                
                # Calculate validation error (RMSE)
                error = np.sqrt(np.mean((actual_values - predictions) ** 2))
                
                validation_predictions[comp_name] = predictions
                validation_errors[comp_name] = error
                
            except Exception as e:
                self.logger.error(f"Error calculating validation predictions for {comp_name}: {e}")
                continue
        
        if len(validation_errors) < 2:
            return {"error": "Insufficient validation predictions for weight calculation"}
        
        # Calculate weights using softmax of inverse errors
        # Better performing models (lower error) get higher weights
        errors = np.array(list(validation_errors.values()))
        
        # Avoid division by zero and numerical instability
        min_error = np.min(errors)
        if min_error <= 0:
            min_error = 1e-6
            errors = np.maximum(errors, min_error)
        
        # Inverse errors (better performance = higher value)
        inverse_errors = 1.0 / errors
        
        # Softmax normalization
        exp_inverse = np.exp(inverse_errors - np.max(inverse_errors))  # Numerical stability
        weights = exp_inverse / np.sum(exp_inverse)
        
        # Create weight dictionary
        weight_dict = {
            comp_name: float(weight) 
            for comp_name, weight in zip(component_names, weights)
        }
        
        # Fallback to equal weights if weights are too extreme
        max_weight = np.max(weights)
        min_weight = np.min(weights)
        
        if max_weight / min_weight > 10.0:  # Too extreme, use equal weights
            self.logger.warning(f"Extreme weight ratio ({max_weight/min_weight:.2f}), using equal weights")
            equal_weight = 1.0 / len(component_names)
            weight_dict = {comp_name: equal_weight for comp_name in component_names}
        
        weight_details = {
            'weights': weight_dict,
            'validation_errors': validation_errors,
            'weight_calculation_method': 'softmax_inverse_errors',
            'weight_stability': np.std(list(weight_dict.values())),
            'dominant_component': max(weight_dict.keys(), key=lambda k: weight_dict[k])
        }
        
        self.logger.info(f"Ensemble weights calculated: {weight_dict}")
        return weight_details

    def create_ensemble_predictions(self, valid_components: Dict, weights: Dict, data: pd.DataFrame, symbol: str) -> Tuple[np.ndarray, Dict]:
        """
        Create ensemble predictions using weighted component predictions.
        
        Returns:
            Tuple of (predictions, prediction_details)
        """
        component_predictions = {}
        prediction_details = {
            'components_used': [],
            'prediction_shape_info': {},
            'alignment_details': {}
        }
        
        # Generate predictions from each component
        for comp_name, comp_data in valid_components.items():
            if comp_name not in weights:
                continue
                
            model = comp_data['model']
            weight = weights[comp_name]
            
            try:
                if comp_name == 'prophet':
                    future_df = pd.DataFrame({'ds': data.index})
                    predictions = model.predict(future_df)['yhat'].values
                    
                elif comp_name == 'xgboost':
                    builder = self.component_builders['xgboost']
                    features = builder.create_leak_free_features(data, symbol)
                    feature_columns = [col for col in features.columns if col not in ['price', 'timestamp']]
                    X = features[feature_columns].fillna(method='ffill').fillna(0)
                    predictions = model.predict(X)
                
                else:
                    continue
                
                component_predictions[comp_name] = predictions
                prediction_details['components_used'].append(comp_name)
                prediction_details['prediction_shape_info'][comp_name] = len(predictions)
                
            except Exception as e:
                self.logger.error(f"Error generating predictions for {comp_name}: {e}")
                continue
        
        if not component_predictions:
            raise ValueError("No component predictions generated")
        
        # Align all predictions to the same length
        min_length = min(len(preds) for preds in component_predictions.values())
        aligned_predictions = {
            comp: preds[:min_length] 
            for comp, preds in component_predictions.items()
        }
        
        prediction_details['alignment_details']['final_length'] = min_length
        prediction_details['alignment_details']['original_lengths'] = {
            comp: len(preds) for comp, preds in component_predictions.items()
        }
        
        # Create weighted ensemble predictions
        ensemble_predictions = np.zeros(min_length)
        
        for comp_name, predictions in aligned_predictions.items():
            weight = weights.get(comp_name, 0)
            ensemble_predictions += weight * predictions
            self.logger.debug(f"Added {comp_name} with weight {weight:.3f}")
        
        return ensemble_predictions, prediction_details

    def validate_ensemble_performance(self, ensemble_predictions: np.ndarray, actual_values: np.ndarray, component_metrics: Dict) -> Dict:
        """
        Validate final ensemble performance on independent test data.
        
        Returns comprehensive ensemble validation metrics.
        """
        # Align predictions and actuals
        min_len = min(len(ensemble_predictions), len(actual_values))
        ensemble_preds = ensemble_predictions[:min_len]
        actual_vals = actual_values[:min_len]
        
        if min_len < 5:  # Need minimum observations
            return {"error": "Insufficient data for ensemble validation"}
        
        # Calculate ensemble metrics
        mape = np.mean(np.abs((actual_vals - ensemble_preds) / actual_vals)) * 100
        mae = np.mean(np.abs(actual_vals - ensemble_preds))
        rmse = np.sqrt(np.mean((actual_vals - ensemble_preds) ** 2))
        r2 = r2_score(actual_vals, ensemble_preds) if sklearn_available else 0
        
        # Calculate improvement over best component
        best_component_r2 = max(
            comp_data['validation_metrics']['validation_r2'] 
            for comp_data in component_metrics.values()
        )
        r2_improvement = r2 - best_component_r2
        relative_improvement = (r2_improvement / abs(best_component_r2)) * 100 if best_component_r2 != 0 else 0
        
        # Ensemble-specific metrics
        prediction_stability = np.std(ensemble_preds) / np.mean(ensemble_preds) if np.mean(ensemble_preds) > 0 else 0
        actual_stability = np.std(actual_vals) / np.mean(actual_vals) if np.mean(actual_vals) > 0 else 0
        stability_ratio = prediction_stability / actual_stability if actual_stability > 0 else 0
        
        ensemble_metrics = {
            'test_r2': r2,
            'test_mape': mape,
            'test_mae': mae,
            'test_rmse': rmse,
            'best_component_r2': best_component_r2,
            'r2_improvement': r2_improvement,
            'relative_improvement_pct': relative_improvement,
            'prediction_stability': prediction_stability,
            'stability_ratio': stability_ratio,
            'test_observations': min_len
        }
        
        # Realistic performance assessment
        performance_flags = []
        is_realistic = True
        
        # Flag 1: Ensemble R² assessment
        if r2 > self.performance_thresholds['max_realistic_ensemble_r2']:
            performance_flags.append(f"R² suspiciously high: {r2:.4f} > {self.performance_thresholds['max_realistic_ensemble_r2']}")
            is_realistic = False
        
        # Flag 2: Improvement assessment  
        if r2_improvement < self.performance_thresholds['min_ensemble_improvement'] and r2 > 0.05:
            performance_flags.append(f"Insufficient improvement: {r2_improvement:.4f} < {self.performance_thresholds['min_ensemble_improvement']}")
        
        # Flag 3: MAPE assessment
        if mape < 1.0:
            performance_flags.append(f"MAPE suspiciously low: {mape:.2f}% < 1.0%")
            is_realistic = False
        elif mape > 30.0:
            performance_flags.append(f"MAPE too high: {mape:.2f}% > 30.0%")
        
        # Flag 4: Stability assessment
        if stability_ratio < 0.2 or stability_ratio > 5.0:
            performance_flags.append(f"Stability mismatch: {stability_ratio:.2f}")
        
        ensemble_validation = {
            'metrics': ensemble_metrics,
            'is_realistic': is_realistic,
            'performance_flags': performance_flags,
            'performance_grade': self._calculate_performance_grade(ensemble_metrics),
            'validation_status': 'PASS' if is_realistic and len(performance_flags) <= 1 else 'FAIL'
        }
        
        return ensemble_validation

    def _calculate_performance_grade(self, metrics: Dict) -> str:
        """Calculate letter grade based on ensemble performance."""
        r2 = metrics['test_r2']
        mape = metrics['test_mape']
        improvement = metrics['r2_improvement']
        
        score = 0
        
        # R² scoring (40 points)
        if 0.10 <= r2 <= 0.15:
            score += 40  # Excellent realistic range
        elif 0.05 <= r2 < 0.10:
            score += 35  # Good performance
        elif 0.02 <= r2 < 0.05:
            score += 30  # Acceptable performance
        elif 0.00 <= r2 < 0.02:
            score += 20  # Marginal performance
        elif r2 < 0.00:
            score += 10  # Poor performance
        else:  # r2 > 0.15
            score += 15  # Suspiciously high
        
        # MAPE scoring (30 points)
        if 5.0 <= mape <= 15.0:
            score += 30  # Realistic range
        elif 15.0 < mape <= 25.0:
            score += 25  # Acceptable
        elif 2.0 <= mape < 5.0:
            score += 20  # Good but possibly overfitted
        elif mape > 25.0:
            score += 10  # Poor
        else:  # mape < 2.0
            score += 5   # Suspiciously low
        
        # Improvement scoring (30 points)
        if improvement >= 0.05:
            score += 30  # Excellent improvement
        elif improvement >= 0.02:
            score += 25  # Good improvement
        elif improvement >= 0.00:
            score += 20  # Marginal improvement
        else:
            score += 10  # No improvement
        
        # Convert to letter grade
        if score >= 85:
            return 'A'
        elif score >= 75:
            return 'B'
        elif score >= 65:
            return 'C'
        elif score >= 55:
            return 'D'
        else:
            return 'F'

    def build_ensemble_model(self, data: pd.DataFrame, symbol: str, model_name: str = None) -> Dict:
        """
        Build a complete ensemble model with leak-free validation methodology.
        
        Main entry point for ensemble model construction.
        """
        if model_name is None:
            model_name = f"enhanced_ensemble_{symbol}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        self.logger.info(f"🏗️  Building ensemble model: {model_name}")
        self.logger.info(f"📊 Input data shape: {data.shape}")
        
        ensemble_result = {
            'model_name': model_name,
            'symbol': symbol,
            'build_timestamp': datetime.now().isoformat(),
            'status': 'failed',
            'data_info': {
                'input_shape': data.shape,
                'date_range': (str(data.index.min()), str(data.index.max()))
            }
        }
        
        try:
            # Step 1: Create temporal data splits
            self.logger.info("📅 Creating temporal data splits")
            data_splits = self.create_temporal_splits(data)
            ensemble_result['data_splits'] = {
                'train_size': len(data_splits['train']),
                'validation_size': len(data_splits['validation']),
                'test_size': len(data_splits['test'])
            }
            
            # Step 2: Build and validate component models
            self.logger.info("🔧 Building component models")
            component_results = self.build_component_models(data_splits, symbol)
            ensemble_result['component_results'] = {
                'validation_summary': component_results['validation_summary'],
                'valid_components': list(component_results['valid_components'].keys()),
                'rejected_components': list(component_results['rejected_components'].keys())
            }
            
            # Check if we have enough valid components
            if len(component_results['valid_components']) < 2:
                ensemble_result['error'] = f"Insufficient valid components: {len(component_results['valid_components'])}/2 required"
                self.logger.error(ensemble_result['error'])
                return ensemble_result
            
            # Step 3: Calculate ensemble weights
            self.logger.info("⚖️  Calculating ensemble weights")
            weight_details = self.calculate_ensemble_weights(component_results['valid_components'], data_splits)
            if 'error' in weight_details:
                ensemble_result['error'] = weight_details['error']
                return ensemble_result
            
            ensemble_result['weights'] = weight_details
            
            # Step 4: Generate ensemble predictions on test data
            self.logger.info("🔮 Generating ensemble predictions")
            test_data = data_splits['test']
            ensemble_predictions, prediction_details = self.create_ensemble_predictions(
                component_results['valid_components'], 
                weight_details['weights'], 
                test_data, 
                symbol
            )
            
            ensemble_result['prediction_details'] = prediction_details
            
            # Step 5: Validate ensemble performance
            self.logger.info("✅ Validating ensemble performance")
            actual_values = test_data['price'].values
            ensemble_validation = self.validate_ensemble_performance(
                ensemble_predictions, 
                actual_values, 
                component_results['valid_components']
            )
            
            ensemble_result['validation'] = ensemble_validation
            
            # Step 6: Save ensemble model
            ensemble_model = {
                'model_name': model_name,
                'symbol': symbol,
                'components': component_results['valid_components'],
                'weights': weight_details['weights'],
                'metadata': {
                    'build_timestamp': datetime.now().isoformat(),
                    'data_splits': ensemble_result['data_splits'],
                    'validation_metrics': ensemble_validation['metrics'],
                    'performance_grade': ensemble_validation['performance_grade']
                }
            }
            
            # Save ensemble to disk
            model_file = self.ensemble_dir / f"{model_name}.pkl"
            with open(model_file, 'wb') as f:
                pickle.dump(ensemble_model, f)
            
            # Save detailed results
            results_file = self.ensemble_dir / f"{model_name}_results.json"
            with open(results_file, 'w') as f:
                # Convert numpy arrays to lists for JSON serialization
                json_result = self._prepare_for_json(ensemble_result)
                json.dump(json_result, f, indent=2)
            
            ensemble_result['model_file'] = str(model_file)
            ensemble_result['results_file'] = str(results_file)
            ensemble_result['status'] = 'success'
            
            # Log final results
            validation_metrics = ensemble_validation['metrics']
            self.logger.info(f"🎯 Ensemble built successfully: {model_name}")
            self.logger.info(f"📈 Test R²: {validation_metrics['test_r2']:.4f}")
            self.logger.info(f"📊 Test MAPE: {validation_metrics['test_mape']:.2f}%")
            self.logger.info(f"🏆 Performance Grade: {ensemble_validation['performance_grade']}")
            self.logger.info(f"✅ Validation Status: {ensemble_validation['validation_status']}")
            
        except Exception as e:
            ensemble_result['error'] = str(e)
            self.logger.error(f"❌ Ensemble building failed: {e}")
        
        return ensemble_result

    def _prepare_for_json(self, obj):
        """Recursively prepare object for JSON serialization."""
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        elif isinstance(obj, np.floating):
            return float(obj)
        elif isinstance(obj, np.integer):
            return int(obj)
        elif isinstance(obj, dict):
            return {key: self._prepare_for_json(value) for key, value in obj.items()}
        elif isinstance(obj, (list, tuple)):
            return [self._prepare_for_json(item) for item in obj]
        else:
            return obj

def main():
    """Main function for CLI usage."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Enhanced Ensemble Builder with Leak-Free Validation')
    parser.add_argument('--symbol', default='ETH', help='Crypto symbol to build ensemble for')
    parser.add_argument('--model-name', help='Custom model name (optional)')
    parser.add_argument('--data-file', help='Path to CSV data file (optional, will generate sample data if not provided)')
    parser.add_argument('--min-observations', type=int, default=200, help='Minimum observations required')
    
    args = parser.parse_args()
    
    builder = EnhancedEnsembleBuilder()
    
    print("🔧 Enhanced Ensemble Builder with Leak-Free Validation")
    print("=" * 55)
    
    # Generate or load data
    if args.data_file and Path(args.data_file).exists():
        print(f"📂 Loading data from: {args.data_file}")
        data = pd.read_csv(args.data_file, index_col=0, parse_dates=True)
    else:
        print(f"🎲 Generating sample data for {args.symbol}")
        # Generate realistic sample data
        dates = pd.date_range(end=datetime.now(), periods=args.min_observations, freq='1H')
        prices = []
        base_price = 3000 if args.symbol == 'ETH' else 50000
        
        for i in range(len(dates)):
            # Realistic price movement with trend and noise
            trend = 0.0001 * i  # Slight upward trend
            noise = np.random.normal(0, 0.02)  # 2% volatility
            seasonal = 0.01 * np.sin(2 * np.pi * i / 24)  # Daily seasonality
            
            if i == 0:
                price = base_price
            else:
                price_change = trend + noise + seasonal
                price = prices[-1] * (1 + price_change)
                price = max(price, base_price * 0.5)  # Floor price
            
            prices.append(price)
        
        data = pd.DataFrame({
            'price': prices,
            'volume': np.random.normal(10000, 2000, len(dates))  # Sample volume
        }, index=dates)
    
    print(f"📊 Data shape: {data.shape}")
    print(f"📅 Date range: {data.index.min()} to {data.index.max()}")
    
    # Build ensemble model
    result = builder.build_ensemble_model(data, args.symbol, args.model_name)
    
    # Display results
    if result['status'] == 'success':
        print(f"\n✅ Ensemble Model Built Successfully: {result['model_name']}")
        print(f"🏆 Performance Grade: {result['validation']['performance_grade']}")
        print(f"📈 Test R²: {result['validation']['metrics']['test_r2']:.4f}")
        print(f"📊 Test MAPE: {result['validation']['metrics']['test_mape']:.2f}%")
        print(f"⚖️  Components: {', '.join(result['component_results']['valid_components'])}")
        print(f"💾 Model saved to: {result['model_file']}")
        
        if result['validation']['validation_status'] == 'PASS':
            print("✅ Ensemble validation: PASSED")
        else:
            print("⚠️  Ensemble validation: FAILED")
            for flag in result['validation']['performance_flags']:
                print(f"   • {flag}")
    else:
        print(f"\n❌ Ensemble Building Failed: {result.get('error', 'Unknown error')}")

if __name__ == "__main__":
    main()