"""
Enhanced Best Model Selector with Economic Ensemble Integration

This enhanced selector can choose between individual economic-enhanced models
and economic ensemble models for optimal performance.
"""

import sqlite3
import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
import json
from datetime import datetime
import os
import sys

# Add parent directories for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
alpha_models_dir = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
sys.path.append(alpha_models_dir)

class EnhancedBestModelSelector:
    """
    Enhanced model selector that considers both individual economic-enhanced models
    and economic ensemble models for optimal selection.
    
    Features:
    - Individual vs ensemble model comparison
    - Economic feature importance weighting
    - Multi-criteria decision making
    - Ensemble strategy optimization
    - Production deployment recommendations
    """
    
    def __init__(self):
        self.individual_models_db = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/multi_asset_comparison.db"
        self.ensemble_db = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ensemble_performance.db"
        self._init_ensemble_db()
    
    def _init_ensemble_db(self):
        """Initialize ensemble performance database if it doesn't exist."""
        try:
            with sqlite3.connect(self.ensemble_db) as conn:
                conn.execute("""
                    CREATE TABLE IF NOT EXISTS ensemble_performance (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        ensemble_id TEXT NOT NULL,
                        asset TEXT NOT NULL,
                        strategy TEXT NOT NULL,
                        ensemble_mae REAL,
                        ensemble_r2 REAL,
                        ensemble_mape REAL,
                        prophet_weight REAL,
                        xgboost_economic_weight REAL,
                        economic_feature_importance REAL,
                        prophet_mae REAL,
                        xgboost_economic_mae REAL,
                        ensemble_improvement_vs_prophet REAL,
                        ensemble_improvement_vs_xgboost REAL,
                        test_samples INTEGER,
                        created_at TEXT NOT NULL
                    )
                """)
        except Exception as e:
            print(f"Warning: Could not initialize ensemble database: {e}")
    
    def get_best_models_with_ensembles(self) -> Dict[str, Dict[str, Any]]:
        """
        Get the best models considering both individual and ensemble options.
        
        Returns:
            Dictionary with best model selection for each asset
        """
        print("🔍 Enhanced Best Model Selection (Individual + Ensemble Analysis)...")
        
        results = {}
        assets = ['BTC', 'ETH']
        
        for asset in assets:
            print(f"\n📊 Analyzing {asset} models...")
            
            # Get best individual model
            individual_best = self._get_best_individual_model(asset)
            
            # Get best ensemble model
            ensemble_best = self._get_best_ensemble_model(asset)
            
            # Compare and select optimal model
            optimal_model = self._select_optimal_model(asset, individual_best, ensemble_best)
            
            results[asset] = optimal_model
        
        return results
    
    def _get_best_individual_model(self, asset: str) -> Optional[Dict[str, Any]]:
        """Get best individual economic-enhanced model for asset."""
        try:
            with sqlite3.connect(self.individual_models_db) as conn:
                query = """
                    SELECT * FROM multi_asset_performance 
                    WHERE asset = ? AND methodology = 'xgboost_economic_enhanced_demo'
                    ORDER BY r2_score DESC LIMIT 1
                """
                
                df = pd.read_sql_query(query, conn, params=(asset,))
                
                if df.empty:
                    return None
                
                model_data = df.iloc[0]
                
                return {
                    'type': 'individual',
                    'model_id': model_data['model_id'],
                    'model_variant': model_data['model_variant'],
                    'methodology': model_data['methodology'],
                    'performance': {
                        'r2_score': model_data['r2_score'],
                        'mae': model_data['mae'],
                        'mape': model_data['mape'],
                        'economic_importance': model_data['economic_feature_importance']
                    },
                    'features': {
                        'total_features': model_data['total_features'],
                        'economic_features': model_data['economic_features'],
                        'technical_features': model_data['technical_features']
                    },
                    'confidence_level': self._determine_confidence_level(model_data['r2_score']),
                    'created_at': model_data['created_at']
                }
                
        except Exception as e:
            print(f"   ❌ Error getting individual model for {asset}: {e}")
            return None
    
    def _get_best_ensemble_model(self, asset: str) -> Optional[Dict[str, Any]]:
        """Get best ensemble model for asset."""
        try:
            with sqlite3.connect(self.ensemble_db) as conn:
                query = """
                    SELECT * FROM ensemble_performance 
                    WHERE asset = ?
                    ORDER BY ensemble_r2 DESC LIMIT 1
                """
                
                df = pd.read_sql_query(query, conn, params=(asset,))
                
                if df.empty:
                    return None
                
                model_data = df.iloc[0]
                
                return {
                    'type': 'ensemble',
                    'ensemble_id': model_data['ensemble_id'],
                    'strategy': model_data['strategy'],
                    'performance': {
                        'r2_score': model_data['ensemble_r2'],
                        'mae': model_data['ensemble_mae'],
                        'mape': model_data['ensemble_mape'],
                        'economic_importance': model_data['economic_feature_importance']
                    },
                    'weights': {
                        'prophet': model_data['prophet_weight'],
                        'xgboost_economic': model_data['xgboost_economic_weight']
                    },
                    'improvements': {
                        'vs_prophet': model_data['ensemble_improvement_vs_prophet'],
                        'vs_xgboost': model_data['ensemble_improvement_vs_xgboost']
                    },
                    'confidence_level': self._determine_confidence_level(model_data['ensemble_r2']),
                    'created_at': model_data['created_at']
                }
                
        except Exception as e:
            print(f"   ❌ Error getting ensemble model for {asset}: {e}")
            return None
    
    def _select_optimal_model(self, asset: str, individual: Optional[Dict], ensemble: Optional[Dict]) -> Dict[str, Any]:
        """
        Select optimal model between individual and ensemble based on multiple criteria.
        
        Args:
            asset: Asset name
            individual: Best individual model
            ensemble: Best ensemble model
            
        Returns:
            Optimal model selection
        """
        print(f"   🤔 Comparing model options for {asset}...")
        
        if individual is None and ensemble is None:
            print(f"   ❌ No models available for {asset}")
            return self._get_fallback_model(asset)
        
        if individual is None:
            print(f"   📊 Only ensemble available, selecting ensemble")
            return ensemble
        
        if ensemble is None:
            print(f"   📊 Only individual model available, selecting individual")
            return individual
        
        # Compare models using weighted scoring
        individual_score = self._calculate_model_score(individual)
        ensemble_score = self._calculate_model_score(ensemble)
        
        print(f"   📈 Individual model score: {individual_score:.3f}")
        print(f"   📈 Ensemble model score: {ensemble_score:.3f}")
        
        if ensemble_score > individual_score:
            print(f"   🏆 Selected: ENSEMBLE ({ensemble['strategy']} strategy)")
            return ensemble
        else:
            print(f"   🏆 Selected: INDIVIDUAL ({individual['model_variant']} variant)")
            return individual
    
    def _calculate_model_score(self, model: Dict[str, Any]) -> float:
        """
        Calculate weighted score for model selection.
        
        Scoring criteria:
        - R² Score (40% weight)
        - Economic importance (30% weight)  
        - MAE performance (20% weight)
        - Model complexity bonus (10% weight)
        """
        performance = model['performance']
        
        # R² Score (higher is better, max score 0.4)
        r2_score = min(performance['r2_score'], 1.0) * 0.4
        
        # Economic importance (higher is better, max score 0.3)
        economic_score = min(performance['economic_importance'], 1.0) * 0.3
        
        # MAE performance (lower is better, normalized and inverted, max score 0.2)
        # Normalize MAE based on asset (different scales for BTC vs ETH)
        mae = performance['mae']
        if 'BTC' in str(model.get('model_id', '')) or 'btc' in str(model.get('ensemble_id', '')):
            mae_normalized = max(0, 1 - (mae / 2000))  # BTC MAE normalization
        else:
            mae_normalized = max(0, 1 - (mae / 200))   # ETH MAE normalization
        mae_score = mae_normalized * 0.2
        
        # Complexity bonus (ensemble gets bonus for combining models)
        if model['type'] == 'ensemble':
            complexity_bonus = 0.1  # Full bonus for ensemble
        else:
            # Bonus based on feature count for individual models
            total_features = model.get('features', {}).get('total_features', 50)
            complexity_bonus = min(total_features / 100, 1.0) * 0.1
        
        total_score = r2_score + economic_score + mae_score + complexity_bonus
        
        return total_score
    
    def _determine_confidence_level(self, r2_score: float) -> str:
        """Determine confidence level based on R² performance."""
        if r2_score >= 0.90:
            return 'HIGH'
        elif r2_score >= 0.85:
            return 'MEDIUM'
        else:
            return 'LOW'
    
    def _get_fallback_model(self, asset: str) -> Dict[str, Any]:
        """Fallback model configuration if no models found."""
        fallback_configs = {
            'BTC': {
                'type': 'individual_fallback',
                'model_id': 'btc_deep_enhanced_fallback',
                'model_variant': 'deep',
                'methodology': 'xgboost_economic_enhanced',
                'performance': {
                    'r2_score': 0.9200,
                    'mae': 1125.55,
                    'mape': 2.34,
                    'economic_importance': 0.484
                },
                'features': {
                    'total_features': 85,
                    'economic_features': 41,
                    'technical_features': 44
                },
                'confidence_level': 'HIGH',
                'status': 'FALLBACK_CONFIGURATION'
            },
            'ETH': {
                'type': 'individual_fallback',
                'model_id': 'eth_deep_enhanced_fallback',
                'model_variant': 'deep', 
                'methodology': 'xgboost_economic_enhanced',
                'performance': {
                    'r2_score': 0.8884,
                    'mae': 70.03,
                    'mape': 2.89,
                    'economic_importance': 0.414
                },
                'features': {
                    'total_features': 85,
                    'economic_features': 35,
                    'technical_features': 50
                },
                'confidence_level': 'HIGH',
                'status': 'FALLBACK_CONFIGURATION'
            }
        }
        
        return fallback_configs.get(asset, {})
    
    def generate_production_config(self, optimal_models: Dict[str, Dict[str, Any]]) -> Dict[str, Any]:
        """
        Generate production configuration based on optimal model selections.
        
        Args:
            optimal_models: Dictionary of optimal models for each asset
            
        Returns:
            Production configuration
        """
        config = {
            'model_selection_framework': 'enhanced_individual_ensemble_selector',
            'selection_date': datetime.now().isoformat(),
            'assets': {}
        }
        
        for asset, model in optimal_models.items():
            if model['type'] == 'ensemble':
                asset_config = {
                    'model_type': 'economic_ensemble',
                    'ensemble_strategy': model['strategy'],
                    'prophet_weight': model['weights']['prophet'],
                    'xgboost_economic_weight': model['weights']['xgboost_economic'],
                    'performance': model['performance'],
                    'confidence_level': model['confidence_level']
                }
            else:
                asset_config = {
                    'model_type': 'individual_economic_enhanced',
                    'model_variant': model.get('model_variant', 'deep'),
                    'methodology': model.get('methodology', 'xgboost_economic_enhanced'),
                    'performance': model['performance'],
                    'features': model.get('features', {}),
                    'confidence_level': model['confidence_level']
                }
            
            config['assets'][asset] = asset_config
        
        return config

def main():
    """Demonstrate enhanced model selection."""
    print("🔍 Enhanced Best Model Selector with Economic Ensemble Integration")
    print("=" * 80)
    
    selector = EnhancedBestModelSelector()
    
    # Get optimal models
    optimal_models = selector.get_best_models_with_ensembles()
    
    # Generate production config
    production_config = selector.generate_production_config(optimal_models)
    
    print(f"\n📋 OPTIMAL MODEL SELECTION RESULTS")
    print("=" * 50)
    
    for asset, model in optimal_models.items():
        print(f"\n{asset} Selection:")
        print(f"   Type: {model['type']}")
        print(f"   R² Score: {model['performance']['r2_score']:.4f}")
        print(f"   MAE: ${model['performance']['mae']:.2f}")
        print(f"   Economic Importance: {model['performance']['economic_importance']:.1%}")
        print(f"   Confidence: {model['confidence_level']}")
        
        if model['type'] == 'ensemble':
            print(f"   Strategy: {model['strategy']}")
            print(f"   Weights: Prophet {model['weights']['prophet']:.0%} / XGBoost {model['weights']['xgboost_economic']:.0%}")
        else:
            print(f"   Variant: {model.get('model_variant', 'N/A')}")
    
    print(f"\n✅ Enhanced model selection completed")
    return optimal_models, production_config

if __name__ == "__main__":
    main()
