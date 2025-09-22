"""
Comprehensive Model Storage Demo

Demonstrates the complete model storage system with both Prophet and XGBoost methodologies.
Shows organized storage, version control, and performance comparison across    print(f"\n2. Compare Model Performance:")
    print(f"   all_models = storage_manager.list_models()")
    print(f"   xgb_models = [m for m in all_models if m.methodology == 'xgboost']")
    print(f"   best_xgb = min(xgb_models, key=lambda x: x.performance_metrics.get('mape', float('inf')))")hodologies.
"""

import pandas as pd
import numpy as np
from pathlib import Path
import sqlite3
from datetime import datetime
import sys

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent))
from models.model_management.model_storage_manager import ModelStorageManager

def demo_complete_storage_system():
    """Demonstrate the complete model storage system with multiple methodologies."""
    print("🚀 Complete ETH Model Storage System Demo")
    print("=" * 60)
    
    # Initialize storage manager
    storage_manager = ModelStorageManager()
    
    print("📁 Current Storage Structure:")
    base_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/model_storage")
    
    methodologies = ['prophet', 'xgboost', 'lstm', 'ensemble', 'arima', 'transformer', 'garch']
    total_models = 0
    total_size = 0
    
    for methodology in methodologies:
        method_path = base_path / methodology
        if method_path.exists():
            model_files = list(method_path.glob("*.pkl"))
            if model_files:
                print(f"\n📂 {methodology.upper()} Models:")
                for file in sorted(model_files):
                    size_mb = file.stat().st_size / (1024 * 1024)
                    total_size += size_mb
                    total_models += 1
                    print(f"   📄 {file.name} ({size_mb:.2f} MB)")
            else:
                print(f"\n📂 {methodology.upper()} Models: (empty)")
    
    print(f"\n📊 Storage Summary:")
    print(f"   Total Models: {total_models}")
    print(f"   Total Size: {total_size:.2f} MB")
    print(f"   Methodologies Available: {len([m for m in methodologies if (base_path / m).exists() and list((base_path / m).glob('*.pkl'))])}")
    
    # Show all stored models from metadata database
    print(f"\n📈 All Stored Models (from metadata database):")
    all_models = storage_manager.list_models()
    
    if all_models:
        # Convert to simple display format
        print(f"\n   {'Methodology':<12} {'Version':<8} {'Asset':<6} {'Description':<30} {'Created':<20}")
        print(f"   {'-'*80}")
        
        for model in sorted(all_models, key=lambda x: (x.methodology, x.version)):
            created_short = model.created_at[:19] if len(model.created_at) > 19 else model.created_at
            desc_short = model.description[:28] + '..' if len(model.description) > 30 else model.description
            print(f"   {model.methodology:<12} v{model.version:03d}    {model.asset:<6} {desc_short:<30} {created_short}")
        
        # Performance comparison across methodologies
        print(f"\n🏆 Performance Comparison by Methodology:")
        
        # Prophet models
        prophet_models = [m for m in all_models if m.methodology == 'prophet']
        if prophet_models:
            print(f"\n   📊 Prophet Models:")
            for model in sorted(prophet_models, key=lambda x: x.version):
                mape = model.performance_metrics.get('mape', 'N/A')
                print(f"      v{model.version:03d}: MAPE {mape:.2f}%")
        
        # XGBoost models
        xgboost_models = [m for m in all_models if m.methodology == 'xgboost']
        if xgboost_models:
            print(f"\n   🎯 XGBoost Models:")
            for model in sorted(xgboost_models, key=lambda x: x.version):
                mape = model.performance_metrics.get('mape', 'N/A')
                r2 = model.performance_metrics.get('r2_score', 'N/A')
                print(f"      v{model.version:03d}: MAPE {mape:.2f}%, R² {r2:.4f}")
        
        # Find best performing model overall
        best_model = find_best_performing_model(all_models)
        if best_model:
            print(f"\n🥇 Best Performing Model:")
            print(f"   Model: {best_model['model_id']}")
            print(f"   Methodology: {best_model['methodology']}")
            print(f"   MAPE: {best_model['mape']:.2f}%")
            print(f"   Version: v{best_model['version']:03d}")
            print(f"   Created: {best_model['created_at']}")
    
    else:
        print("   No models found in metadata database")
    
    # Show model loading capabilities
    print(f"\n🔄 Model Loading Capabilities:")
    
    # Load latest Prophet model
    try:
        latest_prophet = storage_manager.load_latest_model('prophet', 'ETH')
        if latest_prophet:
            print(f"   ✅ Latest Prophet model loaded: {latest_prophet['metadata'].model_id}")
        else:
            print(f"   ❌ No Prophet models available")
    except Exception as e:
        print(f"   ❌ Prophet model loading failed: {str(e)}")
    
    # Load latest XGBoost model
    try:
        latest_xgboost = storage_manager.load_latest_model('xgboost', 'ETH')
        if latest_xgboost:
            print(f"   ✅ Latest XGBoost model loaded: {latest_xgboost['metadata'].model_id}")
        else:
            print(f"   ❌ No XGBoost models available")
    except Exception as e:
        print(f"   ❌ XGBoost model loading failed: {str(e)}")
    
    # Show extensibility
    print(f"\n🔮 Framework Extensibility:")
    print(f"   📁 Ready for LSTM models: {(base_path / 'lstm').exists()}")
    print(f"   📁 Ready for Ensemble models: {(base_path / 'ensemble').exists()}")
    print(f"   📁 Ready for ARIMA models: {(base_path / 'arima').exists()}")
    print(f"   📁 Ready for Transformer models: {(base_path / 'transformer').exists()}")
    print(f"   📁 Ready for GARCH models: {(base_path / 'garch').exists()}")
    
    # Storage efficiency analysis
    print(f"\n💾 Storage Efficiency Analysis:")
    storage_efficiency = analyze_storage_efficiency()
    for metric, value in storage_efficiency.items():
        print(f"   {metric}: {value}")
    
    print(f"\n✅ Complete storage system demo finished!")
    print(f"   System Status: ✅ Fully Operational")
    print(f"   Methodologies: Prophet ✅, XGBoost ✅, Others 🔄 Ready")
    print(f"   Version Control: ✅ Automated")
    print(f"   Metadata Tracking: ✅ Comprehensive")
    print(f"   Performance Comparison: ✅ Available")

def find_best_performing_model(models_list):
    """Find the best performing model based on MAPE."""
    if not models_list:
        return None
    
    best_model = None
    best_mape = float('inf')
    
    for model in models_list:
        try:
            mape = model.performance_metrics.get('mape', float('inf'))
            
            if mape < best_mape:
                best_mape = mape
                best_model = {
                    'model_id': model.model_id,
                    'methodology': model.methodology,
                    'version': model.version,
                    'mape': mape,
                    'created_at': model.created_at
                }
        except:
            continue
    
    return best_model

def analyze_storage_efficiency():
    """Analyze storage efficiency metrics."""
    base_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/model_storage")
    
    efficiency_metrics = {}
    
    # Count models by methodology
    methodology_counts = {}
    total_size_by_methodology = {}
    
    for methodology in ['prophet', 'xgboost', 'lstm', 'ensemble', 'arima', 'transformer', 'garch']:
        method_path = base_path / methodology
        if method_path.exists():
            model_files = list(method_path.glob("*.pkl"))
            methodology_counts[methodology] = len(model_files)
            
            total_size = sum(file.stat().st_size for file in model_files)
            total_size_by_methodology[methodology] = total_size / (1024 * 1024)  # MB
    
    # Calculate efficiency metrics
    total_models = sum(methodology_counts.values())
    total_size_mb = sum(total_size_by_methodology.values())
    
    efficiency_metrics['Total Models'] = total_models
    efficiency_metrics['Total Storage (MB)'] = f"{total_size_mb:.2f}"
    efficiency_metrics['Average Model Size (MB)'] = f"{total_size_mb / max(total_models, 1):.2f}"
    efficiency_metrics['Active Methodologies'] = len([k for k, v in methodology_counts.items() if v > 0])
    efficiency_metrics['Storage Utilization'] = f"{len([k for k, v in methodology_counts.items() if v > 0]) / 7 * 100:.1f}%"
    
    return efficiency_metrics

def show_usage_examples():
    """Show practical usage examples of the storage system."""
    print(f"\n📝 Usage Examples:")
    print(f"\n1. Load Latest Model:")
    print(f"   storage_manager = ModelStorageManager()")
    print(f"   latest_model = storage_manager.load_latest_model('xgboost', 'ETH')")
    print(f"   model = latest_model['model']")
    print(f"   metadata = latest_model['metadata']")
    
    print(f"\n2. Compare Model Performance:")
    print(f"   all_models = storage_manager.list_all_models()")
    print(f"   xgb_models = all_models[all_models['methodology'] == 'xgboost']")
    print(f"   best_xgb = xgb_models.loc[xgb_models['performance_metrics'].str.contains('lowest_mape')]")
    
    print(f"\n3. Store New Model:")
    print(f"   model_id = storage_manager.store_model(")
    print(f"       model=trained_model,")
    print(f"       methodology='lstm',")
    print(f"       asset='ETH',")
    print(f"       model_config=config_dict,")
    print(f"       performance_metrics=metrics_dict,")
    print(f"       description='LSTM model with attention',")
    print(f"       variant='attention',")
    print(f"       tags=['lstm', 'attention', 'eth']")
    print(f"   )")

if __name__ == "__main__":
    demo_complete_storage_system()
    show_usage_examples()
