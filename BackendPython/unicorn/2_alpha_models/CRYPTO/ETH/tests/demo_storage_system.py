"""
ETH Prophet Model Storage Demo

Quick demonstration of the new organized model storage system.
Shows how easy it is to store, load, and manage multiple model variants.
"""

import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent))

from models.model_management.model_storage_manager import ModelStorageManager
from legacy.eth_prophet_clean import ETHProphetFrameworkWithStorage

def demo_storage_system():
    """Demonstrate the complete storage system capabilities."""
    
    print("🎯 ETH Prophet Model Storage System Demo")
    print("=" * 60)
    
    # Initialize components
    storage = ModelStorageManager()
    framework = ETHProphetFrameworkWithStorage()
    
    # 1. Show current storage state
    print("\n📊 CURRENT STORAGE STATE:")
    storage.print_storage_summary()
    
    # 2. List all Prophet models
    print("\n📋 STORED PROPHET MODELS:")
    prophet_models = storage.list_models(methodology='prophet', asset='ETH')
    
    for i, model in enumerate(prophet_models, 1):
        print(f"\n   {i}. {model.model_id}")
        print(f"      📁 File: {model.file_path.split('/')[-1]}")
        print(f"      📏 Size: {model.file_size/1024:.1f} KB")
        print(f"      📈 MAPE: {model.performance_metrics.get('mape', 'N/A'):.2f}%")
        print(f"      🎯 Directional Accuracy: {model.performance_metrics.get('directional_accuracy', 'N/A'):.1f}%")
        print(f"      📅 Created: {model.created_at[:19]}")
        print(f"      🏷️  Tags: {', '.join(model.tags)}")
    
    # 3. Load and inspect best performing model
    print(f"\n🏆 LOADING BEST PERFORMING MODEL:")
    best_model = min(prophet_models, key=lambda m: m.performance_metrics.get('mape', float('inf')))
    print(f"   Loading: {best_model.model_id} (MAPE: {best_model.performance_metrics['mape']:.2f}%)")
    
    model, metadata = storage.load_model(best_model.model_id)
    print(f"   ✅ Successfully loaded Prophet model")
    print(f"   📊 Model type: {type(model).__name__}")
    print(f"   📋 Config: {metadata.model_config['variant']} variant")
    
    # 4. Show performance comparison
    print(f"\n📈 MODEL PERFORMANCE COMPARISON:")
    comparison = framework.compare_model_performance()
    if not comparison.empty:
        # Sort by MAPE (best first)
        comparison_sorted = comparison.sort_values('mape')
        print("\n   Ranking by MAPE (Mean Absolute Percentage Error):")
        for i, (_, row) in enumerate(comparison_sorted.iterrows(), 1):
            variant = row['model_variant']
            mape = row['mape']
            directional = row['directional_accuracy']
            print(f"   {i}. {variant:10} | MAPE: {mape:6.2f}% | Direction: {directional:5.1f}%")
    
    # 5. Demonstrate easy loading
    print(f"\n🔄 EASY MODEL LOADING EXAMPLES:")
    
    # Load latest model
    try:
        latest_model, latest_metadata = storage.load_latest_model('prophet', 'ETH')
        print(f"   ✅ Latest model: {latest_metadata.model_id}")
    except:
        print(f"   ❌ No models found")
    
    # Load by variant (simulation)
    basic_models = [m for m in prophet_models if 'basic' in m.tags]
    enhanced_models = [m for m in prophet_models if 'enhanced' in m.tags]
    optimized_models = [m for m in prophet_models if 'optimized' in m.tags]
    
    print(f"   📊 Available variants:")
    print(f"      Basic models: {len(basic_models)}")
    print(f"      Enhanced models: {len(enhanced_models)}")  
    print(f"      Optimized models: {len(optimized_models)}")
    
    # 6. Show directory organization
    print(f"\n📁 DIRECTORY ORGANIZATION:")
    base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/model_storage"
    
    import os
    for methodology in ['prophet', 'xgboost', 'lstm', 'ensemble']:
        methodology_path = f"{base_path}/{methodology}"
        if os.path.exists(methodology_path):
            files = [f for f in os.listdir(methodology_path) if f.endswith('.pkl')]
            print(f"   {methodology:12}: {len(files):2} models")
    
    print(f"\n🎯 KEY BENEFITS DEMONSTRATED:")
    print(f"   ✅ Organized storage by methodology")
    print(f"   ✅ Automatic version control")
    print(f"   ✅ Comprehensive metadata tracking")
    print(f"   ✅ Easy model loading and comparison")
    print(f"   ✅ Performance metrics storage")
    print(f"   ✅ Scalable to multiple assets and methodologies")
    
    return storage, framework, prophet_models

def show_usage_examples():
    """Show practical usage examples."""
    
    print(f"\n💡 PRACTICAL USAGE EXAMPLES:")
    print("=" * 50)
    
    print("""
# 1. Store a new model
storage = ModelStorageManager()
model_id = storage.store_model(
    model=my_trained_model,
    methodology='prophet',
    asset='ETH',
    model_config={'variant': 'enhanced'},
    performance_metrics={'mape': 8.5, 'rmse': 425.0},
    description="Enhanced Prophet with volume indicators",
    variant="enhanced",
    tags=['production', 'validated', 'high_performance']
)

# 2. Load best performing model
models = storage.list_models(methodology='prophet', asset='ETH')
best_model = min(models, key=lambda m: m.performance_metrics['mape'])
model, metadata = storage.load_model(best_model.model_id)

# 3. Train all variants at once
framework = ETHProphetFrameworkWithStorage()
model_ids = framework.train_all_variants(price_data)

# 4. Compare performance
comparison = framework.compare_model_performance()
print(comparison.sort_values('mape'))

# 5. Load for production use
production_model, metadata = storage.load_latest_model('prophet', 'ETH')
future_forecast = production_model.predict(future_dataframe)
""")

if __name__ == "__main__":
    # Run comprehensive demo
    storage, framework, models = demo_storage_system()
    
    # Show usage examples
    show_usage_examples()
    
    print(f"\n🚀 ETH Prophet Storage System Demo Complete!")
    print(f"   📁 {len(models)} models stored and organized")
    print(f"   💾 Storage system ready for production use")
    print(f"   🔧 Framework ready for additional methodologies")
