import sqlite3
import pandas as pd

# Connect to database
conn = sqlite3.connect('multi_asset_comparison.db')

# Query all model performance data
df = pd.read_sql_query('''
    SELECT asset, model_variant, methodology, r2_score, mae, mape, 
           economic_feature_importance, total_features, economic_features, technical_features
    FROM multi_asset_performance
    ORDER BY asset, methodology, r2_score DESC
''', conn)

print('📊 COMPLETE MODEL COMPARISON RESULTS')
print('='*80)
print(f'Total models in database: {len(df)}')
print(f'Assets: {list(df["asset"].unique())}')
print(f'Methodologies: {list(df["methodology"].unique())}')
print()

# Show economic-enhanced models
print('🚀 ECONOMIC-ENHANCED MODELS')
print('-'*50)
econ_models = df[df['methodology'] == 'xgboost_economic_enhanced_demo']
for asset in ['BTC', 'ETH']:
    asset_models = econ_models[econ_models['asset'] == asset]
    if not asset_models.empty:
        print(f'{asset} Models:')
        for _, model in asset_models.iterrows():
            print(f'  {model["model_variant"].title()}: R²={model["r2_score"]:.4f}, MAE=${model["mae"]:,.0f}, Econ={model["economic_feature_importance"]:.1%}')
        print()

# Show baseline models
print('📊 BASELINE MODELS')
print('-'*50)
baseline_models = df[df['methodology'] == 'baseline_technical_only']
for asset in ['BTC', 'ETH']:
    asset_models = baseline_models[baseline_models['asset'] == asset]
    if not asset_models.empty:
        print(f'{asset} Models:')
        for _, model in asset_models.iterrows():
            variant_clean = model['model_variant'].replace('baseline_', '').replace('_', ' ').title()
            print(f'  {variant_clean}: R²={model["r2_score"]:.4f}, MAE=${model["mae"]:,.0f}')
        print()

# Performance comparison
print('🎯 PERFORMANCE COMPARISON SUMMARY')
print('-'*50)
for asset in ['BTC', 'ETH']:
    econ_asset = econ_models[econ_models['asset'] == asset]
    base_asset = baseline_models[baseline_models['asset'] == asset]
    
    if not econ_asset.empty and not base_asset.empty:
        best_econ = econ_asset.loc[econ_asset['r2_score'].idxmax()]
        best_base = base_asset.loc[base_asset['r2_score'].idxmax()]
        
        r2_improvement = ((best_econ['r2_score'] - best_base['r2_score']) / best_base['r2_score']) * 100
        mae_improvement = ((best_base['mae'] - best_econ['mae']) / best_base['mae']) * 100
        
        print(f'{asset} Best Model Comparison:')
        print(f'  Economic-Enhanced: {best_econ["model_variant"].title()} (R²={best_econ["r2_score"]:.4f})')
        print(f'  Baseline: {best_base["model_variant"].replace("baseline_", "").replace("_", " ").title()} (R²={best_base["r2_score"]:.4f})')
        print(f'  R² Improvement: {r2_improvement:+.1f}%')
        print(f'  MAE Improvement: {mae_improvement:+.1f}%')
        print(f'  Economic Features: {best_econ["economic_features"]} features contributing {best_econ["economic_feature_importance"]:.1%}')
        print()

# Feature analysis
print('🔍 FEATURE IMPORTANCE ANALYSIS')
print('-'*50)
feature_analysis = econ_models.groupby('asset').agg({
    'economic_feature_importance': ['mean', 'std'],
    'economic_features': 'mean',
    'technical_features': 'mean',
    'total_features': 'mean'
}).round(3)

for asset in ['BTC', 'ETH']:
    if asset in feature_analysis.index:
        print(f'{asset} Feature Analysis:')
        econ_imp_mean = feature_analysis.loc[asset, ('economic_feature_importance', 'mean')]
        econ_imp_std = feature_analysis.loc[asset, ('economic_feature_importance', 'std')]
        econ_feat_avg = feature_analysis.loc[asset, ('economic_features', 'mean')]
        tech_feat_avg = feature_analysis.loc[asset, ('technical_features', 'mean')]
        total_feat_avg = feature_analysis.loc[asset, ('total_features', 'mean')]
        
        print(f'  Average Economic Importance: {econ_imp_mean:.1%} ± {econ_imp_std:.1%}')
        print(f'  Average Economic Features: {econ_feat_avg:.0f}')
        print(f'  Average Technical Features: {tech_feat_avg:.0f}')
        print(f'  Average Total Features: {total_feat_avg:.0f}')
        print()

print('✅ COMPREHENSIVE MODEL ANALYSIS COMPLETE')
print(f'📊 Database: multi_asset_comparison.db contains {len(df)} model records')

conn.close()
