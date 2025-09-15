#!/bin/bash
# Directory Cleanup and Organization Script
# Generated on 2025-09-13T13:45:19.136654

echo "🧹 Starting Myportolio Directory Organization"
echo "============================================="

# Create subdirectories
mkdir -p core backtesting archived testing analysis config

# Move core files
echo "📂 Organizing core files..."
# Core files stay in root or move to core/ as needed

# Move backtesting files
echo "📂 Organizing backtesting files..."
mv robust_backtesting_suite.py backtesting/ 2>/dev/null || true
mv parameter_optimization_backtester.py backtesting/ 2>/dev/null || true

# Archive redundant files
echo "📂 Archiving redundant files..."
mv ensemble_multi_asset_portfolio.py archived/ 2>/dev/null || true
mv robust_ensemble_simulation.py archived/ 2>/dev/null || true
mv production_ensemble_simulation.py archived/ 2>/dev/null || true
mv unicorn_ensemble_demonstration.py archived/ 2>/dev/null || true
mv ensemble_model_wrapper.py archived/ 2>/dev/null || true
mv silver_layer_integration_mapper.py archived/ 2>/dev/null || true
mv train_deploy_models.py archived/ 2>/dev/null || true

# Move testing files
echo "📂 Organizing testing files..."
mv test_kelly_implementation.py testing/ 2>/dev/null || true
mv test_integration.py testing/ 2>/dev/null || true
mv test_bitcoin_integration.py testing/ 2>/dev/null || true
mv test_algorithm_integration.py testing/ 2>/dev/null || true
mv test_frontend_integration.py testing/ 2>/dev/null || true

# Move analysis files
echo "📂 Organizing analysis files..."
mv pipeline_trace.py analysis/ 2>/dev/null || true
mv complete_pipeline_trace.py analysis/ 2>/dev/null || true
mv backtesting_analysis_summary.py analysis/ 2>/dev/null || true
mv directory_analysis.py analysis/ 2>/dev/null || true
mv system_comparison.py analysis/ 2>/dev/null || true
mv PIPELINE_TRACE_COMPLETE.md analysis/ 2>/dev/null || true
mv comprehensive_directory_review_*.json analysis/ 2>/dev/null || true

# Move config files
echo "📂 Organizing config files..."
mv config.json config/ 2>/dev/null || true
mv risk_parameters.json config/ 2>/dev/null || true
mv execution_settings.json config/ 2>/dev/null || true
mv unicorn_ensemble_integration_demonstration_*.json config/ 2>/dev/null || true

echo "✅ Directory organization complete!"
echo "📊 Check each subdirectory for organized files"
