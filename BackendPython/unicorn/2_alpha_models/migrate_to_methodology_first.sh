#!/bin/bash

# Alpha Models Migration Script - Methodology-First Architecture
# This script migrates the existing asset-first structure to methodology-first architecture

set -e  # Exit on any error

ALPHA_MODELS_DIR="/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models"
cd "$ALPHA_MODELS_DIR"

echo "🚀 Starting Alpha Models Migration to Methodology-First Architecture"
echo "Working directory: $(pwd)"

# Create backup timestamp
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
echo "Migration timestamp: $TIMESTAMP"

# ========================================
# PHASE 1: MOVE EXISTING DIRECTORIES TO LEGACY
# ========================================
echo ""
echo "📦 Phase 1: Moving existing directories to legacy..."

# Move remaining asset directories to legacy
if [ -d "EQUITIES" ]; then
    echo "Moving EQUITIES to legacy..."
    mv EQUITIES legacy/EQUITIES_original
fi

# Move model directories to legacy
if [ -d "enhanced_prophet_models" ]; then
    echo "Moving enhanced_prophet_models to legacy..."
    mv enhanced_prophet_models legacy/enhanced_prophet_models_original
fi

if [ -d "multi_asset_models" ]; then
    echo "Moving multi_asset_models to legacy..."
    mv multi_asset_models legacy/multi_asset_models_original
fi

if [ -d "fixed_multi_asset_models" ]; then
    echo "Moving fixed_multi_asset_models to legacy..."
    mv fixed_multi_asset_models legacy/fixed_multi_asset_models_original
fi

# Move utility directories to legacy
if [ -d "shared" ]; then
    echo "Moving shared to legacy..."
    mv shared legacy/shared_original
fi

if [ -d "utils" ]; then
    echo "Moving utils to legacy..."
    mv utils legacy/utils_original
fi

if [ -d "performance_analysis" ]; then
    echo "Moving performance_analysis to legacy..."
    mv performance_analysis legacy/performance_analysis_original
fi

if [ -d "validation_results" ]; then
    echo "Moving validation_results to legacy..."
    mv validation_results legacy/validation_results_original
fi

# Move existing scripts and examples to legacy
if [ -d "scripts" ]; then
    echo "Moving scripts to legacy..."
    mv scripts legacy/scripts_original
fi

if [ -d "examples" ]; then
    echo "Moving examples to legacy..."
    mv examples legacy/examples_original
fi

# ========================================
# PHASE 2: MOVE BUILDER SCRIPTS TO LEGACY
# ========================================
echo ""
echo "🔧 Phase 2: Moving builder scripts to legacy..."

# Create deprecated builders directory
mkdir -p legacy/deprecated_builders

# Move all builder scripts
for builder_script in \
    "enhanced_forex_prophet_builder.py" \
    "enhanced_forex_prophet_model_builder.py" \
    "enhanced_prophet_model_builder.py" \
    "fixed_multi_asset_model_builder.py" \
    "multi_asset_model_builder.py" \
    "multi_asset_model_builder_fixed.py" \
    "individual_asset_model_generator.py"
do
    if [ -f "$builder_script" ]; then
        echo "Moving $builder_script to legacy/deprecated_builders/"
        mv "$builder_script" legacy/deprecated_builders/
    fi
done

# Move validation scripts that will be refactored
if [ -f "forex_model_validator.py" ]; then
    echo "Moving forex_model_validator.py to legacy..."
    mv forex_model_validator.py legacy/deprecated_builders/
fi

if [ -f "validate_architecture.py" ]; then
    echo "Moving validate_architecture.py to legacy..."
    mv validate_architecture.py legacy/deprecated_builders/
fi

# ========================================
# PHASE 3: RENAME NEW DIRECTORIES
# ========================================
echo ""
echo "📁 Phase 3: Activating new directory structure..."

# Rename new directories to their final names
if [ -d "scripts_new" ]; then
    echo "Activating scripts_new -> scripts"
    mv scripts_new scripts
fi

if [ -d "examples_new" ]; then
    echo "Activating examples_new -> examples"
    mv examples_new examples
fi

# ========================================
# PHASE 4: CREATE INIT FILES
# ========================================
echo ""
echo "📄 Phase 4: Creating __init__.py files..."

# Function to create init files
create_init_file() {
    local dir_path="$1"
    local content="$2"
    
    if [ -d "$dir_path" ] && [ ! -f "$dir_path/__init__.py" ]; then
        echo "Creating $dir_path/__init__.py"
        echo "$content" > "$dir_path/__init__.py"
    fi
}

# Methodologies init files
create_init_file "methodologies" "# Alpha Models Methodologies Package"
create_init_file "methodologies/prophet" "# Prophet Methodology Package"
create_init_file "methodologies/prophet/core" "# Prophet Core Implementation"
create_init_file "methodologies/prophet/adapters" "# Prophet Asset Adapters"
create_init_file "methodologies/prophet/configs" "# Prophet Configurations"
create_init_file "methodologies/prophet/models" "# Prophet Model Storage"
create_init_file "methodologies/prophet/models/crypto" "# Prophet Crypto Models"
create_init_file "methodologies/prophet/models/forex" "# Prophet Forex Models"
create_init_file "methodologies/prophet/models/metadata" "# Prophet Model Metadata"

create_init_file "methodologies/xgboost" "# XGBoost Methodology Package"
create_init_file "methodologies/xgboost/core" "# XGBoost Core Implementation"
create_init_file "methodologies/xgboost/adapters" "# XGBoost Asset Adapters"
create_init_file "methodologies/xgboost/configs" "# XGBoost Configurations"
create_init_file "methodologies/xgboost/models" "# XGBoost Model Storage"
create_init_file "methodologies/xgboost/models/crypto" "# XGBoost Crypto Models"
create_init_file "methodologies/xgboost/models/forex" "# XGBoost Forex Models"
create_init_file "methodologies/xgboost/models/metadata" "# XGBoost Model Metadata"

create_init_file "methodologies/ensemble" "# Ensemble Methodology Package"
create_init_file "methodologies/ensemble/core" "# Ensemble Core Implementation"
create_init_file "methodologies/ensemble/adapters" "# Ensemble Asset Adapters"
create_init_file "methodologies/ensemble/configs" "# Ensemble Configurations"
create_init_file "methodologies/ensemble/models" "# Ensemble Model Storage"
create_init_file "methodologies/ensemble/models/crypto" "# Ensemble Crypto Models"
create_init_file "methodologies/ensemble/models/forex" "# Ensemble Forex Models"
create_init_file "methodologies/ensemble/models/metadata" "# Ensemble Model Metadata"

# Assets init files
create_init_file "assets" "# Asset Adapters Package"
create_init_file "assets/crypto" "# Cryptocurrency Asset Adapters"
create_init_file "assets/forex" "# Forex Asset Adapters"
create_init_file "assets/equities" "# Equity Asset Adapters"

# Core validation and orchestration init files
create_init_file "core/validation" "# Model Validation Framework"
create_init_file "core/orchestration" "# Multi-Methodology Orchestration"

# Scripts and examples init files
create_init_file "scripts" "# Alpha Models Scripts"
create_init_file "scripts/training" "# Training Scripts"
create_init_file "scripts/utilities" "# Utility Scripts"
create_init_file "scripts/migration" "# Migration Scripts"
create_init_file "examples" "# Alpha Models Examples"
create_init_file "examples/methodology_examples" "# Methodology Examples"
create_init_file "examples/asset_examples" "# Asset Examples"
create_init_file "examples/integration_examples" "# Integration Examples"

# ========================================
# PHASE 5: MOVE MODEL PERFORMANCE DATABASE
# ========================================
echo ""
echo "💾 Phase 5: Moving model performance database..."

if [ -f "model_performance.db" ]; then
    echo "Moving model_performance.db to storage/metadata/"
    mv model_performance.db storage/metadata/
fi

# ========================================
# PHASE 6: CREATE PLACEHOLDER FILES
# ========================================
echo ""
echo "📝 Phase 6: Creating placeholder files for future implementation..."

# Create placeholder files to indicate where code will go
create_placeholder() {
    local file_path="$1"
    local description="$2"
    local class_name="$3"
    
    if [ ! -f "$file_path" ]; then
        echo "Creating placeholder: $file_path"
        cat > "$file_path" << EOF
"""
$description

This is a placeholder file for the methodology-first architecture migration.
Implementation will be added in subsequent phases.
"""

# TODO: Implement $class_name
# Migrate functionality from legacy asset-first structure

class $class_name:
    """Placeholder for $description"""
    
    def __init__(self):
        """Initialize $class_name"""
        raise NotImplementedError("This class will be implemented in migration Phase 2-4")

# TODO: Add implementation from legacy files
EOF
    fi
}

# Methodology core placeholders
create_placeholder "methodologies/prophet/core/prophet_methodology.py" "Prophet Methodology Implementation" "ProphetMethodology"
create_placeholder "methodologies/prophet/core/feature_engineering.py" "Prophet Feature Engineering" "ProphetFeatureEngine"
create_placeholder "methodologies/prophet/core/validation.py" "Prophet Validation Logic" "ProphetValidator"

create_placeholder "methodologies/xgboost/core/xgboost_methodology.py" "XGBoost Methodology Implementation" "XGBoostMethodology"
create_placeholder "methodologies/xgboost/core/feature_engineering.py" "XGBoost Feature Engineering" "XGBoostFeatureEngine"
create_placeholder "methodologies/xgboost/core/validation.py" "XGBoost Validation Logic" "XGBoostValidator"

create_placeholder "methodologies/ensemble/core/ensemble_methodology.py" "Ensemble Methodology Implementation" "EnsembleMethodology"
create_placeholder "methodologies/ensemble/core/combination_strategies.py" "Ensemble Combination Strategies" "EnsembleCombiner"
create_placeholder "methodologies/ensemble/core/validation.py" "Ensemble Validation Logic" "EnsembleValidator"

# Asset adapter placeholders
create_placeholder "assets/base_adapter.py" "Base Asset Adapter Interface" "BaseAssetAdapter"
create_placeholder "assets/crypto/crypto_adapter.py" "Cryptocurrency Asset Adapter" "CryptoAssetAdapter"
create_placeholder "assets/crypto/eth_adapter.py" "Ethereum Specific Adapter" "EthereumAdapter"
create_placeholder "assets/crypto/btc_adapter.py" "Bitcoin Specific Adapter" "BitcoinAdapter"
create_placeholder "assets/forex/forex_adapter.py" "Forex Asset Adapter" "ForexAssetAdapter"
create_placeholder "assets/forex/major_pairs_adapter.py" "Major Forex Pairs Adapter" "MajorPairsAdapter"
create_placeholder "assets/equities/equity_adapter.py" "Equity Asset Adapter" "EquityAssetAdapter"

# Methodology adapters placeholders
create_placeholder "methodologies/prophet/adapters/crypto_adapter.py" "Prophet Crypto Adapter" "ProphetCryptoAdapter"
create_placeholder "methodologies/prophet/adapters/forex_adapter.py" "Prophet Forex Adapter" "ProphetForexAdapter"
create_placeholder "methodologies/prophet/adapters/equity_adapter.py" "Prophet Equity Adapter" "ProphetEquityAdapter"

create_placeholder "methodologies/xgboost/adapters/crypto_adapter.py" "XGBoost Crypto Adapter" "XGBoostCryptoAdapter"
create_placeholder "methodologies/xgboost/adapters/forex_adapter.py" "XGBoost Forex Adapter" "XGBoostForexAdapter"
create_placeholder "methodologies/xgboost/adapters/equity_adapter.py" "XGBoost Equity Adapter" "XGBoostEquityAdapter"

create_placeholder "methodologies/ensemble/adapters/crypto_adapter.py" "Ensemble Crypto Adapter" "EnsembleCryptoAdapter"
create_placeholder "methodologies/ensemble/adapters/forex_adapter.py" "Ensemble Forex Adapter" "EnsembleForexAdapter"
create_placeholder "methodologies/ensemble/adapters/equity_adapter.py" "Ensemble Equity Adapter" "EnsembleEquityAdapter"

# Core framework placeholders
create_placeholder "core/validation/performance_metrics.py" "Performance Metrics Framework" "PerformanceMetrics"
create_placeholder "core/validation/cross_validation.py" "Cross Validation Framework" "CrossValidator"
create_placeholder "core/validation/backtesting.py" "Backtesting Framework" "BacktestEngine"

create_placeholder "core/orchestration/model_orchestrator.py" "Model Orchestration Engine" "ModelOrchestrator"
create_placeholder "core/orchestration/training_coordinator.py" "Training Coordination" "TrainingCoordinator"
create_placeholder "core/orchestration/forecast_coordinator.py" "Forecast Coordination" "ForecastCoordinator"

# ========================================
# PHASE 7: CREATE DEFAULT CONFIGURATIONS
# ========================================
echo ""
echo "⚙️ Phase 7: Creating default configuration files..."

# Prophet configurations
if [ ! -f "methodologies/prophet/configs/default_config.json" ]; then
    echo "Creating Prophet default configuration..."
    cat > "methodologies/prophet/configs/default_config.json" << 'EOF'
{
    "methodology_name": "prophet",
    "version": "1.0.0",
    "parameters": {
        "seasonality_mode": "multiplicative",
        "yearly_seasonality": true,
        "weekly_seasonality": true,
        "daily_seasonality": false,
        "holidays_prior_scale": 10.0,
        "seasonality_prior_scale": 10.0,
        "changepoint_prior_scale": 0.05,
        "mcmc_samples": 0,
        "interval_width": 0.8,
        "uncertainty_samples": 1000
    },
    "feature_engineering": {
        "price_transforms": ["log", "diff"],
        "technical_indicators": ["sma", "ema", "rsi", "macd"],
        "lag_features": [1, 2, 3, 5, 10],
        "rolling_windows": [5, 10, 20, 50]
    },
    "validation": {
        "cv_folds": 5,
        "horizon_days": 30,
        "metrics": ["mae", "mape", "rmse", "directional_accuracy"]
    }
}
EOF
fi

if [ ! -f "methodologies/prophet/configs/crypto_overrides.json" ]; then
    echo "Creating Prophet crypto overrides..."
    cat > "methodologies/prophet/configs/crypto_overrides.json" << 'EOF'
{
    "crypto_specific": {
        "seasonality_mode": "additive",
        "changepoint_prior_scale": 0.01,
        "holidays_prior_scale": 0.1,
        "volatility_adjustment": true,
        "24h_seasonality": true
    },
    "feature_engineering": {
        "crypto_features": ["volatility", "volume_profile", "market_cap_rank"],
        "defi_indicators": ["tvl", "yield_farming_apy"],
        "sentiment_features": ["fear_greed_index", "social_volume"]
    }
}
EOF
fi

if [ ! -f "methodologies/prophet/configs/forex_overrides.json" ]; then
    echo "Creating Prophet forex overrides..."
    cat > "methodologies/prophet/configs/forex_overrides.json" << 'EOF'
{
    "forex_specific": {
        "seasonality_mode": "multiplicative",
        "market_hours_seasonality": true,
        "economic_calendar_integration": true,
        "central_bank_policy_weight": 0.3
    },
    "feature_engineering": {
        "forex_features": ["carry_trade", "purchasing_power_parity", "real_exchange_rate"],
        "economic_indicators": ["interest_rate_differential", "inflation_differential", "gdp_growth"],
        "market_microstructure": ["bid_ask_spread", "order_flow", "position_of_traders"]
    }
}
EOF
fi

# XGBoost configurations
if [ ! -f "methodologies/xgboost/configs/default_config.json" ]; then
    echo "Creating XGBoost default configuration..."
    cat > "methodologies/xgboost/configs/default_config.json" << 'EOF'
{
    "methodology_name": "xgboost",
    "version": "1.0.0",
    "parameters": {
        "objective": "reg:squarederror",
        "eval_metric": "rmse",
        "max_depth": 6,
        "learning_rate": 0.1,
        "n_estimators": 100,
        "subsample": 0.8,
        "colsample_bytree": 0.8,
        "min_child_weight": 1,
        "gamma": 0,
        "reg_alpha": 0,
        "reg_lambda": 1,
        "random_state": 42
    },
    "feature_engineering": {
        "lag_features": [1, 2, 3, 5, 10, 20],
        "rolling_features": [5, 10, 20, 50],
        "technical_indicators": ["rsi", "macd", "bollinger_bands", "stochastic"],
        "price_transforms": ["returns", "log_returns", "volatility"]
    },
    "validation": {
        "cv_folds": 5,
        "early_stopping_rounds": 50,
        "test_size": 0.2,
        "metrics": ["rmse", "mae", "r2", "directional_accuracy"]
    }
}
EOF
fi

# Ensemble configurations
if [ ! -f "methodologies/ensemble/configs/default_config.json" ]; then
    echo "Creating Ensemble default configuration..."
    cat > "methodologies/ensemble/configs/default_config.json" << 'EOF'
{
    "methodology_name": "ensemble",
    "version": "1.0.0",
    "parameters": {
        "combination_method": "weighted_average",
        "weight_optimization": "variance_minimization",
        "rebalance_frequency": "daily",
        "min_models": 2,
        "max_models": 5
    },
    "constituent_methodologies": ["prophet", "xgboost"],
    "weighting_schemes": {
        "equal_weight": {"prophet": 0.5, "xgboost": 0.5},
        "performance_weighted": {"lookback_days": 30, "metric": "sharpe_ratio"},
        "volatility_adjusted": {"vol_lookback": 20, "target_vol": 0.15}
    },
    "validation": {
        "out_of_sample_period": 0.2,
        "walk_forward_analysis": true,
        "ensemble_metrics": ["ensemble_sharpe", "diversification_ratio", "model_correlation"]
    }
}
EOF
fi

# ========================================
# PHASE 8: UPDATE STORAGE STRUCTURE
# ========================================
echo ""
echo "💾 Phase 8: Updating storage structure..."

# Create storage artifacts directories by methodology
mkdir -p storage/artifacts/prophet
mkdir -p storage/artifacts/xgboost  
mkdir -p storage/artifacts/ensemble
mkdir -p storage/artifacts/metadata

# Create storage performance additional directories
mkdir -p storage/performance

# ========================================
# MIGRATION COMPLETION
# ========================================
echo ""
echo "✅ Migration Phase 1 Complete!"
echo ""
echo "📊 Migration Summary:"
echo "  ✅ Created methodology-first directory structure"
echo "  ✅ Moved legacy asset-first directories to legacy/"
echo "  ✅ Moved deprecated builder scripts to legacy/deprecated_builders/"
echo "  ✅ Created __init__.py files for all packages"
echo "  ✅ Created placeholder implementation files"
echo "  ✅ Created default configuration files"
echo "  ✅ Updated storage structure"
echo ""
echo "📁 New Directory Structure:"
find . -type d -name "__pycache__" -prune -o -type d -print | sort | head -30
echo ""
echo "🔧 Legacy Files Preserved In:"
ls -la legacy/
echo ""
echo "⚠️  Next Steps:"
echo "  1. Implement FeaturePipeline class (core/data_pipeline/feature_pipeline.py)"
echo "  2. Begin Phase 2: Asset adapter implementation"
echo "  3. Begin Phase 3: Methodology migration from legacy files"
echo "  4. Update import statements and dependencies"
echo "  5. Implement integration tests"
echo ""
echo "🎯 Migration script completed successfully at $(date)"