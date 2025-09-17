#!/usr/bin/env python3
"""
Comprehensive XGBoost Model Rebuilding Campaign

Rebuilds all XGBoost models (forex + crypto) with leak-free features
to eliminate the overfitting patterns detected in validation.
"""

import os
import sys
from pathlib import Path
from datetime import datetime
import pandas as pd
import sqlite3

# Add project root to path
current_dir = Path(__file__).resolve().parent
project_root = current_dir.parent.parent
sys.path.append(str(project_root))

# Import our enhanced builder
from enhanced_xgboost_builder import EnhancedXGBoostBuilder

class XGBoostModelRebuilder:
    """Comprehensive XGBoost model rebuilding manager."""
    
    def __init__(self):
        self.results = []
        self.data_sources = {
            'forex': '/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/market_data/yahoo_finance_assets',
            'crypto': '/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/2_bronze/market_data/yahoo_finance_assets'
        }
        
        # Assets to rebuild based on validation results
        self.assets = {
            'forex': ['EURUSD', 'USDJPY', 'GBPUSD', 'AUDUSD', 'USDCHF', 'USDCAD', 'NZDUSD'],
            'crypto': ['BTC', 'ETH']
        }
        
    def load_real_data(self, asset: str, asset_type: str) -> pd.DataFrame:
        """Load real market data for asset."""
        try:
            if asset_type == 'forex':
                # Forex data in yahoo format (e.g., EURUSD=X)
                symbol = f"{asset}=X" if not asset.endswith('=X') else asset
                data_file = os.path.join(self.data_sources['forex'], f"{symbol}_1hour.csv")
            else:
                # Crypto data (e.g., BTC-USD)
                symbol = f"{asset}-USD" if not asset.endswith('-USD') else asset
                data_file = os.path.join(self.data_sources['crypto'], f"{symbol}_1hour.csv")
                
            if os.path.exists(data_file):
                df = pd.read_csv(data_file, index_col='timestamp', parse_dates=True)
                
                # Standardize column names
                df.columns = [col.lower() for col in df.columns]
                
                # Ensure required columns
                required_cols = ['open', 'high', 'low', 'close']
                if all(col in df.columns for col in required_cols):
                    # Take recent data for faster processing
                    df = df.tail(5000)  # Last 5000 hours ≈ 7 months
                    return df
                    
        except Exception as e:
            print(f"Warning: Could not load {asset} data: {e}")
            
        return None
        
    def rebuild_asset_models(self, asset: str, asset_type: str) -> dict:
        """Rebuild XGBoost models for a specific asset."""
        print(f"\n🔄 Rebuilding {asset_type.upper()} {asset} XGBoost Models")
        print("-" * 60)
        
        # Load real data
        df = self.load_real_data(asset, asset_type)
        
        # Initialize builder
        builder = EnhancedXGBoostBuilder(asset, asset_type)
        
        # Use real data if available, otherwise synthetic
        if df is not None:
            print(f"✅ Using real data: {len(df)} samples, {df.index[0]} to {df.index[-1]}")
            data = df
        else:
            print(f"⚠️ Using synthetic data (real data not found)")
            data = builder.get_sample_data(2000)  # More data for better validation
            
        # Build multiple model variants
        variants = ['production', 'conservative', 'aggressive']
        asset_results = []
        
        for variant in variants:
            print(f"\n📊 Building {variant} variant...")
            
            # Adjust model parameters by variant
            if variant == 'conservative':
                # More regularization
                builder.success_criteria['max_r2'] = 0.3  # Lower threshold
            elif variant == 'aggressive':
                # Less regularization but still realistic
                builder.success_criteria['max_r2'] = 0.6  # Higher but realistic
            else:  # production
                # Standard settings
                builder.success_criteria['max_r2'] = 0.5  # Balanced
                
            result = builder.train_enhanced_model(data, variant)
            result['real_data_used'] = df is not None
            result['data_samples'] = len(data)
            
            asset_results.append(result)
            
        return {
            'asset': asset,
            'asset_type': asset_type,
            'models': asset_results,
            'real_data_available': df is not None,
            'total_samples': len(data) if df is not None else len(data)
        }
        
    def rebuild_all_models(self):
        """Execute comprehensive model rebuilding campaign."""
        print("🚀 XGBoost Model Rebuilding Campaign")
        print("=" * 80)
        print("Eliminating overfitting patterns detected in validation:")
        print("• 9/10 models using training data evaluation")
        print("• Unrealistic R² scores (98%+)")
        print("• OHLC-derived feature leakage")
        print("=" * 80)
        
        campaign_results = []
        
        # Rebuild all forex models
        print(f"\n💰 FOREX MODELS ({len(self.assets['forex'])} assets)")
        for asset in self.assets['forex']:
            asset_result = self.rebuild_asset_models(asset, 'forex')
            campaign_results.append(asset_result)
            
        # Rebuild all crypto models  
        print(f"\n🪙 CRYPTO MODELS ({len(self.assets['crypto'])} assets)")
        for asset in self.assets['crypto']:
            asset_result = self.rebuild_asset_models(asset, 'crypto')
            campaign_results.append(asset_result)
            
        # Generate campaign summary
        self.generate_campaign_summary(campaign_results)
        
        return campaign_results
        
    def generate_campaign_summary(self, campaign_results):
        """Generate comprehensive campaign summary."""
        print(f"\n📊 CAMPAIGN SUMMARY")
        print("=" * 80)
        
        total_models = 0
        successful_models = 0
        overfitting_eliminated = 0
        realistic_performance = 0
        
        asset_type_stats = {'forex': {}, 'crypto': {}}
        
        for asset_result in campaign_results:
            asset = asset_result['asset']
            asset_type = asset_result['asset_type']
            models = asset_result['models']
            
            print(f"\n{asset_type.upper()} {asset}:")
            print(f"  Real Data: {'✅' if asset_result['real_data_available'] else '❌'}")
            print(f"  Samples: {asset_result['total_samples']:,}")
            
            for model in models:
                total_models += 1
                variant = model['variant']
                
                if model['success']:
                    successful_models += 1
                if model['overfitting_eliminated']:
                    overfitting_eliminated += 1
                if model['realistic_performance']:
                    realistic_performance += 1
                    
                status = "✅" if model['success'] else "❌"
                print(f"    {variant}: {status} R²={model['validation_r2']:.4f} MAPE={model['validation_mape']:.1f}%")
                
        # Overall statistics
        print(f"\n🎯 OVERALL RESULTS:")
        print(f"  Total Models Built: {total_models}")
        print(f"  Successful Models: {successful_models} ({100*successful_models/total_models:.1f}%)")
        print(f"  Overfitting Eliminated: {overfitting_eliminated} ({100*overfitting_eliminated/total_models:.1f}%)")
        print(f"  Realistic Performance: {realistic_performance} ({100*realistic_performance/total_models:.1f}%)")
        
        # Success assessment
        overfitting_elimination_rate = 100 * overfitting_eliminated / total_models
        
        if overfitting_elimination_rate >= 90:
            print(f"\n🎉 CAMPAIGN SUCCESS: {overfitting_elimination_rate:.1f}% overfitting elimination!")
            print("✅ XGBoost models now have realistic financial performance")
        elif overfitting_elimination_rate >= 70:
            print(f"\n⚠️ PARTIAL SUCCESS: {overfitting_elimination_rate:.1f}% overfitting elimination")
            print("🔧 Some models may need additional tuning")
        else:
            print(f"\n❌ CAMPAIGN NEEDS REVISION: {overfitting_elimination_rate:.1f}% overfitting elimination")
            print("🚨 Significant overfitting patterns remain")
            
        # Save summary to file
        self.save_campaign_summary(campaign_results, total_models, successful_models, overfitting_eliminated)
        
    def save_campaign_summary(self, results, total, successful, overfitting_eliminated):
        """Save campaign summary to file."""
        summary_file = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/validation/xgboost_rebuilding_campaign_summary.md"
        
        with open(summary_file, 'w') as f:
            f.write("# XGBoost Model Rebuilding Campaign Summary\\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\\n\\n")
            
            f.write("## Campaign Objectives\\n")
            f.write("- Eliminate training data evaluation (9/10 models affected)\\n")
            f.write("- Remove OHLC-derived feature leakage\\n")
            f.write("- Achieve realistic financial time series performance\\n")
            f.write("- Implement leak-free feature engineering\\n\\n")
            
            f.write("## Results Summary\\n")
            f.write(f"- **Total Models**: {total}\\n")
            f.write(f"- **Successful Models**: {successful} ({100*successful/total:.1f}%)\\n")
            f.write(f"- **Overfitting Eliminated**: {overfitting_eliminated} ({100*overfitting_eliminated/total:.1f}%)\\n\\n")
            
            f.write("## Individual Asset Results\\n")
            for asset_result in results:
                f.write(f"### {asset_result['asset_type'].upper()} {asset_result['asset']}\\n")
                f.write(f"- Real Data: {'Available' if asset_result['real_data_available'] else 'Synthetic'}\\n")
                f.write(f"- Samples: {asset_result['total_samples']:,}\\n")
                
                for model in asset_result['models']:
                    success = "✅" if model['success'] else "❌"
                    f.write(f"- {model['variant']}: {success} R²={model['validation_r2']:.4f}, MAPE={model['validation_mape']:.1f}%\\n")
                f.write("\\n")
                
        print(f"\\n📋 Campaign summary saved: {summary_file}")

def main():
    """Main execution."""
    rebuilder = XGBoostModelRebuilder()
    results = rebuilder.rebuild_all_models()
    
    print(f"\\n🏁 XGBoost rebuilding campaign completed!")
    print(f"📊 {len(results)} assets processed")

if __name__ == "__main__":
    main()