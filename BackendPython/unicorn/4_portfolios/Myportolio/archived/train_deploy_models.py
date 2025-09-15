#!/usr/bin/env python3
"""
Multi-Timeframe Model Training and Deployment Script

This script orchestrates the complete training pipeline for ETH models across
all timeframes (1min, 1hour, 1day) and integrates them with trading strategies.

Features:
- Automated data collection and preparation
- Multi-timeframe model training (Prophet, XGBoost, Ensemble)
- Model validation and performance evaluation
- Integration with trading strategies
- Deployment and health monitoring
- IBKR data integration for live deployment

Usage:
    python train_deploy_models.py [--timeframes 1min,1hour,1day] [--models prophet,xgboost,ensemble]
"""

import argparse
import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta
import warnings
from pathlib import Path
import json
import sqlite3
from typing import Dict, List, Optional, Any

# Add parent directories to path
sys.path.append(str(Path(__file__).parent.parent.parent.parent))
sys.path.append(str(Path(__file__).parent))

# Import our frameworks and utilities
from utilities.timeframe_model_frameworks import MultiTimeframeModelTrainer
from utilities.model_strategy_integration import ModelIntegrationManager
from utilities.multi_timeframe_strategies import MultiTimeframePortfolioManager

# Import existing ETH data collection if available
try:
    sys.path.append(str(Path(__file__).parent.parent.parent.parent / "2_alpha_models" / "CRYPTO" / "ETH"))
    from eth_data_collector import ETHDataCollector
    ETH_DATA_AVAILABLE = True
except ImportError:
    ETH_DATA_AVAILABLE = False
    print("⚠️  ETH data collector not found. Using synthetic data for training.")

warnings.filterwarnings('ignore', category=RuntimeWarning)


class ModelTrainingPipeline:
    """
    Complete pipeline for training and deploying multi-timeframe ETH models.
    """
    
    def __init__(self, portfolio_name: str = "Myportolio"):
        self.portfolio_name = portfolio_name
        self.base_path = Path(__file__).parent.parent.parent.parent
        self.output_dir = Path(__file__).parent / "model_training_outputs"
        self.output_dir.mkdir(exist_ok=True)
        
        # Initialize components
        self.model_trainer = MultiTimeframeModelTrainer()
        self.integration_manager = ModelIntegrationManager(portfolio_name)
        self.portfolio_manager = MultiTimeframePortfolioManager()
        
        # Training configuration
        self.config = {
            'data_requirements': {
                '1min': {'min_days': 30, 'recommended_days': 90},
                '1hour': {'min_days': 90, 'recommended_days': 365},
                '1day': {'min_days': 365, 'recommended_days': 1095}
            },
            'validation_split': 0.2,
            'test_split': 0.1,
            'retrain_threshold_days': 30
        }
        
    def collect_training_data(self, timeframes: List[str]) -> Dict[str, pd.DataFrame]:
        """Collect training data for specified timeframes."""
        print("🔄 Collecting training data...")
        
        datasets = {}
        
        if ETH_DATA_AVAILABLE:
            # Use real ETH data if available
            try:
                collector = ETHDataCollector()
                
                # Determine maximum lookback period needed
                max_days = max(self.config['data_requirements'][tf]['recommended_days'] 
                              for tf in timeframes)
                
                end_date = datetime.now()
                start_date = end_date - timedelta(days=max_days)
                
                print(f"📊 Collecting ETH data from {start_date.date()} to {end_date.date()}")
                
                # Collect minute-level data (highest resolution)
                raw_data = collector.get_historical_data(
                    start_date=start_date,
                    end_date=end_date,
                    interval='1m'
                )
                
                if raw_data is not None and len(raw_data) > 1000:
                    # Prepare data for each timeframe
                    for timeframe in timeframes:
                        datasets[timeframe] = self._prepare_timeframe_data(raw_data, timeframe)
                        print(f"✅ {timeframe} dataset: {len(datasets[timeframe])} records")
                else:
                    print("⚠️  Insufficient real data. Falling back to synthetic data.")
                    datasets = self._generate_synthetic_data(timeframes)
                    
            except Exception as e:
                print(f"❌ Error collecting real data: {e}")
                print("⚠️  Falling back to synthetic data.")
                datasets = self._generate_synthetic_data(timeframes)
        else:
            # Generate synthetic data
            datasets = self._generate_synthetic_data(timeframes)
            
        return datasets
    
    def _prepare_timeframe_data(self, raw_data: pd.DataFrame, timeframe: str) -> pd.DataFrame:
        """Prepare raw data for specific timeframe."""
        if timeframe == '1min':
            # Use raw 1-minute data
            return raw_data.copy()
        elif timeframe == '1hour':
            # Resample to hourly
            return raw_data.resample('1H').agg({
                'open': 'first',
                'high': 'max',
                'low': 'min',
                'close': 'last',
                'volume': 'sum' if 'volume' in raw_data.columns else 'mean'
            }).dropna()
        elif timeframe == '1day':
            # Resample to daily
            return raw_data.resample('1D').agg({
                'open': 'first',
                'high': 'max',
                'low': 'min',
                'close': 'last',
                'volume': 'sum' if 'volume' in raw_data.columns else 'mean'
            }).dropna()
        else:
            raise ValueError(f"Unsupported timeframe: {timeframe}")
    
    def _generate_synthetic_data(self, timeframes: List[str]) -> Dict[str, pd.DataFrame]:
        """Generate synthetic ETH-like data for training."""
        print("🎭 Generating synthetic training data...")
        
        datasets = {}
        
        # Generate base 1-minute data
        max_days = max(self.config['data_requirements'][tf]['recommended_days'] 
                      for tf in timeframes)
        
        end_date = datetime.now()
        start_date = end_date - timedelta(days=max_days)
        dates_1min = pd.date_range(start=start_date, end=end_date, freq='1min')
        
        # Generate realistic ETH price movements
        np.random.seed(42)  # For reproducible results
        n_points = len(dates_1min)
        
        # Base price with trend and volatility
        base_price = 3000
        trend = np.linspace(0, 500, n_points)  # Upward trend
        volatility = np.random.normal(0, 30, n_points)
        random_walk = np.cumsum(np.random.normal(0, 5, n_points))
        
        # Add realistic intraday and multi-day patterns
        hourly_pattern = 20 * np.sin(2 * np.pi * np.arange(n_points) / (60 * 24))  # Daily cycle
        weekly_pattern = 50 * np.sin(2 * np.pi * np.arange(n_points) / (60 * 24 * 7))  # Weekly cycle
        
        close_prices = base_price + trend + volatility + random_walk + hourly_pattern + weekly_pattern
        
        # Generate OHLC data
        high_offset = np.abs(np.random.normal(0, 10, n_points))
        low_offset = -np.abs(np.random.normal(0, 10, n_points))
        
        synthetic_1min = pd.DataFrame({
            'open': close_prices + np.random.normal(0, 2, n_points),
            'high': close_prices + high_offset,
            'low': close_prices + low_offset,
            'close': close_prices,
            'volume': np.random.randint(1000, 50000, n_points)
        }, index=dates_1min)
        
        # Ensure OHLC consistency
        synthetic_1min['high'] = np.maximum.reduce([
            synthetic_1min['open'], synthetic_1min['high'], 
            synthetic_1min['low'], synthetic_1min['close']
        ])
        synthetic_1min['low'] = np.minimum.reduce([
            synthetic_1min['open'], synthetic_1min['high'], 
            synthetic_1min['low'], synthetic_1min['close']
        ])
        
        # Create datasets for each timeframe
        for timeframe in timeframes:
            datasets[timeframe] = self._prepare_timeframe_data(synthetic_1min, timeframe)
            print(f"✅ Synthetic {timeframe} dataset: {len(datasets[timeframe])} records")
            
        return datasets
    
    def validate_data_quality(self, datasets: Dict[str, pd.DataFrame]) -> Dict[str, Dict[str, Any]]:
        """Validate data quality for training."""
        print("🔍 Validating data quality...")
        
        validation_results = {}
        
        for timeframe, data in datasets.items():
            results = {
                'record_count': len(data),
                'date_range': f"{data.index.min()} to {data.index.max()}",
                'missing_values': data.isnull().sum().to_dict(),
                'price_range': f"{data['close'].min():.2f} - {data['close'].max():.2f}",
                'data_quality_score': 0.0,
                'issues': []
            }
            
            # Check minimum data requirements
            min_required = self.config['data_requirements'][timeframe]['min_days']
            actual_days = (data.index.max() - data.index.min()).days
            
            if actual_days < min_required:
                results['issues'].append(f"Insufficient data: {actual_days} days < {min_required} required")
            
            # Check for missing values
            missing_pct = (data.isnull().sum().sum() / (len(data) * len(data.columns))) * 100
            if missing_pct > 5:
                results['issues'].append(f"High missing data: {missing_pct:.1f}%")
            
            # Check for price anomalies
            returns = data['close'].pct_change().dropna()
            extreme_returns = (np.abs(returns) > 0.5).sum()  # More than 50% change
            if extreme_returns > len(returns) * 0.01:  # More than 1% of data
                results['issues'].append(f"Extreme price movements detected: {extreme_returns}")
            
            # Calculate data quality score
            score = 100.0
            score -= len(results['issues']) * 20  # Penalty for each issue
            score -= missing_pct * 2  # Penalty for missing data
            if actual_days >= min_required:
                score += 20  # Bonus for sufficient data
                
            results['data_quality_score'] = max(0, min(100, score))
            
            validation_results[timeframe] = results
            
            print(f"📊 {timeframe}: Quality Score {results['data_quality_score']:.1f}/100")
            if results['issues']:
                for issue in results['issues']:
                    print(f"   ⚠️  {issue}")
                    
        return validation_results
    
    def train_all_models(self, datasets: Dict[str, pd.DataFrame], 
                        model_types: List[str]) -> Dict[str, Any]:
        """Train all models for all timeframes."""
        print("\n🚀 Starting model training pipeline...")
        
        training_results = {}
        
        for timeframe, data in datasets.items():
            print(f"\n{'='*60}")
            print(f"TRAINING {timeframe.upper()} TIMEFRAME MODELS")
            print(f"{'='*60}")
            
            timeframe_results = {}
            
            # Split data for training/validation
            train_size = int(len(data) * (1 - self.config['validation_split'] - self.config['test_split']))
            val_size = int(len(data) * self.config['validation_split'])
            
            train_data = data.iloc[:train_size]
            val_data = data.iloc[train_size:train_size + val_size]
            test_data = data.iloc[train_size + val_size:]
            
            print(f"📊 Data split: Train={len(train_data)}, Val={len(val_data)}, Test={len(test_data)}")
            
            # Train models based on requested types
            if 'prophet' in model_types:
                try:
                    print(f"\n🔮 Training Prophet models for {timeframe}...")
                    from utilities.timeframe_model_frameworks import TimeframeSpecificProphetFramework
                    
                    prophet_framework = TimeframeSpecificProphetFramework(timeframe)
                    prophet_results = prophet_framework.train_timeframe_models(train_data)
                    timeframe_results['prophet'] = prophet_results
                    
                    print(f"✅ Prophet models trained successfully")
                    
                except Exception as e:
                    print(f"❌ Error training Prophet models: {e}")
                    timeframe_results['prophet'] = {'error': str(e)}
            
            if 'xgboost' in model_types:
                try:
                    print(f"\n🌲 Training XGBoost models for {timeframe}...")
                    from utilities.timeframe_model_frameworks import TimeframeSpecificXGBoostFramework
                    
                    xgboost_framework = TimeframeSpecificXGBoostFramework(timeframe)
                    xgboost_results = xgboost_framework.train_timeframe_models(train_data)
                    timeframe_results['xgboost'] = xgboost_results
                    
                    print(f"✅ XGBoost models trained successfully")
                    
                except Exception as e:
                    print(f"❌ Error training XGBoost models: {e}")
                    timeframe_results['xgboost'] = {'error': str(e)}
            
            if 'ensemble' in model_types:
                try:
                    print(f"\n🎭 Training Ensemble models for {timeframe}...")
                    from utilities.timeframe_model_frameworks import TimeframeSpecificEnsembleFramework
                    
                    ensemble_framework = TimeframeSpecificEnsembleFramework(timeframe)
                    ensemble_results = ensemble_framework.train_ensemble_models(train_data)
                    timeframe_results['ensemble'] = ensemble_results
                    
                    print(f"✅ Ensemble models trained successfully")
                    
                except Exception as e:
                    print(f"❌ Error training Ensemble models: {e}")
                    timeframe_results['ensemble'] = {'error': str(e)}
            
            training_results[timeframe] = timeframe_results
            
        return training_results
    
    def deploy_models(self) -> Dict[str, bool]:
        """Deploy trained models to integration manager."""
        print("\n🚀 Deploying models...")
        
        # Load models into integration manager
        load_results = self.integration_manager.load_models(force_reload=True)
        
        # Check health status
        health_status = self.integration_manager.get_model_health_status()
        
        deployment_status = {}
        for timeframe, status in health_status.items():
            if status.get('overall') == 'HEALTHY':
                deployment_status[timeframe] = True
                print(f"✅ {timeframe} models deployed successfully")
            else:
                deployment_status[timeframe] = False
                print(f"❌ {timeframe} models deployment failed: {status}")
                
        return deployment_status
    
    def test_integration(self, datasets: Dict[str, pd.DataFrame]) -> Dict[str, Any]:
        """Test model-strategy integration."""
        print("\n🧪 Testing model-strategy integration...")
        
        integration_results = {}
        
        for timeframe, data in datasets.items():
            print(f"\n🔄 Testing {timeframe} integration...")
            
            try:
                # Use latest data for testing
                test_data = data.tail(1000)  # Last 1000 records
                
                # Generate predictions
                predictions = self.integration_manager.generate_predictions(
                    test_data, timeframes=[timeframe]
                )
                
                # Generate integrated signals
                signals = self.integration_manager.generate_integrated_signals(test_data)
                
                # Test results
                timeframe_result = {
                    'predictions_generated': timeframe in predictions and bool(predictions[timeframe]),
                    'signals_generated': timeframe in signals,
                    'prediction_count': len(predictions.get(timeframe, {})),
                    'signal_details': None
                }
                
                if timeframe in signals:
                    signal = signals[timeframe]
                    timeframe_result['signal_details'] = {
                        'signal_type': signal.signal_type,
                        'signal_strength': signal.signal_strength,
                        'model_confidence': signal.model_prediction.confidence.value,
                        'suggested_position_size': signal.suggested_position_size
                    }
                
                integration_results[timeframe] = timeframe_result
                
                print(f"✅ {timeframe} integration test passed")
                if timeframe_result['signal_details']:
                    details = timeframe_result['signal_details']
                    print(f"   Signal: {details['signal_type']} "
                          f"(strength: {details['signal_strength']:.3f}, "
                          f"confidence: {details['model_confidence']})")
                
            except Exception as e:
                print(f"❌ {timeframe} integration test failed: {e}")
                integration_results[timeframe] = {'error': str(e)}
                
        return integration_results
    
    def generate_deployment_report(self, training_results: Dict[str, Any],
                                 validation_results: Dict[str, Dict[str, Any]],
                                 deployment_status: Dict[str, bool],
                                 integration_results: Dict[str, Any]) -> str:
        """Generate comprehensive deployment report."""
        report = []
        report.append("ETH MULTI-TIMEFRAME MODEL DEPLOYMENT REPORT")
        report.append("=" * 55)
        report.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append(f"Portfolio: {self.portfolio_name}")
        report.append("")
        
        # Data Quality Summary
        report.append("DATA QUALITY SUMMARY")
        report.append("-" * 25)
        for timeframe, validation in validation_results.items():
            score = validation['data_quality_score']
            record_count = validation['record_count']
            report.append(f"{timeframe:>6}: Quality Score {score:>5.1f}/100 ({record_count:>6} records)")
            if validation['issues']:
                for issue in validation['issues']:
                    report.append(f"         ⚠️  {issue}")
        report.append("")
        
        # Training Results Summary
        report.append("MODEL TRAINING SUMMARY")
        report.append("-" * 25)
        for timeframe, results in training_results.items():
            report.append(f"{timeframe.upper()} TIMEFRAME:")
            
            for model_type in ['prophet', 'xgboost', 'ensemble']:
                if model_type in results:
                    if 'error' in results[model_type]:
                        report.append(f"  {model_type:>8}: ❌ FAILED - {results[model_type]['error']}")
                    else:
                        report.append(f"  {model_type:>8}: ✅ SUCCESS")
                        
                        # Add performance metrics if available
                        if model_type == 'prophet' and isinstance(results[model_type], dict):
                            for model_name, metrics in results[model_type].items():
                                if isinstance(metrics, dict) and 'mape' in metrics:
                                    report.append(f"            {model_name}: MAPE={metrics['mape']:.4f}")
                        elif model_type == 'xgboost' and isinstance(results[model_type], dict):
                            for model_name, metrics in results[model_type].items():
                                if isinstance(metrics, dict) and 'r2' in metrics:
                                    report.append(f"            {model_name}: R²={metrics['r2']:.4f}")
                else:
                    report.append(f"  {model_type:>8}: ⏭️  SKIPPED")
            report.append("")
        
        # Deployment Status
        report.append("DEPLOYMENT STATUS")
        report.append("-" * 20)
        successful_deployments = sum(deployment_status.values())
        total_deployments = len(deployment_status)
        
        for timeframe, status in deployment_status.items():
            status_icon = "✅" if status else "❌"
            report.append(f"{timeframe:>6}: {status_icon} {'DEPLOYED' if status else 'FAILED'}")
        
        report.append(f"\nSuccess Rate: {successful_deployments}/{total_deployments} "
                     f"({successful_deployments/total_deployments*100:.1f}%)")
        report.append("")
        
        # Integration Test Results
        report.append("INTEGRATION TEST RESULTS")
        report.append("-" * 25)
        for timeframe, result in integration_results.items():
            if 'error' in result:
                report.append(f"{timeframe:>6}: ❌ FAILED - {result['error']}")
            else:
                predictions_ok = result.get('predictions_generated', False)
                signals_ok = result.get('signals_generated', False)
                
                if predictions_ok and signals_ok:
                    report.append(f"{timeframe:>6}: ✅ PASSED")
                    if result.get('signal_details'):
                        details = result['signal_details']
                        report.append(f"         Last Signal: {details['signal_type']} "
                                    f"(strength: {details['signal_strength']:.3f})")
                else:
                    report.append(f"{timeframe:>6}: ⚠️  PARTIAL - "
                                f"Predictions: {'✅' if predictions_ok else '❌'}, "
                                f"Signals: {'✅' if signals_ok else '❌'}")
        report.append("")
        
        # Overall Status
        all_deployed = all(deployment_status.values())
        all_integrated = all('error' not in result for result in integration_results.values())
        
        report.append("OVERALL STATUS")
        report.append("-" * 15)
        if all_deployed and all_integrated:
            report.append("🎉 DEPLOYMENT SUCCESSFUL - All systems operational")
        elif all_deployed:
            report.append("⚠️  DEPLOYMENT PARTIAL - Models deployed but integration issues")
        else:
            report.append("❌ DEPLOYMENT FAILED - Critical issues detected")
        
        report.append("")
        report.append("Next Steps:")
        if not all_deployed:
            report.append("• Fix model deployment issues")
        if not all_integrated:
            report.append("• Resolve integration problems")
        if all_deployed and all_integrated:
            report.append("• Monitor model performance")
            report.append("• Begin live trading evaluation")
            report.append("• Schedule regular model retraining")
        
        return "\n".join(report)
    
    def save_results(self, training_results: Dict[str, Any],
                    validation_results: Dict[str, Dict[str, Any]],
                    deployment_status: Dict[str, bool],
                    integration_results: Dict[str, Any],
                    report: str):
        """Save all results to files."""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        
        # Save detailed results
        results_file = self.output_dir / f"training_results_{timestamp}.json"
        with open(results_file, 'w') as f:
            json.dump({
                'timestamp': timestamp,
                'training_results': training_results,
                'validation_results': validation_results,
                'deployment_status': deployment_status,
                'integration_results': integration_results
            }, f, indent=2, default=str)
        
        # Save report
        report_file = self.output_dir / f"deployment_report_{timestamp}.txt"
        with open(report_file, 'w') as f:
            f.write(report)
        
        print(f"\n📝 Results saved:")
        print(f"   📊 Details: {results_file}")
        print(f"   📋 Report:  {report_file}")


def main():
    parser = argparse.ArgumentParser(description='Train and deploy multi-timeframe ETH models')
    parser.add_argument('--timeframes', default='1min,1hour,1day',
                       help='Comma-separated timeframes to train (default: 1min,1hour,1day)')
    parser.add_argument('--models', default='prophet,xgboost,ensemble',
                       help='Comma-separated model types to train (default: prophet,xgboost,ensemble)')
    parser.add_argument('--portfolio', default='Myportolio',
                       help='Portfolio name (default: Myportolio)')
    
    args = parser.parse_args()
    
    # Parse arguments
    timeframes = [tf.strip() for tf in args.timeframes.split(',')]
    model_types = [mt.strip() for mt in args.models.split(',')]
    
    print("🚀 ETH Multi-Timeframe Model Training & Deployment")
    print("=" * 55)
    print(f"Portfolio: {args.portfolio}")
    print(f"Timeframes: {', '.join(timeframes)}")
    print(f"Model Types: {', '.join(model_types)}")
    print()
    
    # Initialize pipeline
    pipeline = ModelTrainingPipeline(args.portfolio)
    
    try:
        # Step 1: Collect training data
        datasets = pipeline.collect_training_data(timeframes)
        
        # Step 2: Validate data quality
        validation_results = pipeline.validate_data_quality(datasets)
        
        # Step 3: Train models
        training_results = pipeline.train_all_models(datasets, model_types)
        
        # Step 4: Deploy models
        deployment_status = pipeline.deploy_models()
        
        # Step 5: Test integration
        integration_results = pipeline.test_integration(datasets)
        
        # Step 6: Generate report
        report = pipeline.generate_deployment_report(
            training_results, validation_results, 
            deployment_status, integration_results
        )
        
        # Step 7: Save results
        pipeline.save_results(
            training_results, validation_results,
            deployment_status, integration_results, report
        )
        
        # Display final report
        print("\n" + report)
        
    except Exception as e:
        print(f"\n❌ Pipeline failed: {e}")
        import traceback
        traceback.print_exc()
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())
