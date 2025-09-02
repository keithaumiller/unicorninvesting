#!/usr/bin/env python3
"""
Bulk Model Generation Script
Unicorn Investing Platform

Generates at least 20 models of each interval and type for production model manager.

Usage:
    python bulk_model_generator.py --target-models 20
    python bulk_model_generator.py --target-models 25 --timeframes 1min,1hour
    python bulk_model_generator.py --target-models 30 --methods prophet,xgboost

Author: Unicorn Investing Platform
Date: September 2, 2025
"""

import argparse
import time
import logging
from datetime import datetime
from production_model_manager import ProductionModelManager

def generate_bulk_models(target_models: int = 20, timeframes: list = None, methods: list = None):
    """
    Generate bulk models for production testing.
    
    Args:
        target_models: Number of models to generate per timeframe/method combination
        timeframes: List of timeframes to generate models for
        methods: List of methods to generate models for
    """
    
    if timeframes is None:
        timeframes = ['1min', '1hour', '1day']
    
    if methods is None:
        methods = ['prophet', 'xgboost']  # Ensemble will be created automatically
    
    print(f"🚀 Starting bulk model generation...")
    print(f"   Target: {target_models} models per timeframe/method")
    print(f"   Timeframes: {timeframes}")
    print(f"   Methods: {methods}")
    print(f"   Total models to generate: {len(timeframes) * len(methods) * target_models}")
    
    # Initialize model manager
    manager = ProductionModelManager()
    
    # Track generation statistics
    stats = {
        'total_generated': 0,
        'total_failed': 0,
        'by_timeframe': {},
        'by_method': {}
    }
    
    start_time = datetime.now()
    
    for timeframe in timeframes:
        stats['by_timeframe'][timeframe] = {'generated': 0, 'failed': 0}
        
        for method in methods:
            if method not in stats['by_method']:
                stats['by_method'][method] = {'generated': 0, 'failed': 0}
            
            print(f"\n📊 Generating {target_models} {method} models for {timeframe}...")
            
            for i in range(target_models):
                try:
                    print(f"   🔄 Training model {i+1}/{target_models} ({method}, {timeframe})", end="")
                    
                    # Train new model
                    model = manager.train_new_model(timeframe, method)
                    
                    if model:
                        stats['total_generated'] += 1
                        stats['by_timeframe'][timeframe]['generated'] += 1
                        stats['by_method'][method]['generated'] += 1
                        print(f" ✅ {model.model_id}")
                    else:
                        stats['total_failed'] += 1
                        stats['by_timeframe'][timeframe]['failed'] += 1
                        stats['by_method'][method]['failed'] += 1
                        print(f" ❌ Failed")
                    
                    # Small delay to prevent overwhelming the system
                    time.sleep(0.5)
                    
                except Exception as e:
                    stats['total_failed'] += 1
                    stats['by_timeframe'][timeframe]['failed'] += 1
                    stats['by_method'][method]['failed'] += 1
                    print(f" ❌ Error: {e}")
    
    # Generate ensemble models by running cycles
    print(f"\n🔗 Generating ensemble models by running production cycles...")
    ensemble_generated = 0
    
    for timeframe in timeframes:
        try:
            print(f"   🔄 Running cycle for {timeframe} to generate ensemble models...")
            results = manager.run_interval_cycle(timeframe)
            
            if 'ensemble_trained' in results:
                ensemble_generated += 1
                print(f"   ✅ Ensemble model generated: {results['ensemble_trained']}")
            else:
                print(f"   ℹ️ Ensemble model creation pending (need production models)")
                
        except Exception as e:
            print(f"   ❌ Cycle failed for {timeframe}: {e}")
    
    # Final statistics
    end_time = datetime.now()
    duration = end_time - start_time
    
    print(f"\n🎉 Bulk model generation complete!")
    print(f"   Duration: {duration}")
    print(f"   Total models generated: {stats['total_generated']}")
    print(f"   Total models failed: {stats['total_failed']}")
    print(f"   Ensemble models generated: {ensemble_generated}")
    print(f"   Success rate: {stats['total_generated']/(stats['total_generated']+stats['total_failed'])*100:.1f}%")
    
    print(f"\n📊 Breakdown by timeframe:")
    for timeframe, counts in stats['by_timeframe'].items():
        total = counts['generated'] + counts['failed']
        success_rate = counts['generated']/total*100 if total > 0 else 0
        print(f"   {timeframe}: {counts['generated']}/{total} ({success_rate:.1f}%)")
    
    print(f"\n📊 Breakdown by method:")
    for method, counts in stats['by_method'].items():
        total = counts['generated'] + counts['failed']
        success_rate = counts['generated']/total*100 if total > 0 else 0
        print(f"   {method}: {counts['generated']}/{total} ({success_rate:.1f}%)")
    
    # Show current model status
    print(f"\n📈 Current model status:")
    try:
        status_results = manager.assess_production_status()
        for timeframe, timeframe_status in status_results.items():
            if timeframe != 'overall_health':
                print(f"   {timeframe.upper()}:")
                for method, method_status in timeframe_status.items():
                    if isinstance(method_status, dict):
                        model_counts = method_status.get('model_counts', {})
                        total_models = sum(model_counts.values())
                        print(f"     {method}: {total_models} models ({dict(model_counts)})")
    except Exception as e:
        print(f"   ❌ Status check failed: {e}")

def main():
    parser = argparse.ArgumentParser(description='Generate bulk models for production testing')
    parser.add_argument('--target-models', type=int, default=20, 
                        help='Number of models to generate per timeframe/method (default: 20)')
    parser.add_argument('--timeframes', type=str, default='1min,1hour,1day',
                        help='Comma-separated list of timeframes (default: 1min,1hour,1day)')
    parser.add_argument('--methods', type=str, default='prophet,xgboost',
                        help='Comma-separated list of methods (default: prophet,xgboost)')
    parser.add_argument('--verbose', action='store_true',
                        help='Enable verbose logging')
    
    args = parser.parse_args()
    
    # Setup logging
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.INFO)
    
    # Parse arguments
    timeframes = [tf.strip() for tf in args.timeframes.split(',')]
    methods = [m.strip() for m in args.methods.split(',')]
    
    # Validate inputs
    valid_timeframes = ['1min', '1hour', '1day']
    valid_methods = ['prophet', 'xgboost', 'ensemble']
    
    invalid_timeframes = [tf for tf in timeframes if tf not in valid_timeframes]
    invalid_methods = [m for m in methods if m not in valid_methods]
    
    if invalid_timeframes:
        print(f"❌ Invalid timeframes: {invalid_timeframes}")
        print(f"   Valid options: {valid_timeframes}")
        return
    
    if invalid_methods:
        print(f"❌ Invalid methods: {invalid_methods}")
        print(f"   Valid options: {valid_methods}")
        return
    
    # Remove ensemble from methods (it's generated automatically)
    methods = [m for m in methods if m != 'ensemble']
    
    # Generate models
    generate_bulk_models(args.target_models, timeframes, methods)

if __name__ == "__main__":
    main()
