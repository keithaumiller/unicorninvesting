#!/usr/bin/env python3
"""
ETH Ensemble Model Validation Framework
=======================================

Comprehensive overfitting detection for ensemble models extending proven 
Prophet/XGBoost validation methodology. Detects component bias compounding,
training data evaluation, and improper ensemble validation.

Key Overfitting Patterns Detected:
1. Training data evaluation (evaluating ensemble on component training data)
2. Component bias compounding (weights from overfitted component R² scores)  
3. No independent validation (ensemble validation on same data as components)
4. Economic indicator data leakage (look-ahead bias in features)

Author: Unicorn Investing Platform
Date: January 2025
Purpose: Eliminate ensemble overfitting for production-ready models
"""

import pandas as pd
import numpy as np
import sqlite3
import logging
import json
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any
import warnings
warnings.filterwarnings('ignore')

class EnsembleModelValidator:
    """
    Comprehensive ensemble model validation framework detecting overfitting patterns
    and providing leak-free validation methodology.
    """
    
    def __init__(self, models_dir: str = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH"):
        """Initialize the ensemble validation framework."""
        self.models_dir = Path(models_dir)
        self.reports_dir = self.models_dir / "validation_reports"
        self.reports_dir.mkdir(exist_ok=True)
        
        # Setup logging
        log_file = self.reports_dir / f"ensemble_validation_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
        
        # Performance thresholds for realistic ensemble models
        self.performance_thresholds = {
            'max_realistic_r2': 0.15,  # Maximum realistic validation R² for ensembles
            'min_improvement_threshold': 0.05,  # Minimum improvement over components
            'max_component_r2': 0.10,  # Maximum realistic component R² for validation
            'overfitting_r2_threshold': 0.20,  # Clear overfitting indicator
            'training_evaluation_threshold': 0.80  # If >80% evaluated on training data
        }
        
        self.logger.info("Ensemble Model Validator initialized")
        self.logger.info(f"Models directory: {self.models_dir}")
        self.logger.info(f"Performance thresholds: {self.performance_thresholds}")

    def find_ensemble_implementations(self) -> List[Dict]:
        """Find all ensemble model implementations in the workspace."""
        ensemble_files = []
        
        # Search patterns for ensemble implementations
        search_patterns = [
            "*ensemble*.py",
            "*multi_method*.py", 
            "*combined*.py",
            "*hybrid*.py"
        ]
        
        for pattern in search_patterns:
            for file_path in self.models_dir.rglob(pattern):
                if file_path.is_file():
                    ensemble_files.append({
                        'file_path': str(file_path),
                        'file_name': file_path.name,
                        'relative_path': str(file_path.relative_to(self.models_dir)),
                        'last_modified': datetime.fromtimestamp(file_path.stat().st_mtime).isoformat()
                    })
        
        self.logger.info(f"Found {len(ensemble_files)} ensemble implementation files")
        return ensemble_files

    def analyze_ensemble_code_patterns(self, file_path: str) -> Dict:
        """Analyze ensemble code for overfitting patterns."""
        overfitting_patterns = {
            'training_data_evaluation': False,
            'component_bias_compounding': False,
            'no_independent_validation': False,
            'economic_data_leakage': False,
            'improper_weight_calculation': False,
            'same_data_validation': False
        }
        
        analysis_details = {
            'patterns_found': [],
            'risk_score': 0,
            'code_samples': {},
            'recommendations': []
        }
        
        try:
            with open(file_path, 'r') as f:
                content = f.read()
                lines = content.split('\n')
            
            # Pattern 1: Training data evaluation detection
            training_eval_patterns = [
                'r2_score(y_true, ensemble_predictions)',
                'r2_score(y, ensemble_pred)',
                'data[\'price\'].values',
                'mape = np.mean(np.abs((y_true - ensemble_predictions)',
                'y_true = data[\'price\']',
                'prophet_data[\'y\'].values'
            ]
            
            for i, line in enumerate(lines):
                for pattern in training_eval_patterns:
                    if pattern in line:
                        overfitting_patterns['training_data_evaluation'] = True
                        analysis_details['patterns_found'].append(f"Training data evaluation at line {i+1}: {line.strip()}")
                        analysis_details['code_samples']['training_evaluation'] = f"Line {i+1}: {line.strip()}"
                        break
            
            # Pattern 2: Component bias compounding
            bias_patterns = [
                'prophet_r2 = prophet_metrics.get(\'r2_score\'',
                'xgb_r2 = xgb_metrics.get(\'r2_score\'',
                'prophet_weight = prophet_r2',
                'weight.*r2_score',
                'total_r2 = prophet_r2 + xgb_r2'
            ]
            
            for i, line in enumerate(lines):
                for pattern in bias_patterns:
                    if pattern in line:
                        overfitting_patterns['component_bias_compounding'] = True
                        analysis_details['patterns_found'].append(f"Component bias compounding at line {i+1}: {line.strip()}")
                        analysis_details['code_samples']['bias_compounding'] = f"Line {i+1}: {line.strip()}"
                        break
            
            # Pattern 3: No independent validation
            validation_patterns = [
                'train_test_split',
                'validation_split',
                'holdout',
                'cross_validation'
            ]
            
            has_proper_validation = any(pattern in content for pattern in validation_patterns)
            if not has_proper_validation:
                overfitting_patterns['no_independent_validation'] = True
                analysis_details['patterns_found'].append("No independent validation methodology detected")
            
            # Pattern 4: Economic data leakage
            economic_patterns = [
                'feature_engineering',
                'economic_indicators',
                'future_looking',
                'shift(-1)',
                'look_ahead'
            ]
            
            for i, line in enumerate(lines):
                for pattern in economic_patterns:
                    if pattern in line and ('shift(-1)' in line or 'future' in line.lower()):
                        overfitting_patterns['economic_data_leakage'] = True
                        analysis_details['patterns_found'].append(f"Potential economic data leakage at line {i+1}: {line.strip()}")
                        analysis_details['code_samples']['data_leakage'] = f"Line {i+1}: {line.strip()}"
                        break
            
            # Pattern 5: Improper weight calculation
            weight_patterns = [
                'if total_r2 > 0:',
                'prophet_weight = prophet_r2 / total_r2',
                'else:.*prophet_weight = 0.5'
            ]
            
            for pattern in weight_patterns:
                if pattern in content:
                    overfitting_patterns['improper_weight_calculation'] = True
                    analysis_details['patterns_found'].append(f"Improper weight calculation using overfitted R² scores")
                    break
            
            # Pattern 6: Same data validation
            same_data_patterns = [
                'model.fit(prophet_data)',
                'train_predictions = model.predict(prophet_data)',
                'y_true = prophet_data[\'y\'].values',
                'y_pred = train_predictions[\'yhat\'].values'
            ]
            
            same_data_sequence = 0
            for line in lines:
                for pattern in same_data_patterns:
                    if pattern in line:
                        same_data_sequence += 1
                        break
            
            if same_data_sequence >= 3:
                overfitting_patterns['same_data_validation'] = True
                analysis_details['patterns_found'].append("Same data used for training and validation")
            
            # Calculate risk score
            risk_score = sum(overfitting_patterns.values()) * 20  # 20 points per pattern
            analysis_details['risk_score'] = risk_score
            
            # Generate recommendations
            recommendations = []
            if overfitting_patterns['training_data_evaluation']:
                recommendations.append("Implement proper train/validation split before ensemble evaluation")
            if overfitting_patterns['component_bias_compounding']:
                recommendations.append("Use cross-validation R² scores for component weight calculation")
            if overfitting_patterns['no_independent_validation']:
                recommendations.append("Add independent holdout validation for ensemble performance assessment")
            if overfitting_patterns['economic_data_leakage']:
                recommendations.append("Ensure economic features don't contain look-ahead bias")
            if overfitting_patterns['improper_weight_calculation']:
                recommendations.append("Replace overfitted R² weights with proper validation-based weights")
            if overfitting_patterns['same_data_validation']:
                recommendations.append("Separate training and validation datasets completely")
            
            analysis_details['recommendations'] = recommendations
            
        except Exception as e:
            self.logger.error(f"Error analyzing {file_path}: {e}")
            analysis_details['error'] = str(e)
        
        return {
            'file_path': file_path,
            'overfitting_patterns': overfitting_patterns,
            'analysis_details': analysis_details
        }

    def validate_ensemble_performance(self, model_metrics: Dict) -> Dict:
        """Validate ensemble performance against realistic benchmarks."""
        validation_results = {
            'is_overfitted': False,
            'performance_flags': [],
            'realistic_score': 0,
            'recommendations': []
        }
        
        # Extract metrics
        r2_score = model_metrics.get('r2_score', 0)
        component_r2s = model_metrics.get('component_r2s', [])
        mape = model_metrics.get('mape', 100)
        
        # Flag 1: Unrealistic R² score
        if r2_score > self.performance_thresholds['overfitting_r2_threshold']:
            validation_results['is_overfitted'] = True
            validation_results['performance_flags'].append(
                f"Unrealistic R² score: {r2_score:.4f} > {self.performance_thresholds['overfitting_r2_threshold']}"
            )
        
        # Flag 2: Ensemble worse than realistic threshold
        if r2_score > self.performance_thresholds['max_realistic_r2']:
            validation_results['performance_flags'].append(
                f"R² exceeds realistic ensemble threshold: {r2_score:.4f} > {self.performance_thresholds['max_realistic_r2']}"
            )
        
        # Flag 3: Component models overfitted
        overfitted_components = [r2 for r2 in component_r2s if r2 > self.performance_thresholds['max_component_r2']]
        if overfitted_components:
            validation_results['is_overfitted'] = True
            validation_results['performance_flags'].append(
                f"Overfitted components detected: {len(overfitted_components)} components with R² > {self.performance_thresholds['max_component_r2']}"
            )
        
        # Flag 4: Suspiciously low MAPE
        if mape < 1.0:  # Less than 1% MAPE is suspicious for financial data
            validation_results['performance_flags'].append(
                f"Suspiciously low MAPE: {mape:.2f}% (typical range: 5-15%)"
            )
        
        # Calculate realistic score (0-100)
        realistic_score = 100
        
        if r2_score > self.performance_thresholds['max_realistic_r2']:
            realistic_score -= 50
        if r2_score > self.performance_thresholds['overfitting_r2_threshold']:
            realistic_score -= 30
        if overfitted_components:
            realistic_score -= 20
        if mape < 1.0:
            realistic_score -= 10
        
        validation_results['realistic_score'] = max(0, realistic_score)
        
        # Generate recommendations
        if validation_results['is_overfitted']:
            validation_results['recommendations'].extend([
                "Rebuild ensemble using leak-free component models",
                "Implement proper train/validation split methodology",
                "Use realistic performance expectations (R² < 0.15)",
                "Validate components independently before ensemble integration"
            ])
        
        return validation_results

    def generate_leak_free_validation_methodology(self) -> Dict:
        """Generate comprehensive leak-free validation methodology for ensembles."""
        methodology = {
            'data_splitting': {
                'description': 'Proper temporal splitting for financial time series',
                'implementation': {
                    'training_split': '70% of historical data',
                    'validation_split': '20% of historical data (for component validation)',
                    'test_split': '10% of historical data (for ensemble validation)',
                    'temporal_order': 'Strictly maintain chronological order',
                    'no_overlap': 'Ensure no data leakage between splits'
                }
            },
            'component_validation': {
                'description': 'Validate components before ensemble integration',
                'requirements': {
                    'independent_validation': 'Each component validated on held-out data',
                    'realistic_performance': 'Component R² must be < 0.10',
                    'stability_check': 'Performance stable across multiple time periods',
                    'overfitting_screening': 'Components must pass overfitting detection'
                }
            },
            'ensemble_methodology': {
                'description': 'Leak-free ensemble construction and validation',
                'steps': [
                    'Train components on training data only',
                    'Validate components on validation data',
                    'Calculate ensemble weights using validation performance',
                    'Combine components using validation-based weights',
                    'Evaluate final ensemble on independent test data'
                ]
            },
            'weight_calculation': {
                'description': 'Proper ensemble weight calculation',
                'method': 'Cross-validation based weights',
                'formula': 'weights = softmax(1 / validation_errors)',
                'fallback': 'Equal weights if no component outperforms',
                'constraints': 'Sum to 1.0, all weights >= 0'
            },
            'performance_expectations': {
                'realistic_r2_range': '0.02 to 0.15',
                'typical_improvement': '5-15% error reduction vs best component',
                'mape_range': '5% to 15% for crypto markets',
                'overfitting_threshold': 'R² > 0.20 indicates overfitting'
            }
        }
        
        return methodology

    def run_comprehensive_validation(self) -> Dict:
        """Run comprehensive ensemble model validation."""
        self.logger.info("Starting comprehensive ensemble model validation")
        
        validation_report = {
            'timestamp': datetime.now().isoformat(),
            'summary': {},
            'ensemble_files': [],
            'overfitting_analysis': {},
            'recommendations': [],
            'methodology': {}
        }
        
        # Find ensemble implementations
        ensemble_files = self.find_ensemble_implementations()
        validation_report['ensemble_files'] = ensemble_files
        
        # Analyze each ensemble file
        total_files = len(ensemble_files)
        overfitted_files = 0
        high_risk_files = 0
        
        for file_info in ensemble_files:
            file_path = file_info['file_path']
            self.logger.info(f"Analyzing ensemble file: {file_info['file_name']}")
            
            analysis = self.analyze_ensemble_code_patterns(file_path)
            
            # Count overfitting patterns
            pattern_count = sum(analysis['overfitting_patterns'].values())
            risk_score = analysis['analysis_details']['risk_score']
            
            if pattern_count >= 3:  # 3+ patterns = overfitted
                overfitted_files += 1
            if risk_score >= 60:  # High risk threshold
                high_risk_files += 1
            
            validation_report['overfitting_analysis'][file_info['file_name']] = analysis
        
        # Generate summary
        overfitting_rate = (overfitted_files / total_files * 100) if total_files > 0 else 0
        validation_report['summary'] = {
            'total_ensemble_files': total_files,
            'overfitted_files': overfitted_files,
            'high_risk_files': high_risk_files,
            'overfitting_rate': round(overfitting_rate, 1),
            'validation_status': 'CRITICAL' if overfitting_rate > 50 else 'WARNING' if overfitting_rate > 20 else 'GOOD'
        }
        
        # Generate methodology
        validation_report['methodology'] = self.generate_leak_free_validation_methodology()
        
        # Generate recommendations
        if overfitting_rate > 50:
            validation_report['recommendations'].extend([
                "CRITICAL: Majority of ensemble models show overfitting patterns",
                "Implement comprehensive ensemble rebuilding campaign",
                "Apply leak-free validation methodology to all ensemble models",
                "Validate all component models before ensemble integration"
            ])
        elif overfitting_rate > 20:
            validation_report['recommendations'].extend([
                "WARNING: Significant overfitting detected in ensemble models",
                "Review and rebuild high-risk ensemble implementations",
                "Implement proper validation methodology"
            ])
        else:
            validation_report['recommendations'].append(
                "GOOD: Low overfitting risk, consider periodic validation review"
            )
        
        # Save validation report
        report_file = self.reports_dir / f"ensemble_validation_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(report_file, 'w') as f:
            json.dump(validation_report, f, indent=2)
        
        self.logger.info(f"Validation report saved to: {report_file}")
        self.logger.info(f"Validation complete: {overfitted_files}/{total_files} files overfitted ({overfitting_rate:.1f}%)")
        
        return validation_report

def main():
    """Main function for CLI usage."""
    validator = EnsembleModelValidator()
    
    print("🔍 ETH Ensemble Model Validation Framework")
    print("=" * 50)
    
    # Run comprehensive validation
    report = validator.run_comprehensive_validation()
    
    # Display results
    summary = report['summary']
    print(f"\n📊 Validation Summary:")
    print(f"Total ensemble files: {summary['total_ensemble_files']}")
    print(f"Overfitted files: {summary['overfitted_files']}")
    print(f"High risk files: {summary['high_risk_files']}")
    print(f"Overfitting rate: {summary['overfitting_rate']}%")
    print(f"Status: {summary['validation_status']}")
    
    print(f"\n📋 Key Recommendations:")
    for rec in report['recommendations'][:3]:  # Top 3 recommendations
        print(f"  • {rec}")
    
    print(f"\n🎯 Next Steps:")
    if summary['overfitting_rate'] > 50:
        print("  1. Review detailed analysis in validation report")
        print("  2. Implement enhanced ensemble builder with leak-free validation")
        print("  3. Execute comprehensive ensemble rebuilding campaign")
    elif summary['overfitting_rate'] > 20:
        print("  1. Focus on high-risk ensemble files")
        print("  2. Apply leak-free validation methodology")
        print("  3. Validate component models independently")
    else:
        print("  1. Continue with current validation practices")
        print("  2. Periodic validation review recommended")

if __name__ == "__main__":
    main()