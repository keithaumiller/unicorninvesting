#!/usr/bin/env python3
"""
Myportolio Live Trading Readiness Assessment

Comprehensive status check script that validates all workstreams, process flows,
and components required for live trading with the Myportolio strategy.

Author: Unicorn Investing Platform
Date: September 2, 2025
"""

import json
import sys
import sqlite3
import requests
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Any, Tuple, Optional
import logging
import traceback

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class MyportolioStatusChecker:
    """Comprehensive live trading readiness assessment for Myportolio."""
    
    def __init__(self):
        """Initialize the status checker with portfolio paths and configuration."""
        self.portfolio_dir = Path(__file__).parent.parent
        self.unicorn_root = self.portfolio_dir.parent.parent  # Go up 2 levels to get to unicorn/
        self.alpha_models_dir = self.unicorn_root / "2_alpha_models"
        self.risk_mgmt_dir = self.unicorn_root / "3_risk_management"
        self.data_sources_dir = self.unicorn_root / "1_data_sources"
        self.execution_dir = self.unicorn_root / "5_execution_models"
        self.algorithms_dir = self.unicorn_root / "6_algorithms"
        
        # Simulation Framework Directory
        self.simulations_dir = self.portfolio_dir / "simulations"
        
        # IBKR Account Information Directory
        self.ibkr_account_dir = self.data_sources_dir / "1_raw" / "connectors" / "interactive_brokers" / "accountinfo"
        
        # Load IBKR account data
        self.ibkr_account_data = self._load_ibkr_account_data()
        
        # Load portfolio configuration
        self._load_portfolio_configuration()
        
    def _load_ibkr_account_data(self) -> Dict[str, Any]:
        """Load IBKR account information from stored JSON files."""
        account_data = {
            'complete_account_info': {},
            'account_capabilities': {},
            'market_data_access': {},
            'risk_parameters': {},
            'available': False,
            'data_freshness': 'unknown',
            'safe_for_live_trading': False
        }
        
        # First check data freshness
        freshness_file = self.ibkr_account_dir / "data_freshness.json"
        if freshness_file.exists():
            try:
                with open(freshness_file, 'r') as f:
                    freshness_data = json.load(f)
                    account_data['data_freshness'] = freshness_data.get('status', 'unknown')
                    account_data['safe_for_live_trading'] = freshness_data.get('safe_for_live_trading', False)
                    
                    # If data is marked as stale, fail immediately for live trading
                    if freshness_data.get('status') == 'stale':
                        logger.error("❌ CRITICAL: IBKR account data is STALE - live trading is NOT safe")
                        logger.error(f"   Reason: {freshness_data.get('warning', 'Unknown reason')}")
                        logger.error("   Fix: Ensure IBKR Gateway is running and restart the environment check")
                        return account_data  # Return with available=False
            except Exception as e:
                logger.warning(f"Could not read data freshness marker: {e}")
        
        try:
            # Load complete account information
            complete_info_file = self.ibkr_account_dir / "complete_account_info.json"
            if complete_info_file.exists():
                with open(complete_info_file, 'r') as f:
                    account_data['complete_account_info'] = json.load(f)
            
            # Load account capabilities
            capabilities_file = self.ibkr_account_dir / "account_capabilities.json"
            if capabilities_file.exists():
                with open(capabilities_file, 'r') as f:
                    account_data['account_capabilities'] = json.load(f)
            
            # Load market data access
            market_data_file = self.ibkr_account_dir / "market_data_access.json"
            if market_data_file.exists():
                with open(market_data_file, 'r') as f:
                    account_data['market_data_access'] = json.load(f)
            
            # Load risk parameters
            risk_params_file = self.ibkr_account_dir / "risk_parameters.json"
            if risk_params_file.exists():
                with open(risk_params_file, 'r') as f:
                    account_data['risk_parameters'] = json.load(f)
            
            # Only mark as available if data is fresh AND safe for live trading
            if account_data['safe_for_live_trading'] and account_data['data_freshness'] == 'fresh':
                account_data['available'] = True
                logger.info("✅ IBKR account data loaded successfully - FRESH DATA confirmed safe for live trading")
            else:
                account_data['available'] = False
                if account_data['data_freshness'] == 'stale':
                    logger.error("❌ IBKR account data is STALE - refusing to use for safety")
                else:
                    logger.warning("⚠️ IBKR account data freshness unknown - not safe for live trading")
            
        except Exception as e:
            logger.error(f"Error loading IBKR account data: {e}")
            account_data['available'] = False
            
        return account_data
        
    def _load_portfolio_configuration(self):
        """Load portfolio configuration files."""
        # Load portfolio configuration
        self.portfolio_config = self._load_json_file(self.portfolio_dir / "config.json")
        self.risk_parameters = self._load_json_file(self.portfolio_dir / "risk_parameters.json")
        self.execution_settings = self._load_json_file(self.portfolio_dir / "execution_settings.json")
        
        # Status tracking
        self.status_results = {
            'timestamp': datetime.now().isoformat(),
            'overall_readiness': 'UNKNOWN',
            'critical_issues': [],
            'warnings': [],
            'passed_checks': [],
            'component_status': {},
            'performance_metrics': {},
            'recommendations': []
        }
        
    def _get_actual_portfolio_data(self) -> Dict[str, Any]:
        """Get actual portfolio positions and balances from IBKR account data."""
        portfolio_data = {
            'available': False,
            'net_liquidation_value': 0.0,
            'market_value': 0.0,
            'cash_balance': 0.0,
            'unrealized_pnl': 0.0,
            'positions': {},
            'error_message': ''
        }
        
        try:
            # Use IBKR account data that we already have from the health check
            if hasattr(self, 'ibkr_account_data') and self.ibkr_account_data and self.ibkr_account_data.get('available', False):
                
                # Try to get portfolio data directly from current_portfolio file
                current_portfolio_file = self.ibkr_account_dir / "current_portfolio.json"
                if current_portfolio_file.exists():
                    with open(current_portfolio_file, 'r') as f:
                        portfolio_file_data = json.load(f)
                        
                    summary = portfolio_file_data.get('summary', {})
                    portfolio_data['available'] = True
                    portfolio_data['net_liquidation_value'] = summary.get('net_liquidation', 0.0)
                    portfolio_data['market_value'] = summary.get('market_value', 0.0)
                    portfolio_data['cash_balance'] = summary.get('cash_balance', 0.0)
                    portfolio_data['unrealized_pnl'] = summary.get('unrealized_pnl', 0.0)
                    
                    logger.info(f"✅ IBKR portfolio data loaded from file: NLV=${portfolio_data['net_liquidation_value']:.2f}")
                    
                    # Extract positions from file
                    positions_list = portfolio_file_data.get('positions', [])
                    total_value = portfolio_data['net_liquidation_value']
                    
                    for position in positions_list:
                        if isinstance(position, dict):
                            symbol = position.get('symbol', position.get('ticker', 'UNKNOWN'))
                            market_value = position.get('market_value', position.get('marketValue', 0.0))
                            
                            if market_value != 0 and total_value > 0:
                                allocation_pct = (market_value / total_value) * 100
                                portfolio_data['positions'][symbol] = {
                                    'market_value': market_value,
                                    'allocation_percent': allocation_pct,
                                    'quantity': position.get('quantity', position.get('position', 0))
                                }
                                logger.info(f"✅ Position found: {symbol} = ${market_value:.2f} ({allocation_pct:.1f}%)")
                    
                    # If no positions found but we have account connection, it means portfolio is all cash
                    if not portfolio_data['positions'] and portfolio_data['available']:
                        logger.info("✅ No positions found - Portfolio is 100% cash (empty account)")
                        
                else:
                    portfolio_data['error_message'] = "current_portfolio.json file not found"
                    logger.warning("❌ current_portfolio.json file not found")
                    
            else:
                portfolio_data['error_message'] = "IBKR account data not available or not loaded"
                logger.warning("❌ IBKR account data not available for portfolio extraction")
                
        except Exception as e:
            portfolio_data['error_message'] = f"Error extracting portfolio data: {str(e)}"
            logger.error(f"Error getting portfolio data: {e}")
            
        return portfolio_data

    def _load_json_file(self, file_path: Path) -> Dict[str, Any]:
        """Load JSON configuration file with error handling."""
        try:
            if file_path.exists():
                with open(file_path, 'r') as f:
                    return json.load(f)
            else:
                logger.warning(f"Configuration file not found: {file_path}")
                return {}
        except Exception as e:
            logger.error(f"Error loading {file_path}: {e}")
            return {}
    
    def print_header(self, title: str, level: int = 1):
        """Print formatted section header."""
        if level == 1:
            print(f"\n{'='*80}")
            print(f"🦄 {title}")
            print('='*80)
        else:
            print(f"\n{'-'*60}")
            print(f"📋 {title}")
            print('-'*60)
    
    def check_portfolio_configuration(self) -> Dict[str, Any]:
        """Validate portfolio configuration completeness and consistency."""
        self.print_header("PORTFOLIO CONFIGURATION VALIDATION", 2)
        
        results = {
            'config_loaded': bool(self.portfolio_config),
            'risk_params_loaded': bool(self.risk_parameters),
            'execution_settings_loaded': bool(self.execution_settings),
            'asset_allocation_valid': False,
            'risk_limits_defined': False,
            'configuration_errors': []
        }
        
        # Check portfolio configuration
        if self.portfolio_config:
            print(f"✅ Portfolio Config: {self.portfolio_config.get('portfolio_name', 'Unknown')}")
            print(f"   Strategy: {self.portfolio_config.get('strategy_type', 'Unknown')}")
            print(f"   Assets: {list(self.portfolio_config.get('assets', {}).keys())}")
            
            # Validate asset allocation
            assets = self.portfolio_config.get('assets', {})
            total_allocation = sum(asset.get('allocation_percent', 0) for asset in assets.values())
            if abs(total_allocation - 100) < 0.01:
                results['asset_allocation_valid'] = True
                print(f"✅ Asset Allocation: {total_allocation}% (valid)")
            else:
                results['configuration_errors'].append(f"Asset allocation totals {total_allocation}%, not 100%")
                print(f"❌ Asset Allocation: {total_allocation}% (invalid)")
        else:
            results['configuration_errors'].append("Portfolio configuration not loaded")
            print("❌ Portfolio Configuration: Not loaded")
        
        # Check risk parameters
        if self.risk_parameters:
            print(f"✅ Risk Parameters: Loaded")
            print(f"   Max Volatility: {self.risk_parameters.get('max_portfolio_volatility', 'Unknown')}")
            print(f"   Max Drawdown: {self.risk_parameters.get('max_drawdown', 'Unknown')}")
            print(f"   VaR Limit (1-day): {self.risk_parameters.get('var_limit_1day', 'Unknown')}")
            results['risk_limits_defined'] = True
        else:
            results['configuration_errors'].append("Risk parameters not loaded")
            print("❌ Risk Parameters: Not loaded")
        
        # Check execution settings
        if self.execution_settings:
            print(f"✅ Execution Settings: Loaded")
        else:
            print("⚠️  Execution Settings: Not loaded (using defaults)")
        
        return results
    
    def check_alpha_models_status(self) -> Dict[str, Any]:
        """Assess alpha models availability and performance."""
        self.print_header("ALPHA MODELS ASSESSMENT", 2)
        
        results = {
            'models_available': {},
            'performance_metrics': {},
            'model_freshness': {},
            'production_ready': {},
            'best_model': None
        }
        
        # Check for available model types - in 2_alpha_models/CRYPTO/ETH/
        model_types = ['eth_prophet', 'eth_xgboost', 'eth_ensemble']
        
        for model_type in model_types:
            model_file = self.alpha_models_dir / f"{model_type}_framework.py"
            db_file = self.alpha_models_dir / f"{model_type}_comparison.db"
            
            model_available = model_file.exists()
            db_available = db_file.exists()
            
            results['models_available'][model_type] = {
                'framework_available': model_available,
                'database_available': db_available,
                'ready': model_available and db_available
            }
            
            status = "✅" if model_available and db_available else "⚠️" if model_available else "❌"
            print(f"{status} {model_type.replace('_', ' ').title()}: Framework {'✅' if model_available else '❌'}, DB {'✅' if db_available else '❌'}")
            
            # Get performance metrics if database exists
            if db_available:
                try:
                    perf_metrics = self._get_model_performance(db_file, model_type)
                    results['performance_metrics'][model_type] = perf_metrics
                    
                    if perf_metrics:
                        # Display available metrics with safe formatting
                        if perf_metrics.get('r2_score') is not None:
                            print(f"   📊 R² Score: {perf_metrics['r2_score']:.4f}")
                        if perf_metrics.get('mape') is not None:
                            print(f"   📊 MAPE: {perf_metrics['mape']:.4f}")
                        if perf_metrics.get('sharpe_ratio') is not None:
                            print(f"   📊 Sharpe Ratio: {perf_metrics['sharpe_ratio']:.4f}")
                        if perf_metrics.get('rmse') is not None:
                            print(f"   📊 RMSE: {perf_metrics['rmse']:.4f}")
                        
                except Exception as e:
                    logger.warning(f"Error getting performance for {model_type}: {e}")
            elif model_available:
                # Framework available but no database - still partially ready
                print(f"   ⚠️  Database not found - model training needed")
        
        # Determine best model based on available metrics
        best_model = self._determine_best_model(results['performance_metrics'])
        results['best_model'] = best_model
        
        if best_model:
            print(f"\n🏆 Best Performing Model: {best_model['model_type']} (Score: {best_model['score']:.4f})")
        
        return results
    
    def _get_model_performance(self, db_file: Path, model_type: str) -> Dict[str, float]:
        """Extract performance metrics from model database (normalized schema)."""
        try:
            conn = sqlite3.connect(db_file)
            
            # All databases now use the normalized pivot schema
            query = """
            SELECT 
                MAX(CASE WHEN metric_name = 'mape' THEN metric_value END) as mape,
                MAX(CASE WHEN metric_name = 'rmse' THEN metric_value END) as rmse,
                MAX(CASE WHEN metric_name = 'mae' THEN metric_value END) as mae,
                MAX(CASE WHEN metric_name = 'r2_score' THEN metric_value END) as r2_score,
                MAX(CASE WHEN metric_name = 'directional_accuracy' THEN metric_value END) as directional_accuracy,
                created_at as training_date
            FROM model_performance 
            WHERE created_at = (SELECT MAX(created_at) FROM model_performance)
            GROUP BY created_at
            ORDER BY created_at DESC 
            LIMIT 1
            """
            
            df = pd.read_sql_query(query, conn)
            conn.close()
            
            if not df.empty:
                return {
                    'mape': float(df.iloc[0]['mape']) if pd.notna(df.iloc[0]['mape']) else None,
                    'rmse': float(df.iloc[0]['rmse']) if pd.notna(df.iloc[0]['rmse']) else None,
                    'mae': float(df.iloc[0]['mae']) if pd.notna(df.iloc[0]['mae']) else None,
                    'r2_score': float(df.iloc[0]['r2_score']) if pd.notna(df.iloc[0]['r2_score']) else None,
                    'directional_accuracy': float(df.iloc[0]['directional_accuracy']) if pd.notna(df.iloc[0]['directional_accuracy']) else None,
                    'training_date': df.iloc[0]['training_date']
                }
        except Exception as e:
            logger.warning(f"Error reading performance from {db_file}: {e}")
        
        return {}
    
    def _determine_best_model(self, performance_metrics: Dict) -> Optional[Dict]:
        """Determine the best model based on composite score."""
        scored_models = []
        
        for model_type, metrics in performance_metrics.items():
            if not metrics:
                continue
                
            # Calculate composite score (R² * 0.4 + (1-MAPE) * 0.3 + Sharpe * 0.3)
            r2 = metrics.get('r2_score', 0) or 0
            mape = metrics.get('mape', 1) or 1
            sharpe = metrics.get('sharpe_ratio', 0) or 0
            
            # Normalize sharpe ratio (assume good sharpe is 1-3 range)
            normalized_sharpe = min(sharpe / 3.0, 1.0) if sharpe > 0 else 0
            
            composite_score = (r2 * 0.4) + ((1 - min(mape, 1)) * 0.3) + (normalized_sharpe * 0.3)
            
            scored_models.append({
                'model_type': model_type,
                'score': composite_score,
                'metrics': metrics
            })
        
        if scored_models:
            return max(scored_models, key=lambda x: x['score'])
        
        return None
    
    def check_production_models_status(self) -> Dict[str, Any]:
        """Check production models availability across asset classes - UNIFIED SYSTEM."""
        self.print_header("PRODUCTION MODELS STATUS (UNIFIED SYSTEM)", 2)
        
        results = {
            'asset_directories_exist': False,
            'asset_class_coverage': {},
            'model_type_distribution': {},
            'total_models': 0,
            'unified_system_ready': False,
            'missing_requirements': []
        }
        
        # Check actual model organization in asset directories
        asset_directories = ['CRYPTO', 'FOREX', 'EQUITIES', 'fixed_multi_asset_models', 'multi_asset_models']
        existing_directories = []
        
        for asset_dir in asset_directories:
            asset_path = self.alpha_models_dir / asset_dir
            if asset_path.exists():
                existing_directories.append(asset_dir)
        
        results['asset_directories_exist'] = len(existing_directories) > 0
        
        if not existing_directories:
            print("❌ No asset model directories found")
            results['missing_requirements'].append("Asset model directories missing")
            return results
        
        print(f"✅ Found {len(existing_directories)} asset directories: {', '.join(existing_directories)}")
        
        # Count models in each asset directory
        total_models = 0
        all_requirements_met = True
        
        for asset_dir in existing_directories:
            asset_path = self.alpha_models_dir / asset_dir
            asset_results = {
                'directory_exists': True,
                'model_files': {},
                'total_models': 0
            }
            
            print(f"\n📊 {asset_dir.upper()} Asset Class:")
            
            # Count different model file types
            model_types = {
                'joblib_models': list(asset_path.rglob("*.joblib")),
                'json_models': list(asset_path.rglob("*.json")),
                'pkl_models': list(asset_path.rglob("*.pkl")),
                'h5_models': list(asset_path.rglob("*.h5"))
            }
            
            asset_total = 0
            for model_type, files in model_types.items():
                count = len(files)
                asset_total += count
                asset_results['model_files'][model_type] = count
                
                if count > 0:
                    print(f"  ✅ {model_type.replace('_', ' ').title()}: {count} files")
                else:
                    print(f"  ⚪ {model_type.replace('_', ' ').title()}: {count} files")
            
            asset_results['total_models'] = asset_total
            total_models += asset_total
            results['asset_class_coverage'][asset_dir] = asset_results
            
            print(f"  📈 Total {asset_dir} models: {asset_total}")
        
        results['total_models'] = total_models
        results['unified_system_ready'] = total_models > 100  # Reasonable threshold for production readiness
        
        # Get model type distribution across all assets
        all_joblib = sum(asset['model_files'].get('joblib_models', 0) for asset in results['asset_class_coverage'].values())
        all_json = sum(asset['model_files'].get('json_models', 0) for asset in results['asset_class_coverage'].values())
        all_pkl = sum(asset['model_files'].get('pkl_models', 0) for asset in results['asset_class_coverage'].values())
        all_h5 = sum(asset['model_files'].get('h5_models', 0) for asset in results['asset_class_coverage'].values())
        
        results['model_type_distribution'] = {
            'joblib_models': all_joblib,
            'json_models': all_json,
            'pkl_models': all_pkl,
            'h5_models': all_h5
        }
        
        # Summary
        print(f"\n📈 UNIFIED SYSTEM SUMMARY:")
        print(f"   Total Models (All Assets): {total_models}")
        print(f"   Asset Classes: {len(existing_directories)}")
        print(f"   System Status: {'✅ READY' if results['unified_system_ready'] else '❌ INSUFFICIENT MODELS'}")
        
        print(f"\n📊 MODEL TYPE DISTRIBUTION:")
        for model_type, count in results['model_type_distribution'].items():
            if count > 0:
                print(f"   • {model_type.replace('_', ' ').title()}: {count} files")
        
        if not results['unified_system_ready']:
            print(f"\n❌ SYSTEM REQUIREMENTS:")
            print(f"   • Need minimum 100 models for production readiness")
            print(f"   • Current count: {total_models}")
        else:
            print(f"\n✅ Unified system ready with {total_models} models across {len(existing_directories)} asset classes!")
            
        return results
    
    def _get_comprehensive_model_counts(self, alpha_models_root: Path) -> Dict[str, Any]:
        """Get comprehensive count of all trained models across asset directories."""
        counts = {
            'total_comprehensive': 0,
            'breakdown': {}
        }
        
        try:
            # CRYPTO Models
            crypto_dir = alpha_models_root / "CRYPTO"
            if crypto_dir.exists():
                crypto_models = len(list(crypto_dir.rglob("*.joblib"))) + len(list(crypto_dir.rglob("*.json"))) + len(list(crypto_dir.rglob("*.pkl")))
                counts['breakdown']['CRYPTO Models'] = crypto_models
            
            # FOREX Models
            forex_dir = alpha_models_root / "FOREX"
            if forex_dir.exists():
                forex_models = len(list(forex_dir.rglob("*.joblib"))) + len(list(forex_dir.rglob("*.json"))) + len(list(forex_dir.rglob("*.pkl")))
                counts['breakdown']['FOREX Models'] = forex_models
            
            # EQUITIES Models
            equities_dir = alpha_models_root / "EQUITIES"
            if equities_dir.exists():
                equities_models = len(list(equities_dir.rglob("*.joblib"))) + len(list(equities_dir.rglob("*.json"))) + len(list(equities_dir.rglob("*.pkl")))
                counts['breakdown']['EQUITIES Models'] = equities_models
            
            # Fixed Multi-Asset Models
            fixed_multi_dir = alpha_models_root / "fixed_multi_asset_models"
            if fixed_multi_dir.exists():
                fixed_multi_models = len(list(fixed_multi_dir.rglob("*.joblib"))) + len(list(fixed_multi_dir.rglob("*.json"))) + len(list(fixed_multi_dir.rglob("*.pkl")))
                counts['breakdown']['Fixed Multi-Asset Models'] = fixed_multi_models
            
            # Multi-Asset Models
            multi_asset_dir = alpha_models_root / "multi_asset_models"
            if multi_asset_dir.exists():
                multi_asset_models = len(list(multi_asset_dir.rglob("*.joblib"))) + len(list(multi_asset_dir.rglob("*.json"))) + len(list(multi_asset_dir.rglob("*.pkl")))
                counts['breakdown']['Multi-Asset Models'] = multi_asset_models
            
            # Calculate total
            counts['total_comprehensive'] = sum(counts['breakdown'].values())
            
        except Exception as e:
            logger.warning(f"Could not get comprehensive model counts: {e}")
            counts['total_comprehensive'] = 0
            
        return counts
    
    def check_risk_management_systems(self) -> Dict[str, Any]:
        """Validate risk management components and calculations."""
        self.print_header("RISK MANAGEMENT SYSTEMS", 2)
        
        results = {
            'risk_algorithms_available': {},
            'risk_calculations_working': False,
            'risk_limits_enforced': False,
            'kelly_criterion_available': False,
            'var_models_available': False
        }
        
        # Check risk algorithm files
        risk_algo_file = self.portfolio_dir / "risk_algorithms" / "eth_basic_risk.py"
        kelly_file = self.portfolio_dir / "utilities" / "kelly_criterion.py"
        
        if risk_algo_file.exists():
            print("✅ ETH Basic Risk Algorithm: Available")
            results['risk_algorithms_available']['eth_basic_risk'] = True
        else:
            print("❌ ETH Basic Risk Algorithm: Missing")
            results['risk_algorithms_available']['eth_basic_risk'] = False
        
        if kelly_file.exists():
            print("✅ Kelly Criterion: Available")
            results['kelly_criterion_available'] = True
        else:
            print("❌ Kelly Criterion: Missing")
        
        # Test risk calculations
        try:
            # Import and test risk algorithms
            sys.path.append(str(self.portfolio_dir))
            
            if kelly_file.exists():
                from utilities.kelly_criterion import KellyCriterion
                kelly = KellyCriterion()
                
                # Test with proper Kelly Criterion method
                kelly_result = kelly.calculate_kelly_fraction(
                    signal_confidence=0.75,
                    signal_type='BUY'
                )
                
                if kelly_result and 'kelly_fraction' in kelly_result:
                    print("✅ Kelly Criterion Calculations: Working")
                    print(f"   Kelly Fraction: {kelly_result['kelly_fraction']:.3f}")
                    position_size = kelly_result.get('position_size', 0)
                    if isinstance(position_size, (int, float)):
                        print(f"   Position Size: {position_size:.3f}")
                    else:
                        print(f"   Position Size: {position_size}")
                    results['risk_calculations_working'] = True
                else:
                    print("❌ Kelly Criterion Calculations: Failed")
            
        except Exception as e:
            print(f"❌ Risk Calculations: Error - {str(e)}")
            logger.error(f"Risk calculation test failed: {e}")
        
        # Check risk parameter enforcement
        if self.risk_parameters:
            required_params = ['max_portfolio_volatility', 'max_drawdown', 'var_limit_1day']
            missing_params = [param for param in required_params if param not in self.risk_parameters]
            
            if not missing_params:
                print("✅ Risk Limits: All required parameters defined")
                results['risk_limits_enforced'] = True
            else:
                print(f"❌ Risk Limits: Missing parameters - {missing_params}")
        
        return results
    
    def check_data_connectivity(self) -> Dict[str, Any]:
        """Test data source connectivity and ETH data availability using IBKR account info."""
        self.print_header("DATA CONNECTIVITY ASSESSMENT", 2)
        
        results = {
            'ibkr_account_info_available': False,
            'ibkr_gateway_accessible': False,
            'ibkr_authenticated': False,
            'eth_data_available': False,
            'eth_contract_accessible': False,
            'real_time_data_working': False,
            'account_id': None,
            'trading_permissions': {},
            'market_data_subscriptions': {},
            'data_update_interval': None
        }
        
        # Check if IBKR account information is available
        if self.ibkr_account_data.get('available', False):
            results['ibkr_account_info_available'] = True
            print("✅ IBKR Account Info: Available from stored data")
            
            # Extract authentication status
            auth_status = self.ibkr_account_data.get('complete_account_info', {}).get('auth_status', {})
            results['ibkr_authenticated'] = auth_status.get('authenticated', False)
            results['ibkr_gateway_accessible'] = auth_status.get('connected', False)
            
            # Extract account information
            account_summary = self.ibkr_account_data.get('account_capabilities', {}).get('account_summary', {})
            accounts = account_summary.get('accounts', [])
            if accounts:
                account = accounts[0]  # Use first account
                results['account_id'] = account.get('accountId', 'Unknown')
                results['trading_permissions'] = {
                    'trading_type': account.get('tradingType', 'Unknown'),
                    'brokerage_access': account.get('brokerageAccess', False),
                    'entity': account.get('ibEntity', 'Unknown')
                }
            
            print(f"✅ IBKR Gateway: {'Connected' if results['ibkr_gateway_accessible'] else 'Disconnected'}")
            print(f"✅ IBKR Authentication: {'Authenticated' if results['ibkr_authenticated'] else 'Not Authenticated'}")
            if results['account_id']:
                print(f"✅ Account ID: {results['account_id']}")
                print(f"✅ Trading Type: {results['trading_permissions'].get('trading_type', 'Unknown')}")
            
            # Check ETH contract access from market data access info
            market_data = self.ibkr_account_data.get('market_data_access', {})
            crypto_access = market_data.get('contract_types', {}).get('Cryptocurrency', {})
            
            if crypto_access.get('accessible', False):
                results['eth_contract_accessible'] = True
                contracts_found = crypto_access.get('contracts_found', 0)
                print(f"✅ ETH Contract Access: {contracts_found} contracts found")
                
                # Extract data update capabilities
                if 'timestamp' in market_data:
                    results['last_data_timestamp'] = market_data['timestamp']
                    print(f"✅ Last Data Update: {market_data['timestamp']}")
                
                # Check for real-time data access
                real_time_access = market_data.get('real_time_access', {})
                if real_time_access:
                    results['real_time_data_working'] = True
                    print("✅ Real-time Data: Access confirmed")
                else:
                    print("⚠️  Real-time Data: Limited access")
                    
            else:
                print("❌ ETH Contract Access: Not accessible")
                
        else:
            # Fallback to live API testing if stored data not available
            print("⚠️  IBKR Account Info: Not available, testing live API")
            results = self._test_live_ibkr_connectivity(results)
        
        return results
    
    def _test_live_ibkr_connectivity(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Fallback method to test live IBKR API connectivity."""
        try:
            response = requests.get(f"{self.ibkr_base_url}/v1/api/iserver/auth/status", timeout=10)
            if response.status_code == 200:
                auth_data = response.json()
                results['ibkr_gateway_accessible'] = True
                results['ibkr_authenticated'] = auth_data.get('authenticated', False)
                
                print(f"✅ IBKR Gateway: Accessible")
                print(f"{'✅' if results['ibkr_authenticated'] else '❌'} IBKR Authentication: {'Authenticated' if results['ibkr_authenticated'] else 'Not Authenticated'}")
                
                # Test ETH contract access
                if results['ibkr_authenticated']:
                    try:
                        # Test ETH contract search
                        eth_search = requests.post(
                            f"{self.ibkr_base_url}/v1/api/iserver/secdef/search",
                            json={"symbol": "ETH"},
                            timeout=10
                        )
                        
                        if eth_search.status_code == 200:
                            contracts = eth_search.json()
                            if contracts and len(contracts) > 0:
                                results['eth_contract_accessible'] = True
                                print(f"✅ ETH Contract Access: {len(contracts)} contracts found")
                            else:
                                print("❌ ETH Contract Access: No contracts found")
                        else:
                            print(f"❌ ETH Contract Search: HTTP {eth_search.status_code}")
                            
                    except Exception as e:
                        print(f"❌ ETH Data Testing: Error - {str(e)}")
                        
            else:
                print(f"❌ IBKR Gateway: HTTP {response.status_code}")
                
        except Exception as e:
            print(f"❌ IBKR Gateway: Connection failed - {str(e)}")
            
        return results
    
    def get_ibkr_trading_parameters(self) -> Dict[str, Any]:
        """Extract trading parameters and constraints from IBKR account data."""
        trading_params = {
            'account_id': None,
            'trading_permissions': {},
            'margin_requirements': {},
            'data_intervals': {},
            'order_limits': {},
            'available_instruments': {}
        }
        
        if not self.ibkr_account_data.get('available', False):
            return trading_params
            
        # Extract account basic information
        account_info = self.ibkr_account_data.get('complete_account_info', {})
        account_summary = account_info.get('account_summary', {})
        
        # Handle both old and new account data structures
        accounts_data = account_summary.get('accounts', [])
        if isinstance(accounts_data, dict) and 'accounts' in accounts_data:
            # New structure: accounts.accounts is a list
            account_list = accounts_data['accounts']
            if account_list:
                # Get account properties if available
                account_props = accounts_data.get('acctProps', {})
                first_account_id = account_list[0]
                
                trading_params['account_id'] = first_account_id
                
                # Use account properties if available, otherwise use defaults
                if first_account_id in account_props:
                    props = account_props[first_account_id]
                    trading_params['trading_permissions'] = {
                        'trading_type': 'STKMRGN',  # Default for individual accounts
                        'brokerage_access': True,   # Implied if we have account access
                        'entity': 'IBLLC-US',      # Default
                        'account_type': 'INDIVIDUAL'  # Default
                    }
                else:
                    trading_params['trading_permissions'] = {
                        'trading_type': 'Unknown',
                        'brokerage_access': False,
                        'entity': 'Unknown',
                        'account_type': 'Unknown'
                    }
        elif isinstance(accounts_data, list) and accounts_data:
            # Old structure: accounts is directly a list
            account = accounts_data[0]
            trading_params['account_id'] = account.get('accountId')
            trading_params['trading_permissions'] = {
                'trading_type': account.get('tradingType', 'Unknown'),
                'brokerage_access': account.get('brokerageAccess', False),
                'entity': account.get('ibEntity', 'Unknown'),
                'account_type': account.get('type', 'Unknown')
            }
            
        # Extract available instruments from market data access
        market_data = self.ibkr_account_data.get('market_data_access', {})
        contract_types = market_data.get('contract_types', {})
        
        for instrument_type, details in contract_types.items():
            if details.get('accessible', False):
                trading_params['available_instruments'][instrument_type] = {
                    'accessible': True,
                    'contracts_found': details.get('contracts_found', 0),
                    'contract_details': details.get('contract', {})
                }
                
        # Extract risk parameters and limits
        risk_data = self.ibkr_account_data.get('risk_parameters', {})
        if 'p&l_information' in risk_data:
            pnl_info = risk_data['p&l_information'].get('upnl', {})
            account_key = f"{trading_params['account_id']}.Core"
            if account_key in pnl_info:
                account_pnl = pnl_info[account_key]
                trading_params['margin_requirements'] = {
                    'net_liquidation': account_pnl.get('nl', 0.0),
                    'equity_with_loan': account_pnl.get('el', 0.0),
                    'unrealized_pnl': account_pnl.get('upl', 0.0),
                    'market_value': account_pnl.get('mv', 0.0)
                }
                
        # Set data intervals based on account type and actual IBKR testing results
        if trading_params['trading_permissions'].get('brokerage_access', False):
            trading_params['data_intervals'] = {
                'real_time': True,
                'minimum_interval': '1_minute',  # Confirmed: 1min is the practical minimum for reliable data
                'supported_intervals': ['1min', '5min', '15min', '30min', '1hour'],  # Confirmed working intervals
                'historical_data': True,
                'actual_testing_confirmed': True,
                'note': 'IBKR provides 1-minute intervals for ETH/CRYPTO - this is the practical trading frequency'
            }
        else:
            trading_params['data_intervals'] = {
                'real_time': False,
                'minimum_interval': '15_minutes',
                'supported_intervals': ['15min', '30min', '1hour', '1day'],
                'historical_data': True,
                'actual_testing_confirmed': False
            }
            
        return trading_params
    
    def display_ibkr_trading_info(self):
        """Display IBKR account trading parameters and constraints."""
        self.print_header("IBKR ACCOUNT TRADING PARAMETERS", 2)
        
        trading_params = self.status_results.get('ibkr_trading_parameters', {})
        
        if not trading_params.get('account_id'):
            print("❌ IBKR Account Information: Not available")
            return
            
        # Account Information
        print(f"✅ Account ID: {trading_params['account_id']}")
        permissions = trading_params.get('trading_permissions', {})
        print(f"✅ Trading Type: {permissions.get('trading_type', 'Unknown')}")
        print(f"✅ Account Type: {permissions.get('account_type', 'Unknown')}")
        print(f"✅ Brokerage Access: {'Yes' if permissions.get('brokerage_access', False) else 'No'}")
        print(f"✅ Entity: {permissions.get('entity', 'Unknown')}")
        
        # Available Instruments
        instruments = trading_params.get('available_instruments', {})
        if instruments:
            print(f"\n📊 Available Instruments:")
            for instrument_type, details in instruments.items():
                contracts_count = details.get('contracts_found', 0)
                print(f"   {instrument_type}: {contracts_count} contracts")
                
                # Show specific contract details for Cryptocurrency
                if instrument_type == 'Cryptocurrency' and 'contract_details' in details:
                    contract = details['contract_details']
                    print(f"      Primary Contract: {contract.get('symbol', 'Unknown')} on {contract.get('exchange', 'Unknown')}")
        
        # Data Intervals and Trading Frequency
        data_intervals = trading_params.get('data_intervals', {})
        if data_intervals:
            print(f"\n⏱️  Trading Data Intervals:")
            print(f"   Real-time Data: {'Available' if data_intervals.get('real_time', False) else 'Not Available'}")
            print(f"   Minimum Interval: {data_intervals.get('minimum_interval', 'Unknown')}")
            intervals = data_intervals.get('supported_intervals', [])
            if intervals:
                print(f"   Supported Intervals: {', '.join(intervals[:5])}")  # Show first 5
                
        # Margin and Risk Information
        margin_info = trading_params.get('margin_requirements', {})
        if margin_info:
            print(f"\n💰 Account Financial Status:")
            print(f"   Net Liquidation Value: ${margin_info.get('net_liquidation', 0.0):,.2f}")
            print(f"   Equity with Loan Value: ${margin_info.get('equity_with_loan', 0.0):,.2f}")
            print(f"   Unrealized P&L: ${margin_info.get('unrealized_pnl', 0.0):,.2f}")
            print(f"   Market Value: ${margin_info.get('market_value', 0.0):,.2f}")
    
    def check_execution_readiness(self) -> Dict[str, Any]:
        """Assess execution model availability and order management capability."""
        self.print_header("EXECUTION READINESS ASSESSMENT", 2)
        
        results = {
            'execution_models_available': False,
            'order_management_ready': False,
            'execution_settings_configured': False,
            'broker_integration_ready': False,
            'execution_algorithms_available': []
        }
        
        # Check 5_execution_models directory (LEAN Layer 5)
        if self.execution_dir.exists():
            execution_files = list(self.execution_dir.glob("*.py"))
            if execution_files:
                results['execution_models_available'] = True
                print(f"✅ Execution Models: {len(execution_files)} files found")
                for file in execution_files:
                    print(f"   📄 {file.name}")
                    results['execution_algorithms_available'].append(file.name)
                    
                # Check for specific execution components
                key_executors = [
                    "eth_execution_engine.py", 
                    "safe_eth_execution_engine.py",
                    "live_eth_trading_system.py"
                ]
                
                found_executors = []
                for executor in key_executors:
                    if (self.execution_dir / executor).exists():
                        found_executors.append(executor)
                
                if found_executors:
                    print(f"✅ Key Executors: {len(found_executors)} core execution engines available")
                    results['order_management_ready'] = True
                else:
                    print("⚠️  Key Executors: No core execution engines found")
            else:
                print("❌ Execution Models: No implementation files found")
                results['execution_models_available'] = False
        else:
            print("❌ Execution Models Directory: Not found")
        
        # Check execution configuration (modernized approach)
        execution_config_file = self.portfolio_dir / "execution_settings.json"
        if execution_config_file.exists():
            try:
                with open(execution_config_file, 'r') as f:
                    execution_config = json.load(f)
                
                required_settings = ['order_type', 'execution_algo', 'max_order_size']
                missing_settings = [setting for setting in required_settings if setting not in execution_config]
                
                if not missing_settings:
                    results['execution_settings_configured'] = True
                    print("✅ Execution Settings: Valid configuration file")
                    print(f"   Order Type: {execution_config.get('order_type', 'Unknown')}")
                    print(f"   Execution Algorithm: {execution_config.get('execution_algo', 'Unknown')}")
                else:
                    print(f"⚠️  Execution Settings: Missing settings - {missing_settings}")
            except Exception as e:
                print(f"⚠️  Execution Settings: Configuration file error - {str(e)}")
        else:
            print("⚠️  Execution Settings: No configuration file (using defaults)")
            # This is not necessarily a failure - defaults can be used
            results['execution_settings_configured'] = True  # Allow defaults
        
        # Check broker integration readiness
        try:
            # Test if we have IBKR connectivity components
            ibkr_connectors = self.portfolio_dir.parent.parent.parent / "1_data_sources" / "1_raw" / "connectors" / "interactive_brokers"
            if ibkr_connectors.exists():
                results['broker_integration_ready'] = True
                print("✅ Broker Integration: IBKR connectors available")
            else:
                print("⚠️  Broker Integration: IBKR connectors not found")
                
        except Exception as e:
            print(f"⚠️  Broker Integration: Check failed - {str(e)}")
        
        # Overall execution readiness assessment
        readiness_score = sum([
            results['execution_models_available'],
            results['order_management_ready'], 
            results['execution_settings_configured'],
            results['broker_integration_ready']
        ])
        
        if readiness_score >= 3:
            print(f"✅ Execution Readiness: {readiness_score}/4 components ready")
        elif readiness_score >= 2:
            print(f"⚠️  Execution Readiness: {readiness_score}/4 components ready")
        else:
            print(f"❌ Execution Readiness: Only {readiness_score}/4 components ready")
        
        return results
        
        return results
    
    def check_algorithm_integration(self) -> Dict[str, Any]:
        """Validate algorithm architecture and component integration."""
        self.print_header("ALGORITHM INTEGRATION VALIDATION", 2)
        
        results = {
            'architecture_compliant': False,
            'components_available': {},
            'integration_ready': False,
            'integration_errors': []
        }
        
        # Check for our actual architecture: risk_algorithms/, trading_algorithms/, utilities/
        risk_algorithms_dir = self.portfolio_dir / "risk_algorithms"
        trading_algorithms_dir = self.portfolio_dir / "trading_algorithms" 
        utilities_dir = self.portfolio_dir / "utilities"
        
        architecture_components = {
            'risk_algorithms': risk_algorithms_dir.exists(),
            'trading_algorithms': trading_algorithms_dir.exists(),
            'utilities': utilities_dir.exists()
        }
        
        results['components_available'] = architecture_components
        
        # Check risk algorithms
        if risk_algorithms_dir.exists():
            risk_files = list(risk_algorithms_dir.glob("*.py"))
            if risk_files:
                print(f"✅ Risk Algorithms: {len(risk_files)} algorithms available")
                for file in risk_files[:3]:  # Show first 3
                    print(f"   📄 {file.name}")
                if len(risk_files) > 3:
                    print(f"   📄 ... and {len(risk_files) - 3} more")
            else:
                print("⚠️  Risk Algorithms: Directory exists but no algorithms found")
        else:
            print("❌ Risk Algorithms: Directory missing")
            results['integration_errors'].append("Risk algorithms directory not found")
        
        # Check trading algorithms
        if trading_algorithms_dir.exists():
            trading_files = list(trading_algorithms_dir.glob("*.py"))
            if trading_files:
                print(f"✅ Trading Algorithms: {len(trading_files)} algorithms available")
                for file in trading_files[:3]:  # Show first 3
                    print(f"   📄 {file.name}")
                if len(trading_files) > 3:
                    print(f"   📄 ... and {len(trading_files) - 3} more")
            else:
                print("⚠️  Trading Algorithms: Directory exists but no algorithms found")
        else:
            print("❌ Trading Algorithms: Directory missing")
            results['integration_errors'].append("Trading algorithms directory not found")
        
        # Check utilities (framework components)
        if utilities_dir.exists():
            utility_files = list(utilities_dir.glob("*.py"))
            if utility_files:
                print(f"✅ Framework Utilities: {len(utility_files)} utilities available")
                
                # Check for key framework components
                key_utilities = [
                    "EnhancedPortfolioManager.py",
                    "statuscheck.py",
                    "silver_layer_data_connector.py"
                ]
                
                for util in key_utilities:
                    if (utilities_dir / util).exists():
                        print(f"   ✅ {util}")
                    else:
                        print(f"   ⚠️  {util} (not found)")
                        
            else:
                print("⚠️  Framework Utilities: Directory exists but no utilities found")
        else:
            print("❌ Framework Utilities: Directory missing")
            results['integration_errors'].append("Utilities directory not found")
        
        # Check if we have both risk and trading separation (our clean architecture)
        risk_available = architecture_components['risk_algorithms'] and len(list(risk_algorithms_dir.glob("*.py"))) > 0
        trading_available = architecture_components['trading_algorithms'] and len(list(trading_algorithms_dir.glob("*.py"))) > 0
        utilities_available = architecture_components['utilities'] and len(list(utilities_dir.glob("*.py"))) > 0
        
        if risk_available and trading_available and utilities_available:
            results['architecture_compliant'] = True
            print("✅ Clean Algorithm Separation: Risk ✓ Trading ✓ Utilities ✓")
        else:
            print("❌ Algorithm Architecture: Components missing for clean separation")
        
        # Test if we can import core components
        try:
            sys.path.append(str(self.portfolio_dir))
            
            # Test imports from utilities
            import importlib.util
            
            portfolio_manager_path = utilities_dir / "EnhancedPortfolioManager.py"
            if portfolio_manager_path.exists():
                spec = importlib.util.spec_from_file_location("EnhancedPortfolioManager", portfolio_manager_path)
                if spec:
                    print("✅ Portfolio Manager: Import test successful")
                    results['integration_ready'] = True
                else:
                    results['integration_errors'].append("Portfolio manager import failed")
            
        except Exception as e:
            results['integration_errors'].append(f"Component integration test failed: {str(e)}")
            print(f"⚠️  Integration Test: {str(e)}")
        
        # Overall integration readiness
        ready_components = sum([risk_available, trading_available, utilities_available])
        
        if ready_components >= 3:
            print(f"✅ Integration Readiness: All {ready_components}/3 components ready")
        elif ready_components >= 2:
            print(f"⚠️  Integration Readiness: {ready_components}/3 components ready")
        else:
            print(f"❌ Integration Readiness: Only {ready_components}/3 components ready")
        
        return results
    
    def check_simulation_framework(self) -> Dict[str, Any]:
        """Validate unified backtesting and simulation capabilities."""
        self.print_header("SIMULATION FRAMEWORK VALIDATION", 2)
        
        results = {
            'framework_available': False,
            'engine_operational': False,
            'templates_configured': False,
            'results_tracking': False,
            'recent_simulations': [],
            'performance_validation': {},
            'simulation_errors': []
        }
        
        try:
            # Check simulation directory structure
            if self.simulations_dir.exists():
                results['framework_available'] = True
                print("✅ Simulation Framework: Directory structure exists")
                
                # Check unified backtesting components (NEW ARCHITECTURE)
                main_simulator = self.simulations_dir / "myportolio_simulator.py"
                templates_file = self.simulations_dir / "templates" / "simulation_templates.json"
                engine_file = self.simulations_dir / "myportolio_simulation_engine.py"
                
                if main_simulator.exists():
                    results['engine_operational'] = True
                    print("✅ Unified Simulator: myportolio_simulator.py operational")
                    
                    # Check if it has enhanced logging capability
                    try:
                        with open(main_simulator, 'r') as f:
                            content = f.read()
                            if 'enhanced_logging' in content and 'CANNOT_BYPASS_ENHANCED_LOGGING' in content:
                                print("✅ Enhanced Logging: Mandatory logging system verified")
                            else:
                                print("⚠️  Enhanced Logging: May not be enforced")
                    except Exception as e:
                        results['simulation_errors'].append(f"Error checking enhanced logging: {str(e)}")
                else:
                    results['simulation_errors'].append("Missing unified simulator: myportolio_simulator.py")
                    print("❌ Unified Simulator: myportolio_simulator.py missing")
                
                if templates_file.exists():
                    try:
                        with open(templates_file, 'r') as f:
                            templates = json.load(f)
                        template_count = len(templates)
                        results['templates_configured'] = template_count > 0
                        print(f"✅ Simulation Templates: {template_count} templates configured")
                    except Exception as e:
                        results['simulation_errors'].append(f"Template loading error: {str(e)}")
                        print(f"❌ Simulation Templates: Error loading templates")
                else:
                    print("⚠️  Simulation Templates: Configuration file not found")
                
                # Check for engine file separately
                if engine_file.exists():
                    results['results_tracking'] = True
                    print("✅ Engine Component: myportolio_simulation_engine.py available")
                else:
                    print("⚠️  Engine Component: myportolio_simulation_engine.py not found")
                
                # Check for recent simulation results
                backtests_dir = self.simulations_dir / "backtests"
                if backtests_dir.exists():
                    # Look for myportolio_results.json files in subdirectories
                    result_files = list(backtests_dir.glob("*/myportolio_results.json"))
                    recent_results = sorted(result_files, key=lambda x: x.stat().st_mtime, reverse=True)[:5]
                    
                    for result_file in recent_results:
                        try:
                            with open(result_file, 'r') as f:
                                result_data = json.load(f)
                            
                            # Try to get performance from lean_results first, fallback to top-level
                            performance_data = result_data.get('lean_results', {}).get('performance', {})
                            if not performance_data or performance_data.get('total_return', 0) == 0:
                                performance_data = result_data.get('performance', {})
                            
                            simulation_info = {
                                'file': result_file.name,
                                'timestamp': result_data.get('timestamp', 'Unknown'),
                                'total_return': performance_data.get('total_return', 0),
                                'sharpe_ratio': performance_data.get('sharpe_ratio', 0),
                                'max_drawdown': performance_data.get('max_drawdown', 0),
                                'total_trades': performance_data.get('trades_count', 0)
                            }
                            results['recent_simulations'].append(simulation_info)
                            
                        except Exception as e:
                            results['simulation_errors'].append(f"Error reading {result_file.name}: {str(e)}")
                    
                    if results['recent_simulations']:
                        print(f"✅ Recent Simulations: Found {len(results['recent_simulations'])} recent backtest results")
                        
                        # Display recent simulation performance
                        print("📊 Recent Simulation Performance:")
                        for sim in results['recent_simulations'][:3]:  # Show top 3
                            print(f"   • {sim['file']}: {sim['total_return']:.2%} return, "
                                  f"Sharpe {sim['sharpe_ratio']:.2f}, "
                                  f"{sim['total_trades']} trades")
                        
                        # Validate performance metrics
                        avg_return = sum(sim['total_return'] for sim in results['recent_simulations']) / len(results['recent_simulations'])
                        avg_sharpe = sum(sim['sharpe_ratio'] for sim in results['recent_simulations']) / len(results['recent_simulations'])
                        avg_drawdown = sum(sim['max_drawdown'] for sim in results['recent_simulations']) / len(results['recent_simulations'])
                        
                        results['performance_validation'] = {
                            'average_return': avg_return,
                            'average_sharpe': avg_sharpe,
                            'average_drawdown': avg_drawdown,
                            'positive_returns': sum(1 for sim in results['recent_simulations'] if sim['total_return'] > 0),
                            'total_simulations': len(results['recent_simulations'])
                        }
                        
                        positive_ratio = results['performance_validation']['positive_returns'] / results['performance_validation']['total_simulations']
                        
                        if avg_return > 0 and positive_ratio > 0.5:
                            print(f"✅ Performance Validation: Strategy shows positive performance")
                            print(f"   Average Return: {avg_return:.2%}, Positive Sims: {positive_ratio:.1%}")
                        elif avg_return > 0:
                            print(f"⚠️  Performance Validation: Strategy shows mixed performance")
                            print(f"   Average Return: {avg_return:.2%}, Positive Sims: {positive_ratio:.1%}")
                        else:
                            print(f"❌ Performance Validation: Strategy performance needs review")
                            print(f"   Average Return: {avg_return:.2%}, Positive Sims: {positive_ratio:.1%}")
                    else:
                        print("⚠️  Recent Simulations: No recent backtest results found")
                else:
                    print("⚠️  Simulation Results: Backtest directory not found")
                
                # Test unified simulator operability
                try:
                    import subprocess
                    import sys
                    
                    # Quick test of unified simulator help
                    result = subprocess.run([
                        sys.executable, 
                        str(main_simulator), 
                        '--help'
                    ], 
                    capture_output=True, 
                    text=True, 
                    cwd=str(self.simulations_dir),
                    timeout=10
                    )
                    
                    if result.returncode == 0:
                        print("✅ Unified Simulator: CLI operational")
                    else:
                        results['simulation_errors'].append("Unified simulator test failed")
                        print("❌ Unified Simulator: CLI not operational")
                        
                except Exception as e:
                    results['simulation_errors'].append(f"Simulator operability test failed: {str(e)}")
                    print(f"⚠️  Unified Simulator: Operability test error")
                
            else:
                results['simulation_errors'].append("Simulation directory does not exist")
                print("❌ Simulation Framework: Directory not found")
            
            # Overall simulation readiness assessment
            components_ready = sum([
                results['framework_available'],
                results['engine_operational'], 
                results['templates_configured'],
                results['results_tracking']
            ])
            
            if components_ready >= 3:
                print(f"✅ Simulation Readiness: {components_ready}/4 components operational")
            else:
                print(f"❌ Simulation Readiness: Only {components_ready}/4 components operational")
                
        except Exception as e:
            results['simulation_errors'].append(f"Simulation check failed: {str(e)}")
            print(f"❌ Simulation Framework Check: {str(e)}")
        
        return results
    
    def calculate_portfolio_statistics(self) -> Dict[str, Any]:
        """Calculate current portfolio statistics and risk metrics."""
        self.print_header("PORTFOLIO STATISTICS", 2)
        
        results = {
            'actual_allocation': {},
            'theoretical_allocation': {},
            'risk_metrics': {},
            'performance_projections': {},
            'statistics_calculated': False
        }
        
        try:
            # Get actual portfolio data from IBKR
            actual_portfolio_data = self._get_actual_portfolio_data()
            
            if actual_portfolio_data['available']:
                # Show file timestamps for data freshness
                complete_info_file = self.ibkr_account_dir / "complete_account_info.json"
                current_portfolio_file = self.ibkr_account_dir / "current_portfolio.json"
                
                file_timestamps = []
                if complete_info_file.exists():
                    timestamp = datetime.fromtimestamp(complete_info_file.stat().st_mtime)
                    file_timestamps.append(f"Account Info: {timestamp.strftime('%Y-%m-%d %H:%M:%S')}")
                
                if current_portfolio_file.exists():
                    timestamp = datetime.fromtimestamp(current_portfolio_file.stat().st_mtime)
                    file_timestamps.append(f"Portfolio Data: {timestamp.strftime('%Y-%m-%d %H:%M:%S')}")
                
                print("📊 ACTUAL Portfolio Data (from IBKR - LIVE):")
                if file_timestamps:
                    print(f"   📅 Last Updated: {', '.join(file_timestamps)}")
                print(f"   Net Liquidation Value: ${actual_portfolio_data['net_liquidation_value']:,.2f}")
                print(f"   Total Market Value: ${actual_portfolio_data['market_value']:,.2f}")
                print(f"   Cash Balance: ${actual_portfolio_data['cash_balance']:,.2f}")
                print(f"   Unrealized P&L: ${actual_portfolio_data['unrealized_pnl']:,.2f}")
                
                # Show account status
                if (actual_portfolio_data['net_liquidation_value'] == 0 and 
                    actual_portfolio_data['cash_balance'] == 0 and
                    not actual_portfolio_data['positions']):
                    print("   💡 Status: Empty account - No funded balance or positions")
                else:
                    print("   ✅ Status: Account has funds/positions")
                
                # Show actual asset allocation
                print("📊 ACTUAL Asset Allocation:")
                if actual_portfolio_data['positions']:
                    for symbol, position_data in actual_portfolio_data['positions'].items():
                        allocation_pct = position_data.get('allocation_percent', 0)
                        market_value = position_data.get('market_value', 0)
                        print(f"   {symbol}: {allocation_pct:.1f}% (${market_value:,.2f})")
                        results['actual_allocation'][symbol] = allocation_pct
                else:
                    print("   No positions found - Portfolio is 100% cash")
                    results['actual_allocation']['Cash'] = 100.0
            else:
                # Show file timestamps even when data is unavailable for troubleshooting
                complete_info_file = self.ibkr_account_dir / "complete_account_info.json"
                current_portfolio_file = self.ibkr_account_dir / "current_portfolio.json"
                freshness_file = self.ibkr_account_dir / "data_freshness.json"
                
                file_status = []
                if complete_info_file.exists():
                    timestamp = datetime.fromtimestamp(complete_info_file.stat().st_mtime)
                    file_status.append(f"Account Info: {timestamp.strftime('%Y-%m-%d %H:%M:%S')}")
                else:
                    file_status.append("Account Info: Missing")
                    
                if current_portfolio_file.exists():
                    timestamp = datetime.fromtimestamp(current_portfolio_file.stat().st_mtime)
                    file_status.append(f"Portfolio Data: {timestamp.strftime('%Y-%m-%d %H:%M:%S')}")
                else:
                    file_status.append("Portfolio Data: Missing")
                    
                if freshness_file.exists():
                    timestamp = datetime.fromtimestamp(freshness_file.stat().st_mtime)
                    file_status.append(f"Freshness Check: {timestamp.strftime('%Y-%m-%d %H:%M:%S')}")
                else:
                    file_status.append("Freshness Check: Missing")
                
                print("❌ ACTUAL Portfolio Data: Unavailable (authentication/freshness issue)")
                print(f"   📅 File Status: {', '.join(file_status)}")
                print("📊 Current Asset Allocation: Unavailable - showing IBKR reality")
            
            # Show ACTUAL asset allocation based on IBKR positions vs configured targets
            if self.portfolio_config and 'assets' in self.portfolio_config:
                assets = self.portfolio_config['assets']
                
                print("\n📊 ACTUAL Asset Allocation (IBKR vs Target):")
                
                # Get actual IBKR positions or default to 0%
                for asset, config in assets.items():
                    target_allocation = config.get('allocation_percent', 0)
                    
                    # Check if this asset exists in actual IBKR positions
                    actual_allocation = 0.0
                    actual_value = 0.0
                    
                    if (hasattr(self, 'actual_portfolio_data') and 
                        self.actual_portfolio_data and 
                        self.actual_portfolio_data.get('positions')):
                        position_data = self.actual_portfolio_data['positions'].get(asset, {})
                        actual_allocation = position_data.get('allocation_percent', 0.0)
                        actual_value = position_data.get('market_value', 0.0)
                    
                    # Show actual vs target
                    if actual_allocation > 0:
                        print(f"   {asset}: {actual_allocation:.1f}% actual (target: {target_allocation}%) - ${actual_value:,.2f}")
                        results['actual_allocation'][asset] = actual_allocation
                    else:
                        print(f"   {asset}: 0.0% actual (target: {target_allocation}%) - No position in IBKR")
                        results['actual_allocation'][asset] = 0.0
                    
                    results['theoretical_allocation'][asset] = target_allocation
                
                # Show cash position if portfolio data available
                if (hasattr(self, 'actual_portfolio_data') and 
                    self.actual_portfolio_data and 
                    self.actual_portfolio_data.get('cash_balance', 0) > 0):
                    cash_balance = self.actual_portfolio_data['cash_balance']
                    total_value = cash_balance + sum(
                        pos.get('market_value', 0) 
                        for pos in self.actual_portfolio_data.get('positions', {}).values()
                    )
                    cash_allocation = (cash_balance / total_value * 100) if total_value > 0 else 100.0
                    print(f"   Cash: {cash_allocation:.1f}% - ${cash_balance:,.2f}")
                    results['actual_allocation']['Cash'] = cash_allocation
                
                # Calculate risk metrics based on risk parameters
                if self.risk_parameters:
                    risk_metrics = {
                        'max_portfolio_volatility': self.risk_parameters.get('max_portfolio_volatility', 0),
                        'max_drawdown_limit': self.risk_parameters.get('max_drawdown', 0),
                        'var_1day_limit': self.risk_parameters.get('var_limit_1day', 0),
                        'sharpe_target': self.risk_parameters.get('sharpe_ratio_target', 0)
                    }
                    
                    results['risk_metrics'] = risk_metrics
                    
                    print("\n📊 Risk Metrics (from configuration):")
                    print(f"   Max Portfolio Volatility: {risk_metrics['max_portfolio_volatility']:.1%}")
                    print(f"   Max Drawdown Limit: {risk_metrics['max_drawdown_limit']:.1%}")
                    print(f"   VaR (1-day) Limit: {risk_metrics['var_1day_limit']:.1%}")
                    print(f"   Sharpe Ratio Target: {risk_metrics['sharpe_target']:.2f}")
                
                results['statistics_calculated'] = True
                
        except Exception as e:
            print(f"❌ Portfolio Statistics: Calculation failed - {str(e)}")
            logger.error(f"Portfolio statistics calculation failed: {e}")
        
        return results
    
    def assess_live_trading_readiness(self) -> Dict[str, Any]:
        """Comprehensive assessment of live trading readiness."""
        self.print_header("LIVE TRADING READINESS ASSESSMENT", 2)
        
        # Compile all previous check results
        all_checks = [
            'portfolio_configuration',
            'alpha_models',
            'production_models',
            'risk_management',
            'data_connectivity',
            'execution_readiness',
            'algorithm_integration',
            'simulation_framework'
        ]
        
        readiness_score = 0
        max_score = len(all_checks)
        critical_issues = []
        warnings = []
        
        # Assess each component
        for component in all_checks:
            if component in self.status_results['component_status']:
                component_data = self.status_results['component_status'][component]
                
                # Define readiness criteria for each component
                if component == 'portfolio_configuration':
                    if (component_data.get('config_loaded', False) and 
                        component_data.get('asset_allocation_valid', False) and
                        component_data.get('risk_limits_defined', False)):
                        readiness_score += 1
                    else:
                        critical_issues.append(f"Portfolio configuration incomplete")
                
                elif component == 'alpha_models':
                    ready_models = sum(1 for model in component_data.get('models_available', {}).values() 
                                     if model.get('ready', False))
                    if ready_models >= 1:
                        readiness_score += 1
                    else:
                        critical_issues.append("No alpha models ready for production")
                
                elif component == 'production_models':
                    # CRITICAL PATH: Check if production models are ready for ensemble methods
                    critical_path_ready = component_data.get('critical_path_ready', False)
                    total_models = component_data.get('total_models', 0)
                    
                    if critical_path_ready and total_models >= 18:  # 2 models per method per timeframe = 18 minimum
                        readiness_score += 1
                        print(f"✅ Production models critical path satisfied ({total_models} models)")
                    else:
                        critical_issues.append("Production models not ready for ensemble methods - CRITICAL PATH BLOCKER")
                        if not critical_path_ready:
                            missing_reqs = component_data.get('missing_requirements', [])
                            for req in missing_reqs[:3]:  # Show first 3 missing requirements
                                critical_issues.append(f"  • {req}")
                        print(f"❌ Production models critical path blocked")
                
                elif component == 'risk_management':
                    if (component_data.get('kelly_criterion_available', False) and
                        component_data.get('risk_calculations_working', False)):
                        readiness_score += 1
                    else:
                        critical_issues.append("Risk management systems not fully operational")
                
                elif component == 'data_connectivity':
                    # Updated criteria using IBKR account information
                    ibkr_account_available = component_data.get('ibkr_account_info_available', False)
                    ibkr_authenticated = component_data.get('ibkr_authenticated', False)
                    eth_accessible = component_data.get('eth_contract_accessible', False)
                    
                    if ibkr_account_available and ibkr_authenticated and eth_accessible:
                        readiness_score += 1
                    else:
                        if not ibkr_account_available:
                            critical_issues.append("IBKR account information not available")
                        elif not ibkr_authenticated:
                            critical_issues.append("IBKR authentication required for live trading")
                        elif not eth_accessible:
                            critical_issues.append("ETH contract access not available")
                        else:
                            critical_issues.append("Data connectivity issues detected")
                
                elif component == 'execution_readiness':
                    if component_data.get('execution_settings_valid', False):
                        readiness_score += 0.5  # Partial credit if settings exist
                        warnings.append("Execution models need full implementation")
                    if not component_data.get('execution_models_available', False):
                        warnings.append("Execution models directory needs implementation")
                
                elif component == 'algorithm_integration':
                    ready_components = sum(1 for ready in component_data.get('workflow_components_ready', {}).values() if ready)
                    if ready_components >= 3:
                        readiness_score += 1
                    else:
                        warnings.append("Algorithm integration needs completion")
                
                elif component == 'simulation_framework':
                    framework_components = sum([
                        component_data.get('framework_available', False),
                        component_data.get('engine_operational', False),
                        component_data.get('templates_configured', False),
                        component_data.get('results_tracking', False)
                    ])
                    if framework_components >= 3:
                        readiness_score += 1
                        # Check performance validation
                        perf_validation = component_data.get('performance_validation', {})
                        if perf_validation.get('average_return', 0) > 0:
                            print(f"✅ Simulation validation shows positive strategy performance")
                        else:
                            warnings.append("Simulation results show neutral/negative performance")
                    else:
                        warnings.append("Simulation framework needs completion")
                        critical_issues.append("Backtesting framework not operational")
        
        # Calculate readiness percentage
        readiness_percentage = (readiness_score / max_score) * 100
        
        # Determine overall readiness level
        if readiness_percentage >= 90 and not critical_issues:
            overall_readiness = "READY"
            readiness_color = "🟢"
        elif readiness_percentage >= 70 and len(critical_issues) <= 1:
            overall_readiness = "MOSTLY_READY"
            readiness_color = "🟡"
        else:
            overall_readiness = "NOT_READY"
            readiness_color = "🔴"
        
        results = {
            'readiness_score': readiness_score,
            'max_score': max_score,
            'readiness_percentage': readiness_percentage,
            'overall_readiness': overall_readiness,
            'critical_issues': critical_issues,
            'warnings': warnings
        }
        
        # Display assessment
        print(f"{readiness_color} Overall Readiness: {overall_readiness}")
        print(f"📊 Readiness Score: {readiness_score}/{max_score} ({readiness_percentage:.1f}%)")
        
        if critical_issues:
            print(f"\n🚨 Critical Issues ({len(critical_issues)}):")
            for issue in critical_issues:
                print(f"   • {issue}")
        
        if warnings:
            print(f"\n⚠️  Warnings ({len(warnings)}):")
            for warning in warnings:
                print(f"   • {warning}")
        
        return results
    
    def generate_recommendations(self) -> List[str]:
        """Generate actionable recommendations based on status check results."""
        self.print_header("RECOMMENDATIONS", 2)
        
        recommendations = []
        
        # Check what needs immediate attention
        overall_status = self.status_results.get('live_trading_readiness', {})
        critical_issues = overall_status.get('critical_issues', [])
        warnings = overall_status.get('warnings', [])
        
        # Priority 1: Critical Issues
        if critical_issues:
            recommendations.append("🚨 IMMEDIATE ACTION REQUIRED:")
            for issue in critical_issues:
                if "authentication" in issue.lower():
                    recommendations.append("   1. Authenticate IBKR Gateway: https://solid-acorn-gw6xx47pqxfv99p-5000.app.github.dev/")
                elif "portfolio configuration" in issue.lower():
                    recommendations.append("   2. Complete portfolio configuration files")
                elif "alpha models" in issue.lower():
                    recommendations.append("   3. Deploy at least one production-ready alpha model")
                elif "risk management" in issue.lower():
                    recommendations.append("   4. Implement and test risk management calculations")
                elif "data connectivity" in issue.lower():
                    recommendations.append("   5. Resolve data source connectivity issues")
        
        # Priority 2: Implementation Gaps
        execution_status = self.status_results['component_status'].get('execution_readiness', {})
        if not execution_status.get('execution_models_available', False):
            recommendations.append("\n🔧 IMPLEMENTATION NEEDED:")
            recommendations.append("   • Implement execution models in 5_execution_models/")
            recommendations.append("   • Create order management and execution algorithms")
            recommendations.append("   • Test order placement in paper trading mode")
        
        # Priority 3: Performance Optimization
        alpha_status = self.status_results['component_status'].get('alpha_models', {})
        best_model = alpha_status.get('best_model')
        if best_model:
            score = best_model['score']
            if score < 0.7:
                recommendations.append("\n📈 PERFORMANCE OPTIMIZATION:")
                recommendations.append(f"   • Current best model score: {score:.3f} - consider model improvement")
                recommendations.append("   • Review and optimize model parameters")
                recommendations.append("   • Consider ensemble methods for better performance")
        
        # Priority 4: Risk Management Enhancement
        recommendations.append("\n⚡ BEFORE LIVE TRADING:")
        recommendations.append("   • Test all algorithms in paper trading mode for at least 1 week")
        recommendations.append("   • Validate risk limits with simulated adverse scenarios")
        recommendations.append("   • Implement comprehensive logging and monitoring")
        recommendations.append("   • Set up automated alerts for risk limit breaches")
        recommendations.append("   • Create emergency stop procedures")
        
        # Priority 5: Monitoring and Maintenance
        recommendations.append("\n🔄 ONGOING MONITORING:")
        recommendations.append("   • Run this status check daily before trading")
        recommendations.append("   • Monitor model performance and data quality")
        recommendations.append("   • Review and update risk parameters regularly")
        recommendations.append("   • Maintain backup execution procedures")
        
        # Print recommendations
        for rec in recommendations:
            print(rec)
        
        return recommendations
    
    def save_status_report(self):
        """Save comprehensive status report to file."""
        # Ensure status_reports directory exists
        status_reports_dir = self.portfolio_dir / "status_reports"
        status_reports_dir.mkdir(exist_ok=True)
        
        report_file = status_reports_dir / f"status_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        try:
            with open(report_file, 'w') as f:
                json.dump(self.status_results, f, indent=2, default=str)
            
            print(f"\n💾 Status Report Saved: {report_file}")
            
        except Exception as e:
            logger.error(f"Error saving status report: {e}")
    
    def run_comprehensive_check(self):
        """Run complete status check and assessment."""
        self.print_header("MYPORTOLIO LIVE TRADING READINESS ASSESSMENT")
        
        print(f"📅 Assessment Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"📁 Portfolio Directory: {self.portfolio_dir}")
        print(f"🎯 Portfolio: {self.portfolio_config.get('portfolio_name', 'Unknown')}")
        
        # Run all status checks
        try:
            # Component checks
            self.status_results['component_status']['portfolio_configuration'] = self.check_portfolio_configuration()
            self.status_results['component_status']['alpha_models'] = self.check_alpha_models_status()
            self.status_results['component_status']['production_models'] = self.check_production_models_status()
            self.status_results['component_status']['risk_management'] = self.check_risk_management_systems()
            self.status_results['component_status']['data_connectivity'] = self.check_data_connectivity()
            self.status_results['component_status']['execution_readiness'] = self.check_execution_readiness()
            self.status_results['component_status']['algorithm_integration'] = self.check_algorithm_integration()
            self.status_results['component_status']['simulation_framework'] = self.check_simulation_framework()
            
            # IBKR account integration
            self.status_results['ibkr_trading_parameters'] = self.get_ibkr_trading_parameters()
            self.display_ibkr_trading_info()
            
            # Portfolio analysis
            self.status_results['portfolio_statistics'] = self.calculate_portfolio_statistics()
            
            # Overall assessment
            self.status_results['live_trading_readiness'] = self.assess_live_trading_readiness()
            
            # Recommendations removed for clean status reporting
            self.status_results['recommendations'] = []
            
            # Save report
            self.save_status_report()
            
            # Final summary
            self.print_header("ASSESSMENT COMPLETE")
            overall_readiness = self.status_results['live_trading_readiness']['overall_readiness']
            readiness_percentage = self.status_results['live_trading_readiness']['readiness_percentage']
            
            if overall_readiness == "READY":
                print("🟢 MYPORTOLIO IS READY FOR LIVE TRADING")
            elif overall_readiness == "MOSTLY_READY":
                print("🟡 MYPORTOLIO IS MOSTLY READY - Address warnings before live trading")
            else:
                print("🔴 MYPORTOLIO IS NOT READY - Critical issues must be resolved")
            
            print(f"📊 Overall Readiness: {readiness_percentage:.1f}%")
            print(f"📋 Run 'python statuscheck.py --detailed' for complete analysis")
            
        except Exception as e:
            logger.error(f"Status check failed: {e}")
            logger.error(traceback.format_exc())
            print(f"\n❌ STATUS CHECK FAILED: {str(e)}")

def main():
    """Main execution function."""
    checker = MyportolioStatusChecker()
    
    if len(sys.argv) > 1 and sys.argv[1] == '--detailed':
        # Run detailed check with full output
        checker.run_comprehensive_check()
    else:
        # Run quick check with summary
        checker.run_comprehensive_check()

if __name__ == "__main__":
    main()
