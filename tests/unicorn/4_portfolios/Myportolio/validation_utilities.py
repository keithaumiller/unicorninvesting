#!/usr/bin/env python3
"""
Data Validation Backend Utilities

Provides backend data access and validation utilities for comprehensive
frontend-backend data validation testing.
"""

import json
import os
import sqlite3
from pathlib import Path
from typing import Dict, List, Any, Optional, Union
from datetime import datetime
import glob

class BackendDataLoader:
    """Load and access backend portfolio data"""
    
    def __init__(self):
        self.portfolios_path = "/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios"
        self.myportolio_path = f"{self.portfolios_path}/Myportolio"
        
    def get_myportolio_config(self) -> Dict:
        """Load Myportolio configuration"""
        config_file = f"{self.myportolio_path}/config.json"
        if os.path.exists(config_file):
            with open(config_file, 'r') as f:
                return json.load(f)
        return {}
    
    def get_risk_parameters(self) -> Dict:
        """Load risk management parameters"""
        risk_file = f"{self.myportolio_path}/risk_parameters.json"
        if os.path.exists(risk_file):
            with open(risk_file, 'r') as f:
                return json.load(f)
        return {}
    
    def get_latest_status_report(self) -> Dict:
        """Get the most recent status report"""
        pattern = f"{self.myportolio_path}/status_report_*.json"
        status_files = glob.glob(pattern)
        
        if status_files:
            latest_file = max(status_files, key=os.path.getctime)
            with open(latest_file, 'r') as f:
                return json.load(f)
        return {}
    
    def get_latest_risk_report(self) -> Dict:
        """Get the most recent risk report"""
        pattern = f"{self.myportolio_path}/risk_report_*.json"
        risk_files = glob.glob(pattern)
        
        if risk_files:
            latest_file = max(risk_files, key=os.path.getctime)
            with open(latest_file, 'r') as f:
                return json.load(f)
        return {}
    
    def get_simulation_config(self, simulation_id: str) -> Dict:
        """Load LEAN simulation configuration"""
        sim_path = f"{self.myportolio_path}/simulations/backtests/{simulation_id}"
        config_file = f"{sim_path}/lean_config.json"
        
        if os.path.exists(config_file):
            with open(config_file, 'r') as f:
                return json.load(f)
        return {}
    
    def get_simulation_results(self, simulation_id: str) -> Dict:
        """Load simulation backtest results"""
        sim_path = f"{self.myportolio_path}/simulations/backtests/{simulation_id}"
        results_file = f"{sim_path}/myportolio_results.json"
        
        if os.path.exists(results_file):
            with open(results_file, 'r') as f:
                return json.load(f)
        return {}
    
    def list_available_simulations(self) -> List[str]:
        """List all available backtest simulations"""
        simulations_path = f"{self.myportolio_path}/simulations/backtests"
        
        if os.path.exists(simulations_path):
            return [d for d in os.listdir(simulations_path) if os.path.isdir(f"{simulations_path}/{d}")]
        return []
    
    def get_all_backend_data(self, simulation_id: str = "Myportolio") -> Dict:
        """Get comprehensive backend data for validation"""
        
        backend_data = {
            'config': self.get_myportolio_config(),
            'risk_parameters': self.get_risk_parameters(),
            'status_report': self.get_latest_status_report(),
            'risk_report': self.get_latest_risk_report()
        }
        
        # Add simulation-specific data if not live portfolio
        if simulation_id != "Myportolio":
            backend_data['lean_config'] = self.get_simulation_config(simulation_id)
            backend_data['simulation_results'] = self.get_simulation_results(simulation_id)
        
        return backend_data


class ETHModelDataAccess:
    """Access ETH model performance data from SQLite database"""
    
    def __init__(self):
        self.db_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/eth_models/model_performance.db"
        
    def get_model_performance_data(self) -> Dict:
        """Get ETH model performance metrics"""
        
        if not os.path.exists(self.db_path):
            return {}
        
        performance_data = {}
        
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            # Get model performance metrics
            cursor.execute("""
                SELECT model_name, metric_name, metric_value, timestamp
                FROM model_performance 
                ORDER BY timestamp DESC
            """)
            
            performance_records = cursor.fetchall()
            
            for model_name, metric_name, metric_value, timestamp in performance_records:
                if model_name not in performance_data:
                    performance_data[model_name] = {}
                
                performance_data[model_name][metric_name] = {
                    'value': metric_value,
                    'timestamp': timestamp
                }
            
            conn.close()
            
        except Exception as e:
            print(f"Error accessing ETH model database: {e}")
        
        return performance_data
    
    def get_latest_model_predictions(self) -> Dict:
        """Get latest ETH model predictions"""
        
        if not os.path.exists(self.db_path):
            return {}
        
        predictions = {}
        
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            # Get latest predictions
            cursor.execute("""
                SELECT model_name, prediction_type, predicted_value, confidence, timestamp
                FROM model_predictions 
                WHERE timestamp = (
                    SELECT MAX(timestamp) FROM model_predictions
                )
            """)
            
            prediction_records = cursor.fetchall()
            
            for model_name, pred_type, pred_value, confidence, timestamp in prediction_records:
                if model_name not in predictions:
                    predictions[model_name] = {}
                
                predictions[model_name][pred_type] = {
                    'predicted_value': pred_value,
                    'confidence': confidence,
                    'timestamp': timestamp
                }
            
            conn.close()
            
        except Exception as e:
            print(f"Error accessing predictions from ETH model database: {e}")
        
        return predictions


class ValidationHelpers:
    """Utility functions for data validation"""
    
    @staticmethod
    def compare_values(frontend_value: Any, backend_value: Any, tolerance: float = 0.01) -> Dict:
        """Compare frontend and backend values with tolerance"""
        
        result = {
            'match_status': 'unknown',
            'frontend_value': str(frontend_value),
            'backend_value': str(backend_value),
            'difference': None
        }
        
        # Handle string comparison
        if isinstance(frontend_value, str) and isinstance(backend_value, str):
            if frontend_value.strip() == backend_value.strip():
                result['match_status'] = 'exact'
            else:
                result['match_status'] = 'different'
            return result
        
        # Handle numeric comparison
        try:
            front_num = float(str(frontend_value).replace(',', '').replace('$', '').replace('%', ''))
            back_num = float(str(backend_value).replace(',', '').replace('$', '').replace('%', ''))
            
            difference = abs(front_num - back_num)
            result['difference'] = difference
            
            if difference <= tolerance:
                result['match_status'] = 'exact'
            elif difference / max(abs(front_num), abs(back_num), 1) <= tolerance:
                result['match_status'] = 'within_tolerance'
            else:
                result['match_status'] = 'different'
                
        except (ValueError, TypeError):
            # Non-numeric comparison
            if str(frontend_value) == str(backend_value):
                result['match_status'] = 'exact'
            else:
                result['match_status'] = 'different'
        
        return result
    
    @staticmethod
    def extract_numeric_value(text_value: str) -> Optional[float]:
        """Extract numeric value from formatted text"""
        
        if not text_value:
            return None
        
        # Remove common formatting characters
        cleaned = str(text_value).replace(',', '').replace('$', '').replace('%', '').strip()
        
        try:
            return float(cleaned)
        except (ValueError, TypeError):
            return None
    
    @staticmethod
    def format_percentage(decimal_value: float) -> str:
        """Format decimal as percentage string"""
        return f"{decimal_value * 100:.2f}%"
    
    @staticmethod
    def format_currency(numeric_value: float) -> str:
        """Format numeric value as currency string"""
        return f"${numeric_value:,.2f}"
    
    @staticmethod
    def calculate_allocation_percentage(asset_value: float, total_value: float) -> float:
        """Calculate allocation percentage"""
        if total_value == 0:
            return 0.0
        return (asset_value / total_value) * 100


class DataMappingRules:
    """Defines mapping rules between frontend and backend data"""
    
    def __init__(self):
        self.validation_helpers = ValidationHelpers()
    
    def get_portfolio_overview_mappings(self) -> List[Dict]:
        """Get mapping rules for portfolio overview page"""
        
        return [
            {
                'frontend_key': 'portfolio_value',
                'backend_path': ['config', 'target_portfolio_value'],
                'data_source': 'config',
                'validation_type': 'currency',
                'description': 'Total portfolio value'
            },
            {
                'frontend_key': 'asset_eth_percent',
                'backend_path': ['config', 'assets', 'ETH', 'allocation_percent'],
                'data_source': 'config',
                'validation_type': 'percentage',
                'description': 'ETH allocation percentage'
            },
            {
                'frontend_key': 'asset_btc_percent',
                'backend_path': ['config', 'assets', 'BTC', 'allocation_percent'],
                'data_source': 'config',
                'validation_type': 'percentage',
                'description': 'BTC allocation percentage'
            },
            {
                'frontend_key': 'risk_profile',
                'backend_path': ['risk_parameters', 'risk_profile'],
                'data_source': 'risk_parameters',
                'validation_type': 'string',
                'description': 'Risk profile setting'
            },
            {
                'frontend_key': 'target_volatility',
                'backend_path': ['risk_parameters', 'max_portfolio_volatility'],
                'data_source': 'risk_parameters',
                'validation_type': 'percentage',
                'description': 'Target portfolio volatility'
            }
        ]
    
    def get_simulation_parameter_mappings(self) -> List[Dict]:
        """Get mapping rules for simulation parameters"""
        
        return [
            {
                'frontend_key': 'sim_date',
                'backend_path': ['lean_config', 'start-date'],
                'data_source': 'lean_config',
                'validation_type': 'string',
                'description': 'Simulation start date'
            },
            {
                'frontend_key': 'sim_algorithm_type_name',
                'backend_path': ['lean_config', 'algorithm-type-name'],
                'data_source': 'lean_config',
                'validation_type': 'string',
                'description': 'Algorithm type name'
            },
            {
                'frontend_key': 'sim_algorithm_location',
                'backend_path': ['lean_config', 'algorithm-location'],
                'data_source': 'lean_config',
                'validation_type': 'string',
                'description': 'Algorithm file location'
            }
        ]
    
    def get_performance_mappings(self) -> List[Dict]:
        """Get mapping rules for performance metrics"""
        
        return [
            {
                'frontend_key': 'perf_total_return',
                'backend_path': ['simulation_results', 'Statistics', 'Total Performance', 'Total Return'],
                'data_source': 'simulation_results',
                'validation_type': 'percentage',
                'description': 'Total return performance'
            },
            {
                'frontend_key': 'perf_sharpe_ratio',
                'backend_path': ['simulation_results', 'Statistics', 'Risk', 'Sharpe Ratio'],
                'data_source': 'simulation_results',
                'validation_type': 'numeric',
                'description': 'Sharpe ratio'
            },
            {
                'frontend_key': 'perf_max_drawdown',
                'backend_path': ['simulation_results', 'Statistics', 'Risk', 'Maximum Drawdown'],
                'data_source': 'simulation_results',
                'validation_type': 'percentage',
                'description': 'Maximum drawdown'
            }
        ]
    
    def get_all_mapping_rules(self) -> Dict[str, List[Dict]]:
        """Get all mapping rules organized by page type"""
        
        return {
            'portfolio_overview': self.get_portfolio_overview_mappings(),
            'simulation_parameters': self.get_simulation_parameter_mappings(),
            'performance': self.get_performance_mappings(),
            'holdings': [],  # To be expanded
            'algorithms': [],  # To be expanded
            'algorithm_performance': [],  # To be expanded
            'backtest_results': []  # To be expanded
        }


def validate_single_data_point(frontend_value: Any, backend_data: Dict, mapping_rule: Dict) -> Dict:
    """Validate a single data point against backend data"""
    
    validation_result = {
        'frontend_value': str(frontend_value),
        'backend_value': None,
        'backend_source': mapping_rule['data_source'],
        'backend_path': mapping_rule['backend_path'],
        'validation_type': mapping_rule['validation_type'],
        'match_status': 'not_found',
        'description': mapping_rule['description']
    }
    
    # Extract backend value using path
    backend_value = extract_nested_value(backend_data, mapping_rule['data_source'], mapping_rule['backend_path'])
    
    if backend_value is not None:
        validation_result['backend_value'] = str(backend_value)
        
        # Perform validation based on type
        comparison = ValidationHelpers.compare_values(frontend_value, backend_value)
        validation_result.update(comparison)
    
    return validation_result


def extract_nested_value(backend_data: Dict, data_source: str, path: List[str]) -> Any:
    """Extract nested value from backend data"""
    
    if data_source not in backend_data:
        return None
    
    current_data = backend_data[data_source]
    
    try:
        for key in path:
            if isinstance(current_data, dict) and key in current_data:
                current_data = current_data[key]
            else:
                return None
        
        return current_data
    except (KeyError, TypeError):
        return None
