#!/usr/bin/env python3
"""
🦄 Portfolio Configuration Manager
Loads and manages portfolio configurations from JSON files
"""

import json
import os
from dataclasses import dataclass
from typing import Dict, List, Optional, Any
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

@dataclass
class AssetConfig:
    """Configuration for a single asset in portfolio"""
    symbol: str
    allocation_percent: float
    asset_type: str
    data_source: str
    model_type: str
    contract_id: Optional[str] = None
    exchange: str = "SMART"

@dataclass
class PortfolioConfig:
    """Complete portfolio configuration"""
    portfolio_name: str
    description: str
    strategy_type: str
    assets: Dict[str, AssetConfig]
    total_allocation: float
    currency: str
    rebalancing_frequency: str
    minimum_trade_size: float
    target_volatility: float
    created_date: str
    last_updated: str
    status: str

@dataclass
class RiskParameters:
    """Risk management parameters for portfolio"""
    portfolio_name: str
    risk_profile: str
    max_portfolio_volatility: float
    max_single_asset_weight: float
    var_limit_1day: float
    var_limit_1week: float
    max_drawdown: float
    sharpe_ratio_target: float
    risk_budget_allocation: Dict[str, float]
    correlation_limits: Dict[str, float]
    volatility_targets: Dict[str, float]
    position_limits: Dict[str, float]
    stop_loss_settings: Dict[str, Any]

@dataclass
class ExecutionSettings:
    """Execution parameters for portfolio"""
    portfolio_name: str
    execution_method: str
    order_type: str
    slippage_tolerance: float
    order_size_limit: float
    trading_session: Dict[str, str]
    broker_settings: Dict[str, str]
    order_execution: Dict[str, Any]
    price_improvement: Dict[str, Any]
    market_impact: Dict[str, Any]
    emergency_settings: Dict[str, Any]
    monitoring: Dict[str, bool]

class PortfolioConfigManager:
    """
    🎯 Portfolio Configuration Manager
    Loads and validates portfolio configurations from JSON files
    """
    
    def __init__(self, base_path: str = None):
        """
        Initialize portfolio configuration manager
        
        Args:
            base_path: Base path to portfolios directory
        """
        if base_path is None:
            # Default to the portfolios directory
            self.base_path = Path(__file__).parent / "portfolios"
        else:
            self.base_path = Path(base_path)
        
        self.portfolios_cache = {}
        logger.info(f"🎯 Portfolio Config Manager initialized: {self.base_path}")
    
    def list_available_portfolios(self) -> List[str]:
        """List all available portfolio configurations"""
        portfolios = []
        
        if not self.base_path.exists():
            logger.warning(f"⚠️ Portfolios directory not found: {self.base_path}")
            return portfolios
        
        for item in self.base_path.iterdir():
            if item.is_dir() and item.name not in ['templates', '__pycache__']:
                config_file = item / "config.json"
                if config_file.exists():
                    portfolios.append(item.name)
        
        logger.info(f"📋 Found {len(portfolios)} portfolio configurations")
        return portfolios
    
    def load_portfolio_config(self, portfolio_name: str) -> PortfolioConfig:
        """Load portfolio configuration from JSON file"""
        config_path = self.base_path / portfolio_name / "config.json"
        
        if not config_path.exists():
            raise FileNotFoundError(f"Portfolio config not found: {config_path}")
        
        try:
            with open(config_path, 'r') as f:
                data = json.load(f)
            
            # Convert assets to AssetConfig objects
            assets = {}
            for symbol, asset_data in data.get('assets', {}).items():
                assets[symbol] = AssetConfig(
                    symbol=symbol,
                    allocation_percent=asset_data['allocation_percent'],
                    asset_type=asset_data['asset_type'],
                    data_source=asset_data['data_source'],
                    model_type=asset_data['model_type'],
                    contract_id=asset_data.get('contract_id'),
                    exchange=asset_data.get('exchange', 'SMART')
                )
            
            config = PortfolioConfig(
                portfolio_name=data['portfolio_name'],
                description=data['description'],
                strategy_type=data['strategy_type'],
                assets=assets,
                total_allocation=data['total_allocation'],
                currency=data['currency'],
                rebalancing_frequency=data['rebalancing_frequency'],
                minimum_trade_size=data['minimum_trade_size'],
                target_volatility=data['target_volatility'],
                created_date=data['created_date'],
                last_updated=data['last_updated'],
                status=data['status']
            )
            
            logger.info(f"✅ Loaded portfolio config: {portfolio_name}")
            return config
            
        except Exception as e:
            logger.error(f"❌ Failed to load portfolio config {portfolio_name}: {e}")
            raise
    
    def load_risk_parameters(self, portfolio_name: str) -> RiskParameters:
        """Load risk parameters from JSON file"""
        risk_path = self.base_path / portfolio_name / "risk_parameters.json"
        
        if not risk_path.exists():
            raise FileNotFoundError(f"Risk parameters not found: {risk_path}")
        
        try:
            with open(risk_path, 'r') as f:
                data = json.load(f)
            
            risk_params = RiskParameters(
                portfolio_name=data['portfolio_name'],
                risk_profile=data['risk_profile'],
                max_portfolio_volatility=data['max_portfolio_volatility'],
                max_single_asset_weight=data['max_single_asset_weight'],
                var_limit_1day=data['var_limit_1day'],
                var_limit_1week=data['var_limit_1week'],
                max_drawdown=data['max_drawdown'],
                sharpe_ratio_target=data['sharpe_ratio_target'],
                risk_budget_allocation=data['risk_budget_allocation'],
                correlation_limits=data['correlation_limits'],
                volatility_targets=data['volatility_targets'],
                position_limits=data['position_limits'],
                stop_loss_settings=data['stop_loss_settings']
            )
            
            logger.info(f"✅ Loaded risk parameters: {portfolio_name}")
            return risk_params
            
        except Exception as e:
            logger.error(f"❌ Failed to load risk parameters {portfolio_name}: {e}")
            raise
    
    def load_execution_settings(self, portfolio_name: str) -> ExecutionSettings:
        """Load execution settings from JSON file"""
        exec_path = self.base_path / portfolio_name / "execution_settings.json"
        
        if not exec_path.exists():
            raise FileNotFoundError(f"Execution settings not found: {exec_path}")
        
        try:
            with open(exec_path, 'r') as f:
                data = json.load(f)
            
            exec_settings = ExecutionSettings(
                portfolio_name=data['portfolio_name'],
                execution_method=data['execution_method'],
                order_type=data['order_type'],
                slippage_tolerance=data['slippage_tolerance'],
                order_size_limit=data['order_size_limit'],
                trading_session=data['trading_session'],
                broker_settings=data['broker_settings'],
                order_execution=data['order_execution'],
                price_improvement=data['price_improvement'],
                market_impact=data['market_impact'],
                emergency_settings=data['emergency_settings'],
                monitoring=data['monitoring']
            )
            
            logger.info(f"✅ Loaded execution settings: {portfolio_name}")
            return exec_settings
            
        except Exception as e:
            logger.error(f"❌ Failed to load execution settings {portfolio_name}: {e}")
            raise
    
    def load_complete_portfolio(self, portfolio_name: str) -> Dict[str, Any]:
        """Load complete portfolio configuration (config + risk + execution)"""
        try:
            config = self.load_portfolio_config(portfolio_name)
            risk_params = self.load_risk_parameters(portfolio_name)
            execution_settings = self.load_execution_settings(portfolio_name)
            
            complete_portfolio = {
                'config': config,
                'risk_parameters': risk_params,
                'execution_settings': execution_settings
            }
            
            logger.info(f"🎯 Complete portfolio loaded: {portfolio_name}")
            return complete_portfolio
            
        except Exception as e:
            logger.error(f"❌ Failed to load complete portfolio {portfolio_name}: {e}")
            raise
    
    def validate_portfolio_config(self, portfolio_name: str) -> Dict[str, bool]:
        """Validate portfolio configuration for completeness and consistency"""
        validation_results = {
            'config_exists': False,
            'risk_params_exist': False,
            'execution_settings_exist': False,
            'allocation_sums_to_100': False,
            'risk_params_consistent': False,
            'all_assets_have_params': False
        }
        
        try:
            # Check if files exist
            portfolio_dir = self.base_path / portfolio_name
            validation_results['config_exists'] = (portfolio_dir / "config.json").exists()
            validation_results['risk_params_exist'] = (portfolio_dir / "risk_parameters.json").exists()
            validation_results['execution_settings_exist'] = (portfolio_dir / "execution_settings.json").exists()
            
            if validation_results['config_exists']:
                config = self.load_portfolio_config(portfolio_name)
                
                # Check allocation sums to 100%
                total_allocation = sum(asset.allocation_percent for asset in config.assets.values())
                validation_results['allocation_sums_to_100'] = abs(total_allocation - 100.0) < 0.01
                
                # Check if risk parameters are consistent
                if validation_results['risk_params_exist']:
                    risk_params = self.load_risk_parameters(portfolio_name)
                    
                    # Verify all assets have risk budget allocation
                    config_assets = set(config.assets.keys())
                    risk_assets = set(risk_params.risk_budget_allocation.keys())
                    validation_results['all_assets_have_params'] = config_assets.issubset(risk_assets)
                    
                    # Check consistency
                    validation_results['risk_params_consistent'] = (
                        config.target_volatility <= risk_params.max_portfolio_volatility
                    )
            
            # Overall validation
            all_valid = all(validation_results.values())
            logger.info(f"✅ Portfolio validation {portfolio_name}: {'PASSED' if all_valid else 'FAILED'}")
            
            return validation_results
            
        except Exception as e:
            logger.error(f"❌ Portfolio validation failed {portfolio_name}: {e}")
            return validation_results
    
    def get_portfolio_summary(self) -> Dict[str, Dict]:
        """Get summary of all available portfolios"""
        portfolios = self.list_available_portfolios()
        summary = {}
        
        for portfolio_name in portfolios:
            try:
                config = self.load_portfolio_config(portfolio_name)
                validation = self.validate_portfolio_config(portfolio_name)
                
                summary[portfolio_name] = {
                    'description': config.description,
                    'status': config.status,
                    'assets': list(config.assets.keys()),
                    'target_volatility': config.target_volatility,
                    'rebalancing_frequency': config.rebalancing_frequency,
                    'validation_passed': all(validation.values()),
                    'last_updated': config.last_updated
                }
                
            except Exception as e:
                summary[portfolio_name] = {
                    'error': str(e),
                    'validation_passed': False
                }
        
        return summary

# Example usage
def demo_portfolio_config_manager():
    """Demonstrate portfolio configuration management"""
    print("🎯 PORTFOLIO CONFIGURATION MANAGER DEMO")
    print("=" * 50)
    
    manager = PortfolioConfigManager()
    
    # List available portfolios
    portfolios = manager.list_available_portfolios()
    print(f"\n📋 Available Portfolios: {portfolios}")
    
    # Get portfolio summary
    summary = manager.get_portfolio_summary()
    print(f"\n📊 Portfolio Summary:")
    for name, info in summary.items():
        print(f"  {name}: {info.get('description', 'No description')}")
        print(f"    Status: {info.get('status', 'Unknown')}")
        print(f"    Valid: {'✅' if info.get('validation_passed') else '❌'}")
    
    # Load a specific portfolio if available
    if portfolios:
        portfolio_name = portfolios[0]
        try:
            complete_portfolio = manager.load_complete_portfolio(portfolio_name)
            print(f"\n✅ Successfully loaded complete portfolio: {portfolio_name}")
            
            config = complete_portfolio['config']
            print(f"   Assets: {list(config.assets.keys())}")
            print(f"   Target Volatility: {config.target_volatility:.1%}")
            
        except Exception as e:
            print(f"❌ Failed to load portfolio {portfolio_name}: {e}")

if __name__ == "__main__":
    demo_portfolio_config_manager()
