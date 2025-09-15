#!/usr/bin/env python3
"""
Comprehensive Simulation Runner
Uses the organized core trading systems for various simulation scenarios
"""

import sys
import os
import logging
from datetime import datetime
from pathlib import Path

# Add project paths for organized structure
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn')
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')

class SimulationRunner:
    """Manages and executes various trading simulations"""
    
    def __init__(self):
        self.simulation_options = {
            '1': {
                'name': 'Core Ensemble Portfolio Simulation',
                'file': 'core/simplified_ensemble_portfolio.py',
                'description': 'Primary ensemble trading system with Kelly Criterion + ML'
            },
            '2': {
                'name': 'Live ETH Kelly Portfolio Simulation', 
                'file': 'core/live_eth_kelly_portfolio.py',
                'description': 'Kelly Criterion portfolio optimization for ETH'
            },
            '3': {
                'name': 'Dual Crypto Portfolio Simulation',
                'file': 'core/dual_crypto_portfolio_manager.py', 
                'description': 'BTC/ETH dual crypto portfolio management'
            },
            '4': {
                'name': 'Comprehensive Backtesting Suite',
                'file': 'backtesting/comprehensive_backtesting_suite.py',
                'description': 'Multi-strategy backtesting with 5+ strategies'
            },
            '5': {
                'name': 'Robust Backtesting Suite',
                'file': 'backtesting/robust_backtesting_suite.py',
                'description': 'Robust strategy testing with pre-optimized parameters'
            },
            '6': {
                'name': 'Parameter Optimization Backtester',
                'file': 'backtesting/parameter_optimization_backtester.py',
                'description': 'Advanced parameter optimization engine'
            },
            '7': {
                'name': 'Integrated Six Position System',
                'file': 'core/integrated_six_position_system.py',
                'description': 'Multi-position trading framework'
            },
            '8': {
                'name': 'Live Market Data Feed Test',
                'file': 'core/live_market_data_feed.py',
                'description': 'Test real-time Coinbase API data feed'
            },
            '9': {
                'name': 'LEAN Backtesting Integration',
                'file': 'core/lean_backtesting_integration.py',
                'description': 'QuantConnect LEAN framework integration'
            }
        }
        
        # Setup logging
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(__name__)
    
    def display_options(self):
        """Display available simulation options"""
        print("🚀 UNICORN INVESTING - SIMULATION RUNNER")
        print("=" * 60)
        print("Available simulations from organized core systems:")
        print()
        
        for key, option in self.simulation_options.items():
            print(f"{key}. {option['name']}")
            print(f"   📁 {option['file']}")
            print(f"   📝 {option['description']}")
            print()
    
    def run_simulation(self, choice: str):
        """Execute selected simulation"""
        if choice not in self.simulation_options:
            print(f"❌ Invalid choice: {choice}")
            return False
        
        option = self.simulation_options[choice]
        file_path = f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/{option['file']}"
        
        print(f"🎯 RUNNING: {option['name']}")
        print("=" * 60)
        print(f"📁 File: {option['file']}")
        print(f"📝 Description: {option['description']}")
        print()
        
        if not os.path.exists(file_path):
            print(f"❌ File not found: {file_path}")
            return False
        
        try:
            # Change to Myportolio directory for proper imports
            original_dir = os.getcwd()
            os.chdir('/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio')
            
            print(f"⚡ Executing simulation...")
            print("-" * 40)
            
            # Execute the Python file
            exec(open(file_path).read(), {'__name__': '__main__'})
            
            print()
            print("✅ Simulation completed successfully!")
            
            return True
            
        except Exception as e:
            print(f"❌ Simulation failed: {e}")
            self.logger.error(f"Simulation error: {e}")
            return False
        
        finally:
            os.chdir(original_dir)
    
    def run_all_core_simulations(self):
        """Run all core trading simulations"""
        core_simulations = ['1', '2', '3', '7', '8']  # Core trading systems
        
        print("🌟 RUNNING ALL CORE TRADING SIMULATIONS")
        print("=" * 60)
        
        results = {}
        for choice in core_simulations:
            print(f"\n📊 Starting simulation {choice}...")
            success = self.run_simulation(choice)
            results[choice] = success
            
            if success:
                print(f"✅ Simulation {choice} completed")
            else:
                print(f"❌ Simulation {choice} failed")
            
            print("-" * 40)
        
        # Summary
        successful = sum(results.values())
        total = len(results)
        
        print(f"\n🏆 CORE SIMULATIONS SUMMARY")
        print("=" * 40)
        print(f"✅ Successful: {successful}/{total}")
        print(f"❌ Failed: {total - successful}/{total}")
        print(f"📊 Success Rate: {successful/total*100:.1f}%")
        
        return results
    
    def run_all_backtesting_simulations(self):
        """Run all backtesting simulations"""
        backtesting_simulations = ['4', '5', '6', '9']  # Backtesting systems
        
        print("🧪 RUNNING ALL BACKTESTING SIMULATIONS")
        print("=" * 60)
        
        results = {}
        for choice in backtesting_simulations:
            print(f"\n📊 Starting backtesting {choice}...")
            success = self.run_simulation(choice)
            results[choice] = success
            
            if success:
                print(f"✅ Backtesting {choice} completed")
            else:
                print(f"❌ Backtesting {choice} failed")
            
            print("-" * 40)
        
        # Summary
        successful = sum(results.values())
        total = len(results)
        
        print(f"\n🏆 BACKTESTING SUMMARY")
        print("=" * 40)
        print(f"✅ Successful: {successful}/{total}")
        print(f"❌ Failed: {total - successful}/{total}")
        print(f"📊 Success Rate: {successful/total*100:.1f}%")
        
        return results
    
    def interactive_mode(self):
        """Run in interactive mode"""
        while True:
            self.display_options()
            
            print("Special options:")
            print("A. Run all core trading simulations")
            print("B. Run all backtesting simulations") 
            print("Q. Quit")
            print()
            
            choice = input("Select simulation (1-9, A, B, Q): ").strip().upper()
            
            if choice == 'Q':
                print("👋 Goodbye!")
                break
            elif choice == 'A':
                self.run_all_core_simulations()
            elif choice == 'B':
                self.run_all_backtesting_simulations()
            elif choice in self.simulation_options:
                self.run_simulation(choice)
            else:
                print(f"❌ Invalid choice: {choice}")
            
            print("\n" + "="*60 + "\n")

def main():
    """Main simulation runner"""
    runner = SimulationRunner()
    
    print("🎯 Welcome to the Unicorn Investing Simulation Runner!")
    print("Using organized core systems from clean directory structure.")
    print()
    
    # Check if command line argument provided
    if len(sys.argv) > 1:
        choice = sys.argv[1].upper()
        
        if choice == 'ALL-CORE':
            runner.run_all_core_simulations()
        elif choice == 'ALL-BACKTESTING':
            runner.run_all_backtesting_simulations()
        elif choice in runner.simulation_options:
            runner.run_simulation(choice)
        else:
            print(f"❌ Invalid command line option: {choice}")
            print("Usage: python simulation_runner.py [1-9|ALL-CORE|ALL-BACKTESTING]")
    else:
        # Interactive mode
        runner.interactive_mode()

if __name__ == "__main__":
    main()