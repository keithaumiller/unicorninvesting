#!/usr/bin/env python3
"""
Myportolio Simulation Command Line Interface
===========================================

CLI tool for running and managing LEAN-integrated simulations for Myportolio.
Provides easy access to backtesting, paper trading, and optimization capabilities.

Usage:
    python simulation_cli.py backtest --start 2024-01-01 --end 2024-03-31
    python simulation_cli.py paper --duration 30
    python simulation_cli.py optimize --target sharpe --iterations 50
    python simulation_cli.py results --list
    python simulation_cli.py compare sim1 sim2 sim3

Author: Unicorn Investing Platform  
Date: September 3, 2025
"""

import argparse
import json
import sys
from datetime import datetime, timedelta
from pathlib import Path

# Import our simulation components
try:
    from lean_simulation_engine import LEANSimulationEngine
    from lean_result_handler import LEANResultHandler
except ImportError:
    # Add current directory to path for imports
    import os
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))
    from lean_simulation_engine import LEANSimulationEngine
    from lean_result_handler import LEANResultHandler

def load_template(template_name: str) -> dict:
    """Load simulation template from templates directory."""
    templates_path = Path(__file__).parent / "templates" / "simulation_templates.json"
    
    if not templates_path.exists():
        print(f"❌ Templates file not found: {templates_path}")
        return {}
    
    with open(templates_path, 'r') as f:
        templates = json.load(f)
    
    return templates.get(template_name, {})

def run_backtest(args):
    """Run historical backtest simulation."""
    print("🚀 Starting Historical Backtest Simulation")
    print("=" * 50)
    
    # Initialize simulation engine
    engine = LEANSimulationEngine()
    
    # Load template if specified
    template = {}
    if args.template:
        template = load_template(args.template)
        if not template:
            print(f"❌ Template '{args.template}' not found")
            return False
        print(f"📋 Using template: {template.get('name', args.template)}")
    
    # Prepare parameters
    parameters = template.get("parameters", {})
    if args.kelly:
        parameters["kelly_fraction"] = args.kelly
    if args.volatility:
        parameters["max_volatility"] = args.volatility
    
    # Set default dates if not provided
    start_date = args.start or "2024-01-01"
    end_date = args.end or "2024-03-31"
    
    print(f"📅 Period: {start_date} to {end_date}")
    print(f"⚙️  Parameters: {json.dumps(parameters, indent=2)}")
    
    try:
        # Run backtest
        simulation_id = engine.run_backtest(
            start_date=args.start,
            end_date=args.end,
            algorithm_name=template.get("algorithm", "MyportolioETHMomentum"),
            parameters=template.get("parameters", {}),
            template_name=args.template
        )
        
        print(f"✅ Backtest completed successfully!")
        print(f"📊 Simulation ID: {simulation_id}")
        print(f"📁 Results stored in: simulations/backtests/{simulation_id}/")
        
        return True
        
    except Exception as e:
        print(f"❌ Backtest failed: {str(e)}")
        return False

def run_paper_trading(args):
    """Run paper trading simulation."""
    print("📝 Starting Paper Trading Simulation")
    print("=" * 50)
    
    # This will be implemented in phase 2
    print("🚧 Paper trading simulation coming in Phase 2")
    print("📋 Template: paper_trading_template")
    print(f"⏱️  Duration: {args.duration} days")
    
    return True

def run_optimization(args):
    """Run parameter optimization."""
    print("🎯 Starting Parameter Optimization")
    print("=" * 50)
    
    # This will be implemented in phase 3
    print("🚧 Parameter optimization coming in Phase 3")
    print(f"🎯 Target: {args.target}")
    print(f"🔄 Iterations: {args.iterations}")
    
    return True

def list_results(args):
    """List simulation results."""
    print("📊 Simulation Results")
    print("=" * 50)
    
    handler = LEANResultHandler()
    simulations = handler.list_simulations(
        simulation_type=args.type,
        limit=args.limit
    )
    
    if not simulations:
        print("📭 No simulations found")
        return True
    
    print(f"Found {len(simulations)} simulation(s):\n")
    
    # Print header
    print(f"{'ID':<25} {'Type':<12} {'Return':<10} {'Sharpe':<8} {'Drawdown':<10} {'Status':<10}")
    print("-" * 80)
    
    # Print simulation data
    for sim in simulations:
        print(f"{sim.simulation_id[:24]:<25} "
              f"{sim.simulation_type:<12} "
              f"{sim.total_return:>8.2%} "
              f"{sim.sharpe_ratio:>7.2f} "
              f"{sim.max_drawdown:>9.2%} "
              f"{sim.status:<10}")
    
    return True

def compare_simulations(args):
    """Compare multiple simulations."""
    print("🔍 Comparing Simulations")
    print("=" * 50)
    
    if len(args.simulation_ids) < 2:
        print("❌ Please provide at least 2 simulation IDs to compare")
        return False
    
    handler = LEANResultHandler()
    comparison = handler.compare_simulations(args.simulation_ids)
    
    if "error" in comparison:
        print(f"❌ {comparison['error']}")
        return False
    
    print(f"📊 Comparing {comparison['simulations_compared']} simulations\n")
    
    # Best performers
    print("🏆 Best Performers:")
    print(f"  Best Return: {comparison['best_return']['simulation_id']} "
          f"({comparison['best_return']['value']:.2%})")
    print(f"  Best Sharpe: {comparison['best_sharpe']['simulation_id']} "
          f"({comparison['best_sharpe']['value']:.2f})")
    print(f"  Lowest Drawdown: {comparison['lowest_drawdown']['simulation_id']} "
          f"({comparison['lowest_drawdown']['value']:.2%})")
    
    # Summary statistics
    stats = comparison['summary_statistics']
    print(f"\n📈 Summary Statistics:")
    print(f"  Average Return: {stats['avg_return']:.2%}")
    print(f"  Average Sharpe: {stats['avg_sharpe']:.2f}")
    print(f"  Average Drawdown: {stats['avg_drawdown']:.2%}")
    print(f"  Total Trades: {stats['total_trades']}")
    
    return True

def show_report(args):
    """Show detailed simulation report."""
    print(f"📋 Detailed Report: {args.simulation_id}")
    print("=" * 50)
    
    handler = LEANResultHandler()
    report = handler.generate_performance_report(args.simulation_id)
    
    if "error" in report:
        print(f"❌ {report['error']}")
        return False
    
    # Summary
    summary = report['summary']
    print(f"📊 Summary:")
    print(f"  Type: {summary['type']}")
    print(f"  Date: {summary['timestamp']}")
    print(f"  Total Return: {summary['total_return']:.2%}")
    print(f"  Sharpe Ratio: {summary['sharpe_ratio']:.2f}")
    print(f"  Max Drawdown: {summary['max_drawdown']:.2%}")
    print(f"  Trades: {summary['trades_count']}")
    print(f"  Status: {summary['status']}")
    
    # Risk analysis
    if 'risk_analysis' in report:
        risk = report['risk_analysis']
        print(f"\n🛡️  Risk Analysis:")
        print(f"  Risk Score: {risk.get('risk_score', 0):.1f}/100")
        print(f"  Risk Category: {risk.get('risk_category', 'Unknown')}")
        print(f"  Sharpe Category: {risk.get('sharpe_category', 'Unknown')}")
    
    # Recommendations
    if 'recommendations' in report and report['recommendations']:
        print(f"\n💡 Recommendations:")
        for i, rec in enumerate(report['recommendations'], 1):
            print(f"  {i}. {rec}")
    
    return True

def list_templates():
    """List available simulation templates."""
    print("📋 Available Simulation Templates")
    print("=" * 50)
    
    templates_path = Path(__file__).parent / "templates" / "simulation_templates.json"
    
    if not templates_path.exists():
        print("❌ Templates file not found")
        return False
    
    with open(templates_path, 'r') as f:
        templates = json.load(f)
    
    for name, template in templates.items():
        print(f"\n🔧 {name}:")
        print(f"  Name: {template.get('name', 'Unknown')}")
        print(f"  Description: {template.get('description', 'No description')}")
        print(f"  Environment: {template.get('environment', 'Unknown')}")
        print(f"  Algorithm: {template.get('algorithm', 'Unknown')}")
    
    return True

def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Myportolio LEAN Simulation CLI",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run backtest with default parameters
  python simulation_cli.py backtest --start 2024-01-01 --end 2024-03-31
  
  # Run backtest with template
  python simulation_cli.py backtest --template backtest_template --start 2024-01-01 --end 2024-03-31
  
  # Run backtest with custom parameters
  python simulation_cli.py backtest --start 2024-01-01 --end 2024-03-31 --kelly 0.15 --volatility 0.20
  
  # List recent simulations
  python simulation_cli.py results --list --limit 10
  
  # Compare simulations
  python simulation_cli.py compare sim1 sim2 sim3
  
  # Show detailed report
  python simulation_cli.py report sim_id_here
  
  # List available templates
  python simulation_cli.py templates
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Backtest command
    backtest_parser = subparsers.add_parser('backtest', help='Run historical backtest')
    backtest_parser.add_argument('--start', type=str, help='Start date (YYYY-MM-DD)')
    backtest_parser.add_argument('--end', type=str, help='End date (YYYY-MM-DD)')
    backtest_parser.add_argument('--template', type=str, help='Template name to use')
    backtest_parser.add_argument('--kelly', type=float, help='Kelly fraction override')
    backtest_parser.add_argument('--volatility', type=float, help='Max volatility override')
    
    # Paper trading command
    paper_parser = subparsers.add_parser('paper', help='Run paper trading simulation')
    paper_parser.add_argument('--duration', type=int, default=30, help='Duration in days')
    paper_parser.add_argument('--template', type=str, default='paper_trading_template', help='Template name')
    
    # Optimization command
    optimize_parser = subparsers.add_parser('optimize', help='Run parameter optimization')
    optimize_parser.add_argument('--target', type=str, default='sharpe', help='Optimization target')
    optimize_parser.add_argument('--iterations', type=int, default=50, help='Number of iterations')
    optimize_parser.add_argument('--template', type=str, default='optimization_template', help='Template name')
    
    # Results command
    results_parser = subparsers.add_parser('results', help='Manage simulation results')
    results_parser.add_argument('--list', action='store_true', help='List simulations')
    results_parser.add_argument('--type', type=str, help='Filter by simulation type')
    results_parser.add_argument('--limit', type=int, default=20, help='Maximum results to show')
    
    # Compare command
    compare_parser = subparsers.add_parser('compare', help='Compare simulations')
    compare_parser.add_argument('simulation_ids', nargs='+', help='Simulation IDs to compare')
    
    # Report command
    report_parser = subparsers.add_parser('report', help='Show detailed simulation report')
    report_parser.add_argument('simulation_id', type=str, help='Simulation ID')
    
    # Templates command
    templates_parser = subparsers.add_parser('templates', help='List available templates')
    
    # Parse arguments
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    # Execute command
    success = True
    
    if args.command == 'backtest':
        success = run_backtest(args)
    elif args.command == 'paper':
        success = run_paper_trading(args)
    elif args.command == 'optimize':
        success = run_optimization(args)
    elif args.command == 'results':
        success = list_results(args)
    elif args.command == 'compare':
        success = compare_simulations(args)
    elif args.command == 'report':
        success = show_report(args)
    elif args.command == 'templates':
        success = list_templates()
    
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
