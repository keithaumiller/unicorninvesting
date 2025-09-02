#!/usr/bin/env python3
"""
Risk Monitoring Dashboard
Real-time risk monitoring and alerting system for Myportolio
"""

import json
import sys
import os
import time
from datetime import datetime, timedelta
import pandas as pd
import numpy as np

# Add paths for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)
sys.path.append(os.path.dirname(current_dir))

from comprehensive_risk_manager import ComprehensiveRiskManager

def load_portfolio_data():
    """Load current portfolio data from IBKR"""
    
    try:
        # Try to get live IBKR data (placeholder - would integrate with actual IBKR connector)
        # For now, use mock data representing current state (100% cash)
        pass
    except Exception as e:
        print(f"⚠️  Could not load live IBKR data: {e}")
    
    # Use current portfolio state from status check
    return {
        'total_value': 1000.0,
        'cash': 1000.0,  # Currently 100% cash per status check
        'positions': {},  # No positions currently
        'market_values': {},
        'timestamp': datetime.now()
    }

def print_risk_dashboard(risk_assessment: dict):
    """Print formatted risk dashboard"""
    
    print("\n" + "="*80)
    print("🦄 MYPORTOLIO RISK MONITORING DASHBOARD")
    print("="*80)
    print(f"📅 Assessment Time: {risk_assessment['timestamp'].strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"🎯 Overall Risk Score: {risk_assessment['overall_risk_score']:.1f}/10")
    
    # Risk Metrics Section
    print(f"\n📊 CURRENT RISK METRICS")
    print("-" * 40)
    metrics = risk_assessment['risk_metrics']
    print(f"📉 Current Drawdown: {metrics.get('current_drawdown', 0):.1%}")
    print(f"📊 Portfolio Volatility: {metrics.get('portfolio_volatility', 0):.1%}")
    print(f"⚡ 1-Day VaR: {metrics.get('var_1day', 0):.1%}")
    print(f"📅 1-Week VaR: {metrics.get('var_1week', 0):.1%}")
    print(f"📈 Sharpe Ratio: {metrics.get('sharpe_ratio', 0):.2f}")
    print(f"🏋️  Max Position Weight: {metrics.get('max_position_weight', 0):.1%}")
    
    # Risk Violations
    violations = risk_assessment['risk_violations']
    if violations:
        print(f"\n🚨 RISK VIOLATIONS ({len(violations)})")
        print("-" * 40)
        for violation in violations:
            severity_emoji = {"critical": "🔴", "high": "🟠", "medium": "🟡", "low": "🟢"}
            emoji = severity_emoji.get(violation['severity'], "⚪")
            print(f"{emoji} {violation['description']}")
    else:
        print(f"\n✅ NO RISK VIOLATIONS")
        print("-" * 40)
        print("All risk metrics are within acceptable limits")
    
    # Emergency Assessment
    emergency = risk_assessment['emergency_assessment']
    if emergency['emergency_stop_recommended']:
        print(f"\n🚨 EMERGENCY STOP RECOMMENDED")
        print("-" * 40)
        print(f"Emergency Score: {emergency['emergency_score']}/5")
        print("Triggers:", ", ".join(emergency['emergency_triggers']))
        print("⚠️  IMMEDIATE ACTION REQUIRED")
    else:
        print(f"\n✅ NO EMERGENCY CONDITIONS")
        print("-" * 40)
        print(f"Emergency Score: {emergency['emergency_score']}/5 (Safe)")
    
    # Position Recommendations
    print(f"\n🎯 POSITION RECOMMENDATIONS")
    print("-" * 40)
    for asset, rec in risk_assessment['position_recommendations'].items():
        action_emoji = {"BUY": "🟢", "SELL": "🔴", "HOLD": "🟡"}
        emoji = action_emoji.get(rec['recommended_action'], "⚪")
        print(f"{emoji} {asset}: {rec['recommended_action']}")
        print(f"   Target: ${rec['risk_adjusted_value']:.2f} ({rec['target_allocation']:.1%})")
        print(f"   Current: ${rec['current_value']:.2f}")
    
    # Alerts
    alerts = risk_assessment['risk_alerts']
    if alerts:
        print(f"\n🔔 ACTIVE ALERTS ({len(alerts)})")
        print("-" * 40)
        for alert in alerts[-5:]:  # Show last 5 alerts
            severity_emoji = {"critical": "🔴", "high": "🟠", "medium": "🟡", "low": "🟢"}
            emoji = severity_emoji.get(alert['severity'], "⚪")
            print(f"{emoji} {alert['message']}")
    else:
        print(f"\n✅ NO ACTIVE ALERTS")
        print("-" * 40)
        print("System operating normally")
    
    # Recommendations
    print(f"\n💡 RECOMMENDATIONS")
    print("-" * 40)
    for i, rec in enumerate(risk_assessment['recommended_actions'], 1):
        print(f"{i}. {rec}")

def generate_risk_report(risk_manager: ComprehensiveRiskManager, portfolio_data: dict):
    """Generate detailed risk report"""
    
    # Perform comprehensive risk assessment
    risk_assessment = risk_manager.add_risk_check(portfolio_data)
    
    # Print dashboard
    print_risk_dashboard(risk_assessment)
    
    # Save detailed report
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    report_path = f"/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/risk_report_{timestamp}.json"
    
    # Make risk assessment JSON serializable
    serializable_assessment = json.loads(json.dumps(risk_assessment, default=str))
    
    with open(report_path, 'w') as f:
        json.dump(serializable_assessment, f, indent=2, default=str)
    
    print(f"\n💾 Detailed report saved: {report_path}")
    
    return risk_assessment

def monitor_risk_continuously():
    """Continuous risk monitoring mode"""
    
    print("🔄 Starting continuous risk monitoring...")
    print("Press Ctrl+C to stop")
    
    risk_manager = ComprehensiveRiskManager()
    
    try:
        while True:
            portfolio_data = load_portfolio_data()
            risk_assessment = generate_risk_report(risk_manager, portfolio_data)
            
            # Check for emergency conditions
            if risk_assessment['emergency_assessment']['emergency_stop_recommended']:
                print("\n🚨 EMERGENCY STOP TRIGGERED - STOPPING MONITORING")
                break
            
            print(f"\n⏰ Next check in 60 seconds...")
            time.sleep(60)
            
    except KeyboardInterrupt:
        print("\n⏹️  Risk monitoring stopped by user")

def main():
    """Main risk monitoring function"""
    
    import argparse
    
    parser = argparse.ArgumentParser(description="Myportolio Risk Monitoring Dashboard")
    parser.add_argument("--continuous", action="store_true", help="Run continuous monitoring")
    parser.add_argument("--detailed", action="store_true", help="Show detailed analysis")
    
    args = parser.parse_args()
    
    # Initialize risk manager
    risk_manager = ComprehensiveRiskManager()
    
    if args.continuous:
        monitor_risk_continuously()
    else:
        # Single assessment
        portfolio_data = load_portfolio_data()
        risk_assessment = generate_risk_report(risk_manager, portfolio_data)
        
        if args.detailed:
            print(f"\n📋 DETAILED RISK ANALYSIS")
            print("="*60)
            
            # Risk manager state
            risk_summary = risk_manager.get_risk_summary()
            print(f"System Status: {risk_summary['system_status']}")
            print(f"Active Alerts: {risk_summary['active_alerts']}")
            
            # Historical analysis if available
            if len(risk_manager.risk_metrics_history) > 0:
                print(f"\nRisk History: {len(risk_manager.risk_metrics_history)} assessments")
                
                # Calculate risk trends
                recent_scores = [a['overall_risk_score'] for a in risk_manager.risk_metrics_history[-10:]]
                if len(recent_scores) > 1:
                    avg_score = np.mean(recent_scores)
                    trend = "increasing" if recent_scores[-1] > avg_score else "decreasing"
                    print(f"Risk Trend: {trend} (avg: {avg_score:.1f})")
        
        print(f"\n🦄 Risk assessment complete")

if __name__ == "__main__":
    main()
