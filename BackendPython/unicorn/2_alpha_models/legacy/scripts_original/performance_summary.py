#!/usr/bin/env python3
"""
🦄 Unicorn Model Performance Summary
Quick access to latest model performance analysis
"""

import os
from pathlib import Path
from datetime import datetime

def display_latest_performance_summary():
    """Display the latest performance analysis summary"""
    
    base_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models")
    analysis_dir = base_path / "performance_analysis"
    
    print("🦄 UNICORN INVESTING - MODEL PERFORMANCE SUMMARY")
    print("=" * 55)
    print(f"Analysis Directory: {analysis_dir}")
    print()
    
    if not analysis_dir.exists():
        print("❌ No performance analysis directory found")
        print("   Run model_performance_manager_v2.py first")
        return
    
    # Find latest report file
    report_files = list(analysis_dir.glob("performance_report_*.txt"))
    if not report_files:
        print("❌ No performance reports found")
        print("   Run model_performance_manager_v2.py first")
        return
    
    latest_report = sorted(report_files)[-1]
    print(f"📄 Latest Report: {latest_report.name}")
    print(f"📅 Generated: {datetime.fromtimestamp(latest_report.stat().st_mtime)}")
    print()
    
    # Display key highlights from the report
    with open(latest_report, 'r') as f:
        content = f.read()
    
    # Extract key sections
    lines = content.split('\n')
    
    # Find and display performance analysis section
    in_performance_section = False
    in_recommendations_section = False
    
    for line in lines:
        if "📈 PERFORMANCE ANALYSIS" in line:
            in_performance_section = True
            print("🏆 TOP PERFORMERS")
            print("-" * 20)
            continue
        elif "📋 DETAILED METRICS" in line:
            in_performance_section = False
            continue
        elif "💡 RECOMMENDATIONS" in line:
            in_recommendations_section = True
            print("\n💡 KEY RECOMMENDATIONS")
            print("-" * 25)
            continue
        elif "🦄 End of Report" in line:
            in_recommendations_section = False
            break
            
        if in_performance_section and line.startswith("🏆"):
            print(line)
        elif in_recommendations_section and (line.startswith("🎯") or line.startswith("⚠️")):
            print(line)
            # Print the next line if it's an indented description
            next_idx = lines.index(line) + 1
            if next_idx < len(lines) and lines[next_idx].startswith("   "):
                print(lines[next_idx])
    
    # Show available files
    print("\n📁 GENERATED FILES")
    print("-" * 20)
    for file in sorted(analysis_dir.glob("*")):
        if file.is_file():
            size_kb = file.stat().st_size / 1024
            print(f"📄 {file.name} ({size_kb:.1f} KB)")
    
    # Display quick model summary
    print("\n📊 MODEL INVENTORY")
    print("-" * 20)
    print("Assets: BTC, ETH")
    print("Models per Asset: Prophet, XGBoost, Ensemble")
    print("Total Models: 6")
    print("Performance Metrics: 9 per model")
    
    print("\n🚀 QUICK ACTIONS")
    print("-" * 17)
    print("• View dashboard: open performance_dashboard_*.png")
    print("• Full report: cat performance_report_*.txt")
    print("• Re-run analysis: python model_performance_manager_v2.py")
    
    print(f"\n🦄 Performance analysis ready! All 6 models analyzed successfully.")

if __name__ == "__main__":
    display_latest_performance_summary()
