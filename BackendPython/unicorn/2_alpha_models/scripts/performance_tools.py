#!/usr/bin/env python3
"""
🦄 Unicorn Model Performance Tools
Unified access to performance analysis and monitoring tools
"""

import sys
import os
from pathlib import Path

def show_menu():
    """Display the performance tools menu"""
    print("🦄 UNICORN MODEL PERFORMANCE TOOLS")
    print("=" * 40)
    print("1. 📊 Run Full Performance Analysis")
    print("2. 📈 View Performance Summary")
    print("3. 📁 List Generated Reports")
    print("4. 🧹 Clean Old Reports")
    print("5. ❓ Help")
    print("0. 🚪 Exit")
    print()

def run_full_analysis():
    """Run the complete performance analysis"""
    print("🚀 Running Full Performance Analysis...")
    os.system("python /workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/scripts/model_performance_manager_v2.py")

def show_summary():
    """Show the performance summary"""
    print("📈 Loading Performance Summary...")
    os.system("python /workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/scripts/performance_summary.py")

def list_reports():
    """List all generated performance reports"""
    analysis_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/performance_analysis")
    
    print("📁 GENERATED PERFORMANCE REPORTS")
    print("-" * 35)
    
    if not analysis_dir.exists():
        print("❌ No reports directory found")
        return
    
    files = list(analysis_dir.glob("*"))
    if not files:
        print("📄 No reports found")
        return
    
    for i, file in enumerate(sorted(files), 1):
        size_kb = file.stat().st_size / 1024
        file_type = "🖼️" if file.suffix == '.png' else "📄"
        print(f"{i:2d}. {file_type} {file.name} ({size_kb:.1f} KB)")

def clean_old_reports():
    """Clean old performance reports"""
    analysis_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/performance_analysis")
    
    if not analysis_dir.exists():
        print("❌ No reports directory found")
        return
    
    files = list(analysis_dir.glob("*"))
    
    if len(files) <= 2:  # Keep at least latest dashboard and report
        print("🛡️ Only recent files found, nothing to clean")
        return
    
    # Sort by modification time, keep latest 2 of each type
    dashboards = sorted([f for f in files if 'dashboard' in f.name], key=lambda x: x.stat().st_mtime, reverse=True)
    reports = sorted([f for f in files if 'report' in f.name], key=lambda x: x.stat().st_mtime, reverse=True)
    
    to_remove = []
    if len(dashboards) > 2:
        to_remove.extend(dashboards[2:])
    if len(reports) > 2:
        to_remove.extend(reports[2:])
    
    if not to_remove:
        print("🛡️ No old files to clean")
        return
    
    print(f"🧹 Cleaning {len(to_remove)} old files...")
    for file in to_remove:
        file.unlink()
        print(f"   🗑️ Removed {file.name}")
    
    print("✅ Cleanup complete")

def show_help():
    """Display help information"""
    print("🦄 UNICORN MODEL PERFORMANCE TOOLS - HELP")
    print("=" * 45)
    print()
    print("📊 ANALYSIS TOOLS:")
    print("   Full Analysis: Comprehensive performance analysis of all models")
    print("   - Discovers all trained models")
    print("   - Calculates 9 performance metrics per model")
    print("   - Generates visualization dashboard")
    print("   - Creates detailed text report")
    print()
    print("📈 MONITORING TOOLS:")
    print("   Performance Summary: Quick view of latest analysis results")
    print("   - Shows top performers")
    print("   - Displays key recommendations")
    print("   - Lists available files")
    print()
    print("📁 FILE MANAGEMENT:")
    print("   List Reports: View all generated performance files")
    print("   Clean Reports: Remove old analysis files (keeps latest 2)")
    print()
    print("🎯 PERFORMANCE METRICS:")
    print("   • Accuracy: MSE, MAE, RMSE, MAPE, R²")
    print("   • Trading: Directional Accuracy, Sharpe Ratio, Information Ratio")
    print("   • Risk: Maximum Drawdown")
    print()
    print("📂 OUTPUT LOCATIONS:")
    print("   All files saved to: performance_analysis/")
    print("   Dashboard: performance_dashboard_YYYYMMDD_HHMMSS.png")
    print("   Report: performance_report_YYYYMMDD_HHMMSS.txt")
    print()
    print("🚀 CURRENT STATUS:")
    print("   • 6 models discovered (BTC & ETH: Prophet, XGBoost, Ensemble)")
    print("   • All models analyzed successfully")
    print("   • ETH-Ensemble recommended for production")

def main():
    """Main menu loop"""
    while True:
        show_menu()
        
        try:
            choice = input("🎯 Select option (0-5): ").strip()
            print()
            
            if choice == '0':
                print("👋 Goodbye!")
                break
            elif choice == '1':
                run_full_analysis()
            elif choice == '2':
                show_summary()
            elif choice == '3':
                list_reports()
            elif choice == '4':
                clean_old_reports()
            elif choice == '5':
                show_help()
            else:
                print("❌ Invalid option. Please choose 0-5.")
            
            print("\n" + "="*50)
            input("Press Enter to continue...")
            print()
            
        except KeyboardInterrupt:
            print("\n\n👋 Goodbye!")
            break
        except Exception as e:
            print(f"❌ Error: {e}")
            print("Please try again.")

if __name__ == "__main__":
    main()
