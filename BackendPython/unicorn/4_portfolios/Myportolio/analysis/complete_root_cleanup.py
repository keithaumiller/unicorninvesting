#!/usr/bin/env python3
"""
Complete Root Directory Cleanup
Move ALL files from root to appropriate subdirectories
"""

import shutil
from pathlib import Path

def organize_all_root_files():
    """Move all files from root to appropriate subdirectories"""
    
    root_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio")
    
    print("🧹 COMPLETE ROOT DIRECTORY CLEANUP")
    print("=" * 50)
    
    # File organization plan - where each file should go
    file_moves = {
        # Core trading systems → core/
        'simplified_ensemble_portfolio.py': 'core/',
        'live_market_data_feed.py': 'core/',
        'live_eth_kelly_portfolio.py': 'core/',
        'lean_backtesting_integration.py': 'core/',
        
        # Portfolio management systems → core/
        'dual_crypto_portfolio_manager.py': 'core/',
        'integrated_six_position_system.py': 'core/',
        'btc_model_manager.py': 'core/',
        
        # Integration and algorithm files → core/
        'eth_algorithm_integration.py': 'core/',
        'eth_kelly_integration.py': 'core/',
        
        # Backtesting systems → backtesting/
        'comprehensive_backtesting_suite.py': 'backtesting/',
        
        # Analysis tools → analysis/
        'comprehensive_directory_review.py': 'analysis/',
        'directory_cleanup_organizer.py': 'analysis/',
        'completeness_analyzer.py': 'analysis/',
        
        # Legacy/success markers → archived/
        'INTEGRATION_SUCCESS.py': 'archived/',
        
        # Scripts → analysis/ (utility scripts)
        'organize_directory.sh': 'analysis/',
        
        # Documentation backups → analysis/
        'README_OLD.md': 'analysis/'
    }
    
    # Execute moves
    moved_files = []
    errors = []
    
    for filename, target_subdir in file_moves.items():
        source_path = root_path / filename
        target_dir = root_path / target_subdir
        target_path = target_dir / filename
        
        if source_path.exists():
            try:
                # Ensure target directory exists
                target_dir.mkdir(exist_ok=True)
                
                # Move file
                shutil.move(str(source_path), str(target_path))
                moved_files.append(f"{filename} → {target_subdir}")
                print(f"✅ Moved {filename} → {target_subdir}")
                
            except Exception as e:
                errors.append(f"❌ Error moving {filename}: {e}")
                print(f"❌ Error moving {filename}: {e}")
        else:
            print(f"⚠️ File not found: {filename}")
    
    # Summary
    print(f"\n📊 CLEANUP SUMMARY")
    print("-" * 30)
    print(f"✅ Files moved: {len(moved_files)}")
    print(f"❌ Errors: {len(errors)}")
    
    if moved_files:
        print(f"\n📂 MOVED FILES:")
        for move in moved_files:
            print(f"   {move}")
    
    if errors:
        print(f"\n❌ ERRORS:")
        for error in errors:
            print(f"   {error}")
    
    return moved_files, errors

def verify_clean_root():
    """Verify root directory is clean"""
    root_path = Path("/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio")
    
    print(f"\n🔍 ROOT DIRECTORY VERIFICATION")
    print("-" * 30)
    
    # List remaining files in root
    python_files = list(root_path.glob("*.py"))
    json_files = list(root_path.glob("*.json"))
    shell_files = list(root_path.glob("*.sh"))
    
    remaining_files = python_files + json_files + shell_files
    
    if remaining_files:
        print(f"⚠️ Remaining files in root:")
        for file in remaining_files:
            print(f"   📄 {file.name}")
        return False
    else:
        print(f"✅ Root directory is clean!")
        print(f"📂 Only subdirectories and README.md remain")
        
        # Show subdirectories
        subdirs = [d for d in root_path.iterdir() if d.is_dir() and not d.name.startswith('.')]
        print(f"\n📁 Subdirectories:")
        for subdir in subdirs:
            file_count = len(list(subdir.glob("*")))
            print(f"   📂 {subdir.name}/ ({file_count} files)")
        
        return True

def update_final_readme():
    """Update README to reflect final clean structure"""
    print(f"\n📝 README UPDATE NEEDED")
    print("-" * 30)
    print("✏️ README.md should be updated to reflect:")
    print("   📂 All core files now in core/ subdirectory")
    print("   📂 No files remaining in root directory")
    print("   📂 Clean subdirectory-only structure")

def main():
    """Execute complete root cleanup"""
    print("🎯 Starting complete root directory cleanup...")
    
    # Move all files
    moved_files, errors = organize_all_root_files()
    
    # Verify cleanup
    is_clean = verify_clean_root()
    
    # README update reminder
    update_final_readme()
    
    print(f"\n🎉 ROOT CLEANUP COMPLETE!")
    print("=" * 50)
    print(f"📊 Files organized: {len(moved_files)}")
    print(f"🧹 Root directory clean: {'✅ Yes' if is_clean else '❌ No'}")
    print(f"📝 README update: Required")
    
    return {
        'moved_files': moved_files,
        'errors': errors,
        'root_clean': is_clean
    }

if __name__ == "__main__":
    main()