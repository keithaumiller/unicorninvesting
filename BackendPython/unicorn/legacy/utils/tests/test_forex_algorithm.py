#!/usr/bin/env python3
"""
Unicorn LEAN Forex Algorithm Test Runner
Tests our basic forex algorithm with LEAN framework
"""

import os
import sys
import subprocess
import json
import tempfile
from pathlib import Path

# Add LEAN paths to Python path
LEAN_PATH = Path(__file__).parent.parent / "Lean"
ALGORITHM_PATH = Path(__file__).parent / "algorithms"

def create_test_config():
    """Create a temporary config file for our forex algorithm test"""
    
    config = {
        "environment": "backtesting",
        "algorithm-type-name": "UnicornBasicForexAlgorithm",
        "algorithm-language": "Python",
        "algorithm-location": str(ALGORITHM_PATH / "unicorn_basic_forex_algorithm.py"),
        "data-folder": str(LEAN_PATH / "Data"),
        "debugging": False,
        "debugging-method": "LocalCmdline",
        
        # Handlers
        "log-handler": "QuantConnect.Logging.CompositeLogHandler",
        "messaging-handler": "QuantConnect.Messaging.Messaging",
        "job-queue-handler": "QuantConnect.Queues.JobQueue",
        "api-handler": "QuantConnect.Api.Api",
        "map-file-provider": "QuantConnect.Data.Auxiliary.LocalDiskMapFileProvider",
        "factor-file-provider": "QuantConnect.Data.Auxiliary.LocalDiskFactorFileProvider",
        "data-provider": "QuantConnect.Lean.Engine.DataFeeds.DefaultDataProvider",
        "data-channel-provider": "DataChannelProvider",
        "object-store": "QuantConnect.Lean.Engine.Storage.LocalObjectStore",
        "data-aggregator": "QuantConnect.Lean.Engine.DataFeeds.AggregationManager",
        
        # Symbol limits
        "symbol-minute-limit": 10000,
        "symbol-second-limit": 10000,
        "symbol-tick-limit": 10000,
        
        # Output configuration
        "results-destination-folder": "./test_results",
        "log-destination-file": "./test_results/log.txt",
        
        # Backtest configuration
        "backtesting": {
            "name": "Unicorn Forex Test",
            "cash": 100000,
            "start-date": "2023-01-01",
            "end-date": "2023-12-31"
        }
    }
    
    return config

def run_lean_algorithm():
    """Run the LEAN algorithm test"""
    
    print("🦄 Starting Unicorn LEAN Forex Algorithm Test")
    print("=" * 60)
    
    # Create test results directory
    results_dir = Path("./test_results")
    results_dir.mkdir(exist_ok=True)
    
    # Create temporary config file
    config = create_test_config()
    config_file = results_dir / "test_config.json"
    
    with open(config_file, 'w') as f:
        json.dump(config, f, indent=2)
    
    print(f"📝 Created test config: {config_file}")
    print(f"🧪 Algorithm: {config['algorithm-type-name']}")
    print(f"📊 Testing period: {config['backtesting']['start-date']} to {config['backtesting']['end-date']}")
    print(f"💰 Starting cash: ${config['backtesting']['cash']:,}")
    
    # Check if LEAN executable exists
    lean_exe = LEAN_PATH / "Launcher" / "bin" / "Debug" / "QuantConnect.Lean.Launcher.exe"
    if not lean_exe.exists():
        lean_exe = LEAN_PATH / "Launcher" / "bin" / "Release" / "QuantConnect.Lean.Launcher.exe"
    
    if not lean_exe.exists():
        print("❌ LEAN executable not found. Need to build LEAN first.")
        print("   Run: cd BackendPython/Lean && dotnet build")
        return False
    
    print(f"🚀 Running LEAN engine: {lean_exe}")
    
    try:
        # Run LEAN with our config
        cmd = [str(lean_exe), "--config", str(config_file)]
        
        print("⏳ Executing algorithm...")
        print(f"Command: {' '.join(cmd)}")
        
        result = subprocess.run(
            cmd,
            cwd=str(LEAN_PATH / "Launcher"),
            capture_output=True,
            text=True,
            timeout=300  # 5 minute timeout
        )
        
        # Save output
        output_file = results_dir / "lean_output.txt"
        with open(output_file, 'w') as f:
            f.write("STDOUT:\n")
            f.write(result.stdout)
            f.write("\n\nSTDERR:\n")
            f.write(result.stderr)
        
        print(f"📄 Output saved to: {output_file}")
        
        if result.returncode == 0:
            print("✅ Algorithm completed successfully!")
            
            # Look for key metrics in output
            if "Portfolio Value:" in result.stdout:
                print("💰 Found portfolio value updates in output")
            if "Order Filled:" in result.stdout:
                print("📈 Found trade executions in output")
            if "Algorithm Completed!" in result.stdout:
                print("🏁 Algorithm reached completion")
                
            return True
        else:
            print(f"❌ Algorithm failed with return code: {result.returncode}")
            print("Error output:")
            print(result.stderr[:500] + "..." if len(result.stderr) > 500 else result.stderr)
            return False
            
    except subprocess.TimeoutExpired:
        print("⏰ Algorithm timed out after 5 minutes")
        return False
    except Exception as e:
        print(f"❌ Error running algorithm: {e}")
        return False

def validate_results():
    """Validate the algorithm results"""
    
    print("\n🔍 Validating Results")
    print("=" * 30)
    
    results_dir = Path("./test_results")
    output_file = results_dir / "lean_output.txt"
    
    if not output_file.exists():
        print("❌ No output file found")
        return False
    
    with open(output_file, 'r') as f:
        output = f.read()
    
    # Check for expected patterns
    checks = [
        ("🦄 Unicorn Forex Algorithm Initialized!", "Algorithm initialization"),
        ("Trading: ", "Symbol setup"),
        ("Portfolio Value:", "Portfolio tracking"),
        ("Order Filled:", "Trade execution"),
        ("Algorithm Completed!", "Successful completion")
    ]
    
    passed = 0
    for pattern, description in checks:
        if pattern in output:
            print(f"✅ {description}: Found")
            passed += 1
        else:
            print(f"⚠️  {description}: Not found")
    
    print(f"\n📊 Validation Results: {passed}/{len(checks)} checks passed")
    
    # Extract key metrics if available
    lines = output.split('\n')
    for line in lines:
        if "Final Portfolio Value:" in line:
            print(f"💰 {line.strip()}")
        elif "Total Return:" in line:
            print(f"📈 {line.strip()}")
    
    return passed >= 3  # At least 3 checks should pass

if __name__ == "__main__":
    print("🦄 Unicorn LEAN Forex Algorithm Validation")
    print("Testing our hello world forex portfolio with LEAN framework")
    print("=" * 80)
    
    # Step 1: Run the algorithm
    success = run_lean_algorithm()
    
    if success:
        # Step 2: Validate results
        validated = validate_results()
        
        if validated:
            print("\n🎉 Forex algorithm test completed successfully!")
            print("✅ LEAN system is working as expected")
        else:
            print("\n⚠️  Algorithm ran but validation found issues")
            print("❓ Check test_results/lean_output.txt for details")
    else:
        print("\n❌ Algorithm test failed")
        print("🔧 Check LEAN installation and algorithm code")
    
    print("\n📁 Test results saved in: ./test_results/")
    sys.exit(0 if success and validated else 1)
