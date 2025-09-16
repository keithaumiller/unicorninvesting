#!/usr/bin/env python3
"""
BTC-focused simulation runner for validation testing
"""

import sys
from pathlib import Path

# Add the Myportolio simulation path
sys.path.append(str(Path(__file__).parent))

from myportolio_simulator import main

if __name__ == "__main__":
    # Override sys.argv to run BTC backtest
    sys.argv = [
        "btc_validator.py",
        "backtest", 
        "--start", "2024-03-01",
        "--end", "2024-06-01", 
        "--strategy", "btc_momentum_validation",
        "--asset", "BTC"
    ]
    
    print("🚀 Starting BTC-focused validation simulation...")
    print("📊 Asset: Bitcoin (BTC)")
    print("📅 Period: March 2024 - June 2024 (3 months)")
    print("🎯 Strategy: BTC Momentum (5/20 MA crossover)")
    print("=" * 50)
    
    main()