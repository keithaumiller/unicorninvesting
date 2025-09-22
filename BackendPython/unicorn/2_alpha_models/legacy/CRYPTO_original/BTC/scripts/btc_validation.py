"""
BTC Model Validation Script

Comprehensive validation of all BTC models.
"""

import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from shared.testing_framework import run_model_test_suite, generate_test_report
from models.btc_alpha import BTCAlphaModel

def generate_test_data() -> pd.DataFrame:
    """Generate test data for BTC validation."""
    np.random.seed(42)
    
    base_price = 50000 if 'BTC' == 'BTC' else 3000 if 'BTC' == 'ETH' else 100
    volatility = 0.04 if 'BTC' == 'BTC' else 0.05 if 'BTC' == 'ETH' else 0.03
    
    periods = 730
    dates = pd.date_range(
        start=datetime.now() - timedelta(days=periods),
        periods=periods,
        freq='D'
    )
    
    # Generate returns
    returns = np.random.normal(0, volatility, periods)
    
    # Convert to prices
    log_prices = np.log(base_price) + np.cumsum(returns)
    prices = np.exp(log_prices)
    
    # Generate OHLC
    close_prices = prices
    open_prices = np.roll(close_prices, 1)
    open_prices[0] = close_prices[0]
    
    intraday_noise = np.random.normal(0, volatility * 0.2, periods)
    high_prices = np.maximum(open_prices, close_prices) + np.abs(intraday_noise)
    low_prices = np.minimum(open_prices, close_prices) - np.abs(intraday_noise)
    
    # Generate volume
    base_volume = 1000000 if 'BTC' in ['BTC', 'ETH'] else 100000
    volume = base_volume * np.random.lognormal(0, 0.3, periods)
    
    return pd.DataFrame({
        'Open': open_prices,
        'High': high_prices,
        'Low': low_prices,
        'Close': close_prices,
        'Volume': volume
    }, index=dates)

def run_comprehensive_validation() -> Dict[str, Any]:
    """Run comprehensive validation of all BTC models."""
    print(f"🚀 Starting Comprehensive BTC Model Validation")
    print("=" * 60)
    
    # Validate technical alpha model
    print(f"🧪 Validating BTC Technical Alpha Model...")
    test_results = run_model_test_suite(
        model_class=BTCAlphaModel,
        asset_name='BTC',
        model_type="technical"
    )
    
    # Generate test report
    report = generate_test_report(test_results)
    report_file = f"btc_validation_report.txt"
    with open(report_file, 'w') as f:
        f.write(report)
    
    print(f"✅ Validation complete. Report saved to {report_file}")
    
    return test_results

if __name__ == "__main__":
    results = run_comprehensive_validation()
    print(f"🏁 BTC Model Validation Complete!")
