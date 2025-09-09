"""
Sample Economic Indicators Data Generator

Creates sample bronze layer economic data for demonstration
of silver layer processing capabilities.
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import os

def generate_sample_economic_data():
    """Generate sample economic indicators data for bronze layer."""
    
    # Set up paths
    current_dir = Path(__file__).parent
    bronze_path = current_dir.parent.parent / '2_bronze' / 'economic_indicators' / 'processed_data'
    bronze_path.mkdir(parents=True, exist_ok=True)
    
    # Date range for sample data (last 5 years)
    end_date = datetime.now()
    start_date = end_date - timedelta(days=1825)  # 5 years
    
    print("📊 Generating Sample Economic Indicators Data")
    print("=" * 60)
    
    # 1. Economic Growth Indicators
    print("📈 Creating Economic Growth Indicators...")
    
    # GDP Growth (quarterly)
    quarterly_dates = pd.date_range(start=start_date, end=end_date, freq='Q')
    gdp_data = pd.DataFrame({
        'timestamp': quarterly_dates,
        'indicator_name': 'gdp_growth',
        'indicator_value': np.random.normal(2.5, 1.2, len(quarterly_dates)),  # Average 2.5% growth
        'variable': 'gdp_growth',
        'category': 'economic_growth',
        'unit': 'percent',
        'frequency': 'quarterly',
        'source': 'bea'
    })
    
    # Industrial Production (monthly)
    monthly_dates = pd.date_range(start=start_date, end=end_date, freq='M')
    industrial_production = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'industrial_production',
        'indicator_value': 100 + np.cumsum(np.random.normal(0.1, 0.8, len(monthly_dates))),
        'variable': 'industrial_production',
        'category': 'economic_growth',
        'unit': 'index',
        'frequency': 'monthly',
        'source': 'federal_reserve'
    })
    
    # Unemployment Rate (monthly)
    unemployment_data = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'unemployment_rate',
        'indicator_value': np.maximum(2.0, 5.5 + np.cumsum(np.random.normal(-0.01, 0.15, len(monthly_dates)))),
        'variable': 'unemployment_rate',
        'category': 'economic_growth',
        'unit': 'percent',
        'frequency': 'monthly',
        'source': 'bls'
    })
    
    # Combine economic growth data
    economic_growth_df = pd.concat([gdp_data, industrial_production, unemployment_data], ignore_index=True)
    economic_growth_df.to_csv(bronze_path / 'economic_growth_indicators.csv', index=False)
    print(f"✅ Created {len(economic_growth_df)} economic growth records")
    
    # 2. Consumer & Business Indicators
    print("🏪 Creating Consumer & Business Indicators...")
    
    # Consumer Confidence (monthly)
    consumer_confidence = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'consumer_confidence',
        'indicator_value': np.maximum(50, 100 + np.cumsum(np.random.normal(0, 2, len(monthly_dates)))),
        'variable': 'consumer_confidence',
        'category': 'consumer_business',
        'unit': 'index',
        'frequency': 'monthly',
        'source': 'conference_board'
    })
    
    # Retail Sales (monthly)
    retail_sales = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'retail_sales',
        'indicator_value': 400 + np.cumsum(np.random.normal(1.2, 3, len(monthly_dates))),
        'variable': 'retail_sales',
        'category': 'consumer_business',
        'unit': 'billions_usd',
        'frequency': 'monthly',
        'source': 'census_bureau'
    })
    
    # Housing Starts (monthly)
    housing_starts = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'housing_starts',
        'indicator_value': np.maximum(800, 1200 + np.cumsum(np.random.normal(0, 50, len(monthly_dates)))),
        'variable': 'housing_starts',
        'category': 'consumer_business',
        'unit': 'thousands',
        'frequency': 'monthly',
        'source': 'census_bureau'
    })
    
    # Combine consumer/business data
    consumer_business_df = pd.concat([consumer_confidence, retail_sales, housing_starts], ignore_index=True)
    consumer_business_df.to_csv(bronze_path / 'consumer_business_indicators.csv', index=False)
    print(f"✅ Created {len(consumer_business_df)} consumer/business records")
    
    # 3. Monetary Policy Indicators
    print("🏦 Creating Monetary Policy Indicators...")
    
    # Interest Rates (monthly)
    interest_rates = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'interest_rates',
        'indicator_value': np.maximum(0.1, 2.5 + np.cumsum(np.random.normal(0, 0.1, len(monthly_dates)))),
        'variable': 'interest_rates',
        'category': 'monetary_policy',
        'unit': 'percent',
        'frequency': 'monthly',
        'source': 'federal_reserve'
    })
    
    # PMI Manufacturing (monthly)
    pmi_manufacturing = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'pmi_manufacturing',
        'indicator_value': np.maximum(35, 50 + np.cumsum(np.random.normal(0, 1.5, len(monthly_dates)))),
        'variable': 'pmi_manufacturing',
        'category': 'monetary_policy',
        'unit': 'index',
        'frequency': 'monthly',
        'source': 'ism'
    })
    
    # Corporate Earnings (quarterly)
    corporate_earnings = pd.DataFrame({
        'timestamp': quarterly_dates,
        'indicator_name': 'corporate_earnings',
        'indicator_value': 1500 + np.cumsum(np.random.normal(20, 80, len(quarterly_dates))),
        'variable': 'corporate_earnings',
        'category': 'monetary_policy',
        'unit': 'billions_usd',
        'frequency': 'quarterly',
        'source': 'sp500'
    })
    
    # Combine monetary policy data
    monetary_policy_df = pd.concat([interest_rates, pmi_manufacturing, corporate_earnings], ignore_index=True)
    monetary_policy_df.to_csv(bronze_path / 'monetary_policy_indicators.csv', index=False)
    print(f"✅ Created {len(monetary_policy_df)} monetary policy records")
    
    # 4. International Trade Indicators
    print("🌍 Creating International Trade Indicators...")
    
    # Trade Balance (monthly)
    trade_balance = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'trade_balance',
        'indicator_value': -50 + np.cumsum(np.random.normal(0, 3, len(monthly_dates))),
        'variable': 'trade_balance',
        'category': 'international_trade',
        'unit': 'billions_usd',
        'frequency': 'monthly',
        'source': 'census_bureau'
    })
    
    # Treasury Yields (daily - sample monthly)
    treasury_yields = pd.DataFrame({
        'timestamp': monthly_dates,
        'indicator_name': 'treasury_yields',
        'indicator_value': np.maximum(0.5, 2.8 + np.cumsum(np.random.normal(0, 0.15, len(monthly_dates)))),
        'variable': 'treasury_yields',
        'category': 'international_trade',
        'unit': 'percent',
        'frequency': 'monthly',
        'source': 'treasury'
    })
    
    # Combine international trade data
    international_trade_df = pd.concat([trade_balance, treasury_yields], ignore_index=True)
    international_trade_df.to_csv(bronze_path / 'international_trade_indicators.csv', index=False)
    print(f"✅ Created {len(international_trade_df)} international trade records")
    
    # Summary
    total_records = (len(economic_growth_df) + len(consumer_business_df) + 
                    len(monetary_policy_df) + len(international_trade_df))
    
    print(f"\n📊 SAMPLE DATA GENERATION COMPLETE")
    print(f"Total Records Created: {total_records}")
    print(f"Categories: 4 (Economic Growth, Consumer/Business, Monetary Policy, International Trade)")
    print(f"Time Range: {start_date.strftime('%Y-%m-%d')} to {end_date.strftime('%Y-%m-%d')}")
    print(f"Bronze Data Location: {bronze_path}")
    
    return {
        'total_records': total_records,
        'categories': 4,
        'files_created': [
            'economic_growth_indicators.csv',
            'consumer_business_indicators.csv', 
            'monetary_policy_indicators.csv',
            'international_trade_indicators.csv'
        ],
        'bronze_path': str(bronze_path)
    }

if __name__ == "__main__":
    results = generate_sample_economic_data()
    print(f"\n🎯 Ready for silver layer processing!")
    print(f"Run: python economic_indicators_processor.py")
