"""
Consumer & Business Indicators Transformation Script

Silver layer transformation for consumer and business indicators including:
- Consumer Confidence
- Retail Sales
- Housing Starts
- Business Investment
"""

import pandas as pd
import numpy as np
from datetime import datetime
import logging

logger = logging.getLogger(__name__)

def transform_consumer_confidence(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform consumer confidence data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        if 'consumer_confidence' in df.columns:
            # Add confidence level categories
            df['confidence_category'] = pd.cut(
                df['consumer_confidence'],
                bins=[0, 80, 100, 120, np.inf],
                labels=['Low', 'Moderate', 'High', 'Very High']
            )
            
            # Calculate momentum (change from previous month)
            df['confidence_momentum'] = df['consumer_confidence'].diff()
            
            # Add moving averages
            df['confidence_ma3'] = df['consumer_confidence'].rolling(window=3).mean()
            df['confidence_ma6'] = df['consumer_confidence'].rolling(window=6).mean()
            
            # Calculate volatility
            df['confidence_volatility'] = df['consumer_confidence'].rolling(window=12).std()
        
        # Add metadata
        df['indicator_category'] = 'consumer_business'
        df['frequency'] = 'monthly'
        df['unit'] = 'index'
        df['source'] = 'conference_board'
        
        logger.info(f"✅ Transformed {len(df)} consumer confidence records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming consumer confidence: {e}")
    
    return df

def transform_retail_sales(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform retail sales data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        if 'retail_sales' in df.columns:
            # Calculate year-over-year growth
            df = df.sort_values('timestamp')
            df['retail_sales_yoy'] = df['retail_sales'].pct_change(periods=12) * 100
            
            # Calculate month-over-month growth
            df['retail_sales_mom'] = df['retail_sales'].pct_change(periods=1) * 100
            
            # Add seasonal adjustment proxy
            df['month'] = df['timestamp'].dt.month
            monthly_avg = df.groupby('month')['retail_sales'].transform('mean')
            df['retail_sales_seasonal_adj'] = df['retail_sales'] / monthly_avg * df['retail_sales'].mean()
            
            # Add growth categories
            df['sales_growth_category'] = pd.cut(
                df['retail_sales_yoy'],
                bins=[-np.inf, -2, 0, 3, 6, np.inf],
                labels=['Declining', 'Weak', 'Flat', 'Moderate', 'Strong']
            )
        
        # Add metadata
        df['indicator_category'] = 'consumer_business'
        df['frequency'] = 'monthly'
        df['unit'] = 'billions_usd'
        df['source'] = 'census_bureau'
        
        logger.info(f"✅ Transformed {len(df)} retail sales records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming retail sales: {e}")
    
    return df

def transform_housing_starts(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform housing starts data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        if 'housing_starts' in df.columns:
            # Convert to annualized rate if needed
            if df['housing_starts'].max() < 10:  # If in millions
                df['housing_starts_annualized'] = df['housing_starts'] * 1000  # Convert to thousands
            
            # Calculate year-over-year change
            df = df.sort_values('timestamp')
            df['housing_starts_yoy'] = df['housing_starts'].pct_change(periods=12) * 100
            
            # Add moving averages
            df['housing_starts_ma3'] = df['housing_starts'].rolling(window=3).mean()
            df['housing_starts_ma12'] = df['housing_starts'].rolling(window=12).mean()
            
            # Add housing market categories
            df['housing_market_category'] = pd.cut(
                df['housing_starts_yoy'],
                bins=[-np.inf, -10, 0, 10, 20, np.inf],
                labels=['Declining', 'Weak', 'Stable', 'Growing', 'Booming']
            )
        
        # Add metadata
        df['indicator_category'] = 'consumer_business'
        df['frequency'] = 'monthly'
        df['unit'] = 'thousands_annualized'
        df['source'] = 'census_bureau'
        
        logger.info(f"✅ Transformed {len(df)} housing starts records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming housing starts: {e}")
    
    return df

def transform_business_investment(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform business investment data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        if 'business_investment' in df.columns:
            # Calculate quarterly change
            df = df.sort_values('timestamp')
            df['investment_qoq'] = df['business_investment'].pct_change(periods=1) * 100
            df['investment_yoy'] = df['business_investment'].pct_change(periods=4) * 100
            
            # Add investment trend
            df['investment_trend'] = df['business_investment'].rolling(window=4).apply(
                lambda x: np.polyfit(range(len(x)), x, 1)[0] if len(x) > 1 else 0
            )
            
            # Add investment categories
            df['investment_category'] = pd.cut(
                df['investment_yoy'],
                bins=[-np.inf, -5, 0, 5, 10, np.inf],
                labels=['Contracting', 'Weak', 'Stable', 'Growing', 'Expanding']
            )
        
        # Add metadata
        df['indicator_category'] = 'consumer_business'
        df['frequency'] = 'quarterly'
        df['unit'] = 'billions_usd'
        df['source'] = 'bureau_of_economic_analysis'
        
        logger.info(f"✅ Transformed {len(df)} business investment records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming business investment: {e}")
    
    return df

def create_consumer_business_composite(dataframes: list) -> pd.DataFrame:
    """Create composite consumer and business sentiment indicator."""
    
    try:
        # Combine all consumer/business indicators
        combined_df = pd.concat(dataframes, ignore_index=True)
        
        # Create pivot for composite calculation
        pivot_df = combined_df.pivot_table(
            index='timestamp',
            columns='indicator_name',
            values='indicator_value',
            aggfunc='first'
        ).reset_index()
        
        # Normalize indicators using z-score method
        numeric_cols = pivot_df.select_dtypes(include=[np.number]).columns
        normalized_data = {}
        
        for col in numeric_cols:
            values = pivot_df[col].dropna()
            if len(values) > 1 and values.std() > 0:
                normalized_data[f"{col}_norm"] = (pivot_df[col] - values.mean()) / values.std()
        
        # Create weighted composite (consumer confidence gets higher weight)
        weights = {
            'consumer_confidence_norm': 0.4,
            'retail_sales_norm': 0.3,
            'housing_starts_norm': 0.2,
            'business_investment_norm': 0.1
        }
        
        composite_values = []
        for idx, row in pivot_df.iterrows():
            weighted_sum = 0
            total_weight = 0
            
            for indicator, weight in weights.items():
                if indicator in normalized_data and not pd.isna(normalized_data[indicator].iloc[idx]):
                    weighted_sum += normalized_data[indicator].iloc[idx] * weight
                    total_weight += weight
            
            composite_values.append(weighted_sum / total_weight if total_weight > 0 else np.nan)
        
        # Create composite dataframe
        composite_df = pd.DataFrame({
            'timestamp': pivot_df['timestamp'],
            'indicator_category': 'consumer_business',
            'indicator_name': 'consumer_business_composite',
            'indicator_value': composite_values,
            'unit': 'normalized_index',
            'frequency': 'monthly',
            'source': 'composite_calculation',
            'quality_score': 0.9,
            'is_interpolated': False,
            'is_seasonally_adjusted': False,
            'metadata': '{"type": "weighted_composite", "weights": {"consumer_confidence": 0.4, "retail_sales": 0.3, "housing_starts": 0.2, "business_investment": 0.1}}'
        })
        
        logger.info(f"✅ Created consumer/business composite with {len(composite_df)} records")
        return composite_df
    
    except Exception as e:
        logger.error(f"❌ Error creating consumer/business composite: {e}")
    
    return pd.DataFrame()

if __name__ == "__main__":
    print("🏪 Consumer & Business Indicators Transformation")
    print("This script transforms bronze layer consumer and business data to silver layer.")
