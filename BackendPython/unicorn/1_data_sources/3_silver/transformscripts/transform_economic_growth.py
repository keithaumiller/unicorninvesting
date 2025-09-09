"""
Economic Growth Indicators Transformation Script

Silver layer transformation for economic growth indicators including:
- GDP Growth Rate
- Industrial Production
- Employment Data
- Productivity Metrics
"""

import pandas as pd
import numpy as np
from datetime import datetime
import logging

logger = logging.getLogger(__name__)

def transform_gdp_data(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform GDP growth data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        # Standardize GDP growth data
        if 'gdp_growth' in df.columns:
            # Convert to quarterly annualized rate if needed
            df['gdp_growth_annualized'] = df['gdp_growth'] * 4  # Quarterly to annual
            
            # Add GDP growth categories
            df['gdp_growth_category'] = pd.cut(
                df['gdp_growth'],
                bins=[-np.inf, 0, 2, 4, np.inf],
                labels=['Recession', 'Slow Growth', 'Moderate Growth', 'Strong Growth']
            )
        
        # Add metadata
        df['indicator_category'] = 'economic_growth'
        df['frequency'] = 'quarterly'
        df['unit'] = 'percent'
        df['source'] = 'bureau_of_economic_analysis'
        
        logger.info(f"✅ Transformed {len(df)} GDP records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming GDP data: {e}")
    
    return df

def transform_industrial_production(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform industrial production data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        # Calculate year-over-year change
        if 'industrial_production' in df.columns:
            df = df.sort_values('timestamp')
            df['industrial_production_yoy'] = df['industrial_production'].pct_change(periods=12) * 100
            
            # Add moving averages
            df['industrial_production_ma3'] = df['industrial_production'].rolling(window=3).mean()
            df['industrial_production_ma12'] = df['industrial_production'].rolling(window=12).mean()
        
        # Add metadata
        df['indicator_category'] = 'economic_growth'
        df['frequency'] = 'monthly'
        df['unit'] = 'index'
        df['source'] = 'federal_reserve'
        
        logger.info(f"✅ Transformed {len(df)} industrial production records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming industrial production: {e}")
    
    return df

def transform_employment_data(bronze_df: pd.DataFrame) -> pd.DataFrame:
    """Transform employment data for silver layer."""
    
    df = bronze_df.copy()
    
    try:
        # Unemployment rate transformations
        if 'unemployment_rate' in df.columns:
            # Add unemployment categories
            df['unemployment_category'] = pd.cut(
                df['unemployment_rate'],
                bins=[0, 3.5, 5.0, 7.5, np.inf],
                labels=['Very Low', 'Low', 'Moderate', 'High']
            )
            
            # Calculate change in unemployment
            df['unemployment_change'] = df['unemployment_rate'].diff()
        
        # Non-farm payrolls transformations
        if 'nonfarm_payrolls' in df.columns:
            # Convert to thousands if needed
            if df['nonfarm_payrolls'].max() > 1000000:  # If in raw numbers
                df['nonfarm_payrolls'] = df['nonfarm_payrolls'] / 1000
            
            # Calculate moving averages
            df['payrolls_ma3'] = df['nonfarm_payrolls'].rolling(window=3).mean()
            df['payrolls_ma6'] = df['nonfarm_payrolls'].rolling(window=6).mean()
        
        # Add metadata
        df['indicator_category'] = 'economic_growth'
        df['frequency'] = 'monthly'
        df['unit'] = 'percent'
        df['source'] = 'bureau_of_labor_statistics'
        
        logger.info(f"✅ Transformed {len(df)} employment records")
        
    except Exception as e:
        logger.error(f"❌ Error transforming employment data: {e}")
    
    return df

def create_economic_growth_composite(dataframes: list) -> pd.DataFrame:
    """Create composite economic growth indicator."""
    
    try:
        # Combine all growth indicators
        combined_df = pd.concat(dataframes, ignore_index=True)
        
        # Create pivot for composite calculation
        pivot_df = combined_df.pivot_table(
            index='timestamp',
            columns='indicator_name',
            values='indicator_value',
            aggfunc='first'
        ).reset_index()
        
        # Normalize indicators (z-score)
        numeric_cols = pivot_df.select_dtypes(include=[np.number]).columns
        for col in numeric_cols:
            values = pivot_df[col].dropna()
            if len(values) > 1 and values.std() > 0:
                pivot_df[f"{col}_normalized"] = (pivot_df[col] - values.mean()) / values.std()
        
        # Create composite index (average of normalized indicators)
        normalized_cols = [col for col in pivot_df.columns if col.endswith('_normalized')]
        if normalized_cols:
            pivot_df['economic_growth_composite'] = pivot_df[normalized_cols].mean(axis=1)
            
            # Convert back to long format
            composite_df = pd.DataFrame({
                'timestamp': pivot_df['timestamp'],
                'indicator_category': 'economic_growth',
                'indicator_name': 'economic_growth_composite',
                'indicator_value': pivot_df['economic_growth_composite'],
                'unit': 'normalized_index',
                'frequency': 'monthly',
                'source': 'composite_calculation',
                'quality_score': 0.9,
                'is_interpolated': False,
                'is_seasonally_adjusted': False,
                'metadata': '{"type": "composite", "components": "gdp,industrial_production,employment"}'
            })
            
            logger.info(f"✅ Created economic growth composite with {len(composite_df)} records")
            return composite_df
    
    except Exception as e:
        logger.error(f"❌ Error creating economic growth composite: {e}")
    
    return pd.DataFrame()

if __name__ == "__main__":
    print("📈 Economic Growth Indicators Transformation")
    print("This script transforms bronze layer economic growth data to silver layer.")
