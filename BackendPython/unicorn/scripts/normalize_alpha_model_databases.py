#!/usr/bin/env python3
"""
Database Schema Normalization Script for Alpha Models

Converts XGBoost database schema to match the standardized Prophet schema
using metric_name and metric_value pivot structure for consistency.
"""

import sqlite3
import os
from pathlib import Path
from datetime import datetime

def normalize_xgboost_database(db_path: str):
    """Convert XGBoost database from direct columns to pivot structure"""
    
    print(f"🔄 Normalizing database: {db_path}")
    
    # Create backup first
    backup_path = f"{db_path}.backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    import shutil
    shutil.copy2(db_path, backup_path)
    print(f"  💾 Backup created: {backup_path}")
    
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Check if old schema exists
    cursor.execute("SELECT sql FROM sqlite_master WHERE type='table' AND name='model_performance';")
    schema_result = cursor.fetchone()
    
    if not schema_result:
        print(f"  ❌ No model_performance table found")
        conn.close()
        return False
    
    current_schema = schema_result[0]
    
    # Check if it's already normalized (has metric_name column)
    if 'metric_name' in current_schema:
        print(f"  ✅ Already normalized - skipping")
        conn.close()
        return True
    
    print(f"  🔄 Converting from direct columns to pivot structure...")
    
    # Read existing data
    cursor.execute("SELECT * FROM model_performance")
    old_data = cursor.fetchall()
    
    # Get column names
    cursor.execute("PRAGMA table_info(model_performance)")
    columns = [col[1] for col in cursor.fetchall()]
    
    print(f"  📊 Found {len(old_data)} records with {len(columns)} columns")
    
    # Create new normalized table
    cursor.execute("DROP TABLE IF EXISTS model_performance_new")
    cursor.execute("""
        CREATE TABLE model_performance_new (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            model_id TEXT NOT NULL,
            model_variant TEXT NOT NULL,
            metric_name TEXT NOT NULL,
            metric_value REAL NOT NULL,
            created_at TEXT NOT NULL,
            data_period TEXT NOT NULL DEFAULT 'training',
            methodology TEXT DEFAULT 'xgboost',
            asset TEXT DEFAULT 'ETH',
            model_config TEXT,
            feature_importance TEXT
        )
    """)
    
    # Define metrics to extract
    metric_columns = ['mae', 'mse', 'rmse', 'mape', 'r2_score', 'training_time']
    info_columns = ['model_id', 'model_variant', 'methodology', 'asset', 'created_at', 'model_config', 'feature_importance']
    
    # Convert each old record to multiple pivot records
    conversion_count = 0
    for row in old_data:
        row_dict = dict(zip(columns, row))
        
        # Extract info fields
        model_id = row_dict.get('model_id', 'unknown')
        model_variant = row_dict.get('model_variant', 'standard')
        methodology = row_dict.get('methodology', 'xgboost')
        asset = row_dict.get('asset', 'ETH')
        created_at = row_dict.get('created_at', datetime.now().isoformat())
        model_config = row_dict.get('model_config', '{}')
        feature_importance = row_dict.get('feature_importance', '{}')
        
        # Insert one record per metric
        for metric_col in metric_columns:
            if metric_col in row_dict and row_dict[metric_col] is not None:
                cursor.execute("""
                    INSERT INTO model_performance_new 
                    (model_id, model_variant, metric_name, metric_value, created_at, data_period, 
                     methodology, asset, model_config, feature_importance)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    model_id, model_variant, metric_col, float(row_dict[metric_col]),
                    created_at, 'training', methodology, asset, model_config, feature_importance
                ))
                conversion_count += 1
    
    # Replace old table with new normalized table
    cursor.execute("DROP TABLE model_performance")
    cursor.execute("ALTER TABLE model_performance_new RENAME TO model_performance")
    
    # Create index for performance
    cursor.execute("CREATE INDEX idx_model_performance_lookup ON model_performance(model_id, metric_name, created_at)")
    
    conn.commit()
    conn.close()
    
    print(f"  ✅ Conversion complete: {conversion_count} metric records created")
    return True

def normalize_prophet_database(db_path: str):
    """Ensure Prophet database has all required columns"""
    
    print(f"🔄 Checking Prophet database: {db_path}")
    
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Check current schema
    cursor.execute("PRAGMA table_info(model_performance)")
    columns = [col[1] for col in cursor.fetchall()]
    
    # Add missing columns if needed
    required_columns = ['methodology', 'asset', 'model_config', 'feature_importance']
    
    for column in required_columns:
        if column not in columns:
            if column == 'methodology':
                cursor.execute("ALTER TABLE model_performance ADD COLUMN methodology TEXT DEFAULT 'prophet'")
                print(f"  ➕ Added column: {column}")
            elif column == 'asset':
                cursor.execute("ALTER TABLE model_performance ADD COLUMN asset TEXT DEFAULT 'ETH'")
                print(f"  ➕ Added column: {column}")
            elif column == 'model_config':
                cursor.execute("ALTER TABLE model_performance ADD COLUMN model_config TEXT DEFAULT '{}'")
                print(f"  ➕ Added column: {column}")
            elif column == 'feature_importance':
                cursor.execute("ALTER TABLE model_performance ADD COLUMN feature_importance TEXT DEFAULT '{}'")
                print(f"  ➕ Added column: {column}")
    
    # Create index if it doesn't exist
    try:
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_model_performance_lookup ON model_performance(model_id, metric_name, created_at)")
        print(f"  ✅ Index ensured")
    except:
        pass
    
    conn.commit()
    conn.close()
    
    print(f"  ✅ Prophet database normalized")
    return True

def normalize_ensemble_database(db_path: str):
    """Handle ensemble database with ensemble_performance table"""
    
    print(f"🔄 Checking Ensemble database: {db_path}")
    
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Check if ensemble_performance table exists
    cursor.execute("SELECT sql FROM sqlite_master WHERE type='table' AND name='ensemble_performance';")
    ensemble_table = cursor.fetchone()
    
    if ensemble_table:
        print(f"  ✅ Found ensemble_performance table")
        
        # Create model_performance table if it doesn't exist (for consistency)
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS model_performance (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                model_id TEXT NOT NULL,
                model_variant TEXT NOT NULL,
                metric_name TEXT NOT NULL,
                metric_value REAL NOT NULL,
                created_at TEXT NOT NULL,
                data_period TEXT NOT NULL DEFAULT 'training',
                methodology TEXT DEFAULT 'ensemble',
                asset TEXT DEFAULT 'ETH',
                model_config TEXT DEFAULT '{}',
                feature_importance TEXT DEFAULT '{}'
            )
        """)
        
        # Check if we need to copy data
        cursor.execute("SELECT COUNT(*) FROM model_performance")
        model_perf_count = cursor.fetchone()[0]
        
        if model_perf_count == 0:
            # Copy data from ensemble_performance to model_performance
            cursor.execute("SELECT * FROM ensemble_performance LIMIT 1")
            sample_row = cursor.fetchone()
            
            if sample_row:
                # This would need to be customized based on actual ensemble_performance schema
                print(f"  📊 Ensemble table has data - manual migration needed")
                print(f"  ⚠️  Please manually migrate ensemble_performance to model_performance format")
        
        # Create index
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_model_performance_lookup ON model_performance(model_id, metric_name, created_at)")
        
    else:
        # Create the standard model_performance table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS model_performance (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                model_id TEXT NOT NULL,
                model_variant TEXT NOT NULL,
                metric_name TEXT NOT NULL,
                metric_value REAL NOT NULL,
                created_at TEXT NOT NULL,
                data_period TEXT NOT NULL DEFAULT 'training',
                methodology TEXT DEFAULT 'ensemble',
                asset TEXT DEFAULT 'ETH',
                model_config TEXT DEFAULT '{}',
                feature_importance TEXT DEFAULT '{}'
            )
        """)
        print(f"  ✅ Created standard model_performance table")
    
    conn.commit()
    conn.close()
    
    print(f"  ✅ Ensemble database processed")
    return True

def main():
    """Normalize all alpha model databases"""
    
    print("🔧 ALPHA MODEL DATABASE NORMALIZATION")
    print("=" * 50)
    
    # Find all alpha model databases
    alpha_models_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models")
    
    if not alpha_models_dir.exists():
        print(f"❌ Alpha models directory not found: {alpha_models_dir}")
        return False
    
    databases_found = []
    databases_normalized = 0
    
    # Find all comparison databases
    for db_file in alpha_models_dir.rglob("*_comparison.db"):
        databases_found.append(db_file)
    
    print(f"📊 Found {len(databases_found)} databases to normalize")
    
    for db_path in databases_found:
        try:
            if "prophet" in db_path.name:
                if normalize_prophet_database(str(db_path)):
                    databases_normalized += 1
            elif "xgboost" in db_path.name:
                if normalize_xgboost_database(str(db_path)):
                    databases_normalized += 1
            elif "ensemble" in db_path.name:
                if normalize_ensemble_database(str(db_path)):
                    databases_normalized += 1
            else:
                print(f"  ⚠️  Unknown database type: {db_path.name}")
        except Exception as e:
            print(f"  ❌ Error normalizing {db_path.name}: {e}")
    
    print(f"\n✅ NORMALIZATION COMPLETE")
    print(f"   📊 Databases Found: {len(databases_found)}")
    print(f"   ✅ Successfully Normalized: {databases_normalized}")
    print(f"   📈 Success Rate: {(databases_normalized/len(databases_found)*100):.1f}%")
    
    if databases_normalized == len(databases_found):
        print(f"   🎉 ALL DATABASES NORMALIZED SUCCESSFULLY!")
        return True
    else:
        print(f"   ⚠️  Some databases may need manual attention")
        return False

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)
