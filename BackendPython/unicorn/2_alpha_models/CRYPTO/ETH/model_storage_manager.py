"""
ETH Model Storage Manager

Scalable and organized model storage system with version control and easy retrieval.
Supports multiple methodologies and maintains clean directory structure.
"""

import os
import pickle
import json
from datetime import datetime
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
import sqlite3
from dataclasses import dataclass, asdict

@dataclass
class ModelMetadata:
    """Metadata for stored models."""
    model_id: str
    methodology: str  # prophet, xgboost, lstm, etc.
    version: int
    asset: str
    created_at: str
    file_path: str
    file_size: int
    model_config: Dict[str, Any]
    performance_metrics: Dict[str, float]
    description: str
    tags: List[str]

class ModelStorageManager:
    """
    Manages organized storage and retrieval of ML models.
    
    Directory Structure:
    model_storage/
    ├── prophet/
    │   ├── v001_eth_prophet_basic_20250902_143025.pkl
    │   ├── v002_eth_prophet_enhanced_20250902_143030.pkl
    │   └── v003_eth_prophet_optimized_20250902_143035.pkl
    ├── xgboost/
    │   ├── v001_eth_xgboost_standard_20250902_143040.pkl
    │   └── v002_eth_xgboost_tuned_20250902_143045.pkl
    ├── lstm/
    ├── ensemble/
    └── arima/
    """
    
    def __init__(self, base_path: str = None):
        if base_path is None:
            base_path = "/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/model_storage"
        
        self.base_path = Path(base_path)
        self.metadata_db = self.base_path / "model_metadata.db"
        
        # Ensure directories exist
        self._ensure_directories()
        self._init_metadata_db()
    
    def _ensure_directories(self):
        """Create necessary directories if they don't exist."""
        methodologies = ['prophet', 'xgboost', 'lstm', 'ensemble', 'arima', 'transformer', 'garch']
        
        for methodology in methodologies:
            (self.base_path / methodology).mkdir(parents=True, exist_ok=True)
    
    def _init_metadata_db(self):
        """Initialize metadata database."""
        with sqlite3.connect(self.metadata_db) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS model_metadata (
                    model_id TEXT PRIMARY KEY,
                    methodology TEXT NOT NULL,
                    version INTEGER NOT NULL,
                    asset TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    file_path TEXT NOT NULL,
                    file_size INTEGER NOT NULL,
                    model_config TEXT NOT NULL,
                    performance_metrics TEXT NOT NULL,
                    description TEXT NOT NULL,
                    tags TEXT NOT NULL,
                    UNIQUE(methodology, asset, version)
                )
            """)
            
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_methodology_asset 
                ON model_metadata (methodology, asset)
            """)
            
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_created_at 
                ON model_metadata (created_at)
            """)
    
    def get_next_version(self, methodology: str, asset: str) -> int:
        """Get the next version number for a methodology-asset combination."""
        with sqlite3.connect(self.metadata_db) as conn:
            cursor = conn.execute(
                "SELECT MAX(version) FROM model_metadata WHERE methodology = ? AND asset = ?",
                (methodology, asset)
            )
            result = cursor.fetchone()[0]
            return (result or 0) + 1
    
    def generate_model_filename(self, methodology: str, asset: str, variant: str = "") -> Tuple[str, int]:
        """
        Generate a standardized filename for model storage.
        
        Args:
            methodology: Model methodology (prophet, xgboost, etc.)
            asset: Asset name (ETH, BTC, etc.)
            variant: Optional variant name (basic, enhanced, optimized)
            
        Returns:
            Tuple of (filename, version_number)
        """
        version = self.get_next_version(methodology, asset.upper())
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        if variant:
            filename = f"v{version:03d}_{asset.lower()}_{methodology}_{variant}_{timestamp}.pkl"
        else:
            filename = f"v{version:03d}_{asset.lower()}_{methodology}_{timestamp}.pkl"
        
        return filename, version
    
    def store_model(self, 
                   model: Any,
                   methodology: str,
                   asset: str,
                   model_config: Dict[str, Any],
                   performance_metrics: Dict[str, float],
                   description: str = "",
                   variant: str = "",
                   tags: List[str] = None) -> str:
        """
        Store a model with organized structure and metadata.
        
        Args:
            model: The trained model object
            methodology: Model methodology
            asset: Asset name
            model_config: Model configuration parameters
            performance_metrics: Performance metrics dict
            description: Human-readable description
            variant: Model variant name
            tags: List of tags for categorization
            
        Returns:
            model_id: Unique identifier for the stored model
        """
        if tags is None:
            tags = []
        
        # Generate filename and version
        filename, version = self.generate_model_filename(methodology, asset, variant)
        file_path = self.base_path / methodology / filename
        
        # Create model ID
        model_id = f"{methodology}_{asset.upper()}_v{version:03d}"
        
        # Store the model file
        with open(file_path, 'wb') as f:
            pickle.dump(model, f)
        
        # Get file size
        file_size = file_path.stat().st_size
        
        # Create metadata
        metadata = ModelMetadata(
            model_id=model_id,
            methodology=methodology,
            version=version,
            asset=asset.upper(),
            created_at=datetime.now().isoformat(),
            file_path=str(file_path),
            file_size=file_size,
            model_config=model_config,
            performance_metrics=performance_metrics,
            description=description,
            tags=tags
        )
        
        # Store metadata in database
        with sqlite3.connect(self.metadata_db) as conn:
            conn.execute("""
                INSERT INTO model_metadata 
                (model_id, methodology, version, asset, created_at, file_path, file_size,
                 model_config, performance_metrics, description, tags)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                metadata.model_id,
                metadata.methodology,
                metadata.version,
                metadata.asset,
                metadata.created_at,
                metadata.file_path,
                metadata.file_size,
                json.dumps(metadata.model_config),
                json.dumps(metadata.performance_metrics),
                metadata.description,
                json.dumps(metadata.tags)
            ))
        
        print(f"✅ Model stored successfully:")
        print(f"   Model ID: {model_id}")
        print(f"   File: {filename}")
        print(f"   Size: {file_size / 1024:.1f} KB")
        print(f"   Path: {file_path}")
        
        return model_id
    
    def load_model(self, model_id: str) -> Tuple[Any, ModelMetadata]:
        """
        Load a model by ID.
        
        Args:
            model_id: Model identifier
            
        Returns:
            Tuple of (model_object, metadata)
        """
        # Get metadata
        with sqlite3.connect(self.metadata_db) as conn:
            cursor = conn.execute(
                "SELECT * FROM model_metadata WHERE model_id = ?",
                (model_id,)
            )
            row = cursor.fetchone()
            
            if not row:
                raise ValueError(f"Model {model_id} not found")
            
            # Convert row to metadata object
            metadata = ModelMetadata(
                model_id=row[0],
                methodology=row[1],
                version=row[2],
                asset=row[3],
                created_at=row[4],
                file_path=row[5],
                file_size=row[6],
                model_config=json.loads(row[7]),
                performance_metrics=json.loads(row[8]),
                description=row[9],
                tags=json.loads(row[10])
            )
        
        # Load model file
        with open(metadata.file_path, 'rb') as f:
            model = pickle.load(f)
        
        return model, metadata
    
    def load_latest_model(self, methodology: str, asset: str) -> Tuple[Any, ModelMetadata]:
        """Load the latest version of a model for given methodology and asset."""
        with sqlite3.connect(self.metadata_db) as conn:
            cursor = conn.execute("""
                SELECT model_id FROM model_metadata 
                WHERE methodology = ? AND asset = ?
                ORDER BY version DESC LIMIT 1
            """, (methodology, asset.upper()))
            
            row = cursor.fetchone()
            if not row:
                raise ValueError(f"No models found for {methodology}/{asset}")
            
            return self.load_model(row[0])
    
    def list_models(self, 
                   methodology: str = None, 
                   asset: str = None,
                   limit: int = None) -> List[ModelMetadata]:
        """
        List models with optional filtering.
        
        Args:
            methodology: Filter by methodology
            asset: Filter by asset
            limit: Limit number of results
            
        Returns:
            List of model metadata
        """
        query = "SELECT * FROM model_metadata"
        params = []
        conditions = []
        
        if methodology:
            conditions.append("methodology = ?")
            params.append(methodology)
        
        if asset:
            conditions.append("asset = ?")
            params.append(asset.upper())
        
        if conditions:
            query += " WHERE " + " AND ".join(conditions)
        
        query += " ORDER BY created_at DESC"
        
        if limit:
            query += f" LIMIT {limit}"
        
        models = []
        with sqlite3.connect(self.metadata_db) as conn:
            cursor = conn.execute(query, params)
            for row in cursor.fetchall():
                metadata = ModelMetadata(
                    model_id=row[0],
                    methodology=row[1],
                    version=row[2],
                    asset=row[3],
                    created_at=row[4],
                    file_path=row[5],
                    file_size=row[6],
                    model_config=json.loads(row[7]),
                    performance_metrics=json.loads(row[8]),
                    description=row[9],
                    tags=json.loads(row[10])
                )
                models.append(metadata)
        
        return models
    
    def delete_model(self, model_id: str) -> bool:
        """
        Delete a model and its metadata.
        
        Args:
            model_id: Model identifier
            
        Returns:
            True if deleted successfully
        """
        # Get file path
        with sqlite3.connect(self.metadata_db) as conn:
            cursor = conn.execute(
                "SELECT file_path FROM model_metadata WHERE model_id = ?",
                (model_id,)
            )
            row = cursor.fetchone()
            
            if not row:
                return False
            
            file_path = Path(row[0])
            
            # Delete file if exists
            if file_path.exists():
                file_path.unlink()
            
            # Delete metadata
            conn.execute(
                "DELETE FROM model_metadata WHERE model_id = ?",
                (model_id,)
            )
        
        print(f"✅ Model {model_id} deleted successfully")
        return True
    
    def get_storage_stats(self) -> Dict[str, Any]:
        """Get storage statistics."""
        with sqlite3.connect(self.metadata_db) as conn:
            # Total models
            cursor = conn.execute("SELECT COUNT(*) FROM model_metadata")
            total_models = cursor.fetchone()[0]
            
            # Models by methodology
            cursor = conn.execute("""
                SELECT methodology, COUNT(*), AVG(file_size) 
                FROM model_metadata 
                GROUP BY methodology
            """)
            by_methodology = {row[0]: {"count": row[1], "avg_size": row[2]} 
                            for row in cursor.fetchall()}
            
            # Total storage used
            cursor = conn.execute("SELECT SUM(file_size) FROM model_metadata")
            total_size = cursor.fetchone()[0] or 0
            
            return {
                "total_models": total_models,
                "total_size_bytes": total_size,
                "total_size_mb": total_size / (1024 * 1024),
                "by_methodology": by_methodology
            }
    
    def print_storage_summary(self):
        """Print a summary of model storage."""
        stats = self.get_storage_stats()
        
        print("=" * 50)
        print("MODEL STORAGE SUMMARY")
        print("=" * 50)
        print(f"Total Models: {stats['total_models']}")
        print(f"Total Storage: {stats['total_size_mb']:.1f} MB")
        print()
        
        print("BY METHODOLOGY:")
        for method, data in stats['by_methodology'].items():
            avg_size_mb = data['avg_size'] / (1024 * 1024)
            print(f"  {method:12}: {data['count']:3} models, avg {avg_size_mb:.1f} MB")
        
        print("\nDIRECTORY STRUCTURE:")
        for methodology_dir in sorted(self.base_path.iterdir()):
            if methodology_dir.is_dir() and methodology_dir.name != '__pycache__':
                file_count = len(list(methodology_dir.glob("*.pkl")))
                print(f"  {methodology_dir.name:12}: {file_count} files")


def migrate_existing_models():
    """Migrate existing models to the new storage structure."""
    storage_manager = ModelStorageManager()
    
    # Look for existing models in the models/ directory
    old_models_dir = Path("/workspaces/unicorninvesting/BackendPython/unicorn/2_alpha_models/CRYPTO/ETH/models")
    
    if not old_models_dir.exists():
        print("No existing models directory found.")
        return
    
    migrations = []
    for model_file in old_models_dir.glob("*.pkl"):
        filename = model_file.name
        
        # Determine methodology from filename
        if "prophet" in filename.lower():
            methodology = "prophet"
        elif "xgboost" in filename.lower():
            methodology = "xgboost"
        elif "ensemble" in filename.lower():
            methodology = "ensemble"
        else:
            methodology = "unknown"
        
        # Load the model
        try:
            with open(model_file, 'rb') as f:
                model = pickle.load(f)
            
            # Create basic metadata
            model_config = {"migrated": True, "original_filename": filename}
            performance_metrics = {"migrated": True}
            description = f"Migrated from {filename}"
            
            # Store in new structure
            model_id = storage_manager.store_model(
                model=model,
                methodology=methodology,
                asset="ETH",
                model_config=model_config,
                performance_metrics=performance_metrics,
                description=description,
                variant="migrated",
                tags=["migrated", "legacy"]
            )
            
            migrations.append((filename, model_id))
            
        except Exception as e:
            print(f"❌ Failed to migrate {filename}: {e}")
    
    print(f"\n✅ Migrated {len(migrations)} models:")
    for old_name, new_id in migrations:
        print(f"  {old_name} → {new_id}")


if __name__ == "__main__":
    # Demonstrate the storage manager
    storage_manager = ModelStorageManager()
    
    print("ETH Model Storage Manager")
    print("=" * 40)
    
    # Show current storage stats
    storage_manager.print_storage_summary()
    
    # Offer to migrate existing models
    print("\nMigration available:")
    print("Run migrate_existing_models() to move old models to new structure")
