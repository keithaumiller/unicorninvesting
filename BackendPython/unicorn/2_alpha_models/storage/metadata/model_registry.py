"""
Model registry for centralized model management.

Provides storage, retrieval, and metadata management for trained models.
"""

from typing import Dict, Any, List, Optional, Union, Tuple
from pathlib import Path
import sqlite3
import json
import hashlib
import logging
from datetime import datetime, timedelta
import pickle
import joblib

from core.interfaces.model_interface import TrainedModel
from core.interfaces.data_interfaces import PerformanceMetrics

logger = logging.getLogger(__name__)

class ModelRegistry:
    """
    Centralized registry for managing trained models.
    
    Provides storage, retrieval, versioning, and metadata management
    for all trained models across methodologies and assets.
    """
    
    def __init__(self, registry_path: Optional[Union[str, Path]] = None):
        """
        Initialize model registry.
        
        Args:
            registry_path: Path to registry database (default: auto-detect)
        """
        if registry_path is None:
            current_dir = Path(__file__).parent
            self.registry_path = current_dir / "model_metadata.db"
            self.models_dir = current_dir.parent / "artifacts"
        else:
            self.registry_path = Path(registry_path)
            self.models_dir = self.registry_path.parent / "artifacts"
        
        # Create directories
        self.registry_path.parent.mkdir(parents=True, exist_ok=True)
        self.models_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize database
        self._init_database()
        
        logger.info(f"ModelRegistry initialized: {self.registry_path}")
    
    def _init_database(self):
        """Initialize SQLite database with required tables"""
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            # Models table
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS models (
                    model_id TEXT PRIMARY KEY,
                    methodology TEXT NOT NULL,
                    asset_class TEXT NOT NULL,
                    symbol TEXT NOT NULL,
                    version TEXT NOT NULL,
                    created_at TIMESTAMP NOT NULL,
                    updated_at TIMESTAMP NOT NULL,
                    is_active BOOLEAN DEFAULT TRUE,
                    file_path TEXT NOT NULL,
                    file_size_bytes INTEGER,
                    checksum TEXT,
                    metadata_json TEXT,
                    performance_json TEXT,
                    training_config_json TEXT,
                    notes TEXT
                )
            """)
            
            # Model versions table
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS model_versions (
                    version_id TEXT PRIMARY KEY,
                    model_id TEXT NOT NULL,
                    version_number INTEGER NOT NULL,
                    created_at TIMESTAMP NOT NULL,
                    performance_metrics TEXT,
                    changelog TEXT,
                    is_production BOOLEAN DEFAULT FALSE,
                    FOREIGN KEY (model_id) REFERENCES models (model_id)
                )
            """)
            
            # Model performance tracking
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS model_performance (
                    performance_id TEXT PRIMARY KEY,
                    model_id TEXT NOT NULL,
                    evaluation_date TIMESTAMP NOT NULL,
                    evaluation_period_start TIMESTAMP,
                    evaluation_period_end TIMESTAMP,
                    r2_score REAL,
                    mape REAL,
                    mae REAL,
                    rmse REAL,
                    sharpe_ratio REAL,
                    max_drawdown REAL,
                    hit_rate REAL,
                    additional_metrics_json TEXT,
                    FOREIGN KEY (model_id) REFERENCES models (model_id)
                )
            """)
            
            # Create indexes
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_models_methodology ON models (methodology)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_models_asset ON models (asset_class, symbol)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_models_active ON models (is_active)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_performance_model ON model_performance (model_id)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_performance_date ON model_performance (evaluation_date)")
            
            conn.commit()
    
    def store_model(self, model: TrainedModel, methodology: str, asset_class: str, 
                   symbol: str, performance: PerformanceMetrics, 
                   config: Dict[str, Any], notes: str = "") -> str:
        """
        Store a trained model in the registry.
        
        Args:
            model: Trained model instance
            methodology: Methodology name
            asset_class: Asset class
            symbol: Asset symbol
            performance: Performance metrics
            config: Training configuration
            notes: Optional notes
            
        Returns:
            Model ID
        """
        # Generate model ID
        model_id = self._generate_model_id(methodology, asset_class, symbol)
        
        # Create model file path
        model_filename = f"{model_id}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.joblib"
        model_file_path = self.models_dir / methodology / f"{asset_class}_{symbol}" / model_filename
        model_file_path.parent.mkdir(parents=True, exist_ok=True)
        
        try:
            # Save model to disk
            model.save_model(model_file_path, format_type='joblib')
            
            # Calculate file metadata
            file_size = model_file_path.stat().st_size
            checksum = self._calculate_checksum(model_file_path)
            
            # Store in database
            with sqlite3.connect(self.registry_path) as conn:
                cursor = conn.cursor()
                
                now = datetime.now()
                
                cursor.execute("""
                    INSERT OR REPLACE INTO models (
                        model_id, methodology, asset_class, symbol, version,
                        created_at, updated_at, is_active, file_path,
                        file_size_bytes, checksum, metadata_json,
                        performance_json, training_config_json, notes
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    model_id, methodology, asset_class, symbol, model.version,
                    now, now, True, str(model_file_path), file_size, checksum,
                    json.dumps(model.get_metadata()),
                    json.dumps(performance.__dict__),
                    json.dumps(config),
                    notes
                ))
                
                # Store performance metrics
                self._store_performance_metrics(cursor, model_id, performance)
                
                conn.commit()
            
            logger.info(f"Stored model {model_id} at {model_file_path}")
            return model_id
            
        except Exception as e:
            logger.error(f"Failed to store model {model_id}: {e}")
            # Clean up file if database storage failed
            if model_file_path.exists():
                model_file_path.unlink()
            raise
    
    def _store_performance_metrics(self, cursor: sqlite3.Cursor, model_id: str, 
                                 performance: PerformanceMetrics):
        """Store performance metrics in database"""
        performance_id = f"{model_id}_perf_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        cursor.execute("""
            INSERT INTO model_performance (
                performance_id, model_id, evaluation_date,
                evaluation_period_start, evaluation_period_end,
                r2_score, mape, mae, rmse, sharpe_ratio,
                max_drawdown, hit_rate, additional_metrics_json
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            performance_id, model_id, datetime.now(),
            performance.evaluation_period.get('start'),
            performance.evaluation_period.get('end'),
            performance.r2_score, performance.mape, performance.mae, performance.rmse,
            performance.sharpe_ratio, performance.max_drawdown, performance.hit_rate,
            json.dumps(performance.additional_metrics)
        ))
    
    def get_model(self, model_id: str) -> Optional[TrainedModel]:
        """
        Retrieve a model by ID.
        
        Args:
            model_id: Model identifier
            
        Returns:
            TrainedModel instance or None if not found
        """
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                SELECT file_path, methodology, asset_class, symbol, checksum
                FROM models 
                WHERE model_id = ? AND is_active = TRUE
            """, (model_id,))
            
            result = cursor.fetchone()
            
            if result:
                file_path, methodology, asset_class, symbol, expected_checksum = result
                file_path = Path(file_path)
                
                if file_path.exists():
                    # Verify checksum
                    actual_checksum = self._calculate_checksum(file_path)
                    if actual_checksum != expected_checksum:
                        logger.warning(f"Checksum mismatch for model {model_id}")
                    
                    # Load model
                    try:
                        # Create empty model instance
                        model = TrainedModel(methodology, asset_class, symbol)
                        model.load_model(file_path, format_type='joblib')
                        model.model_id = model_id
                        
                        return model
                    except Exception as e:
                        logger.error(f"Failed to load model {model_id}: {e}")
                        return None
                else:
                    logger.error(f"Model file not found: {file_path}")
                    return None
        
        return None
    
    def get_models(self, asset_class: str = None, symbol: str = None, 
                  methodology: str = None, active_only: bool = True) -> Dict[str, Dict[str, Any]]:
        """
        Get models matching criteria.
        
        Args:
            asset_class: Filter by asset class
            symbol: Filter by symbol
            methodology: Filter by methodology
            active_only: Only return active models
            
        Returns:
            Dictionary of model metadata
        """
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            # Build query
            conditions = []
            params = []
            
            if asset_class:
                conditions.append("asset_class = ?")
                params.append(asset_class)
            
            if symbol:
                conditions.append("symbol = ?")
                params.append(symbol)
            
            if methodology:
                conditions.append("methodology = ?")
                params.append(methodology)
            
            if active_only:
                conditions.append("is_active = TRUE")
            
            where_clause = " AND ".join(conditions) if conditions else "1=1"
            
            cursor.execute(f"""
                SELECT model_id, methodology, asset_class, symbol, version,
                       created_at, updated_at, file_path, metadata_json,
                       performance_json, training_config_json
                FROM models 
                WHERE {where_clause}
                ORDER BY updated_at DESC
            """, params)
            
            results = {}
            for row in cursor.fetchall():
                model_id = row[0]
                results[model_id] = {
                    'model_id': model_id,
                    'methodology': row[1],
                    'asset_class': row[2],
                    'symbol': row[3],
                    'version': row[4],
                    'created_at': row[5],
                    'updated_at': row[6],
                    'file_path': row[7],
                    'metadata': json.loads(row[8]) if row[8] else {},
                    'performance': json.loads(row[9]) if row[9] else {},
                    'training_config': json.loads(row[10]) if row[10] else {}
                }
            
            return results
    
    def get_best_model(self, asset_class: str, symbol: str, 
                      methodology: str = None, metric: str = 'r2_score') -> Optional[str]:
        """
        Get best performing model for asset.
        
        Args:
            asset_class: Asset class
            symbol: Asset symbol
            methodology: Filter by methodology (optional)
            metric: Performance metric to optimize
            
        Returns:
            Model ID of best model or None
        """
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            # Build query based on metric
            if metric in ['r2_score', 'sharpe_ratio', 'hit_rate']:
                order = "DESC"  # Higher is better
            else:
                order = "ASC"   # Lower is better (mape, mae, rmse, max_drawdown)
            
            conditions = ["m.asset_class = ?", "m.symbol = ?", "m.is_active = TRUE"]
            params = [asset_class, symbol]
            
            if methodology:
                conditions.append("m.methodology = ?")
                params.append(methodology)
            
            where_clause = " AND ".join(conditions)
            
            cursor.execute(f"""
                SELECT m.model_id, p.{metric}
                FROM models m
                JOIN model_performance p ON m.model_id = p.model_id
                WHERE {where_clause} AND p.{metric} IS NOT NULL
                ORDER BY p.{metric} {order}
                LIMIT 1
            """, params)
            
            result = cursor.fetchone()
            return result[0] if result else None
    
    def deactivate_model(self, model_id: str, reason: str = ""):
        """
        Deactivate a model.
        
        Args:
            model_id: Model identifier
            reason: Reason for deactivation
        """
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute("""
                UPDATE models 
                SET is_active = FALSE, updated_at = ?, notes = ?
                WHERE model_id = ?
            """, (datetime.now(), reason, model_id))
            
            conn.commit()
        
        logger.info(f"Deactivated model {model_id}: {reason}")
    
    def delete_model(self, model_id: str, delete_file: bool = True):
        """
        Delete a model from registry.
        
        Args:
            model_id: Model identifier
            delete_file: Whether to delete the model file
        """
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            # Get file path
            if delete_file:
                cursor.execute("SELECT file_path FROM models WHERE model_id = ?", (model_id,))
                result = cursor.fetchone()
                if result:
                    file_path = Path(result[0])
                    if file_path.exists():
                        file_path.unlink()
                        logger.info(f"Deleted model file: {file_path}")
            
            # Delete from database
            cursor.execute("DELETE FROM model_performance WHERE model_id = ?", (model_id,))
            cursor.execute("DELETE FROM model_versions WHERE model_id = ?", (model_id,))
            cursor.execute("DELETE FROM models WHERE model_id = ?", (model_id,))
            
            conn.commit()
        
        logger.info(f"Deleted model {model_id} from registry")
    
    def get_registry_stats(self) -> Dict[str, Any]:
        """
        Get registry statistics.
        
        Returns:
            Dictionary with registry statistics
        """
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            # Model counts
            cursor.execute("SELECT COUNT(*) FROM models WHERE is_active = TRUE")
            active_models = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(*) FROM models")
            total_models = cursor.fetchone()[0]
            
            # By methodology
            cursor.execute("""
                SELECT methodology, COUNT(*) 
                FROM models 
                WHERE is_active = TRUE 
                GROUP BY methodology
            """)
            by_methodology = dict(cursor.fetchall())
            
            # By asset class
            cursor.execute("""
                SELECT asset_class, COUNT(*) 
                FROM models 
                WHERE is_active = TRUE 
                GROUP BY asset_class
            """)
            by_asset_class = dict(cursor.fetchall())
            
            # Storage usage
            cursor.execute("SELECT SUM(file_size_bytes) FROM models WHERE is_active = TRUE")
            storage_bytes = cursor.fetchone()[0] or 0
            
            return {
                'active_models': active_models,
                'total_models': total_models,
                'by_methodology': by_methodology,
                'by_asset_class': by_asset_class,
                'storage_mb': storage_bytes / (1024 * 1024),
                'registry_path': str(self.registry_path),
                'models_directory': str(self.models_dir)
            }
    
    def cleanup_old_models(self, days_old: int = 30, keep_best: bool = True):
        """
        Clean up old models.
        
        Args:
            days_old: Remove models older than this many days
            keep_best: Keep best performing model for each asset/methodology
        """
        cutoff_date = datetime.now() - timedelta(days=days_old)
        
        with sqlite3.connect(self.registry_path) as conn:
            cursor = conn.cursor()
            
            if keep_best:
                # Get best models to preserve
                cursor.execute("""
                    SELECT DISTINCT m1.model_id
                    FROM models m1
                    JOIN model_performance p1 ON m1.model_id = p1.model_id
                    WHERE NOT EXISTS (
                        SELECT 1 
                        FROM models m2
                        JOIN model_performance p2 ON m2.model_id = p2.model_id
                        WHERE m2.asset_class = m1.asset_class 
                        AND m2.symbol = m1.symbol
                        AND m2.methodology = m1.methodology
                        AND m2.is_active = TRUE
                        AND p2.r2_score > p1.r2_score
                    )
                """)
                
                best_models = [row[0] for row in cursor.fetchall()]
                best_models_placeholders = ','.join(['?'] * len(best_models))
                
                # Find old models excluding best ones
                cursor.execute(f"""
                    SELECT model_id, file_path 
                    FROM models 
                    WHERE created_at < ? 
                    AND model_id NOT IN ({best_models_placeholders})
                """, [cutoff_date] + best_models)
            else:
                # Find all old models
                cursor.execute("""
                    SELECT model_id, file_path 
                    FROM models 
                    WHERE created_at < ?
                """, (cutoff_date,))
            
            old_models = cursor.fetchall()
            
            # Delete old models
            for model_id, file_path in old_models:
                try:
                    self.delete_model(model_id, delete_file=True)
                    logger.info(f"Cleaned up old model: {model_id}")
                except Exception as e:
                    logger.error(f"Failed to clean up model {model_id}: {e}")
        
        logger.info(f"Cleanup completed: removed {len(old_models)} old models")
    
    def _generate_model_id(self, methodology: str, asset_class: str, symbol: str) -> str:
        """Generate unique model ID"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        base_id = f"{methodology}_{asset_class}_{symbol}_{timestamp}"
        
        # Add hash for uniqueness
        hash_input = f"{base_id}_{datetime.now().microsecond}"
        hash_suffix = hashlib.md5(hash_input.encode()).hexdigest()[:8]
        
        return f"{base_id}_{hash_suffix}"
    
    def _calculate_checksum(self, file_path: Path) -> str:
        """Calculate file checksum"""
        hash_md5 = hashlib.md5()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_md5.update(chunk)
        return hash_md5.hexdigest()
    
    def __str__(self) -> str:
        """String representation of model registry"""
        return f"ModelRegistry({self.registry_path})"
    
    def __repr__(self) -> str:
        """Detailed representation of model registry"""
        stats = self.get_registry_stats()
        return (f"ModelRegistry(path='{self.registry_path}', "
                f"active_models={stats['active_models']}, "
                f"storage_mb={stats['storage_mb']:.1f})")