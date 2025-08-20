"""
Neural Network Models for Portfolio Optimization and Financial Prediction.

This module implements:
- Feed-forward neural networks for portfolio allocation
- LSTM networks for time series prediction
- Custom loss functions for financial objectives
- Model training and evaluation pipelines

Migrated from: BackendPython/recomendationsystems/1_modeltrainer_FCNN4R.R
"""

import numpy as np
import pandas as pd
import logging
from typing import List, Dict, Tuple, Optional, Callable, Any
from dataclasses import dataclass
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers, optimizers, callbacks
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.model_selection import train_test_split
import joblib
import os
from datetime import datetime

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class ModelConfig:
    """Configuration for neural network models."""
    # Architecture
    hidden_layers: List[int] = None
    dropout_rate: float = 0.2
    activation: str = 'relu'
    output_activation: str = 'softmax'
    
    # Training
    learning_rate: float = 0.001
    batch_size: int = 32
    epochs: int = 100
    validation_split: float = 0.2
    early_stopping_patience: int = 20
    
    # Regularization
    l1_reg: float = 0.0
    l2_reg: float = 0.001
    
    # Data preprocessing
    scaler_type: str = 'standard'  # 'standard', 'minmax', 'none'
    
    # Model saving
    model_dir: str = 'models'
    save_best_only: bool = True
    
    def __post_init__(self):
        if self.hidden_layers is None:
            self.hidden_layers = [64, 32, 16]

class PortfolioNeuralNetwork:
    """Neural network for portfolio allocation optimization."""
    
    def __init__(self, config: ModelConfig = None):
        """Initialize with configuration."""
        self.config = config or ModelConfig()
        self.model = None
        self.scaler_X = None
        self.scaler_y = None
        self.feature_names = None
        self.training_history = None
    
    def _create_portfolio_loss(self, lambda_reg: float = 0.01) -> Callable:
        """
        Create custom loss function for portfolio optimization.
        
        Args:
            lambda_reg: Regularization parameter for allocation constraints
            
        Returns:
            Custom loss function
        """
        def portfolio_loss(y_true, y_pred):
            # Mean squared error for allocation targets
            mse_loss = tf.keras.losses.MeanSquaredError()(y_true, y_pred)
            
            # Constraint: allocations should sum to 1
            allocation_sum = tf.reduce_sum(y_pred, axis=1)
            sum_constraint = tf.reduce_mean(tf.square(allocation_sum - 1.0))
            
            # Constraint: no negative allocations (handled by softmax activation)
            # But we can add penalty for extreme allocations
            concentration_penalty = tf.reduce_mean(tf.reduce_max(y_pred, axis=1))
            
            # Total loss
            total_loss = mse_loss + lambda_reg * sum_constraint + 0.1 * concentration_penalty
            
            return total_loss
        
        return portfolio_loss
    
    def build_model(self, input_dim: int, output_dim: int) -> keras.Model:
        """
        Build neural network architecture.
        
        Args:
            input_dim: Number of input features
            output_dim: Number of portfolio assets
            
        Returns:
            Compiled Keras model
        """
        try:
            # Input layer
            inputs = keras.Input(shape=(input_dim,), name='features')
            x = inputs
            
            # Hidden layers
            for i, units in enumerate(self.config.hidden_layers):
                x = layers.Dense(
                    units,
                    activation=self.config.activation,
                    kernel_regularizer=keras.regularizers.L1L2(
                        l1=self.config.l1_reg,
                        l2=self.config.l2_reg
                    ),
                    name=f'hidden_{i+1}'
                )(x)
                
                # Dropout for regularization
                if self.config.dropout_rate > 0:
                    x = layers.Dropout(self.config.dropout_rate)(x)
            
            # Output layer (portfolio allocations)
            outputs = layers.Dense(
                output_dim,
                activation=self.config.output_activation,
                name='allocations'
            )(x)
            
            # Create model
            model = keras.Model(inputs=inputs, outputs=outputs, name='portfolio_nn')
            
            # Compile model
            optimizer = optimizers.Adam(learning_rate=self.config.learning_rate)
            
            model.compile(
                optimizer=optimizer,
                loss=self._create_portfolio_loss(),
                metrics=['mae', 'mse']
            )
            
            logger.info(f"Built neural network with {input_dim} inputs and {output_dim} outputs")
            logger.info(f"Architecture: {self.config.hidden_layers}")
            
            return model
            
        except Exception as e:
            logger.error(f"Error building model: {e}")
            raise
    
    def prepare_data(self, X: np.ndarray, y: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Prepare and scale training data.
        
        Args:
            X: Feature data
            y: Target allocations
            
        Returns:
            Tuple of scaled (X, y)
        """
        try:
            # Scale features
            if self.config.scaler_type == 'standard':
                self.scaler_X = StandardScaler()
            elif self.config.scaler_type == 'minmax':
                self.scaler_X = MinMaxScaler()
            else:
                self.scaler_X = None
            
            if self.scaler_X is not None:
                X_scaled = self.scaler_X.fit_transform(X)
            else:
                X_scaled = X.copy()
            
            # For portfolio allocations, we typically don't scale y
            # since they should sum to 1 and be between 0 and 1
            y_scaled = y.copy()
            
            # Ensure allocations sum to 1
            row_sums = np.sum(y_scaled, axis=1, keepdims=True)
            row_sums[row_sums == 0] = 1  # Avoid division by zero
            y_scaled = y_scaled / row_sums
            
            logger.info(f"Prepared data: X shape {X_scaled.shape}, y shape {y_scaled.shape}")
            
            return X_scaled, y_scaled
            
        except Exception as e:
            logger.error(f"Error preparing data: {e}")
            raise
    
    def train(self, X: np.ndarray, y: np.ndarray, 
              feature_names: List[str] = None) -> Dict[str, Any]:
        """
        Train the neural network model.
        
        Args:
            X: Feature data
            y: Target allocations
            feature_names: Names of features
            
        Returns:
            Training history and metrics
        """
        try:
            self.feature_names = feature_names
            
            # Prepare data
            X_scaled, y_scaled = self.prepare_data(X, y)
            
            # Split into train/validation
            X_train, X_val, y_train, y_val = train_test_split(
                X_scaled, y_scaled,
                test_size=self.config.validation_split,
                random_state=42,
                shuffle=False  # Keep temporal order for financial data
            )
            
            # Build model
            self.model = self.build_model(X_scaled.shape[1], y_scaled.shape[1])
            
            # Set up callbacks
            callbacks_list = self._create_callbacks()
            
            # Train model
            logger.info("Starting model training...")
            
            history = self.model.fit(
                X_train, y_train,
                batch_size=self.config.batch_size,
                epochs=self.config.epochs,
                validation_data=(X_val, y_val),
                callbacks=callbacks_list,
                verbose=1
            )
            
            self.training_history = history.history
            
            # Evaluate final performance
            train_loss = self.model.evaluate(X_train, y_train, verbose=0)
            val_loss = self.model.evaluate(X_val, y_val, verbose=0)
            
            results = {
                'training_history': self.training_history,
                'final_train_loss': train_loss,
                'final_val_loss': val_loss,
                'model_architecture': self.model.summary(),
                'feature_names': self.feature_names
            }
            
            logger.info(f"Training completed. Final validation loss: {val_loss[0]:.6f}")
            
            return results
            
        except Exception as e:
            logger.error(f"Error training model: {e}")
            raise
    
    def _create_callbacks(self) -> List[callbacks.Callback]:
        """Create training callbacks."""
        callback_list = []
        
        # Early stopping
        early_stopping = callbacks.EarlyStopping(
            monitor='val_loss',
            patience=self.config.early_stopping_patience,
            restore_best_weights=True,
            verbose=1
        )
        callback_list.append(early_stopping)
        
        # Learning rate reduction
        lr_scheduler = callbacks.ReduceLROnPlateau(
            monitor='val_loss',
            factor=0.5,
            patience=10,
            min_lr=1e-7,
            verbose=1
        )
        callback_list.append(lr_scheduler)
        
        # Model checkpointing
        os.makedirs(self.config.model_dir, exist_ok=True)
        checkpoint_path = os.path.join(
            self.config.model_dir,
            f'portfolio_model_{datetime.now().strftime("%Y%m%d_%H%M%S")}.h5'
        )
        
        checkpoint = callbacks.ModelCheckpoint(
            checkpoint_path,
            monitor='val_loss',
            save_best_only=self.config.save_best_only,
            verbose=1
        )
        callback_list.append(checkpoint)
        
        return callback_list
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        """
        Make predictions with the trained model.
        
        Args:
            X: Feature data
            
        Returns:
            Predicted allocations
        """
        try:
            if self.model is None:
                raise ValueError("Model not trained yet")
            
            # Scale input data
            if self.scaler_X is not None:
                X_scaled = self.scaler_X.transform(X)
            else:
                X_scaled = X.copy()
            
            # Make predictions
            predictions = self.model.predict(X_scaled)
            
            # Ensure allocations sum to 1 (softmax should handle this, but double-check)
            row_sums = np.sum(predictions, axis=1, keepdims=True)
            predictions = predictions / row_sums
            
            return predictions
            
        except Exception as e:
            logger.error(f"Error making predictions: {e}")
            raise
    
    def save_model(self, filepath: str = None) -> str:
        """
        Save the trained model and scalers.
        
        Args:
            filepath: Path to save the model
            
        Returns:
            Path where model was saved
        """
        try:
            if self.model is None:
                raise ValueError("No model to save")
            
            if filepath is None:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                filepath = os.path.join(self.config.model_dir, f'portfolio_model_{timestamp}')
            
            os.makedirs(os.path.dirname(filepath), exist_ok=True)
            
            # Save model architecture and weights
            self.model.save(f'{filepath}.h5')
            
            # Save scalers
            if self.scaler_X is not None:
                joblib.dump(self.scaler_X, f'{filepath}_scaler_X.pkl')
            
            # Save metadata
            metadata = {
                'config': self.config.__dict__,
                'feature_names': self.feature_names,
                'training_history': self.training_history
            }
            
            import json
            with open(f'{filepath}_metadata.json', 'w') as f:
                json.dump(metadata, f, indent=2, default=str)
            
            logger.info(f"Model saved to {filepath}")
            
            return filepath
            
        except Exception as e:
            logger.error(f"Error saving model: {e}")
            raise
    
    def load_model(self, filepath: str) -> None:
        """
        Load a previously saved model.
        
        Args:
            filepath: Path to the saved model
        """
        try:
            # Load model
            self.model = keras.models.load_model(
                f'{filepath}.h5',
                custom_objects={'portfolio_loss': self._create_portfolio_loss()}
            )
            
            # Load scalers
            scaler_path = f'{filepath}_scaler_X.pkl'
            if os.path.exists(scaler_path):
                self.scaler_X = joblib.load(scaler_path)
            
            # Load metadata
            metadata_path = f'{filepath}_metadata.json'
            if os.path.exists(metadata_path):
                import json
                with open(metadata_path, 'r') as f:
                    metadata = json.load(f)
                
                self.feature_names = metadata.get('feature_names')
                self.training_history = metadata.get('training_history')
            
            logger.info(f"Model loaded from {filepath}")
            
        except Exception as e:
            logger.error(f"Error loading model: {e}")
            raise

class TimeSeriesLSTM:
    """LSTM neural network for time series prediction."""
    
    def __init__(self, config: ModelConfig = None):
        """Initialize LSTM model."""
        self.config = config or ModelConfig()
        self.model = None
        self.scaler = None
        self.sequence_length = 60  # Default lookback period
    
    def build_lstm_model(self, input_shape: Tuple[int, int], output_dim: int) -> keras.Model:
        """
        Build LSTM architecture for time series prediction.
        
        Args:
            input_shape: (sequence_length, features)
            output_dim: Number of outputs
            
        Returns:
            Compiled LSTM model
        """
        try:
            inputs = keras.Input(shape=input_shape, name='sequences')
            
            # LSTM layers
            x = layers.LSTM(
                64,
                return_sequences=True,
                dropout=self.config.dropout_rate,
                recurrent_dropout=self.config.dropout_rate
            )(inputs)
            
            x = layers.LSTM(
                32,
                return_sequences=False,
                dropout=self.config.dropout_rate,
                recurrent_dropout=self.config.dropout_rate
            )(x)
            
            # Dense layers
            x = layers.Dense(16, activation=self.config.activation)(x)
            x = layers.Dropout(self.config.dropout_rate)(x)
            
            outputs = layers.Dense(output_dim, activation='linear')(x)
            
            model = keras.Model(inputs=inputs, outputs=outputs, name='lstm_predictor')
            
            model.compile(
                optimizer=optimizers.Adam(learning_rate=self.config.learning_rate),
                loss='mse',
                metrics=['mae']
            )
            
            return model
            
        except Exception as e:
            logger.error(f"Error building LSTM model: {e}")
            raise
    
    def create_sequences(self, data: np.ndarray, 
                        target_data: np.ndarray = None) -> Tuple[np.ndarray, np.ndarray]:
        """
        Create sequences for LSTM training.
        
        Args:
            data: Input time series data
            target_data: Target data (if None, uses data for prediction)
            
        Returns:
            Tuple of (sequences, targets)
        """
        try:
            if target_data is None:
                target_data = data
            
            X, y = [], []
            
            for i in range(self.sequence_length, len(data)):
                X.append(data[i-self.sequence_length:i])
                y.append(target_data[i])
            
            return np.array(X), np.array(y)
            
        except Exception as e:
            logger.error(f"Error creating sequences: {e}")
            raise

# Example usage and model evaluation functions
def evaluate_portfolio_model(model: PortfolioNeuralNetwork, 
                           X_test: np.ndarray, 
                           y_test: np.ndarray,
                           returns_data: np.ndarray) -> Dict[str, float]:
    """
    Evaluate portfolio model performance.
    
    Args:
        model: Trained portfolio model
        X_test: Test features
        y_test: Test targets
        returns_data: Historical returns for portfolio simulation
        
    Returns:
        Dictionary of performance metrics
    """
    try:
        # Make predictions
        predictions = model.predict(X_test)
        
        # Calculate portfolio returns for predictions and targets
        pred_returns = np.sum(predictions * returns_data[-len(predictions):], axis=1)
        true_returns = np.sum(y_test * returns_data[-len(y_test):], axis=1)
        
        # Calculate metrics
        pred_sharpe = np.mean(pred_returns) / np.std(pred_returns) if np.std(pred_returns) > 0 else 0
        true_sharpe = np.mean(true_returns) / np.std(true_returns) if np.std(true_returns) > 0 else 0
        
        pred_total_return = np.prod(1 + pred_returns) - 1
        true_total_return = np.prod(1 + true_returns) - 1
        
        allocation_error = np.mean(np.abs(predictions - y_test))
        
        metrics = {
            'predicted_sharpe_ratio': pred_sharpe,
            'true_sharpe_ratio': true_sharpe,
            'predicted_total_return': pred_total_return,
            'true_total_return': true_total_return,
            'mean_allocation_error': allocation_error,
            'sharpe_ratio_difference': abs(pred_sharpe - true_sharpe)
        }
        
        return metrics
        
    except Exception as e:
        logger.error(f"Error evaluating portfolio model: {e}")
        return {}

# Example usage
if __name__ == "__main__":
    # Example usage
    config = ModelConfig(
        hidden_layers=[64, 32, 16],
        learning_rate=0.001,
        epochs=50
    )
    
    # Create sample data
    n_samples, n_features, n_assets = 1000, 20, 10
    X = np.random.normal(0, 1, (n_samples, n_features))
    
    # Create sample target allocations
    y = np.random.dirichlet(np.ones(n_assets), n_samples)
    
    # Train model
    model = PortfolioNeuralNetwork(config)
    results = model.train(X, y)
    
    print(f"Training completed with final validation loss: {results['final_val_loss'][0]:.6f}")
    
    # Make predictions
    predictions = model.predict(X[:5])
    print(f"Sample predictions shape: {predictions.shape}")
    print(f"Sample allocation sums: {np.sum(predictions, axis=1)}")
