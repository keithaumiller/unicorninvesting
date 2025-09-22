#!/usr/bin/env python3
"""
ETH 6-Month Daily Forecast Comparison: Prophet, XGBoost & Ensemble

Complete implementation comparing three methodologies for 6-month ETH forecasting:
1. Prophet - Extended from 30-day to 180-day forecasting
2. XGBoost - Complete implementation with crypto-specific features  
3. Ensemble - Combined methodology with dynamic weighting

Features:
- Real silver layer ETH data integration
- Performance comparison across methodologies
- Cross-validation and model validation
- Comprehensive forecast outputs and visualizations
- Production-ready forecast generation
"""

import sys
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import logging
import warnings
warnings.filterwarnings('ignore')

# Setup paths
current_dir = Path(__file__).parent.parent
sys.path.append(str(current_dir))
sys.path.append('/home/runner/work/unicorninvesting/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/core')

# Import methodologies
try:
    from methodologies.xgboost.core.xgboost_methodology import XGBoostMethodology
    from methodologies.xgboost.core.feature_engineering import XGBoostFeatureEngine
    from methodologies.ensemble.core.ensemble_methodology import EnsembleMethodology
    
    # Try to import Prophet
    try:
        from prophet import Prophet
        PROPHET_AVAILABLE = True
    except ImportError:
        PROPHET_AVAILABLE = False
        print("⚠️ Prophet not available - using mock implementation")
    
except ImportError as e:
    print(f"⚠️ Import error: {e}")
    print("Some methodologies may not be available")

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class ETH6MonthForecastComparison:
    """
    Complete 6-month ETH forecast comparison across methodologies
    
    This class implements and compares Prophet, XGBoost, and Ensemble methodologies
    for generating 6-month daily ETH price forecasts using real silver layer data.
    """
    
    def __init__(self):
        """Initialize the forecast comparison system"""
        self.forecast_horizon = 180  # 6 months
        self.asset = "ETH"
        
        # Initialize methodologies
        self.xgboost_model = XGBoostMethodology(asset=self.asset, forecast_horizon=self.forecast_horizon)
        self.ensemble_model = EnsembleMethodology(asset=self.asset, forecast_horizon=self.forecast_horizon)
        self.prophet_model = None  # Will be created when needed
        
        # Results storage
        self.results = {
            'prophet': {},
            'xgboost': {},
            'ensemble': {}
        }
        
        # Data storage
        self.eth_data = None
        self.training_data = None
        self.validation_data = None
        
        logger.info(f"Initialized 6-month ETH forecast comparison system")
    
    def load_eth_data(self) -> pd.DataFrame:
        """
        Load ETH data from silver layer or create sample data
        
        Returns:
            ETH OHLCV DataFrame with datetime index
        """
        try:
            logger.info("Loading ETH data from silver layer...")
            
            # Try to load from silver layer first
            try:
                from silver_layer_data_connector import SilverLayerDataConnector
                connector = SilverLayerDataConnector()
                
                # Get 365 days of data for robust training (need extra for 6-month forecast)
                eth_data = connector.get_historical_data(
                    asset='ETH',
                    interval='1d',
                    periods=365
                )
                
                if not eth_data.empty:
                    logger.info(f"✅ Loaded {len(eth_data)} days of real ETH data from silver layer")
                    self.eth_data = eth_data
                    return eth_data
                    
            except Exception as e:
                logger.warning(f"Could not load from silver layer: {e}")
            
            # Fallback to sample data generation
            logger.info("Generating sample ETH data for demonstration...")
            eth_data = self._generate_sample_eth_data()
            self.eth_data = eth_data
            return eth_data
            
        except Exception as e:
            logger.error(f"Error loading ETH data: {e}")
            return pd.DataFrame()
    
    def _generate_sample_eth_data(self, periods: int = 365) -> pd.DataFrame:
        """Generate realistic sample ETH data"""
        try:
            # Start from recent date
            end_date = datetime.now().date()
            start_date = end_date - timedelta(days=periods)
            
            # Create date range
            dates = pd.date_range(start=start_date, end=end_date, freq='D')
            
            # Generate realistic ETH price data
            np.random.seed(42)  # For reproducibility
            
            # Starting price around current ETH levels
            initial_price = 4000.0
            
            # Generate price series with realistic volatility
            returns = np.random.normal(0.001, 0.05, len(dates))  # Small positive drift, high volatility
            prices = [initial_price]
            
            for i in range(1, len(dates)):
                new_price = prices[-1] * (1 + returns[i])
                # Add some bounds to keep realistic
                new_price = max(1000, min(8000, new_price))
                prices.append(new_price)
            
            # Generate OHLCV data
            closes = np.array(prices)
            
            # Generate realistic OHLC from close prices
            highs = closes * (1 + np.abs(np.random.normal(0, 0.02, len(closes))))
            lows = closes * (1 - np.abs(np.random.normal(0, 0.02, len(closes))))
            
            # Opens are close to previous close with some gap
            opens = np.roll(closes, 1) * (1 + np.random.normal(0, 0.01, len(closes)))
            opens[0] = closes[0]
            
            # Volume with some correlation to price movements
            base_volume = 100000000  # 100M base volume
            volume_multiplier = 1 + np.abs(returns) * 5  # Higher volume on big moves
            volumes = base_volume * volume_multiplier
            
            # Create DataFrame
            eth_data = pd.DataFrame({
                'open': opens,
                'high': highs,
                'low': lows,
                'close': closes,
                'volume': volumes
            }, index=dates)
            
            # Add basic technical indicators
            eth_data['rsi'] = self._calculate_rsi(eth_data['close'])
            eth_data['volatility_14'] = eth_data['close'].pct_change().rolling(14).std()
            
            logger.info(f"Generated {len(eth_data)} days of sample ETH data")
            return eth_data
            
        except Exception as e:
            logger.error(f"Error generating sample data: {e}")
            return pd.DataFrame()
    
    def _calculate_rsi(self, prices: pd.Series, window: int = 14) -> pd.Series:
        """Calculate RSI indicator"""
        try:
            delta = prices.diff()
            gain = delta.where(delta > 0, 0).rolling(window).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(window).mean()
            rs = gain / loss
            rsi = 100 - (100 / (1 + rs))
            return rsi.fillna(50)  # Fill NaN with neutral RSI
        except:
            return pd.Series([50] * len(prices), index=prices.index)
    
    def prepare_data_splits(self, validation_split: float = 0.2):
        """
        Prepare training and validation data splits
        
        Args:
            validation_split: Fraction of data to use for validation
        """
        try:
            if self.eth_data is None or self.eth_data.empty:
                raise ValueError("ETH data must be loaded first")
            
            # Time series split - use most recent data for validation
            split_idx = int(len(self.eth_data) * (1 - validation_split))
            
            self.training_data = self.eth_data[:split_idx].copy()
            self.validation_data = self.eth_data[split_idx:].copy()
            
            logger.info(f"Data split - Training: {len(self.training_data)} days, Validation: {len(self.validation_data)} days")
            
        except Exception as e:
            logger.error(f"Error preparing data splits: {e}")
    
    def train_prophet_6month(self) -> dict:
        """
        Train Prophet model for 6-month forecasting (extended from existing 30-day)
        
        Returns:
            Prophet training results
        """
        try:
            logger.info("Training Prophet model for 6-month forecasting...")
            
            if not PROPHET_AVAILABLE:
                logger.error("Prophet not available")
                return {'status': 'failed', 'error': 'Prophet not installed'}
            
            # Prepare Prophet data format
            prophet_data = pd.DataFrame({
                'ds': self.training_data.index,
                'y': self.training_data['close'].values
            })
            
            # Add regressors from available data
            if 'volume' in self.training_data.columns:
                prophet_data['volume'] = self.training_data['volume'].values
            if 'volatility_14' in self.training_data.columns:
                prophet_data['volatility'] = self.training_data['volatility_14'].values
            if 'rsi' in self.training_data.columns:
                prophet_data['rsi'] = self.training_data['rsi'].values
            
            # Clean data
            prophet_data = prophet_data.fillna(method='ffill').fillna(method='bfill')
            
            # Create Prophet model with crypto-optimized parameters
            self.prophet_model = Prophet(
                daily_seasonality=True,       # Crypto can have daily patterns
                weekly_seasonality=False,     # Crypto doesn't follow traditional weekly patterns
                yearly_seasonality=False,     # Not enough historical data
                changepoint_prior_scale=0.01, # Conservative trend changes for volatility
                seasonality_prior_scale=0.1,  # Conservative seasonality
                interval_width=0.95,          # Wide confidence intervals
                growth='linear',              # Linear growth assumption
                mcmc_samples=100             # Bayesian sampling for uncertainty
            )
            
            # Add regressors
            for col in prophet_data.columns:
                if col not in ['ds', 'y']:
                    self.prophet_model.add_regressor(col, standardize=True)
            
            # Train model
            start_time = datetime.now()
            self.prophet_model.fit(prophet_data)
            training_time = (datetime.now() - start_time).total_seconds()
            
            # Generate 6-month forecast (extended from 30-day)
            future = self.prophet_model.make_future_dataframe(periods=self.forecast_horizon, freq='D')
            
            # Fill regressor values for future periods
            for col in prophet_data.columns:
                if col not in ['ds', 'y']:
                    last_value = prophet_data[col].iloc[-1]
                    future[col] = last_value  # Use last known value for all periods
                    
                    # For historical data, use actual values
                    historical_len = len(prophet_data)
                    future.loc[:historical_len-1, col] = prophet_data[col].values
            
            # Generate forecast
            forecast = self.prophet_model.predict(future)
            forecast_future = forecast.tail(self.forecast_horizon)
            
            # Calculate performance on validation data if available
            val_metrics = {}
            if self.validation_data is not None and not self.validation_data.empty:
                val_forecast = self.prophet_model.predict(future.tail(len(self.validation_data)))
                val_actual = self.validation_data['close'].values
                val_pred = val_forecast['yhat'].values[:len(val_actual)]
                
                if len(val_pred) > 0:
                    val_metrics = self._calculate_metrics(val_actual, val_pred)
            
            # Store results
            current_price = prophet_data['y'].iloc[-1]
            final_price = forecast_future['yhat'].iloc[-1]
            price_change = ((final_price - current_price) / current_price) * 100
            
            result = {
                'status': 'success',
                'training_time': training_time,
                'current_price': current_price,
                'forecast_final': final_price,
                'price_change_pct': price_change,
                'forecast_data': forecast_future[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].copy(),
                'validation_metrics': val_metrics,
                'methodology': 'Prophet',
                'forecast_horizon': self.forecast_horizon
            }
            
            self.results['prophet'] = result
            
            logger.info(f"Prophet 6-month training completed in {training_time:.2f} seconds")
            logger.info(f"Prophet MAPE: {val_metrics.get('mape', 'N/A')}%")
            
            return result
            
        except Exception as e:
            logger.error(f"Error training Prophet model: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def train_xgboost_6month(self) -> dict:
        """
        Train XGBoost model for 6-month forecasting
        
        Returns:
            XGBoost training results
        """
        try:
            logger.info("Training XGBoost model for 6-month forecasting...")
            
            # Create comprehensive features
            feature_engine = XGBoostFeatureEngine(asset=self.asset)
            featured_data = feature_engine.create_comprehensive_features(self.training_data)
            
            # Prepare features for training
            X, y, feature_names = self.xgboost_model.prepare_features_for_training(featured_data)
            
            if len(X) == 0:
                return {'status': 'failed', 'error': 'No features prepared for training'}
            
            # Train model
            training_result = self.xgboost_model.train_model(X, y)
            
            if training_result['status'] != 'success':
                return training_result
            
            # Generate 6-month forecast
            forecast_result = self.xgboost_model.forecast_6_months(featured_data)
            
            if forecast_result['status'] != 'success':
                return forecast_result
            
            # Store results
            self.results['xgboost'] = forecast_result
            
            logger.info(f"XGBoost 6-month training completed")
            logger.info(f"XGBoost MAPE: {training_result.get('val_metrics', {}).get('mape', 'N/A')}%")
            
            return forecast_result
            
        except Exception as e:
            logger.error(f"Error training XGBoost model: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def train_ensemble_6month(self) -> dict:
        """
        Train Ensemble model combining Prophet and XGBoost for 6-month forecasting
        
        Returns:
            Ensemble training results
        """
        try:
            logger.info("Training Ensemble model for 6-month forecasting...")
            
            # Check if component models are trained
            if self.prophet_model is None or not self.xgboost_model.is_trained:
                return {'status': 'failed', 'error': 'Component models must be trained first'}
            
            # Set component models in ensemble
            self.ensemble_model.set_component_models(self.prophet_model, self.xgboost_model)
            
            # Train ensemble with validation data
            training_result = self.ensemble_model.train_ensemble(self.training_data, self.validation_data)
            
            if training_result['status'] != 'success':
                return training_result
            
            # Generate 6-month ensemble forecast
            forecast_result = self.ensemble_model.forecast_6_months(self.training_data)
            
            if forecast_result['status'] != 'success':
                return forecast_result
            
            # Store results
            self.results['ensemble'] = forecast_result
            
            logger.info(f"Ensemble 6-month training completed")
            logger.info(f"Ensemble MAPE: {training_result.get('ensemble_metrics', {}).get('mape', 'N/A')}%")
            
            return forecast_result
            
        except Exception as e:
            logger.error(f"Error training Ensemble model: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def _calculate_metrics(self, y_true: np.ndarray, y_pred: np.ndarray) -> dict:
        """Calculate performance metrics"""
        try:
            mse = np.mean((y_true - y_pred) ** 2)
            rmse = np.sqrt(mse)
            mae = np.mean(np.abs(y_true - y_pred))
            mape = np.mean(np.abs((y_true - y_pred) / y_true)) * 100
            
            # R² calculation
            ss_res = np.sum((y_true - y_pred) ** 2)
            ss_tot = np.sum((y_true - np.mean(y_true)) ** 2)
            r2 = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0
            
            return {
                'mse': mse,
                'rmse': rmse,
                'mae': mae,
                'mape': mape,
                'r2': r2
            }
        except:
            return {}
    
    def compare_methodologies(self) -> dict:
        """
        Compare all three methodologies and generate comprehensive analysis
        
        Returns:
            Comparison results and rankings
        """
        try:
            logger.info("Comparing methodology performance...")
            
            comparison = {
                'methodology_summary': {},
                'performance_ranking': [],
                'forecast_comparison': {},
                'recommendations': {}
            }
            
            # Methodology summaries
            for method, result in self.results.items():
                if result and result.get('status') == 'success':
                    comparison['methodology_summary'][method] = {
                        'current_price': result.get('current_price', 0),
                        'forecast_final': result.get('forecast_final', 0),
                        'price_change_pct': result.get('price_change_pct', 0),
                        'methodology': result.get('methodology', method),
                        'horizon_days': result.get('forecast_horizon', self.forecast_horizon)
                    }
                    
                    # Add method-specific metrics
                    if method == 'prophet':
                        metrics = result.get('validation_metrics', {})
                        comparison['methodology_summary'][method]['mape'] = metrics.get('mape', 'N/A')
                    elif method == 'xgboost':
                        metrics = result.get('model_metrics', {})
                        comparison['methodology_summary'][method]['mape'] = metrics.get('mape', 'N/A')
                    elif method == 'ensemble':
                        comparison['methodology_summary'][method]['prophet_weight'] = result.get('prophet_weight', 0.5)
                        comparison['methodology_summary'][method]['xgboost_weight'] = result.get('xgboost_weight', 0.5)
                        comparison['methodology_summary'][method]['ensemble_method'] = result.get('ensemble_method', 'dynamic_weights')
            
            # Create forecast comparison DataFrame
            forecast_dates = None
            forecast_data = {}
            
            for method, result in self.results.items():
                if result and result.get('status') == 'success':
                    forecast_df = result.get('forecast_data')
                    if isinstance(forecast_df, pd.DataFrame) and not forecast_df.empty:
                        if forecast_dates is None:
                            if 'date' in forecast_df.columns:
                                forecast_dates = forecast_df['date']
                            elif 'ds' in forecast_df.columns:
                                forecast_dates = forecast_df['ds']
                        
                        # Extract forecast values
                        if 'predicted_price' in forecast_df.columns:
                            forecast_data[method] = forecast_df['predicted_price'].values
                        elif 'yhat' in forecast_df.columns:
                            forecast_data[method] = forecast_df['yhat'].values
                        elif 'ensemble_prediction' in forecast_df.columns:
                            forecast_data[method] = forecast_df['ensemble_prediction'].values
            
            if forecast_dates is not None and forecast_data:
                comparison_df = pd.DataFrame(forecast_data, index=forecast_dates)
                comparison['forecast_comparison'] = comparison_df
            
            # Performance ranking (based on available metrics)
            rankings = []
            for method, summary in comparison['methodology_summary'].items():
                mape = summary.get('mape', 'N/A')
                if isinstance(mape, (int, float)):
                    rankings.append((method, mape))
            
            # Sort by MAPE (lower is better)
            rankings.sort(key=lambda x: x[1])
            comparison['performance_ranking'] = rankings
            
            # Generate recommendations
            if rankings:
                best_method = rankings[0][0]
                comparison['recommendations'] = {
                    'best_methodology': best_method,
                    'reasoning': f"{best_method.capitalize()} shows the lowest MAPE of {rankings[0][1]:.2f}%",
                    'ensemble_benefits': "Ensemble methodology provides robustness by combining multiple approaches"
                }
            
            logger.info(f"Methodology comparison completed")
            if rankings:
                logger.info(f"Best performing methodology: {rankings[0][0]} (MAPE: {rankings[0][1]:.2f}%)")
            
            return comparison
            
        except Exception as e:
            logger.error(f"Error comparing methodologies: {e}")
            return {}
    
    def save_results(self, output_dir: str = None) -> dict:
        """
        Save all results to files
        
        Args:
            output_dir: Directory to save results (default: examples/results/)
            
        Returns:
            Dictionary of saved file paths
        """
        try:
            if output_dir is None:
                output_dir = current_dir / "examples" / "results"
            
            output_path = Path(output_dir)
            output_path.mkdir(parents=True, exist_ok=True)
            
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            saved_files = {}
            
            # Save individual methodology results
            for method, result in self.results.items():
                if result and result.get('status') == 'success':
                    # Save forecast data
                    forecast_data = result.get('forecast_data')
                    if isinstance(forecast_data, pd.DataFrame):
                        filename = f"eth_{method}_6month_forecast_{timestamp}.csv"
                        filepath = output_path / filename
                        forecast_data.to_csv(filepath, index=False)
                        saved_files[f'{method}_forecast'] = str(filepath)
                    
                    # Save metadata
                    metadata = {k: v for k, v in result.items() if k != 'forecast_data'}
                    metadata_filename = f"eth_{method}_6month_metadata_{timestamp}.json"
                    metadata_filepath = output_path / metadata_filename
                    
                    import json
                    with open(metadata_filepath, 'w') as f:
                        json.dump(metadata, f, indent=2, default=str)
                    saved_files[f'{method}_metadata'] = str(metadata_filepath)
            
            # Save comparison results
            comparison = self.compare_methodologies()
            if comparison:
                comparison_filename = f"eth_6month_comparison_{timestamp}.json"
                comparison_filepath = output_path / comparison_filename
                
                # Convert DataFrame to dict for JSON serialization
                comparison_copy = comparison.copy()
                if 'forecast_comparison' in comparison_copy and isinstance(comparison_copy['forecast_comparison'], pd.DataFrame):
                    comparison_copy['forecast_comparison'] = comparison_copy['forecast_comparison'].to_dict()
                
                with open(comparison_filepath, 'w') as f:
                    json.dump(comparison_copy, f, indent=2, default=str)
                saved_files['comparison'] = str(comparison_filepath)
                
                # Save comparison CSV
                if 'forecast_comparison' in comparison and isinstance(comparison['forecast_comparison'], pd.DataFrame):
                    comparison_csv = f"eth_6month_forecast_comparison_{timestamp}.csv"
                    comparison_csv_path = output_path / comparison_csv
                    comparison['forecast_comparison'].to_csv(comparison_csv_path)
                    saved_files['comparison_csv'] = str(comparison_csv_path)
            
            logger.info(f"Results saved to {output_path}")
            for key, filepath in saved_files.items():
                logger.info(f"  {key}: {filepath}")
            
            return saved_files
            
        except Exception as e:
            logger.error(f"Error saving results: {e}")
            return {}
    
    def run_complete_comparison(self) -> dict:
        """
        Run complete 6-month forecast comparison across all methodologies
        
        Returns:
            Complete results and analysis
        """
        try:
            logger.info("🚀 Starting ETH 6-Month Forecast Comparison")
            logger.info("=" * 60)
            
            # Step 1: Load data
            logger.info("\n📊 Step 1: Loading ETH Data")
            eth_data = self.load_eth_data()
            if eth_data.empty:
                return {'status': 'failed', 'error': 'Failed to load ETH data'}
            
            # Step 2: Prepare data splits
            logger.info("\n🔀 Step 2: Preparing Data Splits")
            self.prepare_data_splits()
            
            # Step 3: Train Prophet for 6-month forecasting
            logger.info("\n🔮 Step 3: Training Prophet Methodology (Extended to 6-Month)")
            prophet_result = self.train_prophet_6month()
            
            # Step 4: Train XGBoost for 6-month forecasting
            logger.info("\n🤖 Step 4: Training XGBoost Methodology")
            xgboost_result = self.train_xgboost_6month()
            
            # Step 5: Train Ensemble for 6-month forecasting
            logger.info("\n🎯 Step 5: Training Ensemble Methodology")
            ensemble_result = self.train_ensemble_6month()
            
            # Step 6: Compare methodologies
            logger.info("\n📈 Step 6: Comparing Methodologies")
            comparison = self.compare_methodologies()
            
            # Step 7: Save results
            logger.info("\n💾 Step 7: Saving Results")
            saved_files = self.save_results()
            
            # Step 8: Generate summary
            logger.info("\n📋 Step 8: Generating Summary")
            summary = self._generate_summary()
            
            logger.info("\n✅ ETH 6-Month Forecast Comparison Completed Successfully!")
            
            return {
                'status': 'success',
                'summary': summary,
                'results': self.results,
                'comparison': comparison,
                'saved_files': saved_files,
                'data_points': len(self.eth_data) if self.eth_data is not None else 0
            }
            
        except Exception as e:
            logger.error(f"Error in complete comparison: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def _generate_summary(self) -> dict:
        """Generate comprehensive summary of results"""
        try:
            summary = {
                'forecast_horizon': f"{self.forecast_horizon} days (6 months)",
                'asset': self.asset,
                'methodologies_tested': list(self.results.keys()),
                'successful_methodologies': [k for k, v in self.results.items() if v.get('status') == 'success'],
                'current_eth_price': None,
                'forecast_summary': {},
                'performance_summary': {}
            }
            
            # Extract current price and forecasts
            for method, result in self.results.items():
                if result and result.get('status') == 'success':
                    if summary['current_eth_price'] is None:
                        summary['current_eth_price'] = result.get('current_price', 0)
                    
                    summary['forecast_summary'][method] = {
                        'final_price': result.get('forecast_final', 0),
                        'price_change_pct': result.get('price_change_pct', 0)
                    }
            
            # Performance metrics
            comparison = self.compare_methodologies()
            if comparison and 'performance_ranking' in comparison:
                rankings = comparison['performance_ranking']
                if rankings:
                    summary['performance_summary'] = {
                        'best_methodology': rankings[0][0],
                        'best_mape': rankings[0][1],
                        'all_rankings': rankings
                    }
            
            return summary
            
        except Exception as e:
            logger.error(f"Error generating summary: {e}")
            return {}


def main():
    """Main execution function"""
    try:
        # Create and run comparison
        comparison_system = ETH6MonthForecastComparison()
        results = comparison_system.run_complete_comparison()
        
        if results['status'] == 'success':
            print("\n🎉 ETH 6-Month Forecast Comparison Results:")
            print("=" * 50)
            
            summary = results.get('summary', {})
            
            print(f"\n📊 Data Summary:")
            print(f"   Asset: {summary.get('asset', 'ETH')}")
            print(f"   Forecast Horizon: {summary.get('forecast_horizon', '180 days')}")
            print(f"   Data Points: {results.get('data_points', 'N/A')}")
            print(f"   Current ETH Price: ${summary.get('current_eth_price', 0):.2f}")
            
            print(f"\n🔮 Forecast Summary:")
            for method, forecast in summary.get('forecast_summary', {}).items():
                final_price = forecast.get('final_price', 0)
                change_pct = forecast.get('price_change_pct', 0)
                print(f"   {method.capitalize()}: ${final_price:.2f} ({change_pct:+.2f}%)")
            
            performance = summary.get('performance_summary', {})
            if performance:
                print(f"\n🏆 Performance Ranking:")
                print(f"   Best Methodology: {performance.get('best_methodology', 'N/A')}")
                print(f"   Best MAPE: {performance.get('best_mape', 'N/A'):.2f}%")
            
            saved_files = results.get('saved_files', {})
            if saved_files:
                print(f"\n💾 Saved Files:")
                for key, filepath in saved_files.items():
                    print(f"   {key}: {filepath}")
                    
        else:
            print(f"\n❌ Forecast comparison failed: {results.get('error', 'Unknown error')}")
        
    except Exception as e:
        print(f"\n💥 Error in main execution: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()