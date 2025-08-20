"""
Genetic Algorithm for Portfolio Optimization and Feature Selection.

This module implements sophisticated genetic algorithms for:
- Portfolio allocation optimization
- Feature selection for neural networks
- Multi-objective optimization (return vs risk)
- Parameter evolution for trading strategies

Migrated from: BackendPython/recomendationsystems/GA_parameter_explorer.R
"""

import numpy as np
import pandas as pd
import logging
from typing import List, Dict, Tuple, Optional, Callable, Any
from dataclasses import dataclass, field
import random
from abc import ABC, abstractmethod
import concurrent.futures
from datetime import datetime
import json

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class GAConfig:
    """Configuration for Genetic Algorithm optimization."""
    population_size: int = 100
    num_generations: int = 200
    mutation_rate: float = 0.1
    crossover_rate: float = 0.8
    elitism_rate: float = 0.1
    tournament_size: int = 5
    convergence_threshold: float = 1e-6
    max_stagnant_generations: int = 50
    parallel_processing: bool = True
    random_seed: Optional[int] = None

@dataclass
class Individual:
    """Represents an individual in the genetic algorithm."""
    genes: np.ndarray
    fitness: float = 0.0
    age: int = 0
    metadata: Dict[str, Any] = field(default_factory=dict)

class FitnessFunction(ABC):
    """Abstract base class for fitness functions."""
    
    @abstractmethod
    def evaluate(self, individual: Individual, data: Dict[str, Any]) -> float:
        """Evaluate fitness of an individual."""
        pass
    
    @abstractmethod
    def is_maximization(self) -> bool:
        """Return True if this is a maximization problem."""
        pass

class PortfolioFitnessFunction(FitnessFunction):
    """Fitness function for portfolio optimization."""
    
    def __init__(self, risk_aversion: float = 0.5, 
                 return_lookback: int = 252,
                 min_allocation: float = 0.0,
                 max_allocation: float = 0.3):
        """
        Initialize portfolio fitness function.
        
        Args:
            risk_aversion: Risk aversion parameter (0=no risk penalty, 1=high penalty)
            return_lookback: Days to look back for return calculation
            min_allocation: Minimum allocation per asset
            max_allocation: Maximum allocation per asset
        """
        self.risk_aversion = risk_aversion
        self.return_lookback = return_lookback
        self.min_allocation = min_allocation
        self.max_allocation = max_allocation
    
    def evaluate(self, individual: Individual, data: Dict[str, Any]) -> float:
        """
        Evaluate portfolio fitness based on Sharpe ratio and constraints.
        
        Args:
            individual: Individual with allocation weights
            data: Dictionary containing price data and other info
            
        Returns:
            Fitness score (higher is better)
        """
        try:
            allocations = individual.genes
            returns_data = data['returns']
            
            # Ensure allocations sum to 1
            allocations = allocations / np.sum(allocations)
            
            # Apply allocation constraints
            allocations = np.clip(allocations, self.min_allocation, self.max_allocation)
            allocations = allocations / np.sum(allocations)  # Renormalize
            
            # Calculate portfolio returns
            portfolio_returns = np.dot(returns_data, allocations)
            
            # Calculate metrics
            mean_return = np.mean(portfolio_returns[-self.return_lookback:])
            volatility = np.std(portfolio_returns[-self.return_lookback:])
            
            # Sharpe ratio (assuming risk-free rate = 0)
            sharpe_ratio = mean_return / volatility if volatility > 0 else 0
            
            # Apply risk adjustment
            risk_penalty = self.risk_aversion * volatility
            fitness = sharpe_ratio - risk_penalty
            
            # Add diversification bonus
            diversification_bonus = self._calculate_diversification_bonus(allocations)
            fitness += diversification_bonus
            
            # Store metadata
            individual.metadata.update({
                'mean_return': mean_return,
                'volatility': volatility,
                'sharpe_ratio': sharpe_ratio,
                'diversification': diversification_bonus
            })
            
            return fitness
            
        except Exception as e:
            logger.error(f"Error evaluating portfolio fitness: {e}")
            return -np.inf
    
    def _calculate_diversification_bonus(self, allocations: np.ndarray) -> float:
        """Calculate diversification bonus based on allocation distribution."""
        # Use inverse of Herfindahl index
        herfindahl = np.sum(allocations ** 2)
        max_diversification = 1.0 / len(allocations)  # Perfect equal weighting
        diversification_score = (1.0 / herfindahl - 1.0) / (1.0 / max_diversification - 1.0)
        return 0.1 * diversification_score  # 10% weight on diversification
    
    def is_maximization(self) -> bool:
        """Portfolio optimization is a maximization problem."""
        return True

class FeatureSelectionFitnessFunction(FitnessFunction):
    """Fitness function for feature selection in machine learning."""
    
    def __init__(self, model_evaluator: Callable, 
                 complexity_penalty: float = 0.01,
                 max_features: int = 50):
        """
        Initialize feature selection fitness function.
        
        Args:
            model_evaluator: Function to evaluate model performance
            complexity_penalty: Penalty for model complexity
            max_features: Maximum number of features
        """
        self.model_evaluator = model_evaluator
        self.complexity_penalty = complexity_penalty
        self.max_features = max_features
    
    def evaluate(self, individual: Individual, data: Dict[str, Any]) -> float:
        """
        Evaluate feature selection fitness.
        
        Args:
            individual: Individual with binary feature selection
            data: Dictionary containing training data
            
        Returns:
            Fitness score (higher is better)
        """
        try:
            feature_selection = individual.genes > 0.5  # Binary selection
            
            if np.sum(feature_selection) == 0:
                return 0.0  # No features selected
            
            # Limit number of features
            num_selected = np.sum(feature_selection)
            if num_selected > self.max_features:
                # Keep top features based on some criteria
                feature_indices = np.where(feature_selection)[0]
                feature_scores = individual.genes[feature_indices]
                top_indices = feature_indices[np.argsort(feature_scores)[-self.max_features:]]
                feature_selection = np.zeros_like(individual.genes, dtype=bool)
                feature_selection[top_indices] = True
                num_selected = self.max_features
            
            # Evaluate model performance with selected features
            X_train = data['X_train'][:, feature_selection]
            y_train = data['y_train']
            X_val = data['X_val'][:, feature_selection]
            y_val = data['y_val']
            
            model_score = self.model_evaluator(X_train, y_train, X_val, y_val)
            
            # Apply complexity penalty
            complexity_penalty = self.complexity_penalty * num_selected
            fitness = model_score - complexity_penalty
            
            # Store metadata
            individual.metadata.update({
                'model_score': model_score,
                'num_features': num_selected,
                'complexity_penalty': complexity_penalty
            })
            
            return fitness
            
        except Exception as e:
            logger.error(f"Error evaluating feature selection fitness: {e}")
            return -np.inf
    
    def is_maximization(self) -> bool:
        """Feature selection is a maximization problem."""
        return True

class GeneticAlgorithm:
    """Main genetic algorithm implementation."""
    
    def __init__(self, config: GAConfig, fitness_function: FitnessFunction):
        """
        Initialize genetic algorithm.
        
        Args:
            config: GA configuration
            fitness_function: Fitness evaluation function
        """
        self.config = config
        self.fitness_function = fitness_function
        self.population: List[Individual] = []
        self.generation = 0
        self.best_individual: Optional[Individual] = None
        self.fitness_history: List[float] = []
        self.convergence_history: List[float] = []
        
        # Set random seed if provided
        if config.random_seed is not None:
            np.random.seed(config.random_seed)
            random.seed(config.random_seed)
    
    def initialize_population(self, gene_length: int, 
                            initialization_method: str = "random") -> None:
        """
        Initialize the population.
        
        Args:
            gene_length: Length of each individual's gene array
            initialization_method: Method for initialization
        """
        try:
            self.population = []
            
            for _ in range(self.config.population_size):
                if initialization_method == "random":
                    genes = np.random.random(gene_length)
                elif initialization_method == "uniform":
                    genes = np.ones(gene_length) / gene_length
                elif initialization_method == "normal":
                    genes = np.abs(np.random.normal(0.5, 0.2, gene_length))
                else:
                    genes = np.random.random(gene_length)
                
                # Ensure genes are in valid range [0, 1]
                genes = np.clip(genes, 0, 1)
                
                individual = Individual(genes=genes)
                self.population.append(individual)
            
            logger.info(f"Initialized population of {len(self.population)} individuals")
            
        except Exception as e:
            logger.error(f"Error initializing population: {e}")
            raise
    
    def evaluate_population(self, data: Dict[str, Any]) -> None:
        """
        Evaluate fitness for entire population.
        
        Args:
            data: Data needed for fitness evaluation
        """
        try:
            if self.config.parallel_processing:
                self._evaluate_population_parallel(data)
            else:
                self._evaluate_population_sequential(data)
            
            # Update age for all individuals
            for individual in self.population:
                individual.age += 1
            
            # Update best individual
            self._update_best_individual()
            
        except Exception as e:
            logger.error(f"Error evaluating population: {e}")
            raise
    
    def _evaluate_population_sequential(self, data: Dict[str, Any]) -> None:
        """Evaluate population sequentially."""
        for individual in self.population:
            individual.fitness = self.fitness_function.evaluate(individual, data)
    
    def _evaluate_population_parallel(self, data: Dict[str, Any]) -> None:
        """Evaluate population in parallel."""
        with concurrent.futures.ThreadPoolExecutor() as executor:
            futures = [
                executor.submit(self.fitness_function.evaluate, individual, data)
                for individual in self.population
            ]
            
            for i, future in enumerate(concurrent.futures.as_completed(futures)):
                result_idx = futures.index(future)
                self.population[result_idx].fitness = future.result()
    
    def _update_best_individual(self) -> None:
        """Update the best individual found so far."""
        if self.fitness_function.is_maximization():
            current_best = max(self.population, key=lambda x: x.fitness)
        else:
            current_best = min(self.population, key=lambda x: x.fitness)
        
        if (self.best_individual is None or 
            (self.fitness_function.is_maximization() and current_best.fitness > self.best_individual.fitness) or
            (not self.fitness_function.is_maximization() and current_best.fitness < self.best_individual.fitness)):
            
            # Deep copy the best individual
            self.best_individual = Individual(
                genes=current_best.genes.copy(),
                fitness=current_best.fitness,
                age=current_best.age,
                metadata=current_best.metadata.copy()
            )
    
    def selection(self) -> List[Individual]:
        """
        Select parents for reproduction using tournament selection.
        
        Returns:
            List of selected parents
        """
        try:
            parents = []
            
            for _ in range(self.config.population_size):
                # Tournament selection
                tournament = random.sample(self.population, self.config.tournament_size)
                
                if self.fitness_function.is_maximization():
                    winner = max(tournament, key=lambda x: x.fitness)
                else:
                    winner = min(tournament, key=lambda x: x.fitness)
                
                parents.append(winner)
            
            return parents
            
        except Exception as e:
            logger.error(f"Error in selection: {e}")
            return self.population.copy()
    
    def crossover(self, parent1: Individual, parent2: Individual) -> Tuple[Individual, Individual]:
        """
        Create offspring through crossover.
        
        Args:
            parent1: First parent
            parent2: Second parent
            
        Returns:
            Tuple of two offspring
        """
        try:
            if random.random() > self.config.crossover_rate:
                # No crossover - return copies of parents
                return (
                    Individual(genes=parent1.genes.copy()),
                    Individual(genes=parent2.genes.copy())
                )
            
            # Uniform crossover
            mask = np.random.random(len(parent1.genes)) < 0.5
            
            child1_genes = np.where(mask, parent1.genes, parent2.genes)
            child2_genes = np.where(mask, parent2.genes, parent1.genes)
            
            child1 = Individual(genes=child1_genes)
            child2 = Individual(genes=child2_genes)
            
            return child1, child2
            
        except Exception as e:
            logger.error(f"Error in crossover: {e}")
            return (
                Individual(genes=parent1.genes.copy()),
                Individual(genes=parent2.genes.copy())
            )
    
    def mutation(self, individual: Individual) -> Individual:
        """
        Apply mutation to an individual.
        
        Args:
            individual: Individual to mutate
            
        Returns:
            Mutated individual
        """
        try:
            if random.random() > self.config.mutation_rate:
                return individual
            
            # Gaussian mutation
            mutation_strength = 0.1
            mutation_mask = np.random.random(len(individual.genes)) < self.config.mutation_rate
            
            mutations = np.random.normal(0, mutation_strength, len(individual.genes))
            individual.genes += mutation_mask * mutations
            
            # Ensure genes stay in valid range
            individual.genes = np.clip(individual.genes, 0, 1)
            
            return individual
            
        except Exception as e:
            logger.error(f"Error in mutation: {e}")
            return individual
    
    def evolve_generation(self, data: Dict[str, Any]) -> None:
        """Evolve one generation."""
        try:
            # Selection
            parents = self.selection()
            
            # Create new population through crossover and mutation
            new_population = []
            
            # Elitism - keep best individuals
            elite_count = int(self.config.elitism_rate * self.config.population_size)
            if elite_count > 0:
                sorted_pop = sorted(self.population, 
                                  key=lambda x: x.fitness, 
                                  reverse=self.fitness_function.is_maximization())
                new_population.extend(sorted_pop[:elite_count])
            
            # Generate offspring
            while len(new_population) < self.config.population_size:
                parent1, parent2 = random.sample(parents, 2)
                child1, child2 = self.crossover(parent1, parent2)
                
                child1 = self.mutation(child1)
                child2 = self.mutation(child2)
                
                new_population.extend([child1, child2])
            
            # Trim to exact population size
            self.population = new_population[:self.config.population_size]
            
            # Evaluate new population
            self.evaluate_population(data)
            
            # Update generation counter
            self.generation += 1
            
            # Track fitness history
            best_fitness = self.best_individual.fitness if self.best_individual else 0
            self.fitness_history.append(best_fitness)
            
            # Calculate convergence metric
            if len(self.fitness_history) > 10:
                recent_fitness = self.fitness_history[-10:]
                convergence = np.std(recent_fitness)
                self.convergence_history.append(convergence)
            
        except Exception as e:
            logger.error(f"Error evolving generation: {e}")
            raise
    
    def optimize(self, data: Dict[str, Any], gene_length: int) -> Individual:
        """
        Run the complete optimization process.
        
        Args:
            data: Data for fitness evaluation
            gene_length: Length of gene arrays
            
        Returns:
            Best individual found
        """
        try:
            logger.info("Starting genetic algorithm optimization")
            
            # Initialize population
            self.initialize_population(gene_length)
            
            # Initial evaluation
            self.evaluate_population(data)
            
            stagnant_generations = 0
            last_best_fitness = None
            
            # Evolution loop
            for generation in range(self.config.num_generations):
                self.evolve_generation(data)
                
                current_best_fitness = self.best_individual.fitness
                
                # Check for convergence
                if last_best_fitness is not None:
                    improvement = abs(current_best_fitness - last_best_fitness)
                    if improvement < self.config.convergence_threshold:
                        stagnant_generations += 1
                    else:
                        stagnant_generations = 0
                
                last_best_fitness = current_best_fitness
                
                # Log progress
                if generation % 20 == 0:
                    logger.info(f"Generation {generation}: Best fitness = {current_best_fitness:.6f}")
                
                # Check for early stopping
                if stagnant_generations >= self.config.max_stagnant_generations:
                    logger.info(f"Converged after {generation} generations")
                    break
            
            logger.info(f"Optimization completed. Best fitness: {self.best_individual.fitness:.6f}")
            
            return self.best_individual
            
        except Exception as e:
            logger.error(f"Error in optimization: {e}")
            raise
    
    def get_optimization_results(self) -> Dict[str, Any]:
        """Get detailed optimization results."""
        return {
            'best_individual': self.best_individual,
            'final_generation': self.generation,
            'fitness_history': self.fitness_history,
            'convergence_history': self.convergence_history,
            'population_size': self.config.population_size,
            'total_evaluations': self.generation * self.config.population_size
        }

# Example usage
if __name__ == "__main__":
    # Example: Portfolio optimization
    config = GAConfig(population_size=50, num_generations=100)
    fitness_func = PortfolioFitnessFunction(risk_aversion=0.3)
    
    ga = GeneticAlgorithm(config, fitness_func)
    
    # Sample data (in real use, load actual return data)
    sample_returns = np.random.normal(0.001, 0.02, (1000, 10))  # 1000 days, 10 assets
    data = {'returns': sample_returns}
    
    # Run optimization
    best_portfolio = ga.optimize(data, gene_length=10)
    
    print(f"Best portfolio allocation: {best_portfolio.genes}")
    print(f"Best fitness: {best_portfolio.fitness}")
    print(f"Portfolio metadata: {best_portfolio.metadata}")
