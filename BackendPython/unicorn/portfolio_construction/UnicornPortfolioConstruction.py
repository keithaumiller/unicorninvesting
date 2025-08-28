"""
Unicorn Portfolio Construction Models
====================================

Custom portfolio construction models for the Unicorn Investing platform.
These models handle position sizing, allocation decisions, and portfolio rebalancing
based on Insights from Alpha Models.

Separation of Concerns:
- Alpha Models generate Insights (forecasts)
- Portfolio Construction Models decide position sizes and allocations
- Execution Models handle order placement
- Risk Models manage risk controls
"""

from AlgorithmImports import *
import numpy as np

class UnicornEqualWeightPortfolioConstruction(PortfolioConstructionModel):
    """
    Equal weight portfolio construction with Unicorn-specific enhancements.
    
    Features:
    - Equal allocation across all assets with positive Insights
    - Rebalancing on new Insights or time-based schedule
    - Cash management and position sizing controls
    - Unicorn logging and monitoring integration
    """
    
    def __init__(self, rebalance_frequency=Resolution.DAILY, max_position_size=0.3):
        """
        Initialize equal weight portfolio construction.
        
        Args:
            rebalance_frequency: How often to rebalance (Daily, Weekly, etc.)
            max_position_size: Maximum allocation per position (e.g., 30%)
        """
        self.rebalancing_func = rebalance_frequency
        self.max_position_size = max_position_size
        self.rebalancing_time = datetime.min
        
        # Track portfolio state
        self.last_insights = []
        self.target_allocations = {}
        
    def create_targets(self, algorithm, insights):
        """
        Create portfolio targets based on Insights from Alpha Models.
        
        Args:
            algorithm: Algorithm instance
            insights: List of Insights from Alpha Models
            
        Returns:
            List of PortfolioTarget objects
        """
        targets = []
        
        # Check if we should rebalance
        if not self.should_rebalance(algorithm, insights):
            return targets
            
        # Filter and validate insights
        valid_insights = self.filter_insights(insights)
        
        if not valid_insights:
            # No valid insights - liquidate all positions
            for security in algorithm.securities.values():
                if security.invested:
                    targets.append(PortfolioTarget(security.symbol, 0))
                    algorithm.debug(f"📤 LIQUIDATE {security.symbol} - No valid insights")
            return targets
        
        # Calculate equal weight allocations
        weight_per_asset = min(1.0 / len(valid_insights), self.max_position_size)
        
        # Group insights by symbol
        insights_by_symbol = {}
        for insight in valid_insights:
            symbol = insight.symbol
            if symbol not in insights_by_symbol:
                insights_by_symbol[symbol] = []
            insights_by_symbol[symbol].append(insight)
        
        # Create targets for each symbol
        for symbol, symbol_insights in insights_by_symbol.items():
            # Combine multiple insights for same symbol
            combined_direction, combined_confidence = self.combine_insights(symbol_insights)
            
            if combined_direction != InsightDirection.FLAT:
                # Calculate target weight
                direction_multiplier = 1 if combined_direction == InsightDirection.UP else -1
                target_weight = weight_per_asset * direction_multiplier * combined_confidence
                
                # Create portfolio target
                targets.append(PortfolioTarget(symbol, target_weight))
                
                algorithm.debug(f"🎯 TARGET {symbol}: {target_weight:.2%} "
                              f"(direction: {combined_direction}, confidence: {combined_confidence:.2f})")
        
        # Liquidate positions not in current insights
        for security in algorithm.securities.values():
            if security.symbol not in insights_by_symbol and security.invested:
                targets.append(PortfolioTarget(security.symbol, 0))
                algorithm.debug(f"📤 LIQUIDATE {security.symbol} - Not in current insights")
        
        # Update rebalancing time
        self.rebalancing_time = algorithm.utc_time
        self.last_insights = insights
        
        return targets
    
    def should_rebalance(self, algorithm, insights):
        """
        Determine if portfolio should be rebalanced.
        
        Rebalance when:
        1. Time-based rebalancing schedule
        2. New insights received
        3. Significant changes in insights
        """
        # Time-based rebalancing
        if algorithm.utc_time >= self.rebalancing_time + self.rebalancing_func.to_time_span():
            algorithm.debug("🔄 Time-based rebalancing triggered")
            return True
        
        # New insights received
        if len(insights) != len(self.last_insights):
            algorithm.debug("🔄 New insights count triggered rebalancing")
            return True
        
        # Check for significant changes in insights
        if self.insights_changed_significantly(insights):
            algorithm.debug("🔄 Significant insight changes triggered rebalancing")
            return True
        
        return False
    
    def insights_changed_significantly(self, current_insights):
        """
        Check if current insights differ significantly from last insights.
        """
        if not self.last_insights:
            return True
            
        # Simple comparison - check if symbols or directions changed
        current_symbols = {insight.symbol for insight in current_insights}
        last_symbols = {insight.symbol for insight in self.last_insights}
        
        if current_symbols != last_symbols:
            return True
        
        # Check direction changes for same symbols
        current_directions = {insight.symbol: insight.direction for insight in current_insights}
        last_directions = {insight.symbol: insight.direction for insight in self.last_insights}
        
        for symbol in current_symbols:
            if current_directions.get(symbol) != last_directions.get(symbol):
                return True
        
        return False
    
    def filter_insights(self, insights):
        """
        Filter insights to remove invalid or expired ones.
        """
        valid_insights = []
        
        for insight in insights:
            # Check if insight is still valid (not expired)
            if insight.close_time_utc > insight.generated_time_utc:
                # Check if insight has minimum confidence/magnitude
                if hasattr(insight, 'confidence') and insight.confidence and insight.confidence > 0.1:
                    valid_insights.append(insight)
                elif insight.magnitude and insight.magnitude > 0.01:  # 1% minimum expected return
                    valid_insights.append(insight)
        
        return valid_insights
    
    def combine_insights(self, insights):
        """
        Combine multiple insights for the same symbol.
        
        Returns combined direction and confidence.
        """
        if not insights:
            return InsightDirection.FLAT, 0
        
        if len(insights) == 1:
            insight = insights[0]
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            return insight.direction, confidence
        
        # Multiple insights for same symbol - combine them
        up_count = sum(1 for insight in insights if insight.direction == InsightDirection.UP)
        down_count = sum(1 for insight in insights if insight.direction == InsightDirection.DOWN)
        
        # Determine combined direction
        if up_count > down_count:
            combined_direction = InsightDirection.UP
        elif down_count > up_count:
            combined_direction = InsightDirection.DOWN
        else:
            combined_direction = InsightDirection.FLAT
        
        # Average confidence
        confidences = []
        for insight in insights:
            if hasattr(insight, 'confidence') and insight.confidence:
                confidences.append(insight.confidence)
            else:
                confidences.append(0.5)  # Default confidence
        
        combined_confidence = np.mean(confidences) if confidences else 0.5
        
        return combined_direction, combined_confidence


class UnicornConfidenceWeightedPortfolioConstruction(PortfolioConstructionModel):
    """
    Confidence-weighted portfolio construction for Unicorn platform.
    
    Features:
    - Allocates more capital to higher-confidence insights
    - Scales position sizes based on forecast confidence
    - Risk-adjusted position sizing using Kelly criterion concepts
    - Dynamic rebalancing based on confidence changes
    """
    
    def __init__(self, rebalance_frequency=Resolution.DAILY, max_total_leverage=1.0):
        """
        Initialize confidence-weighted portfolio construction.
        
        Args:
            rebalance_frequency: How often to rebalance
            max_total_leverage: Maximum total portfolio leverage (1.0 = no leverage)
        """
        self.rebalancing_func = rebalance_frequency
        self.max_total_leverage = max_total_leverage
        self.rebalancing_time = datetime.min
        
        # Portfolio state tracking
        self.last_insights = []
        self.confidence_history = {}
        
    def create_targets(self, algorithm, insights):
        """
        Create portfolio targets with confidence-weighted allocations.
        """
        targets = []
        
        # Check if we should rebalance
        if not self.should_rebalance(algorithm, insights):
            return targets
        
        # Filter and process insights
        valid_insights = self.filter_insights(insights)
        
        if not valid_insights:
            # Liquidate all positions
            for security in algorithm.securities.values():
                if security.invested:
                    targets.append(PortfolioTarget(security.symbol, 0))
            return targets
        
        # Calculate confidence-weighted allocations
        allocations = self.calculate_confidence_weights(valid_insights)
        
        # Create portfolio targets
        for symbol, weight in allocations.items():
            if abs(weight) > 0.01:  # Minimum 1% allocation
                targets.append(PortfolioTarget(symbol, weight))
                algorithm.debug(f"🎯 CONFIDENCE TARGET {symbol}: {weight:.2%}")
        
        # Liquidate positions not in current allocations
        for security in algorithm.securities.values():
            if security.symbol not in allocations and security.invested:
                targets.append(PortfolioTarget(security.symbol, 0))
        
        self.rebalancing_time = algorithm.utc_time
        self.last_insights = insights
        
        return targets
    
    def calculate_confidence_weights(self, insights):
        """
        Calculate portfolio weights based on insight confidence levels.
        
        Uses a modified Kelly criterion approach for position sizing.
        """
        # Group insights by symbol and combine
        symbol_insights = {}
        for insight in insights:
            symbol = insight.symbol
            if symbol not in symbol_insights:
                symbol_insights[symbol] = []
            symbol_insights[symbol].append(insight)
        
        # Calculate weights for each symbol
        allocations = {}
        total_confidence = 0
        
        # First pass: calculate total confidence
        for symbol, symbol_insight_list in symbol_insights.items():
            combined_confidence = self.combine_symbol_confidence(symbol_insight_list)
            if combined_confidence > 0:
                total_confidence += combined_confidence
        
        # Second pass: allocate weights
        if total_confidence > 0:
            for symbol, symbol_insight_list in symbol_insights.items():
                combined_direction, combined_confidence, expected_return = self.combine_symbol_insights(symbol_insight_list)
                
                if combined_confidence > 0 and combined_direction != InsightDirection.FLAT:
                    # Base weight from confidence proportion
                    base_weight = combined_confidence / total_confidence
                    
                    # Apply Kelly-style position sizing
                    kelly_weight = self.calculate_kelly_weight(expected_return, combined_confidence)
                    
                    # Combine base weight with Kelly weight
                    final_weight = base_weight * kelly_weight
                    
                    # Apply direction
                    direction_multiplier = 1 if combined_direction == InsightDirection.UP else -1
                    final_weight *= direction_multiplier
                    
                    # Scale to respect max leverage
                    final_weight *= self.max_total_leverage
                    
                    allocations[symbol] = final_weight
        
        # Normalize if total allocation exceeds max leverage
        total_allocation = sum(abs(weight) for weight in allocations.values())
        if total_allocation > self.max_total_leverage:
            scale_factor = self.max_total_leverage / total_allocation
            allocations = {symbol: weight * scale_factor for symbol, weight in allocations.items()}
        
        return allocations
    
    def combine_symbol_insights(self, insights):
        """
        Combine multiple insights for the same symbol.
        
        Returns (direction, confidence, expected_return).
        """
        if not insights:
            return InsightDirection.FLAT, 0, 0
        
        directions = [insight.direction for insight in insights]
        confidences = []
        magnitudes = []
        
        for insight in insights:
            # Extract confidence
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            confidences.append(confidence)
            
            # Extract magnitude (expected return)
            magnitude = insight.magnitude if insight.magnitude else 0.01
            magnitudes.append(magnitude)
        
        # Determine combined direction (majority vote)
        up_count = directions.count(InsightDirection.UP)
        down_count = directions.count(InsightDirection.DOWN)
        
        if up_count > down_count:
            combined_direction = InsightDirection.UP
        elif down_count > up_count:
            combined_direction = InsightDirection.DOWN
        else:
            combined_direction = InsightDirection.FLAT
        
        # Average confidence and magnitude
        combined_confidence = np.mean(confidences)
        combined_magnitude = np.mean(magnitudes)
        
        return combined_direction, combined_confidence, combined_magnitude
    
    def combine_symbol_confidence(self, insights):
        """Get combined confidence for a symbol's insights."""
        if not insights:
            return 0
        
        confidences = []
        for insight in insights:
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            confidences.append(confidence)
        
        return np.mean(confidences)
    
    def calculate_kelly_weight(self, expected_return, confidence):
        """
        Calculate Kelly criterion weight for position sizing.
        
        Simplified Kelly: f = (bp - q) / b
        Where:
        - f = fraction of capital to wager
        - b = odds (expected return)
        - p = probability of winning (confidence)
        - q = probability of losing (1 - confidence)
        """
        if expected_return <= 0 or confidence <= 0:
            return 0
        
        # Treat confidence as probability of correct direction
        win_probability = confidence
        lose_probability = 1 - confidence
        
        # Use expected return as odds
        odds = expected_return
        
        # Kelly formula
        kelly_fraction = (odds * win_probability - lose_probability) / odds
        
        # Cap Kelly fraction to reasonable limits (max 25% per position)
        kelly_fraction = max(0, min(kelly_fraction, 0.25))
        
        return kelly_fraction
    
    def filter_insights(self, insights):
        """Filter insights for validity and minimum thresholds."""
        valid_insights = []
        
        for insight in insights:
            # Check expiration
            if insight.close_time_utc > insight.generated_time_utc:
                # Check minimum confidence/magnitude
                confidence = getattr(insight, 'confidence', 0) if hasattr(insight, 'confidence') else 0
                magnitude = insight.magnitude if insight.magnitude else 0
                
                if confidence > 0.2 or magnitude > 0.005:  # 20% confidence or 0.5% expected return
                    valid_insights.append(insight)
        
        return valid_insights
    
    def should_rebalance(self, algorithm, insights):
        """Determine if rebalancing is needed."""
        # Time-based rebalancing
        if algorithm.utc_time >= self.rebalancing_time + self.rebalancing_func.to_time_span():
            return True
        
        # Significant insight changes
        if len(insights) != len(self.last_insights):
            return True
        
        # Check for confidence changes
        current_confidences = {}
        for insight in insights:
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            current_confidences[insight.symbol] = confidence
        
        last_confidences = {}
        for insight in self.last_insights:
            confidence = getattr(insight, 'confidence', 0.5) if hasattr(insight, 'confidence') else 0.5
            last_confidences[insight.symbol] = confidence
        
        # Check for significant confidence changes (>10% change)
        for symbol in current_confidences:
            if symbol in last_confidences:
                confidence_change = abs(current_confidences[symbol] - last_confidences[symbol])
                if confidence_change > 0.1:
                    return True
        
        return False
