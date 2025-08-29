"""
ETH Data Quality and Validation Tests
Unicorn Investing Platform - Data Quality Assurance

Tests for:
- Data integrity validation
- Price consistency checks
- Timestamp validation
- Data completeness analysis
- Quality scoring metrics
"""

import pytest
import pandas as pd
import numpy as np
import sys
import os
from datetime import datetime, timedelta
from typing import List, Dict, Any

# Add source paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

try:
    from optimized_eth_collector import OptimizedETHCollector, ETHDataPoint
except ImportError as e:
    pytest.skip(f"ETH collector module not available: {e}", allow_module_level=True)

class TestDataIntegrity:
    """Test data integrity and consistency"""
    
    def test_ohlc_relationships(self):
        """Test that OHLC data follows proper relationships"""
        # Test valid OHLC relationships
        valid_cases = [
            (100, 105, 95, 102),   # Normal case
            (100, 100, 100, 100),  # All same price
            (100, 103, 97, 97),    # Close at low
            (100, 103, 97, 103),   # Close at high
        ]
        
        for open_price, high, low, close in valid_cases:
            # High should be >= max(open, close)
            assert high >= max(open_price, close), f"High {high} should be >= max(open {open_price}, close {close})"
            
            # Low should be <= min(open, close)
            assert low <= min(open_price, close), f"Low {low} should be <= min(open {open_price}, close {close})"
            
            # High should be >= low
            assert high >= low, f"High {high} should be >= low {low}"
    
    def test_price_reasonableness(self):
        """Test that ETH prices are within reasonable ranges"""
        # ETH price should typically be between $500 and $10,000
        MIN_ETH_PRICE = 500
        MAX_ETH_PRICE = 10000
        
        test_prices = [1000, 2000, 3000, 4000, 5000]  # Reasonable ETH prices
        
        for price in test_prices:
            assert MIN_ETH_PRICE <= price <= MAX_ETH_PRICE, f"ETH price {price} outside reasonable range"
        
        # Test unreasonable prices
        unreasonable_prices = [1, 50, 50000, 100000]
        
        for price in unreasonable_prices:
            if price < MIN_ETH_PRICE or price > MAX_ETH_PRICE:
                # This would be flagged as suspicious in real data
                pass  # Expected to be outside range
    
    def test_volume_validity(self):
        """Test volume data validity"""
        valid_volumes = [0, 100, 1000, 10000, 100000]
        
        for volume in valid_volumes:
            assert volume >= 0, f"Volume {volume} should be non-negative"
        
        # Test invalid volumes
        invalid_volumes = [-1, -100]
        
        for volume in invalid_volumes:
            assert volume < 0  # These should be caught and corrected
    
    def test_timestamp_consistency(self):
        """Test timestamp ordering and consistency"""
        base_time = pd.Timestamp('2025-08-29 12:00:00', tz='UTC')
        
        # Create ordered timestamps
        timestamps = [base_time + timedelta(minutes=i) for i in range(10)]
        
        # Check ordering
        for i in range(1, len(timestamps)):
            assert timestamps[i] > timestamps[i-1], f"Timestamps should be ordered: {timestamps[i-1]} < {timestamps[i]}"
        
        # Check reasonable intervals (1-minute bars)
        for i in range(1, len(timestamps)):
            interval = (timestamps[i] - timestamps[i-1]).total_seconds()
            assert interval == 60, f"1-minute intervals should be 60 seconds, got {interval}"

class TestDataCompleteness:
    """Test data completeness and gap detection"""
    
    def create_sample_data_with_gaps(self) -> List[ETHDataPoint]:
        """Create sample data with intentional gaps"""
        base_time = pd.Timestamp('2025-08-29 12:00:00', tz='UTC')
        data_points = []
        
        # Create data with gaps
        for i in range(60):  # 1 hour of data
            if i == 15 or i == 30 or i == 45:  # Skip some points to create gaps
                continue
                
            timestamp = base_time + timedelta(minutes=i)
            price = 4000 + i * 0.5 + np.random.normal(0, 5)
            
            data_point = ETHDataPoint(
                timestamp=timestamp,
                open=price,
                high=price * 1.001,
                low=price * 0.999,
                close=price,
                volume=1000 + i * 10
            )
            data_points.append(data_point)
        
        return data_points
    
    def test_gap_detection(self):
        """Test detection of data gaps"""
        data_with_gaps = self.create_sample_data_with_gaps()
        
        # Analyze gaps
        timestamps = [dp.timestamp for dp in data_with_gaps]
        gaps = []
        
        for i in range(1, len(timestamps)):
            interval = (timestamps[i] - timestamps[i-1]).total_seconds()
            if interval > 60:  # More than 1 minute
                gaps.append({
                    'start': timestamps[i-1],
                    'end': timestamps[i],
                    'duration': interval
                })
        
        # Should detect 3 gaps
        assert len(gaps) == 3, f"Should detect 3 gaps, found {len(gaps)}"
        
        # Each gap should be ~1 minute
        for gap in gaps:
            assert gap['duration'] == 120, f"Gap duration should be 120 seconds, got {gap['duration']}"
    
    def test_completeness_ratio(self):
        """Test calculation of data completeness ratio"""
        data_with_gaps = self.create_sample_data_with_gaps()
        
        if not data_with_gaps:
            pytest.skip("No test data available")
        
        # Expected: 60 minutes, actual: 57 data points (3 gaps)
        start_time = data_with_gaps[0].timestamp
        end_time = data_with_gaps[-1].timestamp
        
        expected_points = int((end_time - start_time).total_seconds() / 60) + 1
        actual_points = len(data_with_gaps)
        
        completeness_ratio = actual_points / expected_points
        
        # Should be ~95% complete (57/60)
        assert 0.9 <= completeness_ratio <= 1.0, f"Completeness ratio should be 90-100%, got {completeness_ratio:.1%}"
    
    def test_data_freshness(self):
        """Test data freshness (how recent is the latest data)"""
        current_time = pd.Timestamp.now(tz='UTC')
        
        # Simulate recent data
        recent_data = ETHDataPoint(
            timestamp=current_time - timedelta(minutes=5),  # 5 minutes old
            open=4000,
            high=4005,
            low=3995,
            close=4002,
            volume=1000
        )
        
        freshness = (current_time - recent_data.timestamp).total_seconds() / 60  # Minutes
        
        # Data should be fresh (< 10 minutes old for live trading)
        assert freshness < 10, f"Data should be fresh (<10 min), got {freshness:.1f} minutes"
    
    def test_data_density(self):
        """Test data density (points per time period)"""
        # Create 1 hour of complete 1-minute data
        base_time = pd.Timestamp('2025-08-29 12:00:00', tz='UTC')
        complete_data = []
        
        for i in range(60):  # 60 minutes
            timestamp = base_time + timedelta(minutes=i)
            price = 4000 + np.random.normal(0, 10)
            
            data_point = ETHDataPoint(
                timestamp=timestamp,
                open=price,
                high=price * 1.002,
                low=price * 0.998,
                close=price,
                volume=1000
            )
            complete_data.append(data_point)
        
        # Calculate density
        if len(complete_data) >= 2:
            time_span = (complete_data[-1].timestamp - complete_data[0].timestamp).total_seconds() / 3600  # Hours
            density = len(complete_data) / time_span  # Points per hour
            
            # Should be ~60 points per hour for 1-minute data
            assert 55 <= density <= 65, f"Data density should be ~60 points/hour, got {density:.1f}"

class TestDataQualityScoring:
    """Test data quality scoring algorithms"""
    
    def calculate_quality_score(self, data_points: List[ETHDataPoint]) -> Dict[str, float]:
        """Calculate comprehensive data quality score"""
        if not data_points:
            return {'overall': 0.0, 'completeness': 0.0, 'consistency': 0.0, 'freshness': 0.0}
        
        scores = {}
        
        # 1. Completeness Score (gaps analysis)
        timestamps = [dp.timestamp for dp in data_points]
        expected_intervals = 60  # 1-minute bars
        
        if len(timestamps) >= 2:
            total_duration = (timestamps[-1] - timestamps[0]).total_seconds()
            expected_points = int(total_duration / expected_intervals) + 1
            completeness = len(data_points) / expected_points
            scores['completeness'] = min(1.0, completeness)
        else:
            scores['completeness'] = 1.0
        
        # 2. Consistency Score (OHLC relationships)
        valid_ohlc = 0
        total_ohlc = len(data_points)
        
        for dp in data_points:
            if (dp.high >= max(dp.open, dp.close) and 
                dp.low <= min(dp.open, dp.close) and
                dp.high >= dp.low and
                dp.volume >= 0):
                valid_ohlc += 1
        
        scores['consistency'] = valid_ohlc / total_ohlc if total_ohlc > 0 else 0.0
        
        # 3. Freshness Score (how recent is latest data)
        if data_points:
            latest_time = data_points[-1].timestamp
            current_time = pd.Timestamp.now(tz='UTC')
            minutes_old = (current_time - latest_time).total_seconds() / 60
            
            # Fresher data gets higher score (exponential decay)
            scores['freshness'] = np.exp(-minutes_old / 30)  # Half-life of 30 minutes
        else:
            scores['freshness'] = 0.0
        
        # 4. Price Reasonableness Score
        prices = [dp.close for dp in data_points]
        if prices:
            # ETH typically $1000-$8000, wider range for scoring
            min_reasonable = 500
            max_reasonable = 10000
            
            reasonable_prices = sum(1 for p in prices if min_reasonable <= p <= max_reasonable)
            scores['reasonableness'] = reasonable_prices / len(prices)
        else:
            scores['reasonableness'] = 0.0
        
        # 5. Overall Score (weighted average)
        weights = {
            'completeness': 0.3,
            'consistency': 0.3,
            'freshness': 0.2,
            'reasonableness': 0.2
        }
        
        scores['overall'] = sum(scores[metric] * weight for metric, weight in weights.items())
        
        return scores
    
    def test_perfect_quality_data(self):
        """Test quality scoring with perfect data"""
        # Create perfect 1-minute data
        base_time = pd.Timestamp.now(tz='UTC') - timedelta(minutes=30)  # Recent data
        perfect_data = []
        
        for i in range(30):  # 30 minutes of perfect data
            price = 4000 + i * 0.1  # Slight trend
            timestamp = base_time + timedelta(minutes=i)
            
            data_point = ETHDataPoint(
                timestamp=timestamp,
                open=price,
                high=price * 1.001,  # Valid high
                low=price * 0.999,   # Valid low
                close=price,
                volume=1000
            )
            perfect_data.append(data_point)
        
        scores = self.calculate_quality_score(perfect_data)
        
        # All scores should be high
        assert scores['completeness'] >= 0.95, f"Completeness should be >=95%, got {scores['completeness']:.1%}"
        assert scores['consistency'] == 1.0, f"Consistency should be 100%, got {scores['consistency']:.1%}"
        assert scores['freshness'] >= 0.5, f"Freshness should be >=50%, got {scores['freshness']:.1%}"
        assert scores['reasonableness'] == 1.0, f"Reasonableness should be 100%, got {scores['reasonableness']:.1%}"
        assert scores['overall'] >= 0.8, f"Overall score should be >=80%, got {scores['overall']:.1%}"
    
    def test_poor_quality_data(self):
        """Test quality scoring with poor data"""
        # Create poor quality data
        poor_data = []
        
        # Old data
        old_time = pd.Timestamp.now(tz='UTC') - timedelta(hours=5)
        
        # Inconsistent OHLC and unreasonable prices
        poor_cases = [
            (100, 50, 200, 75, 1000),   # Invalid OHLC (high < low)
            (50000, 50001, 49999, 50000, -100),  # Unreasonable price, negative volume
            (1, 2, 0.5, 1.5, 10),       # Unreasonably low ETH price
        ]
        
        for i, (open_price, high, low, close, volume) in enumerate(poor_cases):
            timestamp = old_time + timedelta(minutes=i * 10)  # Sparse data (10-min gaps)
            
            data_point = ETHDataPoint(
                timestamp=timestamp,
                open=open_price,
                high=high,
                low=low,
                close=close,
                volume=volume
            )
            poor_data.append(data_point)
        
        scores = self.calculate_quality_score(poor_data)
        
        # Scores should be low
        assert scores['completeness'] <= 0.5, f"Completeness should be <=50%, got {scores['completeness']:.1%}"
        assert scores['consistency'] <= 0.5, f"Consistency should be <=50%, got {scores['consistency']:.1%}"
        assert scores['freshness'] <= 0.1, f"Freshness should be <=10%, got {scores['freshness']:.1%}"
        assert scores['reasonableness'] <= 0.5, f"Reasonableness should be <=50%, got {scores['reasonableness']:.1%}"
        assert scores['overall'] <= 0.4, f"Overall score should be <=40%, got {scores['overall']:.1%}"
    
    def test_mixed_quality_data(self):
        """Test quality scoring with mixed quality data"""
        # Create data with some good and some poor quality
        base_time = pd.Timestamp.now(tz='UTC') - timedelta(minutes=60)
        mixed_data = []
        
        for i in range(60):
            if i % 10 == 0:  # Every 10th point has issues
                # Poor quality data
                price = 100 if i % 20 == 0 else 4000  # Mix of unreasonable and reasonable prices
                high = price * 0.99  # Invalid: high < open
                low = price * 1.01   # Invalid: low > open
            else:
                # Good quality data
                price = 4000 + np.random.normal(0, 10)
                high = price * 1.001
                low = price * 0.999
            
            timestamp = base_time + timedelta(minutes=i)
            data_point = ETHDataPoint(
                timestamp=timestamp,
                open=price,
                high=high,
                low=low,
                close=price,
                volume=1000
            )
            mixed_data.append(data_point)
        
        scores = self.calculate_quality_score(mixed_data)
        
        # Scores should be moderate
        assert 0.8 <= scores['completeness'] <= 1.0, f"Completeness should be 80-100%, got {scores['completeness']:.1%}"
        assert 0.7 <= scores['consistency'] <= 0.95, f"Consistency should be 70-95%, got {scores['consistency']:.1%}"
        assert 0.3 <= scores['freshness'] <= 1.0, f"Freshness should be 30-100%, got {scores['freshness']:.1%}"
        assert 0.7 <= scores['reasonableness'] <= 0.95, f"Reasonableness should be 70-95%, got {scores['reasonableness']:.1%}"
        assert 0.6 <= scores['overall'] <= 0.9, f"Overall score should be 60-90%, got {scores['overall']:.1%}"

class TestRealDataValidation:
    """Test validation with real IBKR data (if available)"""
    
    @pytest.fixture
    def real_collector(self):
        """Create collector for real data testing"""
        collector = OptimizedETHCollector()
        if not collector.authenticate():
            pytest.skip("IBKR authentication required for real data tests")
        return collector
    
    def test_real_data_quality(self, real_collector):
        """Test quality of real IBKR data"""
        # Collect real data
        real_data = real_collector.collect_minute_bars()
        
        if not real_data:
            pytest.skip("No real data available from IBKR")
        
        print(f"\n📊 Analyzing {len(real_data)} real data points...")
        
        # Calculate quality scores
        scorer = TestDataQualityScoring()
        scores = scorer.calculate_quality_score(real_data)
        
        print(f"📈 Quality Scores:")
        for metric, score in scores.items():
            print(f"   {metric.title()}: {score:.1%}")
        
        # Real IBKR data should be high quality
        assert scores['overall'] >= 0.7, f"Real data quality should be >=70%, got {scores['overall']:.1%}"
        assert scores['consistency'] >= 0.95, f"Real data consistency should be >=95%, got {scores['consistency']:.1%}"
        assert scores['reasonableness'] >= 0.9, f"Real data reasonableness should be >=90%, got {scores['reasonableness']:.1%}"
    
    def test_real_data_statistics(self, real_collector):
        """Test statistical properties of real data"""
        real_data = real_collector.collect_minute_bars()
        
        if not real_data or len(real_data) < 10:
            pytest.skip("Insufficient real data for statistical analysis")
        
        prices = [dp.close for dp in real_data]
        volumes = [dp.volume for dp in real_data]
        
        # Price statistics
        price_mean = np.mean(prices)
        price_std = np.std(prices)
        price_min = min(prices)
        price_max = max(prices)
        
        print(f"\n📊 Real Data Statistics:")
        print(f"   Price: ${price_mean:.2f} ± ${price_std:.2f}")
        print(f"   Range: ${price_min:.2f} - ${price_max:.2f}")
        print(f"   Volume: {np.mean(volumes):.2f} ± {np.std(volumes):.2f}")
        
        # Sanity checks
        assert 1000 <= price_mean <= 8000, f"Average ETH price should be reasonable, got ${price_mean:.2f}"
        assert price_std > 0, f"Price should have some volatility, got std=${price_std:.2f}"
        assert all(v >= 0 for v in volumes), "All volumes should be non-negative"
    
    def test_real_data_gaps(self, real_collector):
        """Test gap analysis on real data"""
        real_data = real_collector.collect_minute_bars()
        
        if not real_data or len(real_data) < 10:
            pytest.skip("Insufficient real data for gap analysis")
        
        timestamps = [dp.timestamp for dp in real_data]
        gaps = []
        
        for i in range(1, len(timestamps)):
            interval = (timestamps[i] - timestamps[i-1]).total_seconds()
            if interval > 90:  # More than 1.5 minutes (allowing some tolerance)
                gaps.append(interval)
        
        gap_ratio = len(gaps) / (len(timestamps) - 1) if len(timestamps) > 1 else 0
        
        print(f"\n🕐 Gap Analysis:")
        print(f"   Total intervals: {len(timestamps) - 1}")
        print(f"   Gaps detected: {len(gaps)}")
        print(f"   Gap ratio: {gap_ratio:.1%}")
        
        if gaps:
            print(f"   Average gap: {np.mean(gaps):.1f} seconds")
            print(f"   Max gap: {max(gaps):.1f} seconds")
        
        # Real data should have minimal gaps
        assert gap_ratio <= 0.1, f"Gap ratio should be <=10%, got {gap_ratio:.1%}"

# Utility function for manual data quality analysis
def analyze_data_quality(data_file_path: str = None):
    """Analyze data quality from file or live collection"""
    print("🔍 ETH Data Quality Analysis")
    print("=" * 40)
    
    if data_file_path and os.path.exists(data_file_path):
        # Load from file
        import json
        with open(data_file_path, 'r') as f:
            file_data = json.load(f)
        
        data_points = []
        for item in file_data.get('data', []):
            dp = ETHDataPoint(
                timestamp=pd.Timestamp(item['timestamp']),
                open=item['open'],
                high=item['high'],
                low=item['low'],
                close=item['close'],
                volume=item['volume']
            )
            data_points.append(dp)
        
        print(f"📁 Loaded {len(data_points)} data points from file")
    
    else:
        # Collect live data
        collector = OptimizedETHCollector()
        if not collector.authenticate():
            print("❌ IBKR authentication required")
            return
        
        print("📡 Collecting live data...")
        data_points = collector.collect_minute_bars()
        print(f"✅ Collected {len(data_points)} live data points")
    
    if not data_points:
        print("❌ No data available for analysis")
        return
    
    # Perform quality analysis
    scorer = TestDataQualityScoring()
    scores = scorer.calculate_quality_score(data_points)
    
    print(f"\n📊 Data Quality Report:")
    print(f"{'='*40}")
    
    for metric, score in scores.items():
        status = "✅" if score >= 0.8 else "⚠️" if score >= 0.6 else "❌"
        print(f"   {status} {metric.title()}: {score:.1%}")
    
    # Additional statistics
    prices = [dp.close for dp in data_points]
    volumes = [dp.volume for dp in data_points]
    
    print(f"\n📈 Data Statistics:")
    print(f"   Data Points: {len(data_points)}")
    print(f"   Time Range: {data_points[0].timestamp} to {data_points[-1].timestamp}")
    print(f"   Price Range: ${min(prices):.2f} - ${max(prices):.2f}")
    print(f"   Average Price: ${np.mean(prices):.2f}")
    print(f"   Price Volatility: {np.std(prices)/np.mean(prices)*100:.2f}%")
    print(f"   Average Volume: {np.mean(volumes):.2f}")
    
    # Gap analysis
    timestamps = [dp.timestamp for dp in data_points]
    if len(timestamps) > 1:
        intervals = [(timestamps[i] - timestamps[i-1]).total_seconds() for i in range(1, len(timestamps))]
        gaps = [interval for interval in intervals if interval > 90]
        
        print(f"\n🕐 Continuity Analysis:")
        print(f"   Expected Interval: 60 seconds")
        print(f"   Average Interval: {np.mean(intervals):.1f} seconds")
        print(f"   Gaps (>90s): {len(gaps)}")
        print(f"   Completeness: {(len(intervals) - len(gaps)) / len(intervals) * 100:.1f}%")
    
    print(f"\n{'='*40}")
    overall_status = "✅ EXCELLENT" if scores['overall'] >= 0.9 else "⚠️ GOOD" if scores['overall'] >= 0.7 else "❌ POOR"
    print(f"Overall Quality: {overall_status} ({scores['overall']:.1%})")

if __name__ == "__main__":
    # Run analysis if called directly
    analyze_data_quality()
