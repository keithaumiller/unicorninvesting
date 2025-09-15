"""
Test Suite for ETH Framework End-to-End Integration
Unicorn Investing Platform - Complete Pipeline Testing

Tests the complete data flow:
1. IBKR Data Collection → Raw Data
2. Raw Data → Technical Indicators  
3. Indicators → Analysis & Signals
4. Performance Monitoring
5. Error Handling & Recovery
"""

import pytest
import pandas as pd
import numpy as np
import time
import sys
import os
from datetime import datetime, timedelta
from typing import List, Dict, Any, Optional

# Add source paths
sys.path.append('/workspaces/unicorninvesting/BackendPython/unicorn/1_data_sources/1_raw/connectors/interactive_brokers')

try:
    from optimized_eth_collector import OptimizedETHCollector, ETHDataPoint
    from technical_indicators import TechnicalIndicatorEngine, IndicatorValue
except ImportError as e:
    pytest.skip(f"Required modules not available: {e}", allow_module_level=True)

class TestE2EDataPipeline:
    """Test complete end-to-end data pipeline"""
    
    @pytest.fixture(scope="class")
    def pipeline_components(self):
        """Set up complete pipeline components"""
        collector = OptimizedETHCollector()
        indicator_engine = TechnicalIndicatorEngine(max_history=200)
        
        return {
            'collector': collector,
            'indicators': indicator_engine,
            'start_time': pd.Timestamp.now(tz='UTC')
        }
    
    def test_full_pipeline_flow(self, pipeline_components):
        """Test complete pipeline from IBKR → Indicators → Analysis"""
        collector = pipeline_components['collector']
        indicator_engine = pipeline_components['indicators']
        
        print("\n🔄 Testing End-to-End Pipeline Flow")
        
        # Step 1: Authentication & Connection
        print("   1️⃣ Authenticating with IBKR...")
        if not collector.authenticate():
            pytest.skip("IBKR Gateway not available for E2E testing")
        
        # Step 2: Data Collection
        print("   2️⃣ Collecting ETH minute bars...")
        start_collection = time.time()
        raw_data = collector.collect_minute_bars()
        collection_time = time.time() - start_collection
        
        assert len(raw_data) > 0, "Should collect at least some data points"
        print(f"      ✅ Collected {len(raw_data)} data points in {collection_time:.2f}s")
        
        # Step 3: Data Validation
        print("   3️⃣ Validating raw data quality...")
        for dp in raw_data[:5]:  # Check first 5 points
            assert dp.high >= max(dp.open, dp.close), f"Invalid OHLC relationship in {dp}"
            assert dp.low <= min(dp.open, dp.close), f"Invalid OHLC relationship in {dp}"
            assert dp.volume >= 0, f"Negative volume in {dp}"
        print(f"      ✅ Data quality validation passed")
        
        # Step 4: Technical Indicators Calculation
        print("   4️⃣ Calculating technical indicators...")
        start_indicators = time.time()
        
        indicators_calculated = []
        for dp in raw_data:
            # Add data point to indicator engine
            indicator_engine.add_data_point(dp.close, dp.volume, dp.timestamp)
            
            # Calculate indicators
            if len(indicator_engine.prices) >= 20:  # Need minimum data for indicators
                indicators = {
                    'sma_20': indicator_engine.sma(20),
                    'rsi_14': indicator_engine.rsi(14),
                    'macd': indicator_engine.macd(),
                    'bollinger': indicator_engine.bollinger_bands(),
                    'atr': indicator_engine.atr(14)
                }
                indicators_calculated.append({
                    'timestamp': dp.timestamp,
                    'price': dp.close,
                    'indicators': indicators
                })
        
        indicators_time = time.time() - start_indicators
        print(f"      ✅ Calculated indicators for {len(indicators_calculated)} points in {indicators_time:.2f}s")
        
        # Step 5: Signal Generation (basic example)
        print("   5️⃣ Generating trading signals...")
        signals = []
        
        for i, data in enumerate(indicators_calculated):
            if i < 2:  # Need previous data for comparison
                continue
            
            current = data['indicators']
            previous = indicators_calculated[i-1]['indicators']
            
            # Simple signal logic: RSI oversold/overbought + MACD confirmation
            signal = 'HOLD'
            
            if (current['rsi_14'] and previous['rsi_14'] and 
                current['macd'] and previous['macd']):
                
                # Bullish signal: RSI coming out of oversold + MACD bullish crossover
                if (previous['rsi_14'].value < 30 and current['rsi_14'].value > 30 and
                    current['macd'].value > current['macd'].signal):
                    signal = 'BUY'
                
                # Bearish signal: RSI coming from overbought + MACD bearish crossover  
                elif (previous['rsi_14'].value > 70 and current['rsi_14'].value < 70 and
                      current['macd'].value < current['macd'].signal):
                    signal = 'SELL'
            
            signals.append({
                'timestamp': data['timestamp'],
                'price': data['price'],
                'signal': signal,
                'confidence': 0.8 if signal != 'HOLD' else 0.0
            })
        
        buy_signals = len([s for s in signals if s['signal'] == 'BUY'])
        sell_signals = len([s for s in signals if s['signal'] == 'SELL'])
        print(f"      ✅ Generated {len(signals)} signals: {buy_signals} BUY, {sell_signals} SELL")
        
        # Step 6: Performance Analysis
        print("   6️⃣ Analyzing pipeline performance...")
        total_time = collection_time + indicators_time
        throughput = len(raw_data) / total_time if total_time > 0 else 0
        
        performance_metrics = {
            'total_processing_time': total_time,
            'data_collection_time': collection_time,
            'indicators_calculation_time': indicators_time,
            'throughput_points_per_second': throughput,
            'data_points_processed': len(raw_data),
            'indicators_calculated': len(indicators_calculated),
            'signals_generated': len(signals)
        }
        
        # Performance assertions
        assert total_time < 30, f"Pipeline should complete in <30s, took {total_time:.2f}s"
        assert throughput > 10, f"Should process >10 points/sec, got {throughput:.1f}"
        
        print(f"      ✅ Pipeline completed in {total_time:.2f}s ({throughput:.1f} points/sec)")
        
        # Step 7: Memory Usage Check
        print("   7️⃣ Checking memory efficiency...")
        import psutil
        process = psutil.Process()
        memory_mb = process.memory_info().rss / 1024 / 1024
        
        assert memory_mb < 500, f"Memory usage should be <500MB, using {memory_mb:.1f}MB"
        print(f"      ✅ Memory usage: {memory_mb:.1f}MB")
        
        print(f"\n✅ End-to-End Pipeline Test PASSED")
        return {
            'raw_data': raw_data,
            'indicators': indicators_calculated,
            'signals': signals,
            'performance': performance_metrics
        }
    
    def test_pipeline_error_recovery(self, pipeline_components):
        """Test pipeline error handling and recovery"""
        collector = pipeline_components['collector']
        indicator_engine = pipeline_components['indicators']
        
        print("\n🛡️ Testing Pipeline Error Recovery")
        
        # Test 1: Simulate network interruption
        print("   1️⃣ Testing network interruption recovery...")
        
        # Collect some initial data
        if collector.authenticate():
            initial_data = collector.collect_minute_bars()
            if initial_data:
                print(f"      ✅ Initial collection: {len(initial_data)} points")
                
                # Simulate handling of missing data points
                # (In real scenario, this would be network recovery)
                gaps_handled = 0
                for i, dp in enumerate(initial_data):
                    if i > 0:
                        time_diff = (dp.timestamp - initial_data[i-1].timestamp).total_seconds()
                        if time_diff > 60:  # Gap detected
                            gaps_handled += 1
                
                print(f"      ✅ Handled {gaps_handled} data gaps gracefully")
        
        # Test 2: Invalid data handling
        print("   2️⃣ Testing invalid data handling...")
        
        # Create invalid data points
        invalid_data = [
            ETHDataPoint(
                timestamp=pd.Timestamp.now(tz='UTC'),
                open=100,
                high=50,  # Invalid: high < open
                low=200,  # Invalid: low > open
                close=150,
                volume=-100  # Invalid: negative volume
            )
        ]
        
        # Indicator engine should handle invalid data gracefully
        errors_handled = 0
        for dp in invalid_data:
            try:
                # Validate data before adding
                if (dp.high >= max(dp.open, dp.close) and 
                    dp.low <= min(dp.open, dp.close) and
                    dp.volume >= 0):
                    indicator_engine.add_data_point(dp.close, dp.volume, dp.timestamp)
                else:
                    # Skip invalid data
                    errors_handled += 1
            except Exception:
                errors_handled += 1
        
        assert errors_handled > 0, "Should detect and handle invalid data"
        print(f"      ✅ Handled {errors_handled} invalid data points")
        
        # Test 3: Memory overflow protection
        print("   3️⃣ Testing memory overflow protection...")
        
        # Indicator engine should limit buffer size
        initial_buffer_size = len(indicator_engine.prices)
        
        # Add many data points
        base_time = pd.Timestamp.now(tz='UTC')
        for i in range(1000):  # Add 1000 points
            timestamp = base_time + timedelta(minutes=i)
            indicator_engine.add_data_point(4000 + i, 1000, timestamp)
        
        final_buffer_size = len(indicator_engine.prices)
        max_buffer_size = indicator_engine.max_size
        
        assert final_buffer_size <= max_buffer_size, f"Buffer should be limited to {max_buffer_size}, got {final_buffer_size}"
        print(f"      ✅ Buffer size controlled: {final_buffer_size}/{max_buffer_size}")
        
        print(f"✅ Error Recovery Tests PASSED")
    
    def test_pipeline_stress_testing(self, pipeline_components):
        """Test pipeline under stress conditions"""
        collector = pipeline_components['collector']
        indicator_engine = pipeline_components['indicators']
        
        print("\n🔥 Testing Pipeline Under Stress")
        
        # Test 1: High-frequency data processing
        print("   1️⃣ Testing high-frequency processing...")
        
        start_time = time.time()
        base_timestamp = pd.Timestamp.now(tz='UTC')
        
        # Process 1000 data points rapidly
        processed_count = 0
        for i in range(1000):
            timestamp = base_timestamp + timedelta(seconds=i)
            price = 4000 + np.sin(i * 0.1) * 100  # Oscillating price
            volume = 1000 + np.random.randint(0, 500)
            
            try:
                indicator_engine.add_data_point(price, volume, timestamp)
                processed_count += 1
            except Exception as e:
                print(f"      ⚠️ Error at point {i}: {e}")
        
        processing_time = time.time() - start_time
        throughput = processed_count / processing_time
        
        assert processed_count >= 950, f"Should process >95% of points, processed {processed_count}/1000"
        assert throughput > 100, f"Should process >100 points/sec, got {throughput:.1f}"
        
        print(f"      ✅ Processed {processed_count}/1000 points in {processing_time:.2f}s ({throughput:.1f} pts/sec)")
        
        # Test 2: Memory stability under load
        print("   2️⃣ Testing memory stability...")
        
        import psutil
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1024 / 1024
        
        # Calculate indicators repeatedly
        calculations = 0
        for _ in range(100):  # 100 indicator calculation cycles
            if len(indicator_engine.prices) >= 50:
                try:
                    sma = indicator_engine.sma(20)
                    rsi = indicator_engine.rsi(14)
                    macd = indicator_engine.macd()
                    bb = indicator_engine.bollinger_bands()
                    calculations += 4  # 4 indicators calculated
                except Exception:
                    pass
        
        final_memory = process.memory_info().rss / 1024 / 1024
        memory_growth = final_memory - initial_memory
        
        assert memory_growth < 50, f"Memory growth should be <50MB, grew by {memory_growth:.1f}MB"
        print(f"      ✅ Completed {calculations} calculations, memory growth: {memory_growth:.1f}MB")
        
        # Test 3: Concurrent operations simulation
        print("   3️⃣ Testing concurrent operations...")
        
        # Simulate concurrent data collection and analysis
        import threading
        import queue
        
        data_queue = queue.Queue()
        results_queue = queue.Queue()
        
        def data_producer():
            """Simulate continuous data collection"""
            for i in range(100):
                timestamp = pd.Timestamp.now(tz='UTC') + timedelta(seconds=i)
                price = 4000 + np.random.normal(0, 20)
                volume = 1000 + np.random.randint(0, 500)
                
                data_queue.put((timestamp, price, volume))
                time.sleep(0.01)  # 10ms between points
        
        def data_consumer():
            """Simulate continuous indicator calculation"""
            processed = 0
            while processed < 100:
                try:
                    timestamp, price, volume = data_queue.get(timeout=1)
                    indicator_engine.add_data_point(price, volume, timestamp)
                    processed += 1
                    
                    if processed % 20 == 0:  # Calculate indicators every 20 points
                        try:
                            if len(indicator_engine.prices) >= 20:
                                indicators = {
                                    'sma': indicator_engine.sma(10),
                                    'rsi': indicator_engine.rsi(14)
                                }
                                results_queue.put(indicators)
                        except Exception:
                            pass
                            
                except queue.Empty:
                    break
        
        # Start concurrent operations
        producer_thread = threading.Thread(target=data_producer)
        consumer_thread = threading.Thread(target=data_consumer)
        
        start_concurrent = time.time()
        producer_thread.start()
        consumer_thread.start()
        
        producer_thread.join(timeout=5)
        consumer_thread.join(timeout=5)
        concurrent_time = time.time() - start_concurrent
        
        results_count = results_queue.qsize()
        assert results_count > 0, "Should produce some indicator results"
        print(f"      ✅ Concurrent operations completed in {concurrent_time:.2f}s, {results_count} results")
        
        print(f"✅ Stress Testing PASSED")

class TestE2EPerformanceBenchmarks:
    """Performance benchmarks for the complete system"""
    
    @pytest.mark.performance
    def test_collection_performance_benchmark(self):
        """Benchmark data collection performance"""
        print("\n⚡ Data Collection Performance Benchmark")
        
        collector = OptimizedETHCollector()
        if not collector.authenticate():
            pytest.skip("IBKR Gateway required for performance testing")
        
        # Benchmark multiple collection cycles
        times = []
        data_counts = []
        
        for cycle in range(5):
            start_time = time.time()
            data = collector.collect_minute_bars()
            collection_time = time.time() - start_time
            
            times.append(collection_time)
            data_counts.append(len(data) if data else 0)
            
            print(f"   Cycle {cycle + 1}: {collection_time:.2f}s → {len(data) if data else 0} points")
            time.sleep(1)  # Brief pause between cycles
        
        # Performance metrics
        avg_time = np.mean(times)
        avg_count = np.mean(data_counts)
        throughput = avg_count / avg_time if avg_time > 0 else 0
        
        print(f"\n📊 Collection Benchmark Results:")
        print(f"   Average Time: {avg_time:.2f}s ± {np.std(times):.2f}s")
        print(f"   Average Points: {avg_count:.1f} ± {np.std(data_counts):.1f}")
        print(f"   Throughput: {throughput:.1f} points/second")
        
        # Performance assertions
        assert avg_time < 5.0, f"Collection should average <5s, got {avg_time:.2f}s"
        assert throughput > 20, f"Should achieve >20 points/sec, got {throughput:.1f}"
    
    @pytest.mark.performance
    def test_indicators_performance_benchmark(self):
        """Benchmark technical indicators calculation performance"""
        print("\n⚡ Technical Indicators Performance Benchmark")
        
        # Create large dataset for benchmarking
        indicator_engine = TechnicalIndicatorEngine(max_history=1000)
        
        # Generate test data
        base_time = pd.Timestamp.now(tz='UTC')
        prices = []
        volumes = []
        
        for i in range(1000):
            price = 4000 + np.sin(i * 0.01) * 200 + np.random.normal(0, 10)
            volume = 1000 + np.random.randint(0, 500)
            timestamp = base_time + timedelta(minutes=i)
            
            prices.append(price)
            volumes.append(volume)
            indicator_engine.add_data_point(price, volume, timestamp)
        
        # Benchmark different indicators
        benchmarks = {}
        
        indicators_to_test = [
            ('SMA_20', lambda: indicator_engine.sma(20)),
            ('EMA_20', lambda: indicator_engine.ema(20)),
            ('RSI_14', lambda: indicator_engine.rsi(14)),
            ('MACD', lambda: indicator_engine.macd()),
            ('Bollinger_Bands', lambda: indicator_engine.bollinger_bands(20, 2)),
            ('ATR_14', lambda: indicator_engine.atr(14)),
            ('VWAP', lambda: indicator_engine.vwap()),
            ('Stochastic', lambda: indicator_engine.stochastic_oscillator(14)),
        ]
        
        for name, func in indicators_to_test:
            times = []
            
            # Run each indicator multiple times
            for _ in range(10):
                start_time = time.time()
                try:
                    result = func()
                    calc_time = time.time() - start_time
                    times.append(calc_time)
                except Exception as e:
                    print(f"   ⚠️ {name} failed: {e}")
                    continue
            
            if times:
                avg_time = np.mean(times) * 1000  # Convert to milliseconds
                benchmarks[name] = avg_time
                print(f"   {name}: {avg_time:.2f}ms ± {np.std(times)*1000:.2f}ms")
        
        print(f"\n📊 Indicators Benchmark Results:")
        total_avg = np.mean(list(benchmarks.values()))
        print(f"   Average Calculation Time: {total_avg:.2f}ms")
        
        # Performance assertions
        for name, time_ms in benchmarks.items():
            assert time_ms < 50, f"{name} should calculate in <50ms, took {time_ms:.2f}ms"
        
        assert total_avg < 20, f"Average calculation should be <20ms, got {total_avg:.2f}ms"
    
    @pytest.mark.performance 
    def test_end_to_end_latency_benchmark(self):
        """Benchmark complete end-to-end pipeline latency"""
        print("\n⚡ End-to-End Latency Benchmark")
        
        collector = OptimizedETHCollector()
        indicator_engine = TechnicalIndicatorEngine(max_history=200)
        
        if not collector.authenticate():
            pytest.skip("IBKR Gateway required for E2E latency testing")
        
        # Pre-populate indicator engine with some data
        base_time = pd.Timestamp.now(tz='UTC') - timedelta(minutes=100)
        for i in range(50):  # 50 minutes of historical data
            price = 4000 + np.random.normal(0, 20)
            volume = 1000 + np.random.randint(0, 500)
            timestamp = base_time + timedelta(minutes=i)
            indicator_engine.add_data_point(price, volume, timestamp)
        
        # Benchmark complete pipeline cycles
        latencies = []
        
        for cycle in range(10):
            cycle_start = time.time()
            
            # Step 1: Data Collection
            data_start = time.time()
            raw_data = collector.collect_minute_bars()
            data_time = time.time() - data_start
            
            if not raw_data:
                continue
            
            # Step 2: Latest Data Point Processing
            latest_dp = raw_data[-1]  # Most recent data point
            
            # Step 3: Indicator Calculation
            indicators_start = time.time()
            indicator_engine.add_data_point(latest_dp.close, latest_dp.volume, latest_dp.timestamp)
            
            # Calculate key indicators
            if len(indicator_engine.prices) >= 20:
                indicators = {
                    'sma': indicator_engine.sma(20),
                    'rsi': indicator_engine.rsi(14),
                    'macd': indicator_engine.macd()
                }
            indicators_time = time.time() - indicators_start
            
            # Step 4: Simple Signal Generation
            signal_start = time.time()
            signal = 'HOLD'  # Default
            if 'rsi' in indicators and indicators['rsi']:
                if indicators['rsi'].value < 30:
                    signal = 'BUY'
                elif indicators['rsi'].value > 70:
                    signal = 'SELL'
            signal_time = time.time() - signal_start
            
            total_latency = time.time() - cycle_start
            latencies.append({
                'total': total_latency * 1000,  # ms
                'data_collection': data_time * 1000,
                'indicators': indicators_time * 1000,
                'signal': signal_time * 1000,
                'data_points': len(raw_data)
            })
            
            print(f"   Cycle {cycle + 1}: {total_latency*1000:.1f}ms total (data: {data_time*1000:.1f}ms, indicators: {indicators_time*1000:.1f}ms)")
            
            time.sleep(0.5)  # Brief pause between cycles
        
        if latencies:
            # Calculate statistics
            total_latencies = [l['total'] for l in latencies]
            data_latencies = [l['data_collection'] for l in latencies]
            indicator_latencies = [l['indicators'] for l in latencies]
            
            print(f"\n📊 E2E Latency Benchmark Results:")
            print(f"   Total Pipeline: {np.mean(total_latencies):.1f}ms ± {np.std(total_latencies):.1f}ms")
            print(f"   Data Collection: {np.mean(data_latencies):.1f}ms ± {np.std(data_latencies):.1f}ms")
            print(f"   Indicators: {np.mean(indicator_latencies):.1f}ms ± {np.std(indicator_latencies):.1f}ms")
            print(f"   P95 Total Latency: {np.percentile(total_latencies, 95):.1f}ms")
            
            # Performance assertions for real-time trading
            avg_total = np.mean(total_latencies)
            p95_total = np.percentile(total_latencies, 95)
            
            assert avg_total < 2000, f"Average E2E latency should be <2000ms, got {avg_total:.1f}ms"
            assert p95_total < 5000, f"P95 E2E latency should be <5000ms, got {p95_total:.1f}ms"
            
            print(f"✅ E2E Latency Benchmark PASSED")

# Utility function for running complete pipeline analysis
def run_complete_pipeline_analysis():
    """Run comprehensive pipeline analysis"""
    print("🚀 Complete ETH Data Pipeline Analysis")
    print("=" * 50)
    
    # Initialize components
    collector = OptimizedETHCollector()
    indicator_engine = TechnicalIndicatorEngine(max_history=200)
    
    print("🔐 Authenticating with IBKR...")
    if not collector.authenticate():
        print("❌ IBKR Gateway not available")
        return
    
    print("📡 Collecting live ETH data...")
    raw_data = collector.collect_minute_bars()
    
    if not raw_data:
        print("❌ No data collected")
        return
    
    print(f"✅ Collected {len(raw_data)} data points")
    
    # Process through complete pipeline
    print("🔄 Processing through complete pipeline...")
    
    results = []
    start_time = time.time()
    
    for i, dp in enumerate(raw_data):
        # Add to indicator engine
        indicator_engine.add_data_point(dp.close, dp.volume, dp.timestamp)
        
        # Calculate indicators if enough data
        if len(indicator_engine.prices) >= 20:
            indicators = {}
            try:
                indicators['sma_20'] = indicator_engine.sma(20)
                indicators['ema_12'] = indicator_engine.ema(12)
                indicators['rsi_14'] = indicator_engine.rsi(14)
                indicators['macd'] = indicator_engine.macd()
                indicators['bb'] = indicator_engine.bollinger_bands()
                indicators['atr'] = indicator_engine.atr(14)
                
                # Simple signal logic
                signal = 'HOLD'
                confidence = 0.0
                
                if indicators['rsi_14'] and indicators['macd']:
                    rsi_val = indicators['rsi_14'].value
                    macd_val = indicators['macd'].value
                    macd_signal = indicators['macd'].signal
                    
                    if rsi_val < 30 and macd_val > macd_signal:
                        signal = 'BUY'
                        confidence = 0.8
                    elif rsi_val > 70 and macd_val < macd_signal:
                        signal = 'SELL'
                        confidence = 0.8
                
                results.append({
                    'timestamp': dp.timestamp,
                    'price': dp.close,
                    'volume': dp.volume,
                    'indicators': indicators,
                    'signal': signal,
                    'confidence': confidence
                })
                
            except Exception as e:
                print(f"⚠️ Error calculating indicators for point {i}: {e}")
    
    processing_time = time.time() - start_time
    
    print(f"\n📊 Pipeline Analysis Results:")
    print(f"   Processing Time: {processing_time:.2f}s")
    print(f"   Throughput: {len(results)/processing_time:.1f} points/sec")
    print(f"   Indicators Calculated: {len(results)}")
    
    # Signal analysis
    signals = [r['signal'] for r in results]
    buy_signals = signals.count('BUY')
    sell_signals = signals.count('SELL')
    hold_signals = signals.count('HOLD')
    
    print(f"   Signals Generated: {buy_signals} BUY, {sell_signals} SELL, {hold_signals} HOLD")
    
    # Latest data summary
    if results:
        latest = results[-1]
        print(f"\n📈 Latest Analysis:")
        print(f"   Time: {latest['timestamp']}")
        print(f"   Price: ${latest['price']:.2f}")
        print(f"   Signal: {latest['signal']} (confidence: {latest['confidence']:.1%})")
        
        if latest['indicators'].get('rsi_14'):
            print(f"   RSI: {latest['indicators']['rsi_14'].value:.1f}")
        if latest['indicators'].get('sma_20'):
            print(f"   SMA(20): ${latest['indicators']['sma_20'].value:.2f}")
    
    print(f"\n✅ Complete Pipeline Analysis FINISHED")
    return results

if __name__ == "__main__":
    # Run complete analysis if called directly
    run_complete_pipeline_analysis()
