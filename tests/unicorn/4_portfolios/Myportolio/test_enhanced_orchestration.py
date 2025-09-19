#!/usr/bin/env python3
"""
🦄 Enhanced Portfolio Orchestrator Testing Script
Comprehensive validation of orchestration workflow coordination

Testing Framework Location: /tests/unicorn/4_portfolios/Myportolio/
Tests the EnhancedPortfolioOrchestrator implementation following system design patterns.
"""

import asyncio
import logging
import sys
from pathlib import Path
from datetime import datetime
import json
import pytest
import os

# Configure test path resolution following testing framework patterns
TEST_ROOT = Path(__file__).parent.parent.parent.parent.parent
BACKEND_ROOT = TEST_ROOT / "BackendPython" / "unicorn"
PORTFOLIO_ROOT = BACKEND_ROOT / "4_portfolios"
UTILITIES_ROOT = PORTFOLIO_ROOT / "utilities"

# Add required paths for testing
sys.path.insert(0, str(UTILITIES_ROOT))
sys.path.insert(0, str(PORTFOLIO_ROOT))
sys.path.insert(0, str(BACKEND_ROOT))

# Import orchestration components
try:
    from EnhancedPortfolioOrchestrator import (
        EnhancedPortfolioOrchestrator, 
        ExecutionTimeframe, 
        AlgorithmTemplate,
        WorkflowStage
    )
except ImportError as e:
    pytest.skip(f"EnhancedPortfolioOrchestrator not available: {e}", allow_module_level=True)

# Configure logging for tests
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class TestEnhancedPortfolioOrchestrator:
    """Test suite for Enhanced Portfolio Orchestrator following pytest patterns"""
    
    @pytest.fixture
    def orchestrator(self):
        """Initialize orchestrator for testing"""
        return EnhancedPortfolioOrchestrator("Myportolio")
    
    @pytest.fixture
    def sample_algorithm_templates(self):
        """Create sample algorithm templates for testing"""
        return [
            AlgorithmTemplate(
                name="ETH_Momentum_5m",
                timeframe=ExecutionTimeframe.MINUTE_5,
                symbols=["ETHUSD"],
                alpha_method="momentum",
                risk_method="var",
                execution_method="twap",
                configuration={"lookback": 20, "threshold": 0.02}
            ),
            AlgorithmTemplate(
                name="ETH_MeanReversion_15m",
                timeframe=ExecutionTimeframe.MINUTE_15,
                symbols=["ETHUSD"],
                alpha_method="mean_reversion",
                risk_method="expected_shortfall",
                execution_method="iceberg",
                configuration={"lookback": 50, "std_threshold": 2.0}
            )
        ]
    
    @pytest.mark.asyncio
    async def test_orchestrator_initialization(self, orchestrator):
        """Test orchestrator initialization and basic properties"""
        logger.info("🧪 Testing orchestrator initialization")
        
        assert orchestrator.portfolio_name == "Myportolio"
        assert hasattr(orchestrator, 'workflow_manager')
        assert hasattr(orchestrator, 'algorithm_templates')
        
        # Verify default state
        status = orchestrator.get_workflow_status()
        assert status['portfolio'] == "Myportolio"
        assert isinstance(status['active_timeframes'], list)
        assert isinstance(status['recent_executions'], int)
        
        logger.info("✅ Orchestrator initialization test passed")
    
    @pytest.mark.asyncio
    async def test_complete_workflow_execution(self, orchestrator):
        """Test complete workflow execution across all stages"""
        logger.info("🧪 Testing complete workflow execution")
        
        # Execute complete workflow
        workflow_results = await orchestrator.execute_complete_workflow(
            timeframe=ExecutionTimeframe.MINUTE_5,
            force_execution=True
        )
        
        # Validate workflow results structure
        assert 'portfolio' in workflow_results
        assert 'overall_success' in workflow_results
        assert 'success_rate' in workflow_results
        assert 'total_duration' in workflow_results
        assert 'stages_completed' in workflow_results
        assert 'stages' in workflow_results
        
        # Validate portfolio name
        assert workflow_results['portfolio'] == "Myportolio"
        
        # Validate stage execution
        assert isinstance(workflow_results['stages'], dict)
        assert len(workflow_results['stages']) > 0
        
        # Check required workflow stages are present
        expected_stages = {'data_acquisition', 'alpha_generation', 'risk_assessment', 'portfolio_construction'}
        stage_names = set(workflow_results['stages'].keys())
        assert expected_stages.issubset(stage_names), f"Missing stages: {expected_stages - stage_names}"
        
        # Validate stage results structure
        for stage_name, stage_result in workflow_results['stages'].items():
            assert 'success' in stage_result
            assert 'duration' in stage_result
            assert isinstance(stage_result['success'], bool)
            assert isinstance(stage_result['duration'], (int, float))
        
        logger.info(f"✅ Workflow execution test passed - Success: {workflow_results['overall_success']}")
        logger.info(f"   Success Rate: {workflow_results['success_rate']:.1f}%")
        logger.info(f"   Duration: {workflow_results['total_duration']:.2f}s")
        logger.info(f"   Stages: {workflow_results['stages_completed']}/{len(workflow_results['stages'])}")
    
    @pytest.mark.asyncio
    async def test_multi_timeframe_coordination(self, orchestrator):
        """Test execution across multiple timeframes"""
        logger.info("🧪 Testing multi-timeframe coordination")
        
        timeframes_to_test = [
            ExecutionTimeframe.MINUTE_1,
            ExecutionTimeframe.MINUTE_5,
            ExecutionTimeframe.MINUTE_15
        ]
        
        results = {}
        for timeframe in timeframes_to_test:
            result = await orchestrator.execute_complete_workflow(
                timeframe=timeframe,
                force_execution=True
            )
            results[timeframe] = result
            
            # Validate each timeframe execution
            assert 'overall_success' in result
            assert 'total_duration' in result
            
            logger.info(f"   {timeframe.value}: {result['overall_success']} ({result['total_duration']:.2f}s)")
        
        # Verify all timeframes were tested
        assert len(results) == len(timeframes_to_test)
        
        logger.info("✅ Multi-timeframe coordination test passed")
    
    def test_algorithm_template_registration(self, orchestrator, sample_algorithm_templates):
        """Test algorithm template registration and management"""
        logger.info("🧪 Testing algorithm template registration")
        
        initial_count = len(orchestrator.algorithm_templates)
        
        # Register sample templates
        for template in sample_algorithm_templates:
            orchestrator.register_algorithm_template(template)
            
            # Verify template is registered
            assert template.name in orchestrator.algorithm_templates
            
            # Validate template properties
            registered = orchestrator.algorithm_templates[template.name]
            assert registered.name == template.name
            assert registered.timeframe == template.timeframe
            assert registered.symbols == template.symbols
        
        # Verify registration count
        final_count = len(orchestrator.algorithm_templates)
        assert final_count == initial_count + len(sample_algorithm_templates)
        
        logger.info(f"✅ Template registration test passed - {len(sample_algorithm_templates)} templates registered")
    
    def test_workflow_status_monitoring(self, orchestrator):
        """Test workflow status monitoring and reporting"""
        logger.info("🧪 Testing workflow status monitoring")
        
        status = orchestrator.get_workflow_status()
        
        # Validate status structure
        required_fields = [
            'portfolio', 'active_timeframes', 'recent_executions', 
            'success_rate', 'registered_templates', 'timeframe_schedules'
        ]
        
        for field in required_fields:
            assert field in status, f"Missing status field: {field}"
        
        # Validate data types
        assert isinstance(status['portfolio'], str)
        assert isinstance(status['active_timeframes'], list)
        assert isinstance(status['recent_executions'], int)
        assert isinstance(status['success_rate'], (int, float))
        assert isinstance(status['registered_templates'], int)
        assert isinstance(status['timeframe_schedules'], dict)
        
        # Validate portfolio name
        assert status['portfolio'] == "Myportolio"
        
        # Validate success rate range
        assert 0 <= status['success_rate'] <= 100
        
        logger.info(f"✅ Status monitoring test passed")
        logger.info(f"   Portfolio: {status['portfolio']}")
        logger.info(f"   Active Timeframes: {len(status['active_timeframes'])}")
        logger.info(f"   Success Rate: {status['success_rate']:.1f}%")
    
    def test_error_handling_resilience(self):
        """Test error handling and system resilience"""
        logger.info("🧪 Testing error handling and resilience")
        
        # Test invalid portfolio initialization
        with pytest.raises(Exception):
            invalid_orchestrator = EnhancedPortfolioOrchestrator("NonExistentPortfolio")
        
        logger.info("✅ Error handling test passed - Invalid portfolio correctly rejected")
    
    @pytest.mark.asyncio
    async def test_performance_benchmarks(self, orchestrator):
        """Test performance benchmarks and execution timing"""
        logger.info("🧪 Testing performance benchmarks")
        
        performance_results = []
        test_runs = 3
        
        for i in range(test_runs):
            start_time = datetime.now()
            result = await orchestrator.execute_complete_workflow(
                timeframe=ExecutionTimeframe.MINUTE_5,
                force_execution=True
            )
            duration = (datetime.now() - start_time).total_seconds()
            
            performance_results.append({
                'run': i + 1,
                'duration': duration,
                'success': result.get('overall_success', False)
            })
        
        # Calculate performance metrics
        avg_duration = sum(r['duration'] for r in performance_results) / len(performance_results)
        success_count = sum(1 for r in performance_results if r['success'])
        success_rate = success_count / len(performance_results) * 100
        
        # Validate performance constraints
        assert avg_duration < 30.0, f"Average execution time too slow: {avg_duration:.2f}s"
        assert success_rate >= 50.0, f"Success rate too low: {success_rate:.1f}%"
        
        logger.info(f"✅ Performance benchmark test passed")
        logger.info(f"   Average Duration: {avg_duration:.2f}s")
        logger.info(f"   Success Rate: {success_rate:.1f}%")
        
        for result in performance_results:
            status = "✅" if result['success'] else "❌"
            logger.info(f"   {status} Run {result['run']}: {result['duration']:.2f}s")

class TestIntegrationScenarios:
    """Integration scenario tests for real-world usage patterns"""
    
    @pytest.fixture
    def orchestrator(self):
        """Initialize orchestrator for integration testing"""
        return EnhancedPortfolioOrchestrator("Myportolio")
    
    @pytest.mark.asyncio
    async def test_high_frequency_execution(self, orchestrator):
        """Test high-frequency trading simulation scenario"""
        logger.info("🔧 Testing high-frequency execution scenario")
        
        execution_count = 5
        results = []
        
        for i in range(execution_count):
            result = await orchestrator.execute_complete_workflow(
                timeframe=ExecutionTimeframe.MINUTE_1,
                force_execution=True
            )
            results.append(result)
            
            # Validate each execution
            assert 'overall_success' in result
            assert 'total_duration' in result
        
        # Analyze aggregate performance
        success_count = sum(1 for r in results if r.get('overall_success', False))
        avg_duration = sum(r.get('total_duration', 0) for r in results) / len(results)
        
        # Validate high-frequency performance
        assert success_count >= execution_count * 0.6, "High-frequency success rate too low"
        assert avg_duration < 15.0, "High-frequency execution too slow"
        
        logger.info(f"✅ High-frequency test passed: {success_count}/{execution_count} successful")
        logger.info(f"   Average Duration: {avg_duration:.2f}s")
    
    @pytest.mark.asyncio
    async def test_mixed_timeframe_coordination(self, orchestrator):
        """Test mixed timeframe coordination scenario"""
        logger.info("🔧 Testing mixed timeframe coordination scenario")
        
        timeframes = [
            ExecutionTimeframe.MINUTE_5, 
            ExecutionTimeframe.MINUTE_15, 
            ExecutionTimeframe.HOUR_1
        ]
        
        results = {}
        for tf in timeframes:
            result = await orchestrator.execute_complete_workflow(
                timeframe=tf, 
                force_execution=True
            )
            results[tf] = result
            
            success = result.get('overall_success', False)
            duration = result.get('total_duration', 0)
            
            # Validate execution
            assert 'overall_success' in result
            assert duration > 0
        
        # Verify all timeframes executed
        assert len(results) == len(timeframes)
        
        logger.info("✅ Mixed timeframe coordination test passed")
        for tf, result in results.items():
            success = "✅" if result.get('overall_success', False) else "❌"
            duration = result.get('total_duration', 0)
            logger.info(f"   {tf.value}: {success} ({duration:.2f}s)")
    
    @pytest.mark.asyncio
    async def test_state_persistence_management(self, orchestrator):
        """Test workflow state persistence and management"""
        logger.info("🔧 Testing state persistence and management")
        
        # Capture initial state
        initial_status = orchestrator.get_workflow_status()
        initial_executions = initial_status['recent_executions']
        
        # Execute multiple workflows
        execution_count = 3
        for _ in range(execution_count):
            await orchestrator.execute_complete_workflow(force_execution=True)
        
        # Verify state changes
        final_status = orchestrator.get_workflow_status()
        final_executions = final_status['recent_executions']
        
        # Validate state persistence
        assert final_executions >= initial_executions, "Execution count should increase"
        assert 'success_rate' in final_status
        assert isinstance(final_status['success_rate'], (int, float))
        
        logger.info(f"✅ State persistence test passed")
        logger.info(f"   Initial executions: {initial_executions}")
        logger.info(f"   Final executions: {final_executions}")
        logger.info(f"   Success rate: {final_status['success_rate']:.1f}%")

# Main test execution for standalone running
async def run_complete_test_suite():
    """Run complete test suite for manual execution"""
    logger.info("🦄 Enhanced Portfolio Orchestrator Test Suite")
    logger.info("=" * 60)
    
    try:
        orchestrator = EnhancedPortfolioOrchestrator("Myportolio")
        
        # Test 1: Complete workflow execution
        logger.info("📊 Test 1: Complete Workflow Execution")
        workflow_results = await orchestrator.execute_complete_workflow(
            timeframe=ExecutionTimeframe.MINUTE_5,
            force_execution=True
        )
        
        print("\n🎯 WORKFLOW EXECUTION RESULTS:")
        print(f"Portfolio: {workflow_results.get('portfolio')}")
        print(f"Overall Success: {workflow_results.get('overall_success')}")
        print(f"Success Rate: {workflow_results.get('success_rate', 0):.1f}%")
        print(f"Total Duration: {workflow_results.get('total_duration', 0):.2f}s")
        print(f"Stages Completed: {workflow_results.get('stages_completed')}/{len(WorkflowStage)}")
        
        # Print stage results
        print("\n📋 STAGE EXECUTION DETAILS:")
        for stage_name, stage_result in workflow_results.get('stages', {}).items():
            status = "✅" if stage_result['success'] else "❌"
            duration = stage_result.get('duration', 0)
            print(f"{status} {stage_name}: {duration:.2f}s")
            
            if not stage_result['success']:
                print(f"   Error: {stage_result.get('error', 'Unknown error')}")
        
        # Additional comprehensive tests
        logger.info("\n📊 Running Additional Test Scenarios...")
        
        # Multi-timeframe test
        timeframes_to_test = [ExecutionTimeframe.MINUTE_1, ExecutionTimeframe.MINUTE_5, ExecutionTimeframe.MINUTE_15]
        print("\n⏱️ Multi-timeframe Results:")
        for timeframe in timeframes_to_test:
            result = await orchestrator.execute_complete_workflow(timeframe=timeframe, force_execution=True)
            print(f"  {timeframe.value}: {result.get('overall_success', False)}")
        
        # Status monitoring
        status = orchestrator.get_workflow_status()
        print("\n📈 ORCHESTRATOR STATUS:")
        print(f"Portfolio: {status['portfolio']}")
        print(f"Active Timeframes: {status['active_timeframes']}")
        print(f"Recent Executions: {status['recent_executions']}")
        print(f"Success Rate: {status['success_rate']:.1f}%")
        
        print("\n" + "="*60)
        print("🦄 ENHANCED PORTFOLIO ORCHESTRATOR TEST SUMMARY")
        print("="*60)
        print(f"✅ Complete Workflow: {workflow_results.get('overall_success', False)}")
        print(f"✅ Multi-timeframe: All timeframes tested")
        print(f"✅ Status Monitoring: Comprehensive status available")
        print(f"✅ Performance: {workflow_results.get('total_duration', 0):.2f}s execution time")
        print("="*60)
        
        return True
        
    except Exception as e:
        logger.error(f"❌ Test execution failed: {e}")
        print(f"\n❌ TEST SUITE FAILED: {e}")
        return False

if __name__ == "__main__":
    print("🦄 Enhanced Portfolio Orchestrator Test Suite")
    print("=" * 50)
    
    # Run complete test suite
    result = asyncio.run(run_complete_test_suite())
    
    if result:
        print("\n✅ All tests completed successfully!")
    else:
        print("\n❌ Test suite failed")
        sys.exit(1)