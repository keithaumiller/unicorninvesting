<?php

namespace Drupal\Tests\unicornmetrics\Unit\Service;

use Drupal\Core\Config\ConfigFactoryInterface;
use Drupal\Core\Logger\LoggerChannelFactoryInterface;
use Drupal\Core\Logger\LoggerChannelInterface;
use Drupal\Tests\UnitTestBase;
use Drupal\unicornmetrics\Service\PortfolioApiService;

/**
 * Unit tests for PortfolioApiService.
 *
 * @group unicornmetrics
 * @group service
 * @coversDefaultClass \Drupal\unicornmetrics\Service\PortfolioApiService
 */
class PortfolioApiServiceTest extends UnitTestBase {

  /**
   * The portfolio API service under test.
   *
   * @var \Drupal\unicornmetrics\Service\PortfolioApiService
   */
  protected $portfolioApiService;

  /**
   * Mock config factory.
   *
   * @var \Drupal\Core\Config\ConfigFactoryInterface|\PHPUnit\Framework\MockObject\MockObject
   */
  protected $configFactory;

  /**
   * Mock logger factory.
   *
   * @var \Drupal\Core\Logger\LoggerChannelFactoryInterface|\PHPUnit\Framework\MockObject\MockObject
   */
  protected $loggerFactory;

  /**
   * Mock logger channel.
   *
   * @var \Drupal\Core\Logger\LoggerChannelInterface|\PHPUnit\Framework\MockObject\MockObject
   */
  protected $logger;

  /**
   * Test backend path.
   *
   * @var string
   */
  protected $testBackendPath;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    // Create a temporary directory for testing
    $this->testBackendPath = sys_get_temp_dir() . '/test_portfolios_' . uniqid();
    mkdir($this->testBackendPath, 0755, TRUE);

    $this->configFactory = $this->createMock(ConfigFactoryInterface::class);
    $this->logger = $this->createMock(LoggerChannelInterface::class);
    $this->loggerFactory = $this->createMock(LoggerChannelFactoryInterface::class);
    
    $this->loggerFactory->expects($this->any())
      ->method('get')
      ->with('unicornmetrics')
      ->willReturn($this->logger);

    $this->portfolioApiService = new PortfolioApiService(
      $this->configFactory,
      $this->loggerFactory
    );

    // Use reflection to set the backend path for testing
    $reflection = new \ReflectionClass($this->portfolioApiService);
    $property = $reflection->getProperty('backendPath');
    $property->setAccessible(TRUE);
    $property->setValue($this->portfolioApiService, $this->testBackendPath);
  }

  /**
   * {@inheritdoc}
   */
  protected function tearDown(): void {
    // Clean up test directory
    if (is_dir($this->testBackendPath)) {
      $this->recursiveRemoveDirectory($this->testBackendPath);
    }
    parent::tearDown();
  }

  /**
   * Tests getPortfolioConfig with valid config file.
   *
   * @covers ::getPortfolioConfig
   */
  public function testGetPortfolioConfigValidFile(): void {
    $portfolio_name = 'TestPortfolio';
    $config_data = [
      'portfolio_name' => $portfolio_name,
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.20,
    ];

    // Create test directory and config file
    $portfolio_dir = $this->testBackendPath . '/' . $portfolio_name;
    mkdir($portfolio_dir, 0755, TRUE);
    file_put_contents($portfolio_dir . '/config.json', json_encode($config_data));

    $result = $this->portfolioApiService->getPortfolioConfig($portfolio_name);

    $this->assertEquals($config_data, $result);
  }

  /**
   * Tests getPortfolioConfig with missing config file.
   *
   * @covers ::getPortfolioConfig
   * @covers ::getFallbackConfigData
   */
  public function testGetPortfolioConfigMissingFile(): void {
    $portfolio_name = 'NonExistentPortfolio';

    $this->logger->expects($this->once())
      ->method('warning')
      ->with(
        $this->stringContains('Portfolio config file not found'),
        $this->isType('array')
      );

    $result = $this->portfolioApiService->getPortfolioConfig($portfolio_name);

    // Should return fallback data
    $this->assertEquals($portfolio_name, $result['portfolio_name']);
    $this->assertEquals('fallback_data', $result['backend_status']);
    $this->assertArrayHasKey('assets', $result);
  }

  /**
   * Tests getPortfolioConfig with malformed JSON.
   *
   * @covers ::getPortfolioConfig
   * @covers ::getFallbackConfigData
   */
  public function testGetPortfolioConfigMalformedJson(): void {
    $portfolio_name = 'TestPortfolio';

    // Create test directory with invalid JSON
    $portfolio_dir = $this->testBackendPath . '/' . $portfolio_name;
    mkdir($portfolio_dir, 0755, TRUE);
    file_put_contents($portfolio_dir . '/config.json', '{"invalid": json}');

    $this->logger->expects($this->once())
      ->method('warning')
      ->with(
        $this->stringContains('Portfolio config file not found'),
        $this->isType('array')
      );

    $result = $this->portfolioApiService->getPortfolioConfig($portfolio_name);

    // Should return fallback data
    $this->assertEquals($portfolio_name, $result['portfolio_name']);
    $this->assertEquals('fallback_data', $result['backend_status']);
  }

  /**
   * Tests getAvailablePortfolios with valid portfolios.
   *
   * @covers ::getAvailablePortfolios
   */
  public function testGetAvailablePortfoliosValid(): void {
    $portfolios = ['Portfolio1', 'Portfolio2', 'Portfolio3'];

    // Create test directories with config files
    foreach ($portfolios as $portfolio) {
      $portfolio_dir = $this->testBackendPath . '/' . $portfolio;
      mkdir($portfolio_dir, 0755, TRUE);
      file_put_contents($portfolio_dir . '/config.json', '{"name": "' . $portfolio . '"}');
    }

    // Create a directory without config file (should be ignored)
    mkdir($this->testBackendPath . '/InvalidPortfolio', 0755, TRUE);

    $result = $this->portfolioApiService->getAvailablePortfolios();

    $this->assertCount(3, $result);
    $this->assertContains('Portfolio1', $result);
    $this->assertContains('Portfolio2', $result);
    $this->assertContains('Portfolio3', $result);
    $this->assertNotContains('InvalidPortfolio', $result);
  }

  /**
   * Tests getAvailablePortfolios with no portfolios.
   *
   * @covers ::getAvailablePortfolios
   */
  public function testGetAvailablePortfoliosEmpty(): void {
    $result = $this->portfolioApiService->getAvailablePortfolios();

    // Should return default portfolio
    $this->assertEquals(['Myportolio'], $result);
  }

  /**
   * Tests getEthAlgorithmStatus with existing directories.
   *
   * @covers ::getEthAlgorithmStatus
   */
  public function testGetEthAlgorithmStatusValid(): void {
    $portfolio_name = 'TestPortfolio';

    // Create portfolio directory structure
    $portfolio_dir = $this->testBackendPath . '/' . $portfolio_name;
    mkdir($portfolio_dir . '/risk_algorithms', 0755, TRUE);
    mkdir($portfolio_dir . '/trading_algorithms', 0755, TRUE);

    $result = $this->portfolioApiService->getEthAlgorithmStatus($portfolio_name);

    $this->assertTrue($result['risk_algorithm']['available']);
    $this->assertTrue($result['trading_algorithm']['available']);
    $this->assertEquals('operational', $result['integration_status']);
    $this->assertEquals('active', $result['risk_algorithm']['status']);
    $this->assertEquals('active', $result['trading_algorithm']['status']);
  }

  /**
   * Tests getEthAlgorithmStatus with missing directories.
   *
   * @covers ::getEthAlgorithmStatus
   */
  public function testGetEthAlgorithmStatusMissing(): void {
    $portfolio_name = 'TestPortfolio';

    $result = $this->portfolioApiService->getEthAlgorithmStatus($portfolio_name);

    $this->assertFalse($result['risk_algorithm']['available']);
    $this->assertFalse($result['trading_algorithm']['available']);
    $this->assertEquals('operational', $result['integration_status']);
  }

  /**
   * Tests getPortfolioStatus fallback behavior.
   *
   * @covers ::getPortfolioStatus
   * @covers ::getFallbackPortfolioData
   */
  public function testGetPortfolioStatusFallback(): void {
    $portfolio_name = 'TestPortfolio';

    $this->logger->expects($this->once())
      ->method('error')
      ->with(
        $this->stringContains('Failed to get portfolio status'),
        $this->isType('array')
      );

    $result = $this->portfolioApiService->getPortfolioStatus($portfolio_name);

    // Should return fallback data
    $this->assertEquals($portfolio_name, $result['portfolio_name']);
    $this->assertEquals('fallback_data', $result['backend_status']);
    $this->assertTrue($result['validation_passed']);
    $this->assertEquals('active', $result['status']);
  }

  /**
   * Tests getRiskMetrics fallback behavior.
   *
   * @covers ::getRiskMetrics
   * @covers ::getFallbackRiskMetrics
   */
  public function testGetRiskMetricsFallback(): void {
    $portfolio_name = 'TestPortfolio';

    $this->logger->expects($this->once())
      ->method('error')
      ->with(
        $this->stringContains('Exception getting risk metrics'),
        $this->isType('array')
      );

    $result = $this->portfolioApiService->getRiskMetrics($portfolio_name);

    // Should return fallback risk metrics
    $this->assertEquals('fallback_data', $result['backend_status']);
    $this->assertArrayHasKey('current_drawdown', $result);
    $this->assertArrayHasKey('risk_score', $result);
    $this->assertArrayHasKey('portfolio_volatility', $result);
    $this->assertEquals('moderate', $result['risk_profile']);
  }

  /**
   * Tests Python script creation methods.
   *
   * @covers ::createPortfolioStatusScript
   * @covers ::createRiskMetricsScript
   */
  public function testPythonScriptCreation(): void {
    $portfolio_name = 'TestPortfolio';

    // Use reflection to test private methods
    $reflection = new \ReflectionClass($this->portfolioApiService);
    
    $statusMethod = $reflection->getMethod('createPortfolioStatusScript');
    $statusMethod->setAccessible(TRUE);
    $statusScript = $statusMethod->invoke($this->portfolioApiService, $portfolio_name);

    $riskMethod = $reflection->getMethod('createRiskMetricsScript');
    $riskMethod->setAccessible(TRUE);
    $riskScript = $riskMethod->invoke($this->portfolioApiService, $portfolio_name);

    // Verify scripts contain expected content
    $this->assertStringContainsString('#!/usr/bin/env python3', $statusScript);
    $this->assertStringContainsString($portfolio_name, $statusScript);
    $this->assertStringContainsString('PortfolioConfigManager', $statusScript);

    $this->assertStringContainsString('#!/usr/bin/env python3', $riskScript);
    $this->assertStringContainsString($portfolio_name, $riskScript);
    $this->assertStringContainsString('risk_parameters.json', $riskScript);
  }

  /**
   * Tests fallback data structure and validation.
   *
   * @covers ::getFallbackPortfolioData
   * @covers ::getFallbackConfigData
   * @covers ::getFallbackRiskMetrics
   */
  public function testFallbackDataStructures(): void {
    $portfolio_name = 'TestPortfolio';

    // Test fallback portfolio data
    $reflection = new \ReflectionClass($this->portfolioApiService);
    $method = $reflection->getMethod('getFallbackPortfolioData');
    $method->setAccessible(TRUE);
    $portfolioData = $method->invoke($this->portfolioApiService, $portfolio_name);

    $this->assertArrayHasKey('portfolio_name', $portfolioData);
    $this->assertArrayHasKey('validation_passed', $portfolioData);
    $this->assertTrue($portfolioData['validation_passed']);
    $this->assertEquals('fallback_data', $portfolioData['backend_status']);

    // Test fallback config data
    $configMethod = $reflection->getMethod('getFallbackConfigData');
    $configMethod->setAccessible(TRUE);
    $configData = $configMethod->invoke($this->portfolioApiService, $portfolio_name);

    $this->assertArrayHasKey('assets', $configData);
    $this->assertArrayHasKey('ETH', $configData['assets']);
    $this->assertArrayHasKey('BTC', $configData['assets']);
    $this->assertEquals(100, $configData['total_allocation']);

    // Test fallback risk metrics
    $riskMethod = $reflection->getMethod('getFallbackRiskMetrics');
    $riskMethod->setAccessible(TRUE);
    $riskData = $riskMethod->invoke($this->portfolioApiService);

    $this->assertArrayHasKey('current_drawdown', $riskData);
    $this->assertArrayHasKey('risk_score', $riskData);
    $this->assertTrue($riskData['current_drawdown'] <= 0);
    $this->assertTrue($riskData['risk_score'] >= 0 && $riskData['risk_score'] <= 1);
  }

  /**
   * Recursively remove a directory and its contents.
   */
  private function recursiveRemoveDirectory(string $dir): void {
    if (is_dir($dir)) {
      $objects = scandir($dir);
      foreach ($objects as $object) {
        if ($object != "." && $object != "..") {
          if (is_dir($dir . "/" . $object) && !is_link($dir . "/" . $object)) {
            $this->recursiveRemoveDirectory($dir . "/" . $object);
          } else {
            unlink($dir . "/" . $object);
          }
        }
      }
      rmdir($dir);
    }
  }

}