<?php

namespace Drupal\Tests\unicornmetrics\Unit\Controller;

use Drupal\unicornmetrics\Controller\DashboardController;
use Drupal\Tests\UnitTestCase;
use Drupal\Core\Render\Markup;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\ParameterBag;

/**
 * Unit tests for DashboardController.
 *
 * Tests individual controller methods to ensure proper data handling,
 * portfolio management, and response generation.
 *
 * @group unicornmetrics
 * @group unit
 *
 * @coversDefaultClass \Drupal\unicornmetrics\Controller\DashboardController
 */
class DashboardControllerTest extends UnitTestCase {

  /**
   * The controller under test.
   *
   * @var \Drupal\unicornmetrics\Controller\DashboardController
   */
  protected $controller;

  /**
   * Mock request service.
   *
   * @var \Symfony\Component\HttpFoundation\Request|\PHPUnit\Framework\MockObject\MockObject
   */
  protected $request;

  /**
   * Mock extension list service.
   *
   * @var \Drupal\Core\Extension\ModuleExtensionList|\PHPUnit\Framework\MockObject\MockObject
   */
  protected $extensionList;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    // Mock the request service
    $this->request = $this->createMock(Request::class);
    $query = $this->createMock(ParameterBag::class);
    $this->request->query = $query;

    // Mock the extension list service
    $this->extensionList = $this->createMock('\Drupal\Core\Extension\ModuleExtensionList');

    // Create controller instance
    $this->controller = new DashboardController();

    // Mock container services
    $container = $this->createMock('\Drupal\Core\DependencyInjection\ContainerInterface');
    $container->expects($this->any())
      ->method('get')
      ->willReturnMap([
        ['request_stack', $this->createMockRequestStack()],
        ['extension.list.module', $this->extensionList],
      ]);

    \Drupal::setContainer($container);
  }

  /**
   * Creates a mock request stack.
   */
  protected function createMockRequestStack() {
    $requestStack = $this->createMock('\Symfony\Component\HttpFoundation\RequestStack');
    $requestStack->expects($this->any())
      ->method('getCurrentRequest')
      ->willReturn($this->request);
    return $requestStack;
  }

  /**
   * Tests the main dashboard method.
   *
   * @covers ::dashboard
   */
  public function testDashboard() {
    // Mock extension info
    $this->extensionList->expects($this->once())
      ->method('getExtensionInfo')
      ->with('unicornmetrics')
      ->willReturn(['version' => '4.1.0']);

    // Mock query parameter for portfolio selection
    $this->request->query->expects($this->once())
      ->method('get')
      ->with('portfolio')
      ->willReturn('forex');

    // Execute dashboard method
    $result = $this->controller->dashboard();

    // Verify response structure
    $this->assertIsArray($result);
    $this->assertArrayHasKey('#markup', $result);
    $this->assertArrayHasKey('#attached', $result);

    // Verify markup content contains expected elements
    $markup = (string) $result['#markup'];
    $this->assertStringContainsString('Unicorn Portfolio Management System', $markup);
    $this->assertStringContainsString('Primary Forex Portfolio', $markup);
    $this->assertStringContainsString('Version 4.1.0', $markup);
    $this->assertStringContainsString('UnicornForexEnsemble', $markup);

    // Verify attached styles are included
    $this->assertArrayHasKey('html_head', $result['#attached']);
  }

  /**
   * Tests portfolio selection with default value.
   *
   * @covers ::dashboard
   */
  public function testDashboardWithDefaultPortfolio() {
    // Mock extension info
    $this->extensionList->expects($this->once())
      ->method('getExtensionInfo')
      ->with('unicornmetrics')
      ->willReturn(['version' => '4.1.0']);

    // Mock query parameter returning null (should default to 'forex')
    $this->request->query->expects($this->once())
      ->method('get')
      ->with('portfolio')
      ->willReturn(null);

    $result = $this->controller->dashboard();
    $markup = (string) $result['#markup'];

    // Should default to forex portfolio
    $this->assertStringContainsString('Primary Forex Portfolio', $markup);
  }

  /**
   * Tests LEAN portfolio method.
   *
   * @covers ::leanPortfolio
   */
  public function testLeanPortfolio() {
    // Mock query parameter for portfolio selection
    $this->request->query->expects($this->once())
      ->method('get')
      ->with('portfolio')
      ->willReturn('equity');

    $result = $this->controller->leanPortfolio();

    // Verify response structure
    $this->assertIsArray($result);
    $this->assertArrayHasKey('#markup', $result);
    $this->assertArrayHasKey('#attached', $result);

    // Verify content
    $markup = (string) $result['#markup'];
    $this->assertStringContainsString('Growth Equity Portfolio', $markup);
    $this->assertStringContainsString('Portfolio Value', $markup);
    $this->assertStringContainsString('Cash Position', $markup);
    $this->assertStringContainsString('Unrealized P&L', $markup);
  }

  /**
   * Tests LEAN holdings method.
   *
   * @covers ::leanHoldings
   */
  public function testLeanHoldings() {
    $this->request->query->expects($this->once())
      ->method('get')
      ->with('portfolio')
      ->willReturn('forex');

    $result = $this->controller->leanHoldings();

    $this->assertIsArray($result);
    $this->assertArrayHasKey('#markup', $result);

    $markup = (string) $result['#markup'];
    $this->assertStringContainsString('Holdings', $markup);
    $this->assertStringContainsString('Symbol', $markup);
    $this->assertStringContainsString('Current Price', $markup);
    $this->assertStringContainsString('Market Value', $markup);
  }

  /**
   * Tests LEAN performance method.
   *
   * @covers ::leanPerformance
   */
  public function testLeanPerformance() {
    $this->request->query->expects($this->once())
      ->method('get')
      ->with('portfolio')
      ->willReturn('forex');

    $result = $this->controller->leanPerformance();

    $this->assertIsArray($result);
    $markup = (string) $result['#markup'];
    
    $this->assertStringContainsString('Performance', $markup);
    $this->assertStringContainsString('Return Metrics', $markup);
    $this->assertStringContainsString('Risk Metrics', $markup);
    $this->assertStringContainsString('Sharpe Ratio', $markup);
    $this->assertStringContainsString('Max Drawdown', $markup);
  }

  /**
   * Tests LEAN algorithms method.
   *
   * @covers ::leanAlgorithms
   */
  public function testLeanAlgorithms() {
    $this->request->query->expects($this->once())
      ->method('get')
      ->with('portfolio')
      ->willReturn('forex');

    $result = $this->controller->leanAlgorithms();

    $this->assertIsArray($result);
    $markup = (string) $result['#markup'];
    
    $this->assertStringContainsString('Algorithm', $markup);
    $this->assertStringContainsString('Current Algorithm', $markup);
    $this->assertStringContainsString('UnicornForexEnsemble', $markup);
    $this->assertStringContainsString('Performance', $markup);
  }

  /**
   * Tests portfolio data retrieval with valid portfolio ID.
   */
  public function testGetPortfolioByIdValid() {
    $reflection = new \ReflectionClass($this->controller);
    $method = $reflection->getMethod('getPortfolioById');
    $method->setAccessible(TRUE);

    $portfolio = $method->invoke($this->controller, 'forex');

    $this->assertIsArray($portfolio);
    $this->assertEquals('Primary Forex Portfolio', $portfolio['name']);
    $this->assertEquals('UnicornForexEnsemble', $portfolio['algorithm']);
    $this->assertEquals('live', $portfolio['environment']);
    $this->assertIsArray($portfolio['symbols']);
    $this->assertContains('EURUSD', $portfolio['symbols']);
  }

  /**
   * Tests portfolio data retrieval with invalid portfolio ID (should default).
   */
  public function testGetPortfolioByIdInvalid() {
    $reflection = new \ReflectionClass($this->controller);
    $method = $reflection->getMethod('getPortfolioById');
    $method->setAccessible(TRUE);

    $portfolio = $method->invoke($this->controller, 'invalid_portfolio');

    // Should default to forex portfolio
    $this->assertIsArray($portfolio);
    $this->assertEquals('Primary Forex Portfolio', $portfolio['name']);
  }

  /**
   * Tests security name retrieval.
   */
  public function testGetSecurityName() {
    $reflection = new \ReflectionClass($this->controller);
    $method = $reflection->getMethod('getSecurityName');
    $method->setAccessible(TRUE);

    // Test known symbols
    $this->assertEquals('Apple Inc.', $method->invoke($this->controller, 'AAPL'));
    $this->assertEquals('Euro / US Dollar', $method->invoke($this->controller, 'EURUSD'));
    $this->assertEquals('Bitcoin', $method->invoke($this->controller, 'BTC'));

    // Test unknown symbol
    $this->assertEquals('UNKNOWN Security', $method->invoke($this->controller, 'UNKNOWN'));
  }

  /**
   * Tests LEAN portfolio data generation.
   */
  public function testGetLeanPortfolioData() {
    $reflection = new \ReflectionClass($this->controller);
    $method = $reflection->getMethod('getLeanPortfolioData');
    $method->setAccessible(TRUE);

    $data = $method->invoke($this->controller, 'forex');

    $this->assertIsArray($data);
    $this->assertArrayHasKey('total_value', $data);
    $this->assertArrayHasKey('cash', $data);
    $this->assertArrayHasKey('positions_value', $data);
    $this->assertArrayHasKey('unrealized_pnl', $data);
    $this->assertArrayHasKey('holdings_count', $data);
    $this->assertArrayHasKey('last_updated', $data);

    // Verify data relationships
    $this->assertGreaterThan(0, $data['total_value']);
    $this->assertEquals($data['cash'] + $data['positions_value'], $data['total_value']);
  }

}