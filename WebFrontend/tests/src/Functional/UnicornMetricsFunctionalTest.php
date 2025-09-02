<?php

namespace Drupal\Tests\unicornmetrics\Functional;

use Drupal\Tests\BrowserTestBase;
use Drupal\Core\Url;

/**
 * Functional tests for UnicornMetrics module workflows.
 *
 * Tests complete user journeys and module functionality integration
 * with Drupal core systems.
 *
 * @group unicornmetrics
 * @group functional
 */
class UnicornMetricsFunctionalTest extends BrowserTestBase {

  /**
   * {@inheritdoc}
   */
  protected $defaultTheme = 'stark';

  /**
   * Modules to enable.
   *
   * @var array
   */
  protected static $modules = ['unicornmetrics', 'system', 'user'];

  /**
   * A user with permission to access UnicornMetrics.
   *
   * @var \Drupal\user\UserInterface
   */
  protected $authorizedUser;

  /**
   * A user without permission to access UnicornMetrics.
   *
   * @var \Drupal\user\UserInterface
   */
  protected $unauthorizedUser;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    // Create users for testing permissions
    $this->authorizedUser = $this->drupalCreateUser([
      'access unicorn metrics',
      'administer unicorn metrics',
    ]);

    $this->unauthorizedUser = $this->drupalCreateUser([]);
  }

  /**
   * Tests main dashboard access and functionality.
   */
  public function testMainDashboardAccess() {
    // Test unauthorized access
    $this->drupalLogin($this->unauthorizedUser);
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(403);

    // Test authorized access
    $this->drupalLogin($this->authorizedUser);
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(200);

    // Verify dashboard content
    $this->assertSession()->pageTextContains('Unicorn Portfolio Management System');
    $this->assertSession()->pageTextContains('Primary Forex Portfolio');
    $this->assertSession()->pageTextContains('UnicornForexEnsemble');

    // Verify navigation links are present
    $this->assertSession()->linkExists('📊 Portfolio Overview');
    $this->assertSession()->linkExists('📈 Securities & Holdings');
    $this->assertSession()->linkExists('📊 Performance Metrics');
    $this->assertSession()->linkExists('🤖 Managing Algorithm');
  }

  /**
   * Tests portfolio navigation and selection.
   */
  public function testPortfolioNavigation() {
    $this->drupalLogin($this->authorizedUser);

    // Test default portfolio (forex)
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->pageTextContains('Primary Forex Portfolio');

    // Test portfolio parameter
    $this->drupalGet('/admin/metrics?portfolio=equity');
    $this->assertSession()->pageTextContains('Growth Equity Portfolio');

    // Test invalid portfolio (should default to forex)
    $this->drupalGet('/admin/metrics?portfolio=invalid');
    $this->assertSession()->pageTextContains('Primary Forex Portfolio');
  }

  /**
   * Tests LEAN portfolio overview functionality.
   */
  public function testLeanPortfolioOverview() {
    $this->drupalLogin($this->authorizedUser);

    // Test portfolio overview page
    $this->drupalGet('/admin/metrics/lean/portfolio');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Portfolio Value');
    $this->assertSession()->pageTextContains('Cash Position');
    $this->assertSession()->pageTextContains('Unrealized P&L');

    // Test with portfolio parameter
    $this->drupalGet('/admin/metrics/lean/portfolio?portfolio=equity');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Growth Equity Portfolio');
  }

  /**
   * Tests LEAN holdings page functionality.
   */
  public function testLeanHoldings() {
    $this->drupalLogin($this->authorizedUser);

    $this->drupalGet('/admin/metrics/lean/holdings');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Holdings');
    $this->assertSession()->pageTextContains('Symbol');
    $this->assertSession()->pageTextContains('Current Price');
    $this->assertSession()->pageTextContains('Market Value');

    // Verify holdings table structure
    $this->assertSession()->elementExists('css', 'table.holdings-table');
    $this->assertSession()->elementExists('css', 'table.holdings-table thead');
    $this->assertSession()->elementExists('css', 'table.holdings-table tbody');
  }

  /**
   * Tests LEAN performance metrics page.
   */
  public function testLeanPerformance() {
    $this->drupalLogin($this->authorizedUser);

    $this->drupalGet('/admin/metrics/lean/performance');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Performance');
    $this->assertSession()->pageTextContains('Return Metrics');
    $this->assertSession()->pageTextContains('Risk Metrics');
    $this->assertSession()->pageTextContains('Sharpe Ratio');
    $this->assertSession()->pageTextContains('Max Drawdown');
    $this->assertSession()->pageTextContains('Alpha Generation');
  }

  /**
   * Tests LEAN algorithms page functionality.
   */
  public function testLeanAlgorithms() {
    $this->drupalLogin($this->authorizedUser);

    $this->drupalGet('/admin/metrics/lean/algorithms');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Algorithm');
    $this->assertSession()->pageTextContains('Current Algorithm');
    $this->assertSession()->pageTextContains('UnicornForexEnsemble');
    $this->assertSession()->pageTextContains('Performance');
    $this->assertSession()->pageTextContains('Insights');

    // Test action links
    $this->assertSession()->linkExists('📊 Performance Analysis');
    $this->assertSession()->linkExists('🔬 Backtest Results');
  }

  /**
   * Tests algorithm performance analysis page.
   */
  public function testAlgorithmPerformanceAnalysis() {
    $this->drupalLogin($this->authorizedUser);

    $this->drupalGet('/admin/metrics/lean/algorithms/performance');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Algorithm Performance');
    $this->assertSession()->pageTextContains('Signal Quality');
    $this->assertSession()->pageTextContains('Financial Impact');
    $this->assertSession()->pageTextContains('Direction Accuracy');
    $this->assertSession()->pageTextContains('Total Alpha Generated');
  }

  /**
   * Tests backtest results page.
   */
  public function testBacktestResults() {
    $this->drupalLogin($this->authorizedUser);

    $this->drupalGet('/admin/metrics/lean/backtest');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Backtest Results');
    $this->assertSession()->pageTextContains('Test Period');
    $this->assertSession()->pageTextContains('Overall Performance');
    $this->assertSession()->pageTextContains('Trade Statistics');
    $this->assertSession()->pageTextContains('Total Return');
  }

  /**
   * Tests complete user journey through all pages.
   */
  public function testCompleteUserJourney() {
    $this->drupalLogin($this->authorizedUser);

    // Start at main dashboard
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(200);

    // Navigate to portfolio overview
    $this->clickLink('📊 Portfolio Overview');
    $this->assertSession()->addressEquals('/admin/metrics/lean/portfolio');
    $this->assertSession()->pageTextContains('Portfolio Value');

    // Navigate to holdings
    $this->clickLink('📈 View Securities & Holdings');
    $this->assertSession()->addressMatches('/\/admin\/metrics\/lean\/holdings/');
    $this->assertSession()->pageTextContains('Holdings');

    // Navigate back to main dashboard using action link
    $this->drupalGet('/admin/metrics/lean/algorithms');
    $this->clickLink('🏠 Dashboard Home');
    $this->assertSession()->addressEquals('/admin/metrics');
    $this->assertSession()->pageTextContains('Unicorn Portfolio Management System');
  }

  /**
   * Tests responsive design elements and styling.
   */
  public function testResponsiveDesign() {
    $this->drupalLogin($this->authorizedUser);
    $this->drupalGet('/admin/metrics');

    // Verify key CSS classes for responsive design are present
    $this->assertSession()->elementExists('css', '.dashboard-header');
    $this->assertSession()->elementExists('css', '.dashboard-sections');
    $this->assertSession()->elementExists('css', '.portfolio-stats');
    $this->assertSession()->elementExists('css', '.lean-nav-table');

    // Verify responsive grid structures
    $this->drupalGet('/admin/metrics/lean/portfolio');
    $this->assertSession()->elementExists('css', '.portfolio-overview-grid');
    $this->assertSession()->elementExists('css', '.portfolio-card');

    $this->drupalGet('/admin/metrics/lean/performance');
    $this->assertSession()->elementExists('css', '.performance-grid');
    $this->assertSession()->elementExists('css', '.performance-section');
  }

  /**
   * Tests error handling for malformed requests.
   */
  public function testErrorHandling() {
    $this->drupalLogin($this->authorizedUser);

    // Test invalid portfolio parameter (should gracefully default)
    $this->drupalGet('/admin/metrics?portfolio=nonexistent');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Primary Forex Portfolio');

    // Test empty portfolio parameter
    $this->drupalGet('/admin/metrics?portfolio=');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Primary Forex Portfolio');
  }

  /**
   * Tests module information and versioning.
   */
  public function testModuleInformation() {
    $this->drupalLogin($this->authorizedUser);
    $this->drupalGet('/admin/metrics');

    // Verify version information is displayed
    $this->assertSession()->pageTextContains('Version');
    $this->assertSession()->pageTextContains('Last Updated');

    // Verify module metadata
    $module_info = \Drupal::service('extension.list.module')->getExtensionInfo('unicornmetrics');
    $this->assertNotEmpty($module_info);
    $this->assertEquals('Unicorn Metrics', $module_info['name']);
  }

  /**
   * Tests page caching and performance.
   */
  public function testPageCaching() {
    $this->drupalLogin($this->authorizedUser);

    // First request
    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics');
    $first_load_time = microtime(true) - $start_time;
    $this->assertSession()->statusCodeEquals(200);

    // Second request (should be faster if caching works)
    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics');
    $second_load_time = microtime(true) - $start_time;
    $this->assertSession()->statusCodeEquals(200);

    // Basic performance check (second load should not be significantly slower)
    $this->assertLessThan($first_load_time * 2, $second_load_time, 'Page loading performance is reasonable');
  }

}