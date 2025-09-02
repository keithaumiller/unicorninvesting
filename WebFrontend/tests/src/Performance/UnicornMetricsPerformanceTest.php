<?php

namespace Drupal\Tests\unicornmetrics\Performance;

use Drupal\Tests\BrowserTestBase;
use Drupal\Core\Url;

/**
 * Performance tests for UnicornMetrics module.
 *
 * Tests page load times, memory usage, database queries,
 * and overall system performance under load.
 *
 * @group unicornmetrics
 * @group performance
 */
class UnicornMetricsPerformanceTest extends BrowserTestBase {

  /**
   * {@inheritdoc}
   */
  protected $defaultTheme = 'stark';

  /**
   * Modules to enable.
   *
   * @var array
   */
  protected static $modules = ['unicornmetrics', 'system', 'user', 'dblog'];

  /**
   * Performance benchmarks in seconds.
   *
   * @var array
   */
  protected $performanceBenchmarks = [
    'dashboard_load_time' => 2.0,
    'portfolio_load_time' => 3.0,
    'holdings_load_time' => 4.0,
    'performance_load_time' => 2.5,
    'algorithms_load_time' => 2.0,
  ];

  /**
   * Memory usage benchmarks in MB.
   *
   * @var array
   */
  protected $memoryBenchmarks = [
    'peak_memory_usage' => 256, // 256 MB
    'memory_increase_per_request' => 10, // 10 MB max increase
  ];

  /**
   * Database query benchmarks.
   *
   * @var array
   */
  protected $queryBenchmarks = [
    'max_queries_per_page' => 50,
    'slow_query_threshold' => 0.1, // 100ms
  ];

  /**
   * User for testing.
   *
   * @var \Drupal\user\UserInterface
   */
  protected $testUser;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    $this->testUser = $this->drupalCreateUser([
      'access unicorn metrics',
      'administer unicorn metrics',
    ]);
  }

  /**
   * Tests dashboard page load performance.
   */
  public function testDashboardLoadPerformance() {
    $this->drupalLogin($this->testUser);

    $memory_start = memory_get_usage(true);
    $start_time = microtime(true);

    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(200);

    $load_time = microtime(true) - $start_time;
    $memory_used = (memory_get_usage(true) - $memory_start) / 1024 / 1024; // Convert to MB

    // Performance assertions
    $this->assertLessThan($this->performanceBenchmarks['dashboard_load_time'], $load_time,
      "Dashboard should load within {$this->performanceBenchmarks['dashboard_load_time']} seconds");

    $this->assertLessThan($this->memoryBenchmarks['memory_increase_per_request'], $memory_used,
      "Dashboard should use less than {$this->memoryBenchmarks['memory_increase_per_request']} MB per request");

    // Log performance metrics
    $this->addToAssertionCount(1);
    $this->getLogger()->info('Dashboard load time: ' . round($load_time, 3) . 's, Memory: ' . round($memory_used, 2) . ' MB');
  }

  /**
   * Tests portfolio overview page performance.
   */
  public function testPortfolioOverviewPerformance() {
    $this->drupalLogin($this->testUser);

    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics/lean/portfolio');
    $this->assertSession()->statusCodeEquals(200);
    $load_time = microtime(true) - $start_time;

    $this->assertLessThan($this->performanceBenchmarks['portfolio_load_time'], $load_time,
      "Portfolio overview should load within {$this->performanceBenchmarks['portfolio_load_time']} seconds");
  }

  /**
   * Tests holdings page performance with large datasets.
   */
  public function testHoldingsPagePerformance() {
    $this->drupalLogin($this->testUser);

    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics/lean/holdings');
    $this->assertSession()->statusCodeEquals(200);
    $load_time = microtime(true) - $start_time;

    $this->assertLessThan($this->performanceBenchmarks['holdings_load_time'], $load_time,
      "Holdings page should load within {$this->performanceBenchmarks['holdings_load_time']} seconds");

    // Verify table rendering performance
    $this->assertSession()->elementExists('css', 'table.holdings-table');
    $table_rows = $this->getSession()->getPage()->findAll('css', 'table.holdings-table tbody tr');
    
    // Should handle reasonable number of holdings efficiently
    $this->assertGreaterThan(0, count($table_rows), 'Holdings table should contain data');
  }

  /**
   * Tests performance metrics page rendering performance.
   */
  public function testPerformanceMetricsPagePerformance() {
    $this->drupalLogin($this->testUser);

    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics/lean/performance');
    $this->assertSession()->statusCodeEquals(200);
    $load_time = microtime(true) - $start_time;

    $this->assertLessThan($this->performanceBenchmarks['performance_load_time'], $load_time,
      "Performance metrics page should load within {$this->performanceBenchmarks['performance_load_time']} seconds");
  }

  /**
   * Tests algorithms page performance.
   */
  public function testAlgorithmsPagePerformance() {
    $this->drupalLogin($this->testUser);

    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics/lean/algorithms');
    $this->assertSession()->statusCodeEquals(200);
    $load_time = microtime(true) - $start_time;

    $this->assertLessThan($this->performanceBenchmarks['algorithms_load_time'], $load_time,
      "Algorithms page should load within {$this->performanceBenchmarks['algorithms_load_time']} seconds");
  }

  /**
   * Tests concurrent user performance.
   */
  public function testConcurrentUserPerformance() {
    $users = [];
    $load_times = [];

    // Create multiple users
    for ($i = 0; $i < 5; $i++) {
      $users[] = $this->drupalCreateUser(['access unicorn metrics']);
    }

    // Simulate concurrent access
    foreach ($users as $user) {
      $this->drupalLogin($user);
      
      $start_time = microtime(true);
      $this->drupalGet('/admin/metrics');
      $this->assertSession()->statusCodeEquals(200);
      $load_times[] = microtime(true) - $start_time;
    }

    // Average load time should be reasonable
    $average_load_time = array_sum($load_times) / count($load_times);
    $this->assertLessThan($this->performanceBenchmarks['dashboard_load_time'] * 1.5, $average_load_time,
      'Average load time with concurrent users should be reasonable');
  }

  /**
   * Tests memory usage patterns over multiple requests.
   */
  public function testMemoryUsagePatterns() {
    $this->drupalLogin($this->testUser);

    $initial_memory = memory_get_usage(true);
    $peak_memory = $initial_memory;
    
    $pages = [
      '/admin/metrics',
      '/admin/metrics/lean/portfolio',
      '/admin/metrics/lean/holdings',
      '/admin/metrics/lean/performance',
      '/admin/metrics/lean/algorithms',
    ];

    foreach ($pages as $page) {
      $this->drupalGet($page);
      $this->assertSession()->statusCodeEquals(200);
      
      $current_memory = memory_get_usage(true);
      $peak_memory = max($peak_memory, $current_memory);
    }

    $total_memory_mb = ($peak_memory - $initial_memory) / 1024 / 1024;
    $this->assertLessThan($this->memoryBenchmarks['peak_memory_usage'], $total_memory_mb,
      "Peak memory usage should be less than {$this->memoryBenchmarks['peak_memory_usage']} MB");
  }

  /**
   * Tests CSS and JavaScript asset loading performance.
   */
  public function testAssetLoadingPerformance() {
    $this->drupalLogin($this->testUser);
    $this->drupalGet('/admin/metrics');

    // Verify critical CSS is inline (should be fast)
    $page_content = $this->getSession()->getPage()->getContent();
    $this->assertStringContainsString('<style>', $page_content, 'Critical CSS should be inline for performance');

    // Check for excessive inline styles (should be reasonable)
    $style_count = substr_count($page_content, '<style>');
    $this->assertLessThan(10, $style_count, 'Should not have excessive inline style blocks');
  }

  /**
   * Tests caching effectiveness.
   */
  public function testCachingEffectiveness() {
    $this->drupalLogin($this->testUser);

    // First request (cache miss)
    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics');
    $first_load_time = microtime(true) - $start_time;
    $this->assertSession()->statusCodeEquals(200);

    // Second request (should benefit from caching)
    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics');
    $second_load_time = microtime(true) - $start_time;
    $this->assertSession()->statusCodeEquals(200);

    // Third request (should be consistently fast)
    $start_time = microtime(true);
    $this->drupalGet('/admin/metrics');
    $third_load_time = microtime(true) - $start_time;
    $this->assertSession()->statusCodeEquals(200);

    // Subsequent requests should not be significantly slower
    $this->assertLessThan($first_load_time * 2, $second_load_time, 'Cached requests should not be significantly slower');
    $this->assertLessThan($first_load_time * 2, $third_load_time, 'Consistently cached requests should remain fast');
  }

  /**
   * Tests database query performance.
   */
  public function testDatabaseQueryPerformance() {
    // Enable query logging
    \Drupal::database()->enableLog('performance_test');
    
    $this->drupalLogin($this->testUser);
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(200);

    // Analyze query log
    $queries = \Drupal::database()->getLog('performance_test');
    
    $this->assertLessThan($this->queryBenchmarks['max_queries_per_page'], count($queries),
      "Page should execute fewer than {$this->queryBenchmarks['max_queries_per_page']} database queries");

    // Check for slow queries
    $slow_queries = 0;
    foreach ($queries as $query) {
      if ($query['time'] > $this->queryBenchmarks['slow_query_threshold']) {
        $slow_queries++;
      }
    }

    $this->assertEquals(0, $slow_queries, 'No database queries should exceed the slow query threshold');
  }

  /**
   * Tests page size and transfer performance.
   */
  public function testPageSizePerformance() {
    $this->drupalLogin($this->testUser);
    $this->drupalGet('/admin/metrics');

    $page_content = $this->getSession()->getPage()->getContent();
    $page_size_kb = strlen($page_content) / 1024;

    // Page size should be reasonable (under 500KB for good performance)
    $this->assertLessThan(500, $page_size_kb, 'Page size should be under 500KB for good performance');

    // Verify essential content is present despite size optimization
    $this->assertStringContainsString('Unicorn Portfolio Management System', $page_content);
    $this->assertStringContainsString('dashboard-header', $page_content);
  }

  /**
   * Tests responsive performance across different viewport sizes.
   */
  public function testResponsivePerformance() {
    $this->drupalLogin($this->testUser);

    // Test different viewport sizes
    $viewports = [
      ['width' => 1920, 'height' => 1080], // Desktop
      ['width' => 1024, 'height' => 768],  // Tablet
      ['width' => 375, 'height' => 667],   // Mobile
    ];

    foreach ($viewports as $viewport) {
      $this->getSession()->resizeWindow($viewport['width'], $viewport['height']);
      
      $start_time = microtime(true);
      $this->drupalGet('/admin/metrics');
      $load_time = microtime(true) - $start_time;
      
      $this->assertSession()->statusCodeEquals(200);
      $this->assertLessThan($this->performanceBenchmarks['dashboard_load_time'] * 1.2, $load_time,
        "Page should load efficiently at {$viewport['width']}x{$viewport['height']} viewport");
    }
  }

  /**
   * Gets a logger instance for performance metrics.
   */
  protected function getLogger() {
    return \Drupal::logger('unicornmetrics_performance');
  }

  /**
   * Tests performance under stress conditions.
   */
  public function testStressTestPerformance() {
    $this->drupalLogin($this->testUser);

    $total_requests = 20;
    $load_times = [];
    $memory_usage = [];

    // Perform multiple rapid requests
    for ($i = 0; $i < $total_requests; $i++) {
      $memory_before = memory_get_usage(true);
      $start_time = microtime(true);
      
      $this->drupalGet('/admin/metrics');
      $this->assertSession()->statusCodeEquals(200);
      
      $load_times[] = microtime(true) - $start_time;
      $memory_usage[] = (memory_get_usage(true) - $memory_before) / 1024 / 1024;
    }

    // Calculate performance statistics
    $avg_load_time = array_sum($load_times) / count($load_times);
    $max_load_time = max($load_times);
    $avg_memory = array_sum($memory_usage) / count($memory_usage);

    // Performance should remain stable under stress
    $this->assertLessThan($this->performanceBenchmarks['dashboard_load_time'] * 2, $avg_load_time,
      'Average load time should remain reasonable under stress');
    
    $this->assertLessThan($this->performanceBenchmarks['dashboard_load_time'] * 3, $max_load_time,
      'Maximum load time should not exceed 3x the benchmark under stress');

    $this->assertLessThan($this->memoryBenchmarks['memory_increase_per_request'] * 2, $avg_memory,
      'Average memory usage should remain stable under stress');
  }

}