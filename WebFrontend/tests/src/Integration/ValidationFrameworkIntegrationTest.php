<?php

namespace Drupal\Tests\unicornmetrics\Integration;

use Drupal\Tests\BrowserTestBase;
use Drupal\unicornmetrics\Plugin\Validation\Constraint\PortfolioDataConstraint;
use Drupal\unicornmetrics\Plugin\Validation\Constraint\ApiResponseConstraint;

/**
 * Integration tests for UnicornMetrics validation framework.
 *
 * Tests integration between validation constraints, forms, and services
 * to ensure complete validation coverage.
 *
 * @group unicornmetrics
 * @group integration
 * @group validation
 */
class ValidationFrameworkIntegrationTest extends BrowserTestBase {

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
   * A user with administrative permissions.
   *
   * @var \Drupal\user\UserInterface
   */
  protected $adminUser;

  /**
   * The portfolio API service.
   *
   * @var \Drupal\unicornmetrics\Service\PortfolioApiService
   */
  protected $portfolioApi;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    $this->adminUser = $this->drupalCreateUser([
      'access unicorn metrics',
      'administer unicorn metrics',
    ]);

    $this->portfolioApi = $this->container->get('unicornmetrics.portfolio_api');
  }

  /**
   * Tests AdminSettingsForm validation integration.
   */
  public function testAdminSettingsFormValidation(): void {
    $this->drupalLogin($this->adminUser);
    $this->drupalGet('/admin/config/unicornmetrics/settings');

    // Test form renders without errors
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('General Settings');
    $this->assertSession()->pageTextContains('API Settings');
    $this->assertSession()->pageTextContains('Validation Settings');
    $this->assertSession()->pageTextContains('Performance Settings');

    // Test validation with invalid values
    $edit = [
      'refresh_interval' => 5, // Below minimum
      'api_timeout' => 500, // Above maximum
      'max_allocation_variance' => 10, // Above maximum
      'page_load_threshold' => 15, // Above maximum
      'memory_threshold' => 2048, // Above maximum
      'backend_path' => '/invalid/path', // Non-existent path
    ];

    $this->submitForm($edit, 'Save configuration');

    // Verify validation errors are shown
    $this->assertSession()->pageTextContains('Refresh interval must be between 10 and 3600 seconds');
    $this->assertSession()->pageTextContains('API timeout must be between 5 and 300 seconds');
    $this->assertSession()->pageTextContains('Maximum allocation variance must be between 0.01% and 5.0%');
    $this->assertSession()->pageTextContains('Page load threshold must be between 0.5 and 10.0 seconds');
    $this->assertSession()->pageTextContains('Memory threshold must be between 64 and 1024 MB');
    $this->assertSession()->pageTextContains('Backend path does not exist or is not accessible');

    // Test with valid values
    $valid_edit = [
      'refresh_interval' => 120,
      'api_timeout' => 45,
      'max_allocation_variance' => 0.5,
      'page_load_threshold' => 2.5,
      'memory_threshold' => 512,
      'enable_fallback' => TRUE,
      'strict_validation' => TRUE,
    ];

    $this->submitForm($valid_edit, 'Save configuration');
    $this->assertSession()->pageTextContains('The configuration options have been saved');
  }

  /**
   * Tests portfolio data validation constraint integration.
   */
  public function testPortfolioDataValidationConstraint(): void {
    $validator = $this->container->get('validator');

    // Test valid portfolio data
    $valid_data = [
      'portfolio_name' => 'TestPortfolio',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.20,
    ];

    $constraint = new PortfolioDataConstraint();
    $violations = $validator->validate($valid_data, $constraint);
    $this->assertCount(0, $violations, 'Valid portfolio data should not have violations');

    // Test invalid portfolio data
    $invalid_data = [
      'portfolio_name' => 'Invalid-Name!',
      'strategy_type' => 'unsupported_type',
      'assets' => [
        'ETH' => ['allocation_percent' => 70.0],
        'BTC' => ['allocation_percent' => 40.0], // Total = 110%
      ],
      'target_volatility' => 0.80, // Above maximum
    ];

    $violations = $validator->validate($invalid_data, $constraint);
    $this->assertGreaterThan(0, $violations->count(), 'Invalid portfolio data should have violations');

    $violation_messages = [];
    foreach ($violations as $violation) {
      $violation_messages[] = $violation->getMessage();
    }

    $this->assertContains('The portfolio name "Invalid-Name!" is invalid', implode(' ', $violation_messages));
  }

  /**
   * Tests API response validation constraint integration.
   */
  public function testApiResponseValidationConstraint(): void {
    $validator = $this->container->get('validator');

    // Test valid API response
    $valid_response = [
      'portfolio_name' => 'TestPortfolio',
      'status' => 'active',
      'target_volatility' => 0.25,
      'last_updated' => '2024-01-01 12:00:00',
    ];

    $constraint = new ApiResponseConstraint();
    $violations = $validator->validate($valid_response, $constraint);
    $this->assertCount(0, $violations, 'Valid API response should not have violations');

    // Test invalid API response with malformed JSON string
    $invalid_json = '{"invalid": json}';
    $violations = $validator->validate($invalid_json, $constraint);
    $this->assertGreaterThan(0, $violations->count(), 'Malformed JSON should have violations');

    // Test API response with values out of range
    $out_of_range_response = [
      'current_drawdown' => -2.0, // Below minimum
      'portfolio_volatility' => 3.0, // Above maximum
      'var_5pct' => 1.5, // Above maximum
      'risk_score' => 2.0, // Above maximum
    ];

    $violations = $validator->validate($out_of_range_response, $constraint);
    $this->assertGreaterThan(0, $violations->count(), 'Out of range values should have violations');
  }

  /**
   * Tests portfolio API service with validation integration.
   */
  public function testPortfolioApiServiceValidation(): void {
    // Test getting portfolio configuration
    $config = $this->portfolioApi->getPortfolioConfig('Myportolio');
    $this->assertIsArray($config);
    $this->assertArrayHasKey('portfolio_name', $config);
    $this->assertArrayHasKey('assets', $config);

    // Validate the returned configuration against the constraint
    $validator = $this->container->get('validator');
    $constraint = new PortfolioDataConstraint();
    $violations = $validator->validate($config, $constraint);

    // Configuration from service should be valid
    $this->assertCount(0, $violations, 'Portfolio configuration from API service should be valid');

    // Test getting risk metrics
    $risk_metrics = $this->portfolioApi->getRiskMetrics('Myportolio');
    $this->assertIsArray($risk_metrics);
    $this->assertArrayHasKey('risk_score', $risk_metrics);
    $this->assertArrayHasKey('current_drawdown', $risk_metrics);

    // Validate risk metrics against API response constraint
    $api_constraint = new ApiResponseConstraint();
    $violations = $validator->validate($risk_metrics, $api_constraint);
    $this->assertCount(0, $violations, 'Risk metrics from API service should be valid');

    // Test ETH algorithm status
    $algorithm_status = $this->portfolioApi->getEthAlgorithmStatus('Myportolio');
    $this->assertIsArray($algorithm_status);
    $this->assertArrayHasKey('integration_status', $algorithm_status);

    // Validate algorithm status
    $violations = $validator->validate($algorithm_status, $api_constraint);
    $this->assertCount(0, $violations, 'Algorithm status from API service should be valid');
  }

  /**
   * Tests validation framework performance.
   */
  public function testValidationFrameworkPerformance(): void {
    $validator = $this->container->get('validator');
    $portfolio_constraint = new PortfolioDataConstraint();
    $api_constraint = new ApiResponseConstraint();

    $valid_portfolio_data = [
      'portfolio_name' => 'PerformanceTest',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.20,
    ];

    $valid_api_response = [
      'portfolio_name' => 'PerformanceTest',
      'status' => 'active',
      'target_volatility' => 0.20,
      'last_updated' => '2024-01-01 12:00:00',
    ];

    // Measure validation performance
    $start_time = microtime(TRUE);
    
    for ($i = 0; $i < 100; $i++) {
      $validator->validate($valid_portfolio_data, $portfolio_constraint);
      $validator->validate($valid_api_response, $api_constraint);
    }
    
    $end_time = microtime(TRUE);
    $total_time = $end_time - $start_time;

    // Validation should complete within reasonable time (< 1 second for 200 validations)
    $this->assertLessThan(1.0, $total_time, 'Validation framework should perform efficiently');

    // Average time per validation should be very fast
    $avg_time = $total_time / 200;
    $this->assertLessThan(0.005, $avg_time, 'Individual validations should be very fast');
  }

  /**
   * Tests error handling in validation framework.
   */
  public function testValidationFrameworkErrorHandling(): void {
    $validator = $this->container->get('validator');

    // Test with null values
    $portfolio_constraint = new PortfolioDataConstraint();
    $violations = $validator->validate(NULL, $portfolio_constraint);
    $this->assertCount(0, $violations, 'Null values should be handled gracefully');

    // Test with empty arrays
    $violations = $validator->validate([], $portfolio_constraint);
    $this->assertCount(0, $violations, 'Empty arrays should be handled gracefully');

    // Test API constraint with invalid data types
    $api_constraint = new ApiResponseConstraint();
    $violations = $validator->validate(123, $api_constraint);
    $this->assertGreaterThan(0, $violations->count(), 'Invalid data types should trigger violations');

    // Test portfolio constraint with invalid data structure
    $violations = $validator->validate('invalid_string', $portfolio_constraint);
    $this->assertCount(0, $violations, 'Invalid data structures should be handled gracefully');
  }

  /**
   * Tests validation configuration integration.
   */
  public function testValidationConfigurationIntegration(): void {
    // Get current configuration
    $config = $this->config('unicornmetrics.settings');
    
    // Test default configuration values
    $this->assertNull($config->get('default_portfolio'));
    $this->assertNull($config->get('strict_validation'));

    // Test setting validation configuration
    $config
      ->set('strict_validation', TRUE)
      ->set('max_allocation_variance', 0.2)
      ->save();

    $updated_config = $this->config('unicornmetrics.settings');
    $this->assertTrue($updated_config->get('strict_validation'));
    $this->assertEquals(0.2, $updated_config->get('max_allocation_variance'));
  }

  /**
   * Tests complete validation workflow.
   */
  public function testCompleteValidationWorkflow(): void {
    $this->drupalLogin($this->adminUser);

    // 1. Configure validation settings
    $this->drupalGet('/admin/config/unicornmetrics/settings');
    $edit = [
      'strict_validation' => TRUE,
      'max_allocation_variance' => 0.1,
    ];
    $this->submitForm($edit, 'Save configuration');
    $this->assertSession()->pageTextContains('The configuration options have been saved');

    // 2. Test dashboard with validation active
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Unicorn Portfolio Management System');

    // 3. Test portfolio navigation with validation
    $this->drupalGet('/admin/metrics?portfolio=Myportolio');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Portfolio Value');

    // 4. Test API service integration with validation
    $portfolio_config = $this->portfolioApi->getPortfolioConfig('Myportolio');
    $this->assertIsArray($portfolio_config);

    // 5. Validate the complete data flow
    $validator = $this->container->get('validator');
    $constraint = new PortfolioDataConstraint();
    $violations = $validator->validate($portfolio_config, $constraint);
    $this->assertCount(0, $violations, 'Complete workflow should produce valid data');
  }

}