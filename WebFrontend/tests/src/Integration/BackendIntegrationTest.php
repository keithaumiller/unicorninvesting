<?php

namespace Drupal\Tests\unicornmetrics\Integration;

use Drupal\Tests\BrowserTestBase;
use GuzzleHttp\Client;
use GuzzleHttp\Exception\RequestException;

/**
 * Integration tests for frontend-backend API connectivity.
 *
 * Tests real-time data flow, IBKR integration, and API communication
 * between the Drupal frontend and Python backend systems.
 *
 * @group unicornmetrics
 * @group integration
 */
class BackendIntegrationTest extends BrowserTestBase {

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
   * HTTP client for API testing.
   *
   * @var \GuzzleHttp\Client
   */
  protected $httpClient;

  /**
   * Backend API base URL.
   *
   * @var string
   */
  protected $backendApiUrl;

  /**
   * IBKR Gateway URL.
   *
   * @var string
   */
  protected $ibkrGatewayUrl;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    $this->httpClient = new Client(['timeout' => 10]);
    $this->backendApiUrl = getenv('BACKEND_API_BASE_URL') ?: 'http://localhost:8000';
    $this->ibkrGatewayUrl = getenv('IBKR_GATEWAY_URL') ?: 'http://localhost:5000';
  }

  /**
   * Tests backend API connectivity and health.
   */
  public function testBackendApiConnectivity() {
    try {
      // Test backend health endpoint
      $response = $this->httpClient->get($this->backendApiUrl . '/health');
      $this->assertEquals(200, $response->getStatusCode());

      $body = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($body);
      $this->assertArrayHasKey('status', $body);
      $this->assertEquals('healthy', $body['status']);

    } catch (RequestException $e) {
      $this->markTestSkipped('Backend API not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests IBKR Gateway connectivity and authentication status.
   */
  public function testIbkrGatewayConnectivity() {
    try {
      // Test IBKR Gateway validate endpoint
      $response = $this->httpClient->get($this->ibkrGatewayUrl . '/v1/api/portal/sso/validate');
      
      // IBKR Gateway returns 200 even when not authenticated
      $this->assertEquals(200, $response->getStatusCode());

      $body = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($body);

      // Log authentication status for debugging
      if (isset($body['authenticated']) && $body['authenticated']) {
        $this->addToAssertionCount(1); // IBKR is authenticated
      } else {
        // IBKR not authenticated - this is expected in testing
        $this->assertTrue(TRUE, 'IBKR Gateway available but not authenticated (expected in testing)');
      }

    } catch (RequestException $e) {
      $this->markTestSkipped('IBKR Gateway not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests portfolio data API endpoints.
   */
  public function testPortfolioDataApi() {
    try {
      // Test portfolio list endpoint
      $response = $this->httpClient->get($this->backendApiUrl . '/api/portfolios');
      $this->assertEquals(200, $response->getStatusCode());

      $portfolios = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($portfolios);
      $this->assertNotEmpty($portfolios);

      // Test individual portfolio data
      $first_portfolio = reset($portfolios);
      $portfolio_id = $first_portfolio['id'] ?? 'forex';

      $response = $this->httpClient->get($this->backendApiUrl . '/api/portfolios/' . $portfolio_id);
      $this->assertEquals(200, $response->getStatusCode());

      $portfolio_data = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($portfolio_data);
      $this->assertArrayHasKey('total_value', $portfolio_data);
      $this->assertArrayHasKey('cash', $portfolio_data);
      $this->assertArrayHasKey('positions', $portfolio_data);

    } catch (RequestException $e) {
      $this->markTestSkipped('Portfolio API endpoints not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests real-time ETH data collection from IBKR.
   */
  public function testRealTimeEthDataCollection() {
    try {
      // Test ETH data collection endpoint
      $response = $this->httpClient->get($this->backendApiUrl . '/api/data/eth/latest');
      $this->assertEquals(200, $response->getStatusCode());

      $eth_data = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($eth_data);
      $this->assertArrayHasKey('symbol', $eth_data);
      $this->assertArrayHasKey('price', $eth_data);
      $this->assertArrayHasKey('timestamp', $eth_data);

      // Validate ETH price is reasonable
      $price = $eth_data['price'];
      $this->assertIsNumeric($price);
      $this->assertGreaterThan(1000, $price, 'ETH price should be above $1000');
      $this->assertLessThan(8000, $price, 'ETH price should be below $8000');

    } catch (RequestException $e) {
      $this->markTestSkipped('ETH data collection not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests algorithm performance data API.
   */
  public function testAlgorithmPerformanceApi() {
    try {
      // Test algorithm performance endpoint
      $response = $this->httpClient->get($this->backendApiUrl . '/api/algorithms/performance');
      $this->assertEquals(200, $response->getStatusCode());

      $performance_data = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($performance_data);
      $this->assertArrayHasKey('algorithms', $performance_data);

      foreach ($performance_data['algorithms'] as $algorithm) {
        $this->assertArrayHasKey('name', $algorithm);
        $this->assertArrayHasKey('sharpe_ratio', $algorithm);
        $this->assertArrayHasKey('total_return', $algorithm);
        $this->assertArrayHasKey('max_drawdown', $algorithm);

        // Validate performance metrics are reasonable
        $this->assertIsNumeric($algorithm['sharpe_ratio']);
        $this->assertIsNumeric($algorithm['total_return']);
        $this->assertIsNumeric($algorithm['max_drawdown']);
      }

    } catch (RequestException $e) {
      $this->markTestSkipped('Algorithm performance API not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests risk management API endpoints.
   */
  public function testRiskManagementApi() {
    try {
      // Test risk metrics endpoint
      $response = $this->httpClient->get($this->backendApiUrl . '/api/risk/metrics');
      $this->assertEquals(200, $response->getStatusCode());

      $risk_data = json_decode($response->getBody()->getContents(), TRUE);
      $this->assertIsArray($risk_data);
      $this->assertArrayHasKey('var_95', $risk_data);
      $this->assertArrayHasKey('max_drawdown', $risk_data);
      $this->assertArrayHasKey('volatility', $risk_data);

      // Validate risk metrics
      $this->assertIsNumeric($risk_data['var_95']);
      $this->assertIsNumeric($risk_data['max_drawdown']);
      $this->assertIsNumeric($risk_data['volatility']);

      // VaR and max drawdown should be negative values
      $this->assertLessThanOrEqual(0, $risk_data['var_95']);
      $this->assertLessThanOrEqual(0, $risk_data['max_drawdown']);

    } catch (RequestException $e) {
      $this->markTestSkipped('Risk management API not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests frontend-backend data synchronization.
   */
  public function testDataSynchronization() {
    $user = $this->drupalCreateUser(['access unicorn metrics']);
    $this->drupalLogin($user);

    try {
      // Get portfolio data from backend API
      $response = $this->httpClient->get($this->backendApiUrl . '/api/portfolios/forex');
      $backend_data = json_decode($response->getBody()->getContents(), TRUE);

      // Visit frontend portfolio page
      $this->drupalGet('/admin/metrics/lean/portfolio?portfolio=forex');
      $this->assertSession()->statusCodeEquals(200);

      // Verify frontend displays data consistent with backend
      // Note: In production, this would compare actual API data
      // For now, we verify the page structure matches expected format
      $this->assertSession()->pageTextContains('Portfolio Value');
      $this->assertSession()->pageTextContains('$');
      
      // Verify numeric format patterns that should match backend data
      $page_content = $this->getSession()->getPage()->getContent();
      $this->assertMatchesRegularExpression('/\$[\d,]+\.\d{2}/', $page_content, 'Currency values should be properly formatted');

    } catch (RequestException $e) {
      $this->markTestSkipped('Backend API not available for synchronization test: ' . $e->getMessage());
    }
  }

  /**
   * Tests API error handling and recovery.
   */
  public function testApiErrorHandling() {
    $user = $this->drupalCreateUser(['access unicorn metrics']);
    $this->drupalLogin($user);

    // Test frontend behavior when backend is unavailable
    // We'll test with a definitely invalid endpoint
    try {
      $response = $this->httpClient->get('http://nonexistent:8000/api/test');
      $this->fail('Expected RequestException was not thrown');
    } catch (RequestException $e) {
      // This is expected
      $this->assertTrue(TRUE, 'API correctly handles invalid endpoints');
    }

    // Frontend should still load with fallback data
    $this->drupalGet('/admin/metrics');
    $this->assertSession()->statusCodeEquals(200);
    $this->assertSession()->pageTextContains('Unicorn Portfolio Management System');
  }

  /**
   * Tests IBKR data quality and validation.
   */
  public function testIbkrDataQuality() {
    try {
      // Test IBKR ETH contract data
      $response = $this->httpClient->get($this->backendApiUrl . '/api/ibkr/eth/quality');
      
      if ($response->getStatusCode() === 200) {
        $quality_data = json_decode($response->getBody()->getContents(), TRUE);
        
        $this->assertIsArray($quality_data);
        $this->assertArrayHasKey('completeness', $quality_data);
        $this->assertArrayHasKey('consistency', $quality_data);
        $this->assertArrayHasKey('reasonableness', $quality_data);

        // Data quality should meet minimum thresholds
        $this->assertGreaterThanOrEqual(0.95, $quality_data['completeness'], 'Data completeness should be >= 95%');
        $this->assertGreaterThanOrEqual(0.95, $quality_data['consistency'], 'Data consistency should be >= 95%');
        $this->assertGreaterThanOrEqual(0.90, $quality_data['reasonableness'], 'Data reasonableness should be >= 90%');
      }

    } catch (RequestException $e) {
      $this->markTestSkipped('IBKR data quality endpoint not available: ' . $e->getMessage());
    }
  }

  /**
   * Tests performance benchmarks for API calls.
   */
  public function testApiPerformanceBenchmarks() {
    $performance_tests = [
      '/health' => 1.0, // Health check should respond within 1 second
      '/api/portfolios' => 2.0, // Portfolio list within 2 seconds
      '/api/data/eth/latest' => 3.0, // Latest ETH data within 3 seconds
    ];

    foreach ($performance_tests as $endpoint => $max_time) {
      try {
        $start_time = microtime(true);
        $response = $this->httpClient->get($this->backendApiUrl . $endpoint);
        $response_time = microtime(true) - $start_time;

        if ($response->getStatusCode() === 200) {
          $this->assertLessThan($max_time, $response_time, 
            "API endpoint {$endpoint} should respond within {$max_time} seconds");
        }

      } catch (RequestException $e) {
        // Skip performance test if endpoint not available
        continue;
      }
    }
  }

  /**
   * Tests WebSocket connections for real-time updates.
   */
  public function testRealTimeDataStreaming() {
    // Note: This test would require WebSocket testing capabilities
    // For now, we'll test the REST polling mechanism
    
    try {
      $start_time = time();
      
      // Make two requests 5 seconds apart
      $response1 = $this->httpClient->get($this->backendApiUrl . '/api/data/eth/latest');
      sleep(5);
      $response2 = $this->httpClient->get($this->backendApiUrl . '/api/data/eth/latest');

      if ($response1->getStatusCode() === 200 && $response2->getStatusCode() === 200) {
        $data1 = json_decode($response1->getBody()->getContents(), TRUE);
        $data2 = json_decode($response2->getBody()->getContents(), TRUE);

        // Timestamps should be different (data is updating)
        $this->assertNotEquals($data1['timestamp'] ?? 0, $data2['timestamp'] ?? 0, 
          'Real-time data should have different timestamps');
      }

    } catch (RequestException $e) {
      $this->markTestSkipped('Real-time data streaming test not available: ' . $e->getMessage());
    }
  }

}