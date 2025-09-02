<?php

/**
 * @file
 * Test fixtures for UnicornMetrics module testing.
 * 
 * Provides standardized test data for consistent testing
 * across unit, functional, and integration tests.
 */

namespace Drupal\Tests\unicornmetrics\Fixtures;

/**
 * Portfolio test data fixtures.
 */
class PortfolioFixtures {

  /**
   * Standard portfolio test data.
   *
   * @return array
   *   Array of portfolio test data.
   */
  public static function getPortfolioData() {
    return [
      'forex' => [
        'id' => 'forex',
        'name' => 'Primary Forex Portfolio',
        'description' => 'Main forex trading portfolio with multi-currency pairs',
        'algorithm' => 'UnicornForexEnsemble',
        'environment' => 'live',
        'total_value' => 125847.62,
        'positions' => 8,
        'daily_pnl' => '+$2,347.18',
        'status' => 'active',
        'project_id' => 12345,
        'symbols' => ['EURUSD', 'USDJPY', 'USDCNH', 'ETHUSD'],
        'last_updated' => '2024-01-01T12:00:00Z',
      ],
      'equity' => [
        'id' => 'equity',
        'name' => 'Growth Equity Portfolio',
        'description' => 'Large-cap growth equity portfolio with tech focus',
        'algorithm' => 'UnicornEquityGrowth',
        'environment' => 'live',
        'total_value' => 250000.00,
        'positions' => 12,
        'daily_pnl' => '+$1,250.00',
        'status' => 'active',
        'project_id' => 12346,
        'symbols' => ['SPY', 'AAPL', 'TSLA', 'MSFT', 'GOOGL'],
        'last_updated' => '2024-01-01T12:00:00Z',
      ],
      'test' => [
        'id' => 'test',
        'name' => 'Test Portfolio',
        'description' => 'Portfolio for unit testing',
        'algorithm' => 'TestAlgorithm',
        'environment' => 'test',
        'total_value' => 10000.00,
        'positions' => 2,
        'daily_pnl' => '+$100.00',
        'status' => 'active',
        'project_id' => 99999,
        'symbols' => ['TEST1', 'TEST2'],
        'last_updated' => '2024-01-01T10:00:00Z',
      ],
    ];
  }

  /**
   * Holdings test data.
   *
   * @return array
   *   Array of holdings test data.
   */
  public static function getHoldingsData() {
    return [
      [
        'symbol' => 'EURUSD',
        'name' => 'Euro / US Dollar',
        'quantity' => 100000,
        'average_cost' => 1.0850,
        'current_price' => 1.0900,
        'market_value' => 15734.62,
        'unrealized_pnl' => 500.00,
        'unrealized_pnl_percent' => 0.0325,
        'weight' => 0.125,
      ],
      [
        'symbol' => 'USDJPY',
        'name' => 'US Dollar / Japanese Yen',
        'quantity' => 100000,
        'average_cost' => 145.50,
        'current_price' => 147.20,
        'market_value' => 15734.62,
        'unrealized_pnl' => 1167.12,
        'unrealized_pnl_percent' => 0.0797,
        'weight' => 0.125,
      ],
      [
        'symbol' => 'ETHUSD',
        'name' => 'Ethereum / US Dollar',
        'quantity' => 10,
        'average_cost' => 2200.00,
        'current_price' => 2350.00,
        'market_value' => 23500.00,
        'unrealized_pnl' => 1500.00,
        'unrealized_pnl_percent' => 0.0682,
        'weight' => 0.187,
      ],
    ];
  }

  /**
   * Performance metrics test data.
   *
   * @return array
   *   Array of performance test data.
   */
  public static function getPerformanceData() {
    return [
      'total_return' => 0.0847,
      'annualized_return' => 0.1245,
      'sharpe_ratio' => 1.85,
      'max_drawdown' => -0.0845,
      'volatility' => 0.1567,
      'alpha' => 0.0234,
      'beta' => 0.98,
      'var_95' => -0.0287,
      'information_ratio' => 1.23,
      'win_rate' => 0.67,
      'profit_factor' => 1.45,
      'calmar_ratio' => 1.47,
    ];
  }

  /**
   * Algorithm test data.
   *
   * @return array
   *   Array of algorithm test data.
   */
  public static function getAlgorithmData() {
    return [
      'current' => [
        'name' => 'UnicornForexEnsemble',
        'status' => 'RUNNING',
        'runtime' => '2h 34m',
        'signals_generated' => 1847,
        'signal_accuracy' => 0.721,
        'alpha_score' => 2.34,
        'last_signal' => '2024-01-01T11:58:00Z',
      ],
      'insights' => [
        'total' => 1847,
        'direction_accuracy' => 0.721,
        'magnitude_accuracy' => 0.643,
        'confidence_avg' => 0.78,
      ],
      'performance' => [
        'direction_accuracy' => 0.721,
        'magnitude_accuracy' => 0.643,
        'avg_confidence' => 0.78,
        'total_alpha' => 18247.33,
        'avg_trade_value' => 2543.67,
        'win_rate' => 0.67,
      ],
    ];
  }

  /**
   * Backtest results test data.
   *
   * @return array
   *   Array of backtest test data.
   */
  public static function getBacktestData() {
    return [
      'start_date' => '2023-01-01',
      'end_date' => '2024-12-31',
      'duration_days' => 504,
      'total_return' => 0.1847,
      'sharpe_ratio' => 1.67,
      'total_trades' => 2340,
      'win_rate' => 0.634,
      'max_drawdown' => -0.0923,
      'profit_factor' => 1.56,
      'expectancy' => 45.23,
    ];
  }

  /**
   * IBKR integration test data.
   *
   * @return array
   *   Array of IBKR test data.
   */
  public static function getIbkrTestData() {
    return [
      'gateway_status' => [
        'connected' => true,
        'authenticated' => false,
        'session_id' => 'test_session_123',
        'server_version' => '1.0.0',
      ],
      'eth_data' => [
        'symbol' => 'ETHUSD',
        'contract_id' => 541686654,
        'price' => 2350.45,
        'bid' => 2350.20,
        'ask' => 2350.70,
        'volume' => 156789,
        'timestamp' => '2024-01-01T12:00:00Z',
        'quality_score' => 0.98,
      ],
      'market_data' => [
        [
          'timestamp' => '2024-01-01T12:00:00Z',
          'open' => 2345.00,
          'high' => 2355.00,
          'low' => 2340.00,
          'close' => 2350.45,
          'volume' => 15678,
        ],
        [
          'timestamp' => '2024-01-01T12:01:00Z',
          'open' => 2350.45,
          'high' => 2352.00,
          'low' => 2348.00,
          'close' => 2351.20,
          'volume' => 12456,
        ],
      ],
    ];
  }

  /**
   * Error scenarios test data.
   *
   * @return array
   *   Array of error test scenarios.
   */
  public static function getErrorScenarios() {
    return [
      'api_timeout' => [
        'error_type' => 'timeout',
        'message' => 'Request timeout after 30 seconds',
        'status_code' => 408,
        'retry_after' => 60,
      ],
      'server_error' => [
        'error_type' => 'server_error',
        'message' => 'Internal server error',
        'status_code' => 500,
        'details' => 'Database connection failed',
      ],
      'authentication_failed' => [
        'error_type' => 'auth_error',
        'message' => 'Authentication required',
        'status_code' => 401,
        'login_url' => '/user/login',
      ],
      'invalid_portfolio' => [
        'error_type' => 'validation_error',
        'message' => 'Portfolio not found',
        'status_code' => 404,
        'default_portfolio' => 'forex',
      ],
    ];
  }

  /**
   * Performance benchmark data.
   *
   * @return array
   *   Array of performance benchmarks.
   */
  public static function getPerformanceBenchmarks() {
    return [
      'page_load_times' => [
        'dashboard' => 2.0,           // seconds
        'portfolio' => 3.0,
        'holdings' => 4.0,
        'performance' => 2.5,
        'algorithms' => 2.0,
      ],
      'memory_limits' => [
        'peak_usage_mb' => 256,       // MB
        'per_request_mb' => 10,
      ],
      'database_queries' => [
        'max_per_page' => 50,
        'slow_query_threshold' => 0.1, // seconds
      ],
      'api_response_times' => [
        'health_check' => 1.0,        // seconds
        'portfolio_data' => 2.0,
        'real_time_data' => 3.0,
        'performance_metrics' => 5.0,
      ],
    ];
  }

  /**
   * Test user data.
   *
   * @return array
   *   Array of test user data.
   */
  public static function getTestUsers() {
    return [
      'admin' => [
        'name' => 'admin',
        'password' => 'admin',
        'email' => 'admin@unicorn.test',
        'roles' => ['administrator'],
        'permissions' => [
          'access unicorn metrics',
          'administer unicorn metrics',
          'access administration pages',
        ],
      ],
      'metrics_user' => [
        'name' => 'metrics_user',
        'password' => 'test123',
        'email' => 'metrics@unicorn.test',
        'roles' => ['unicorn_metrics_user'],
        'permissions' => [
          'access unicorn metrics',
        ],
      ],
      'unauthorized' => [
        'name' => 'unauthorized',
        'password' => 'test123',
        'email' => 'unauthorized@unicorn.test',
        'roles' => ['authenticated'],
        'permissions' => [],
      ],
    ];
  }

}