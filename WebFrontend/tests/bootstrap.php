<?php

/**
 * @file
 * Bootstrap file for WebFrontend testing framework.
 * 
 * This file initializes the testing environment for comprehensive 
 * UnicornMetrics module testing including unit, functional, 
 * integration, and performance tests.
 */

use Drupal\Core\DrupalKernel;
use Symfony\Component\HttpFoundation\Request;

// Set testing environment constants
define('UNICORN_TEST_MODE', TRUE);
define('UNICORN_TEST_BASE_PATH', dirname(__FILE__));
define('UNICORN_WEB_ROOT', dirname(__DIR__) . '/web');

// Include Drupal autoloader
require_once UNICORN_WEB_ROOT . '/autoload.php';

// Set up Drupal testing environment
$autoloader = require UNICORN_WEB_ROOT . '/autoload.php';

// Initialize test environment variables
putenv('SIMPLETEST_BASE_URL=http://localhost');
putenv('SIMPLETEST_DB=mysql://root:@localhost/unicorn_test');

// Backend API testing configuration
putenv('BACKEND_API_BASE_URL=http://localhost:8000');
putenv('IBKR_GATEWAY_URL=http://localhost:5000');

// Ensure we're in testing mode
$_SERVER['SERVER_SOFTWARE'] = 'PHP-CLI';
$_SERVER['REQUEST_URI'] = '/';
$_SERVER['SCRIPT_NAME'] = '/index.php';
$_SERVER['HTTP_HOST'] = 'localhost';

/**
 * Initialize test kernel for integration tests.
 */
function initializeTestKernel() {
  $request = Request::createFromGlobals();
  $kernel = DrupalKernel::createFromRequest($request, $autoloader, 'testing');
  $kernel->boot();
  $kernel->preHandle($request);
  
  return $kernel;
}

/**
 * Set up test database connection.
 */
function setupTestDatabase() {
  // This will be used for database-dependent tests
  $test_db_url = getenv('SIMPLETEST_DB') ?: 'mysql://root:@localhost/unicorn_test';
  
  return $test_db_url;
}

/**
 * Validate backend connectivity for integration tests.
 */
function validateBackendConnectivity() {
  $backend_url = getenv('BACKEND_API_BASE_URL') ?: 'http://localhost:8000';
  $ibkr_url = getenv('IBKR_GATEWAY_URL') ?: 'http://localhost:5000';
  
  return [
    'backend_available' => @file_get_contents($backend_url . '/health', false, stream_context_create(['http' => ['timeout' => 1]])) !== FALSE,
    'ibkr_available' => @file_get_contents($ibkr_url . '/v1/api/portal/sso/validate', false, stream_context_create(['http' => ['timeout' => 1]])) !== FALSE,
  ];
}

// Register test cleanup handlers
register_shutdown_function(function() {
  // Cleanup any test artifacts
  if (defined('UNICORN_TEST_MODE') && UNICORN_TEST_MODE) {
    // Clean up temporary files, database connections, etc.
    // This will be expanded as tests are developed
  }
});

// Display test environment info
if (php_sapi_name() === 'cli') {
  echo "🧪 Unicorn WebFrontend Testing Framework Initialized\n";
  echo "   Test Base Path: " . UNICORN_TEST_BASE_PATH . "\n";
  echo "   Web Root: " . UNICORN_WEB_ROOT . "\n";
  echo "   Test DB: " . (getenv('SIMPLETEST_DB') ?: 'mysql://root:@localhost/unicorn_test') . "\n";
  echo "   Backend API: " . (getenv('BACKEND_API_BASE_URL') ?: 'http://localhost:8000') . "\n";
  echo "   IBKR Gateway: " . (getenv('IBKR_GATEWAY_URL') ?: 'http://localhost:5000') . "\n";
}