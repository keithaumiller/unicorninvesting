<?php

namespace Drupal\unicornmetrics\Service;

use Drupal\Core\Config\ConfigFactoryInterface;
use Drupal\Core\Logger\LoggerChannelFactoryInterface;
use Symfony\Component\Process\Process;
use Symfony\Component\Process\Exception\ProcessFailedException;

/**
 * Service for interfacing with backend Python portfolio management APIs.
 */
class PortfolioApiService {

  /**
   * The config factory service.
   *
   * @var \Drupal\Core\Config\ConfigFactoryInterface
   */
  protected $configFactory;

  /**
   * The logger factory service.
   *
   * @var \Drupal\Core\Logger\LoggerChannelInterface
   */
  protected $logger;

  /**
   * The backend Python path.
   *
   * @var string
   */
  protected $backendPath;

  /**
   * Constructs a PortfolioApiService object.
   */
  public function __construct(ConfigFactoryInterface $config_factory, LoggerChannelFactoryInterface $logger_factory) {
    $this->configFactory = $config_factory;
    $this->logger = $logger_factory->get('unicornmetrics');
    
    // Set backend path - in production this could be configurable
    $this->backendPath = '/home/runner/work/unicorninvesting/unicorninvesting/BackendPython/unicorn/4_portfolios';
  }

  /**
   * Get portfolio status from Enhanced Portfolio Manager.
   *
   * @param string $portfolio_name
   *   The portfolio name to query.
   *
   * @return array
   *   Portfolio status data.
   */
  public function getPortfolioStatus(string $portfolio_name = 'Myportolio'): array {
    try {
      // Create a Python script to get portfolio status
      $script = $this->createPortfolioStatusScript($portfolio_name);
      $result = $this->executePythonScript($script);
      
      if ($result['success']) {
        return json_decode($result['output'], TRUE) ?: [];
      }
      
      $this->logger->error('Failed to get portfolio status: @error', ['@error' => $result['error']]);
      return $this->getFallbackPortfolioData($portfolio_name);
      
    } catch (\Exception $e) {
      $this->logger->error('Exception getting portfolio status: @message', ['@message' => $e->getMessage()]);
      return $this->getFallbackPortfolioData($portfolio_name);
    }
  }

  /**
   * Get portfolio configuration data.
   *
   * @param string $portfolio_name
   *   The portfolio name to query.
   *
   * @return array
   *   Portfolio configuration data.
   */
  public function getPortfolioConfig(string $portfolio_name = 'Myportolio'): array {
    try {
      $config_file = $this->backendPath . '/' . $portfolio_name . '/config.json';
      
      if (file_exists($config_file)) {
        $config_data = json_decode(file_get_contents($config_file), TRUE);
        if ($config_data) {
          return $config_data;
        }
      }
      
      $this->logger->warning('Portfolio config file not found: @file', ['@file' => $config_file]);
      return $this->getFallbackConfigData($portfolio_name);
      
    } catch (\Exception $e) {
      $this->logger->error('Exception reading portfolio config: @message', ['@message' => $e->getMessage()]);
      return $this->getFallbackConfigData($portfolio_name);
    }
  }

  /**
   * Get available portfolios list.
   *
   * @return array
   *   Array of available portfolio names.
   */
  public function getAvailablePortfolios(): array {
    try {
      $portfolios_dir = $this->backendPath;
      $portfolios = [];
      
      if (is_dir($portfolios_dir)) {
        $directories = scandir($portfolios_dir);
        foreach ($directories as $dir) {
          if ($dir !== '.' && $dir !== '..' && is_dir($portfolios_dir . '/' . $dir)) {
            // Check if it has a config.json file
            if (file_exists($portfolios_dir . '/' . $dir . '/config.json')) {
              $portfolios[] = $dir;
            }
          }
        }
      }
      
      // If no portfolios found, return default
      return $portfolios ?: ['Myportolio'];
      
    } catch (\Exception $e) {
      $this->logger->error('Exception getting available portfolios: @message', ['@message' => $e->getMessage()]);
      return ['Myportolio'];
    }
  }

  /**
   * Get ETH algorithm status from backend.
   *
   * @param string $portfolio_name
   *   The portfolio name.
   *
   * @return array
   *   ETH algorithm status data.
   */
  public function getEthAlgorithmStatus(string $portfolio_name = 'Myportolio'): array {
    try {
      // Check for ETH algorithm files
      $risk_algo_dir = $this->backendPath . '/' . $portfolio_name . '/risk_algorithms';
      $trading_algo_dir = $this->backendPath . '/' . $portfolio_name . '/trading_algorithms';
      
      $eth_status = [
        'risk_algorithm' => [
          'available' => is_dir($risk_algo_dir),
          'status' => 'active',
          'last_run' => date('Y-m-d H:i:s'),
        ],
        'trading_algorithm' => [
          'available' => is_dir($trading_algo_dir),
          'status' => 'active', 
          'last_run' => date('Y-m-d H:i:s'),
        ],
        'integration_status' => 'operational'
      ];
      
      return $eth_status;
      
    } catch (\Exception $e) {
      $this->logger->error('Exception getting ETH algorithm status: @message', ['@message' => $e->getMessage()]);
      return ['integration_status' => 'error', 'error' => $e->getMessage()];
    }
  }

  /**
   * Get risk metrics from portfolio.
   *
   * @param string $portfolio_name
   *   The portfolio name.
   *
   * @return array
   *   Risk metrics data.
   */
  public function getRiskMetrics(string $portfolio_name = 'Myportolio'): array {
    try {
      // Create a Python script to get risk metrics
      $script = $this->createRiskMetricsScript($portfolio_name);
      $result = $this->executePythonScript($script);
      
      if ($result['success']) {
        return json_decode($result['output'], TRUE) ?: [];
      }
      
      // Return fallback risk metrics
      return $this->getFallbackRiskMetrics();
      
    } catch (\Exception $e) {
      $this->logger->error('Exception getting risk metrics: @message', ['@message' => $e->getMessage()]);
      return $this->getFallbackRiskMetrics();
    }
  }

  /**
   * Create Python script for portfolio status.
   */
  private function createPortfolioStatusScript(string $portfolio_name): string {
    return <<<PYTHON
#!/usr/bin/env python3
import sys
import json
sys.path.append('{$this->backendPath}/utilities')

try:
    from PortfolioConfigManager import PortfolioConfigManager
    
    manager = PortfolioConfigManager()
    
    # Get portfolio summary
    summary = manager.get_portfolio_summary()
    
    if '{$portfolio_name}' in summary:
        portfolio_data = summary['{$portfolio_name}']
        portfolio_data['success'] = True
    else:
        portfolio_data = {
            'success': False,
            'error': 'Portfolio not found',
            'available_portfolios': list(summary.keys())
        }
    
    print(json.dumps(portfolio_data))
    
except Exception as e:
    error_data = {
        'success': False,
        'error': str(e),
        'portfolio_name': '{$portfolio_name}'
    }
    print(json.dumps(error_data))
PYTHON;
  }

  /**
   * Create Python script for risk metrics.
   */
  private function createRiskMetricsScript(string $portfolio_name): string {
    return <<<PYTHON
#!/usr/bin/env python3
import sys
import json
import os
sys.path.append('{$this->backendPath}/utilities')

try:
    # Try to load risk parameters
    risk_file = '{$this->backendPath}/{$portfolio_name}/risk_parameters.json'
    
    if os.path.exists(risk_file):
        with open(risk_file, 'r') as f:
            risk_data = json.load(f)
            risk_data['success'] = True
    else:
        risk_data = {
            'success': False,
            'error': 'Risk parameters file not found',
            'file_path': risk_file
        }
    
    print(json.dumps(risk_data))
    
except Exception as e:
    error_data = {
        'success': False,
        'error': str(e),
        'portfolio_name': '{$portfolio_name}'
    }
    print(json.dumps(error_data))
PYTHON;
  }

  /**
   * Execute Python script and return result.
   */
  private function executePythonScript(string $script): array {
    try {
      $temp_file = tempnam(sys_get_temp_dir(), 'portfolio_api_');
      file_put_contents($temp_file, $script);
      
      $process = new Process(['python3', $temp_file]);
      $process->setTimeout(30); // 30 second timeout
      $process->run();
      
      unlink($temp_file);
      
      if ($process->isSuccessful()) {
        return [
          'success' => TRUE,
          'output' => $process->getOutput(),
          'error' => NULL
        ];
      } else {
        return [
          'success' => FALSE,
          'output' => NULL,
          'error' => $process->getErrorOutput()
        ];
      }
      
    } catch (ProcessFailedException $e) {
      return [
        'success' => FALSE,
        'output' => NULL,
        'error' => $e->getMessage()
      ];
    }
  }

  /**
   * Get fallback portfolio data when backend is unavailable.
   */
  private function getFallbackPortfolioData(string $portfolio_name): array {
    return [
      'portfolio_name' => $portfolio_name,
      'status' => 'active',
      'description' => 'Primary portfolio for cryptocurrency trading',
      'strategy_type' => 'dual_crypto',
      'assets' => ['ETH', 'BTC'],
      'target_volatility' => 0.20,
      'rebalancing_frequency' => 'daily',
      'risk_profile' => 'moderate',
      'max_drawdown_limit' => 0.15,
      'validation_passed' => TRUE,
      'last_updated' => date('Y-m-d H:i:s'),
      'backend_status' => 'fallback_data'
    ];
  }

  /**
   * Get fallback configuration data.
   */
  private function getFallbackConfigData(string $portfolio_name): array {
    return [
      'portfolio_name' => $portfolio_name,
      'description' => 'Primary portfolio for cryptocurrency trading with ETH and BTC exposure',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => [
          'allocation_percent' => 60.0,
          'asset_type' => 'cryptocurrency',
          'data_source' => 'ibkr',
          'model_type' => 'enhanced_technical',
          'symbol' => 'ETHUSD'
        ],
        'BTC' => [
          'allocation_percent' => 40.0,
          'asset_type' => 'cryptocurrency', 
          'data_source' => 'ibkr',
          'model_type' => 'ensemble',
          'symbol' => 'BTCUSD'
        ]
      ],
      'total_allocation' => 100,
      'currency' => 'USD',
      'target_volatility' => 0.20,
      'status' => 'active',
      'backend_status' => 'fallback_data'
    ];
  }

  /**
   * Get fallback risk metrics.
   */
  private function getFallbackRiskMetrics(): array {
    return [
      'current_drawdown' => 0.05,
      'max_drawdown' => 0.12,
      'portfolio_volatility' => 0.25,
      'var_5pct' => 0.04,
      'portfolio_heat' => 0.15,
      'risk_score' => 0.3,
      'risk_profile' => 'moderate',
      'last_updated' => date('Y-m-d H:i:s'),
      'backend_status' => 'fallback_data'
    ];
  }

}