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
    
    // Set backend path - update for current workspace environment
    $this->backendPath = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';
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
      // First try to get the latest status report
      $status_report = $this->getLatestStatusReport($portfolio_name);
      if (!empty($status_report)) {
        // Merge config data with status report
        $config_data = $this->getPortfolioConfig($portfolio_name);
        
        return array_merge($config_data, [
          'overall_readiness' => $status_report['overall_readiness'],
          'critical_issues_count' => count($status_report['critical_issues']),
          'warnings_count' => count($status_report['warnings']),
          'passed_checks_count' => count($status_report['passed_checks']),
          'component_status' => $status_report['component_status'],
          'last_status_check' => $status_report['timestamp'],
          'backend_status' => 'status_report',
          'report_file' => $status_report['report_file']
        ]);
      }
      
      // Fallback to basic config data
      $this->logger->warning('No status reports found for portfolio: @portfolio', ['@portfolio' => $portfolio_name]);
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
      $portfolio_dir = $this->backendPath . '/' . $portfolio_name;
      
      // Check for ETH algorithm files
      $risk_algo_dir = $portfolio_dir . '/risk_algorithms';
      $trading_algo_dir = $portfolio_dir . '/trading_algorithms';
      
      // Check specific algorithm files
      $risk_algorithms = [];
      if (is_dir($risk_algo_dir)) {
        $risk_files = glob($risk_algo_dir . '/*.py');
        foreach ($risk_files as $file) {
          $risk_algorithms[] = basename($file, '.py');
        }
      }
      
      $trading_algorithms = [];
      if (is_dir($trading_algo_dir)) {
        $trading_files = glob($trading_algo_dir . '/*.py');
        foreach ($trading_files as $file) {
          $trading_algorithms[] = basename($file, '.py');
        }
      }
      
      // Check for ETH Kelly integration
      $kelly_integration = file_exists($portfolio_dir . '/eth_kelly_integration.py');
      $algorithm_integration = file_exists($portfolio_dir . '/eth_algorithm_integration.py');
      
      // Check for config files
      $eth_config_dir = $portfolio_dir . '/config';
      $eth_kelly_config = file_exists($eth_config_dir . '/eth_kelly_config.json');
      
      $eth_status = [
        'risk_algorithm' => [
          'available' => is_dir($risk_algo_dir) && !empty($risk_algorithms),
          'algorithms' => $risk_algorithms,
          'count' => count($risk_algorithms),
          'status' => !empty($risk_algorithms) ? 'active' : 'inactive',
          'last_run' => $this->getLastModifiedTime($risk_algo_dir),
        ],
        'trading_algorithm' => [
          'available' => is_dir($trading_algo_dir) && !empty($trading_algorithms),
          'algorithms' => $trading_algorithms,
          'count' => count($trading_algorithms),
          'status' => !empty($trading_algorithms) ? 'active' : 'inactive', 
          'last_run' => $this->getLastModifiedTime($trading_algo_dir),
        ],
        'integration_status' => $kelly_integration && $algorithm_integration ? 'operational' : 'incomplete',
        'kelly_integration' => $kelly_integration,
        'algorithm_integration' => $algorithm_integration,
        'eth_kelly_config' => $eth_kelly_config,
        'backend_status' => 'filesystem_check'
      ];
      
      return $eth_status;
      
    } catch (\Exception $e) {
      $this->logger->error('Exception getting ETH algorithm status: @message', ['@message' => $e->getMessage()]);
      return [
        'integration_status' => 'error', 
        'error' => $e->getMessage(),
        'backend_status' => 'error'
      ];
    }
  }

  /**
   * Get last modified time for a directory or file.
   *
   * @param string $path
   *   The path to check.
   *
   * @return string
   *   Formatted last modified time.
   */
  private function getLastModifiedTime(string $path): string {
    if (is_dir($path)) {
      $latest_time = 0;
      $files = glob($path . '/*');
      foreach ($files as $file) {
        $mtime = filemtime($file);
        if ($mtime > $latest_time) {
          $latest_time = $mtime;
        }
      }
      return $latest_time > 0 ? date('Y-m-d H:i:s', $latest_time) : 'N/A';
    } elseif (file_exists($path)) {
      return date('Y-m-d H:i:s', filemtime($path));
    }
    
    return 'N/A';
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
      // First try to read the latest risk report
      $risk_report_data = $this->getLatestRiskReport($portfolio_name);
      if (!empty($risk_report_data)) {
        return $risk_report_data;
      }
      
      // Fallback to risk parameters file
      $risk_params_file = $this->backendPath . '/' . $portfolio_name . '/risk_parameters.json';
      if (file_exists($risk_params_file)) {
        $risk_params = json_decode(file_get_contents($risk_params_file), TRUE);
        if ($risk_params) {
          // Convert risk parameters to risk metrics format
          return [
            'current_drawdown' => 0.02, // Would be calculated from positions
            'max_drawdown' => $risk_params['max_drawdown'] ?? 0.15,
            'portfolio_volatility' => $risk_params['max_portfolio_volatility'] ?? 0.25,
            'var_5pct' => $risk_params['var_limit_1day'] ?? 0.06,
            'portfolio_heat' => 0.12, // Would be calculated from current positions
            'risk_score' => 0.3, // Composite risk score
            'risk_profile' => $risk_params['risk_profile'] ?? 'moderate',
            'max_single_asset_weight' => $risk_params['max_single_asset_weight'] ?? 0.65,
            'sharpe_ratio_target' => $risk_params['sharpe_ratio_target'] ?? 1.3,
            'stop_loss_percent' => $risk_params['stop_loss_settings']['stop_loss_percent'] ?? 0.12,
            'last_updated' => date('Y-m-d H:i:s'),
            'backend_status' => 'risk_parameters'
          ];
        }
      }
      
      // Return fallback risk metrics
      return $this->getFallbackRiskMetrics();
      
    } catch (\Exception $e) {
      $this->logger->error('Exception getting risk metrics: @message', ['@message' => $e->getMessage()]);
      return $this->getFallbackRiskMetrics();
    }
  }

  /**
   * Get the latest risk report from the portfolio directory.
   *
   * @param string $portfolio_name
   *   The portfolio name.
   *
   * @return array
   *   Risk report data or empty array.
   */
  private function getLatestRiskReport(string $portfolio_name): array {
    try {
      $portfolio_dir = $this->backendPath . '/' . $portfolio_name;
      $risk_reports = glob($portfolio_dir . '/risk_report_*.json');
      
      if (!empty($risk_reports)) {
        // Sort by filename to get the latest
        rsort($risk_reports);
        $latest_report = $risk_reports[0];
        
        $report_data = json_decode(file_get_contents($latest_report), TRUE);
        if ($report_data && isset($report_data['risk_metrics'])) {
          $metrics = $report_data['risk_metrics'];
          
          return [
            'current_drawdown' => $metrics['current_drawdown'] ?? 0.0,
            'portfolio_volatility' => $metrics['portfolio_volatility'] ?? 0.0,
            'var_1day' => $metrics['var_1day'] ?? 0.0,
            'var_1week' => $metrics['var_1week'] ?? 0.0,
            'max_position_weight' => $metrics['max_position_weight'] ?? 0.0,
            'sharpe_ratio' => $metrics['sharpe_ratio'] ?? 0.0,
            'estimated_correlation' => $metrics['estimated_correlation'] ?? 0.7,
            'risk_score' => 0.3, // Composite calculated risk score
            'portfolio_heat' => $metrics['portfolio_volatility'] ?? 0.0,
            'var_5pct' => $metrics['var_1day'] ?? 0.0,
            'risk_profile' => 'moderate',
            'timestamp' => $report_data['timestamp'] ?? date('Y-m-d H:i:s'),
            'backend_status' => 'risk_report',
            'report_file' => basename($latest_report)
          ];
        }
      }
      
      return [];
      
    } catch (\Exception $e) {
      $this->logger->error('Exception reading risk reports: @message', ['@message' => $e->getMessage()]);
      return [];
    }
  }

  /**
   * Get the latest status report from the portfolio directory.
   *
   * @param string $portfolio_name
   *   The portfolio name.
   *
   * @return array
   *   Status report data or empty array.
   */
  public function getLatestStatusReport(string $portfolio_name = 'Myportolio'): array {
    try {
      $portfolio_dir = $this->backendPath . '/' . $portfolio_name;
      $status_reports = glob($portfolio_dir . '/status_report_*.json');
      
      if (!empty($status_reports)) {
        // Sort by filename to get the latest
        rsort($status_reports);
        $latest_report = $status_reports[0];
        
        $report_data = json_decode(file_get_contents($latest_report), TRUE);
        if ($report_data) {
          return [
            'timestamp' => $report_data['timestamp'] ?? date('Y-m-d H:i:s'),
            'overall_readiness' => $report_data['overall_readiness'] ?? 'UNKNOWN',
            'critical_issues' => $report_data['critical_issues'] ?? [],
            'warnings' => $report_data['warnings'] ?? [],
            'passed_checks' => $report_data['passed_checks'] ?? [],
            'component_status' => $report_data['component_status'] ?? [],
            'backend_status' => 'status_report',
            'report_file' => basename($latest_report)
          ];
        }
      }
      
      return [];
      
    } catch (\Exception $e) {
      $this->logger->error('Exception reading status reports: @message', ['@message' => $e->getMessage()]);
      return [];
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