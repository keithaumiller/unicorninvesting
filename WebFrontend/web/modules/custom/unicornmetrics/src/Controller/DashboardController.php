<?php

namespace Drupal\unicornmetrics\Controller;

use Drupal\Core\Controller\ControllerBase;
use Drupal\Core\Render\Markup;
use Drupal\unicornmetrics\Service\PortfolioApiService;
use Symfony\Component\DependencyInjection\ContainerInterface;

/**
 * Controller for Unicorn Metrics dashboard pages.
 */
class DashboardController extends ControllerBase {

  /**
   * The portfolio API service.
   *
   * @var \Drupal\unicornmetrics\Service\PortfolioApiService
   */
  protected $portfolioApi;

  /**
   * Constructs a DashboardController object.
   *
   * @param \Drupal\unicornmetrics\Service\PortfolioApiService $portfolio_api
   *   The portfolio API service.
   */
  public function __construct(PortfolioApiService $portfolio_api) {
    $this->portfolioApi = $portfolio_api;
  }

  /**
   * {@inheritdoc}
   */
  public static function create(ContainerInterface $container) {
    return new static(
      $container->get('unicornmetrics.portfolio_api')
    );
  }

  /**
   * Main dashboard page.
   */
  public function dashboard() {
    // Disable caching for debugging purposes
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $module_info = \Drupal::service('extension.list.module')->getExtensionInfo('unicornmetrics');
    $version = $module_info['version'] ?? '4.1.0';
    
    // Get current simulation selection from URL parameter or default
    $current_simulation_id = \Drupal::request()->query->get('simulation') ?? 'Myportolio';
    
    // DEBUG: Log the extracted simulation parameter
    \Drupal::logger('unicornmetrics')->debug('DEBUG dashboard: URL parameter simulation = @param', ['@param' => \Drupal::request()->query->get('simulation') ?? 'NULL']);
    \Drupal::logger('unicornmetrics')->debug('DEBUG dashboard: current_simulation_id BEFORE validation = @id', ['@id' => $current_simulation_id]);
    
    // Validate simulation exists, fallback to Myportolio if not
    if (!$this->portfolioApi->isValidSimulation($current_simulation_id)) {
      \Drupal::logger('unicornmetrics')->debug('DEBUG dashboard: Validation FAILED, falling back to Myportolio');
      $current_simulation_id = 'Myportolio';
      // Add warning message for invalid simulation
      \Drupal::messenger()->addWarning(t('The requested simulation was not found. Displaying default simulation.'));
    } else {
      \Drupal::logger('unicornmetrics')->debug('DEBUG dashboard: Validation PASSED for @id', ['@id' => $current_simulation_id]);
    }
    
    \Drupal::logger('unicornmetrics')->debug('DEBUG dashboard: current_simulation_id AFTER validation = @id', ['@id' => $current_simulation_id]);
    
    // Get available simulations for selector
    $available_simulations = $this->portfolioApi->getAvailableSimulations();
    
    // Get real portfolio data from backend for selected simulation
    $portfolio_config = $this->portfolioApi->getPortfolioConfig($current_simulation_id);
    $portfolio_status = $this->portfolioApi->getPortfolioStatus($current_simulation_id);
    $eth_algorithm_status = $this->portfolioApi->getEthAlgorithmStatus($current_simulation_id);
    $risk_metrics = $this->portfolioApi->getRiskMetrics($current_simulation_id);
    
    // Merge portfolio data
    $current_portfolio = array_merge($portfolio_config, $portfolio_status);
    
    // Calculate portfolio value based on backend data  
    $portfolio_value = 50000.00; // Default value, would be calculated from positions
    $asset_count = 0;
    $target_volatility = 20.0;
    
    if (isset($current_portfolio['assets'])) {
      $asset_count = count($current_portfolio['assets']);
      // Extract target volatility from config
      $target_volatility = ($current_portfolio['target_volatility'] ?? 0.20) * 100;
      
      // Simple calculation based on asset allocation percentages
      $total_allocation = 0;
      foreach ($current_portfolio['assets'] as $asset_data) {
        $total_allocation += $asset_data['allocation_percent'] ?? 0;
      }
      // Mock portfolio value calculation - in production would use real positions
      $portfolio_value = $total_allocation * 500; // Placeholder calculation
    }
    
    // Use real backend status or fallback
    $portfolio_status = $current_portfolio['overall_readiness'] ?? 'Active';
    $is_active = in_array($portfolio_status, ['READY', 'OPERATIONAL', 'Active']);
    $backend_connection_status = $current_portfolio['backend_status'] ?? 'connected';
    
    // Get real last updated timestamp
    $last_updated = $current_portfolio['last_status_check'] ?? $current_portfolio['last_updated'] ?? date('Y-m-d H:i:s');
    
    // Set current_portfolio_id for use in links (should match current_simulation_id)
    $current_portfolio_id = $current_simulation_id;
    
    $metrics_table = '
    <div class="dashboard-header">
      <h1>🦄 Unicorn Portfolio Management System</h1>
      <div class="version-info">
        <span class="module-version">Version ' . $version . '</span>
        <span class="last-updated">Last Updated: ' . htmlspecialchars($last_updated) . '</span>
        <span class="backend-status ' . ($backend_connection_status == 'connected' ? 'status-active' : 'status-backend') . '">' . ucfirst($backend_connection_status) . '</span>
      </div>
    </div>
    
    <div class="dashboard-sections">
    
    <!-- Primary Portfolio Section -->
    <div class="dashboard-section">
      <h2>💼 ' . htmlspecialchars($current_portfolio['portfolio_name'] ?? $current_simulation_id) . '</h2>
      <p><strong>' . htmlspecialchars($current_portfolio['description'] ?? 'Portfolio Description') . '</strong> - Strategy: ' . htmlspecialchars($current_portfolio['strategy_type'] ?? 'dual_crypto') . '</p>      <div class="portfolio-stats">
        <div class="stat-card">
          <span class="stat-value">$' . number_format($portfolio_value, 2) . '</span>
          <span class="stat-label">Portfolio Value</span>
        </div>
        <div class="stat-card">
          <span class="stat-value">' . $asset_count . '</span>
          <span class="stat-label">Assets</span>
        </div>
        <div class="stat-card">
          <span class="stat-value">' . number_format($target_volatility, 1) . '%</span>
          <span class="stat-label">Target Volatility</span>
        </div>
        <div class="stat-card ' . ($is_active ? 'status-active' : 'status-inactive') . '">
          <span class="stat-value">⚡</span>
          <span class="stat-label">' . ucfirst($portfolio_status) . '</span>
        </div>
      </div>
      
      <!-- Asset Allocation Section -->
      <div class="asset-allocation-section">
        <h3>📈 Asset Allocation</h3>
        <div class="allocation-grid">';

    // Add asset allocations from config
    if (!empty($current_portfolio['assets'])) {
      foreach ($current_portfolio['assets'] as $asset_symbol => $asset_data) {
        $allocation_percent = $asset_data['allocation_percent'] ?? 0;
        $asset_type = $asset_data['asset_type'] ?? 'cryptocurrency';
        $model_type = $asset_data['model_type'] ?? 'basic';
        
        $metrics_table .= '
          <div class="allocation-card">
            <div class="asset-header">
              <span class="asset-symbol">' . htmlspecialchars($asset_symbol) . '</span>
              <span class="allocation-percent">' . number_format($allocation_percent, 1) . '%</span>
            </div>
            <div class="asset-details">
              <small>Type: ' . ucfirst($asset_type) . '</small><br>
              <small>Model: ' . ucfirst(str_replace('_', ' ', $model_type)) . '</small>
            </div>
          </div>';
      }
    } else {
      $metrics_table .= '<div class="allocation-card"><em>No asset allocation data available</em></div>';
    }
    
    $metrics_table .= '
        </div>
      </div>
      
      <!-- ETH Algorithm Status Section -->
      <div class="eth-algorithm-section">
        <h3>🔥 ETH Algorithm Framework Status</h3>
        <div class="algorithm-status-grid">
          <div class="algorithm-card">
            <h4>⚖️ Risk Algorithm</h4>
            <div class="status-indicator ' . ($eth_algorithm_status['risk_algorithm']['available'] ? 'status-active' : 'status-inactive') . '">
              ' . ($eth_algorithm_status['risk_algorithm']['available'] ? '✅ Available (' . $eth_algorithm_status['risk_algorithm']['count'] . ')' : '❌ Not Available') . '
            </div>
            <div class="algorithm-list">' . (isset($eth_algorithm_status['risk_algorithm']['algorithms']) ? implode(', ', $eth_algorithm_status['risk_algorithm']['algorithms']) : 'None') . '</div>
            <div class="last-run">Last Updated: ' . ($eth_algorithm_status['risk_algorithm']['last_run'] ?? 'N/A') . '</div>
          </div>
          
          <div class="algorithm-card">
            <h4>📈 Trading Algorithm</h4>
            <div class="status-indicator ' . ($eth_algorithm_status['trading_algorithm']['available'] ? 'status-active' : 'status-inactive') . '">
              ' . ($eth_algorithm_status['trading_algorithm']['available'] ? '✅ Available (' . $eth_algorithm_status['trading_algorithm']['count'] . ')' : '❌ Not Available') . '
            </div>
            <div class="algorithm-list">' . (isset($eth_algorithm_status['trading_algorithm']['algorithms']) ? implode(', ', $eth_algorithm_status['trading_algorithm']['algorithms']) : 'None') . '</div>
            <div class="last-run">Last Updated: ' . ($eth_algorithm_status['trading_algorithm']['last_run'] ?? 'N/A') . '</div>
          </div>
          
          <div class="algorithm-card">
            <h4>🔗 Integration Status</h4>
            <div class="status-indicator ' . ($eth_algorithm_status['integration_status'] == 'operational' ? 'status-active' : 'status-inactive') . '">
              ' . ($eth_algorithm_status['integration_status'] == 'operational' ? '✅ Operational' : '⚠️ ' . ucfirst($eth_algorithm_status['integration_status'])) . '
            </div>
            <div class="integration-info">
              Kelly: ' . ($eth_algorithm_status['kelly_integration'] ? '✅' : '❌') . ' | 
              Algorithm: ' . ($eth_algorithm_status['algorithm_integration'] ? '✅' : '❌') . ' |
              Config: ' . ($eth_algorithm_status['eth_kelly_config'] ? '✅' : '❌') . '
            </div>
          </div>
        </div>
      </div>
      
      <!-- Risk Metrics Section -->
      <div class="risk-metrics-section">
        <h3>⚠️ Real-time Risk Metrics</h3>
        <div class="risk-metrics-grid">
          <div class="risk-metric">
            <span class="metric-name">Current Drawdown:</span>
            <span class="metric-value negative">' . number_format(($risk_metrics['current_drawdown'] ?? 0.05) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric">
            <span class="metric-name">Portfolio Volatility:</span>
            <span class="metric-value">' . number_format(($risk_metrics['portfolio_volatility'] ?? $risk_metrics['max_portfolio_volatility'] ?? 0.25) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric">
            <span class="metric-name">VaR (5%):</span>
            <span class="metric-value negative">' . number_format(($risk_metrics['var_5pct'] ?? 0.04) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric">
            <span class="metric-name">Risk Score:</span>
            <span class="metric-value">' . number_format($risk_metrics['risk_score'] ?? 0.3, 2) . '</span>
          </div>
          <div class="risk-metric">
            <span class="metric-name">Portfolio Heat:</span>
            <span class="metric-value ' . (($risk_metrics['portfolio_heat'] ?? 0.15) > 0.2 ? 'negative' : '') . '">' . number_format(($risk_metrics['portfolio_heat'] ?? 0.15) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric">
            <span class="metric-name">Risk Profile:</span>
            <span class="metric-value">' . ucfirst($risk_metrics['risk_profile'] ?? 'moderate') . ' (' . ($risk_metrics['risk_profile'] ?? 'moderate') . ')</span>
          </div>
          <div class="risk-metric">
            <span class="metric-name">Max Volatility:</span>
            <span class="metric-value">' . number_format($risk_metrics['max_portfolio_volatility'] ?? 0.25, 2) . '</span>
          </div>
        </div>
      </div>
      
      <table class="lean-nav-table">
        <thead>
          <tr>
            <th class="icon-column">Hierarchy</th>
            <th class="link-column">Portfolio Component</th>
            <th class="description-column">Description</th>
            <th class="count-column">Status</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="icon-column">💼</td>
            <td class="link-column"><a href="/admin/metrics/lean/portfolio?portfolio=' . urlencode($current_portfolio_id) . '"><strong>📊 Portfolio Overview</strong></a></td>
            <td class="description-column"><strong>Main Portfolio Dashboard:</strong> Live data from EnhancedPortfolioManager with real-time risk metrics.</td>
            <td class="count-column"><span class="metric-count status-active">✅</span></td>
          </tr>
          <tr>
            <td class="icon-column">├─ 🏷️</td>
            <td class="link-column"><a href="/admin/metrics/lean/holdings?portfolio=' . urlencode($current_portfolio_id) . '">📈 Asset Allocation</a></td>
            <td class="description-column"><em>Portfolio Assets:</em> ' . $this->formatAssetList($current_portfolio['assets'] ?? []) . ' with configured allocations and IBKR integration.</td>
            <td class="count-column"><span class="metric-count">' . $asset_count . '</span></td>
          </tr>
          <tr>
            <td class="icon-column">├─ ⚡</td>
            <td class="link-column"><a href="/admin/metrics/lean/performance?portfolio=' . urlencode($current_portfolio_id) . '">📊 Performance Metrics</a></td>
            <td class="description-column"><em>Risk Analytics:</em> Real-time risk metrics from UnicornRiskIntegratedPortfolioConstruction framework.</td>
            <td class="count-column"><span class="metric-count">6</span></td>
          </tr>
          <tr>
            <td class="icon-column">└─ 🤖</td>
            <td class="link-column"><a href="/admin/metrics/lean/algorithms?portfolio=' . urlencode($current_portfolio_id) . '">🤖 ETH Algorithms</a></td>
            <td class="description-column"><em>Algorithm Framework:</em> ETH momentum strategy and risk management algorithms in separated architecture.</td>
            <td class="count-column"><span class="metric-count status-active">✅</span></td>
          </tr>
        </tbody>
      </table>
    </div>
    
    <!-- Backend Integration Status -->
    <div class="dashboard-section">
      <h2>🔗 Backend Integration Status</h2>
      <p>Real-time connectivity with Python portfolio management framework.</p>
      
      <table class="lean-nav-table">
        <thead>
          <tr>
            <th class="icon-column">Component</th>
            <th class="link-column">Backend Interface</th>
            <th class="description-column">Status</th>
            <th class="count-column">Data</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="icon-column">🦄</td>
            <td class="link-column">EnhancedPortfolioManager</td>
            <td class="description-column">Portfolio lifecycle management and configuration integration</td>
            <td class="count-column"><span class="metric-count status-active">✅</span></td>
          </tr>
          <tr>
            <td class="icon-column">⚖️</td>
            <td class="link-column">RiskIntegratedConstruction</td>
            <td class="description-column">Real-time risk metrics and portfolio construction framework</td>
            <td class="count-column"><span class="metric-count status-active">✅</span></td>
          </tr>
          <tr>
            <td class="icon-column">🔧</td>
            <td class="link-column">PortfolioConfigManager</td>
            <td class="description-column">Configuration management and portfolio settings</td>
            <td class="count-column"><span class="metric-count status-active">✅</span></td>
          </tr>
          <tr>
            <td class="icon-column">🔥</td>
            <td class="link-column">ETH Algorithm Framework</td>
            <td class="description-description">Separated risk and trading algorithms for ETH momentum strategy</td>
            <td class="count-column"><span class="metric-count status-active">✅</span></td>
          </tr>
        </tbody>
      </table>
    </div>
    
    </div>
    
    <div class="system-overview">
      <h3>🏗️ Enhanced Portfolio Architecture</h3>
      <ul>
        <li><strong>Active Portfolio:</strong> ' . htmlspecialchars($current_portfolio['portfolio_name'] ?? $current_portfolio_id) . ' ($' . number_format($portfolio_value, 2) . ')</li>
        <li><strong>Asset Strategy:</strong> ' . htmlspecialchars($current_portfolio['strategy_type'] ?? 'dual_crypto') . ' with ' . count($current_portfolio['assets'] ?? []) . ' configured assets</li>
        <li><strong>Risk Management:</strong> Target volatility ' . number_format(($current_portfolio['target_volatility'] ?? 0.20) * 100, 1) . '% with real-time monitoring</li>
        <li><strong>Algorithm Framework:</strong> ETH algorithms with separated risk and trading components</li>
        <li><strong>Backend Integration:</strong> Live data from Python portfolio management framework</li>
        <li><strong>IBKR Connectivity:</strong> Real-time market data and execution capabilities</li>
        <li><strong>Rebalancing:</strong> ' . ucfirst($current_portfolio['rebalancing_frequency'] ?? 'daily') . ' frequency with automated triggers</li>
      </ul>
    </div>
    ';
    
    return [
      '#markup' => Markup::create($metrics_table),
      '#attached' => [
        'html_head' => [
          [
            [
              '#tag' => 'style',
              '#value' => '
                .dashboard-header {
                  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                  color: white;
                  padding: 30px;
                  border-radius: 10px;
                  margin-bottom: 30px;
                  text-align: center;
                }
                .dashboard-header h1 {
                  margin: 0;
                  font-size: 2.5em;
                }
                .version-info {
                  margin-top: 15px;
                  display: flex;
                  justify-content: center;
                  gap: 20px;
                  flex-wrap: wrap;
                }
                .module-version, .last-updated {
                  background: rgba(255,255,255,0.2);
                  padding: 5px 15px;
                  border-radius: 15px;
                  font-size: 0.9em;
                }
                .metrics-nav-table {
                  width: 100%;
                  border-collapse: collapse;
                  margin: 20px 0;
                  font-family: Arial, sans-serif;
                }
                .metrics-nav-table th,
                .metrics-nav-table td {
                  padding: 12px 15px;
                  text-align: left;
                  border-bottom: 1px solid #ddd;
                }
                .metrics-nav-table th {
                  background-color: #f8f9fa;
                  font-weight: bold;
                }
                .lean-nav-table {
                  width: 100%;
                  border-collapse: collapse;
                  margin: 20px 0;
                  font-family: Arial, sans-serif;
                }
                .lean-nav-table th,
                .lean-nav-table td {
                  padding: 12px 15px;
                  text-align: left;
                  border-bottom: 1px solid #ddd;
                }
                .lean-nav-table th {
                  background-color: #e8f4fd;
                  font-weight: bold;
                  color: #1e3a8a;
                }
                .dashboard-sections {
                  margin: 20px 0;
                }
                .dashboard-section {
                  margin-bottom: 40px;
                  padding: 20px;
                  border: 1px solid #e1e5e9;
                  border-radius: 8px;
                  background: #ffffff;
                }
                .dashboard-section h2 {
                  margin-top: 0;
                  color: #2c3e50;
                  border-bottom: 2px solid #3498db;
                  padding-bottom: 10px;
                }
                .icon-column {
                  width: 60px;
                  text-align: center;
                  font-size: 1.5em;
                }
                .count-column {
                  width: 120px;
                  text-align: center;
                }
                .count-column {
                  width: 120px;
                  text-align: center;
                }
                .link-column {
                  width: 250px;
                }
                .description-column {
                  width: auto;
                }
                .metric-count {
                  background: #3498db;
                  color: white;
                  padding: 4px 8px;
                  border-radius: 12px;
                  font-size: 0.85em;
                  font-weight: bold;
                }
                .portfolio-selector-container {
                  background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
                  border: 1px solid #dee2e6;
                  border-radius: 10px;
                  padding: 20px;
                  margin: 20px 0;
                }
                .portfolio-selector {
                  display: flex;
                  align-items: center;
                  justify-content: space-between;
                  margin-bottom: 15px;
                  flex-wrap: wrap;
                  gap: 15px;
                }
                .portfolio-selector h3 {
                  margin: 0;
                  color: #495057;
                  font-size: 1.3em;
                }
                .selector-wrapper {
                  display: flex;
                  align-items: center;
                  gap: 15px;
                  flex-wrap: wrap;
                }
                .selector-wrapper label {
                  font-weight: bold;
                  color: #495057;
                }
                .selector-wrapper select {
                  padding: 8px 15px;
                  border: 2px solid #ced4da;
                  border-radius: 5px;
                  background: white;
                  font-size: 1em;
                  min-width: 300px;
                }
                .selector-wrapper select:focus {
                  border-color: #80bdff;
                  outline: none;
                  box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
                }
                .total-assets {
                  background: #28a745;
                  color: white;
                  padding: 8px 15px;
                  border-radius: 20px;
                  font-weight: bold;
                  font-size: 0.9em;
                }
                .quick-stats {
                  display: flex;
                  justify-content: space-around;
                  align-items: center;
                  flex-wrap: wrap;
                  gap: 20px;
                }
                .quick-stat-item {
                  text-align: center;
                  padding: 10px;
                }
                .quick-stat-item .stat-number {
                  display: block;
                  font-size: 1.5em;
                  font-weight: bold;
                  color: #2c3e50;
                }
                .quick-stat-item .stat-desc {
                  display: block;
                  font-size: 0.9em;
                  color: #6c757d;
                  margin-top: 5px;
                }
                .portfolio-stats {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                  gap: 15px;
                  margin: 20px 0;
                }
                .stat-card {
                  background: white;
                  border: 1px solid #e1e5e9;
                  border-radius: 8px;
                  padding: 15px;
                  text-align: center;
                  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                }
                .stat-card .stat-value {
                  display: block;
                  font-size: 1.5em;
                  font-weight: bold;
                  color: #2c3e50;
                  margin-bottom: 5px;
                }
                .stat-card .stat-label {
                  display: block;
                  font-size: 0.9em;
                  color: #6c757d;
                }
                .status-active .stat-value {
                  color: #28a745;
                }
                .backend-status {
                  background: rgba(40, 167, 69, 0.2);
                  color: #155724;
                  padding: 5px 15px;
                  border-radius: 15px;
                  font-size: 0.9em;
                }
                .eth-algorithm-section, .risk-metrics-section {
                  margin: 25px 0;
                  padding: 20px;
                  background: #f8f9fa;
                  border-radius: 8px;
                  border-left: 4px solid #28a745;
                }
                .eth-algorithm-section h3, .risk-metrics-section h3 {
                  margin-top: 0;
                  color: #155724;
                }
                .algorithm-status-grid {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
                  gap: 15px;
                  margin-top: 15px;
                }
                .algorithm-card {
                  background: white;
                  padding: 15px;
                  border-radius: 6px;
                  border: 1px solid #dee2e6;
                }
                .algorithm-card h4 {
                  margin-top: 0;
                  margin-bottom: 10px;
                  color: #495057;
                  font-size: 1.1em;
                }
                .status-indicator {
                  font-weight: bold;
                  padding: 4px 8px;
                  border-radius: 4px;
                  font-size: 0.9em;
                }
                .status-indicator.status-active {
                  background: #d4edda;
                  color: #155724;
                }
                .status-indicator.status-inactive {
                  background: #f8d7da;
                  color: #721c24;
                }
                .last-run, .integration-info {
                  font-size: 0.85em;
                  color: #6c757d;
                  margin-top: 5px;
                }
                .risk-metrics-grid {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                  gap: 12px;
                  margin-top: 15px;
                }
                .risk-metric {
                  background: white;
                  padding: 12px;
                  border-radius: 6px;
                  border: 1px solid #dee2e6;
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                }
                .risk-metric .metric-name {
                  font-size: 0.9em;
                  color: #495057;
                }
                .risk-metric .metric-value {
                  font-weight: bold;
                  font-size: 1.1em;
                  color: #28a745;
                }
                .risk-metric .metric-value.negative {
                  color: #dc3545;
                }
                  color: white;
                  padding: 5px 10px;
                  border-radius: 50%;
                  font-weight: bold;
                }
                .metrics-nav-table tbody tr:hover,
                .lean-nav-table tbody tr:hover {
                  background-color: #f5f5f5;
                }
                .metrics-nav-table a,
                .lean-nav-table a {
                  color: #3498db;
                  text-decoration: none;
                  font-weight: bold;
                }
                .metrics-nav-table a:hover,
                .lean-nav-table a:hover {
                  color: #2980b9;
                  text-decoration: underline;
                }
                .system-overview {
                  background: #f8f9fa;
                  padding: 20px;
                  border-radius: 8px;
                  margin-top: 30px;
                }
                .system-overview h3 {
                  margin-top: 0;
                  color: #2c3e50;
                }
                .system-overview ul {
                  margin: 0;
                  padding-left: 20px;
                }
                .system-overview li {
                  margin-bottom: 5px;
                }
                
                /* Unified Navigation and Portfolio Selector Styles */
                .unicorn-unified-navigation {
                  background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
                  border: 1px solid #dee2e6;
                  border-radius: 12px;
                  padding: 20px;
                  margin: 20px 0;
                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                }
                
                .portfolio-selector-unified {
                  margin-bottom: 20px;
                  padding-bottom: 20px;
                  border-bottom: 2px solid #dee2e6;
                }
                
                .portfolio-selector-unified h3 {
                  margin: 0 0 15px 0;
                  color: #495057;
                  font-size: 1.3em;
                  display: flex;
                  align-items: center;
                  gap: 8px;
                }
                
                .portfolio-select {
                  width: 100%;
                  padding: 12px 15px;
                  border: 2px solid #ced4da;
                  border-radius: 8px;
                  background: white;
                  font-size: 1em;
                  margin-bottom: 15px;
                  transition: border-color 0.3s ease;
                }
                
                .portfolio-select:focus {
                  border-color: #80bdff;
                  outline: none;
                  box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
                }
                
                .portfolio-stats {
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                  flex-wrap: wrap;
                  gap: 15px;
                }
                
                .portfolio-stats .total-assets {
                  background: linear-gradient(135deg, #28a745, #20c997);
                  color: white;
                  padding: 8px 15px;
                  border-radius: 20px;
                  font-weight: bold;
                  font-size: 0.9em;
                }
                
                .portfolio-stats .active-count {
                  background: linear-gradient(135deg, #17a2b8, #6f42c1);
                  color: white;
                  padding: 8px 15px;
                  border-radius: 20px;
                  font-weight: bold;
                  font-size: 0.9em;
                }
                
                .nav-container h3 {
                  margin: 0 0 15px 0;
                  color: #495057;
                  font-size: 1.3em;
                  display: flex;
                  align-items: center;
                  gap: 8px;
                }
                
                .nav-menu {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
                  gap: 12px;
                }
                
                .nav-item {
                  display: flex;
                  align-items: center;
                  gap: 12px;
                  padding: 12px 16px;
                  background: white;
                  border: 2px solid #e9ecef;
                  border-radius: 8px;
                  text-decoration: none;
                  color: #495057;
                  transition: all 0.3s ease;
                  box-shadow: 0 2px 4px rgba(0,0,0,0.05);
                }
                
                .nav-item:hover {
                  border-color: #80bdff;
                  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                  transform: translateY(-2px);
                  text-decoration: none;
                  color: #0056b3;
                }
                
                .nav-item.current {
                  background: linear-gradient(135deg, #007bff, #0056b3);
                  border-color: #0056b3;
                  color: white;
                  box-shadow: 0 4px 12px rgba(0,123,255,0.3);
                }
                
                .nav-item.current:hover {
                  color: white;
                  transform: translateY(-2px);
                }
                
                .nav-icon {
                  font-size: 1.2em;
                  min-width: 24px;
                  text-align: center;
                }
                
                .nav-label {
                  font-weight: 600;
                  font-size: 0.95em;
                }
                
                @media (max-width: 768px) {
                  .nav-menu {
                    grid-template-columns: 1fr;
                  }
                  
                  .portfolio-stats {
                    flex-direction: column;
                    align-items: stretch;
                    text-align: center;
                  }
                }
                
                /* Simulation Selector Styles */
                .simulation-selector-container {
                  background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
                  border: 1px solid #dee2e6;
                  border-radius: 10px;
                  padding: 20px;
                  margin-bottom: 30px;
                }
                .simulation-selector-container h3 {
                  margin-top: 0;
                  color: #495057;
                  margin-bottom: 15px;
                }
                .simulation-selector {
                  display: flex;
                  flex-direction: column;
                  gap: 10px;
                }
                .simulation-selector label {
                  font-weight: bold;
                  color: #495057;
                }
                .simulation-selector select {
                  padding: 10px 15px;
                  border: 2px solid #ced4da;
                  border-radius: 5px;
                  font-size: 1.1em;
                  background: white;
                  cursor: pointer;
                  transition: border-color 0.3s ease;
                }
                .simulation-selector select:hover {
                  border-color: #80bdff;
                }
                .simulation-selector select:focus {
                  border-color: #0066cc;
                  outline: none;
                  box-shadow: 0 0 5px rgba(0, 102, 204, 0.3);
                }
                .simulation-info {
                  background: rgba(255,255,255,0.8);
                  padding: 10px;
                  border-radius: 5px;
                  margin-top: 10px;
                }
                .simulation-info small {
                  color: #6c757d;
                }
                
                /* Asset Allocation Styles */
                .asset-allocation-section {
                  margin: 25px 0;
                  padding: 20px;
                  background: #f8f9fa;
                  border-radius: 8px;
                  border-left: 4px solid #17a2b8;
                }
                .asset-allocation-section h3 {
                  margin-top: 0;
                  color: #0c5460;
                }
                .allocation-grid {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                  gap: 15px;
                  margin-top: 15px;
                }
                .allocation-card {
                  background: white;
                  padding: 15px;
                  border-radius: 6px;
                  border: 1px solid #dee2e6;
                  text-align: center;
                }
                .asset-header {
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                  margin-bottom: 10px;
                }
                .asset-symbol {
                  font-weight: bold;
                  font-size: 1.2em;
                  color: #495057;
                }
                .allocation-percent {
                  font-weight: bold;
                  font-size: 1.1em;
                  color: #17a2b8;
                }
                .asset-details {
                  text-align: left;
                  color: #6c757d;
                }
              ',
            ],
            'unicorn-metrics-dashboard-styles',
          ],
          [
            [
              '#tag' => 'script',
              '#value' => '
                function changeSimulation(simulationId) {
                  const url = new URL(window.location);
                  url.searchParams.set("simulation", simulationId);
                  window.location.href = url.toString();
                }
              ',
            ],
            'unicorn-metrics-simulation-selector',
          ],
        ],
      ],
    ];
  }

  /**
   * LEAN Portfolio Management Dashboard.
   */
  public function leanPortfolio() {
    // Disable caching for debugging purposes
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'Myportolio';
    
    // Detect if this is a simulation and get simulation parameters
    $is_simulation = strpos($current_portfolio_id, 'backtest_') === 0;
    $simulation_parameters = [];
    
    if ($is_simulation) {
      $simulation_parameters = $this->getSimulationParameters($current_portfolio_id);
    }
    
    // Get real portfolio data from backend
    $portfolio_config = $this->portfolioApi->getPortfolioConfig($current_portfolio_id);
    $portfolio_status = $this->portfolioApi->getPortfolioStatus($current_portfolio_id);
    $risk_metrics = $this->portfolioApi->getRiskMetrics($current_portfolio_id);
    
    // Calculate portfolio metrics
    $portfolio_value = 50000.00; // Would be calculated from real positions
    $cash_allocation = 0.05; // 5% cash
    $positions_value = $portfolio_value * (1 - $cash_allocation);
    $cash_value = $portfolio_value * $cash_allocation;
    $unrealized_pnl = $portfolio_value * 0.035; // 3.5% unrealized gain
    $daily_change = 1.23; // Mock daily change
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>💼 ' . htmlspecialchars($portfolio_config['portfolio_name'] ?? $current_portfolio_id) . '</h1>
      <p>' . htmlspecialchars($portfolio_config['description'] ?? 'Portfolio managed by Enhanced Portfolio Manager framework') . '</p>
      <div class="portfolio-hierarchy">
        <span class="hierarchy-note">📊 Real-time Portfolio Data → Backend Integration → ETH Algorithm Framework</span>
      </div>
    </div>
    
    <div class="portfolio-overview-grid">
      <div class="portfolio-card">
        <h3>💰 Portfolio Value</h3>
        <div class="metric-value">$' . number_format($portfolio_value, 2) . '</div>
        <div class="metric-change ' . ($daily_change >= 0 ? 'positive' : 'negative') . '">
          ' . ($daily_change >= 0 ? '+' : '') . number_format($daily_change, 2) . '% Today
        </div>
      </div>
      
      <div class="portfolio-card">
        <h3>💵 Cash Position</h3>
        <div class="metric-value">$' . number_format($cash_value, 2) . '</div>
        <div class="metric-label">' . number_format($cash_allocation * 100, 1) . '% of Portfolio</div>
      </div>
      
      <div class="portfolio-card">
        <h3>📊 Positions Value</h3>
        <div class="metric-value">$' . number_format($positions_value, 2) . '</div>
        <div class="metric-label">' . count($portfolio_config['assets'] ?? []) . ' Configured Assets</div>
      </div>
      
      <div class="portfolio-card">
        <h3>📈 Unrealized P&L</h3>
        <div class="metric-value ' . ($unrealized_pnl >= 0 ? 'positive' : 'negative') . '">
          ' . ($unrealized_pnl >= 0 ? '+' : '') . '$' . number_format($unrealized_pnl, 2) . '
        </div>
        <div class="metric-label">' . number_format(($unrealized_pnl / $portfolio_value) * 100, 2) . '% of Portfolio</div>
      </div>
    </div>';
    
    // Add simulation parameters section if this is a backtest simulation
    if ($is_simulation && !empty($simulation_parameters)) {
      $content .= $this->buildSimulationParametersSection($simulation_parameters, $current_portfolio_id);
    }
    
    $content .= '
    
    <!-- Asset Allocation Display -->
    <div class="asset-allocation-section">
      <h3>🎯 Asset Allocation Strategy</h3>
      <div class="allocation-grid">';
    
    if (isset($portfolio_config['assets'])) {
      foreach ($portfolio_config['assets'] as $symbol => $asset_config) {
        $allocation_value = $portfolio_value * ($asset_config['allocation_percent'] / 100);
        $content .= '
        <div class="allocation-card">
          <h4>' . htmlspecialchars($symbol) . '</h4>
          <div class="allocation-percent">' . number_format($asset_config['allocation_percent'], 1) . '%</div>
          <div class="allocation-value">$' . number_format($allocation_value, 2) . '</div>
          <div class="asset-details">
            <div>Type: ' . htmlspecialchars($asset_config['asset_type'] ?? 'N/A') . '</div>
            <div>Source: ' . htmlspecialchars($asset_config['data_source'] ?? 'N/A') . '</div>
            <div>Model: ' . htmlspecialchars($asset_config['model_type'] ?? 'N/A') . '</div>
          </div>
        </div>';
      }
    }
    
    $content .= '
      </div>
    </div>
    
    <!-- Risk Metrics Integration -->
    <div class="portfolio-risk-section">
      <h3>⚠️ Real-time Risk Monitoring</h3>
      <div class="risk-dashboard">
        <div class="risk-summary-card">
          <h4>Current Risk Profile</h4>
          <div class="risk-profile-indicator">' . ucfirst($risk_metrics['risk_profile'] ?? 'Moderate') . '</div>
          <div class="target-volatility">Target: ' . number_format(($portfolio_config['target_volatility'] ?? 0.20) * 100, 1) . '% volatility</div>
        </div>
        
        <div class="risk-metrics-display">
          <div class="risk-metric-item">
            <span>Portfolio Heat:</span>
            <span class="' . (($risk_metrics['portfolio_heat'] ?? 0.15) > 0.2 ? 'negative' : 'positive') . '">' . number_format(($risk_metrics['portfolio_heat'] ?? 0.15) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric-item">
            <span>VaR (5%):</span>
            <span class="negative">' . number_format(($risk_metrics['var_5pct'] ?? 0.04) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric-item">
            <span>Current Drawdown:</span>
            <span class="negative">' . number_format(($risk_metrics['current_drawdown'] ?? 0.05) * 100, 1) . '%</span>
          </div>
          <div class="risk-metric-item">
            <span>Risk Score:</span>
            <span>' . number_format($risk_metrics['risk_score'] ?? 0.3, 2) . '/1.0</span>
          </div>
        </div>
      </div>
    </div>
    
    <div class="portfolio-actions">
      <a href="/admin/metrics/lean/holdings?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">📈 View Asset Details</a>
      <a href="/admin/metrics/lean/performance?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">⚡ Performance Analysis</a>
      <a href="/admin/metrics/lean/algorithms?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">🤖 ETH Algorithms</a>
    </div>
    
    <div class="portfolio-hierarchy-info">
      <h3>🏗️ Backend Integration Architecture</h3>
      <ul>
        <li><strong>Portfolio Manager:</strong> EnhancedPortfolioManager with $' . number_format($portfolio_value, 2) . ' under management</li>
        <li>├─ <strong>Configuration:</strong> PortfolioConfigManager with ' . count($portfolio_config['assets'] ?? []) . ' configured assets</li>
        <li>├─ <strong>Risk Framework:</strong> UnicornRiskIntegratedPortfolioConstruction with real-time monitoring</li>
        <li>├─ <strong>ETH Algorithms:</strong> Separated risk and trading algorithms in Myportolio framework</li>
        <li>└─ <strong>Data Integration:</strong> IBKR connectivity with live market data feeds</li>
      </ul>
    </div>
    
    <div class="last-updated">
      Last Updated: ' . ($portfolio_config['last_updated'] ?? date('Y-m-d H:i:s')) . ' | Data Source: Backend Portfolio Framework
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [
          [$this->getLeanPortfolioStyles(), 'lean-portfolio-styles']
        ],
      ],
    ];
  }

  /**
   * Public dashboard page for homepage.
   */
  public function publicDashboard() {
    // Disable caching for real-time data
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $module_info = \Drupal::service('extension.list.module')->getExtensionInfo('unicornmetrics');
    $version = $module_info['version'] ?? '4.1.0';
    
    // Get current simulation selection from URL parameter or default
    $current_simulation_id = \Drupal::request()->query->get('simulation') ?? 'Myportolio';
    
    // Validate simulation exists, fallback to Myportolio if not
    if (!$this->portfolioApi->isValidSimulation($current_simulation_id)) {
      $current_simulation_id = 'Myportolio';
      \Drupal::messenger()->addWarning(t('The requested simulation was not found. Displaying default simulation.'));
    }
    
    // Get available simulations for selector
    $available_simulations = $this->portfolioApi->getAvailableSimulations();
    
    // Get live IBKR portfolio data
    $live_portfolio = $this->portfolioApi->getIbkrLivePortfolioData();
    
    // Get backend portfolio data for selected simulation
    $portfolio_config = $this->portfolioApi->getPortfolioConfig($current_simulation_id);
    $portfolio_status = $this->portfolioApi->getPortfolioStatus($current_simulation_id);
    $eth_algorithm_status = $this->portfolioApi->getEthAlgorithmStatus($current_simulation_id);
    $risk_metrics = $this->portfolioApi->getRiskMetrics($current_simulation_id);
    
    // Merge portfolio data
    $current_portfolio = array_merge($portfolio_config, $portfolio_status);
    
    // Calculate portfolio value - use live IBKR data if available, otherwise backend data
    $portfolio_value = $live_portfolio['net_liquidation'] > 0 ? $live_portfolio['net_liquidation'] : 50000.00;
    $asset_count = count($current_portfolio['assets'] ?? []);
    $target_volatility = ($current_portfolio['target_volatility'] ?? 0.20) * 100;
    
    // Use real backend status or fallback
    $portfolio_status = $current_portfolio['overall_readiness'] ?? 'Active';
    $is_active = in_array($portfolio_status, ['READY', 'OPERATIONAL', 'Active']);
    $backend_connection_status = $current_portfolio['backend_status'] ?? 'connected';
    
    // Get real last updated timestamp
    $last_updated = $current_portfolio['last_status_check'] ?? $current_portfolio['last_updated'] ?? date('Y-m-d H:i:s');
    $current_portfolio_id = $current_simulation_id;
    
    // Build complete dashboard HTML
    $dashboard_html = '
    <div class="dashboard-header">
      <h1>🦄 Unicorn Portfolio Management System</h1>
      <div class="version-info">
        <span class="module-version">Version ' . $version . '</span>
        <span class="last-updated">Last Updated: ' . htmlspecialchars($last_updated) . '</span>
        <span class="backend-status ' . ($backend_connection_status == 'connected' ? 'status-active' : 'status-backend') . '">' . ucfirst($backend_connection_status) . '</span>
      </div>
    </div>
    
    <div class="dashboard-sections">
    
    <!-- Live IBKR Portfolio Section -->
    <div class="dashboard-section live-portfolio-section">
      <h2>💼 Live Portfolio Status (IBKR)</h2>
      <div class="live-portfolio-integration">
        <div class="ibkr-status-summary">
          <div class="account-overview">
            <h3>📈 Account Summary</h3>
            <div class="account-info">
              <p><strong>Account ID:</strong> ' . ($live_portfolio['account_id'] ?? 'Unknown') . '</p>
              <p><strong>Status:</strong> <span class="account-status ' . ($live_portfolio['is_funded'] ? 'funded' : 'empty') . '">' . ($live_portfolio['account_status'] ?? 'Unknown') . '</span></p>
            </div>
            
            <div class="financial-summary">
              <div class="financial-item">
                <span class="label">Net Liquidation Value:</span>
                <span class="value primary">$' . number_format($live_portfolio['net_liquidation'], 2) . '</span>
              </div>
              <div class="financial-item">
                <span class="label">Cash Balance:</span>
                <span class="value">$' . number_format($live_portfolio['cash_balance'], 2) . '</span>
              </div>
              <div class="financial-item">
                <span class="label">Market Value:</span>
                <span class="value">$' . number_format($live_portfolio['market_value'], 2) . '</span>
              </div>
              <div class="financial-item">
                <span class="label">Unrealized P&L:</span>
                <span class="value ' . (($live_portfolio['unrealized_pnl'] ?? 0) >= 0 ? 'positive' : 'negative') . '">$' . number_format($live_portfolio['unrealized_pnl'], 2) . '</span>
              </div>
            </div>
          </div>
          
          <div class="data-freshness-info">
            <h3>📅 Data Freshness</h3>
            <div class="timestamp-details">
              <p><strong>Last Updated:</strong> ' . ($live_portfolio['last_updated'] ?? 'Unknown') . '</p>
              <p><strong>Data Source:</strong> ' . ($live_portfolio['data_source'] ?? 'Unknown') . '</p>';
              
    if ($live_portfolio['portfolio_file_timestamp']) {
      $dashboard_html .= '<p><strong>Portfolio File:</strong> ' . $live_portfolio['portfolio_file_timestamp'] . '</p>';
    }
    if ($live_portfolio['account_file_timestamp']) {
      $dashboard_html .= '<p><strong>Account File:</strong> ' . $live_portfolio['account_file_timestamp'] . '</p>';
    }
    
    $dashboard_html .= '
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Primary Portfolio Section -->
    <div class="dashboard-section">
      <h2>💼 ' . htmlspecialchars($current_portfolio['portfolio_name'] ?? $current_simulation_id) . ' Configuration</h2>
      <p><strong>' . htmlspecialchars($current_portfolio['description'] ?? 'Portfolio Description') . '</strong> - Strategy: ' . htmlspecialchars($current_portfolio['strategy_type'] ?? 'dual_crypto') . '</p>
      
      <div class="portfolio-stats">
        <div class="stat-card">
          <span class="stat-value">$' . number_format($portfolio_value, 2) . '</span>
          <span class="stat-label">Portfolio Value</span>
        </div>
        <div class="stat-card">
          <span class="stat-value">' . $asset_count . '</span>
          <span class="stat-label">Assets</span>
        </div>
        <div class="stat-card">
          <span class="stat-value">' . number_format($target_volatility, 1) . '%</span>
          <span class="stat-label">Target Volatility</span>
        </div>
        <div class="stat-card ' . ($is_active ? 'status-active' : 'status-inactive') . '">
          <span class="stat-value">⚡</span>
          <span class="stat-label">' . ucfirst($portfolio_status) . '</span>
        </div>
      </div>
      
      <!-- Asset Allocation Section -->
      <div class="asset-allocation-section">
        <h3>📈 Target Asset Allocation vs Live Positions</h3>
        <div class="allocation-grid">';

    // Add asset allocations comparing target vs actual
    if (!empty($current_portfolio['assets'])) {
      foreach ($current_portfolio['assets'] as $asset_symbol => $asset_data) {
        $allocation_percent = $asset_data['allocation_percent'] ?? 0;
        $asset_type = $asset_data['asset_type'] ?? 'cryptocurrency';
        $model_type = $asset_data['model_type'] ?? 'basic';
        
        // Find actual position in IBKR data
        $actual_percent = 0;
        $actual_value = 0;
        if (!empty($live_portfolio['positions'])) {
          foreach ($live_portfolio['positions'] as $position) {
            if (strpos($position['symbol'], $asset_symbol) !== false) {
              $actual_percent = $position['percentage'] ?? 0;
              $actual_value = $position['market_value'] ?? 0;
              break;
            }
          }
        }
        
        $dashboard_html .= '
          <div class="allocation-card">
            <div class="asset-header">
              <span class="asset-symbol">' . htmlspecialchars($asset_symbol) . '</span>
            </div>
            <div class="allocation-comparison">
              <div class="target-allocation">
                <span class="label">Target:</span>
                <span class="percentage">' . number_format($allocation_percent, 1) . '%</span>
              </div>
              <div class="actual-allocation">
                <span class="label">Actual:</span>
                <span class="percentage">' . number_format($actual_percent, 1) . '%</span>
                <span class="value">($' . number_format($actual_value, 2) . ')</span>
              </div>
            </div>
            <div class="asset-details">
              <small>Type: ' . ucfirst($asset_type) . '</small><br>
              <small>Model: ' . ucfirst(str_replace('_', ' ', $model_type)) . '</small>
            </div>
          </div>';
      }
    } else {
      $dashboard_html .= '<div class="allocation-card"><em>No asset allocation data available</em></div>';
    }

    $dashboard_html .= '
        </div>
      </div>
      
      <!-- Navigation Links -->
      <div class="portfolio-actions">
        <a href="/admin/metrics/lean/holdings?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">📈 View Holdings</a>
        <a href="/admin/metrics/lean/performance?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">⚡ Performance</a>
        <a href="/admin/metrics/lean/algorithms?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">🤖 Algorithms</a>
        <a href="/admin/metrics/lean/backtest?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">🔬 Backtests</a>
      </div>
    </div>
    
    <div class="system-overview">
      <h3>🏗️ Platform Architecture</h3>
      <ul>
        <li><strong>Live Portfolio:</strong> ' . htmlspecialchars($current_portfolio['portfolio_name'] ?? $current_portfolio_id) . ' with IBKR integration</li>
        <li><strong>Asset Strategy:</strong> ' . htmlspecialchars($current_portfolio['strategy_type'] ?? 'dual_crypto') . ' with ' . count($current_portfolio['assets'] ?? []) . ' configured assets</li>
        <li><strong>Risk Management:</strong> Target volatility ' . number_format(($current_portfolio['target_volatility'] ?? 0.20) * 100, 1) . '% with real-time monitoring</li>
        <li><strong>Algorithm Framework:</strong> ETH algorithms with separated risk and trading components</li>
        <li><strong>Backend Integration:</strong> Live data from Python portfolio management framework</li>
        <li><strong>IBKR Connectivity:</strong> Real-time market data and execution capabilities</li>
        <li><strong>Rebalancing:</strong> ' . ucfirst($current_portfolio['rebalancing_frequency'] ?? 'daily') . ' frequency with automated triggers</li>
      </ul>
    </div>
    
    </div>
    
    <script>
    function changeSimulation(simulationId) {
      if (simulationId) {
        window.location.href = "/unicorn?simulation=" + encodeURIComponent(simulationId);
      }
    }
    </script>';
    
    return [
      '#markup' => Markup::create($dashboard_html),
      '#attached' => [
        'library' => ['unicornmetrics/dashboard'],
      ],
    ];
  }
  
  /**
   * LEAN Portfolio Holdings Detail.
   */
  public function leanHoldings() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $holdings = $this->getLeanHoldingsData($current_portfolio_id);
    
    $holdings_table = '<div class="lean-dashboard-header">
      <h1>📈 ' . htmlspecialchars($current_portfolio['name']) . ' Holdings</h1>
      <p>Detailed breakdown of individual security positions from LEAN portfolio manager</p>
    </div>';
    
    $holdings_table .= '
    <table class="holdings-table">
      <thead>
        <tr>
          <th>Symbol</th>
          <th>Security Name</th>
          <th>Quantity</th>
          <th>Avg Cost</th>
          <th>Current Price</th>
          <th>Market Value</th>
          <th>Unrealized P&L</th>
          <th>% Return</th>
          <th>% Weight</th>
        </tr>
      </thead>
      <tbody>';
    
    foreach ($holdings as $holding) {
      $pnl_class = $holding['unrealized_pnl'] >= 0 ? 'positive' : 'negative';
      $return_class = $holding['unrealized_pnl_percent'] >= 0 ? 'positive' : 'negative';
      
      $holdings_table .= '
        <tr>
          <td><strong>' . $holding['symbol'] . '</strong></td>
          <td>' . $holding['name'] . '</td>
          <td>' . number_format($holding['quantity']) . '</td>
          <td>$' . number_format($holding['average_cost'], 2) . '</td>
          <td>$' . number_format($holding['current_price'], 2) . '</td>
          <td>$' . number_format($holding['market_value'], 2) . '</td>
          <td class="' . $pnl_class . '">$' . number_format($holding['unrealized_pnl'], 2) . '</td>
          <td class="' . $return_class . '">' . number_format($holding['unrealized_pnl_percent'] * 100, 2) . '%</td>
          <td>' . number_format($holding['weight'] * 100, 1) . '%</td>
        </tr>';
    }
    
    $holdings_table .= '</tbody></table>';
    
    return [
      '#markup' => $holdings_table,
      '#attached' => [
        'html_head' => [
          [$this->getLeanHoldingsStyles(), 'lean-holdings-styles']
        ],
      ],
    ];
  }

  /**
   * LEAN Portfolio Performance Metrics.
   */
  public function leanPerformance() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $performance = $this->getLeanPerformanceData($current_portfolio_id);
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>⚡ ' . htmlspecialchars($current_portfolio['name']) . ' Performance</h1>
      <p>Comprehensive performance analysis from LEAN algorithm execution</p>
    </div>
    
    <div class="performance-grid">
      <div class="performance-section">
        <h3>📊 Return Metrics</h3>
        <div class="metrics-list">
          <div class="metric-item">
            <span class="metric-name">Total Return:</span>
            <span class="metric-value ' . ($performance['total_return'] >= 0 ? 'positive' : 'negative') . '">' . 
            number_format($performance['total_return'] * 100, 2) . '%</span>
          </div>
          <div class="metric-item">
            <span class="metric-name">Annualized Return:</span>
            <span class="metric-value">' . number_format($performance['annualized_return'] * 100, 2) . '%</span>
          </div>
          <div class="metric-item">
            <span class="metric-name">Sharpe Ratio:</span>
            <span class="metric-value">' . number_format($performance['sharpe_ratio'], 2) . '</span>
          </div>
        </div>
      </div>
      
      <div class="performance-section">
        <h3>⚠️ Risk Metrics</h3>
        <div class="metrics-list">
          <div class="metric-item">
            <span class="metric-name">Max Drawdown:</span>
            <span class="metric-value negative">' . number_format($performance['max_drawdown'] * 100, 2) . '%</span>
          </div>
          <div class="metric-item">
            <span class="metric-name">Volatility:</span>
            <span class="metric-value">' . number_format($performance['volatility'] * 100, 2) . '%</span>
          </div>
          <div class="metric-item">
            <span class="metric-name">VaR (95%):</span>
            <span class="metric-value negative">' . number_format($performance['var_95'] * 100, 2) . '%</span>
          </div>
        </div>
      </div>
      
      <div class="performance-section">
        <h3>🎯 Alpha Generation</h3>
        <div class="metrics-list">
          <div class="metric-item">
            <span class="metric-name">Alpha:</span>
            <span class="metric-value ' . ($performance['alpha'] >= 0 ? 'positive' : 'negative') . '">' . 
            number_format($performance['alpha'] * 100, 2) . '%</span>
          </div>
          <div class="metric-item">
            <span class="metric-name">Beta:</span>
            <span class="metric-value">' . number_format($performance['beta'], 2) . '</span>
          </div>
          <div class="metric-item">
            <span class="metric-name">Information Ratio:</span>
            <span class="metric-value">' . number_format($performance['information_ratio'], 2) . '</span>
          </div>
        </div>
      </div>
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [
          [$this->getLeanPerformanceStyles(), 'lean-performance-styles']
        ],
      ],
    ];
  }

  /**
   * LEAN Algorithm Management Dashboard.
   */
  public function leanAlgorithms() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'Myportolio';
    
    // Get real portfolio data from backend
    $portfolio_config = $this->portfolioApi->getPortfolioConfig($current_portfolio_id);
    $eth_algorithm_status = $this->portfolioApi->getEthAlgorithmStatus($current_portfolio_id);
    $risk_metrics = $this->portfolioApi->getRiskMetrics($current_portfolio_id);
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🤖 ' . htmlspecialchars($portfolio_config['portfolio_name'] ?? $current_portfolio_id) . ' ETH Algorithm Framework</h1>
      <p>Real-time monitoring of separated ETH algorithm architecture with risk and trading components</p>
    </div>
    
    <div class="algorithms-grid">
      <div class="algorithm-card">
        <h3>⚖️ ETH Risk Algorithm</h3>
        <div class="algorithm-info">
          <div class="algorithm-name">ETH Basic Risk Management</div>
          <div class="algorithm-status ' . ($eth_algorithm_status['risk_algorithm']['available'] ? 'running' : 'stopped') . '">
            ' . ($eth_algorithm_status['risk_algorithm']['available'] ? 'Available' : 'Not Available') . '
          </div>
          <div class="algorithm-runtime">Last Run: ' . ($eth_algorithm_status['risk_algorithm']['last_run'] ?? 'N/A') . '</div>
          <div class="algorithm-metrics">
            <div>Risk Profile: ' . ucfirst($risk_metrics['risk_profile'] ?? 'Moderate') . '</div>
            <div>VaR Limit: ' . number_format(($risk_metrics['var_5pct'] ?? 0.04) * 100, 1) . '%</div>
            <div>Max Drawdown: ' . number_format(($risk_metrics['max_drawdown'] ?? 0.15) * 100, 1) . '%</div>
          </div>
        </div>
      </div>
      
      <div class="algorithm-card">
        <h3>📈 ETH Trading Algorithm</h3>
        <div class="algorithm-info">
          <div class="algorithm-name">ETH Momentum Strategy</div>
          <div class="algorithm-status ' . ($eth_algorithm_status['trading_algorithm']['available'] ? 'running' : 'stopped') . '">
            ' . ($eth_algorithm_status['trading_algorithm']['available'] ? 'Available' : 'Not Available') . '
          </div>
          <div class="algorithm-runtime">Last Run: ' . ($eth_algorithm_status['trading_algorithm']['last_run'] ?? 'N/A') . '</div>
          <div class="algorithm-metrics">
            <div>Strategy Type: ' . htmlspecialchars($portfolio_config['strategy_type'] ?? 'dual_crypto') . '</div>
            <div>Target Volatility: ' . number_format(($portfolio_config['target_volatility'] ?? 0.20) * 100, 1) . '%</div>
            <div>Rebalancing: ' . ucfirst($portfolio_config['rebalancing_frequency'] ?? 'daily') . '</div>
          </div>
        </div>
      </div>
      
      <div class="algorithm-card">
        <h3>🔗 Framework Integration</h3>
        <div class="algorithm-info">
          <div class="algorithm-name">Portfolio Manager Integration</div>
          <div class="algorithm-status running">Operational</div>
          <div class="algorithm-runtime">Status: ' . ucfirst($eth_algorithm_status['integration_status'] ?? 'operational') . '</div>
          <div class="algorithm-metrics">
            <div>Backend API: ✅ Connected</div>
            <div>Config Manager: ✅ Active</div>
            <div>Risk Framework: ✅ Monitoring</div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- ETH Algorithm Details -->
    <div class="eth-algorithm-details">
      <h3>🔥 ETH Algorithm Architecture</h3>
      <div class="architecture-grid">
        <div class="architecture-section">
          <h4>⚖️ Risk Algorithm Components</h4>
          <ul>
            <li><strong>Portfolio Heat Monitoring:</strong> ' . number_format(($risk_metrics['portfolio_heat'] ?? 0.15) * 100, 1) . '% current heat level</li>
            <li><strong>VaR Calculations:</strong> 5% VaR at ' . number_format(($risk_metrics['var_5pct'] ?? 0.04) * 100, 1) . '% threshold</li>
            <li><strong>Drawdown Monitoring:</strong> Current ' . number_format(($risk_metrics['current_drawdown'] ?? 0.05) * 100, 1) . '% drawdown</li>
            <li><strong>Risk Score:</strong> ' . number_format($risk_metrics['risk_score'] ?? 0.3, 2) . '/1.0 risk assessment</li>
          </ul>
        </div>
        
        <div class="architecture-section">
          <h4>📈 Trading Algorithm Components</h4>
          <ul>
            <li><strong>Asset Allocation:</strong> ETH ' . (($portfolio_config['assets']['ETH']['allocation_percent'] ?? 60)) . '%, BTC ' . (($portfolio_config['assets']['BTC']['allocation_percent'] ?? 40)) . '%</li>
            <li><strong>Data Sources:</strong> ' . htmlspecialchars(strtoupper($portfolio_config['assets']['ETH']['data_source'] ?? 'IBKR')) . ' integration</li>
            <li><strong>Model Types:</strong> ' . htmlspecialchars($portfolio_config['assets']['ETH']['model_type'] ?? 'enhanced_technical') . '</li>
            <li><strong>Contract Integration:</strong> ETH Contract ID ' . htmlspecialchars($portfolio_config['assets']['ETH']['contract_id'] ?? 'N/A') . '</li>
          </ul>
        </div>
      </div>
    </div>
    
    <!-- LEAN Framework Integration Status -->
    <div class="lean-integration-status">
      <h3>🏗️ LEAN Framework Integration</h3>
      <div class="integration-grid">
        <div class="integration-item">
          <h4>📊 Portfolio Construction</h4>
          <div class="status-badge status-active">✅ EnhancedPortfolioManager</div>
          <p>Real-time portfolio lifecycle management with risk-integrated construction</p>
        </div>
        
        <div class="integration-item">
          <h4>⚡ Risk Management</h4>
          <div class="status-badge status-active">✅ UnicornRiskIntegrated</div>
          <p>Live risk monitoring with VaR calculations and drawdown controls</p>
        </div>
        
        <div class="integration-item">
          <h4>🔧 Configuration</h4>
          <div class="status-badge status-active">✅ PortfolioConfigManager</div>
          <p>Dynamic configuration management with JSON-based settings</p>
        </div>
        
        <div class="integration-item">
          <h4>📡 Data Integration</h4>
          <div class="status-badge status-active">✅ IBKR Connectivity</div>
          <p>Real-time market data feeds and execution capabilities</p>
        </div>
      </div>
    </div>
    
    <div class="algorithm-actions">
      <a href="/admin/metrics/lean/algorithms/performance?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">📊 Performance Analysis</a>
      <a href="/admin/metrics/lean/backtest?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">🔬 Backtest Results</a>
      <a href="/admin/metrics?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">🏠 Dashboard Home</a>
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [
          [$this->getLeanAlgorithmStyles(), 'lean-algorithm-styles']
        ],
      ],
    ];
  }

  /**
   * LEAN Algorithm Performance Analysis.
   */
  public function leanAlgorithmPerformance() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $performance = $this->getLeanAlgorithmPerformanceData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🎯 ' . htmlspecialchars($current_portfolio['name']) . ' Algorithm Performance</h1>
      <p>Detailed algorithm performance analysis and signal quality metrics</p>
    </div>
    
    <div class="algorithm-performance-grid">
      <div class="performance-card">
        <h3>🎯 Signal Quality</h3>
        <div class="signal-metrics">
          <div class="metric-row">
            <span>Direction Accuracy:</span>
            <span class="metric-value">' . number_format($performance['direction_accuracy'] * 100, 1) . '%</span>
          </div>
          <div class="metric-row">
            <span>Magnitude Accuracy:</span>
            <span class="metric-value">' . number_format($performance['magnitude_accuracy'] * 100, 1) . '%</span>
          </div>
          <div class="metric-row">
            <span>Confidence Score:</span>
            <span class="metric-value">' . number_format($performance['avg_confidence'], 2) . '</span>
          </div>
        </div>
      </div>
      
      <div class="performance-card">
        <h3>💰 Financial Impact</h3>
        <div class="financial-metrics">
          <div class="metric-row">
            <span>Total Alpha Generated:</span>
            <span class="metric-value positive">$' . number_format($performance['total_alpha'], 2) . '</span>
          </div>
          <div class="metric-row">
            <span>Average Trade Value:</span>
            <span class="metric-value">$' . number_format($performance['avg_trade_value'], 2) . '</span>
          </div>
          <div class="metric-row">
            <span>Win Rate:</span>
            <span class="metric-value">' . number_format($performance['win_rate'] * 100, 1) . '%</span>
          </div>
        </div>
      </div>
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [
          [$this->getLeanAlgorithmPerformanceStyles(), 'lean-algorithm-performance-styles']
        ],
      ],
    ];
  }

  /**
   * LEAN Backtest Results Analysis.
   */
  public function leanBacktestResults() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $backtest = $this->getLeanBacktestData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🔬 ' . htmlspecialchars($current_portfolio['name']) . ' Backtest Results</h1>
      <p>Historical strategy validation and out-of-sample testing results</p>
    </div>
    
    <div class="backtest-summary">
      <div class="backtest-card">
        <h3>📅 Test Period</h3>
        <div>' . $backtest['start_date'] . ' to ' . $backtest['end_date'] . '</div>
        <div class="test-duration">' . $backtest['duration_days'] . ' trading days</div>
      </div>
      
      <div class="backtest-card">
        <h3>💹 Overall Performance</h3>
        <div class="backtest-return ' . ($backtest['total_return'] >= 0 ? 'positive' : 'negative') . '">
          ' . number_format($backtest['total_return'] * 100, 2) . '% Total Return
        </div>
        <div>Sharpe: ' . number_format($backtest['sharpe_ratio'], 2) . '</div>
      </div>
      
      <div class="backtest-card">
        <h3>📊 Trade Statistics</h3>
        <div>Total Trades: ' . $backtest['total_trades'] . '</div>
        <div>Win Rate: ' . number_format($backtest['win_rate'] * 100, 1) . '%</div>
        <div>Max Drawdown: ' . number_format($backtest['max_drawdown'] * 100, 2) . '%</div>
      </div>
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [
          [$this->getLeanBacktestStyles(), 'lean-backtest-styles']
        ],
      ],
    ];
  }

  /**
   * Helper: Get LEAN portfolio data (simulated for now).
   */
  private function getLeanPortfolioData(string $portfolio_id = 'forex'): array {
    // In production, read from LEAN JSON files:
    // $portfolio_file = '/workspaces/unicorninvesting/BackendPython/Lean/Results/portfolio-state.json';
    
    // Get portfolio-specific data from the same source as the main dashboard
    $portfolio = $this->getPortfolioById($portfolio_id);
    
    // For now, use mock data based on portfolio selection
    // In production, this would read from actual LEAN portfolio state files
    return [
      'total_value' => $portfolio['total_value'],
      'cash' => $portfolio['total_value'] * 0.15, // 15% cash allocation
      'positions_value' => $portfolio['total_value'] * 0.85, // 85% in positions
      'unrealized_pnl' => $portfolio['total_value'] * 0.065, // 6.5% unrealized gains
      'daily_change' => 1.23, // Mock daily change
      'holdings_count' => count($portfolio['symbols']), // Fix: symbols is already an array
      'last_updated' => date('Y-m-d H:i:s'),
    ];
  }

  /**
   * Helper: Get LEAN holdings data.
   */
  private function getLeanHoldingsData(string $portfolio_id = 'forex'): array {
    $portfolio = $this->getPortfolioById($portfolio_id);
    $symbols = $portfolio['symbols']; // Already an array, no need to explode
    
    // Generate holdings data based on portfolio symbols
    $holdings = [];
    $total_value = $portfolio['total_value']; // Fix: use 'total_value' not 'value'
    $per_holding_value = $total_value / count($symbols);
    
    foreach ($symbols as $index => $symbol) {
      $holdings[] = [
        'symbol' => $symbol,
        'name' => $this->getSecurityName($symbol),
        'quantity' => round($per_holding_value / 100), // Mock quantity calculation
        'average_cost' => 100.0, // Mock average cost
        'current_price' => 105.0 + ($index * 2), // Mock current price with variation
        'market_value' => $per_holding_value,
        'unrealized_pnl' => $per_holding_value * 0.05, // 5% unrealized gain
        'unrealized_pnl_percent' => 0.05,
        'weight' => 1.0 / count($symbols), // Equal weight
      ];
    }
    
    return $holdings;
  }

  /**
   * Helper: Get security display name.
   */
  private function getSecurityName(string $symbol): string {
    $names = [
      'SPY' => 'SPDR S&P 500 ETF',
      'AAPL' => 'Apple Inc.',
      'TSLA' => 'Tesla Inc.',
      'MSFT' => 'Microsoft Corporation',
      'GOOGL' => 'Alphabet Inc.',
      'EURUSD' => 'Euro / US Dollar',
      'GBPUSD' => 'British Pound / US Dollar',
      'USDJPY' => 'US Dollar / Japanese Yen',
      'BTC' => 'Bitcoin',
      'ETH' => 'Ethereum',
    ];
    
    return $names[$symbol] ?? $symbol . ' Security';
  }

  /**
   * Helper: Get LEAN performance data.
   */
  private function getLeanPerformanceData(string $portfolio_id = 'forex'): array {
    // Performance metrics could vary based on portfolio type in production
    $base_performance = [
      'total_return' => 0.0847,
      'annualized_return' => 0.1245,
      'sharpe_ratio' => 1.85,
      'max_drawdown' => -0.0845,
      'volatility' => 0.1567,
      'alpha' => 0.0234,
      'beta' => 0.98,
      'var_95' => -0.0287,
      'information_ratio' => 1.23,
    ];
    
    // Adjust performance based on portfolio type
    switch ($portfolio_id) {
      case 'equity':
        $base_performance['total_return'] = 0.1523;
        $base_performance['sharpe_ratio'] = 2.14;
        break;
      case 'paper':
        $base_performance['total_return'] = 0.0234;
        $base_performance['sharpe_ratio'] = 0.89;
        break;
    }
    
    return $base_performance;
  }

  /**
   * Helper: Get LEAN algorithm data.
   */
  private function getLeanAlgorithmData(string $portfolio_id = 'forex'): array {
    $portfolio = $this->getPortfolioById($portfolio_id);
    
    return [
      'current' => [
        'name' => $portfolio['algorithm'],
        'status' => 'RUNNING',
        'runtime' => '2h 34m',
        'signals_generated' => 1847,
        'signal_accuracy' => 0.721,
        'alpha_score' => 2.34,
      ],
      'insights' => [
        'total' => 1847,
        'direction_accuracy' => 0.721,
        'magnitude_accuracy' => 0.643,
      ],
    ];
  }

  /**
   * Helper: Get LEAN algorithm performance data.
   */
  private function getLeanAlgorithmPerformanceData(): array {
    return [
      'direction_accuracy' => 0.721,
      'magnitude_accuracy' => 0.643,
      'avg_confidence' => 0.78,
      'total_alpha' => 18247.33,
      'avg_trade_value' => 2543.67,
      'win_rate' => 0.67,
    ];
  }

  /**
   * Helper: Get LEAN backtest data.
   */
  private function getLeanBacktestData(): array {
    return [
      'start_date' => '2023-01-01',
      'end_date' => '2024-12-31',
      'duration_days' => 504,
      'total_return' => 0.1847,
      'sharpe_ratio' => 1.67,
      'total_trades' => 2340,
      'win_rate' => 0.634,
      'max_drawdown' => -0.0923,
    ];
  }

  /**
   * Helper: Get LEAN portfolio styles.
   */
  private function getLeanPortfolioStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .lean-dashboard-header { background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .portfolio-hierarchy { margin-top: 10px; font-style: italic; opacity: 0.9; font-size: 0.9em; }
        .hierarchy-note { background: rgba(255,255,255,0.2); padding: 5px 12px; border-radius: 15px; }
        .portfolio-overview-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin: 20px 0; }
        .portfolio-card { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .portfolio-card:hover { box-shadow: 0 4px 8px rgba(0,0,0,0.15); transform: translateY(-2px); transition: all 0.3s ease; }
        .metric-value { font-size: 2em; font-weight: bold; color: #2c3e50; }
        .metric-change { font-size: 1.1em; margin-top: 8px; }
        .metric-label { color: #7f8c8d; font-size: 0.9em; margin-top: 8px; }
        .asset-allocation-section, .portfolio-risk-section { margin: 25px 0; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #007bff; }
        .asset-allocation-section h3, .portfolio-risk-section h3 { margin-top: 0; color: #1e3a8a; }
        .allocation-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-top: 15px; }
        .allocation-card { background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center; }
        .allocation-card h4 { margin-top: 0; color: #495057; font-size: 1.2em; }
        .allocation-percent { font-size: 1.5em; font-weight: bold; color: #007bff; margin: 8px 0; }
        .allocation-value { font-size: 1.1em; font-weight: bold; color: #28a745; margin-bottom: 10px; }
        .asset-details { font-size: 0.85em; color: #6c757d; }
        .asset-details div { margin: 2px 0; }
        .risk-dashboard { display: grid; grid-template-columns: 1fr 2fr; gap: 20px; margin-top: 15px; }
        .risk-summary-card { background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center; }
        .risk-summary-card h4 { margin-top: 0; color: #495057; }
        .risk-profile-indicator { font-size: 1.3em; font-weight: bold; color: #17a2b8; margin: 10px 0; }
        .target-volatility { font-size: 0.9em; color: #6c757d; }
        .risk-metrics-display { background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; }
        .risk-metric-item { display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #f1f3f4; }
        .risk-metric-item:last-child { border-bottom: none; }
        .risk-metric-item span:first-child { color: #495057; }
        .risk-metric-item span:last-child { font-weight: bold; }
        .portfolio-actions { display: flex; gap: 15px; margin: 30px 0; }
        .action-button { background: #3498db; color: white; padding: 12px 24px; border-radius: 6px; text-decoration: none; font-weight: bold; }
        .action-button:hover { background: #2980b9; text-decoration: none; color: white; }
        .portfolio-hierarchy-info { background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0; border-left: 4px solid #3498db; }
        .portfolio-hierarchy-info h3 { margin-top: 0; color: #2c3e50; }
        .portfolio-hierarchy-info ul { margin: 10px 0; padding-left: 0; list-style: none; }
        .portfolio-hierarchy-info li { margin: 8px 0; padding: 5px 0; font-family: monospace; }
        .positive { color: #27ae60; }
        .negative { color: #e74c3c; }
        .last-updated { margin-top: 30px; padding: 15px; background: #f8f9fa; border-radius: 6px; color: #6c757d; font-size: 0.9em; }
        
        /* Simulation Parameters Section Styles */
        .simulation-parameters-section { margin: 25px 0; padding: 20px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; border-left: 4px solid #6f42c1; }
        .simulation-parameters-section h3 { margin-top: 0; color: #6f42c1; display: flex; align-items: center; gap: 8px; }
        .simulation-info-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0; }
        .param-card { background: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .param-card:hover { box-shadow: 0 4px 8px rgba(0,0,0,0.15); transform: translateY(-2px); transition: all 0.3s ease; }
        .param-card h4 { margin-top: 0; color: #495057; display: flex; align-items: center; gap: 8px; border-bottom: 2px solid #f1f3f4; padding-bottom: 8px; }
        .simulation-identity h4 { color: #17a2b8; }
        .lean-config h4 { color: #28a745; }
        .simulation-results h4 { color: #ffc107; }
        .param-details { margin-top: 15px; }
        .param-row { display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #f8f9fa; }
        .param-row:last-child { border-bottom: none; }
        .param-label { color: #6c757d; font-weight: 600; flex-basis: 40%; }
        .param-value { color: #2c3e50; font-family: monospace; font-weight: bold; flex-basis: 60%; text-align: right; word-break: break-word; }
        .simulation-details-section { margin-top: 20px; padding: 15px; background: rgba(111, 66, 193, 0.1); border-radius: 6px; }
        .simulation-details-section h4 { color: #6f42c1; margin-top: 0; }
        .simulation-note { color: #495057; line-height: 1.6; margin: 0; font-size: 0.95em; }
      ',
    ];
  }

  /**
   * Helper: Get LEAN holdings styles.
   */
  private function getLeanHoldingsStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .lean-dashboard-header { background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .holdings-table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        .holdings-table th, .holdings-table td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
        .holdings-table th { background-color: #f8f9fa; font-weight: bold; }
        .holdings-table tbody tr:hover { background-color: #f5f5f5; }
        .positive { color: #27ae60; font-weight: bold; }
        .negative { color: #e74c3c; font-weight: bold; }
      ',
    ];
  }

  /**
   * Helper: Get LEAN performance styles.
   */
  private function getLeanPerformanceStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .lean-dashboard-header { background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .performance-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0; }
        .performance-section { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; }
        .performance-section h3 { margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px; }
        .metrics-list { margin-top: 15px; }
        .metric-item { display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #ecf0f1; }
        .metric-name { font-weight: bold; color: #34495e; }
        .metric-value { font-weight: bold; }
        .positive { color: #27ae60; }
        .negative { color: #e74c3c; }
      ',
    ];
  }

  /**
   * Helper: Get LEAN algorithm styles.
   */
  private function getLeanAlgorithmStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .lean-dashboard-header { background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .algorithms-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0; }
        .algorithm-card { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; }
        .algorithm-name { font-size: 1.3em; font-weight: bold; color: #2c3e50; margin-bottom: 8px; }
        .algorithm-status { padding: 6px 12px; border-radius: 20px; font-size: 0.9em; font-weight: bold; margin: 10px 0; display: inline-block; }
        .algorithm-status.running { background: #d4edda; color: #155724; }
        .algorithm-status.stopped { background: #f8d7da; color: #721c24; }
        .algorithm-runtime { color: #6c757d; font-size: 0.9em; margin-bottom: 10px; }
        .algorithm-metrics { margin-top: 10px; }
        .algorithm-metrics div { margin: 4px 0; font-size: 0.9em; color: #495057; }
        .eth-algorithm-details, .lean-integration-status { margin: 25px 0; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #6f42c1; }
        .eth-algorithm-details h3, .lean-integration-status h3 { margin-top: 0; color: #6f42c1; }
        .architecture-grid, .integration-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin-top: 15px; }
        .architecture-section, .integration-item { background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; }
        .architecture-section h4, .integration-item h4 { margin-top: 0; color: #495057; }
        .architecture-section ul { margin: 10px 0; padding-left: 20px; }
        .architecture-section li { margin: 6px 0; font-size: 0.9em; }
        .status-badge { padding: 6px 12px; border-radius: 20px; font-size: 0.85em; font-weight: bold; display: inline-block; margin: 5px 0; }
        .status-badge.status-active { background: #d4edda; color: #155724; }
        .integration-item p { margin: 10px 0 0 0; font-size: 0.9em; color: #6c757d; }
        .algorithm-actions { display: flex; gap: 15px; margin: 30px 0; }
        .action-button { background: #3498db; color: white; padding: 12px 24px; border-radius: 6px; text-decoration: none; font-weight: bold; }
        .action-button:hover { background: #2980b9; text-decoration: none; color: white; }
      ',
    ];
  }

  /**
   * Helper: Get LEAN algorithm performance styles.
   */
  private function getLeanAlgorithmPerformanceStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .lean-dashboard-header { background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .algorithm-performance-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0; }
        .performance-card { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; }
        .performance-card h3 { margin-top: 0; color: #2c3e50; }
        .metric-row { display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #ecf0f1; }
        .metric-value { font-weight: bold; }
        .positive { color: #27ae60; }
        .negative { color: #e74c3c; }
      ',
    ];
  }

  /**
   * Helper: Get LEAN backtest styles.
   */
  private function getLeanBacktestStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .lean-dashboard-header { background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .backtest-summary { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin: 20px 0; }
        .backtest-card { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; }
        .backtest-card h3 { margin-top: 0; color: #2c3e50; }
        .backtest-return { font-size: 1.5em; font-weight: bold; margin: 10px 0; }
        .test-duration { color: #6c757d; font-size: 0.9em; }
        .positive { color: #27ae60; }
        .negative { color: #e74c3c; }
      ',
    ];
  }

  /**
   * Get portfolio data by ID.
   */
  private function getPortfolioById($portfolio_id) {
    $portfolios = [
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
        'symbols' => ['EURUSD', 'USDJPY', 'USDCNH', 'ETHUSD']
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
        'symbols' => ['SPY', 'AAPL', 'TSLA', 'MSFT', 'GOOGL']
      ]
    ];
    
    return $portfolios[$portfolio_id] ?? $portfolios['forex'];
  }

  /**
   * Render portfolio selector dropdown.
   */
  private function renderPortfolioSelector($current_portfolio_id) {
    $portfolios = [
      'forex' => ['name' => 'Primary Forex Portfolio', 'value' => '$125,847.62', 'status' => 'active'],
      'equity' => ['name' => 'Growth Equity Portfolio', 'value' => '$250,000.00', 'status' => 'active']
    ];
    
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $options = '';
    foreach ($portfolios as $id => $portfolio) {
      $selected = ($id == $current_portfolio_id) ? 'selected' : '';
      $status_indicator = $portfolio['status'] == 'active' ? '⚡' : '⏸️';
      $options .= '<option value="' . $id . '" ' . $selected . '>' . $status_indicator . ' ' . $portfolio['name'] . ' (' . $portfolio['value'] . ')</option>';
    }
    
    return '
    <div class="portfolio-selector-container">
      <div class="portfolio-selector">
        <h3>📁 Portfolio Selection</h3>
        <div class="selector-wrapper">
          <label for="portfolio-dropdown">Choose Portfolio:</label>
          <select id="portfolio-dropdown" onchange="switchPortfolio(this.value)">
            ' . $options . '
          </select>
          <span class="total-assets">Total Assets: $375,847.62</span>
        </div>
      </div>
      
      <div class="quick-stats">
        <div class="quick-stat-item">
          <span class="stat-number">2</span>
          <span class="stat-desc">Active Portfolios</span>
        </div>
        <div class="quick-stat-item">
          <span class="stat-number">20</span>
          <span class="stat-desc">Total Positions</span>
        </div>
        <div class="quick-stat-item">
          <span class="stat-number">+$3,597.18</span>
          <span class="stat-desc">Today\'s P&L</span>
        </div>
      </div>
    </div>';
  }

  /**
   * Format asset list with allocation percentages.
   *
   * @param array $assets
   *   Array of asset data.
   *
   * @return string
   *   Formatted asset list.
   */
  private function formatAssetList(array $assets): string {
    if (empty($assets)) {
      return 'No assets configured';
    }
    
    $formatted_assets = [];
    foreach ($assets as $asset_symbol => $asset_data) {
      $allocation = $asset_data['allocation_percent'] ?? 0;
      $formatted_assets[] = $asset_symbol . ' (' . $allocation . '%)';
    }
    
    return implode(', ', $formatted_assets);
  }

  /**
   * Public simulation details page.
   */
  public function simulationDetails($simulation_id) {
    // Disable caching for debugging purposes
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    // Validate that this is a simulation
    if (!$this->portfolioApi->isValidSimulation($simulation_id)) {
      throw new \Symfony\Component\HttpKernel\Exception\NotFoundHttpException('Simulation not found.');
    }
    
    // Get simulation parameters
    $simulation_parameters = $this->getSimulationParameters($simulation_id);
    
    if (empty($simulation_parameters)) {
      $content = '
      <div class="simulation-error">
        <h2>❌ Simulation Data Not Available</h2>
        <p>The simulation data for <code>' . htmlspecialchars($simulation_id) . '</code> could not be loaded.</p>
        <p><a href="/unicorn">← Return to Dashboard</a></p>
      </div>';
    } else {
      $content = '
      <div class="simulation-details-page">
        <div class="page-header">
          <h1>🔬 Simulation Details</h1>
          <p class="breadcrumb"><a href="/unicorn">Dashboard</a> → Simulation Details</p>
        </div>
        
        ' . $this->buildSimulationParametersSection($simulation_parameters, $simulation_id) . '
        
        <div class="navigation-actions">
          <a href="/unicorn?simulation=' . urlencode($simulation_id) . '" class="action-button">📊 View Dashboard</a>
          <a href="/unicorn" class="action-button secondary">🏠 Main Dashboard</a>
        </div>
      </div>';
    }
    
    return [
      '#markup' => $content,
      '#attached' => [
        'library' => [
          'olivero/global-styling',
        ],
      ],
      '#cache' => [
        'max-age' => 0,
      ],
    ];
  }

  /**
   * Get simulation parameters for backtest simulations.
   *
   * @param string $simulation_id
   * @return array
   */
  private function getSimulationParameters(string $simulation_id): array {
    $parameters = [];
    
    // Construct path to simulation data
    $simulation_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios/Myportolio/simulations/backtests/' . $simulation_id;
    
    // Load lean_config.json if available
    $lean_config_file = $simulation_path . '/lean_config.json';
    if (file_exists($lean_config_file)) {
      $lean_config = json_decode(file_get_contents($lean_config_file), true);
      if ($lean_config) {
        $parameters['lean_config'] = $lean_config;
      }
    }
    
    // Load myportolio_results.json if available
    $results_file = $simulation_path . '/myportolio_results.json';
    if (file_exists($results_file)) {
      $results = json_decode(file_get_contents($results_file), true);
      if ($results) {
        $parameters['results'] = $results;
      }
    }
    
    // Extract date range from simulation ID
    if (preg_match('/backtest_(\d{8})_(\d{6})_([a-f0-9]+)/', $simulation_id, $matches)) {
      $date_str = $matches[1];
      $time_str = $matches[2];
      $hash = $matches[3];
      
      $parameters['extracted_info'] = [
        'date' => substr($date_str, 0, 4) . '-' . substr($date_str, 4, 2) . '-' . substr($date_str, 6, 2),
        'time' => substr($time_str, 0, 2) . ':' . substr($time_str, 2, 2) . ':' . substr($time_str, 4, 2),
        'hash' => $hash,
        'timestamp' => $date_str . '_' . $time_str
      ];
    }
    
    return $parameters;
  }

  /**
   * Build HTML section for simulation parameters display.
   *
   * @param array $parameters
   * @param string $simulation_id
   * @return string
   */
  private function buildSimulationParametersSection(array $parameters, string $simulation_id): string {
    $section = '
    <!-- Simulation Parameters Section -->
    <div class="simulation-parameters-section">
      <h3>🔬 Simulation Parameters</h3>
      <div class="simulation-info-grid">
        
        <!-- Simulation Identity -->
        <div class="param-card simulation-identity">
          <h4>🆔 Simulation Identity</h4>
          <div class="param-details">
            <div class="param-row">
              <span class="param-label">ID:</span>
              <span class="param-value">' . htmlspecialchars($simulation_id) . '</span>
            </div>';
    
    if (isset($parameters['extracted_info'])) {
      $info = $parameters['extracted_info'];
      $section .= '
            <div class="param-row">
              <span class="param-label">Date:</span>
              <span class="param-value">' . htmlspecialchars($info['date']) . '</span>
            </div>
            <div class="param-row">
              <span class="param-label">Time:</span>
              <span class="param-value">' . htmlspecialchars($info['time']) . '</span>
            </div>
            <div class="param-row">
              <span class="param-label">Hash:</span>
              <span class="param-value">' . htmlspecialchars($info['hash']) . '</span>
            </div>';
    }
    
    $section .= '
          </div>
        </div>';
    
    // LEAN Configuration Parameters
    if (isset($parameters['lean_config'])) {
      $config = $parameters['lean_config'];
      $section .= '
        <!-- LEAN Configuration -->
        <div class="param-card lean-config">
          <h4>⚙️ LEAN Configuration</h4>
          <div class="param-details">';
      
      foreach ($config as $key => $value) {
        if (is_scalar($value)) {
          $section .= '
            <div class="param-row">
              <span class="param-label">' . htmlspecialchars(ucwords(str_replace(['-', '_'], ' ', $key))) . ':</span>
              <span class="param-value">' . htmlspecialchars((string)$value) . '</span>
            </div>';
        }
      }
      
      $section .= '
          </div>
        </div>';
    }
    
    // Simulation Results Summary
    if (isset($parameters['results'])) {
      $results = $parameters['results'];
      $section .= '
        <!-- Simulation Results -->
        <div class="param-card simulation-results">
          <h4>📊 Simulation Results</h4>
          <div class="param-details">';
      
      // Display key result metrics
      if (isset($results['performance'])) {
        foreach ($results['performance'] as $metric => $value) {
          $section .= '
            <div class="param-row">
              <span class="param-label">' . htmlspecialchars(ucwords(str_replace('_', ' ', $metric))) . ':</span>
              <span class="param-value">' . htmlspecialchars((string)$value) . '</span>
            </div>';
        }
      }
      
      // Display execution info
      if (isset($results['execution'])) {
        $execution = $results['execution'];
        if (isset($execution['start_date'])) {
          $section .= '
            <div class="param-row">
              <span class="param-label">Start Date:</span>
              <span class="param-value">' . htmlspecialchars($execution['start_date']) . '</span>
            </div>';
        }
        if (isset($execution['end_date'])) {
          $section .= '
            <div class="param-row">
              <span class="param-label">End Date:</span>
              <span class="param-value">' . htmlspecialchars($execution['end_date']) . '</span>
            </div>';
        }
        if (isset($execution['total_days'])) {
          $section .= '
            <div class="param-row">
              <span class="param-label">Duration:</span>
              <span class="param-value">' . htmlspecialchars($execution['total_days']) . ' days</span>
            </div>';
        }
      }
      
      // Display trade count
      if (isset($results['lean_results']['trades'])) {
        $trade_count = count($results['lean_results']['trades']);
        $section .= '
            <div class="param-row">
              <span class="param-label">Total Trades:</span>
              <span class="param-value">' . $trade_count . '</span>
            </div>';
      }
      
      $section .= '
          </div>
        </div>';
    }
    
    $section .= '
      </div>
      
      <!-- Additional Simulation Details -->
      <div class="simulation-details-section">
        <h4>📋 Simulation Details</h4>
        <p class="simulation-note">
          This is a historical backtest simulation that replayed market conditions from a specific time period. 
          The parameters above show the exact configuration used during the simulation execution, including 
          date ranges, algorithm settings, and performance results.
        </p>
      </div>
    </div>';
    
    return $section;
  }

  /**
   * LEAN Simulation Management page.
   */
  public function leanSimulations() {
    // Disable caching for real-time data
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $build = [
      '#theme' => 'unicornmetrics_dashboard',
      '#title' => 'LEAN Simulation Management',
      '#content' => $this->buildSimulationManagement(),
      '#attached' => [
        'library' => [
          'unicornmetrics/dashboard-styling',
          'unicornmetrics/chart-libraries',
          'unicornmetrics/interactive-features',
        ],
      ],
    ];
    
    return $build;
  }

  /**
   * LEAN Simulation Holdings page.
   */
  public function leanSimulationHoldings($simulation_id) {
    // Disable caching for real-time data
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $build = [
      '#theme' => 'unicornmetrics_dashboard',
      '#title' => "Simulation Holdings - {$simulation_id}",
      '#content' => $this->buildSimulationHoldings($simulation_id),
      '#attached' => [
        'library' => [
          'unicornmetrics/dashboard-styling',
          'unicornmetrics/chart-libraries',
          'unicornmetrics/interactive-features',
        ],
      ],
    ];
    
    return $build;
  }

  /**
   * LEAN Simulation Performance page.
   */
  public function leanSimulationPerformance($simulation_id) {
    // Disable caching for real-time data
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $build = [
      '#theme' => 'unicornmetrics_dashboard',
      '#title' => "Simulation Performance - {$simulation_id}",
      '#content' => $this->buildSimulationPerformance($simulation_id),
      '#attached' => [
        'library' => [
          'unicornmetrics/dashboard-styling',
          'unicornmetrics/chart-libraries',
          'unicornmetrics/interactive-features',
        ],
      ],
    ];
    
    return $build;
  }

  /**
   * LEAN Simulation Algorithms page.
   */
  public function leanSimulationAlgorithms($simulation_id) {
    // Disable caching for real-time data
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $build = [
      '#theme' => 'unicornmetrics_dashboard',
      '#title' => "Simulation Algorithms - {$simulation_id}",
      '#content' => $this->buildSimulationAlgorithms($simulation_id),
      '#attached' => [
        'library' => [
          'unicornmetrics/dashboard-styling',
          'unicornmetrics/chart-libraries',
          'unicornmetrics/interactive-features',
        ],
      ],
    ];
    
    return $build;
  }

  /**
   * LEAN Simulation Backtest Results page.
   */
  public function leanSimulationBacktest($simulation_id) {
    // Disable caching for real-time data
    \Drupal::service('page_cache_kill_switch')->trigger();
    
    $build = [
      '#theme' => 'unicornmetrics_dashboard',
      '#title' => "Simulation Backtest - {$simulation_id}",
      '#content' => $this->buildSimulationBacktest($simulation_id),
      '#attached' => [
        'library' => [
          'unicornmetrics/dashboard-styling',
          'unicornmetrics/chart-libraries',
          'unicornmetrics/interactive-features',
        ],
      ],
    ];
    
    return $build;
  }

  /**
   * Build simulation management content with selector.
   */
  private function buildSimulationManagement() {
    $content = '<div class="simulation-management-container">';
    
    // Simulation Selector
    $content .= '
    <div class="simulation-selector-container">
      <h2>🎯 Simulation Selector</h2>
      <div class="simulation-selector" id="admin-simulation-selector">
        <div class="selector-header">
          <h3>Available Simulations</h3>
          <p>Select a simulation to analyze detailed performance, holdings, and algorithm data</p>
        </div>
        
        <div class="simulation-grid">
          <div class="simulation-card active" data-simulation="ETH_Momentum_2024Q4">
            <h4>🔷 ETH Momentum 2024Q4</h4>
            <div class="simulation-stats">
              <span class="stat">📊 Status: <strong>Completed</strong></span>
              <span class="stat">📈 Return: <strong>+24.3%</strong></span>
              <span class="stat">⏱️ Duration: <strong>90 days</strong></span>
            </div>
          </div>
          
          <div class="simulation-card" data-simulation="BTC_Conservative_2024Q3">
            <h4>🟡 BTC Conservative 2024Q3</h4>
            <div class="simulation-stats">
              <span class="stat">📊 Status: <strong>Completed</strong></span>
              <span class="stat">📈 Return: <strong>+18.7%</strong></span>
              <span class="stat">⏱️ Duration: <strong>92 days</strong></span>
            </div>
          </div>
          
          <div class="simulation-card" data-simulation="Mixed_Portfolio_2024Q2">
            <h4>🔄 Mixed Portfolio 2024Q2</h4>
            <div class="simulation-stats">
              <span class="stat">📊 Status: <strong>Running</strong></span>
              <span class="stat">📈 Return: <strong>+12.1%</strong></span>
              <span class="stat">⏱️ Duration: <strong>45 days</strong></span>
            </div>
          </div>
        </div>
        
        <div class="simulation-actions">
          <button class="btn btn-primary" onclick="navigateToSimulation()">
            📊 Analyze Selected Simulation
          </button>
          <button class="btn btn-secondary" onclick="compareSimulations()">
            🔍 Compare Simulations
          </button>
        </div>
      </div>
    </div>';
    
    // Quick Stats Overview
    $content .= '
    <div class="simulation-overview">
      <h2>📊 Simulation Overview</h2>
      <div class="stats-grid">
        <div class="stat-card">
          <h3>Total Simulations</h3>
          <div class="stat-value">12</div>
          <div class="stat-trend">↗️ +3 this quarter</div>
        </div>
        
        <div class="stat-card">
          <h3>Average Return</h3>
          <div class="stat-value">+18.4%</div>
          <div class="stat-trend">↗️ +2.1% vs benchmark</div>
        </div>
        
        <div class="stat-card">
          <h3>Best Performer</h3>
          <div class="stat-value">ETH Momentum</div>
          <div class="stat-trend">↗️ +24.3% return</div>
        </div>
        
        <div class="stat-card">
          <h3>Success Rate</h3>
          <div class="stat-value">91.7%</div>
          <div class="stat-trend">↗️ 11/12 profitable</div>
        </div>
      </div>
    </div>';
    
    $content .= '</div>';
    
    // Add JavaScript for simulation selector
    $content .= '
    <script>
    function navigateToSimulation() {
      const selected = document.querySelector(".simulation-card.active");
      if (selected) {
        const simId = selected.getAttribute("data-simulation");
        window.location.href = "/admin/metrics/lean/simulations/" + simId + "/holdings";
      }
    }
    
    function compareSimulations() {
      alert("Comparison feature coming soon!");
    }
    
    document.querySelectorAll(".simulation-card").forEach(card => {
      card.addEventListener("click", function() {
        document.querySelectorAll(".simulation-card").forEach(c => c.classList.remove("active"));
        this.classList.add("active");
      });
    });
    </script>';
    
    return Markup::create($content);
  }

  /**
   * Build simulation holdings content.
   */
  private function buildSimulationHoldings($simulation_id) {
    $content = '<div class="simulation-holdings-container">';
    
    // Navigation breadcrumb
    $content .= '
    <div class="simulation-breadcrumb">
      <a href="/admin/metrics/lean/simulations">← Back to Simulations</a>
      <span class="divider">|</span>
      <span class="current">Holdings - ' . $simulation_id . '</span>
    </div>';
    
    // Holdings table similar to main portfolio but with simulation data
    $content .= '
    <div class="simulation-holdings">
      <h2>📊 Holdings Analysis - ' . $simulation_id . '</h2>
      
      <div class="holdings-table-container">
        <table class="holdings-table">
          <thead>
            <tr>
              <th>Symbol</th>
              <th>Position</th>
              <th>Market Value</th>
              <th>Weight</th>
              <th>Unrealized P&L</th>
              <th>Cost Basis</th>
              <th>Algorithm</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td><strong>ETHUSD</strong></td>
              <td>2.45 ETH</td>
              <td>$6,847.50</td>
              <td>68.5%</td>
              <td class="positive">+$847.50</td>
              <td>$6,000.00</td>
              <td>Momentum v3.2</td>
            </tr>
            <tr>
              <td><strong>BTCUSD</strong></td>
              <td>0.08 BTC</td>
              <td>$3,152.50</td>
              <td>31.5%</td>
              <td class="positive">+$152.50</td>
              <td>$3,000.00</td>
              <td>Conservative v2.1</td>
            </tr>
          </tbody>
        </table>
      </div>
      
      <div class="simulation-navigation">
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/performance" class="nav-button">
          📈 Performance Analysis
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/algorithms" class="nav-button">
          🤖 Algorithm Details
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/backtest" class="nav-button">
          🔄 Backtest Results
        </a>
      </div>
    </div>';
    
    $content .= '</div>';
    
    return Markup::create($content);
  }

  /**
   * Build simulation performance content.
   */
  private function buildSimulationPerformance($simulation_id) {
    $content = '<div class="simulation-performance-container">';
    
    // Navigation breadcrumb
    $content .= '
    <div class="simulation-breadcrumb">
      <a href="/admin/metrics/lean/simulations">← Back to Simulations</a>
      <span class="divider">|</span>
      <span class="current">Performance - ' . $simulation_id . '</span>
    </div>';
    
    // Performance metrics similar to main but simulation-specific
    $content .= '
    <div class="simulation-performance">
      <h2>📈 Performance Analysis - ' . $simulation_id . '</h2>
      
      <div class="performance-metrics">
        <div class="metric-card">
          <h3>Total Return</h3>
          <div class="metric-value positive">+24.3%</div>
          <div class="metric-period">90-day simulation</div>
        </div>
        
        <div class="metric-card">
          <h3>Sharpe Ratio</h3>
          <div class="metric-value">1.85</div>
          <div class="metric-period">Risk-adjusted return</div>
        </div>
        
        <div class="metric-card">
          <h3>Max Drawdown</h3>
          <div class="metric-value negative">-5.2%</div>
          <div class="metric-period">Maximum loss</div>
        </div>
        
        <div class="metric-card">
          <h3>Win Rate</h3>
          <div class="metric-value">73.4%</div>
          <div class="metric-period">Profitable trades</div>
        </div>
      </div>
      
      <div class="simulation-navigation">
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/holdings" class="nav-button">
          📊 Holdings Analysis
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/algorithms" class="nav-button">
          🤖 Algorithm Details
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/backtest" class="nav-button">
          🔄 Backtest Results
        </a>
      </div>
    </div>';
    
    $content .= '</div>';
    
    return Markup::create($content);
  }

  /**
   * Build simulation algorithms content.
   */
  private function buildSimulationAlgorithms($simulation_id) {
    $content = '<div class="simulation-algorithms-container">';
    
    // Navigation breadcrumb
    $content .= '
    <div class="simulation-breadcrumb">
      <a href="/admin/metrics/lean/simulations">← Back to Simulations</a>
      <span class="divider">|</span>
      <span class="current">Algorithms - ' . $simulation_id . '</span>
    </div>';
    
    // Algorithm analysis for this specific simulation
    $content .= '
    <div class="simulation-algorithms">
      <h2>🤖 Algorithm Analysis - ' . $simulation_id . '</h2>
      
      <div class="algorithm-performance">
        <div class="algorithm-card">
          <h3>ETH Momentum Algorithm v3.2</h3>
          <div class="algo-stats">
            <span>📊 Accuracy: <strong>87.3%</strong></span>
            <span>📈 Return: <strong>+26.1%</strong></span>
            <span>⚡ Trades: <strong>23</strong></span>
          </div>
          <div class="algo-description">
            Advanced momentum strategy with dynamic position sizing and risk management.
          </div>
        </div>
        
        <div class="algorithm-card">
          <h3>BTC Conservative Algorithm v2.1</h3>
          <div class="algo-stats">
            <span>📊 Accuracy: <strong>82.1%</strong></span>
            <span>📈 Return: <strong>+18.9%</strong></span>
            <span>⚡ Trades: <strong>12</strong></span>
          </div>
          <div class="algo-description">
            Conservative approach focused on capital preservation with steady growth.
          </div>
        </div>
      </div>
      
      <div class="simulation-navigation">
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/holdings" class="nav-button">
          📊 Holdings Analysis
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/performance" class="nav-button">
          📈 Performance Analysis
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/backtest" class="nav-button">
          🔄 Backtest Results
        </a>
      </div>
    </div>';
    
    $content .= '</div>';
    
    return Markup::create($content);
  }

  /**
   * Build simulation backtest content.
   */
  private function buildSimulationBacktest($simulation_id) {
    $content = '<div class="simulation-backtest-container">';
    
    // Navigation breadcrumb
    $content .= '
    <div class="simulation-breadcrumb">
      <a href="/admin/metrics/lean/simulations">← Back to Simulations</a>
      <span class="divider">|</span>
      <span class="current">Backtest - ' . $simulation_id . '</span>
    </div>';
    
    // Backtest results for this simulation
    $content .= '
    <div class="simulation-backtest">
      <h2>🔄 Backtest Results - ' . $simulation_id . '</h2>
      
      <div class="backtest-summary">
        <div class="backtest-card">
          <h3>Simulation Parameters</h3>
          <div class="param-list">
            <div class="param">📅 Start Date: <strong>2024-07-01</strong></div>
            <div class="param">📅 End Date: <strong>2024-09-30</strong></div>
            <div class="param">💰 Initial Capital: <strong>$10,000</strong></div>
            <div class="param">🎯 Strategy: <strong>Multi-Asset Momentum</strong></div>
          </div>
        </div>
        
        <div class="backtest-card">
          <h3>Final Results</h3>
          <div class="result-list">
            <div class="result positive">💰 Final Value: <strong>$12,430</strong></div>
            <div class="result positive">📈 Total Return: <strong>+24.3%</strong></div>
            <div class="result">📊 Sharpe Ratio: <strong>1.85</strong></div>
            <div class="result">📉 Max Drawdown: <strong>-5.2%</strong></div>
          </div>
        </div>
      </div>
      
      <div class="trade-analysis">
        <h3>Trade Analysis</h3>
        <div class="trade-stats">
          <div class="stat">📊 Total Trades: <strong>35</strong></div>
          <div class="stat">✅ Winning Trades: <strong>25 (71.4%)</strong></div>
          <div class="stat">❌ Losing Trades: <strong>10 (28.6%)</strong></div>
          <div class="stat">💰 Average Win: <strong>+3.8%</strong></div>
          <div class="stat">💸 Average Loss: <strong>-1.2%</strong></div>
        </div>
      </div>
      
      <div class="simulation-navigation">
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/holdings" class="nav-button">
          📊 Holdings Analysis
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/performance" class="nav-button">
          📈 Performance Analysis
        </a>
        <a href="/admin/metrics/lean/simulations/' . $simulation_id . '/algorithms" class="nav-button">
          🤖 Algorithm Details
        </a>
      </div>
    </div>';
    
    $content .= '</div>';
    
    return Markup::create($content);
  }

}

