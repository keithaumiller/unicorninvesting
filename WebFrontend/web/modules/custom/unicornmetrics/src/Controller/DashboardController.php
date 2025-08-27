<?php

namespace Drupal\unicornmetrics\Controller;

use Drupal\Core\Controller\ControllerBase;
use Drupal\Core\Render\Markup;

/**
 * Controller for Unicorn Metrics dashboard pages.
 */
class DashboardController extends ControllerBase {

  /**
   * Main dashboard page.
   */
  public function dashboard() {
    $module_info = \Drupal::service('extension.list.module')->getExtensionInfo('unicornmetrics');
    $version = $module_info['version'] ?? '1.0.0';
    
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $metrics_table = '
    <div class="dashboard-header">
      <h1>🦄 Unicorn Portfolio Management System</h1>
      <div class="version-info">
        <span class="module-version">Version ' . $version . '</span>
        <span class="last-updated">Last Updated: ' . date('Y-m-d H:i:s') . '</span>
      </div>
    </div>
    
    ' . $this->renderNavigationMenu($current_portfolio_id) . '
    
    <div class="dashboard-sections">
    
    <!-- Primary Portfolio Section -->
    <div class="dashboard-section">
      <h2>💼 ' . $current_portfolio['name'] . '</h2>
      <p><strong>' . $current_portfolio['description'] . '</strong> - Portfolio managed by ' . $current_portfolio['algorithm'] . ' algorithm in ' . $current_portfolio['environment'] . ' mode.</p>
      
      <div class="portfolio-stats">
        <div class="stat-card">
          <span class="stat-value">$' . number_format($current_portfolio['total_value'], 2) . '</span>
          <span class="stat-label">Total Value</span>
        </div>
        <div class="stat-card">
          <span class="stat-value">' . $current_portfolio['positions'] . '</span>
          <span class="stat-label">Active Positions</span>
        </div>
        <div class="stat-card">
          <span class="stat-value">' . $current_portfolio['daily_pnl'] . '</span>
          <span class="stat-label">Daily P&L</span>
        </div>
        <div class="stat-card ' . ($current_portfolio['status'] == 'active' ? 'status-active' : 'status-inactive') . '">
          <span class="stat-value">⚡</span>
          <span class="stat-label">' . ucfirst($current_portfolio['status']) . '</span>
        </div>
      </div>
      
      <table class="lean-nav-table">
        <thead>
          <tr>
            <th class="icon-column">Hierarchy</th>
            <th class="link-column">Portfolio Component</th>
            <th class="description-column">Description</th>
            <th class="count-column">Count</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="icon-column">💼</td>
            <td class="link-column"><a href="/admin/metrics/lean/portfolio?portfolio=' . $current_portfolio_id . '"><strong>📊 Portfolio Overview</strong></a></td>
            <td class="description-column"><strong>Main Portfolio Dashboard:</strong> Total value, cash positions, and portfolio-level metrics from LEAN SecurityPortfolioManager.</td>
            <td class="count-column"><span class="metric-count">1</span></td>
          </tr>
          <tr>
            <td class="icon-column">├─ 🏷️</td>
            <td class="link-column"><a href="/admin/metrics/lean/holdings?portfolio=' . $current_portfolio_id . '">📈 Securities & Holdings</a></td>
            <td class="description-column"><em>Portfolio Component:</em> Individual security positions, market values, and unrealized P&L within the portfolio.</td>
            <td class="count-column"><span class="metric-count">' . $current_portfolio['positions'] . '</span></td>
          </tr>
          <tr>
            <td class="icon-column">├─ ⚡</td>
            <td class="link-column"><a href="/admin/metrics/lean/performance?portfolio=' . $current_portfolio_id . '">📊 Performance Metrics</a></td>
            <td class="description-column"><em>Portfolio Analytics:</em> Returns, Sharpe ratio, drawdown, and risk metrics for overall portfolio performance.</td>
            <td class="count-column"><span class="metric-count">12</span></td>
          </tr>
          <tr>
            <td class="icon-column">└─ �</td>
            <td class="link-column"><a href="/admin/metrics/lean/algorithms?portfolio=' . $current_portfolio_id . '">🤖 Managing Algorithm</a></td>
            <td class="description-column"><em>Portfolio Strategy:</em> ' . $current_portfolio['algorithm'] . ' algorithm that executes trades and manages this portfolio.</td>
            <td class="count-column"><span class="metric-count">1</span></td>
          </tr>
        </tbody>
      </table>
    </div>
    
    <!-- Algorithm Tools Section -->
    <div class="dashboard-section">
      <h2>🔧 Algorithm Management Tools</h2>
      <p>Strategy analysis and backtesting tools for the algorithms managing your portfolio.</p>
      
      <table class="lean-nav-table">
        <thead>
          <tr>
            <th class="icon-column">Tool</th>
            <th class="link-column">Algorithm Analysis</th>
            <th class="description-column">Description</th>
            <th class="count-column">Metrics</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="icon-column">📊</td>
            <td class="link-column"><a href="/admin/metrics/lean/algorithms/performance?portfolio=' . $current_portfolio_id . '">🎯 Algorithm Performance</a></td>
            <td class="description-column">Signal accuracy, financial impact, and alpha generation analysis for portfolio management strategies.</td>
            <td class="count-column"><span class="metric-count">6</span></td>
          </tr>
          <tr>
            <td class="icon-column">🔬</td>
            <td class="link-column"><a href="/admin/metrics/lean/backtest?portfolio=' . $current_portfolio_id . '">📈 Backtest Results</a></td>
            <td class="description-column">Historical strategy validation and out-of-sample testing for algorithm performance verification.</td>
            <td class="count-column"><span class="metric-count">3</span></td>
          </tr>
        </tbody>
      </table>
    </div>
    
    </div>
    
    <div class="system-overview">
      <h3>🏗️ LEAN Multi-Portfolio Architecture</h3>
      <ul>
        <li><strong>Selected Portfolio:</strong> ' . $current_portfolio['name'] . ' ($' . number_format($current_portfolio['total_value'], 2) . ')</li>
        <li><strong>Portfolio Components:</strong> ' . $current_portfolio['positions'] . ' active securities and holdings within this portfolio</li>
        <li><strong>Algorithm Strategy:</strong> ' . $current_portfolio['algorithm'] . ' managing portfolio trades and positions</li>
        <li><strong>Trading Environment:</strong> ' . ucfirst($current_portfolio['environment']) . ' trading mode with real-time monitoring</li>
        <li><strong>Asset Classes:</strong> ' . implode(', ', $current_portfolio['symbols']) . '</li>
        <li><strong>Multi-Portfolio Management:</strong> 4 total portfolios with $525,847.62 in total assets</li>
        <li><strong>Performance Analytics:</strong> Comprehensive portfolio and algorithm analysis tools</li>
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
                .status-inactive .stat-value {
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
              ',
            ],
            'unicorn-metrics-dashboard-styles',
          ],
          [
            [
              '#tag' => 'script',
              '#value' => '
                <script>
                function switchPortfolio(portfolioId) {
                  window.location.href = "/admin/metrics?portfolio=" + portfolioId;
                }
              </script>
              ',
            ],
            'unicorn-portfolio-switcher-js',
          ],
        ],
      ],
    ];
  }

  /**
   * LEAN Portfolio Management Dashboard.
   */
  public function leanPortfolio() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    // Read LEAN portfolio state from JSON files based on selected portfolio
    $portfolio_data = $this->getLeanPortfolioData($current_portfolio_id);
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>💼 ' . htmlspecialchars($current_portfolio['name']) . '</h1>
      <p>Your main investment portfolio - the central container managed by LEAN SecurityPortfolioManager</p>
      <div class="portfolio-hierarchy">
        <span class="hierarchy-note">📊 Portfolio Overview → Securities & Holdings → Algorithm Strategy</span>
      </div>
    </div>
    
    ' . $this->renderNavigationMenu($current_portfolio_id, 'portfolio') . '
      </div>
    </div>
    
    <div class="portfolio-overview-grid">
      <div class="portfolio-card">
        <h3>💰 Portfolio Value</h3>
        <div class="metric-value">$' . number_format($portfolio_data['total_value'], 2) . '</div>
        <div class="metric-change ' . ($portfolio_data['daily_change'] >= 0 ? 'positive' : 'negative') . '">
          ' . ($portfolio_data['daily_change'] >= 0 ? '+' : '') . number_format($portfolio_data['daily_change'], 2) . '% Today
        </div>
      </div>
      
      <div class="portfolio-card">
        <h3>💵 Cash Position</h3>
        <div class="metric-value">$' . number_format($portfolio_data['cash'], 2) . '</div>
        <div class="metric-label">' . number_format(($portfolio_data['cash'] / $portfolio_data['total_value']) * 100, 1) . '% of Portfolio</div>
      </div>
      
      <div class="portfolio-card">
        <h3>📊 Positions Value</h3>
        <div class="metric-value">$' . number_format($portfolio_data['positions_value'], 2) . '</div>
        <div class="metric-label">' . $portfolio_data['holdings_count'] . ' Active Holdings</div>
      </div>
      
      <div class="portfolio-card">
        <h3>📈 Unrealized P&L</h3>
        <div class="metric-value ' . ($portfolio_data['unrealized_pnl'] >= 0 ? 'positive' : 'negative') . '">
          ' . ($portfolio_data['unrealized_pnl'] >= 0 ? '+' : '') . '$' . number_format($portfolio_data['unrealized_pnl'], 2) . '
        </div>
        <div class="metric-label">' . number_format(($portfolio_data['unrealized_pnl'] / $portfolio_data['total_value']) * 100, 2) . '% of Portfolio</div>
      </div>
    </div>
    
    <div class="portfolio-actions">
      <a href="/admin/metrics/lean/holdings?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">� View Securities & Holdings</a>
      <a href="/admin/metrics/lean/performance?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">⚡ Portfolio Performance</a>
      <a href="/admin/metrics/lean/algorithms?portfolio=' . urlencode($current_portfolio_id) . '" class="action-button">� Managing Algorithm</a>
    </div>
    
    <div class="portfolio-hierarchy-info">
      <h3>📊 Portfolio Hierarchy</h3>
      <ul>
        <li><strong>Portfolio:</strong> Main container with $125,847.62 total value</li>
        <li>├─ <strong>Securities:</strong> 8 active holdings across multiple asset classes</li>
        <li>├─ <strong>Cash:</strong> $15,432.18 in cash positions (12.3% allocation)</li>
        <li>└─ <strong>Algorithm:</strong> UnicornForexEnsemble strategy managing trades</li>
      </ul>
    </div>
    
    <div class="last-updated">
      Last Updated: ' . $portfolio_data['last_updated'] . ' | Data Source: LEAN Framework
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [[$this->getLeanPortfolioStyles(), 'lean-portfolio-styles']],
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
    </div>
    
    ' . $this->renderNavigationMenu($current_portfolio_id, 'holdings') . '';
    
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
        'html_head' => [[$this->getLeanHoldingsStyles(), 'lean-holdings-styles']],
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
    
    $navigation = $this->renderNavigationMenu($current_portfolio_id, 'performance');
    
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
        'html_head' => [[$this->getLeanPerformanceStyles(), 'lean-performance-styles']],
      ],
    ];
  }

  /**
   * LEAN Algorithm Management Dashboard.
   */
  public function leanAlgorithms() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $algorithms = $this->getLeanAlgorithmData($current_portfolio_id);
    
    $navigation = $this->renderNavigationMenu($current_portfolio_id, 'algorithms');
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🤖 ' . htmlspecialchars($current_portfolio['name']) . ' Algorithm</h1>
      <p>Algorithm execution status and performance monitoring</p>
    </div>
    
    <div class="algorithms-grid">
      <div class="algorithm-card">
        <h3>📊 Current Algorithm</h3>
        <div class="algorithm-info">
          <div class="algorithm-name">' . $algorithms['current']['name'] . '</div>
          <div class="algorithm-status ' . strtolower($algorithms['current']['status']) . '">' . $algorithms['current']['status'] . '</div>
          <div class="algorithm-runtime">Runtime: ' . $algorithms['current']['runtime'] . '</div>
        </div>
      </div>
      
      <div class="algorithm-card">
        <h3>⚡ Performance</h3>
        <div class="algorithm-metrics">
          <div>Signals Generated: ' . $algorithms['current']['signals_generated'] . '</div>
          <div>Signal Accuracy: ' . number_format($algorithms['current']['signal_accuracy'] * 100, 1) . '%</div>
          <div>Alpha Score: ' . number_format($algorithms['current']['alpha_score'], 2) . '</div>
        </div>
      </div>
      
      <div class="algorithm-card">
        <h3>📈 Insights</h3>
        <div class="insights-summary">
          <div>Total Insights: ' . $algorithms['insights']['total'] . '</div>
          <div>Direction Accuracy: ' . number_format($algorithms['insights']['direction_accuracy'] * 100, 1) . '%</div>
          <div>Magnitude Accuracy: ' . number_format($algorithms['insights']['magnitude_accuracy'] * 100, 1) . '%</div>
        </div>
      </div>
    </div>
    
    <div class="algorithm-actions">
      <a href="/admin/metrics/lean/algorithms/performance" class="action-button">📊 Performance Analysis</a>
      <a href="/admin/metrics/lean/backtest" class="action-button">🔬 Backtest Results</a>
      <a href="/admin/metrics" class="action-button">🏠 Dashboard Home</a>
    </div>
    ';
    
    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [[$this->getLeanAlgorithmStyles(), 'lean-algorithm-styles']],
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
    
    $navigation = $this->renderNavigationMenu($current_portfolio_id, 'algorithm-performance');
    
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
        'html_head' => [[$this->getLeanAlgorithmPerformanceStyles(), 'lean-algorithm-performance-styles']],
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
    
    $navigation = $this->renderNavigationMenu($current_portfolio_id, 'backtest');
    
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
        'html_head' => [[$this->getLeanBacktestStyles(), 'lean-backtest-styles']],
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
        .algorithms-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin: 20px 0; }
        .algorithm-card { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; }
        .algorithm-name { font-size: 1.3em; font-weight: bold; color: #2c3e50; }
        .algorithm-status { padding: 6px 12px; border-radius: 20px; font-size: 0.9em; font-weight: bold; margin: 10px 0; display: inline-block; }
        .algorithm-status.running { background: #d4edda; color: #155724; }
        .algorithm-runtime { color: #6c757d; font-size: 0.9em; }
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
   * Render unified navigation and portfolio selector with normalized appearance.
   */
  private function renderNavigationMenu($current_portfolio_id, $current_page = 'dashboard') {
    // Get portfolio selector HTML
    $portfolio_selector = $this->renderPortfolioSelectorInternal($current_portfolio_id);
    
    $pages = [
      'dashboard' => ['url' => '/admin/metrics', 'icon' => '🏠', 'label' => 'Portfolio Dashboard'],
      'portfolio' => ['url' => '/admin/metrics/lean/portfolio', 'icon' => '💼', 'label' => 'Portfolio Overview'],
      'holdings' => ['url' => '/admin/metrics/lean/holdings', 'icon' => '📈', 'label' => 'Securities & Holdings'],
      'performance' => ['url' => '/admin/metrics/lean/performance', 'icon' => '⚡', 'label' => 'Performance Metrics'],
      'algorithms' => ['url' => '/admin/metrics/lean/algorithms', 'icon' => '🤖', 'label' => 'Algorithm Management'],
      'algorithm-performance' => ['url' => '/admin/metrics/lean/algorithms/performance', 'icon' => '🎯', 'label' => 'Algorithm Performance'],
      'backtest' => ['url' => '/admin/metrics/lean/backtest', 'icon' => '🔬', 'label' => 'Backtest Results']
    ];
    
    $nav_items = '';
    foreach ($pages as $page_key => $page_info) {
      $active_class = ($page_key == $current_page) ? ' current' : '';
      $nav_items .= '
          <a href="' . $page_info['url'] . '?portfolio=' . urlencode($current_portfolio_id) . '" class="nav-item' . $active_class . '">
            <span class="nav-icon">' . $page_info['icon'] . '</span>
            <span class="nav-label">' . $page_info['label'] . '</span>
          </a>';
    }
    
    return '
    <div class="unicorn-unified-navigation">
      ' . $portfolio_selector . '
      <div class="nav-container">
        <h3>🧭 Navigation Menu</h3>
        <div class="nav-menu">' . $nav_items . '
        </div>
      </div>
    </div>';
  }

  /**
   * Get available portfolios for selection.
   */
  private function getAvailablePortfolios() {
    return [
      'forex' => [
        'id' => 'forex',
        'name' => 'Primary Forex Portfolio',
        'value' => 125847.62,
      ],
      'equity' => [
        'id' => 'equity',
        'name' => 'Growth Equity Portfolio',
        'value' => 250000.00,
      ]
    ];
  }

  /**
   * Internal method for rendering portfolio selector without container.
   */
  private function renderPortfolioSelectorInternal($current_portfolio_id) {
    $portfolios = $this->getAvailablePortfolios();
    $options = '';
    
    foreach ($portfolios as $id => $portfolio) {
      $selected = ($id === $current_portfolio_id) ? ' selected' : '';
      $options .= '<option value="' . htmlspecialchars($id) . '"' . $selected . '>' . 
                  htmlspecialchars($portfolio['name']) . ' - $' . number_format($portfolio['value'], 2) . '</option>';
    }
    
    return '
    <div class="portfolio-selector-unified">
      <h3>📁 Portfolio Selection</h3>
      <div class="selector-wrapper">
        <select id="portfolio-dropdown" onchange="switchPortfolio(this.value)" class="portfolio-select">
          ' . $options . '
        </select>
        <div class="portfolio-stats">
          <span class="total-assets">Total Assets: $375,847.62</span>
          <span class="active-count">2 Active Portfolios</span>
        </div>
      </div>
    </div>';
  }

}
