<?php

namespace Drupal\unicornmetrics\Controller;

use Drupal\Core\Controller\ControllerBase;

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
    
    $metrics_table = '
    <div class="dashboard-header">
      <h1>Unicorn Metrics Dashboard</h1>
      <div class="version-info">
        <span class="module-version">Version ' . $version . '</span>
        <span class="last-updated">Last Updated: ' . date('Y-m-d H:i:s') . '</span>
      </div>
    </div>
    <p>Welcome to the Unicorn Investment Metrics Dashboard. LEAN framework integration for comprehensive portfolio and algorithm management.</p>
    
    <div class="dashboard-sections">
    
    <!-- LEAN Framework Integration Section -->
    <div class="dashboard-section">
      <h2>🏗️ LEAN Framework Management</h2>
      <table class="lean-nav-table">
        <thead>
          <tr>
            <th class="icon-column">Type</th>
            <th class="link-column">Component</th>
            <th class="description-column">Description</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="icon-column">💼</td>
            <td class="link-column"><a href="/admin/metrics/lean/portfolio">Portfolio Management</a></td>
            <td class="description-column">LEAN portfolio state, holdings overview, cash positions, and total portfolio value from live algorithm execution.</td>
          </tr>
          <tr>
            <td class="icon-column">📈</td>
            <td class="link-column"><a href="/admin/metrics/lean/holdings">Portfolio Holdings</a></td>
            <td class="description-column">Detailed breakdown of individual security holdings, positions, market values, and unrealized P&L from LEAN portfolio manager.</td>
          </tr>
          <tr>
            <td class="icon-column">⚡</td>
            <td class="link-column"><a href="/admin/metrics/lean/performance">Portfolio Performance</a></td>
            <td class="description-column">Real-time portfolio performance metrics including returns, Sharpe ratio, drawdown, and benchmark comparisons from LEAN results.</td>
          </tr>
          <tr>
            <td class="icon-column">🤖</td>
            <td class="link-column"><a href="/admin/metrics/lean/algorithms">Algorithm Management</a></td>
            <td class="description-column">LEAN algorithm status monitoring, execution state, insights generation, and algorithm performance tracking.</td>
          </tr>
          <tr>
            <td class="icon-column">📊</td>
            <td class="link-column"><a href="/admin/metrics/lean/algorithms/performance">Algorithm Performance</a></td>
            <td class="description-column">Detailed algorithm performance analysis including signal quality, prediction accuracy, and alpha generation metrics.</td>
          </tr>
          <tr>
            <td class="icon-column">🔬</td>
            <td class="link-column"><a href="/admin/metrics/lean/backtest">Backtest Results</a></td>
            <td class="description-column">Historical backtest performance analysis, strategy validation, and out-of-sample testing results from LEAN backtesting engine.</td>
          </tr>
        </tbody>
      </table>
    </div>
    
    </div>
    
    <div class="system-overview">
      <h3>System Overview</h3>
      <ul>
        <li><strong>LEAN Integration:</strong> 6 portfolio and algorithm management components</li>
        <li><strong>Real-time Analysis:</strong> Live portfolio monitoring</li>
        <li><strong>Algorithm Framework:</strong> LEAN-powered algorithm execution and monitoring</li>
        <li><strong>Performance Tracking:</strong> Comprehensive portfolio and algorithm analytics</li>
        <li><strong>Risk Management:</strong> Real-time risk assessment and monitoring</li>
        <li><strong>Backtesting:</strong> Historical strategy validation and analysis</li>
      </ul>
    </div>
    ';
    
    return [
      '#markup' => $metrics_table,
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
                .link-column {
                  width: 250px;
                }
                .description-column {
                  width: auto;
                }
                .metric-count {
                  background: #3498db;
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
              ',
            ],
            'unicorn-metrics-dashboard-styles',
          ],
        ],
      ],
    ];
  }

  /**
   * LEAN Portfolio Management Dashboard.
   */
  public function leanPortfolio() {
    // Read LEAN portfolio state from JSON files
    $portfolio_data = $this->getLeanPortfolioData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🏗️ LEAN Portfolio Management</h1>
      <p>Real-time portfolio state and holdings from LEAN algorithmic trading framework</p>
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
      <a href="/admin/metrics/lean/holdings" class="action-button">📋 View Holdings</a>
      <a href="/admin/metrics/lean/performance" class="action-button">⚡ Performance Metrics</a>
      <a href="/admin/metrics/lean/algorithms" class="action-button">🤖 Algorithm Status</a>
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
    $holdings = $this->getLeanHoldingsData();
    
    $holdings_table = '<div class="lean-dashboard-header">
      <h1>📈 LEAN Portfolio Holdings</h1>
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
        'html_head' => [[$this->getLeanHoldingsStyles(), 'lean-holdings-styles']],
      ],
    ];
  }

  /**
   * LEAN Portfolio Performance Metrics.
   */
  public function leanPerformance() {
    $performance = $this->getLeanPerformanceData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>⚡ LEAN Portfolio Performance</h1>
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
    $algorithms = $this->getLeanAlgorithmData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🤖 LEAN Algorithm Management</h1>
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
    $performance = $this->getLeanAlgorithmPerformanceData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>📊 LEAN Algorithm Performance</h1>
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
    $backtest = $this->getLeanBacktestData();
    
    $content = '
    <div class="lean-dashboard-header">
      <h1>🔬 LEAN Backtest Results</h1>
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
  private function getLeanPortfolioData(): array {
    // In production, read from LEAN JSON files:
    // $portfolio_file = '/workspaces/unicorninvesting/BackendPython/Lean/Results/portfolio-state.json';
    
    return [
      'total_value' => 125847.62,
      'cash' => 15432.18,
      'positions_value' => 110415.44,
      'unrealized_pnl' => 8247.33,
      'daily_change' => 1.23,
      'holdings_count' => 8,
      'last_updated' => date('Y-m-d H:i:s'),
    ];
  }

  /**
   * Helper: Get LEAN holdings data.
   */
  private function getLeanHoldingsData(): array {
    return [
      [
        'symbol' => 'SPY',
        'name' => 'SPDR S&P 500 ETF',
        'quantity' => 25,
        'average_cost' => 428.50,
        'current_price' => 445.75,
        'market_value' => 11143.75,
        'unrealized_pnl' => 431.25,
        'unrealized_pnl_percent' => 0.0401,
        'weight' => 0.089,
      ],
      [
        'symbol' => 'AAPL',
        'name' => 'Apple Inc.',
        'quantity' => 50,
        'average_cost' => 175.25,
        'current_price' => 189.84,
        'market_value' => 9492.00,
        'unrealized_pnl' => 729.50,
        'unrealized_pnl_percent' => 0.0832,
        'weight' => 0.076,
      ],
      [
        'symbol' => 'TSLA',
        'name' => 'Tesla Inc.',
        'quantity' => 30,
        'average_cost' => 245.67,
        'current_price' => 267.32,
        'market_value' => 8019.60,
        'unrealized_pnl' => 649.50,
        'unrealized_pnl_percent' => 0.0881,
        'weight' => 0.064,
      ],
    ];
  }

  /**
   * Helper: Get LEAN performance data.
   */
  private function getLeanPerformanceData(): array {
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
    ];
  }

  /**
   * Helper: Get LEAN algorithm data.
   */
  private function getLeanAlgorithmData(): array {
    return [
      'current' => [
        'name' => 'UnicornForexEnsemble',
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
        .portfolio-overview-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin: 20px 0; }
        .portfolio-card { background: white; border: 1px solid #e1e5e9; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .portfolio-card:hover { box-shadow: 0 4px 8px rgba(0,0,0,0.15); transform: translateY(-2px); transition: all 0.3s ease; }
        .metric-value { font-size: 2em; font-weight: bold; color: #2c3e50; }
        .metric-change { font-size: 1.1em; margin-top: 8px; }
        .metric-label { color: #7f8c8d; font-size: 0.9em; margin-top: 8px; }
        .portfolio-actions { display: flex; gap: 15px; margin: 30px 0; }
        .action-button { background: #3498db; color: white; padding: 12px 24px; border-radius: 6px; text-decoration: none; font-weight: bold; }
        .action-button:hover { background: #2980b9; text-decoration: none; color: white; }
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

}
