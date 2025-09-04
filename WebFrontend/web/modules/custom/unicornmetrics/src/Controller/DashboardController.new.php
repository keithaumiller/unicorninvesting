<?php

namespace Drupal\unicornmetrics\Controller;

use Drupal\Core\Controller\ControllerBase;
use Drupal\Core\Extension\ModuleExtensionList;
use Drupal\Core\Render\Markup;
use Symfony\Component\DependencyInjection\ContainerInterface;
use Symfony\Component\HttpFoundation\RequestStack;

/**
 * Controller for Unicorn Metrics dashboard pages.
 *
 * Provides comprehensive portfolio management and metrics display for LEAN
 * algorithmic trading framework integration.
 */
class DashboardController extends ControllerBase {

  /**
   * The module extension list service.
   *
   * @var \Drupal\Core\Extension\ModuleExtensionList
   */
  protected $moduleExtensionList;

  /**
   * The request stack service.
   *
   * @var \Symfony\Component\HttpFoundation\RequestStack
   */
  protected $requestStack;

  /**
   * Constructs a DashboardController object.
   *
   * @param \Drupal\Core\Extension\ModuleExtensionList $module_extension_list
   *   The module extension list service.
   * @param \Symfony\Component\HttpFoundation\RequestStack $request_stack
   *   The request stack service.
   */
  public function __construct(ModuleExtensionList $module_extension_list, RequestStack $request_stack) {
    $this->moduleExtensionList = $module_extension_list;
    $this->requestStack = $request_stack;
  }

  /**
   * {@inheritdoc}
   */
  public static function create(ContainerInterface $container) {
    return new static(
      $container->get('extension.list.module'),
      $container->get('request_stack')
    );
  }

  /**
   * Main dashboard page.
   *
   * Displays the primary portfolio management interface with metrics,
   * portfolio selector, and comprehensive analytics.
   *
   * @return array
   *   A render array for the dashboard page.
   */
  public function dashboard() {
    $module_info = $this->moduleExtensionList->getExtensionInfo('unicornmetrics');
    $version = $module_info['version'] ?? '4.0.0';
    
    // Get current portfolio selection from URL parameter or default.
    $current_request = $this->requestStack->getCurrentRequest();
    $current_portfolio_id = $current_request->query->get('portfolio', 'forex');
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $portfolio_selector = $this->renderPortfolioSelector($current_portfolio_id);
    
    $build = [
      '#theme' => 'unicornmetrics_dashboard',
      '#portfolio_data' => $current_portfolio,
      '#metrics' => $this->getPortfolioMetrics($current_portfolio),
      '#portfolio_selector' => $portfolio_selector,
      '#attached' => [
        'library' => [
          'unicornmetrics/dashboard',
        ],
      ],
    ];

    return $build;
  }

  /**
   * LEAN Portfolio management page.
   *
   * @return array
   *   A render array for the LEAN portfolio page.
   */
  public function leanPortfolio() {
    $portfolio_data = $this->getLeanPortfolioData();
    
    return [
      '#markup' => $this->buildLeanPortfolioMarkup($portfolio_data),
      '#attached' => [
        'library' => ['unicornmetrics/lean-framework'],
      ],
    ];
  }

  /**
   * LEAN Holdings analysis page.
   *
   * @return array
   *   A render array for the LEAN holdings page.
   */
  public function leanHoldings() {
    $holdings_data = $this->getLeanHoldingsData();
    
    return [
      '#markup' => $this->buildLeanHoldingsMarkup($holdings_data),
      '#attached' => [
        'library' => ['unicornmetrics/lean-framework'],
      ],
    ];
  }

  /**
   * LEAN Performance analytics page.
   *
   * @return array
   *   A render array for the LEAN performance page.
   */
  public function leanPerformance() {
    $performance_data = $this->getLeanPerformanceData();
    
    return [
      '#markup' => $this->buildLeanPerformanceMarkup($performance_data),
      '#attached' => [
        'library' => ['unicornmetrics/lean-framework'],
      ],
    ];
  }

  /**
   * Get portfolio data by ID.
   *
   * @param string $portfolio_id
   *   The portfolio identifier.
   *
   * @return array
   *   Portfolio data array.
   */
  protected function getPortfolioById($portfolio_id) {
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
        'symbols' => ['EURUSD', 'USDJPY', 'USDCNH', 'ETHUSD'],
      ],
      'equity' => [
        'id' => 'equity',
        'name' => 'Growth Equity Portfolio',
        'description' => 'High-growth equity strategies with momentum focus',
        'algorithm' => 'UnicornGrowthStrategy',
        'environment' => 'live',
        'total_value' => 250000.00,
        'positions' => 12,
        'daily_pnl' => '+$1,234.56',
        'status' => 'active',
        'project_id' => 12346,
        'symbols' => ['AAPL', 'GOOGL', 'TSLA', 'NVDA'],
      ],
      'bonds' => [
        'id' => 'bonds',
        'name' => 'Conservative Bond Portfolio',
        'description' => 'Fixed income strategies for capital preservation',
        'algorithm' => 'UnicornBondLadder',
        'environment' => 'live',
        'total_value' => 100000.00,
        'positions' => 15,
        'daily_pnl' => '+$89.23',
        'status' => 'active',
        'project_id' => 12347,
        'symbols' => ['TLT', 'AGG', 'GOVT', 'CORP'],
      ],
      'paper' => [
        'id' => 'paper',
        'name' => 'Paper Trading Portfolio',
        'description' => 'Simulated trading for strategy testing and development',
        'algorithm' => 'UnicornTestStrategy',
        'environment' => 'paper',
        'total_value' => 50000.00,
        'positions' => 6,
        'daily_pnl' => '+$125.00',
        'status' => 'active',
        'project_id' => 12348,
        'symbols' => ['SPY', 'EURUSD', 'BTC'],
      ],
    ];

    return $portfolios[$portfolio_id] ?? $portfolios['forex'];
  }

  /**
   * Get portfolio metrics data.
   *
   * @param array $portfolio
   *   The portfolio data array.
   *
   * @return array
   *   Portfolio metrics array.
   */
  protected function getPortfolioMetrics(array $portfolio) {
    return [
      'sharpe_ratio' => 1.85,
      'max_drawdown' => -0.08,
      'annual_return' => 0.15,
      'volatility' => 0.12,
      'beta' => 0.95,
      'alpha' => 0.03,
    ];
  }

  /**
   * Render portfolio selector dropdown.
   *
   * @param string $current_portfolio_id
   *   The current portfolio ID.
   *
   * @return \Drupal\Core\Render\Markup
   *   The rendered portfolio selector markup.
   */
  protected function renderPortfolioSelector($current_portfolio_id) {
    $portfolios = [
      'forex' => [
        'name' => 'Primary Forex Portfolio',
        'value' => '$125,847.62',
        'status' => 'active',
      ],
      'equity' => [
        'name' => 'Growth Equity Portfolio',
        'value' => '$250,000.00',
        'status' => 'active',
      ],
      'bonds' => [
        'name' => 'Conservative Bond Portfolio',
        'value' => '$100,000.00',
        'status' => 'active',
      ],
      'paper' => [
        'name' => 'Paper Trading Portfolio',
        'value' => '$50,000.00',
        'status' => 'active',
      ],
    ];
    
    $current_portfolio = $this->getPortfolioById($current_portfolio_id);
    
    $options = '';
    foreach ($portfolios as $id => $portfolio) {
      $selected = ($id == $current_portfolio_id) ? 'selected' : '';
      $status_indicator = $portfolio['status'] == 'active' ? '⚡' : '⏸️';
      $options .= '<option value="' . $id . '" ' . $selected . '>' . $status_indicator . ' ' . $portfolio['name'] . ' (' . $portfolio['value'] . ')</option>';
    }
    
    return Markup::create('
    <div class="portfolio-selector-container">
      <div class="portfolio-selector">
        <h3>📁 Portfolio Selection</h3>
        <div class="selector-wrapper">
          <label for="portfolio-dropdown">Choose Portfolio:</label>
          <select id="portfolio-dropdown" onchange="switchPortfolio(this.value)">
            ' . $options . '
          </select>
          <span class="total-assets">Total Assets: $525,847.62</span>
        </div>
      </div>
      
      <div class="quick-stats">
        <div class="quick-stat-item">
          <span class="stat-number">4</span>
          <span class="stat-desc">Active Portfolios</span>
        </div>
        <div class="quick-stat-item">
          <span class="stat-number">31</span>
          <span class="stat-desc">Total Positions</span>
        </div>
        <div class="quick-stat-item">
          <span class="stat-number">+$3,809.68</span>
          <span class="stat-desc">Today\'s P&L</span>
        </div>
      </div>
    </div>');
  }

  // Placeholder methods for LEAN framework integration.
  // These would be implemented with actual LEAN data sources.

  /**
   * Get LEAN portfolio data.
   *
   * @return array
   *   LEAN portfolio data.
   */
  protected function getLeanPortfolioData() {
    // Placeholder implementation.
    return [];
  }

  /**
   * Get LEAN holdings data.
   *
   * @return array
   *   LEAN holdings data.
   */
  protected function getLeanHoldingsData() {
    // Placeholder implementation.
    return [];
  }

  /**
   * Get LEAN performance data.
   *
   * @return array
   *   LEAN performance data.
   */
  protected function getLeanPerformanceData() {
    // Placeholder implementation.
    return [];
  }

  /**
   * Build LEAN portfolio markup.
   *
   * @param array $data
   *   Portfolio data.
   *
   * @return string
   *   HTML markup.
   */
  protected function buildLeanPortfolioMarkup(array $data) {
    return '<p>LEAN Portfolio implementation pending.</p>';
  }

  /**
   * Build LEAN holdings markup.
   *
   * @param array $data
   *   Holdings data.
   *
   * @return string
   *   HTML markup.
   */
  protected function buildLeanHoldingsMarkup(array $data) {
    return '<p>LEAN Holdings implementation pending.</p>';
  }

  /**
   * Build LEAN performance markup.
   *
   * @param array $data
   *   Performance data.
   *
   * @return string
   *   HTML markup.
   */
  protected function buildLeanPerformanceMarkup(array $data) {
    return '<p>LEAN Performance implementation pending.</p>';
  }

}
