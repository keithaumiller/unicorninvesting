<?php

namespace Drupal\unicornmetrics\Plugin\Block;

use Drupal\Core\Block\BlockBase;
use Drupal\Core\Render\Markup;

/**
 * Provides a Portfolio Navigation Block.
 *
 * @Block(
 *   id = "portfolio_navigation_block",
 *   admin_label = @Translation("Portfolio Navigation & Selector"),
 *   category = @Translation("Unicorn Metrics")
 * )
 */
class PortfolioNavigationBlock extends BlockBase {

  /**
   * {@inheritdoc}
   */
  public function build() {
    // Get current portfolio selection from URL parameter or default
    $current_portfolio_id = \Drupal::request()->query->get('portfolio') ?? 'forex';
    $current_page = $this->getCurrentPage();
    
    // Generate navigation HTML
    $navigation_html = $this->renderNavigationMenu($current_portfolio_id, $current_page);
    
    return [
      '#markup' => Markup::create($navigation_html),
      '#attached' => [
        'html_head' => [
          [$this->getNavigationStyles(), 'portfolio-navigation-styles'],
        ],
      ],
    ];
  }

  /**
   * Determine current page based on URL.
   */
  private function getCurrentPage() {
    $current_path = \Drupal::service('path.current')->getPath();
    
    if (strpos($current_path, '/admin/metrics/lean/portfolio') !== false) {
      return 'portfolio';
    }
    if (strpos($current_path, '/admin/metrics/lean/holdings') !== false) {
      return 'holdings';
    }
    if (strpos($current_path, '/admin/metrics/lean/performance') !== false) {
      return 'performance';
    }
    if (strpos($current_path, '/admin/metrics/lean/algorithms/performance') !== false) {
      return 'algorithm-performance';
    }
    if (strpos($current_path, '/admin/metrics/lean/algorithms') !== false) {
      return 'algorithms';
    }
    if (strpos($current_path, '/admin/metrics/lean/backtest') !== false) {
      return 'backtest';
    }
    if (strpos($current_path, '/admin/metrics') !== false) {
      return 'dashboard';
    }
    
    return 'dashboard';
  }

  /**
   * Render navigation menu and portfolio selector.
   */
  private function renderNavigationMenu($current_portfolio_id, $current_page = 'dashboard') {
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
    
    // Build portfolio selector
    $portfolios = $this->getAvailablePortfolios();
    $options = '';
    foreach ($portfolios as $id => $portfolio) {
      $selected = ($id === $current_portfolio_id) ? ' selected' : '';
      $options .= '<option value="' . htmlspecialchars($id) . '"' . $selected . '>' . 
                  htmlspecialchars($portfolio['name']) . '</option>';
    }
    
    return '
    <div class="unicorn-unified-navigation">
      <div class="portfolio-selector-unified">
        <h3>📁 Portfolio Selection</h3>
        <div class="selector-wrapper">
          <select id="portfolio-dropdown" onchange="switchPortfolio(this.value)" class="portfolio-select">
            ' . $options . '
          </select>
          <div class="portfolio-stats">
            <span class="active-count">2 Active Portfolios</span>
          </div>
        </div>
      </div>
      <div class="nav-container">
        <h3>🧭 Navigation Menu</h3>
        <div class="nav-menu">' . $nav_items . '
        </div>
      </div>
    </div>
    
    <script>
    function switchPortfolio(portfolioId) {
      const currentUrl = new URL(window.location);
      currentUrl.searchParams.set("portfolio", portfolioId);
      window.location.href = currentUrl.toString();
    }
    </script>';
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
        'value' => 0.00,
      ]
    ];
  }

  /**
   * Get navigation styles.
   */
  private function getNavigationStyles(): array {
    return [
      '#tag' => 'style',
      '#value' => '
        .unicorn-unified-navigation { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
          color: white; 
          padding: 20px; 
          border-radius: 10px; 
          margin: 20px 0; 
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); 
        }
        .portfolio-selector-unified { 
          margin-bottom: 20px; 
          padding: 15px; 
          background: rgba(255,255,255,0.1); 
          border-radius: 8px; 
        }
        .portfolio-selector-unified h3 { 
          margin: 0 0 10px 0; 
          font-size: 1.1em; 
          font-weight: bold; 
        }
        .selector-wrapper { 
          display: flex; 
          align-items: center; 
          gap: 15px; 
          flex-wrap: wrap; 
        }
        .portfolio-select { 
          padding: 8px 12px; 
          border: 1px solid rgba(255,255,255,0.3); 
          border-radius: 5px; 
          background: rgba(255,255,255,0.9); 
          color: #333; 
          font-size: 14px; 
          min-width: 250px; 
        }
        .portfolio-stats { 
          display: flex; 
          gap: 15px; 
          font-size: 0.9em; 
          color: rgba(255,255,255,0.9); 
        }
        .nav-container h3 { 
          margin: 0 0 15px 0; 
          font-size: 1.1em; 
          font-weight: bold; 
        }
        .nav-menu { 
          display: grid; 
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); 
          gap: 10px; 
        }
        .nav-item { 
          display: flex; 
          align-items: center; 
          gap: 8px; 
          padding: 10px 15px; 
          background: rgba(255,255,255,0.1); 
          color: white; 
          text-decoration: none; 
          border-radius: 6px; 
          font-size: 0.9em; 
          transition: all 0.3s ease; 
        }
        .nav-item:hover { 
          background: rgba(255,255,255,0.2); 
          text-decoration: none; 
          color: white; 
          transform: translateY(-1px); 
        }
        .nav-item.current { 
          background: rgba(255,255,255,0.3); 
          font-weight: bold; 
          border: 1px solid rgba(255,255,255,0.4); 
        }
        .nav-icon { 
          font-size: 1.1em; 
          width: 20px; 
          text-align: center; 
        }
        .nav-label { 
          flex: 1; 
        }
        @media (max-width: 768px) {
          .nav-menu {
            grid-template-columns: 1fr;
          }
          .selector-wrapper {
            flex-direction: column;
            align-items: stretch;
          }
          .portfolio-stats {
            justify-content: center;
          }
        }
      ',
    ];
  }

}
