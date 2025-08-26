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
    $metrics_table = '
    <h1>Unicorn Metrics Dashboard</h1>
    <p>Welcome to the Unicorn Investment Metrics Dashboard. Access comprehensive performance analytics across 5 key categories with 78 total metrics.</p>
    
    <table class="metrics-nav-table">
      <thead>
        <tr>
          <th class="count-column">Metrics Count</th>
          <th class="link-column">Category</th>
          <th class="description-column">Description</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td class="count-column"><span class="metric-count">15</span></td>
          <td class="link-column"><a href="/unicorn/portfolio">Portfolio Statistics</a></td>
          <td class="description-column">Comprehensive portfolio performance analysis including returns, volatility, Sharpe ratio, portfolio value, and composition metrics for investment analysis.</td>
        </tr>
        <tr>
          <td class="count-column"><span class="metric-count">16</span></td>
          <td class="link-column"><a href="/unicorn/trades">Trade Statistics</a></td>
          <td class="description-column">Detailed trading performance metrics covering win rates, average gains/losses, trade duration, drawdowns, and execution analysis for strategy optimization.</td>
        </tr>
        <tr>
          <td class="count-column"><span class="metric-count">14</span></td>
          <td class="link-column"><a href="/unicorn/risk">Risk Metrics</a></td>
          <td class="description-column">Advanced risk assessment tools including VaR, CVaR, maximum drawdown, Kelly criterion, and risk budget analysis for comprehensive risk management.</td>
        </tr>
        <tr>
          <td class="count-column"><span class="metric-count">13</span></td>
          <td class="link-column"><a href="/unicorn/forecasting">Forecasting Metrics</a></td>
          <td class="description-column">Predictive analytics and machine learning model performance including forecast accuracy, confidence intervals, trend analysis, and signal quality scoring.</td>
        </tr>
        <tr>
          <td class="count-column"><span class="metric-count">20</span></td>
          <td class="link-column"><a href="/unicorn/market">Market Relationship Metrics</a></td>
          <td class="description-column">Market correlation analysis including alpha/beta calculations, benchmark comparisons, sector allocation, currency exposure, and systematic risk factors.</td>
        </tr>
      </tbody>
    </table>
    
    <div class="system-overview">
      <h3>System Overview</h3>
      <ul>
        <li><strong>Total Metrics:</strong> 78 performance indicators</li>
        <li><strong>Categories:</strong> 5 specialized metric groups</li>
        <li><strong>Real-time Analysis:</strong> Live portfolio monitoring</li>
        <li><strong>Risk Management:</strong> Comprehensive risk assessment tools</li>
        <li><strong>Predictive Analytics:</strong> Machine learning forecasting</li>
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
                  color: #333;
                }
                .metrics-nav-table tr:hover {
                  background-color: #f5f5f5;
                }
                .metrics-nav-table a {
                  color: #0073aa;
                  text-decoration: none;
                  font-weight: 500;
                }
                .metrics-nav-table a:hover {
                  text-decoration: underline;
                  color: #005a87;
                }
                .metric-count {
                  background: #e7f3ff;
                  color: #0066cc;
                  padding: 4px 12px;
                  border-radius: 15px;
                  font-size: 0.9em;
                  font-weight: bold;
                  text-align: center;
                  display: inline-block;
                  min-width: 45px;
                }
                .metrics-nav-table .count-column {
                  text-align: center;
                  width: 120px;
                }
                .metrics-nav-table .link-column {
                  width: 250px;
                }
                .metrics-nav-table .description-column {
                  width: auto;
                }
                .system-overview {
                  margin-top: 30px;
                  padding: 20px;
                  background: #f8f9fa;
                  border-radius: 8px;
                }
                .system-overview ul {
                  margin: 10px 0;
                  padding-left: 20px;
                }
                .metrics-list {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
                  gap: 20px;
                  margin: 20px 0;
                }
                .metric-item {
                  padding: 15px;
                  background: #f8f9fa;
                  border-radius: 8px;
                  border-left: 4px solid #0073aa;
                }
                .metric-name {
                  font-weight: bold;
                  color: #333;
                  margin-bottom: 5px;
                }
                .metric-description {
                  color: #666;
                  font-size: 0.9em;
                  line-height: 1.4;
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
   * Portfolio statistics page.
   */
  public function portfolio() {
    $metrics = [
      'Compounding Annual Return' => 'Geometric mean of annual returns, showing the constant rate of return that would yield the same cumulative return.',
      'Annual Return' => 'Simple annualized return percentage based on average daily performance.',
      'Annual Volatility (Standard Deviation)' => 'Annualized standard deviation of daily returns, measuring portfolio volatility.',
      'Annual Variance' => 'Annualized variance of daily returns, quantifying return dispersion.',
      'Sharpe Ratio' => 'Risk-adjusted return metric: (Annual Return - Risk Free Rate) / Annual Volatility.',
      'Sortino Ratio' => 'Modified Sharpe ratio using only downside deviation instead of total volatility.',
      'Probabilistic Sharpe Ratio' => 'Probability that the Sharpe ratio exceeds a benchmark level.',
      'Information Ratio' => 'Excess return per unit of tracking error relative to benchmark.',
      'Treynor Ratio' => 'Return per unit of systematic risk (beta), measuring market risk efficiency.',
      'Portfolio Turnover' => 'Rate at which portfolio holdings are replaced, indicating trading activity.',
      'Portfolio Value (Start Equity)' => 'Initial portfolio value at the beginning of the analysis period.',
      'Portfolio Value (End Equity)' => 'Final portfolio value at the end of the analysis period.',
      'Net Profit' => 'Total profit/loss percentage after all fees and expenses.',
      'Total Fees' => 'Cumulative transaction costs, commissions, and management fees.',
      'Estimated Strategy Capacity' => 'Maximum capital the strategy can handle while maintaining performance.',
    ];

    return $this->renderMetricsPage('Portfolio Statistics', $metrics, 'Comprehensive portfolio performance analysis including returns, volatility, and risk-adjusted metrics.');
  }

  /**
   * Trade statistics page.
   */
  public function trades() {
    $metrics = [
      'Total Orders' => 'Total number of buy and sell orders executed during the analysis period.',
      'Total Trades' => 'Number of completed round-trip trades (buy and sell combinations).',
      'Win Rate' => 'Percentage of profitable trades out of total completed trades.',
      'Loss Rate' => 'Percentage of losing trades out of total completed trades.',
      'Average Win' => 'Average profit percentage per winning trade.',
      'Average Loss' => 'Average loss percentage per losing trade.',
      'Profit Loss Ratio' => 'Ratio of average win to average loss, measuring trade efficiency.',
      'Expectancy' => 'Expected value per trade: (Win Rate × Average Win) - (Loss Rate × Average Loss).',
      'Maximum Drawdown' => 'Largest peak-to-trough decline in account value during the period.',
      'Drawdown Recovery' => 'Time taken to recover from the maximum drawdown to new highs.',
      'Maximum Consecutive Wins' => 'Longest streak of consecutive profitable trades.',
      'Maximum Consecutive Losses' => 'Longest streak of consecutive losing trades.',
      'Largest Winning Trade' => 'Single largest profit from any individual trade.',
      'Largest Losing Trade' => 'Single largest loss from any individual trade.',
      'Average Trade Length' => 'Average duration that positions are held open.',
      'Total Sales Volume' => 'Total dollar value of all securities sold during the period.',
    ];

    return $this->renderMetricsPage('Trade Statistics', $metrics, 'Detailed analysis of trading performance, execution quality, and trade management effectiveness.');
  }

  /**
   * Risk metrics page.
   */
  public function risk() {
    $metrics = [
      'Beta' => 'Portfolio sensitivity to market movements; Beta > 1 indicates higher volatility than market.',
      'Alpha' => 'Excess return relative to the market benchmark, measuring outperformance.',
      'Value at Risk (95%)' => 'Maximum expected loss at 95% confidence level over a specific time horizon.',
      'Value at Risk (99%)' => 'Maximum expected loss at 99% confidence level, capturing extreme risk scenarios.',
      'Maximum Drawdown' => 'Largest peak-to-trough decline, indicating worst-case loss scenario.',
      'Drawdown Recovery Period' => 'Time required to recover from maximum drawdown to previous highs.',
      'Downside Deviation' => 'Standard deviation of negative returns only, focusing on downside risk.',
      'Tracking Error' => 'Standard deviation of excess returns relative to benchmark.',
      'Probabilistic Sharpe Ratio' => 'Statistical confidence that the Sharpe ratio exceeds a threshold.',
      'Sortino Ratio' => 'Risk-adjusted return using downside deviation instead of total volatility.',
      'Calmar Ratio' => 'Annual return divided by maximum drawdown, measuring return per unit of drawdown risk.',
      'Sterling Ratio' => 'Compounding annual return divided by average maximum drawdown.',
      'Burke Ratio' => 'Excess return divided by the square root of sum of squared drawdowns.',
      'Conditional Sharpe Ratio' => 'Sharpe ratio calculated using only periods exceeding a performance threshold.',
    ];

    return $this->renderMetricsPage('Risk Metrics', $metrics, 'Advanced risk assessment tools for comprehensive portfolio risk management and control.');
  }

  /**
   * Forecasting metrics page.
   */
  public function forecasting() {
    $metrics = [
      'Prediction Accuracy' => 'Percentage of correct directional forecasts over the evaluation period.',
      'Mean Absolute Error (MAE)' => 'Average magnitude of prediction errors without considering direction.',
      'Root Mean Square Error (RMSE)' => 'Square root of average squared prediction errors, penalizing large errors.',
      'Mean Absolute Percentage Error (MAPE)' => 'Average absolute percentage difference between predicted and actual values.',
      'Forecast Bias' => 'Systematic tendency to over-predict or under-predict market movements.',
      'Prediction Interval Coverage' => 'Percentage of actual values falling within predicted confidence intervals.',
      'Directional Accuracy' => 'Percentage of correct up/down movement predictions.',
      'Hit Rate' => 'Proportion of profitable trading signals generated by the forecasting model.',
      'Signal Quality Score' => 'Composite metric evaluating overall forecasting model effectiveness.',
      'Model Confidence Level' => 'Statistical confidence in the forecasting model predictions.',
      'Forecast Horizon Reliability' => 'Accuracy degradation as prediction horizon extends.',
      'Regime Detection Accuracy' => 'Ability to correctly identify market regime changes.',
      'Adaptive Learning Rate' => 'Speed at which the model adapts to new market conditions.',
    ];

    return $this->renderMetricsPage('Forecasting Metrics', $metrics, 'Predictive analytics and machine learning model performance evaluation metrics.');
  }

  /**
   * Market relationship metrics page.
   */
  public function market() {
    $metrics = [
      'Alpha (Jensen\'s Alpha)' => 'Risk-adjusted excess return relative to the market benchmark.',
      'Beta (Market Beta)' => 'Systematic risk measure showing portfolio sensitivity to market movements.',
      'Correlation Coefficient' => 'Statistical relationship strength between portfolio and market returns.',
      'R-Squared' => 'Percentage of portfolio variance explained by market movements.',
      'Tracking Error' => 'Standard deviation of excess returns relative to benchmark.',
      'Information Ratio' => 'Excess return per unit of tracking error, measuring active management skill.',
      'Treynor Ratio' => 'Risk-adjusted return per unit of systematic risk (beta).',
      'Active Share' => 'Percentage of portfolio holdings that differ from the benchmark.',
      'Up Capture Ratio' => 'Portfolio performance relative to benchmark during market up periods.',
      'Down Capture Ratio' => 'Portfolio performance relative to benchmark during market down periods.',
      'Bull Market Beta' => 'Portfolio sensitivity during rising market conditions.',
      'Bear Market Beta' => 'Portfolio sensitivity during declining market conditions.',
      'Market Timing Ability' => 'Skill in adjusting portfolio exposure based on market conditions.',
      'Security Selection Skill' => 'Ability to select securities that outperform within sectors.',
      'Style Exposure Analysis' => 'Portfolio bias toward growth, value, size, or quality factors.',
      'Sector Allocation Effects' => 'Performance attribution from sector weight decisions.',
      'Geographic Diversification' => 'Risk reduction benefits from international exposure.',
      'Currency Exposure Impact' => 'Performance effect from foreign exchange fluctuations.',
      'Interest Rate Sensitivity' => 'Portfolio response to changes in interest rate environment.',
      'Benchmark Outperformance Frequency' => 'Percentage of periods beating the benchmark index.',
    ];

    return $this->renderMetricsPage('Market Relationship Metrics', $metrics, 'Comprehensive analysis of portfolio relationships with market factors and benchmarks.');
  }

  /**
   * Helper method to render metrics pages.
   */
  private function renderMetricsPage($title, $metrics, $description) {
    $metrics_list = '';
    foreach ($metrics as $name => $desc) {
      $metrics_list .= "
        <div class='metric-item'>
          <div class='metric-name'>{$name}</div>
          <div class='metric-description'>{$desc}</div>
        </div>
      ";
    }

    $content = "
      <h1>{$title}</h1>
      <p>{$description}</p>
      <p><strong>Available Metrics:</strong> " . count($metrics) . " performance indicators</p>
      
      <div class='metrics-list'>
        {$metrics_list}
      </div>
      
      <div style='margin-top: 30px; text-align: center;'>
        <a href='/unicorn' style='background: #0073aa; color: white; padding: 10px 20px; text-decoration: none; border-radius: 5px;'>← Back to Dashboard</a>
      </div>
    ";

    return [
      '#markup' => $content,
      '#attached' => [
        'html_head' => [
          [
            [
              '#tag' => 'style',
              '#value' => '
                .metrics-list {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
                  gap: 20px;
                  margin: 20px 0;
                }
                .metric-item {
                  padding: 15px;
                  background: #f8f9fa;
                  border-radius: 8px;
                  border-left: 4px solid #0073aa;
                  transition: all 0.3s ease;
                }
                .metric-item:hover {
                  background: #e9ecef;
                  transform: translateY(-2px);
                  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                }
                .metric-name {
                  font-weight: bold;
                  color: #333;
                  margin-bottom: 8px;
                  font-size: 1.1em;
                }
                .metric-description {
                  color: #666;
                  font-size: 0.95em;
                  line-height: 1.5;
                }
              ',
            ],
            'unicorn-metrics-pages-styles',
          ],
        ],
      ],
    ];
  }

}
