<?php

namespace Drupal\unicornmetrics\Form;

use Drupal\Core\Config\ConfigFactoryInterface;
use Drupal\Core\Form\ConfigFormBase;
use Drupal\Core\Form\FormStateInterface;
use Drupal\unicornmetrics\Service\PortfolioApiService;
use Symfony\Component\DependencyInjection\ContainerInterface;

/**
 * Configure UnicornMetrics settings for the site.
 */
class AdminSettingsForm extends ConfigFormBase {

  /**
   * The portfolio API service.
   *
   * @var \Drupal\unicornmetrics\Service\PortfolioApiService
   */
  protected $portfolioApi;

  /**
   * Constructs a AdminSettingsForm object.
   *
   * @param \Drupal\Core\Config\ConfigFactoryInterface $config_factory
   *   The factory for configuration objects.
   * @param \Drupal\unicornmetrics\Service\PortfolioApiService $portfolio_api
   *   The portfolio API service.
   */
  public function __construct(ConfigFactoryInterface $config_factory, PortfolioApiService $portfolio_api) {
    parent::__construct($config_factory);
    $this->portfolioApi = $portfolio_api;
  }

  /**
   * {@inheritdoc}
   */
  public static function create(ContainerInterface $container) {
    return new static(
      $container->get('config.factory'),
      $container->get('unicornmetrics.portfolio_api')
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function getEditableConfigNames(): array {
    return ['unicornmetrics.settings'];
  }

  /**
   * {@inheritdoc}
   */
  public function getFormId(): string {
    return 'unicornmetrics_admin_settings';
  }

  /**
   * {@inheritdoc}
   */
  public function buildForm(array $form, FormStateInterface $form_state): array {
    $config = $this->config('unicornmetrics.settings');

    $form['general'] = [
      '#type' => 'fieldset',
      '#title' => $this->t('General Settings'),
      '#description' => $this->t('Configure general UnicornMetrics settings.'),
    ];

    $form['general']['default_portfolio'] = [
      '#type' => 'select',
      '#title' => $this->t('Default Portfolio'),
      '#description' => $this->t('Select the default portfolio to display on the dashboard.'),
      '#options' => $this->getPortfolioOptions(),
      '#default_value' => $config->get('default_portfolio') ?? 'Myportolio',
      '#required' => TRUE,
    ];

    $form['general']['refresh_interval'] = [
      '#type' => 'number',
      '#title' => $this->t('Data Refresh Interval (seconds)'),
      '#description' => $this->t('How often to refresh portfolio data from the backend API.'),
      '#default_value' => $config->get('refresh_interval') ?? 60,
      '#min' => 10,
      '#max' => 3600,
      '#step' => 1,
      '#required' => TRUE,
    ];

    $form['api'] = [
      '#type' => 'fieldset',
      '#title' => $this->t('API Settings'),
      '#description' => $this->t('Configure backend API integration settings.'),
    ];

    $form['api']['backend_path'] = [
      '#type' => 'textfield',
      '#title' => $this->t('Backend Path'),
      '#description' => $this->t('Path to the backend Python portfolio framework.'),
      '#default_value' => $config->get('backend_path') ?? '/home/runner/work/unicorninvesting/unicorninvesting/BackendPython/unicorn/4_portfolios',
      '#required' => TRUE,
      '#maxlength' => 255,
    ];

    $form['api']['api_timeout'] = [
      '#type' => 'number',
      '#title' => $this->t('API Timeout (seconds)'),
      '#description' => $this->t('Timeout for backend API calls.'),
      '#default_value' => $config->get('api_timeout') ?? 30,
      '#min' => 5,
      '#max' => 300,
      '#step' => 1,
      '#required' => TRUE,
    ];

    $form['api']['enable_fallback'] = [
      '#type' => 'checkbox',
      '#title' => $this->t('Enable Fallback Data'),
      '#description' => $this->t('Use fallback data when backend API is unavailable.'),
      '#default_value' => $config->get('enable_fallback') ?? TRUE,
    ];

    $form['validation'] = [
      '#type' => 'fieldset',
      '#title' => $this->t('Validation Settings'),
      '#description' => $this->t('Configure data validation parameters.'),
    ];

    $form['validation']['strict_validation'] = [
      '#type' => 'checkbox',
      '#title' => $this->t('Strict Validation'),
      '#description' => $this->t('Enable strict validation of all portfolio data and API responses.'),
      '#default_value' => $config->get('strict_validation') ?? FALSE,
    ];

    $form['validation']['max_allocation_variance'] = [
      '#type' => 'number',
      '#title' => $this->t('Maximum Allocation Variance (%)'),
      '#description' => $this->t('Maximum allowed variance from 100% total allocation.'),
      '#default_value' => $config->get('max_allocation_variance') ?? 0.1,
      '#min' => 0.01,
      '#max' => 5.0,
      '#step' => 0.01,
      '#required' => TRUE,
    ];

    $form['performance'] = [
      '#type' => 'fieldset',
      '#title' => $this->t('Performance Settings'),
      '#description' => $this->t('Configure performance monitoring and thresholds.'),
    ];

    $form['performance']['page_load_threshold'] = [
      '#type' => 'number',
      '#title' => $this->t('Page Load Threshold (seconds)'),
      '#description' => $this->t('Warning threshold for page load times.'),
      '#default_value' => $config->get('page_load_threshold') ?? 3.0,
      '#min' => 0.5,
      '#max' => 10.0,
      '#step' => 0.1,
      '#required' => TRUE,
    ];

    $form['performance']['memory_threshold'] = [
      '#type' => 'number',
      '#title' => $this->t('Memory Usage Threshold (MB)'),
      '#description' => $this->t('Warning threshold for memory usage.'),
      '#default_value' => $config->get('memory_threshold') ?? 256,
      '#min' => 64,
      '#max' => 1024,
      '#step' => 1,
      '#required' => TRUE,
    ];

    // Add API connection test button
    $form['test'] = [
      '#type' => 'fieldset',
      '#title' => $this->t('Connection Test'),
      '#description' => $this->t('Test the connection to the backend API.'),
    ];

    $form['test']['test_connection'] = [
      '#type' => 'button',
      '#value' => $this->t('Test API Connection'),
      '#ajax' => [
        'callback' => '::testApiConnection',
        'wrapper' => 'test-results',
        'effect' => 'fade',
      ],
    ];

    $form['test']['test_results'] = [
      '#type' => 'markup',
      '#markup' => '<div id="test-results"></div>',
    ];

    return parent::buildForm($form, $form_state);
  }

  /**
   * {@inheritdoc}
   */
  public function validateForm(array &$form, FormStateInterface $form_state): void {
    parent::validateForm($form, $form_state);

    // Validate backend path exists
    $backend_path = $form_state->getValue('backend_path');
    if (!empty($backend_path) && !is_dir($backend_path)) {
      $form_state->setErrorByName('backend_path', $this->t('Backend path does not exist or is not accessible.'));
    }

    // Validate portfolio exists
    $default_portfolio = $form_state->getValue('default_portfolio');
    $available_portfolios = $this->portfolioApi->getAvailablePortfolios();
    if (!in_array($default_portfolio, $available_portfolios)) {
      $form_state->setErrorByName('default_portfolio', $this->t('Selected portfolio "@portfolio" is not available.', ['@portfolio' => $default_portfolio]));
    }

    // Validate numeric ranges
    $refresh_interval = $form_state->getValue('refresh_interval');
    if ($refresh_interval < 10 || $refresh_interval > 3600) {
      $form_state->setErrorByName('refresh_interval', $this->t('Refresh interval must be between 10 and 3600 seconds.'));
    }

    $api_timeout = $form_state->getValue('api_timeout');
    if ($api_timeout < 5 || $api_timeout > 300) {
      $form_state->setErrorByName('api_timeout', $this->t('API timeout must be between 5 and 300 seconds.'));
    }

    // Validate allocation variance
    $max_variance = $form_state->getValue('max_allocation_variance');
    if ($max_variance < 0.01 || $max_variance > 5.0) {
      $form_state->setErrorByName('max_allocation_variance', $this->t('Maximum allocation variance must be between 0.01% and 5.0%.'));
    }

    // Validate performance thresholds
    $page_load_threshold = $form_state->getValue('page_load_threshold');
    if ($page_load_threshold < 0.5 || $page_load_threshold > 10.0) {
      $form_state->setErrorByName('page_load_threshold', $this->t('Page load threshold must be between 0.5 and 10.0 seconds.'));
    }

    $memory_threshold = $form_state->getValue('memory_threshold');
    if ($memory_threshold < 64 || $memory_threshold > 1024) {
      $form_state->setErrorByName('memory_threshold', $this->t('Memory threshold must be between 64 and 1024 MB.'));
    }
  }

  /**
   * {@inheritdoc}
   */
  public function submitForm(array &$form, FormStateInterface $form_state): void {
    $config = $this->config('unicornmetrics.settings');
    
    $config
      ->set('default_portfolio', $form_state->getValue('default_portfolio'))
      ->set('refresh_interval', $form_state->getValue('refresh_interval'))
      ->set('backend_path', $form_state->getValue('backend_path'))
      ->set('api_timeout', $form_state->getValue('api_timeout'))
      ->set('enable_fallback', $form_state->getValue('enable_fallback'))
      ->set('strict_validation', $form_state->getValue('strict_validation'))
      ->set('max_allocation_variance', $form_state->getValue('max_allocation_variance'))
      ->set('page_load_threshold', $form_state->getValue('page_load_threshold'))
      ->set('memory_threshold', $form_state->getValue('memory_threshold'))
      ->save();

    parent::submitForm($form, $form_state);
  }

  /**
   * AJAX callback to test API connection.
   */
  public function testApiConnection(array &$form, FormStateInterface $form_state): array {
    try {
      // Test portfolio API connection
      $portfolios = $this->portfolioApi->getAvailablePortfolios();
      $test_portfolio = $portfolios[0] ?? 'Myportolio';
      $status = $this->portfolioApi->getPortfolioStatus($test_portfolio);

      if (!empty($status)) {
        $markup = '<div class="messages messages--status">' . 
          $this->t('✅ API connection successful. Found @count portfolios.', ['@count' => count($portfolios)]) . 
          '</div>';
      } else {
        $markup = '<div class="messages messages--warning">' . 
          $this->t('⚠️ API connection established but no valid response received.') . 
          '</div>';
      }
    } catch (\Exception $e) {
      $markup = '<div class="messages messages--error">' . 
        $this->t('❌ API connection failed: @error', ['@error' => $e->getMessage()]) . 
        '</div>';
    }

    return [
      '#type' => 'markup',
      '#markup' => $markup,
    ];
  }

  /**
   * Get available portfolio options for select field.
   */
  protected function getPortfolioOptions(): array {
    $portfolios = $this->portfolioApi->getAvailablePortfolios();
    $options = [];
    
    foreach ($portfolios as $portfolio) {
      $options[$portfolio] = $this->t('@portfolio Portfolio', ['@portfolio' => ucfirst($portfolio)]);
    }
    
    return $options;
  }

}