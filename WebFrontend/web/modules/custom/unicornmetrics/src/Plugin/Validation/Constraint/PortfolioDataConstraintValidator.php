<?php

namespace Drupal\unicornmetrics\Plugin\Validation\Constraint;

use Drupal\Core\DependencyInjection\ContainerInjectionInterface;
use Drupal\Core\StringTranslation\StringTranslationTrait;
use Symfony\Component\DependencyInjection\ContainerInterface;
use Symfony\Component\Validator\Constraint;
use Symfony\Component\Validator\ConstraintValidator;

/**
 * Validates portfolio data structure and values.
 */
class PortfolioDataConstraintValidator extends ConstraintValidator implements ContainerInjectionInterface {

  use StringTranslationTrait;

  /**
   * Supported portfolio types.
   *
   * @var array
   */
  const SUPPORTED_TYPES = ['dual_crypto', 'forex', 'equity', 'mixed'];

  /**
   * Required portfolio fields.
   *
   * @var array
   */
  const REQUIRED_FIELDS = ['portfolio_name', 'strategy_type', 'assets', 'target_volatility'];

  /**
   * Risk parameter ranges.
   *
   * @var array
   */
  const RISK_RANGES = [
    'target_volatility' => ['min' => 0.05, 'max' => 0.50],
    'max_drawdown_limit' => ['min' => 0.05, 'max' => 0.30],
    'portfolio_heat' => ['min' => 0.0, 'max' => 1.0],
    'var_5pct' => ['min' => 0.01, 'max' => 0.20],
  ];

  /**
   * {@inheritdoc}
   */
  public static function create(ContainerInterface $container): static {
    return new static();
  }

  /**
   * {@inheritdoc}
   */
  public function validate($value, Constraint $constraint): void {
    if (!$constraint instanceof PortfolioDataConstraint) {
      return;
    }

    if (empty($value) || !is_array($value)) {
      return;
    }

    // Validate required fields
    $this->validateRequiredFields($value, $constraint);

    // Validate portfolio name
    $this->validatePortfolioName($value, $constraint);

    // Validate portfolio type
    $this->validatePortfolioType($value, $constraint);

    // Validate asset allocation
    $this->validateAssetAllocation($value, $constraint);

    // Validate risk parameters
    $this->validateRiskParameters($value, $constraint);
  }

  /**
   * Validates required fields are present.
   */
  protected function validateRequiredFields(array $value, PortfolioDataConstraint $constraint): void {
    foreach (self::REQUIRED_FIELDS as $field) {
      if (!isset($value[$field]) || empty($value[$field])) {
        $this->context->buildViolation($constraint->missingFieldMessage)
          ->setParameter('@field', $field)
          ->addViolation();
      }
    }
  }

  /**
   * Validates portfolio name format.
   */
  protected function validatePortfolioName(array $value, PortfolioDataConstraint $constraint): void {
    if (isset($value['portfolio_name'])) {
      $name = $value['portfolio_name'];
      if (!preg_match('/^[a-zA-Z0-9_]+$/', $name)) {
        $this->context->buildViolation($constraint->invalidNameMessage)
          ->setParameter('@name', $name)
          ->addViolation();
      }
    }
  }

  /**
   * Validates portfolio type.
   */
  protected function validatePortfolioType(array $value, PortfolioDataConstraint $constraint): void {
    if (isset($value['strategy_type'])) {
      $type = $value['strategy_type'];
      if (!in_array($type, self::SUPPORTED_TYPES)) {
        $this->context->buildViolation($constraint->invalidTypeMessage)
          ->setParameter('@type', $type)
          ->setParameter('@types', implode(', ', self::SUPPORTED_TYPES))
          ->addViolation();
      }
    }
  }

  /**
   * Validates asset allocation totals 100%.
   */
  protected function validateAssetAllocation(array $value, PortfolioDataConstraint $constraint): void {
    if (isset($value['assets']) && is_array($value['assets'])) {
      $total_allocation = 0;
      foreach ($value['assets'] as $asset_data) {
        if (isset($asset_data['allocation_percent'])) {
          $total_allocation += $asset_data['allocation_percent'];
        }
      }

      // Allow for small rounding errors (within 0.1%)
      if (abs($total_allocation - 100) > 0.1) {
        $this->context->buildViolation($constraint->invalidAllocationMessage)
          ->setParameter('@total', number_format($total_allocation, 2))
          ->addViolation();
      }
    }
  }

  /**
   * Validates risk parameters are within acceptable ranges.
   */
  protected function validateRiskParameters(array $value, PortfolioDataConstraint $constraint): void {
    foreach (self::RISK_RANGES as $parameter => $range) {
      if (isset($value[$parameter])) {
        $param_value = $value[$parameter];
        if ($param_value < $range['min'] || $param_value > $range['max']) {
          $this->context->buildViolation($constraint->invalidRiskMessage)
            ->setParameter('@parameter', $parameter)
            ->setParameter('@value', $param_value)
            ->setParameter('@min', $range['min'])
            ->setParameter('@max', $range['max'])
            ->addViolation();
        }
      }
    }
  }

}