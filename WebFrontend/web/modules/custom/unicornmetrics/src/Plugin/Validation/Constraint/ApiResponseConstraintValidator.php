<?php

namespace Drupal\unicornmetrics\Plugin\Validation\Constraint;

use Drupal\Core\DependencyInjection\ContainerInjectionInterface;
use Drupal\Core\StringTranslation\StringTranslationTrait;
use Symfony\Component\DependencyInjection\ContainerInterface;
use Symfony\Component\Validator\Constraint;
use Symfony\Component\Validator\ConstraintValidator;

/**
 * Validates API response data integrity and structure.
 */
class ApiResponseConstraintValidator extends ConstraintValidator implements ContainerInjectionInterface {

  use StringTranslationTrait;

  /**
   * Expected field types for different API responses.
   *
   * @var array
   */
  const FIELD_TYPES = [
    'portfolio_status' => [
      'portfolio_name' => 'string',
      'status' => 'string',
      'target_volatility' => 'float',
      'last_updated' => 'string',
    ],
    'risk_metrics' => [
      'current_drawdown' => 'float',
      'portfolio_volatility' => 'float',
      'var_5pct' => 'float',
      'risk_score' => 'float',
    ],
    'eth_algorithm_status' => [
      'risk_algorithm' => 'array',
      'trading_algorithm' => 'array',
      'integration_status' => 'string',
    ],
  ];

  /**
   * Value ranges for numeric fields.
   *
   * @var array
   */
  const VALUE_RANGES = [
    'target_volatility' => ['min' => 0.0, 'max' => 1.0],
    'current_drawdown' => ['min' => -1.0, 'max' => 0.0],
    'portfolio_volatility' => ['min' => 0.0, 'max' => 2.0],
    'var_5pct' => ['min' => 0.0, 'max' => 1.0],
    'risk_score' => ['min' => 0.0, 'max' => 1.0],
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
    if (!$constraint instanceof ApiResponseConstraint) {
      return;
    }

    if (empty($value)) {
      return;
    }

    // If value is a string, try to decode JSON
    if (is_string($value)) {
      $decoded = json_decode($value, TRUE);
      if (json_last_error() !== JSON_ERROR_NONE) {
        $this->context->buildViolation($constraint->malformedResponseMessage)
          ->addViolation();
        return;
      }
      $value = $decoded;
    }

    if (!is_array($value)) {
      $this->context->buildViolation($constraint->malformedResponseMessage)
        ->addViolation();
      return;
    }

    // Determine response type and validate accordingly
    $response_type = $this->determineResponseType($value);
    if ($response_type && isset(self::FIELD_TYPES[$response_type])) {
      $this->validateFieldTypes($value, $constraint, $response_type);
      $this->validateValueRanges($value, $constraint);
    }
  }

  /**
   * Determines the type of API response based on available fields.
   */
  protected function determineResponseType(array $value): ?string {
    if (isset($value['risk_algorithm']) && isset($value['trading_algorithm'])) {
      return 'eth_algorithm_status';
    }
    if (isset($value['current_drawdown']) && isset($value['risk_score'])) {
      return 'risk_metrics';
    }
    if (isset($value['portfolio_name']) && isset($value['status'])) {
      return 'portfolio_status';
    }
    return NULL;
  }

  /**
   * Validates field types match expected types.
   */
  protected function validateFieldTypes(array $value, ApiResponseConstraint $constraint, string $response_type): void {
    $expected_types = self::FIELD_TYPES[$response_type];
    
    foreach ($expected_types as $field => $expected_type) {
      if (isset($value[$field])) {
        $actual_type = gettype($value[$field]);
        
        // Handle special cases
        if ($expected_type === 'float' && ($actual_type === 'integer' || $actual_type === 'double')) {
          continue; // Allow integers and doubles as floats
        }
        
        if ($actual_type !== $expected_type) {
          $this->context->buildViolation($constraint->invalidTypeMessage)
            ->setParameter('@field', $field)
            ->setParameter('@expected', $expected_type)
            ->setParameter('@actual', $actual_type)
            ->addViolation();
        }
      }
    }
  }

  /**
   * Validates numeric values are within acceptable ranges.
   */
  protected function validateValueRanges(array $value, ApiResponseConstraint $constraint): void {
    foreach (self::VALUE_RANGES as $field => $range) {
      if (isset($value[$field]) && is_numeric($value[$field])) {
        $field_value = (float) $value[$field];
        if ($field_value < $range['min'] || $field_value > $range['max']) {
          $this->context->buildViolation($constraint->valueOutOfRangeMessage)
            ->setParameter('@field', $field)
            ->setParameter('@value', $field_value)
            ->setParameter('@min', $range['min'])
            ->setParameter('@max', $range['max'])
            ->addViolation();
        }
      }
    }
  }

}