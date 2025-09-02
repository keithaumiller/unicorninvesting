<?php

namespace Drupal\unicornmetrics\Plugin\Validation\Constraint;

use Drupal\Core\Validation\Attribute\Constraint;
use Symfony\Component\Validator\Constraint as SymfonyConstraint;

/**
 * Validates API response data integrity and structure.
 *
 * @Constraint(
 *   id = "ApiResponse",
 *   label = @Translation("API Response", context = "Validation"),
 *   type = "string"
 * )
 */
#[Constraint(
  id: 'ApiResponse',
  label: 'API Response',
  type: 'string'
)]
class ApiResponseConstraint extends SymfonyConstraint {

  /**
   * The message that will be shown if API response is malformed.
   *
   * @var string
   */
  public string $malformedResponseMessage = 'API response is malformed or contains invalid JSON.';

  /**
   * The message that will be shown if required response fields are missing.
   *
   * @var string
   */
  public string $missingFieldMessage = 'Required API response field "@field" is missing.';

  /**
   * The message that will be shown if response data types are invalid.
   *
   * @var string
   */
  public string $invalidTypeMessage = 'API response field "@field" has invalid type. Expected @expected, got @actual.';

  /**
   * The message that will be shown if response values are out of range.
   *
   * @var string
   */
  public string $valueOutOfRangeMessage = 'API response field "@field" value "@value" is out of acceptable range (@min to @max).';

}