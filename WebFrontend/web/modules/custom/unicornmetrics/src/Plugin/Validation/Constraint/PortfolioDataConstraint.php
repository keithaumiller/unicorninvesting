<?php

namespace Drupal\unicornmetrics\Plugin\Validation\Constraint;

use Drupal\Core\Validation\Attribute\Constraint;
use Symfony\Component\Validator\Constraint as SymfonyConstraint;

/**
 * Validates portfolio data structure and values.
 *
 * @Constraint(
 *   id = "PortfolioData",
 *   label = @Translation("Portfolio Data", context = "Validation"),
 *   type = "entity"
 * )
 */
#[Constraint(
  id: 'PortfolioData',
  type: 'entity'
)]
class PortfolioDataConstraint extends SymfonyConstraint {

  /**
   * The message that will be shown if portfolio name is invalid.
   *
   * @var string
   */
  public string $invalidNameMessage = 'The portfolio name "@name" is invalid. Portfolio names must be alphanumeric and can contain underscores.';

  /**
   * The message that will be shown if asset allocation is invalid.
   *
   * @var string
   */
  public string $invalidAllocationMessage = 'Asset allocation must total 100%. Current total: @total%.';

  /**
   * The message that will be shown if risk parameters are invalid.
   *
   * @var string
   */
  public string $invalidRiskMessage = 'Risk parameter "@parameter" value "@value" is outside acceptable range (@min to @max).';

  /**
   * The message that will be shown if required fields are missing.
   *
   * @var string
   */
  public string $missingFieldMessage = 'Required portfolio field "@field" is missing.';

  /**
   * The message that will be shown if portfolio type is unsupported.
   *
   * @var string
   */
  public string $invalidTypeMessage = 'Portfolio type "@type" is not supported. Supported types: @types.';

}