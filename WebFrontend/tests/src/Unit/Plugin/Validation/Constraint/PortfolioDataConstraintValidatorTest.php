<?php

namespace Drupal\Tests\unicornmetrics\Unit\Plugin\Validation\Constraint;

use Drupal\Tests\UnitTestBase;
use Drupal\unicornmetrics\Plugin\Validation\Constraint\PortfolioDataConstraint;
use Drupal\unicornmetrics\Plugin\Validation\Constraint\PortfolioDataConstraintValidator;
use Symfony\Component\Validator\Context\ExecutionContext;
use Symfony\Component\Validator\Violation\ConstraintViolationBuilder;

/**
 * Unit tests for PortfolioDataConstraintValidator.
 *
 * @group unicornmetrics
 * @group validation
 * @coversDefaultClass \Drupal\unicornmetrics\Plugin\Validation\Constraint\PortfolioDataConstraintValidator
 */
class PortfolioDataConstraintValidatorTest extends UnitTestBase {

  /**
   * The constraint validator under test.
   *
   * @var \Drupal\unicornmetrics\Plugin\Validation\Constraint\PortfolioDataConstraintValidator
   */
  protected $validator;

  /**
   * The constraint being tested.
   *
   * @var \Drupal\unicornmetrics\Plugin\Validation\Constraint\PortfolioDataConstraint
   */
  protected $constraint;

  /**
   * The execution context mock.
   *
   * @var \Symfony\Component\Validator\Context\ExecutionContext|\PHPUnit\Framework\MockObject\MockObject
   */
  protected $context;

  /**
   * {@inheritdoc}
   */
  protected function setUp(): void {
    parent::setUp();

    $this->validator = new PortfolioDataConstraintValidator();
    $this->constraint = new PortfolioDataConstraint();
    
    $this->context = $this->createMock(ExecutionContext::class);
    $this->validator->initialize($this->context);
  }

  /**
   * Tests validation with valid portfolio data.
   *
   * @covers ::validate
   */
  public function testValidateValidPortfolioData(): void {
    $valid_data = [
      'portfolio_name' => 'TestPortfolio',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.20,
    ];

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($valid_data, $this->constraint);
  }

  /**
   * Tests validation with missing required fields.
   *
   * @covers ::validate
   * @covers ::validateRequiredFields
   */
  public function testValidateMissingRequiredFields(): void {
    $invalid_data = [
      'portfolio_name' => 'TestPortfolio',
      // Missing required fields
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->atLeastOnce())
      ->method('setParameter')
      ->willReturnSelf();
    $violation_builder->expects($this->atLeastOnce())
      ->method('addViolation');

    $this->context->expects($this->atLeastOnce())
      ->method('buildViolation')
      ->with($this->constraint->missingFieldMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_data, $this->constraint);
  }

  /**
   * Tests validation with invalid portfolio name.
   *
   * @covers ::validate
   * @covers ::validatePortfolioName
   */
  public function testValidateInvalidPortfolioName(): void {
    $invalid_data = [
      'portfolio_name' => 'Invalid-Name!', // Contains invalid characters
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.20,
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->once())
      ->method('setParameter')
      ->with('@name', 'Invalid-Name!')
      ->willReturnSelf();
    $violation_builder->expects($this->once())
      ->method('addViolation');

    $this->context->expects($this->once())
      ->method('buildViolation')
      ->with($this->constraint->invalidNameMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_data, $this->constraint);
  }

  /**
   * Tests validation with invalid portfolio type.
   *
   * @covers ::validate
   * @covers ::validatePortfolioType
   */
  public function testValidateInvalidPortfolioType(): void {
    $invalid_data = [
      'portfolio_name' => 'TestPortfolio',
      'strategy_type' => 'invalid_type',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.20,
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->exactly(2))
      ->method('setParameter')
      ->willReturnSelf();
    $violation_builder->expects($this->once())
      ->method('addViolation');

    $this->context->expects($this->once())
      ->method('buildViolation')
      ->with($this->constraint->invalidTypeMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_data, $this->constraint);
  }

  /**
   * Tests validation with invalid asset allocation.
   *
   * @covers ::validate
   * @covers ::validateAssetAllocation
   */
  public function testValidateInvalidAssetAllocation(): void {
    $invalid_data = [
      'portfolio_name' => 'TestPortfolio',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 70.0],
        'BTC' => ['allocation_percent' => 40.0], // Total = 110%
      ],
      'target_volatility' => 0.20,
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->once())
      ->method('setParameter')
      ->with('@total', '110.00')
      ->willReturnSelf();
    $violation_builder->expects($this->once())
      ->method('addViolation');

    $this->context->expects($this->once())
      ->method('buildViolation')
      ->with($this->constraint->invalidAllocationMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_data, $this->constraint);
  }

  /**
   * Tests validation with invalid risk parameters.
   *
   * @covers ::validate
   * @covers ::validateRiskParameters
   */
  public function testValidateInvalidRiskParameters(): void {
    $invalid_data = [
      'portfolio_name' => 'TestPortfolio',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.0],
        'BTC' => ['allocation_percent' => 40.0],
      ],
      'target_volatility' => 0.60, // Too high (max 0.50)
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->exactly(4))
      ->method('setParameter')
      ->willReturnSelf();
    $violation_builder->expects($this->once())
      ->method('addViolation');

    $this->context->expects($this->once())
      ->method('buildViolation')
      ->with($this->constraint->invalidRiskMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_data, $this->constraint);
  }

  /**
   * Tests validation with valid asset allocation within tolerance.
   *
   * @covers ::validate
   * @covers ::validateAssetAllocation
   */
  public function testValidateAssetAllocationWithinTolerance(): void {
    $valid_data = [
      'portfolio_name' => 'TestPortfolio',
      'strategy_type' => 'dual_crypto',
      'assets' => [
        'ETH' => ['allocation_percent' => 60.05],
        'BTC' => ['allocation_percent' => 39.95], // Total = 100.00%
      ],
      'target_volatility' => 0.20,
    ];

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($valid_data, $this->constraint);
  }

  /**
   * Tests validation with null or empty values.
   *
   * @covers ::validate
   */
  public function testValidateEmptyValues(): void {
    // Test with null
    $this->context->expects($this->never())
      ->method('buildViolation');
    
    $this->validator->validate(NULL, $this->constraint);

    // Test with empty array
    $this->validator->validate([], $this->constraint);

    // Test with non-array
    $this->validator->validate('invalid', $this->constraint);
  }

  /**
   * Tests supported portfolio types constant.
   *
   * @covers ::SUPPORTED_TYPES
   */
  public function testSupportedTypes(): void {
    $expected_types = ['dual_crypto', 'forex', 'equity', 'mixed'];
    $this->assertEquals($expected_types, PortfolioDataConstraintValidator::SUPPORTED_TYPES);
  }

  /**
   * Tests required fields constant.
   *
   * @covers ::REQUIRED_FIELDS
   */
  public function testRequiredFields(): void {
    $expected_fields = ['portfolio_name', 'strategy_type', 'assets', 'target_volatility'];
    $this->assertEquals($expected_fields, PortfolioDataConstraintValidator::REQUIRED_FIELDS);
  }

  /**
   * Tests risk ranges constant.
   *
   * @covers ::RISK_RANGES
   */
  public function testRiskRanges(): void {
    $risk_ranges = PortfolioDataConstraintValidator::RISK_RANGES;
    
    $this->assertArrayHasKey('target_volatility', $risk_ranges);
    $this->assertArrayHasKey('max_drawdown_limit', $risk_ranges);
    $this->assertArrayHasKey('portfolio_heat', $risk_ranges);
    $this->assertArrayHasKey('var_5pct', $risk_ranges);

    // Test specific ranges
    $this->assertEquals(0.05, $risk_ranges['target_volatility']['min']);
    $this->assertEquals(0.50, $risk_ranges['target_volatility']['max']);
  }

}