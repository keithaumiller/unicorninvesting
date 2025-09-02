<?php

namespace Drupal\Tests\unicornmetrics\Unit\Plugin\Validation\Constraint;

use Drupal\Tests\UnitTestBase;
use Drupal\unicornmetrics\Plugin\Validation\Constraint\ApiResponseConstraint;
use Drupal\unicornmetrics\Plugin\Validation\Constraint\ApiResponseConstraintValidator;
use Symfony\Component\Validator\Context\ExecutionContext;
use Symfony\Component\Validator\Violation\ConstraintViolationBuilder;

/**
 * Unit tests for ApiResponseConstraintValidator.
 *
 * @group unicornmetrics
 * @group validation
 * @coversDefaultClass \Drupal\unicornmetrics\Plugin\Validation\Constraint\ApiResponseConstraintValidator
 */
class ApiResponseConstraintValidatorTest extends UnitTestBase {

  /**
   * The constraint validator under test.
   *
   * @var \Drupal\unicornmetrics\Plugin\Validation\Constraint\ApiResponseConstraintValidator
   */
  protected $validator;

  /**
   * The constraint being tested.
   *
   * @var \Drupal\unicornmetrics\Plugin\Validation\Constraint\ApiResponseConstraint
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

    $this->validator = new ApiResponseConstraintValidator();
    $this->constraint = new ApiResponseConstraint();
    
    $this->context = $this->createMock(ExecutionContext::class);
    $this->validator->initialize($this->context);
  }

  /**
   * Tests validation with valid portfolio status response.
   *
   * @covers ::validate
   * @covers ::determineResponseType
   * @covers ::validateFieldTypes
   * @covers ::validateValueRanges
   */
  public function testValidateValidPortfolioStatusResponse(): void {
    $valid_response = [
      'portfolio_name' => 'TestPortfolio',
      'status' => 'active',
      'target_volatility' => 0.20,
      'last_updated' => '2024-01-01 12:00:00',
    ];

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($valid_response, $this->constraint);
  }

  /**
   * Tests validation with valid risk metrics response.
   *
   * @covers ::validate
   * @covers ::determineResponseType
   */
  public function testValidateValidRiskMetricsResponse(): void {
    $valid_response = [
      'current_drawdown' => -0.05,
      'portfolio_volatility' => 0.25,
      'var_5pct' => 0.04,
      'risk_score' => 0.3,
    ];

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($valid_response, $this->constraint);
  }

  /**
   * Tests validation with valid ETH algorithm status response.
   *
   * @covers ::validate
   * @covers ::determineResponseType
   */
  public function testValidateValidEthAlgorithmStatusResponse(): void {
    $valid_response = [
      'risk_algorithm' => [
        'available' => TRUE,
        'status' => 'active',
      ],
      'trading_algorithm' => [
        'available' => TRUE,
        'status' => 'active',
      ],
      'integration_status' => 'operational',
    ];

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($valid_response, $this->constraint);
  }

  /**
   * Tests validation with malformed JSON string.
   *
   * @covers ::validate
   */
  public function testValidateMalformedJson(): void {
    $malformed_json = '{"invalid": json}';

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->once())
      ->method('addViolation');

    $this->context->expects($this->once())
      ->method('buildViolation')
      ->with($this->constraint->malformedResponseMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($malformed_json, $this->constraint);
  }

  /**
   * Tests validation with invalid field types.
   *
   * @covers ::validate
   * @covers ::validateFieldTypes
   */
  public function testValidateInvalidFieldTypes(): void {
    $invalid_response = [
      'portfolio_name' => 123, // Should be string
      'status' => 'active',
      'target_volatility' => 'invalid', // Should be float
      'last_updated' => '2024-01-01',
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->atLeastOnce())
      ->method('setParameter')
      ->willReturnSelf();
    $violation_builder->expects($this->atLeastOnce())
      ->method('addViolation');

    $this->context->expects($this->atLeastOnce())
      ->method('buildViolation')
      ->with($this->constraint->invalidTypeMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_response, $this->constraint);
  }

  /**
   * Tests validation with values out of range.
   *
   * @covers ::validate
   * @covers ::validateValueRanges
   */
  public function testValidateValuesOutOfRange(): void {
    $invalid_response = [
      'current_drawdown' => -2.0, // Too low (min -1.0)
      'portfolio_volatility' => 0.25,
      'var_5pct' => 1.5, // Too high (max 1.0)
      'risk_score' => 0.3,
    ];

    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->atLeastOnce())
      ->method('setParameter')
      ->willReturnSelf();
    $violation_builder->expects($this->atLeastOnce())
      ->method('addViolation');

    $this->context->expects($this->atLeastOnce())
      ->method('buildViolation')
      ->with($this->constraint->valueOutOfRangeMessage)
      ->willReturn($violation_builder);

    $this->validator->validate($invalid_response, $this->constraint);
  }

  /**
   * Tests validation with valid JSON string.
   *
   * @covers ::validate
   */
  public function testValidateValidJsonString(): void {
    $valid_json = json_encode([
      'portfolio_name' => 'TestPortfolio',
      'status' => 'active',
      'target_volatility' => 0.20,
      'last_updated' => '2024-01-01 12:00:00',
    ]);

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($valid_json, $this->constraint);
  }

  /**
   * Tests validation with non-array, non-string input.
   *
   * @covers ::validate
   */
  public function testValidateNonArrayInput(): void {
    $violation_builder = $this->createMock(ConstraintViolationBuilder::class);
    $violation_builder->expects($this->once())
      ->method('addViolation');

    $this->context->expects($this->once())
      ->method('buildViolation')
      ->with($this->constraint->malformedResponseMessage)
      ->willReturn($violation_builder);

    $this->validator->validate(123, $this->constraint);
  }

  /**
   * Tests validation with empty values.
   *
   * @covers ::validate
   */
  public function testValidateEmptyValues(): void {
    $this->context->expects($this->never())
      ->method('buildViolation');

    // Test with null
    $this->validator->validate(NULL, $this->constraint);

    // Test with empty string
    $this->validator->validate('', $this->constraint);

    // Test with empty array
    $this->validator->validate([], $this->constraint);
  }

  /**
   * Tests determineResponseType method with unknown response.
   *
   * @covers ::determineResponseType
   */
  public function testDetermineResponseTypeUnknown(): void {
    $unknown_response = [
      'unknown_field' => 'value',
    ];

    // Should not trigger violations for unknown response types
    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($unknown_response, $this->constraint);
  }

  /**
   * Tests validation accepts integers as floats.
   *
   * @covers ::validateFieldTypes
   */
  public function testValidateIntegerAsFloat(): void {
    $response_with_integers = [
      'current_drawdown' => 0, // Integer that should be accepted as float
      'portfolio_volatility' => 25, // Integer that should be converted to float
      'var_5pct' => 4,
      'risk_score' => 1,
    ];

    $this->context->expects($this->never())
      ->method('buildViolation');

    $this->validator->validate($response_with_integers, $this->constraint);
  }

  /**
   * Tests field types constants.
   *
   * @covers ::FIELD_TYPES
   */
  public function testFieldTypesConstants(): void {
    $field_types = ApiResponseConstraintValidator::FIELD_TYPES;
    
    $this->assertArrayHasKey('portfolio_status', $field_types);
    $this->assertArrayHasKey('risk_metrics', $field_types);
    $this->assertArrayHasKey('eth_algorithm_status', $field_types);

    // Test portfolio_status fields
    $portfolio_fields = $field_types['portfolio_status'];
    $this->assertEquals('string', $portfolio_fields['portfolio_name']);
    $this->assertEquals('float', $portfolio_fields['target_volatility']);
  }

  /**
   * Tests value ranges constants.
   *
   * @covers ::VALUE_RANGES
   */
  public function testValueRangesConstants(): void {
    $value_ranges = ApiResponseConstraintValidator::VALUE_RANGES;
    
    $this->assertArrayHasKey('target_volatility', $value_ranges);
    $this->assertArrayHasKey('current_drawdown', $value_ranges);
    $this->assertArrayHasKey('risk_score', $value_ranges);

    // Test specific ranges
    $this->assertEquals(0.0, $value_ranges['target_volatility']['min']);
    $this->assertEquals(1.0, $value_ranges['target_volatility']['max']);
    $this->assertEquals(-1.0, $value_ranges['current_drawdown']['min']);
    $this->assertEquals(0.0, $value_ranges['current_drawdown']['max']);
  }

}