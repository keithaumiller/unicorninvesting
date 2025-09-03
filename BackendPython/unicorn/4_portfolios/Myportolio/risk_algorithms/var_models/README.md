# Value at Risk (VaR) Models

## Overview

Calculate portfolio Value at Risk using different methodologies for risk assessment.

## Components

- **var_calculator.py** - VaR calculation methods

## Features

- Historical VaR calculation
- Parametric VaR (normal distribution)
- Multiple confidence levels (95%, 99%)

## Usage

```python
from var_calculator import VaRCalculator

var_calc = VaRCalculator()
historical_var = var_calc.historical_var(returns, 0.95)
parametric_var = var_calc.parametric_var(returns, 0.95)
```
