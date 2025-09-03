# Basic Risk Management

## Overview

Simple risk controls for position sizing and portfolio exposure limits.

## Components

- **basic_risk.py** - Basic risk manager implementation

## Features

- Maximum position size limits
- Total portfolio exposure controls
- Simple validation methods

## Usage

```python
from basic_risk import BasicRiskManager

risk_manager = BasicRiskManager()
risk_manager.validate_position(0.15)  # Check 15% position
risk_manager.validate_portfolio(0.85)  # Check 85% exposure
```
