# Kelly Criterion Position Sizing

## Overview

Optimal position sizing based on the Kelly Criterion formula for maximizing long-term growth.

## Components

- **kelly_criterion.py** - Kelly fraction calculation

## Features

- Win rate and win/loss ratio analysis
- Optimal position size calculation
- Conservative Kelly fraction capping

## Usage

```python
from kelly_criterion import KellyCriterion

kelly = KellyCriterion()
position_size = kelly.calculate_kelly_fraction(0.6, 0.08, 0.05)
```
