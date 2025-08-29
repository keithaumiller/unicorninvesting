"""
Unicorn Investing Alpha Models Package

Organized by asset class for scalable development:
- ETH/: Ethereum and crypto alpha models
- FOREX/: Foreign exchange models  
- CRYPTO/: General cryptocurrency models
- EQUITIES/: Stock and equity models
- shared/: Common utilities and templates
- utils/: Shared utility functions
- examples/: Example implementations
"""

__version__ = "2.1.0"
__author__ = "Unicorn Investing Team"

# Import main asset modules
try:
    from .ETH import models as eth_models
    from .FOREX import models as forex_models
    from .CRYPTO import models as crypto_models
    from .EQUITIES import models as equity_models
    from .shared import utils as shared_utils
except ImportError:
    # Handle missing modules gracefully
    pass

__all__ = [
    'eth_models',
    'forex_models', 
    'crypto_models',
    'equity_models',
    'shared_utils'
]
