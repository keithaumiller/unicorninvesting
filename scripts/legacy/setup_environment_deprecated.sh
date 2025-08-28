#!/bin/bash

# ❌ DEPRECATED - Use unicorn_environment.sh instead
# This script has been replaced by /workspaces/unicorninvesting/scripts/unicorn_environment.sh
# 
# The new script provides:
# - All functionality from this setup_environment.sh
# - Complete system health checks from health_check.sh
# - Better alias management
# - Comprehensive environment validation
#
# Usage: ./scripts/unicorn_environment.sh [--setup-only|--check-only|--help]

echo "❌ This script is DEPRECATED"
echo ""
echo "✅ Use the new comprehensive script instead:"
echo "   ./scripts/unicorn_environment.sh --setup-only"
echo ""
echo "Available options:"
echo "   --setup-only    Setup environment only (replaces this script)"
echo "   --check-only    Health checks only"
echo "   --help          Show help"
echo "   (no options)    Run both setup and health checks"
echo ""
echo "🔄 Redirecting to new script..."
echo ""

# Redirect to the new script with setup-only flag
exec /workspaces/unicorninvesting/scripts/unicorn_environment.sh --setup-only
