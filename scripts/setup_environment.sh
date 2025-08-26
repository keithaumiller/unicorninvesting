#!/bin/bash

# Unicorn Investing - Environment Setup
# This script sets up aliases and environment variables for the workspace

# Add to ~/.bashrc for persistent aliases
if [ -f ~/.bashrc ]; then
    # Check if our aliases are already in .bashrc
    if ! grep -q "# Unicorn Investing Aliases" ~/.bashrc; then
        echo "" >> ~/.bashrc
        echo "# Unicorn Investing Aliases" >> ~/.bashrc
        echo "alias drupal-start='/workspaces/unicorninvesting/scripts/startup_drupal.sh'" >> ~/.bashrc
        echo "alias drupal-status='sudo service apache2 status && sudo service mysql status'" >> ~/.bashrc
        echo "alias drupal-logs='sudo tail -20 /var/log/apache2/drupal_error.log'" >> ~/.bashrc
        echo "alias drupal-restart='sudo service apache2 restart && sudo service mysql restart'" >> ~/.bashrc
        echo "alias drupal-cd='cd /workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
        echo "alias unicorn-root='cd /workspaces/unicorninvesting'" >> ~/.bashrc
        echo "" >> ~/.bashrc
        echo "# Unicorn Investing Environment" >> ~/.bashrc
        echo "export UNICORN_ROOT='/workspaces/unicorninvesting'" >> ~/.bashrc
        echo "export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'" >> ~/.bashrc
        echo "export DRUPAL_URL='https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/'" >> ~/.bashrc
        
        echo "✅ Aliases added to ~/.bashrc"
        echo "💡 Run 'source ~/.bashrc' or restart your terminal to use them"
    else
        echo "✅ Aliases already exist in ~/.bashrc"
    fi
fi

# Set up aliases for current session
alias drupal-start='/workspaces/unicorninvesting/scripts/startup_drupal.sh'
alias drupal-status='sudo service apache2 status && sudo service mysql status'
alias drupal-logs='sudo tail -20 /var/log/apache2/drupal_error.log'
alias drupal-restart='sudo service apache2 restart && sudo service mysql restart'
alias drupal-cd='cd /workspaces/unicorninvesting/WebFrontend'
alias unicorn-root='cd /workspaces/unicorninvesting'

# Set environment variables for current session
export UNICORN_ROOT='/workspaces/unicorninvesting'
export DRUPAL_ROOT='/workspaces/unicorninvesting/WebFrontend'
export DRUPAL_URL='https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/'

echo ""
echo "🦄 Unicorn Investing Environment Ready!"
echo ""
echo "Available commands:"
echo "  drupal-start    - Start and validate Drupal system"
echo "  drupal-status   - Check Apache and MySQL status"
echo "  drupal-logs     - View recent Drupal error logs"
echo "  drupal-restart  - Restart Apache and MySQL services"
echo "  drupal-cd       - Change to Drupal root directory"
echo "  unicorn-root    - Change to project root directory"
echo ""
echo "Environment variables:"
echo "  UNICORN_ROOT = $UNICORN_ROOT"
echo "  DRUPAL_ROOT = $DRUPAL_ROOT"
echo "  DRUPAL_URL = $DRUPAL_URL"
