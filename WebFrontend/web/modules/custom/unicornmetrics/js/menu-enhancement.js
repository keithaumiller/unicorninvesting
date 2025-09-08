/**
 * Enhanced menu functionality for Unicorn Metrics
 */

(function ($, Drupal) {
  'use strict';

  Drupal.behaviors.unicornMenuEnhancement = {
    attach: function (context, settings) {
      
      // Add active state indicators to menu items
      $('.unicorn-menu-item', context).each(function() {
        var $item = $(this);
        var href = $item.find('a').attr('href');
        var currentPath = window.location.pathname;
        
        // Highlight current page and parent pages
        if (currentPath === href || currentPath.indexOf(href + '/') === 0) {
          $item.addClass('menu-item--active-trail');
        }
      });
      
      // Add hover effects and tooltips for menu items
      $('.portfolio-management, .algorithm-management, .trading-settings', context).hover(
        function() {
          $(this).addClass('menu-hover-effect');
        },
        function() {
          $(this).removeClass('menu-hover-effect');
        }
      );
      
      // Add click tracking for menu analytics (if needed)
      $('.unicorn-menu-item a', context).on('click', function() {
        var menuItem = $(this).closest('.unicorn-menu-item').attr('class');
        var href = $(this).attr('href');
        
        // Optional: Track menu usage analytics
        if (typeof gtag !== 'undefined') {
          gtag('event', 'menu_click', {
            'event_category': 'navigation',
            'event_label': menuItem + ' -> ' + href
          });
        }
      });
      
      // Enhance action buttons with confirmation dialogs where appropriate
      $('.refresh-action', context).on('click', function(e) {
        if (!confirm('This will refresh all portfolio data from the backend. Continue?')) {
          e.preventDefault();
          return false;
        }
      });
      
      $('.backtest-action', context).on('click', function(e) {
        if (!confirm('Running a backtest may take several minutes. Continue?')) {
          e.preventDefault();
          return false;
        }
      });
      
      // Add keyboard navigation support
      $('.unicorn-menu-item a', context).on('keydown', function(e) {
        // Navigate menu items with arrow keys
        if (e.keyCode === 40) { // Down arrow
          e.preventDefault();
          $(this).closest('.unicorn-menu-item').next().find('a').focus();
        } else if (e.keyCode === 38) { // Up arrow
          e.preventDefault();
          $(this).closest('.unicorn-menu-item').prev().find('a').focus();
        }
      });
      
      // Add loading states for menu actions
      $('.refresh-action, .backtest-action, .analysis-action', context).on('click', function() {
        var $button = $(this);
        var originalText = $button.text();
        
        $button.text('Loading...').prop('disabled', true);
        
        // Re-enable after a reasonable timeout (page should redirect)
        setTimeout(function() {
          $button.text(originalText).prop('disabled', false);
        }, 5000);
      });
    }
  };

  // Add custom CSS classes for enhanced styling
  Drupal.behaviors.unicornMenuStyling = {
    attach: function (context, settings) {
      // Add gradient background to main trading dashboard link
      $('.trading-main', context).each(function() {
        $(this).wrap('<div class="trading-main-wrapper"></div>');
      });
      
      // Add notification badges (if needed for alerts)
      $('.algorithm-management', context).each(function() {
        // Example: Add badge for model performance alerts
        // $(this).append('<span class="menu-badge">!</span>');
      });
    }
  };

})(jQuery, Drupal);
