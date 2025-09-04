/**
 * @file
 * JavaScript for portfolio switching functionality.
 */

(function (Drupal) {
  'use strict';

  /**
   * Switch to a different portfolio.
   *
   * @param {string} portfolioId
   *   The ID of the portfolio to switch to.
   */
  function switchPortfolio(portfolioId) {
    if (portfolioId) {
      window.location.href = '/admin/metrics/dashboard?portfolio=' + encodeURIComponent(portfolioId);
    }
  }

  // Make function globally available for onclick handlers.
  window.switchPortfolio = switchPortfolio;

})(Drupal);
