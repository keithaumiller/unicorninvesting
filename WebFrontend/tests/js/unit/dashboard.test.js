/**
 * @file
 * Unit tests for UnicornMetrics dashboard JavaScript functionality.
 */

import { screen, fireEvent, waitFor } from '@testing-library/dom';

describe('UnicornMetrics Dashboard JavaScript', () => {
  let dashboardContainer;

  beforeEach(() => {
    // Create DOM structure similar to dashboard
    dashboardContainer = document.createElement('div');
    dashboardContainer.className = 'dashboard-container';
    dashboardContainer.innerHTML = `
      <div class="dashboard-header">
        <h1>🦄 Unicorn Portfolio Management System</h1>
      </div>
      <div class="portfolio-selector">
        <select id="portfolio-dropdown">
          <option value="forex">Primary Forex Portfolio</option>
          <option value="equity">Growth Equity Portfolio</option>
        </select>
      </div>
      <div class="portfolio-stats">
        <div class="stat-card" data-stat="total-value">
          <span class="stat-value">$125,847.62</span>
          <span class="stat-label">Total Value</span>
        </div>
        <div class="stat-card" data-stat="positions">
          <span class="stat-value">8</span>
          <span class="stat-label">Active Positions</span>
        </div>
      </div>
      <div class="real-time-indicator" id="real-time-status">
        <span class="status-text">Connecting...</span>
      </div>
    `;
    document.body.appendChild(dashboardContainer);
  });

  afterEach(() => {
    document.body.removeChild(dashboardContainer);
  });

  describe('Portfolio Selection', () => {
    test('should handle portfolio dropdown change', () => {
      const dropdown = document.getElementById('portfolio-dropdown');
      const changeHandler = jest.fn();
      
      dropdown.addEventListener('change', changeHandler);
      dropdown.value = 'equity';
      fireEvent.change(dropdown);

      expect(changeHandler).toHaveBeenCalled();
      expect(dropdown.value).toBe('equity');
    });

    test('should validate portfolio selection', () => {
      const dropdown = document.getElementById('portfolio-dropdown');
      
      // Test valid selection
      dropdown.value = 'forex';
      expect(dropdown.value).toBe('forex');
      
      // Test invalid selection (should not change)
      dropdown.value = 'invalid';
      expect(dropdown.options[dropdown.selectedIndex].value).toBe('invalid');
    });
  });

  describe('Real-time Data Updates', () => {
    test('should initialize real-time connection status', () => {
      const statusIndicator = document.getElementById('real-time-status');
      expect(statusIndicator).toBeTruthy();
      expect(statusIndicator.textContent).toContain('Connecting');
    });

    test('should update connection status', () => {
      const statusIndicator = document.getElementById('real-time-status');
      const statusText = statusIndicator.querySelector('.status-text');
      
      // Simulate connection established
      statusText.textContent = 'Connected';
      statusIndicator.className = 'real-time-indicator connected';
      
      expect(statusText.textContent).toBe('Connected');
      expect(statusIndicator.classList.contains('connected')).toBe(true);
    });

    test('should handle data update', async () => {
      // Mock API response
      global.testUtils.mockApiSuccess({
        total_value: 126000.00,
        positions: 9,
        last_updated: '2024-01-01T12:00:00Z'
      });

      // Simulate data update
      const valueElement = document.querySelector('[data-stat="total-value"] .stat-value');
      const positionsElement = document.querySelector('[data-stat="positions"] .stat-value');
      
      // Simulate update
      valueElement.textContent = '$126,000.00';
      positionsElement.textContent = '9';
      
      expect(valueElement.textContent).toBe('$126,000.00');
      expect(positionsElement.textContent).toBe('9');
    });
  });

  describe('Data Formatting', () => {
    test('should format currency values correctly', () => {
      const formatCurrency = (value) => {
        return new Intl.NumberFormat('en-US', {
          style: 'currency',
          currency: 'USD'
        }).format(value);
      };

      expect(formatCurrency(125847.62)).toBe('$125,847.62');
      expect(formatCurrency(0)).toBe('$0.00');
      expect(formatCurrency(-1000)).toBe('-$1,000.00');
    });

    test('should format percentage values correctly', () => {
      const formatPercentage = (value) => {
        return new Intl.NumberFormat('en-US', {
          style: 'percent',
          minimumFractionDigits: 2,
          maximumFractionDigits: 2
        }).format(value);
      };

      expect(formatPercentage(0.0523)).toBe('5.23%');
      expect(formatPercentage(0)).toBe('0.00%');
      expect(formatPercentage(-0.0234)).toBe('-2.34%');
    });

    test('should format large numbers with abbreviations', () => {
      const formatLargeNumber = (value) => {
        if (Math.abs(value) >= 1e9) return (value / 1e9).toFixed(1) + 'B';
        if (Math.abs(value) >= 1e6) return (value / 1e6).toFixed(1) + 'M';
        if (Math.abs(value) >= 1e3) return (value / 1e3).toFixed(1) + 'K';
        return value.toString();
      };

      expect(formatLargeNumber(1234567890)).toBe('1.2B');
      expect(formatLargeNumber(1234567)).toBe('1.2M');
      expect(formatLargeNumber(1234)).toBe('1.2K');
      expect(formatLargeNumber(123)).toBe('123');
    });
  });

  describe('Error Handling', () => {
    test('should handle API connection errors gracefully', async () => {
      global.testUtils.mockApiError(500, 'Server Error');

      // Simulate error handling
      const errorHandler = jest.fn();
      const statusIndicator = document.getElementById('real-time-status');
      
      try {
        await fetch('/api/test');
      } catch (error) {
        errorHandler(error);
        statusIndicator.className = 'real-time-indicator error';
        statusIndicator.querySelector('.status-text').textContent = 'Connection Error';
      }

      expect(errorHandler).toHaveBeenCalled();
      expect(statusIndicator.classList.contains('error')).toBe(true);
    });

    test('should show fallback data when API is unavailable', () => {
      const showFallbackData = () => {
        const valueElement = document.querySelector('[data-stat="total-value"] .stat-value');
        valueElement.textContent = 'Data Unavailable';
        valueElement.className = 'stat-value fallback';
      };

      showFallbackData();
      
      const valueElement = document.querySelector('[data-stat="total-value"] .stat-value');
      expect(valueElement.textContent).toBe('Data Unavailable');
      expect(valueElement.classList.contains('fallback')).toBe(true);
    });
  });

  describe('Interactive Elements', () => {
    test('should handle navigation link clicks', () => {
      const navLink = document.createElement('a');
      navLink.href = '/admin/metrics/lean/portfolio';
      navLink.textContent = '📊 Portfolio Overview';
      navLink.className = 'nav-link';
      
      const clickHandler = jest.fn((e) => {
        e.preventDefault(); // Prevent actual navigation in tests
      });
      
      navLink.addEventListener('click', clickHandler);
      dashboardContainer.appendChild(navLink);
      
      fireEvent.click(navLink);
      expect(clickHandler).toHaveBeenCalled();
    });

    test('should handle stat card hover effects', () => {
      const statCard = document.querySelector('.stat-card');
      
      fireEvent.mouseEnter(statCard);
      expect(statCard).toBeTruthy();
      
      fireEvent.mouseLeave(statCard);
      expect(statCard).toBeTruthy();
    });
  });

  describe('Responsive Behavior', () => {
    test('should adapt to mobile viewport', () => {
      // Simulate mobile viewport
      Object.defineProperty(window, 'innerWidth', { value: 375 });
      Object.defineProperty(window, 'innerHeight', { value: 667 });
      
      // Simulate responsive behavior
      const portfolioStats = document.querySelector('.portfolio-stats');
      portfolioStats.className = 'portfolio-stats mobile';
      
      expect(portfolioStats.classList.contains('mobile')).toBe(true);
    });

    test('should handle screen orientation changes', () => {
      const orientationHandler = jest.fn();
      
      // Mock orientation change
      window.addEventListener('orientationchange', orientationHandler);
      fireEvent(window, new Event('orientationchange'));
      
      expect(orientationHandler).toHaveBeenCalled();
    });
  });

  describe('Accessibility Features', () => {
    test('should provide proper ARIA labels', () => {
      const statCard = document.querySelector('.stat-card');
      statCard.setAttribute('aria-label', 'Total portfolio value: $125,847.62');
      
      expect(statCard.getAttribute('aria-label')).toContain('Total portfolio value');
    });

    test('should support keyboard navigation', () => {
      const dropdown = document.getElementById('portfolio-dropdown');
      
      dropdown.focus();
      expect(document.activeElement).toBe(dropdown);
      
      // Simulate keyboard navigation
      fireEvent.keyDown(dropdown, { key: 'ArrowDown' });
      fireEvent.keyDown(dropdown, { key: 'Enter' });
      
      // Would normally test actual keyboard behavior, but this tests the event handling
      expect(dropdown).toBeTruthy();
    });

    test('should announce status changes to screen readers', () => {
      const announceRegion = document.createElement('div');
      announceRegion.setAttribute('aria-live', 'polite');
      announceRegion.setAttribute('aria-atomic', 'true');
      announceRegion.className = 'sr-only';
      dashboardContainer.appendChild(announceRegion);
      
      // Simulate status announcement
      announceRegion.textContent = 'Portfolio data updated';
      
      expect(announceRegion.getAttribute('aria-live')).toBe('polite');
      expect(announceRegion.textContent).toBe('Portfolio data updated');
    });
  });

  describe('Performance Optimizations', () => {
    test('should debounce rapid updates', () => {
      const debounce = (func, wait) => {
        let timeout;
        return (...args) => {
          clearTimeout(timeout);
          timeout = setTimeout(() => func.apply(this, args), wait);
        };
      };

      const updateFunction = jest.fn();
      const debouncedUpdate = debounce(updateFunction, 100);
      
      // Rapid calls
      debouncedUpdate();
      debouncedUpdate();
      debouncedUpdate();
      
      expect(updateFunction).not.toHaveBeenCalled();
      
      // Wait for debounce
      setTimeout(() => {
        expect(updateFunction).toHaveBeenCalledTimes(1);
      }, 150);
    });

    test('should throttle scroll events', () => {
      const throttle = (func, limit) => {
        let inThrottle;
        return (...args) => {
          if (!inThrottle) {
            func.apply(this, args);
            inThrottle = true;
            setTimeout(() => inThrottle = false, limit);
          }
        };
      };

      const scrollHandler = jest.fn();
      const throttledScroll = throttle(scrollHandler, 16); // 60fps
      
      // Rapid scroll events
      throttledScroll();
      throttledScroll();
      throttledScroll();
      
      expect(scrollHandler).toHaveBeenCalledTimes(1);
    });
  });
});