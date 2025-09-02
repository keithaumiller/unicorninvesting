/**
 * @file
 * End-to-end tests for UnicornMetrics user journeys.
 * 
 * Tests complete user workflows using Cypress for realistic
 * browser automation and interaction testing.
 */

describe('UnicornMetrics End-to-End User Journey', () => {
  beforeEach(() => {
    // Login as authorized user
    cy.visit('/user/login');
    cy.get('#edit-name').type('admin');
    cy.get('#edit-pass').type('admin');
    cy.get('#edit-submit').click();
    
    // Verify login successful
    cy.url().should('not.include', '/user/login');
  });

  describe('Dashboard Navigation', () => {
    it('should display main dashboard with all key elements', () => {
      cy.visit('/admin/metrics');
      
      // Verify page loads successfully
      cy.get('.dashboard-header').should('be.visible');
      cy.contains('Unicorn Portfolio Management System').should('be.visible');
      
      // Verify version information
      cy.contains('Version').should('be.visible');
      cy.contains('Last Updated').should('be.visible');
      
      // Verify portfolio stats
      cy.get('.portfolio-stats').should('be.visible');
      cy.get('.stat-card').should('have.length.greaterThan', 0);
      
      // Verify navigation table
      cy.get('.lean-nav-table').should('be.visible');
      cy.get('.lean-nav-table tbody tr').should('have.length.greaterThan', 0);
    });

    it('should navigate through all portfolio pages', () => {
      cy.visit('/admin/metrics');
      
      // Navigate to portfolio overview
      cy.contains('📊 Portfolio Overview').click();
      cy.url().should('include', '/admin/metrics/lean/portfolio');
      cy.contains('Portfolio Value').should('be.visible');
      
      // Navigate to holdings
      cy.contains('📈 View Securities & Holdings').click();
      cy.url().should('include', '/admin/metrics/lean/holdings');
      cy.contains('Holdings').should('be.visible');
      cy.get('.holdings-table').should('be.visible');
      
      // Navigate to performance
      cy.visit('/admin/metrics');
      cy.contains('📊 Performance Metrics').click();
      cy.url().should('include', '/admin/metrics/lean/performance');
      cy.contains('Performance').should('be.visible');
      
      // Navigate to algorithms
      cy.visit('/admin/metrics');
      cy.contains('🤖 Managing Algorithm').click();
      cy.url().should('include', '/admin/metrics/lean/algorithms');
      cy.contains('Algorithm').should('be.visible');
    });

    it('should handle portfolio selection changes', () => {
      cy.visit('/admin/metrics');
      
      // Test default portfolio (forex)
      cy.contains('Primary Forex Portfolio').should('be.visible');
      
      // Test portfolio parameter change
      cy.visit('/admin/metrics?portfolio=equity');
      cy.contains('Growth Equity Portfolio').should('be.visible');
      
      // Test invalid portfolio (should default to forex)
      cy.visit('/admin/metrics?portfolio=invalid');
      cy.contains('Primary Forex Portfolio').should('be.visible');
    });
  });

  describe('Portfolio Management Workflow', () => {
    it('should complete full portfolio analysis workflow', () => {
      // Start at main dashboard
      cy.visit('/admin/metrics');
      cy.get('.dashboard-header').should('be.visible');
      
      // Step 1: Review portfolio overview
      cy.contains('📊 Portfolio Overview').click();
      cy.get('.portfolio-overview-grid').should('be.visible');
      cy.get('.portfolio-card').should('have.length.greaterThan', 0);
      
      // Verify key metrics are displayed
      cy.contains('Portfolio Value').should('be.visible');
      cy.contains('Cash Position').should('be.visible');
      cy.contains('Unrealized P&L').should('be.visible');
      
      // Step 2: Analyze individual holdings
      cy.contains('📈 View Securities & Holdings').click();
      cy.get('.holdings-table').should('be.visible');
      cy.get('.holdings-table thead th').should('contain', 'Symbol');
      cy.get('.holdings-table thead th').should('contain', 'Market Value');
      cy.get('.holdings-table tbody tr').should('have.length.greaterThan', 0);
      
      // Step 3: Review performance metrics
      cy.visit('/admin/metrics/lean/performance');
      cy.get('.performance-grid').should('be.visible');
      cy.contains('Return Metrics').should('be.visible');
      cy.contains('Risk Metrics').should('be.visible');
      cy.contains('Alpha Generation').should('be.visible');
      
      // Step 4: Check algorithm status
      cy.visit('/admin/metrics/lean/algorithms');
      cy.get('.algorithms-grid').should('be.visible');
      cy.contains('Current Algorithm').should('be.visible');
      cy.contains('Performance').should('be.visible');
      
      // Step 5: Return to dashboard
      cy.contains('🏠 Dashboard Home').click();
      cy.url().should('include', '/admin/metrics');
      cy.contains('Unicorn Portfolio Management System').should('be.visible');
    });

    it('should handle algorithm performance analysis', () => {
      cy.visit('/admin/metrics/lean/algorithms');
      
      // Access performance analysis
      cy.contains('📊 Performance Analysis').click();
      cy.url().should('include', '/admin/metrics/lean/algorithms/performance');
      
      // Verify performance metrics
      cy.contains('Algorithm Performance').should('be.visible');
      cy.contains('Signal Quality').should('be.visible');
      cy.contains('Financial Impact').should('be.visible');
      cy.contains('Direction Accuracy').should('be.visible');
      cy.contains('Total Alpha Generated').should('be.visible');
    });

    it('should display backtest results', () => {
      cy.visit('/admin/metrics/lean/algorithms');
      
      // Access backtest results
      cy.contains('🔬 Backtest Results').click();
      cy.url().should('include', '/admin/metrics/lean/backtest');
      
      // Verify backtest information
      cy.contains('Backtest Results').should('be.visible');
      cy.contains('Test Period').should('be.visible');
      cy.contains('Overall Performance').should('be.visible');
      cy.contains('Trade Statistics').should('be.visible');
    });
  });

  describe('Real-time Data Integration', () => {
    it('should display real-time portfolio updates', () => {
      cy.visit('/admin/metrics/lean/portfolio');
      
      // Verify initial data load
      cy.get('.portfolio-overview-grid').should('be.visible');
      cy.get('.metric-value').should('have.length.greaterThan', 0);
      
      // Check for last updated timestamp
      cy.contains('Last Updated').should('be.visible');
      
      // Verify data formatting
      cy.get('.metric-value').each(($el) => {
        const text = $el.text();
        // Should contain currency or percentage formatting
        expect(text).to.match(/[\$%]|^\d+$/);
      });
    });

    it('should handle data refresh scenarios', () => {
      cy.visit('/admin/metrics');
      
      // Refresh page to simulate data update
      cy.reload();
      
      // Verify page still loads correctly
      cy.get('.dashboard-header').should('be.visible');
      cy.contains('Unicorn Portfolio Management System').should('be.visible');
      
      // Verify data consistency
      cy.get('.stat-card').should('have.length.greaterThan', 0);
      cy.get('.stat-value').each(($el) => {
        expect($el.text()).to.not.be.empty;
      });
    });
  });

  describe('Responsive Design Testing', () => {
    const viewports = [
      { device: 'iphone-6', width: 375, height: 667 },
      { device: 'ipad-2', width: 768, height: 1024 },
      { device: 'macbook-15', width: 1440, height: 900 },
    ];

    viewports.forEach((viewport) => {
      it(`should be responsive on ${viewport.device}`, () => {
        cy.viewport(viewport.width, viewport.height);
        cy.visit('/admin/metrics');
        
        // Verify page loads and key elements are visible
        cy.get('.dashboard-header').should('be.visible');
        cy.contains('Unicorn Portfolio Management System').should('be.visible');
        
        // Verify navigation is accessible
        cy.get('.lean-nav-table').should('be.visible');
        
        // Test navigation on smaller screens
        if (viewport.width < 768) {
          // Mobile-specific tests
          cy.get('.portfolio-stats').should('be.visible');
        }
        
        // Test a few key pages
        cy.visit('/admin/metrics/lean/portfolio');
        cy.get('.portfolio-overview-grid').should('be.visible');
        
        cy.visit('/admin/metrics/lean/holdings');
        cy.get('.holdings-table').should('be.visible');
      });
    });
  });

  describe('Error Handling and Edge Cases', () => {
    it('should handle unauthorized access gracefully', () => {
      // Logout first
      cy.visit('/user/logout');
      
      // Try to access protected page
      cy.visit('/admin/metrics');
      
      // Should redirect to login or show access denied
      cy.url().should('satisfy', (url) => {
        return url.includes('/user/login') || url.includes('/access-denied');
      });
    });

    it('should handle malformed portfolio parameters', () => {
      cy.visit('/admin/metrics');
      
      // Test various invalid parameters
      const invalidParams = [
        '?portfolio=',
        '?portfolio=nonexistent',
        '?portfolio=<script>alert("xss")</script>',
        '?portfolio=' + 'x'.repeat(1000), // Very long string
      ];
      
      invalidParams.forEach((param) => {
        cy.visit('/admin/metrics' + param);
        
        // Should still load successfully with default portfolio
        cy.get('.dashboard-header').should('be.visible');
        cy.contains('Primary Forex Portfolio').should('be.visible');
      });
    });

    it('should maintain functionality when JavaScript is partially loaded', () => {
      cy.visit('/admin/metrics');
      
      // Verify basic functionality without full JS enhancement
      cy.get('.lean-nav-table a').first().click();
      
      // Should still navigate successfully
      cy.url().should('include', '/admin/metrics/lean/');
    });
  });

  describe('Performance Testing', () => {
    it('should load pages within acceptable time limits', () => {
      const pages = [
        '/admin/metrics',
        '/admin/metrics/lean/portfolio',
        '/admin/metrics/lean/holdings',
        '/admin/metrics/lean/performance',
        '/admin/metrics/lean/algorithms',
      ];
      
      pages.forEach((page) => {
        const startTime = Date.now();
        
        cy.visit(page);
        cy.get('.lean-dashboard-header, .dashboard-header').should('be.visible');
        
        cy.then(() => {
          const loadTime = Date.now() - startTime;
          expect(loadTime).to.be.lessThan(5000); // 5 second limit
        });
      });
    });

    it('should handle rapid navigation without issues', () => {
      const pages = [
        '/admin/metrics',
        '/admin/metrics/lean/portfolio',
        '/admin/metrics/lean/holdings',
        '/admin/metrics/lean/performance',
      ];
      
      // Rapidly navigate between pages
      pages.forEach((page) => {
        cy.visit(page);
        cy.get('.lean-dashboard-header, .dashboard-header').should('be.visible');
      });
      
      // Return to dashboard - should still work
      cy.visit('/admin/metrics');
      cy.contains('Unicorn Portfolio Management System').should('be.visible');
    });
  });

  describe('Accessibility Testing', () => {
    it('should meet basic accessibility standards', () => {
      cy.visit('/admin/metrics');
      
      // Test keyboard navigation
      cy.get('body').tab();
      cy.focused().should('be.visible');
      
      // Test heading structure
      cy.get('h1').should('exist');
      cy.get('h1').should('contain', 'Unicorn Portfolio Management System');
      
      // Test link accessibility
      cy.get('a').should('have.attr', 'href');
      cy.get('a').each(($link) => {
        expect($link.text().trim()).to.not.be.empty;
      });
      
      // Test table accessibility
      cy.get('.lean-nav-table').within(() => {
        cy.get('th').should('exist');
        cy.get('td').should('exist');
      });
    });

    it('should provide meaningful page titles', () => {
      const pages = [
        { url: '/admin/metrics', titleContent: 'Metrics' },
        { url: '/admin/metrics/lean/portfolio', titleContent: 'Portfolio' },
        { url: '/admin/metrics/lean/holdings', titleContent: 'Holdings' },
      ];
      
      pages.forEach((page) => {
        cy.visit(page.url);
        cy.title().should('include', page.titleContent);
      });
    });
  });
});