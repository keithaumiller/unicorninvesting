/**
 * @file
 * Test setup configuration for Jest testing framework.
 * 
 * Configures testing environment for UnicornMetrics frontend JavaScript tests.
 */

import '@testing-library/jest-dom';

// Mock global Drupal object
global.Drupal = {
  behaviors: {},
  attachBehaviors: jest.fn(),
  detachBehaviors: jest.fn(),
  url: jest.fn((path) => `http://localhost${path}`),
  formatPlural: jest.fn(),
  t: jest.fn((str) => str),
  locale: {
    pluralFormula: jest.fn(),
  },
};

// Mock jQuery
global.$ = global.jQuery = jest.fn(() => ({
  ready: jest.fn(),
  on: jest.fn(),
  off: jest.fn(),
  trigger: jest.fn(),
  find: jest.fn(),
  addClass: jest.fn(),
  removeClass: jest.fn(),
  toggleClass: jest.fn(),
  attr: jest.fn(),
  removeAttr: jest.fn(),
  data: jest.fn(),
  val: jest.fn(),
  text: jest.fn(),
  html: jest.fn(),
  append: jest.fn(),
  prepend: jest.fn(),
  remove: jest.fn(),
  hide: jest.fn(),
  show: jest.fn(),
  fadeIn: jest.fn(),
  fadeOut: jest.fn(),
  animate: jest.fn(),
  css: jest.fn(),
  each: jest.fn(),
  click: jest.fn(),
}));

// Mock fetch for API testing
global.fetch = jest.fn(() =>
  Promise.resolve({
    ok: true,
    status: 200,
    statusText: 'OK',
    json: () => Promise.resolve({
      status: 'success',
      data: {}
    }),
    text: () => Promise.resolve(''),
    headers: new Headers(),
  })
);

// Mock console methods for cleaner test output
global.console = {
  ...console,
  warn: jest.fn(),
  error: jest.fn(),
  log: jest.fn(),
};

// Mock localStorage
const localStorageMock = {
  getItem: jest.fn(),
  setItem: jest.fn(),
  removeItem: jest.fn(),
  clear: jest.fn(),
};
global.localStorage = localStorageMock;

// Mock sessionStorage
const sessionStorageMock = {
  getItem: jest.fn(),
  setItem: jest.fn(),
  removeItem: jest.fn(),
  clear: jest.fn(),
};
global.sessionStorage = sessionStorageMock;

// Mock window.location
delete window.location;
window.location = {
  href: 'http://localhost/',
  origin: 'http://localhost',
  protocol: 'http:',
  host: 'localhost',
  hostname: 'localhost',
  port: '',
  pathname: '/',
  search: '',
  hash: '',
  assign: jest.fn(),
  replace: jest.fn(),
  reload: jest.fn(),
};

// Mock ResizeObserver
global.ResizeObserver = jest.fn().mockImplementation(() => ({
  observe: jest.fn(),
  unobserve: jest.fn(),
  disconnect: jest.fn(),
}));

// Mock IntersectionObserver
global.IntersectionObserver = jest.fn().mockImplementation(() => ({
  observe: jest.fn(),
  unobserve: jest.fn(),
  disconnect: jest.fn(),
}));

// Test utilities
global.testUtils = {
  /**
   * Create a mock Drupal behavior.
   */
  createMockBehavior: (name, attachFn, detachFn) => {
    const behavior = {
      attach: attachFn || jest.fn(),
      detach: detachFn || jest.fn(),
    };
    global.Drupal.behaviors[name] = behavior;
    return behavior;
  },

  /**
   * Reset all mocks.
   */
  resetMocks: () => {
    jest.clearAllMocks();
    fetch.mockClear();
    localStorage.clear();
    sessionStorage.clear();
  },

  /**
   * Mock successful API response.
   */
  mockApiSuccess: (data) => {
    fetch.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: () => Promise.resolve({ status: 'success', data }),
    });
  },

  /**
   * Mock API error response.
   */
  mockApiError: (status = 500, message = 'Server Error') => {
    fetch.mockRejectedValueOnce(new Error(message));
  },
};

// Setup DOM environment
beforeEach(() => {
  document.body.innerHTML = '';
  global.testUtils.resetMocks();
});

afterEach(() => {
  jest.restoreAllMocks();
});