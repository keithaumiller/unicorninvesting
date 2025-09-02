# WebFrontend - Unicorn Investing Platform

This directory contains the Drupal 11 web frontend for the Unicorn Investing platform, including the integrated test management interface.

## 🧪 Test Management Interface

The WebFrontend now includes a comprehensive web-based testing interface that integrates with the backend Python testing framework:

### Test Dashboards

1. **Basic Test Dashboard** (`test_dashboard.php`)
   - Simple, functional interface for test execution
   - Real-time test results and system status
   - Integrated with existing test scripts

2. **Advanced Test Dashboard** (`advanced_test_dashboard.html`)
   - Modern, feature-rich interface with tabbed navigation
   - Real-time system monitoring and test history
   - Enhanced UI with responsive design
   - Live test execution tracking

### RESTful Test API (`api/test_api.php`)

Comprehensive API backend providing:
- Test execution endpoints
- System status monitoring
- Test log management
- Historical test data

### Key Features

- **Real-time Test Execution**: Run individual tests with live progress feedback
- **System Health Monitoring**: Continuous monitoring of system status and resources
- **Test History & Analytics**: Track test results and success rates over time
- **Responsive Design**: Optimized for desktop and mobile viewing
- **API Integration**: RESTful API for programmatic access

### Quick Start

1. Start the PHP development server:
   ```bash
   cd WebFrontend
   php -S 0.0.0.0:8080
   ```

2. Access the test dashboards:
   - Basic Dashboard: `http://localhost:8080/test_dashboard.php`
   - Advanced Dashboard: `http://localhost:8080/advanced_test_dashboard.html`
   - API Endpoint: `http://localhost:8080/api/test_api.php`

3. Run tests directly from the web interface

### Available Tests

- **Comprehensive Test Suite**: Complete testing with detailed reporting
- **Quick Test Suite**: Essential tests for rapid validation
- **System Validation**: Health checks and component validation
- **Architecture Test**: System architecture compliance validation
- **Frontend Basic Validation**: Basic frontend functionality tests

### Screenshots

![Test Dashboard Overview](https://github.com/user-attachments/assets/0094448b-0060-421f-bc8a-c61eac4725f0)
*Basic Test Dashboard Interface*

![Advanced Test Dashboard](https://github.com/user-attachments/assets/12ebb347-42bc-47c7-a923-cb09ce3d6c39)
*Advanced Test Dashboard with Real-time Monitoring*

![Test Execution Results](https://github.com/user-attachments/assets/74a792ee-4b56-4dbf-b432-21fc13c200b7)
*Live Test Execution with Results Display*

### Documentation

See [WEB_TESTING_INTEGRATION.md](WEB_TESTING_INTEGRATION.md) for complete documentation on the testing integration implementation.

## 🌐 Drupal 11 Frontend

The main Drupal 11 installation is located in the `web/` directory and provides the primary user interface for the Unicorn Investing platform.

### Structure

```
WebFrontend/
├── README.md                        # This file
├── WEB_TESTING_INTEGRATION.md       # Testing integration documentation
├── test_dashboard.php               # Basic test dashboard
├── advanced_test_dashboard.html     # Advanced test dashboard
├── api/
│   └── test_api.php                # RESTful test management API
├── web/                            # Drupal 11 installation
│   ├── index.php                   # Drupal entry point
│   ├── core/                       # Drupal core files
│   ├── modules/                    # Custom and contrib modules
│   ├── themes/                     # Custom and contrib themes
│   └── sites/                      # Site configuration
├── vendor/                         # Composer dependencies
└── composer.json                   # Composer configuration
```

### Development Workflow

1. **Testing Interface**: Use the web dashboards for test management
2. **Drupal Development**: Standard Drupal development in `web/` directory
3. **API Integration**: Use the test API for automated testing integration

### Requirements

- PHP 8.0+ with required extensions
- MySQL/MariaDB for Drupal database
- Python 3.9+ for test execution
- Composer for dependency management