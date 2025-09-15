# Data Warehouse Testing Results

## Purpose
This directory contains JSON test results from the data warehouse testing suite. Results are automatically generated and should not be manually edited.

## File Structure

### Timestamped Results
- `test_results_YYYYMMDD_HHMMSS.json` - Complete test execution results
- `summary_YYYYMMDD_HHMMSS.json` - Executive summary with success rates

### Current Results
- `latest_results.json` - Symlink to most recent complete results
- `latest_summary.json` - Symlink to most recent summary

## JSON Schema

### Complete Results (`test_results_*.json`)
```json
{
  "metadata": {
    "timestamp": "2025-09-15T13:16:26Z",
    "test_version": "1.0",
    "environment": "development",
    "python_version": "3.9.18",
    "pytest_version": "7.4.2"
  },
  "configuration": {
    "layer": "all",
    "connector": "all",
    "mode": "full",
    "quick_mode": false,
    "verbose": false
  },
  "summary": {
    "total_tests": 11,
    "passed": 5,
    "failed": 6,
    "skipped": 0,
    "success_rate": 45.45,
    "execution_time": 12.34
  },
  "layers": {
    "raw": {
      "total_tests": 7,
      "passed": 1,
      "failed": 6,
      "connectors": {
        "yahoo_finance": {
          "status": "PASSED",
          "execution_time": 2.3,
          "tests": [
            {
              "name": "test_eth_data_collection",
              "status": "PASSED",
              "duration": 1.2,
              "message": "Successfully collected ETH data"
            }
          ]
        },
        "fred": {
          "status": "FAILED",
          "execution_time": 1.5,
          "error": "Connection timeout",
          "tests": [
            {
              "name": "test_fred_api_connection",
              "status": "FAILED",
              "duration": 1.5,
              "error": "ConnectionError: Unable to connect to FRED API"
            }
          ]
        }
      }
    },
    "bronze": {
      "total_tests": 1,
      "passed": 1,
      "failed": 0,
      "status": "PASSED"
    },
    "silver": {
      "total_tests": 2,
      "passed": 2,
      "failed": 0,
      "status": "PASSED"
    },
    "gold": {
      "total_tests": 1,
      "passed": 1,
      "failed": 0,
      "status": "PASSED"
    }
  },
  "recommendations": [
    "Check FRED API key configuration",
    "Verify IBKR Gateway is running",
    "Review forex connector API credentials"
  ]
}
```

### Summary Results (`summary_*.json`)
```json
{
  "timestamp": "2025-09-15T13:16:26Z",
  "test_run_id": "20250915_131626",
  "overall_status": "PARTIAL_SUCCESS",
  "summary": {
    "total_tests": 11,
    "passed": 5,
    "failed": 6,
    "skipped": 0,
    "success_rate": 45.45
  },
  "layer_summary": {
    "raw": {"status": "FAILED", "success_rate": 14.3},
    "bronze": {"status": "PASSED", "success_rate": 100.0},
    "silver": {"status": "PASSED", "success_rate": 100.0},
    "gold": {"status": "PASSED", "success_rate": 100.0}
  },
  "critical_issues": [
    "FRED connector not working",
    "IBKR Gateway connection failed",
    "Forex API credentials missing"
  ],
  "next_actions": [
    "Configure FRED API key",
    "Start IBKR Gateway service",
    "Update forex API configuration"
  ]
}
```

## Usage

### Reading Results Programmatically
```python
import json
from datetime import datetime

# Load latest results
with open('latest_results.json', 'r') as f:
    results = json.load(f)

# Check overall success rate
success_rate = results['summary']['success_rate']
print(f"Test Success Rate: {success_rate:.1f}%")

# Get failed tests
for layer_name, layer_data in results['layers'].items():
    if 'connectors' in layer_data:
        for connector, details in layer_data['connectors'].items():
            if details['status'] == 'FAILED':
                print(f"Failed: {layer_name}/{connector} - {details['error']}")
```

### Historical Analysis
```bash
# List all test runs
ls -la test_results_*.json

# Compare success rates over time
grep -o '"success_rate": [0-9.]*' test_results_*.json | sort

# Find latest failing tests
jq '.layers.raw.connectors | to_entries[] | select(.value.status == "FAILED") | .key' latest_results.json
```

## Retention Policy

- **Last 30 days**: Keep all detailed results
- **Last 90 days**: Keep summary results only
- **Older than 90 days**: Automatic cleanup (if implemented)

## File Permissions
- All files in this directory should be readable by the test runner
- JSON files are automatically generated and should not be manually edited
- Directory is included in .gitignore to prevent accidental commits

## Monitoring Integration

These JSON results can be integrated with monitoring systems:
- **Grafana**: Import metrics for visualization
- **Prometheus**: Export success rates and timing metrics
- **Alerting**: Trigger alerts on test failures
- **CI/CD**: Use results for deployment gates

---

**Note**: This directory is automatically managed by the data warehouse testing suite. Manual modifications are not recommended.