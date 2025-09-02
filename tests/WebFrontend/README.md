# WebFrontend Testing Suite

This directory contains tests for the Drupal 11 web frontend components.

## 🎯 **Purpose**

Tests the web interface, dashboard functionality, and user experience components of the Unicorn Investing platform.

## 📁 **Test Files**

### **Basic Validation**
- `test_basic_validation.py` - Core frontend functionality tests (needs implementation)

### **Homepage & Navigation**  
- `simple_homepage_test.py` - Homepage loading and basic functionality (placeholder)

### **Dashboard Testing**
- `test_forecasting_dashboard.py` - ETH forecasting dashboard validation (placeholder)

## 🌐 **Test Coverage**

### **Current Status**
- ✅ **Directory Structure**: Properly organized to mirror `/WebFrontend/`
- ⚠️ **Implementation**: Most files are placeholders (empty)
- 🎯 **Priority**: Medium - Important for user interface validation

### **Planned Coverage**
- **Homepage**: Loading, navigation, authentication
- **Dashboard**: Data display, charts, real-time updates
- **ETH Forecasting**: Algorithm results visualization
- **User Interface**: Responsive design, accessibility
- **API Integration**: Frontend-backend communication

## 🚀 **Usage**

```bash
# Run all WebFrontend tests  
cd /workspaces/unicorninvesting/tests
pytest WebFrontend/ -v

# Run specific test files
python WebFrontend/simple_homepage_test.py

# Include in comprehensive testing
./run_comprehensive_tests.sh
```

## 🔗 **Related Components**

- **Main Frontend Code**: `/WebFrontend/`
- **Drupal Configuration**: `/WebFrontend/web/`
- **Database Tests**: `/tests/database/`
- **System Integration**: `/tests/system/`

## 🛠️ **Testing Framework**

### **Drupal Testing**
- PHP unit tests for custom modules
- JavaScript tests for frontend components
- Integration tests for data flow
- Performance tests for page loading

### **Dashboard Testing**
- Real-time data display validation
- Chart rendering and accuracy
- User interaction testing
- Mobile responsiveness

## 📊 **Quality Metrics**

### **Performance Targets**
- Page load time: <3 seconds
- Dashboard updates: <1 second  
- Mobile responsiveness: 100%
- Accessibility compliance: WCAG 2.1 AA

### **Functionality Targets**
- Homepage availability: 99.9%
- Dashboard accuracy: 100%
- User authentication: 100%
- Data synchronization: <5 second lag

## 📝 **Development Notes**

Frontend tests should validate:
- Proper Drupal 11 functionality
- Integration with Python backend APIs
- Real-time ETH data display
- User experience and accessibility
- Cross-browser compatibility
