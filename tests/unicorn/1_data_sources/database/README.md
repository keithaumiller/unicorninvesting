# Database Testing Suite

This directory contains tests for database connectivity, data persistence, and storage validation for the data sources layer.

## 💾 **Purpose**

Tests database functionality specifically related to data collection and storage:
- Data source database connections
- Market data persistence and retrieval
- Data quality storage and tracking
- Performance metrics database operations
- Historical data archival and cleanup

## 📁 **Planned Test Structure**

### **Database Connectivity**
- `test_data_source_db_connectivity.py` - Database connection validation
- `test_market_data_persistence.py` - OHLCV data storage/retrieval
- `test_data_quality_tracking.py` - Quality metrics storage

### **Performance Testing**
- `test_bulk_data_insertion.py` - Large dataset storage performance
- `test_historical_data_queries.py` - Data retrieval performance
- `test_database_cleanup.py` - Data archival and cleanup

### **Data Integrity**
- `test_data_validation_storage.py` - Data validation rule storage
- `test_metadata_consistency.py` - Metadata storage and consistency
- `test_transaction_integrity.py` - ACID compliance testing

## 🎯 **Current Status**

- ✅ **Directory Created**: Ready for database test implementation
- ⚠️ **Implementation**: All test files are placeholders (need creation)
- 🎯 **Priority**: Medium - Important for data persistence validation

## 🔗 **Related Components**

### **Data Sources Integration**
- **IBKR Connector**: Tests should validate IBKR data storage
- **Market Data**: Tests should validate OHLCV data persistence
- **Data Quality**: Tests should validate quality metrics storage

### **Database Schema**
- **Tables**: market_data, data_quality_metrics, collection_metadata
- **Indexes**: timestamp, symbol, quality_score indexes
- **Relationships**: Foreign keys between data and metadata tables

## 🚀 **Usage**

```bash
# Future usage - when tests are implemented
cd /workspaces/unicorninvesting/tests
pytest unicorn/1_data_sources/database/ -v

# Run specific database test categories
pytest unicorn/1_data_sources/database/ -m connectivity
pytest unicorn/1_data_sources/database/ -m performance
pytest unicorn/1_data_sources/database/ -m integrity
```

## 📊 **Test Categories**

### **Unit Tests** (`@pytest.mark.unit`)
- Database connection establishment
- Single record CRUD operations
- Query result validation
- Schema validation

### **Integration Tests** (`@pytest.mark.integration`)
- End-to-end data pipeline storage
- Multi-table transaction testing
- Real data storage validation
- Performance benchmarking

### **Performance Tests** (`@pytest.mark.performance`)
- Bulk data insertion (>1000 records)
- Query performance optimization
- Index efficiency validation
- Memory usage monitoring

## 🛠️ **Database Requirements**

### **MySQL Configuration**
- Database: `unicorn_testing`
- Tables: Mirrored from production schema
- Test data: Isolated from production
- Cleanup: Automatic test data cleanup

### **Connection Settings**
- Host: localhost (or test database)
- User: test_user (limited permissions)
- SSL: Required for production-like testing
- Timeout: Appropriate timeouts for testing

## 📈 **Performance Targets**

### **Data Insertion**
- Single record: <10ms
- Bulk insertion (1000 records): <5 seconds
- Concurrent writes: >100 ops/second

### **Data Retrieval**
- Single record lookup: <5ms
- Range queries (1 day data): <500ms
- Complex aggregations: <2 seconds

### **Data Quality**
- Storage consistency: 100%
- Retrieval accuracy: 100%
- Transaction integrity: 100%

## 📝 **Implementation Notes**

Database tests should validate:
- Proper data type storage and retrieval
- Index performance and query optimization
- Transaction handling and rollback scenarios
- Data integrity constraints and validation
- Concurrent access and locking behavior
- Backup and recovery procedures

## 🔄 **Development Workflow**

1. **Create Test Database Schema**: Mirror production tables
2. **Implement Connection Tests**: Validate basic connectivity
3. **Add CRUD Tests**: Test basic operations
4. **Performance Testing**: Benchmark database operations
5. **Integration Testing**: End-to-end data pipeline validation
6. **Cleanup Procedures**: Automatic test data cleanup
