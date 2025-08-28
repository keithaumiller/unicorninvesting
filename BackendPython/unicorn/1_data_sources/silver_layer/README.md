# Silver Layer - Cleaned & Normalized Data

## 🥈 Purpose

The **Silver Layer** contains cleaned, validated, and normalized data from the Bronze Layer. This layer standardizes data formats, fixes quality issues, and prepares data for business analytics.

## 🏗️ Architecture Role

**Data Flow**: Bronze Layer → **Silver Layer** → Gold Layer → Data Marts

The Silver Layer transforms raw data into **clean, standardized datasets** ready for analysis.

## 🎯 **Data Transformations**

### **1. Data Cleaning**
- **Missing Value Handling**: Imputation or removal strategies
- **Outlier Detection**: Statistical outlier identification and treatment
- **Duplicate Removal**: Deduplication based on business keys
- **Format Standardization**: Consistent date, time, and numeric formats

### **2. Data Validation**
- **Schema Validation**: Ensure data conforms to expected schemas
- **Business Rule Validation**: Apply business logic constraints
- **Data Type Validation**: Correct data type enforcement
- **Range Validation**: Value range and boundary checks

### **3. Data Normalization**
- **Symbol Standardization**: Consistent symbol naming (AAPL, EURUSD)
- **Currency Standardization**: Base currency conversion
- **Time Zone Standardization**: UTC standardization
- **Unit Standardization**: Consistent measurement units

### **4. Data Enrichment**
- **Calculated Fields**: Derived metrics and indicators
- **Lookup Tables**: Reference data joins
- **Market Data Alignment**: Synchronize different data sources
- **Feature Engineering**: Basic feature calculations

## 📊 **Silver Layer Schema Standards**

### **Market Data Schema**
```python
# Standardized market data format
market_data_schema = {
    "symbol": "string",           # Standardized symbol (e.g., "AAPL", "EURUSD")
    "timestamp": "datetime",      # UTC timestamp
    "open": "decimal",           # Opening price
    "high": "decimal",           # High price
    "low": "decimal",            # Low price
    "close": "decimal",          # Closing price
    "volume": "integer",         # Trading volume
    "source": "string",          # Data source identifier
    "currency": "string",        # Price currency (USD, EUR, etc.)
    "data_quality_score": "float", # Quality metric (0-1)
    "created_at": "datetime",    # Processing timestamp
    "version": "string"          # Schema version
}
```

### **Reference Data Schema**
```python
# Standardized reference data format
reference_data_schema = {
    "symbol": "string",          # Primary identifier
    "name": "string",           # Full name
    "exchange": "string",       # Exchange code
    "sector": "string",         # Business sector
    "industry": "string",       # Industry classification
    "currency": "string",       # Trading currency
    "country": "string",        # Country code
    "is_active": "boolean",     # Active trading status
    "listing_date": "date",     # Exchange listing date
    "delisting_date": "date",   # Delisting date (if applicable)
    "last_updated": "datetime"  # Last update timestamp
}
```

## 🔧 **Data Processing Pipeline**

### **1. Bronze to Silver ETL**
```python
def bronze_to_silver_pipeline(bronze_data_path, silver_output_path):
    """
    Process bronze layer data to silver layer
    """
    # Load bronze data
    raw_data = load_bronze_data(bronze_data_path)
    
    # Apply cleaning transformations
    cleaned_data = apply_cleaning_rules(raw_data)
    
    # Validate data quality
    validated_data = validate_data_quality(cleaned_data)
    
    # Standardize schema
    standardized_data = standardize_schema(validated_data)
    
    # Add metadata
    enriched_data = add_processing_metadata(standardized_data)
    
    # Write to silver layer
    write_silver_data(enriched_data, silver_output_path)
    
    # Generate quality report
    generate_quality_report(enriched_data, silver_output_path)
```

### **2. Data Quality Rules**
```python
def apply_cleaning_rules(data):
    """Apply comprehensive data cleaning rules"""
    
    # Remove duplicates
    data = data.drop_duplicates(subset=['symbol', 'timestamp'])
    
    # Handle missing values
    data['volume'] = data['volume'].fillna(0)
    data['close'] = data['close'].fillna(method='ffill')  # Forward fill prices
    
    # Fix outliers (prices outside 3 standard deviations)
    for price_col in ['open', 'high', 'low', 'close']:
        data = remove_price_outliers(data, price_col, std_threshold=3)
    
    # Standardize symbols
    data['symbol'] = data['symbol'].apply(standardize_symbol)
    
    # Convert timestamps to UTC
    data['timestamp'] = pd.to_datetime(data['timestamp']).dt.tz_convert('UTC')
    
    return data
```

### **3. Data Validation Framework**
```python
class SilverDataValidator:
    def __init__(self):
        self.validation_rules = [
            self.validate_required_fields,
            self.validate_data_types,
            self.validate_business_rules,
            self.validate_data_ranges
        ]
    
    def validate_data_quality(self, data):
        """Run all validation rules"""
        results = {
            "passed": True,
            "errors": [],
            "warnings": [],
            "quality_score": 1.0
        }
        
        for rule in self.validation_rules:
            rule_result = rule(data)
            if not rule_result["passed"]:
                results["passed"] = False
                results["errors"].extend(rule_result["errors"])
            
            results["warnings"].extend(rule_result["warnings"])
        
        # Calculate overall quality score
        results["quality_score"] = self.calculate_quality_score(results)
        
        return results
    
    def validate_required_fields(self, data):
        """Validate presence of required fields"""
        required_fields = ['symbol', 'timestamp', 'close']
        missing_fields = [field for field in required_fields if field not in data.columns]
        
        return {
            "passed": len(missing_fields) == 0,
            "errors": [f"Missing required field: {field}" for field in missing_fields],
            "warnings": []
        }
    
    def validate_business_rules(self, data):
        """Validate business logic rules"""
        errors = []
        warnings = []
        
        # Price validation
        invalid_prices = data[data['close'] <= 0]
        if not invalid_prices.empty:
            errors.append(f"Found {len(invalid_prices)} records with invalid prices")
        
        # Volume validation
        negative_volume = data[data['volume'] < 0]
        if not negative_volume.empty:
            errors.append(f"Found {len(negative_volume)} records with negative volume")
        
        # Future date validation
        future_dates = data[data['timestamp'] > datetime.utcnow()]
        if not future_dates.empty:
            warnings.append(f"Found {len(future_dates)} records with future dates")
        
        return {
            "passed": len(errors) == 0,
            "errors": errors,
            "warnings": warnings
        }
```

## 📊 **Quality Monitoring**

### **Data Quality Metrics**
```python
def calculate_quality_metrics(silver_data):
    """Calculate comprehensive quality metrics"""
    
    metrics = {
        "completeness": {
            "total_records": len(silver_data),
            "complete_records": len(silver_data.dropna()),
            "completeness_rate": len(silver_data.dropna()) / len(silver_data)
        },
        "accuracy": {
            "invalid_prices": len(silver_data[silver_data['close'] <= 0]),
            "outliers_detected": count_outliers(silver_data),
            "accuracy_rate": calculate_accuracy_rate(silver_data)
        },
        "consistency": {
            "schema_compliance": validate_schema_compliance(silver_data),
            "data_type_compliance": validate_data_types(silver_data),
            "business_rule_compliance": validate_business_rules(silver_data)
        },
        "timeliness": {
            "latest_data_age": calculate_data_age(silver_data),
            "processing_lag": calculate_processing_lag(silver_data),
            "data_freshness_score": calculate_freshness_score(silver_data)
        }
    }
    
    return metrics
```

### **Quality Dashboard**
```python
def generate_quality_dashboard(silver_layer_path):
    """Generate data quality dashboard"""
    
    # Load recent silver data
    recent_data = load_recent_silver_data(silver_layer_path, days=7)
    
    # Calculate quality metrics
    quality_metrics = calculate_quality_metrics(recent_data)
    
    # Generate visualizations
    quality_plots = {
        "completeness_trend": plot_completeness_trend(recent_data),
        "accuracy_distribution": plot_accuracy_distribution(recent_data),
        "processing_lag_chart": plot_processing_lag(recent_data),
        "quality_score_trend": plot_quality_score_trend(recent_data)
    }
    
    # Create dashboard
    dashboard = create_quality_dashboard(quality_metrics, quality_plots)
    
    return dashboard
```

## 🔍 **Querying Silver Layer**

### **Standard Query Interface**
```python
class SilverDataQuery:
    def __init__(self, silver_layer_path):
        self.silver_path = silver_layer_path
    
    def get_market_data(self, symbols, date_range, data_type="minute"):
        """Get standardized market data"""
        
        query = {
            "symbols": symbols,
            "start_date": date_range[0],
            "end_date": date_range[1],
            "data_type": data_type
        }
        
        # Load and filter data
        data = self.load_silver_data(query)
        
        # Apply additional quality filters
        filtered_data = self.apply_quality_filters(data)
        
        return filtered_data
    
    def get_reference_data(self, symbols=None, exchange=None):
        """Get standardized reference data"""
        
        ref_data = self.load_reference_data()
        
        if symbols:
            ref_data = ref_data[ref_data['symbol'].isin(symbols)]
        
        if exchange:
            ref_data = ref_data[ref_data['exchange'] == exchange]
        
        return ref_data
```

## 🚨 **Best Practices**

### **1. Data Quality First**
- ✅ Implement comprehensive validation rules
- ✅ Monitor quality metrics continuously
- ✅ Set quality thresholds and alerts
- ✅ Document all transformation logic

### **2. Schema Management**
- ✅ Version all schemas
- ✅ Implement backward compatibility
- ✅ Document schema changes
- ✅ Test schema migrations thoroughly

### **3. Performance Optimization**
- ✅ Partition data by date and symbol
- ✅ Use efficient file formats (Parquet)
- ✅ Implement incremental processing
- ✅ Cache frequently accessed data

### **4. Error Handling**
- ✅ Graceful handling of data quality issues
- ✅ Comprehensive logging and alerting
- ✅ Data lineage tracking
- ✅ Recovery procedures for failed jobs

## 🦄 **Unicorn Platform Integration**

Silver layer supports the platform by providing:
- **Clean Data**: High-quality data for algorithmic trading
- **Standardization**: Consistent data formats across all analyses
- **Real-time Analytics**: Fast access to clean, current data
- **Risk Management**: Validated data for risk calculations

---

*The Silver Layer ensures data quality and standardization - the foundation for reliable algorithmic trading!*
