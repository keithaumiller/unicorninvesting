# 1_raw - Raw Data Staging Layer

## 🏪 Purpose

The **1_raw** layer provides temporary storage for data validation and initial processing before moving to the 2_bronze layer. This area acts as a buffer and quality gate for incoming data.

## 🏗️ Architecture Role

**Data Flow**: External APIs → **1_raw/connectors** → **1_raw** → 2_bronze → 3_silver → 4_gold

The 1_raw layer ensures **data quality and validation** before permanent storage.

## 📂 Directory Structure

```
1_raw/
├── connectors/           # 🔌 Data source API integrations
│   ├── AlphaVantageMinuteData.py
│   ├── YahooFinanceMinuteData.py
│   └── YAHOO_FINANCE_INTEGRATION_GUIDE.md
├── transformscripts/     # ⚙️ Raw data processing scripts
├── market_data/         # Raw market data feeds (runtime)
├── news_feeds/          # Unprocessed news data (runtime)
├── economic_data/       # Raw economic indicators (runtime)
└── README.md           # This documentation
```

## 🎯 **Raw Data Functions**

### **1. Data Ingestion (connectors/)**
External API integrations and data source connections:

- **Schema Validation**: Ensure data conforms to expected structure
- **Format Validation**: Verify file formats and encoding
- **Completeness Checks**: Detect missing required fields
- **Range Validation**: Check data values are within expected ranges

### **2. Duplicate Detection**
Identify and handle duplicate data:

- **Exact Duplicates**: Identical records from same source
- **Near Duplicates**: Similar records with minor differences
- **Cross-Source Duplicates**: Same data from multiple sources
- **Temporal Duplicates**: Same data received multiple times

### **3. Data Standardization**
Initial normalization and cleaning:

- **Date/Time Standardization**: Convert to standard timezone (UTC)
- **Symbol Normalization**: Standardize ticker symbols and identifiers
- **Currency Conversion**: Note source currencies for later conversion
- **Unit Standardization**: Ensure consistent measurement units

### **4. Quarantine Management**
Handle problematic data:

- **Quality Issues**: Data below quality thresholds
- **Schema Violations**: Data not matching expected schema
- **Suspicious Data**: Potential data quality issues requiring review
- **Processing Errors**: Failed transformations or validations

## 🔧 **Staging Process Framework**

### **1. Staging Workflow**
```python
class StagingProcessor:
    """Process data through staging area"""
    
    def __init__(self, staging_config):
        self.config = staging_config
        self.validator = DataValidator()
        self.quarantine = QuarantineManager()
        self.logger = StagingLogger()
    
    def process_incoming_data(self, raw_data, source_info):
        """Process incoming data through staging"""
        
        staging_id = self.generate_staging_id()
        
        try:
            self.logger.info(f"🏪 Starting staging process: {staging_id}")
            
            # Stage 1: Basic validation
            self.logger.info("✅ Performing basic validation...")
            basic_validation = self.validator.basic_validation(raw_data)
            
            if not basic_validation["passed"]:
                self.quarantine.quarantine_data(
                    raw_data, "basic_validation_failed", 
                    basic_validation["errors"], staging_id
                )
                return {"status": "quarantined", "reason": "basic_validation"}
            
            # Stage 2: Schema validation
            self.logger.info("📋 Validating schema...")
            schema_validation = self.validator.validate_schema(raw_data, source_info["expected_schema"])
            
            if not schema_validation["passed"]:
                self.quarantine.quarantine_data(
                    raw_data, "schema_validation_failed",
                    schema_validation["errors"], staging_id
                )
                return {"status": "quarantined", "reason": "schema_validation"}
            
            # Stage 3: Business rule validation
            self.logger.info("🔍 Applying business rules...")
            business_validation = self.validator.validate_business_rules(raw_data)
            
            if not business_validation["passed"]:
                # Some business rule failures can be corrected
                if business_validation["correctable"]:
                    corrected_data = self.validator.apply_corrections(raw_data, business_validation)
                    self.logger.info(f"🔧 Applied {len(business_validation['corrections'])} corrections")
                    processed_data = corrected_data
                else:
                    self.quarantine.quarantine_data(
                        raw_data, "business_rule_violation",
                        business_validation["errors"], staging_id
                    )
                    return {"status": "quarantined", "reason": "business_rules"}
            else:
                processed_data = raw_data
            
            # Stage 4: Duplicate detection
            self.logger.info("🔄 Checking for duplicates...")
            duplicate_check = self.detect_duplicates(processed_data, source_info)
            
            if duplicate_check["duplicates_found"]:
                deduplicated_data = self.handle_duplicates(processed_data, duplicate_check)
                self.logger.info(f"🗑️ Removed {duplicate_check['duplicate_count']} duplicates")
                processed_data = deduplicated_data
            
            # Stage 5: Quality scoring
            self.logger.info("📊 Calculating quality score...")
            quality_score = self.calculate_quality_score(processed_data, source_info)
            
            if quality_score < self.config["minimum_quality_threshold"]:
                self.quarantine.quarantine_data(
                    processed_data, "quality_below_threshold",
                    f"Quality score {quality_score} below threshold {self.config['minimum_quality_threshold']}",
                    staging_id
                )
                return {"status": "quarantined", "reason": "quality_threshold"}
            
            # Stage 6: Prepare for bronze layer
            self.logger.info("📦 Preparing for bronze layer...")
            bronze_ready_data = self.prepare_for_bronze(processed_data, source_info, quality_score)
            
            # Stage 7: Move to bronze layer
            bronze_result = self.move_to_bronze(bronze_ready_data, staging_id)
            
            self.logger.info(f"✅ Staging completed successfully: {staging_id}")
            
            return {
                "status": "success",
                "staging_id": staging_id,
                "quality_score": quality_score,
                "records_processed": len(processed_data),
                "bronze_location": bronze_result["location"]
            }
            
        except Exception as e:
            self.logger.error(f"❌ Staging failed: {staging_id} - {e}")
            self.quarantine.quarantine_data(
                raw_data, "processing_error", str(e), staging_id
            )
            return {"status": "failed", "error": str(e)}
        
        finally:
            # Cleanup staging area
            self.cleanup_staging_files(staging_id)
```

### **2. Data Validation Rules**
```python
class DataValidator:
    """Comprehensive data validation for staging"""
    
    def __init__(self):
        self.validation_rules = self.load_validation_rules()
    
    def basic_validation(self, data):
        """Basic structure and format validation"""
        
        validation_result = {
            "passed": True,
            "errors": [],
            "warnings": []
        }
        
        # Check if data is empty
        if data is None or len(data) == 0:
            validation_result["passed"] = False
            validation_result["errors"].append("Empty dataset")
            return validation_result
        
        # Check basic structure
        if not hasattr(data, 'columns'):
            validation_result["passed"] = False
            validation_result["errors"].append("Invalid data structure - not a DataFrame")
            return validation_result
        
        # Check minimum columns
        required_columns = self.validation_rules.get("required_columns", [])
        missing_columns = [col for col in required_columns if col not in data.columns]
        
        if missing_columns:
            validation_result["passed"] = False
            validation_result["errors"].append(f"Missing required columns: {missing_columns}")
        
        return validation_result
    
    def validate_schema(self, data, expected_schema):
        """Validate data against expected schema"""
        
        validation_result = {
            "passed": True,
            "errors": [],
            "warnings": []
        }
        
        # Check column presence
        expected_columns = set(expected_schema.keys())
        actual_columns = set(data.columns)
        
        missing_columns = expected_columns - actual_columns
        extra_columns = actual_columns - expected_columns
        
        if missing_columns:
            validation_result["passed"] = False
            validation_result["errors"].append(f"Missing columns: {missing_columns}")
        
        if extra_columns:
            validation_result["warnings"].append(f"Unexpected columns: {extra_columns}")
        
        # Check data types
        for column, expected_type in expected_schema.items():
            if column in data.columns:
                if not self.validate_column_type(data[column], expected_type):
                    validation_result["errors"].append(
                        f"Column '{column}' has incorrect type. Expected: {expected_type}"
                    )
        
        return validation_result
    
    def validate_business_rules(self, data):
        """Apply business-specific validation rules"""
        
        validation_result = {
            "passed": True,
            "errors": [],
            "warnings": [],
            "correctable": False,
            "corrections": []
        }
        
        # Financial data specific rules
        if 'price' in data.columns or any(col.endswith('_price') for col in data.columns):
            price_validation = self.validate_price_data(data)
            validation_result["errors"].extend(price_validation["errors"])
            
            if price_validation["correctable_issues"]:
                validation_result["correctable"] = True
                validation_result["corrections"].extend(price_validation["corrections"])
        
        # Volume validation
        if 'volume' in data.columns:
            volume_validation = self.validate_volume_data(data)
            validation_result["errors"].extend(volume_validation["errors"])
        
        # Date/time validation
        if any(col in data.columns for col in ['timestamp', 'date', 'datetime']):
            datetime_validation = self.validate_datetime_data(data)
            validation_result["errors"].extend(datetime_validation["errors"])
        
        # Symbol validation
        if 'symbol' in data.columns:
            symbol_validation = self.validate_symbol_data(data)
            validation_result["errors"].extend(symbol_validation["errors"])
        
        validation_result["passed"] = len(validation_result["errors"]) == 0
        
        return validation_result
    
    def validate_price_data(self, data):
        """Validate financial price data"""
        
        price_columns = [col for col in data.columns if 'price' in col.lower()]
        
        validation = {
            "errors": [],
            "correctable_issues": [],
            "corrections": []
        }
        
        for col in price_columns:
            # Check for negative prices
            negative_prices = data[data[col] < 0]
            if not negative_prices.empty:
                validation["errors"].append(f"Negative prices found in {col}: {len(negative_prices)} records")
            
            # Check for zero prices
            zero_prices = data[data[col] == 0]
            if not zero_prices.empty:
                validation["correctable_issues"].append(f"Zero prices in {col}: {len(zero_prices)} records")
                validation["corrections"].append(f"Replace zero prices in {col} with NaN for interpolation")
            
            # Check for extremely high prices (outliers)
            price_mean = data[col].mean()
            price_std = data[col].std()
            outlier_threshold = price_mean + (5 * price_std)
            
            outliers = data[data[col] > outlier_threshold]
            if not outliers.empty:
                validation["correctable_issues"].append(f"Potential outliers in {col}: {len(outliers)} records")
                validation["corrections"].append(f"Flag outliers in {col} for manual review")
        
        return validation
```

### **3. Quarantine Management**
```python
class QuarantineManager:
    """Manage quarantined data that failed validation"""
    
    def __init__(self, quarantine_path):
        self.quarantine_path = quarantine_path
        self.quarantine_log = QuarantineLog()
    
    def quarantine_data(self, data, reason, details, staging_id):
        """Move problematic data to quarantine"""
        
        quarantine_record = {
            "quarantine_id": self.generate_quarantine_id(),
            "staging_id": staging_id,
            "timestamp": datetime.utcnow(),
            "reason": reason,
            "details": details,
            "record_count": len(data) if hasattr(data, '__len__') else 1,
            "data_sample": self.get_data_sample(data),
            "remediation_status": "pending_review"
        }
        
        # Save quarantined data
        quarantine_path = self.save_quarantined_data(data, quarantine_record)
        quarantine_record["file_path"] = quarantine_path
        
        # Log quarantine event
        self.quarantine_log.record_quarantine(quarantine_record)
        
        # Send alert if critical
        if self.is_critical_quarantine(reason):
            self.send_quarantine_alert(quarantine_record)
        
        return quarantine_record
    
    def review_quarantined_data(self, quarantine_id):
        """Review and potentially remediate quarantined data"""
        
        quarantine_record = self.quarantine_log.get_record(quarantine_id)
        quarantined_data = self.load_quarantined_data(quarantine_record["file_path"])
        
        review_result = {
            "quarantine_id": quarantine_id,
            "review_timestamp": datetime.utcnow(),
            "data_quality_assessment": self.assess_quarantined_data(quarantined_data),
            "remediation_options": self.identify_remediation_options(quarantine_record),
            "recommended_action": None
        }
        
        # Determine recommended action
        if review_result["data_quality_assessment"]["recoverable"]:
            review_result["recommended_action"] = "remediate_and_reprocess"
        elif review_result["data_quality_assessment"]["partial_recovery"]:
            review_result["recommended_action"] = "partial_remediation"
        else:
            review_result["recommended_action"] = "discard"
        
        return review_result
    
    def remediate_and_reprocess(self, quarantine_id, remediation_plan):
        """Apply remediation and reprocess quarantined data"""
        
        quarantine_record = self.quarantine_log.get_record(quarantine_id)
        quarantined_data = self.load_quarantined_data(quarantine_record["file_path"])
        
        # Apply remediation steps
        remediated_data = quarantined_data
        for step in remediation_plan["steps"]:
            remediated_data = self.apply_remediation_step(remediated_data, step)
        
        # Reprocess through staging
        staging_processor = StagingProcessor(self.staging_config)
        reprocess_result = staging_processor.process_incoming_data(
            remediated_data, 
            remediation_plan["source_info"]
        )
        
        # Update quarantine record
        self.quarantine_log.update_remediation_status(
            quarantine_id, "remediated", reprocess_result
        )
        
        return reprocess_result
```

## 🚨 **Best Practices**

### **1. Validation Strategy**
- ✅ Implement multi-layered validation (basic → schema → business rules)
- ✅ Make validation rules configurable and maintainable
- ✅ Provide clear error messages and remediation guidance
- ✅ Balance thoroughness with processing speed

### **2. Quarantine Management**
- ✅ Preserve all quarantined data for analysis
- ✅ Implement systematic review and remediation processes
- ✅ Alert on critical data quality issues immediately
- ✅ Track quarantine metrics and trends

### **3. Performance**
- ✅ Keep staging area lightweight and fast
- ✅ Implement parallel processing where possible
- ✅ Regular cleanup of staging files
- ✅ Monitor staging area resource usage

### **4. Monitoring**
- ✅ Track staging success/failure rates
- ✅ Monitor data quality trends
- ✅ Alert on processing delays or errors
- ✅ Maintain audit trail of all staging activities

## 🦄 **Unicorn Platform Integration**

The Staging Area supports the platform by:
- **Data Quality**: Ensuring only high-quality data enters the warehouse
- **Risk Management**: Preventing bad data from affecting trading algorithms
- **Operational Efficiency**: Automated quality gates reduce manual intervention
- **Audit Compliance**: Complete tracking of data validation and remediation

---

*The Staging Area is your quality gate - invest in robust validation to ensure data integrity throughout the platform!*
