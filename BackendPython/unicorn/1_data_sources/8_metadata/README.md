# Metadata Management & Data Governance

## 📋 Purpose

This directory manages **metadata, data lineage, governance policies, and data catalog** for the entire data warehouse. Metadata provides comprehensive information about data sources, transformations, quality, and usage.

## 🏗️ Architecture Role

**Data Governance**: Oversees data quality, lineage, and compliance across all warehouse layers

Metadata management ensures **data trust, discoverability, and compliance** throughout the platform.

## 📊 **Metadata Categories**

### **📚 Data Catalog**
Comprehensive inventory of all datasets and data assets:

**Components**:
- **Dataset Inventory**: Complete list of all datasets across all layers
- **Schema Registry**: Centralized schema definitions and versions
- **Data Lineage**: End-to-end data flow and transformation tracking
- **Usage Analytics**: Data consumption patterns and access statistics

### **🔍 Data Discovery**
Tools and metadata for finding and understanding data:

**Features**:
- **Search Interface**: Search datasets by name, content, or business terms
- **Data Profiling**: Statistical profiles and data distribution analysis
- **Business Glossary**: Business term definitions and mappings
- **Recommendation Engine**: Suggest relevant datasets based on usage patterns

### **📈 Data Quality Management**
Comprehensive data quality monitoring and reporting:

**Metrics**:
- **Completeness**: Percentage of non-null values
- **Accuracy**: Data correctness and validation results
- **Consistency**: Cross-system data consistency checks
- **Timeliness**: Data freshness and processing lag monitoring

### **🛡️ Data Governance**
Policies, procedures, and compliance management:

**Areas**:
- **Access Control**: Data access permissions and audit trails
- **Retention Policies**: Data lifecycle and retention rules
- **Privacy Management**: PII identification and protection
- **Compliance Tracking**: Regulatory compliance monitoring

## 🔧 **Metadata Management Framework**

### **1. Data Catalog Schema**
```python
# Data catalog metadata structure
data_catalog_schema = {
    "dataset_id": "string",              # Unique dataset identifier
    "dataset_name": "string",            # Human-readable name
    "description": "text",               # Dataset description
    "business_purpose": "text",          # Business use case
    "data_layer": "enum",               # bronze, silver, gold, mart
    "schema_name": "string",            # Database schema
    "table_name": "string",             # Table or file name
    "file_format": "string",            # CSV, Parquet, JSON, etc.
    "location": "string",               # Physical storage location
    "size_bytes": "bigint",             # Dataset size
    "record_count": "bigint",           # Number of records
    "column_count": "integer",          # Number of columns
    "created_date": "datetime",         # Creation timestamp
    "last_updated": "datetime",         # Last modification
    "update_frequency": "string",       # daily, hourly, real-time
    "owner": "string",                  # Data owner/steward
    "source_systems": "array[string]", # Source system list
    "tags": "array[string]",           # Classification tags
    "sensitivity_level": "enum",        # public, internal, confidential
    "retention_days": "integer",        # Data retention period
    "quality_score": "decimal",         # Overall quality score
    "usage_count": "integer",          # Access frequency
    "last_accessed": "datetime"        # Last access time
}
```

### **2. Data Lineage Tracking**
```python
class DataLineageTracker:
    """Track data lineage across all transformations"""
    
    def __init__(self):
        self.lineage_graph = LineageGraph()
        self.transformation_log = TransformationLog()
    
    def track_transformation(self, source_datasets, target_dataset, transformation_info):
        """Record a data transformation"""
        
        lineage_record = {
            "transformation_id": self.generate_transformation_id(),
            "timestamp": datetime.utcnow(),
            "source_datasets": source_datasets,
            "target_dataset": target_dataset,
            "transformation_type": transformation_info["type"],
            "pipeline_name": transformation_info["pipeline"],
            "transformation_logic": transformation_info["logic"],
            "data_volume": {
                "input_records": transformation_info["input_count"],
                "output_records": transformation_info["output_count"],
                "processing_time": transformation_info["duration"]
            },
            "quality_impact": transformation_info.get("quality_changes"),
            "user_context": transformation_info.get("user", "system")
        }
        
        # Add to lineage graph
        self.lineage_graph.add_transformation(lineage_record)
        
        # Log transformation
        self.transformation_log.record(lineage_record)
    
    def get_upstream_lineage(self, dataset_id, max_depth=10):
        """Get complete upstream lineage for a dataset"""
        
        lineage = {
            "dataset_id": dataset_id,
            "upstream_sources": [],
            "transformation_path": [],
            "total_depth": 0
        }
        
        current_dataset = dataset_id
        depth = 0
        
        while depth < max_depth:
            upstream = self.lineage_graph.get_immediate_upstream(current_dataset)
            
            if not upstream:
                break
                
            lineage["upstream_sources"].extend(upstream["sources"])
            lineage["transformation_path"].append(upstream["transformation"])
            
            # Move to next level
            current_dataset = upstream["sources"][0] if upstream["sources"] else None
            depth += 1
        
        lineage["total_depth"] = depth
        return lineage
    
    def get_downstream_impact(self, dataset_id):
        """Get all datasets impacted by changes to this dataset"""
        
        impact_analysis = {
            "dataset_id": dataset_id,
            "immediate_downstream": [],
            "all_downstream": [],
            "critical_dependencies": []
        }
        
        # Find immediate downstream
        immediate = self.lineage_graph.get_immediate_downstream(dataset_id)
        impact_analysis["immediate_downstream"] = immediate
        
        # Find all downstream recursively
        all_downstream = self.lineage_graph.get_all_downstream(dataset_id)
        impact_analysis["all_downstream"] = all_downstream
        
        # Identify critical dependencies
        for downstream in all_downstream:
            if self.is_critical_dataset(downstream):
                impact_analysis["critical_dependencies"].append(downstream)
        
        return impact_analysis
```

### **3. Data Quality Monitoring**
```python
class DataQualityMonitor:
    """Comprehensive data quality monitoring system"""
    
    def __init__(self):
        self.quality_rules = self.load_quality_rules()
        self.quality_history = QualityHistoryManager()
    
    def assess_dataset_quality(self, dataset_id):
        """Comprehensive quality assessment for a dataset"""
        
        dataset = self.load_dataset(dataset_id)
        
        quality_assessment = {
            "dataset_id": dataset_id,
            "assessment_timestamp": datetime.utcnow(),
            "completeness": self.assess_completeness(dataset),
            "accuracy": self.assess_accuracy(dataset),
            "consistency": self.assess_consistency(dataset),
            "timeliness": self.assess_timeliness(dataset),
            "validity": self.assess_validity(dataset),
            "uniqueness": self.assess_uniqueness(dataset)
        }
        
        # Calculate overall quality score
        quality_assessment["overall_score"] = self.calculate_overall_score(quality_assessment)
        
        # Store assessment history
        self.quality_history.record_assessment(quality_assessment)
        
        # Generate alerts if needed
        self.check_quality_alerts(quality_assessment)
        
        return quality_assessment
    
    def assess_completeness(self, dataset):
        """Assess data completeness"""
        
        total_cells = len(dataset) * len(dataset.columns)
        non_null_cells = dataset.count().sum()
        
        completeness = {
            "score": non_null_cells / total_cells,
            "total_cells": total_cells,
            "non_null_cells": non_null_cells,
            "null_percentage": (total_cells - non_null_cells) / total_cells * 100,
            "column_completeness": (dataset.count() / len(dataset)).to_dict()
        }
        
        return completeness
    
    def assess_accuracy(self, dataset):
        """Assess data accuracy using business rules"""
        
        accuracy_results = {
            "score": 1.0,
            "total_records": len(dataset),
            "accurate_records": len(dataset),
            "rule_violations": []
        }
        
        # Apply business rules
        for rule in self.quality_rules.get("accuracy", []):
            violations = rule.check_violations(dataset)
            
            if violations:
                accuracy_results["rule_violations"].extend(violations)
                accuracy_results["accurate_records"] -= len(violations)
        
        # Calculate accuracy score
        accuracy_results["score"] = accuracy_results["accurate_records"] / accuracy_results["total_records"]
        
        return accuracy_results
    
    def generate_quality_dashboard(self, time_range="7d"):
        """Generate comprehensive quality dashboard"""
        
        dashboard = {
            "summary": {
                "total_datasets": self.get_dataset_count(),
                "average_quality_score": self.get_average_quality_score(time_range),
                "datasets_below_threshold": self.count_low_quality_datasets(),
                "quality_trend": self.get_quality_trend(time_range)
            },
            "by_layer": {
                "bronze": self.get_layer_quality_summary("bronze"),
                "silver": self.get_layer_quality_summary("silver"),
                "gold": self.get_layer_quality_summary("gold"),
                "marts": self.get_layer_quality_summary("marts")
            },
            "alerts": self.get_active_quality_alerts(),
            "recommendations": self.generate_quality_recommendations()
        }
        
        return dashboard
```

### **4. Data Usage Analytics**
```python
class DataUsageAnalytics:
    """Track and analyze data usage patterns"""
    
    def __init__(self):
        self.usage_tracker = UsageTracker()
        self.access_logger = AccessLogger()
    
    def track_dataset_access(self, dataset_id, user_id, access_type, query_info=None):
        """Record dataset access for analytics"""
        
        access_record = {
            "access_id": self.generate_access_id(),
            "timestamp": datetime.utcnow(),
            "dataset_id": dataset_id,
            "user_id": user_id,
            "access_type": access_type,  # read, write, schema_view
            "query_info": {
                "query_type": query_info.get("type") if query_info else None,
                "records_accessed": query_info.get("record_count") if query_info else None,
                "execution_time": query_info.get("duration") if query_info else None,
                "query_complexity": query_info.get("complexity") if query_info else None
            },
            "access_method": query_info.get("method") if query_info else "unknown",  # API, SQL, dashboard
            "source_ip": query_info.get("ip") if query_info else None
        }
        
        self.access_logger.record_access(access_record)
        self.usage_tracker.update_usage_stats(dataset_id, access_record)
    
    def generate_usage_report(self, time_range="30d"):
        """Generate comprehensive usage analytics report"""
        
        report = {
            "summary": {
                "total_accesses": self.count_total_accesses(time_range),
                "unique_users": self.count_unique_users(time_range),
                "most_accessed_datasets": self.get_top_datasets(time_range, limit=10),
                "usage_trend": self.get_usage_trend(time_range)
            },
            "by_dataset": self.get_dataset_usage_breakdown(time_range),
            "by_user": self.get_user_usage_patterns(time_range),
            "by_layer": self.get_layer_usage_stats(time_range),
            "performance": {
                "avg_query_time": self.get_average_query_time(time_range),
                "slow_queries": self.identify_slow_queries(time_range),
                "resource_usage": self.get_resource_usage_stats(time_range)
            },
            "recommendations": {
                "optimization_opportunities": self.identify_optimization_opportunities(),
                "underutilized_datasets": self.find_underutilized_datasets(),
                "access_pattern_insights": self.analyze_access_patterns()
            }
        }
        
        return report
```

## 📊 **Metadata Query Interface**

### **Unified Metadata API**
```python
class MetadataAPI:
    """Unified interface for all metadata operations"""
    
    def __init__(self):
        self.catalog = DataCatalog()
        self.lineage = DataLineageTracker()
        self.quality = DataQualityMonitor()
        self.usage = DataUsageAnalytics()
    
    def search_datasets(self, query, filters=None):
        """Search datasets by content, name, or business terms"""
        
        search_results = {
            "query": query,
            "filters": filters,
            "results": [],
            "total_count": 0,
            "facets": {}
        }
        
        # Search in catalog
        catalog_results = self.catalog.search(query, filters)
        
        # Enhance with quality and usage info
        for result in catalog_results:
            enhanced_result = {
                **result,
                "quality_score": self.quality.get_latest_quality_score(result["dataset_id"]),
                "usage_frequency": self.usage.get_usage_frequency(result["dataset_id"]),
                "last_accessed": self.usage.get_last_access_time(result["dataset_id"])
            }
            search_results["results"].append(enhanced_result)
        
        search_results["total_count"] = len(search_results["results"])
        search_results["facets"] = self.catalog.get_search_facets(catalog_results)
        
        return search_results
    
    def get_dataset_profile(self, dataset_id):
        """Get comprehensive dataset profile"""
        
        profile = {
            "dataset_info": self.catalog.get_dataset_info(dataset_id),
            "schema": self.catalog.get_dataset_schema(dataset_id),
            "quality_assessment": self.quality.get_latest_assessment(dataset_id),
            "lineage": {
                "upstream": self.lineage.get_upstream_lineage(dataset_id),
                "downstream": self.lineage.get_downstream_impact(dataset_id)
            },
            "usage_analytics": self.usage.get_dataset_usage_summary(dataset_id),
            "recommendations": self.generate_dataset_recommendations(dataset_id)
        }
        
        return profile
    
    def get_governance_dashboard(self):
        """Get comprehensive data governance dashboard"""
        
        dashboard = {
            "overview": {
                "total_datasets": self.catalog.get_total_dataset_count(),
                "quality_distribution": self.quality.get_quality_distribution(),
                "compliance_status": self.get_compliance_status(),
                "active_issues": self.get_active_governance_issues()
            },
            "quality": self.quality.generate_quality_dashboard(),
            "usage": self.usage.generate_usage_report(),
            "lineage": self.lineage.get_lineage_summary(),
            "alerts": self.get_all_active_alerts(),
            "recommendations": self.generate_governance_recommendations()
        }
        
        return dashboard
```

## 🚨 **Best Practices**

### **1. Metadata Completeness**
- ✅ Maintain comprehensive metadata for all datasets
- ✅ Automate metadata collection where possible
- ✅ Regularly validate metadata accuracy
- ✅ Encourage self-service metadata updates

### **2. Data Lineage**
- ✅ Track lineage at every transformation step
- ✅ Maintain lineage across system boundaries
- ✅ Implement impact analysis capabilities
- ✅ Provide visual lineage representations

### **3. Quality Monitoring**
- ✅ Implement continuous quality monitoring
- ✅ Set appropriate quality thresholds by dataset
- ✅ Alert on quality degradation immediately
- ✅ Track quality trends over time

### **4. Governance**
- ✅ Implement proper access controls and auditing
- ✅ Maintain data classification and sensitivity levels
- ✅ Regular compliance assessments
- ✅ Document governance policies and procedures

## 🦄 **Unicorn Platform Integration**

Metadata management enables the platform by providing:
- **Data Trust**: Comprehensive quality and lineage information
- **Compliance**: Complete audit trails and governance controls
- **Efficiency**: Fast data discovery and understanding
- **Risk Management**: Impact analysis and change management

---

*Metadata is the foundation of data trust - invest in comprehensive metadata management for long-term success!*
