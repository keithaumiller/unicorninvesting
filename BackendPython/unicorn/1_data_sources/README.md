# Data Sources - Modern Data Warehouse Architecture

## 🏗️ Data Warehouse Architecture Overview

This directory implements a **modern data warehouse architecture** following industry standards for financial data processing. The architecture supports the complete data lifecycle from raw ingestion to business-ready analytics.

## 📁 Data Warehouse Layers

## 📁 Data Warehouse Layers

```
1_data_sources/
├── 1_raw/                  # 🏪 Raw/Staging - Unprocessed data from sources
├── 2_bronze/               # 🥉 Bronze - Raw data with basic validation  
├── 3_silver/               # 🥈 Silver - Cleaned, normalized data
├── 4_gold/                 # 🥇 Gold - Business-ready, aggregated data
├── 5_data_marts/           # 🏢 Data Marts - Subject-specific data stores
├── 6_etl_pipelines/        # ⚙️ ETL/ELT - Data transformation processes
├── 7_connectors/           # 🔌 Data Connectors - Source system integrations
├── 8_metadata/             # 📋 Metadata Management - Schema, lineage, quality
├── schemas/                # 📊 Database Schemas & Data Models
├── README.md              # 📖 This Architecture Guide
└── ARCHITECTURE.md        # 🏗️ Technical Implementation Details
```

## 🔄 Data Warehouse Flow

```
External APIs → 7_connectors → 1_raw → 2_bronze → 3_silver → 4_gold → 5_data_marts
                                 ↓        ↓        ↓        ↓        ↓
                            6_etl_pipelines → 8_metadata (governance & lineage)
```

## 🔄 Data Flow Architecture

```
External APIs → Connectors → Staging → Bronze → Silver → Gold → Data Marts
                                ↓        ↓        ↓        ↓
                            ETL Pipelines → Metadata Management
```

## 🔌 **Connectors Layer** (`connectors/`)

**Purpose**: Data source integrations and API connections
- **Input**: External APIs, feeds, databases
- **Output**: Raw data streams to staging area
- **Technology**: Python data connectors, API clients

**Current Implementations**:
- `YahooFinanceMinuteData.py` - Yahoo Finance free API integration
- `AlphaVantageMinuteData.py` - Alpha Vantage API connector
- `YAHOO_FINANCE_INTEGRATION_GUIDE.md` - Integration documentation

**Standards**:
- Error handling and retry logic
- Rate limiting compliance
- Data validation at ingestion
- Logging and monitoring

## 🏪 **Staging Layer** (`staging/`)

**Purpose**: Temporary storage for data validation and initial processing
- **Input**: Raw data from connectors
- **Output**: Validated data ready for bronze layer
- **Retention**: 24-48 hours (configurable)

**Use Cases**:
- Data quality checks
- Duplicate detection
- Schema validation
- Temporary storage during processing

## 🥉 **Bronze Layer** (`bronze_layer/`)

**Purpose**: Raw data storage with minimal processing
- **Input**: Validated data from staging
- **Output**: Historical raw data archive
- **Format**: Original format with metadata

**Current Data**:
- `exchangedata/` - Market reference data (symbols, currencies)
- `old/` - Historical datasets and archives
- `results/` - Raw analytical results and outputs
- Historical market data files

**Characteristics**:
- Immutable storage
- Complete data lineage
- Schema-on-read approach
- Long-term retention

## 🥈 **Silver Layer** (`silver_layer/`)

**Purpose**: Cleaned, normalized, and structured data
- **Input**: Raw data from bronze layer
- **Output**: Clean datasets for analytics
- **Format**: Standardized schemas and formats

**Transformations**:
- Data cleaning and validation
- Standardization (dates, currencies, symbols)
- Deduplication
- Data type conversion
- Missing value handling

**Quality Standards**:
- Consistent schemas
- Validated data types
- No duplicates
- Complete data lineage

## 🥇 **Gold Layer** (`gold_layer/`)

**Purpose**: Business-ready, aggregated data optimized for analytics
- **Input**: Cleaned data from silver layer
- **Output**: Analysis-ready datasets
- **Format**: Optimized for query performance

**Features**:
- Pre-calculated metrics
- Aggregated time series
- Performance indicators
- Risk metrics
- Portfolio analytics

**Optimization**:
- Indexed for fast queries
- Partitioned by time/symbol
- Compressed storage
- Cached frequently used data

## 🏢 **Data Marts** (`data_marts/`)

**Purpose**: Subject-specific data stores for business domains
- **Input**: Gold layer data
- **Output**: Domain-specific datasets
- **Organization**: By business function

**Planned Data Marts**:
- `forex/` - Foreign exchange data and analytics
- `equities/` - Stock market data and metrics
- `crypto/` - Cryptocurrency data and indicators
- `portfolio/` - Portfolio analytics and performance
- `risk/` - Risk metrics and monitoring
- `compliance/` - Regulatory and compliance data

## ⚙️ **ETL Pipelines** (`etl_pipelines/`)

**Purpose**: Data transformation and processing workflows
- **Input**: Data from any layer
- **Output**: Processed data to target layer
- **Technology**: Python, SQL, workflow orchestration

**Current Pipelines**:
- Data gathering and collection scripts
- Dataset creation and feature engineering
- Data cleaning and validation
- Performance analysis and reporting

**Pipeline Standards**:
- Idempotent operations
- Error handling and recovery
- Monitoring and alerting
- Data lineage tracking

## 📊 **Schemas** (`schemas/`)

**Purpose**: Database schemas, data models, and structure definitions
- **Content**: SQL schemas, DDL scripts, data models
- **Standards**: Database design patterns
- **Documentation**: Schema documentation and relationships

**Current Schemas**:
- Production database schemas
- Historical database dumps
- Data model documentation
- Migration scripts

## 📋 **Metadata** (`metadata/`)

**Purpose**: Data governance, lineage, and quality management
- **Data Catalog**: Inventory of all datasets
- **Data Lineage**: Track data flow and transformations
- **Data Quality**: Metrics and monitoring
- **Data Governance**: Policies and standards

**Components**:
- Data dictionary and catalog
- Quality metrics and monitoring
- Lineage tracking
- Governance policies

## 🎯 **Data Warehouse Benefits**

### **For Developers**
1. **Clear Structure**: Know exactly where to find/put data
2. **Quality Assurance**: Built-in data validation and cleaning
3. **Scalability**: Each layer can be scaled independently
4. **Maintenance**: Easy to debug and maintain pipelines

### **For Analytics**
1. **Performance**: Gold layer optimized for fast queries
2. **Consistency**: Standardized data across all analyses
3. **Reliability**: Data quality checks at every layer
4. **Traceability**: Complete data lineage and audit trail

### **For Operations**
1. **Monitoring**: Each layer has its own monitoring
2. **Recovery**: Can rebuild from any layer
3. **Compliance**: Built-in governance and audit
4. **Cost Optimization**: Tiered storage strategies

## 🔧 **Development Guidelines**

### **1. Data Flow Rules**
- Data should flow through layers in sequence: Bronze → Silver → Gold
- Never bypass layers (maintain data lineage)
- Each layer should add value through transformation or optimization
- Immutable storage in bronze layer

### **2. Quality Standards**
- Validate data at each layer boundary
- Implement data quality metrics
- Log all transformations and lineage
- Handle errors gracefully with recovery

### **3. Performance Optimization**
- Partition large datasets by time/symbol
- Index frequently queried columns
- Use appropriate compression
- Cache frequently accessed data

### **4. Security & Compliance**
- Encrypt sensitive data at rest and in transit
- Implement access controls by layer
- Log all data access and modifications
- Maintain data retention policies

## 🚀 **Quick Start Guide**

### **1. Adding New Data Source**
```python
# 1. Create connector in connectors/
class NewDataConnector(PythonData):
    def get_source(self):
        # Implement data retrieval
        pass

# 2. Add to staging for validation
# 3. Process to bronze layer (raw storage)
# 4. Clean and move to silver layer
# 5. Aggregate for gold layer
```

### **2. Creating ETL Pipeline**
```python
# In etl_pipelines/
def bronze_to_silver_pipeline():
    # Read from bronze
    raw_data = read_bronze_layer(source)
    
    # Clean and validate
    clean_data = clean_and_validate(raw_data)
    
    # Write to silver
    write_silver_layer(clean_data)
```

### **3. Querying Data**
```python
# Always start with highest appropriate layer
# Gold layer for business analytics
gold_data = query_gold_layer("forex_daily_metrics")

# Silver layer for detailed analysis
silver_data = query_silver_layer("raw_prices")

# Bronze layer only for data lineage/debugging
bronze_data = query_bronze_layer("source_files")
```

## 🦄 **Unicorn Platform Integration**

This data warehouse architecture supports:
- **Real-time Trading**: Low-latency gold layer data for algorithms
- **Risk Management**: Pre-calculated risk metrics in data marts
- **Portfolio Analytics**: Optimized portfolio performance data
- **Compliance**: Complete audit trail and data governance
- **Scalability**: Handle increasing data volumes efficiently

## 🎯 **Current Status: REORGANIZED & PRODUCTION READY**

✅ **Clean Architecture**: All R scripts moved to legacy structure  
✅ **Modern Connectors**: Python APIs ready for real-time data  
✅ **Reference Data**: Current market data properly organized  
✅ **Legacy Preserved**: Complete historical archive maintained  
✅ **Documentation**: All layers documented with implementation guides  

📄 **See**: `REORGANIZATION_COMPLETE.md` for detailed cleanup summary

---

*This architecture follows modern data warehouse best practices for financial services, ensuring data quality, performance, and governance throughout the data lifecycle.*
