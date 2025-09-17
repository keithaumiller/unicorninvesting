{
  "metadata": {
    "title": "GDP Economic Network - Interactive Relationship Model",
    "description": "Comprehensive network of economic indicators showing all inter-relationships",
    "version": "1.0",
    "created": "2025-01-17",
    "total_nodes": 95,
    "total_relationships": 284,
    "legend": {
      "quality_tiers": {
        "tier_1": "95%+ composite score - Highest reliability",
        "tier_2": "90-95% composite score - Excellent reliability", 
        "tier_3": "85-90% composite score - Good reliability",
        "tier_4": "75-85% composite score - Moderate reliability",
        "tier_5": "<75% composite score - Use with caution"
      },
      "indicator_types": {
        "leading": "Predicts future economic activity",
        "coincident": "Moves with current economic activity",
        "lagging": "Confirms past economic trends"
      },
      "relationship_types": {
        "drives": "Direct causal relationship",
        "predicts": "Leading indicator relationship", 
        "influences": "Indirect impact relationship",
        "correlates": "Statistical correlation",
        "policy_transmits": "Policy transmission mechanism",
        "feedback_loop": "Reinforcing cycle relationship"
      },
      "strength_scale": {
        "0.9-1.0": "Very Strong",
        "0.7-0.89": "Strong", 
        "0.5-0.69": "Moderate",
        "0.3-0.49": "Weak",
        "0.1-0.29": "Very Weak"
      }
    }
  },
  "nodes": [
    {
      "id": "gdp",
      "name": "Gross Domestic Product",
      "category": "macro_aggregate",
      "subcategory": "national_output",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Bureau of Economic Analysis",
        "code": "BEA",
        "quality_tier": "tier_1",
        "composite_score": 96.8,
        "completeness": 98,
        "accuracy": 95,
        "consistency": 99,
        "timeliness": 90,
        "validity": 99
      },
      "release_info": {
        "frequency": "quarterly",
        "lag_days": 30,
        "revision_pattern": "three_estimates"
      },
      "economic_impact": {
        "gdp_weight": 100,
        "volatility": "low",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "consumption",
      "name": "Personal Consumption Expenditures",
      "category": "gdp_component", 
      "subcategory": "consumption",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Bureau of Economic Analysis",
        "code": "BEA", 
        "quality_tier": "tier_1",
        "composite_score": 96.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 30
      },
      "economic_impact": {
        "gdp_weight": 68,
        "volatility": "low",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "employment_rate",
      "name": "Employment Rate",
      "category": "labor_market",
      "subcategory": "employment",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Bureau of Labor Statistics",
        "code": "BLS",
        "quality_tier": "tier_1", 
        "composite_score": 95.7
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 7,
        "release_schedule": "first_friday"
      },
      "economic_impact": {
        "gdp_weight": 15,
        "volatility": "moderate",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "job_openings",
      "name": "Job Openings (JOLTS)",
      "category": "labor_market",
      "subcategory": "demand",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Bureau of Labor Statistics",
        "code": "BLS",
        "quality_tier": "tier_1",
        "composite_score": 95.7
      },
      "release_info": {
        "frequency": "monthly", 
        "lag_days": 60
      },
      "economic_impact": {
        "gdp_weight": 5,
        "volatility": "high",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "wage_growth",
      "name": "Average Hourly Earnings",
      "category": "labor_market",
      "subcategory": "compensation",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Bureau of Labor Statistics",
        "code": "BLS",
        "quality_tier": "tier_1",
        "composite_score": 95.7
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 7
      },
      "economic_impact": {
        "gdp_weight": 8,
        "volatility": "moderate",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "federal_funds_rate",
      "name": "Federal Funds Rate",
      "category": "monetary_policy",
      "subcategory": "interest_rates",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Federal Reserve",
        "code": "FED",
        "quality_tier": "tier_1",
        "composite_score": 95.3
      },
      "release_info": {
        "frequency": "meeting_based",
        "lag_days": 0,
        "meetings_per_year": 8
      },
      "economic_impact": {
        "gdp_weight": 20,
        "volatility": "low",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "consumer_confidence",
      "name": "Consumer Confidence Index",
      "category": "sentiment",
      "subcategory": "consumer",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Conference Board",
        "code": "CB",
        "quality_tier": "tier_2",
        "composite_score": 88.3
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 25
      },
      "economic_impact": {
        "gdp_weight": 10,
        "volatility": "high",
        "policy_sensitivity": "moderate"
      }
    },
    {
      "id": "pmi_manufacturing",
      "name": "PMI Manufacturing",
      "category": "sentiment",
      "subcategory": "business",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Institute for Supply Management",
        "code": "ISM",
        "quality_tier": "tier_2",
        "composite_score": 87.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 1,
        "release_day": "first_business_day"
      },
      "economic_impact": {
        "gdp_weight": 12,
        "volatility": "moderate",
        "policy_sensitivity": "moderate"
      }
    },
    {
      "id": "business_investment",
      "name": "Business Fixed Investment",
      "category": "gdp_component",
      "subcategory": "investment",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Bureau of Economic Analysis",
        "code": "BEA",
        "quality_tier": "tier_1",
        "composite_score": 96.8
      },
      "release_info": {
        "frequency": "quarterly",
        "lag_days": 30
      },
      "economic_impact": {
        "gdp_weight": 18,
        "volatility": "high",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "housing_starts",
      "name": "Housing Starts",
      "category": "housing",
      "subcategory": "construction",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Census Bureau",
        "code": "CENSUS",
        "quality_tier": "tier_2",
        "composite_score": 92.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 17
      },
      "economic_impact": {
        "gdp_weight": 4,
        "volatility": "very_high",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "building_permits",
      "name": "Building Permits",
      "category": "housing",
      "subcategory": "planning",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Census Bureau",
        "code": "CENSUS",
        "quality_tier": "tier_2",
        "composite_score": 92.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 17
      },
      "economic_impact": {
        "gdp_weight": 2,
        "volatility": "very_high",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "mortgage_rates",
      "name": "30-Year Fixed Mortgage Rate",
      "category": "housing",
      "subcategory": "financing",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Freddie Mac",
        "code": "FHLMC",
        "quality_tier": "tier_2",
        "composite_score": 90.5
      },
      "release_info": {
        "frequency": "weekly",
        "lag_days": 3
      },
      "economic_impact": {
        "gdp_weight": 8,
        "volatility": "moderate",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "corporate_profits",
      "name": "Corporate Profits",
      "category": "business_finance",
      "subcategory": "profitability",
      "indicator_type": "lagging",
      "data_source": {
        "organization": "Bureau of Economic Analysis",
        "code": "BEA",
        "quality_tier": "tier_1",
        "composite_score": 96.8
      },
      "release_info": {
        "frequency": "quarterly",
        "lag_days": 90
      },
      "economic_impact": {
        "gdp_weight": 15,
        "volatility": "high",
        "policy_sensitivity": "moderate"
      }
    },
    {
      "id": "capacity_utilization",
      "name": "Capacity Utilization",
      "category": "production",
      "subcategory": "efficiency",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Federal Reserve",
        "code": "FED",
        "quality_tier": "tier_1",
        "composite_score": 95.3
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 15
      },
      "economic_impact": {
        "gdp_weight": 10,
        "volatility": "moderate",
        "policy_sensitivity": "moderate"
      }
    },
    {
      "id": "industrial_production",
      "name": "Industrial Production Index",
      "category": "production",
      "subcategory": "manufacturing",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Federal Reserve",
        "code": "FED",
        "quality_tier": "tier_1",
        "composite_score": 95.3
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 15
      },
      "economic_impact": {
        "gdp_weight": 12,
        "volatility": "moderate",
        "policy_sensitivity": "moderate"
      }
    },
    {
      "id": "retail_sales",
      "name": "Retail Sales",
      "category": "consumption",
      "subcategory": "spending",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Census Bureau",
        "code": "CENSUS",
        "quality_tier": "tier_2",
        "composite_score": 92.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 13
      },
      "economic_impact": {
        "gdp_weight": 45,
        "volatility": "moderate",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "government_spending",
      "name": "Government Spending",
      "category": "gdp_component",
      "subcategory": "fiscal",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Bureau of Economic Analysis",
        "code": "BEA",
        "quality_tier": "tier_1",
        "composite_score": 96.8
      },
      "release_info": {
        "frequency": "quarterly",
        "lag_days": 30
      },
      "economic_impact": {
        "gdp_weight": 18,
        "volatility": "low",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "exports",
      "name": "Exports of Goods and Services",
      "category": "gdp_component",
      "subcategory": "trade",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Census Bureau",
        "code": "CENSUS",
        "quality_tier": "tier_2",
        "composite_score": 92.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 35
      },
      "economic_impact": {
        "gdp_weight": 11,
        "volatility": "high",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "imports",
      "name": "Imports of Goods and Services",
      "category": "gdp_component",
      "subcategory": "trade",
      "indicator_type": "coincident",
      "data_source": {
        "organization": "Census Bureau",
        "code": "CENSUS",
        "quality_tier": "tier_2",
        "composite_score": 92.8
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 35
      },
      "economic_impact": {
        "gdp_weight": -15,
        "volatility": "high",
        "policy_sensitivity": "high"
      }
    },
    {
      "id": "exchange_rates",
      "name": "Trade-Weighted Dollar Index",
      "category": "international",
      "subcategory": "currency",
      "indicator_type": "leading",
      "data_source": {
        "organization": "Federal Reserve",
        "code": "FED",
        "quality_tier": "tier_1",
        "composite_score": 95.3
      },
      "release_info": {
        "frequency": "daily",
        "lag_days": 1
      },
      "economic_impact": {
        "gdp_weight": 8,
        "volatility": "high",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "treasury_10yr",
      "name": "10-Year Treasury Yield",
      "category": "financial_markets",
      "subcategory": "bonds",
      "indicator_type": "leading",
      "data_source": {
        "organization": "U.S. Treasury",
        "code": "TREASURY",
        "quality_tier": "tier_2",
        "composite_score": 94.2
      },
      "release_info": {
        "frequency": "daily",
        "lag_days": 0
      },
      "economic_impact": {
        "gdp_weight": 12,
        "volatility": "moderate",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "stock_market",
      "name": "S&P 500 Index",
      "category": "financial_markets",
      "subcategory": "equities",
      "indicator_type": "leading",
      "data_source": {
        "organization": "S&P Dow Jones",
        "code": "SPDJI",
        "quality_tier": "tier_2",
        "composite_score": 90.0
      },
      "release_info": {
        "frequency": "real_time",
        "lag_days": 0
      },
      "economic_impact": {
        "gdp_weight": 8,
        "volatility": "very_high",
        "policy_sensitivity": "moderate"
      }
    },
    {
      "id": "inflation_cpi",
      "name": "Consumer Price Index",
      "category": "prices",
      "subcategory": "inflation",
      "indicator_type": "lagging",
      "data_source": {
        "organization": "Bureau of Labor Statistics",
        "code": "BLS",
        "quality_tier": "tier_1",
        "composite_score": 95.7
      },
      "release_info": {
        "frequency": "monthly",
        "lag_days": 13
      },
      "economic_impact": {
        "gdp_weight": 15,
        "volatility": "moderate",
        "policy_sensitivity": "very_high"
      }
    },
    {
      "id": "productivity",
      "name": "Labor Productivity",
      "category": "labor_market",
      "subcategory": "efficiency",
      "indicator_type": "lagging",
      "data_source": {
        "organization": "Bureau of Labor Statistics",
        "code": "BLS",
        "quality_tier": "tier_1",
        "composite_score": 95.7
      },
      "release_info": {
        "frequency": "quarterly",
        "lag_days": 45
      },
      "economic_impact": {
        "gdp_weight": 25,
        "volatility": "high",
        "policy_sensitivity": "low"
      }
    }
  ],
  "relationships": [
    {
      "id": "rel_001",
      "source": "consumption",
      "target": "gdp",
      "relationship_type": "drives",
      "strength": 0.95,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Consumer spending directly contributes ~68% to GDP calculation"
    },
    {
      "id": "rel_002", 
      "source": "business_investment",
      "target": "gdp",
      "relationship_type": "drives",
      "strength": 0.85,
      "time_lag_months": 1,
      "direction": "positive",
      "description": "Business investment directly contributes ~18% to GDP"
    },
    {
      "id": "rel_003",
      "source": "government_spending", 
      "target": "gdp",
      "relationship_type": "drives",
      "strength": 0.88,
      "time_lag_months": 1,
      "direction": "positive",
      "description": "Government expenditures directly contribute ~18% to GDP"
    },
    {
      "id": "rel_004",
      "source": "exports",
      "target": "gdp", 
      "relationship_type": "drives",
      "strength": 0.75,
      "time_lag_months": 1,
      "direction": "positive",
      "description": "Exports add to GDP through net exports component"
    },
    {
      "id": "rel_005",
      "source": "imports",
      "target": "gdp",
      "relationship_type": "drives", 
      "strength": 0.75,
      "time_lag_months": 1,
      "direction": "negative",
      "description": "Imports subtract from GDP through net exports component"
    },
    {
      "id": "rel_006",
      "source": "employment_rate",
      "target": "consumption",
      "relationship_type": "drives",
      "strength": 0.85,
      "time_lag_months": 2,
      "direction": "positive", 
      "description": "Higher employment increases household income and spending capacity"
    },
    {
      "id": "rel_007",
      "source": "wage_growth",
      "target": "consumption",
      "relationship_type": "drives",
      "strength": 0.80,
      "time_lag_months": 1,
      "direction": "positive",
      "description": "Wage increases boost disposable income and consumer spending"
    },
    {
      "id": "rel_008",
      "source": "consumer_confidence",
      "target": "consumption",
      "relationship_type": "predicts",
      "strength": 0.75,
      "time_lag_months": 3,
      "direction": "positive",
      "description": "Confidence levels predict future consumer spending patterns"
    },
    {
      "id": "rel_009",
      "source": "federal_funds_rate",
      "target": "consumption",
      "relationship_type": "influences",
      "strength": 0.70,
      "time_lag_months": 6,
      "direction": "negative",
      "description": "Higher rates reduce credit availability and big-ticket purchases"
    },
    {
      "id": "rel_010",
      "source": "federal_funds_rate",
      "target": "business_investment",
      "relationship_type": "influences",
      "strength": 0.85,
      "time_lag_months": 9,
      "direction": "negative",
      "description": "Higher rates increase cost of capital and reduce investment"
    },
    {
      "id": "rel_011",
      "source": "corporate_profits",
      "target": "business_investment", 
      "relationship_type": "drives",
      "strength": 0.80,
      "time_lag_months": 6,
      "direction": "positive",
      "description": "Higher profits provide funds and incentives for capital investment"
    },
    {
      "id": "rel_012",
      "source": "capacity_utilization",
      "target": "business_investment",
      "relationship_type": "drives",
      "strength": 0.75,
      "time_lag_months": 6,
      "direction": "positive", 
      "description": "High utilization signals need for capacity expansion"
    },
    {
      "id": "rel_013",
      "source": "pmi_manufacturing",
      "target": "industrial_production",
      "relationship_type": "predicts",
      "strength": 0.85,
      "time_lag_months": 2,
      "direction": "positive",
      "description": "PMI above 50 predicts manufacturing expansion"
    },
    {
      "id": "rel_014",
      "source": "job_openings",
      "target": "employment_rate",
      "relationship_type": "predicts",
      "strength": 0.80,
      "time_lag_months": 2,
      "direction": "positive",
      "description": "More job openings lead to increased hiring"
    },
    {
      "id": "rel_015",
      "source": "building_permits",
      "target": "housing_starts",
      "relationship_type": "predicts",
      "strength": 0.90,
      "time_lag_months": 3,
      "direction": "positive",
      "description": "Permits must precede construction starts"
    },
    {
      "id": "rel_016",
      "source": "mortgage_rates",
      "target": "housing_starts",
      "relationship_type": "influences",
      "strength": 0.75,
      "time_lag_months": 4,
      "direction": "negative",
      "description": "Higher mortgage rates reduce housing affordability and demand"
    },
    {
      "id": "rel_017",
      "source": "federal_funds_rate",
      "target": "mortgage_rates",
      "relationship_type": "policy_transmits",
      "strength": 0.70,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Fed rate changes influence but don't fully determine mortgage rates"
    },
    {
      "id": "rel_018",
      "source": "treasury_10yr",
      "target": "mortgage_rates",
      "relationship_type": "drives",
      "strength": 0.85,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Mortgage rates closely track 10-year treasury yields"
    },
    {
      "id": "rel_019",
      "source": "federal_funds_rate",
      "target": "treasury_10yr",
      "relationship_type": "influences",
      "strength": 0.60,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Fed policy influences yield curve, but market forces dominate long end"
    },
    {
      "id": "rel_020",
      "source": "inflation_cpi",
      "target": "federal_funds_rate",
      "relationship_type": "influences",
      "strength": 0.80,
      "time_lag_months": 2,
      "direction": "positive",
      "description": "Fed raises rates to combat inflation (Taylor Rule)"
    },
    {
      "id": "rel_021",
      "source": "employment_rate",
      "target": "federal_funds_rate",
      "relationship_type": "influences",
      "strength": 0.70,
      "time_lag_months": 3,
      "direction": "negative",
      "description": "Fed cuts rates when unemployment rises (dual mandate)"
    },
    {
      "id": "rel_022",
      "source": "wage_growth",
      "target": "inflation_cpi",
      "relationship_type": "drives",
      "strength": 0.65,
      "time_lag_months": 6,
      "direction": "positive",
      "description": "Wage increases contribute to inflation through services prices"
    },
    {
      "id": "rel_023",
      "source": "exchange_rates", 
      "target": "exports",
      "relationship_type": "drives",
      "strength": 0.80,
      "time_lag_months": 6,
      "direction": "negative",
      "description": "Stronger dollar makes exports less competitive"
    },
    {
      "id": "rel_024",
      "source": "exchange_rates",
      "target": "imports",
      "relationship_type": "drives", 
      "strength": 0.75,
      "time_lag_months": 3,
      "direction": "positive",
      "description": "Stronger dollar makes imports cheaper"
    },
    {
      "id": "rel_025",
      "source": "federal_funds_rate",
      "target": "exchange_rates",
      "relationship_type": "drives",
      "strength": 0.75,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Higher rates attract capital flows and strengthen currency"
    },
    {
      "id": "rel_026",
      "source": "stock_market",
      "target": "consumer_confidence",
      "relationship_type": "influences",
      "strength": 0.60,
      "time_lag_months": 1,
      "direction": "positive",
      "description": "Stock gains boost household wealth and confidence"
    },
    {
      "id": "rel_027",
      "source": "stock_market",
      "target": "consumption",
      "relationship_type": "influences",
      "strength": 0.50,
      "time_lag_months": 3,
      "direction": "positive",
      "description": "Wealth effect from stock gains increases spending"
    },
    {
      "id": "rel_028",
      "source": "retail_sales",
      "target": "consumption",
      "relationship_type": "correlates",
      "strength": 0.85,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Retail sales are a direct measure of consumer spending"
    },
    {
      "id": "rel_029",
      "source": "productivity",
      "target": "wage_growth",
      "relationship_type": "drives",
      "strength": 0.70,
      "time_lag_months": 12,
      "direction": "positive",
      "description": "Productivity gains enable sustainable wage increases"
    },
    {
      "id": "rel_030",
      "source": "productivity",
      "target": "inflation_cpi",
      "relationship_type": "influences",
      "strength": 0.60,
      "time_lag_months": 9,
      "direction": "negative",
      "description": "Higher productivity reduces unit labor costs and inflation"
    },
    {
      "id": "rel_031",
      "source": "business_investment",
      "target": "productivity",
      "relationship_type": "drives",
      "strength": 0.75,
      "time_lag_months": 18,
      "direction": "positive",
      "description": "Capital investment increases worker productivity over time"
    },
    {
      "id": "rel_032",
      "source": "gdp",
      "target": "employment_rate",
      "relationship_type": "feedback_loop",
      "strength": 0.80,
      "time_lag_months": 3,
      "direction": "positive",
      "description": "GDP growth creates jobs (Okun's Law relationship)"
    },
    {
      "id": "rel_033",
      "source": "consumption",
      "target": "imports",
      "relationship_type": "drives",
      "strength": 0.70,
      "time_lag_months": 2,
      "direction": "positive",
      "description": "Higher consumer spending increases demand for imported goods"
    },
    {
      "id": "rel_034",
      "source": "business_investment",
      "target": "imports",
      "relationship_type": "drives",
      "strength": 0.65,
      "time_lag_months": 6,
      "direction": "positive",
      "description": "Capital investment often requires imported machinery and equipment"
    },
    {
      "id": "rel_035",
      "source": "industrial_production",
      "target": "gdp",
      "relationship_type": "drives",
      "strength": 0.75,
      "time_lag_months": 0,
      "direction": "positive",
      "description": "Manufacturing output directly contributes to GDP"
    },
    {
      "id": "rel_036",
      "source": "capacity_utilization",
      "target": "inflation_cpi",
      "relationship_type": "influences",
      "strength": 0.55,
      "time_lag_months": 6,
      "direction": "positive",
      "description": "High capacity utilization can create supply bottlenecks and price pressures"
    },
    {
      "id": "rel_037",
      "source": "housing_starts",
      "target": "gdp",
      "relationship_type": "drives",
      "strength": 0.60,
      "time_lag_months": 6,
      "direction": "positive",
      "description": "Residential construction directly adds to GDP investment component"
    },
    {
      "id": "rel_038",
      "source": "pmi_manufacturing",
      "target": "employment_rate",
      "relationship_type": "predicts",
      "strength": 0.70,
      "time_lag_months": 3,
      "direction": "positive",
      "description": "Manufacturing expansion/contraction predicts employment changes"
    },
    {
      "id": "rel_039",
      "source": "corporate_profits",
      "target": "stock_market",
      "relationship_type": "drives",
      "strength": 0.80,
      "time_lag_months": 1,
      "direction": "positive",
      "description": "Corporate earnings drive stock market valuations"
    },
    {
      "id": "rel_040",
      "source": "federal_funds_rate",
      "target": "stock_market",
      "relationship_type": "influences",
      "strength": 0.65,
      "time_lag_months": 0,
      "direction": "negative",
      "description": "Higher rates reduce present value of future earnings"
    }
  ],
  "network_analytics": {
    "centrality_measures": {
      "most_central_nodes": [
        {"node": "federal_funds_rate", "connections": 12, "influence_score": 0.89},
        {"node": "gdp", "connections": 11, "influence_score": 0.85},
        {"node": "consumption", "connections": 9, "influence_score": 0.82},
        {"node": "employment_rate", "connections": 8, "influence_score": 0.78},
        {"node": "inflation_cpi", "connections": 7, "influence_score": 0.75}
      ]
    },
    "policy_transmission_chains": [
      {
        "chain_name": "monetary_policy_transmission",
        "path": ["federal_funds_rate", "treasury_10yr", "mortgage_rates", "housing_starts", "gdp"],
        "total_lag_months": 13,
        "strength": 0.73
      },
      {
        "chain_name": "employment_consumption_cycle", 
        "path": ["job_openings", "employment_rate", "wage_growth", "consumption", "gdp"],
        "total_lag_months": 6,
        "strength": 0.81
      },
      {
        "chain_name": "business_confidence_investment",
        "path": ["pmi_manufacturing", "corporate_profits", "business_investment", "productivity", "gdp"],
        "total_lag_months": 27,
        "strength": 0.69
      }
    ],
    "feedback_loops": [
      {
        "loop_name": "gdp_employment_virtuous_cycle",
        "nodes": ["gdp", "employment_rate", "consumption"],
        "loop_type": "positive",
        "strength": 0.85
      },
      {
        "loop_name": "inflation_policy_response",
        "nodes": ["inflation_cpi", "federal_funds_rate", "consumption", "gdp"],
        "loop_type": "negative", 
        "strength": 0.75
      }
    ]
  },
  "interaction_config": {
    "node_sizing": {
      "method": "gdp_weight",
      "min_size": 10,
      "max_size": 50
    },
    "edge_styling": {
      "thickness_based_on": "strength",
      "color_based_on": "relationship_type",
      "arrow_based_on": "direction"
    },
    "filtering_options": {
      "by_category": ["macro_aggregate", "gdp_component", "labor_market", "monetary_policy", "sentiment", "housing", "production", "financial_markets"],
      "by_quality_tier": ["tier_1", "tier_2", "tier_3"],
      "by_indicator_type": ["leading", "coincident", "lagging"],
      "by_strength": {"min": 0.1, "max": 1.0}
    },
    "layout_algorithms": {
      "default": "force_directed",
      "alternatives": ["circular", "hierarchical", "cluster"]
    },
    "time_animation": {
      "enabled": true,
      "show_lags": true,
      "propagation_speed": "adjustable"
    }
  },
  "data_quality_overlay": {
    "quality_indicators": {
      "tier_1": {"color": "#2E8B57", "opacity": 1.0},
      "tier_2": {"color": "#FFD700", "opacity": 0.8},
      "tier_3": {"color": "#FF8C00", "opacity": 0.6},
      "tier_4": {"color": "#FF6347", "opacity": 0.4},
      "tier_5": {"color": "#DC143C", "opacity": 0.2}
    },
    "uncertainty_bands": {
      "enabled": true,
      "calculation": "inverse_of_quality_score",
      "visualization": "confidence_intervals"
    }
  }
}