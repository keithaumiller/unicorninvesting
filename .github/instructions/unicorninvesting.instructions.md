---
applyTo: '**'
---

# Unicorn Investing Platform - Development Instructions

## Project Overview

Unicorn Investing is a financial analytics platform for unicorn startups, high-growth companies, and market analysis. It provides investment analysis, portfolio management, algorithmic trading, and ML-driven recommendations.

## ⚠️ IMPORTANT - First Time Setup After Codespace Restart

**After codespace restart or pause:**
- Run: `./scripts/unicorn_environment.sh` (comprehensive setup + health check)
- Or use: `unicorn-env` (after initial setup)
- Start services: `drupal-start` or `scripts/startup_drupal.sh`
- Available aliases: `drupal-start`, `drupal-status`, `drupal-logs`, `drupal-restart`, `drupal-cd`, `unicorn-root`, `unicorn-env`

### 🌐 GitHub Codespace URL Translation for Debugging

**Debugging tip:** Use `http://localhost/...` in terminal (not external GitHub Codespace URLs).

### Current State
### Architecture
- Frontend: Drupal 11 (PHP 8.2+)
- Backend: Python 3.9+ (pandas, scikit-learn, etc.)
- Database: MySQL 8.0+
- Web Server: Apache/Nginx

## Technology Stack

### Primary Technologies
### Key Technologies
- Drupal 11, PHP 8.2+
- Python 3.9+, pandas, scikit-learn, etc.
- MySQL 8.0+
- Apache/Nginx
- Git

### Python Dependencies
### Python Dependencies
- pandas, numpy, scipy
- scikit-learn, tensorflow, keras
- quantlib, yfinance, alpha_vantage
- SQLAlchemy, PyMySQL
- FastAPI, requests
- matplotlib, plotly, seaborn

### Frontend Dependencies
### Frontend Dependencies
- Drupal 11
- PHP 8.2+
- JavaScript (ES6+)
- SCSS/Sass, Bootstrap

## Coding Standards

### Drupal 11 Standards
Follow official Drupal coding standards and best practices:

1. **PHP Code Style**
   - Use PSR-12 coding standard
   - Follow Drupal API documentation patterns
   - Implement proper dependency injection
   - Use typed properties and return types where possible

2. **Module Development**
   - Create custom modules for unicorn-specific functionality
   - Use proper hook implementations
   - Follow configuration management best practices
   - Implement proper caching strategies
   - **CRITICAL**: Always clear Drupal cache after ANY module changes using the proper su method (see Drush Operations section)

3. **Theme Development**
   - Use Twig templating system
   - Implement responsive design patterns
   - Follow accessibility guidelines (WCAG 2.1)
   - Use Drupal's CSS and JavaScript libraries

4. **Database Integration**
   - Use Drupal's database abstraction layer
   - Implement proper entity relationships
   - Use Views for data display
   - Follow content type and field best practices

### Drupal Development Best Practices

1. **Cache Management**
   - Always fix permissions before cache operations to avoid errors
   - Use proper user context for cache rebuilds
   - **IMPORTANT**: Run Drush cache operations as www-data user to prevent MySQL PDO errors
   ```bash
   # Fix permissions before cache operations
   
   # Remove problematic cached files if needed
   sudo rm -rf /workspaces/unicorninvesting/WebFrontend/web/sites/default/files/css/*
   sudo rm -rf /workspaces/unicorninvesting/WebFrontend/web/sites/default/files/js/*
   
   ```

2. **Drush Operations**
   - Always use system PHP for Drush operations: `/usr/bin/php8.3 ./vendor/bin/drush.php`
   - **CRITICAL**: For Drush commands with database operations, use sudo to avoid permission issues:
     ```bash
     # Simple method that works in this environment:
     cd /workspaces/unicorninvesting/WebFrontend
     sudo /usr/bin/php8.3 ./vendor/bin/drush.php cache:rebuild
     ```
   - Verify PHP extensions are available before troubleshooting
   - See `/workspaces/unicorninvesting/docs/DRUSH_DEPENDENCIES_CHECKLIST.md` for complete troubleshooting guide

3. **Module Development & Changes**
    - **MANDATORY**: After ANY change to Drupal modules (code, CSS, routing, etc.), ALWAYS clear the Drupal cache.
    - To clear cache:
       ```bash
       su root
       ./scripts/drupalcachreset.sh
       ```

### Python Standards
- Follow PEP 8
- Use type hints and dataclasses
- Use pandas for data manipulation
- Use scikit-learn pipelines, validate models
- Use FastAPI for APIs, Pydantic for validation

### Database Standards
### Database Standards
- Normalize schema, use indexes and FKs
- Use snake_case naming
- Use parameterized queries, transactions
- Use SQLAlchemy ORM

## Migration Strategy

## Migration Strategy
- Phase 1: Infra setup (LAMP, Drupal, MySQL, Python venv)
- Phase 2: Data migration (R→Python, files→DB, validation)
- Phase 3: Backend (Python analytics, REST APIs, ML migration)
- Phase 4: Frontend (Drupal content types, auth, dashboard)


   - Monitor application performance metrics
## Legacy Code Migration

### R to Python Conversion Guidelines
1. **Data Processing Functions**
   ```r
   # R Code (Legacy)
     # R implementation
   }
   ```
   
   ```

2. **Machine Learning Models**
   - Convert R neural networks to TensorFlow/Keras
   - Migrate statistical models to scikit-learn
   - Implement proper model versioning
   - Maintain backward compatibility during transition

### WPF Application Replacement
1. Replace desktop WPF applications with web-based Drupal interface
2. Migrate user workflows to web forms
3. Implement real-time data updates via WebSockets
4. Ensure feature parity with legacy applications

## Quality Assurance

### Code Review Process
### Documentation Requirements
### Deployment Process
## Quality & Deployment
- Code review required for all changes
- Automated tests must pass
- Security scan for vulnerabilities
- Document functions, APIs, DB changes, user features
- Use Git, CI/CD, staging, monitor deployments

## 📝 Documentation Standards

### **CRITICAL: No Summary Files After Actions**
- **❌ DO NOT** create separate summary files (e.g., `*_SUMMARY.md`, `*_COMPLETE.md`) after completing tasks
- **❌ DO NOT** create standalone documentation files for completed work

### **✅ Documentation Approach:**

#### **1. README.md Files (Primary Documentation)**
- **Update existing README.md files** in the relevant directory after any changes
- **Create README.md files** for new directories or components
- Include comprehensive information about functionality, usage, and architecture
- Keep documentation current and accurate

#### **2. Commit Messages (Action Reporting)**
- **Use detailed commit messages** for after-action reporting
- Include what was accomplished, why it was done, and any important notes
- Structure: `[Type]: Brief description`
- Body: Detailed explanation of changes and impact

#### **3. Inline Code Documentation**
- Document functions, classes, and complex logic directly in code
- Use appropriate comment styles for each language (docstrings, JSDoc, etc.)
- Include usage examples where helpful

### **Documentation Workflow:**
1. **Make changes** to code/configuration
2. **Update relevant README.md** with new information
3. **Commit with detailed message** explaining the changes
4. **NO separate summary files**

### **Example Good Commit Message:**
```
feat: Consolidate health_check.sh and setup_environment.sh scripts

- Created comprehensive unicorn_environment.sh script combining functionality
- Added modular execution options (--setup-only, --check-only, --help)
- Enhanced health checks with 23 system validations
- Moved legacy scripts to scripts/legacy/ directory
- Added deprecation wrappers for backward compatibility
- Updated documentation in scripts/README.md
- Success rate: 86% system validation with minor issues identified
```

## README.md Context
When working on any part of this codebase, always consider:
## 🎯 **GitHub Issues Management Strategy**

### **Issue Creation Policy**
- **Create GitHub Issues** for all documented but unimplemented features
- **Track architectural decisions** that need future implementation
- **Use epics** for high-level components and use cases
- **Link issues** to documentation and commits for traceability

### **Issue Categories & Labels**

#### **Epic Issues** (`epic` label)
- **Purpose**: Major architectural components requiring multiple sub-issues
- **Scope**: Multi-week initiatives (4+ weeks)
- **Examples**: "Epic: ETH Framework Implementation", "Epic: LEAN Integration"
- **When to Create**: High-level architecture documented but not implemented

#### **Feature Issues** (`feature` label)  
- **Purpose**: Specific functionality or components to be built
- **Scope**: 1-3 week development cycles
- **Examples**: "Feature: Real-time ETH Data Pipeline", "Feature: ML Model Training"
- **When to Create**: Detailed requirements exist but implementation pending

#### **Task Issues** (`task` label)
- **Purpose**: Specific coding tasks or configurations
- **Scope**: 1-5 day implementation items
- **Examples**: "Task: Implement Kelly Criterion", "Task: Add VaR calculations"
- **When to Create**: Specific technical requirements identified

#### **Documentation Issues** (`documentation` label)
- **Purpose**: Missing documentation that should be created
- **Examples**: "Docs: API documentation", "Docs: User guide for trading"
- **When to Create**: Functionality exists but documentation missing

### **Automatic Issue Creation Rules**

**✅ CREATE ISSUES FOR:**
- Any README.md that references unimplemented features
- Use cases defined in specifications but not coded
- Process flows documented but not built
- Architecture diagrams with missing components
- Data requirements specified but not collected
- Framework components planned but not developed

**🔧 ISSUE TEMPLATES:**

```markdown
## Epic Issue Template
**Epic**: [Component Name]
**Description**: [High-level scope and purpose]
**Requirements**: [Major requirements and features]
**Use Cases**: [Which use cases this epic serves]
**Dependencies**: [Other epics or features this depends on]
**Acceptance Criteria**: [What constitutes epic completion]
**Estimated Timeline**: [Rough weeks estimate]
**Priority**: [High/Medium/Low based on business impact]
```

```markdown
## Feature Issue Template  
**Feature**: [Specific functionality]
**Description**: [Detailed feature description]
**Technical Requirements**: [Specific technical specs]
**User Story**: [As a user, I want... so that...]
**Dependencies**: [Prerequisites for this feature]
**Acceptance Criteria**: [Specific completion criteria]
**Related Epic**: [Link to parent epic]
**Estimated Effort**: [Story points or days]
```

### **Issue Management Workflow**

#### **Issue Lifecycle**
1. **Created** → Issue identified from documentation/planning
2. **Planned** → Added to milestone and prioritized
3. **In Progress** → Developer assigned and working
4. **Review** → Implementation complete, under review
5. **Done** → Merged and deployed

#### **Integration with Development**
```bash
# Commit message format linking to issues
git commit -m "feat: Add ETH data collector (closes #123)"
git commit -m "docs: Update framework docs (refs #456)"

# Branch naming convention
git checkout -b "epic/123-eth-framework"
git checkout -b "feature/456-realtime-data"
git checkout -b "task/789-kelly-criterion"
```

#### **Progress Tracking**
- **Milestones**: Group issues by implementation phases
- **Projects**: Kanban boards for visual progress tracking
- **Labels**: Priority, type, status, and component classification
- **Assignees**: Track ownership and workload distribution

### **Documentation-to-Issue Mapping**

**🔍 SCAN FOR ISSUES IN:**
- `/BackendPython/unicorn/eth_framework/*.md` - ETH framework specs
- `/BackendPython/unicorn/*/README.md` - Component documentation  
- `/docs/*.md` - High-level architecture docs
- `/deployment/*.md` - Deployment and setup docs
- Code comments with `TODO:`, `FIXME:`, `HACK:` annotations

## General Principles
- High accuracy and security (financial platform)
- Performance is critical for real-time trading
- Intuitive UX for financial professionals
- Maintainable, well-documented, auditable code
- **Use GitHub Issues** to track all planned but unimplemented work

## Parent Directory Context

### /workspaces/unicorninvesting/
## Repo Context
- Main repo: R scripts, data pipelines, portfolio/optimization, MySQL integration, batch jobs, research/backtesting
- Goal: Modernize, preserve financial logic, improve scalability, maintainability, UX
- **Track progress via GitHub Issues** for all architectural components

- Always review and update README.md in the same directory after any file change.
- **Create GitHub Issues** for any new unimplemented features discovered in documentation.