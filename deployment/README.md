# Deployment

**Status**: ✅ Production infrastructure deployed and operational

Deployment configurations, infrastructure setup, and operational procedures for the Unicorn Investing platform.

## Infrastructure Status (August 2025)

### ✅ Production Environment
- **Operating System**: Ubuntu 24.04.2 LTS
- **Web Server**: Apache 2.4.58 with virtual hosts
- **Database**: MySQL 8.0.40 with financial-grade security
- **SSL Certificates**: Let's Encrypt with auto-renewal
- **Python Runtime**: Python 3.12.3 with virtual environment

### 🌐 Multi-Domain Setup
```bash
# Production domains with SSL certificates
thetruthperspective.org     # Primary domain with Cloudflare
stlouisintegration.com      # Business integration services
angelicafeliciano.com       # Professional portfolio site
unicorninvesting.us         # Main trading platform
```

### 🔐 Security Configuration
- **HTTPS Everywhere**: All traffic encrypted with SSL
- **Database Security**: Isolated databases with secure authentication
- **File Permissions**: Proper web application security
- **Firewall**: Ubuntu firewall configured for web services

## Deployment Architecture

### Web Server Configuration
```apache
# Apache virtual hosts for all domains
<VirtualHost *:443>
    ServerName unicorninvesting.us
    DocumentRoot /var/www/unicorninvesting.us
    SSLEngine on
    SSLCertificateFile /etc/letsencrypt/live/unicorninvesting.us/fullchain.pem
    SSLCertificateKeyFile /etc/letsencrypt/live/unicorninvesting.us/privkey.pem
</VirtualHost>
```

### Database Configuration
```sql
-- Isolated database setup for security
CREATE DATABASE unicorn_analytics;
CREATE USER 'unicorn_admin'@'localhost' IDENTIFIED BY '[SECURE_PASSWORD]';
GRANT ALL PRIVILEGES ON unicorn_analytics.* TO 'unicorn_admin'@'localhost';
```

### SSL Certificate Management
```bash
# Automated certificate renewal
certbot certonly --webroot -w /var/www/html -d unicorninvesting.us
certbot certonly --webroot -w /var/www/html -d stlouisintegration.com
certbot certonly --webroot -w /var/www/html -d angelicafeliciano.com

# Auto-renewal configured in crontab
0 0,12 * * * /usr/bin/certbot renew --quiet
```

## Application Deployment

### Python Backend
```bash
# Virtual environment setup
cd /workspaces/unicorninvesting
python3 -m venv .venv
source .venv/bin/activate

# Production deployment
cd BackendPython/unicorn/backend
/workspaces/unicorninvesting/.venv/bin/python -m uvicorn api.main:app --host 0.0.0.0 --port 8000
```

### Drupal Frontend
```bash
# Web root deployment
/var/www/unicorninvesting.us          # Main platform
/var/www/stlouisintegration.com       # Business site
/var/www/angelicafeliciano.com        # Portfolio site
/var/www/html                         # Default/thetruthperspective.org
```

## Monitoring and Maintenance

### Health Monitoring
```bash
# System health checks
systemctl status apache2    # Web server status
systemctl status mysql      # Database status
curl -s http://localhost:8000/health  # API health check
```

### Log Management
```bash
# Application logs
/var/log/apache2/           # Web server logs
/var/log/mysql/             # Database logs
journalctl -u apache2       # System service logs
```

### Backup Procedures
```bash
# Database backups
mysqldump --single-transaction unicorn_analytics > backup_$(date +%Y%m%d).sql

# Application backups
tar -czf backup_$(date +%Y%m%d).tar.gz /var/www/
```

## Development vs Production

### Development Environment
- **Location**: `/workspaces/unicorninvesting/`
- **Database**: Development databases with relaxed security
- **SSL**: Self-signed certificates for testing
- **Debugging**: Enhanced logging and error reporting

### Production Environment
- **Location**: `/var/www/` for web applications
- **Database**: Production databases with strict security
- **SSL**: Valid Let's Encrypt certificates
- **Performance**: Optimized for production workloads

## Deployment Procedures

### Application Updates
1. **Code Updates**: Deploy through git version control
2. **Database Migrations**: Apply schema changes with rollback plans
3. **SSL Renewal**: Automated certificate renewal processes
4. **Service Restart**: Coordinated service restart procedures

### Security Updates
1. **System Updates**: Regular Ubuntu security updates
2. **Package Updates**: Python package security updates
3. **Certificate Renewal**: Automated SSL certificate management
4. **Database Security**: Regular MySQL security patches

## Operational Commands

### Service Management
```bash
# Web server management
sudo systemctl start apache2
sudo systemctl stop apache2
sudo systemctl restart apache2
sudo systemctl reload apache2

# Database management
sudo systemctl start mysql
sudo systemctl stop mysql
sudo systemctl restart mysql

# SSL certificate renewal
sudo certbot renew --dry-run
sudo certbot renew
```

### Application Management
```bash
# Python backend
cd /workspaces/unicorninvesting/BackendPython/unicorn/backend
/workspaces/unicorninvesting/.venv/bin/python -m uvicorn api.main:app --reload

# Database operations
mysql -u unicorn_admin -p unicorn_analytics
```

## Future Deployment Considerations

### Containerization
- **Docker**: Container deployment for scalability
- **Kubernetes**: Container orchestration for microservices
- **CI/CD**: Automated deployment pipelines

### Performance Optimization
- **Load Balancing**: Multiple server deployment
- **Caching**: Redis caching layer implementation
- **CDN**: Content delivery network for global access