# Unicorn Investing Drupal Baseline Export

**Export Date:** 2025-09-05 00:13:13  
**Export Name:** unicorn_drupal_baseline_20250905_001311

## Contents

This baseline export contains a complete working Drupal installation for the Unicorn Investing platform.

### Files Included:
- `database_dump.sql` - Complete database backup
- `config/` - Drupal configuration files  
- `files/` - Site files and uploads
- `settings/` - Important configuration files
- `restore_baseline.sh` - Automated restore script

### Site Information:
- **Site Name:** unicorninvesting.us
- **Admin User:** admin
- **Admin Password:** admin123
- **Database:** unicorn_drupal
- **Database User:** drupal_user

## Usage

### Quick Restore:
```bash
# Extract the baseline
tar -xzf unicorn_drupal_baseline_20250905_001311.tar.gz

# Run the restore script
cd unicorn_drupal_baseline_20250905_001311
./restore_baseline.sh

# Validate the installation
cd /workspaces/unicorninvesting
./scripts/startup_drupal.sh
```

### Manual Restore:
1. Import database: `sudo mysql -u root unicorn_drupal < database_dump.sql`
2. Copy files to `web/sites/default/files/`
3. Copy settings.php to `web/sites/default/settings.php`
4. Run `drush cache:rebuild`

## Notes

This baseline can be used to quickly set up a working Drupal environment that passes all validation checks in the startup_drupal.sh script.

Export created by: `export_drupal_baseline.sh`
