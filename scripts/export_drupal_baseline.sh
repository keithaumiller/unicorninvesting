#!/bin/bash

# Unicorn Investing - Drupal Baseline Export Script
# This script creates a complete backup/baseline of a working Drupal installation
# for use as a reference installation point in fresh environments

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
WORKSPACE_ROOT="/workspaces/unicorninvesting"
DRUPAL_ROOT="$WORKSPACE_ROOT/WebFrontend"
SCRIPTS_DIR="$WORKSPACE_ROOT/scripts"
EXPORT_DIR="$SCRIPTS_DIR/drupal_baseline_exports"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
EXPORT_NAME="unicorn_drupal_baseline_$TIMESTAMP"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}🦄 Drupal Baseline Export Utility${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Function to create export directory
create_export_directory() {
    echo -e "${YELLOW}📁 Creating export directory...${NC}"
    
    mkdir -p "$EXPORT_DIR/$EXPORT_NAME"
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ Export directory created: $EXPORT_DIR/$EXPORT_NAME${NC}"
        return 0
    else
        echo -e "${RED}❌ Failed to create export directory${NC}"
        return 1
    fi
}

# Function to export database
export_database() {
    echo -e "${YELLOW}🗄️  Exporting database...${NC}"
    
    cd "$DRUPAL_ROOT" || return 1
    
    local db_file="$EXPORT_DIR/$EXPORT_NAME/database_dump.sql"
    
    if /usr/bin/php8.3 ./vendor/bin/drush.php sql:dump > "$db_file" 2>/dev/null; then
        local db_size=$(du -h "$db_file" | cut -f1)
        echo -e "${GREEN}✅ Database exported successfully ($db_size)${NC}"
        echo -e "${CYAN}ℹ️  Database file: database_dump.sql${NC}"
        return 0
    else
        echo -e "${RED}❌ Database export failed${NC}"
        return 1
    fi
}

# Function to export configuration
export_configuration() {
    echo -e "${YELLOW}⚙️  Exporting Drupal configuration...${NC}"
    
    cd "$DRUPAL_ROOT" || return 1
    
    local config_dir="$EXPORT_DIR/$EXPORT_NAME/config"
    mkdir -p "$config_dir"
    
    if /usr/bin/php8.3 ./vendor/bin/drush.php config:export --destination="$config_dir" -y 2>/dev/null; then
        local config_count=$(find "$config_dir" -name "*.yml" | wc -l)
        echo -e "${GREEN}✅ Configuration exported successfully ($config_count files)${NC}"
        echo -e "${CYAN}ℹ️  Configuration directory: config/${NC}"
        return 0
    else
        echo -e "${YELLOW}⚠️  Configuration export failed (may not be critical)${NC}"
        return 0  # Don't fail entire export for config issues
    fi
}

# Function to export site files
export_site_files() {
    echo -e "${YELLOW}📄 Exporting site files...${NC}"
    
    local files_source="$DRUPAL_ROOT/web/sites/default/files"
    local files_dest="$EXPORT_DIR/$EXPORT_NAME/files"
    
    if [ -d "$files_source" ]; then
        cp -r "$files_source" "$files_dest" 2>/dev/null
        if [ $? -eq 0 ]; then
            local files_count=$(find "$files_dest" -type f | wc -l)
            local files_size=$(du -sh "$files_dest" 2>/dev/null | cut -f1 || echo "unknown")
            echo -e "${GREEN}✅ Site files exported successfully ($files_count files, $files_size)${NC}"
            echo -e "${CYAN}ℹ️  Files directory: files/${NC}"
            return 0
        else
            echo -e "${YELLOW}⚠️  Site files export failed${NC}"
            return 0  # Don't fail for files issues
        fi
    else
        echo -e "${YELLOW}⚠️  No site files directory found${NC}"
        return 0
    fi
}

# Function to export settings and configuration files
export_settings() {
    echo -e "${YELLOW}🔧 Exporting settings and configuration files...${NC}"
    
    local settings_dir="$EXPORT_DIR/$EXPORT_NAME/settings"
    mkdir -p "$settings_dir"
    
    # Copy important configuration files
    local files_to_copy=(
        "web/sites/default/settings.php"
        "web/.htaccess"
        "composer.json"
        "composer.lock"
    )
    
    for file in "${files_to_copy[@]}"; do
        local source_file="$DRUPAL_ROOT/$file"
        if [ -f "$source_file" ]; then
            local dest_path="$settings_dir/$(basename "$file")"
            cp "$source_file" "$dest_path"
            echo -e "${CYAN}ℹ️  Copied: $(basename "$file")${NC}"
        fi
    done
    
    echo -e "${GREEN}✅ Settings files exported${NC}"
    echo -e "${CYAN}ℹ️  Settings directory: settings/${NC}"
    return 0
}

# Function to create installation script
create_installation_script() {
    echo -e "${YELLOW}📋 Creating installation script...${NC}"
    
    local install_script="$EXPORT_DIR/$EXPORT_NAME/restore_baseline.sh"
    
    cat > "$install_script" << 'EOF'
#!/bin/bash

# Unicorn Investing - Drupal Baseline Restore Script
# This script restores a Drupal installation from the exported baseline

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

WORKSPACE_ROOT="/workspaces/unicorninvesting"
DRUPAL_ROOT="$WORKSPACE_ROOT/WebFrontend"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}🦄 Drupal Baseline Restore Utility${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Get the directory where this script is located
BASELINE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo -e "${YELLOW}📁 Restoring from baseline: $(basename "$BASELINE_DIR")${NC}"

# 1. Ensure database exists and restore
echo -e "${YELLOW}🗄️  Restoring database...${NC}"
if [ -f "$BASELINE_DIR/database_dump.sql" ]; then
    # Create database if it doesn't exist
    sudo mysql -u root -e "CREATE DATABASE IF NOT EXISTS unicorn_drupal;" 2>/dev/null
    
    # Import database
    if sudo mysql -u root unicorn_drupal < "$BASELINE_DIR/database_dump.sql" 2>/dev/null; then
        echo -e "${GREEN}✅ Database restored successfully${NC}"
    else
        echo -e "${RED}❌ Database restore failed${NC}"
        exit 1
    fi
else
    echo -e "${RED}❌ Database dump file not found${NC}"
    exit 1
fi

# 2. Restore files if they exist
if [ -d "$BASELINE_DIR/files" ]; then
    echo -e "${YELLOW}📄 Restoring site files...${NC}"
    mkdir -p "$DRUPAL_ROOT/web/sites/default/files"
    cp -r "$BASELINE_DIR/files/"* "$DRUPAL_ROOT/web/sites/default/files/" 2>/dev/null
    sudo chown -R www-data:www-data "$DRUPAL_ROOT/web/sites/default/files" 2>/dev/null
    sudo chmod -R 755 "$DRUPAL_ROOT/web/sites/default/files" 2>/dev/null
    echo -e "${GREEN}✅ Site files restored${NC}"
fi

# 3. Restore settings if available
if [ -f "$BASELINE_DIR/settings/settings.php" ]; then
    echo -e "${YELLOW}🔧 Restoring settings...${NC}"
    cp "$BASELINE_DIR/settings/settings.php" "$DRUPAL_ROOT/web/sites/default/settings.php"
    chmod 644 "$DRUPAL_ROOT/web/sites/default/settings.php"
    echo -e "${GREEN}✅ Settings restored${NC}"
fi

# 4. Clear caches
echo -e "${YELLOW}🧹 Clearing Drupal caches...${NC}"
cd "$DRUPAL_ROOT"
if /usr/bin/php8.3 ./vendor/bin/drush.php cache:rebuild 2>/dev/null; then
    echo -e "${GREEN}✅ Caches cleared${NC}"
else
    echo -e "${YELLOW}⚠️  Cache clearing failed (may not be critical)${NC}"
fi

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}🎉 Baseline restore completed!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo -e "${CYAN}ℹ️  Run the startup_drupal.sh script to validate the installation${NC}"
EOF

    chmod +x "$install_script"
    echo -e "${GREEN}✅ Installation script created: restore_baseline.sh${NC}"
    return 0
}

# Function to create archive
create_archive() {
    echo -e "${YELLOW}📦 Creating compressed archive...${NC}"
    
    cd "$EXPORT_DIR" || return 1
    
    local archive_name="$EXPORT_NAME.tar.gz"
    
    if tar -czf "$archive_name" "$EXPORT_NAME" 2>/dev/null; then
        local archive_size=$(du -h "$archive_name" | cut -f1)
        echo -e "${GREEN}✅ Archive created successfully: $archive_name ($archive_size)${NC}"
        echo -e "${CYAN}ℹ️  Archive path: $EXPORT_DIR/$archive_name${NC}"
        return 0
    else
        echo -e "${RED}❌ Archive creation failed${NC}"
        return 1
    fi
}

# Function to generate documentation
generate_documentation() {
    echo -e "${YELLOW}📋 Generating documentation...${NC}"
    
    local readme_file="$EXPORT_DIR/$EXPORT_NAME/README.md"
    
    cat > "$readme_file" << EOF
# Unicorn Investing Drupal Baseline Export

**Export Date:** $(date '+%Y-%m-%d %H:%M:%S')  
**Export Name:** $EXPORT_NAME

## Contents

This baseline export contains a complete working Drupal installation for the Unicorn Investing platform.

### Files Included:
- \`database_dump.sql\` - Complete database backup
- \`config/\` - Drupal configuration files  
- \`files/\` - Site files and uploads
- \`settings/\` - Important configuration files
- \`restore_baseline.sh\` - Automated restore script

### Site Information:
- **Site Name:** unicorninvesting.us
- **Admin User:** admin
- **Admin Password:** admin123
- **Database:** unicorn_drupal
- **Database User:** drupal_user

## Usage

### Quick Restore:
\`\`\`bash
# Extract the baseline
tar -xzf $EXPORT_NAME.tar.gz

# Run the restore script
cd $EXPORT_NAME
./restore_baseline.sh

# Validate the installation
cd /workspaces/unicorninvesting
./scripts/startup_drupal.sh
\`\`\`

### Manual Restore:
1. Import database: \`sudo mysql -u root unicorn_drupal < database_dump.sql\`
2. Copy files to \`web/sites/default/files/\`
3. Copy settings.php to \`web/sites/default/settings.php\`
4. Run \`drush cache:rebuild\`

## Notes

This baseline can be used to quickly set up a working Drupal environment that passes all validation checks in the startup_drupal.sh script.

Export created by: \`export_drupal_baseline.sh\`
EOF

    echo -e "${GREEN}✅ Documentation generated: README.md${NC}"
    return 0
}

# Main execution
main() {
    echo -e "${BLUE}📋 Starting Drupal baseline export...${NC}"
    echo ""
    
    # Validate we're in the right environment
    if [ ! -d "$DRUPAL_ROOT" ]; then
        echo -e "${RED}❌ Drupal directory not found: $DRUPAL_ROOT${NC}"
        exit 1
    fi
    
    # Check if Drupal is installed
    cd "$DRUPAL_ROOT"
    if ! /usr/bin/php8.3 ./vendor/bin/drush.php status 2>/dev/null | grep -q "Drupal bootstrap.*Successful"; then
        echo -e "${RED}❌ Drupal does not appear to be properly installed${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}✅ Drupal installation detected${NC}"
    echo ""
    
    # Execute export steps
    create_export_directory || exit 1
    echo ""
    
    export_database || exit 1
    echo ""
    
    export_configuration
    echo ""
    
    export_site_files
    echo ""
    
    export_settings
    echo ""
    
    create_installation_script
    echo ""
    
    generate_documentation
    echo ""
    
    create_archive || exit 1
    echo ""
    
    # Final summary
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}🎉 Drupal baseline export completed!${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    echo -e "${BLUE}📁 Export Location:${NC}"
    echo -e "${CYAN}   Directory: $EXPORT_DIR/$EXPORT_NAME${NC}"
    echo -e "${CYAN}   Archive: $EXPORT_DIR/$EXPORT_NAME.tar.gz${NC}"
    echo ""
    echo -e "${BLUE}📋 Usage:${NC}"
    echo -e "${CYAN}   Extract: tar -xzf $EXPORT_NAME.tar.gz${NC}"
    echo -e "${CYAN}   Restore: cd $EXPORT_NAME && ./restore_baseline.sh${NC}"
    echo ""
    echo -e "${GREEN}✅ Baseline ready for deployment!${NC}"
}

# Run main function
main "$@"
