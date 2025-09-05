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
