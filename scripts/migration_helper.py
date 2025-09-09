#!/usr/bin/env python3
"""
Security Migration Helper - Unicorn Investing Platform

Scans Python files for hardcoded credentials and provides migration suggestions.
Updates files to use the secure configuration manager where possible.
"""

import os
import sys
import re
import argparse
from pathlib import Path

# Credential patterns to find and replace
CREDENTIAL_PATTERNS = {
    'fred_api_key': r'e4de78babaac7891e9896f8fa390e675',
    'bea_api_key': r'8E9AE912-2B48-435A-8910-521609627585',
    'ibkr_account': r'DUM785491',
    'ibkr_username': r'xyzyuc422',
    'mysql_password': r'unicorn123'
}

# Migration templates
MIGRATION_TEMPLATES = {
    'api_key_import': """# Replace hardcoded API key with secure configuration
from config.config_manager import get_api_key

# Before: api_key = 'hardcoded_key'
# After:
api_key = get_api_key('{service}')""",
    
    'database_import': """# Replace hardcoded database config with secure configuration  
from config.config_manager import get_database_config, get_database_url

# Before: connection = pymysql.connect(host='localhost', user='unicorn', password='unicorn123')
# After:
db_config = get_database_config('development')  # or 'production'
connection = pymysql.connect(**db_config)

# Or for SQLAlchemy:
database_url = get_database_url('development')
engine = create_engine(database_url)""",
    
    'ibkr_import': """# Replace hardcoded IBKR config with secure configuration
from config.config_manager import get_ibkr_config

# Before: account_id = 'DUM785491'
# After:
ibkr_config = get_ibkr_config()
account_id = ibkr_config['account_id']
username = ibkr_config['username']"""
}

def scan_file_for_credentials(file_path):
    """Scan a file for hardcoded credentials."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        findings = []
        for cred_type, pattern in CREDENTIAL_PATTERNS.items():
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                line_num = content[:match.start()].count('\n') + 1
                findings.append({
                    'type': cred_type,
                    'pattern': pattern,
                    'line': line_num,
                    'match': match.group()
                })
        
        return findings
        
    except Exception as e:
        return []

def suggest_migration(findings, file_path):
    """Suggest migration steps for found credentials."""
    if not findings:
        return None
        
    suggestions = {
        'file': file_path,
        'issues': findings,
        'migration_steps': []
    }
    
    # Group findings by type
    credential_types = set(f['type'] for f in findings)
    
    for cred_type in credential_types:
        if cred_type in ['fred_api_key', 'bea_api_key']:
            service = cred_type.replace('_api_key', '')
            suggestions['migration_steps'].append({
                'type': 'api_key',
                'service': service,
                'template': MIGRATION_TEMPLATES['api_key_import'].format(service=service)
            })
        elif cred_type in ['mysql_password']:
            suggestions['migration_steps'].append({
                'type': 'database',
                'template': MIGRATION_TEMPLATES['database_import']
            })
        elif cred_type in ['ibkr_account', 'ibkr_username']:
            suggestions['migration_steps'].append({
                'type': 'ibkr',
                'template': MIGRATION_TEMPLATES['ibkr_import']
            })
    
    return suggestions

def scan_directory(directory):
    """Scan directory for Python files with credentials."""
    directory = Path(directory)
    results = []
    
    for python_file in directory.rglob('*.py'):
        # Skip vendor directories and virtual environments
        if any(skip in str(python_file) for skip in ['vendor', 'node_modules', '.venv', '__pycache__']):
            continue
            
        findings = scan_file_for_credentials(python_file)
        if findings:
            suggestions = suggest_migration(findings, python_file)
            if suggestions:
                results.append(suggestions)
    
    return results

def print_migration_report(results):
    """Print a formatted migration report."""
    if not results:
        print("✅ No hardcoded credentials found in Python files!")
        return
    
    print(f"🔍 CREDENTIAL MIGRATION REPORT")
    print("=" * 50)
    print(f"Found {len(results)} files with hardcoded credentials\n")
    
    for i, result in enumerate(results, 1):
        print(f"📁 File {i}: {result['file']}")
        print("-" * 40)
        
        for issue in result['issues']:
            print(f"  Line {issue['line']}: {issue['type']} ({issue['match']})")
        
        print(f"\n🔧 Migration Steps:")
        for step in result['migration_steps']:
            print(f"\n{step['template']}")
        
        print("\n" + "="*50 + "\n")

def main():
    parser = argparse.ArgumentParser(description='Scan for hardcoded credentials and suggest migrations')
    parser.add_argument('directory', nargs='?', default='.', help='Directory to scan (default: current directory)')
    parser.add_argument('--fix', action='store_true', help='Attempt to automatically fix some issues')
    
    args = parser.parse_args()
    
    print("🔒 Unicorn Investing Security Migration Helper")
    print("=" * 50)
    print(f"Scanning: {os.path.abspath(args.directory)}\n")
    
    results = scan_directory(args.directory)
    print_migration_report(results)
    
    if results:
        print("📋 Next Steps:")
        print("1. Review each file and update hardcoded credentials")
        print("2. Test with: python3 config/usage_examples.py")
        print("3. Run security audit: ./scripts/security_audit.sh")
        print("4. Ensure config/secrets.json is properly configured")
    
    return len(results)

if __name__ == '__main__':
    sys.exit(main())
